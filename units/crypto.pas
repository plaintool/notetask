unit crypto;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Base64,
  DCPrijndael,
  DCPsha256;

function EncryptData(const PlainText, Password: string): string;

function DecryptData(const CipherBase64, Password: string): string;

implementation

/// EncryptData returns Base64 string containing: IV(16) || HMAC(32) || CipherText
function EncryptData(const PlainText, Password: string): string;
var
  Cipher: TDCP_rijndael;
  Hash: TDCP_sha256;
  InputStream, OutputStream: TMemoryStream;
  // initialize small arrays at declaration to avoid FPC uninitialized hints
  Key: array[0..15] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  TempHash: array[0..31] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  IV: array[0..15] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  HMAC: array[0..31] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  UTF8Input: rawbytestring;
  Buffer: rawbytestring;
  i: integer;
begin
  Result := string.Empty;
  // convert input to UTF-8 bytes
  UTF8Input := UTF8Encode(PlainText);
  Buffer := string.Empty;

  InputStream := TMemoryStream.Create;
  OutputStream := TMemoryStream.Create;
  Cipher := TDCP_rijndael.Create(nil);
  Hash := TDCP_sha256.Create(nil);
  try
    if Length(UTF8Input) > 0 then
      InputStream.WriteBuffer(UTF8Input[1], Length(UTF8Input));
    InputStream.Position := 0;

    // generate random IV
    Randomize;
    for i := 0 to High(IV) do
      IV[i] := Random(256);

    // derive key: SHA256(password) -> take first 16 bytes (AES-128)
    Hash.Init;
    Hash.UpdateStr(Password);
    Hash.Final(TempHash[0]);
    Move(TempHash[0], Key[0], SizeOf(Key));

    // encrypt (AES-128)
    Cipher.Init(Key[0], 128, @IV[0]);
    try
      Cipher.EncryptStream(InputStream, OutputStream, InputStream.Size);
    finally
      Cipher.Burn;
    end;

    // compute HMAC-like = SHA256(key || ciphertext)
    Hash.Init;
    Hash.Update(Key[0], SizeOf(Key));
    if OutputStream.Size > 0 then
      // faster: take direct pointer to memory
      Hash.Update(OutputStream.Memory^, OutputStream.Size);
    Hash.Final(HMAC[0]);

    // build container: IV + HMAC + CipherText
    SetLength(Buffer, Length(IV) + Length(HMAC) + OutputStream.Size);
    // copy IV
    Move(IV[0], Buffer[1], Length(IV));
    // copy HMAC
    Move(HMAC[0], Buffer[1 + Length(IV)], Length(HMAC));
    // copy ciphertext (if any)
    if OutputStream.Size > 0 then
      Move(OutputStream.Memory^, Buffer[1 + Length(IV) + Length(HMAC)], OutputStream.Size);

    // base64 encode the whole container
    Result := EncodeStringBase64(Buffer);

    // clear sensitive memory
    FillChar(Key, SizeOf(Key), 0);
    FillChar(TempHash, SizeOf(TempHash), 0);
    FillChar(HMAC, SizeOf(HMAC), 0);
  finally
    InputStream.Free;
    OutputStream.Free;
    Cipher.Free;
    Hash.Free;
  end;
end;

/// DecryptData returns decrypted UTF-8 string and sets Success = True on success
function DecryptData(const CipherBase64, Password: string): string;
var
  Decoded: rawbytestring;
  InputStream, EncryptedStream, OutputStream: TMemoryStream;
  Cipher: TDCP_rijndael;
  Hash: TDCP_sha256;
  Key: array[0..15] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  TempHash: array[0..31] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  IV: array[0..15] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  HMAC: array[0..31] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  ExpectedHMAC: array[0..31] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  EncryptedSize: integer;
  OutBytes: rawbytestring;
  i: integer;
begin
  Result := string.Empty;
  if CipherBase64 = string.Empty then Exit;
  OutBytes := string.Empty;
  Decoded := DecodeStringBase64(CipherBase64);

  // basic length check (IV + HMAC)
  if Length(Decoded) < (16 + 32) then Exit;

  InputStream := TMemoryStream.Create;
  EncryptedStream := TMemoryStream.Create;
  OutputStream := TMemoryStream.Create;
  Cipher := TDCP_rijndael.Create(nil);
  Hash := TDCP_sha256.Create(nil);
  try
    // put decoded bytes into stream for easy reading
    InputStream.WriteBuffer(Decoded[1], Length(Decoded));
    InputStream.Position := 0;

    // read IV and HMAC
    InputStream.ReadBuffer(IV[0], SizeOf(IV));
    InputStream.ReadBuffer(HMAC[0], SizeOf(HMAC));

    // remaining is encrypted data
    EncryptedSize := InputStream.Size - (SizeOf(IV) + SizeOf(HMAC));
    if EncryptedSize < 0 then Exit;

    // copy encrypted bytes into memory stream
    EncryptedStream.CopyFrom(InputStream, EncryptedSize);
    EncryptedStream.Position := 0;

    // derive key
    Hash.Init;
    Hash.UpdateStr(Password);
    Hash.Final(TempHash[0]);
    Move(TempHash[0], Key[0], SizeOf(Key));

    // compute expected HMAC = SHA256(key || ciphertext)
    Hash.Init;
    Hash.Update(Key[0], SizeOf(Key));
    if EncryptedStream.Size > 0 then
      Hash.Update(EncryptedStream.Memory^, EncryptedStream.Size);
    Hash.Final(ExpectedHMAC[0]);

    // constant-time compare HMACs
    for i := 0 to High(HMAC) do
      if HMAC[i] <> ExpectedHMAC[i] then
        Exit(string.Empty);

    // decrypt
    EncryptedStream.Position := 0;
    Cipher.Init(Key[0], 128, @IV[0]);
    try
      Cipher.DecryptStream(EncryptedStream, OutputStream, EncryptedSize);
    finally
      Cipher.Burn;
    end;

    // convert decrypted UTF-8 bytes to string
    if OutputStream.Size > 0 then
    begin
      SetLength(OutBytes, OutputStream.Size);
      OutputStream.Position := 0;
      OutputStream.ReadBuffer(OutBytes[1], OutputStream.Size);
      Result := string(UTF8ToString(OutBytes));
    end
    else
      Result := string.Empty;

    // clear sensitive memory
    FillChar(Key, SizeOf(Key), 0);
    FillChar(TempHash, SizeOf(TempHash), 0);
    FillChar(ExpectedHMAC, SizeOf(ExpectedHMAC), 0);
  finally
    InputStream.Free;
    EncryptedStream.Free;
    OutputStream.Free;
    Cipher.Free;
    Hash.Free;
  end;
end;

end.
