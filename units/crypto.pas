//-----------------------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the GNU General Public License, Version 3 (GPL-3.0)
//  You may obtain a copy of the License at https://www.gnu.org/licenses/gpl-3.0.html
//-----------------------------------------------------------------------------------

unit crypto;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  PasZLib,
  Base64,
  DCPrijndael,
  DCPsha256;

/// Compresses a memory stream and prepends its original size for later decompression.
function CompressMemoryStream(InputStream: TMemoryStream): TMemoryStream;

/// Decompresses a memory stream using the stored original size to allocate output buffer.
function DecompressMemoryStream(InputStream: TMemoryStream): TMemoryStream;

/// EncryptData returns Base64 string containing: IV(16) || HMAC(32) || CipherText
function EncryptData(const PlainText: string; const Hash: TBytes): string;

/// DecryptData returns decrypted UTF-8 string, returns empty string on errors
function DecryptData(const CipherBase64: string; const Hash: TBytes): string;

/// Generate hash bytes from token string
function GetHash(const Token: string): TBytes;

/// Checks if a file is likely encrypted with our format (IV + HMAC + ciphertext)
/// Does not require the password, only tries to decrypt first few bytes
function CouldBeEncryptedFile(const FileName: string): boolean;

// Load file as string
function LoadFileAsString(const FileName: string): rawbytestring;

implementation

const
  FILE_MAGIC: array[0..3] of byte = (78, 84, 83, 75); // 'NTSK'

function CompressBound(SourceLen: cardinal): cardinal;
begin
  // formula from zlib to estimate max compressed size
  Result := SourceLen + ((SourceLen + 7) shr 3) + ((SourceLen + 63) shr 6) + 11;
end;

function CompressMemoryStream(InputStream: TMemoryStream): TMemoryStream;
var
  Source, Dest: pchar;
  SourceLen, DestLen: cardinal;
  MemPtr: pchar;
begin
  if InputStream.Size = 0 then
  begin
    Result := TMemoryStream.Create;
    Exit;
  end;

  InputStream.Position := 0;
  SourceLen := InputStream.Size;
  Source := InputStream.Memory;

  Result := TMemoryStream.Create;
  try
    // allocate memory: 4 bytes for original size + estimated compressed size
    DestLen := compressBound(SourceLen);
    Result.SetSize(4 + DestLen);

    MemPtr := Result.Memory;
    Dest := MemPtr;
    Inc(pbyte(Dest), 4); // move pointer 4 bytes forward for compressed data

    if compress(Dest, DestLen, Source, SourceLen) <> Z_OK then
      raise Exception.Create('Compression failed');

    // store original size in first 4 bytes
    PCardinal(MemPtr)^ := SourceLen;

    // adjust memory size to actual compressed data
    Result.SetSize(4 + DestLen);
    Result.Position := 0;
  except
    Result.Free;
    raise;
  end;
end;

function DecompressMemoryStream(InputStream: TMemoryStream): TMemoryStream;
var
  Source, Dest: pchar;
  OriginalSize, DestLen: cardinal;
  MemPtr: pchar;
begin
  if InputStream.Size = 0 then
  begin
    Result := TMemoryStream.Create;
    Exit;
  end;

  InputStream.Position := 0;
  MemPtr := InputStream.Memory;

  // read original uncompressed size
  OriginalSize := PCardinal(MemPtr)^;

  Source := MemPtr;
  Inc(pbyte(Source), 4); // compressed data starts after 4 bytes

  Result := TMemoryStream.Create;
  try
    DestLen := OriginalSize;
    Result.SetSize(DestLen);
    Dest := Result.Memory;

    if uncompress(Dest, DestLen, Source, InputStream.Size - 4) <> Z_OK then
      raise Exception.Create('Decompression failed');

    Result.Position := 0;
  except
    Result.Free;
    raise;
  end;
end;

function EncryptData(const PlainText: string; const Hash: TBytes): string;
var
  Cipher: TDCP_rijndael;
  Sha256: TDCP_sha256;
  InputStream, CompressedStream, OutputStream: TMemoryStream;
  // initialize small arrays at declaration to avoid FPC uninitialized hints
  Key: array[0..15] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
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
  Sha256 := TDCP_sha256.Create(nil);
  try
    if Length(UTF8Input) > 0 then
      InputStream.WriteBuffer(UTF8Input[1], Length(UTF8Input));

    // generate random IV
    Randomize;
    for i := 0 to High(IV) do
      IV[i] := Random(256);

    // derive key directly from precomputed hash
    if Length(Hash) >= 16 then
      Move(Hash[0], Key[0], SizeOf(Key)) // take first 16 bytes for AES-128
    else
      FillChar(Key, SizeOf(Key), 0);

    CompressedStream := CompressMemoryStream(InputStream);
    try
      CompressedStream.Position := 0;

      // encrypt (AES-128)
      Cipher.Init(Key[0], 128, @IV[0]);
      try
        Cipher.EncryptStream(CompressedStream, OutputStream, CompressedStream.Size);
      finally
        Cipher.Burn;
      end;
    finally
      CompressedStream.Free;
    end;

    // compute HMAC-like = SHA256(key || ciphertext)
    Sha256.Init;
    Sha256.Update(Key[0], SizeOf(Key));
    if OutputStream.Size > 0 then
      // faster: take direct pointer to memory
      Sha256.Update(OutputStream.Memory^, OutputStream.Size);
    Sha256.Final(HMAC[0]);

    // build container: IV + HMAC + CipherText
    SetLength(Buffer, Length(FILE_MAGIC) + Length(IV) + Length(HMAC) + OutputStream.Size);
    // copy Magic
    Move(FILE_MAGIC[0], Buffer[1], Length(FILE_MAGIC));
    // copy IV
    Move(IV[0], Buffer[1 + Length(FILE_MAGIC)], Length(IV));
    // copy HMAC
    Move(HMAC[0], Buffer[1 + Length(FILE_MAGIC) + Length(IV)], Length(HMAC));
    // copy ciphertext (if any)
    if OutputStream.Size > 0 then
      Move(OutputStream.Memory^, Buffer[1 + Length(FILE_MAGIC) + Length(IV) + Length(HMAC)], OutputStream.Size);

    // base64 encode the whole container
    Result := EncodeStringBase64(Buffer);

    // clear sensitive memory
    FillChar(Key, SizeOf(Key), 0);
    FillChar(HMAC, SizeOf(HMAC), 0);
  finally
    InputStream.Free;
    OutputStream.Free;
    Cipher.Free;
    Sha256.Free;
  end;
end;

function DecryptData(const CipherBase64: string; const Hash: TBytes): string;
var
  Decoded: rawbytestring;
  InputStream, EncryptedStream, OutputStream, DecompressedStream: TMemoryStream;
  Cipher: TDCP_rijndael;
  sha256: TDCP_sha256;
  Key: array[0..15] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  IV: array[0..15] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  HMAC: array[0..31] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  ExpectedHMAC: array[0..31] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  Magic: array[0..3] of byte = (0, 0, 0, 0);
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
  sha256 := TDCP_sha256.Create(nil);
  try
    // put decoded bytes into stream for easy reading
    InputStream.WriteBuffer(Decoded[1], Length(Decoded));
    InputStream.Position := 0;

    // read and check MAGIC
    InputStream.ReadBuffer(Magic[0], SizeOf(Magic));
    if not CompareMem(@Magic[0], @FILE_MAGIC[0], SizeOf(FILE_MAGIC)) then
      Exit(string.Empty); // not our encrypted file

    // read IV and HMAC
    InputStream.ReadBuffer(IV[0], SizeOf(IV));
    InputStream.ReadBuffer(HMAC[0], SizeOf(HMAC));

    // remaining is encrypted data
    EncryptedSize := InputStream.Size - (SizeOf(FILE_MAGIC) + SizeOf(IV) + SizeOf(HMAC));
    if EncryptedSize < 0 then Exit;

    // copy encrypted bytes into memory stream
    EncryptedStream.CopyFrom(InputStream, EncryptedSize);
    EncryptedStream.Position := 0;

    // derive key directly from precomputed hash
    if Length(Hash) >= 16 then
      Move(Hash[0], Key[0], SizeOf(Key)) // take first 16 bytes for AES-128
    else
      FillChar(Key, SizeOf(Key), 0);

    // compute expected HMAC = SHA256(key || ciphertext)
    sha256.Init;
    sha256.Update(Key[0], SizeOf(Key));
    if EncryptedStream.Size > 0 then
      sha256.Update(EncryptedStream.Memory^, EncryptedStream.Size);
    sha256.Final(ExpectedHMAC[0]);

    // constant-time compare HMACs
    for i := 0 to High(HMAC) do
      if HMAC[i] <> ExpectedHMAC[i] then
        Exit(string.Empty); // password incorrect or data corrupt

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
      DecompressedStream := DecompressMemoryStream(OutputStream);
      try
        SetLength(OutBytes, DecompressedStream.Size);
        DecompressedStream.Position := 0;
        DecompressedStream.ReadBuffer(OutBytes[1], DecompressedStream.Size);
      finally
        DecompressedStream.Free;
      end;

      Result := string(UTF8ToString(OutBytes));
    end
    else
      Result := string.Empty;

    // clear sensitive memory
    FillChar(Key, SizeOf(Key), 0);
    FillChar(ExpectedHMAC, SizeOf(ExpectedHMAC), 0);
  finally
    InputStream.Free;
    EncryptedStream.Free;
    OutputStream.Free;
    Cipher.Free;
    sha256.Free;
  end;
end;

function GetHash(const Token: string): TBytes;
var
  Hash: TDCP_sha256;
  TempHash: array[0..31] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
begin
  Result := nil;
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Token);       // feed the Token string
    Hash.Final(TempHash[0]);        // get the 32-byte hash
    SetLength(Result, 32);
    Move(TempHash[0], Result[0], 32);
    // clear temporary memory
    FillChar(TempHash, SizeOf(TempHash), 0);
  finally
    Hash.Free;
  end;
end;

function CouldBeEncryptedFile(const FileName: string): boolean;
var
  FS: TFileStream;
  Encoded: ansistring;
  Decoded: rawbytestring;
  Magic: array[0..3] of byte = (0, 0, 0, 0);
begin
  Result := False;
  if not FileExists(FileName) then Exit;
  Encoded := string.Empty;

  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Encoded, FS.Size);
    FS.ReadBuffer(Encoded[1], FS.Size);
  finally
    FS.Free;
  end;

  try
    Decoded := DecodeStringBase64(Encoded);
    if Length(Decoded) < Length(FILE_MAGIC) then Exit;
    Move(Decoded[1], Magic[0], Length(Magic));
    Result := CompareMem(@Magic[0], @FILE_MAGIC[0], SizeOf(FILE_MAGIC));
  except
    Result := False;
  end;
end;

function LoadFileAsString(const FileName: string): rawbytestring;
var
  FS: TFileStream;
begin
  Result := '';
  if not FileExists(FileName) then Exit;
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, FS.Size);
    if FS.Size > 0 then
      FS.ReadBuffer(Result[1], FS.Size);
  finally
    FS.Free;
  end;
end;

end.
