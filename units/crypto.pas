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
  {$IFDEF MSWINDOWS}
  JwaWinCrypt,
  {$ELSE}
  Unix,
  {$ENDIF}
  PasZLib,
  DCPcrypt2,
  DCPrijndael,
  DCPsha256;

/// Compresses InputStream and returns a memory stream with original size prepended.
function CompressMemoryStream(InputStream: TMemoryStream): TMemoryStream;

/// Decompresses InputStream using stored original size and returns resulting memory stream.
function DecompressMemoryStream(InputStream: TMemoryStream): TMemoryStream;

/// Generate cryptographically secure random bytes.
function GetRandomBytes(len: integer): TBytes;

/// Encrypts PlainData using Hash and returns combined TBytes: IV(16) || HMAC(32) || CipherText.
function EncryptData(const PlainData: TBytes; const Hash, Salt: TBytes): TBytes;

/// Decrypts CipherData using Hash and returns decrypted TBytes, empty on error.
function DecryptData(const CipherData: TBytes; const Token: string; out Salt: TBytes): TBytes;

/// GetHash replacement that uses PBKDF2-HMAC-SHA256.
/// - Token: user password (string). It will be treated as UTF-8 bytes.
/// - Salt: if not assigned, will be generated (16 bytes) using GetRandomBytes.
/// Returns 32-byte derived key.
function GetHash(const Token: string; var Salt: TBytes): TBytes;

/// Checks if FileName could be encrypted with our format (IV + HMAC + CipherText) without password.
function CheckEncryptedFile(const FileName: string): boolean;

/// Loads the entire file as TBytes array.
function LoadFileAsBytes(const FileName: string): TBytes;

/// Convert TBytes to array of byte given length
procedure BytesToArray(const Data: TBytes; const len: integer; var ResArray: array of byte);

/// Constant-time comparison
function ConstantTimeCompare(const A, B: TBytes): boolean;

// Gentle cleaning of TBytes
procedure FreeBytesSecure(var Bytes: TBytes);

// Gentle cleaning of string
procedure ClearStringSecure(var S: string);

/// HMAC-SHA256 using TDCP_sha256 from DCPcrypt.
/// Input: Key (any length), Data (any length)
/// Output: 32-byte MAC
function HMAC_SHA256(const Key, Data: TBytes): TBytes;

/// PBKDF2-HMAC-SHA256 implementation (RFC 2898-like).
/// Password: arbitrary bytes (we will pass UTF-8 bytes of Token)
/// Salt: arbitrary bytes
/// Iterations: number of iterations (>= 1)
/// DKLen: desired output length in bytes
function PBKDF2_HMAC_SHA256(const Password, Salt: TBytes; Iterations, DKLen: integer): TBytes;

implementation

const
  FILE_MAGIC: array[0..3] of byte = (78, 84, 83, 75); // 'NTSK'

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
    DestLen := SourceLen + ((SourceLen + 7) shr 3) + ((SourceLen + 63) shr 6) + 11; // formula from zlib to estimate max compressed size
    Result.SetSize(4 + DestLen);

    MemPtr := Result.Memory;
    Dest := MemPtr;
    Inc(pbyte(Dest), 4); // move pointer 4 bytes forward for compressed data

    if compress(Dest, DestLen, Source, SourceLen) <> Z_OK then
      raise Exception.Create('Operation failed');

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
      raise Exception.Create('Operation failed');

    Result.Position := 0;
  except
    Result.Free;
    raise;
  end;
end;

function EncryptData(const PlainData: TBytes; const Hash, Salt: TBytes): TBytes;
var
  Cipher: TDCP_rijndael;
  Sha256: TDCP_sha256;
  InputStream, CompressedStream, OutputStream: TMemoryStream;
  // AES-256 key
  Key: array[0..31] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  // Initialization Vector
  IV: array[0..15] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  // SHA-256 result
  HMAC: array[0..31] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  Container: TBytes = nil;
  DataToAuthenticate: TBytes = nil;
begin
  Result := nil;

  InputStream := TMemoryStream.Create;
  OutputStream := TMemoryStream.Create;
  Cipher := TDCP_rijndael.Create(nil);
  Sha256 := TDCP_sha256.Create(nil);
  try
    // write input data to stream
    if Length(PlainData) > 0 then
      InputStream.WriteBuffer(PlainData[0], Length(PlainData));

    // generate random IV
    BytesToArray(GetRandomBytes(16), 16, IV);

    // derive key directly from precomputed hash (first 32 bytes for AES-256)
    FillChar(Key, SizeOf(Key), 0);
    if Length(Hash) >= 32 then
      Move(Hash[0], Key[0], SizeOf(Key));

    // compress before encryption
    CompressedStream := CompressMemoryStream(InputStream);
    try
      CompressedStream.Position := 0;

      // encrypt
      Cipher.CipherMode := cmCBC;
      Cipher.Init(Key[0], 256, @IV[0]);
      try
        Cipher.EncryptStream(CompressedStream, OutputStream, CompressedStream.Size);
      finally
        Cipher.Burn;
      end;
    finally
      CompressedStream.Free;
    end;

    // compute data to authenticate
    SetLength(DataToAuthenticate, Length(Salt) + Length(IV) + OutputStream.Size);
    Move(Salt[0], DataToAuthenticate[0], Length(Salt));
    Move(IV[0], DataToAuthenticate[Length(Salt)], Length(IV));
    if OutputStream.Size > 0 then
      Move(OutputStream.Memory^, DataToAuthenticate[Length(Salt) + Length(IV)], OutputStream.Size);
    // compute HMAC
    BytesToArray(HMAC_SHA256(Key, DataToAuthenticate), 32, HMAC);
    FreeBytesSecure(DataToAuthenticate);

    // allocate container: MAGIC + SALT + IV + HMAC + CipherText
    SetLength(Container, Length(FILE_MAGIC) + Length(Salt) + Length(IV) + Length(HMAC) + OutputStream.Size);

    // copy parts into container
    if Length(FILE_MAGIC) > 0 then
      Move(FILE_MAGIC[0], Container[0], Length(FILE_MAGIC));

    // write SALT after MAGIC
    if Length(Salt) > 0 then
      Move(Salt[0], Container[Length(FILE_MAGIC)], Length(Salt));

    // write IV after SALT
    Move(IV[0], Container[Length(FILE_MAGIC) + Length(Salt)], Length(IV));

    // write HMAC after IV
    Move(HMAC[0], Container[Length(FILE_MAGIC) + Length(Salt) + Length(IV)], Length(HMAC));

    // write ciphertext after HMAC
    if OutputStream.Size > 0 then
      Move(OutputStream.Memory^,
        Container[Length(FILE_MAGIC) + Length(Salt) + Length(IV) + Length(HMAC)],
        OutputStream.Size);

    Result := Container;
  finally
    // clear sensitive memory
    FillChar(Key, SizeOf(Key), 0);
    FillChar(HMAC, SizeOf(HMAC), 0);

    InputStream.Free;
    OutputStream.Free;
    Cipher.Free;
    Sha256.Free;
  end;
end;

function DecryptData(const CipherData: TBytes; const Token: string; out Salt: TBytes): TBytes;
var
  InputStream, EncryptedStream, OutputStream, DecompressedStream: TMemoryStream;
  Cipher: TDCP_rijndael;
  sha256: TDCP_sha256;
  // AES-256 key
  Key: array[0..31] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  // Initialization Vector
  IV: array[0..15] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  // SHA-256 HMAC
  HMAC: array[0..31] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  ExpectedHMAC: array[0..31] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  Magic: array[0..3] of byte = (0, 0, 0, 0);
  Hash: TBytes;
  EncryptedSize: integer;
  OutBytes: TBytes = nil;
  DataToVerify: TBytes = nil;
begin
  Result := nil;
  if Length(CipherData) = 0 then Exit;

  // initialize salt
  Salt := nil;
  SetLength(Salt, 16);

  // basic length check (IV + HMAC)
  if Length(CipherData) < (SizeOf(Magic) + Length(Salt) + SizeOf(IV) + SizeOf(HMAC)) then Exit;

  InputStream := TMemoryStream.Create;
  EncryptedStream := TMemoryStream.Create;
  OutputStream := TMemoryStream.Create;
  Cipher := TDCP_rijndael.Create(nil);
  sha256 := TDCP_sha256.Create(nil);
  try
    InputStream.WriteBuffer(CipherData[0], Length(CipherData));
    InputStream.Position := 0;

    // read and check MAGIC
    InputStream.ReadBuffer(Magic[0], SizeOf(Magic));
    if not CompareMem(@Magic[0], @FILE_MAGIC[0], SizeOf(FILE_MAGIC)) then Exit;

    // read IV and HMAC
    InputStream.ReadBuffer(Salt[0], Length(Salt));
    Hash := GetHash(Token, Salt);

    InputStream.ReadBuffer(IV[0], SizeOf(IV));
    InputStream.ReadBuffer(HMAC[0], SizeOf(HMAC));

    // remaining is encrypted data
    EncryptedSize := InputStream.Size - (SizeOf(Magic) + Length(Salt) + SizeOf(IV) + SizeOf(HMAC));
    if EncryptedSize < 0 then Exit;

    EncryptedStream.CopyFrom(InputStream, EncryptedSize);
    EncryptedStream.Position := 0;

    // derive key
    if Length(Hash) >= SizeOf(Key) then
      Move(Hash[0], Key[0], SizeOf(Key))
    else
      Exit;

    // compute data to verify
    SetLength(DataToVerify, Length(Salt) + SizeOf(IV) + EncryptedStream.Size);
    Move(Salt[0], DataToVerify[0], Length(Salt));
    Move(IV[0], DataToVerify[Length(Salt)], SizeOf(IV));
    if EncryptedStream.Size > 0 then
      Move(EncryptedStream.Memory^, DataToVerify[Length(Salt) + SizeOf(IV)], EncryptedStream.Size);
    // compute expected HMAC
    BytesToArray(HMAC_SHA256(Key, DataToVerify), 32, ExpectedHMAC);
    FillChar(DataToVerify[0], Length(DataToVerify), 0);

    // verify HMAC using constant-time comparison
    if not ConstantTimeCompare(TBytes(HMAC), TBytes(ExpectedHMAC)) then
      Exit;

    // decrypt
    EncryptedStream.Position := 0;
    Cipher.CipherMode := cmCBC;
    Cipher.Init(Key[0], 256, @IV[0]);
    try
      Cipher.DecryptStream(EncryptedStream, OutputStream, EncryptedSize);
    finally
      Cipher.Burn;
    end;

    // decompress
    if OutputStream.Size > 0 then
    begin
      DecompressedStream := DecompressMemoryStream(OutputStream);
      try
        SetLength(OutBytes, DecompressedStream.Size);
        DecompressedStream.Position := 0;
        DecompressedStream.ReadBuffer(OutBytes[0], DecompressedStream.Size);
      finally
        DecompressedStream.Free;
      end;

      Result := Outbytes;
    end;
  finally
    // clear sensitive memory
    FillChar(Key, SizeOf(Key), 0);
    FillChar(ExpectedHMAC, SizeOf(ExpectedHMAC), 0);
    FreeBytesSecure(Hash);
    FreeBytesSecure(DataToVerify);

    InputStream.Free;
    EncryptedStream.Free;
    OutputStream.Free;
    Cipher.Free;
    sha256.Free;
  end;
end;

function GetRandomBytes(len: integer): TBytes;
  {$IFDEF UNIX}
var
  stream: TFileStream;
  {$ELSE}
var
  hProv: QWord = 0;
  {$ENDIF}
begin
  Result := nil;
  SetLength(Result, len);
  {$IFDEF UNIX}
    stream := TFileStream.Create('/dev/urandom', fmOpenRead);
  try
    if stream.Read(Result[0], len) <> len then
      raise Exception.Create('Failed to read enough bytes from /dev/urandom');
  finally
    stream.Free;
  end;
  {$ELSE}
  if not CryptAcquireContext(hProv, nil, nil, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT) then
    raise Exception.Create('CryptAcquireContext failed');
  try
    if not CryptGenRandom(hProv, len, @Result[0]) then
      raise Exception.Create('CryptGenRandom failed');
  finally
    CryptReleaseContext(hProv, 0);
  end;
  {$ENDIF}
end;

function GetHash(const Token: string; var Salt: TBytes): TBytes;
var
  PasswordBytes: TBytes = nil;
  Iterations: integer;
  DKLen: integer;
  Derived: TBytes;
  TempToken: string;
begin
  Result := nil;

  // Parameters: you may tune iterations for your environment.
  // For desktop/bulk use: 100000 iterations is a reasonable starting point.
  Iterations := 50000;
  DKLen := 32; // want 32 bytes (256 bits) output

  // Ensure salt exists (16 bytes)
  if (not Assigned(Salt)) or (Length(Salt) <> 16) then
  begin
    Salt := GetRandomBytes(16);
  end;

  // Convert Token (Unicode) to UTF-8 bytes
  TempToken := UTF8Encode(Token);
  try
    SetLength(PasswordBytes, Length(TempToken));
    if Length(PasswordBytes) > 0 then
      Move(TempToken[1], PasswordBytes[0], Length(PasswordBytes));
  finally
    ClearStringSecure(TempToken);
  end;

  // Derive key with PBKDF2-HMAC-SHA256
  Derived := PBKDF2_HMAC_SHA256(PasswordBytes, Salt, Iterations, DKLen);

  // Return derived key
  SetLength(Result, DKLen);
  if Length(Derived) >= DKLen then
    Move(Derived[0], Result[0], DKLen)
  else
    FillChar(Result[0], DKLen, 0);

  // clear sensitive memory
  if (Assigned(PasswordBytes)) and (Length(PasswordBytes) > 0) then
    FreeBytesSecure(PasswordBytes);
  FreeBytesSecure(Derived);
end;

function CheckEncryptedFile(const FileName: string): boolean;
var
  FS: TFileStream;
  Magic: array[0..3] of byte = (0, 0, 0, 0);
begin
  Result := False;
  if not FileExists(FileName) then Exit;

  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    if FS.Size < SizeOf(Magic) then Exit;

    FS.ReadBuffer(Magic, SizeOf(Magic));

    Result := CompareMem(@Magic[0], @FILE_MAGIC[0], SizeOf(Magic));
  finally
    FS.Free;
  end;
end;

function LoadFileAsBytes(const FileName: string): TBytes;
var
  FS: TFileStream;
begin
  Result := nil;
  SetLength(Result, 0);
  if not FileExists(FileName) then Exit;

  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, FS.Size);
    if FS.Size > 0 then
      FS.ReadBuffer(Result[0], FS.Size);
  finally
    FS.Free;
  end;
end;

procedure BytesToArray(const Data: TBytes; const len: integer; var ResArray: array of byte);
var
  CopyLength: integer;
  ResArrayLen: integer;
  SafeLen: integer;
begin
  ResArrayLen := Length(ResArray);

  // Automatically limit the length to the size of the target array
  SafeLen := len;
  if SafeLen > ResArrayLen then
    SafeLen := ResArrayLen;

  CopyLength := Length(Data);
  if CopyLength > SafeLen then
    CopyLength := SafeLen;

  if CopyLength > 0 then
    Move(Data[0], ResArray[0], CopyLength);

  if SafeLen > CopyLength then
    FillChar(ResArray[CopyLength], SafeLen - CopyLength, 0);
end;

function ConstantTimeCompare(const A, B: TBytes): boolean;
var
  i: integer;
  diff: byte;
begin
  if Length(A) <> Length(B) then
    Exit(False);

  diff := 0;
  for i := 0 to Length(A) - 1 do
    diff := diff or (A[i] xor B[i]);
  Result := diff = 0;
end;

procedure FreeBytesSecure(var Bytes: TBytes);
begin
  if Length(Bytes) > 0 then
    FillChar(Bytes[0], Length(Bytes), 0);
  SetLength(Bytes, 0);
end;

procedure ClearStringSecure(var S: string);
begin
  if S <> string.Empty then
  begin
    FillChar(S[1], Length(S) * SizeOf(char), 0);
    S := string.Empty;
  end;
end;

function HMAC_SHA256(const Key, Data: TBytes): TBytes;
var
  Sha: TDCP_sha256;
  BlockKey: array[0..63] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  // block size for SHA-256 is 64
  IKeyPad: array[0..63] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  OKeyPad: array[0..63] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  TempHash: array[0..31] of byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  i: integer;
  KeyLen: integer = 0;
  TempData: TBytes = nil;
begin
  // Prepare result length
  Result := nil;
  SetLength(Result, 32);
  // initialise
  FillChar(BlockKey, SizeOf(BlockKey), 0);
  FillChar(IKeyPad, SizeOf(IKeyPad), 0);
  FillChar(OKeyPad, SizeOf(OKeyPad), 0);
  FillChar(TempHash, SizeOf(TempHash), 0);

  try
    KeyLen := Length(Key);
    if KeyLen > 64 then
    begin
      // If key > block size, key = SHA256(key)
      Sha := TDCP_sha256.Create(nil);
      try
        Sha.Init;
        if KeyLen > 0 then
          Sha.Update(Key[0], KeyLen);
        Sha.Final(TempHash[0]);
        Move(TempHash[0], BlockKey[0], 32);
        // rest already zero
      finally
        Sha.Free;
      end;
    end
    else
    begin
      if KeyLen > 0 then
        Move(Key[0], BlockKey[0], KeyLen);
    end;

    // ipad = 0x36, opad = 0x5c
    for i := 0 to 63 do
    begin
      IKeyPad[i] := BlockKey[i] xor $36;
      OKeyPad[i] := BlockKey[i] xor $5C;
    end;

    // inner hash: H(i_key_pad || data)
    Sha := TDCP_sha256.Create(nil);
    try
      Sha.Init;
      Sha.Update(IKeyPad[0], SizeOf(IKeyPad));
      if Length(Data) > 0 then
        Sha.Update(Data[0], Length(Data));
      Sha.Final(TempHash[0]);
    finally
      Sha.Free;
    end;

    // outer hash: H(o_key_pad || inner_hash)
    Sha := TDCP_sha256.Create(nil);
    try
      Sha.Init;
      Sha.Update(OKeyPad[0], SizeOf(OKeyPad));
      Sha.Update(TempHash[0], SizeOf(TempHash));
      Sha.Final(TempHash[0]);
      Move(TempHash[0], Result[0], 32);
    finally
      Sha.Free;
    end;

  finally
    // clear sensitive local buffers
    FillChar(BlockKey, SizeOf(BlockKey), 0);
    FillChar(IKeyPad, SizeOf(IKeyPad), 0);
    FillChar(OKeyPad, SizeOf(OKeyPad), 0);
    FillChar(TempHash, SizeOf(TempHash), 0);
    FreeBytesSecure(TempData);
  end;
end;

function PBKDF2_HMAC_SHA256(const Password, Salt: TBytes; Iterations, DKLen: integer): TBytes;
var
  HLen: integer;
  L, i, j, k: integer;
  TBlk: TBytes = nil;
  U: TBytes = nil;
  IntBlock: array[0..3] of byte;
  BlockIndex: cardinal;
  TmpKey: TBytes = nil;
  Accumulator: TBytes = nil;
  OutPos: integer;
begin
  Result := nil;
  try
    // HLen for SHA-256 is 32
    HLen := 32;
    if (Iterations < 1) or (DKLen <= 0) then
      raise Exception.Create('Invalid PBKDF2 parameters');

    // number of blocks needed
    L := (DKLen + HLen - 1) div HLen;
    SetLength(Result, DKLen);
    SetLength(TBlk, HLen);
    SetLength(U, HLen);
    SetLength(Accumulator, HLen);
    SetLength(TmpKey, 0);

    OutPos := 0;

    for i := 1 to L do
    begin
      // INT_32_BE(i)
      BlockIndex := cardinal(i);
      IntBlock[0] := byte((BlockIndex shr 24) and $FF);
      IntBlock[1] := byte((BlockIndex shr 16) and $FF);
      IntBlock[2] := byte((BlockIndex shr 8) and $FF);
      IntBlock[3] := byte(BlockIndex and $FF);

      // U1 = HMAC(Password, Salt || INT(i))
      SetLength(TmpKey, Length(Salt) + 4);
      if Length(Salt) > 0 then
        Move(Salt[0], TmpKey[0], Length(Salt));
      Move(IntBlock[0], TmpKey[Length(Salt)], 4);

      Accumulator := HMAC_SHA256(Password, TmpKey);  // U1 = HMAC(Password, Salt || INT(i))
      U := Accumulator;

      // iterate U2..Uc
      for j := 2 to Iterations do
      begin
        U := HMAC_SHA256(Password, U);  // Uj = HMAC(Password, Uj-1)
        for k := 0 to HLen - 1 do
          Accumulator[k] := Accumulator[k] xor U[k];
      end;

      // append T to output
      for k := 0 to HLen - 1 do
      begin
        if OutPos < DKLen then
        begin
          Result[OutPos] := Accumulator[k];
          Inc(OutPos);
        end
        else
          Break;
      end;

      // clear accumulator and tmp buffers for next block
      FillChar(Accumulator[0], Length(Accumulator), 0);
      FillChar(U[0], Length(U), 0);
      SetLength(TmpKey, 0);
    end;
  finally
    // clear locals
    FreeBytesSecure(TBlk);
    FreeBytesSecure(U);
    FreeBytesSecure(Accumulator);
    FreeBytesSecure(TmpKey);
  end;
end;

end.
