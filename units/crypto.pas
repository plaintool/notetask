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
  DCPrijndael,
  DCPsha256;

/// Compresses InputStream and returns a memory stream with original size prepended.
function CompressMemoryStream(InputStream: TMemoryStream): TMemoryStream;

/// Decompresses InputStream using stored original size and returns resulting memory stream.
function DecompressMemoryStream(InputStream: TMemoryStream): TMemoryStream;

/// Encrypts PlainData using Hash and returns combined TBytes: IV(16) || HMAC(32) || CipherText.
function EncryptData(const PlainData: TBytes; const Hash: TBytes): TBytes;

/// Decrypts CipherData using Hash and returns decrypted TBytes, empty on error.
function DecryptData(const CipherData: TBytes; const Hash: TBytes): TBytes;

/// Generates a hash as TBytes from given Token string.
function GetHash(const Token: string): TBytes;

/// Checks if FileName could be encrypted with our format (IV + HMAC + CipherText) without password.
function CouldBeEncryptedFile(const FileName: string): boolean;

/// Loads the entire file as raw bytes string.
function LoadFileAsString(const FileName: string): rawbytestring;

/// Loads the entire file as TBytes array.
function LoadFileAsBytes(const FileName: string): TBytes;

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

function EncryptData(const PlainData: TBytes; const Hash: TBytes): TBytes;
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
  i: integer;
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
    Randomize;
    for i := 0 to High(IV) do
      IV[i] := Random(256);

    // derive key directly from precomputed hash (first 32 bytes for AES-256)
    FillChar(Key, SizeOf(Key), 0);
    if Length(Hash) >= 32 then
      Move(Hash[0], Key[0], SizeOf(Key));

    // compress before encryption
    CompressedStream := CompressMemoryStream(InputStream);
    try
      CompressedStream.Position := 0;

      // encrypt
      Cipher.Init(Key[0], 256, @IV[0]);
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
      Sha256.Update(OutputStream.Memory^, OutputStream.Size);
    Sha256.Final(HMAC[0]);

    // allocate container: MAGIC + IV + HMAC + CipherText
    SetLength(Container, Length(FILE_MAGIC) + Length(IV) + Length(HMAC) + OutputStream.Size);

    // copy parts into container
    if Length(FILE_MAGIC) > 0 then
      Move(FILE_MAGIC[0], Container[0], Length(FILE_MAGIC));
    Move(IV[0], Container[Length(FILE_MAGIC)], Length(IV));
    Move(HMAC[0], Container[Length(FILE_MAGIC) + Length(IV)], Length(HMAC));
    if OutputStream.Size > 0 then
      Move(OutputStream.Memory^,
        Container[Length(FILE_MAGIC) + Length(IV) + Length(HMAC)],
        OutputStream.Size);

    Result := Container;

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

function DecryptData(const CipherData: TBytes; const Hash: TBytes): TBytes;
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
  EncryptedSize: integer;
  OutBytes: TBytes = nil;
  i: integer;
begin
  Result := nil;
  if Length(CipherData) = 0 then Exit;

  // basic length check (IV + HMAC)
  if Length(CipherData) < (SizeOf(IV) + SizeOf(HMAC) + SizeOf(Magic)) then Exit;

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
    InputStream.ReadBuffer(IV[0], SizeOf(IV));
    InputStream.ReadBuffer(HMAC[0], SizeOf(HMAC));

    // remaining is encrypted data
    EncryptedSize := InputStream.Size - (SizeOf(Magic) + SizeOf(IV) + SizeOf(HMAC));
    if EncryptedSize < 0 then Exit;

    EncryptedStream.CopyFrom(InputStream, EncryptedSize);
    EncryptedStream.Position := 0;

    // derive key
    if Length(Hash) >= SizeOf(Key) then
      Move(Hash[0], Key[0], SizeOf(Key))
    else
      FillChar(Key, SizeOf(Key), 0);

    // compute expected HMAC
    sha256.Init;
    sha256.Update(Key[0], SizeOf(Key));
    if EncryptedStream.Size > 0 then
      sha256.Update(EncryptedStream.Memory^, EncryptedStream.Size);
    sha256.Final(ExpectedHMAC[0]);

    // verify HMAC
    for i := 0 to High(HMAC) do
      if HMAC[i] <> ExpectedHMAC[i] then Exit;

    // decrypt
    EncryptedStream.Position := 0;
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

function LoadFileAsString(const FileName: string): rawbytestring;
var
  FS: TFileStream;
begin
  Result := string.Empty;
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
