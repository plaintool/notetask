//-----------------------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the GNU General Public License, Version 3 (GPL-3.0)
//  You may obtain a copy of the License at https://www.gnu.org/licenses/gpl-3.0.html
//-----------------------------------------------------------------------------------

unit filemanager;

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  {$IFDEF UNIX}
  BaseUnix,
  Unix,
  DateUtils,
  {$ELSE}
  Windows,
  {$ENDIF}
  lineending,
  crypto;

function FindPowerShellCore: string;

procedure UpdateFileReadAccess(const FileName: string);

function GetEncodingName(Encoding: TEncoding): string;

function IsUserEncoding(Enc: TEncoding): boolean;

function IsBOMEncoding(Encoding: TEncoding): boolean;

function IsValidUTF8(var Buffer: TBytes; BytesRead: integer): boolean;

function IsValidAscii(var Buffer: TBytes; BytesRead: integer): boolean;

function IsValidAnsi(var Buffer: TBytes; BytesRead: integer): boolean;

function DetectEncoding(const Buffer: TBytes): TEncoding; overload;

function DetectEncoding(const FileName: string): TEncoding; overload;

function DetectLineEnding(const Buffer: TBytes; Encoding: TEncoding; MaxLines: integer = 100): TLineEnding; overload;

function DetectLineEnding(const FileName: string; Encoding: TEncoding; MaxLines: integer = 100): TLineEnding; overload;

function EndsWithLineBreak(const Buffer: TBytes): boolean; overload;

function EndsWithLineBreak(const FileName: string): boolean; overload;

function TryLockFile(const AFileName: string; out Stream: TFileStream): boolean;

procedure ReadTextFile(const Bytes: TBytes; out Content: string; out FileEncoding: TEncoding; out LineEnding: TLineEnding;
  out LineCount: integer); overload;

procedure ReadTextFile(const FileName: string; out Content: string; out FileEncoding: TEncoding;
  out LineEnding: TLineEnding; out LineCount: integer); overload;

procedure SaveTextFile(const FileName: string; StringList: TStringList; FileEncoding: TEncoding; LineEnding: TLineEnding;
  Encrypt: boolean; Token: string; var Salt, KeyEnc, KeyAuth: TBytes); overload;

procedure SaveTextFile(const FileName: string; StringList: TStringList; FileEncoding: TEncoding; LineEnding: TLineEnding); overload;

var
  UTF8BOMEncoding: TEncoding;
  UTF16LEBOMEncoding, UTF16BEBOMEncoding: TEncoding;

implementation

function FindPowerShellCore: string;
var
  SearchPaths: array of string;
  PathEnv, PathPart, TrimmedPath: string;
  I: integer;
  Paths: array of string;
begin
  Result := string.Empty;

  // Common install locations for PowerShell 7 and 6
  SearchPaths := ['C:\Program Files\PowerShell\6\pwsh.exe', 'C:\Program Files\PowerShell\7\pwsh.exe',
    'C:\Program Files\PowerShell\8\pwsh.exe', 'C:\Program Files\PowerShell\9\pwsh.exe', 'C:\Program Files\PowerShell\10\pwsh.exe'];

  // Check known fixed locations first
  for I := Low(SearchPaths) to High(SearchPaths) do
    if FileExists(SearchPaths[I]) then
      Exit(SearchPaths[I]);

  // Check all folders in PATH environment variable
  PathEnv := SysUtils.GetEnvironmentVariable('PATH');
  Paths := SplitString(PathEnv, ';');

  for I := 0 to Length(Paths) - 1 do
  begin
    TrimmedPath := Trim(Paths[I]);
    if TrimmedPath <> '' then
    begin
      PathPart := IncludeTrailingPathDelimiter(TrimmedPath) + 'pwsh.exe';
      if FileExists(PathPart) then
        Exit(PathPart);
    end;
  end;
end;

procedure UpdateFileReadAccess(const FileName: string);
var
  {$IFDEF UNIX}
  t: utimbuf;
  {$ELSE}
  h: THandle;
  ft: TFileTime;
  {$ENDIF}
begin
  {$IFDEF UNIX}
  // Convert local time to UTC and update only access time (atime) on UNIX
  t.actime := DateTimeToUnix(Now, False);
  // Keep the modification time (mtime) unchanged
  t.modtime := FileAge(FileName);
  // Apply the updated times to the file
  fpUTime(FileName, @t);
  {$ELSE}
  // Zero initialize FILETIME
  ft.dwLowDateTime := 0;
  ft.dwHighDateTime := 0;

  // Open the file handle for writing to update LastAccessTime
  h := CreateFile(PChar(FileName), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if h <> INVALID_HANDLE_VALUE then
  begin
    try
      // Get the current system time as FILETIME
      GetSystemTimeAsFileTime(ft);
      // Set only the LastAccessTime of the file
      SetFileTime(h, nil, @ft, nil);
    finally
      // Close the file handle
      CloseHandle(h);
    end;
  end;
  {$ENDIF}
end;

function GetEncodingName(Encoding: TEncoding): string;
begin
  if Encoding = TEncoding.UTF8 then
    Result := 'UTF-8'
  else if Encoding = TEncoding.Unicode then
    Result := 'UTF-16 LE'
  else if Encoding = TEncoding.BigEndianUnicode then
    Result := 'UTF-16 BE'
  else if (Encoding.CodePage = 65001) then // Encoding.CodePage = 65001
    Result := 'UTF-8 BOM'
  else if (Encoding.CodePage = 1200) then // Encoding.CodePage = 1200
    Result := 'UTF-16 LE BOM'
  else if Encoding.CodePage = 1201 then // Encoding.CodePage = 1201
    Result := 'UTF-16 BE BOM'
  else if Encoding = TEncoding.ANSI then
    Result := 'ANSI'
  else if Encoding = TEncoding.ASCII then
    Result := 'ASCII'
  else if Encoding = TEncoding.UTF7 then
    Result := 'UTF-7'
  else if Encoding = TEncoding.Default then
    Result := 'Default'
  else
    Result := 'Unknown';
end;

function IsUserEncoding(Enc: TEncoding): boolean;
begin
  Result := Assigned(Enc) and not TEncoding.IsStandardEncoding(Enc);
end;

function IsBOMEncoding(Encoding: TEncoding): boolean;
begin
  // Assume false by default
  Result := False;

  if Encoding = TEncoding.UTF8 then
    exit(False)
  else if Encoding = TEncoding.Unicode then
    exit(False)
  else if Encoding = TEncoding.BigEndianUnicode then
    exit(False)
  else if Encoding.CodePage = 65001 then // UTF-8 с BOM
    exit(True)
  else if Encoding.CodePage = 1200 then // UTF-16 LE с BOM
    exit(True)
  else if Encoding.CodePage = 1201 then // UTF-16 BE с BOM
    exit(True);
end;

function IsValidUTF8(var Buffer: TBytes; BytesRead: integer): boolean;
var
  i: integer;
  remaining: integer;
  codePoint: longword;
  minCode: longword;
begin
  Result := True;
  remaining := 0;
  codePoint := 0;
  minCode := 0;

  // If buffer is empty, it's technically valid UTF-8
  if BytesRead <= 0 then
    Exit;

  for i := 0 to BytesRead - 1 do
  begin
    if remaining = 0 then
    begin
      // Handle new character sequence
      if Buffer[i] <= $7F then
      begin
        // Valid ASCII character (0xxxxxxx) - always valid in UTF-8
        continue;
      end
      else if (Buffer[i] >= $C2) and (Buffer[i] <= $DF) then
      begin
        // 2-byte sequence (110xxxxx) - NOTE: Starts from $C2, not $C0
        // $C0 and $C1 would create overlong encodings for ASCII chars
        remaining := 1;
        codePoint := Buffer[i] and $1F; // Extract 5 bits
        minCode := $80; // Minimum code point for 2-byte sequence
      end
      else if (Buffer[i] >= $E0) and (Buffer[i] <= $EF) then
      begin
        // 3-byte sequence (1110xxxx)
        remaining := 2;
        codePoint := Buffer[i] and $0F; // Extract 4 bits
        minCode := $800; // Minimum code point for 3-byte sequence
      end
      else if (Buffer[i] >= $F0) and (Buffer[i] <= $F4) then
      begin
        // 4-byte sequence (11110xxx) - NOTE: Only up to $F4 (Unicode max is U+10FFFF)
        remaining := 3;
        codePoint := Buffer[i] and $07; // Extract 3 bits
        minCode := $10000; // Minimum code point for 4-byte sequence
      end
      else
      begin
        // Invalid starting byte:
        // - $C0, $C1: Overlong encoding (should use 1 byte for ASCII)
        // - $F5-$FF: Beyond Unicode maximum (U+10FFFF)
        // - $80-$BF: Continuation bytes without leading byte
        Result := False;
        Exit;
      end;
    end
    else
    begin
      // Handle continuation byte (must be 10xxxxxx)
      if (Buffer[i] < $80) or (Buffer[i] > $BF) then
      begin
        // Invalid continuation byte
        Result := False;
        Exit;
      end;

      // Add 6 bits to the code point
      codePoint := (codePoint shl 6) or (Buffer[i] and $3F);
      Dec(remaining);

      // If sequence is complete, validate the code point
      if remaining = 0 then
      begin
        // Check for overlong encoding (using more bytes than necessary)
        if codePoint < minCode then
        begin
          Result := False;
          Exit;
        end;

        // Check for 3-byte sequences that could represent surrogates
        if minCode = $800 then
        begin
          // UTF-8 should not encode surrogate pairs (U+D800 to U+DFFF)
          // These are reserved for UTF-16 encoding
          if (codePoint >= $D800) and (codePoint <= $DFFF) then
          begin
            Result := False;
            Exit;
          end;
        end
        // Check for 4-byte sequences beyond Unicode maximum
        else if minCode = $10000 then
        begin
          // Unicode maximum is U+10FFFF
          if codePoint > $10FFFF then
          begin
            Result := False;
            Exit;
          end;
        end;

        // Additional validation for specific starting bytes
        if (Buffer[i - remaining - 1] = $E0) and (codePoint < $800) then
        begin
          // Overlong encoding for 3-byte sequence starting with $E0
          Result := False;
          Exit;
        end
        else if (Buffer[i - remaining - 1] = $F0) and (codePoint < $10000) then
        begin
          // Overlong encoding for 4-byte sequence starting with $F0
          Result := False;
          Exit;
        end;
      end;
    end;
  end;

  // Check for incomplete multi-byte sequence at the end of buffer
  if remaining > 0 then
    Result := False;
end;

function IsValidAscii(var Buffer: TBytes; BytesRead: integer): boolean;
var
  i: integer;
begin
  Result := True; // Assume valid ASCII

  for i := 0 to BytesRead - 1 do
  begin
    if Buffer[i] > $7F then
    begin
      Result := False; // Invalid ASCII character found
      Exit;
    end;
  end;
end;

function IsValidAnsi(var Buffer: TBytes; BytesRead: integer): boolean;
var
  i: integer;
begin
  Result := True;

  // If buffer is empty, consider it valid ANSI (empty text)
  if BytesRead <= 0 then
    Exit;

  i := 0;
  while i < BytesRead do
  begin
    // Check for UTF-8 BOM (EF BB BF)
    if (i + 2 < BytesRead) and (Buffer[i] = $EF) and (Buffer[i + 1] = $BB) and (Buffer[i + 2] = $BF) then
    begin
      Result := False; // UTF-8 BOM found
      Exit;
    end;

    // Check for UTF-16 LE BOM (FF FE)
    if (i + 1 < BytesRead) and (Buffer[i] = $FF) and (Buffer[i + 1] = $FE) then
    begin
      Result := False; // UTF-16 LE BOM found
      Exit;
    end;

    // Check for UTF-16 BE BOM (FE FF)
    if (i + 1 < BytesRead) and (Buffer[i] = $FE) and (Buffer[i + 1] = $FF) then
    begin
      Result := False; // UTF-16 BE BOM found
      Exit;
    end;

    // Check for UTF-8 multi-byte sequences
    if (Buffer[i] and $80) <> 0 then // High bit set - potential multi-byte
    begin
      // 2-byte UTF-8 sequence (110xxxxx 10xxxxxx)
      if (Buffer[i] and $E0) = $C0 then
      begin
        if (i + 1 >= BytesRead) or ((Buffer[i + 1] and $C0) <> $80) then
        begin
          // Invalid UTF-8 continuation byte, but could be valid ANSI
          Inc(i);
          Continue;
        end
        else
        begin
          // Valid UTF-8 2-byte sequence found
          Result := False;
          Exit;
        end;
      end
      // 3-byte UTF-8 sequence (1110xxxx 10xxxxxx 10xxxxxx)
      else if (Buffer[i] and $F0) = $E0 then
      begin
        if (i + 2 >= BytesRead) or ((Buffer[i + 1] and $C0) <> $80) or ((Buffer[i + 2] and $C0) <> $80) then
        begin
          Inc(i);
          Continue;
        end
        else
        begin
          Result := False;
          Exit;
        end;
      end
      // 4-byte UTF-8 sequence (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx)
      else if (Buffer[i] and $F8) = $F0 then
      begin
        if (i + 3 >= BytesRead) or ((Buffer[i + 1] and $C0) <> $80) or ((Buffer[i + 2] and $C0) <> $80) or
          ((Buffer[i + 3] and $C0) <> $80) then
        begin
          Inc(i);
          Continue;
        end
        else
        begin
          Result := False;
          Exit;
        end;
      end
      else
      begin
        // Single byte with high bit set - valid in ANSI
        // Continue checking next bytes
        Inc(i);
        Continue;
      end;
    end
    else
    begin
      // Standard ASCII character (0-127) - always valid in ANSI
      Inc(i);
    end;
  end;
end;

function DetectEncoding(const Buffer: TBytes): TEncoding; overload;
var
  ContentBuffer: TBytes = nil;
  BytesToCheck: integer;
begin
  Result := TEncoding.UTF8;

  if Length(Buffer) = 0 then Exit(TEncoding.UTF8);

  // Check BOM first
  if (Length(Buffer) >= 3) and (Buffer[0] = $EF) and (Buffer[1] = $BB) and (Buffer[2] = $BF) then
    Exit(UTF8BOMEncoding)
  else if (Length(Buffer) >= 2) and (Buffer[0] = $FF) and (Buffer[1] = $FE) then
    Exit(UTF16LEBOMEncoding)
  else if (Length(Buffer) >= 2) and (Buffer[0] = $FE) and (Buffer[1] = $FF) then
    Exit(UTF16BEBOMEncoding);

  // No BOM found, check the first up to 1024*4 bytes
  if Length(Buffer) > 1024 * 4 then
    BytesToCheck := 1024 * 4
  else
    BytesToCheck := Length(Buffer);

  SetLength(ContentBuffer, BytesToCheck);
  Move(Buffer[0], ContentBuffer[0], BytesToCheck);

  // Check content heuristics
  if IsValidUtf8(ContentBuffer, BytesToCheck) then
    Result := TEncoding.UTF8
  else if IsValidAnsi(ContentBuffer, BytesToCheck) then
    Result := TEncoding.ANSI
  else if IsValidAscii(ContentBuffer, BytesToCheck) then
    Result := TEncoding.ASCII
  else
    Result := TEncoding.UTF8;
end;

function DetectEncoding(const FileName: string): TEncoding;
var
  Bytes: TBytes;
begin
  Bytes := LoadFileAsBytes(FileName);
  Result := DetectEncoding(Bytes);
end;

function DetectLineEnding(const Buffer: TBytes; Encoding: TEncoding; MaxLines: integer = 100): TLineEnding; overload;
var
  Text: unicodestring;
  Ch, PrevCh: widechar;
  I, LinesChecked: integer;
  CountCRLF, CountLF, CountCR: integer;
begin
  CountCRLF := 0;
  CountLF := 0;
  CountCR := 0;
  LinesChecked := 0;
  PrevCh := #0;

  // Convert bytes to UnicodeString
  if Encoding = UTF16LEBOMEncoding then
    Text := TEncoding.Unicode.GetString(Buffer)
  else if Encoding = UTF16BEBOMEncoding then
    Text := TEncoding.BigEndianUnicode.GetString(Buffer)
  else
    Text := TEncoding.UTF8.GetString(Buffer); // UTF-8 or ANSI/ASCII

  I := 1;
  while (I <= Length(Text)) and (LinesChecked < MaxLines) do
  begin
    Ch := Text[I];

    if (PrevCh = #13) and (Ch = #10) then
    begin
      Inc(CountCRLF);
      Inc(LinesChecked);
      PrevCh := #0; // reset previous
    end
    else
    begin
      if Ch = #13 then
        PrevCh := #13
      else
      begin
        if Ch = #10 then
        begin
          Inc(CountLF);
          Inc(LinesChecked);
        end
        else if PrevCh = #13 then
        begin
          Inc(CountCR);
          Inc(LinesChecked);
          PrevCh := #0;
        end;
      end;
    end;

    Inc(I);
  end;

  // Handle last CR at the end
  if PrevCh = #13 then
  begin
    Inc(CountCR);
    Inc(LinesChecked);
  end;

  // Determine dominant line ending
  if (CountCRLF >= CountLF) and (CountCRLF >= CountCR) and (CountCRLF > 0) then
    Result := TLineEnding.WindowsCRLF
  else if (CountLF >= CountCR) and (CountLF > 0) then
    Result := TLineEnding.UnixLF
  else if CountCR > 0 then
    Result := TLineEnding.MacintoshCR
  else
  begin
    {$IFDEF Windows}
      Result := TLineEnding.WindowsCRLF;
    {$ELSE}
    {$IFDEF Linux}
        Result := TLineEnding.UnixLF;
    {$ELSE}
    {$IFDEF Darwin}
          Result := TLineEnding.MacintoshCR;
    {$ELSE}
    Result := TLineEnding.Unknown;
    {$ENDIF}
    {$ENDIF}
    {$ENDIF}
  end;
end;

function DetectLineEnding(const FileName: string; Encoding: TEncoding; MaxLines: integer = 100): TLineEnding;
var
  Bytes: TBytes;
begin
  Bytes := LoadFileAsBytes(FileName);
  Result := DetectLineEnding(Bytes, Encoding, MaxLines);
end;

function EndsWithLineBreak(const Buffer: TBytes): boolean;
begin
  Result := False;
  if Length(Buffer) = 0 then Exit;

  if (Buffer[High(Buffer)] = byte(#10)) or (Buffer[High(Buffer)] = byte(#13)) then
    Result := True;
end;

function EndsWithLineBreak(const FileName: string): boolean;
var
  Bytes: TBytes;
begin
  Bytes := LoadFileAsBytes(FileName);
  Result := EndsWithLineBreak(Bytes);
end;

function TryLockFile(const AFileName: string; out Stream: TFileStream): boolean;
var
  fs: TFileStream;
begin
  Result := False;
  Stream := nil;

  {$IFDEF MSWINDOWS}
  try
    // Try to open file in read/write mode with exclusive share.
    fs := TFileStream.Create(AFileName, fmOpenReadWrite or fmShareDenyWrite);
    // If we reached here, file is successfully locked.
    Stream := fs;
    Result := True;
  except
    on E: EFOpenError do
    begin
      // File is probably in use or cannot be locked.
      Result := False;
    end;
    on E: Exception do
    begin
      // Any other unexpected error.
      Result := False;
    end;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  try
    try
      // Open file for reading and writing
      fs := TFileStream.Create(AFileName, fmOpenReadWrite);
    except
      fs := nil;
      Stream := nil;
      exit(False);
    end;
    // Shared lock that allows reading but prevents writing by others
    if fpflock(fs.Handle, LOCK_SH or LOCK_NB) = 0 then
    begin
      Stream := fs;
      Result := True;
    end
    else
    begin
      fs.Free;
      Stream := nil;
      Result := False;
    end;
  except
    on E: Exception do
    begin
      if Assigned(fs) then
        fs.Free;
      Stream := nil;
      Result := False;
    end;
  end;
  {$ENDIF}
end;

procedure ReadTextFile(const Bytes: TBytes; out Content: string; out FileEncoding: TEncoding; out LineEnding: TLineEnding;
  out LineCount: integer);
var
  StringList: TStringList;
  MemoryStream: TMemoryStream;
begin
  // Determine the encoding from byte array
  FileEncoding := DetectEncoding(Bytes);
  LineEnding := DetectLineEnding(Bytes, FileEncoding);

  // Load the bytes into TStringList using MemoryStream
  StringList := TStringList.Create;
  MemoryStream := TMemoryStream.Create;
  try
    if (Length(Bytes) > 0) then
      MemoryStream.WriteBuffer(Bytes[0], Length(Bytes));
    MemoryStream.Position := 0;

    // Don't add line break at end string
    StringList.Options := StringList.Options - [soTrailingLineBreak];
    StringList.LoadFromStream(MemoryStream, FileEncoding);
    Content := StringList.Text;

    // Determine the line ending type if content is empty
    if Content = string.Empty then
    begin
      LineEnding := TLineEnding.WindowsCRLF;
      if (StringList.Count = 1) and (StringList[0] = string.Empty) then
      begin
        StringList.Add(string.Empty);
        Content += LineEnding.Value;
      end
      else if StringList.Count = 0 then
      begin
        StringList.Add(string.Empty);
        Content += '[]';
      end;
    end
    else
    begin
      // Check if original byte array ended with line break
      if EndsWithLineBreak(Bytes) then
      begin
        StringList.Add(string.Empty);
        Content += LineEnding.Value;
      end;
    end;

    // Count the number of lines
    LineCount := StringList.Count;
  finally
    MemoryStream.Free;
    StringList.Free;
  end;
end;

procedure ReadTextFile(const FileName: string; out Content: string; out FileEncoding: TEncoding;
  out LineEnding: TLineEnding; out LineCount: integer);
var
  Bytes: TBytes;
begin
  Bytes := LoadFileAsBytes(FileName);
  ReadTextFile(Bytes, Content, FileEncoding, LineEnding, LineCount);
end;

procedure SaveTextFile(const FileName: string; StringList: TStringList; FileEncoding: TEncoding;
  LineEnding: TLineEnding; Encrypt: boolean; Token: string; var Salt, KeyEnc, KeyAuth: TBytes);
var
  FileStream: TFileStream;
  i: integer;
  LineWithEnding: string;
  Bytes: TBytes = nil;
  Preamble: TBytes = nil; // Array for BOM
  TextBytes: TBytes = nil;
  Text: string;
begin
  // Open the file for writing
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    if Encrypt then
    begin
      // Combine all lines into a single string
      StringList.Options := StringList.Options - [soTrailingLineBreak]; // don't add extra line breaks
      StringList.LineBreak := LineEnding.Value;
      Text := StringList.Text;
      // Add [] to empty files to allow opening
      if (Text = string.Empty) then Text := Text + '[]';
      if FileEncoding = TEncoding.ANSI then
        TextBytes := TEncoding.ANSI.GetAnsiBytes(Text)
      else
      if FileEncoding = TEncoding.ASCII then
        TextBytes := TEncoding.ASCII.GetAnsiBytes(Text)
      else
        TextBytes := FileEncoding.GetBytes(unicodestring(Text));

      // Prepend BOM inside text (if needed)
      if IsBOMEncoding(FileEncoding) then
      begin
        Preamble := FileEncoding.GetPreamble;
        if Length(Preamble) > 0 then
        begin
          SetLength(Bytes, Length(Preamble) + Length(TextBytes));
          Move(Preamble[0], Bytes[0], Length(Preamble));
          Move(TextBytes[0], Bytes[Length(Preamble)], Length(TextBytes));
        end
        else
          Bytes := TextBytes;
      end
      else
        Bytes := TextBytes;

      // Encrypt bytes
      Bytes := EncryptData(Bytes, Token, Salt, KeyEnc, KeyAuth);

      // Write encrypted bytes to file
      if Length(Bytes) > 0 then
        FileStream.WriteBuffer(Bytes[0], Length(Bytes));
    end
    else
    begin
      // For non-encrypted files, write BOM normally at the start
      if IsBOMEncoding(FileEncoding) then
      begin
        Preamble := FileEncoding.GetPreamble;
        if Length(Preamble) > 0 then
          FileStream.WriteBuffer(Preamble[0], Length(Preamble));
      end;

      // Write plain text line by line
      for i := 0 to StringList.Count - 1 do
      begin
        if i < StringList.Count - 1 then
          LineWithEnding := StringList[i] + LineEnding.Value
        else
          LineWithEnding := StringList[i];

        Bytes := FileEncoding.GetBytes(unicodestring(LineWithEnding));
        if Assigned(Bytes) then
          FileStream.WriteBuffer(Bytes[0], Length(Bytes));
      end;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure SaveTextFile(const FileName: string; StringList: TStringList; FileEncoding: TEncoding; LineEnding: TLineEnding);
var
  dummy: TBytes = nil;
begin
  try
    SaveTextFile(FileName, StringList, FileEncoding, LineEnding, False, string.Empty, dummy, dummy, dummy);
  finally
    FreeBytesSecure(dummy);
  end;
end;

initialization
  UTF8BOMEncoding := TEncoding.GetEncoding(65001);
  UTF16LEBOMEncoding := TEncoding.GetEncoding(1200);
  UTF16BEBOMEncoding := TEncoding.GetEncoding(1201);

finalization
  if (Assigned(UTF8BOMEncoding)) then
    UTF8BOMEncoding.Free;
  if (Assigned(UTF16LEBOMEncoding)) then
    UTF16LEBOMEncoding.Free;
  if (Assigned(UTF16BEBOMEncoding)) then
    UTF16BEBOMEncoding.Free;

end.
