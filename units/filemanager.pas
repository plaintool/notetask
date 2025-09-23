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
  DateUtils,
  {$ELSE}
  Windows,
  {$ENDIF}
  lineending;

function GetEncodingName(Encoding: TEncoding): string;

function IsUserEncoding(Enc: TEncoding): boolean;

function IsBOMEncoding(Encoding: TEncoding): boolean;

function IsValidUTF8(var Buffer: array of byte; BytesRead: integer): boolean;

function IsValidAscii(var Buffer: array of byte; BytesRead: integer): boolean;

function IsValidAnsi(var Buffer: array of byte; BytesRead: integer): boolean;

function DetectEncoding(const FileName: string): TEncoding;

function DetectLineEnding(const FileName: string; MaxLines: integer = 100): TLineEnding;

function EndsWithLineBreak(const FileName: string): boolean;

procedure ReadStringFile(var Content: string; out FileEncoding: TEncoding; out LineEnding: TLineEnding; out LineCount: integer);

procedure ReadTextFile(const FileName: string; out Content: string; out FileEncoding: TEncoding;
  out LineEnding: TLineEnding; out LineCount: integer);

procedure SaveTextFile(const FileName: string; StringList: TStringList; FileEncoding: TEncoding; LineEnding: TLineEnding;
  Encrypt: boolean = False; Hash: TBytes = nil);

function FindPowerShellCore: string;

procedure UpdateFileReadAccess(const FileName: string);

var
  UTF8BOMEncoding: TEncoding;
  UTF16LEBOMEncoding, UTF16BEBOMEncoding: TEncoding;

implementation

uses crypto;

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

function IsValidUTF8(var Buffer: array of byte; BytesRead: integer): boolean;
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

function IsValidAscii(var Buffer: array of byte; BytesRead: integer): boolean;
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

function IsValidAnsi(var Buffer: array of byte; BytesRead: integer): boolean;
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

function DetectEncoding(const FileName: string): TEncoding;
var
  FileStream: TFileStream;
  Buffer: array[0..3] of byte = (0, 0, 0, 0);
  ContentBuffer: array of byte; // Dynamic array
  BytesRead: integer;
begin
  Result := TEncoding.UTF8; // Assume UTF-8 by default
  ContentBuffer := [];
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    // If the file is empty, treat it as UTF-8
    if FileStream.Size = 0 then
      Exit(TEncoding.UTF8);

    // Read the first 4 bytes to check for BOM
    BytesRead := FileStream.Read(Buffer, SizeOf(Buffer));

    // Check if the read bytes correspond to BOM
    if BytesRead >= 3 then
    begin
      // Check for UTF-8 BOM
      if (Buffer[0] = $EF) and (Buffer[1] = $BB) and (Buffer[2] = $BF) then
        exit(UTF8BOMEncoding)// UTF-8 BOM
      // Check for UTF-16 LE BOM
      else if (Buffer[0] = $FF) and (Buffer[1] = $FE) then
        exit(UTF16LEBOMEncoding) // UTF-16 LE BOM
      // Check for UTF-16 BE BOM
      else if (Buffer[0] = $FE) and (Buffer[1] = $FF) then
        exit(UTF16BEBOMEncoding); // UTF-16 BE BOM
    end;

    // If no BOM is found, check the content for text patterns
    // Reset position to the beginning of the file
    FileStream.Position := 0;

    // Create a dynamic array for up to the first 1024 bytes
    if FileStream.Size > 1024 * 4 then
      SetLength(ContentBuffer, 1024 * 4)
    else
      SetLength(ContentBuffer, FileStream.Size);
    BytesRead := FileStream.Read(ContentBuffer[0], Length(ContentBuffer));

    // Check if the file content could be ANSI
    if IsValidUtf8(ContentBuffer, BytesRead) then
      Result := TEncoding.UTF8
    else if IsValidAnsi(ContentBuffer, BytesRead) then
      Result := TEncoding.ANSI
    else if IsValidAscii(ContentBuffer, BytesRead) then
      Result := TEncoding.ASCII
    else
      Result := TEncoding.UTF8; // If not detected, assume UTF-8
  finally
    FileStream.Free;
  end;
end;

function DetectLineEnding(const FileName: string; MaxLines: integer = 100): TLineEnding;
type
  TBuffer = array[0..4095] of byte;
var
  FileStream: TFileStream;
  Buffer: TBuffer;
  BytesRead, I: integer;
  CountCRLF, CountLF, CountCR, LinesChecked: integer;
begin
  Buffer := Default(TBuffer);
  CountCRLF := 0;
  CountLF := 0;
  CountCR := 0;
  LinesChecked := 0;

  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    while (LinesChecked < MaxLines) and (FileStream.Position < FileStream.Size) do
    begin
      BytesRead := FileStream.Read(Buffer, SizeOf(Buffer));
      I := 0;
      while (I < BytesRead) and (LinesChecked < MaxLines) do
      begin
        if (Buffer[I] = 13) and (I + 1 < BytesRead) and (Buffer[I + 1] = 10) then
        begin
          Inc(CountCRLF);
          Inc(LinesChecked);
          Inc(I, 2);
          Continue;
        end
        else if Buffer[I] = 10 then
        begin
          Inc(CountLF);
          Inc(LinesChecked);
        end
        else if Buffer[I] = 13 then
        begin
          Inc(CountCR);
          Inc(LinesChecked);
        end;
        Inc(I);
      end;
    end;
  finally
    FileStream.Free;
  end;

  if (CountCRLF >= CountLF) and (CountCRLF >= CountCR) and (CountCRLF > 0) then
    Result := TLineEnding.WindowsCRLF
  else if (CountLF >= CountCR) and (CountLF > 0) then
    Result := TLineEnding.UnixLF
  else if CountCR > 0 then
    Result := TLineEnding.MacintoshCR
  else
    Result := TLineEnding.Unknown;
end;

function EndsWithLineBreak(const FileName: string): boolean;
var
  FileStream: TFileStream;
  Buffer: array of byte;
begin
  Result := False; // Assume there is no line break
  Buffer := [];
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    // Check file size
    if FileStream.Size > 0 then
    begin
      // Create a buffer with size of 1 byte
      SetLength(Buffer, 1);
      // Move to the end of the file
      FileStream.Position := FileStream.Size - 1;
      // Read the last byte
      FileStream.Read(Buffer[0], 1);

      // Check if the last byte is a line break character
      if (Buffer[0] = byte(#10)) or (Buffer[0] = byte(#13)) then
        Result := True;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure ReadStringFile(var Content: string; out FileEncoding: TEncoding; out LineEnding: TLineEnding; out LineCount: integer);
var
  StringList: TStringList;
begin
  // Default encoding – UTF8, можно расширить, если нужно определять по BOM
  FileEncoding := TEncoding.UTF8;

  // Detect line endings
  if Pos(#13#10, Content) > 0 then
    LineEnding := TLineEnding.WindowsCRLF
  else if Pos(#10, Content) > 0 then
    LineEnding := TLineEnding.UnixLF
  else if Pos(#13, Content) > 0 then
    LineEnding := TLineEnding.MacintoshCR
  else
    LineEnding := TLineEnding.Unknown;

  // Load into TStringList to count lines
  StringList := TStringList.Create;
  StringList.Options := StringList.Options - [soTrailingLineBreak]; // don't add extra line breaks
  try
    StringList.Text := Content;

    // Handle empty content
    if Content = string.Empty then
    begin
      LineEnding := TLineEnding.WindowsCRLF;
      if StringList.Count = 0 then
      begin
        StringList.Add(string.Empty);
        Content := '[]';
      end;
    end
    else
    begin
      // Append line break if content originally ended with one
      if (Length(Content) > 0) and (Content[Length(Content)] in [#10, #13]) then
      begin
        StringList.Add(string.Empty);
        Content := Content + LineEnding.Value;
      end;
    end;

    LineCount := StringList.Count;
  finally
    StringList.Free;
  end;
end;

procedure ReadTextFile(const FileName: string; out Content: string; out FileEncoding: TEncoding;
  out LineEnding: TLineEnding; out LineCount: integer);
var
  StringList: TStringList;
begin
  // Determine the encoding
  FileEncoding := DetectEncoding(FileName);
  LineEnding := DetectLineEnding(FileName);

  // Read the file content using TStringList
  StringList := TStringList.Create;
  // Don't add line break at end string
  StringList.Options := StringList.Options - [soTrailingLineBreak];
  try
    StringList.LoadFromFile(FileName, FileEncoding);
    Content := StringList.Text;

    UpdateFileReadAccess(FileName);

    // Determine the line ending type
    //if Pos(#13#10, Content) > 0 then
    //  LineEnding := TLineEnding.WindowsCRLF
    //else if Pos(#10, Content) > 0 then
    //  LineEnding := TLineEnding.UnixLF
    //else if Pos(#13, Content) > 0 then
    //  LineEnding := TLineEnding.MacintoshCR
    //else
    //  LineEnding := TLineEnding.Unknown;

    if Content = string.Empty then
    begin
      LineEnding := TLineEnding.WindowsCRLF;
      if (StringList.Count = 1) and (Stringlist[0] = string.empty) then
      begin
        StringList.Add(string.Empty);
        Content += LineEnding.Value;
      end
      else
      if StringList.Count = 0 then
      begin
        StringList.Add(string.Empty);
        Content += '[]';
      end;
    end
    else
    if (EndsWithLineBreak(FileName)) then
    begin
      StringList.Add(string.Empty);
      Content += LineEnding.Value;
    end;

    // Count the number of lines
    LineCount := StringList.Count;
  finally
    StringList.Free;
  end;
end;

procedure SaveTextFile(const FileName: string; StringList: TStringList; FileEncoding: TEncoding; LineEnding: TLineEnding;
  Encrypt: boolean = False; Hash: TBytes = nil);
var
  LineEndingStr: string;
  FullText: string;
  FileStream: TFileStream;
  i: integer;
  LineWithEnding: string;
  Bytes: TBytes;
  Preamble: TBytes; // Array for BOM
begin
  // Set the line ending type based on the provided LineEnding
  if LineEnding = TLineEnding.WindowsCRLF then
    LineEndingStr := sLineBreak // CRLF
  else if LineEnding = TLineEnding.UnixLF then
    LineEndingStr := #10 // LF
  else if LineEnding = TLineEnding.MacintoshCR then
    LineEndingStr := #13 // CR
  else
    LineEndingStr := sLineBreak; // Default to standard line ending

  // Open the file for writing
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    // Write the BOM (if any) before writing the content
    if (IsBOMEncoding(FileEncoding)) then
    begin
      Preamble := FileEncoding.GetPreamble;
      if Length(Preamble) > 0 then
        FileStream.WriteBuffer(Preamble[0], Length(Preamble)); // Write BOM
    end;

    // Write empty file
    if StringList.Count = 0 then
      Exit;

    if Encrypt then
    begin
      // Combine all lines into a single string
      StringList.Options := StringList.Options - [soTrailingLineBreak]; // don't add extra line breaks
      StringList.LineBreak := LineEndingStr;

      FullText := StringList.Text;

      // Convert encrypted string to bytes with the specified encoding
      Bytes := FileEncoding.GetBytes(unicodestring(EncryptData(FullText, Hash)));
      if Assigned(Bytes) then
        FileStream.WriteBuffer(Bytes[0], Length(Bytes));
    end
    else
      for i := 0 to StringList.Count - 1 do
      begin
        // For each line except the last, add LineEndingStr
        if i < StringList.Count - 1 then
          LineWithEnding := StringList[i] + LineEndingStr
        else
          LineWithEnding := StringList[i];

        // Convert the string to bytes with the specified encoding
        Bytes := FileEncoding.GetBytes(unicodestring(LineWithEnding));
        if Assigned(Bytes) then
          FileStream.WriteBuffer(Bytes[0], Length(Bytes)); // Write bytes to the file
      end;
  finally
    // Free resources
    FileStream.Free;
  end;
end;

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
