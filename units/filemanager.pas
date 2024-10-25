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
  lineending;

function GetEncodingName(Encoding: TEncoding): string;

function IsBOMEncoding(Encoding: TEncoding): boolean;

function IsValidAscii(var Buffer: array of byte; BytesRead: integer): boolean;

function IsValidAnsi(var Buffer: array of byte; BytesRead: integer): boolean;

function DetectEncoding(const FileName: string): TEncoding;

function EndsWithLineBreak(const FileName: string): boolean;

procedure ReadTextFile(const FileName: string; out Content: string; out FileEncoding: TEncoding;
  out LineEnding: TLineEnding; out LineCount: integer);

procedure SaveTextFile(const FileName: string; StringList: TStringList; FileEncoding: TEncoding; LineEnding: TLineEnding);

implementation

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
  Result := True; // Assume valid ANSI

  for i := 0 to BytesRead - 1 do
  begin
    // Allow characters from 0 to 255 (Windows-1251)
    if (Buffer[i] > $7F) and (Buffer[i] < $C0) and (Buffer[i] <> $A0) and (Buffer[i] <> $A1) and
      (Buffer[i] <> $A2) and (Buffer[i] <> $A3) and (Buffer[i] <> $A4) and (Buffer[i] <> $A5) and
      (Buffer[i] <> $A6) and (Buffer[i] <> $A7) and (Buffer[i] <> $A8) and (Buffer[i] <> $A9) and
      (Buffer[i] <> $AA) and (Buffer[i] <> $AB) and (Buffer[i] <> $AC) and (Buffer[i] <> $AD) and
      (Buffer[i] <> $AE) and (Buffer[i] <> $AF) then
    begin
      Result := False; // Invalid ANSI character found
      Exit;
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
    // Read the first 4 bytes to check for BOM
    BytesRead := FileStream.Read(Buffer, SizeOf(Buffer));

    // Check if the read bytes correspond to BOM
    if BytesRead >= 3 then
    begin
      // Check for UTF-8 BOM
      if (Buffer[0] = $EF) and (Buffer[1] = $BB) and (Buffer[2] = $BF) then
        exit(TEncoding.GetEncoding(65001)) // UTF-8 BOM
      // Check for UTF-16 LE BOM
      else if (Buffer[0] = $FF) and (Buffer[1] = $FE) then
        exit(TEncoding.GetEncoding(1200)) // UTF-16 LE BOM
      // Check for UTF-16 BE BOM
      else if (Buffer[0] = $FE) and (Buffer[1] = $FF) then
        exit(TEncoding.GetEncoding(1201)); // UTF-16 BE BOM
    end;

    // If no BOM is found, check the content for text patterns
    // Reset position to the beginning of the file
    FileStream.Position := 0;
    // Create a dynamic array for the first 1024 bytes
    SetLength(ContentBuffer, 1024);
    BytesRead := FileStream.Read(ContentBuffer[0], Length(ContentBuffer));

    // Check if the file content could be ANSI
    if IsValidAnsi(ContentBuffer, BytesRead) then
      Result := TEncoding.ANSI
    else if IsValidAscii(ContentBuffer, BytesRead) then
      Result := TEncoding.ASCII
    else
      Result := TEncoding.UTF8; // If neither ANSI nor ASCII, assume UTF-8
  finally
    FileStream.Free;
  end;
end;

function EndsWithLineBreak(const FileName: string): boolean;
var
  FileStream: TFileStream;
  Buffer: array of byte;
begin
  Result := False; // Предполагаем, что переноса нет
  Buffer := [];
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    // Проверяем размер файла
    if FileStream.Size > 0 then
    begin
      // Создаем буфер размером 1 байт
      SetLength(Buffer, 1);
      // Переходим в конец файла
      FileStream.Position := FileStream.Size - 1;
      // Читаем последний байт
      FileStream.Read(Buffer[0], 1);

      // Проверяем, является ли последний байт символом переноса строки
      if (Buffer[0] = byte(#10)) or (Buffer[0] = byte(#13)) then
        Result := True;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure ReadTextFile(const FileName: string; out Content: string; out FileEncoding: TEncoding;
  out LineEnding: TLineEnding; out LineCount: integer);
var
  StringList: TStringList;
begin
  // Determine the encoding
  FileEncoding := DetectEncoding(FileName);

  // Read the file content using TStringList
  StringList := TStringList.Create;
  // Don't add line break at end string
  StringList.Options := StringList.Options - [soTrailingLineBreak];
  try
    StringList.LoadFromFile(FileName, FileEncoding);
    Content := StringList.Text;

    // Determine the line ending type
    if Pos(#13#10, Content) > 0 then
      LineEnding := TLineEnding.WindowsCRLF
    else if Pos(#10, Content) > 0 then
      LineEnding := TLineEnding.UnixLF
    else if Pos(#13, Content) > 0 then
      LineEnding := TLineEnding.MacintoshCR
    else
      LineEnding := TLineEnding.Unknown;

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

procedure SaveTextFile(const FileName: string; StringList: TStringList; FileEncoding: TEncoding; LineEnding: TLineEnding);
var
  LineEndingStr: string;
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
    StringList.Free;
  end;
end;

end.
