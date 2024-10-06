unit filemanager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, lineending;

function DetectEncoding(const FileName: string): TEncoding;

function GetEncodingName(Encoding: TEncoding): string;

function TextToStringList(const TextContent: string): TStringList;

procedure ReadTextFile(const FileName: string; out Content: string; out FileEncoding: TEncoding;
  out LineEnding: TLineEnding; out LineCount: integer);

procedure SaveTextFile(const FileName: string; StringList: TStringList; FileEncoding: TEncoding; LineEnding: TLineEnding);

implementation

function DetectEncoding(const FileName: string): TEncoding;
var
  FileStream: TFileStream;
  Buffer: array[0..3] of byte;
  ContentBuffer: array of byte; // Dynamic array
  BytesRead: integer;
  i: Integer;
  Utf8Candidate: Boolean;
begin
  Result := TEncoding.UTF8;
  // Assume UTF-8 by default
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    // Read the first 4 bytes to check for BOM
    BytesRead := FileStream.Read(Buffer, SizeOf(Buffer));

    if BytesRead >= 3 then
    begin
      // Check for UTF-8 BOM
      if (Buffer[0] = $EF) and (Buffer[1] = $BB) and (Buffer[2] = $BF) then
        Result := TEncoding.UTF8
      // Check for UTF-16 LE BOM
      else if (Buffer[0] = $FF) and (Buffer[1] = $FE) then
        Result := TEncoding.Unicode
      // Check for UTF-16 BE BOM
      else if (Buffer[0] = $FE) and (Buffer[1] = $FF) then
        Result := TEncoding.BigEndianUnicode;
    end
    else
    begin
      // If no BOM is found, check the content for text patterns
      FileStream.Position := 0;
      // Reset position to the beginning of the file
      SetLength(ContentBuffer, 1024);
      // Create a dynamic array for the first 1024 bytes
      BytesRead := FileStream.Read(ContentBuffer[0], Length(ContentBuffer));

      // Check if the file content could be UTF-8
      Utf8Candidate := True;
      for i := 0 to BytesRead - 1 do
      begin
        // If any byte does not match UTF-8 structure, assume ANSI
        if (ContentBuffer[i] >= $80) and (ContentBuffer[i] <= $BF) then
        begin
          Utf8Candidate := False;
          Break;
        end;
      end;

      // If no invalid UTF-8 bytes were found, assume UTF-8, otherwise ANSI
      if Utf8Candidate then
        Result := TEncoding.UTF8
      else
        Result := TEncoding.ANSI;
    end;
  finally
    FileStream.Free;
  end;
end;

function GetEncodingName(Encoding: TEncoding): string;
begin
  if Encoding = TEncoding.UTF8 then
    Result := 'UTF-8'
  else if Encoding = TEncoding.Unicode then
    Result := 'UTF-16 LE'
  else if Encoding = TEncoding.BigEndianUnicode then
    Result := 'UTF-16 BE'
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

function TextToStringList(const TextContent: string): TStringList;
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  // Создаем новый экземпляр TStringList
  try
    StringList.Text := TextContent; // Загружаем текст в TStringList
    Result := StringList; // Возвращаем TStringList
  except
    StringList.Free; // Освобождаем память при ошибке
    raise; // Пробрасываем исключение дальше
  end;
end;

procedure ReadTextFile(const FileName: string; out Content: string; out FileEncoding: TEncoding;
  out LineEnding: TLineEnding; out LineCount: integer);
var
  StringList: TStringList;
begin
  // Определяем кодировку
  FileEncoding := DetectEncoding(FileName);

  // Читаем содержимое файла с использованием TStringList
  StringList := TStringList.Create;
  try
    StringList.LoadFromFile(FileName, FileEncoding);
    Content := StringList.Text;

    // Определяем тип переноса строк
    if Pos(#13#10, Content) > 0 then
      LineEnding := TLineEnding.WindowsCRLF
    else if Pos(#10, Content) > 0 then
      LineEnding := TLineEnding.UnixLF
    else if Pos(#13, Content) > 0 then
      LineEnding := TLineEnding.MacintoshCR
    else
      LineEnding := TLineEnding.Unknown;

    // Считаем количество строк
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
begin
  // Устанавливаем тип переноса строк в зависимости от заданного LineEnding
  if LineEnding = TLineEnding.WindowsCRLF then
    LineEndingStr := sLineBreak // CRLF
  else if LineEnding = TLineEnding.UnixLF then
    LineEndingStr := #10 // LF
  else if LineEnding = TLineEnding.MacintoshCR then
    LineEndingStr := #13 // CR
  else
    LineEndingStr := sLineBreak; // По умолчанию используем стандартный перенос

  // Открываем файл для записи
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    for i := 0 to StringList.Count - 1 do
    begin
      // Для каждой строки, кроме последней, добавляем LineEndingStr
      if i < StringList.Count - 1 then
        LineWithEnding := StringList[i] + LineEndingStr
      else
        LineWithEnding := StringList[i];

      // Конвертируем строку в байты с учетом кодировки
      Bytes := FileEncoding.GetBytes(LineWithEnding);
      FileStream.WriteBuffer(Bytes[0], Length(Bytes)); // Записываем байты в файл
    end;
  finally
    FileStream.Free; // Освобождаем ресурсы
  end;
end;

end.
