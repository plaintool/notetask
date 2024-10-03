unit filemanager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, lineending;

function DetectEncoding(const FileName: string): TEncoding;

function TextToStringList(const TextContent: string): TStringList;

procedure ReadTextFile(const FileName: string; out Content: string; out FileEncoding: TEncoding;
  out LineEnding: TLineEnding; out LineCount: integer);

procedure SaveTextFile(const FileName: string; StringList: TStringList; FileEncoding: TEncoding; LineEnding: TLineEnding);

implementation

function DetectEncoding(const FileName: string): TEncoding;
var
  FileStream: TFileStream;
  Buffer: array[0..3] of byte;
  ContentBuffer: array of byte; // Динамический массив
  BytesRead: integer;
begin
  Result := TEncoding.UTF8;
  // Предполагаем, что файл в UTF-8 по умолчанию
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    // Читаем первые 4 байта для анализа BOM
    BytesRead := FileStream.Read(Buffer, SizeOf(Buffer));

    if BytesRead >= 3 then
    begin
      // Проверяем на UTF-8 BOM
      if (Buffer[0] = $EF) and (Buffer[1] = $BB) and (Buffer[2] = $BF) then
        Result := TEncoding.UTF8
      // Проверяем на UTF-16 LE BOM
      else if (Buffer[0] = $FF) and (Buffer[1] = $FE) then
        Result := TEncoding.Unicode
      // Проверяем на UTF-16 BE BOM
      else if (Buffer[0] = $FE) and (Buffer[1] = $FF) then
        Result := TEncoding.BigEndianUnicode;
    end
    else
    begin
      // Если BOM не найден, проверяем содержимое на наличие текстовых символов
      FileStream.Position := 0;
      // Сбросить позицию на начало файла
      SetLength(ContentBuffer, 1024);
      // Создаем динамический массив для первых 1024 байт
      BytesRead := FileStream.Read(ContentBuffer[0], Length(ContentBuffer));

      // Проверка на наличие символов UTF-8
      if (BytesRead > 0) and (Pos(#0, string(pansichar(@ContentBuffer[0]))) = 0) then
      begin
        Result := TEncoding.UTF8; // Предполагаем, что это UTF-8
      end;
    end;
  finally
    FileStream.Free;
  end;
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
  ModifiedList: TStringList;
  i:integer;
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

  // Создаем новый TStringList для замены переносов строк
  ModifiedList := TStringList.Create;
  try
    for i := 0 to StringList.Count - 1 do
    begin
      // Заменяем переносы строк в каждой строке на заданный формат
      ModifiedList.Add(StringReplace(StringList[i], sLineBreak, LineEndingStr, [rfReplaceAll]));
    end;

    // Сохраняем измененный TStringList в файл с указанной кодировкой
    ModifiedList.SaveToFile(FileName, FileEncoding);
  finally
    ModifiedList.Free; // Освобождаем ресурсы
  end;
end;


end.
