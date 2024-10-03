unit settings;

{$mode ObjFPC}{$H+}

interface

uses
  Forms, Classes, SysUtils, FileUtil, fpjson, jsonparser, Grids, Graphics, mainform;

type
  TGridSettings = record
    ColumnWidths: array of integer;
    RowHeights: array of integer;
  end;

procedure SaveFormSettings(Form: TformNotetask);
function LoadFormSettings(Form: TformNotetask): boolean;
procedure SaveGridSettings(Grid: TStringGrid);
function LoadGridSettings(Grid: TStringGrid): boolean;

implementation

function GetSettingsDirectory(fileName: string = ''): string;
begin
  {$IFDEF Windows}
  Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('LOCALAPPDATA')) + 'notetask\'+fileName;
  {$ELSE}
  Result := IncludeTrailingPathDelimiter(GetUserDir) + '.config/notetask/' + filename;
  {$ENDIF}
end;

procedure SaveFormSettings(Form: TformNotetask);
var
  JSONObj: TJSONObject;
  FileName: string;
begin
  FileName := GetSettingsDirectory('form_settings.json'); // Получаем имя файла настроек
  ForceDirectories(GetSettingsDirectory); // Убедитесь, что каталог существует
  JSONObj := TJSONObject.Create;
  try
    // Сохранение положения и размера формы
    JSONObj.Add('Left', Form.Left);
    JSONObj.Add('Top', Form.Top);
    JSONObj.Add('Width', Form.Width);
    JSONObj.Add('Height', Form.Height);
    JSonObj.Add('WordWrap', Form.WordWrap);

    // Сохранение шрифта
    JSONObj.Add('FontName', Form.Font.Name);
    JSONObj.Add('FontSize', Form.Font.Size);
    JSONObj.Add('FontStyle', Integer(Form.Font.Style));  // Преобразуем стиль шрифта в число

    // Запись в файл
    with TStringList.Create do
    try
      Add(JSONObj.AsJSON);
      SaveToFile(FileName);
    finally
      Free;
    end;
  finally
    JSONObj.Free;
  end;
end;

function LoadFormSettings(Form: TformNotetask): boolean;
var
  JSONData: TJSONData;
  JSONObj: TJSONObject;
  FileName: string;
  FileStream: TFileStream;
  FileContent: string;
begin
  Result := False;
  FileName := GetSettingsDirectory('form_settings.json'); // Get the settings file name
  if not FileExists(FileName) then Exit; // Exit if the file does not exist

  // Read from file
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(FileContent, FileStream.Size);
    FileStream.Read(Pointer(FileContent)^, FileStream.Size);
    JSONData := GetJSON(FileContent);
    try
      JSONObj := JSONData as TJSONObject;

      // Check and load form's position and size
      if JSONObj.FindPath('Left') <> nil then
        Form.Left := JSONObj.FindPath('Left').AsInteger;
      if JSONObj.FindPath('Top') <> nil then
        Form.Top := JSONObj.FindPath('Top').AsInteger;
      if JSONObj.FindPath('Width') <> nil then
        Form.Width := JSONObj.FindPath('Width').AsInteger;
      if JSONObj.FindPath('Height') <> nil then
        Form.Height := JSONObj.FindPath('Height').AsInteger;
      if JSONObj.FindPath('WordWrap') <> nil then
        Form.WordWrap := JSONObj.FindPath('WordWrap').AsBoolean;

      // Check and load font properties
      if JSONObj.FindPath('FontName') <> nil then
        Form.Font.Name := JSONObj.FindPath('FontName').AsString;
      if JSONObj.FindPath('FontSize') <> nil then
        Form.Font.Size := JSONObj.FindPath('FontSize').AsInteger;
      if JSONObj.FindPath('FontStyle') <> nil then
        Form.Font.Style := TFontStyles(JSONObj.FindPath('FontStyle').AsInteger); // Convert integer back to TFontStyles

      Result := True;
    finally
      JSONData.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure SaveGridSettings(Grid: TStringGrid);
var
  JSONObj: TJSONObject;
  ColumnArray, RowArray: TJSONArray;
  i: integer;
  FileName: string;
begin
  FileName := GetSettingsDirectory('grid_settings.json'); // Получаем имя файла настроек
  ForceDirectories(GetSettingsDirectory);
  JSONObj := TJSONObject.Create;
  ColumnArray := TJSONArray.Create;
  RowArray := TJSONArray.Create;

  // Сохранение ширины столбцов
  for i := 0 to Grid.ColCount - 1 do
    ColumnArray.Add(Grid.ColWidths[i]);

  // Сохранение высоты строк
  //for i := 0 to Grid.RowCount - 1 do
  //  RowArray.Add(Grid.RowHeights[i]);

  JSONObj.Add('ColumnWidths', ColumnArray);
  //JSONObj.Add('RowHeights', RowArray);

  // Запись в файл
  with TStringList.Create do
  try
    Add(JSONObj.AsJSON);
    SaveToFile(FileName);
  finally
    Free;
  end;

  JSONObj.Free;
end;

function LoadGridSettings(Grid: TStringGrid): boolean;
var
  JSONData: TJSONData;
  JSONObj: TJSONObject;
  ColumnArray, RowArray: TJSONArray;
  i: integer;
  FileContent: string;
  FileStream: TFileStream;
  FileName: string;
begin
  Result := False;
  FileName := GetSettingsDirectory('grid_settings.json'); // Получаем имя файла настроек
  ForceDirectories(GetSettingsDirectory);
  if not FileExists(FileName) then Exit;

  // Чтение из файла
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(FileContent, FileStream.Size);
    FileStream.Read(Pointer(FileContent)^, FileStream.Size);
    JSONData := GetJSON(FileContent);
    try
      JSONObj := JSONData as TJSONObject;
      ColumnArray := JSONObj.FindPath('ColumnWidths') as TJSONArray;
      RowArray := JSONObj.FindPath('RowHeights') as TJSONArray;

      // Установка ширины столбцов
      for i := 0 to ColumnArray.Count - 1 do
        Grid.ColWidths[i] := ColumnArray.Items[i].AsInteger;

      // Установка высоты строк
      //for i := 0 to RowArray.Count - 1 do
      //  Grid.RowHeights[i] := RowArray.Items[i].AsInteger;

      Result := True;
    finally
      JSONData.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

end.
