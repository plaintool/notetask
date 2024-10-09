unit settings;

{$mode ObjFPC}{$H+}

interface

uses
  Forms, Classes, SysUtils, FileUtil, fpjson, jsonparser, Grids, Graphics, Registry, mainform;

type
  TGridSettings = record
    ColumnWidths: array of integer;
    RowHeights: array of integer;
  end;

procedure SaveFormSettings(Form: TformNotetask);

function LoadFormSettings(Form: TformNotetask): boolean;

procedure SaveGridSettings(Grid: TStringGrid);

function LoadGridSettings(Grid: TStringGrid): boolean;

function SetFileTypeIcon(const Ext: string; IconIndex: integer): boolean;

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
  FileName := GetSettingsDirectory('form_settings.json'); // Get settings file name
  ForceDirectories(GetSettingsDirectory); // Ensure the directory exists
  JSONObj := TJSONObject.Create;
  try
    // Save form position and size

    JSONObj.Add('WordWrap', Form.WordWrap);
    JSONObj.Add('ShowArchived', Form.ShowArchived);
    JSONObj.Add('ShowStatusBar', Form.ShowStatusBar);
    JSONObj.Add('WindowState', Ord(Form.WindowState));
    JSONObj.Add('Left', Form.RestoredLeft);
    JSONObj.Add('Top', Form.RestoredTop);
    JSONObj.Add('Width', Form.RestoredWidth);
    JSONObj.Add('Height', Form.RestoredHeight);

    // Save font
    JSONObj.Add('FontName', Form.Font.Name);
    JSONObj.Add('FontSize', Form.Font.Size);
    JSONObj.Add('FontStyle', integer(Form.Font.Style));  // Convert font style to number

    // Write to file
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
      if JSONObj.FindPath('WordWrap') <> nil then
        Form.WordWrap := JSONObj.FindPath('WordWrap').AsBoolean;

      if JSONObj.FindPath('ShowArchived') <> nil then
        Form.FShowArchived := JSONObj.FindPath('ShowArchived').AsBoolean;

      if JSONObj.FindPath('ShowStatusBar') <> nil then
        Form.ShowStatusBar := JSONObj.FindPath('ShowStatusBar').AsBoolean;

      if JSONObj.FindPath('WindowState') <> nil then
        Form.WindowState := TWindowState(JSONObj.FindPath('WindowState').AsInteger);

      if JSONObj.FindPath('Left') <> nil then
        Form.Left := JSONObj.FindPath('Left').AsInteger;

      if JSONObj.FindPath('Top') <> nil then
        Form.Top := JSONObj.FindPath('Top').AsInteger;

      if JSONObj.FindPath('Width') <> nil then
        Form.Width := JSONObj.FindPath('Width').AsInteger;

      if JSONObj.FindPath('Height') <> nil then
        Form.Height := JSONObj.FindPath('Height').AsInteger;

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
  FileName := GetSettingsDirectory('grid_settings.json'); // Get settings file name
  ForceDirectories(GetSettingsDirectory);
  JSONObj := TJSONObject.Create;
  ColumnArray := TJSONArray.Create;
  RowArray := TJSONArray.Create;

  // Save column widths
  for i := 0 to Grid.ColCount - 1 do
    ColumnArray.Add(Grid.ColWidths[i]);

  // Save row heights
  //for i := 0 to Grid.RowCount - 1 do
  //  RowArray.Add(Grid.RowHeights[i]);

  JSONObj.Add('ColumnWidths', ColumnArray);
  //JSONObj.Add('RowHeights', RowArray);

  // Write to file
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
  FileName := GetSettingsDirectory('grid_settings.json'); // Get settings file name
  ForceDirectories(GetSettingsDirectory);
  if not FileExists(FileName) then Exit;

  // Read from file
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(FileContent, FileStream.Size);
    FileStream.Read(Pointer(FileContent)^, FileStream.Size);
    JSONData := GetJSON(FileContent);
    try
      JSONObj := JSONData as TJSONObject;
      ColumnArray := JSONObj.FindPath('ColumnWidths') as TJSONArray;
      RowArray := JSONObj.FindPath('RowHeights') as TJSONArray;

      // Set column widths
      for i := 0 to ColumnArray.Count - 1 do
        Grid.ColWidths[i] := ColumnArray.Items[i].AsInteger;

      // Set row heights
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

function SetFileTypeIcon(const Ext: string; IconIndex: integer): boolean;
var
  Reg: TRegistry;
  IconPath: string;
  AppPath: string;
begin
  Result := False; // Initialize result to false

  Reg := TRegistry.Create;
  try
    AppPath := Application.ExeName;
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Create a key for the file extension
    if Reg.OpenKey(Ext, True) then
    begin
      Reg.WriteString('', 'Notetask'); // Assign the class name
      Reg.CloseKey;
    end;

    // Create a key for Notetask
    if Reg.OpenKey('Notetask\DefaultIcon', True) then
    begin
      IconPath := Format('%s,%d', [AppPath, IconIndex]);
      Reg.WriteString('', IconPath); // Set the icon path
      Reg.CloseKey;
    end;

    // Create a key for opening the file
    if Reg.OpenKey('Notetask\shell\open\command', True) then
    begin
      Reg.WriteString('', Format('"%s" "%%1"', [AppPath])); // Command to open the file
      Reg.CloseKey;
    end;

    Result := True; // Set result to true if all operations succeeded
  except
    on E: Exception do
    begin
      // Handle any exceptions here (optional: log the error)
    end;
  end;

  Reg.Free; // Free the registry object
end;


end.
