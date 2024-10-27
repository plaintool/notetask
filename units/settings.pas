//-----------------------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the GNU General Public License, Version 3 (GPL-3.0)
//  You may obtain a copy of the License at https://www.gnu.org/licenses/gpl-3.0.html
//-----------------------------------------------------------------------------------

unit settings;

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

uses
  Forms,
  Classes,
  SysUtils,
  fpjson,
  Grids,
  Graphics,
  mainform;

type
  TGridSettings = record
    ColumnWidths: array of integer;
    RowHeights: array of integer;
  end;

procedure SaveFormSettings(Form: TformNotetask);

function LoadFormSettings(Form: TformNotetask): boolean;

procedure SaveGridSettings(Grid: TStringGrid);

function LoadColumnWidthsFromFile: TJSONArray;

function LoadGridSettings(Grid: TStringGrid): boolean;

implementation

uses systemtool;

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
    JSONObj.Add('BidiRightToLeft', Form.BiDiRightToLeft);
    JSONObj.Add('ShowArchived', Form.ShowArchived);
    JSONObj.Add('ShowStatusBar', Form.ShowStatusBar);
    JSONObj.Add('ShowDuration', Form.ShowDuration);
    JSONObj.Add('ShowColumnDone', Form.ShowColumnDone);
    JSONObj.Add('ShowColumnComment', Form.ShowColumnComment);
    JSONObj.Add('ShowColumnDate', Form.ShowColumnDate);
    JSONObj.Add('ShowColumnAmount', Form.ShowColumnAmount);
    JSONObj.Add('ShowColumnFavorite', Form.ShowColumnFavorite);
    JSONObj.Add('WindowState', Ord(Form.WindowState));
    JSONObj.Add('Left', Form.RestoredLeft);
    JSONObj.Add('Top', Form.RestoredTop);
    JSONObj.Add('Width', Form.RestoredWidth);
    JSONObj.Add('Height', Form.RestoredHeight);
    JSONObj.Add('Language', Language);

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
  FileContent := string.Empty;
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

      if JSONObj.FindPath('BidiRightToLeft') <> nil then
        Form.BiDiRightToLeft := JSONObj.FindPath('BidiRightToLeft').AsBoolean;

      if JSONObj.FindPath('ShowArchived') <> nil then
        Form.FShowArchived := JSONObj.FindPath('ShowArchived').AsBoolean;

      if JSONObj.FindPath('ShowDuration') <> nil then
        Form.FShowDuration := JSONObj.FindPath('ShowDuration').AsBoolean;

      if JSONObj.FindPath('ShowColumnDone') <> nil then
        Form.FShowColumnDone := JSONObj.FindPath('ShowColumnDone').AsBoolean;

      if JSONObj.FindPath('ShowColumnComment') <> nil then
        Form.FShowColumnComment := JSONObj.FindPath('ShowColumnComment').AsBoolean;

      if JSONObj.FindPath('ShowColumnDate') <> nil then
        Form.FShowColumnDate := JSONObj.FindPath('ShowColumnDate').AsBoolean;

      if JSONObj.FindPath('ShowColumnAmount') <> nil then
        Form.FShowColumnAmount := JSONObj.FindPath('ShowColumnAmount').AsBoolean;

      if JSONObj.FindPath('ShowColumnFavorite') <> nil then
        Form.FShowColumnFavorite := JSONObj.FindPath('ShowColumnFavorite').AsBoolean;

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

      if JSONObj.FindPath('Language') <> nil then
      begin
        if (JSONObj.FindPath('Language').AsString <> string.Empty) and (Language <> JSONObj.FindPath('Language').AsString) then
        begin
          Language := JSONObj.FindPath('Language').AsString;
          Form.SetLanguage(Language);
        end;
      end;

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
  ColumnArray: TJSONArray;
  SavedArray: TJSONArray;
  // RowArray: TJSONArray;
  i: integer;
  FileName: string;
begin
  FileName := GetSettingsDirectory('grid_settings.json'); // Get settings file name
  ForceDirectories(GetSettingsDirectory);
  JSONObj := TJSONObject.Create;
  ColumnArray := TJSONArray.Create;
  // RowArray := TJSONArray.Create;

  // Save column widths
  SavedArray := LoadColumnWidthsFromFile;
  for i := 0 to Grid.ColCount - 1 do
  begin
    if (Grid.ColWidths[i] > 0) then
      ColumnArray.Add(Grid.ColWidths[i])
    else
    begin
      if (Assigned(SavedArray)) then
        ColumnArray.Add(SavedArray[i]);
    end;
  end;

  // Save row heights
  //  for i := 0 to Grid.RowCount - 1 do
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

function LoadColumnWidthsFromFile: TJSONArray;
var
  JSONData: TJSONData;
  JSONObj: TJSONObject;
  ColumnArray: TJSONArray;
  FileContent: string;
  FileStream: TFileStream;
  FileName: string;
begin
  Result := nil;
  FileContent := string.Empty;
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
      Result := ColumnArray.Clone as TJSONArray; // Clone to return a copy
    finally
      JSONData.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

function LoadGridSettings(Grid: TStringGrid): boolean;
var
  ColumnArray: TJSONArray;
  i: integer;
begin
  Result := False;

  // Get column widths array from file
  ColumnArray := LoadColumnWidthsFromFile;

  if ColumnArray = nil then Exit;

  // Set column widths
  for i := 1 to ColumnArray.Count - 1 do
    Grid.ColWidths[i] := ColumnArray.Items[i].AsInteger;

  Result := True;
end;

end.
