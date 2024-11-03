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

procedure SaveGridSettings(Form: TformNotetask; Grid: TStringGrid; Item: string);

function LoadGridSettings(Form: TformNotetask; Grid: TStringGrid; Item: string): boolean;

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
    if (Form.WindowState in [wsMaximized, wsMinimized]) then
    begin
      JSONObj.Add('Left', Form.RestoredLeft);
      JSONObj.Add('Top', Form.RestoredTop);
      JSONObj.Add('Width', Form.RestoredWidth);
      JSONObj.Add('Height', Form.RestoredHeight);
    end
    else
    begin
      JSONObj.Add('Left', Form.Left);
      JSONObj.Add('Top', Form.Top);
      JSONObj.Add('Width', Form.Width);
      JSONObj.Add('Height', Form.Height);
    end;
    JSONObj.Add('WindowState', Ord(Form.WindowState));
    JSONObj.Add('WordWrap', Form.WordWrap);
    JSONObj.Add('BidiRightToLeft', Form.BiDiRightToLeft);
    JSONObj.Add('ShowStatusBar', Form.ShowStatusBar);
    JSONObj.Add('ShowArchived', Form.ShowArchived);
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
      if JSONObj.FindPath('Left') <> nil then
        Form.Left := JSONObj.FindPath('Left').AsInteger;

      if JSONObj.FindPath('Top') <> nil then
        Form.Top := JSONObj.FindPath('Top').AsInteger;

      if JSONObj.FindPath('Width') <> nil then
        Form.Width := JSONObj.FindPath('Width').AsInteger;

      if JSONObj.FindPath('Height') <> nil then
        Form.Height := JSONObj.FindPath('Height').AsInteger;

      if JSONObj.FindPath('WindowState') <> nil then
        Form.WindowState := TWindowState(JSONObj.FindPath('WindowState').AsInteger);

      if JSONObj.FindPath('WordWrap') <> nil then
        Form.WordWrap := JSONObj.FindPath('WordWrap').AsBoolean;

      if JSONObj.FindPath('BidiRightToLeft') <> nil then
        Form.BiDiRightToLeft := JSONObj.FindPath('BidiRightToLeft').AsBoolean;

      if JSONObj.FindPath('ShowArchived') <> nil then
        Form.FShowArchived := JSONObj.FindPath('ShowArchived').AsBoolean;

      if JSONObj.FindPath('ShowStatusBar') <> nil then
        Form.ShowStatusBar := JSONObj.FindPath('ShowStatusBar').AsBoolean;

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

procedure SaveGridSettings(Form: TformNotetask; Grid: TStringGrid; Item: string);
var
  ItemSettings: TJSONObject;
  JSONFile: TJSONObject;
  ExistingItem: TJSONData;
  FileName: string;
  FileStream: TFileStream;
begin
  FileName := GetSettingsDirectory('grid_settings.json'); // Get settings file name
  Item := Item.ToLower;
  ForceDirectories(GetSettingsDirectory); // Ensure the directory exists

  // Load the existing JSON file or create a new JSON object
  if FileExists(FileName) then
  begin
    FileStream := TFileStream.Create(FileName, fmOpenRead);
    try
      JSONFile := TJSONObject(GetJSON(FileStream));
    finally
      FileStream.Free;
    end;
  end
  else
    JSONFile := TJSONObject.Create;

  try
    // Create a JSON object for the specified Item settings
    ItemSettings := TJSONObject.Create;
    ItemSettings.Add('ShowDuration', Form.ShowDuration);
    ItemSettings.Add('ShowColumnDone', Form.ShowColumnDone);
    ItemSettings.Add('ShowColumnTask', Form.ShowColumnTask);
    ItemSettings.Add('ShowColumnComment', Form.ShowColumnComment);
    ItemSettings.Add('ShowColumnDate', Form.ShowColumnDate);
    ItemSettings.Add('ShowColumnAmount', Form.ShowColumnAmount);
    ItemSettings.Add('ShowColumnFavorite', Form.ShowColumnFavorite);

    // Add column widths if the columns are visible
    if Grid.Columns[0].Visible then
      ItemSettings.Add('ColumnDone', Grid.Columns[0].Width);
    if Grid.Columns[1].Visible then
      ItemSettings.Add('ColumnTask', Grid.Columns[1].Width);
    if Grid.Columns[2].Visible then
      ItemSettings.Add('ColumnComment', Grid.Columns[2].Width);
    if Grid.Columns[3].Visible then
      ItemSettings.Add('ColumnDate', Grid.Columns[3].Width);
    if Grid.Columns[4].Visible then
      ItemSettings.Add('ColumnAmount', Grid.Columns[4].Width);
    if Grid.Columns[5].Visible then
      ItemSettings.Add('ColumnFavorite', Grid.Columns[5].Width);

    ItemSettings.Add('SortColumn', Form.SortColumn);
    ItemSettings.Add('SortOrder', Ord(Form.SortOrder));

    ExistingItem := JSONFile.Find(Item); // Find the existing Item by key
    if Assigned(ExistingItem) then
      JSONFile.Remove(ExistingItem); // Remove the existing Item if found

    JSONFile.Add(Item, ItemSettings); // Add the new or updated Item settings

    // Write the JSON object back to the file
    FileStream := TFileStream.Create(FileName, fmCreate);
    try
      FileStream.Write(Pointer(JSONFile.AsJSON)^, Length(JSONFile.AsJSON));
    finally
      FileStream.Free;
    end;
  finally
    JSONFile.Free;
  end;
end;

function LoadGridSettings(Form: TformNotetask; Grid: TStringGrid; Item: string): boolean;
var
  JSONData: TJSONData;
  JSONObj, ItemSettings: TJSONObject;
  FileContent: string;
  FileStream: TFileStream;
  FileName: string;
begin
  Result := False;
  FileContent := '';
  FileName := GetSettingsDirectory('grid_settings.json'); // Get settings file name
  Item := Item.ToLower;
  ForceDirectories(GetSettingsDirectory); // Ensure the directory exists
  if not FileExists(FileName) then Exit;

  // Read from the settings file
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(FileContent, FileStream.Size);
    FileStream.Read(Pointer(FileContent)^, FileStream.Size);
    JSONData := GetJSON(FileContent);
    try
      JSONObj := JSONData as TJSONObject;

      // Attempt to find settings for the specified Item
      if JSONObj.Find(Item) = nil then Item := string.Empty;
      if JSONObj.Find(Item) = nil then exit;
      ItemSettings := JSONObj.Find(Item) as TJSONObject;

      // Load settings into the form and grid from the ItemSettings object
      if ItemSettings.FindPath('ShowDuration') <> nil then
        Form.FShowDuration := ItemSettings.FindPath('ShowDuration').AsBoolean;

      if ItemSettings.FindPath('ShowColumnDone') <> nil then
        Form.FShowColumnDone := ItemSettings.FindPath('ShowColumnDone').AsBoolean;

      if ItemSettings.FindPath('ShowColumnTask') <> nil then
        Form.FShowColumnTask := ItemSettings.FindPath('ShowColumnTask').AsBoolean;

      if ItemSettings.FindPath('ShowColumnComment') <> nil then
        Form.FShowColumnComment := ItemSettings.FindPath('ShowColumnComment').AsBoolean;

      if ItemSettings.FindPath('ShowColumnDate') <> nil then
        Form.FShowColumnDate := ItemSettings.FindPath('ShowColumnDate').AsBoolean;

      if ItemSettings.FindPath('ShowColumnAmount') <> nil then
        Form.FShowColumnAmount := ItemSettings.FindPath('ShowColumnAmount').AsBoolean;

      if ItemSettings.FindPath('ShowColumnFavorite') <> nil then
        Form.FShowColumnFavorite := ItemSettings.FindPath('ShowColumnFavorite').AsBoolean;

      if ItemSettings.FindPath('ColumnDone') <> nil then
        Grid.Columns[0].Width := ItemSettings.FindPath('ColumnDone').AsInteger;

      if ItemSettings.FindPath('ColumnTask') <> nil then
        Grid.Columns[1].Width := ItemSettings.FindPath('ColumnTask').AsInteger;

      if ItemSettings.FindPath('ColumnComment') <> nil then
        Grid.Columns[2].Width := ItemSettings.FindPath('ColumnComment').AsInteger;

      if ItemSettings.FindPath('ColumnDate') <> nil then
        Grid.Columns[3].Width := ItemSettings.FindPath('ColumnDate').AsInteger;

      if ItemSettings.FindPath('ColumnAmount') <> nil then
        Grid.Columns[4].Width := ItemSettings.FindPath('ColumnAmount').AsInteger;

      if ItemSettings.FindPath('ColumnFavorite') <> nil then
        Grid.Columns[5].Width := ItemSettings.FindPath('ColumnFavorite').AsInteger;

      if ItemSettings.FindPath('SortColumn') <> nil then
        Form.SortColumn := ItemSettings.FindPath('SortColumn').AsInteger;

      if ItemSettings.FindPath('SortOrder') <> nil then
      begin
        Form.SortOrder := TSortOrder(ItemSettings.FindPath('SortOrder').AsInteger);
      end;
    finally
      JSONData.Free;
    end;
  finally
    FileStream.Free;
  end;
  Result := True;
end;

end.
