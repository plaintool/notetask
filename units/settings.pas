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
  Grids,
  Graphics,
  Printers,
  fpjson,
  mainform,
  TagEdit;

type
  TGridSettings = record
    ColumnWidths: array of integer;
    RowHeights: array of integer;
  end;

procedure SaveFormSettings(Form: TformNotetask; TagEdit: TTagEdit);

function LoadFormSettings(Form: TformNotetask; TagEdit: TTagEdit): boolean;

procedure SaveGridSettings(Form: TformNotetask; Grid: TStringGrid; Item: string);

function LoadGridSettings(Form: TformNotetask; Grid: TStringGrid; Item: string): boolean;

implementation

uses systemtool;

function GetSettingsDirectory(fileName: string = ''): string;
  {$IFDEF Windows}
var
  baseDir: string;
  {$ENDIF}
begin
  {$IFDEF Windows}
  // LOCALAPPDATA does not exist on Windows XP, fallback to APPDATA
  baseDir := GetEnvironmentVariable('LOCALAPPDATA');
  if baseDir = '' then
    baseDir := GetEnvironmentVariable('APPDATA');

  Result := IncludeTrailingPathDelimiter(baseDir) + 'notetask\' + fileName;
  {$ELSE}
  Result := IncludeTrailingPathDelimiter(GetUserDir) + '.config/notetask/' + filename;
  {$ENDIF}
end;

procedure SaveFormSettings(Form: TformNotetask; TagEdit: TTagEdit);
var
  FileName: string;
  JSONObj: TJSONObject;
  TagArr: TJSONArray;
  ItemObj: TJSONObject;
  i: integer;
begin
  FileName := GetSettingsDirectory('form_settings.json'); // Get settings file name
  ForceDirectories(GetSettingsDirectory); // Ensure the directory exists
  JSONObj := TJSONObject.Create;
  try
    // Save form position and size
    if (Form.WindowState in [wsMaximized, wsMinimized]) then
    begin
      JSONObj.Add('Left', Form.ScaleFormTo96(Form.RestoredLeft));
      JSONObj.Add('Top', Form.ScaleFormTo96(Form.RestoredTop));
      JSONObj.Add('Width', Form.ScaleFormTo96(Form.RestoredWidth));
      JSONObj.Add('Height', Form.ScaleFormTo96(Form.RestoredHeight));
    end
    else
    begin
      JSONObj.Add('Left', Form.ScaleFormTo96(Form.Left));
      JSONObj.Add('Top', Form.ScaleFormTo96(Form.Top));
      JSONObj.Add('Width', Form.ScaleFormTo96(Form.Width));
      JSONObj.Add('Height', Form.ScaleFormTo96(Form.Height));
    end;
    JSONObj.Add('WindowState', Ord(Form.WindowState));
    JSONObj.Add('WordWrap', Form.WordWrap);
    JSONObj.Add('EnterSubmit', Form.EnterSubmit);
    JSONObj.Add('BidiRightToLeft', Form.BiDiRightToLeft);
    JSONObj.Add('Language', Language);

    // Save font
    JSONObj.Add('FontName', Form.Font.Name);
    JSONObj.Add('FontSize', Form.Font.Size);
    JSONObj.Add('FontStyle', integer(Form.Font.Style));  // Convert font style to number
    JSONObj.Add('FontCharset', Form.Font.Charset);
    JSONObj.Add('FontColor', Form.Font.Color);
    JSONObj.Add('FontPitch', Ord(Form.Font.Pitch));

    // Printer
    JSONObj.Add('PrinterPaperName', Printer.PaperSize.PaperName);
    if Printer.Orientation = poLandscape then
      JSONObj.Add('PrinterOrientation', 'Landscape')
    else
      JSONObj.Add('PrinterOrientation', 'Portrait');
    JSONObj.Add('PrinterMarginLeft', Form.pageSetupDialog.MarginLeft);
    JSONObj.Add('PrinterMarginRight', Form.pageSetupDialog.MarginRight);
    JSONObj.Add('PrinterMarginTop', Form.pageSetupDialog.MarginTop);
    JSONObj.Add('PrinterMarginBottom', Form.pageSetupDialog.MarginBottom);

    // Save Tag Colors
    TagArr := TJSONArray.Create; // JSON array for colors

    // iterate TagItems
    for i := 0 to TagEdit.TagColors.Count - 1 do
    begin
      ItemObj := TJSONObject.Create;
      // store tag name
      ItemObj.Add('Tag', TagEdit.TagColors[i].TagName);
      // store color as integer
      ItemObj.Add('Color', TagEdit.TagColors[i].Color);
      TagArr.Add(ItemObj);
    end;
    JSONObj.Add('TagColors', TagArr);

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

function LoadFormSettings(Form: TformNotetask; TagEdit: TTagEdit): boolean;
var
  JSONData: TJSONData;
  JSONObj: TJSONObject;
  FileName: string;
  FileStream: TFileStream;
  FileContent: string;
  TagData: TJSONData;
  TagArr: TJSONArray;
  ItemObj: TJSONObject;
  TagStr: string;
  ColVal: TColor;
  i: integer;
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
        Form.Left := Form.Scale96ToForm(JSONObj.FindPath('Left').AsInteger);

      if JSONObj.FindPath('Top') <> nil then
        Form.Top := Form.Scale96ToForm(JSONObj.FindPath('Top').AsInteger);

      if JSONObj.FindPath('Width') <> nil then
        Form.Width := Form.Scale96ToForm(JSONObj.FindPath('Width').AsInteger);

      if JSONObj.FindPath('Height') <> nil then
        Form.Height := Form.Scale96ToForm(JSONObj.FindPath('Height').AsInteger);

      if JSONObj.FindPath('WindowState') <> nil then
      begin
        {$IFDEF UNIX}
        Form.FWindowStateLoaded := TWindowState(JSONObj.FindPath('WindowState').AsInteger);
        {$ELSE}
        Form.WindowState := TWindowState(JSONObj.FindPath('WindowState').AsInteger);
        {$ENDIF}
      end;

      if JSONObj.FindPath('WordWrap') <> nil then
        Form.WordWrap := JSONObj.FindPath('WordWrap').AsBoolean;

      if JSONObj.FindPath('EnterSubmit') <> nil then
        Form.EnterSubmit := JSONObj.FindPath('EnterSubmit').AsBoolean;

      if JSONObj.FindPath('BidiRightToLeft') <> nil then
        Form.BiDiRightToLeft := JSONObj.FindPath('BidiRightToLeft').AsBoolean;

      if JSONObj.FindPath('Language') <> nil then
      begin
        if (JSONObj.FindPath('Language').AsString <> string.Empty) and (Language <> JSONObj.FindPath('Language').AsString) then
          Language := JSONObj.FindPath('Language').AsString;
      end;

      // Printer
      if JSONObj.FindPath('PrinterPaperName') <> nil then
        Printer.PaperSize.PaperName := JSONObj.FindPath('PrinterPaperName').AsString;
      if JSONObj.FindPath('PrinterOrientation') <> nil then
      begin
        if JSONObj.FindPath('PrinterOrientation').AsString = 'Landscape' then
          Printer.Orientation := poLandscape
        else
          Printer.Orientation := poPortrait;
      end;
      if JSONObj.FindPath('PrinterMarginLeft') <> nil then
        Form.pageSetupDialog.MarginLeft := JSONObj.FindPath('PrinterMarginLeft').AsInteger;
      if JSONObj.FindPath('PrinterMarginRight') <> nil then
        Form.pageSetupDialog.MarginRight := JSONObj.FindPath('PrinterMarginRight').AsInteger;
      if JSONObj.FindPath('PrinterMarginTop') <> nil then
        Form.pageSetupDialog.MarginTop := JSONObj.FindPath('PrinterMarginTop').AsInteger;
      if JSONObj.FindPath('PrinterMarginBottom') <> nil then
        Form.pageSetupDialog.MarginBottom := JSONObj.FindPath('PrinterMarginBottom').AsInteger;

      // Check and load font properties
      if JSONObj.FindPath('FontName') <> nil then
        Form.Font.Name := JSONObj.FindPath('FontName').AsString;
      if JSONObj.FindPath('FontSize') <> nil then
        Form.Font.Size := JSONObj.FindPath('FontSize').AsInteger;
      if JSONObj.FindPath('FontStyle') <> nil then
        Form.Font.Style := TFontStyles(JSONObj.FindPath('FontStyle').AsInteger); // Convert integer back to TFontStyles
      if JSONObj.FindPath('FontCharset') <> nil then
        Form.Font.Charset := JSONObj.FindPath('FontCharset').AsInteger;
      if JSONObj.FindPath('FontColor') <> nil then
        Form.Font.Color := JSONObj.FindPath('FontColor').AsInteger;
      if JSONObj.FindPath('FontPitch') <> nil then
        Form.Font.Pitch := TFontPitch(JSONObj.FindPath('FontPitch').AsInteger);

      // Load Tag Colors
      TagData := JSONObj.FindPath('TagColors');  // safe lookup
      if (TagData <> nil) and (TagData.JSONType = jtArray) then
      begin
        TagArr := TJSONArray(TagData);
        TagEdit.TagColors.Clear;

        for i := 0 to TagArr.Count - 1 do
        begin
          if TagArr.Items[i] is TJSONObject then
          begin
            ItemObj := TJSONObject(TagArr.Items[i]);

            // read tag name
            if ItemObj.Find('Tag') <> nil then
              TagStr := ItemObj.Strings['Tag']
            else
              TagStr := '';

            // read color
            if ItemObj.Find('Color') <> nil then
              ColVal := ItemObj.Integers['Color']
            else
              ColVal := clNone;

            TagEdit.TagColors.Add(TagStr, ColVal);
          end;
        end;
      end;

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
  RowsArray: TJSONArray;
  RectObj: TJSONObject;
  i: integer;
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
    ItemSettings.Add('ShowTime', Form.ShowTime);
    ItemSettings.Add('ShowTags', Form.ShowTags);
    ItemSettings.Add('ShowNote', Form.ShowNote);
    ItemSettings.Add('HideNoteText', Form.HideNoteText);
    ItemSettings.Add('ShowStatusBar', Form.ShowStatusBar);
    ItemSettings.Add('ShowArchived', Form.ShowArchived);
    ItemSettings.Add('NoteHeight', Form.ScaleFormTo96(Form.panelNote.Height));
    ItemSettings.Add('TagsHeight', Form.ScaleFormTo96(Form.panelTags.Height));
    ItemSettings.Add('FilterWidth', Form.ScaleFormTo96(Form.filterBox.Width));
    ItemSettings.Add('ShowColumnDone', Form.ShowColumnDone);
    ItemSettings.Add('ShowColumnTask', Form.ShowColumnTask);
    ItemSettings.Add('ShowColumnNote', Form.ShowColumnNote);
    ItemSettings.Add('ShowColumnDate', Form.ShowColumnDate);
    ItemSettings.Add('ShowColumnAmount', Form.ShowColumnAmount);
    ItemSettings.Add('ShowColumnFavorite', Form.ShowColumnFavorite);
    ItemSettings.Add('Scale', Form.Zoom);

    // Add column widths if the columns are visible
    if Grid.Columns[0].Visible then
      ItemSettings.Add('ColumnDone', Form.ScaleFormTo96(Grid.Columns[0].Width));
    if Grid.Columns[1].Visible then
      ItemSettings.Add('ColumnTask', Form.ScaleFormTo96(Grid.Columns[1].Width));
    if Grid.Columns[2].Visible then
      ItemSettings.Add('ColumnNote', Form.ScaleFormTo96(Grid.Columns[2].Width));
    if Grid.Columns[3].Visible then
      ItemSettings.Add('ColumnDate', Form.ScaleFormTo96(Grid.Columns[3].Width));
    if Grid.Columns[4].Visible then
      ItemSettings.Add('ColumnAmount', Form.ScaleFormTo96(Grid.Columns[4].Width));
    if Grid.Columns[5].Visible then
      ItemSettings.Add('ColumnFavorite', Form.ScaleFormTo96(Grid.Columns[5].Width));

    // Sorting
    if (Item <> string.Empty) then
    begin
      ItemSettings.Add('SortColumn', Form.SortColumn);
      ItemSettings.Add('SortOrder', Ord(Form.SortOrder));
    end;

    // Save selected Tab
    ItemSettings.Add('SelectedTab', Form.SelectedTab);

    // Save selected Row
    ItemSettings.Add('SelectedRow', Form.SelectedRow);

    // Save grid selection
    RectObj := TJSONObject.Create;
    RectObj.Add('Left', Form.Selection.Left);
    RectObj.Add('Top', Form.Selection.Top);
    RectObj.Add('Right', Form.Selection.Right);
    RectObj.Add('Bottom', Form.Selection.Bottom);
    ItemSettings.Add('SelectionRect', RectObj);

    // Save selected Rows
    RowsArray := TJSONArray.Create;
    for i := Low(Form.SelectedRows) to High(Form.SelectedRows) do
      RowsArray.Add(Form.SelectedRows[i]);
    ItemSettings.Add('SelectedRows', RowsArray);

    // Save memo note scroll position
    ItemSettings.Add('MemoNoteScroll', Form.ScaleFormTo96(Form.MemoNoteScroll));

    // Save memo note SelStart
    ItemSettings.Add('MemoNoteSelStart', Form.MemoNoteSelStart);

    // Save memo note SelLength
    ItemSettings.Add('MemoNoteSelLength', Form.MemoNoteSelLength);

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
  RowsArrayJSON: TJSONArray;
  TempArr: array of integer;
  i: integer;
begin
  Result := False;
  FileContent := string.Empty;
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

      if ItemSettings.FindPath('ShowTime') <> nil then
        Form.FShowTime := ItemSettings.FindPath('ShowTime').AsBoolean;

      if ItemSettings.FindPath('ShowTags') <> nil then
        Form.FShowTags := ItemSettings.FindPath('ShowTags').AsBoolean;

      if ItemSettings.FindPath('ShowNote') <> nil then
        Form.FShowNote := ItemSettings.FindPath('ShowNote').AsBoolean;

      if ItemSettings.FindPath('HideNoteText') <> nil then
        Form.FHideNoteText := ItemSettings.FindPath('HideNoteText').AsBoolean;

      if ItemSettings.FindPath('NoteHeight') <> nil then
        Form.panelNote.Height := Form.Scale96ToForm(ItemSettings.FindPath('NoteHeight').AsInteger);

      if ItemSettings.FindPath('TagsHeight') <> nil then
        Form.panelTags.Height := Form.Scale96ToForm(ItemSettings.FindPath('TagsHeight').AsInteger);

      if ItemSettings.FindPath('FilterWidth') <> nil then
        Form.filterBox.Width := Form.Scale96ToForm(ItemSettings.FindPath('FilterWidth').AsInteger);

      if ItemSettings.FindPath('ShowArchived') <> nil then
        Form.FShowArchived := ItemSettings.FindPath('ShowArchived').AsBoolean;

      if ItemSettings.FindPath('ShowStatusBar') <> nil then
        Form.FShowStatusBar := ItemSettings.FindPath('ShowStatusBar').AsBoolean;

      if ItemSettings.FindPath('ShowColumnDone') <> nil then
        Form.FShowColumnDone := ItemSettings.FindPath('ShowColumnDone').AsBoolean;

      if ItemSettings.FindPath('ShowColumnTask') <> nil then
        Form.FShowColumnTask := ItemSettings.FindPath('ShowColumnTask').AsBoolean;

      if ItemSettings.FindPath('ShowColumnNote') <> nil then
        Form.FShowColumnNote := ItemSettings.FindPath('ShowColumnNote').AsBoolean;

      if ItemSettings.FindPath('ShowColumnDate') <> nil then
        Form.FShowColumnDate := ItemSettings.FindPath('ShowColumnDate').AsBoolean;

      if ItemSettings.FindPath('ShowColumnAmount') <> nil then
        Form.FShowColumnAmount := ItemSettings.FindPath('ShowColumnAmount').AsBoolean;

      if ItemSettings.FindPath('ShowColumnFavorite') <> nil then
        Form.FShowColumnFavorite := ItemSettings.FindPath('ShowColumnFavorite').AsBoolean;

      if ItemSettings.FindPath('Scale') <> nil then
        Form.FZoom := ItemSettings.FindPath('Scale').AsFloat;

      if ItemSettings.FindPath('ColumnDone') <> nil then
        Grid.Columns[0].Width := Form.Scale96ToForm(ItemSettings.FindPath('ColumnDone').AsInteger);

      if ItemSettings.FindPath('ColumnTask') <> nil then
        Grid.Columns[1].Width := Form.Scale96ToForm(ItemSettings.FindPath('ColumnTask').AsInteger);

      if ItemSettings.FindPath('ColumnNote') <> nil then
        Grid.Columns[2].Width := Form.Scale96ToForm(ItemSettings.FindPath('ColumnNote').AsInteger);

      if ItemSettings.FindPath('ColumnDate') <> nil then
        Grid.Columns[3].Width := Form.Scale96ToForm(ItemSettings.FindPath('ColumnDate').AsInteger);

      if ItemSettings.FindPath('ColumnAmount') <> nil then
        Grid.Columns[4].Width := Form.Scale96ToForm(ItemSettings.FindPath('ColumnAmount').AsInteger);

      if ItemSettings.FindPath('ColumnFavorite') <> nil then
        Grid.Columns[5].Width := Form.Scale96ToForm(ItemSettings.FindPath('ColumnFavorite').AsInteger);

      // Load selected Tab
      if ItemSettings.FindPath('SelectedTab') <> nil then
        Form.SelectedTab := ItemSettings.FindPath('SelectedTab').AsInteger;

      // Load selected Row
      if ItemSettings.FindPath('SelectedRow') <> nil then
        Form.SelectedRow := ItemSettings.FindPath('SelectedRow').AsInteger;

      // Load selection Rect
      if ItemSettings.FindPath('SelectionRect') <> nil then
      begin
        with ItemSettings.FindPath('SelectionRect') as TJSONObject do
          Form.Selection := Rect(Get('Left', 0), Get('Top', 0), Get('Right', 0), Get('Bottom', 0));
      end;

      // Load selected Rows
      if ItemSettings.FindPath('SelectedRows') <> nil then
      begin
        TempArr := [];
        RowsArrayJSON := ItemSettings.FindPath('SelectedRows') as TJSONArray;
        SetLength(TempArr, RowsArrayJSON.Count);
        for i := 0 to RowsArrayJSON.Count - 1 do
          TempArr[i] := RowsArrayJSON.Items[i].AsInteger;
        Form.SelectedRows := TempArr;
      end;

      // Load memo note scroll position
      if ItemSettings.FindPath('MemoNoteScroll') <> nil then
        Form.MemoNoteScroll := Form.Scale96ToForm(ItemSettings.FindPath('MemoNoteScroll').AsInteger);

      // Load memo note SelStart
      if ItemSettings.FindPath('MemoNoteSelStart') <> nil then
        Form.MemoNoteSelStart := ItemSettings.FindPath('MemoNoteSelStart').AsInteger;

      // Load memo note SelLength
      if ItemSettings.FindPath('MemoNoteSelLength') <> nil then
        Form.MemoNoteSelLength := ItemSettings.FindPath('MemoNoteSelLength').AsInteger;

      if (Item <> string.Empty) then
      begin
        if ItemSettings.FindPath('SortColumn') <> nil then
          Form.SortColumn := ItemSettings.FindPath('SortColumn').AsInteger;

        if ItemSettings.FindPath('SortOrder') <> nil then
        begin
          Form.SortOrder := TSortOrder(ItemSettings.FindPath('SortOrder').AsInteger);
        end;
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
