//---------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the MIT License
//---------------------------------------------------------------------

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
  mainform
  {$IFDEF Windows}
  ,Registry
  {$ENDIF}
  {$IFDEF Linux}
  ,Unix
  {$ENDIF}
  ;

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

function SetFileTypeIcon(const Ext: string; IconIndex: integer): boolean;

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

function SetFileTypeIcon(const Ext: string; IconIndex: integer): boolean;
var
  AppPath: string;
  {$IFDEF Windows}
  Reg: TRegistry;
  IconPath: string;
  {$ENDIF}
  {$IFDEF Linux}
  ThemeFile: TextFile;
  MimeFile: TextFile;
  DesktopFile: TextFile;
  MimeType: string;
  UserHome: string;
  {$ENDIF}
  {$IFDEF MacOS}
  PlistFile: TextFile;
  BundlePath: string;
  UserHome: string;
  {$ENDIF}

  {$IFDEF Linux}
  procedure SaveIconFromResources(const ResName, OutputPath: string);
  var
    ResourceStream: TResourceStream;
    FileStream: TFileStream;
  begin
    try
      // Open the resource stream (ResName is the name of the resource, e.g., "icon.png")
      ResourceStream := TResourceStream.Create(HInstance, ResName, RT_GROUP_ICON);
      try
        // Create the output file
        FileStream := TFileStream.Create(OutputPath, fmCreate);
        try
          // Copy the content of the resource to the file
          FileStream.CopyFrom(ResourceStream, ResourceStream.Size);
        finally
          FileStream.Free; // Free the file stream
        end;
      finally
        ResourceStream.Free; // Free the resource stream
      end;
      Writeln('Icon successfully saved to: ', OutputPath); // Success message
    except
      on E: Exception do
        Writeln('Error while saving the icon: ', E.Message); // Error message
    end;
  end;
  {$ENDIF}
begin
  Result := False; // Initialize result to false

  {$IFDEF Windows}
  try
    Reg := TRegistry.Create;
    AppPath := Application.ExeName;
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // Create a key for the file extension
    if Reg.OpenKey(Ext, True) then
    begin
      Reg.WriteString('', 'notetask'); // Assign the class name
      Reg.CloseKey;
    end;

    // Create a key for Notetask
    if Reg.OpenKey('notetask\DefaultIcon', True) then
    begin
      IconPath := Format('%s,%d', [AppPath, IconIndex]);
      Reg.WriteString('', IconPath); // Set the icon path
      Reg.CloseKey;
    end;

    // Create a key for opening the file
    if Reg.OpenKey('notetask\shell\open\command', True) then
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
  {$ENDIF}

  {$IFDEF Linux}
  try
    AppPath := Application.ExeName;
    MimeType := 'application/x-notetask';
    UserHome := GetEnvironmentVariable('HOME');

    // Create necessary directories if they do not exist
    ForceDirectories(UserHome + '/.local/share/mime/packages/');
    ForceDirectories(UserHome + '/.local/share/applications/');
    ForceDirectories(UserHome + '/.local/share/icons/notetask');

    SaveIconFromResources('TASKDOC', UserHome + '/.local/share/icons/notetask/taskdoc.ico');

    // Create the index.theme file for the icon theme
    AssignFile(ThemeFile, UserHome + '/.local/share/icons/notetask/index.theme');
    Rewrite(ThemeFile);
    Writeln(ThemeFile, '[Icon Theme]');
    Writeln(ThemeFile, 'Name=notetask');
    Writeln(ThemeFile, 'Comment=Icons for Notetask application');
    Writeln(ThemeFile, 'Inherits=default'); // Optionally specify a parent icon theme
    Writeln(ThemeFile, 'Directories=notetask');
    Writeln(ThemeFile, '');
    Writeln(ThemeFile, '[notetask]');
    Writeln(ThemeFile, 'Size=16;32;48;64;128;256'); // Specify available icon sizes
    Writeln(ThemeFile, 'Type=Fixed'); // Type can be Fixed or Scalable
    Writeln(ThemeFile, 'Icon=taskdoc.ico'); // Name of the ICO file
    CloseFile(ThemeFile);

    // Create a .xml file for MIME type
    AssignFile(MimeFile, UserHome + '/.local/share/mime/packages/x-notetask.xml');
    Rewrite(MimeFile);
    Writeln(MimeFile, '<?xml version="1.0" encoding="UTF-8"?>');
    Writeln(MimeFile, '<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">');
    Writeln(MimeFile, '  <mime-type type="', MimeType, '">');
    Writeln(MimeFile, '    <comment>Notetask file</comment>');
    Writeln(MimeFile, '    <glob pattern="*', Ext, '"/>');
    Writeln(MimeFile, '    <icon name="notetask/taskdoc.ico"/>');
    Writeln(MimeFile, '  </mime-type>');
    Writeln(MimeFile, '</mime-info>');
    CloseFile(MimeFile);

    // Create a .desktop file
    AssignFile(DesktopFile, UserHome + '/.local/share/applications/x-notetask.desktop');
    Rewrite(DesktopFile);
    Writeln(DesktopFile, '[Desktop Entry]');
    Writeln(DesktopFile, 'Name=Notetask');
    Writeln(DesktopFile, 'Exec=', AppPath, ' %f');
    //Writeln(DesktopFile, 'Icon=notetask'); // Specify the icon name or full path if necessary
    Writeln(DesktopFile, 'Type=Application');
    Writeln(DesktopFile, 'MimeType=', MimeType);
    CloseFile(DesktopFile);

    // Update MIME database
    if (FpSystem('xdg-mime install --mode user ' + UserHome + '/.local/share/mime/packages/x-notetask.xml') = 0) and
       (FpSystem('update-mime-database ' + UserHome + '/.local/share/mime') = 0) and
       (FpSystem('xdg-desktop-menu install --mode user ' + UserHome + '/.local/share/applications/x-notetask.desktop') = 0) and
       (FpSystem('gtk-update-icon-cache '+UserHome+'/.local/share/icons/notetask/ -f') = 0) then
    begin
      Result := True; // Indicate success
    end
    else
    begin
      // Log error or handle failure
      Writeln('Error updating MIME database or desktop menu.');
    end;
  except
    on E: Exception do
    begin
      Writeln('Error: ', E.Message); // Print the error message for diagnosis
      Exit;
    end;
  end;
  {$ENDIF}

  {$IFDEF MacOS}
  try
    AppPath := Application.ExeName;
    UserHome := GetEnvironmentVariable('HOME');
    BundlePath := UserHome + '/Library/Application Support/Notetask'; // Define a bundle path for the app

    // Create directory for app support if it does not exist
    if not DirectoryExists(BundlePath) then
      CreateDir(BundlePath);

    // Create a .plist file for the application
    AssignFile(PlistFile, BundlePath + '/com.example.notetask.plist'); // Adjust the bundle identifier as needed
    Rewrite(PlistFile);
    Writeln(PlistFile, '<?xml version="1.0" encoding="UTF-8"?>');
    Writeln(PlistFile, '<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">');
    Writeln(PlistFile, '<plist version="1.0">');
    Writeln(PlistFile, '<dict>');
    Writeln(PlistFile, '  <key>CFBundleTypeDeclarations</key>');
    Writeln(PlistFile, '  <array>');
    Writeln(PlistFile, '    <dict>');
    Writeln(PlistFile, '      <key>CFBundleTypeName</key>');
    Writeln(PlistFile, '      <string>Notetask file</string>');
    Writeln(PlistFile, '      <key>CFBundleTypeRole</key>');
    Writeln(PlistFile, '      <string>Editor</string>');
    Writeln(PlistFile, '      <key>LSItemContentTypes</key>');
    Writeln(PlistFile, '      <array>');
    Writeln(PlistFile, '        <string>public.data</string>'); // Adjust the content type as needed
    Writeln(PlistFile, '      </array>');
    Writeln(PlistFile, '      <key>LSHandlerRank</key>');
    Writeln(PlistFile, '      <string>Owner</string>');
    Writeln(PlistFile, '      <key>CFBundleTypeIconFile</key>');
    Writeln(PlistFile, '      <string>your_icon.icns</string>'); // Replace with your icon file
    Writeln(PlistFile, '    </dict>');
    Writeln(PlistFile, '  </array>');
    Writeln(PlistFile, '</dict>');
    Writeln(PlistFile, '</plist>');
    CloseFile(PlistFile);


    // Associate the file extension with the application
    FpSystem(Format('duti -s com.example.notetask .%s public.data', [Ext])); // Adjust the bundle identifier as needed

    Result := True; // Set result to true if all operations succeeded
  except
    on E: Exception do
    begin
      // Handle file creation error
      Exit;
    end;
  end;
  {$ENDIF}
end;

end.
