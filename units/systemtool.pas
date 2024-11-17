//-----------------------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the GNU General Public License, Version 3 (GPL-3.0)
//  You may obtain a copy of the License at https://www.gnu.org/licenses/gpl-3.0.html
//-----------------------------------------------------------------------------------

unit systemtool;

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

uses
  Forms,
  Classes,
  SysUtils,
  gettext,
  DefaultTranslator,
  Translations,
  LResources,
  LCLTranslator
  {$IFDEF Windows}
  ,Windows
  ,Registry
  {$ENDIF}
  {$IFDEF Linux}
  ,Unix
  {$ENDIF}
  {$IFDEF MacOS}
  ,MacOSAll
  {$ENDIF}
  ;

function GetOSLanguage: string;
function ApplicationTranslate(const Language: string; AForm: TCustomForm = nil): boolean;
{$IFDEF Windows}
function IsWindowsDarkThemeEnabled: Boolean;
{$ENDIF}
function SetFileTypeIcon(const Ext: string; IconIndex: integer): boolean;

var
  Language: string;

implementation

function GetOSLanguage: string;
  {platform-independent method to read the language of the user interface}
var
  fbl: string;
  {$IFDEF Windows}
  l:string;
  {$ENDIF}
  {$IFDEF LCLCarbon}
  l:string;
  theLocaleRef: CFLocaleRef;
  locale: CFStringRef;
  buffer: StringPtr;
  bufferSize: CFIndex;
  encoding: CFStringEncoding;
  success: boolean;
  {$ENDIF}
begin
  fbl := string.Empty;
  {$IFDEF LCLCarbon}
  l := string.Empty;
  theLocaleRef := CFLocaleCopyCurrent;
  locale := CFLocaleGetIdentifier(theLocaleRef);
  encoding := 0;
  bufferSize := 256;
  buffer := new(StringPtr);
  success := CFStringGetPascalString(locale, buffer, bufferSize, encoding);
  if success then
    l := string(buffer^)
  else
    l := '';
  fbl := Copy(l, 1, 2);
  dispose(buffer);
  {$ELSE}
  {$IFDEF LINUX}
  fbl := Copy(GetEnvironmentVariable('LANG'), 1, 2);
  {$ELSE}
  l := string.Empty;
  GetLanguageIDs(l, fbl);
  {$ENDIF}
  {$ENDIF}
  Result := fbl;
end;

function ApplicationTranslate(const Language: string; AForm: TCustomForm = nil): boolean;
var
  Res: TResourceStream;
  PoStringStream: TStringStream;
  PoFile: TPOFile;
  LocalTranslator: TUpdateTranslator;
  i: integer;
begin
  Result := False;

  // Wrap in try-finally to ensure resources are freed
  Res := nil;
  PoStringStream := nil;
  PoFile := nil;
  LocalTranslator := nil;

  try
    try
      // Create string stream
      PoStringStream := TStringStream.Create('');

      // Load the resource file and save the resource to the string stream
      Res := TResourceStream.Create(HInstance, 'notetask.' + Language, RT_RCDATA);
      Res.SaveToStream(PoStringStream);

      // Read strings from the file
      PoFile := TPOFile.Create(False);
      PoFile.ReadPOText(PoStringStream.DataString);

      // Translate resource strings (this works for messagestring and resourcestring)
      if (not Assigned(AForm)) then
        Result := TranslateResourceStrings(PoFile);

      if (Result) or (Assigned(AForm)) then
      begin
        // Create a local translator for the form or forms
        LocalTranslator := TPOTranslator.Create(PoFile);
        if (Assigned(LRSTranslator)) then LRSTranslator.Free;
        LRSTranslator := LocalTranslator;

        if Assigned(AForm) then
        begin
          // Translate only the specified form
          LocalTranslator.UpdateTranslation(AForm);
        end
        else
        begin
          // Translate all forms
          for i := 0 to Screen.CustomFormCount - 1 do
            LocalTranslator.UpdateTranslation(Screen.CustomForms[i]);

          // Translate all data modules
          for i := 0 to Screen.DataModuleCount - 1 do
            LocalTranslator.UpdateTranslation(Screen.DataModules[i]);
        end;
      end;

    except
      on E: Exception do
      begin
        // Handle translation error and display message
        WriteLn('Error during translation: ', E.Message);
        Result := False; // Return False in case of error
      end;
    end;

  finally
    // Free all used resources
    if Assigned(LocalTranslator) then
    begin
      LRSTranslator := nil;
      LocalTranslator.Free;
    end
    else
    if Assigned(PoFile) then
      PoFile.Free;

    if Assigned(PoStringStream) then
      PoStringStream.Free;

    if Assigned(Res) then
      Res.Free;
  end;
end;

{$IFDEF Windows}
function IsWindowsDarkThemeEnabled: Boolean;
var
  Key: HKEY;
  Value: DWORD;
  ValueSize: DWORD;
begin
  Result := False;
  if RegOpenKeyEx(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize', 0, KEY_READ, Key) = ERROR_SUCCESS then
  begin
    ValueSize := SizeOf(Value);
    if RegQueryValueEx(Key, 'AppsUseLightTheme', nil, nil, @Value, @ValueSize) = ERROR_SUCCESS then
    begin
      Result := Value = 0; // 0 - тёмная тема, 1 - светлая тема
    end;
    RegCloseKey(Key);
  end;
end;
{$ENDIF}

function SetFileTypeIcon(const Ext: string; IconIndex: integer): boolean;
var
  AppPath: string;
  {$IFDEF Windows}
  Reg: TRegistry;
  IconPath: string;
  {$ENDIF}
  {$IFDEF Linux}
  //ThemeFile: TextFile;
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
  procedure SaveIconFromResources(const ResName, OutputPath: string; ResType: PChar = RT_RCDATA);
  var
    ResourceStream: TResourceStream;
    FileStream: TFileStream;
  begin
    try
      // Open the resource stream (ResName is the name of the resource, e.g., "icon.png")
      ResourceStream := TResourceStream.Create(HInstance, ResName, ResType);
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
    //ForceDirectories(UserHome + '/.local/share/icons/hicolor/48x48/mimetypes');

    //SaveIconFromResources('X-TASKDOC', UserHome + '/.local/share/icons/hicolor/48x48/mimetypes/x-taskdoc.png');

    // Create the index.theme file for the icon theme
    //AssignFile(ThemeFile, UserHome + '/.local/share/icons/hicolor/index.theme');
    //Rewrite(ThemeFile);
    //Writeln(ThemeFile, '[Icon Theme]');
    //Writeln(ThemeFile, 'Name=Hicolor');
    //Writeln(ThemeFile, 'Comment=Fallback icon theme');
    //Writeln(ThemeFile, 'Hidden=true');
    //Writeln(ThemeFile, 'Directories=48x48/mimetypes');
    //Writeln(ThemeFile, '');
    //Writeln(ThemeFile, '[48x48/mimetypes]');
    //Writeln(ThemeFile, 'Size=48'); // Specify available icon sizes
    //Writeln(ThemeFile, 'Type=Fixed'); // Type can be Fixed or Scalable
    //CloseFile(ThemeFile);

    // Create a .xml file for MIME type
    AssignFile(MimeFile, UserHome + '/.local/share/mime/packages/x-notetask.xml');
    Rewrite(MimeFile);
    Writeln(MimeFile, '<?xml version="1.0" encoding="UTF-8"?>');
    Writeln(MimeFile, '<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">');
    Writeln(MimeFile, '  <mime-type type="', MimeType, '">');
    Writeln(MimeFile, '    <comment>Notetask file</comment>');
    Writeln(MimeFile, '    <glob pattern="*', Ext, '"/>');
    //Writeln(MimeFile, '    <icon name="x-taskdoc"/>');
    Writeln(MimeFile, '  </mime-type>');
    Writeln(MimeFile, '</mime-info>');
    CloseFile(MimeFile);

    // Create a .desktop file
    AssignFile(DesktopFile, UserHome + '/.local/share/applications/x-notetask.desktop');
    Rewrite(DesktopFile);
    Writeln(DesktopFile, '[Desktop Entry]');
    Writeln(DesktopFile, 'Name=Notetask');
    Writeln(DesktopFile, 'Exec=', AppPath, ' %f');
    Writeln(DesktopFile, 'Type=Application');
    Writeln(DesktopFile, 'MimeType=', MimeType);
    CloseFile(DesktopFile);

    // Update MIME database
    if (FpSystem('xdg-mime install --mode user ' + UserHome + '/.local/share/mime/packages/x-notetask.xml') = 0) and
       (FpSystem('xdg-icon-resource install --context mimetypes --size 48 ' + UserHome + '/.local/share/icons/hicolor/48x48/mimetypes/x-taskdoc.png x-taskdoc') = 0) and
       (FpSystem('update-mime-database ' + UserHome + '/.local/share/mime') = 0) and
       (FpSystem('gtk-update-icon-cache '+UserHome+'/.local/share/icons/hicolor -f') = 0) and
       (FpSystem('xdg-desktop-menu install --mode user ' + UserHome + '/.local/share/applications/x-notetask.desktop') = 0)
       then
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
