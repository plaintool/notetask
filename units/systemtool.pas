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
  Controls,
  Graphics,
  FileInfo,
  gettext,
  DefaultTranslator,
  Translations,
  LResources,
  LCLTranslator,
  LCLIntf,
  Dialogs,
  {$IFDEF Windows}
  Windows,
  Registry,
  wininet,
  {$ENDIF}
  {$IFDEF Linux}
  Unix,
  LCLType,
  Process,
  fphttpclient,
  opensslsockets,
  {$ENDIF}
  {$IFDEF MacOS}
  MacOSAll,
  fphttpclient,
  opensslsockets,
  {$ENDIF}
  fpjson,
  jsonparser;

function GetOSLanguage: string;

function ApplicationTranslate(const Language: string; AForm: TCustomForm = nil): boolean;

{$IFDEF Windows}

function IsWindowsDarkThemeEnabled: Boolean;

{$ENDIF}

function SetCursorTo(Control: TControl; const ResName: string; CursorIndex: integer = 1001): boolean;

function SetFileTypeIcon(const Ext: string; IconIndex: integer): boolean;

function IsSystemKey(Key: word): boolean;

function GetAppVersion: string;

function CheckGithubLatestVersion(const Repo: string = 'plaintool/notetask'): boolean;

var
  Language: string;

resourcestring
  newversion = 'New version available: %s. Open GitHub page to download?';
  newversionuptodate = 'Your version is up to date.';
  newversioncheckerror = 'Error checking version:';

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
  LangToUse: string;
  LangFound: boolean;
begin
  Result := False;
  Res := nil;
  PoStringStream := nil;
  PoFile := nil;
  LocalTranslator := nil;

  // Determine which language to load
  LangToUse := Language;

  try
    try
      PoStringStream := TStringStream.Create('');

      // Try to load the language resource file
      try
        Res := TResourceStream.Create(HInstance, 'notetask.' + LangToUse, RT_RCDATA);
        LangFound := True;
      except
        // If language resource not found, fall back to English
        LangToUse := 'en';
        Res := TResourceStream.Create(HInstance, 'notetask.en', RT_RCDATA);
        LangFound := False;
      end;

      // Save resource to string stream
      Res.SaveToStream(PoStringStream);

      // Read PO strings
      PoFile := TPOFile.Create(False);
      PoFile.ReadPOText(PoStringStream.DataString);

      // Apply translations to resource strings
      if not Assigned(AForm) then
        Result := TranslateResourceStrings(PoFile);

      if Result or Assigned(AForm) then
      begin
        // Create a local translator for the form or all forms
        LocalTranslator := TPOTranslator.Create(PoFile);
        if Assigned(LRSTranslator) then
          LRSTranslator.Free;
        LRSTranslator := LocalTranslator;

        // Translate only the specified form
        if Assigned(AForm) then
          LocalTranslator.UpdateTranslation(AForm)
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
      Result := False;
    end;

    Result := Result and LangFound;
  finally
    // Free all used resources
    if Assigned(LocalTranslator) then
    begin
      LRSTranslator := nil;
      LocalTranslator.Free;
    end
    else if Assigned(PoFile) then
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
  Key:=HKEY_CURRENT_USER;
  if RegOpenKeyEx(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize', 0, KEY_READ, Key) = ERROR_SUCCESS then
  begin
    ValueSize := SizeOf(Value);
    if RegQueryValueEx(Key, 'AppsUseLightTheme', nil, nil, @Value, @ValueSize) = ERROR_SUCCESS then
    begin
      Result := Value = 0; // 0 - Dark theme, 1 - Light theme
    end;
    RegCloseKey(Key);
  end;
end;

{$ENDIF}

function SetCursorTo(Control: TControl; const ResName: string; CursorIndex: integer = 1001): boolean;
var
  ResStream: TResourceStream;
  Curs: TCursorImage;
begin
  Result := False;
  if not Assigned(Control) then Exit;

  ResStream := nil;
  Curs := TCursorImage.Create;
  try
    try
      ResStream := TResourceStream.Create(HInstance, ResName, RT_RCDATA);
      ResStream.Position := 0;
      Curs.LoadFromStream(ResStream);
      Screen.Cursors[CursorIndex] := Curs.ReleaseHandle;
      Control.Cursor := CursorIndex;
      Result := True;
    except
      Result := False;
    end;
  finally
    ResStream.Free;
    Curs.Free;
  end;
end;

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

function IsSystemKey(Key: word): boolean;
begin
  case Key of
    // Navigation keys
    VK_TAB, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT,
    VK_HOME, VK_END, VK_PRIOR, VK_NEXT,

    // Function keys
    VK_F1..VK_F24,

    // Modifiers
    VK_SHIFT, VK_CONTROL, VK_MENU,
    VK_LSHIFT, VK_RSHIFT, VK_LCONTROL, VK_RCONTROL,
    VK_LMENU, VK_RMENU, VK_LWIN, VK_RWIN,

    // Special keys
    VK_ESCAPE, VK_INSERT, VK_DELETE, VK_SCROLL, VK_PAUSE,
    VK_CAPITAL, VK_NUMLOCK, VK_SNAPSHOT, VK_CANCEL,
    VK_BACK, VK_RETURN, VK_CLEAR,

    // Numpad keys
    VK_ADD, VK_SUBTRACT, VK_MULTIPLY, VK_DIVIDE, VK_DECIMAL,
    VK_NUMPAD0..VK_NUMPAD9,

    // Extended keys (multimedia/browser)
    VK_BROWSER_BACK..VK_LAUNCH_APP2,
    VK_KANA..VK_MODECHANGE:
      Result := True;
    else
      Result := False;
  end;
end;

function GetAppVersion: string;
var
  Info: TFileVersionInfo;
begin
  Info := TFileVersionInfo.Create(nil);
  try
    Info.FileName := ParamStr(0);
    Info.ReadFileInfo;
    Result := Info.VersionStrings.Values['ProductVersion'];
  finally
    Info.Free;
  end;
end;

function CheckGithubLatestVersion(const Repo: string = 'plaintool/notetask'): boolean;
var
  JsonData: TJSONData;
  LatestVersion, Msg: string;
  Url: string;
  CurrentVersion: string;
  ResponseContent: string;

{$IFDEF WINDOWS}
  function HttpGetWinInet(const AUrl: string): string;
  var
    hInet, hUrl: HINTERNET;
    Buffer: array[0..4095] of Char;
    BytesRead: DWORD = 0;
    I: Integer;
  begin
    for I := 0 to High(Buffer) do
      Buffer[I] := #0;

    Result := '';
    hInet := InternetOpen('NotetaskVersionChecker', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
    if hInet = nil then
      Exit;

    try
      hUrl := InternetOpenUrl(hInet, PChar(AUrl), nil, 0,
                             INTERNET_FLAG_RELOAD or INTERNET_FLAG_SECURE or
                             INTERNET_FLAG_EXISTING_CONNECT, 0);
      if hUrl = nil then
        Exit;

      try
        while InternetReadFile(hUrl, @Buffer, SizeOf(Buffer), BytesRead) and (BytesRead > 0) do
        begin
          Result := Result + Copy(Buffer, 1, BytesRead);
        end;
      finally
        InternetCloseHandle(hUrl);
      end;
    finally
      InternetCloseHandle(hInet);
    end;
  end;
{$ELSE}

  function HttpGetCurl(const AUrl: string): string;
  var
    Process: TProcess;
    OutputStream: TMemoryStream;
    BytesRead: longint;
    Buffer: TBytes = nil;
    OutputString: ansistring = '';
  begin
    Result := '';
    SetLength(Buffer, 2048);
    Process := TProcess.Create(nil);
    OutputStream := TMemoryStream.Create;
    try
      Process.Executable := 'curl';
      Process.Parameters.Add('-s');
      Process.Parameters.Add('-L');
      Process.Parameters.Add('-H');
      Process.Parameters.Add('User-Agent: NotetaskVersionChecker');
      Process.Parameters.Add(AUrl);

      Process.Options := [poUsePipes, poNoConsole];
      Process.Execute;

      while Process.Running or (Process.Output.NumBytesAvailable > 0) do
      begin
        BytesRead := Process.Output.Read(Buffer[1], SizeOf(Buffer));
        if BytesRead > 0 then
          OutputStream.Write(Buffer[1], BytesRead);
      end;

      Process.WaitOnExit;

      if OutputStream.Size > 0 then
      begin
        SetLength(OutputString, OutputStream.Size);
        OutputStream.Position := 0;
        OutputStream.Read(OutputString[1], OutputStream.Size);
        Result := string(OutputString);
      end;
    finally
      OutputStream.Free;
      Process.Free;
    end;
  end;

  function IsCurlAvailable: boolean;
  var
    Process: TProcess;
    ExitStatus: integer;
  begin
    Result := False;
    Process := TProcess.Create(nil);
    try
      Process.Executable := 'curl';
      Process.Parameters.Add('--version');
      Process.Options := [poWaitOnExit, poNoConsole, poUsePipes];
      Process.ShowWindow := swoHIDE;

      try
        Process.Execute;
        Process.WaitOnExit;
        ExitStatus := Process.ExitStatus;
        Result := (ExitStatus = 0);
      except
        on E: EProcess do
          Result := False;
        on E: Exception do
          Result := False;
      end;
    finally
      Process.Free;
    end;
  end;

  function HttpGetWget(const AUrl: string): string;
  var
    Process: TProcess;
    OutputStream: TMemoryStream;
    BytesRead: longint;
    Buffer: TBytes = ();
  begin
    Result := '';
    SetLength(Buffer, 2048);
    Process := TProcess.Create(nil);
    OutputStream := TMemoryStream.Create;
    try
      Process.Executable := 'wget';
      Process.Parameters.Add('-q');
      Process.Parameters.Add('-O');
      Process.Parameters.Add('-');
      Process.Parameters.Add('--header=User-Agent: NotetaskVersionChecker');
      Process.Parameters.Add(AUrl);

      Process.Options := [poUsePipes, poNoConsole];
      Process.Execute;

      while Process.Running or (Process.Output.NumBytesAvailable > 0) do
      begin
        BytesRead := Process.Output.Read(Buffer[0], Length(Buffer));
        if BytesRead > 0 then
          OutputStream.Write(Buffer[0], BytesRead);
      end;

      Process.WaitOnExit;

      if OutputStream.Size > 0 then
      begin
        SetLength(Result, OutputStream.Size);
        OutputStream.Position := 0;
        OutputStream.Read(Result[1], OutputStream.Size);
      end;
    finally
      OutputStream.Free;
      Process.Free;
    end;
  end;

  function IsWgetAvailable: boolean;
  var
    Process: TProcess;
  begin
    Result := False;
    Process := TProcess.Create(nil);
    try
      Process.Executable := 'wget';
      Process.Parameters.Add('--version');
      Process.Options := [poWaitOnExit, poNoConsole];
      try
        Process.Execute;
        Process.WaitOnExit;
        Result := (Process.ExitStatus = 0);
      except
        Result := False;
      end;
    finally
      Process.Free;
    end;
  end;

{$ENDIF}
begin
  CurrentVersion := GetAppVersion;
  Result := False;
  Url := Format('https://api.github.com/repos/%s/releases/latest', [Repo]);

  try
    {$IFDEF WINDOWS}
    ResponseContent := HttpGetWinInet(Url);
    {$ELSE}
    try
      with TFPHttpClient.Create(nil) do
      try
        AddHeader('User-Agent', 'NotetaskVersionChecker');
        ResponseContent := Get(Url);
      finally
        Free;
      end;
    except
      on E: Exception do
      begin
        if IsCurlAvailable then
        begin
          ResponseContent := HttpGetCurl(Url);
        end
        else if IsWgetAvailable then
        begin
          ResponseContent := HttpGetWget(Url);
        end
        else
        begin
          ShowMessage(newversioncheckerror + ' ' + 'Please install OpenSSL, curl or wget library!');
          Exit;
        end;
      end;
    end;
    {$ENDIF}

    if ResponseContent <> '' then
    begin
      JsonData := GetJSON(ResponseContent);
      try
        LatestVersion := JsonData.GetPath('tag_name').AsString;

        if AnsiLowerCase(StringReplace(LatestVersion, 'v', '', [rfReplaceAll])) <> AnsiLowerCase(
          StringReplace(CurrentVersion, 'v', '', [rfReplaceAll])) then
        begin
          Msg := Format(newversion, [LatestVersion]);
          if MessageDlg(Msg, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
            OpenURL(Format('https://github.com/%s/releases/latest', [Repo]));
        end
        else
          ShowMessage(newversionuptodate);

        Result := True;
      finally
        JsonData.Free;
      end;
    end
    else
      ShowMessage(newversioncheckerror);

  except
    on E: Exception do
      ShowMessage(newversioncheckerror + ' ' + E.Message);
  end;
end;

end.
