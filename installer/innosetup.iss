[Registry]
;Registry data from file notetask.reg
Root: HKA; Subkey: "Software\Classes\.tsk"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\.tsk"; ValueType: string; ValueName: ""; ValueData: "notetask"; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\.tsk\ShellNew"; ValueType: string; ValueName: "NullFile"; ValueData: ""
Root: HKA; Subkey: "Software\Classes\notetask"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\notetask"; ValueType: string; ValueName: ""; ValueData: "Notetask Tasks"; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\notetask\DefaultIcon"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\notetask\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\notetask.exe,1"; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\notetask\shell"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\notetask\shell\open"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\notetask\shell\open\command"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\notetask\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\notetask.exe"" ""%1"""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\notetask\ShellNew"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\Applications\notetask.exe"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\Applications\notetask.exe\shell"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\Applications\notetask.exe\shell\open"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\Applications\notetask.exe\shell\open\command"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\Applications\notetask.exe\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\notetask.exe"" ""%1"""; Flags: uninsdeletevalue
;End of registry data from file notetask.reg

#define MyAppName "Notetask"
#define FileHandle FileOpen("..\VERSION")
#define MyAppVersion Trim(FileRead(FileHandle))
#if FileHandle
  #expr FileClose(FileHandle)
#endif
#define MyAppPublisher "Alexander Tverskoy"
#define MyAppURL "https://github.com/plaintool/notetask"
#define MyAppExeName "notetask.exe"

[Setup]
AppId={{F803A17B-C1FD-4B8A-8284-0AEDA0F8B73D}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
UninstallDisplayIcon={app}\notetask.exe
DefaultDirName={autopf}\{#MyAppName}
ArchitecturesAllowed=x64compatible x86 arm64
ArchitecturesInstallIn64BitMode=x64compatible arm64
DisableProgramGroupPage=yes
LicenseFile=..\LICENSE
PrivilegesRequiredOverridesAllowed=dialog
OutputDir=.\
OutputBaseFilename=notetask-{#MyAppVersion}-any-x86-x64
Compression=lzma
SolidCompression=yes
WizardStyle=modern

[Languages]
Name: "arabic"; MessagesFile: "compiler:Languages\Arabic.isl"
Name: "belarusian"; MessagesFile: "compiler:Languages\Belarusian.isl"
Name: "chinese"; MessagesFile: "compiler:Languages\ChineseSimplified.isl"
Name: "czech"; MessagesFile: "compiler:Languages\Czech.isl"
Name: "danish"; MessagesFile: "compiler:Languages\Danish.isl"
Name: "dutch"; MessagesFile: "compiler:Languages\Dutch.isl"
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "finnish"; MessagesFile: "compiler:Languages\Finnish.isl"
Name: "french"; MessagesFile: "compiler:Languages\French.isl"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"
Name: "greek"; MessagesFile: "compiler:Languages\Greek.isl"
Name: "hebrew"; MessagesFile: "compiler:Languages\Hebrew.isl"
Name: "hindi"; MessagesFile: "compiler:Languages\Hindi.isl"
Name: "indonesian"; MessagesFile: "compiler:Languages\Indonesian.isl"
Name: "italian"; MessagesFile: "compiler:Languages\Italian.isl"
Name: "japanese"; MessagesFile: "compiler:Languages\Japanese.isl"
Name: "korean"; MessagesFile: "compiler:Languages\Korean.isl"
Name: "polish"; MessagesFile: "compiler:Languages\Polish.isl"
Name: "portuguese"; MessagesFile: "compiler:Languages\Portuguese.isl"
Name: "romanian"; MessagesFile: "compiler:Languages\Romanian.isl"
Name: "russian"; MessagesFile: "compiler:Languages\Russian.isl"
Name: "spanish"; MessagesFile: "compiler:Languages\Spanish.isl"
Name: "swedish"; MessagesFile: "compiler:Languages\Swedish.isl"
Name: "turkish"; MessagesFile: "compiler:Languages\Turkish.isl"
Name: "ukrainian"; MessagesFile: "compiler:Languages\Ukrainian.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
; 64-bit
Source: "..\{#MyAppExeName}"; DestDir: "{app}"; DestName: "{#MyAppExeName}"; Check: Is64BitInstallMode; Flags: ignoreversion
; 32-bit
Source: "..\notetask32.exe"; DestDir: "{app}"; DestName: "{#MyAppExeName}"; Check: not Is64BitInstallMode; Flags: ignoreversion
; License
Source: "..\LICENSE"; DestDir: "{app}"; Flags: ignoreversion

[Icons]
Name: "{autoprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent