//----------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under CC BY-NC-SA 4.0
//  Full license text: https://creativecommons.org/licenses/by-nc-sa/4.0/
//----------------------------------------------------------------------

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
  {$ENDIF}
  {$IFDEF Linux}
  ,Unix
  {$ENDIF}
  {$IFDEF MacOS}
  ,MacOSAll
  {$ENDIF}
  ;

function GetOSLanguage: string;
function ApplicationTranslate(const Language: string): boolean;

implementation

function GetOSLanguage: string;
  {platform-independent method to read the language of the user interface}
var
  l, fbl: string;
  {$IFDEF LCLCarbon}
  theLocaleRef: CFLocaleRef;
  locale: CFStringRef;
  buffer: StringPtr;
  bufferSize: CFIndex;
  encoding: CFStringEncoding;
  success: boolean;
  {$ENDIF}
begin
  fbl := string.Empty;
  l := string.Empty;
  {$IFDEF LCLCarbon}
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
  fbl := Copy(GetEnvironmentVariable('LC_CTYPE'), 1, 2);
  {$ELSE}
  GetLanguageIDs(l, fbl);
  {$ENDIF}
  {$ENDIF}
  Result := fbl;
end;

function ApplicationTranslate(const Language: string): boolean;
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
      // Load the resource file
      Res := TResourceStream.Create(HInstance, 'notetask.' + Language, RT_RCDATA);
      PoStringStream := TStringStream.Create('');

      // Save the resource to the string stream
      Res.SaveToStream(PoStringStream);

      // Read strings from the file
      PoFile := TPOFile.Create(False);
      PoFile.ReadPOText(PoStringStream.DataString);

      // Translate resource strings (this works for messagestring and resourcestring)
      Result := TranslateResourceStrings(PoFile);

      if Result then
      begin
        // Create a local translator for the form
        LocalTranslator := TPOTranslator.Create(PoFile);
        LRSTranslator := LocalTranslator;

        // Update the translation for all forms
        for i := 0 to Screen.CustomFormCount - 1 do
          LocalTranslator.UpdateTranslation(Screen.CustomForms[i]);

        // Update the translation for all data modules
        for i := 0 to Screen.DataModuleCount - 1 do
          LocalTranslator.UpdateTranslation(Screen.DataModules[i]);
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

end.
