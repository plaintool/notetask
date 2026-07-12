//-----------------------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the GNU General Public License, Version 3 (GPL-3.0)
//  You may obtain a copy of the License at https://www.gnu.org/licenses/gpl-3.0.html
//-----------------------------------------------------------------------------------

program notetask;

{$mode objfpc}{$H+}
{$codepage utf8}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  SysUtils,
  uDateTimePicker,
  Consts,
  mainform,
  forminput,
  formmemo,
  formfind,
  formreplace,
  localize
  {$IFDEF WINDOWS}
  ,uDarkStyle
  ,uWin32WidgetSetDark
  {$ENDIF}
  ;

  {$R *.res}

begin
  {$IFDEF DEBUG}
  GlobalSkipIfNoLeaks := True;
  {$ENDIF}
  RequireDerivedFormResource := True;
  Language := TLocalize.GetOSLanguage;
  Application.Title:='Notetask';
  Application.Scaled:=True;
  Application.Initialize;
  {$IFDEF WINDOWS}
  ApplyDarkStyle;
  {$ENDIF}
  Application.CreateForm(TformNotetask, formNotetask);
  Application.CreateForm(TformFindText, formFindText);
  Application.CreateForm(TformReplaceText, formReplaceText);
  Application.CreateForm(TformInputText, formInputText);
  Application.CreateForm(TformMemoText, formMemoText);
  TLocalize.ApplicationTranslate(APP_NAME, Language);
  TLocalize.UpdatePackageTranslations(APP_NAME, 'checkupdates', Language);
  Application.Run;
end.
