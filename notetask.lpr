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
  systemtool,
  mainform,
  formmemo,
  formfind,
  formreplace, forminput;

  {$R *.res}

begin
  RequireDerivedFormResource := True;
  Language := GetOSLanguage;
  DefaultFormatSettings.DecimalSeparator := '.';
  Application.Title:='Notetask';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TformNotetask, formNotetask);
  Application.CreateForm(TformFindText, formFindText);
  Application.CreateForm(TformReplaceText, formReplaceText);
  Application.CreateForm(TformInputText, formInputText);
  Application.CreateForm(TformMemoText, formMemoText);
  ApplicationTranslate(Language);
  //SetFileTypeIcon('.tsk', 1);
  Application.Run;
end.
