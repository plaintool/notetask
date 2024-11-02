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
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  SysUtils,
  DateTimeCtrls,
  lineending,
  filemanager,
  task,
  settings,
  systemtool,
  stringtool,
  mainform,
  forminput,
  formfind,
  formreplace,
  formabout,
  formdonate;

  {$R *.res}

begin
  RequireDerivedFormResource := True;
  Language := GetOSLanguage;
  DefaultFormatSettings.DecimalSeparator := '.';
  Application.Title := 'Notetask';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TformNotetask, formNotetask);
  Application.CreateForm(TformInputText, formInputText);
  Application.CreateForm(TformFindText, formFindText);
  Application.CreateForm(TformReplaceText, formReplaceText);
  ApplicationTranslate(Language);
  //SetFileTypeIcon('.tsk', 1);
  Application.Run;
end.
