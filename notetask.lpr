//----------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under CC BY-NC-SA 4.0
//  Full license text: https://creativecommons.org/licenses/by-nc-sa/4.0/
//----------------------------------------------------------------------

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
  Application.Title := 'Notetask';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TformNotetask, formNotetask);
  Application.CreateForm(TformInputText, formInputText);
  Application.CreateForm(TformFindText, formFindText);
  Application.CreateForm(TformReplaceText, formReplaceText);
  ApplicationTranslate(Language);
  SetFileTypeIcon('.tsk', 1);
  Application.Run;
end.
