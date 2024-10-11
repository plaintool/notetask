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
  formreplace;

  {$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title:='Notetask';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TformNotetask, formNotetask);
  Application.CreateForm(TformInputText, formInputText);
  Application.CreateForm(TformFindText, formFindText);
  Application.CreateForm(TformReplaceText, formReplaceText);
  ApplicationTranslate(GetOSLanguage);
  SetFileTypeIcon('.tsk', 1);
  Application.Run;
end.
