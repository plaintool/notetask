program notetask;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Classes,
  GridPrn,
  printer4lazarus,
  mainform,
  lineending,
  filemanager,
  task,
  settings,
  systemtool;

  {$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title:='Notetask';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TformNotetask, formNotetask);
  ApplicationTranslate(GetOSLanguage);
  Application.Run;
end.
