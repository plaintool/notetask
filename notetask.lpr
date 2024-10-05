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
  LCLTranslator,
  DefaultTranslator,
  GridPrn,
  printer4lazarus,
  mainform,
  lineending,
  filemanager,
  task,
  settings;

  {$R *.res}

begin
  RequireDerivedFormResource := True;
  SetDefaultLang('ru');
  Application.Title:='Notetask';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TformNotetask, formNotetask);
  Application.Run;
end.
