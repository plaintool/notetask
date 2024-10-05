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
  LCLTranslator,
  DefaultTranslator,
  Translations,
  LCLType,
  GridPrn,
  printer4lazarus,
  mainform,
  lineending,
  filemanager,
  task,
  settings;

  {$R *.res}


  function Translate(const Language: string): boolean;
  var
    Res: TResourceStream;
    PoStringStream: TStringStream;
    PoFile: TPOFile;
  begin
    Res := TResourceStream.Create(HInstance, 'notetask.' + Language, RT_RCDATA);
    PoStringStream := TStringStream.Create('');
    Res.SaveToStream(PoStringStream);
    Res.Free;

    PoFile := TPOFile.Create(False);
    PoFile.ReadPOText(PoStringStream.DataString);
    PoStringStream.Free;

    Result := TranslateResourceStrings(PoFile);
    PoFile.Free;
  end;


begin
  RequireDerivedFormResource := True;
  Translate('ru');
  Application.Title := 'Notetask';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TformNotetask, formNotetask);
  Application.Run;
end.
