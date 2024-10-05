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
    ResourceID: HRSRC;
  begin
    Result := False;

    // Check if the resource exists
    ResourceID := FindResource(HInstance, PChar('notetask.' + Language), RT_RCDATA);

    if ResourceID = 0 then
    begin
      //SetDefaultLang(Language);
      Exit; // Resource not found, exit function
    end;

    // Load the resource stream if it exists
    Res := TResourceStream.Create(HInstance, 'notetask.' + Language, RT_RCDATA);
    try
      PoStringStream := TStringStream.Create('');
      try
        Res.SaveToStream(PoStringStream);

        PoFile := TPOFile.Create(False);
        try
          PoFile.ReadPOText(PoStringStream.DataString);
          Result := TranslateResourceStrings(PoFile);
        finally
          PoFile.Free;
        end;
      finally
        PoStringStream.Free;
      end;
    finally
      Res.Free;
    end;
  end;

begin
  RequireDerivedFormResource := True;
  //Translate('ru');
  Application.Title := 'Notetask';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TformNotetask, formNotetask);
  Application.Run;
end.
