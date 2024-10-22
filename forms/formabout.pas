//---------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the MIT License
//---------------------------------------------------------------------

unit formabout;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Menus,
  Classes,
  LCLIntf,
  LCLType,
  LCLProc;

type

  { TformAboutNotetask }

  TformAboutNotetask = class(TForm)
    buttonOk: TButton;
    imageLogo: TImage;
    labelBy: TLabel;
    labelName: TLabel;
    labelLic: TLabel;
    LabelLicUrl: TLabel;
    Memo1: TMemo;
    procedure LabelLicUrlClick(Sender: TObject);
  private

  public

  end;

var
  formAboutNotetask: TformAboutNotetask;

implementation

{$R *.lfm}

{ TformAboutNotetask }

procedure TformAboutNotetask.LabelLicUrlClick(Sender: TObject);
begin
  OpenUrl(labelLicUrl.Caption);
end;

end.
