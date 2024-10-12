//----------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under CC BY-NC-SA 4.0
//  Full license text: https://creativecommons.org/licenses/by-nc-sa/4.0/
//----------------------------------------------------------------------

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
  LCLIntf,
  LCLType,
  LCLProc, Menus;

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
    Image1: TImage;
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
