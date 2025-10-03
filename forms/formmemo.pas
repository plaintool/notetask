//-----------------------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the GNU General Public License, Version 3 (GPL-3.0)
//  You may obtain a copy of the License at https://www.gnu.org/licenses/gpl-3.0.html
//-----------------------------------------------------------------------------------

unit formmemo;

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

uses
  Forms,
  StdCtrls,
  Classes,
  LCLType;

type

  { TformMemoText }

  TformMemoText = class(TForm)
    buttonOk: TButton;
    buttonCancel: TButton;
    LabelCaption: TLabel;
    memoText: TMemo;
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  public
    procedure SetMode(aCaption, aPrompt, aButtonOk: string; aDefault: string = ''; aWidth: integer = 300;
      aHeight: integer = 120; aWordWrap: boolean = True);
  end;

var
  formMemoText: TformMemoText;

implementation

{$R *.lfm}

{ TformMemoText }

procedure TformMemoText.FormShow(Sender: TObject);
begin
  SetFocus;
  memoText.SetFocus;
  memoText.SelStart := 0;
end;

procedure TformMemoText.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    buttonCancel.Click;
    Key := 0;
  end;
end;

procedure TformMemoText.SetMode(aCaption, aPrompt, aButtonOk: string; aDefault: string = ''; aWidth: integer = 300;
  aHeight: integer = 120; aWordWrap: boolean = True);
begin
  Caption := aCaption;
  LabelCaption.Caption := aPrompt;
  buttonOk.Caption := aButtonOk;
  memoText.Text := aDefault;
  memoText.WordWrap := aWordWrap;
  Width := aWidth;
  Height := aHeight;
end;

end.
