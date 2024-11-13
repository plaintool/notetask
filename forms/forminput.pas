//-----------------------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the GNU General Public License, Version 3 (GPL-3.0)
//  You may obtain a copy of the License at https://www.gnu.org/licenses/gpl-3.0.html
//-----------------------------------------------------------------------------------

unit forminput;

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

uses
  Forms,
  StdCtrls, Classes;

type

  { TformInputText }

  TformInputText = class(TForm)
    buttonOk: TButton;
    buttonCancel: TButton;
    editText: TEdit;
    LabelCaption: TLabel;
    procedure editTextKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FNumbersOnly: boolean;

  public
    procedure SetCaption(aCaption, aPrompt, aButtonOk: string; aDefault: string = ''; aNumbersOnly: boolean = False);
  end;

var
  formInputText: TformInputText;

implementation

{$R *.lfm}

{ TformInputText }

procedure TformInputText.FormCreate(Sender: TObject);
begin
  FNumbersOnly := True;
end;

procedure TformInputText.FormShow(Sender: TObject);
begin
  editText.SetFocus;
  editText.SelectAll;
end;

procedure TformInputText.editTextKeyPress(Sender: TObject; var Key: char);
begin
  // Allow only numeric characters and control characters (like Backspace)
  if (FNumbersOnly) and (not (Key in ['0'..'9', #8])) then
    Key := #0; // Block other characters
end;

procedure TformInputText.SetCaption(aCaption, aPrompt, aButtonOk: string; aDefault: string = ''; aNumbersOnly: boolean = False);
begin
  Caption := aCaption;
  LabelCaption.Caption := aPrompt;
  buttonOk.Caption := aButtonOk;
  FNumbersOnly := aNumbersOnly;
  editText.Text := aDefault;
end;

end.
