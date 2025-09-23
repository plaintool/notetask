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
  StdCtrls,
  Classes,
  Controls,
  Dialogs,
  LCLType;

type

  { TformInputText }

  TformInputText = class(TForm)
    buttonOk: TButton;
    buttonCancel: TButton;
    checkShowPassword: TCheckBox;
    editText: TEdit;
    editText2: TEdit;
    LabelCaption: TLabel;
    LabelCaption2: TLabel;
    procedure checkShowPasswordChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  public
    procedure SetMode(aCaption, aPrompt, aButtonOk: string; aDefault: string = ''; aNumbersOnly: boolean = False;
      aPassword: boolean = False; aConfirmPassword: boolean = False);
  end;

var
  formInputText: TformInputText;

resourcestring
  rpasswordsnotequal = 'Passwords are not equal!';

implementation

{$R *.lfm}

{ TformInputText }

procedure TformInputText.FormShow(Sender: TObject);
begin
  SetFocus;
  editText.SetFocus;
  editText.SelectAll;
end;

procedure TformInputText.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    if buttonOk.Default then
      buttonOk.Click;
    Key := 0;
  end
  else
  if Key = VK_ESCAPE then
  begin
    buttonCancel.Click;
    Key := 0;
  end;
end;

procedure TformInputText.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  // Check if modal result is OK (user pressed OK button)
  if ModalResult = mrOk then
  begin
    if (editText2.Visible) and (editText.Text <> editText2.Text) then
    begin
      CanClose := False; // prevent form closing
      ShowMessage(rpasswordsnotequal);
    end;
  end;
end;

procedure TformInputText.checkShowPasswordChange(Sender: TObject);
begin
  if (checkShowPassword.Checked) then
  begin
    editText.PasswordChar := #0;
    editText2.PasswordChar := #0;
  end
  else
  begin
    editText.PasswordChar := '*';
    editText2.PasswordChar := '*';
  end;
end;

procedure TformInputText.SetMode(aCaption, aPrompt, aButtonOk: string; aDefault: string = '';
  aNumbersOnly: boolean = False; aPassword: boolean = False; aConfirmPassword: boolean = False);
begin
  Caption := aCaption;
  LabelCaption.Caption := aPrompt;
  buttonOk.Caption := aButtonOk;
  editText.NumbersOnly := aNumbersOnly;
  editText.Text := aDefault;
  editText2.Text := aDefault;
  if aPassword and not aConfirmPassword then
  begin
    editText.PasswordChar := '*';
    labelCaption2.Visible := False;
    editText2.Visible := False;
    checkShowPassword.Visible := True;
    checkShowPassword.Top := 72;
    Height := 135;
  end
  else
  if aConfirmPassword then
  begin
    editText.PasswordChar := '*';
    editText2.PasswordChar := '*';
    labelCaption2.Visible := True;
    editText2.Visible := True;
    checkShowPassword.Visible := True;
    checkShowPassword.Top := 128;
    Height := 190;
  end
  else
  begin
    editText.PasswordChar := #0;
    labelCaption2.Visible := False;
    editText2.Visible := False;
    checkShowPassword.Visible := False;
    checkShowPassword.Top := 128;
    Height := 103;
  end;
end;

end.
