//-----------------------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the GNU General Public License, Version 3 (GPL-3.0)
//  You may obtain a copy of the License at https://www.gnu.org/licenses/gpl-3.0.html
//-----------------------------------------------------------------------------------

unit formfind;

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

uses
  Classes,
  SysUtils,
  Forms,
  StdCtrls,
  ExtCtrls,
  LCLType, Controls;

type

  { TformFindText }

  TformFindText = class(TForm)
    labelWhat: TLabel;
    editFind: TEdit;
    buttonFind: TButton;
    buttonCancel: TButton;
    radioDirection: TRadioGroup;
    checkMatchCase: TCheckBox;
    checkWrapAround: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure buttonFindClick(Sender: TObject);
    procedure buttonCancelClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure editFindKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure editFindChange(Sender: TObject);
  private

  public

  end;

var
  formFindText: TformFindText;

resourcestring
  rdirectionup = 'Up';
  rdirectiondown = 'Down';

implementation

uses mainform, formreplace;

  {$R *.lfm}

  { TformFindText }

procedure TformFindText.FormCreate(Sender: TObject);
begin
  radioDirection.ItemIndex := 1;
end;

procedure TformFindText.FormShow(Sender: TObject);
var
  ind: integer;
begin
  ind := radioDirection.ItemIndex;
  radioDirection.Items.Clear;
  radioDirection.Items.Add(rdirectionup);
  radioDirection.Items.Add(rdirectiondown);
  radioDirection.ItemIndex := ind;

  editFind.SetFocus;
  formReplaceText.Hide;
end;

procedure TformFindText.buttonFindClick(Sender: TObject);
begin
  formNotetask.Find(editFind.Text, checkMatchCase.Checked, checkWrapAround.Checked, radioDirection.ItemIndex = 1);
end;

procedure TformFindText.buttonCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TformFindText.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  //if Key = VK_ESCAPE then
  //  Close;

  //if Key = VK_RETURN then
  //  buttonFind.Click;
end;

procedure TformFindText.editFindKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  Edit: TEdit;
begin
  Edit := (Sender as TEdit);

  if Key = VK_DELETE then // Delete
  begin
    if Edit.SelLength = 0 then
    begin
      Edit.SelStart := editFind.SelStart;
      Edit.SelLength := 1;
    end;
    Edit.ClearSelection;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_Z) then // Ctrl + Z
  begin
    Edit.Undo;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_X) then // Ctrl + X
  begin
    Edit.CutToClipboard;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_C) then // Ctrl + C
  begin
    Edit.CopyToClipboard;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_V) then // Ctrl + V
  begin
    Edit.PasteFromClipboard;
    Key := 0;
  end
  else
  if (Shift = [ssCtrl]) and (Key = VK_A) then // Ctrl + A
  begin
    Edit.SelectAll;
    Key := 0;
  end;
end;

procedure TformFindText.editFindChange(Sender: TObject);
begin
  buttonFind.Enabled := editFind.Text <> string.Empty;
end;

end.
