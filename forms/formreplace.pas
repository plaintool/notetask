//---------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the MIT License
//---------------------------------------------------------------------

unit formreplace;

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

uses
  Classes,
  SysUtils,
  Forms,
  StdCtrls,
  LCLType, Controls;

type

  { TformReplaceText }

  TformReplaceText = class(TForm)
    labelWhat: TLabel;
    editFind: TEdit;
    buttonFind: TButton;
    buttonCancel: TButton;
    checkMatchCase: TCheckBox;
    checkWrapAround: TCheckBox;
    labelWith: TLabel;
    editReplace: TEdit;
    buttonReplace: TButton;
    buttonReplaceAll: TButton;
    procedure buttonFindClick(Sender: TObject);
    procedure buttonReplaceClick(Sender: TObject);
    procedure buttonReplaceAllClick(Sender: TObject);
    procedure buttonCancelClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure editFindKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure editFindChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  formReplaceText: TformReplaceText;

implementation

uses mainform, formfind;

  {$R *.lfm}

  { TformReplaceText }

procedure TformReplaceText.FormShow(Sender: TObject);
begin
  editFind.SetFocus;
  formFindText.Hide;
end;

procedure TformReplaceText.buttonFindClick(Sender: TObject);
begin
  formNotetask.Find(editFind.Text, checkMatchCase.Checked, checkWrapAround.Checked, True);
end;

procedure TformReplaceText.buttonReplaceClick(Sender: TObject);
begin
  formNotetask.Replace(editFind.Text, editReplace.Text, checkMatchCase.Checked, checkWrapAround.Checked);
end;

procedure TformReplaceText.buttonReplaceAllClick(Sender: TObject);
begin
  formNotetask.ReplaceAll(editFind.Text, editReplace.Text, checkMatchCase.Checked, checkWrapAround.Checked);
end;

procedure TformReplaceText.buttonCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TformReplaceText.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  //if Key = VK_ESCAPE then
  //  Close;
  //
  //if Key = VK_RETURN then
  //  buttonFind.Click;
end;

procedure TformReplaceText.editFindKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
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

procedure TformReplaceText.editFindChange(Sender: TObject);
begin
  buttonFind.Enabled := editFind.Text <> string.Empty;
  buttonReplace.Enabled := buttonFind.Enabled;
  buttonReplaceAll.Enabled := buttonFind.Enabled;
end;

end.
