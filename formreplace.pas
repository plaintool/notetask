unit formreplace;

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

uses
  Classes,
  Forms,
  StdCtrls,
  LCLType;

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
  private

  public

  end;

var
  formReplaceText: TformReplaceText;

implementation

uses mainform;

  {$R *.lfm}

  { TformReplaceText }

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
  if Key = VK_ESCAPE then
    Close;
end;

end.
