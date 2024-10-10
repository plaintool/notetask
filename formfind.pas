unit formfind;

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

uses
  Classes,
  Forms,
  StdCtrls,
  ExtCtrls,
  LCLType;

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
  private

  public

  end;

var
  formFindText: TformFindText;

resourcestring
  rdirectionup = 'Up';
  rdirectiondown = 'Down';

implementation

uses mainform;

  {$R *.lfm}

  { TformFindText }

procedure TformFindText.FormCreate(Sender: TObject);
begin
  radioDirection.Items.Clear;
  radioDirection.Items.Add(rdirectionup);
  radioDirection.Items.Add(rdirectiondown);
  radioDirection.ItemIndex := 1;
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
  if Key = VK_ESCAPE then
    Close;
end;

end.
