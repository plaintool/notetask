unit forminput;

{$mode ObjFPC}{$H+}

interface

uses
  Forms,
  StdCtrls;

type

  { TformInputText }

  TformInputText = class(TForm)
    buttonOk: TButton;
    buttonCancel: TButton;
    editText: TEdit;
    LabelCaption: TLabel;
    procedure editTextKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  formInputText: TformInputText;

implementation

{$R *.lfm}

{ TformInputText }

procedure TformInputText.editTextKeyPress(Sender: TObject; var Key: char);
begin
  // Allow only numeric characters and control characters (like Backspace)
  if not (Key in ['0'..'9', #8]) then
    Key := #0; // Block other characters
end;

procedure TformInputText.FormShow(Sender: TObject);
begin
  editText.SetFocus;
  editText.SelectAll;
end;

end.
