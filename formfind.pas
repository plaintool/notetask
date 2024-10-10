unit formfind;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TformFindText }

  TformFindText = class(TForm)
    labelWhat: TLabel;
    editSearch: TEdit;
    buttonSearch: TButton;
    buttonCancel: TButton;
    radioDirection: TRadioGroup;
    checkCase: TCheckBox;
    checkLoop: TCheckBox;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  formFindText: TformFindText;

resourcestring
  rdirectionup = 'Up';
  rdirectiondown = 'Down';

implementation

{$R *.lfm}

{ TformFindText }

procedure TformFindText.FormCreate(Sender: TObject);
begin
  radioDirection.Items.Clear;
  radioDirection.Items.Add(rdirectionup);
  radioDirection.Items.Add(rdirectiondown);
end;

end.
