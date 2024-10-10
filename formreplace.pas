unit formreplace;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TformReplaceText }

  TformReplaceText = class(TForm)
    labelWhat: TLabel;
    editSearch: TEdit;
    buttonSearch: TButton;
    buttonCancel: TButton;
    checkCase: TCheckBox;
    checkLoop: TCheckBox;
    labelWith: TLabel;
    editReplace: TEdit;
    buttonReplace: TButton;
    buttonReplaceAll: TButton;
  private

  public

  end;

var
  formReplaceText: TformReplaceText;

implementation

{$R *.lfm}

end.

