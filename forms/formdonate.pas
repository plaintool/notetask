unit formdonate;

{$mode ObjFPC}{$H+}

interface

uses
  Forms,
  StdCtrls,
  Buttons,
  Clipbrd,
  LCLIntf, Classes;

type

  { TformDonateNotetask }

  TformDonateNotetask = class(TForm)
    buttonOk: TButton;
    editWallet1: TEdit;
    buttonCopy1: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    editWallet2: TEdit;
    buttonCopy2: TSpeedButton;
    Label4: TLabel;
    Label5: TLabel;
    editWallet3: TEdit;
    buttonCopy3: TSpeedButton;
    Label6: TLabel;
    Label7: TLabel;
    editWallet4: TEdit;
    buttonCopy4: TSpeedButton;
    Label8: TLabel;
    Label9: TLabel;
    labelUrl: TLabel;
    Label10: TLabel;
    procedure buttonCopy1Click(Sender: TObject);
    procedure buttonCopy2Click(Sender: TObject);
    procedure buttonCopy3Click(Sender: TObject);
    procedure buttonCopy4Click(Sender: TObject);
    procedure labelUrlClick(Sender: TObject);
    procedure editWallet4Click(Sender: TObject);
    procedure editWallet3Click(Sender: TObject);
    procedure editWallet1Click(Sender: TObject);
    procedure editWallet2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  formDonateNotetask: TformDonateNotetask;

implementation

uses systemtool;

  {$R *.lfm}

  { TformDonateNotetask }

procedure TformDonateNotetask.FormCreate(Sender: TObject);
begin
  ApplicationTranslate(language, self);
end;

procedure TformDonateNotetask.buttonCopy1Click(Sender: TObject);
begin
  Clipboard.AsText := editWallet1.Text;
end;

procedure TformDonateNotetask.buttonCopy2Click(Sender: TObject);
begin
  Clipboard.AsText := editWallet2.Text;
end;

procedure TformDonateNotetask.buttonCopy3Click(Sender: TObject);
begin
  Clipboard.AsText := editWallet3.Text;
end;

procedure TformDonateNotetask.buttonCopy4Click(Sender: TObject);
begin
  Clipboard.AsText := editWallet4.Text;
end;

procedure TformDonateNotetask.labelUrlClick(Sender: TObject);
begin
  OpenUrl(labelUrl.Caption);
end;

procedure TformDonateNotetask.editWallet4Click(Sender: TObject);
begin
  editWallet4.SelectAll;
end;

procedure TformDonateNotetask.editWallet3Click(Sender: TObject);
begin
  editWallet3.SelectAll;
end;

procedure TformDonateNotetask.editWallet1Click(Sender: TObject);
begin
  editWallet1.SelectAll;
end;

procedure TformDonateNotetask.editWallet2Click(Sender: TObject);
begin
  editWallet2.SelectAll;
end;

end.
