unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CheckLst, ValEdit, Grids, Menus, ActnList, ComCtrls, PrintersDlgs;

type

  { TformNotetask }

  TformNotetask = class(TForm)
    aPrinterSetup: TAction;
    aExit: TAction;
    aPrint: TAction;
    aPageProperties: TAction;
    aSaveAs: TAction;
    aSave: TAction;
    aNew: TAction;
    aNewWindow: TAction;
    aOpen: TAction;
    ActionList: TActionList;
    MainMenu: TMainMenu;
    menuFile: TMenuItem;
    menuNewWindow: TMenuItem;
    menuOpen: TMenuItem;
    menuSave: TMenuItem;
    menuSaveAs: TMenuItem;
    menuPrinterSetup: TMenuItem;
    menuPageProperties: TMenuItem;
    menuPrint: TMenuItem;
    menuNew: TMenuItem;
    OpenDialog1: TOpenDialog;
    PageSetupDialog1: TPageSetupDialog;
    PrintDialog1: TPrintDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    SaveDialog1: TSaveDialog;
    Separator1: TMenuItem;
    menuExit: TMenuItem;
    Separator3: TMenuItem;
    StatusBar1: TStatusBar;
    task: TStringGrid;
    procedure aExitExecute(Sender: TObject);
    procedure ValueListEditor1Click(Sender: TObject);
  private

  public

  end;

var
  formNotetask: TformNotetask;

implementation

{$R *.lfm}

{ TformNotetask }

procedure TformNotetask.ValueListEditor1Click(Sender: TObject);
begin

end;

procedure TformNotetask.aExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

end.

