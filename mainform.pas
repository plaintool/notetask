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
    openDialog: TOpenDialog;
    pageSetupDialog: TPageSetupDialog;
    printDialog: TPrintDialog;
    printerSetupDialog: TPrinterSetupDialog;
    saveDialog: TSaveDialog;
    Separator1: TMenuItem;
    menuExit: TMenuItem;
    Separator3: TMenuItem;
    statusBar: TStatusBar;
    taskGrid: TStringGrid;
    procedure aExitExecute(Sender: TObject);
    procedure aNewExecute(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
  private

  public

  end;

var
  formNotetask: TformNotetask;

implementation

uses filemanager, lineending, task;

  {$R *.lfm}

  { TformNotetask }

procedure TformNotetask.aExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TformNotetask.aNewExecute(Sender: TObject);
var
  TaskStrings: TStringList; // Список строк для хранения задач
  Tasks: TTasks; // Коллекция задач
begin
  TaskStrings := TStringList.Create;
  TaskStrings.Add('[ ] - 01.01.2024, Comment 1, Task 1');
  TaskStrings.Add('[x] - 02.02.2024, Comment 2, Task 2');
  TaskStrings.Add('[ ] - 03.03.2024, Comment 3, Task 3');

  // Создаем экземпляр TTasks с переданным списком строк
  Tasks := TTasks.Create(TaskStrings);

  Tasks.FillGrid(taskGrid);
end;

procedure TformNotetask.aOpenExecute(Sender: TObject);
var
  Content: string;
  FileEncoding: TEncoding;
  LineEnding: TLineEnding;
  LineCount: integer;
  Tasks: TStringList;
begin
  if openDialog.Execute then
  begin
    ReadTextFile(openDialog.FileName, Content, FileEncoding, LineEnding, LineCount);

    statusBar.Panels[1].Text := UpperCase(FileEncoding.EncodingName);
    statusBar.Panels[2].Text := LineEnding.ToString;
    statusBar.Panels[3].Text := LineCount.ToString + ' rows';

    Tasks := TextToStringList(Content);

  end;
end;

end.
