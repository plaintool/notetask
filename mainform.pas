unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CheckLst, ValEdit, Grids, Menus, ActnList, ComCtrls, PrintersDlgs, LCLIntf, LCLType, task, Types;

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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure taskGridDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure taskGridHeaderSized(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure taskGridPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    procedure taskGridResize(Sender: TObject);
    procedure taskGridSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
  private
    procedure MemoChange(Sender: TObject);
  public
    procedure OpenFile(fileName: string);
  end;

var
  formNotetask: TformNotetask;
  Tasks: TTasks; // Коллекция задач

resourcestring
  rrows = ' rows';

implementation

uses filemanager, lineending, settings;

  {$R *.lfm}

  { TformNotetask }

procedure TformNotetask.FormCreate(Sender: TObject);
var
  FilePath: string;
begin
  LoadFormSettings(self);
  LoadGridSettings(taskGrid);

  // Проверяем, передан ли аргумент командной строки
  if ParamCount > 0 then
  begin
    FilePath := ParamStr(1); // Получаем путь к файлу
    if (not FilePath.StartsWith('--')) then
      OpenFile(FilePath); // Ваша функция для загрузки задачи из файла
  end;
end;

procedure TformNotetask.FormDestroy(Sender: TObject);
begin
  SaveFormSettings(self);
end;

procedure TformNotetask.aExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TformNotetask.aNewExecute(Sender: TObject);
var
  TaskStrings: TStringList; // Список строк для хранения задач
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
begin
  if openDialog.Execute then
  begin
    OpenFile(openDialog.FileName);
  end;
end;

procedure TformNotetask.OpenFile(fileName: string);
var
  Content: string;
  FileEncoding: TEncoding;
  LineEnding: TLineEnding;
  LineCount: integer;
begin
  ReadTextFile(fileName, Content, FileEncoding, LineEnding, LineCount);

  statusBar.Panels[1].Text := UpperCase(FileEncoding.EncodingName);
  statusBar.Panels[2].Text := LineEnding.ToString;
  statusBar.Panels[3].Text := LineCount.ToString + rrows;

  Tasks := TTasks.Create(TextToStringList(Content));
  Tasks.FillGrid(taskGrid);
end;

procedure TformNotetask.taskGridPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
var
  MyTextStyle: TTextStyle;
begin
  if (aCol in [2, 3]) then
  begin
    MyTextStyle := taskGrid.Canvas.TextStyle;
    MyTextStyle.SingleLine := False;
    MyTextStyle.Wordbreak := True;
    taskGrid.Canvas.TextStyle := MyTextStyle;
  end;
end;

procedure TformNotetask.taskGridDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
var
  grid: TStringGrid;
  S: string;
  drawrect: TRect;
  bgFill: TColor;
begin
  if (aCol in [0, 1]) or (aRow = 0) then exit;
  grid := Sender as TStringGrid;

  //if gdSelected in aState then
  //  bgFill := $FFF0D0
  //else
  bgFill := clWhite;

  grid.Canvas.Brush.Color := bgFill;
  grid.canvas.Brush.Style := bsSolid;
  grid.canvas.fillrect(aRect);

  S := grid.Cells[ACol, ARow];
  if Length(S) > 0 then
  begin
    drawrect := aRect;
    drawrect.Inflate(-4, 0);
    DrawText(grid.canvas.handle, PChar(S), Length(S), drawrect,
      dt_calcrect or dt_wordbreak or dt_left);
    if (drawrect.bottom - drawrect.top) > grid.RowHeights[ARow] then
      grid.RowHeights[ARow] := (drawrect.bottom - drawrect.top + 2)
    // changing the row height fires the event again!
    else
    begin
      drawrect.Right := aRect.Right;
      // grid.canvas.fillrect(drawrect);
      DrawText(grid.canvas.handle, PChar(S), Length(S), drawrect,
        dt_wordbreak or dt_left);
    end;
  end;

  if gdFocused in aState then
    grid.Canvas.DrawFocusRect(aRect);
end;

procedure TformNotetask.FormResize(Sender: TObject);
begin
  taskGridResize(Sender);
end;

procedure TformNotetask.FormWindowStateChange(Sender: TObject);
begin

end;

procedure TformNotetask.taskGridHeaderSized(Sender: TObject; IsColumn: boolean; Index: integer);
begin
  taskGridResize(Sender);
  SaveGridSettings(taskGrid);
end;

procedure TformNotetask.taskGridResize(Sender: TObject);
var
  Rect: TRect;
begin
  // Получаем размеры ячейки
  Rect := taskGrid.CellRect(taskGrid.Col, taskGrid.Row);

  // Обновляем размер и позицию Memo
  if Assigned(taskGrid.Editor) and (taskGrid.Editor is TMemo) then
  begin
    TMemo(taskGrid.Editor).SetBounds(Rect.Left + 4, Rect.Top, Rect.Right - Rect.Left - 5, Rect.Bottom - Rect.Top - 1);
  end;
end;

procedure TformNotetask.MemoChange(Sender: TObject);
begin
  taskGrid.Cells[taskGrid.Col, taskGrid.Row] := TMemo(Sender).Text;
end;

procedure TformNotetask.taskGridSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
var
  EditorControl: TMemo;
  Rect: TRect;
begin
  if aCol = 1 then exit;
  EditorControl := TMemo.Create(Self);
  EditorControl.Visible := False;
  EditorControl.Parent := taskGrid;
  EditorControl.BorderStyle := bsNone;
  EditorControl.OnChange := @MemoChange; // Привязываем событие

  Rect := taskGrid.CellRect(aCol, aRow);
  EditorControl.SetBounds(Rect.Left + 4, Rect.Top, Rect.Right - Rect.Left - 5, Rect.Bottom - Rect.Top - 1);

  EditorControl.Text := taskGrid.Cells[aCol, aRow];
  EditorControl.WordWrap := True; // Включение многострочного режима
  Editor := EditorControl;
  EditorControl.Visible := True;
end;

end.
