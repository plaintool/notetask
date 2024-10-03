unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Types, CheckLst, ValEdit, Grids, Menus, ActnList, ComCtrls, PrintersDlgs,
  LCLIntf, LCLType, ExtDlgs, LMessages, task;

type

  { TformNotetask }
  TformNotetask = class(TForm)
    aFont: TAction;
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
    calendarDialog: TCalendarDialog;
    fontDialog: TFontDialog;
    MainMenu: TMainMenu;
    menuFile: TMenuItem;
    menuFormat: TMenuItem;
    menuFont: TMenuItem;
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
    procedure aFontExecute(Sender: TObject);
    procedure aNewExecute(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure taskGridCheckboxToggled(Sender: TObject; aCol, aRow: integer; aState: TCheckboxState);
    procedure taskGridDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure taskGridEditButtonClick(Sender: TObject);
    procedure taskGridHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure taskGridHeaderSized(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure taskGridMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure taskGridPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    procedure taskGridResize(Sender: TObject);
    procedure taskGridSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
    procedure taskGridValidateEntry(Sender: TObject; aCol, aRow: integer; const OldValue: string; var NewValue: string);
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
  rdeleteconfirm = 'Are you sure you want to delete this task?';

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

procedure TformNotetask.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  Row: integer;
  ConfirmDelete: integer;
begin
  if (Key = 46) and (not taskGrid.EditorMode) then // #46 соответствует клавише Del
  begin
    Row := taskGrid.Row; // Получаем текущую выбранную строку
    if (Row > 0) and (Row <= Tasks.Count) then
    begin
      // Показываем диалог для подтверждения удаления
      ConfirmDelete := MessageDlg(rdeleteconfirm, mtConfirmation, [mbYes, mbNo], 0);

      if ConfirmDelete = mrYes then
      begin
        Tasks.RemoveTask(Row - 1); // Удаляем задачу из коллекции
        taskGrid.DeleteRow(Row); // Удаляем строку из грида
        Abort;
      end;
    end;
  end;
end;

procedure TformNotetask.aExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TformNotetask.aFontExecute(Sender: TObject);
begin
  fontDialog.Font := Font;
  if fontDialog.Execute then  // Открыть диалоговое окно шрифта
  begin
    // Применить выбранный шрифт к форме
    Self.Font := fontDialog.Font;
  end;
end;

procedure TformNotetask.aNewExecute(Sender: TObject);
begin
  // Создаем экземпляр TTasks с переданным списком строк
  Tasks := TTasks.Create();
  taskGrid.Clean;
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

  // Determine background color
  if gdSelected in aState then
  begin
    bgFill := clHighlight;
    grid.Canvas.Font.Color := clWhite; // Set font color to white when selected
  end
  else
  begin
    bgFill := clWhite;
    grid.Canvas.Font.Color := clBlack; // Set font color to black when not selected
  end;

  // Fill the cell background
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

procedure TformNotetask.taskGridEditButtonClick(Sender: TObject);
var
  Col, Row: integer;
  CellRect: TRect;
  CalendarX, CalendarY: integer;
begin
  // Get the current column and row
  Col := taskGrid.Col;
  Row := taskGrid.Row;

  // Get the rectangle of the cell
  CellRect := taskGrid.CellRect(Col, Row);

  // Calculate the position for the calendar dialog
  CalendarX := CellRect.CenterPoint.X; // Position to the left of the cell
  CalendarY := CellRect.CenterPoint.Y; // Position below the cell

  // Set the position of the calendar dialog
  calendarDialog.Left := CalendarX + Left;
  calendarDialog.Top := CalendarY + Top;

  if calendarDialog.Execute then
  begin
    // Set the selected date in the corresponding cell
    taskGrid.Cells[Col, Row] := DateToStr(calendarDialog.Date);
  end;
end;

procedure TformNotetask.taskGridValidateEntry(Sender: TObject; aCol, aRow: integer; const OldValue: string; var NewValue: string);
var
  DateTime: TDateTime;
begin
  if (aCol = 4) then
  begin
    if (NewValue <> '') and (not TryDateTime(NewValue, DateTime)) then
      abort;
  end;
end;

procedure TformNotetask.taskGridHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
begin
  taskGrid.EditorMode := False;
  if IsColumn then
  begin
    if Index = 0 then
      Tasks.FillGrid(taskGrid, taskGrid.SortOrder);
  end
  else
  begin
    taskGrid.Row := index;
  end;
end;

procedure TformNotetask.FormResize(Sender: TObject);
begin
  taskGridResize(Sender);
end;

procedure TformNotetask.taskGridCheckboxToggled(Sender: TObject; aCol, aRow: integer; aState: TCheckboxState);
begin
  if (aState = cbChecked) and (taskGrid.Cells[4, aRow] = '') then
    taskGrid.Cells[4, aRow] := FormatDateTime('dd.mm.yyyy hh:nn:ss', Now);
  Tasks.SetTask(taskGrid, aRow, aCol);
end;

procedure TformNotetask.taskGridHeaderSized(Sender: TObject; IsColumn: boolean; Index: integer);
begin
  taskGridResize(Sender);
  SaveGridSettings(taskGrid);
end;

procedure TformNotetask.taskGridMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint; var Handled: boolean);
begin
  taskGrid.EditorMode := False;
end;

procedure TformNotetask.taskGridResize(Sender: TObject);
var
  Rect: TRect;
begin
  // Get the cell dimensions
  Rect := taskGrid.CellRect(taskGrid.Col, taskGrid.Row);

  // Update the size and position of the Memo
  if Assigned(taskGrid.Editor) and (taskGrid.Editor is TMemo) then
  begin
    TMemo(taskGrid.Editor).SetBounds(Rect.Left + 4, Rect.Top, Rect.Right - Rect.Left - 5, Rect.Bottom - Rect.Top - 1);
  end;
end;

procedure TformNotetask.MemoChange(Sender: TObject);
begin
  taskGrid.Cells[taskGrid.Col, taskGrid.Row] := TMemo(Sender).Text;

  Tasks.SetTask(taskGrid, taskGrid.Row, taskGrid.Col);
end;

procedure TformNotetask.taskGridSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
var
  Memo: TMemo;
  Rect: TRect;
begin
  if aCol in [1, 4] then exit;
  Memo := TMemo.Create(Self);
  Memo.Color := clHighlight;
  Memo.Font.Name := taskGrid.Font.Name;
  Memo.Font.Size := taskGrid.Font.Size;
  Memo.Font.Color := clWhite;

  Memo.Visible := False;
  Memo.Parent := taskGrid;
  Memo.BorderStyle := bsNone;
  Memo.OnChange := @MemoChange; // Привязываем событие

  Rect := taskGrid.CellRect(aCol, aRow);
  Memo.SetBounds(Rect.Left + 4, Rect.Top, Rect.Right - Rect.Left - 5, Rect.Bottom - Rect.Top - 1);

  Memo.Text := taskGrid.Cells[aCol, aRow];
  Memo.WordWrap := True; // Включение многострочного режима
  Editor := Memo;
  Memo.Visible := True;
end;

end.
