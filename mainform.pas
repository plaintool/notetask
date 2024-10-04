unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Types, CheckLst, ValEdit, Grids, Menus, ActnList, ComCtrls, PrintersDlgs,
  LCLIntf, LCLType, ExtDlgs, LMessages, task, lineending;

type

  { TformNotetask }
  TformNotetask = class(TForm)
    aWordWrap: TAction;
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
    MenuItem1: TMenuItem;
    menuWordWrap: TMenuItem;
    menuTask: TMenuItem;
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
    procedure aSaveExecute(Sender: TObject);
    procedure aWordWrapExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure taskGridCheckboxToggled(Sender: TObject; aCol, aRow: integer; aState: TCheckboxState);
    procedure taskGridColRowDeleted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
    procedure taskGridColRowInserted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
    procedure taskGridDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure taskGridEditButtonClick(Sender: TObject);
    procedure taskGridEditingDone(Sender: TObject);
    procedure taskGridHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure taskGridHeaderSized(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure taskGridMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure taskGridPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    procedure taskGridResize(Sender: TObject);
    procedure taskGridSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
    procedure taskGridValidateEntry(Sender: TObject; aCol, aRow: integer; const OldValue: string; var NewValue: string);
  private
    Memo: TMemo;
    FChanged: boolean;
    IsEditing: boolean;
    NeedFocusMemo: boolean;
    FFileName: string;
    FEncoding: TEncoding;
    FLineEnding: TLineEnding;
    FWordWrap: boolean;
    procedure MemoChange(Sender: TObject);
    procedure MemoExit(Sender: TObject);
  public
    procedure OpenFile(fileName: string);
    procedure SaveFile(fileName: string);
    procedure SetChanged(aChanged: boolean = True);
    function CheckCanClose: boolean;
    procedure EditCell(aCol, aRow: integer);
    procedure EditComplite;
    property WordWrap: boolean read FWordWrap write FWordWrap;
  end;

var
  formNotetask: TformNotetask;
  Tasks: TTasks; // Tasks collection
  clRowHighlight: TColor;

resourcestring
  rrows = ' rows';
  rdeleteconfirm = 'Are you sure you want to delete this task?';
  rsavechanges = 'Do you want to save the changes?';

implementation

uses filemanager, settings;

  {$R *.lfm}

  { TformNotetask }

procedure TformNotetask.FormCreate(Sender: TObject);
var
  FilePath: string;
begin
  FWordWrap := True;

  LoadFormSettings(self);
  LoadGridSettings(taskGrid);

  aWordWrap.Checked := FWordWrap;
  clRowHighlight := RGBToColor(224, 224, 224);

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
  if (Key = VK_DELETE) then
  begin
    if (not taskGrid.EditorMode) and (not IsEditing) then
    begin
      Row := taskGrid.Row; // Get current row selected
      if (Row > 0) and (Row <= Tasks.Count) then
      begin
        // Show confirm delete dialog
        ConfirmDelete := MessageDlg(rdeleteconfirm, mtConfirmation, [mbYes, mbNo], 0);

        if ConfirmDelete = mrYes then
        begin
          // RemoveTask from collection
          Tasks.RemoveTask(Row - 1);
          taskGrid.DeleteRow(Row);
        end;
        Abort;
      end;
    end
    else
      Abort;
  end
  else
  if (Key = VK_ESCAPE) then
  begin
    if (taskGrid.EditorMode) or (IsEditing) then
      EditComplite;
    Abort;
  end
  else
  if (Key = VK_RETURN) or (Key = VK_F2) then
  begin
    EditCell(taskGrid.Col, taskGrid.Row);
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
  if CheckCanClose then
  begin
    SetChanged(False);
    FFileName := string.Empty;
    taskGrid.Clean;
    taskGrid.RowCount := 2;
    Tasks.AddTask('[ ]');
    taskGrid.Cells[1, 1] := '0';
  end;
end;

procedure TformNotetask.aOpenExecute(Sender: TObject);
begin
  if openDialog.Execute then
  begin
    OpenFile(openDialog.FileName);
  end;
end;

procedure TformNotetask.aSaveExecute(Sender: TObject);
begin
  SaveFile(FFileName);
end;

procedure TformNotetask.aWordWrapExecute(Sender: TObject);
var
  i: integer;
begin
  EditComplite;
  FWordWrap := aWordWrap.Checked;
  for i := 0 to taskGrid.RowCount - 1 do
  begin
    taskGrid.RowHeights[i] := taskGrid.DefaultRowHeight; // Установить нужное значение высоты
  end;
  Invalidate;
end;

function TformNotetask.CheckCanClose: boolean;
var
  UserResponse: integer;
begin
  if FChanged then
  begin
    // Show message with Yes, No, and Cancel options
    UserResponse := MessageDlg(rsavechanges, mtConfirmation, [mbYes, mbNo, mbCancel], 0);

    case UserResponse of
      mrYes:
      begin
        // Call save method and allow form to close
        aSave.Execute;
        Result := True;
      end;
      mrNo:
      begin
        // Do not save, but allow form to close
        Result := True;
      end;
      mrCancel:
      begin
        // Cancel the form closing
        Result := False;
      end;
    end;
  end
  else
    Result := True; // No changes, just close the form
end;

procedure TformNotetask.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CheckCanClose;
end;

procedure TformNotetask.OpenFile(fileName: string);
var
  Content: string;
  LineCount: integer;
begin
  FFileName := fileName;

  ReadTextFile(FFileName, Content, FEncoding, FLineEnding, LineCount);
  statusBar.Panels[1].Text := UpperCase(FEncoding.EncodingName);
  statusBar.Panels[2].Text := FLineEnding.ToString;
  statusBar.Panels[3].Text := LineCount.ToString + rrows;

  Tasks := TTasks.Create(TextToStringList(Content));
  Tasks.FillGrid(taskGrid);
end;

procedure TformNotetask.SaveFile(fileName: string);
begin
  if (fileName = string.Empty) and (saveDialog.Execute) then
  begin
    FFileName := saveDialog.FileName;
    fileName := FFileName;
  end;

  if (fileName <> string.Empty) then
  begin
    SaveTextFile(fileName, Tasks.ToStringList, FEncoding, FLineEnding);
    SetChanged(False);
  end;
end;

procedure TformNotetask.SetChanged(aChanged: boolean = True);
begin
  if (aChanged = False) then
    taskGrid.Modified := False;

  FChanged := taskGrid.Modified;
  aSave.Enabled := FChanged;
  if (FChanged) and (Caption <> '') and (Caption[1] <> '*') then
    Caption := '*' + Caption
  else if not FChanged and (Caption <> '') and (Caption[1] = '*') then
    Caption := Copy(Caption, 2, Length(Caption) - 1);
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
    if (NewValue <> '') and (not TryStrToDateTime(NewValue, DateTime)) then
      abort;

    if (OldValue <> NewValue) then
      SetChanged;
  end;
end;

procedure TformNotetask.taskGridHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
begin
  EditComplite;
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
  SetChanged;
  if (aState = cbChecked) and (taskGrid.Cells[4, aRow] = '') then
    taskGrid.Cells[4, aRow] := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat, Now)
  else
    taskGrid.Cells[4, aRow] := string.Empty;
  Tasks.SetTask(taskGrid, aRow, aCol);
end;

procedure TformNotetask.taskGridColRowInserted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
begin
  if (not IsColumn) then
  begin
    Tasks.AddTask('[ ]');
    taskGrid.Cells[1, tIndex] := '0';
  end;
end;

procedure TformNotetask.taskGridColRowDeleted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
begin
  if (not IsColumn) then
    Tasks.RemoveTask(tIndex);
end;

procedure TformNotetask.taskGridHeaderSized(Sender: TObject; IsColumn: boolean; Index: integer);
begin
  taskGridResize(Sender);
  SaveGridSettings(taskGrid);
end;

procedure TformNotetask.taskGridMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint; var Handled: boolean);
begin
  EditComplite;
end;

procedure TformNotetask.taskGridResize(Sender: TObject);
var
  Rect: TRect;
begin
  // Get the cell dimensions
  Rect := taskGrid.CellRect(taskGrid.Col, taskGrid.Row);

  // Update the size and position of the Memo
  if Assigned(taskGrid.Editor) and (taskGrid.Editor is TMemo) then
    TMemo(taskGrid.Editor).SetBounds(Rect.Left + 5, Rect.Top + 1, Rect.Right - Rect.Left - 10, Rect.Bottom - Rect.Top - 3);
end;

procedure TformNotetask.MemoChange(Sender: TObject);
begin
  taskGrid.Cells[taskGrid.Col, taskGrid.Row] := TMemo(Sender).Text;

  Tasks.SetTask(taskGrid, taskGrid.Row, taskGrid.Col);
  SetChanged;
end;

procedure TformNotetask.taskGridDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
var
  grid: TStringGrid;
  S: string;
  drawrect: TRect;
  bgFill: TColor;
  flags: cardinal;
begin
  if (aCol in [0, 1]) or (aRow = 0) then exit;
  grid := Sender as TStringGrid;

  // Determine background color
  if gdRowHighlight in aState then
  begin
    bgFill := clRowHighlight;
    grid.Canvas.Font.Color := clBlack; // Set font color to white when selected
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
    if (FWordWrap) then
      flags := dt_calcrect or dt_wordbreak or dt_left
    else
      flags := dt_calcrect or dt_left;
    DrawText(grid.canvas.handle, PChar(S), Length(S), drawrect, flags);

    if gdFocused in aState then
      DrawFocusRect(grid.canvas.handle, drawrect);

    if (drawrect.bottom - drawrect.top) > grid.RowHeights[ARow] then
      grid.RowHeights[ARow] := (drawrect.bottom - drawrect.top + 2) // changing the row height fires the event again!
    else
    begin
      drawrect.Right := aRect.Right;
      // grid.canvas.fillrect(drawrect);
      if (FWordWrap) then
        flags := dt_wordbreak or dt_left
      else
        flags := dt_left;
      DrawText(grid.canvas.handle, PChar(S), Length(S), drawrect, dt_wordbreak or dt_left);
    end;
  end;
end;

procedure TformNotetask.taskGridSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
var
  Rect: TRect;
begin
  if aCol in [1, 4] then exit;
  Memo := TMemo.Create(Self);
  Memo.Color := clRowHighlight;
  Memo.Font.Name := taskGrid.Font.Name;
  Memo.Font.Size := taskGrid.Font.Size;
  Memo.Font.Color := clBlack;

  Memo.Visible := False;
  Memo.Parent := taskGrid;
  Memo.BorderStyle := bsNone;
  Memo.OnChange := @MemoChange; // Event
  Memo.OnExit := @MemoExit; // Set OnExit event

  Rect := taskGrid.CellRect(aCol, aRow);
  Memo.SetBounds(Rect.Left + 5, Rect.Top + 1, Rect.Right - Rect.Left - 10, Rect.Bottom - Rect.Top - 3);

  Memo.Text := taskGrid.Cells[aCol, aRow];
  Memo.WordWrap := FWordWrap; // WordWrap setting
  Memo.WantReturns := FWordWrap;
  Editor := Memo;
  Memo.CaretPos := Point(Length(Memo.Text), 0);
  Memo.Visible := True;
  IsEditing := True;
  if (NeedFocusMemo) then
  begin
    Memo.SelectAll;
    Memo.SetFocus;
    NeedFocusMemo := False;
  end;
end;

procedure TformNotetask.MemoExit(Sender: TObject);
begin
  //  IsEditing := False; // Reset editing flag when exiting
end;

procedure TformNotetask.EditCell(aCol, aRow: integer);
begin
  taskGrid.Row := aRow;
  taskGrid.Col := aCol;
  taskGrid.EditorMode := True; //Set editing mode
  IsEditing := True;
  NeedFocusMemo := True;
end;

procedure TformNotetask.EditComplite;
begin
  taskGrid.EditorMode := False;
  IsEditing := False; // Reset editing flag when exiting
  NeedFocusMemo := False;
end;

procedure TformNotetask.taskGridEditingDone(Sender: TObject);
begin
  if (taskGrid.Col = 4) then
  begin
    Tasks.SetTask(taskGrid, taskGrid.Row, taskGrid.Col);
    EditComplite;
  end;
end;

end.
