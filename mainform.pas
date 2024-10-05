unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ActnList, ComCtrls,
  Types, CheckLst, ValEdit, Grids, Menus, PrintersDlgs, Printers, GridPrn, LCLIntf, LCLType,
  ExtDlgs, LMessages, Clipbrd, Process, task, lineending;

type

  { TformNotetask }
  TformNotetask = class(TForm)
    aMoveTaskBottom: TAction;
    aMoveTaskDown: TAction;
    aMoveTaskUp: TAction;
    aInsertTask: TAction;
    aMoveTaskTop: TAction;
    ADeleteTask: TAction;
    aWordWrap: TAction;
    aFont: TAction;
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
    menuInsertTask: TMenuItem;
    menuDeleteTask: TMenuItem;
    menuMoveTaskTop: TMenuItem;
    menuMoveTaskUp: TMenuItem;
    menuMoveTaskDown: TMenuItem;
    menuMoveTaskBottom: TMenuItem;
    menuWordWrap: TMenuItem;
    menuTask: TMenuItem;
    menuNewWindow: TMenuItem;
    menuOpen: TMenuItem;
    menuSave: TMenuItem;
    menuSaveAs: TMenuItem;
    menuPageProperties: TMenuItem;
    menuPrint: TMenuItem;
    menuNew: TMenuItem;
    openDialog: TOpenDialog;
    pageSetupDialog: TPageSetupDialog;
    printDialog: TPrintDialog;
    saveDialog: TSaveDialog;
    Separator1: TMenuItem;
    menuExit: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    statusBar: TStatusBar;
    taskGrid: TStringGrid;
    procedure ADeleteTaskExecute(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
    procedure aFontExecute(Sender: TObject);
    procedure aInsertTaskExecute(Sender: TObject);
    procedure aMoveTaskBottomExecute(Sender: TObject);
    procedure aMoveTaskDownExecute(Sender: TObject);
    procedure aMoveTaskTopExecute(Sender: TObject);
    procedure aMoveTaskUpExecute(Sender: TObject);
    procedure aNewExecute(Sender: TObject);
    procedure aNewWindowExecute(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure aPagePropertiesExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aSaveAsExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aWordWrapExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure taskGridCheckboxToggled(Sender: TObject; aCol, aRow: integer; aState: TCheckboxState);
    procedure taskGridColRowDeleted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
    procedure taskGridColRowInserted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
    procedure taskGridDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure taskGridEditButtonClick(Sender: TObject);
    procedure taskGridHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure taskGridHeaderSized(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure taskGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure taskGridMouseLeave(Sender: TObject);
    procedure taskGridMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure taskGridResize(Sender: TObject);
    procedure taskGridSelectCell(Sender: TObject; aCol, aRow: integer; var CanSelect: boolean);
    procedure taskGridSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
    procedure taskGridUserCheckboxBitmap(Sender: TObject; const aCol, aRow: integer; const CheckedState: TCheckboxState;
      var ABitmap: TBitmap);
    procedure taskGridValidateEntry(Sender: TObject; aCol, aRow: integer; const OldValue: string; var NewValue: string);
  private
    Memo: TMemo;
    FChanged: boolean;
    IsEditing: boolean;
    IsSelecting: boolean;
    FFileName: string;
    FEncoding: TEncoding;
    FLineEnding: TLineEnding;
    FLineCount: integer;
    FWordWrap: boolean;
    procedure MemoChange(Sender: TObject);
    procedure MemoSetBounds(aCol: integer; aRow: integer);
    procedure PrinterGetCellText(Sender: TObject; AGrid: TCustomGrid; ACol, ARow: integer; var AText: string);
    procedure PrinterPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    procedure SetChanged(aChanged: boolean = True);
    procedure EditCell(aCol, aRow: integer);
    procedure EditComplite;
    procedure SetInfo;
    procedure SetCaption;
    procedure ClearSelected;
    procedure DeleteTask(aRow: integer = 0);
    function IsCanClose: boolean;
  public
    procedure OpenFile(fileName: string);
    procedure SaveFile(fileName: string = string.Empty);

    property WordWrap: boolean read FWordWrap write FWordWrap;
  end;

var
  formNotetask: TformNotetask;
  Tasks: TTasks; // Tasks collection
  clRowHighlight: TColor;
  ResourceBitmapCheck: TBitmap;
  ResourceBitmapUncheck: TBitmap;

resourcestring
  rapp = 'Notetask';
  runtitled = 'Untitled';
  rrows = ' tasks';
  rdeleteconfirm = 'Are you sure you want to delete this task?';
  rsavechanges = 'Do you want to save the changes?';
  rclearconfirm = 'Are you sure you want to clear the data in the selected area?';
  ropendialogfilter = 'Task files (*.tsk)|*.tsk|Text files (*.txt)|*.txt|Markdown files (*.md)|*.md|All files (*.*)|*.*';
  rsavedialogfilter = 'Task files (*.tsk)|*.tsk|Text files (*.txt)|*.txt|Markdown files (*.md)|*.md|All files (*.*)|*.*';

implementation

uses filemanager, settings;

  {$R *.lfm}

  { TformNotetask }

procedure TformNotetask.FormCreate(Sender: TObject);
var
  FilePath: string;
begin
  FWordWrap := True;
  clRowHighlight := RGBToColor(210, 230, 255);
  openDialog.Filter := ropendialogfilter;
  saveDialog.Filter := rsavedialogfilter;

  // Create TBitmap objects
  ResourceBitmapCheck := TBitmap.Create;
  ResourceBitmapUncheck := TBitmap.Create;

  // Load bitmaps from resources
  ResourceBitmapCheck.LoadFromResourceName(HInstance, 'CHECK');
  ResourceBitmapUncheck.LoadFromResourceName(HInstance, 'UNCHECK');

  LoadFormSettings(self);
  LoadGridSettings(taskGrid);

  // After load wordwrap setting
  aWordWrap.Checked := FWordWrap;

  // Check if a command line argument is passed
  if ParamCount > 0 then
  begin
    FilePath := ParamStr(1); // Get the file path
    if (not FilePath.StartsWith('--')) then
    begin
      OpenFile(FilePath); // Your function to load a task from the file
      exit;
    end;
  end;

  aNew.Execute;
end;

procedure TformNotetask.FormDestroy(Sender: TObject);
begin
  SaveFormSettings(self);

  // Free allocated resources
  ResourceBitmapCheck.Free;
  ResourceBitmapUncheck.Free;
end;

procedure TformNotetask.ClearSelected;
var
  Confirm: integer;
  i, j: integer;
  Rect: TRect;
begin
  if (taskGrid.Selection.Width > 0) or (taskGrid.Selection.Height > 0) then
  begin
    // Show confirm delete dialog
    Confirm := MessageDlg(rclearconfirm, mtConfirmation, [mbYes, mbNo], 0);

    if Confirm = mrYes then
    begin
      Rect := taskGrid.Selection;
      for i := Rect.Top to Rect.Bottom do
      begin
        for j := Rect.Left to Rect.Right do
        begin
          if (j = 1) then Tasks.GetTask(i - 1).IsCompleted := False;
          if (j = 2) then Tasks.GetTask(i - 1).TaskDescription := '';
          if (j = 3) then Tasks.GetTask(i - 1).Comment := '';
          if (j = 4) then Tasks.GetTask(i - 1).CompletionDate := 0;
          if (j = 1) then
            taskGrid.Cells[j, i] := '0'
          else
            taskGrid.Cells[j, i] := string.Empty;
        end;
      end;
      SetChanged;
      FLineCount := Tasks.Count;
      SetInfo;
    end;
  end;
end;

procedure TformNotetask.DeleteTask(aRow: integer = 0);
var
  Row: integer;
  Confirm: integer;
begin
  // Get current row selected
  if (aRow = 0) then
    Row := taskGrid.Row
  else
    Row := aRow;
  if (Row > 0) and (Row <= Tasks.Count) then
  begin
    // Show confirm delete dialog
    Confirm := MessageDlg(rdeleteconfirm, mtConfirmation, [mbYes, mbNo], 0);

    if Confirm = mrYes then
    begin
      // RemoveTask from collection
      Tasks.RemoveTask(Row - 1);
      taskGrid.DeleteRow(Row);
      SetChanged;
    end;
  end;
end;

procedure TformNotetask.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) then // Del
  begin
    if (not taskGrid.EditorMode) and (not IsEditing) then
    begin
      if (taskGrid.Selection.Width > 0) or (taskGrid.Selection.Height > 0) then
      begin
        ClearSelected;
        Abort;
      end
      else
      begin
        DeleteTask;
        Abort;
      end;
    end
    else
      Abort;
  end
  else
  if (Key = VK_ESCAPE) then // Escape
  begin
    if (taskGrid.EditorMode) or (IsEditing) then
      EditComplite;
    Abort;
  end
  else
  if (Key = VK_F2) then // F2
  begin
    EditComplite;
    EditCell(taskGrid.Col, taskGrid.Row);
    Abort;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_C) then // Ctrl + C
  begin
    Tasks.CopyToClipboard(taskGrid);
    Abort;
  end
  else
  if (Shift = [ssCtrl]) and (Key = VK_PRIOR) then // Ctrl + Page Up
  begin
    aMoveTaskTop.Execute;
    Abort;
  end
  else
  if (Shift = [ssCtrl]) and (Key = VK_NEXT) then // Ctrl + Page Down
  begin
    aMoveTaskBottom.Execute;
    Abort;
  end
  else
  if (Shift = [ssCtrl]) and (Key = VK_UP) then // Ctrl + Up
  begin
    aMoveTaskUp.Execute;
    Abort;
  end
  else
  if (Shift = [ssCtrl]) and (Key = VK_DOWN) then // Ctrl + Down
  begin
    aMoveTaskDown.Execute;
    Abort;
  end;
end;

procedure TformNotetask.aExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TformNotetask.aFontExecute(Sender: TObject);
begin
  fontDialog.Font := Font;
  if fontDialog.Execute then  // Open the font dialog
  begin
    // Apply the selected font to the form
    Self.Font := fontDialog.Font;
  end;
end;

procedure TformNotetask.aInsertTaskExecute(Sender: TObject);
begin
  Tasks.InsertTask('[ ]', taskGrid.Row);
  Tasks.FillGrid(taskGrid);
  FLineCount += 1;
  SetInfo;
  SetChanged;
end;

procedure TformNotetask.aMoveTaskTopExecute(Sender: TObject);
begin
  Tasks.MoveTaskToTop(taskGrid.Row - 1);
  Tasks.FillGrid(taskGrid);
  taskGrid.Row := 1;
  SetChanged;
  Invalidate;
  Application.ProcessMessages;
end;

procedure TformNotetask.aMoveTaskUpExecute(Sender: TObject);
begin
  Tasks.MoveTaskUp(taskGrid.Row - 1);
  Tasks.FillGrid(taskGrid);
  if taskGrid.Row > 1 then taskGrid.Row := taskGrid.Row - 1;
  SetChanged;
  Invalidate;
  Application.ProcessMessages;
end;

procedure TformNotetask.aMoveTaskDownExecute(Sender: TObject);
begin
  Tasks.MoveTaskDown(taskGrid.Row - 1);
  Tasks.FillGrid(taskGrid);
  if taskGrid.Row < taskGrid.RowCount - 1 then taskGrid.Row := taskGrid.Row + 1;
  SetChanged;
  Invalidate;
  Application.ProcessMessages;
end;

procedure TformNotetask.aMoveTaskBottomExecute(Sender: TObject);
begin
  Tasks.MoveTaskToBottom(taskGrid.Row - 1);
  Tasks.FillGrid(taskGrid);
  taskGrid.Row := taskGrid.RowCount - 1;
  SetChanged;
  Invalidate;
  Application.ProcessMessages;
end;

procedure TformNotetask.ADeleteTaskExecute(Sender: TObject);
begin
  DeleteTask;
end;

procedure TformNotetask.aNewExecute(Sender: TObject);
begin
  if IsCanClose then
  begin
    Tasks := TTasks.Create();
    SetChanged(False);
    EditComplite;
    FFileName := string.Empty;
    FEncoding := TEncoding.UTF8;
    FLineEnding := FLineEnding.WindowsCRLF;
    taskGrid.Clean;
    taskGrid.RowCount := 2;
    FLineCount := 1;

    Tasks.AddTask('[ ]');
    taskGrid.Cells[1, 1] := '0';
    SetInfo;
  end;
end;

procedure TformNotetask.aNewWindowExecute(Sender: TObject);
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil); // Create a new process
  try
    Process.Executable := ParamStr(0); // Set the executable to the current application
    Process.Options := []; // No wait, open and forget
    Process.Execute; // Execute the new instance
  finally
    Process.Free; // Free the process object
  end;
end;

procedure TformNotetask.aOpenExecute(Sender: TObject);
begin
  if (IsCanClose) and (openDialog.Execute) then
  begin
    OpenFile(openDialog.FileName);
  end;
end;

procedure TformNotetask.aPagePropertiesExecute(Sender: TObject);
begin
  pageSetupDialog.Execute;
end;

procedure TformNotetask.aPrintExecute(Sender: TObject);
var
  gridPrinter: TGridPrinter;
begin
  if printDialog.Execute then
  begin
    gridPrinter := TGridPrinter.Create(self);
    gridPrinter.Grid := taskGrid;
    gridPrinter.OnGetCellText := @PrinterGetCellText;
    gridPrinter.OnPrepareCanvas := @PrinterPrepareCanvas;
    gridPrinter.Orientation := Printer.Orientation;
    gridPrinter.Margins.LeftMargin := pageSetupDialog.MarginLeft / 100;
    gridPrinter.Margins.RightMargin := pageSetupDialog.MarginRight / 100;
    gridPrinter.Margins.TopMargin := pageSetupDialog.MarginTop / 100;
    gridPrinter.Margins.BottomMargin := pageSetupDialog.MarginBottom / 100;

    gridPrinter.FixedLineColor := clSilver;
    gridPrinter.BorderLineColor := clSilver;
    gridPrinter.FooterLineColor := clSilver;
    gridPrinter.GridLineColor := clSilver;
    gridPrinter.HeaderLineColor := clSilver;

    gridPrinter.Print;
    gridPrinter.Free;
  end;
end;

procedure TformNotetask.PrinterPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
var
  MyTextStyle: TTextStyle;
begin
  if (aCol in [2, 3]) then;
  begin
    MyTextStyle := TGridPrinter(Sender).Canvas.TextStyle;
    MyTextStyle.SingleLine := not FWordWrap;
    MyTextStyle.Wordbreak := WordWrap;
    TGridPrinter(Sender).Canvas.TextStyle := MyTextStyle;
  end;
end;

procedure TformNotetask.PrinterGetCellText(Sender: TObject; AGrid: TCustomGrid; ACol, ARow: integer; var AText: string);
begin
  if AGrid is TStringGrid then
    AText := TStringGrid(AGrid).Cells[ACol, ARow];
end;

procedure TformNotetask.aSaveAsExecute(Sender: TObject);
begin
  if (saveDialog.Execute) then
  begin
    SaveFile(saveDialog.FileName);
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
    taskGrid.RowHeights[i] := taskGrid.DefaultRowHeight;
  end;
  Invalidate;
end;

function TformNotetask.IsCanClose: boolean;
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
      else
        Result := False;
    end;
  end
  else
    Result := True; // No changes, just close the form
end;

procedure TformNotetask.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := IsCanClose;
end;

procedure TformNotetask.OpenFile(fileName: string);
var
  Content: string;
  i: integer;
begin
  FFileName := fileName;
  EditComplite;
  ReadTextFile(FFileName, Content, FEncoding, FLineEnding, FLineCount);
  SetInfo;

  Tasks := TTasks.Create(TextToStringList(Content));
  Tasks.FillGrid(taskGrid);

  for i := 0 to taskGrid.RowCount - 1 do
  begin
    taskGrid.RowHeights[i] := taskGrid.DefaultRowHeight;
  end;
  SetChanged(False);
end;

procedure TformNotetask.SaveFile(fileName: string = string.Empty);
begin
  if (fileName = string.Empty) and (FFileName = string.Empty) then
    aSaveAs.Execute;

  if (fileName = string.Empty) then
    fileName := FFileName
  else
    FFileName := fileName;

  if (fileName <> string.Empty) then
  begin
    EditComplite;
    SaveTextFile(fileName, Tasks.ToStringList, FEncoding, FLineEnding);
    SetChanged(False);
  end;

  SetInfo;
end;

procedure TformNotetask.SetInfo;
begin
  statusBar.Panels[1].Text := UpperCase(FEncoding.EncodingName);
  statusBar.Panels[2].Text := FLineEnding.ToString;
  statusBar.Panels[3].Text := FLineCount.ToString + rrows;
  SetCaption;
end;

procedure TformNotetask.SetCaption;
begin
  if (FFileName <> string.Empty) then
    Caption := ExtractFileName(FFileName) + ' - ' + rapp
  else
    Caption := runtitled + ' - ' + rapp;

  if (FChanged) then
    Caption := '*' + Caption;
end;

procedure TformNotetask.SetChanged(aChanged: boolean = True);
begin
  if (aChanged = False) then
    taskGrid.Modified := False;

  FChanged := taskGrid.Modified or aChanged;
  aSave.Enabled := FChanged;
  SetCaption;
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
    begin
      SetChanged;

      Tasks.SetTask(taskGrid, taskGrid.Row, taskGrid.Col);
      EditComplite;
    end;
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

procedure TformNotetask.FormShow(Sender: TObject);
begin
  SetCaption;
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
    FLineCount += 1;
    SetInfo;
  end;
end;

procedure TformNotetask.taskGridColRowDeleted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
begin
  if (not IsColumn) then
  begin
    Tasks.RemoveTask(tIndex - 1);
    FLineCount -= 1;
    SetInfo;
  end;
end;

procedure TformNotetask.taskGridHeaderSized(Sender: TObject; IsColumn: boolean; Index: integer);
begin
  taskGridResize(Sender);
  SaveGridSettings(taskGrid);
end;

procedure TformNotetask.taskGridSelectCell(Sender: TObject; aCol, aRow: integer; var CanSelect: boolean);
begin
  IsSelecting := True;
end;

procedure TformNotetask.taskGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  IsSelecting := False;
end;

procedure TformNotetask.taskGridMouseLeave(Sender: TObject);
begin
  IsSelecting := False;
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
  MemoSetBounds(taskGrid.Col, taskGrid.Row);
  Tasks.SetTask(taskGrid, taskGrid.Row, taskGrid.Col);
  SetChanged;
end;

procedure TformNotetask.MemoSetBounds(aCol: integer; aRow: integer);
var
  Rect: TRect;
begin
  Rect := taskGrid.CellRect(aCol, aRow);
  Memo.SetBounds(Rect.Left + 5, Rect.Top + 1, Rect.Right - Rect.Left - 10, Rect.Bottom - Rect.Top - 3);
end;

procedure TformNotetask.taskGridDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
var
  grid: TStringGrid;
  S: string;
  drawrect: TRect;
  bgFill: TColor;
  flags: cardinal;
begin
  grid := Sender as TStringGrid;

  // Border for fixed cells
  if (aRow < grid.FixedRows) or (aCol < grid.FixedCols) then
  begin
    grid.Canvas.Pen.Color := clSilver;
    grid.Canvas.Pen.Style := psSolid;
    grid.Canvas.Pen.Width := 1;
    grid.Canvas.Brush.Style := bsClear;
    grid.Canvas.Rectangle(aRect.Left - 1, aRect.Top - 1, aRect.Right, aRect.Bottom);
  end
  else
  begin
    // Drawing only data cells
    //if (aCol in [0]) or (aRow = 0) then exit;

    // Determine background color
    if (gdSelected in aState) and ((taskGrid.Selection.Height > 0) or (taskGrid.Selection.Width > 0)) then
    begin
      bgFill := clHighlight;
      grid.Canvas.Font.Color := clWhite; // Set font color to white when selected
    end
    else
    if gdRowHighlight in aState then
    begin
      bgFill := clRowHighlight;
      grid.Canvas.Font.Color := clBlack;
    end
    else
    begin
      bgFill := clWhite;
      grid.Canvas.Font.Color := clBlack;
    end;

    // Fill the cell background
    grid.Canvas.Brush.Color := bgFill;
    grid.canvas.Brush.Style := bsSolid;
    grid.canvas.fillrect(aRect);

    if (aCol = 1) then
    begin
      Grid.DefaultDrawCell(aCol, aRow, aRect, aState);
      exit;
    end;

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

      //if gdFocused in aState then
      //  DrawFocusRect(grid.canvas.handle, drawrect);

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
end;

procedure TformNotetask.taskGridSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
var
  h, w: integer;
begin
  if (aCol in [1, 4]) then exit;
  if (Assigned(Memo)) then Memo.Free;

  Memo := TMemo.Create(Self);
  Memo.Visible := False;
  w := taskGrid.Selection.Width;
  h := taskGrid.Selection.Height;
  if (taskGrid.IsCellSelected[aCol, aRow]) and ((taskGrid.Selection.Height > 0) or (taskGrid.Selection.Width > 0)) then
  begin
    Memo.Color := clHighlight;
    Memo.Font.Color := clWhite;
  end
  else
  begin
    Memo.Color := clRowHighlight;
    Memo.Font.Color := clBlack;
  end;
  Memo.Font.Name := taskGrid.Font.Name;
  Memo.Font.Size := taskGrid.Font.Size;
  Memo.HideSelection := True;
  Memo.TabStop := False;
  Memo.WantTabs := True;
  Memo.BorderStyle := bsNone;
  Memo.WordWrap := FWordWrap;
  Memo.WantReturns := FWordWrap;
  Memo.ScrollBars := ssAutoVertical;

  MemoSetBounds(aCol, aRow);
  Memo.Parent := taskGrid;
  Memo.OnChange := @MemoChange; // Event
  Memo.Text := taskGrid.Cells[aCol, aRow];
  Memo.CaretPos := Point(Length(Memo.Text), 0);

  Editor := Memo;

  if (IsSelecting) or (taskGrid.Selection.Height > 0) or (taskGrid.Selection.Width > 0) then
  begin
    IsSelecting := False;
    Memo.Visible := False;
    IsEditing := False;
  end
  else
  begin
    Memo.Visible := True;
    IsEditing := True;
  end;
end;

procedure TformNotetask.taskGridUserCheckboxBitmap(Sender: TObject; const aCol, aRow: integer;
  const CheckedState: TCheckboxState; var ABitmap: TBitmap);
begin
  // Check if we're in the correct column
  if aCol = 1 then
  begin
    // Assign the appropriate bitmap based on the CheckedState
    ABitmap := TBitmap.Create;
    if CheckedState = cbChecked then
      ABitmap.Assign(ResourceBitmapCheck) // Use check bitmap
    else
      ABitmap.Assign(ResourceBitmapUncheck); // Use uncheck bitmap
  end;
end;

procedure TformNotetask.EditCell(aCol, aRow: integer);
begin
  taskGrid.Row := aRow;
  taskGrid.Col := aCol;
  IsEditing := True;
  taskGrid.EditorMode := True; //Set editing mode

  if (Assigned(Memo)) and (Memo.Visible) then
  begin
    Memo.SelectAll;
    Memo.SetFocus;
  end;
end;

procedure TformNotetask.EditComplite;
begin
  taskGrid.EditorMode := False;
  IsEditing := False; // Reset editing flag when exiting
end;

end.
