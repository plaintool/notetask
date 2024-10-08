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
    aArchiveTasks: TAction;
    aAbout: TAction;
    aCopy: TAction;
    aUndoAll: TAction;
    aDelete: TAction;
    aDateTime: TAction;
    aSelectAll: TAction;
    aGoTo: TAction;
    aReplace: TAction;
    aFindPrev: TAction;
    aFindNext: TAction;
    aFind: TAction;
    aPaste: TAction;
    aCut: TAction;
    aUndo: TAction;
    AShowArchived: TAction;
    aShowStatusBar: TAction;
    aMoveTaskBottom: TAction;
    aMoveTaskDown: TAction;
    aMoveTaskUp: TAction;
    aInsertTask: TAction;
    aMoveTaskTop: TAction;
    ADeleteTasks: TAction;
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
    menuDeleteTasks: TMenuItem;
    menuArchiveTasks: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    menuUndoAll: TMenuItem;
    menuPaste: TMenuItem;
    menuCopy: TMenuItem;
    menuCut: TMenuItem;
    MenuItem9: TMenuItem;
    menuShowArchived: TMenuItem;
    menuFindNext: TMenuItem;
    menuFindPrev: TMenuItem;
    menuReplace: TMenuItem;
    menuGoTo: TMenuItem;
    menuDateTime: TMenuItem;
    menuShowStatusBar: TMenuItem;
    menuHelp: TMenuItem;
    menuAbout: TMenuItem;
    menuEdit: TMenuItem;
    menuUndo: TMenuItem;
    menuDelete: TMenuItem;
    menuFind: TMenuItem;
    menuView: TMenuItem;
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
    Popup: TPopupMenu;
    printDialog: TPrintDialog;
    saveDialog: TSaveDialog;
    Separator1: TMenuItem;
    menuExit: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    menuSelectAll: TMenuItem;
    Separator6: TMenuItem;
    Separator7: TMenuItem;
    Separator8: TMenuItem;
    statusBar: TStatusBar;
    taskGrid: TStringGrid;
    procedure aArchiveTasksExecute(Sender: TObject);
    procedure aCopyExecute(Sender: TObject);
    procedure aDateTimeExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure ADeleteTasksExecute(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
    procedure aFontExecute(Sender: TObject);
    procedure aGoToExecute(Sender: TObject);
    procedure aInsertTaskExecute(Sender: TObject);
    procedure aMoveTaskTopExecute(Sender: TObject);
    procedure aMoveTaskBottomExecute(Sender: TObject);
    procedure aMoveTaskUpExecute(Sender: TObject);
    procedure aMoveTaskDownExecute(Sender: TObject);
    procedure aNewExecute(Sender: TObject);
    procedure aNewWindowExecute(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure aPagePropertiesExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aSaveAsExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aSelectAllExecute(Sender: TObject);
    procedure AShowArchivedExecute(Sender: TObject);
    procedure aShowStatusBarExecute(Sender: TObject);
    procedure aUndoAllExecute(Sender: TObject);
    procedure aUndoExecute(Sender: TObject);
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
    procedure taskGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
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
    FIsEditing: boolean;
    FIsSelecting: boolean;
    FFileName: string;
    FEncoding: TEncoding;
    FLineEnding: TLineEnding;
    FLineCount: integer;
    FWordWrap: boolean;
    FShowStatusBar: boolean;
    FSortOrder: TSortOrder;
    FSortColumn: integer;
    procedure MemoChange(Sender: TObject);
    procedure MemoSetBounds(aCol: integer; aRow: integer);
    procedure PrinterGetCellText(Sender: TObject; AGrid: TCustomGrid; ACol, ARow: integer; var AText: string);
    procedure PrinterPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    procedure SetChanged(aChanged: boolean = True);
    procedure EditCell(aCol, aRow: integer);
    procedure EditComplite(Validate: boolean = True);
    procedure SetInfo;
    procedure SetCaption;
    procedure ClearSelected;
    procedure DeleteTask(aRow: integer = 0);
    procedure DeleteTasks;
    procedure ArchiveTask(aRow: integer = 0);
    procedure ArchiveTasks;
    procedure SetShowStatusBar(Value: boolean);
    procedure SetShowArchived(Value: boolean);
    function GetIsEditing: boolean;
    procedure CompleteTasks(aRow: integer = 0);
    procedure ResetRowHeight(aRow: integer = 0; aHeight: integer = 0);
    procedure SwapRowHeights(RowIndex1, RowIndex2: integer);
    function IsCanClose: boolean;
  public
    FShowArchived: boolean;

    procedure OpenFile(fileName: string);
    procedure SaveFile(fileName: string = string.Empty);

    property WordWrap: boolean read FWordWrap write FWordWrap;
    property ShowStatusBar: boolean read FShowStatusBar write SetShowStatusBar;
    property ShowArchived: boolean read FShowArchived write SetShowArchived;
    property SortOrder: TSortOrder read FSortOrder write FSortOrder;
    property SortColumn: integer read FSortColumn write FSortColumn;
    property IsEditing: boolean read GetIsEditing write FIsEditing;
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
  rdeletesconfirm = 'Are you sure you want to delete selected tasks?';
  rarchiveconfirm = 'Are you sure you want to archive / unarchive this task?';
  rarchivesconfirm = 'Are you sure you want to archive / unarchive selected tasks?';
  rsavechanges = 'Do you want to save the changes?';
  rclearconfirm = 'Are you sure you want to clear the data in the selected area?';
  ropendialogfilter = 'Task files (*.tsk)|*.tsk|Text files (*.txt)|*.txt|Markdown files (*.md)|*.md|All files (*.*)|*.*';
  rsavedialogfilter = 'Task files (*.tsk)|*.tsk|Text files (*.txt)|*.txt|Markdown files (*.md)|*.md|All files (*.*)|*.*';
  rundoconfirm = 'Are you sure you want to discard all changes? This action cannot be undone.';

implementation

uses filemanager, settings, forminput;

  {$R *.lfm}

  { TformNotetask }

procedure TformNotetask.FormCreate(Sender: TObject);
var
  FilePath: string;
begin
  FWordWrap := True;
  FShowStatusBar := True;
  FSortOrder := soAscending;
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
  aShowStatusBar.Checked := FShowStatusBar;
  aShowArchived.Checked := FShowArchived;

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
      Tasks.ClearTasksInRect(taskGrid, taskGrid.Selection);
      //Rect := taskGrid.Selection;
      //for i := Rect.Top to Rect.Bottom do
      //begin
      //  for j := Rect.Left to Rect.Right do
      //  begin
      //    if (j = 1) then Tasks.GetTask(i).IsCompleted := False;
      //    if (j = 2) then Tasks.GetTask(i).TaskDescription := '';
      //    if (j = 3) then Tasks.GetTask(i).Comment := '';
      //    if (j = 4) then Tasks.GetTask(i).CompletionDate := 0;
      //    if (j = 1) then
      //      taskGrid.Cells[j, i] := '0'
      //    else
      //      taskGrid.Cells[j, i] := string.Empty;
      //  end;
      //end;
      SetChanged;
      FLineCount := Tasks.Count;
      SetInfo;
    end;
  end;
end;

procedure TformNotetask.DeleteTask(aRow: integer = 0);
var
  RowIndex: integer;
  Confirm: integer;
begin
  // Get current RowIndex selected
  if (aRow = 0) then
    RowIndex := taskGrid.Row
  else
    RowIndex := aRow;
  if (RowIndex > 0) and (RowIndex <= Tasks.Count) then
  begin
    // Show confirm delete dialog
    Confirm := MessageDlg(rdeleteconfirm, mtConfirmation, [mbYes, mbNo], 0);

    if Confirm = mrYes then
    begin
      Tasks.CreateBackup;

      // RemoveTask from collection
      //Tasks.DeleteTask(RowIndex);
      taskGrid.DeleteRow(RowIndex);
      SetChanged;
    end;
  end;
end;

procedure TformNotetask.DeleteTasks;
var
  i, RowIndex, Confirm: integer;
begin
  // If multiple rows are selected
  if (taskGrid.Selection.Width > 0) or (taskGrid.Selection.Height > 0) then
  begin
    // Request confirmation for deletion
    Confirm := MessageDlg(rdeletesconfirm, mtConfirmation, [mbYes, mbNo], 0);

    if Confirm = mrYes then
    begin
      Tasks.CreateBackup;

      // Delete rows from the end to avoid index shifting
      for i := taskGrid.Selection.Bottom downto taskGrid.Selection.Top do
      begin
        RowIndex := i;
        if (RowIndex > 0) and (RowIndex <= Tasks.Count) then
        begin
          // Remove the task from the collection
          //Tasks.DeleteTask(RowIndex);
          taskGrid.DeleteRow(RowIndex);
        end;
      end;
      SetChanged; // Mark that data has changed
    end;
  end
  else
    DeleteTask;
end;

procedure TformNotetask.ArchiveTask(aRow: integer = 0);
var
  RowIndex: integer;
  Confirm: integer;
begin
  // Get current RowIndex selected
  if (aRow = 0) then
    RowIndex := taskGrid.Row
  else
    RowIndex := aRow;
  if (RowIndex > 0) and (RowIndex <= Tasks.Count) then
  begin
    // Show confirm delete dialog
    Confirm := MessageDlg(rarchiveconfirm, mtConfirmation, [mbYes, mbNo], 0);

    if Confirm = mrYes then
    begin
      Tasks.CreateBackup;

      // Archivate task
      Tasks.ArchiveTask(RowIndex);
      Tasks.FillGrid(taskGrid, FShowArchived, SortOrder, SortColumn);
      SetChanged;
    end;
  end;
end;

procedure TformNotetask.ArchiveTasks;
var
  i, RowIndex, Confirm: integer;
begin
  // If multiple rows are selected
  if (taskGrid.Selection.Width > 0) or (taskGrid.Selection.Height > 0) then
  begin
    // Request confirmation for archiving
    Confirm := MessageDlg(rarchivesconfirm, mtConfirmation, [mbYes, mbNo], 0);

    if Confirm = mrYes then
    begin
      Tasks.CreateBackup;

      // Archive tasks from the end to avoid index shifting
      for i := taskGrid.Selection.Bottom downto taskGrid.Selection.Top do
      begin
        RowIndex := i;
        if (RowIndex > 0) and (RowIndex <= Tasks.Count) then
        begin
          // Archive the task from the collection
          Tasks.ArchiveTask(RowIndex);
        end;
      end;
      Tasks.FillGrid(taskGrid, FShowArchived, SortOrder, SortColumn);
      SetChanged; // Mark that data has changed
    end;
  end
  else
    ArchiveTask;
  Invalidate;
end;

procedure TformNotetask.CompleteTasks(aRow: integer = 0);
var
  RowIndex, Confirm: integer;
  i: integer;
begin
  // If multiple rows are selected
  if (taskGrid.Selection.Width > 0) or (taskGrid.Selection.Height > 0) then
  begin
    // Mark tasks as completed from the end to avoid index shifting
    for i := taskGrid.Selection.Bottom downto taskGrid.Selection.Top do
    begin
      RowIndex := i;
      if (RowIndex > 0) and (RowIndex <= Tasks.Count) then
      begin
        // Mark the task as completed in the collection
        Tasks.CompleteTask(RowIndex);
        if Tasks.GetTask(RowIndex).IsCompleted then
          taskGrid.Cells[1, RowIndex] := '1'
        else
          taskGrid.Cells[1, RowIndex] := '0';
      end;
    end;
    SetChanged; // Mark that data has changed
  end
  else
  begin
    // Get current RowIndex selected if no multiple selection
    if (aRow = 0) then
      RowIndex := taskGrid.Row
    else
      RowIndex := aRow;

    if (RowIndex > 0) and (RowIndex <= Tasks.Count) then
    begin
      // Mark the task as completed in the collection
      Tasks.CompleteTask(RowIndex);
      if Tasks.GetTask(RowIndex).IsCompleted then
        taskGrid.Cells[1, RowIndex] := '1'
      else
        taskGrid.Cells[1, RowIndex] := '0';
      SetChanged; // Mark that data has changed
    end;
  end;
  Invalidate;
end;

procedure TformNotetask.ResetRowHeight(aRow: integer = 0; aHeight: integer = 0);
var
  i: integer;
begin
  if (aRow = 0) then
  begin
    for i := 0 to taskGrid.RowCount - 1 do
    begin
      if aHeight = 0 then
        taskGrid.RowHeights[i] := taskGrid.DefaultRowHeight
      else
        taskGrid.RowHeights[i] := aHeight;
    end;
  end
  else
  begin
    if aHeight = 0 then
      taskGrid.RowHeights[aRow] := taskGrid.DefaultRowHeight
    else
      taskGrid.RowHeights[aRow] := aHeight;
  end;
end;

procedure TformNotetask.aDeleteExecute(Sender: TObject);
begin
  if not IsEditing then
  begin
    if (taskGrid.Selection.Width > 0) or (taskGrid.Selection.Height > 0) then
      ClearSelected
    else
      DeleteTask;
  end;
end;

procedure TformNotetask.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_DELETE) then // Ctrl + Del
  begin
    if not IsEditing then
      DeleteTasks;
    Key := 0;
  end
  else
  if (Key = VK_DELETE) then // Del
  begin
    if not IsEditing then
    begin
      aDelete.Execute;
      Key := 0;
    end;
  end
  else
  if (Key = VK_F2) then // F2
  begin
    EditComplite;
    EditCell(taskGrid.Col, taskGrid.Row);
    Key := 0;
  end
  else
  //if (ssCtrl in Shift) and (Key = VK_C) then // Ctrl + C
  //begin
  //  Tasks.CopyToClipboard(taskGrid);
  //  Key := 0;
  //end
  //else
  if (Shift = [ssCtrl]) and (Key = VK_PRIOR) then // Ctrl + Page Up
  begin
    aMoveTaskTop.Execute;
    Key := 0;
  end
  else
  if (Shift = [ssCtrl]) and (Key = VK_NEXT) then // Ctrl + Page Down
  begin
    aMoveTaskBottom.Execute;
    Key := 0;
  end
  else
  if (Shift = [ssCtrl]) and (Key = VK_UP) then // Ctrl + Up
  begin
    aMoveTaskUp.Execute;
    Key := 0;
  end
  else
  if (Shift = [ssCtrl]) and (Key = VK_DOWN) then // Ctrl + Down
  begin
    aMoveTaskDown.Execute;
    Key := 0;
  end
  else
  if (Shift = [ssCtrl, ssShift]) and (Key = VK_UP) then // Ctrl + Shift + Up
  begin
    if IsEditing then
    begin
      EditComplite;
      if taskGrid.Row > 0 then
        taskGrid.Row := taskGrid.Row - 1;
      Key := 0;
    end;
  end
  else
  if (Shift = [ssCtrl, ssShift]) and (Key = VK_DOWN) then // Ctrl + Shift + Down
  begin
    if IsEditing then
    begin
      EditComplite;
      if (taskGrid.Row < taskGrid.RowCount - 1) then
        taskGrid.Row := taskGrid.Row + 1;
      Key := 0;
    end;
  end
  else
  if (Key = VK_SPACE) then // Space
  begin
    if not IsEditing then
    begin
      CompleteTasks;
      Key := 0;
    end;
  end
  else
  if (Key = VK_ESCAPE) then // Escape
  begin
    if IsEditing then
      EditComplite;
    Key := 0;
  end
  else
  if (Shift = [ssCtrl]) and (Key = VK_RETURN) then // Ctrl + Enter
  begin
    if IsEditing then
    begin
      EditComplite;
      Key := 0;
    end;
  end
  else
  if (Shift = [ssCtrl]) and (Key = VK_A) then // Ctrl + A
  begin
    if not IsEditing then
      aSelectAll.Execute
    else
    if (Assigned(Memo)) then
      Memo.SelectAll;
    Key := 0;
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

//procedure TformNotetask.aGoToExecute(Sender: TObject);
//var
//  rowNumStr: string;
//  rowNum: Integer;
//begin
//  // Prompt the user to enter the row number
//  rowNumStr := InputBox('Перейти к строке', 'Введите номер строки:', '');

//  // Try to convert the input to an integer
//  if TryStrToInt(rowNumStr, rowNum) then
//  begin
//    // Ensure the entered row is within the valid range
//    if (rowNum >= 1) and (rowNum <= taskGrid.RowCount - 1) then
//    begin
//      // Move to the specified row
//      taskGrid.Row := rowNum;
//    end
//    else
//      ShowMessage('Номер строки выходит за пределы допустимого диапазона.');
//  end
//  else
//    ShowMessage('Введите допустимое число.');
//end;

procedure TformNotetask.aGoToExecute(Sender: TObject);
var
  rowNum: integer;
begin
  // Create an instance of the form
  with formInputText do
  try
    Left := self.Left + 14;
    Top := self.top + 52;
    editText.Text := IntToStr(taskGrid.Row);

    // Show the form as a modal dialog
    if ShowModal = mrOk then
    begin
      // Try to convert the entered value to an integer
      if TryStrToInt(editText.Text, rowNum) then
      begin
        // Ensure the entered row is within the valid range
        if (rowNum >= 1) and (rowNum <= taskGrid.RowCount - 1) then
        begin
          // Move to the specified row
          taskGrid.Row := rowNum;
        end
        else
          ShowMessage('Номер строки выходит за пределы допустимого диапазона.');
      end;
    end;
  finally
    Hide;
  end;
end;

procedure TformNotetask.aInsertTaskExecute(Sender: TObject);
begin
  Tasks.InsertTask('[ ]', taskGrid.Row);
  Tasks.FillGrid(taskGrid, FShowArchived, SortOrder, SortColumn);
  FLineCount += 1;
  SetInfo;
  SetChanged;
end;

procedure TformNotetask.SwapRowHeights(RowIndex1, RowIndex2: integer);
var
  TempHeight: integer;
begin
  // Check if the row indices are valid
  if (RowIndex1 < 0) or (RowIndex1 >= taskGrid.RowCount) or (RowIndex2 < 0) or (RowIndex2 >= taskGrid.RowCount) then
    Exit; // Exit if the indices are invalid

  // Store the height of the first row
  TempHeight := taskGrid.RowHeights[RowIndex1];

  // Swap the heights of the two rows
  taskGrid.RowHeights[RowIndex1] := taskGrid.RowHeights[RowIndex2];
  taskGrid.RowHeights[RowIndex2] := TempHeight;
end;

procedure TformNotetask.aMoveTaskTopExecute(Sender: TObject);
var
  newRow: integer;
begin
  newRow := Tasks.MoveTaskTop(taskGrid.Row);
  Tasks.FillGrid(taskGrid, FShowArchived, SortOrder, SortColumn);
  if (newRow > -1) then
  begin
    SwapRowHeights(taskGrid.Row, newRow);
    taskGrid.Row := newRow;
  end;
  SetChanged;
end;

procedure TformNotetask.aMoveTaskBottomExecute(Sender: TObject);
var
  newRow: integer;
begin
  newRow := Tasks.MoveTaskBottom(taskGrid.Row);
  Tasks.FillGrid(taskGrid, FShowArchived, SortOrder, SortColumn);
  if (newRow > -1) then
  begin
    SwapRowHeights(taskGrid.Row, newRow);
    taskGrid.Row := newRow;
  end;
  SetChanged;
end;

procedure TformNotetask.aMoveTaskUpExecute(Sender: TObject);
var
  newRow: integer;
begin
  newRow := Tasks.MoveTaskUp(taskGrid.Row);
  Tasks.FillGrid(taskGrid, FShowArchived, SortOrder, SortColumn);
  if (newRow > -1) then
  begin
    SwapRowHeights(taskGrid.Row, newRow);
    taskGrid.Row := newRow;
  end;
  SetChanged;
end;

procedure TformNotetask.aMoveTaskDownExecute(Sender: TObject);
var
  newRow: integer;
begin
  newRow := Tasks.MoveTaskDown(taskGrid.Row);
  Tasks.FillGrid(taskGrid, FShowArchived, SortOrder, SortColumn);
  if (newRow > -1) then
  begin
    SwapRowHeights(taskGrid.Row, newRow);
    taskGrid.Row := newRow;
  end;
  SetChanged;
end;

procedure TformNotetask.ADeleteTasksExecute(Sender: TObject);
begin
  DeleteTasks;
end;

procedure TformNotetask.aArchiveTasksExecute(Sender: TObject);
begin
  ArchiveTasks;
end;

procedure TformNotetask.aCopyExecute(Sender: TObject);
begin
  Tasks.CopyToClipboard(taskGrid);
end;

procedure TformNotetask.aDateTimeExecute(Sender: TObject);
var
  PosStart: integer;
  CurrentDateTime: string;
begin
  CurrentDateTime := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat, Now);
  if IsEditing then
  begin
    if (taskGrid.Col = 4) then
    begin
      taskGrid.Cells[4, taskGrid.Row] := CurrentDateTime;
    end
    else
    begin
      PosStart := Memo.SelStart;
      Memo.SelText := CurrentDateTime;
      Memo.SelStart := PosStart + Length(CurrentDateTime);
    end;
    SetChanged;
  end
  else
  begin
    if (taskGrid.Col > 0) then
    begin
      if taskGrid.Cells[taskGrid.Col, taskGrid.Row].Trim = string.Empty then
        taskGrid.Cells[taskGrid.Col, taskGrid.Row] := CurrentDateTime
      else
        taskGrid.Cells[taskGrid.Col, taskGrid.Row] := taskGrid.Cells[taskGrid.Col, taskGrid.Row].Trim + ' ' + CurrentDateTime;
      SetChanged;
    end;
  end;
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

    Tasks.InitMap(1);
    Tasks.AddMap(Tasks.AddTask('[ ]'));
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

procedure TformNotetask.aSelectAllExecute(Sender: TObject);
begin
  if not IsEditing then
  begin
    taskGrid.Selection := TGridRect.Create(0, 0, 4, taskGrid.RowCount);
  end;
end;

procedure TformNotetask.AShowArchivedExecute(Sender: TObject);
begin
  ShowArchived := aShowArchived.Checked;
end;

procedure TformNotetask.aShowStatusBarExecute(Sender: TObject);
begin
  ShowStatusBar := aShowStatusBar.Checked;
end;

procedure TformNotetask.aUndoExecute(Sender: TObject);
begin
  Tasks.UndoBackup;
  Tasks.FillGrid(taskGrid, FShowArchived, SortOrder, SortColumn);
end;

procedure TformNotetask.aUndoAllExecute(Sender: TObject);
var
  Confirm: TModalResult;
begin
  Confirm := MessageDlg(rundoconfirm, mtConfirmation, [mbYes, mbNo], 0);

  if Confirm = mrYes then
  begin
    Tasks.UndoBackupInit;
    Tasks.FillGrid(taskGrid, FShowArchived, SortOrder, SortColumn);
    ResetRowHeight;
    Tasks.CreateBackup;
    SetChanged(False);
  end;
end;

procedure TformNotetask.SetShowStatusBar(Value: boolean);
begin
  FShowStatusBar := Value;
  StatusBar.Visible := FShowStatusBar;
end;

procedure TformNotetask.SetShowArchived(Value: boolean);
begin
  FShowArchived := Value;
  Tasks.FillGrid(taskGrid, FShowArchived, SortOrder, SortColumn);
end;

procedure TformNotetask.aWordWrapExecute(Sender: TObject);
var
  i: integer;
begin
  EditComplite;
  FWordWrap := aWordWrap.Checked;
  ResetRowHeight;
  Invalidate;
end;

function TformNotetask.GetIsEditing: boolean;
begin
  Result := (taskGrid.EditorMode) or (FIsEditing);
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
  Tasks.FillGrid(taskGrid, FShowArchived, SortOrder, SortColumn);

  ResetRowHeight;
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
    Tasks.CreateBackupInit;
  end;

  SetInfo;
end;

procedure TformNotetask.SetInfo;
begin
  statusBar.Panels[1].Text := UpperCase(GetEncodingName(FEncoding));
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
  aUndo.Enabled := FChanged;
  aUndoAll.Enabled := FChanged;
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

procedure TformNotetask.taskGridHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
begin
  EditComplite;
  if IsColumn then
  begin
    if (FSortColumn <> Index) then
      SortOrder := soAscending
    else
    if SortOrder = soAscending then
      SortOrder := soDescending
    else
      SortOrder := soAscending;

    FSortColumn := Index;

    Tasks.FillGrid(taskGrid, FShowArchived, SortOrder, SortColumn);

    aMoveTaskTop.Enabled := SortColumn = 0;
    aMoveTaskBottom.Enabled := SortColumn = 0;
    aMoveTaskUp.Enabled := SortColumn = 0;
    aMoveTaskDown.Enabled := SortColumn = 0;
  end
  else
  begin
    // Set row when clicked on begining of row
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
  if (aState = cbChecked) then
  begin
    if (taskGrid.Cells[4, aRow] = '') then
      taskGrid.Cells[4, aRow] := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat, Now);
  end;
  //else
  //  taskGrid.Cells[4, aRow] := string.Empty;
  Tasks.SetTask(taskGrid, aRow, aCol);
end;

procedure TformNotetask.taskGridColRowInserted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
begin
  if (not IsColumn) then
  begin
    Tasks.AddMap(Tasks.AddTask('[ ]'));
    taskGrid.Cells[1, tIndex] := '0';
    FLineCount += 1;
    SetInfo;
  end;
end;

procedure TformNotetask.taskGridColRowDeleted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
begin
  if (not IsColumn) then
  begin
    Tasks.DeleteTask(tIndex);
    //Tasks.FillGrid(taskGrid, FShowArchived, SortOrder, SortColumn);
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
  FIsSelecting := True;
end;

procedure TformNotetask.taskGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FIsSelecting := False;
end;

procedure TformNotetask.taskGridMouseLeave(Sender: TObject);
begin
  FIsSelecting := False;
end;

procedure TformNotetask.taskGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbRight) and (not IsEditing) then
    Popup.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
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

    if (aCol = 2) and (Tasks.HasTask(ARow) and Tasks.GetTask(ARow).IsArchive) then
      grid.Canvas.Font.Style := [fsStrikeOut];

    // Fill the cell background
    grid.Canvas.Brush.Color := bgFill;
    grid.canvas.Brush.Style := bsSolid;
    grid.canvas.fillrect(aRect);

    if (aCol = 1) then
    begin
      grid.DefaultDrawCell(aCol, aRow, aRect, aState);
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

procedure TformNotetask.MemoChange(Sender: TObject);
var
  Rect: TRect;
begin
  taskGrid.Cells[taskGrid.Col, taskGrid.Row] := TMemo(Sender).Text;
  Tasks.SetTask(taskGrid, taskGrid.Row, taskGrid.Col);
  SetChanged;
  MemoSetBounds(taskGrid.Col, taskGrid.Row);
end;

procedure TformNotetask.MemoSetBounds(aCol: integer; aRow: integer);
var
  Rect: TRect;
begin
  Application.ProcessMessages;
  Rect := taskGrid.CellRect(aCol, aRow);
  Memo.SetBounds(Rect.Left + 5, Rect.Top + 1, Rect.Right - Rect.Left - 10, Rect.Bottom - Rect.Top - 2);
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
  Memo.ScrollBars := ssNone;

  MemoSetBounds(aCol, aRow);
  Memo.Parent := taskGrid;
  Memo.OnChange := @MemoChange; // Event
  Memo.Text := taskGrid.Cells[aCol, aRow];
  Memo.CaretPos := Point(Length(Memo.Text), 0);

  Editor := Memo;

  if (FIsSelecting) or (taskGrid.Selection.Height > 0) or (taskGrid.Selection.Width > 0) then
  begin
    FIsSelecting := False;
    Memo.Visible := False;
    FIsEditing := False;
  end
  else
  begin
    Memo.Visible := True;
    FIsEditing := True;
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
  FIsEditing := True;
  taskGrid.EditorMode := True; //Set editing mode

  if (Assigned(Memo)) and (Memo.Visible) then
  begin
    Memo.SelectAll;
    Memo.SetFocus;
  end;
end;

procedure TformNotetask.taskGridValidateEntry(Sender: TObject; aCol, aRow: integer; const OldValue: string; var NewValue: string);
var
  DateTime: TDateTime;
begin
  if (aCol = 4) then
  begin
    if (NewValue <> '') and (not TryStrToDateTime(NewValue, DateTime)) then
      Abort;

    if (OldValue <> NewValue) then
    begin
      SetChanged;

      Tasks.SetTask(taskGrid, taskGrid.Row, taskGrid.Col);
      EditComplite(False);
    end;
  end;
end;

procedure TformNotetask.EditComplite(Validate: boolean = True);
var
  oldValue, newValue: string;
begin
  if taskGrid.EditorMode then
  begin
    if (Validate) then
    begin
      oldValue := Tasks.GetTaskValue(taskGrid.Col, taskGrid.Row);
      newvalue := taskGrid.Cells[taskGrid.Col, taskGrid.Row];
      taskGrid.OnValidateEntry(taskGrid, taskGrid.Col, taskGrid.Row, oldValue, newValue);
    end;
    taskGrid.EditorMode := False;
    FIsEditing := False; // Reset editing flag when exiting
  end;
end;

end.
