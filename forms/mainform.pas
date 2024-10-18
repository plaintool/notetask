//----------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under CC BY-NC-SA 4.0
//  Full license text: https://creativecommons.org/licenses/by-nc-sa/4.0/
//----------------------------------------------------------------------

unit mainform;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ActnList,
  ComCtrls,
  Grids,
  Menus,
  PrintersDlgs,
  DateTimePicker,
  Printers,
  LCLIntf,
  LCLType,
  Process,
  StrUtils,
  GridPrn,
  task,
  lineending;

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
    aShowArchived: TAction;
    aShowStatusBar: TAction;
    aMoveTaskBottom: TAction;
    aMoveTaskDown: TAction;
    aMoveTaskUp: TAction;
    aInsertTask: TAction;
    aMoveTaskTop: TAction;
    aDeleteTasks: TAction;
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
    aLangEnglish: TAction;
    menuLanguage: TMenuItem;
    Separator9: TMenuItem;
    menuEnglish: TMenuItem;
    menuRussian: TMenuItem;
    aLangRussian: TAction;
    aLangGerman: TAction;
    menuGerman: TMenuItem;
    aBidiRightToLeft: TAction;
    MenuItem6: TMenuItem;
    aChatGpt: TAction;
    Separator10: TMenuItem;
    menuChatGpt: TMenuItem;
    Separator11: TMenuItem;
    MenuItem7: TMenuItem;
    aLangSpanish: TAction;
    aLangFrench: TAction;
    aLangItalian: TAction;
    aLangPortuguese: TAction;
    aLangJapanese: TAction;
    aLangKorean: TAction;
    aLangChinese: TAction;
    menuSpanish: TMenuItem;
    menuFrench: TMenuItem;
    menuItalian: TMenuItem;
    menuPortuguese: TMenuItem;
    menuJapanese: TMenuItem;
    menuKorean: TMenuItem;
    menuChinese: TMenuItem;
    aDonate: TAction;
    menuDonate: TMenuItem;
    MenuItem8: TMenuItem;
    Separator12: TMenuItem;
    MenuItem10: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure taskGridCheckboxToggled(Sender: TObject; aCol, aRow: integer; aState: TCheckboxState);
    procedure taskGridColRowDeleted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
    procedure taskGridColRowInserted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
    procedure taskGridDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure taskGridHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure taskGridHeaderSized(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure taskGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure taskGridMouseLeave(Sender: TObject);
    procedure taskGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure taskGridMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure taskGridResize(Sender: TObject);
    procedure taskGridSelectCell(Sender: TObject; aCol, aRow: integer; var CanSelect: boolean);
    procedure taskGridSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
    procedure taskGridUserCheckboxBitmap(Sender: TObject; const aCol, aRow: integer; const CheckedState: TCheckboxState; var ABitmap: TBitmap);
    procedure taskGridColRowMoved(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
    procedure aArchiveTasksExecute(Sender: TObject);
    procedure aCopyExecute(Sender: TObject);
    procedure aCutExecute(Sender: TObject);
    procedure aDateTimeExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure aDeleteTasksExecute(Sender: TObject);
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
    procedure aPasteExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aSaveAsExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aSelectAllExecute(Sender: TObject);
    procedure aShowArchivedExecute(Sender: TObject);
    procedure aShowStatusBarExecute(Sender: TObject);
    procedure aUndoAllExecute(Sender: TObject);
    procedure aUndoExecute(Sender: TObject);
    procedure aWordWrapExecute(Sender: TObject);
    procedure aFindExecute(Sender: TObject);
    procedure aReplaceExecute(Sender: TObject);
    procedure aFindNextExecute(Sender: TObject);
    procedure aFindPrevExecute(Sender: TObject);
    procedure aAboutExecute(Sender: TObject);
    procedure aLangEnglishExecute(Sender: TObject);
    procedure aLangRussianExecute(Sender: TObject);
    procedure aLangGermanExecute(Sender: TObject);
    procedure aBidiRightToLeftExecute(Sender: TObject);
    procedure aChatGptExecute(Sender: TObject);
    procedure aLangSpanishExecute(Sender: TObject);
    procedure aLangFrenchExecute(Sender: TObject);
    procedure aLangItalianExecute(Sender: TObject);
    procedure aLangPortugueseExecute(Sender: TObject);
    procedure aLangJapaneseExecute(Sender: TObject);
    procedure aLangKoreanExecute(Sender: TObject);
    procedure aLangChineseExecute(Sender: TObject);
    procedure aDonateExecute(Sender: TObject);
  private
    Memo: TMemo;
    DatePicker: TDateTimePicker;
    FChanged: boolean;
    FBackup: boolean;
    FMemoStartEdit: boolean;
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
    FMatchCase: boolean;
    FWrapAround: boolean;
    FBiDiRightToLeft: boolean;
    FFindActive: boolean;
    FFindText: string;
    FFoundText: string;
    FLastFoundRow, FLastFoundCol, FLastFoundSelStart, FLastFoundSelLength: integer;
    procedure MemoChange(Sender: TObject);
    procedure MemoEnter(Sender: TObject);
    procedure DatePickerChange(Sender: TObject);
    procedure EditControlSetBounds(Sender: TWinControl; aCol, aRow: integer; OffsetLeft: integer = 5; OffsetTop: integer = 1;
      OffsetRight: integer = -10; OffsetBottom: integer = -2);
    procedure PrinterGetCellText(Sender: TObject; AGrid: TCustomGrid; ACol, ARow: integer; var AText: string);
    procedure PrinterPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    procedure SetChanged(aChanged: boolean = True);
    procedure EditCell(aCol, aRow: integer);
    procedure EditComplite;
    procedure SetInfo;
    procedure SetCaption;
    procedure ClearSelected(ShowConfirm: boolean = True);
    procedure DeleteTask(aRow: integer = 0; ShowConfirm: boolean = True);
    procedure DeleteTasks(ShowConfirm: boolean = True);
    procedure ArchiveTask(aRow: integer = 0);
    procedure ArchiveTasks;
    procedure SetShowStatusBar(Value: boolean);
    procedure SetShowArchived(Value: boolean);
    procedure SetBiDiRightToLeft(Value: boolean);
    procedure CompleteTasks(aRow: integer = 0);
    procedure ResetRowHeight(aRow: integer = 0; aHeight: integer = 0);
    procedure SwapRowHeights(RowIndex1, RowIndex2: integer);
    function GetIsEditing: boolean;
    function IsCanClose: boolean;
  public
    FShowArchived: boolean;

    procedure SetLanguage(aLanguage: string = string.Empty);
    function OpenFile(fileName: string): boolean;
    procedure SaveFile(fileName: string = string.Empty);
    procedure FillGrid;

    function Find(aText: string; aMatchCase, aWrapAround, aDirectionDown: boolean; Silent: boolean = False): boolean;
    function Replace(aText, aToText: string; aMatchCase, aWrapAround: boolean): boolean;
    function ReplaceAll(aText, aToText: string; aMatchCase, aWrapAround: boolean): boolean;

    property WordWrap: boolean read FWordWrap write FWordWrap;
    property BiDiRightToLeft: boolean read FBiDiRightToLeft write SetBiDiRightToLeft;
    property ShowStatusBar: boolean read FShowStatusBar write SetShowStatusBar;
    property ShowArchived: boolean read FShowArchived write SetShowArchived;
    property SortOrder: TSortOrder read FSortOrder write FSortOrder;
    property SortColumn: integer read FSortColumn write FSortColumn;
    property IsEditing: boolean read GetIsEditing write FIsEditing;
    property FindText: string read FFindText write FFindText;
    property MatchCase: boolean read FMatchCase write FMatchCase;
    property WrapAround: boolean read FWrapAround write FWrapAround;
  end;

var
  formNotetask: TformNotetask;
  Tasks: TTasks; // Tasks collection
  clRowHighlight: TColor;
  clRowExpired: TColor;
  ResourceBitmapCheck: TBitmap;
  ResourceBitmapUncheck: TBitmap;

resourcestring
  rapp = 'Notetask';
  runtitled = 'Untitled';
  rrows = ' tasks';
  rcantfind = 'Can''t find';
  rfilenotfound = 'The requested file was not found on the disk.';
  rdeleteconfirm = 'Are you sure you want to delete this task?';
  rdeletesconfirm = 'Are you sure you want to delete selected tasks?';
  rarchiveconfirm = 'Are you sure you want to archive / unarchive this task?';
  rarchivesconfirm = 'Are you sure you want to archive / unarchive selected tasks?';
  rsavechanges = 'Do you want to save the changes?';
  rclearconfirm = 'Are you sure you want to clear the data in the selected area?';
  ropendialogfilter = 'Task files (*.tsk)|*.tsk|Text files (*.txt)|*.txt|Markdown files (*.md)|*.md|All files (*.*)|*.*';
  rsavedialogfilter = 'Task files (*.tsk)|*.tsk|Text files (*.txt)|*.txt|Markdown files (*.md)|*.md|All files (*.*)|*.*';
  rundoconfirm = 'Are you sure you want to discard all changes? This action cannot be undone.';
  rnumstringtoolarge = 'The line number is out of the allowed range.';
  rchatgpt = 'https://chatgpt.com?q=';

implementation

uses filemanager, settings, stringtool, systemtool, forminput, formfind, formreplace, formabout, formdonate;

  {$R *.lfm}

  { TformNotetask }

procedure TformNotetask.FormCreate(Sender: TObject);
var
  FilePath: string;
begin
  // Initialize variables
  FBackup := True;
  FWordWrap := True;
  FBiDiRightToLeft := self.BiDiMode = bdRightToLeft;
  FShowStatusBar := True;
  FSortOrder := soAscending;
  clRowHighlight := RGBToColor(210, 230, 255);
  clRowExpired := RGBToColor(255, 220, 220);
  openDialog.Filter := ropendialogfilter;
  saveDialog.Filter := rsavedialogfilter;

  {$IFDEF Linux}
  taskGrid.DefaultRowHeight := 33;
  {$ENDIF}

  // Create TBitmap objects
  ResourceBitmapCheck := TBitmap.Create;
  ResourceBitmapUncheck := TBitmap.Create;

  // Load bitmaps from resources
  ResourceBitmapCheck.LoadFromResourceName(HInstance, 'CHECK');
  ResourceBitmapUncheck.LoadFromResourceName(HInstance, 'UNCHECK');

  LoadFormSettings(self);
  LoadGridSettings(taskGrid);

  // Set language
  SetLanguage;

  // After load wordwrap setting
  aWordWrap.Checked := FWordWrap;
  aBidiRightToLeft.Checked := FBiDiRightToLeft;
  aShowStatusBar.Checked := FShowStatusBar;
  aShowArchived.Checked := FShowArchived;

  // Check if a command line argument is passed
  if ParamCount > 0 then
  begin
    FilePath := ParamStr(1); // Get the file path
    if (not FilePath.StartsWith('--')) then
    begin
      if OpenFile(FilePath) then // Function to load a task from the file
        exit;
    end;
  end;

  aNew.Execute;
end;

procedure TformNotetask.FormDestroy(Sender: TObject);
begin
  SaveFormSettings(self);

  // Free allocated resources
  FLineEnding.Free;
  Tasks.Free;
  ResourceBitmapCheck.Free;
  ResourceBitmapUncheck.Free;
end;

procedure TformNotetask.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Screen.ActiveForm <> Self then
  begin
    Key := 0;
    Exit;
  end;

  if (ssCtrl in Shift) and (Key = VK_DELETE) then // Ctrl + Del
  begin
    if not IsEditing then
      DeleteTasks;
    Key := 0;
  end
  else
  if (Key = VK_F2) then // F2
  begin
    EditComplite;
    EditCell(taskGrid.Col, taskGrid.Row);
    Key := 0;
  end
  else
  if (Key = VK_DELETE) then // Del
  begin
    aDelete.Execute;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (not (ssShift in Shift)) and (Key = VK_Z) then // Ctrl + Z
  begin
    aUndo.Execute;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_X) then // Ctrl + X
  begin
    aCut.Execute;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_C) then // Ctrl + C
  begin
    aCopy.Execute;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_V) then // Ctrl + V
  begin
    aPaste.Execute;
    Key := 0;
  end
  else
  if (Shift = [ssCtrl]) and (Key = VK_A) then // Ctrl + A
  begin
    aSelectAll.Execute;
    Key := 0;
  end
  else
  if (Shift = [ssCtrl]) and (Key = VK_F) then // Ctrl + F
  begin
    aFind.Execute;
    Key := 0;
  end
  else
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
  if ((Shift = [ssCtrl]) or not FWordWrap) and (Key = VK_RETURN) then // Ctrl + Enter
  begin
    if IsEditing then
    begin
      EditComplite;
      Key := 0;
    end;
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

procedure TformNotetask.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := IsCanClose;
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

    FillGrid;

    aMoveTaskTop.Enabled := SortColumn = 0;
    aMoveTaskBottom.Enabled := SortColumn = 0;
    aMoveTaskUp.Enabled := SortColumn = 0;
    aMoveTaskDown.Enabled := SortColumn = 0;
    if (SortColumn = 0) then
      taskGrid.Options := taskGrid.Options + [goRowMoving]
    else
      taskGrid.Options := taskGrid.Options - [goRowMoving];
  end
  else
    // Set row when clicked on begining of row
  begin
    if (ssShift in GetKeyShiftState) and (taskGrid.Selection.Height = 0) and (taskGrid.Selection.Top <> index) then
    begin
      taskGrid.Selection := TGridRect.Create(1, taskGrid.Selection.Top, 4, index);
    end
    else
    begin
      taskGrid.Row := index;
      taskGrid.Selection := TGridRect.Create(1, index, 4, index);
    end;
  end;
end;

procedure TformNotetask.taskGridCheckboxToggled(Sender: TObject; aCol, aRow: integer; aState: TCheckboxState);
begin
  SetChanged;
  if (aState = cbChecked) then
  begin
    if (taskGrid.Cells[4, aRow] = '') then
      taskGrid.Cells[4, aRow] := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat, Now);
  end;
  Tasks.SetTask(taskGrid, aRow, FBackup);
end;

procedure TformNotetask.taskGridColRowInserted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
begin
  if (not IsColumn) then
  begin
    if FBackup then Tasks.CreateBackup;
    Tasks.AddMap(Tasks.AddTask('[ ]'));
    taskGrid.Cells[1, tIndex] := '0';
    FLineCount += 1;
    SetInfo;
    SetChanged();
  end;
end;

procedure TformNotetask.taskGridColRowDeleted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
begin
  if (not IsColumn) then
  begin
    Tasks.DeleteTask(tIndex);
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
var
  Cell: TPoint;
begin
  if (Button = mbRight) and (not IsEditing) then
  begin
    if (taskGrid.Selection.Height = 0) then
    begin
      // Get the row index at the mouse coordinates
      Cell := taskGrid.MouseToCell(TPoint.Create(X, Y));

      // Check if the row index is valid
      if (Cell.Y >= 0) and (Cell.Y < taskGrid.RowCount) then
        taskGrid.Row := Cell.Y;
      if (Cell.X > 0) and (Cell.X < 5) then
        taskGrid.Col := Cell.X;
    end;
    Popup.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

procedure TformNotetask.taskGridMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
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
  task: TTask;
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
      task := Tasks.GetTask(ARow);
      if (not task.Done) and (task.Date > 0) and (task.Date < Now) then // Color expired task
      begin
        bgFill := clRowExpired;
        grid.Canvas.Font.Color := clBlack;
      end
      else
      if (not task.Done) and (task.Archive) then
      begin
        bgFill := clWhite;
        grid.Canvas.Font.Color := clMaroon;
      end
      else
      begin
        bgFill := clWhite;
        grid.Canvas.Font.Color := clBlack;
      end;
    end;

    if (aCol = 2) and (Tasks.HasTask(ARow) and Tasks.GetTask(ARow).Archive) then
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

      flags := DT_CALCRECT;
      if FBiDiRightToLeft then
        flags := flags + DT_RIGHT
      else
        flags := flags + DT_LEFT;
      if FWordWrap then
        flags := flags or DT_WORDBREAK;

      DrawText(grid.canvas.handle, PChar(S), Length(S), drawrect, flags);

      if (drawrect.bottom - drawrect.top) > grid.RowHeights[ARow] then
        grid.RowHeights[ARow] := (drawrect.bottom - drawrect.top + 2) // changing the row height fires the event again!
      else
      begin
        drawrect.Right := aRect.Right - 4;
        if FBiDiRightToLeft then
          flags := DT_RIGHT
        else
          flags := DT_LEFT;
        if FWordWrap then
          flags := flags or DT_WORDBREAK;
        DrawText(grid.canvas.handle, PChar(S), Length(S), drawrect, flags);
      end;
    end;
  end;
end;

procedure TformNotetask.taskGridSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
var
  sDateTime: TDateTime;
begin
  if (aCol in [2, 3]) then
  begin
    if (Assigned(Memo)) then Memo.Free;

    Memo := TMemo.Create(Self);
    Memo.Visible := False;
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
    Memo.HideSelection := False;
    Memo.TabStop := False;
    Memo.WantTabs := True;
    Memo.BorderStyle := bsNone;
    Memo.WordWrap := FWordWrap;
    Memo.WantReturns := FWordWrap;
    Memo.ScrollBars := ssNone;
    if (FBiDiRightToLeft) then
      Memo.BiDiMode := bdRightToLeft
    else
      Memo.BiDiMode := bdLeftToRight;

    EditControlSetBounds(Memo, aCol, aRow);
    Memo.Parent := taskGrid;
    Memo.OnChange := @MemoChange; // Event Change
    Memo.OnEnter := @MemoEnter; //Event Enter
    Memo.Text := taskGrid.Cells[aCol, aRow];
    Memo.SelStart := Length(Memo.Text);
    Memo.SelLength := 0;
    //  Memo.CaretPos := Point(Length(Memo.Text), 0);

    Editor := Memo;

    if (FIsSelecting) or (taskGrid.Selection.Height > 0) or (taskGrid.Selection.Width > 0) then
    begin
      Memo.Visible := False;
      FIsSelecting := False;
      FIsEditing := False;
    end
    else
    begin
      Memo.Visible := True;
      FIsEditing := True;
      FMemoStartEdit := True;
    end;
  end
  else
  if (aCol = 4) then
  begin
    if (Assigned(DatePicker)) then DatePicker.Free;

    DatePicker := TDateTimePicker.Create(Self);
    DatePicker.AutoSize := False;
    DatePicker.Kind := dtkDateTime;
    DatePicker.TimeDisplay := tdHMS;
    DatePicker.Options := [dtpoFlatButton];
    if (FBiDiRightToLeft) then
      DatePicker.BiDiMode := bdRightToLeft
    else
      DatePicker.BiDiMode := bdLeftToRight;

    EditControlSetBounds(DatePicker, aCol, aRow, 0, 0, 0, 0);

    if (taskGrid.Cells[aCol, aRow] = string.Empty) then
      DatePicker.DateTime := Now
    else
    begin
      TryStrToDateTime(taskGrid.Cells[aCol, aRow], sDateTime);
      DatePicker.DateTime := sDateTime;
    end;

    DatePicker.OnChange := @DatePickerChange; // Event Change

    Editor := DatePicker;
    if (FIsSelecting) or (taskGrid.Selection.Height > 0) or (taskGrid.Selection.Width > 0) then
    begin
      DatePicker.Visible := False;
      FIsSelecting := False;
      FIsEditing := False;
    end
    else
    begin
      DatePicker.Visible := True;
      FIsEditing := True;
    end;
  end;
end;

procedure TformNotetask.taskGridUserCheckboxBitmap(Sender: TObject; const aCol, aRow: integer; const CheckedState: TCheckboxState; var ABitmap: TBitmap);
begin
  // Check if we're in the correct column
  if aCol = 1 then
  begin
    // Assign the appropriate bitmap based on the CheckedState
    if CheckedState = cbChecked then
      ABitmap := ResourceBitmapCheck // Use check bitmap
    else
      ABitmap := ResourceBitmapUncheck; // Use uncheck bitmap
  end;
end;

procedure TformNotetask.taskGridColRowMoved(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
begin
  if (not IsColumn) then
  begin
    Tasks.MoveTask(sIndex, tIndex);
    SetChanged;
  end;
end;

procedure TformNotetask.aUndoExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  if not IsEditing then
  begin
    Tasks.UndoBackup;
    FillGrid;
  end
  else
  if (taskGrid.InplaceEditor.InheritsFrom(TCustomEdit)) then
    (taskGrid.InplaceEditor as TCustomEdit).Undo;
end;

procedure TformNotetask.aUndoAllExecute(Sender: TObject);
var
  Confirm: TModalResult;
begin
  if Screen.ActiveForm <> Self then exit;

  if not IsEditing then
  begin
    // Need confirm?
    Confirm := MessageDlg(rundoconfirm, mtConfirmation, [mbYes, mbNo], 0);

    if Confirm = mrYes then
    begin
      Tasks.UndoBackupInit;
      FillGrid;
      ResetRowHeight;
      Tasks.CreateBackup;
      SetChanged(False);
    end;
  end;
end;

procedure TformNotetask.aCutExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;

  if not IsEditing then
  begin
    Tasks.CopyToClipboard(taskGrid);
    if (taskGrid.Selection.Width < 3) then
      ClearSelected(False)
    else
      DeleteTasks(False);
  end
  else
  if (taskGrid.InplaceEditor.InheritsFrom(TCustomEdit)) then
    (taskGrid.InplaceEditor as TCustomEdit).CutToClipboard;
end;

procedure TformNotetask.aCopyExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;

  if not IsEditing then
    Tasks.CopyToClipboard(taskGrid)
  else
  if (taskGrid.InplaceEditor.InheritsFrom(TCustomEdit)) then
    (taskGrid.InplaceEditor as TCustomEdit).CopyToClipboard;
end;

procedure TformNotetask.aPasteExecute(Sender: TObject);
var
  Sel: TGridRect;
begin
  if Screen.ActiveForm <> Self then exit;

  if not IsEditing then
  begin
    Sel := Tasks.PasteFromClipboard(taskGrid);
    FillGrid;
    if (SortColumn = 0) then
      taskGrid.Selection := Sel;
    SetChanged;
  end
  else
  if (taskGrid.InplaceEditor.InheritsFrom(TCustomEdit)) then
    (taskGrid.InplaceEditor as TCustomEdit).PasteFromClipboard;
end;

procedure TformNotetask.aDeleteExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;

  if not IsEditing then
  begin
    //    if (taskGrid.Selection.Width > 0) or (taskGrid.Selection.Height > 0) then
    ClearSelected(False);
    //    else
    //      DeleteTask;
  end
  else
  if (taskGrid.InplaceEditor is TCustomEdit) then
    with taskGrid.InplaceEditor as TCustomEdit do
    begin
      if SelLength = 0 then
      begin
        SelStart := SelStart;
        SelLength := 1;
      end;
      ClearSelection;
    end;
end;

procedure TformNotetask.aSelectAllExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;

  if not IsEditing then
    taskGrid.Selection := TGridRect.Create(0, 0, 4, taskGrid.RowCount)
  else
  if (taskGrid.InplaceEditor.InheritsFrom(TCustomEdit)) then
    (taskGrid.InplaceEditor as TCustomEdit).SelectAll;
end;

procedure TformNotetask.aExitExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  Application.Terminate;
end;

procedure TformNotetask.aFontExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  fontDialog.Font := Font;
  if fontDialog.Execute then  // Open the font dialog
  begin
    // Apply the selected font to the form
    Self.Font := fontDialog.Font;
  end;
end;

procedure TformNotetask.aInsertTaskExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  Tasks.InsertTask('[ ]', taskGrid.Row);
  FillGrid;
  FLineCount += 1;
  taskGrid.Row := taskGrid.Row + 1;
  SetInfo;
  SetChanged;
end;

procedure TformNotetask.aMoveTaskTopExecute(Sender: TObject);
var
  newRow: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 3 then exit;

  newRow := Tasks.MoveTaskTop(taskGrid.Row);
  FillGrid;
  if (newRow > -1) and (taskGrid.Row <> newRow) then
  begin
    ResetRowHeight();
    taskGrid.Row := newRow;
  end;
  SetChanged;
end;

procedure TformNotetask.aMoveTaskBottomExecute(Sender: TObject);
var
  newRow: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 3 then exit;

  newRow := Tasks.MoveTaskBottom(taskGrid.Row);
  FillGrid;
  if (newRow > -1) and (taskGrid.Row <> newRow) then
  begin
    ResetRowHeight();
    taskGrid.Row := newRow;
  end;
  SetChanged;
end;

procedure TformNotetask.aMoveTaskUpExecute(Sender: TObject);
var
  newRow: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 3 then exit;

  newRow := Tasks.MoveTaskUp(taskGrid.Row);
  FillGrid;
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
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 3 then exit;

  newRow := Tasks.MoveTaskDown(taskGrid.Row);
  FillGrid;
  if (newRow > -1) then
  begin
    SwapRowHeights(taskGrid.Row, newRow);
    taskGrid.Row := newRow;
  end;
  SetChanged;
end;

procedure TformNotetask.aDeleteTasksExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;

  DeleteTasks;
end;

procedure TformNotetask.aArchiveTasksExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;

  ArchiveTasks;
end;

procedure TformNotetask.aDateTimeExecute(Sender: TObject);
var
  PosStart: integer;
  CurrentDateTime: string;
begin
  if Screen.ActiveForm <> Self then exit;

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
    Tasks.SetTask(taskGrid, taskGrid.Row, FBackup);
    SetChanged;
  end
  else
  begin
    if (taskGrid.Col > 0) then
    begin
      if (taskGrid.RowCount > 1) then
      begin
        if taskGrid.Cells[taskGrid.Col, taskGrid.Row].Trim = string.Empty then
          taskGrid.Cells[taskGrid.Col, taskGrid.Row] := CurrentDateTime
        else
          taskGrid.Cells[taskGrid.Col, taskGrid.Row] := taskGrid.Cells[taskGrid.Col, taskGrid.Row].Trim + ' ' + CurrentDateTime;
        Tasks.SetTask(taskGrid, taskGrid.Row, FBackup);
      end
      else
      begin
        Tasks.InsertTask('- [ ] ' + CurrentDateTime + ',', taskGrid.Row);
        FillGrid;
        FLineCount += 1;
        taskGrid.Row := taskGrid.Row + 1;
      end;
      SetChanged;
    end;
  end;
end;

procedure TformNotetask.aNewExecute(Sender: TObject);
var
  new: TStringList;
begin
  if IsCanClose then
  begin
    new := TStringList.Create;
    new.Add(string.Empty);
    Tasks := TTasks.Create(new);
    SetChanged(False);
    EditComplite;
    FFileName := string.Empty;
    FEncoding := TEncoding.UTF8;
    FLineEnding := FLineEnding.WindowsCRLF;
    taskGrid.Clean;
    taskGrid.RowCount := 2;
    taskGrid.Col := 2;
    FLineCount := 1;
    SetInfo;
  end;
end;

procedure TformNotetask.aNewWindowExecute(Sender: TObject);
var
  Process: TProcess;
begin
  if Screen.ActiveForm <> Self then exit;

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
  if Screen.ActiveForm <> Self then exit;

  if (IsCanClose) and (openDialog.Execute) then
  begin
    OpenFile(openDialog.FileName);
  end;
end;

procedure TformNotetask.aPagePropertiesExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  pageSetupDialog.Execute;
end;

procedure TformNotetask.aPrintExecute(Sender: TObject);
var
  gridPrinter: TGridPrinter;
begin
  if Screen.ActiveForm <> Self then exit;

  if printDialog.Execute then
  begin
    gridPrinter := TGridPrinter.Create(self);
    try
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
    finally
      gridPrinter.Free;
    end;
  end;
end;

procedure TformNotetask.aSaveAsExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  if (saveDialog.Execute) then
  begin
    SaveFile(saveDialog.FileName);
  end;
end;

procedure TformNotetask.aSaveExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  SaveFile(FFileName);
end;

procedure TformNotetask.aShowArchivedExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  ShowArchived := aShowArchived.Checked;
end;

procedure TformNotetask.aShowStatusBarExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  ShowStatusBar := aShowStatusBar.Checked;
end;

procedure TformNotetask.aWordWrapExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  EditComplite;
  FWordWrap := aWordWrap.Checked;
  ResetRowHeight;
  Invalidate;
end;

procedure TformNotetask.aBidiRightToLeftExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  EditComplite;
  BiDiRightToLeft := aBidiRightToLeft.Checked;
  ResetRowHeight;
  Invalidate;
end;

procedure TformNotetask.aChatGptExecute(Sender: TObject);
begin
  if taskGrid.RowCount < 2 then exit;

  OpenURL(rchatgpt + EncodeUrl(Tasks.GetTaskValue(2, taskGrid.Row)));
end;

procedure TformNotetask.aGoToExecute(Sender: TObject);
var
  rowNum: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;

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
          ShowMessage(rnumstringtoolarge);
      end;
    end;
  finally
    Hide;
  end;
end;

procedure TformNotetask.aFindExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;

  formFindText.editFind.Text := FindText;
  if (formFindText.Left = 0) then
    formFindText.Left := self.Left + 80;
  if (formFindText.Top = 0) then
    formFindText.Top := self.top + 100;
  formFindText.Show;
end;

procedure TformNotetask.aReplaceExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;

  formReplaceText.editFind.Text := FindText;
  if (formReplaceText.Left = 0) then
    formReplaceText.Left := self.Left + 80;
  if (formReplaceText.Top = 0) then
    formReplaceText.Top := self.top + 100;
  formReplaceText.Show;
end;

procedure TformNotetask.aFindNextExecute(Sender: TObject);
begin
  if taskGrid.RowCount < 2 then exit;

  if (FindText <> string.Empty) then
    Find(FindText, MatchCase, WrapAround, True)
  else
    aFind.Execute;
end;

procedure TformNotetask.aFindPrevExecute(Sender: TObject);
begin
  if taskGrid.RowCount < 2 then exit;

  if (FindText <> string.Empty) then
    Find(FindText, MatchCase, WrapAround, False)
  else
    aFind.Execute;
end;

procedure TformNotetask.aAboutExecute(Sender: TObject);
begin
  formAboutNotetask := TformAboutNotetask.Create(nil);
  try
    formAboutNotetask.Left := Self.Left + 100;
    formAboutNotetask.Top := Self.Top + 100;
    formAboutNotetask.ShowModal;
  finally
    formAboutNotetask.Free;
  end;
end;

procedure TformNotetask.aLangEnglishExecute(Sender: TObject);
begin
  SetLanguage('en');
  SetCaption;
end;

procedure TformNotetask.aLangSpanishExecute(Sender: TObject);
begin
  SetLanguage('es');
  SetCaption;
end;

procedure TformNotetask.aLangFrenchExecute(Sender: TObject);
begin
  SetLanguage('fr');
  SetCaption;
end;

procedure TformNotetask.aLangGermanExecute(Sender: TObject);
begin
  SetLanguage('de');
  SetCaption;
end;

procedure TformNotetask.aLangItalianExecute(Sender: TObject);
begin
  SetLanguage('it');
  SetCaption;
end;

procedure TformNotetask.aLangPortugueseExecute(Sender: TObject);
begin
  SetLanguage('pt');
  SetCaption;
end;

procedure TformNotetask.aLangRussianExecute(Sender: TObject);
begin
  SetLanguage('ru');
  SetCaption;
end;

procedure TformNotetask.aLangJapaneseExecute(Sender: TObject);
begin
  SetLanguage('ja');
  SetCaption;
end;

procedure TformNotetask.aLangKoreanExecute(Sender: TObject);
begin
  SetLanguage('ko');
  SetCaption;
end;

procedure TformNotetask.aLangChineseExecute(Sender: TObject);
begin
  SetLanguage('zh');
  SetCaption;
end;

procedure TformNotetask.aDonateExecute(Sender: TObject);
begin
  formDonateNotetask := TformDonateNotetask.Create(nil);
  try
    formDonateNotetask.Left := Self.Left + 100;
    formDonateNotetask.Top := Self.Top + 100;
    formDonateNotetask.ShowModal;
  finally
    formDonateNotetask.Free;
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
    MyTextStyle.RightToLeft := BiDiRightToLeft;
    TGridPrinter(Sender).Canvas.TextStyle := MyTextStyle;
  end;
end;

procedure TformNotetask.PrinterGetCellText(Sender: TObject; AGrid: TCustomGrid; ACol, ARow: integer; var AText: string);
begin
  if AGrid is TStringGrid then
    AText := TStringGrid(AGrid).Cells[ACol, ARow];
end;

procedure TformNotetask.MemoEnter(Sender: TObject);
begin
  FMemoStartEdit := True;
  if (taskGrid.IsCellSelected[taskGrid.Col, taskGrid.Row]) and ((taskGrid.Selection.Height > 0) or (taskGrid.Selection.Width > 0)) then
  begin
    Memo.Color := clHighlight;
    Memo.Font.Color := clWhite;
  end
  else
  begin
    Memo.Color := clRowHighlight;
    Memo.Font.Color := clBlack;
  end;
end;

procedure TformNotetask.MemoChange(Sender: TObject);
begin
  taskGrid.Cells[taskGrid.Col, taskGrid.Row] := TMemo(Sender).Text;
  Tasks.SetTask(taskGrid, taskGrid.Row, FMemoStartEdit); // Backup only on begin edit
  FMemoStartEdit := False;
  SetChanged;
  EditControlSetBounds(Memo, taskGrid.Col, taskGrid.Row);
end;

procedure TformNotetask.DatePickerChange(Sender: TObject);
begin
  taskGrid.Cells[taskGrid.Col, taskGrid.Row] := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat,
    TDateTimePicker(Sender).DateTime);
  Tasks.SetTask(taskGrid, taskGrid.Row, FBackup);
  SetChanged;
  EditControlSetBounds(DatePicker, taskGrid.Col, taskGrid.Row, 0, 0, 0, 0);
end;

procedure TformNotetask.EditControlSetBounds(Sender: TWinControl; aCol, aRow: integer; OffsetLeft: integer = 5; OffsetTop: integer = 1;
  OffsetRight: integer = -10; OffsetBottom: integer = -2);
var
  Rect: TRect;
begin
  Rect := taskGrid.CellRect(aCol, aRow);
  Sender.SetBounds(Rect.Left + OffsetLeft, Rect.Top + OffsetTop, Rect.Right - Rect.Left + OffsetRight, Rect.Bottom - Rect.Top + OffsetBottom);
end;

procedure TformNotetask.ClearSelected(ShowConfirm: boolean = True);
var
  Confirm: integer;
begin
  Confirm := mrYes;
  // Show confirm delete dialog
  if (ShowConfirm) then
    Confirm := MessageDlg(rclearconfirm, mtConfirmation, [mbYes, mbNo], 0);

  if (Confirm = mrYes) or (not ShowConfirm) then
  begin
    Tasks.ClearTasksInRect(taskGrid, taskGrid.Selection);
    if (Assigned(Memo)) then
    begin
      Memo.OnChange := nil;
      Memo.Clear;
      Memo.OnChange := @MemoChange;
    end;
    SetChanged;
    FLineCount := Tasks.Count;
    SetInfo;
  end;
end;

procedure TformNotetask.DeleteTask(aRow: integer = 0; ShowConfirm: boolean = True);
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
    Confirm := mrYes;

    // Show confirm delete dialog
    if (ShowConfirm) then
      Confirm := MessageDlg(rdeleteconfirm, mtConfirmation, [mbYes, mbNo], 0);

    if (Confirm = mrYes) or (not ShowConfirm) then
    begin
      if (FBackup) then Tasks.CreateBackup;

      // RemoveTask from collection
      taskGrid.DeleteRow(RowIndex);
      SetChanged;
    end;
  end;
end;

procedure TformNotetask.DeleteTasks(ShowConfirm: boolean = True);
var
  i, RowIndex, Confirm: integer;
begin
  // If multiple rows are selected
  if (taskGrid.Selection.Width > 0) or (taskGrid.Selection.Height > 0) then
  begin
    Confirm := mrYes;

    // Request confirmation for deletion
    if (ShowConfirm) then
      Confirm := MessageDlg(rdeletesconfirm, mtConfirmation, [mbYes, mbNo], 0);

    if (Confirm = mrYes) or (not ShowConfirm) then
    begin
      if (FBackup) then Tasks.CreateBackup;

      // Delete rows from the end to avoid index shifting
      for i := taskGrid.Selection.Bottom downto taskGrid.Selection.Top do
      begin
        RowIndex := i;
        if (RowIndex > 0) and (RowIndex <= Tasks.Count) then
        begin
          // Remove the task from the collection
          taskGrid.DeleteRow(RowIndex);
        end;
      end;
      SetChanged; // Mark that data has changed
    end;
  end
  else
    DeleteTask(0, ShowConfirm);
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
      if (FBackup) then Tasks.CreateBackup;

      // Archivate task
      Tasks.ArchiveTask(RowIndex);
      FillGrid;
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
      if (FBackup) then Tasks.CreateBackup;

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
      FillGrid;
      SetChanged; // Mark that data has changed
    end;
  end
  else
    ArchiveTask;
  Invalidate;
end;

procedure TformNotetask.CompleteTasks(aRow: integer = 0);
var
  RowIndex: integer;
  i: integer;
begin
  // If multiple rows are selected
  if (taskGrid.Selection.Width > 0) or (taskGrid.Selection.Height > 0) then
  begin
    if FBackup then Tasks.CreateBackup;
    // Mark tasks as completed from the end to avoid index shifting
    for i := taskGrid.Selection.Bottom downto taskGrid.Selection.Top do
    begin
      RowIndex := i;
      if (RowIndex > 0) and (RowIndex <= Tasks.Count) then
      begin
        // Mark the task as completed in the collection
        Tasks.CompleteTask(RowIndex);

        if Tasks.GetTask(RowIndex).Done then
        begin
          taskGrid.Cells[1, RowIndex] := '1';
          if (taskGrid.Cells[4, RowIndex] = '') then
            taskGrid.Cells[4, RowIndex] := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat, Now);
        end
        else
          taskGrid.Cells[1, RowIndex] := '0';

        Tasks.SetTask(taskGrid, RowIndex, False); // Backup created on start
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
      if Tasks.GetTask(RowIndex).Done then
      begin
        taskGrid.Cells[1, RowIndex] := '1';
        if (taskGrid.Cells[4, RowIndex] = '') then
          taskGrid.Cells[4, RowIndex] := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat, Now);
      end
      else
        taskGrid.Cells[1, RowIndex] := '0';

      Tasks.SetTask(taskGrid, RowIndex, FBackup);
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

procedure TformNotetask.SetShowStatusBar(Value: boolean);
begin
  FShowStatusBar := Value;
  StatusBar.Visible := FShowStatusBar;
end;

procedure TformNotetask.SetShowArchived(Value: boolean);
begin
  FShowArchived := Value;
  FillGrid;
end;

procedure TformNotetask.SetBiDiRightToLeft(Value: boolean);
var
  i: integer;
begin
  FBiDiRightToLeft := Value;

  if (Value) then
  begin
    taskGrid.BiDiMode := bdRightToLeft;
    for i := 1 to taskGrid.Columns.Count - 1 do
      taskGrid.Columns[i].Alignment := taRightJustify;
  end
  else
  begin
    taskGrid.BiDiMode := bdLeftToRight;
    for i := 1 to taskGrid.Columns.Count - 1 do
      taskGrid.Columns[i].Alignment := taLeftJustify;
  end;
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

function TformNotetask.OpenFile(fileName: string): boolean;
var
  Content: string;
begin
  Result := False;
  if not FileExists(fileName) then
  begin
    ShowMessage(rfilenotfound);
    exit;
  end;

  FFileName := fileName;
  EditComplite;
  ReadTextFile(FFileName, Content, FEncoding, FLineEnding, FLineCount);
  SetInfo;

  Tasks := TTasks.Create(TextToStringList(Content));
  FillGrid;

  taskGrid.Row := 1;
  taskGrid.Col := 2;
  ResetRowHeight;
  SetChanged(False);
  Result := True;
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

procedure TformNotetask.FillGrid;
begin
  Tasks.FillGrid(taskGrid, FShowArchived, SortOrder, SortColumn);
end;

procedure TformNotetask.SetInfo;
begin
  statusBar.Panels[1].Text := UpperCase(GetEncodingName(FEncoding));
  statusBar.Panels[2].Text := FLineEnding.ToString;
  statusBar.Panels[3].Text := FLineCount.ToString + rrows;
  SetCaption;
end;

procedure TformNotetask.SetLanguage(aLanguage: string = string.Empty);
begin
  aLangEnglish.Checked := False;
  aLangSpanish.Checked := False;
  aLangFrench.Checked := False;
  aLangGerman.Checked := False;
  aLangItalian.Checked := False;
  aLangPortuguese.Checked := False;
  aLangRussian.Checked := False;
  aLangJapanese.Checked := False;
  aLangKorean.Checked := False;
  aLangChinese.Checked := False;

  if (aLanguage <> string.Empty) then
  begin
    Language := aLanguage;
    ApplicationTranslate(Language);
  end;

  case Language of
    'en': aLangEnglish.Checked := True;
    'es': aLangSpanish.Checked := True;
    'fr': aLangFrench.Checked := True;
    'de': aLangGerman.Checked := True;
    'it': aLangItalian.Checked := True;
    'pt': aLangPortuguese.Checked := True;
    'ru': aLangRussian.Checked := True;
    'ja': aLangJapanese.Checked := True;
    'ko': aLangKorean.Checked := True;
    'zh': aLangChinese.Checked := True;
  end;
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

procedure TformNotetask.EditCell(aCol, aRow: integer);
begin
  taskGrid.Row := aRow;
  taskGrid.Col := aCol;
  FIsEditing := True;
  taskGrid.EditorMode := True; //Set editing mode

  if (Assigned(Memo)) and (Memo.Visible) then
  begin
    Memo.Text := Tasks.GetTaskValue(aCol, aRow);
    Memo.SelectAll;
    Memo.SetFocus;
    FMemoStartEdit := True;
  end;
end;

procedure TformNotetask.EditComplite;
begin
  if taskGrid.EditorMode then
  begin
    taskGrid.EditorMode := False;
    FIsEditing := False;
  end;
end;

function TformNotetask.Find(aText: string; aMatchCase, aWrapAround, aDirectionDown: boolean; Silent: boolean = False): boolean;
var
  sValue, sText: unicodestring;
  Counter, CurRow, CurCol: integer;

  function FindMemo: boolean;
  var
    SelEnd, FindPos: integer;
  begin
    // Start searching from the current cursor position
    if (aDirectionDown) then
    begin
      SelEnd := Memo.SelStart + Memo.SelLength + 1;

      // Find the position of the search text, starting from SelEnd
      if (MatchCase) then
        FindPos := PosEx(sText, sValue, SelEnd)
      else
        FindPos := PosEx(UnicodeLowerCase(sText), UnicodeLowerCase(sValue), SelEnd);
    end
    else
    begin
      SelEnd := Memo.SelStart;

      // Find the position of the search text, starting from SelEnd
      if (MatchCase) then
        FindPos := PosExReverse(sText, sValue, SelEnd)
      else
        FindPos := PosExReverse(UnicodeLowerCase(sText), UnicodeLowerCase(sValue), SelEnd);
    end;

    // If the text is found
    if FindPos > 0 then
    begin
      // Select the found text
      Memo.SelStart := FindPos - 1;
      Memo.SelLength := Length(sText);
      Result := True;  // Return True, text is found
    end
    else
    begin
      // Text is not found
      Result := False; // Return False, text is not found
    end;
  end;

  procedure NotFound;
  begin
    taskGrid.Row := FLastFoundRow;
    taskGrid.Col := FLastFoundCol;
    taskGrid.EditorMode := True;
    Memo.SelStart := FLastFoundSelStart;
    Memo.SelLength := FLastFoundSelLength;
    if (not Silent) then
      ShowMessage(rcantfind + ' "' + string(sText) + '"');
  end;

begin
  if (FFindActive) or (taskGrid.RowCount = 0) then exit;
  FFindActive := True;
  try
    FindText := aText;
    MatchCase := aMatchCase;
    WrapAround := aWrapAround;

    if taskGrid.Col = 1 then taskGrid.Col := 2;
    taskGrid.EditorMode := True;
    if (Memo.SelStart > Length(unicodestring(Memo.Text)) - 1) then
    begin
      if (aDirectionDown) then
        Memo.SelStart := 0;
    end
    else
    begin
      if (aDirectionDown) then
        Memo.SelStart := Memo.SelStart + Memo.SelLength;
    end;
    Memo.SelLength := 0;

    CurRow := taskGrid.Row;
    CurCol := taskGrid.Col;
    Counter := 0;

    repeat
      if (Assigned(Memo)) then
      begin
        sValue := unicodestring(Memo.Text);
        sText := unicodestring(aText);
        if (Pos(UnicodeLowerCase(sText), UnicodeLowercase(sValue)) > 0) and (FindMemo) then
        begin
          taskGrid.EditorMode := True;
          FLastFoundRow := taskGrid.Row;
          FLastFoundCol := taskGrid.Col;
          FLastFoundSelStart := Memo.SelStart;
          FLastFoundSelLength := Memo.SelLength;
          FFoundText := aText;
          Counter := 0;
          Break;
        end;
      end;

      // Move to col
      if ((aDirectionDown) and (CurCol < 3)) or ((not aDirectionDown) and (CurCol > 2)) then
      begin
        // Move to next col
        if (aDirectionDown) then
        begin
          Inc(CurCol);
          taskGrid.Col := taskGrid.Col + 1;
          Memo.SelStart := 0;
          Memo.SelLength := 0;
        end
        else
          // Move to prev col
        begin
          Dec(CurCol);
          taskGrid.Col := taskGrid.Col - 1;
          Memo.SelStart := Length(unicodestring(Memo.Text));
          Memo.SelLength := 0;
        end;
      end
      else
      begin
        // Move to row
        if ((aDirectionDown) and (CurRow < taskGrid.RowCount)) or ((not aDirectionDown) and (CurRow > 0)) then
        begin
          // Move to next row
          if (aDirectionDown) then
          begin
            Inc(CurRow);
            CurCol := 1;
            taskGrid.Row := taskGrid.Row + 1;
            taskGrid.Col := 2;
            Memo.SelStart := 0;
            Memo.SelLength := 0;
          end
          else
            // Move to prev row
          begin
            Dec(CurRow);
            CurCol := 4;
            taskGrid.Row := taskGrid.Row - 1;
            taskGrid.Col := 3;
            Memo.SelStart := Length(unicodestring(Memo.Text)) - 1;
            Memo.SelLength := 0;
          end;
          Inc(Counter);
        end;
      end;
      // Move to begin
      if ((aDirectionDown) and (CurRow >= taskGrid.RowCount)) or ((not aDirectionDown) and (CurRow = 0)) then
      begin
        if (WrapAround) then
        begin
          // Move to begin start
          if (aDirectionDown) then
          begin
            CurRow := 1;
            taskGrid.Row := 1;
            CurCol := 2;
            taskGrid.Col := 2;
            Memo.SelStart := 0;
          end
          else
            // Move to begin end
          begin
            CurRow := taskGrid.RowCount - 1;
            taskGrid.Row := taskGrid.RowCount - 1;
            CurCol := 3;
            taskGrid.Col := 3;
          end;
          Inc(Counter);
        end
        else
        begin
          NotFound;
          Result := False;
          exit;
        end;
      end;

    until ((not WrapAround) and (((aDirectionDown) and (CurRow >= taskGrid.RowCount)) or ((not aDirectionDown) and (CurRow = 0)))) or
      (WrapAround and (Counter > taskGrid.RowCount));

    if (WrapAround and (Counter > taskGrid.RowCount)) then
    begin
      NotFound;
      Result := False;
      exit;
    end;

    Result := True;
  finally
    FFindActive := False;
  end;
end;

function TformNotetask.Replace(aText, aToText: string; aMatchCase, aWrapAround: boolean): boolean;

  procedure FindNextExecute;
  begin
    FFindText := aText;
    FMatchCase := aMatchCase;
    FWrapAround := aWrapAround;
    aFindNext.Execute;
  end;

begin
  if (FFoundText = string.Empty) or (Memo.SelText <> aText) then
    FindNextExecute
  else
  begin
    Memo.SelText := aToText;
    FindNextExecute;
  end;

  Result := True;
end;

function TformNotetask.ReplaceAll(aText, aToText: string; aMatchCase, aWrapAround: boolean): boolean;
var
  Row, Col: integer;
begin
  FBackup := False;
  Row := taskGrid.Row;
  Col := taskGrid.Col;
  Tasks.CreateBackup; // FBackup = false here
  try
    while (Find(aText, aMatchCase, aWrapAround, True, True)) do
    begin
      Memo.SelText := aToText;
    end;

    taskGrid.Row := Row;
    taskGrid.Col := Col;
    Memo.SelLength := 0;
    Result := True;
  finally
    FBackup := True;
  end;
end;

end.
