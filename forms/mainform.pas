//-----------------------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the GNU General Public License, Version 3 (GPL-3.0)
//  You may obtain a copy of the License at https://www.gnu.org/licenses/gpl-3.0.html
//-----------------------------------------------------------------------------------

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
    aShowDuration: TAction;
    menuShowDuration: TMenuItem;
    aLangArabic: TAction;
    aLangUkrainian: TAction;
    aLangBelarusian: TAction;
    aLangHindi: TAction;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    aShowColumnDone: TAction;
    aShowColumnComment: TAction;
    aShowColumnDate: TAction;
    Separator13: TMenuItem;
    menuColumnDone: TMenuItem;
    menuColumnComment: TMenuItem;
    menuColumnDate: TMenuItem;
    aShowColumnAmount: TAction;
    aShowColumnFavorite: TAction;
    menuColumnAmount: TMenuItem;
    menuColumnFavorite: TMenuItem;
    aIndentTasks: TAction;
    aOutdentTasks: TAction;
    menuIndentTasks: TMenuItem;
    Separator14: TMenuItem;
    menuOutdentTasks: TMenuItem;
    aShowColumnTask: TAction;
    menuColumnTask: TMenuItem;
    TitleImages: TImageList;
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
    procedure taskGridUserCheckboxBitmap(Sender: TObject; const aCol, aRow: integer; const CheckedState: TCheckboxState;
      var ABitmap: TBitmap);
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
    procedure aShowDurationExecute(Sender: TObject);
    procedure aLangUkrainianExecute(Sender: TObject);
    procedure aLangBelarusianExecute(Sender: TObject);
    procedure aLangHindiExecute(Sender: TObject);
    procedure aLangArabicExecute(Sender: TObject);
    procedure taskGridSetCheckboxState(Sender: TObject; ACol, ARow: integer; const Value: TCheckboxState);
    procedure aShowColumnDoneExecute(Sender: TObject);
    procedure aShowColumnTaskExecute(Sender: TObject);
    procedure aShowColumnCommentExecute(Sender: TObject);
    procedure aShowColumnDateExecute(Sender: TObject);
    procedure aShowColumnAmountExecute(Sender: TObject);
    procedure aShowColumnFavoriteExecute(Sender: TObject);
    procedure aIndentTasksExecute(Sender: TObject);
    procedure aOutdentTasksExecute(Sender: TObject);
    procedure taskGridSelection(Sender: TObject; aCol, aRow: integer);
  private
    Memo: TMemo;
    DatePicker: TDateTimePicker;
    FChanged: boolean;
    FBackup: boolean;
    FMemoStartEdit: boolean;
    FIsEditing: boolean;
    FIsSelecting: boolean;
    FDisableCheckToggle: boolean;
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
    FLastGridSelection: TRect;
    FLastGridRow, FLastGridCol: integer;
    FLastSelectionHeight: integer;
    FLastFoundRow, FLastFoundCol, FLastFoundSelStart, FLastFoundSelLength: integer;
    procedure MemoChange(Sender: TObject);
    procedure MemoEnter(Sender: TObject);
    procedure MemoKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure MemoKeyPress(Sender: TObject; var Key: char);
    procedure DatePickerChange(Sender: TObject);
    procedure EditControlSetBounds(Sender: TWinControl; aCol, aRow: integer; OffsetLeft: integer = 4;
      OffsetTop: integer = 0; OffsetRight: integer = -8; OffsetBottom: integer = 0);
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
    procedure CompleteTasks(aRow: integer = 0);
    procedure StarTask(aRow: integer = 0);
    procedure IndentTasks(Outdent: boolean = False);
    procedure SetBiDiRightToLeft(Value: boolean);
    procedure SetShowStatusBar(Value: boolean);
    procedure SetShowDuration(Value: boolean);
    procedure SetShowArchived(Value: boolean);
    procedure SetShowColumnDone(Value: boolean);
    procedure SetShowColumnTask(Value: boolean);
    procedure SetShowColumnComment(Value: boolean);
    procedure SetShowColumnDate(Value: boolean);
    procedure SetShowColumnAmount(Value: boolean);
    procedure SetShowColumnFavorite(Value: boolean);
    procedure ApplyColumnSetting;
    procedure ApplySortArrow;
    procedure ApplySorting;
    procedure GridBackupSelection;
    procedure GridClearSelection;
    procedure ResetRowHeight(aRow: integer = 0; aHeight: integer = 0);
    procedure SwapRowHeights(RowIndex1, RowIndex2: integer);
    function GetIsEditing: boolean;
    function IsCanClose: boolean;
  public
    FShowArchived: boolean;
    FShowDuration: boolean;
    FShowColumnDone: boolean;
    FShowColumnTask: boolean;
    FShowColumnComment: boolean;
    FShowColumnAmount: boolean;
    FShowColumnDate: boolean;
    FShowColumnFavorite: boolean;

    procedure SetLanguage(aLanguage: string = string.Empty);
    procedure FillGrid;
    function SaveFileAs: boolean;
    function SaveFile(fileName: string = string.Empty): boolean;
    function OpenFile(fileName: string): boolean;
    function Find(aText: string; aMatchCase, aWrapAround, aDirectionDown: boolean; Silent: boolean = False): boolean;
    function Replace(aText, aToText: string; aMatchCase, aWrapAround: boolean): boolean;
    function ReplaceAll(aText, aToText: string; aMatchCase, aWrapAround: boolean): boolean;

    property WordWrap: boolean read FWordWrap write FWordWrap;
    property BiDiRightToLeft: boolean read FBiDiRightToLeft write SetBiDiRightToLeft;
    property ShowStatusBar: boolean read FShowStatusBar write SetShowStatusBar;
    property ShowDuration: boolean read FShowDuration write SetShowDuration;
    property ShowColumnDone: boolean read FShowColumnDone write SetShowColumnDone;
    property ShowColumnTask: boolean read FShowColumnTask write SetShowColumnTask;
    property ShowColumnComment: boolean read FShowColumnComment write SetShowColumnComment;
    property ShowColumnDate: boolean read FShowColumnDate write SetShowColumnDate;
    property ShowColumnAmount: boolean read FShowColumnAmount write SetShowColumnAmount;
    property ShowColumnFavorite: boolean read FShowColumnFavorite write SetShowColumnFavorite;
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
  clRowFocused: TColor;
  clRowExpired: TColor;
  ResourceBitmapCheck: TBitmap;
  ResourceBitmapUncheck: TBitmap;
  ResourceBitmapStarGold: TBitmap;
  ResourceBitmapStarGray: TBitmap;

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
  FShowStatusBar := True;
  FShowColumnDone := True;
  FShowColumnTask := True;
  FShowColumnComment := True;
  FShowColumnDate := True;
  FShowColumnAmount := False;
  FShowColumnFavorite := True;
  FBiDiRightToLeft := self.BiDiMode = bdRightToLeft;
  FSortColumn := 0;
  FSortOrder := soAscending;
  clRowHighlight := RGBToColor(220, 240, 255);
  clRowFocused := RGBToColor(200, 220, 255);
  clRowExpired := RGBToColor(255, 220, 220);
  openDialog.Filter := ropendialogfilter;
  saveDialog.Filter := rsavedialogfilter;

  {$IFDEF Windows}
  taskGrid.DefaultRowHeight := 22;
  {$ENDIF}
  {$IFDEF Linux}
  taskGrid.DefaultRowHeight := 33;
  {$ENDIF}

  // Create TBitmap objects
  ResourceBitmapCheck := TBitmap.Create;
  ResourceBitmapUncheck := TBitmap.Create;
  ResourceBitmapStarGold := TBitmap.Create;
  ResourceBitmapStarGray := TBitmap.Create;

  // Load bitmaps from resources
  ResourceBitmapCheck.LoadFromResourceName(HInstance, 'CHECK');
  ResourceBitmapUncheck.LoadFromResourceName(HInstance, 'UNCHECK');
  ResourceBitmapStarGold.LoadFromResourceName(HInstance, 'STARGOLD');
  ResourceBitmapStarGold.Transparent := True;
  ResourceBitmapStarGold.TransparentColor := clFuchsia;
  ResourceBitmapStarGray.LoadFromResourceName(HInstance, 'STARGRAY');
  ResourceBitmapStarGray.Transparent := True;
  ResourceBitmapStarGray.TransparentColor := clFuchsia;

  LoadFormSettings(Self);
  LoadGridSettings(Self, taskGrid, string.Empty);

  // Set language
  SetLanguage;

  // After load wordwrap setting
  aWordWrap.Checked := FWordWrap;
  aBidiRightToLeft.Checked := FBiDiRightToLeft;
  aShowStatusBar.Checked := FShowStatusBar;
  aShowArchived.Checked := FShowArchived;

  // Apply loaded settings to columns
  ApplyColumnSetting;

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
  SaveFormSettings(Self);
  SaveGridSettings(Self, taskGrid, FFileName);

  // Free allocated resources
  FLineEnding.Free;
  Tasks.Free;
  ResourceBitmapCheck.Free;
  ResourceBitmapUncheck.Free;
  ResourceBitmapStarGold.Free;
  ResourceBitmapStarGray.Free;
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
  if (Shift = [ssCtrl]) and (Key = VK_TAB) then // Ctrl + Tab
  begin
    aIndentTasks.Execute;
    Key := 0;
  end
  else
  if (Shift = [ssCtrl, ssShift]) and (Key = VK_TAB) then // Ctrl + Shift + Tab
  begin
    aOutdentTasks.Execute;
    Key := 0;
  end
  else
  if ((Shift = [ssCtrl]) or (Shift = [ssAlt])) and (Key = VK_PRIOR) then // Ctrl || Alt + Page Up
  begin
    aMoveTaskTop.Execute;
    Key := 0;
  end
  else
  if ((Shift = [ssCtrl]) or (Shift = [ssAlt])) and (Key = VK_NEXT) then // Ctrl || Alt + Page Down
  begin
    aMoveTaskBottom.Execute;
    Key := 0;
  end
  else
  if ((Shift = [ssCtrl]) or (Shift = [ssAlt])) and (Key = VK_UP) then // Ctrl || Alt + Up
  begin
    aMoveTaskUp.Execute;
    Key := 0;
  end
  else
  if ((Shift = [ssCtrl]) or (Shift = [ssAlt])) and (Key = VK_DOWN) then // Ctrl || Alt + Down
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
      if (taskGrid.Col = 6) and (taskGrid.Selection.Height = 0) and (taskGrid.Selection.Width = 0) then
        StarTask
      else
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
  if ((Shift = [ssCtrl]) or (not FWordWrap) or (taskGrid.Col = 4)) and (Key = VK_RETURN) then // Ctrl + Enter
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
    taskGrid.SortOrder := SortOrder;

    FSortColumn := Index;

    ApplySorting;
  end
  else
    // Set row when clicked on begining of row
  begin
    if (ssShift in GetKeyShiftState) and (taskGrid.Selection.Height = 0) and (taskGrid.Selection.Top <> index) then
    begin
      taskGrid.Selection := TGridRect.Create(1, taskGrid.Selection.Top, 6, index);
    end
    else
    begin
      taskGrid.Row := index;
      taskGrid.Selection := TGridRect.Create(1, index, 6, index);
    end;
  end;
end;

procedure TformNotetask.taskGridSetCheckboxState(Sender: TObject; ACol, ARow: integer; const Value: TCheckboxState);
var
  MousePosScreen, MousePosClient, CheckBoxCenter: TPoint;
  CheckBoxRect: TRect;
  CheckBoxSize: integer;
begin
  if (aCol = 1) then
  begin
    // Define checkbox area size (16x16)
    CheckBoxSize := 14;

    // Get mouse position in screen coordinates
    MousePosScreen := Mouse.CursorPos;

    // Convert screen coordinates to client coordinates (relative to the form)
    MousePosClient := taskGrid.ScreenToClient(MousePosScreen);

    // Get the center of the checkbox (approximately the center of the cell)
    CheckBoxCenter := taskGrid.CellRect(ACol, ARow).CenterPoint;

    // Define the 16x16 rectangle around the checkbox
    CheckBoxRect.Left := CheckBoxCenter.X - CheckBoxSize div 2;
    CheckBoxRect.Top := CheckBoxCenter.Y - CheckBoxSize div 2;
    CheckBoxRect.Right := CheckBoxCenter.X + CheckBoxSize div 2;
    CheckBoxRect.Bottom := CheckBoxCenter.Y + CheckBoxSize div 2;

    // Check if the mouse is within the 16x16 checkbox area
    if not PtInRect(CheckBoxRect, MousePosClient) then
    begin
      // If the mouse is outside the checkbox, prevent the state from being changed
      FDisableCheckToggle := True;
      exit;
    end;
    FDisableCheckToggle := False;
  end;
  taskGrid.Cells[ACol, ARow] := IfThen(Value = cbChecked, '1', '0');
end;

procedure TformNotetask.taskGridCheckboxToggled(Sender: TObject; aCol, aRow: integer; aState: TCheckboxState);
begin
  if (aCol = 1) then
  begin
    if (FDisableCheckToggle) then exit;

    CompleteTasks(aRow);
  end
  else
  if (aCol = 6) then
  begin
    StarTask(aRow);
  end;
end;

procedure TformNotetask.taskGridColRowInserted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
begin
  if (not IsColumn) then
  begin
    if FBackup then
    begin
      GridBackupSelection;
      Tasks.CreateBackup;
    end;
    Tasks.AddMap(Tasks.AddTask('[ ]'));
    taskGrid.Cells[1, tIndex] := '0';
    SetInfo;
    SetChanged();
  end;
end;

procedure TformNotetask.taskGridColRowDeleted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
begin
  if (not IsColumn) then
  begin
    Tasks.DeleteTask(tIndex);
    SetInfo;

    if ShowDuration then FillGrid;
  end;
end;

procedure TformNotetask.taskGridHeaderSized(Sender: TObject; IsColumn: boolean; Index: integer);
begin
  taskGridResize(Sender);
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
  task: TTask;
  amount: double;
begin
  grid := Sender as TStringGrid;

  // Border for fixed cells
  if (aRow < grid.FixedRows) or (aCol < grid.FixedCols) then
  begin
    grid.Canvas.Pen.Color := clSilver;
    grid.Canvas.Pen.Style := psSolid;
    grid.Canvas.Pen.Width := 1;
    grid.Canvas.Pen.Width := 0;
    grid.Canvas.Brush.Style := bsClear;
    grid.Canvas.Rectangle(aRect.Left - 1, aRect.Top - 1, aRect.Right, aRect.Bottom);
  end
  else
  begin
    // Determine background color
    if (gdFocused in aState) and (taskGrid.Selection.Height = 0) and (taskGrid.Selection.Width = 0) then
    begin
      bgFill := clRowFocused;    // Focused
      grid.Canvas.Font.Color := clBlack;
    end
    else
    if (gdSelected in aState) and ((taskGrid.Selection.Height > 0) or (taskGrid.Selection.Width > 0)) then
    begin
      bgFill := clHighlight;    // Multiselect
      grid.Canvas.Font.Color := clWhite;
    end
    else
    if gdRowHighlight in aState then
    begin
      bgFill := clRowHighlight; // Highlight
      grid.Canvas.Font.Color := clBlack;
    end
    else
    begin
      task := Tasks.GetTask(ARow);
      if (ShowColumnDate) and (not task.Done) and (task.Date > 0) and (task.Date < Now) then // Color expired task
      begin
        bgFill := clRowExpired; // Expired warning red
        grid.Canvas.Font.Color := clBlack;
      end
      else
      if (not task.Done) and (task.Archive) then
      begin
        bgFill := clWhite; // Not done but arhive warning color
        grid.Canvas.Font.Color := clMaroon;
      end
      else
      begin
        bgFill := clWhite; // All other white
        grid.Canvas.Font.Color := clBlack;
      end;
    end;

    if (Tasks.HasTask(ARow)) and (Tasks.GetTask(ARow).Star) then
      grid.Canvas.Font.Style := grid.Canvas.Font.Style + [fsBold];

    if (aCol = 2) and (Tasks.HasTask(ARow)) and (Tasks.GetTask(ARow).Archive) then
      grid.Canvas.Font.Style := grid.Canvas.Font.Style + [fsStrikeOut];

    if (aCol = 3) and (Tasks.HasTask(ARow) and Tasks.GetTask(ARow).CommentItalic) then
      grid.Canvas.Font.Style := grid.Canvas.Font.Style + [fsItalic];

    // Fill the cell background
    grid.Canvas.Brush.Color := bgFill;
    grid.canvas.Brush.Style := bsSolid;
    grid.canvas.FillRect(aRect);

    if (aCol in [1, 6]) then
    begin
      grid.DefaultDrawCell(aCol, aRow, aRect, aState);
      exit;
    end;

    if (aCol = 4) and (TryStrToFloat(grid.Cells[ACol, ARow], amount)) then
      S := FormatFloat('#,##0.##########', StrToFloat(grid.Cells[ACol, ARow]))
    else
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
      begin
        // Changing the row height fires the event again!
        grid.RowHeights[ARow] := (drawrect.bottom - drawrect.top + 2);
        {$IFDEF Linux}
        grid.Invalidate;
        {$ENDIF}
      end
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
  if (aCol in [2, 3, 4]) then
  begin
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
    Memo.BorderStyle := bsNone;
    Memo.ScrollBars := ssNone;
    Memo.TabStop := False;
    Memo.WantTabs := True;
    Memo.WordWrap := (FWordWrap) and (aCol <> 4);
    Memo.WantReturns := (FWordWrap) and (aCol <> 4);
    if (FBiDiRightToLeft) then
    begin
      Memo.BiDiMode := bdRightToLeft;
      Memo.Alignment := taRightJustify;
    end
    else
    begin
      Memo.BiDiMode := bdLeftToRight;
      Memo.Alignment := taLeftJustify;
    end;

    EditControlSetBounds(Memo, aCol, aRow);
    Memo.Parent := taskGrid;
    Memo.OnChange := @MemoChange; // Event Change
    Memo.OnEnter := @MemoEnter; // Event Enter
    Memo.OnKeyDown := @MemoKeyDown; // Event KeyDown
    if (aCol = 4) then
      Memo.OnKeyPress := @MemoKeyPress; // Event KeyPress for amount column only
    Memo.Text := taskGrid.Cells[aCol, aRow];
    Memo.SelStart := Length(Memo.Text);
    Memo.SelLength := 0;

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
  if (aCol = 5) then
  begin
    DatePicker := TDateTimePicker.Create(Self);
    DatePicker.Visible := False;
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

procedure TformNotetask.taskGridUserCheckboxBitmap(Sender: TObject; const aCol, aRow: integer;
  const CheckedState: TCheckboxState; var ABitmap: TBitmap);
begin
  // Check if we're in the correct column
  if aCol = 1 then
  begin
    // Assign the appropriate bitmap based on the CheckedState
    if CheckedState = cbChecked then
      ABitmap := ResourceBitmapCheck // Use check bitmap
    else
      ABitmap := ResourceBitmapUncheck; // Use uncheck bitmap
  end
  else
  if aCol = 6 then
  begin
    // Assign the appropriate bitmap based on the CheckedState
    if CheckedState = cbChecked then
      ABitmap := ResourceBitmapStarGold // Use check bitmap
    else
      ABitmap := ResourceBitmapStarGray; // Use uncheck bitmap
  end;
end;

procedure TformNotetask.taskGridColRowMoved(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
begin
  if (not IsColumn) then
  begin
    Tasks.MoveTask(sIndex, tIndex);
    SetChanged;

    if ShowDuration then FillGrid;
  end;
end;

procedure TformNotetask.aUndoExecute(Sender: TObject);
var
  TempRect: TRect;
  TempLastRow, TempLastCol: integer;
begin
  if Screen.ActiveForm <> Self then exit;

  if not IsEditing then
  begin
    TempRect := FLastGridSelection;
    TempLastRow := FLastGridRow;
    TempLastCol := FLastGridCol;
    GridBackupSelection;

    Tasks.UndoBackup;
    FillGrid;

    taskGrid.Col := TempLastCol;
    if (TempLastRow > 1) then
      taskGrid.Row := TempLastRow;
    if (TempRect.Width > 0) and (TempRect.Height > 0) then
      taskGrid.Selection := TRect.Create(TempRect.Left, TempRect.Top, TempRect.Right, TempRect.Bottom);
    ResetRowHeight;
    SetInfo;
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
      SetInfo;
      GridClearSelection;
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
    SetInfo;
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
    ClearSelected(False);
    if ShowDuration then FillGrid;
    SetInfo;
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
    taskGrid.Selection := TGridRect.Create(0, 0, 6, taskGrid.RowCount)
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
  taskGrid.Row := taskGrid.Row + 1;
  ResetRowHeight;
  SetInfo;
  SetChanged;
end;

procedure TformNotetask.aMoveTaskTopExecute(Sender: TObject);
var
  newRow, len: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 3 then exit;

  GridBackupSelection;
  Len := taskGrid.Selection.Bottom - taskGrid.Selection.Top + 1;
  if (SortOrder = soAscending) then
    newRow := Tasks.MoveTasksTop(taskGrid.Selection.Top, taskGrid.Selection.Bottom)
  else
    newRow := Tasks.MoveTasksBottom(taskGrid.Selection.Bottom, taskGrid.Selection.Top);

  FillGrid;
  if (newRow > -1) then
  begin
    ResetRowHeight();
    taskGrid.Row := 0;
    taskGrid.Selection := TGridRect.Create(taskGrid.Selection.Left, 0, taskGrid.Selection.Right, Len);
  end;
  SetChanged;
end;

procedure TformNotetask.aMoveTaskBottomExecute(Sender: TObject);
var
  newRow, Len: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 3 then exit;

  GridBackupSelection;
  Len := taskGrid.Selection.Bottom - taskGrid.Selection.Top + 1;
  if (SortOrder = soAscending) then
    newRow := Tasks.MoveTasksBottom(taskGrid.Selection.Top, taskGrid.Selection.Bottom)
  else
    newRow := Tasks.MoveTasksTop(taskGrid.Selection.Bottom, taskGrid.Selection.Top);

  FillGrid;
  if (newRow > -1) then
  begin
    ResetRowHeight();
    taskGrid.Row := taskGrid.RowCount - Len;
    taskGrid.Selection := TGridRect.Create(taskGrid.Selection.Left, taskGrid.RowCount - Len, taskGrid.Selection.Right,
      taskGrid.RowCount);
  end;
  SetChanged;
end;

procedure TformNotetask.aMoveTaskUpExecute(Sender: TObject);
var
  newRow, Len: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 3 then exit;

  GridBackupSelection;
  Len := taskGrid.Selection.Bottom - taskGrid.Selection.Top + 1;
  if (SortOrder = soAscending) then
    newRow := Tasks.MoveTasksUp(taskGrid.Selection.Top, taskGrid.Selection.Bottom)
  else
    newRow := Tasks.MoveTasksDown(taskGrid.Selection.Bottom, taskGrid.Selection.Top);

  FillGrid;
  if (newRow > -1) then
  begin
    ResetRowHeight();
    taskGrid.Row := newRow;
    taskGrid.Selection := TGridRect.Create(taskGrid.Selection.Left, newRow, taskGrid.Selection.Right, newRow + Len - 1);
  end;
  SetChanged;
end;

procedure TformNotetask.aMoveTaskDownExecute(Sender: TObject);
var
  newRow, Len: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 3 then exit;

  GridBackupSelection;
  Len := taskGrid.Selection.Bottom - taskGrid.Selection.Top + 1;
  if (SortOrder = soAscending) then
    newRow := Tasks.MoveTasksDown(taskGrid.Selection.Top, taskGrid.Selection.Bottom)
  else
    newRow := Tasks.MoveTasksUp(taskGrid.Selection.Bottom, taskGrid.Selection.Top);

  FillGrid;
  if (newRow > -1) then
  begin
    ResetRowHeight();
    taskGrid.Row := newRow;
    taskGrid.Selection := TGridRect.Create(taskGrid.Selection.Left, newRow - Len + 1, taskGrid.Selection.Right, newRow);
  end;
  SetChanged;
end;

procedure TformNotetask.aIndentTasksExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;
  IndentTasks;
end;

procedure TformNotetask.aOutdentTasksExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;
  IndentTasks(True);
end;

procedure TformNotetask.taskGridSelection(Sender: TObject; aCol, aRow: integer);
begin
  if (taskGrid.Selection.Height > 0) or (FLastSelectionHeight > 0) then
    SetInfo;
  FLastSelectionHeight := taskGrid.Selection.Height;
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

  CurrentDateTime := DateToString(Now);
  if IsEditing then
  begin
    if (taskGrid.Col = 5) then
    begin
      taskGrid.Cells[5, taskGrid.Row] := CurrentDateTime;
      FillGrid;
    end
    else
    if (taskGrid.Col in [2, 3]) then
    begin
      PosStart := Memo.SelStart;
      Memo.SelText := CurrentDateTime;
      Memo.SelStart := PosStart + Length(CurrentDateTime);
    end;
    Tasks.SetTask(taskGrid, taskGrid.Row, FBackup);
    SetChanged;
    SetInfo;
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
        if (FShowDuration) and (taskGrid.Col = 5) then
          FillGrid;
      end
      else
      begin
        Tasks.InsertTask('- [ ] ' + CurrentDateTime + ',', taskGrid.Row);
        FillGrid;
        taskGrid.Row := taskGrid.Row + 1;
      end;
      SetChanged;
      SetInfo;
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
    new.Add('[ ]');
    Tasks := TTasks.Create(new);
    SetChanged(False);
    EditComplite;
    FFileName := string.Empty;
    FEncoding := TEncoding.UTF8;
    FLineEnding := FLineEnding.WindowsCRLF;
    taskGrid.Clean;
    taskGrid.RowCount := 2;
    taskGrid.Col := 2;
    SetInfo;
  end;
end;

procedure TformNotetask.aNewWindowExecute(Sender: TObject);
var
  Process: TProcess;
begin
  if Screen.ActiveForm <> Self then exit;

  SaveFormSettings(self); // Save setting for new process

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

procedure TformNotetask.aSaveAsExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  SaveFileAs;
end;

procedure TformNotetask.aSaveExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  SaveFile(FFileName);
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

procedure TformNotetask.aShowStatusBarExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  ShowStatusBar := aShowStatusBar.Checked;
  SetInfo;
end;

procedure TformNotetask.aShowArchivedExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  ShowArchived := aShowArchived.Checked;
end;

procedure TformNotetask.aShowDurationExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  ShowDuration := aShowDuration.Checked;
end;

procedure TformNotetask.aShowColumnDoneExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  ShowColumnDone := aShowColumnDone.Checked;
end;

procedure TformNotetask.aShowColumnTaskExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  ShowColumnTask := aShowColumnTask.Checked;
end;

procedure TformNotetask.aShowColumnCommentExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  ShowColumnComment := aShowColumnComment.Checked;
end;

procedure TformNotetask.aShowColumnDateExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  ShowColumnDate := aShowColumnDate.Checked;
end;

procedure TformNotetask.aShowColumnAmountExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  ShowColumnAmount := aShowColumnAmount.Checked;
end;

procedure TformNotetask.aShowColumnFavoriteExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  ShowColumnFavorite := aShowColumnFavorite.Checked;
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
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangSpanishExecute(Sender: TObject);
begin
  SetLanguage('es');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangFrenchExecute(Sender: TObject);
begin
  SetLanguage('fr');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangGermanExecute(Sender: TObject);
begin
  SetLanguage('de');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangItalianExecute(Sender: TObject);
begin
  SetLanguage('it');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangPortugueseExecute(Sender: TObject);
begin
  SetLanguage('pt');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangRussianExecute(Sender: TObject);
begin
  SetLanguage('ru');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangJapaneseExecute(Sender: TObject);
begin
  SetLanguage('ja');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangKoreanExecute(Sender: TObject);
begin
  SetLanguage('ko');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangChineseExecute(Sender: TObject);
begin
  SetLanguage('zh');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangUkrainianExecute(Sender: TObject);
begin
  SetLanguage('uk');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangBelarusianExecute(Sender: TObject);
begin
  SetLanguage('be');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangHindiExecute(Sender: TObject);
begin
  SetLanguage('hi');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangArabicExecute(Sender: TObject);
begin
  SetLanguage('ar');
  if ShowDuration then FillGrid;
  SetInfo;
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
    Memo.Color := clRowFocused;
    Memo.Font.Color := clBlack;
  end;
end;

procedure TformNotetask.MemoChange(Sender: TObject);
begin
  taskGrid.Cells[taskGrid.Col, taskGrid.Row] := TMemo(Sender).Text;
  Tasks.SetTask(taskGrid, taskGrid.Row, FMemoStartEdit and FBackup); // Backup only on begin edit
  FMemoStartEdit := False;
  SetChanged;
  taskGrid.Repaint;
  EditControlSetBounds(Memo, taskGrid.Col, taskGrid.Row);

  if (taskGrid.Col = 4) then
    SetInfo;
end;

procedure TformNotetask.MemoKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_TAB then
  begin
    // Insert spaces
    Memo.SelText := '    ';
    Key := 0;
  end;
end;

procedure TformNotetask.MemoKeyPress(Sender: TObject; var Key: char);
begin
  // Replace comma with dot for decimal input
  if Key = ',' then
    Key := '.';

  // Allow digits and one decimal point
  if not (Key in ['0'..'9', '.', #8, #13]) then
    Key := #0 // Block other keys
  else if (Key = '.') and (Pos('.', TMemo(Sender).Text) > 0) then
    Key := #0; // Block second decimal point
end;

procedure TformNotetask.DatePickerChange(Sender: TObject);
begin
  taskGrid.Cells[taskGrid.Col, taskGrid.Row] :=
    FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat, TDateTimePicker(Sender).DateTime);
  Tasks.SetTask(taskGrid, taskGrid.Row, FBackup);
  EditControlSetBounds(DatePicker, taskGrid.Col, taskGrid.Row, 0, 0, 0, 0);
  if (FShowDuration) then FillGrid;
  SetChanged;
  SetInfo;
end;

procedure TformNotetask.EditControlSetBounds(Sender: TWinControl; aCol, aRow: integer; OffsetLeft: integer;
  OffsetTop: integer; OffsetRight: integer; OffsetBottom: integer);
var
  Rect: TRect;
begin
  Rect := taskGrid.CellRect(aCol, aRow);
  Sender.SetBounds(Rect.Left + OffsetLeft, Rect.Top + OffsetTop, Rect.Right - Rect.Left + OffsetRight,
    Rect.Bottom - Rect.Top + OffsetBottom);
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
    GridBackupSelection;
    Tasks.ClearTasksInRect(taskGrid, taskGrid.Selection);
    if (Assigned(Memo)) then
    begin
      Memo.OnChange := nil;
      Memo.Clear;
      Memo.OnChange := @MemoChange;
    end;
    SetChanged;
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
      if (FBackup) then
      begin
        GridBackupSelection;
        Tasks.CreateBackup;
      end;

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
      if (FBackup) then
      begin
        GridBackupSelection;
        Tasks.CreateBackup;
      end;

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
      if (FBackup) then
      begin
        GridBackupSelection;
        Tasks.CreateBackup;
      end;

      // Archivate task
      Tasks.ArchiveTask(RowIndex);
      FillGrid;
      ResetRowHeight;
      SetInfo;
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
      if (FBackup) then
      begin
        GridBackupSelection;
        Tasks.CreateBackup;
      end;

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
      ResetRowHeight;
      SetInfo;
      SetChanged; // Mark that data has changed
    end;
  end
  else
    ArchiveTask;
end;

procedure TformNotetask.CompleteTasks(aRow: integer = 0);
var
  RowIndex: integer;
  Check: boolean;
  i: integer;
begin
  // If multiple rows are selected
  if (taskGrid.Selection.Width > 0) or (taskGrid.Selection.Height > 0) then
  begin
    if FBackup then
    begin
      GridBackupSelection;
      Tasks.CreateBackup;
    end;
    // Mark tasks as completed from the end to avoid index shifting
    for i := taskGrid.Selection.Bottom downto taskGrid.Selection.Top do
    begin
      RowIndex := i;
      if (RowIndex > 0) and (RowIndex <= Tasks.Count) then
      begin
        // Mark the task as completed in the collection
        Tasks.CompleteTask(RowIndex, False);

        if Tasks.GetTask(RowIndex).Done then
        begin
          taskGrid.Cells[1, RowIndex] := '1';
          if (taskGrid.Cells[5, RowIndex] = '') then
            taskGrid.Cells[5, RowIndex] := DateToString(Now);
        end
        else
          taskGrid.Cells[1, RowIndex] := '0';

        Tasks.SetTask(taskGrid, RowIndex, False); // Backup created on start
      end;
    end;
    if ShowDuration then FillGrid;
    SetChanged; // Mark that data has changed
    SetInfo;
  end
  else
  begin
    // Get current RowIndex selected if no multiple selection
    if (aRow = 0) then
      RowIndex := taskGrid.Row
    else
      RowIndex := aRow;

    Check := False;
    if (RowIndex > 0) and (RowIndex <= Tasks.Count) then
    begin
      if FBackup then
      begin
        GridBackupSelection;
        Tasks.CreateBackup;
      end;
      // Mark the task as completed in the collection
      Tasks.CompleteTask(RowIndex, False);
      if Tasks.GetTask(RowIndex).Done then
      begin
        Check := True;
        taskGrid.Cells[1, RowIndex] := '1';
        if (taskGrid.Cells[5, RowIndex] = '') then
          taskGrid.Cells[5, RowIndex] := DateToString(Now);
      end
      else
        taskGrid.Cells[1, RowIndex] := '0';

      Tasks.SetTask(taskGrid, RowIndex, False);
      if (ShowDuration) and (Check) then FillGrid;
      SetChanged; // Mark that data has changed
      SetInfo;
    end;
  end;
end;

procedure TformNotetask.StarTask(aRow: integer = 0);
var
  RowIndex: integer;
begin
  if (aRow = 0) then
    RowIndex := taskGrid.Row
  else
    RowIndex := aRow;

  if (RowIndex > 0) and (RowIndex <= Tasks.Count) then
  begin
    if FBackup then
    begin
      GridBackupSelection;
      Tasks.CreateBackup;
    end;
    // Mark the task as completed in the collection
    Tasks.StarTask(RowIndex, False);
    if Tasks.GetTask(RowIndex).Star then
      taskGrid.Cells[6, RowIndex] := '1'
    else
      taskGrid.Cells[6, RowIndex] := '0';

    Tasks.SetTask(taskGrid, RowIndex, False);
    SetChanged; // Mark that data has changed
    Invalidate;
  end;
end;

procedure TformNotetask.IndentTasks(Outdent: boolean = False);
var
  RowIndex: integer;
  i: integer;
begin
  if FBackup then
  begin
    GridBackupSelection;
    Tasks.CreateBackup;
  end;
  // Mark tasks as completed from the end to avoid index shifting
  for i := taskGrid.Selection.Bottom downto taskGrid.Selection.Top do
  begin
    RowIndex := i;
    if (RowIndex > 0) and (RowIndex <= Tasks.Count) then
    begin
      if (not Outdent) then
        taskGrid.Cells[2, RowIndex] := '    ' + taskGrid.Cells[2, RowIndex]
      else
        taskGrid.Cells[2, RowIndex] := TrimLeadingSpaces(taskGrid.Cells[2, RowIndex]);

      Tasks.SetTask(taskGrid, RowIndex, False); // Backup created on start
    end;
  end;
  SetChanged; // Mark that data has changed
end;

procedure TformNotetask.GridBackupSelection;
begin
  FLastGridSelection := taskGrid.Selection;
  FLastGridRow := taskGrid.Row;
  FLastGridCol := taskGrid.Col;
end;

procedure TformNotetask.GridClearSelection;
begin
  FLastGridSelection := TRect.Empty;
  FLastGridRow := 1;
  FLastGridCol := 2;
  taskGrid.ClearSelections;
  taskGrid.Row := 1;
  taskGrid.Col := 2;
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
  if (Assigned(Memo)) and ((aRow = 0) or (aRow = taskGrid.Row)) then
    Memo.Height := taskGrid.DefaultRowHeight;
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

procedure TformNotetask.SetShowStatusBar(Value: boolean);
begin
  FShowStatusBar := Value;
  StatusBar.Visible := FShowStatusBar;
end;

procedure TformNotetask.SetShowArchived(Value: boolean);
begin
  FShowArchived := Value;
  FillGrid;
  SetInfo;
end;

procedure TformNotetask.SetShowDuration(Value: boolean);
begin
  FShowDuration := Value;

  if (FShowDuration) then
    taskGrid.DefaultColWidth := 55
  else
    taskGrid.DefaultColWidth := 40;

  FillGrid;
  SetInfo;
end;

procedure TformNotetask.SetShowColumnDone(Value: boolean);
begin
  FShowColumnDone := Value;
  taskGrid.Columns.Items[0].Visible := FShowColumnDone;
end;

procedure TformNotetask.SetShowColumnTask(Value: boolean);
begin
  FShowColumnTask := Value;
  taskGrid.Columns.Items[1].Visible := FShowColumnTask;
end;

procedure TformNotetask.SetShowColumnComment(Value: boolean);
begin
  FShowColumnComment := Value;
  taskGrid.Columns.Items[2].Visible := FShowColumnComment;
end;

procedure TformNotetask.SetShowColumnAmount(Value: boolean);
begin
  FShowColumnAmount := Value;
  taskGrid.Columns.Items[3].Visible := FShowColumnAmount;
  SetInfo;
end;

procedure TformNotetask.SetShowColumnDate(Value: boolean);
begin
  FShowColumnDate := Value;
  taskGrid.Columns.Items[4].Visible := FShowColumnDate;
end;

procedure TformNotetask.SetShowColumnFavorite(Value: boolean);
begin
  FShowColumnFavorite := Value;
  taskGrid.Columns.Items[5].Visible := FShowColumnFavorite;
end;

procedure TformNotetask.ApplyColumnSetting;
begin
  aShowDuration.Checked := FShowDuration;
  if (FShowDuration) then
    taskGrid.DefaultColWidth := 55
  else
    taskGrid.DefaultColWidth := 40;
  aShowColumnDone.Checked := FShowColumnDone;
  aShowColumnTask.Checked := FShowColumnTask;
  aShowColumnComment.Checked := FShowColumnComment;
  aShowColumnDate.Checked := FShowColumnDate;
  aShowColumnAmount.Checked := FShowColumnAmount;
  aShowColumnFavorite.Checked := FShowColumnFavorite;
  taskGrid.Columns.Items[0].Visible := FShowColumnDone;
  taskGrid.Columns.Items[1].Visible := FShowColumnTask;
  taskGrid.Columns.Items[2].Visible := FShowColumnComment;
  taskGrid.Columns.Items[3].Visible := FShowColumnAmount;
  taskGrid.Columns.Items[4].Visible := FShowColumnDate;
  taskGrid.Columns.Items[5].Visible := FShowColumnFavorite;

  ApplySortArrow;
end;

procedure TformNotetask.ApplySortArrow;
var
  i: integer;
begin
  for i := 0 to taskGrid.Columns.Count - 1 do
    taskGrid.Columns[i].Title.ImageIndex := -1;
  if (SortColumn > 0) then
  begin
    if SortOrder = soAscending then
      taskGrid.Columns[SortColumn - 1].Title.ImageIndex := 0
    else
      taskGrid.Columns[SortColumn - 1].Title.ImageIndex := 1;
  end;
end;

procedure TformNotetask.ApplySorting;
var
  i: integer;
begin
  FillGrid;
  ResetRowHeight;

  for i := 0 to taskGrid.Columns.Count - 1 do
    taskGrid.Columns[i].Title.ImageIndex := -1;

  aMoveTaskTop.Enabled := SortColumn = 0;
  aMoveTaskBottom.Enabled := SortColumn = 0;
  aMoveTaskUp.Enabled := SortColumn = 0;
  aMoveTaskDown.Enabled := SortColumn = 0;
  if (SortColumn = 0) then
    taskGrid.Options := taskGrid.Options + [goRowMoving]
  else
    taskGrid.Options := taskGrid.Options - [goRowMoving];
end;

procedure TformNotetask.FillGrid;
begin
  Tasks.FillGrid(taskGrid, FShowArchived, FShowDuration, SortOrder, SortColumn);
end;

procedure TformNotetask.SetInfo;
var
  CurAll: integer;
  CurDone: integer;
  SumAll: double;
  SumDone: double;
  DurationAll: string;
  DurationCurrent: string;
begin
  SetCaption;
  if (not ShowStatusBar) then exit;

  if Assigned(FEncoding) then
    statusBar.Panels[1].Text := UpperCase(GetEncodingName(FEncoding));
  if Assigned(FLineEnding) then
    statusBar.Panels[2].Text := FLineEnding.ToString;

  // Task counts
  if (taskGrid.Selection.Height = 0) then
  begin
    CurAll := Tasks.CalcCount(ShowArchived, False);
    CurDone := Tasks.CalcCount(ShowArchived, True);
  end
  else
  begin
    CurAll := Tasks.CalcCount(ShowArchived, False, taskGrid.Selection.Top, taskGrid.Selection.Bottom);
    CurDone := Tasks.CalcCount(ShowArchived, True, taskGrid.Selection.Top, taskGrid.Selection.Bottom);
  end;

  if (CurAll = CurDone) or (CurDone = 0) then
    statusBar.Panels[3].Text := CurAll.ToString + rrows
  else
    statusBar.Panels[3].Text := CurDone.ToString + ' / ' + CurAll.ToString + rrows;

  // Task amounts
  if (ShowColumnAmount) then
  begin
    if (taskGrid.Selection.Height = 0) then
    begin
      SumAll := Tasks.CalcSum(ShowArchived, False);
      SumDone := Tasks.CalcSum(ShowArchived, True);
    end
    else
    begin
      SumAll := Tasks.CalcSum(ShowArchived, False, taskGrid.Selection.Top, taskGrid.Selection.Bottom);
      SumDone := Tasks.CalcSum(ShowArchived, True, taskGrid.Selection.Top, taskGrid.Selection.Bottom);
    end;
    if (SumAll > 0) then
    begin
      if (SumAll = SumDone) or (SumDone = 0) then
        statusBar.Panels[4].Text := FormatFloat('#,##0.00', SumAll)
      else
        statusBar.Panels[4].Text := FormatFloat('#,##0.00', SumDone) + ' / ' + FormatFloat('#,##0.00', SumAll);
    end
    else
      statusBar.Panels[4].Text := string.empty;
  end
  else
    statusBar.Panels[4].Text := string.empty;

  // Task durations
  if (ShowDuration) then
  begin
    if (taskGrid.Selection.Height = 0) then
    begin
      DurationAll := Tasks.CalcDuration(ShowArchived, False);
      DurationCurrent := Tasks.CalcDuration(ShowArchived, True);
    end
    else
    begin
      DurationAll := Tasks.CalcDuration(ShowArchived, False, taskGrid.Selection.Top, taskGrid.Selection.Bottom);
      DurationCurrent := Tasks.CalcDuration(ShowArchived, True, taskGrid.Selection.Top, taskGrid.Selection.Bottom);
    end;
    if (DurationAll = DurationCurrent) or (DurationCurrent = string.Empty) then
      statusBar.Panels[5].Text := DurationAll
    else
      statusBar.Panels[5].Text := DurationCurrent + ' / ' + DurationAll;
  end
  else
    statusBar.Panels[5].Text := string.empty;
end;

procedure TformNotetask.SetLanguage(aLanguage: string = string.Empty);
begin
  aLangEnglish.Checked := False;
  aLangGerman.Checked := False;
  aLangSpanish.Checked := False;
  aLangFrench.Checked := False;
  aLangItalian.Checked := False;
  aLangPortuguese.Checked := False;
  aLangRussian.Checked := False;
  aLangUkrainian.Checked := False;
  aLangBelarusian.Checked := False;
  aLangHindi.Checked := False;
  aLangArabic.Checked := False;
  aLangChinese.Checked := False;
  aLangJapanese.Checked := False;
  aLangKorean.Checked := False;

  if (aLanguage <> string.Empty) then
  begin
    Language := aLanguage;
    ApplicationTranslate(Language);
  end;

  case Language of
    'en': aLangEnglish.Checked := True;
    'de': aLangGerman.Checked := True;
    'es': aLangSpanish.Checked := True;
    'fr': aLangFrench.Checked := True;
    'it': aLangItalian.Checked := True;
    'pt': aLangPortuguese.Checked := True;
    'ru': aLangRussian.Checked := True;
    'uk': aLangUkrainian.Checked := True;
    'be': aLangBelarusian.Checked := True;
    'hi': aLangHindi.Checked := True;
    'ar': aLangArabic.Checked := True;
    'zh': aLangChinese.Checked := True;
    'ja': aLangJapanese.Checked := True;
    'ko': aLangKorean.Checked := True;
    else
    // nolang
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

function TformNotetask.SaveFileAs: boolean;
begin
  if (saveDialog.Execute) then
  begin
    Result := SaveFile(saveDialog.FileName);
  end
  else
    Result := False;
end;

function TformNotetask.SaveFile(fileName: string = string.Empty): boolean;
begin
  if (fileName = string.Empty) and (FFileName = string.Empty) then
    Result := SaveFileAs;

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
    Result := True;
  end
  else
    Result := False;

  SetInfo;
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
        Result := SaveFile(FFileName);
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
  // Save settings for new file
  //SaveGridSettings(Self, taskGrid, string.Empty);
  // Save settings for current file
  SaveGridSettings(Self, taskGrid, FFileName);

  FFileName := fileName;
  EditComplite;
  ReadTextFile(FFileName, Content, FEncoding, FLineEnding, FLineCount);

  if Assigned(Tasks) then
    Tasks.Free;
  Tasks := TTasks.Create(TextToStringList(Content));

  // Load saved settings for file
  LoadGridSettings(Self, taskGrid, FFileName);
  ApplyColumnSetting;

  FillGrid;

  taskGrid.Row := 1;
  taskGrid.Col := 2;
  ResetRowHeight;
  SetChanged(False);
  SetInfo;
  Result := True;
end;

function TformNotetask.GetIsEditing: boolean;
begin
  Result := (taskGrid.EditorMode) or (FIsEditing);
end;

function TformNotetask.Find(aText: string; aMatchCase, aWrapAround, aDirectionDown: boolean; Silent: boolean = False): boolean;
var
  sValue, sText: unicodestring;
  Counter, CurRow, CurCol, StartRow, StartCol: integer;

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
    if (aText = FFoundText) then
    begin
      taskGrid.Row := FLastFoundRow;
      taskGrid.Col := FLastFoundCol;
      taskGrid.EditorMode := True;
      Memo.SelStart := FLastFoundSelStart;
      Memo.SelLength := FLastFoundSelLength;
    end
    else
    begin
      FLastFoundRow := StartRow;
      FLastFoundCol := StartCol;
      taskGrid.Row := FLastFoundRow;
      taskGrid.Col := FLastFoundCol;
    end;
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
    StartRow := taskGrid.Row;
    StartCol := taskGrid.Col;

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
      if ((aDirectionDown) and (CurCol < 4)) or ((not aDirectionDown) and (CurCol > 2)) then
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
            taskGrid.Col := 4;
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
            CurCol := 4;
            taskGrid.Col := 4;
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
var
  sValue, sText: unicodestring;

  procedure FindNextExecute;
  begin
    FFindText := aText;
    FMatchCase := aMatchCase;
    FWrapAround := aWrapAround;
    aFindNext.Execute;
  end;

begin
  sValue := unicodestring(Memo.SelText);
  sText := unicodestring(aText);

  if (FFoundText = string.Empty) or ((aMatchCase) and (sValue <> sText)) or ((not aMatchCase) and
    (UnicodeLowerCase(sValue) <> UnicodeLowerCase(sText))) then
    FindNextExecute
  else
  begin
    GridBackupSelection;
    Tasks.CreateBackup;
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
  GridBackupSelection;
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
