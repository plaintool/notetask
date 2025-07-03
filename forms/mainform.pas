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
  StrUtils,
  ActnList,
  ComCtrls,
  ExtCtrls,
  Grids,
  Math,
  Menus,
  PrintersDlgs,
  DateTimePicker,
  Printers,
  Process,
  LCLIntf,
  LCLType,
  LConvEncoding,
  GridPrn,
  task,
  lineending;

type

  { TformNotetask }
  TformNotetask = class(TForm)
    aArchiveTasks: TAction;
    aAbout: TAction;
    aCopy: TAction;
    aRunPowershell: TAction;
    aEnterSubmit: TAction;
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
    contextUndo: TMenuItem;
    contextCut: TMenuItem;
    contextCopy: TMenuItem;
    contextPaste: TMenuItem;
    contextDelete: TMenuItem;
    menuEnterSubmit: TMenuItem;
    menuRunPowershell: TMenuItem;
    MenuShowTime: TMenuItem;
    menuUndoAll: TMenuItem;
    menuPaste: TMenuItem;
    menuCopy: TMenuItem;
    menuCut: TMenuItem;
    contextSelectAll: TMenuItem;
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
    menuBidiRightToLeft: TMenuItem;
    aChatGpt: TAction;
    menuChatGpt: TMenuItem;
    Separator11: TMenuItem;
    contextAskChatGPT: TMenuItem;
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
    contextDeleteTasks: TMenuItem;
    Separator12: TMenuItem;
    contextArchiveTasks: TMenuItem;
    aShowDuration: TAction;
    aShowTime: TAction;
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
    aShowColumnNote: TAction;
    aShowColumnDate: TAction;
    Separator13: TMenuItem;
    menuColumnDone: TMenuItem;
    menuColumnNote: TMenuItem;
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
    aRunTerminal: TAction;
    menuRunTerminal: TMenuItem;
    contextRunTerminal: TMenuItem;
    aMergeTasks: TAction;
    MenuItem1: TMenuItem;
    contextMergeTasks: TMenuItem;
    memoNote: TMemo;
    Splitter: TSplitter;
    aShowNote: TAction;
    MenuItem2: TMenuItem;
    groupTabs: TTabControl;
    aInsertGroup: TAction;
    aDeleteGroup: TAction;
    aRenameGroup: TAction;
    aMoveTaskRight: TAction;
    aMoveTaskLeft: TAction;
    Separator15: TMenuItem;
    menuGroup: TMenuItem;
    menuInsertGroup: TMenuItem;
    menuRenameGroup: TMenuItem;
    menuDeleteGroup: TMenuItem;
    menuMoveTasksLeft: TMenuItem;
    menuMoveTasksRight: TMenuItem;
    aCopyGroup: TAction;
    MenuItem3: TMenuItem;
    aMoveGroupLeft: TAction;
    aMoveGroupRight: TAction;
    MenuItem4: TMenuItem;
    Separator16: TMenuItem;
    MenuItem5: TMenuItem;
    PopupTabs: TPopupMenu;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem15: TMenuItem;
    procedure aEnterSubmitExecute(Sender: TObject);
    procedure aRunPowershellExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure ApplicationException(Sender: TObject; E: Exception);
    procedure OnQueryEndSession(var CanEnd: boolean);
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
    procedure taskGridSetCheckboxState(Sender: TObject; ACol, ARow: integer; const Value: TCheckboxState);
    procedure taskGridSelection(Sender: TObject; aCol, aRow: integer);
    procedure memoNoteKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure memoNoteChange(Sender: TObject);
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
    procedure aShowTimeExecute(Sender: TObject);
    procedure aLangUkrainianExecute(Sender: TObject);
    procedure aLangBelarusianExecute(Sender: TObject);
    procedure aLangHindiExecute(Sender: TObject);
    procedure aLangArabicExecute(Sender: TObject);
    procedure aShowColumnDoneExecute(Sender: TObject);
    procedure aShowColumnTaskExecute(Sender: TObject);
    procedure aShowColumnNoteExecute(Sender: TObject);
    procedure aShowColumnDateExecute(Sender: TObject);
    procedure aShowColumnAmountExecute(Sender: TObject);
    procedure aShowColumnFavoriteExecute(Sender: TObject);
    procedure aIndentTasksExecute(Sender: TObject);
    procedure aOutdentTasksExecute(Sender: TObject);
    procedure aRunTerminalExecute(Sender: TObject);
    procedure aMergeTasksExecute(Sender: TObject);
    procedure aShowNoteExecute(Sender: TObject);
    procedure groupTabsChange(Sender: TObject);
    procedure groupTabsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure groupTabsMouseLeave(Sender: TObject);
    procedure groupTabsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure groupTabsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure aInsertGroupExecute(Sender: TObject);
    procedure aRenameGroupExecute(Sender: TObject);
    procedure aDeleteGroupExecute(Sender: TObject);
    procedure aMoveTaskLeftExecute(Sender: TObject);
    procedure aMoveTaskRightExecute(Sender: TObject);
    procedure aCopyGroupExecute(Sender: TObject);
    procedure aMoveGroupLeftExecute(Sender: TObject);
    procedure aMoveGroupRightExecute(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
  private
    Memo: TMemo;
    DatePicker: TDateTimePicker;
    FChanged: boolean;
    FBackup: boolean;
    FMemoStartEdit: boolean;
    FMemoOldText: TCaption;
    FMemoNeedSelectAll: boolean;
    FDatePickerOldDate: TDateTime;
    FIsEditing: boolean;
    FIsSelecting: boolean;
    FDisableCheckToggle: boolean;
    FDisableStarToggle: boolean;
    FFileName: string;
    FEncoding: TEncoding;
    FLineEnding: TLineEnding;
    FLineCount: integer;
    FWordWrap: boolean;
    FEnterSubmit: boolean;
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
    FLastRowHeights: array of integer;
    FLastRow: integer;
    FLastRowMem: array of integer;
    FDragTab: integer;
    procedure MemoChange(Sender: TObject);
    procedure MemoEnter(Sender: TObject);
    procedure MemoKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure MemoKeyPress(Sender: TObject; var Key: char);
    procedure DatePickerChange(Sender: TObject);
    procedure DatePickerEnter(Sender: TObject);
    procedure EditControlSetBounds(Sender: TWinControl; aCol, aRow: integer; OffsetLeft: integer = 4;
      OffsetTop: integer = 0; OffsetRight: integer = -8; OffsetBottom: integer = 0);
    procedure PrinterGetCellText(Sender: TObject; AGrid: TCustomGrid; ACol, ARow: integer; var AText: string);
    procedure PrinterPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    procedure SetChanged(aChanged: boolean = True);
    procedure EditCell(aCol, aRow: integer);
    procedure EditComplite(aEnter: boolean = False; aEscape: boolean = False);
    procedure DisableDrag;
    procedure SetInfo;
    procedure SetNote;
    procedure SetTabs;
    procedure SetCaption;
    procedure ClearSelected(ShowConfirm: boolean = True);
    procedure MergeTasks;
    procedure DeleteTask(aRow: integer = 0; ShowConfirm: boolean = True);
    procedure DeleteTasks(ShowConfirm: boolean = True);
    procedure ArchiveTask(aRow: integer = 0);
    procedure ArchiveTasks;
    procedure CompleteTasks(aRow: integer = 0);
    procedure StarTask(aRow: integer = 0);
    procedure IndentTasks(Outdent: boolean = False);
    procedure SetBiDiRightToLeft(Value: boolean);
    procedure SetShowStatusBar(Value: boolean);
    procedure SetShowNote(Value: boolean);
    procedure SetShowDuration(Value: boolean);
    procedure SetShowTime(Value: boolean);
    procedure SetShowArchived(Value: boolean);
    procedure SetShowColumnDone(Value: boolean);
    procedure SetShowColumnTask(Value: boolean);
    procedure SetShowColumnNote(Value: boolean);
    procedure SetShowColumnDate(Value: boolean);
    procedure SetShowColumnAmount(Value: boolean);
    procedure SetShowColumnFavorite(Value: boolean);
    procedure ApplyColumnSetting;
    procedure ApplySortArrow;
    procedure ApplySorting;
    procedure ApplySortingActions;
    procedure GridBackupSelection;
    procedure GridClearSelection;
    function GetExecuteValue(aRow: integer; memoPriority: boolean = False): string;
    procedure ExecuteChatGpt;
    procedure ExecuteTerminal(usePowershell: boolean = True);
    procedure MoveTabLeft(Index: integer);
    procedure MoveTabRight(Index: integer);
    procedure ChangeGroup(Index: integer);
    procedure CalcDefaultColWidth;
    procedure CalcRowHeights(aRow: integer = 0; aForce: boolean = False);
    procedure ResetRowHeight(aRow: integer = 0; aCalcRowHeight: boolean = True);
    procedure SwapRowHeights(RowIndex1, RowIndex2: integer);
    function LastRowHeight(aRow: integer): integer;
    function GetScrollPosition: integer;
    function GetIsEditing: boolean;
    function IsCanClose: boolean;
  public
    FShowArchived: boolean;
    FShowDuration: boolean;
    FShowTime: boolean;
    FShowNote: boolean;
    FShowStatusBar: boolean;
    FShowColumnDone: boolean;
    FShowColumnTask: boolean;
    FShowColumnNote: boolean;
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
    property EnterSubmit: boolean read FEnterSubmit write FEnterSubmit;
    property BiDiRightToLeft: boolean read FBiDiRightToLeft write SetBiDiRightToLeft;
    property ShowArchived: boolean read FShowArchived write SetShowArchived;
    property ShowDuration: boolean read FShowDuration write SetShowDuration;
    property ShowTime: boolean read FShowTime write SetShowTime;
    property ShowNote: boolean read FShowNote write SetShowNote;
    property ShowStatusBar: boolean read FShowStatusBar write SetShowStatusBar;
    property ShowColumnDone: boolean read FShowColumnDone write SetShowColumnDone;
    property ShowColumnTask: boolean read FShowColumnTask write SetShowColumnTask;
    property ShowColumnNote: boolean read FShowColumnNote write SetShowColumnNote;
    property ShowColumnDate: boolean read FShowColumnDate write SetShowColumnDate;
    property ShowColumnAmount: boolean read FShowColumnAmount write SetShowColumnAmount;
    property ShowColumnFavorite: boolean read FShowColumnFavorite write SetShowColumnFavorite;
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
  clDarkBlue: TColor;
  ResourceBitmapCheck: TBitmap;
  ResourceBitmapUncheck: TBitmap;
  ResourceBitmapStarGold: TBitmap;
  ResourceBitmapStarGray: TBitmap;

const
  {$IFDEF UNIX}
  DefRowHeight = 33;
  {$ELSE}
  DefRowHeight = 22;
  {$ENDIF}

resourcestring
  rapp = 'Notetask';
  runtitled = 'Untitled';
  rrows = ' tasks';
  rcantfind = 'Can''t find';
  rgroupuntitled = 'Ungrouped';
  rfilenotfound = 'The requested file was not found on the disk.';
  rdeleteconfirm = 'Are you sure you want to delete this task?';
  rdeletesconfirm = 'Are you sure you want to delete selected tasks?';
  rmergesconfirm = 'Are you sure you want to merge selected tasks?';
  rarchiveconfirm = 'Are you sure you want to archive / unarchive this task?';
  rarchivesconfirm = 'Are you sure you want to archive / unarchive selected tasks?';
  rsavechanges = 'Do you want to save the changes?';
  rclearconfirm = 'Are you sure you want to clear the data in the selected area?';
  ropendialogfilter = 'Task files (*.tsk)|*.tsk|Text files (*.txt)|*.txt|Markdown files (*.md)|*.md|All files (*.*)|*.*';
  rsavedialogfilter = 'Task files (*.tsk)|*.tsk|Text files (*.txt)|*.txt|Markdown files (*.md)|*.md|All files (*.*)|*.*';
  rundoconfirm = 'Are you sure you want to discard all changes? This action cannot be undone.';
  rnumstringtoolarge = 'The line number is out of the allowed range.';
  rchatgpt = 'https://chatgpt.com?q=';
  rdeletegroupconfirm = 'Are you sure you want to delete this group? This will also delete all tasks within this group.';
  rentergroupname = 'Enter the group name:';
  rgroup = 'Group';
  rOK = 'OK';

implementation

uses filemanager, settings, stringtool, systemtool, forminput, formfind, formreplace, formabout, formdonate;

  {$R *.lfm}

  { TformNotetask }

procedure TformNotetask.FormCreate(Sender: TObject);
var
  FilePath: string;
  FileOpened: boolean;
begin
  // Initialize variables
  FBackup := True;
  FWordWrap := True;
  FEnterSubmit := True;
  FShowTime := True;
  FShowStatusBar := True;
  FShowNote := False;
  FMemoNeedSelectAll := True;
  FShowColumnDone := True;
  FShowColumnTask := True;
  FShowColumnNote := True;
  FShowColumnDate := True;
  FShowColumnAmount := False;
  FShowColumnFavorite := True;
  FBiDiRightToLeft := self.BiDiMode = bdRightToLeft;
  FDragTab := -1;
  FSortColumn := 0;
  FSortOrder := soAscending;
  clRowHighlight := RGBToColor(220, 240, 255);
  clRowFocused := RGBToColor(200, 220, 255);
  clRowExpired := RGBToColor(255, 220, 220);
  clDarkBlue := RGBToColor(0, 0, 180);
  openDialog.Filter := ropendialogfilter;
  saveDialog.Filter := rsavedialogfilter;

  Application.OnException := @ApplicationException;
  Application.OnQueryEndSession := @OnQueryEndSession;

  taskGrid.DefaultRowHeight := DefRowHeight;

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

  // After load settings
  aWordWrap.Checked := FWordWrap;
  memoNote.WordWrap := FWordWrap;
  aEnterSubmit.Checked := FEnterSubmit;
  aBidiRightToLeft.Checked := FBiDiRightToLeft;
  aShowArchived.Checked := FShowArchived;
  ShowNote := FShowNote;
  ShowStatusBar := FShowStatusBar;
  ShowTime := FShowTime;

  // Apply loaded settings to columns
  ApplyColumnSetting;
  ApplySortingActions;

  // Set language
  SetLanguage(Language);

  // Menu access
  {$IFDEF UNIX}
  aRunPowershell.Visible := False;
  aRunPowershell.Enabled := False;
  {$ENDIF}

  // Check if a command line argument is passed
  FileOpened := False;
  if ParamCount > 0 then
  begin
    FilePath := ParamStr(1); // Get the file path
    if (not FilePath.StartsWith('--')) then
      FileOpened := OpenFile(FilePath); // Function to load a task from the file
  end;

  if not FileOpened then aNew.Execute;
end;

procedure TformNotetask.FormDestroy(Sender: TObject);
begin
  SaveFormSettings(Self);
  SaveGridSettings(Self, taskGrid, ExtractFileName(FFileName));

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
  if memoNote.Focused then exit;
  if Screen.ActiveForm <> Self then
  begin
    Key := 0;
    Exit;
  end;

  if (ssCtrl in Shift) and (ssShift in Shift) and (Key = VK_DELETE) then // Ctrl + Shift + Del
  begin
    aDeleteGroup.Execute;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_F2) then // Ctrl + F2
  begin
    aRenameGroup.Execute;
    Key := 0;
  end
  else
  if (Key = VK_F2) then // F2
  begin
    if ActiveControl is TCustomTabControl then
      aRenameGroup.Execute
    else
    begin
      if (not FIsEditing) then
        EditCell(taskGrid.Col, taskGrid.Row);
    end;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_INSERT) then // Ctrl + Insert
  begin
    aInsertGroup.Execute;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (ssShift in Shift) and (Key = VK_C) then // Ctrl + Shift + C
  begin
    aCopyGroup.Execute;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_DELETE) then // Ctrl + Del
  begin
    if not IsEditing then
      DeleteTasks;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key in [VK_1, VK_2, VK_3, VK_4, VK_5, VK_6, VK_7, VK_8, VK_9, VK_0]) then // Ctrl + Number
  begin
    EditComplite;
    if (Key = VK_0) then
      ChangeGroup(9)
    else
      ChangeGroup(Key - VK_0 - 1);
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
  if ((Shift = [ssCtrl]) or (Shift = [ssAlt])) and (Key = VK_LEFT) then // Ctrl || Alt + Left
  begin
    aMoveTaskLeft.Execute;
    Key := 0;
  end
  else
  if ((Shift = [ssCtrl]) or (Shift = [ssAlt])) and (Key = VK_RIGHT) then // Ctrl || Alt + Right
  begin
    aMoveTaskRight.Execute;
    Key := 0;
  end
  else
  if (Shift = [ssCtrl, ssShift]) and (Key = VK_LEFT) then // Ctrl + Shift + Left
  begin
    aMoveGroupLeft.Execute;
    Key := 0;
  end
  else
  if (Shift = [ssCtrl, ssShift]) and (Key = VK_RIGHT) then // Ctrl + Shift + Right
  begin
    aMoveGroupRight.Execute;
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
  if (Key in [VK_SPACE]) then // Space
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
      EditComplite(False, True)
    else
      taskGrid.ClearSelections;
    Key := 0;
  end
  else
  if (Key = VK_RETURN) then // Enter
  begin
    if IsEditing then
    begin
      if (taskGrid.Col in [4, 5]) or ((taskGrid.Col in [2, 3]) and ((FEnterSubmit and (Shift = [])) or
        (not FEnterSubmit and ((Shift = [ssCtrl]) or (Shift = [ssShift]))))) then
      begin
        EditComplite(True);
        Key := 0;
      end;
    end
    else
    begin
      if (taskGrid.Col in [2, 3, 4]) then
        FMemoNeedSelectAll := False
      else
      if (taskGrid.Col = 1) then
        CompleteTasks
      else
      if (taskGrid.Col = 6) and (taskGrid.Selection.Height = 0) and (taskGrid.Selection.Width = 0) then
        StarTask;
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

  CalcRowHeights(0, True);
end;

procedure TformNotetask.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := IsCanClose;
end;

procedure TformNotetask.ApplicationException(Sender: TObject; E: Exception);
begin
  MessageDlg('Notetask', E.Message, mtWarning, [mbOK], 0);
end;

procedure TformNotetask.OnQueryEndSession(var CanEnd: boolean);
begin
  CanEnd := IsCanClose;
  if (CanEnd) then
    Application.Terminate;
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
    // Trigger event
    taskGrid.OnSelection(taskGrid, taskGrid.Col, taskGrid.Row);
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
    exit;
  end;
  if (aCol = 6) then
  begin
    // Get mouse position in screen coordinates
    MousePosScreen := Mouse.CursorPos;

    // Convert screen coordinates to client coordinates (relative to the form)
    MousePosClient := taskGrid.ScreenToClient(MousePosScreen);

    // Check if the mouse is within the 16x16 checkbox area
    if not PtInRect(taskGrid.CellRect(ACol, ARow), MousePosClient) then
    begin
      // If the mouse is outside the checkbox, prevent the state from being changed
      FDisableStarToggle := True;
      exit;
    end;
    FDisableStarToggle := False;
    exit;
  end;
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
    if (FDisableStarToggle) then exit;

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
    SetChanged;
    SetNote;
  end;
end;

procedure TformNotetask.taskGridColRowDeleted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
begin
  if (not IsColumn) then
  begin
    Tasks.DeleteTask(tIndex);
    if (taskGrid.RowCount > 0) and (taskGrid.Row > 0) then taskGrid.Row := taskGrid.Row - 1;
    if ShowDuration then FillGrid;
    SetInfo;
    SetNote;
  end;
end;

procedure TformNotetask.taskGridHeaderSized(Sender: TObject; IsColumn: boolean; Index: integer);
begin
  taskGridResize(Sender);
  CalcRowHeights;
  EditControlSetBounds(Memo, taskGrid.Col, taskGrid.Row);
  EditControlSetBounds(DatePicker, taskGrid.Col, taskGrid.Row, 0, 0, 0, 0);
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

      if (taskGrid.CanFocus and taskGrid.Visible) then taskGrid.SetFocus;
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
  FS: TFormatSettings;
begin
  grid := Sender as TStringGrid;
  bgFill := clWhite;

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
      if (Tasks.HasTask(ARow)) then
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
    end;

    if (Tasks.HasTask(ARow)) then
    begin
      task := Tasks.GetTask(ARow);
      if task.Star then
        grid.Canvas.Font.Style := grid.Canvas.Font.Style + [fsBold];

      if (aCol = 2) and (task.Archive) then
        grid.Canvas.Font.Style := grid.Canvas.Font.Style + [fsStrikeOut];

      if (aCol = 3) and (task.NoteItalic) then
        grid.Canvas.Font.Style := grid.Canvas.Font.Style + [fsItalic];

      if (aCol = 5) and (task.Date > Now) and (not (gdSelected in aState)) then
        grid.Canvas.Font.Color := clDarkBlue;
    end;

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
    begin
      FS := DefaultFormatSettings;
      FS.ThousandSeparator := ' ';
      S := FormatFloat('#,##0.##########', StrToFloat(grid.Cells[ACol, ARow]), FS);
    end
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
    Memo.WantReturns := aCol in [2, 3];
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
    DatePicker.BorderStyle := bsNone;
    DatePicker.ParentFont := True;
    DatePicker.ArrowShape := asClassicSmaller;
    DatePicker.Options := [dtpoFlatButton];
    if (FShowTime) then
      DatePicker.Kind := dtkDateTime
    else
      DatePicker.Kind := dtkDate;
    DatePicker.TimeDisplay := tdHMS;
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
    DatePicker.OnEnter := @DatePickerEnter; // Event Enter

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
    FillGrid;
    SetChanged;
  end;
end;

procedure TformNotetask.taskGridSelection(Sender: TObject; aCol, aRow: integer);
begin
  if (taskGrid.Selection.Height > 0) or (FLastSelectionHeight > 0) then
    SetInfo;

  if (aRow <> FLastRow) then SetNote;
  FLastRow := aRow;

  // Disable merge if no multiselect
  if (taskGrid.Selection.Height > 0) then
    aMergeTasks.Enabled := True
  else
    aMergeTasks.Enabled := False;

  FLastSelectionHeight := taskGrid.Selection.Height;
end;

procedure TformNotetask.memoNoteKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  Field: TMemo;
  LinesPerPage: integer;
begin
  Field := (Sender as TMemo);

  if Key = VK_DELETE then // Delete
  begin
    if Field.SelLength = 0 then
    begin
      Field.SelStart := Field.SelStart;
      Field.SelLength := 1;
    end;
    Field.ClearSelection;
    Key := 0;
  end
  else
  if Key = VK_PRIOR then
  begin
    LinesPerPage := Field.ClientHeight div Canvas.TextHeight('Wg');
    Field.CaretPos := Point(0, Max(0, Field.CaretPos.Y - LinesPerPage));
    Field.VertScrollBar.Position := Field.CaretPos.Y - (LinesPerPage div 2);
    Field.Invalidate;
    Key := 0;
  end
  else
  if key = VK_NEXT then
  begin
    LinesPerPage := Field.ClientHeight div Canvas.TextHeight('Wg');
    Field.CaretPos := Point(0, Min(Field.Lines.Count - 1, Field.CaretPos.Y + LinesPerPage));
    Field.VertScrollBar.Position := Field.CaretPos.Y - (LinesPerPage div 2);
    Field.Invalidate;
    key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_Z) then // Ctrl + Z
  begin
    Field.Undo;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_X) then // Ctrl + X
  begin
    Field.CutToClipboard;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_C) then // Ctrl + C
  begin
    Field.CopyToClipboard;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_V) then // Ctrl + V
  begin
    Field.PasteFromClipboard;
    Key := 0;
  end
  else
  if (Shift = [ssCtrl]) and (Key = VK_A) then // Ctrl + A
  begin
    Field.SelectAll;
    Key := 0;
  end;

end;

procedure TformNotetask.memoNoteChange(Sender: TObject);
begin
  taskGrid.Cells[3, taskGrid.Row] := memoNote.Text;
  Tasks.SetTask(taskGrid, taskGrid.Row, FBackup, FShowTime);
  CalcRowHeights(taskGrid.Row);
  SetChanged;
end;

procedure TformNotetask.groupTabsChange(Sender: TObject);
begin
  if (Length(FLastRowMem) > Tasks.SelectedGroup) then
    FLastRowMem[Tasks.SelectedGroup] := taskGrid.Row;

  Tasks.ChangeGroup(groupTabs.TabIndex, True);

  FillGrid;

  taskGrid.Col := 2;
  if (Length(FLastRowMem) > Tasks.SelectedGroup) then
    taskGrid.Row := FLastRowMem[Tasks.SelectedGroup]
  else
    taskGrid.Row := 1;
  taskGrid.ClearSelections;

  Tasks.CreateBackup;
  GridBackupSelection;

  ResetRowHeight;
  SetNote;
  SetInfo;
end;

procedure TformNotetask.groupTabsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbLeft) then
    if (groupTabs.IndexOfTabAt(X, Y) = groupTabs.TabIndex) and not ((groupTabs.TabIndex = 0) and (Tasks.GroupNames[0] = string.Empty)) then
      FDragTab := groupTabs.TabIndex;
end;

procedure TformNotetask.groupTabsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  target: integer;
begin
  if FDragTab >= 0 then
  begin
    Screen.Cursor := crDrag;
    target := groupTabs.IndexOfTabAt(X, Y);
    if target >= 0 then
    begin
      if target > FDragTab then
      begin
        MoveTabRight(groupTabs.TabIndex);
        if (groupTabs.IndexOfTabAt(X, Y) <> groupTabs.TabIndex) then
          DisableDrag;
      end
      else
      if target < FDragTab then
      begin
        MoveTabLeft(groupTabs.TabIndex);
        if (groupTabs.IndexOfTabAt(X, Y) <> groupTabs.TabIndex) then
          DisableDrag;
      end;
    end;
  end;
end;

procedure TformNotetask.groupTabsMouseLeave(Sender: TObject);
begin
  DisableDrag;
end;

procedure TformNotetask.groupTabsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  TabIndex: integer;
begin
  DisableDrag;

  if Button = mbRight then
  begin
    TabIndex := groupTabs.IndexOfTabAt(X, Y);
    if TabIndex <> -1 then
      groupTabs.TabIndex := TabIndex;
    PopupTabs.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
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

    // Encoding of new file
    FEncoding := TEncoding.UTF8;

    // Lineending
    {$IFDEF UNIX}
    FLineEnding := FLineEnding.UnixLF;
    {$ELSE}
    FLineEnding := FLineEnding.WindowsCRLF;
    {$ENDIF}

    taskGrid.Clean;
    taskGrid.RowCount := 2;
    taskGrid.Col := 2;
    ResetRowHeight;
    SetInfo;
    SetTabs;
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

procedure TformNotetask.aUndoExecute(Sender: TObject);
var
  TempRect: TRect;
  TempLastRow, TempLastCol: integer;
  TempTopRow: integer;
begin
  if memoNote.Focused then exit;
  if Screen.ActiveForm <> Self then exit;

  if not IsEditing then
  begin
    TempTopRow := taskGrid.TopRow;
    TempRect := FLastGridSelection;
    TempLastRow := FLastGridRow;
    TempLastCol := FLastGridCol;
    GridBackupSelection;

    Tasks.UndoBackup;

    FillGrid;
    ResetRowHeight;
    taskGrid.Col := TempLastCol;
    if (TempLastRow > 1) then
      taskGrid.Row := TempLastRow;
    if (TempRect.Width > 0) and (TempRect.Height > 0) then
      taskGrid.Selection := TRect.Create(TempRect.Left, TempRect.Top, TempRect.Right, TempRect.Bottom);
    taskGrid.TopRow := TempTopRow;
    SetInfo;
    SetNote;
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
      SetNote;
      SetTabs;
      GridClearSelection;
      Tasks.CreateBackup;
      SetChanged(False);
    end;
  end;
end;

procedure TformNotetask.aCutExecute(Sender: TObject);
begin
  if memoNote.Focused then exit;
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
    Tasks.CopyToClipboard(taskGrid, FShowNote)
  else
  if (taskGrid.InplaceEditor.InheritsFrom(TCustomEdit)) then
    (taskGrid.InplaceEditor as TCustomEdit).CopyToClipboard;
end;

procedure TformNotetask.aPasteExecute(Sender: TObject);
var
  Sel: TGridRect;
begin
  if memoNote.Focused then exit;
  if Screen.ActiveForm <> Self then exit;

  if not IsEditing then
  begin
    Sel := Tasks.PasteFromClipboard(taskGrid);
    FillGrid;
    if (Assigned(DatePicker)) then
      DatePicker.DateTime := Tasks.GetTask(taskGrid.Row).Date;
    if (SortColumn = 0) then
      taskGrid.Selection := Sel;
    SetChanged;
    SetInfo;
    SetNote;
  end
  else
  if (taskGrid.InplaceEditor.InheritsFrom(TCustomEdit)) then
    (taskGrid.InplaceEditor as TCustomEdit).PasteFromClipboard;
end;

procedure TformNotetask.aDeleteExecute(Sender: TObject);
begin
  if memoNote.Focused then exit;
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
  if memoNote.Focused then exit;
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;

  if not IsEditing then
  begin
    taskGrid.Selection := TGridRect.Create(0, 0, 6, taskGrid.RowCount);
    SetInfo;
  end
  else
  if (taskGrid.InplaceEditor.InheritsFrom(TCustomEdit)) then
    (taskGrid.InplaceEditor as TCustomEdit).SelectAll;
end;

procedure TformNotetask.aExitExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  if IsCanClose then
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

    CalcRowHeights(0, True);
  end;
end;

procedure TformNotetask.aInsertTaskExecute(Sender: TObject);
var
  Ind: integer;
begin
  if Screen.ActiveForm <> Self then exit;

  EditComplite;
  GridBackupSelection;
  Ind := Tasks.InsertTask('[ ]', taskGrid.Row);
  FillGrid;
  ResetRowHeight;
  if (Ind > 0) then
    taskGrid.Row := Tasks.ReverseMap(Ind)
  else
    taskGrid.Row := taskGrid.Row + 1;
  SetInfo;
  SetChanged;
  SetNote;
end;

procedure TformNotetask.aMergeTasksExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;

  MergeTasks;
end;

procedure TformNotetask.aDeleteTasksExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;

  DeleteTasks;
end;

procedure TformNotetask.aMoveTaskTopExecute(Sender: TObject);
var
  newRow, selLen, selCol, selLeft, selRight: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 3 then exit;

  GridBackupSelection;
  selLen := taskGrid.Selection.Bottom - taskGrid.Selection.Top + 1;
  selLeft := taskGrid.Selection.Left;
  selRight := taskGrid.Selection.Right;
  selCol := taskGrid.Col;

  if (SortOrder = soAscending) then
    newRow := Tasks.MoveTasksTop(taskGrid.Selection.Top, taskGrid.Selection.Bottom)
  else
    newRow := Tasks.MoveTasksBottom(taskGrid.Selection.Bottom, taskGrid.Selection.Top);

  FillGrid;
  if (newRow > -1) then
  begin
    ResetRowHeight;
    taskGrid.Row := 0;
    taskGrid.Col := selCol;
    taskGrid.Selection := TGridRect.Create(selLeft, 0, selRight, selLen);
  end;
  SetChanged;
  SetInfo;
end;

procedure TformNotetask.aMoveTaskBottomExecute(Sender: TObject);
var
  newRow, selLen, selCol, selLeft, selRight: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 3 then exit;

  GridBackupSelection;
  selLen := taskGrid.Selection.Bottom - taskGrid.Selection.Top + 1;
  selLeft := taskGrid.Selection.Left;
  selRight := taskGrid.Selection.Right;
  selCol := taskGrid.Col;

  if (SortOrder = soAscending) then
    newRow := Tasks.MoveTasksBottom(taskGrid.Selection.Top, taskGrid.Selection.Bottom)
  else
    newRow := Tasks.MoveTasksTop(taskGrid.Selection.Bottom, taskGrid.Selection.Top);

  FillGrid;
  if (newRow > -1) then
  begin
    ResetRowHeight;
    taskGrid.Row := taskGrid.RowCount - selLen;
    taskGrid.Col := selCol;
    taskGrid.Selection := TGridRect.Create(selLeft, taskGrid.RowCount - selLen, selRight, taskGrid.RowCount);
  end;
  SetChanged;
  SetInfo;
end;

procedure TformNotetask.aMoveTaskUpExecute(Sender: TObject);
var
  newRow, selLen, selCol, selLeft, selRight: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 3 then exit;

  GridBackupSelection;
  selLen := taskGrid.Selection.Bottom - taskGrid.Selection.Top + 1;
  selLeft := taskGrid.Selection.Left;
  selRight := taskGrid.Selection.Right;
  selCol := taskGrid.Col;

  if (SortOrder = soAscending) then
    newRow := Tasks.MoveTasksUp(taskGrid.Selection.Top, taskGrid.Selection.Bottom)
  else
    newRow := Tasks.MoveTasksDown(taskGrid.Selection.Bottom, taskGrid.Selection.Top);

  FillGrid;
  if (newRow > -1) then
  begin
    ResetRowHeight(-1);
    taskGrid.Row := newRow;
    taskGrid.Col := selCol;
    taskGrid.Selection := TGridRect.Create(selLeft, newRow, selRight, newRow + selLen - 1);
    ResetRowHeight(-1);
  end;
  SetChanged;
  SetInfo;
end;

procedure TformNotetask.aMoveTaskDownExecute(Sender: TObject);
var
  newRow, selLen, selCol, selLeft, selRight: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 3 then exit;

  GridBackupSelection;
  selLen := taskGrid.Selection.Bottom - taskGrid.Selection.Top + 1;
  selLeft := taskGrid.Selection.Left;
  selRight := taskGrid.Selection.Right;
  selCol := taskGrid.Col;

  if (SortOrder = soAscending) then
    newRow := Tasks.MoveTasksDown(taskGrid.Selection.Top, taskGrid.Selection.Bottom)
  else
    newRow := Tasks.MoveTasksUp(taskGrid.Selection.Bottom, taskGrid.Selection.Top);

  FillGrid;
  if (newRow > -1) then
  begin
    ResetRowHeight(-1);
    taskGrid.Row := newRow;
    taskGrid.Col := selCol;
    taskGrid.Selection := TGridRect.Create(selLeft, newRow - selLen + 1, selRight, newRow);
    ResetRowHeight(-1);
  end;
  SetChanged;
  SetInfo;
end;

procedure TformNotetask.aMoveTaskLeftExecute(Sender: TObject);
var
  newRow, selCol, selLen, selLeft, selRight, selEnd: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if (groupTabs.TabIndex <= 0) then exit;
  if taskGrid.RowCount < 2 then exit;

  GridBackupSelection;
  selLen := taskGrid.Selection.Bottom - taskGrid.Selection.Top + 1;
  selLeft := taskGrid.Selection.Left;
  selRight := taskGrid.Selection.Right;
  selCol := taskGrid.Col;

  newRow := Tasks.MoveGroupTasks(taskGrid.Selection.Top, taskGrid.Selection.Bottom, Tasks.SelectedGroup - 1);

  if (newRow > -1) then
  begin
    ChangeGroup(Tasks.SelectedGroup);
    newRow := Tasks.ReverseMap(newRow);
    taskGrid.Row := newRow;
    if (SortOrder = soAscending) then
      selEnd := newRow + selLen - 1
    else
      selEnd := newRow - selLen - 1;
    taskGrid.Col := selCol;
    taskGrid.Selection := TGridRect.Create(selLeft, newRow, selRight, selEnd);
  end;
  SetChanged;
  SetInfo;
end;

procedure TformNotetask.aMoveTaskRightExecute(Sender: TObject);
var
  newRow, selCol, selLen, selLeft, selRight, selEnd: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if (groupTabs.TabIndex >= groupTabs.Tabs.Count - 1) then exit;
  if taskGrid.RowCount < 2 then exit;

  GridBackupSelection;
  selLen := taskGrid.Selection.Bottom - taskGrid.Selection.Top + 1;
  selLeft := taskGrid.Selection.Left;
  selRight := taskGrid.Selection.Right;
  selCol := taskGrid.Col;

  newRow := Tasks.MoveGroupTasks(taskGrid.Selection.Top, taskGrid.Selection.Bottom, Tasks.SelectedGroup + 1);

  if (newRow > -1) then
  begin
    ChangeGroup(Tasks.SelectedGroup);
    newRow := Tasks.ReverseMap(newRow);
    taskGrid.Row := newRow;
    if (SortOrder = soAscending) then
      selEnd := newRow + selLen - 1
    else
      selEnd := newRow - selLen - 1;
    taskGrid.Col := selCol;
    taskGrid.Selection := TGridRect.Create(selLeft, newRow, selRight, selEnd);
  end;
  SetChanged;
  SetInfo;
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

procedure TformNotetask.aArchiveTasksExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;

  ArchiveTasks;
end;

procedure TformNotetask.aDateTimeExecute(Sender: TObject);
var
  PosStart: integer;
  CurrentDateTimeISO: string;
  CurrentDateTime: string;
begin
  if Screen.ActiveForm <> Self then exit;

  CurrentDateTime := DateTimeToString(Now, FShowTime);
  CurrentDateTimeISO := DateTimeToStringISO(Now, FShowTime);

  if memoNote.Focused then
  begin
    PosStart := memoNote.SelStart;
    memoNote.SelText := CurrentDateTime;
    memoNote.SelStart := PosStart;
    memoNote.SelLength := Length(CurrentDateTime);
  end
  else
  if IsEditing then
  begin
    if (taskGrid.Col = 5) then
    begin
      taskGrid.Cells[5, taskGrid.Row] := CurrentDateTime;
      DatePicker.DateTime := Now;
    end
    else
    if (taskGrid.Col in [2, 3]) then
    begin
      PosStart := Memo.SelStart;
      Memo.SelText := CurrentDateTime;
      Memo.SelStart := PosStart;
      Memo.SelLength := Length(CurrentDateTime);
    end;
    Tasks.SetTask(taskGrid, taskGrid.Row, FBackup, FShowTime);
    SetChanged;
    SetInfo;
  end
  else
  begin
    if (taskGrid.Col > 0) then
    begin
      if (taskGrid.RowCount > 1) then
      begin
        if (taskGrid.Cells[taskGrid.Col, taskGrid.Row].Trim = string.Empty) or (taskGrid.Col = 5) then
          taskGrid.Cells[taskGrid.Col, taskGrid.Row] := CurrentDateTime
        else
          taskGrid.Cells[taskGrid.Col, taskGrid.Row] := taskGrid.Cells[taskGrid.Col, taskGrid.Row].Trim + ' ' + CurrentDateTime;
        Tasks.SetTask(taskGrid, taskGrid.Row, FBackup, FShowTime);
        if (Assigned(DatePicker)) then
          DatePicker.DateTime := Now;
        if (FShowDuration) and (taskGrid.Col = 5) then
          FillGrid;
      end
      else
      begin
        Tasks.InsertTask('- [ ] ' + CurrentDateTimeISO + ',', taskGrid.Row);
        FillGrid;
        taskGrid.Row := taskGrid.Row + 1;
      end;
      SetChanged;
      SetInfo;
    end;
  end;
end;

procedure TformNotetask.aInsertGroupExecute(Sender: TObject);
var
  Result: integer;
  newName: string;
begin
  if Screen.ActiveForm <> Self then exit;

  // Create an instance of the form
  with formInputText do
  try
    Left := self.Left + 14;
    Top := self.top + 52;
    SetCaption(rgroup, rentergroupname, rOK);

    // Show the form as a modal dialog
    if ShowModal = mrOk then
    begin
      newName := editText.Text;
      if (newName = rgroupuntitled) then newName := string.Empty;

      Result := Tasks.InsertGroup(newName);
      if (Result <> groupTabs.TabIndex) then
      begin
        SetTabs;
        ChangeGroup(Result);
        SetChanged;
      end;
    end;
  finally
    Hide;
  end;
end;

procedure TformNotetask.aRenameGroupExecute(Sender: TObject);
var
  newName: string;
begin
  if Screen.ActiveForm <> Self then exit;

  // Create an instance of the form
  with formInputText do
  try
    Left := self.Left + 14;
    Top := self.top + 52;
    SetCaption(rgroup, rentergroupname, rOK, groupTabs.Tabs[groupTabs.TabIndex]);

    // Show the form as a modal dialog
    if (ShowModal = mrOk) {and (editText.Text <> groupTabs.Tabs[groupTabs.TabIndex])} then
    begin
      newName := editText.Text;
      if (newName = rgroupuntitled) and (groupTabs.TabIndex = 0) then newName := string.Empty;

      if (Tasks.RenameGroup(groupTabs.TabIndex, newName)) then
      begin
        SetTabs;
        SetChanged;
      end;
    end;
  finally
    Hide;
  end;
end;

procedure TformNotetask.aCopyGroupExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  // Create an instance of the form
  with formInputText do
  try
    Left := self.Left + 14;
    Top := self.top + 52;
    SetCaption(rgroup, rentergroupname, rOK, groupTabs.Tabs[groupTabs.TabIndex]);

    // Show the form as a modal dialog
    if (ShowModal = mrOk) then
    begin
      if (Tasks.CopyGroup(groupTabs.TabIndex, editText.Text)) then
      begin
        SetTabs;
        ChangeGroup(groupTabs.TabIndex + 1);
        SetChanged;
      end;
    end;
  finally
    Hide;
  end;
end;

procedure TformNotetask.aDeleteGroupExecute(Sender: TObject);
var
  Confirm: integer;
begin
  Confirm := MessageDlg(rdeletegroupconfirm, mtConfirmation, [mbYes, mbNo], 0);

  if (Confirm = mrYes) then
  begin
    if (Tasks.DeleteGroup(groupTabs.TabIndex)) then
    begin
      SetTabs;
      ChangeGroup(Tasks.SelectedGroup);
      SetChanged;
    end;
  end;
end;

procedure TformNotetask.aMoveGroupLeftExecute(Sender: TObject);
begin
  MoveTabLeft(groupTabs.TabIndex);
end;

procedure TformNotetask.aMoveGroupRightExecute(Sender: TObject);
begin
  MoveTabRight(groupTabs.TabIndex);
end;

procedure TformNotetask.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  if Length(FileNames) > 0 then
  begin
    if IsCanClose then
      OpenFile(FileNames[0]);
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
      gridPrinter.Margins.Left := pageSetupDialog.MarginLeft / 100;
      gridPrinter.Margins.Right := pageSetupDialog.MarginRight / 100;
      gridPrinter.Margins.Top := pageSetupDialog.MarginTop / 100;
      gridPrinter.Margins.Bottom := pageSetupDialog.MarginBottom / 100;
      gridPrinter.FixedLineColor := clSilver;
      gridPrinter.BorderLineColor := clSilver;
      gridPrinter.GridLineColor := clSilver;
      gridPrinter.Footer.LineColor := clSilver;
      gridPrinter.Header.LineColor := clSilver;

      gridPrinter.Print;
    finally
      gridPrinter.Free;
    end;
  end;
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

procedure TformNotetask.aShowTimeExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  EditComplite;
  ShowTime := aShowTime.Checked;
  FillGrid;
end;

procedure TformNotetask.aShowNoteExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  ShowNote := aShowNote.Checked;
end;

procedure TformNotetask.aShowStatusBarExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  ShowStatusBar := aShowStatusBar.Checked;
  SetInfo;
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

procedure TformNotetask.aShowColumnNoteExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  ShowColumnNote := aShowColumnNote.Checked;
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
  memoNote.WordWrap := FWordWrap;
  ResetRowHeight;
  Invalidate;
end;

procedure TformNotetask.aEnterSubmitExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  FEnterSubmit := aEnterSubmit.Checked;
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

  ExecuteChatGpt;
end;

procedure TformNotetask.aRunTerminalExecute(Sender: TObject);
begin
  if taskGrid.RowCount < 2 then exit;

  ExecuteTerminal(False);
end;

procedure TformNotetask.aRunPowershellExecute(Sender: TObject);
begin
  if taskGrid.RowCount < 2 then exit;

  ExecuteTerminal;
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
        if (rowNum >= 1) and (rowNum <= Tasks.Count) then
        begin
          // Move to the specified row
          taskGrid.Row := Tasks.ReverseMap(rowNum - 1);
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

  if (assigned(formFindText)) and (formFindText.Visible) then
  begin
    MatchCase := formFindText.checkMatchCase.Checked;
    WrapAround := formFindText.checkWrapAround.Checked;
  end;

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
  SaveGridSettings(Self, taskGrid, ExtractFileName(FFileName));

  FFileName := fileName;
  EditComplite;
  ReadTextFile(FFileName, Content, FEncoding, FLineEnding, FLineCount);

  if Assigned(Tasks) then
    Tasks.Free;
  Tasks := TTasks.Create(TextToStringList(Content));

  // Load saved settings for file
  LoadGridSettings(Self, taskGrid, ExtractFileName(FFileName));

  SetChanged(False);
  ShowNote := FShowNote;
  ShowStatusBar := FShowStatusBar;
  ShowArchived := FShowArchived;
  Showtime := FShowTime;

  // Apply loaded settings to columns
  ApplyColumnSetting;
  ApplySortingActions;

  FillGrid;

  taskGrid.Row := 1;
  taskGrid.Col := 2;
  ResetRowHeight;
  SetInfo;
  SetNote;
  SetTabs;
  Result := True;
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
var
  TaskList: TStringList;
begin
  if (fileName = string.Empty) and (FFileName = string.Empty) then
    Result := SaveFileAs;

  if (fileName = string.Empty) then
    fileName := FFileName
  else
    FFileName := fileName;

  if (fileName <> string.Empty) then
  begin
    TaskList := Tasks.ToStringList;
    if Assigned(TaskList) and (TaskList <> nil) then
    begin
      try
        EditComplite;
        SaveTextFile(fileName, TaskList, FEncoding, FLineEnding);
        SetChanged(False);
        Tasks.CreateBackupInit;
        Result := True;
      finally
        TaskList.Free;
      end;
    end;
  end
  else
    Result := False;

  SetInfo;
end;

function TformNotetask.GetExecuteValue(aRow: integer; memoPriority: boolean = False): string;
begin
  // If note column is selected or note panel visible
  if (((taskGrid.Selection.Left = 3) and (taskGrid.Selection.Right >= 3)) or ((memoNote.Visible) and
    ((memoPriority) or (memoNote.SelLength > 0) or (memoNote.Focused)))) then
  begin
    if (memoNote.Visible) and (memoNote.SelLength > 0) then
      Result := memoNote.SelText
    else
    begin
      if (Memo.Visible) and (Memo.SelLength > 0) then
        Result := Memo.SelText
      else
      if (not string.IsNullOrEmpty(Tasks.GetTask(aRow).Note)) then
        Result := Tasks.GetTask(aRow).Note
      else
        Result := Tasks.GetTask(aRow).Text;
    end;
  end
  else
  begin
    if (Memo.Visible) and (Memo.SelLength > 0) then
      Result := Memo.SelText
    else
      Result := Tasks.GetTask(aRow).Text;
  end;
end;

procedure TformNotetask.ExecuteChatGpt;
begin
  OpenURL(rchatgpt + EncodeUrl(GetExecuteValue(taskGrid.Row)));
end;

procedure TformNotetask.ExecuteTerminal(usePowershell: boolean = True);
var
  Process: TProcess;
  Script, ScriptPreview: TStringList;
  TempFile, PwshPath: string;
  Value, EncodedValue, ConsoleEncoding: string;
  ScriptEncoding: TEncoding;
  Overflow: boolean;
  maxPreview: integer;
  i, k: integer;
begin
  // Define the temporary file for commands
  {$IFDEF UNIX}
  TempFile := GetTempDir + 'notetask.sh';   // Path for Linux
  {$ELSE}
  if usePowershell then
    TempFile := GetTempDir + 'notetask.ps1' // PowerShell script
  else
    TempFile := GetTempDir + 'notetask.bat'; // CMD script
  {$ENDIF}

  Script := TStringList.Create;
  ScriptPreview := TStringList.Create;
  try
    Overflow := False;
    maxPreview := 30;

    // Get the current console encoding
    ConsoleEncoding := GetConsoleEncoding;

    {$IFDEF UNIX}
    Script.Add('#!/bin/bash');
    {$ENDIF}

    for i := taskGrid.Selection.Top to taskGrid.Selection.Bottom do
    begin
      Value := GetExecuteValue(i, True);
      if usePowershell then
        EncodedValue := Value
      else
        EncodedValue := ConvertEncoding(Value, 'utf-8', ConsoleEncoding);
      Script.Add(EncodedValue);

      if ScriptPreview.Count < maxPreview then
      begin
        with TStringList.Create do
        try
          Text := Value;
          for k := 0 to Min(maxPreview, Count - 1) do
          begin
            if ScriptPreview.Count < maxPreview then
              ScriptPreview.Add(Strings[k])
            else if not Overflow then
            begin
              ScriptPreview.Add('...');
              Overflow := True;
            end;
          end;
        finally
          Free;
        end;
      end
      else if not Overflow then
      begin
        ScriptPreview.Add('...');
        Overflow := True;
      end;
    end;

    {$IFDEF UNIX}
    Script.Add('read -p "Press any key to continue..."');
    {$ENDIF}

    if (Script.Count = 0) or ((Script.Count = 1) and (Trim(Script[0]) = '')) then
      Exit;

    if usePowershell then
    begin
      ScriptEncoding := TEncoding.GetEncoding(65001); // UTF-8 BOM
      try
        SaveTextFile(TempFile, Script, ScriptEncoding, TLineEnding.WindowsCRLF);
      finally
        ScriptEncoding.Free;
      end;
    end
    else
      Script.SaveToFile(TempFile); // default ANSI

    // Message to confirm
    if usePowershell then
      Value := aRunPowershell.Caption
    else
      Value := aRunTerminal.Caption;
    if (MessageDlg(ReplaceStr(Value, '...', '?') + sLineBreak + sLineBreak + ScriptPreview.Text, mtConfirmation,
      [mbYes, mbNo], 0, mbNo) <> mrYes) then exit;
  finally
    ScriptPreview.Free;
    Script.Free;
  end;

  {$IFDEF UNIX}
  // Make the .sh file executable in Linux
  Process := TProcess.Create(nil);
  try
    Process.Executable := '/bin/chmod';
    Process.Parameters.Add('+x');
    Process.Parameters.Add(TempFile);
    Process.Options := [poWaitOnExit]; // Wait for the process to finish
    Process.Execute;
  finally
    Process.Free;
  end;
  {$ENDIF}

  // Create a new process to execute the script
  Process := TProcess.Create(nil);
  try
    {$IFDEF UNIX}
    Process.Executable := '/bin/bash';  // Use bash to execute the script
    Process.Parameters.Add('-e');
    Process.Parameters.Add(TempFile);
    {$ELSE}
    if usePowershell then
    begin
      PwshPath := FindPowerShellCore; // Search for pwsh.exe
      if PwshPath <> '' then
        Process.Executable := PwshPath
      else
        Process.Executable := 'powershell.exe';
      Process.Parameters.Add('-NoExit');
      Process.Parameters.Add('-ExecutionPolicy');
      Process.Parameters.Add('Bypass');
      Process.Parameters.Add('-File');
      Process.Parameters.Add(TempFile);
    end
    else
    begin
      Process.Executable := 'cmd.exe';
      Process.Parameters.Add('/K');
      Process.Parameters.Add(TempFile);
    end;
    {$ENDIF}
    Process.Options := [poNewConsole]; // Open in a new console window

    // Execute the process
    Process.Execute;
  finally
    Process.Free;
  end;
end;

procedure TformNotetask.MoveTabLeft(Index: integer);
var
  Result: integer;
begin
  if (Index = 1) and (Tasks.GroupNames[0] = string.Empty) then exit;

  Result := Tasks.MoveGroupLeft(Index);
  if (Result <> Index) then
  begin
    SetTabs;
    if (FDragTab >= 0) then FDragTab := Result;
    ChangeGroup(Result);
    SetChanged;
  end;
end;

procedure TformNotetask.MoveTabRight(Index: integer);
var
  Result: integer;
begin
  if (Index = 0) and (Tasks.GroupNames[0] = string.Empty) then exit;

  Result := Tasks.MoveGroupRight(Index);
  if (Result <> Index) then
  begin
    SetTabs;
    if (FDragTab >= 0) then FDragTab := Result;
    ChangeGroup(Result);
    SetChanged;
  end;
end;

procedure TformNotetask.ChangeGroup(Index: integer);
var
  force: boolean;
begin
  if (Index < 0) or (index > groupTabs.Tabs.Count - 1) then exit;
  force := groupTabs.TabIndex = Index;
  groupTabs.TabIndex := Index;
  if (force) then groupTabsChange(groupTabs);
end;

procedure TformNotetask.PrinterPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
var
  prncanvas: TCanvas;
  task: TTask;
begin
  if not Tasks.HasTask(aRow) then exit;

  prncanvas := TGridPrinter(Sender).Canvas;
  task := Tasks.GetTask(aRow);

  // Default text color
  prncanvas.Font.Color := clBlack;
  prncanvas.Font.Style := [];

  // Color and style
  if (ShowColumnDate) and (not task.Done) and (task.Date > 0) and (task.Date < Now) then
    prncanvas.Font.Color := clRed
  else if (not task.Done) and task.Archive then
    prncanvas.Font.Color := clMaroon;

  if task.Star then
    prncanvas.Font.Style := prncanvas.Font.Style + [fsBold];

  if (aCol = 2) and task.Archive then
    prncanvas.Font.Style := prncanvas.Font.Style + [fsStrikeOut];

  if (aCol = 3) and task.NoteItalic then
    prncanvas.Font.Style := prncanvas.Font.Style + [fsItalic];

  if (aCol = 5) and (task.Date > Now) then
    prncanvas.Font.Color := clDarkBlue;

  // Text styles
  with prncanvas.TextStyle do
  begin
    SingleLine := not FWordWrap;
    WordBreak := FWordWrap;
    RightToLeft := FBiDiRightToLeft;
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
  FMemoOldText := taskGrid.Cells[TaskGrid.Col, TaskGrid.Row];

  // If amount column selected then clean when edit
  if (FMemoNeedSelectAll) and (taskGrid.Col in [2, 3, 4]) then
    Memo.SelectAll;
  FMemoNeedSelectAll := True;

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
  Tasks.SetTask(taskGrid, taskGrid.Row, FMemoStartEdit and FBackup, FShowTime); // Backup only on begin edit
  FMemoStartEdit := False;
  SetChanged;
  CalcRowHeights(taskGrid.Row);
  EditControlSetBounds(Memo, taskGrid.Col, taskGrid.Row);
  if (taskGrid.Col = 3) then
    SetNote;
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
  if not (Key in ['0'..'9', '.', '-', #8, #13]) then
    Key := #0 // Block other keys
  else if (Key = '.') and (Pos('.', TMemo(Sender).Text) > 0) then
    Key := #0; // Block second decimal point
end;

procedure TformNotetask.DatePickerEnter(Sender: TObject);
begin
  FDatePickerOldDate := DatePicker.DateTime;
  if (taskGrid.IsCellSelected[taskGrid.Col, taskGrid.Row]) and ((taskGrid.Selection.Height > 0) or (taskGrid.Selection.Width > 0)) then
  begin
    DatePicker.Color := clHighlight;
    DatePicker.Font.Color := clWhite;
  end
  else
  begin
    DatePicker.Color := clRowFocused;
    DatePicker.Font.Color := clBlack;
  end;
end;

procedure TformNotetask.DatePickerChange(Sender: TObject);
begin
  taskGrid.Cells[taskGrid.Col, taskGrid.Row] := DateTimeToString(TDateTimePicker(Sender).DateTime);
  Tasks.SetTask(taskGrid, taskGrid.Row, FBackup, FShowTime);
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
  if Assigned(Sender) then
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

procedure TformNotetask.MergeTasks;
var
  i, Confirm: integer;
  Task, Target: TTask;
  Sel: TRect;
  MaxDate: TDateTime;
begin
  // If multiple rows are selected
  if (taskGrid.Selection.Height > 0) then
  begin
    Confirm := MessageDlg(rmergesconfirm, mtConfirmation, [mbYes, mbNo], 0);

    if (Confirm = mrYes) then
    begin
      if (FBackup) then
      begin
        GridBackupSelection;
        Tasks.CreateBackup;
      end;

      Task := Tasks.GetTask(taskGrid.Selection.Top);
      MaxDate := Task.Date;
      for i := taskGrid.Selection.Top + 1 to taskGrid.Selection.Bottom do
      begin
        Target := Tasks.GetTask(i);
        Task.Text := task.Text + IfThen(Target.Text <> string.Empty, FLineEnding.Value + Target.Text, string.Empty);
        Task.Note := task.Note + IfThen(Target.Note <> string.Empty, FLineEnding.Value + Target.Note, string.Empty);
        Task.Amount := Task.Amount + Target.Amount;

        if (Target.Date > MaxDate) then
        begin
          MaxDate := Target.Date;
          Task.Date := MaxDate;
        end;
        if Target.Done = False then
          Task.Done := False;
        if Target.Archive = False then
          Task.Archive := False;
        if Target.Star = True then
          Task.Star := True;
      end;
      for i := taskGrid.Selection.Bottom downto taskGrid.Selection.Top + 1 do
        Tasks.DeleteTask(i);

      // Mem selection
      Sel := taskGrid.Selection;

      FillGrid;
      SetChanged; // Mark that data has changed

      // Restore selection
      taskGrid.Row := Sel.Top;
      taskGrid.Selection := TGridRect.Create(Sel.Left, Sel.Top, Sel.Right, Sel.Top);
    end;
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

      SetChanged; // Mark that data has changed
    end;
  end;
end;

procedure TformNotetask.DeleteTasks(ShowConfirm: boolean = True);
var
  i, RowIndex, Confirm: integer;
begin
  // If multiple rows are selected
  if (taskGrid.Selection.Height > 0) then
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
      SetNote;
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
      SetNote;
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
  if (not ShowColumnDone) then exit;

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
            taskGrid.Cells[5, RowIndex] := DateTimeToString(Now);
        end
        else
          taskGrid.Cells[1, RowIndex] := '0';

        Tasks.SetTask(taskGrid, RowIndex, False, FShowTime); // Backup created on start
      end;
    end;
    if ShowDuration then FillGrid;
    SetChanged; // Mark that data has changed
    SetInfo;
    Invalidate;
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
          taskGrid.Cells[5, RowIndex] := DateTimeToString(Now);
      end
      else
        taskGrid.Cells[1, RowIndex] := '0';

      Tasks.SetTask(taskGrid, RowIndex, False, FShowTime);
      if (ShowDuration) and (Check) then FillGrid;
      SetChanged; // Mark that data has changed
      SetInfo;
      Invalidate;
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

    Tasks.SetTask(taskGrid, RowIndex, False, FShowTime);
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
      begin
        taskGrid.Cells[2, RowIndex] := '    ' + taskGrid.Cells[2, RowIndex];
        CalcRowHeights();
      end
      else
        taskGrid.Cells[2, RowIndex] := TrimLeadingSpaces(taskGrid.Cells[2, RowIndex]);

      Tasks.SetTask(taskGrid, RowIndex, False, FShowTime); // Backup created on start
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

function TformNotetask.GetScrollPosition: integer;
var
  i: integer;
begin
  Result := 0;

  for i := 1 to taskGrid.TopRow do
  begin
    Result += taskGrid.RowHeights[i] + taskGrid.GridLineWidth;
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

procedure TformNotetask.SetBiDiRightToLeft(Value: boolean);
var
  i: integer;
begin
  FBiDiRightToLeft := Value;

  if (Value) then
  begin
    taskGrid.BiDiMode := bdRightToLeft;
    groupTabs.BiDiMode := bdRightToLeft;
    for i := 1 to taskGrid.Columns.Count - 1 do
      taskGrid.Columns[i].Alignment := taRightJustify;
    memoNote.BiDiMode := bdRightToLeft;
    memoNote.BorderSpacing.Left := 0;
    memoNote.BorderSpacing.Right := 5;
  end
  else
  begin
    taskGrid.BiDiMode := bdLeftToRight;
    groupTabs.BiDiMode := bdLeftToRight;
    for i := 1 to taskGrid.Columns.Count - 1 do
      taskGrid.Columns[i].Alignment := taLeftJustify;
    memoNote.BiDiMode := bdLeftToRight;
    memoNote.BorderSpacing.Left := 5;
    memoNote.BorderSpacing.Right := 0;
  end;
end;

procedure TformNotetask.SetShowStatusBar(Value: boolean);
begin
  FShowStatusBar := Value;

  aShowStatusBar.Checked := FShowStatusBar;
  StatusBar.Visible := FShowStatusBar;
end;

procedure TformNotetask.SetShowArchived(Value: boolean);
begin
  FShowArchived := Value;
  aShowArchived.Checked := FShowArchived;
  FillGrid;
  ResetRowHeight;
  SetInfo;
  SetNote;
end;

procedure TformNotetask.SetShowDuration(Value: boolean);
begin
  FShowDuration := Value;

  CalcDefaultColWidth;
  FillGrid;
  SetInfo;
end;

procedure TformNotetask.SetShowTime(Value: boolean);
begin
  aShowTime.Checked := Value;
  FShowTime := Value;
end;

procedure TformNotetask.SetShowNote(Value: boolean);
begin
  FShowNote := Value;

  aShowNote.Checked := FShowNote;
  memoNote.Visible := FShowNote;
  Splitter.Visible := FShowNote;
  StatusBar.Top := memoNote.Top + MemoNote.Height;

  SetNote;
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
  ResetRowHeight;
end;

procedure TformNotetask.SetShowColumnNote(Value: boolean);
begin
  FShowColumnNote := Value;
  taskGrid.Columns.Items[2].Visible := FShowColumnNote;
  ResetRowHeight;
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
  CalcDefaultColWidth;
  aShowColumnDone.Checked := FShowColumnDone;
  aShowColumnTask.Checked := FShowColumnTask;
  aShowColumnNote.Checked := FShowColumnNote;
  aShowColumnDate.Checked := FShowColumnDate;
  aShowColumnAmount.Checked := FShowColumnAmount;
  aShowColumnFavorite.Checked := FShowColumnFavorite;
  taskGrid.Columns.Items[0].Visible := FShowColumnDone;
  taskGrid.Columns.Items[1].Visible := FShowColumnTask;
  taskGrid.Columns.Items[2].Visible := FShowColumnNote;
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

  ApplySortingActions;

  SetNote;
end;

procedure TformNotetask.ApplySortingActions;
begin
  aMoveTaskTop.Enabled := SortColumn = 0;
  aMoveTaskBottom.Enabled := SortColumn = 0;
  aMoveTaskUp.Enabled := SortColumn = 0;
  aMoveTaskDown.Enabled := SortColumn = 0;
  aMoveTaskLeft.Enabled := SortColumn = 0;
  aMoveTaskRight.Enabled := SortColumn = 0;

  if (SortColumn = 0) then
    taskGrid.Options := taskGrid.Options + [goRowMoving]
  else
    taskGrid.Options := taskGrid.Options - [goRowMoving];
end;

procedure TformNotetask.FillGrid;
begin
  Tasks.FillGrid(taskGrid, FShowArchived, FShowDuration, FShowTime, SortOrder, SortColumn);
  CalcRowHeights;
end;

procedure TformNotetask.CalcDefaultColWidth;
begin
  if (FShowDuration) then
    taskGrid.DefaultColWidth := Canvas.TextWidth('10.10sec') + 10
  else
    taskGrid.DefaultColWidth := Canvas.TextWidth('10000');
end;

procedure TformNotetask.CalcRowHeights(aRow: integer = 0; aForce: boolean = False);
var
  FromRow, ToRow: integer;

  procedure CalcCol(col: integer; force: boolean = False);
  var
    row: integer;
    drawrect: TRect;
    Text: string;
    flags: cardinal;
    h: integer;
  begin
    for row := FromRow to ToRow do
    begin
      drawrect := taskGrid.CellRect(col, row);
      drawrect.Inflate(-4, 0);

      Text := taskGrid.Cells[col, row];

      flags := DT_CALCRECT;
      if FBiDiRightToLeft then
        flags := flags + DT_RIGHT
      else
        flags := flags + DT_LEFT;
      if FWordWrap then
        flags := flags or DT_WORDBREAK;

      DrawText(taskGrid.canvas.handle, PChar(Text), Length(Text), drawrect, flags);

      if (force) or ((drawrect.bottom - drawrect.top) > taskGrid.RowHeights[row]) then
      begin
        h := drawrect.bottom - drawrect.top + 2;
        if (force) and (h < taskGrid.DefaultRowHeight) then
          h := Max(taskGrid.Canvas.TextHeight('A') + 2, taskGrid.DefaultRowHeight);
        FLastRowHeights[row] := h;
        taskGrid.RowHeights[row] := h;
      end
      else
        FLastRowHeights[row] := taskGrid.RowHeights[row];
    end;
  end;

begin
  SetLength(FLastRowHeights, taskGrid.RowCount);

  if aRow = -1 then
  begin
    FromRow := taskGrid.Selection.Top;
    ToRow := taskGrid.Selection.Bottom;
  end
  else
  if aRow = 0 then
  begin
    FromRow := 1;
    ToRow := taskGrid.RowCount - 1;
  end
  else
  begin
    FromRow := aRow;
    ToRow := aRow;
  end;
  if (ShowColumnTask) then CalcCol(2, aForce);
  if (ShowColumnNote) then CalcCol(3);

  // Header, tabs, first col
  taskGrid.RowHeights[0] := Max(Canvas.TextHeight('A') + 2, taskGrid.DefaultRowHeight);
  if (aForce) then
  begin
    {$IFDEF Windows}
    groupTabs.Height := Canvas.TextHeight('A') + 8;
    {$ENDIF}
    {$IFDEF UNIX}
    groupTabs.Height := Canvas.TextHeight('A') + 11;
    {$ENDIF}
    CalcDefaultColWidth;
  end;
end;

procedure TformNotetask.ResetRowHeight(aRow: integer = 0; aCalcRowHeight: boolean = True);
var
  i: integer;
  h: integer;
begin
  h := Max(Canvas.TextHeight('A') + 2, taskGrid.DefaultRowHeight);

  // if -1 only selection
  if (aRow = -1) then
  begin
    for i := taskGrid.Selection.Top to taskGrid.Selection.Bottom do
      taskGrid.RowHeights[i] := h;
  end
  else
  // if 0 for all rows
  if (aRow = 0) then
  begin
    for i := 1 to taskGrid.RowCount - 1 do
      taskGrid.RowHeights[i] := h;
  end
  else // if valid row just that row
    taskGrid.RowHeights[aRow] := taskGrid.DefaultRowHeight;

  if (Assigned(Memo)) and ((aRow = 0) or (aRow = taskGrid.Row)) then
    Memo.Height := h;

  if (aCalcRowHeight) then
    CalcRowHeights(aRow);
end;

function TformNotetask.LastRowHeight(aRow: integer): integer;
begin
  if (Length(FLastRowHeights) > aRow) then
    Result := FLastRowHeights[aRow]
  else
    Result := taskGrid.DefaultRowHeight;
end;

procedure TformNotetask.SetInfo;
var
  CurAll: integer;
  CurDone: integer;
  SumAll: double;
  SumDone: double;
  DurationAll: string;
  DurationCurrent: string;
  FS: TFormatSettings;
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
    if (SumAll <> 0) then
    begin
      FS := DefaultFormatSettings;
      FS.ThousandSeparator := ' ';
      if (SumAll = SumDone) or (SumDone = 0) then
        statusBar.Panels[4].Text := FormatFloat('#,##0.00', SumAll, FS)
      else
        statusBar.Panels[4].Text := FormatFloat('#,##0.00', SumDone, FS) + ' / ' + FormatFloat('#,##0.00', SumAll, FS);
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

procedure TformNotetask.SetNote;
begin
  if (not ShowNote) then exit;

  memoNote.OnChange := nil;
  if (Assigned(Tasks)) and (taskGrid.RowCount > 1) and (Tasks.Map(taskGrid.Row) > -1) then
    memoNote.Text := Tasks.GetTask(taskGrid.Row).Note
  else
    memoNote.Text := string.Empty;
  memoNote.OnChange := @memoNoteChange;
end;

procedure TformNotetask.SetTabs;
var
  Clean: TStringList;
  i: integer;
  LastIndex: integer;
begin
  LastIndex := groupTabs.TabIndex;
  Clean := TStringList.Create;
  try
    for i := 0 to Tasks.CountGroup - 1 do
    begin
      if Tasks.GroupNames[i] = string.Empty then
        Clean.Add(rgroupuntitled)
      else
        Clean.Add(Tasks.GroupNames[i].TrimLeft([' ', '#']).Trim);
    end;

    groupTabs.Tabs := Clean;
    groupTabs.Visible := not ((groupTabs.Tabs.Count = 1) and (Tasks.GroupNames[0] = string.Empty));
    if (LastIndex > 0) and (LastIndex < groupTabs.Tabs.Count) then
      groupTabs.TabIndex := LastIndex;

    // Set selected row memory for tabs
    SetLength(FLastRowMem, groupTabs.Tabs.Count);
  finally
    Clean.Free;
  end;
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

  openDialog.Filter := ropendialogfilter;
  saveDialog.Filter := rsavedialogfilter;

  if (Assigned(Tasks)) and (Tasks.GroupNames[0] = string.Empty) and (groupTabs.Tabs.Count > 0) then
    groupTabs.Tabs[0] := rgroupuntitled;

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
var
  NewCaption: string;
begin
  if (FFileName <> '') then
    NewCaption := ExtractFileName(FFileName) + ' - ' + rapp
  else
    NewCaption := runtitled + ' - ' + rapp;

  if FChanged then
    NewCaption := '*' + NewCaption;

  if Caption <> NewCaption then
    Caption := NewCaption;
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
    EditControlSetBounds(Memo, taskGrid.Col, taskGrid.Row);
    Memo.Text := Tasks.GetTaskValue(aCol, aRow);
    Memo.SelectAll;
    Memo.SetFocus;
    FMemoStartEdit := True;
  end;
  if (Assigned(DatePicker)) and (DatePicker.Visible) then
  begin
    EditControlSetBounds(DatePicker, taskGrid.Col, taskGrid.Row, 0, 0, 0, 0);
  end;
end;

procedure TformNotetask.EditComplite(aEnter: boolean = False; aEscape: boolean = False);
begin
  if taskGrid.EditorMode then
  begin
    if (taskGrid.Col = 5) and (Assigned(DatePicker)) then
    begin
      if (aEnter) then
      begin
        if taskGrid.Cells[taskGrid.Col, taskGrid.Row] = string.empty then
          DatePickerChange(DatePicker);
      end
      else
      // Pressing the Escape key on the date column cancels editing
      if (aEscape) then
      begin
        DatePicker.DateTime := FDatePickerOldDate;
        DatePickerChange(DatePicker);
      end;
    end;

    if (taskGrid.Col in [2, 3, 4]) and (Assigned(Memo)) then
    begin
      // Pressing the Escape key cancels editing
      if (aEscape) then
        Memo.Text := FMemoOldText;
    end;

    taskGrid.EditorMode := False;
    FIsEditing := False;
  end;
end;

procedure TformNotetask.DisableDrag;
begin
  if FDragTab >= 0 then
  begin
    FDragTab := -1;
    Screen.Cursor := crDefault;
  end;
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

function TformNotetask.GetIsEditing: boolean;
begin
  Result := (taskGrid.EditorMode) or (FIsEditing);
end;

function TformNotetask.Find(aText: string; aMatchCase, aWrapAround, aDirectionDown: boolean; Silent: boolean = False): boolean;
var
  sValue, sText: unicodestring;
  Counter, CurRow, CurCol, StartRow, StartCol: integer;
  LastDate: boolean;

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

  procedure IncCurCol;
  begin
    Inc(CurCol);
    taskGrid.Col := CurCol;
    Memo.SelStart := 0;
    Memo.SelLength := 0;
  end;

  procedure DecCurCol;
  begin
    Dec(CurCol);
    taskGrid.Col := CurCol;
    Memo.SelStart := Length(unicodestring(Memo.Text));
    Memo.SelLength := 0;
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
      ShowMessage(rcantfind + ' "' + string(aText) + '"');
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
    LastDate := False;

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

    // For the date, we move to the next or prev line if on the found one
    if (aDirectionDown) and (FLastFoundCol = 5) and (taskGrid.Col = 5) then
    begin
      if (taskGrid.Row < taskGrid.RowCount - 1) then
      begin
        taskGrid.Row := taskGrid.Row + 1;
        taskGrid.Col := 1;
      end
      else
        LastDate := True;
    end
    else
    if (not aDirectionDown) and (FLastFoundCol = 5) and (taskGrid.Col = 5) then
    begin
      if (taskGrid.Row > 1) then
      begin
        taskGrid.Row := taskGrid.Row - 1;
        taskGrid.Col := 5;
      end
      else
        LastDate := True;
    end;

    CurRow := taskGrid.Row;
    CurCol := taskGrid.Col;
    Counter := 0;

    repeat
      if (CurCol < 5) and (CurCol > 1) and (Assigned(Memo)) then
      begin
        sValue := unicodestring(Memo.Text);
        sText := unicodestring(aText);
        if (Pos(UnicodeLowerCase(sText), UnicodeLowerCase(sValue)) > 0) and (FindMemo) then
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
      end
      else
      if (CurCol = 5) and (Assigned(DatePicker)) then
      begin
        sValue := unicodestring(DateTimeToString(DatePicker.DateTime));
        sText := unicodestring(aText);
        if (Pos(UnicodeLowerCase(sText), UnicodeLowercase(sValue)) > 0) and (taskGrid.Cells[taskGrid.Col, taskGrid.Row] <>
          string.Empty) and (not LastDate) then
        begin
          taskGrid.EditorMode := True;
          FLastFoundRow := taskGrid.Row;
          FLastFoundCol := taskGrid.Col;
          FLastFoundSelStart := 0;
          FLastFoundSelLength := Length(sValue);
          FFoundText := aText;
          Counter := 0;
          Break;
        end;
      end;

      // Move to col
      if ((aDirectionDown) and (CurCol < 5)) or ((not aDirectionDown) and (CurCol > 2)) then
      begin
        // Move to next col
        if (aDirectionDown) then
          IncCurCol
        else // Move to prev col
          DecCurCol;
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
            CurCol := 5;
            taskGrid.Row := taskGrid.Row - 1;
            taskGrid.Col := 5;
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
            CurCol := 5;
            taskGrid.Col := 5;
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

      // Skip hidden columns
      if (aDirectionDown) then
      begin
        if (CurCol = 2) and (not ShowColumnTask) then IncCurCol;
        if (CurCol = 3) and (not ShowColumnNote) then IncCurCol;
        if (CurCol = 4) and (not ShowColumnAmount) then IncCurCol;
        if (CurCol = 5) and (not ShowColumnDate) then IncCurCol;
      end
      else
      begin
        if (CurCol = 5) and (not ShowColumnDate) then DecCurCol;
        if (CurCol = 4) and (not ShowColumnAmount) then DecCurCol;
        if (CurCol = 3) and (not ShowColumnNote) then DecCurCol;
        if (CurCol = 2) and (not ShowColumnTask) then DecCurCol;
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
  sValue, sText: unicodestring;
begin
  FBackup := False;
  Row := taskGrid.Row;
  Col := taskGrid.Col;
  GridBackupSelection;
  Tasks.CreateBackup; // FBackup = false here
  try
    // Replace current selection
    sValue := unicodestring(Memo.SelText);
    sText := unicodestring(aText);
    if not ((FFoundText = string.Empty) or ((aMatchCase) and (sValue <> sText)) or ((not aMatchCase) and
      (UnicodeLowerCase(sValue) <> UnicodeLowerCase(sText)))) then
      Memo.SelText := aToText;

    // Replace all
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
