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
  Types,
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
  Clipbrd,
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
    aSaveNotesAs: TAction;
    aDuplicateTasks: TAction;
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
    contextAskChatGPT1: TMenuItem;
    contextCopy1: TMenuItem;
    contextCut1: TMenuItem;
    contextDelete1: TMenuItem;
    contextPaste1: TMenuItem;
    contextRunPowershell1: TMenuItem;
    contextRunTerminal1: TMenuItem;
    contextSelectAll1: TMenuItem;
    contextUndo1: TMenuItem;
    fontDialog: TFontDialog;
    MainMenu: TMainMenu;
    memoNote: TMemo;
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
    contextDuplicateTasks: TMenuItem;
    contextOutdentTasks: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem6: TMenuItem;
    contextRunPowershell: TMenuItem;
    contextIndentTasks: TMenuItem;
    contextSaveNotesAs: TMenuItem;
    MenuItem8: TMenuItem;
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
    panelNote: TPanel;
    Popup: TPopupMenu;
    PopupMemo: TPopupMenu;
    printDialog: TPrintDialog;
    saveDialog: TSaveDialog;
    saveNotesDialog: TSaveDialog;
    Separator1: TMenuItem;
    menuExit: TMenuItem;
    Separator10: TMenuItem;
    Separator17: TMenuItem;
    Separator18: TMenuItem;
    Separator2: TMenuItem;
    Separator21: TMenuItem;
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
    aDuplicateGroup: TAction;
    MenuItem3: TMenuItem;
    aMoveGroupLeft: TAction;
    aMoveGroupRight: TAction;
    MenuItem4: TMenuItem;
    Separator16: TMenuItem;
    MenuItem5: TMenuItem;
    PopupTabs: TPopupMenu;
    MenuItem7: TMenuItem;
    contextInsertTask: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem15: TMenuItem;
    procedure aSaveNotesAsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure ApplicationException(Sender: TObject; E: Exception);
    procedure memoNoteMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure memoNoteDblClick(Sender: TObject);
    procedure memoNoteEnter(Sender: TObject);
    procedure memoNoteExit(Sender: TObject);
    procedure OnQueryEndSession(var CanEnd: boolean);
    procedure groupTabsChange(Sender: TObject);
    procedure groupTabsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure groupTabsMouseLeave(Sender: TObject);
    procedure groupTabsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure groupTabsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure panelNoteMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure panelNoteMouseEnter(Sender: TObject);
    procedure panelNoteMouseLeave(Sender: TObject);
    procedure panelNoteMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure panelNoteMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
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
    procedure aDuplicateTasksExecute(Sender: TObject);
    procedure aEnterSubmitExecute(Sender: TObject);
    procedure aRunPowershellExecute(Sender: TObject);
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
    procedure aInsertGroupExecute(Sender: TObject);
    procedure aRenameGroupExecute(Sender: TObject);
    procedure aDeleteGroupExecute(Sender: TObject);
    procedure aMoveTaskLeftExecute(Sender: TObject);
    procedure aMoveTaskRightExecute(Sender: TObject);
    procedure aDuplicateGroupExecute(Sender: TObject);
    procedure aMoveGroupLeftExecute(Sender: TObject);
    procedure aMoveGroupRightExecute(Sender: TObject);
    procedure memoNoteKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure memoNoteChange(Sender: TObject);
    procedure taskGridUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    Memo: TMemo;
    PanelMemo: TPanel;
    DatePicker: TDateTimePicker;
    FChanged: boolean;
    FBackup: boolean;
    FMemoStartEdit: boolean;
    FMemoOldText: TCaption;
    FMemoNeedSelectAll: boolean;
    FMemoBackup: TCaption;
    FMemoSelStartBackup: integer;
    FMemoSelLengthBackup: integer;
    FMemoFirstKey: boolean;
    FMemoNoteBackup: TCaption;
    FMemoNoteSelStartBackup: integer;
    FMemoNoteSelLengthBackup: integer;
    FMemoNoteCaretBackup: TPoint;
    FMemoNoteVertScrollBackup: integer;
    FMemoNoteFirstKey: boolean;
    FDatePickerOldDate: TDateTime;
    FDatePickerDateSet: boolean;
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
    FGroupIndexMap: array of integer;
    FDragTab: integer;
    FNoteSelecting: boolean;
    FNoteLastIndex, FNoteSelStart, FNoteSelLength: integer;
    FKeyPressed: TUTF8Char;
    FLastTabMouseX: integer;
    FLoadedSelectedTab, FLoadedSelectedRow: integer;
    FLoadedSelection: TRect;
    FLoadedMemoNoteScroll, FLoadedMemoNoteSelStart, FLoadedMemoNoteSelLength: integer;
    FMemoSelStartClicked: integer;
    procedure EditControlSetBounds(Sender: TWinControl; aCol, aRow: integer; OffsetLeft: integer = 4;
      OffsetTop: integer = 0; OffsetRight: integer = -8; OffsetBottom: integer = 0);
    procedure PrinterPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    procedure PrinterGetCellText(Sender: TObject; AGrid: TCustomGrid; ACol, ARow: integer; var AText: string);
    function FindGroupTabIndex(Value: integer): integer;
    function FindGroupRealIndex(Value: integer): integer;
    function GetLineAtEnd: integer;
    function GetLineAtPos(Y: integer): integer;
    procedure PasteWithLineEnding(AMemo: TMemo);
    procedure SelectMemoLine(LineIndex: integer; Move: boolean = False);
    procedure SetMemoFocusDelayed(Data: PtrInt);
    procedure PanelMemoEnter(Sender: TObject);
    procedure PanelMemoUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure MemoEnter(Sender: TObject);
    procedure MemoChange(Sender: TObject);
    procedure MemoKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure MemoKeyPress(Sender: TObject; var Key: char);
    procedure DatePickerEnter(Sender: TObject);
    procedure DatePickerChange(Sender: TObject);
    procedure DatePickerKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure SetChanged(aChanged: boolean = True);
    procedure EditCell(aCol: integer = -1; aRow: integer = -1);
    procedure EditComplite(aEnter: boolean = False; aEscape: boolean = False);
    procedure DisableDrag;
    procedure SetInfo;
    procedure SetNote;
    procedure SetTabs(Change: boolean = True);
    procedure SetCaption;
    procedure ClearSelected(ShowConfirm: boolean = True);
    procedure MergeTasks;
    procedure DuplicateTasks;
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
    procedure MemoNoteSetScrollPosition(Value: integer);
    procedure MemoNoteBackup;
    procedure MemoNoteUndo;
    procedure MemoNoteIndent;
    procedure MemoNoteOutdent;
    procedure MemoNoteToggleComment(aComment: string);
    procedure MemoBackup;
    procedure MemoUndo;
    function IsExecuteValueNote(memoPriority: boolean = False): boolean;
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
    procedure BackupSelectedState;
    procedure RestoreSelectedState;
    function LastRowHeight(aRow: integer): integer;
    function GetScrollPosition: integer;
    function GetIsEditing: boolean;
    function IsCanClose: boolean;
    function GetSelectedTab: integer;
    function GetSelectedRow: integer;
    function GetSelection: TRect;
    function GetSelectedRows: TIntegerArray;
    function GetMemoNoteScroll: integer;
    function GetMemoNoteSelStart: integer;
    function GetMemoNoteSelLength: integer;
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
    procedure NewFile(SaveSetting: boolean = True);
    function OpenFile(fileName: string): boolean;
    function SaveFile(fileName: string = string.Empty): boolean;
    function SaveFileAs: boolean;
    procedure ApplyGridSettings;
    function Find(aText: string; aMatchCase, aWrapAround, aDirectionDown: boolean; Silent: boolean = False): boolean; overload;
    function Find(aText: string; aMatchCase, aWrapAround, aDirectionDown: boolean; out aRowsChanged: integer; Silent: boolean): boolean;
      overload;
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
    property SelectedTab: integer read GetSelectedTab write FLoadedSelectedTab;
    property SelectedRow: integer read GetSelectedRow write FLoadedSelectedRow;
    property SelectedRows: TIntegerArray read GetSelectedRows write FLastRowMem;
    property Selection: TRect read GetSelection write FLoadedSelection;
    property MemoNoteScroll: integer read GetMemoNoteScroll write FLoadedMemoNoteScroll;
    property MemoNoteSelStart: integer read GetMemoNoteSelStart write FLoadedMemoNoteSelStart;
    property MemoNoteSelLength: integer read GetMemoNoteSelLength write FLoadedMemoNoteSelLength;
  end;

var
  formNotetask: TformNotetask;
  Tasks: TTasks; // Tasks collection
  ResourceBitmapCheck: TBitmap;
  ResourceBitmapUncheck: TBitmap;
  ResourceBitmapStarGold: TBitmap;
  ResourceBitmapStarGray: TBitmap;

const
  DefRowHeight = 22;
  IndentStr = '  ';
  CommentSlashStr = '//';
  CommentHashStr = '#';
  CommentStarStr = '*';
  CommentMinusStr = '--';
  CommentSemicolonStr = ';';
  CommentTwoColonStr = '::';
  CommentREMStr = 'REM ';
  CommentApostropheStr = '''';

  clRowHighlight = TColor($FFF0DC); // RGB(220,240,255)
  clRowFocused = TColor($FFdcc8); // RGB(200,220,255)
  clRowExpired = TColor($DCDCFF); // RGB(255,220,220)
  clDarkBlue = TColor($B40000); // RGB(0,0,180)
  clGray = TColor($F0F0F0); // RGB(240,240,240)

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
  rConfirmation = 'Confirmation';
  rgroup = 'Group';
  rOK = 'OK';
  rYes = '&Yes';
  rNo = '&No';

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
  FMemoSelStartClicked := -1;
  FSortOrder := soAscending;
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
  aPageProperties.Visible := False;
  aPageProperties.Enabled := False;
  {$ENDIF}

  // Check if a command line argument is passed
  FileOpened := False;
  if ParamCount > 0 then
  begin
    FilePath := ParamStr(1); // Get the file path
    if (not FilePath.StartsWith('--')) then
      FileOpened := OpenFile(FilePath); // Function to load a task from the file
  end;

  if not FileOpened then NewFile(False);
end;

procedure TformNotetask.FormDestroy(Sender: TObject);
begin
  SaveFormSettings(Self);
  SaveGridSettings(Self, taskGrid, ExtractFileName(FFileName));

  // Free allocated resources
  //FLineEnding.Free;
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
  if (ssCtrl in Shift) and (ssShift in Shift) and (Key = VK_D) then // Ctrl + Shift + D
  begin
    aDuplicateGroup.Execute;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_DELETE) then // Ctrl + Del
  begin
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
    if (IsEditing) and (Memo.Focused) then
      Memo.SelText := IndentStr
    else
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

  RestoreSelectedState;
end;

procedure TformNotetask.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := IsCanClose;
end;

procedure TformNotetask.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  if Length(FileNames) > 0 then
  begin
    if IsCanClose then
      OpenFile(FileNames[0]);
  end;
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
  EditControlSetBounds(PanelMemo, taskGrid.Col, taskGrid.Row);
  EditControlSetBounds(DatePicker, taskGrid.Col, taskGrid.Row, 2, -2, -2, 0);
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
  if Assigned(taskGrid.Editor) and (taskGrid.Editor is TPanel) then
    TPanel(taskGrid.Editor).SetBounds(Rect.Left + 5, Rect.Top + 1, Rect.Right - Rect.Left - 10, Rect.Bottom - Rect.Top - 3);
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
    if (gdFocused in aState) and (taskGrid.Selection.Height = 0) and (taskGrid.Selection.Width = 0) and
      ((IsEditing and ((Assigned(TaskGrid.Editor) and taskGrid.Editor.Focused) or (Assigned(Memo) and Memo.Focused))) or
      (not IsEditing)) then
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
    PanelMemo := TPanel.Create(Self);
    PanelMemo.Parent := taskGrid;
    PanelMemo.BorderStyle := bsNone;
    PanelMemo.Caption := string.Empty;
    PanelMemo.BevelOuter := bvNone;
    PanelMemo.TabStop := False;
    PanelMemo.Visible := False;
    PanelMemo.OnEnter := @PanelMemoEnter; // Event Enter
    PanelMemo.OnUTF8KeyPress := @PanelMemoUTF8KeyPress; // Event UTF8KeyPress
    Memo := TMemo.Create(Self);
    Memo.Parent := PanelMemo;
    Memo.Align := alClient;
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
    EditControlSetBounds(PanelMemo, aCol, aRow);
    Memo.PopupMenu := PopupMemo;
    Memo.OnEnter := @MemoEnter; // Event Enter
    Memo.OnChange := @MemoChange; // Event Change
    Memo.OnKeyDown := @MemoKeyDown; // Event KeyDown
    Memo.OnMouseDown := @memoNoteMouseDown; // Event MouseDown
    Memo.OnDblClick := @memoNoteDblClick; // Event MouseDown
    if (aCol = 4) then
      Memo.OnKeyPress := @MemoKeyPress; // Event KeyPress for amount column only
    Memo.Text := taskGrid.Cells[aCol, aRow];
    Memo.SelStart := Length(Memo.Text);
    Memo.SelLength := 0;
    MemoBackup;

    Editor := PanelMemo;

    if (FIsSelecting) or (taskGrid.Selection.Height > 0) or (taskGrid.Selection.Width > 0) then
    begin
      PanelMemo.Visible := False;
      FIsSelecting := False;
      FIsEditing := False;
    end
    else
    begin
      PanelMemo.Visible := True;
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

    EditControlSetBounds(DatePicker, aCol, aRow, 2, -2, -2, 0);

    if (taskGrid.Cells[aCol, aRow] = string.Empty) then
      DatePicker.DateTime := Now
    else
    begin
      TryStrToDateTime(taskGrid.Cells[aCol, aRow], sDateTime);
      DatePicker.DateTime := sDateTime;
    end;

    DatePicker.OnChange := @DatePickerChange; // Event Change
    DatePicker.OnEnter := @DatePickerEnter; // Event Enter
    DatePicker.OnKeyDown := @DatePickerKeyDown; // Event KeyDown

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

  if (aRow <> FLastRow) or (taskGrid.Selection.Height <> FLastSelectionHeight) then
    SetNote;
  FLastRow := aRow;

  // Disable merge if no multiselect
  if (taskGrid.Selection.Height > 0) then
    aMergeTasks.Enabled := True
  else
    aMergeTasks.Enabled := False;

  FLastSelectionHeight := taskGrid.Selection.Height;
end;

procedure TformNotetask.groupTabsChange(Sender: TObject);
begin
  if FIsEditing then EditComplite;

  if (Length(FLastRowMem) > Tasks.SelectedGroup) then
    FLastRowMem[Tasks.SelectedGroup] := taskGrid.Row;

  Tasks.ChangeGroup(FindGroupRealIndex(groupTabs.TabIndex), True);

  if (groupTabs.TabIndex >= 0) then
    Tasks.ChangeGroup(FindGroupRealIndex(groupTabs.TabIndex), True);

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
    begin
      FDragTab := groupTabs.TabIndex;
      FLastTabMouseX := 0;
    end;
end;

procedure TformNotetask.groupTabsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  target: integer;
begin
  if FDragTab >= 0 then
  begin
    target := groupTabs.IndexOfTabAt(X, Y);
    if target >= 0 then
    begin
      if (FLastTabMouseX <> X) and (FLastTabMouseX > 0) then  Screen.Cursor := crDrag;
      if (target > FDragTab) and (FLastTabMouseX < X) then
        MoveTabRight(groupTabs.TabIndex)
      else
      if (target < FDragTab) and (FLastTabMouseX > X) then
        MoveTabLeft(groupTabs.TabIndex);
    end;
  end;
  FLastTabMouseX := X;
end;

procedure TformNotetask.groupTabsMouseLeave(Sender: TObject);
begin
  FLastTabMouseX := 0;
  DisableDrag;
end;

procedure TformNotetask.groupTabsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  TabIndex: integer;
begin
  FLastTabMouseX := 0;
  DisableDrag;

  if Button = mbRight then
  begin
    TabIndex := groupTabs.IndexOfTabAt(X, Y);
    if TabIndex <> -1 then
      groupTabs.TabIndex := TabIndex;
    PopupTabs.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

procedure TformNotetask.panelNoteMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbLeft) or (Button = mbRight) then
  begin
    if not memoNote.Focused then memoNote.SetFocus;
    FNoteSelecting := True;
    SelectMemoLine(GetLineAtPos(Y));

    if (Button = mbRight) then
      PopupMemo.PopUp;
  end;
end;

procedure TformNotetask.panelNoteMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  Index: integer;
  IndexEnd: integer;
begin
  if (FNoteSelecting) then
  begin
    Index := Self.GetLineAtPos(Y);
    IndexEnd := Self.GetLineAtEnd;
    if (Index <> FNoteLastIndex) then
    begin
      if (Index >= 0) and (Index <= IndexEnd) then
      begin
        SelectMemoLine(Index, True);
        FNoteLastIndex := Index;
      end;

      // Scroll
      //if (Index < 0) then
      //begin
      //  {$IFDEF UNIX}
      //  if (memoNote.VertScrollBar.Position > 0) then
      //  begin
      //    Application.ProcessMessages;
      //    memoNote.VertScrollBar.Position := memoNote.VertScrollBar.Position + Canvas.TextHeight('Th');
      //  end;
      //  {$ELSE}
      //  if (memoNote.VertScrollBar.Position > 0) then
      //    memoNote.VertScrollBar.Position := memoNote.VertScrollBar.Position - 1;
      //  {$ENDIF}
      //end
      //else
      //if (Index > IndexEnd) then
      //begin
      //  {$IFDEF UNIX}
      //  Application.ProcessMessages;
      //  memoNote.VertScrollBar.Position := memoNote.VertScrollBar.Position + Canvas.TextHeight('Th');
      //  {$ELSE}
      //  memoNote.VertScrollBar.Position := memoNote.VertScrollBar.Position + 1;
      //  {$ENDIF}
      //end;
    end;
  end;

end;

procedure TformNotetask.panelNoteMouseEnter(Sender: TObject);
begin
  panelNote.Color := $E3E3E3;
end;

procedure TformNotetask.panelNoteMouseLeave(Sender: TObject);
begin
  FNoteSelecting := False;
  panelNote.Color := $00F5F5F5;
end;

procedure TformNotetask.panelNoteMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FNoteSelecting := False;
end;

procedure TformNotetask.aNewExecute(Sender: TObject);
begin
  NewFile;
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
  if memoNote.Focused then
  begin
    MemoNoteUndo;
    exit;
  end;
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
  if (taskGrid.InplaceEditor.InheritsFrom(TPanel)) then
    MemoUndo; //(taskGrid.InplaceEditor as TCustomEdit).Undo;
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
      BackupSelectedState;
      Tasks.UndoBackupInit;
      FillGrid;
      ResetRowHeight;
      SetInfo;
      SetNote;
      SetTabs;
      GridClearSelection;
      Tasks.CreateBackup;
      SetChanged(False);
      RestoreSelectedState;
    end;
  end;
end;

procedure TformNotetask.aCutExecute(Sender: TObject);
begin
  if memoNote.Focused then
  begin
    MemoNoteBackup;
    memoNote.CutToClipboard;
    exit;
  end;

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
  if (taskGrid.InplaceEditor.InheritsFrom(TPanel)) then
  begin
    MemoBackup;
    Memo.CutToClipboard;
  end;
end;

procedure TformNotetask.aCopyExecute(Sender: TObject);
begin
  if memoNote.Focused then
  begin
    memoNote.CopyToClipboard;
    exit;
  end;

  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;

  if not IsEditing then
    Tasks.CopyToClipboard(taskGrid, FShowNote)
  else
  if (taskGrid.InplaceEditor.InheritsFrom(TPanel)) then
    Memo.CopyToClipboard;
end;

procedure TformNotetask.aPasteExecute(Sender: TObject);
var
  Sel: TGridRect;
begin
  if memoNote.Focused then
  begin
    if (not memoNote.ReadOnly) then
    begin
      MemoNoteBackup;
      PasteWithLineEnding(memoNote);
    end;
    exit;
  end;

  if Screen.ActiveForm <> Self then exit;

  if not IsEditing then
  begin
    Sel := Tasks.PasteFromClipboard(taskGrid, SortOrder);
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
  if (taskGrid.InplaceEditor.InheritsFrom(TPanel)) then
  begin
    MemoBackup;
    PasteWithLineEnding(Memo);
  end;
end;

procedure TformNotetask.aDeleteExecute(Sender: TObject);
begin
  if memoNote.Focused then
  begin
    if (not memoNote.ReadOnly) then
    begin
      MemoNoteBackup;
      memoNote.ClearSelection;
    end;
    exit;
  end;

  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;

  if not IsEditing then
  begin
    ClearSelected(False);
    if ShowDuration then FillGrid;
    SetInfo;
  end
  else
  if (taskGrid.InplaceEditor is TPanel) then
    with Memo do
    begin
      if SelLength = 0 then
      begin
        SelStart := SelStart;
        SelLength := 1;
      end
      else
        MemoBackup;
      ClearSelection;
    end;
end;

procedure TformNotetask.aSelectAllExecute(Sender: TObject);
begin
  if memoNote.Focused then
  begin
    memoNote.SelectAll;
    exit;
  end;

  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;

  if not IsEditing then
  begin
    taskGrid.Selection := TGridRect.Create(0, 0, 6, taskGrid.RowCount);
    FLastSelectionHeight := taskGrid.Selection.Height;
    SetInfo;
    SetNote;
  end
  else
  if (taskGrid.InplaceEditor.InheritsFrom(TPanel)) then
    Memo.SelectAll;
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

  if (taskGrid.CanFocus) then taskGrid.SetFocus;
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

procedure TformNotetask.aDuplicateTasksExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;

  DuplicateTasks;
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

  newRow := Tasks.MoveGroupTasks(taskGrid.Selection.Top, taskGrid.Selection.Bottom, Tasks.GetLeftGroup(
    Tasks.SelectedGroup, FShowArchived));

  if (newRow > -1) then
  begin
    ChangeGroup(FindGroupTabIndex(Tasks.SelectedGroup));
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

  newRow := Tasks.MoveGroupTasks(taskGrid.Selection.Top, taskGrid.Selection.Bottom, Tasks.GetRightGroup(
    Tasks.SelectedGroup, FShowArchived));

  if (newRow > -1) then
  begin
    ChangeGroup(FindGroupTabIndex(Tasks.SelectedGroup));
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
  EditComplite;
  IndentTasks;
end;

procedure TformNotetask.aOutdentTasksExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;
  EditComplite;
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

  procedure InsertDateToCell(ACol, ARow: integer);
  begin
    if (taskGrid.RowCount > 1) then
    begin
      if (taskGrid.Cells[ACol, ARow].Trim = string.Empty) or (ACol = 5) then
        taskGrid.Cells[ACol, ARow] := CurrentDateTime
      else
        taskGrid.Cells[ACol, ARow] := taskGrid.Cells[ACol, ARow].Trim + ' ' + CurrentDateTime;
      Tasks.SetTask(taskGrid, ARow, False, FShowTime);
      if Assigned(DatePicker) then
        DatePicker.DateTime := Now;
      if (FShowDuration) and (ACol = 5) then
        FillGrid;
    end
    else
    begin
      Tasks.InsertTask('- [ ] ' + CurrentDateTimeISO + ',', ARow);
      FillGrid;
      taskGrid.Row := taskGrid.Row + 1;
    end;
    SetChanged;
    SetInfo;
  end;

var
  c, r: integer;
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
    Tasks.CreateBackup;
    for r := taskGrid.Selection.Top to taskGrid.Selection.Bottom do
      for c := taskGrid.Selection.Left to taskGrid.Selection.Right do
        if (c > 0) then
          InsertDateToCell(c, r);
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
        ChangeGroup(FindGroupTabIndex(Result));
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

procedure TformNotetask.aDuplicateGroupExecute(Sender: TObject);
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
      if (Tasks.CopyGroup(FindGroupRealIndex(groupTabs.TabIndex), editText.Text)) then
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
    if (Tasks.DeleteGroup(FindGroupRealIndex(groupTabs.TabIndex))) then
    begin
      SetTabs;
      ChangeGroup(FindGroupTabIndex(Tasks.SelectedGroup));
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
  SetInfo;
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

procedure TformNotetask.memoNoteEnter(Sender: TObject);
begin
  {$IFDEF UNIX}
     aDelete.ShortCut:=0;
  {$ELSE}
  ; // NOP
  {$ENDIF}
end;

procedure TformNotetask.memoNoteExit(Sender: TObject);
begin
  {$IFDEF UNIX}
     aDelete.ShortCut:=VK_DELETE;
  {$ELSE}
  ; // NOP
  {$ENDIF}
end;

procedure TformNotetask.memoNoteKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  LinesPerPage, NewPos: integer;
  {$IFDEF UNIX}
  {$ELSE}
  Len, DeleteCount, Pos: integer;
  {$ENDIF}
begin
  // Test for letter, number, space, back, enter, shift or delete key for backup
  if (Shift * [ssCtrl, ssAlt] = []) and ((not IsSystemKey(Key)) or (Key = VK_SPACE) or (Key = VK_BACK) or
    (Key = VK_RETURN) or (ssShift in Shift) or ((Key = VK_DELETE) and (memoNote.SelLength = 0))) then
  begin
    if (not FMemoNoteFirstKey) then
    begin
      FMemoNoteFirstKey := True;
      MemoNoteBackup;
    end;
  end
  else
    FMemoNoteFirstKey := False;

  if (not (ssShift in Shift)) and (Key = VK_PRIOR) then
  begin
    LinesPerPage := memoNote.ClientHeight div Canvas.TextHeight('Wg');
    NewPos := Max(0, memoNote.CaretPos.Y - LinesPerPage);
    if (NewPos = 0) then
      memoNote.SelStart := 0
    else
      memoNote.CaretPos := Point(0, NewPos);
    MemoNote.VertScrollBar.Position := memoNote.CaretPos.Y - (LinesPerPage div 2);
    memoNote.Invalidate;
    Key := 0;
  end
  else
  if (not (ssShift in Shift)) and (Key = VK_NEXT) then
  begin
    LinesPerPage := memoNote.ClientHeight div Canvas.TextHeight('Wg');
    NewPos := Min(memoNote.Lines.Count - 1, memoNote.CaretPos.Y + LinesPerPage);
    if NewPos >= memoNote.Lines.Count - 1 then
      memoNote.SelStart := memoNote.GetTextLen - Length(unicodestring(memoNote.Lines[memoNote.Lines.Count - 1]))
    else
      memoNote.CaretPos := Point(0, NewPos);
    MemoNote.VertScrollBar.Position := memoNote.CaretPos.Y - (LinesPerPage div 2);
    memoNote.Invalidate;
    key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_C) then // Ctrl + C
  begin
    MemoNote.CopyToClipboard;
    Key := 0;
  end
  else
  if (Shift = [ssCtrl]) and (Key = VK_A) then // Ctrl + A
  begin
    memoNote.SelectAll;
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
    SelectNext(ActiveControl, True, True);
    Key := 0;
  end
  else
  if memoNote.ReadOnly then exit
  else
  if Key = VK_DELETE then // Delete
  begin
    {$IFDEF UNIX}
    if memoNote.SelLength > 0 then
        MemoNoteBackup;
    {$ELSE}
    if memoNote.SelLength = 0 then
    begin
      Len := memoNote.GetTextLen;
      DeleteCount := 0;
      Pos := memoNote.SelStart + 1; // 1-based indexing

      while Pos <= Len do
      begin
        if IsUTF8Char(memoNote.Text, Pos, ' ') then
        begin
          // If space, extend deletion
          Inc(DeleteCount);
          Inc(Pos);
        end
        else if (IsUTF8Char(memoNote.Text, Pos, #13)) or (IsUTF8Char(memoNote.Text, Pos, #10)) then
        begin
          // If CR, delete it and check for following LF
          Inc(DeleteCount);
          Inc(Pos);
          if (Pos <= Len) and (IsUTF8Char(memoNote.Text, Pos, #10)) then
          begin
            Inc(DeleteCount);
            Inc(Pos);
          end;
          Break; // stop loop after CR (and optional LF)
        end
        else
        begin
          DeleteCount := 1;
          Break; // any other char, stop loop
        end;
      end;
      memoNote.SelLength := DeleteCount;
    end
    else
      MemoNoteBackup;

    MemoNote.ClearSelection;
    Key := 0;
    {$ENDIF}
  end
  else
  if (Key = VK_BACK) then
  begin
    if memoNote.SelLength > 0 then
    begin
      MemoNoteBackup;
    end;
  end
  else
  if (ssShift in Shift) and (Key = VK_TAB) then // Shift + Tab
  begin
    MemoNoteOutdent;
    Key := 0;
  end
  else
  if (Key = VK_TAB) then // Tab
  begin
    MemoNoteIndent;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_OEM_2) then // Ctrl + /
  begin
    MemoNoteToggleComment(CommentSlashStr);
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_OEM_MINUS) then // Ctrl + -
  begin
    MemoNoteToggleComment(CommentMinusStr);
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_3) then // Ctrl + #
  begin
    MemoNoteToggleComment(CommentHashStr);
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_8) then // Ctrl + *
  begin
    MemoNoteToggleComment(CommentStarStr);
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_OEM_1) then // Ctrl + :
  begin
    MemoNoteToggleComment(CommentREMStr);
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_4) then // Ctrl + 4
  begin
    MemoNoteToggleComment(CommentSemicolonStr);
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_6) then // Ctrl + 6
  begin
    MemoNoteToggleComment(CommentTwocolonStr);
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_OEM_7) then // Ctrl + '
  begin
    MemoNoteToggleComment(CommentApostropheStr);
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (ssShift in Shift) and (Key = VK_Z) then // Ctrl + Shift + Z
  begin
    aUndoAll.Execute;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_Z) then // Ctrl + Z
  begin
    MemoNoteUndo;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_X) then // Ctrl + X
  begin
    MemoNoteBackup;
    memoNote.CutToClipboard;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_V) then // Ctrl + V
  begin
    MemoNoteBackup;
    PasteWithLineEnding(memoNote);
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

procedure TformNotetask.memoNoteMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if not (ssDouble in Shift) then
  begin
    FMemoSelStartClicked := (Sender as TMemo).SelStart;
  end;
end;

procedure TformNotetask.memoNoteDblClick(Sender: TObject);
var
  Value: unicodestring;
  pos1, leftIdx, rightIdx, len: integer;
  ch: widechar;

  function CharType(ch: widechar): integer;
  begin
    // 1 = letter or digit
    // 2 = space
    // 3 = other symbol
    if IsLetterOrDigit(ch) or (ch = '_') or (ch = '-') then
      Result := 1
    else if ch = ' ' then
      Result := 2
    else
      Result := 3;
  end;

begin
  Value := unicodestring((Sender as TMemo).Text);
  len := Length(Value);
  if len = 0 then Exit;

  if (FMemoSelStartClicked >= 0) then
    pos1 := FMemoSelStartClicked
  else
    pos1 := (Sender as TMemo).SelStart + 1;

  if pos1 < 1 then pos1 := 1;
  if pos1 > len then pos1 := len;

  ch := Value[pos1];
  leftIdx := pos1;
  rightIdx := pos1 + 1;

  case CharType(ch) of
    1: // word (letters/digits/_/-)
    begin
      while (leftIdx > 1) and (CharType(Value[leftIdx - 1]) = 1) do Dec(leftIdx);
      while (rightIdx <= len) and (CharType(Value[rightIdx]) = 1) do Inc(rightIdx);

      // extend with dot-joined tokens (.exe, .tar.gz etc.)
      while (leftIdx > 2) and (Value[leftIdx - 1] = '.') and (CharType(Value[leftIdx - 2]) = 1) do
      begin
        Dec(leftIdx); // include dot
        while (leftIdx > 1) and (CharType(Value[leftIdx - 1]) = 1) do Dec(leftIdx);
      end;
      while (rightIdx < len) and (Value[rightIdx] = '.') and (CharType(Value[rightIdx + 1]) = 1) do
      begin
        Inc(rightIdx); // include dot
        while (rightIdx <= len) and (CharType(Value[rightIdx]) = 1) do Inc(rightIdx);
      end;
    end;

    2: // spaces
    begin
      while (leftIdx > 1) and (Value[leftIdx - 1] = ' ') do Dec(leftIdx);
      while (rightIdx <= len) and (Value[rightIdx] = ' ') do Inc(rightIdx);
    end;

    3: // other symbols
    begin
      while (leftIdx > 1) and (Value[leftIdx - 1] = ch) do Dec(leftIdx);
      while (rightIdx <= len) and (Value[rightIdx] = ch) do Inc(rightIdx);
    end;
    else
      ; // NOP
  end;

  // Apply selection
  (Sender as TMemo).SelStart := leftIdx - 1;
  (Sender as TMemo).SelLength := rightIdx - leftIdx;
  FMemoSelStartClicked := -1;
end;

procedure TformNotetask.NewFile(SaveSetting: boolean = True);
var
  new: TStringList;
begin
  if IsCanClose then
  begin
    EditComplite;

    // Save settings for current file
    if SaveSetting then
      SaveGridSettings(Self, taskGrid, ExtractFileName(FFileName));

    if Assigned(Tasks) then
      Tasks.Free;

    new := TStringList.Create;
    new.Add('[ ]');
    Tasks := TTasks.Create(new);
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

    // Load saved settings for new file
    LoadGridSettings(Self, taskGrid, string.Empty);
    ApplyGridSettings;
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

  ApplyGridSettings;

  Result := True;
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

function TformNotetask.SaveFileAs: boolean;
begin
  if (saveDialog.Execute) then
  begin
    Result := SaveFile(saveDialog.FileName);
  end
  else
    Result := False;
end;

procedure TformNotetask.ApplyGridSettings;
begin
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
  groupTabs.TabIndex := 0;
  ResetRowHeight;
  SetInfo;
  SetNote;
  SetTabs;
end;

function TformNotetask.IsExecuteValueNote(memoPriority: boolean = False): boolean;
begin
  Result := (((taskGrid.Selection.Left = 3) and (taskGrid.Selection.Right >= 3)) or ((panelNote.Visible) and
    ((memoPriority) or (memoNote.SelLength > 0) or (memoNote.Focused)))) and (panelNote.Visible) and (memoNote.SelLength > 0);
end;

function TformNotetask.GetExecuteValue(aRow: integer; memoPriority: boolean = False): string;
begin
  // If note column is selected or note panel visible
  if (((taskGrid.Selection.Left = 3) and (taskGrid.Selection.Right >= 3)) or ((panelNote.Visible) and
    ((memoPriority) or (memoNote.SelLength > 0) or (memoNote.Focused)))) then
  begin
    if (panelNote.Visible) and (memoNote.SelLength > 0) then
      Result := memoNote.SelText
    else
    begin
      if (PanelMemo.Visible) and (Memo.SelLength > 0) then
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
    if (PanelMemo.Visible) and (Memo.SelLength > 0) then
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
  TempFile: string;
  Value, EncodedValue, ConsoleEncoding: string;
  ScriptEncoding: TEncoding;
  Overflow: boolean;
  maxPreview: integer;
  i: integer;
  {$IFNDEF UNIX}
  PwshPath: string;
  {$ENDIF}

  procedure AddLine(index: integer);
  var
    k: integer;
  begin
    Value := GetExecuteValue(index, True);
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
      AddLine(i);
      if (IsExecuteValueNote(True)) then break;
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
      [mbYes, mbNo], 0, mbYes) <> mrYes) then
      exit;
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

procedure TformNotetask.aSaveNotesAsExecute(Sender: TObject);
var
  notes: TStringList;
  fileName: string;
  i: integer;
const
  MAX_FILE_NAME_LEN = 50;
begin
  notes := TStringList.Create;
  try
    fileName := string.Empty;
    notes.LineBreak := FLineEnding.Value;
    notes.Options := notes.Options - [soTrailingLineBreak];


    if taskGrid.Selection.Height > 0 then
    begin
      // Multiple rows selected — concatenate notes
      for i := taskGrid.Selection.Top to taskGrid.Selection.Bottom do
        if Tasks.Map(i) > -1 then
        begin
          notes.Add(Tasks.GetTask(i).Note);
          if (i = taskGrid.Selection.Top) then
            fileName := Tasks.GetTask(i).Text;
        end;
    end
    else if Tasks.Map(taskGrid.Row) > -1 then
    begin
      // Single row selected
      notes.Add(Tasks.GetTask(taskGrid.Row).Note);
      fileName += Tasks.GetTask(taskGrid.Row).Text;
    end;

    // limit file name length
    if Length(fileName) > MAX_FILE_NAME_LEN then
      fileName := Copy(fileName, 1, MAX_FILE_NAME_LEN);

    // sanitize forbidden characters
    fileName := StringReplace(fileName, '\', '_', [rfReplaceAll]);
    fileName := StringReplace(fileName, '/', '_', [rfReplaceAll]);
    fileName := StringReplace(fileName, ':', '_', [rfReplaceAll]);
    fileName := StringReplace(fileName, '*', '_', [rfReplaceAll]);
    fileName := StringReplace(fileName, '?', '_', [rfReplaceAll]);
    fileName := StringReplace(fileName, '"', '_', [rfReplaceAll]);
    fileName := StringReplace(fileName, '<', '_', [rfReplaceAll]);
    fileName := StringReplace(fileName, '>', '_', [rfReplaceAll]);
    fileName := StringReplace(fileName, '|', '_', [rfReplaceAll]);

    saveNotesDialog.FileName := fileName;
    if (saveNotesDialog.Execute) then
      notes.SaveToFile(saveNotesDialog.FileName, FEncoding);
  finally
    notes.Free;
  end;
end;

procedure TformNotetask.MoveTabLeft(Index: integer);
var
  Result, RowMem: integer;
begin
  if (Index = 1) and (Tasks.GroupNames[0] = string.Empty) then exit;

  Result := Tasks.MoveGroupLeft(FindGroupRealIndex(Index), ShowArchived);
  RowMem := FLastRowMem[Result];
  Result := FindGroupTabIndex(Result);
  if (Result >= 0) and (Result <> Index) then
  begin
    FLastRowMem[FindGroupRealIndex(Result)] := FLastRowMem[FindGroupRealIndex(Index)];
    FLastRowMem[FindGroupRealIndex(Index)] := RowMem;
    SetTabs(False);
    if (FDragTab >= 0) then FDragTab := Result;
    ChangeGroup(Result);
    SetChanged;
  end;
end;

procedure TformNotetask.MoveTabRight(Index: integer);
var
  Result, RowMem: integer;
begin
  if (Index = 0) and (Tasks.GroupNames[0] = string.Empty) then exit;

  Result := Tasks.MoveGroupRight(FindGroupRealIndex(Index), ShowArchived);
  RowMem := FLastRowMem[Result];
  Result := FindGroupTabIndex(Result);
  if (Result >= 0) and (Result <> Index) then
  begin
    FLastRowMem[FindGroupRealIndex(Result)] := FLastRowMem[FindGroupRealIndex(Index)];
    FLastRowMem[FindGroupRealIndex(Index)] := RowMem;
    SetTabs(False);
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

function TformNotetask.FindGroupTabIndex(Value: integer): integer;
var
  i: integer;
begin
  for i := 0 to High(FGroupIndexMap) do
    if FGroupIndexMap[i] = Value then
      Exit(i);
  Result := -1;
end;

function TformNotetask.FindGroupRealIndex(Value: integer): integer;
begin
  Result := -1;
  if (Value >= 0) and (Value < Length(FGroupIndexMap)) then
    Result := FGroupIndexMap[Value];
end;

function TformNotetask.GetLineAtEnd: integer;
var
  LineHeight: integer;
  FirstVisibleLine: integer;
begin
  LineHeight := Canvas.TextHeight('Th');
  if LineHeight <= 0 then Exit(0);
  {$IFDEF UNIX}
  FirstVisibleLine := memoNote.VertScrollBar.Position div LineHeight;
  {$ELSE}
  FirstVisibleLine := memoNote.VertScrollBar.Position;
  {$ENDIF}
  Result := FirstVisibleLine + (memoNote.ClientHeight - memoNote.ClientHeight mod LineHeight) div LineHeight;

  if Result < 0 then Result := 0;
  if Result >= memoNote.Lines.Count then Result := memoNote.Lines.Count - 1;
end;

function TformNotetask.GetLineAtPos(Y: integer): integer;
var
  LineHeight: integer;
  FirstVisibleLine: integer;
  {$IFDEF UNIX}
  PixelOffset: integer;
  {$ENDIF}
begin
  LineHeight := Canvas.TextHeight('Th');
  if LineHeight <= 0 then Exit(0);
  {$IFDEF UNIX}
  FirstVisibleLine := memoNote.VertScrollBar.Position div LineHeight;
  PixelOffset := memoNote.VertScrollBar.Position mod LineHeight;
  Result := FirstVisibleLine + (Y + PixelOffset) div LineHeight;
  {$ELSE}
  FirstVisibleLine := memoNote.VertScrollBar.Position;
  Result := FirstVisibleLine + Y div LineHeight;
  {$ENDIF}

  if (Y <= 0) then Result := -1
  else
  if Result < 0 then Result := 0
  else
  if Result >= memoNote.Lines.Count then Result := memoNote.Lines.Count;
end;

procedure TformNotetask.EditCell(aCol: integer = -1; aRow: integer = -1);
var
  Value: string;
begin
  if (aCol >= 0) then
    taskGrid.Col := aCol
  else
    aCol := taskGrid.Col;
  if (aRow >= 0) then
    taskGrid.Row := aRow
  else
    aRow := taskGrid.Row;
  FIsEditing := True;
  taskGrid.EditorMode := True; //Set editing mode

  if (Assigned(PanelMemo)) and (PanelMemo.Visible) then
  begin
    EditControlSetBounds(PanelMemo, taskGrid.Col, taskGrid.Row);
    Value := Tasks.GetTaskValue(aCol, aRow);
    if (aCol <> 4) or (Value <> '0') then
      Memo.Text := Value;
    Memo.SelectAll;
    Memo.SetFocus;
    FMemoStartEdit := True;
  end;
  if (Assigned(DatePicker)) and (DatePicker.Visible) then
  begin
    EditControlSetBounds(DatePicker, taskGrid.Col, taskGrid.Row, 2, -2, -2, 0);
  end;
end;

procedure TformNotetask.EditComplite(aEnter: boolean = False; aEscape: boolean = False);
begin
  if IsEditing then
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
        if (FDatePickerDateSet) then
        begin
          DatePicker.DateTime := FDatePickerOldDate;
          DatePickerChange(DatePicker);
        end;
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
    if taskGrid.CanFocus then taskGrid.SetFocus;
  end;
end;

procedure TformNotetask.PasteWithLineEnding(AMemo: TMemo);
var
  s: string;
begin
  if Clipboard.HasFormat(CF_TEXT) then
  begin
    s := Clipboard.AsText;

    s := StringReplace(s, #13#10, #10, [rfReplaceAll]); // Windows CRLF -> LF
    s := StringReplace(s, #13, #10, [rfReplaceAll]);   // Macintosh CR -> LF

    s := StringReplace(s, #10, FLineEnding.Value, [rfReplaceAll]);
    s := StringReplace(s, #9, IndentStr, [rfReplaceAll]);

    AMemo.SelText := s;
  end;
end;

procedure TformNotetask.SelectMemoLine(LineIndex: integer; Move: boolean = False);
var
  newStart, newLength: integer;
begin
  {$IFDEF UNIX}
  memoNote.Tag := memoNote.VertScrollBar.Position;
  {$ENDIF}
  memoNote.CaretPos := Point(0, LineIndex);
  memoNote.SelLength := Length(unicodestring(memoNote.Lines[LineIndex]));

  if (not Move) then
  begin
    FNoteSelStart := memoNote.SelStart;
    FNoteSelLength := memoNote.SelLength;
  end;

  if (Move) then
  begin
    newStart := memoNote.SelStart;
    newLength := memoNote.SelLength;

    if (newStart > FNoteSelStart) then
    begin
      memoNote.SelStart := FNoteSelStart;
      memoNote.SelLength := newStart + newLength - FNoteSelStart;
    end
    else
      memoNote.SelLength := FNoteSelStart + FNoteSelLength - newStart;
  end;

  {$IFDEF UNIX}
  if (memoNote.Tag > 0) then
    MemoNoteSetScrollPosition(memoNote.Tag);
  {$ENDIF}
end;

procedure TformNotetask.SetMemoFocusDelayed(Data: PtrInt);
begin
  if Assigned(Memo) and (Memo.CanFocus) then
  begin
    Memo.SetFocus;
    if (Memo.SelLength = 0) then
      Memo.SelStart := Length(Memo.Text);
  end;
end;

procedure TformNotetask.PanelMemoEnter(Sender: TObject);
begin
  Application.QueueAsyncCall(@SetMemoFocusDelayed, 0);
end;

procedure TformNotetask.PanelMemoUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  if UTF8Key = #8 then  // backspace
    Memo.SelText := string.Empty
  else
    Memo.SelText := UTF8Key;
end;

procedure TformNotetask.taskGridUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  {$IFDEF UNIX}
  FKeyPressed := UTF8Key;
  {$ELSE}
  ; // NOP
  {$ENDIF}
end;

procedure TformNotetask.MemoEnter(Sender: TObject);
begin
  FMemoStartEdit := True;
  FMemoOldText := taskGrid.Cells[TaskGrid.Col, TaskGrid.Row];

  // If amount column selected then clean when edit
  if (FMemoNeedSelectAll) and (taskGrid.Col in [2, 3, 4]) then
    Memo.SelectAll;
  FMemoNeedSelectAll := True;

  if (FKeyPressed <> '') and (FKeyPressed <> #13) then
  begin
    Memo.SelText := FKeyPressed;
    FKeyPressed := '';
  end;

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
  EditControlSetBounds(PanelMemo, taskGrid.Col, taskGrid.Row);
  if (taskGrid.Col = 3) then
    SetNote;
  if (taskGrid.Col = 4) then
    SetInfo;
end;

procedure TformNotetask.MemoKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  nextCol: integer;
begin
  // Test for letter, number, space or back key for backup
  if (Shift * [ssCtrl, ssAlt] = []) and ((not IsSystemKey(Key)) or (Key = VK_SPACE) or (Key = VK_BACK)) then
  begin
    if (not FMemoFirstKey) then
    begin
      FMemoFirstKey := True;
      MemoBackup;
    end;
  end
  else
    FMemoFirstKey := False;

  if (Key = VK_TAB) then
  begin
    Key := 0;
    EditComplite(True);

    with taskGrid do
    begin
      nextCol := Col + 1;
      while (nextCol < ColCount) and (not Columns.Items[nextCol - 1].Visible) do
        Inc(nextCol);

      if nextCol < ColCount - 1 then
        Col := nextCol
      else
      if Row < RowCount - 1 then
      begin
        Row := Row + 1;
        nextCol := 2;
        while (nextCol < ColCount) and (not Columns[nextCol - 1].Visible) do
          Inc(nextCol);
        if nextCol < ColCount then
          Col := nextCol;
      end;
    end;
    EditCell;
  end
  else
  if (Key = VK_BACK) then
  begin
    if Memo.SelLength > 0 then
      MemoBackup;
  end;
end;

procedure TformNotetask.MemoKeyPress(Sender: TObject; var Key: char);
begin
  // Event KeyPress for Amount column only
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
  FDatePickerOldDate := Tasks.GetTask(taskGrid.Row).Date;
  FDatePickerDateSet := False;
  if (FBackup) then Tasks.CreateBackup;
  if (DatePicker.DateTime = 0) then DatePicker.DateTime := Now;
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
  FDatePickerDateSet := True;
  taskGrid.Cells[taskGrid.Col, taskGrid.Row] := DateTimeToString(TDateTimePicker(Sender).DateTime, FShowTime);
  Tasks.SetTask(taskGrid, taskGrid.Row, False, FShowTime);
  SetChanged;
  EditControlSetBounds(DatePicker, taskGrid.Col, taskGrid.Row, 2, -2, -2, 0);
  if (FShowDuration) then FillGrid;
  SetInfo;
end;

procedure TformNotetask.DatePickerKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  nextCol: integer;
begin
  if (Key = VK_TAB) then
  begin
    Key := 0;
    EditComplite(FDatePickerDateSet, not FDatePickerDateSet);

    with taskGrid do
    begin
      if Row < RowCount - 1 then
      begin
        Row := Row + 1;
        nextCol := 2;
        while (nextCol < ColCount) and (not Columns[nextCol - 1].Visible) do
          Inc(nextCol);
        if nextCol < ColCount then
          Col := nextCol;
      end;
    end;
    EditCell;
  end;
end;

procedure TformNotetask.EditControlSetBounds(Sender: TWinControl; aCol, aRow: integer; OffsetLeft: integer;
  OffsetTop: integer; OffsetRight: integer; OffsetBottom: integer);
var
  Rect: TRect;
begin
  if Assigned(Sender) then
  begin
    Rect := taskGrid.CellRect(aCol, aRow);
    Sender.SetBounds(Rect.Left + OffsetLeft, Max(Rect.Top, taskGrid.RowHeights[0]) + OffsetTop, Rect.Right - Rect.Left + OffsetRight,
      Rect.Bottom - Rect.Top + OffsetBottom);
  end;
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
      SetNote;
      SetChanged; // Mark that data has changed

      // Restore selection
      taskGrid.Row := Sel.Top;
      taskGrid.Selection := TGridRect.Create(Sel.Left, Sel.Top, Sel.Right, Sel.Top);
    end;
  end;
end;

procedure TformNotetask.DuplicateTasks;
var
  Sel, Back: TGridRect;
begin
  taskGrid.Selection := TGridRect.Create(0, taskGrid.Selection.Top, taskGrid.Columns.Count, taskGrid.Selection.Bottom);
  Tasks.CopyToClipboard(taskGrid, FShowNote);
  Back := taskGrid.Selection;
  if (SortOrder = soAscending) then
  begin
    taskGrid.Row := taskGrid.Selection.Bottom;
    taskGrid.Selection := TGridRect.Create(0, taskGrid.Selection.Bottom, taskGrid.Columns.Count, taskGrid.Selection.Bottom);
  end
  else
  begin
    taskGrid.Row := taskGrid.Selection.Top;
    taskGrid.Selection := TGridRect.Create(0, taskGrid.Selection.Top, taskGrid.Columns.Count, taskGrid.Selection.Top);
  end;
  Tasks.PasteFromClipboard(taskGrid, SortOrder);
  if (SortOrder = soAscending) then
    Sel := TGridRect.Create(0, Back.Bottom + 1, taskGrid.Columns.Count, Back.Bottom + Back.Height + 1)
  else
    Sel := TGridRect.Create(0, Back.Top, taskGrid.Columns.Count, Back.Bottom);
  FillGrid;
  if (SortColumn = 0) then
  begin
    if (SortOrder = soAscending) then
      taskGrid.Row := Sel.Top
    else
      taskGrid.Row := Sel.Bottom;
    taskGrid.Selection := Sel;
    FLastSelectionHeight := Sel.Height;
  end;
  CalcRowHeights(0, True);
  SetInfo;
  SetNote;
  SetChanged;
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

      FillGrid;
      SetInfo;
      SetNote;
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
        if (RowIndex > 0) and (RowIndex <= Tasks.Count) and (taskGrid.RowCount > RowIndex) then
        begin
          // Remove the task from the collection
          taskGrid.DeleteRow(RowIndex);
        end;
      end;
      taskGrid.ClearSelections;
      FillGrid;
      SetInfo;
      SetNote;
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
      EditComplite;
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
      EditComplite;
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
      SetTabs;
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
            taskGrid.Cells[5, RowIndex] := DateTimeToString(Now, FShowTime);
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
          taskGrid.Cells[5, RowIndex] := DateTimeToString(Now, FShowTime);
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
        taskGrid.Cells[2, RowIndex] := IndentStr + taskGrid.Cells[2, RowIndex];
        CalcRowHeights();
      end
      else
        taskGrid.Cells[2, RowIndex] := TrimLeadingSpaces(taskGrid.Cells[2, RowIndex], Length(unicodestring(IndentStr)));

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

procedure TformNotetask.MemoNoteSetScrollPosition(Value: integer);
begin
  {$IFDEF UNIX}
  memoNote.Visible := False;
  Application.ProcessMessages;
  memoNote.VertScrollBar.Position;
  memoNote.VertScrollBar.Position := Value;
  memoNote.Visible := True;
  if (memoNote.CanFocus) then memoNote.SetFocus;
  {$ELSE}
  memoNote.VertScrollBar.Position := 0;
  memoNote.VertScrollBar.Position := Value;
  {$ENDIF}
end;

procedure TformNotetask.MemoNoteBackup;
begin
  FMemoNoteBackup := memoNote.Text;
  FMemoNoteSelStartBackup := memoNote.SelStart;
  FMemoNoteSelLengthBackup := memoNote.SelLength;
  FMemoNoteCaretBackup := memoNote.CaretPos;
  FMemoNoteVertScrollBackup := memoNote.VertScrollBar.Position;
end;

procedure TformNotetask.MemoNoteUndo;
var
  newBackup: TCaption;
  SelStart, SelLength: integer;
begin
  // Save current selection and text
  newBackup := MemoNote.Text;
  SelStart := memoNote.SelStart;
  SelLength := memoNote.SelLength;

  // Restore from backup
  memoNote.Text := FMemoNoteBackup;
  memoNote.CaretPos := FMemoNoteCaretBackup;
  memoNote.SelStart := FMemoNoteSelStartBackup;
  memoNote.SelLength := FMemoNoteSelLengthBackup;

  // Adjust scroll position
  MemoNoteSetScrollPosition(FMemoNoteVertScrollBackup);

  // Option with scroll centering
  //if (FMemoNoteVertScrollBackup > 0) then
  //begin
  //  LinesPerPage := memoNote.ClientHeight div Canvas.TextHeight('Wg');
  //  memoNote.VertScrollBar.Position := memoNote.VertScrollBar.Position + LinesPerPage div 2;
  //end;

  // Update backup
  FMemoNotebackup := newBackup;
  FMemoNoteSelStartBackup := SelStart;
  FMemoNoteSelLengthBackup := SelLength;
end;

procedure TformNotetask.MemoNoteIndent;
var
  SelStartPos, SelEndPos, StartLine, EndLine, i: integer;
  Offset: integer;
begin
  MemoNoteBackup;
  if (memoNote.SelLength > 0) then
  begin
    SelStartPos := memoNote.SelStart;
    SelEndPos := SelStartPos + memoNote.SelLength;

    // Calculate start line number of selection
    memoNote.SelStart := SelStartPos;
    StartLine := memoNote.CaretPos.Y;

    // Calculate end line number of selection
    memoNote.SelStart := SelEndPos;
    EndLine := memoNote.CaretPos.Y;

    // Restore selection
    memoNote.SelStart := SelStartPos;
    memoNote.SelLength := SelEndPos - SelStartPos;

    // Add IndentStr at the start of each selected line
    for i := StartLine to EndLine do
      memoNote.Lines[i] := IndentStr + memoNote.Lines[i];

    // Adjust selection length to include inserted spaces
    Offset := Length(IndentStr) * (EndLine - StartLine + 1);
    memoNote.SelStart := SelStartPos;
    memoNote.SelLength := SelEndPos - SelStartPos + Offset;
  end
  else
    memoNote.SelText := IndentStr;
end;

procedure TformNotetask.MemoNoteOutdent;
var
  SelStartPos, SelEndPos, StartLine, EndLine, i: integer;
  Offset: integer;
  line: string;
begin
  MemoNoteBackup;
  SelStartPos := memoNote.SelStart;
  SelEndPos := SelStartPos + memoNote.SelLength;

  // Calculate start line number of selection
  memoNote.SelStart := SelStartPos;
  StartLine := memoNote.CaretPos.Y;

  // Calculate end line number of selection
  memoNote.SelStart := SelEndPos;
  EndLine := memoNote.CaretPos.Y;

  // Restore selection
  memoNote.SelStart := SelStartPos;
  memoNote.SelLength := SelEndPos - SelStartPos;

  // Remove IndentStr at the start of each selected line if present
  Offset := 0;
  for i := StartLine to EndLine do
  begin
    line := memoNote.Lines[i];
    if Length(line) >= Length(IndentStr) then
    begin
      if Copy(line, 1, Length(IndentStr)) = IndentStr then
      begin
        Delete(line, 1, Length(IndentStr));
        memoNote.Lines[i] := line;
        Offset += Length(IndentStr);
      end;
    end;
  end;

  // Adjust selection length to account for removed spaces
  memoNote.SelStart := SelStartPos;
  memoNote.SelLength := SelEndPos - SelStartPos - Offset;
end;

procedure TformNotetask.MemoNoteToggleComment(aComment: string);
var
  SelStartPos, SelEndPos, StartLine, EndLine, i: integer;
  line, trimmed, resultStr: string;
  AllCommented: boolean;
  MinIndent, CurrentIndent: integer;
  CommentOffset: integer;
  FirstCommentPos, j, wordWidth, Count: integer;
begin
  MemoNoteBackup;

  // If no selection and the cursor is on an empty line -> insert a line of the comment character
  if (memoNote.SelLength = 0) then
  begin
    line := Trim(memoNote.Lines[memoNote.CaretPos.Y]);
    if line = '' then
    begin
      // Create a string of the comment character, approximate length to fit the editor width
      // Calculate how many times we can repeat the full word
      wordWidth := Canvas.TextWidth(aComment);
      if wordWidth > 0 then
        Count := Min(memoNote.ClientWidth, 800) div wordWidth
      else
        Count := 60; // fallback value

      // Build the repeated string
      resultStr := '';
      for j := 1 to Count do
        resultStr := resultStr + aComment;

      memoNote.Lines[memoNote.CaretPos.Y] := resultStr;
      Exit; // Stop method execution, nothing else to do
    end;
  end;

  FirstCommentPos := -1;
  SelStartPos := memoNote.SelStart;
  SelEndPos := SelStartPos + memoNote.SelLength;

  // Calculate start and end lines of selection
  memoNote.SelStart := SelStartPos;
  StartLine := memoNote.CaretPos.Y;

  memoNote.SelStart := SelEndPos;
  EndLine := memoNote.CaretPos.Y;

  // Restore selection
  memoNote.SelStart := SelStartPos;
  memoNote.SelLength := SelEndPos - SelStartPos;

  // Find minimum IndentStr among non-empty lines
  MinIndent := MaxInt;
  for i := StartLine to EndLine do
  begin
    line := memoNote.Lines[i];
    trimmed := TrimLeft(line);
    if trimmed <> '' then
    begin
      CurrentIndent := Length(line) - Length(trimmed);
      if CurrentIndent < MinIndent then
        MinIndent := CurrentIndent;
    end;
  end;
  if MinIndent = MaxInt then
    MinIndent := 0;

  // Determine if all non-empty lines are already commented
  AllCommented := True;
  for i := StartLine to EndLine do
  begin
    trimmed := TrimLeft(memoNote.Lines[i]);
    if (trimmed <> '') and (UpperCase(Copy(trimmed, 1, Length(aComment))) <> UpperCase(aComment)) then
    begin
      AllCommented := False;
      Break;
    end;
  end;

  CommentOffset := 0;

  // Add or remove aComment for each line
  for i := StartLine to EndLine do
  begin
    line := memoNote.Lines[i];
    trimmed := TrimLeft(line);

    if trimmed = '' then
      Continue; // skip empty lines

    if AllCommented then
    begin
      // Remove aComment, keep spaces after it
      if UpperCase(Copy(trimmed, 1, Length(aComment))) = UpperCase(aComment) then
      begin
        Delete(trimmed, 1, Length(aComment));
        memoNote.Lines[i] := StringOfChar(' ', Length(line) - Length(TrimLeft(line))) + trimmed;
        CommentOffset -= Length(aComment);
      end;
    end
    else
    begin
      // Add aComment at MinIndent, keep extra spaces
      if Length(line) > MinIndent then
        memoNote.Lines[i] := Copy(line, 1, MinIndent) + aComment + Copy(line, MinIndent + 1, MaxInt)
      else
        memoNote.Lines[i] := StringOfChar(' ', MinIndent) + aComment;
      CommentOffset += Length(aComment);

      // Calculate first comment position
      if (i = startline) then
      begin
        FirstCommentPos := 0;
        for j := 0 to i - 1 do
        begin
          FirstCommentPos := FirstCommentPos + Length(unicodestring(memoNote.Lines[j])) + 1;
          if (FLineEnding = TLineEnding.WindowsCRLF) then Inc(FirstCommentPos);
        end;
      end;
    end;
  end;

  // Restore original selection with offset
  if FirstCommentPos > -1 then
    memoNote.SelStart := FirstCommentPos
  else
    memoNote.SelStart := SelStartPos;
  memoNote.SelLength := (SelEndPos - SelStartPos) + CommentOffset;
end;

procedure TformNotetask.MemoBackup;
begin
  FMemoBackup := Memo.Text;
  FMemoSelStartBackup := Memo.SelStart;
  FMemoSelLengthBackup := Memo.SelLength;
end;

procedure TformNotetask.MemoUndo;
var
  newBackup: TCaption;
  SelStart, SelLength: integer;
begin
  newBackup := Memo.Text;
  SelStart := Memo.SelStart;
  SelLength := Memo.SelLength;
  Memo.Text := FMemoBackup;
  Memo.SelStart := FMemoSelStartBackup;
  Memo.SelLength := FMemoSelLengthBackup;
  FMemobackup := newBackup;
  FMemoSelStartBackup := SelStart;
  FMemoSelLengthBackup := SelLength;
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

procedure TformNotetask.BackupSelectedState;
begin
  FLoadedSelectedTab := groupTabs.TabIndex;
  FLoadedSelectedRow := taskGrid.Row;
  FLoadedSelection := taskGrid.Selection;
  FLoadedMemoNoteSelStart := memoNote.SelStart;
  FLoadedMemoNoteSelLength := memoNote.SelLength;
  FLoadedMemoNoteScroll := memoNote.VertScrollBar.Position;
end;

procedure TformNotetask.RestoreSelectedState;
var
  FirstTabRow, Index: integer;
begin
  // Restore last open tab and rows
  if (FLoadedSelectedTab >= 0) then
  begin
    FirstTabRow := -1;
    if (Length(FLastRowMem) > 0) then
      FirstTabRow := FLastRowMem[FindGroupRealIndex(0)];
    if (FLoadedSelectedTab > 0) then
      groupTabs.TabIndex := FLoadedSelectedTab
    else
    if (FLoadedSelectedTab = 0) and (FindGroupRealIndex(0) > 0) then
      groupTabsChange(groupTabs);

    if (FLoadedSelectedRow > 0) then
      taskGrid.Row := FLoadedSelectedRow;

    // Set current row to mem
    if (Length(FLastRowMem) > 0) then
    begin
      if (FirstTabRow >= 0) then
        FLastRowMem[FindGroupRealIndex(0)] := FirstTabRow;
      Index := FindGroupRealIndex(FLoadedSelectedTab);
      if (Index >= High(FLastRowMem)) then
        FLastRowMem[FindGroupRealIndex(FLoadedSelectedTab)] := FLoadedSelectedRow;
    end;
    FLoadedSelectedTab := -1;
  end;

  // Restore task grid selection
  if (FLoadedSelection.Left > 0) or (FLoadedSelection.Right > 0) or (FLoadedSelection.Top > 0) or (FLoadedSelection.Bottom > 0) then
  begin
    taskGrid.Col := FLoadedSelection.Left;
    taskGrid.Selection := TGridRect.Create(FLoadedSelection);
    FLoadedSelection := Rect(0, 0, 0, 0);
    SetNote;
  end;

  // Restore memo note SelStart
  if (FLoadedMemoNoteSelStart > 0) and (memoNote.Visible) then
  begin
    memoNote.SelStart := FLoadedMemoNoteSelStart;
    FLoadedMemoNoteSelStart := 0;
  end;

  // Restore memo note SelLength
  if (FLoadedMemoNoteSelLength > 0) and (memoNote.Visible) then
  begin
    if memoNote.CanFocus then memoNote.SetFocus;
    memoNote.SelLength := FLoadedMemoNoteSelLength;
    FLoadedMemoNoteSelLength := 0;
  end;

  // Restore memo note scroll position
  if (FLoadedMemoNoteScroll > 0) and (memoNote.Visible) then
  begin
    MemoNoteSetScrollPosition(FLoadedMemoNoteScroll);
    FLoadedMemoNoteScroll := 0;
  end;
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
    memoNote.Alignment := taRightJustify;
    memoNote.BorderSpacing.Left := 0;
    memoNote.BorderSpacing.Right := 10;
    SetCursorTo(panelNote, 'RIGHTARROW');
  end
  else
  begin
    taskGrid.BiDiMode := bdLeftToRight;
    groupTabs.BiDiMode := bdLeftToRight;
    for i := 1 to taskGrid.Columns.Count - 1 do
      taskGrid.Columns[i].Alignment := taLeftJustify;
    memoNote.BiDiMode := bdLeftToRight;
    memoNote.Alignment := taLeftJustify;
    memoNote.BorderSpacing.Left := 10;
    memoNote.BorderSpacing.Right := 0;
    SetCursorTo(panelNote, 'LEFTARROW');
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
  SetTabs;
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
  panelNote.Visible := FShowNote;
  Splitter.Visible := FShowNote;
  StatusBar.Top := panelNote.Top + panelNote.Height;

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

      {$IFDEF UNIX}
      Text := StringReplace(Text, #$0A, #$0A+ '+', [rfReplaceAll]);
      {$ENDIF}

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
    {$IFDEF UNIX}
    groupTabs.Height := Canvas.TextHeight('A') + 11;
    {$ELSE}
    groupTabs.Height := Canvas.TextHeight('A') + 8;
    {$ENDIF}
    CalcDefaultColWidth;
  end;

  EditControlSetBounds(PanelMemo, taskGrid.Col, taskGrid.Row);
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
var
  i: integer;
  notes: TStringList;
begin
  if (not ShowNote) then exit;

  memoNote.OnChange := nil;
  notes := TStringList.Create;
  try
    if Assigned(Tasks) and (taskGrid.RowCount > 1) then
    begin
      if taskGrid.Selection.Height > 0 then
      begin
        // Multiple rows selected — concatenate notes and set read-only
        for i := taskGrid.Selection.Top to taskGrid.Selection.Bottom do
          if Tasks.Map(i) > -1 then
            notes.Add(Tasks.GetTask(i).Note);
        memoNote.Lines.Text := notes.Text;
        memoNote.ReadOnly := True;
        memoNote.Color := clGray;
      end
      else if Tasks.Map(taskGrid.Row) > -1 then
      begin
        // Single row selected — set editable note
        memoNote.Text := Tasks.GetTask(taskGrid.Row).Note;
        memoNote.ReadOnly := False;
        memoNote.Color := clDefault;
      end
      else
        memoNote.Text := string.Empty;
    end
    else
    begin
      memoNote.Text := string.Empty;
      memoNote.ReadOnly := True;
    end;
  finally
    notes.Free;
    MemoNoteBackup;
    memoNote.OnChange := @memoNoteChange;
  end;
end;

procedure TformNotetask.SetTabs(Change: boolean = True);
var
  Clean: TStringList;
  i: integer;
  LastIndex, LastRealIndex: integer;
  FoundTab: boolean;
begin
  LastRealIndex := FindGroupRealIndex(groupTabs.TabIndex);
  SetLength(FGroupIndexMap, 0);
  Clean := TStringList.Create;
  try
    for i := 0 to Tasks.CountGroup - 1 do
    begin
      if Tasks.GroupNames[i] = string.Empty then
      begin
        Clean.Add(rgroupuntitled);
        SetLength(FGroupIndexMap, Length(FGroupIndexMap) + 1);
        FGroupIndexMap[High(FGroupIndexMap)] := i;
      end
      else
      begin
        if (ShowArchived) or (not Tasks.GetGroupArchived(i)) then
        begin
          Clean.Add(Tasks.GroupNames[i].TrimLeft([' ', '#']).Trim);
          SetLength(FGroupIndexMap, Length(FGroupIndexMap) + 1);
          FGroupIndexMap[High(FGroupIndexMap)] := i;
        end;
      end;
    end;

    groupTabs.Tabs := Clean;
    groupTabs.Visible := not ((groupTabs.Tabs.Count = 1) and (Tasks.GroupNames[0] = string.Empty));

    if (Change) and (groupTabs.Visible) and (LastRealIndex >= 0) then
    begin
      FoundTab := False;
      LastIndex := FindGroupTabIndex(LastRealIndex);
      if (LastIndex > 0) and (LastIndex < groupTabs.Tabs.Count) then
        groupTabs.TabIndex := LastIndex
      else
      if (LastIndex >= groupTabs.Tabs.Count) then
        groupTabs.TabIndex := groupTabs.Tabs.Count - 1
      else
      if (LastIndex < 0) then
      begin
        i := LastRealIndex;
        while (i < Tasks.CountGroup) do
        begin
          Inc(i);
          LastIndex := FindGroupTabIndex(i);
          if (LastIndex >= 0) and (LastIndex < groupTabs.Tabs.Count) then
          begin
            groupTabs.TabIndex := LastIndex;
            FoundTab := True;
            break;
          end;
        end;
        if (not FoundTab) then
          groupTabs.TabIndex := groupTabs.Tabs.Count - 1;
      end;

      // Change group if tab was changed
      if (LastRealIndex <> FindGroupRealIndex(groupTabs.TabIndex)) then
        groupTabsChange(groupTabs);
    end
    else
    if not groupTabs.Visible then
    begin
      groupTabs.TabIndex := 0;
      groupTabsChange(groupTabs);
    end;

    // Set selected row memory for tabs
    SetLength(FLastRowMem, Tasks.CountGroup);
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

function TformNotetask.GetSelectedTab: integer;
begin
  Result := groupTabs.TabIndex;
end;

function TformNotetask.GetSelectedRow: integer;
begin
  Result := taskGrid.Row;
end;

function TformNotetask.GetSelectedRows: TIntegerArray;
begin
  Result := FLastRowMem;
end;

function TformNotetask.GetSelection: TRect;
begin
  Result := taskGrid.Selection;
end;

function TformNotetask.GetMemoNoteScroll: integer;
begin
  Result := memoNote.VertScrollBar.Position;
end;

function TformNotetask.GetMemoNoteSelStart: integer;
begin
  Result := memoNote.SelStart;
end;

function TformNotetask.GetMemoNoteSelLength: integer;
begin
  Result := memoNote.SelLength;
end;

function TformNotetask.GetIsEditing: boolean;
begin
  Result := (taskGrid.EditorMode) or (FIsEditing);
end;

function TformNotetask.Find(aText: string; aMatchCase, aWrapAround, aDirectionDown: boolean; Silent: boolean = False): boolean;
var
  rowsChanged: integer;
begin
  Result := Find(aText, aMatchCase, aWrapAround, aDirectionDown, rowsChanged, Silent);
end;

function TformNotetask.Find(aText: string; aMatchCase, aWrapAround, aDirectionDown: boolean; out aRowsChanged: integer;
  Silent: boolean): boolean;
var
  sValue, sText: unicodestring;
  Counter, CurRow, CurCol, StartRow, StartCol: integer;
  LastDate: boolean;

  function FindMemo(Memo: TMemo): boolean;
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

  function NotFound(messageOnly: boolean = False): boolean;
  begin
    if (not messageOnly) then
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
    end;
    if (not Silent) then
      ShowMessage(rcantfind + ' "' + string(aText) + '"');
    Result := False;
  end;

begin
  if (FFindActive) or (taskGrid.RowCount = 0) then exit;
  FFindActive := True;
  aRowsChanged := 0;
  try
    FindText := aText;
    MatchCase := aMatchCase;
    WrapAround := aWrapAround;

    // Search in Note if selected
    if self.ActiveControl = memoNote then
    begin
      sValue := unicodestring(MemoNote.Text);
      sText := unicodestring(aText);
      if (Pos(UnicodeLowerCase(sText), UnicodeLowerCase(sValue)) > 0) then
      begin
        if (FindMemo(memoNote)) then
        begin
          FFoundText := aText;
          Result := True;
        end
        else
        if (WrapAround) then
        begin
          if (aDirectionDown) then
          begin
            memoNote.SelStart := 0;
            memoNote.SelLength := 0;
          end
          else
          begin
            memoNote.SelStart := Length(memoNote.Text);
            memoNote.SelLength := 0;
          end;
          FindMemo(memoNote);
          FFoundText := aText;
          Result := True;
        end
        else
          Result := NotFound(True);
      end
      else
        Result := NotFound(True);
      exit;
    end;

    StartRow := taskGrid.Row;
    StartCol := taskGrid.Col;
    LastDate := False;
    if taskGrid.Col = 1 then taskGrid.Col := 2;
    FMemoNeedSelectAll := False;
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
        Inc(aRowsChanged);
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
        Inc(aRowsChanged);
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
        if (Pos(UnicodeLowerCase(sText), UnicodeLowerCase(sValue)) > 0) and (FindMemo(Memo)) then
        begin
          FMemoNeedSelectAll := False;
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
          FMemoNeedSelectAll := False;
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
            Inc(aRowsChanged);
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
            Inc(aRowsChanged);
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
            Inc(aRowsChanged);
            CurCol := 2;
            taskGrid.Col := 2;
            Memo.SelStart := 0;
          end
          else
            // Move to begin end
          begin
            CurRow := taskGrid.RowCount - 1;
            taskGrid.Row := taskGrid.RowCount - 1;
            Inc(aRowsChanged);
            CurCol := 5;
            taskGrid.Col := 5;
          end;
          Inc(Counter);
        end
        else
          exit(NotFound);
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
      exit(NotFound);

    Result := True;
  finally
    FFindActive := False;
  end;
end;

function TformNotetask.Replace(aText, aToText: string; aMatchCase, aWrapAround: boolean): boolean;
var
  sValue, sText: unicodestring;
  Target: TMemo;

  procedure FindNextExecute;
  begin
    FFindText := aText;
    FMatchCase := aMatchCase;
    FWrapAround := aWrapAround;
    aFindNext.Execute;
  end;

begin
  if self.ActiveControl = memoNote then
    Target := memoNote
  else
    Target := Memo;

  sValue := unicodestring(Target.SelText);
  sText := unicodestring(aText);

  if (FFoundText = string.Empty) or ((aMatchCase) and (sValue <> sText)) or ((not aMatchCase) and
    (UnicodeLowerCase(sValue) <> UnicodeLowerCase(sText))) then
    FindNextExecute
  else
  begin
    if self.ActiveControl = memoNote then
      Target := memoNote
    else
      Target := Memo;

    GridBackupSelection;
    Tasks.CreateBackup;
    Target.SelText := aToText;
    FLastFoundSelLength := Length(unicodestring(aToText));
    Target.SelStart := Max(Target.SelStart - FLastFoundSelLength, 0);
    Target.SelLength := FLastFoundSelLength;

    FindNextExecute;
  end;

  Result := True;
end;

function TformNotetask.ReplaceAll(aText, aToText: string; aMatchCase, aWrapAround: boolean): boolean;
var
  sValue, sText: unicodestring;
  Target: TMemo;
  sShowNote: boolean;
  CounterRow, CounterPos: integer;
  RowsChanged, LastPos: integer;
begin
  FBackup := False;
  sShowNote := FShowNote;
  if self.ActiveControl <> memoNote then
    FShowNote := False;
  GridBackupSelection;
  Tasks.CreateBackup; // FBackup = false here
  try
    if self.ActiveControl = memoNote then
    begin
      Target := memoNote;
      MemoNoteBackup;
    end
    else
      Target := Memo;

    // Replace current selection
    sValue := unicodestring(Target.SelText);
    sText := unicodestring(aText);
    CounterRow := 0;
    CounterPos := 0;
    LastPos := 0;
    RowsChanged := 0;
    if not ((FFoundText = string.Empty) or ((aMatchCase) and (sValue <> sText)) or ((not aMatchCase) and
      (UnicodeLowerCase(sValue) <> UnicodeLowerCase(sText)))) then
      Target.SelText := aToText;

    // Replace all
    while (Find(aText, aMatchCase, aWrapAround, True, RowsChanged, True)) do
    begin
      if self.ActiveControl = memoNote then
        Target := memoNote
      else
        Target := Memo;
      Target.SelText := aToText;
      FLastFoundSelLength := Length(unicodestring(aToText));
      Target.SelStart := Max(Target.SelStart - FLastFoundSelLength, 0);
      Target.SelLength := FLastFoundSelLength;

      // Safeguard to prevent infinite loop
      if aWrapAround then
      begin
        if self.ActiveControl = memoNote then
        begin
          if (Target.SelStart > LastPos) then CounterPos += Target.SelStart - LastPos
          else
            CounterPos += Target.SelStart;
          LastPos := Target.SelStart;
          if (CounterPos > Length(unicodestring(Target.Text))) then
            break;
        end
        else
        begin
          if (RowsChanged > 0) then CounterRow += RowsChanged;
          if (CounterRow > taskGrid.RowCount - 1) then break;
        end;
      end;
    end;

    Result := True;
  finally
    FBackup := True;
    FShowNote := sShowNote;
  end;
end;

end.
