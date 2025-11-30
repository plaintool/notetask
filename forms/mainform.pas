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
  Printers,
  Process,
  Math,
  Clipbrd,
  ActnList,
  ComCtrls,
  ExtCtrls,
  Grids,
  Menus,
  Buttons,
  LCLIntf,
  LCLType,
  LConvEncoding,
  PrintersDlgs,
  DateTimePicker,
  GridPrn,
  task,
  lineending,
  formattool,
  TagEdit;

type
  { TformNotetask }
  TformNotetask = class(TForm)
    aArchiveTasks: TAction;
    aAbout: TAction;
    aCopy: TAction;
    aCheckforupdates: TAction;
    aZoomDefault: TAction;
    aZoomOut: TAction;
    aZoomIn: TAction;
    aFilter: TAction;
    aEditGroupTooltip: TAction;
    aSplitTasks: TAction;
    aHideNoteText: TAction;
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
    colorDialog: TColorDialog;
    contextAskChatGPT1: TMenuItem;
    contextCopy1: TMenuItem;
    contextZoom100: TMenuItem;
    contextCopyTag: TMenuItem;
    contextWindowsCRLF: TMenuItem;
    contextANSI: TMenuItem;
    contextCut1: TMenuItem;
    contextDelete1: TMenuItem;
    contextPaste1: TMenuItem;
    contextRunPowershell1: TMenuItem;
    contextRunTerminal1: TMenuItem;
    contextSelectAll1: TMenuItem;
    contextUndo1: TMenuItem;
    filterBox: TComboBox;
    filterClear: TSpeedButton;
    fontDialog: TFontDialog;
    groupTabs: TTabControl;
    contextColor: TMenuItem;
    contextResetColor: TMenuItem;
    contextZoom90: TMenuItem;
    contextZoom80: TMenuItem;
    contextZoom70: TMenuItem;
    contextZoom60: TMenuItem;
    contextZoom50: TMenuItem;
    contextZoom110: TMenuItem;
    contextZoom120: TMenuItem;
    contextZoom130: TMenuItem;
    contextZoom140: TMenuItem;
    contextZoom150: TMenuItem;
    menuZoomIn: TMenuItem;
    menuZoomOut: TMenuItem;
    menuDefaultZoom: TMenuItem;
    menuZoom: TMenuItem;
    MiscImages: TImageList;
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
    contextSaveNotesAs: TMenuItem;
    contextCopyStatusbar: TMenuItem;
    menuDuplicateTasks: TMenuItem;
    contextRunPowershell: TMenuItem;
    contextIndentTasks: TMenuItem;
    contextSaveNotesAs1: TMenuItem;
    contextUnixLF: TMenuItem;
    contextMacintoshCR: TMenuItem;
    contextASCII: TMenuItem;
    contextUTF8: TMenuItem;
    contextUTF8BOM: TMenuItem;
    contextUTF16BEBOM: TMenuItem;
    contextUTF16LEBOM: TMenuItem;
    menuHideNoteText: TMenuItem;
    contextSplitTasks: TMenuItem;
    menuCheckforupdates: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    menuFilter: TMenuItem;
    contextDeleteTags: TMenuItem;
    menuSplitTasks: TMenuItem;
    menuSaveNotesAs: TMenuItem;
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
    panelTabs: TPanel;
    panelNote: TPanel;
    Popup: TPopupMenu;
    PopupEncoding: TPopupMenu;
    PopupMemo: TPopupMenu;
    PopupStatusbar: TPopupMenu;
    PopupLineEnding: TPopupMenu;
    PopupZoom: TPopupMenu;
    PopupTags: TPopupMenu;
    printDialog: TPrintDialog;
    saveDialog: TSaveDialog;
    saveNotesDialog: TSaveDialog;
    panelTags: TScrollBox;
    Separator1: TMenuItem;
    menuExit: TMenuItem;
    Separator10: TMenuItem;
    Separator17: TMenuItem;
    Separator18: TMenuItem;
    Separator19: TMenuItem;
    Separator2: TMenuItem;
    Separator21: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    menuSelectAll: TMenuItem;
    Separator6: TMenuItem;
    Separator7: TMenuItem;
    Separator8: TMenuItem;
    SplitFilter: TSplitter;
    SplitTags: TSplitter;
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
    menuArabic: TMenuItem;
    menuHindi: TMenuItem;
    menuUkrainian: TMenuItem;
    menuBelarusian: TMenuItem;
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
    menuMergeTasks: TMenuItem;
    contextMergeTasks: TMenuItem;
    Splitter: TSplitter;
    aShowTags: TAction;
    menuShowTags: TMenuItem;
    aShowNote: TAction;
    menuShowNote: TMenuItem;
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
    menuDuplicateGroup: TMenuItem;
    aMoveGroupLeft: TAction;
    aMoveGroupRight: TAction;
    menuMoveGroupLeft: TMenuItem;
    Separator16: TMenuItem;
    menuMoveGroupRight: TMenuItem;
    PopupTabs: TPopupMenu;
    ContextInsertGroup: TMenuItem;
    contextInsertTask: TMenuItem;
    ContextRenameGroup: TMenuItem;
    ContextDuplicateGroup: TMenuItem;
    ContextDeleteGroup: TMenuItem;
    procedure aCheckforupdatesExecute(Sender: TObject);
    procedure aEditGroupTooltipExecute(Sender: TObject);
    procedure aFilterExecute(Sender: TObject);
    procedure aShowTagsExecute(Sender: TObject);
    procedure aSplitTasksExecute(Sender: TObject);
    procedure aZoomDefaultExecute(Sender: TObject);
    procedure aZoomInExecute(Sender: TObject);
    procedure aZoomOutExecute(Sender: TObject);
    procedure contextCopyTagsClick(Sender: TObject);
    procedure contextDeleteTagsClick(Sender: TObject);
    procedure contextColorClick(Sender: TObject);
    procedure contextResetColorClick(Sender: TObject);
    procedure contextZoom100Click(Sender: TObject);
    procedure contextZoom110Click(Sender: TObject);
    procedure contextZoom120Click(Sender: TObject);
    procedure contextZoom130Click(Sender: TObject);
    procedure contextZoom140Click(Sender: TObject);
    procedure contextZoom150Click(Sender: TObject);
    procedure contextZoom50Click(Sender: TObject);
    procedure contextZoom60Click(Sender: TObject);
    procedure contextZoom70Click(Sender: TObject);
    procedure contextZoom80Click(Sender: TObject);
    procedure contextZoom90Click(Sender: TObject);
    procedure filterBoxChange(Sender: TObject);
    procedure filterBoxKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure filterClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure ApplicationOnException(Sender: TObject; E: Exception);
    procedure ApplicationOnQueryEndSession(var CanEnd: boolean);
    procedure ApplicationOnShowHint(var HintStr: string; var CanShow: boolean; var HintInfo: THintInfo);
    procedure memoNoteDblClick(Sender: TObject);
    procedure memoNoteEnter(Sender: TObject);
    procedure memoNoteExit(Sender: TObject);
    procedure memoNoteChange(Sender: TObject);
    procedure memoNoteKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure memoNoteMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure memoNoteKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure memoNoteMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure memoNoteMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure tagsEditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure tagsEditTagClick(Sender: TObject; const TagText: string);
    procedure tagsEditBeforeChange(Sender: TObject);
    procedure tagsEditChange(Sender: TObject);
    procedure tagsEditTagAdd(Sender: TObject; const TagText: string);
    procedure tagsEditTagRemove(Sender: TObject; const TagText: string);
    procedure tagsEditTagReorder(Sender: TObject; const TagText: string; const NewIndex: integer);
    procedure tagsEditExit(Sender: TObject);
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
    procedure SplitFilterChangeBounds(Sender: TObject);
    procedure statusBarContextPopup(Sender: TObject; MousePos: TPoint; var Handled: boolean);
    procedure contextCopyStatusbarClick(Sender: TObject);
    procedure contextANSIClick(Sender: TObject);
    procedure contextASCIIClick(Sender: TObject);
    procedure contextMacintoshCRClick(Sender: TObject);
    procedure contextUnixLFClick(Sender: TObject);
    procedure contextUTF16BEBOMClick(Sender: TObject);
    procedure contextUTF16LEBOMClick(Sender: TObject);
    procedure contextUTF8BOMClick(Sender: TObject);
    procedure contextUTF8Click(Sender: TObject);
    procedure contextWindowsCRLFClick(Sender: TObject);
    procedure taskGridCheckboxToggled(Sender: TObject; aCol, aRow: integer; aState: TCheckboxState);
    procedure taskGridColRowDeleted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
    procedure taskGridColRowInserted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
    procedure taskGridDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure taskGridHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure taskGridHeaderSized(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure taskGridKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure taskGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure taskGridMouseLeave(Sender: TObject);
    procedure taskGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure taskGridMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure taskGridResize(Sender: TObject);
    procedure taskGridSelectCell(Sender: TObject; aCol, aRow: integer; var CanSelect: boolean);
    procedure taskGridSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
    procedure taskGridTopLeftChanged(Sender: TObject);
    procedure taskGridUserCheckboxBitmap(Sender: TObject; const aCol, aRow: integer; const CheckedState: TCheckboxState;
      var ABitmap: TBitmap);
    procedure taskGridColRowMoved(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
    procedure taskGridSetCheckboxState(Sender: TObject; ACol, ARow: integer; const Value: TCheckboxState);
    procedure taskGridSelection(Sender: TObject; aCol, aRow: integer);
    procedure taskGridUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure aSaveNotesAsExecute(Sender: TObject);
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
    procedure aHideNoteTextExecute(Sender: TObject);
  private
    Memo: TMemo;
    PanelMemo: TPanel;
    DatePicker: TDateTimePicker;
    tagsEdit: TTagEdit;
    FChanged: boolean;
    FBackup: boolean;
    FReadOnly: boolean;
    FOriginalFontSize: integer;
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
    FEncrypted: boolean;
    FKeyEnc: TBytes;
    FKeyAuth: TBytes;
    FSalt: TBytes;
    FEncoding: TEncoding;
    FLineEnding: TLineEnding;
    FEncodingOriginal: TEncoding;
    FLineEndingOriginal: TLineEnding;
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
    FLastRow, FLastCol: integer;
    FLastText: string;
    FLastTextMatch: boolean;
    FLastRowMem: array of integer;
    FLastTabMouseX: integer;
    FLastTabTarget: integer;
    FLastTabFilter: integer;
    FNoteLastIndex, FNoteSelStart, FNoteSelLength: integer;
    FNoteLastSelText: string;
    FNoteLastSelStart, FNoteLastSelLength: integer;
    FGroupIndexMap: array of integer;
    FDragTab: integer;
    FNoteSelecting: boolean;
    FKeyPressed: TUTF8Char;
    FLoadedSelectedTab, FLoadedSelectedRow: integer;
    FLoadedSelection: TRect;
    FLoadedRowMem: TIntegerArray;
    FLoadedMemoNoteScroll, FLoadedMemoNoteSelStart, FLoadedMemoNoteSelLength: integer;
    FMemoSelStartClicked: integer;
    FStatusPanelIndex: integer;
    FAdjustingScrollBars: boolean;
    FSReserved: TFileStream;
    FRepaint: boolean;
    FDuplicateHighlight: boolean;
    procedure EditControlSetBounds(Sender: TWinControl; aCol, aRow: integer; OffsetLeft: integer = 4;
      OffsetTop: integer = 0; OffsetRight: integer = -8; OffsetBottom: integer = 0);
    procedure UpdateComboRegion(Combo: TComboBox; AInsetLeft: integer = 1; AInsetTop: integer = 1;
      AInsetRight: integer = 0; AInsetBottom: integer = 1);
    procedure PrinterPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    procedure PrinterGetCellText(Sender: TObject; AGrid: TCustomGrid; ACol, ARow: integer; var AText: string);
    function FindGroupTabIndex(Value: integer): integer;
    function FindGroupRealIndex(Value: integer): integer;
    function GetLineAtEnd: integer;
    function GetLineAtPos(Y: integer): integer;
    procedure PasteWithLineEnding(AMemo: TMemo);
    procedure SelectMemoLine(LineIndex: integer; Move: boolean = False);
    procedure PanelMemoEnter(Sender: TObject);
    procedure PanelMemoUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure MemoEnter(Sender: TObject);
    procedure MemoChange(Sender: TObject);
    procedure MemoKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure MemoKeyPress(Sender: TObject; var Key: char);
    procedure DatePickerEnter(Sender: TObject);
    procedure DatePickerChange(Sender: TObject);
    procedure DatePickerKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure EditCell(aCol: integer = -1; aRow: integer = -1);
    procedure EditComplite(aEnter: boolean = False; aEscape: boolean = False);
    procedure DisableDrag;
    procedure DisableGridEvents;
    procedure EnableGridEvents;
    procedure SetChanged(aChanged: boolean = True);
    procedure SetCaption;
    procedure SetInfo;
    procedure SetTags;
    procedure SetNote;
    procedure SetFilter(FillTags: boolean = True);
    procedure SetTabs(Change: boolean = True);
    procedure SetTabsVisible;
    procedure ClearSelected(ShowConfirm: boolean = True);
    procedure DuplicateTasks;
    procedure MergeTasks;
    procedure SplitTasks;
    procedure DeleteTask(aRow: integer = 0; ShowConfirm: boolean = True);
    procedure DeleteTasks(ShowConfirm: boolean = True);
    procedure ArchiveTask(aRow: integer = 0);
    procedure ArchiveTasks;
    procedure CompleteTasks(aRow: integer = 0);
    procedure StarTasks(aRow: integer = 0);
    procedure IndentTasks(Outdent: boolean = False);
    procedure SetReadOnly(Value: boolean);
    procedure SetZoom(Value: float);
    procedure SetBiDiRightToLeft(Value: boolean);
    procedure SetShowStatusBar(Value: boolean);
    procedure SetShowTags(Value: boolean);
    procedure SetShowNote(Value: boolean);
    procedure SetShowDuration(Value: boolean);
    procedure SetShowTime(Value: boolean);
    procedure SetHideNoteText(Value: boolean);
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
    procedure MemoDelKey(aMemoNote: boolean = True);
    function CalcDeleteCount(const S: string; SelStart: integer): integer;
    function IsExecuteValueNote(memoPriority: boolean = False): boolean;
    function GetExecuteValue(aRow: integer; memoPriority: boolean = False): string;
    procedure ExecuteChatGpt;
    procedure TryOpenAsUrl(Value: string);
    procedure ExecuteTerminal(usePowershell: boolean = True);
    procedure MoveTabLeft(Index: integer);
    procedure MoveTabRight(Index: integer);
    procedure ChangeGroup(Index: integer);
    procedure CalcDefaultColWidth;
    procedure CalcRowHeight(aRow: integer = 0; aForce: boolean = False);
    procedure ResetRowHeight(aRow: integer = 0; aCalcRowHeight: boolean = True);
    procedure SwapRowHeights(RowIndex1, RowIndex2: integer);
    procedure BackupSelectedState(aRowMem: boolean = False);
    procedure RestoreSelectedState(aRowMem: boolean = True; aRowMemPriority: boolean = True; aFocusMemo: boolean = False);
    procedure GridAdjustScrollBars;
    procedure GridInvalidate;
    procedure TagsAdd(const Rect: TRect; const TagText: string);
    function FreeFile: boolean;
    function LastRowHeight(aRow: integer): integer;
    procedure ChangeLastText(const Value: string; aCol: integer = -1; aRow: integer = -1);
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
    procedure DrawHighlightedText(aCanvas: TCanvas; const aText, aFilterText: string; aRect: TRect; aColor: TColor);
    procedure DelayedSetMemoFocus(Data: PtrInt);
    procedure DelayedFinishTagEdit(Data: PtrInt);
    procedure DelayedInvalidate(Data: PtrInt);
  public
    FZoom: float;
    FShowArchived: boolean;
    FShowDuration: boolean;
    FShowTime: boolean;
    FShowTags: boolean;
    FShowNote: boolean;
    FHideNoteText: boolean;
    FShowStatusBar: boolean;
    FShowColumnDone: boolean;
    FShowColumnTask: boolean;
    FShowColumnNote: boolean;
    FShowColumnAmount: boolean;
    FShowColumnDate: boolean;
    FShowColumnFavorite: boolean;
    {$IFDEF UNIX}
    FWindowStateLoaded: TWindowState;
    {$ENDIF}

    procedure SetLanguage(aLanguage: string = string.Empty);
    procedure FillGrid;
    procedure NewFile(SaveSetting: boolean = True);
    function OpenFile(fileName: string; saveSettings: boolean = True; ShowTrigger: boolean = False): boolean;
    function SaveFile(fileName: string = string.Empty; saveAs: boolean = False; encrypt: boolean = False): boolean;
    function SaveFileAs: boolean;
    procedure ApplyGridSettings;
    procedure AlignBottomControls;
    function Find(aText: string; aMatchCase, aWrapAround, aDirectionDown: boolean; Silent: boolean = False): boolean; overload;
    function Find(aText: string; aMatchCase, aWrapAround, aDirectionDown: boolean; out aRowsChanged: integer; Silent: boolean): boolean;
      overload;
    function Replace(aText, aToText: string; aMatchCase, aWrapAround: boolean): boolean;
    function ReplaceAll(aText, aToText: string; aMatchCase, aWrapAround: boolean): boolean;
    property Zoom: float read FZoom write SetZoom;
    property ReadOnly: boolean read FReadOnly write SetReadOnly;
    property WordWrap: boolean read FWordWrap write FWordWrap;
    property EnterSubmit: boolean read FEnterSubmit write FEnterSubmit;
    property BiDiRightToLeft: boolean read FBiDiRightToLeft write SetBiDiRightToLeft;
    property ShowArchived: boolean read FShowArchived write SetShowArchived;
    property ShowDuration: boolean read FShowDuration write SetShowDuration;
    property ShowTime: boolean read FShowTime write SetShowTime;
    property ShowTags: boolean read FShowTags write SetShowTags;
    property ShowNote: boolean read FShowNote write SetShowNote;
    property HideNoteText: boolean read FHideNoteText write SetHideNoteText;
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
    property SelectedRows: TIntegerArray read GetSelectedRows write FLoadedRowMem;
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

  TagsColorBrigtness = 80;
  TagsColorSaturation = 80;
  TagsDimnessSelected = 55;
  TagsDimnessColor = 45;
  TagsDimness = 35;

  IndentStr = '  ';
  CommentSlashStr = '//';
  CommentHashStr = '#';
  CommentStarStr = '*';
  CommentMinusStr = '--';
  CommentSemicolonStr = ';';
  CommentTwoColonStr = '::';
  CommentREMStr = 'REM ';
  CommentApostropheStr = '''';
  mailto = 'mailto:';
  http = 'http://';

  clRowHighlight = TColor($FFF0DC); // RGB(220,240,255)
  clRowFocused = TColor($FFdcc8); // RGB(200,220,255)
  clRowExpired = TColor($DCDCFF); // RGB(255,220,220)
  clDarkBlue = TColor($B40000); // RGB(0,0,180)
  clGray = TColor($F0F0F0); // RGB(240,240,240)
  clGrayLight = TColor($FAFAFA); // RGB(250,250,250)
  clGrayWhite = TColor($FEFEFE); // RGB(254,254,254)
  clGrayDark = TColor($E9E9E9); // RGB(233,233,233)
  clGrayHighlight = TColor($E3E3E3); // RGB(227,227,227)
  clDuplicateHighlight = TColor($BBFFFF); // RGB(204, 255, 255)

resourcestring
  rapp = 'Notetask';
  runtitled = 'Untitled';
  rrows = ' tasks';
  rcantfind = 'Can''t find';
  rgroupuntitled = 'Ungrouped';
  rfilenotfound = 'The requested file was not found on the disk.';
  rfilereadonly = 'The file is read-only or is in use by another user.';
  rdeleteconfirm = 'Are you sure you want to delete this task?';
  rdeletesconfirm = 'Are you sure you want to delete selected tasks?';
  rmergesconfirm = 'Are you sure you want to merge selected tasks?';
  rsplitconfirm = 'Are you sure you want to split the selected tasks based on the current column?';
  rsplitwarning = 'Please select the column with line breaks to split the tasks.';
  rarchiveconfirm = 'Are you sure you want to archive / unarchive this task?';
  rarchivesconfirm = 'Are you sure you want to archive / unarchive selected tasks?';
  rsavechanges = 'Do you want to save the changes?';
  rclearconfirm = 'Are you sure you want to clear the data in the selected area?';
  ropendialogfilter = 'Task files (*.tsk)|*.tsk|Text files (*.txt)|*.txt|Markdown files (*.md)|*.md|All files (*.*)|*.*';
  rsavedialogfilter =
    'Task files (*.tsk)|*.tsk|Encrypted Task files (*.tsk)|*.tsk|Text files (*.txt)|*.txt|Markdown files (*.md)|*.md|All files (*.*)|*.*';
  rundoconfirm = 'Are you sure you want to discard all changes? This action cannot be undone.';
  rnumstringtoolarge = 'The line number is out of the allowed range.';
  rchatgpt = 'https://chatgpt.com?q=';
  rdeletegroupconfirm = 'Are you sure you want to delete this group? This will also delete all tasks within this group.';
  rremovetagtitle = 'Remove tag(s)';
  renternewtag = 'Enter new tag...';
  renternewtaghint = 'The tag is added to or removed from all selected tasks.' + sLineBreak +
    'Colon separates the tag from the suffix.' + sLineBreak + 'Semicolon allows adding multiple tags.';
  rremovetag = 'Are you sure you want to remove tag(s)';
  rentergroupname = 'Enter the group name:';
  rconfirmation = 'Confirmation';
  rgototask = 'Go to task';
  rtasknumber = 'Task number:';
  rpassword = 'Password:';
  rconfirmpassword = 'Confirm Password:';
  rincorrectpassword = 'Incorrect password!';
  rencrypted = 'Encrypted';
  rreadonly = 'Read-only';
  rgroup = 'Group';
  rgoto = 'Go to';
  rok = 'OK';
  ryes = '&Yes';
  rno = '&No';

implementation

uses filemanager, settings, systemtool, crypto, forminput, formmemo, formfind, formreplace, formabout, formdonate;

  {$R *.lfm}

  { TformNotetask }

procedure TformNotetask.FormCreate(Sender: TObject);
begin
  // Init components
  tagsEdit := TTagEdit.Create(Self);
  tagsEdit.Parent := panelTags;
  tagsEdit.AutoSuggest := True;
  tagsEdit.Align := alTop;
  tagsEdit.AutoSizeHeight := True;
  tagsEdit.DragIndicatorColor := clRed;
  tagsEdit.SelectionRectColor := clSilver;
  tagsEdit.TagHoverColor := clNone;
  tagsEdit.TagSuffixColor := clGrayWhite;
  tagsEdit.RoundCorners := 20;
  tagsEdit.TagHeightFactor := 2;
  tagsEdit.AutoColorSeed := 14;
  tagsEdit.EditMinWidth := 150;
  tagsEdit.AutoColorBrigtness := TagsColorBrigtness;
  tagsEdit.AutoColorSaturation := TagsColorSaturation;
  tagsEdit.BackSpaceEditTag := True;
  tagsEdit.ShowHint := True;
  tagsEdit.SuggestedButtonCaption := string.Empty;
  MiscImages.GetBitmap(0, tagsEdit.SuggestedButtonGlyph);
  tagsEdit.PopupMenu := PopupTags;
  tagsEdit.OnKeyDown := @tagsEditKeyDown;
  tagsEdit.OnTagClick := @tagsEditTagClick;
  tagsEdit.OnBeforeChange := @tagsEditBeforeChange;
  tagsEdit.OnChange := @tagsEditChange;
  tagsEdit.OnTagAdd := @tagsEditTagAdd;
  tagsEdit.OnTagRemove := @tagsEditTagRemove;
  tagsEdit.OnTagReorder := @tagsEditTagReorder;
  tagsEdit.OnExit := @tagsEditExit;

  // Initialize variables
  FZoom := 1;
  FBackup := True;
  FReadOnly := False;
  FWordWrap := True;
  FEnterSubmit := True;
  FShowTime := True;
  FHideNoteText := False;
  FShowStatusBar := True;
  FShowTags := False;
  FShowNote := False;
  FMemoNeedSelectAll := True;
  FRepaint := False;
  FDuplicateHighlight := True;
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
  FLastTabFilter := -1;
  FLastTextMatch := False;
  FSortOrder := soAscending;
  FKeyPressed := string.Empty;
  FEncrypted := False;
  FreeBytesSecure(FKeyEnc);
  FreeBytesSecure(FKeyAuth);
  FreeBytesSecure(FSalt);
  openDialog.Filter := ropendialogfilter;
  saveDialog.Filter := rsavedialogfilter;
  panelNote.Color := clGray;
  Splitter.Color := clGrayDark;
  SplitFilter.Color := clGrayLight;

  // Remove standart border
  UpdateComboRegion(filterBox);

  Application.OnException := @ApplicationOnException;
  Application.OnQueryEndSession := @ApplicationOnQueryEndSession;
  Application.OnShowHint := @ApplicationOnShowHint;

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

  LoadFormSettings(Self, tagsEdit);
  LoadGridSettings(Self, taskGrid, string.Empty);

  // After load settings
  aWordWrap.Checked := FWordWrap;
  memoNote.WordWrap := FWordWrap;
  aEnterSubmit.Checked := FEnterSubmit;
  aBidiRightToLeft.Checked := FBiDiRightToLeft;
  aShowArchived.Checked := FShowArchived;
  ShowTags := FShowTags;
  ShowNote := FShowNote;
  ShowStatusBar := FShowStatusBar;
  ShowTime := FShowTime;
  HideNoteText := FHideNoteText;

  // Zoom
  FOriginalFontSize := Font.Size;

  // Apply loaded settings to columns
  ApplyColumnSetting;
  ApplySortingActions;

  // Set language
  SetLanguage(Language);

  // menu access
  {$IFDEF UNIX}
  aRunPowershell.Visible := False;
  aRunPowershell.Enabled := False;
  aPageProperties.Visible := False;
  aPageProperties.Enabled := False;
  filterClear.Flat := False;
  {$ENDIF}
end;

procedure TformNotetask.FormDestroy(Sender: TObject);
begin
  SaveFormSettings(Self, tagsEdit);
  SaveGridSettings(Self, taskGrid, ExtractFileName(FFileName));

  // Free allocated resources
  Tasks.Free;
  ResourceBitmapCheck.Free;
  ResourceBitmapUncheck.Free;
  ResourceBitmapStarGold.Free;
  ResourceBitmapStarGray.Free;

  FreeFile;

  tagsEdit.Free;
end;

procedure TformNotetask.FormShow(Sender: TObject);
var
  FilePath: string;
  FileOpened: boolean;
begin
  Visible := False;

  // Check if a command line argument is passed
  FileOpened := False;
  if ParamCount > 0 then
  begin
    FilePath := ParamStr(1); // Get the file path
    if (not FilePath.StartsWith('--')) then
      FileOpened := OpenFile(FilePath, False, True); // Function to load a task from the file
  end;

  if not FileOpened then NewFile(False);

  // Before paint form
  SetCaption;
  RestoreSelectedState(True, True, True);
  Tasks.CalcTagsWidths(-1, taskGrid.Columns[1].Width, tagsEdit, Font);
  SetZoom(FZoom);

  // Paint Form
  if (not Application.Terminated) then
  begin
    OnShow := nil;
    Visible := True;
    OnShow := @FormShow;
    {$IFDEF UNIX}
    Application.ProcessMessages;
    WindowState := FWindowStateLoaded;
    {$ELSE}
    Application.ProcessMessages;
    {$ENDIF}
  end;

  // After paint form
  if (ReadOnly) then ShowMessage(rfilereadonly);
end;

procedure TformNotetask.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if memoNote.Focused or filterBox.Focused or tagsEdit.Focused then
    exit;

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
  if (ssCtrl in Shift) and (ssShift in Shift) and (Key = VK_F2) then // Ctrl + Shift + F2
  begin
    aEditGroupTooltip.Execute;
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
  if (ssCtrl in Shift) and (Key in [VK_1, VK_2, VK_3, VK_4, VK_5, VK_6, VK_7, VK_8, VK_9]) then // Ctrl + Number
  begin
    EditComplite;
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
  if (ssShift in Shift) and (Key = VK_TAB) then // Ctrl + Shift + Tab
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
    if (not IsEditing) and (taskGrid.Focused) then
    begin
      if (not taskGrid.Columns[0].Visible) or (taskGrid.Col = 6) then
        StarTasks
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
      end
      else
      if (taskGrid.Col in [2, 3]) and (FEnterSubmit) and (Shift = [ssCtrl]) then
      begin
        Memo.SelText := sLineBreak;
        key := 0;
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
        StarTasks;
    end;
  end;
end;

procedure TformNotetask.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (Key = VK_MENU) and (ssShift in Shift) then
  begin
    Key := 0; // block menuZoomIn flicker when Alt+Shift
    Exit;
  end;
end;

procedure TformNotetask.FormResize(Sender: TObject);
begin
  taskGridResize(Sender);

  AlignBottomControls;
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

procedure TformNotetask.ApplicationOnException(Sender: TObject; E: Exception);
begin
  MessageDlg('Notetask', E.Message, mtWarning, [mbOK], 0);
end;

procedure TformNotetask.ApplicationOnQueryEndSession(var CanEnd: boolean);
begin
  CanEnd := IsCanClose;
  if (CanEnd) then
    Application.Terminate;
end;

procedure TFormNotetask.ApplicationOnShowHint(var HintStr: string; var CanShow: boolean; var HintInfo: THintInfo);
var
  TabIndex: integer;
begin
  // Check if the hint is requested for the TabControl
  if HintInfo.HintControl is TNoteBookStringsTabControl then
  begin
    Application.HintPause := 100;

    // Determine which tab is under the mouse cursor
    TabIndex := groupTabs.IndexOfTabAt(HintInfo.CursorPos.X, 5);

    if TabIndex >= 0 then
    begin
      // For testing, just show the tab's caption as the hint
      HintStr := Tasks.GetGroupHint(FindGroupRealIndex(TabIndex));

      if (Trim(HintStr)) <> string.Empty then
      begin
        // Allow the hint to be displayed
        CanShow := True;
        HintInfo.HideTimeout := MaxInt;
      end;
    end
    else
      // Mouse is not over a tab, do not show hint
      CanShow := False;
  end
  else
    Application.HintPause := 500;
end;

procedure TformNotetask.taskGridHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
var
  LastTask: integer;
begin
  EditComplite;
  if IsColumn then
  begin
    LastTask := Tasks.Map(FLastRow);

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

    taskGrid.Row := Tasks.ReverseMap(LastTask);
  end
  else
    // Set LastTask when clicked on begining of LastTask
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

    StarTasks(aRow);
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
    SetTags;
  end;
end;

procedure TformNotetask.taskGridColRowDeleted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
begin
  if (not IsColumn) then
  begin
    Tasks.DeleteTask(tIndex);
    if ShowDuration then FillGrid;
    SetInfo;
    SetNote;
    SetTags;
  end;
end;

procedure TformNotetask.taskGridHeaderSized(Sender: TObject; IsColumn: boolean; Index: integer);
begin
  taskGridResize(Sender);
  if IsColumn then
    CalcRowHeight(0, True);
  EditControlSetBounds(PanelMemo, taskGrid.Col, taskGrid.Row);
  EditControlSetBounds(DatePicker, taskGrid.Col, taskGrid.Row, 2, -2, -2, 0);
end;

procedure TformNotetask.taskGridSelectCell(Sender: TObject; aCol, aRow: integer; var CanSelect: boolean);
begin
  FIsSelecting := True;
end;

procedure TformNotetask.taskGridKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  Grid: TStringGrid;
  i: integer;
  //Sel:TGridRect;
  //Col: integer;
begin
  Grid := Sender as TStringGrid;

  // Remove due to conflict with standard selection behavior
  //// Shift + Home -> select from current position to first visible column
  //if (Key = VK_HOME) and (ssShift in Shift) and not (ssCtrl in Shift) then
  //begin
  //  Sel:=  Grid.Selection;
  //  Col := Grid.Col;
  //  for i := 1 to Grid.ColCount - 1 do
  //    if Grid.ColWidths[i] > 0 then
  //    begin
  //      Grid.Col := i;
  //      Break;
  //    end;
  //  Grid.ClearSelections;
  //  Grid.Selection := Rect(Grid.Col, Sel.Top, Sel.Right, Sel.Bottom);
  //  Grid.Update;
  //  Key := 0;
  //  Exit;
  //end
  //else
  //// Shift + End -> select from current position to last visible column
  //if (Key = VK_END) and (ssShift in Shift) and not (ssCtrl in Shift) then
  //begin
  //  Sel:=  Grid.Selection;
  //  Col := Grid.Col;

  //  for i := Grid.ColCount - 1 downto 0 do
  //    if Grid.ColWidths[i] > 0 then
  //    begin
  //      Grid.Col := i;
  //      Break;
  //    end;
  //  Grid.ClearSelections;
  //  Grid.Selection := Rect(Sel.Left, Sel.Top, Grid.Col, Sel.Bottom);
  //  Grid.Update;
  //  Key := 0;
  //  Exit;
  //end
  //else

  // Default HOME -> move to first visible column
  if (Key = VK_HOME) and not (ssCtrl in Shift) and not (ssShift in Shift) then
  begin
    for i := 1 to Grid.ColCount - 1 do
      if Grid.ColWidths[i] > 0 then
      begin
        Grid.Col := i;
        Break;
      end;
    Key := 0;
    Exit;
  end
  else
  // Default END -> move to last visible column
  if (Key = VK_END) and not (ssCtrl in Shift) and not (ssShift in Shift) then
  begin
    for i := Grid.ColCount - 1 downto 0 do
      if Grid.ColWidths[i] > 0 then
      begin
        Grid.Col := i;
        Break;
      end;
    Key := 0;
    Exit;
  end;
end;

procedure TformNotetask.taskGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FIsSelecting := False;

  if (Button = mbMiddle) and (ssCtrl in Shift) then // Middle button + Ctrl
    aZoomDefault.Execute;
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

      if Visible and taskGrid.Visible and taskGrid.CanFocus then
        taskGrid.SetFocus;
    end;
    Popup.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;

  if (Button = mbLeft) and (ssCtrl in Shift) and (taskGrid.Col in [2, 3]) then
    TryOpenAsUrl(Trim(taskGrid.Cells[taskGrid.Col, taskGrid.Row]));

  if (not FRepaint) then
  begin
    FRepaint := True;
    GridInvalidate;
  end;
end;

procedure TformNotetask.taskGridMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint; var Handled: boolean);
begin
  if IsEditing then
    EditComplite;

  if ssCtrl in Shift then
  begin
    if WheelDelta > 0 then
      aZoomIn.Execute
    else
      aZoomOut.Execute;
    Handled := True;
  end;
end;

procedure TformNotetask.taskGridResize(Sender: TObject);
var
  Rect: TRect;
begin
  {$IFDEF UNIX}
  GridAdjustScrollBars;
  {$ENDIF}

  // Get the cell dimensions
  Rect := taskGrid.CellRect(taskGrid.Col, taskGrid.Row);

  // Update the size and position of the Memo
  if Assigned(taskGrid.Editor) and (taskGrid.Editor is TPanel) then
    TPanel(taskGrid.Editor).SetBounds(Rect.Left + 5, Rect.Top + 1, Rect.Right - Rect.Left - 10, Rect.Bottom - Rect.Top - 3);
end;

procedure TformNotetask.taskGridDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
var
  Grid: TStringGrid;
  Value: string;
  DrawRect: TRect;
  bgFill: TColor;
  Flags: cardinal;
  Task: TTask;
  Amount: double;
  FS: TFormatSettings;
  ImgIndex: integer;
  ImgX, ImgY: integer;
  BitTags: TBitmap;
  TagsWidth: integer = 0;
  OriginalLeft, OriginalRight: integer;
begin
  Grid := Sender as TStringGrid;
  bgFill := clWhite;

  // Border for fixed cells
  if (aRow < Grid.FixedRows) or (aCol < Grid.FixedCols) then
  begin
    Grid.Canvas.Pen.Color := clSilver;
    Grid.Canvas.Pen.Style := psSolid;
    Grid.Canvas.Pen.Width := 1;
    Grid.Canvas.Pen.Width := 0;
    Grid.Canvas.Brush.Style := bsClear;
    Grid.Canvas.Rectangle(aRect.Left - 1, aRect.Top - 1, aRect.Right, aRect.Bottom);

    if (aRow = 0) and (aCol = 0) and (SortColumn = 0) and Assigned(taskGrid.TitleImageList) then
    begin
      if SortOrder = soAscending then
        ImgIndex := 0
      else
        ImgIndex := 1;

      ImgX := aRect.Right - taskGrid.TitleImageList.Width - 4;
      ImgY := aRect.Top + ((aRect.Bottom - aRect.Top - taskGrid.TitleImageList.Height) div 2);

      taskGrid.TitleImageList.Draw(Grid.Canvas, ImgX, ImgY, ImgIndex, True);
    end;
  end
  else
  begin
    // Determine background color
    if (gdFocused in aState) and (taskGrid.Selection.Height = 0) and (taskGrid.Selection.Width = 0) and
      ((IsEditing and ((Assigned(TaskGrid.Editor) and taskGrid.Editor.Focused) or (Assigned(Memo) and Memo.Focused))) or
      (not IsEditing)) then
    begin
      bgFill := clRowFocused;    // Focused
      Grid.Canvas.Font.Color := clBlack;
    end
    else
    if (gdSelected in aState) and ((taskGrid.Selection.Height > 0) or (taskGrid.Selection.Width > 0)) then
    begin
      bgFill := clHighlight;    // Multiselect
      Grid.Canvas.Font.Color := clWhite;
    end
    else
    if gdRowHighlight in aState then
    begin
      bgFill := clRowHighlight; // Highlight
      Grid.Canvas.Font.Color := clBlack;
    end
    else
    begin
      if (Assigned(Tasks)) and (Tasks.HasTask(ARow)) then
      begin
        Task := Tasks.GetTask(ARow);
        if (ShowColumnDate) and (not Task.Done) and (Task.Date > 0) and (Task.Date < Now) then // Color expired Task
        begin
          bgFill := clRowExpired; // Expired warning red
          Grid.Canvas.Font.Color := clBlack;
        end
        else
        if (not Task.Done) and (Task.Archive) then
        begin
          bgFill := clWhite; // Not done but arhive warning color
          Grid.Canvas.Font.Color := clMaroon;
        end
        else
        begin
          bgFill := clWhite; // All other white
          Grid.Canvas.Font.Color := clBlack;
        end;
      end;
    end;

    if (Assigned(Tasks)) and (Tasks.HasTask(ARow)) then
    begin
      Task := Tasks.GetTask(ARow);
      if Task.Star then
        Grid.Canvas.Font.Style := Grid.Canvas.Font.Style + [fsBold];

      if (aCol = 2) and (Task.Archive) then
        Grid.Canvas.Font.Style := Grid.Canvas.Font.Style + [fsStrikeOut];

      if (aCol = 3) and (Task.NoteItalic) then
        Grid.Canvas.Font.Style := Grid.Canvas.Font.Style + [fsItalic];

      if (aCol = 5) and (Task.Date > Now) and (not (gdSelected in aState)) then
        Grid.Canvas.Font.Color := clDarkBlue;
    end;

    // Fill the cell background
    Grid.Canvas.Brush.Color := bgFill;
    Grid.canvas.Brush.Style := bsSolid;
    Grid.canvas.FillRect(aRect);

    if (aCol in [1, 6]) then
    begin
      Grid.DefaultDrawCell(aCol, aRow, aRect, aState);
      exit;
    end;

    if (aCol = 4) and (TryStrToFloat(Grid.Cells[ACol, ARow], Amount)) then
    begin
      FS := DefaultFormatSettings;
      FS.ThousandSeparator := ' ';
      Value := FormatFloat('#,##0.##########', StrToFloat(Grid.Cells[ACol, ARow]), FS);
    end
    else
      Value := Grid.Cells[ACol, ARow];

    if (Assigned(Tasks)) and (Tasks.HasTask(ARow)) then
    begin
      if (aCol = 2) then
      begin
        Task := Tasks.GetTask(ARow);
        if Task.Tags.Count > 0 then
        begin
          BitTags := tagsEdit.GetTagsBitmap(Task.Tags, Round(Max(Max(Font.Size div 2, 8) * FZoom, 1)),
            Min(ARect.Width, 500), ARect.Height, 2, ifthen(gdSelected in aState, TagsDimnessSelected,
            ifthen(bgFill <> clWhite, TagsDimnessColor, TagsDimness)), ColorToRGB(bgFill));
          try
            BitTags.TransparentColor := clWhite;
            BitTags.Transparent := True;
            TagsWidth := BitTags.Width;
            Task.TagsWidth := TagsWidth;
            if TagsWidth < aRect.Width - 50 then
            begin
              if taskGrid.BiDiMode = bdLeftToRight then
                Grid.canvas.Draw(aRect.Right - TagsWidth - 5, aRect.Top, BitTags)
              else
                Grid.canvas.Draw(aRect.Left + 5, aRect.Top, BitTags);
            end
            else
              TagsWidth := 0;
          finally
            BitTags.Free;
          end;
        end
        else
          TagsWidth := Task.TagsWidth;
      end;
    end;

    if Length(Value) > 0 then
    begin
      if FDuplicateHighlight and not (gdSelected in aState) and (FLastText <> string.Empty) and
        (Value = FLastText) and (taskGrid.Selection.Height = 0) and ((aCol <> FLastCol) or (aRow <> FLastRow)) then
      begin
        Grid.canvas.Brush.Style := bsSolid;
        Grid.canvas.Brush.Color := clDuplicateHighlight;
      end
      else
        Grid.canvas.Brush.Style := bsClear;
      DrawRect := aRect;
      DrawRect.Inflate(-4, 0);

      // Save original boundaries
      OriginalLeft := DrawRect.Left;
      OriginalRight := DrawRect.Right;

      // Reduce text area by TagsWidth for text measurement
      if TagsWidth < DrawRect.Width then
      begin
        if FBiDiRightToLeft then
          DrawRect.Left := OriginalLeft + TagsWidth  // For RTL: reserve space on the left
        else
          DrawRect.Right := OriginalRight - TagsWidth; // For LTR: reserve space on the right
      end;

      // First pass: calculate text size
      Flags := DT_CALCRECT;
      if FBiDiRightToLeft then
        Flags := Flags or DT_RIGHT
      else
        Flags := Flags or DT_LEFT;
      if FWordWrap then
        Flags := Flags or DT_WORDBREAK;

      DrawText(Grid.canvas.handle, PChar(Value), Length(Value), DrawRect, Flags);

      // Second pass: actual text drawing
      // Restore the reduced area for drawing
      DrawRect.Left := OriginalLeft;
      DrawRect.Right := OriginalRight;

      if TagsWidth < DrawRect.Width then
      begin
        if FBiDiRightToLeft then
          DrawRect.Left := OriginalLeft + TagsWidth
        else
          DrawRect.Right := OriginalRight - TagsWidth;
      end;

      Flags := DT_NOPREFIX;
      if FBiDiRightToLeft then
        Flags := Flags or DT_RIGHT
      else
        Flags := Flags or DT_LEFT;
      if FWordWrap then
        Flags := Flags or DT_WORDBREAK;

      if (FHideNoteText) and (aCol = 3) then
        Value := MaskTextWithBullets(Value, Grid.Canvas, FLineEnding);

      if (Value = string.Empty) or (filterBox.Text = string.Empty) or (Grid.canvas.Brush.Color = clDuplicateHighlight) or
        (Pos(ULower(filterBox.Text), ULower(ifthen(aCol = 4, ReplaceStr(Value, ' ', ''), Value))) = 0) or
        ((FHideNoteText) and (aCol = 3)) then
      begin
        DrawText(Grid.canvas.handle, PChar(Value), Length(Value), DrawRect, Flags);
      end
      else
      begin
        if (aCol = 4) then Value := ReplaceStr(Value, ' ', '');

        DrawHighlightedText(Grid.Canvas, Value, filterBox.Text, DrawRect, tagsEdit.BlendColors(clDuplicateHighlight, bgFill, 50));
      end;
    end;
  end;
end;

procedure TformNotetask.DrawHighlightedText(aCanvas: TCanvas; const aText, aFilterText: string; aRect: TRect; aColor: TColor);
type
  TTextRange = record
    StartPos: integer;
    EndPos: integer;
    IsMatch: boolean;
  end;
  PTextRange = ^TTextRange;

  TLineWord = record
    word: string;
    Width: integer;
    IsMatch: boolean;
  end;
var
  TextRanges: TList;
  LineWords: array of TLineWord = ();
  LineStartIndex: integer; // Index of first word in current line
  LineWidth: integer;      // Current line width
  Flags: cardinal;
  CurrentY: integer;
  LineHeight: integer;
  SavedBrushStyle: TBrushStyle;
  SavedBrushColor: TColor;
  SavedTextColor: TColor;
  Range: PTextRange;
  Fragment: string;
  CurrentWord: string;
  WordStart, WordEnd: integer;
  WordWidth: integer;
  TotalGroupWidth: integer;
  I, J: integer;

// Build text ranges for highlighting matches
  procedure BuildTextRanges;
  var
    LowerText, LowerFilter: string;
    CurrentPos, MatchPos: integer;
    Range: PTextRange;
  begin
    if (aFilterText = '') or (aText = '') then
    begin
      // No filter text - create single normal range
      New(Range);
      Range^.StartPos := 1;
      Range^.EndPos := Length(aText);
      Range^.IsMatch := False;
      TextRanges.Add(Range);
      Exit;
    end;

    LowerText := ULower(aText);
    LowerFilter := ULower(aFilterText);
    CurrentPos := 1;

    while CurrentPos <= Length(aText) do
    begin
      MatchPos := Pos(LowerFilter, LowerText, CurrentPos);

      if MatchPos = 0 then
      begin
        // No more matches - add remaining text as normal range
        if CurrentPos <= Length(aText) then
        begin
          New(Range);
          Range^.StartPos := CurrentPos;
          Range^.EndPos := Length(aText);
          Range^.IsMatch := False;
          TextRanges.Add(Range);
        end;
        Break;
      end
      else
      begin
        // Add text before match as normal range
        if MatchPos > CurrentPos then
        begin
          New(Range);
          Range^.StartPos := CurrentPos;
          Range^.EndPos := MatchPos - 1;
          Range^.IsMatch := False;
          TextRanges.Add(Range);
        end;

        // Add matching text as highlight range
        New(Range);
        Range^.StartPos := MatchPos;
        Range^.EndPos := MatchPos + Length(aFilterText) - 1;
        Range^.IsMatch := True;
        TextRanges.Add(Range);

        CurrentPos := MatchPos + Length(aFilterText);
      end;
    end;
  end;

  // Draw a complete line
  procedure DrawLine(LineStart, LineEnd: integer; Y: integer);
  var
    J, X: integer;
    DrawRect: TRect;
    TotalLineWidth: integer;
  begin
    // Remove last space
    LineWords[LineEnd].word := TrimRight(LineWords[LineEnd].word);
    LineWords[LineEnd].Width := aCanvas.TextWidth(LineWords[LineEnd].word);
    LineWords[LineStart].word := TrimLeft(LineWords[LineStart].word);
    LineWords[LineStart].Width := aCanvas.TextWidth(LineWords[LineStart].word);

    // Calculate total width of this line
    TotalLineWidth := 0;
    for J := LineStart to LineEnd do
      TotalLineWidth := TotalLineWidth + LineWords[J].Width;

    // Set starting X based on text direction
    if FBiDiRightToLeft then
      X := aRect.Right - TotalLineWidth  // Align line to right
    else
      X := aRect.Left;                   // Align line to left

    // Ensure we don't draw outside the bounds
    if X < aRect.Left then X := aRect.Left;
    if X + TotalLineWidth > aRect.Right then
    begin
      // Adjust if line is too long (shouldn't happen with proper word wrapping)
      TotalLineWidth := aRect.Right - X;
      if FBiDiRightToLeft then
        X := aRect.Right - TotalLineWidth;
    end;

    // Draw all words in the line
    for J := LineStart to LineEnd do
    begin
      // Check if we're still within bounds
      if (X + LineWords[J].Width > aRect.Right) then
        Break;

      DrawRect := Rect(X, Y, X + LineWords[J].Width, Y + LineHeight);
      if LineWords[J].IsMatch then
      begin
        aCanvas.Brush.Style := bsSolid;
        aCanvas.Brush.Color := aColor;
        aCanvas.FillRect(DrawRect);
      end
      else
        aCanvas.Brush.Style := bsClear;

      Flags := DT_NOPREFIX;
      if FBiDiRightToLeft then
        Flags := Flags or DT_RIGHT
      else
        Flags := Flags or DT_LEFT;
      if FWordWrap then
        Flags := Flags or DT_WORDBREAK;

      DrawText(aCanvas.handle, PChar(LineWords[J].word), Length(LineWords[J].word), DrawRect, Flags);
      X := X + LineWords[J].Width;
    end;
  end;

begin
  TextRanges := TList.Create;
  try
    // Save canvas state
    SavedBrushStyle := aCanvas.Brush.Style;
    SavedBrushColor := aCanvas.Brush.Color;
    SavedTextColor := aCanvas.Font.Color;

    try
      BuildTextRanges;
      if TextRanges.Count = 0 then Exit;
      LineHeight := aCanvas.TextHeight('Wg');

      // First, extract all words from all text ranges
      SetLength(LineWords, 0);

      for I := 0 to TextRanges.Count - 1 do
      begin
        Range := PTextRange(TextRanges[I]);
        Fragment := Copy(aText, Range^.StartPos, Range^.EndPos - Range^.StartPos + 1);

        WordStart := 1;
        while WordStart <= Length(Fragment) do
        begin
          // Find word boundaries (include spaces and line breaks as separate "words")
          WordEnd := WordStart;

          if Fragment[WordStart] = ' ' then
          begin
            // This is a space - treat it as a separate word
            while (WordEnd < Length(Fragment)) and (Fragment[WordEnd + 1] = ' ') do
              Inc(WordEnd);
          end
          else if (Fragment[WordStart] = #10) or (Fragment[WordStart] = #13) then
          begin
            // This is a line break - handle different line break types
            if (Fragment[WordStart] = #13) and (WordEnd < Length(Fragment)) and (Fragment[WordEnd + 1] = #10) then
            begin
              // Windows line break (CR+LF) - treat as single word
              Inc(WordEnd);
            end;
            // For Unix line breaks (LF only) or Mac classic (CR only), we don't need to do anything else
            // as WordEnd is already at the current position
          end
          else
          begin
            // This is a non-space word - continue until space or line break
            while (WordEnd < Length(Fragment)) and (Fragment[WordEnd + 1] <> ' ') and (Fragment[WordEnd + 1] <> #10) and
              (Fragment[WordEnd + 1] <> #13) do
              Inc(WordEnd);
          end;

          CurrentWord := Copy(Fragment, WordStart, WordEnd - WordStart + 1);

          // For line breaks, use a special representation or calculate width differently
          if (Fragment[WordStart] = #10) or (Fragment[WordStart] = #13) then
          begin
            // Line breaks have zero width for calculation purposes
            // but we need to handle them specially during drawing
            WordWidth := 0;

            // Optionally, you could replace line break with a visible character for debugging
            // CurrentWord := '¶'; // Uncomment for debugging
            // WordWidth := aCanvas.TextWidth('¶'); // Uncomment for debugging
          end
          else
          begin
            WordWidth := aCanvas.TextWidth(CurrentWord);
          end;

          // Add word to array
          SetLength(LineWords, Length(LineWords) + 1);
          LineWords[High(LineWords)].word := CurrentWord;
          LineWords[High(LineWords)].Width := WordWidth;
          LineWords[High(LineWords)].IsMatch := Range^.IsMatch;

          WordStart := WordEnd + 1;
        end;
      end;

      if Length(LineWords) = 0 then Exit;

      // Now break into lines and draw
      LineStartIndex := 0;
      LineWidth := 0;
      CurrentY := aRect.Top;

      for I := 0 to High(LineWords) do
      begin
        WordWidth := LineWords[I].Width;

        // Calculate total width from current position to next break (space or line break)
        TotalGroupWidth := WordWidth;

        // Look ahead to find the next break and calculate total width
        for J := I + 1 to High(LineWords) do
        begin
          // Stop at next break (space or line break)
          if (LineWords[J].word = ' ') or (LineWords[J].word = #10) or (LineWords[J].word = #13) or
            (LineWords[J].word = #13#10) then
            Break;

          TotalGroupWidth := TotalGroupWidth + LineWords[J].Width;
        end;

        // Check if the entire group fits or if it's a line break
        if ((LineWidth > 0) and (LineWidth + TotalGroupWidth > aRect.Width)) or (LineWords[I].word = #10) or
          (LineWords[I].word = #13) or (LineWords[I].word = #13#10) then
        begin
          DrawLine(LineStartIndex, I - 1, CurrentY);
          CurrentY := CurrentY + LineHeight;
          if CurrentY + LineHeight > aRect.Bottom then
            Exit;
          LineStartIndex := I;
          LineWidth := WordWidth;
        end
        else
        begin
          LineWidth := LineWidth + WordWidth;
        end;
      end;

      // Draw the last line
      if LineStartIndex <= High(LineWords) then
        DrawLine(LineStartIndex, High(LineWords), CurrentY);

    finally
      // Restore canvas state
      aCanvas.Brush.Style := SavedBrushStyle;
      aCanvas.Brush.Color := SavedBrushColor;
      aCanvas.Font.Color := SavedTextColor;
    end;

  finally
    // Clean up text ranges
    for I := 0 to TextRanges.Count - 1 do
      Dispose(PTextRange(TextRanges[I]));
    TextRanges.Free;
  end;
end;

procedure TformNotetask.taskGridSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
var
  sDateTime: TDateTime;
begin
  if FReadOnly then
  begin
    Editor := nil;  // disable editor — grid stays view-only
    exit;
  end;

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
    Memo.OnMouseDown := @MemoNoteMouseDown; // Event MouseDown
    Memo.OnDblClick := @MemoNoteDblClick; // Event MouseDown
    Memo.OnKeyUp := @MemoNoteKeyUp; // Event KeyUp
    Memo.OnMouseUp := @MemoNoteMouseUp; // Event MouseUp
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

procedure TformNotetask.taskGridTopLeftChanged(Sender: TObject);
begin
  EditComplite;

  if taskGrid.TopRow = 1 then
    Application.QueueAsyncCall(@DelayedInvalidate, 0);

  if taskGrid.TopRow + taskGrid.VisibleRowCount >= taskGrid.RowCount then
    Application.QueueAsyncCall(@DelayedInvalidate, 0);
end;

procedure TformNotetask.DelayedInvalidate(Data: PtrInt);
begin
  Repaint;
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

  if (aCol <> FLastCol) or (aRow <> FLastRow) or (taskGrid.Selection.Height <> FLastSelectionHeight) then
  begin
    if (aRow <> FLastRow) or (taskGrid.Selection.Height <> FLastSelectionHeight) then
    begin
      SetNote;
      SetTags;
    end;

    FLastCol := aCol;
    FLastRow := aRow;
    ChangeLastText(taskGrid.Cells[aCol, aRow], aCol, aRow);
  end;

  // Save row to mem
  if Length(FLastRowMem) > FindGroupRealIndex(groupTabs.TabIndex) then
    FLastRowMem[FindGroupRealIndex(groupTabs.TabIndex)] := aRow;

  FLastSelectionHeight := taskGrid.Selection.Height;
end;

procedure TformNotetask.groupTabsChange(Sender: TObject);
begin
  EditComplite;
  tagsEdit.FinishEdit;

  if (Length(FLastRowMem) > Tasks.SelectedGroup) then
    FLastRowMem[Tasks.SelectedGroup] := taskGrid.Row;

  if (groupTabs.TabIndex >= 0) then
    Tasks.ChangeGroup(FindGroupRealIndex(groupTabs.TabIndex), True);

  FillGrid;

  if (Length(FLastRowMem) > Tasks.SelectedGroup) then
    taskGrid.Row := FLastRowMem[Tasks.SelectedGroup]
  else
    taskGrid.Row := 1;
  taskGrid.ClearSelections;

  Tasks.CreateBackup;
  GridBackupSelection;

  Tasks.CalcTagsWidths(-1, taskGrid.Columns[1].Width, tagsEdit, Font);
  CalcRowHeight(0, True);
  SetNote;
  SetTags;
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
  if not (ssLeft in Shift) then
  begin
    groupTabsMouseLeave(Self);
    exit;
  end;

  target := groupTabs.IndexOfTabAt(X, Y);
  if FDragTab >= 0 then
  begin
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
  // Hide hint if long move
  Application.HintPause := 100;
  if (target <> FLastTabTarget) then
    Application.HideHint;
  FLastTabMouseX := X;
  FLastTabTarget := target;
end;

procedure TformNotetask.groupTabsMouseLeave(Sender: TObject);
begin
  FLastTabMouseX := 0;
  Application.HintPause := 500;
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
  panelNote.Color := clGrayHighlight;
end;

procedure TformNotetask.panelNoteMouseLeave(Sender: TObject);
begin
  FNoteSelecting := False;
  panelNote.Color := clGray;
end;

procedure TformNotetask.panelNoteMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FNoteSelecting := False;
end;

procedure TformNotetask.statusBarContextPopup(Sender: TObject; MousePos: TPoint; var Handled: boolean);
var
  i, PosX: integer;
begin
  // If menuZoomIn called by keyboard MousePos is invalid
  if (MousePos.X < 0) or (MousePos.Y < 0) then
  begin
    Handled := True;
    exit;
  end;

  FStatusPanelIndex := -1;
  PosX := 0;
  for i := 0 to statusBar.Panels.Count - 1 do
  begin
    Inc(PosX, statusBar.Panels[i].Width);
    if (MousePos.X < PosX) or (i = statusBar.Panels.Count - 1) then
    begin
      FStatusPanelIndex := i;
      Break;
    end;
  end;

  if (FStatusPanelIndex > 2) and (FStatusPanelIndex < statusBar.Panels.Count) then
    PopupStatusbar.PopUp(statusBar.ClientToScreen(MousePos).X, statusBar.ClientToScreen(MousePos).Y)
  else
  if (FStatusPanelIndex = 0) then
    PopupZoom.PopUp(statusBar.ClientToScreen(MousePos).X, statusBar.ClientToScreen(MousePos).Y)
  else
  if (FStatusPanelIndex = 1) and (not FReadOnly) then
    PopupEncoding.PopUp(statusBar.ClientToScreen(MousePos).X, statusBar.ClientToScreen(MousePos).Y)
  else
  if (FStatusPanelIndex = 2) and (not FReadOnly) then
    PopupLineEnding.PopUp(statusBar.ClientToScreen(MousePos).X, statusBar.ClientToScreen(MousePos).Y);
end;

procedure TformNotetask.filterBoxChange(Sender: TObject);
var
  LastTask, LastTab: integer;
  LastSelTop, LastSelBottom: integer;
  LastRect, NewRect: TGridRect;
begin
  LastTask := Tasks.Map(taskGrid.Row);
  LastTab := FindGroupRealIndex(groupTabs.TabIndex);
  LastSelTop := Tasks.Map(taskGrid.Selection.Top);
  LastSelBottom := Tasks.Map(taskGrid.Selection.Bottom);
  LastRect := taskGrid.Selection;

  if (Trim(filterBox.Text) <> string.Empty) and (FLastTabFilter < 0) then
  begin
    FLastTabFilter := LastTab;
  end;
  SetTabs;
  FillGrid;

  if (LastTab = FindGroupRealIndex(groupTabs.TabIndex)) then
    taskGrid.Row := Tasks.ReverseMap(LastTask);

  if Trim(filterBox.Text) = string.Empty then
    FLastTabFilter := -1;

  NewRect := Rect(LastRect.Left, Tasks.ReverseMap(LastSelTop), LastRect.Right, Tasks.ReverseMap(LastSelBottom));
  if (LastRect.Height = NewRect.Height) then
    taskGrid.Selection := NewRect
  else
    taskGrid.ClearSelections;
  CalcRowHeight(0, True);
  SetInfo;
  SetNote;
  SetTags;
end;

procedure TformNotetask.filterBoxKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  SelStart, SelLen: integer;
  ClipText: string;
begin
  SelStart := filterBox.SelStart;
  SelLen := filterBox.SelLength;

  case Key of
    VK_DELETE: // Delete
    begin
      if SelLen > 0 then
        filterBox.Text := string(Copy(unicodestring(filterBox.Text), 1, SelStart) +
          Copy(unicodestring(filterBox.Text), SelStart + SelLen + 1, MaxInt))
      else
        filterBox.Text := string(Copy(unicodestring(filterBox.Text), 1, SelStart) +
          Copy(unicodestring(filterBox.Text), SelStart + 2, MaxInt));
      filterBox.SelStart := SelStart;
      Key := 0;
    end;

    Ord('A'): if ssCtrl in Shift then // Ctrl + A
      begin
        filterBox.SelectAll;
        Key := 0;
      end;

    Ord('C'): if ssCtrl in Shift then // Ctrl + C
      begin
        if SelLen > 0 then
        begin
          ClipText := string(Copy(unicodestring(filterBox.Text), SelStart + 1, SelLen));
          Clipboard.AsText := ClipText;
        end;
        Key := 0;
      end;

    Ord('X'): if ssCtrl in Shift then // Ctrl + X
      begin
        if SelLen > 0 then
        begin
          ClipText := string(Copy(unicodestring(filterBox.Text), SelStart + 1, SelLen));
          Clipboard.AsText := ClipText;
          filterBox.Text := string(Copy(unicodestring(filterBox.Text), 1, SelStart) +
            Copy(unicodestring(filterBox.Text), SelStart + SelLen + 1, MaxInt));
        end;
        Key := 0;
      end;

    Ord('V'): if ssCtrl in Shift then // Ctrl + V
      begin
        ClipText := Clipboard.AsText;
        filterBox.Text := string(Copy(unicodestring(filterBox.Text), 1, SelStart) + unicodestring(ClipText) +
          Copy(unicodestring(filterBox.Text), SelStart + SelLen + 1, MaxInt));
        filterBox.SelStart := SelStart + Length(unicodestring(ClipText));
        Key := 0;
      end;

    VK_ESCAPE:
      if Visible and taskGrid.Visible and taskGrid.CanFocus then
        taskGrid.SetFocus;
    else
  end;
  filterBox.OnChange(Self);
end;

procedure TformNotetask.filterClearClick(Sender: TObject);
begin
  filterBox.Text := string.Empty;
  filterBox.OnChange(Self);
  if Visible and taskGrid.Visible and taskGrid.CanFocus then
    taskGrid.SetFocus;
end;

procedure TformNotetask.SplitFilterChangeBounds(Sender: TObject);
begin
  UpdateComboRegion(filterBox);
end;

procedure TformNotetask.contextCopyStatusbarClick(Sender: TObject);
var
  PanelText: string;
begin
  if (FStatusPanelIndex >= 0) and (FStatusPanelIndex < statusBar.Panels.Count) then
  begin
    PanelText := statusBar.Panels[FStatusPanelIndex].Text;
    if PanelText <> string.Empty then
      Clipboard.AsText := PanelText;
  end;
end;

procedure TformNotetask.contextCopyTagsClick(Sender: TObject);
begin
  if tagsEdit.SelectedTags.Count > 0 then
    Clipboard.AsText := tagsEdit.SelectedTags.DelimitedText
  else
  if tagsEdit.HoveredTag <> string.Empty then
    Clipboard.AsText := tagsEdit.HoveredTag;
end;

procedure TformNotetask.contextDeleteTagsClick(Sender: TObject);
begin
  if tagsEdit.SelectedTags.Count > 0 then
    tagsEdit.RemoveSelectedTags
  else
  if tagsEdit.HoveredTag <> string.Empty then
    tagsEdit.RemoveTag(tagsEdit.HoveredTag, True);
end;

procedure TformNotetask.contextColorClick(Sender: TObject);
var
  HoverTag: string;
  HoverIndex: integer;
begin
  HoverTag := GetBeforeColon(LowerCase(tagsEdit.HoveredTag));
  HoverIndex := tagsEdit.TagColors.IndexOf(HoverTag);

  if HoverIndex >= 0 then
    colorDialog.Color := tagsEdit.TagColors.Items[HoverIndex].Color
  else
    colorDialog.Color := tagsEdit.GetAutoColor(HoverTag);

  if (colorDialog.Execute) then
  begin
    if HoverIndex >= 0 then
      tagsEdit.TagColors.Items[HoverIndex].Color := colorDialog.Color
    else
      tagsEdit.TagColors.Add(HoverTag, colorDialog.Color);
    tagsEdit.Invalidate;
    GridInvalidate;
  end;
end;

procedure TformNotetask.contextResetColorClick(Sender: TObject);
var
  HoverTag: string;
  HoverIndex: integer;
begin
  HoverTag := GetBeforeColon(LowerCase(tagsEdit.HoveredTag));
  HoverIndex := tagsEdit.TagColors.IndexOf(HoverTag);

  if HoverIndex >= 0 then
  begin
    tagsEdit.TagColors.Delete(HoverIndex);

    tagsEdit.Invalidate;
    GridInvalidate;
  end;
end;

procedure TformNotetask.contextZoom50Click(Sender: TObject);
begin
  SetZoom(0.5);
end;

procedure TformNotetask.contextZoom60Click(Sender: TObject);
begin
  SetZoom(0.6);
end;

procedure TformNotetask.contextZoom70Click(Sender: TObject);
begin
  SetZoom(0.7);
end;

procedure TformNotetask.contextZoom80Click(Sender: TObject);
begin
  SetZoom(0.8);
end;

procedure TformNotetask.contextZoom90Click(Sender: TObject);
begin
  SetZoom(0.9);
end;

procedure TformNotetask.contextZoom100Click(Sender: TObject);
begin
  SetZoom(1.0);
end;

procedure TformNotetask.contextZoom110Click(Sender: TObject);
begin
  SetZoom(1.1);
end;

procedure TformNotetask.contextZoom120Click(Sender: TObject);
begin
  SetZoom(1.2);
end;

procedure TformNotetask.contextZoom130Click(Sender: TObject);
begin
  SetZoom(1.3);
end;

procedure TformNotetask.contextZoom140Click(Sender: TObject);
begin
  SetZoom(1.4);
end;

procedure TformNotetask.contextZoom150Click(Sender: TObject);
begin
  SetZoom(1.5);
end;

procedure TformNotetask.contextWindowsCRLFClick(Sender: TObject);
begin
  FLineEnding := TLineEnding.WindowsCRLF;
  if (contextWindowsCRLF.Checked = False) then
  begin
    contextWindowsCRLF.Checked := True;
    SetInfo;
    SetChanged;
  end;
end;

procedure TformNotetask.contextUnixLFClick(Sender: TObject);
begin
  FLineEnding := TLineEnding.UnixLF;
  if (contextUnixLF.Checked = False) then
  begin
    contextUnixLF.Checked := True;
    SetInfo;
    SetChanged;
  end;
end;

procedure TformNotetask.contextMacintoshCRClick(Sender: TObject);
begin
  FLineEnding := TLineEnding.MacintoshCR;
  if (contextMacintoshCR.Checked = False) then
  begin
    contextMacintoshCR.Checked := True;
    SetInfo;
    SetChanged;
  end;
end;

procedure TformNotetask.contextANSIClick(Sender: TObject);
begin
  FEncoding := TEncoding.ANSI;
  if (contextANSI.Checked = False) then
  begin
    contextANSI.Checked := True;
    SetInfo;
    SetChanged;
  end;
end;

procedure TformNotetask.contextASCIIClick(Sender: TObject);
begin
  FEncoding := TEncoding.ASCII;
  if (contextASCII.Checked = False) then
  begin
    contextASCII.Checked := True;
    SetInfo;
    SetChanged;
  end;
end;

procedure TformNotetask.contextUTF8Click(Sender: TObject);
begin
  FEncoding := TEncoding.UTF8;
  if (contextUTF8.Checked = False) then
  begin
    contextUTF8.Checked := True;
    SetInfo;
    SetChanged;
  end;
end;

procedure TformNotetask.contextUTF8BOMClick(Sender: TObject);
begin
  FEncoding := UTF8BOMEncoding;
  if (contextUTF8BOM.Checked = False) then
  begin
    contextUTF8BOM.Checked := True;
    SetInfo;
    SetChanged;
  end;
end;

procedure TformNotetask.contextUTF16BEBOMClick(Sender: TObject);
begin
  FEncoding := UTF16BEBOMEncoding;
  if (contextUTF16BEBOM.Checked = False) then
  begin
    contextUTF16BEBOM.Checked := True;
    SetInfo;
    SetChanged;
  end;
end;

procedure TformNotetask.contextUTF16LEBOMClick(Sender: TObject);
begin
  FEncoding := UTF16LEBOMEncoding;
  if (contextUTF16LEBOM.Checked = False) then
  begin
    contextUTF16LEBOM.Checked := True;
    SetInfo;
    SetChanged;
  end;
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

  SaveFormSettings(self, tagsEdit); // Save setting for new process

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
  if filterBox.Focused then exit;
  if memoNote.Focused then
  begin
    MemoNoteUndo;
    exit;
  end
  else
  if tagsEdit.Focused and not tagsEdit.ReadOnly and tagsEdit.EditBox.CanUndo then
  begin
    tagsEdit.EditBox.Undo;
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
    SetTags;
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
      FLineEnding := FLineEndingOriginal;
      FEncoding := FEncodingOriginal;
      BackupSelectedState;
      Tasks.UndoBackupInit;
      FillGrid;
      ResetRowHeight;
      SetFilter(False);
      SetInfo;
      SetNote;
      SetTags;
      SetTabs;
      GridClearSelection;
      Tasks.CreateBackup;
      SetChanged(False);
      RestoreSelectedState(True, False);
    end;
  end;
end;

procedure TformNotetask.aCutExecute(Sender: TObject);
begin
  if filterBox.Focused then exit;
  if memoNote.Focused then
  begin
    MemoNoteBackup;
    memoNote.CutToClipboard;
    exit;
  end
  else
  if tagsEdit.Focused then
  begin
    tagsEdit.EditBox.CutToClipboard;
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
  if filterBox.Focused then exit;
  if memoNote.Focused then
  begin
    memoNote.CopyToClipboard;
    exit;
  end
  else
  if tagsEdit.SelectedTags.Count > 0 then
  begin
    Clipboard.AsText := tagsEdit.SelectedTags.DelimitedText;
    exit;
  end
  else
  if tagsEdit.HoveredTag <> string.Empty then
  begin
    Clipboard.AsText := tagsEdit.HoveredTag;
    exit;
  end
  else
  if tagsEdit.EditBox.Focused then
  begin
    tagsEdit.EditBox.CopyToClipboard;
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
  if filterBox.Focused then exit;
  if memoNote.Focused then
  begin
    if (not memoNote.ReadOnly) then
    begin
      MemoNoteBackup;
      PasteWithLineEnding(memoNote);
    end;
    exit;
  end
  else
  if tagsEdit.Focused then
  begin
    if not tagsEdit.ReadOnly then
      tagsEdit.EditBox.PasteFromClipboard;
    exit;
  end;

  if Screen.ActiveForm <> Self then exit;

  if not IsEditing then
  begin
    Sel := Tasks.PasteFromClipboard(taskGrid, SortOrder);
    FillGrid;
    ResetRowHeight;
    if (Assigned(DatePicker)) then
      DatePicker.DateTime := Tasks.GetTask(taskGrid.Row).Date;
    if (SortColumn = 0) then
      taskGrid.Selection := Sel;
    SetChanged;
    SetInfo;
    SetNote;
    SetTags;
    SetFilter;
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
  if Screen.ActiveForm <> Self then exit;
  if filterBox.Focused then exit;

  if memoNote.Focused then
  begin
    if (not memoNote.ReadOnly) then
    begin
      MemoNoteBackup;
      memoNote.ClearSelection;
    end;
    exit;
  end
  else
  if tagsEdit.Focused then
  begin
    {$IFDEF UNIX}
    if not tagsEdit.ReadOnly then
    begin
      if tagsEdit.EditBox.SelLength = 0 then
        tagsEdit.EditBox.SelLength := CalcDeleteCount(tagsEdit.EditBox.Text, tagsEdit.EditBox.SelStart);
      tagsEdit.EditBox.ClearSelection;
    end;
    {$ENDIF}
    exit;
  end;

  if taskGrid.RowCount < 2 then exit;
  if not IsEditing then
  begin
    ClearSelected(False);
    if ShowDuration then FillGrid;
    SetInfo;
    SetNote;
    SetTags;
  end
  else
  if (taskGrid.InplaceEditor is TPanel) then
    with Memo do
    begin
      {$IFDEF UNIX}
      if SelLength = 0 then
      begin
        SelStart := SelStart;
        SelLength := 1;
      end
      else
        MemoBackup;
      ClearSelection;
      {$ELSE}
      MemoDelKey(False);
      {$ENDIF}
    end;
end;

procedure TformNotetask.aSelectAllExecute(Sender: TObject);
begin
  if filterBox.Focused then exit;
  if memoNote.Focused then
  begin
    memoNote.SelectAll;
    exit;
  end
  else
  if tagsEdit.Focused then
  begin
    tagsEdit.EditBox.SelectAll;
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
    SetTags;
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
    FOriginalFontSize := Self.Font.Size;
    SetZoom(FZoom);
  end;
end;

procedure TformNotetask.aInsertTaskExecute(Sender: TObject);
var
  Ind: integer;
  TaskText, Oper, Value: string;
begin
  if Screen.ActiveForm <> Self then exit;

  EditComplite;
  GridBackupSelection;

  TaskText := '[ ]';
  if Length(filterBox.Text) > 0 then
  begin
    StartsWithOperator(filterBox.Text, Oper, Value);
    if (Length(Oper) = 0) or (Oper = '#') or (Oper = '=') then
      TaskText += ' `' + Value + '`';
  end;
  Ind := Tasks.InsertTask(TaskText, taskGrid.Row);
  FillGrid;
  ResetRowHeight;
  if (Ind > 0) then
    taskGrid.Row := Tasks.ReverseMap(Ind)
  else
    taskGrid.Row := taskGrid.Row + 1;

  if Visible and taskGrid.Visible and taskGrid.CanFocus then
    taskGrid.SetFocus;
  SetInfo;
  SetChanged;
  SetNote;
  SetTags;
end;

procedure TformNotetask.aDuplicateTasksExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;

  DuplicateTasks;
end;

procedure TformNotetask.aMergeTasksExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount < 2 then exit;

  MergeTasks;
end;

procedure TformNotetask.aSplitTasksExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if taskGrid.RowCount = 0 then exit;

  SplitTasks;
end;

procedure TformNotetask.aZoomDefaultExecute(Sender: TObject);
begin
  Zoom := 1;
end;

procedure TformNotetask.aZoomInExecute(Sender: TObject);
begin
  if Zoom < 4.9 then
    Zoom := Zoom + 0.1;
end;

procedure TformNotetask.aZoomOutExecute(Sender: TObject);
begin
  if Zoom > 0.2 then
    Zoom := Zoom - 0.1;
end;

procedure TformNotetask.aCheckforupdatesExecute(Sender: TObject);
begin
  CheckGithubLatestVersion;
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
    newRow := Tasks.MoveTasksTop(taskGrid.Selection.Top, taskGrid.Selection.Bottom, FShowArchived)
  else
    newRow := Tasks.MoveTasksBottom(taskGrid.Selection.Bottom, taskGrid.Selection.Top, FShowArchived);

  FillGrid;
  if (newRow > -1) then
  begin
    ResetRowHeight;
    taskGrid.Row := 0;
    taskGrid.Col := selCol;
    taskGrid.Selection := TGridRect.Create(selLeft, 0, selRight, selLen);
  end;
  SetChanged;
  SetNote;
  SetTags;
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
    newRow := Tasks.MoveTasksBottom(taskGrid.Selection.Top, taskGrid.Selection.Bottom, FShowArchived)
  else
    newRow := Tasks.MoveTasksTop(taskGrid.Selection.Bottom, taskGrid.Selection.Top, FShowArchived);

  FillGrid;
  if (newRow > -1) then
  begin
    ResetRowHeight;
    taskGrid.Row := taskGrid.RowCount - selLen;
    taskGrid.Col := selCol;
    taskGrid.Selection := TGridRect.Create(selLeft, taskGrid.RowCount - selLen, selRight, taskGrid.RowCount);
  end;
  SetChanged;
  SetNote;
  SetTags;
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
  SetNote;
  SetTags;
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
  SetNote;
  SetTags;
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
    Tasks.SelectedGroup, FShowArchived, filterBox.Text, FShowTime));

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
  SetNote;
  SetTags;
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
    Tasks.SelectedGroup, FShowArchived, filterBox.Text, FShowTime));

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
  SetNote;
  SetTags;
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
    SetMode(aInsertGroup.Caption, rentergroupname, rOK);

    // Show the form as a modal dialog
    if ShowModal = mrOk then
    begin
      newName := editText.Text;
      if (newName = rgroupuntitled) then newName := string.Empty;

      Result := Tasks.InsertGroup(newName);
      if (Result <> FindGroupRealIndex(groupTabs.TabIndex)) then
      begin
        InsertAtPos(FLastRowMem, Result, 0);
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
    SetMode(aRenameGroup.Caption, rentergroupname, rOK, groupTabs.Tabs[groupTabs.TabIndex]);

    // Show the form as a modal dialog
    if (ShowModal = mrOk) {and (editText.Text <> groupTabs.Tabs[groupTabs.TabIndex])} then
    begin
      newName := editText.Text;
      if (newName = rgroupuntitled) and (groupTabs.TabIndex = 0) then newName := string.Empty;

      if (Tasks.RenameGroup(FindGroupRealIndex(groupTabs.TabIndex), newName)) then
      begin
        SetTabs;
        SetChanged;
      end;
    end;
  finally
    Hide;
  end;
end;

procedure TformNotetask.aEditGroupTooltipExecute(Sender: TObject);
begin
  if (groupTabs.TabIndex = 0) and (Tasks.GroupNames[0] = string.Empty) then
    exit;
  with formMemoText do
  try
    if not formMemoText.Showed then
    begin
      Left := self.Left + 14;
      Top := self.top + 52;
    end;
    SetMode(rapp, aEditGroupTooltip.Caption, rOK, Tasks.GetGroupHint(FindGroupRealIndex(groupTabs.TabIndex)), 400, 180, FWordWrap, True);

    // Show the form as a modal dialog
    if ShowModal = mrOk then
    begin
      Tasks.RehintGroup(FindGroupRealIndex(groupTabs.TabIndex), formMemoText.memoText.Text);
      SetChanged;
    end;
  finally
    Hide;
  end;
end;

procedure TformNotetask.aFilterExecute(Sender: TObject);
begin
  panelTabs.Visible := (not panelTabs.Visible and not filterBox.Focused) or
    (not ((groupTabs.Tabs.Count = 1) and (Tasks.GroupNames[0] = string.Empty)));
  Invalidate;
  Application.ProcessMessages;
  if (panelTabs.Visible) then
  begin
    if filterBox.Focused then
    begin
      if Visible and taskGrid.Visible and taskGrid.CanFocus then
      begin
        filterBox.Clear;
        filterBoxChange(Self);
        taskGrid.SetFocus;
      end;
    end
    else
    begin
      if (Length(filterBox.Text) = 0) then
      begin
        if Assigned(Memo) and (Memo.SelText <> string.Empty) then
        begin
          filterBox.Text := Memo.SelText;
          filterBoxChange(Self);
        end
        else
        if memoNote.Visible and memoNote.Focused and (memoNote.SelText <> string.Empty) then
        begin
          filterBox.Text := memoNote.SelText;
          filterBoxChange(Self);
        end;
      end;

      if Visible and filterBox.Visible and filterBox.CanFocus then
        filterBox.SetFocus;
    end;
  end
  else
  begin
    filterBox.Clear;
    filterBoxChange(Self);
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
    SetMode(aDuplicateGroup.Caption, rentergroupname, rOK, groupTabs.Tabs[groupTabs.TabIndex]);

    // Show the form as a modal dialog
    if (ShowModal = mrOk) then
    begin
      if (Tasks.CopyGroup(FindGroupRealIndex(groupTabs.TabIndex), editText.Text)) then
      begin
        InsertAtPos(FLastRowMem, FindGroupRealIndex(groupTabs.TabIndex) + 1, FLastRowMem[FindGroupRealIndex(groupTabs.TabIndex)]);
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
  Mem: TIntegerArray;
begin
  Confirm := MessageDlg(rdeletegroupconfirm, mtConfirmation, [mbYes, mbNo], 0);

  if (Confirm = mrYes) then
  begin
    if (Tasks.DeleteGroup(FindGroupRealIndex(groupTabs.TabIndex))) then
    begin
      DeleteAtPos(FLastRowMem, FindGroupRealIndex(groupTabs.TabIndex));
      Mem := CloneArray(FLastRowMem);
      SetTabs;
      ChangeGroup(FindGroupTabIndex(Tasks.SelectedGroup));
      FLastRowMem := CloneArray(Mem);
      if (Length(FLastRowMem) > Tasks.SelectedGroup) then
        taskGrid.Row := FLastRowMem[Tasks.SelectedGroup];
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

procedure TformNotetask.aShowTagsExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  ShowTags := aShowTags.Checked;
end;

procedure TformNotetask.aShowNoteExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  ShowNote := aShowNote.Checked;
end;

procedure TformNotetask.aHideNoteTextExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  EditComplite;
  HideNoteText := aHideNoteText.Checked;
  FillGrid;
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
var
  sel: integer;
begin
  if Screen.ActiveForm <> Self then exit;

  EditComplite;
  FWordWrap := aWordWrap.Checked;
  sel := memoNote.SelLength;
  memoNote.WordWrap := FWordWrap;
  if sel = 0 then
    memoNote.SelLength := 0;
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
    SetMode(rgototask, rtasknumber, rgoto, IntToStr(taskGrid.Row), True);

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

  SetFilter;
end;

procedure TformNotetask.memoNoteKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  LinesPerPage, NewPos: integer;
  Render: string;
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
  if not (ssCtrl in Shift) and not (ssShift in Shift) and (Key = VK_TAB) then // Tab
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
    MemoDelKey;
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
  if (ssCtrl in Shift) and (Key = VK_TAB) then // Tab
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
  if (ssCtrl in Shift) and (Key = VK_RETURN) and (trim(memoNote.SelText) <> string.Empty) then // Ctrl + Enter
  begin
    Render := RenderWordCanvas(memoNote.SelText, Font.Name, Max(ifthen(Font.Size = 0, 10, Font.Size) - 2, 2));
    if (Render <> memoNote.SelText) then
    begin
      MemoNoteBackup;
      memoNote.SelText := Render;
    end;
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
  end
  else
  if Key = VK_ESCAPE then // Escape
    if Visible and taskGrid.Visible and taskGrid.CanFocus then
      taskGrid.SetFocus;
end;

procedure TformNotetask.memoNoteMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  Value: string;
begin
  if (Button = mbMiddle) and (ssCtrl in Shift) then // Middle button + Ctrl
    aZoomDefault.Execute
  else
  if not (ssDouble in Shift) then
  begin
    if ssCtrl in Shift then
    begin
      Value := Trim(FNoteLastSelText);

      if IsURL(Value) then
        OpenURL(IfThen(HasScheme(Value), Value, http + Value))
      else
      if IsEmail(Value) then
        OpenURL(IfThen(AnsiStartsText(mailto, Value), Value, mailto + Value));

      (Sender as TMemo).SelStart := FNoteLastSelStart;
      (Sender as TMemo).SelLength := FNoteLastSelLength;
    end
    else
      FMemoSelStartClicked := (Sender as TMemo).SelStart;
  end;
end;

procedure TformNotetask.memoNoteMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if not (ssCtrl in Shift) then
  begin
    FNoteLastSelText := (Sender as TMemo).SelText;
    FNoteLastSelStart := (Sender as TMemo).SelStart;
    FNoteLastSelLength := (Sender as TMemo).SelLength;
  end;
end;

procedure TformNotetask.memoNoteMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint; var Handled: boolean);
begin
  if ssCtrl in Shift then
  begin
    if WheelDelta > 0 then
      aZoomIn.Execute
    else
      aZoomOut.Execute;
    Handled := True;
  end;
end;

procedure TformNotetask.memoNoteKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  FNoteLastSelText := (Sender as TMemo).SelText;
  FNoteLastSelStart := (Sender as TMemo).SelStart;
  FNoteLastSelLength := (Sender as TMemo).SelLength;
end;

procedure TformNotetask.memoNoteChange(Sender: TObject);
begin
  taskGrid.Cells[3, taskGrid.Row] := memoNote.Text;
  Tasks.SetTask(taskGrid, taskGrid.Row, FBackup, FShowTime);
  CalcRowHeight(taskGrid.Row);
  SetChanged;
end;

procedure TformNotetask.memoNoteDblClick(Sender: TObject);
{$IFDEF WINDOWS}
var
  Value: unicodestring;
  pos1, leftIdx, rightIdx, len: integer;
  ch: widechar;

  function CharType(ch: widechar): integer;
  begin
    // 1 = letter or digit
    // 2 = space
    // 3 = other symbol
    if IsLetterOrDigit(ch) or (ch = '_') or (ch = '-') or (ch = '@') then
      Result := 1
    else if ch = ' ' then
      Result := 2
    else
      Result := 3;
  end;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  Value := unicodestring((Sender as TMemo).Text);
  len := Length(Value);
  if len = 0 then Exit;

  if (FMemoSelStartClicked >= 0) then
    pos1 := FMemoSelStartClicked + 1
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
  {$ENDIF}
  FMemoSelStartClicked := -1;
end;

procedure TformNotetask.tagsEditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if tagsEdit.ReadOnly then exit
  else
  if Key = VK_DELETE then // Delete
  begin
    {$IFDEF UNIX}
    {$ELSE}
    if (tagsEdit.EditBox.SelLength = 0) then
      tagsEdit.EditBox.SelLength := CalcDeleteCount(tagsEdit.EditBox.Text, tagsEdit.EditBox.SelStart);
    tagsEdit.EditBox.ClearSelection;
    Key := 0;
    {$ENDIF}
  end
  else
  if (ssCtrl in Shift) and (ssShift in Shift) and (Key = VK_Z) then // Ctrl + Shift + Z
  begin
    aUndoAll.Execute;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_Z) and (not tagsEdit.EditBox.Focused) then // Ctrl + Z
  begin
    aUndo.Execute;
    Key := 0;
  end
  else
  if Key = VK_ESCAPE then // Escape
    if taskGrid.Visible and taskGrid.CanFocus then
      taskGrid.SetFocus;
end;

procedure TformNotetask.tagsEditTagClick(Sender: TObject; const TagText: string);
begin
  if (filterBox.Text <> TagText) then
  begin
    filterBox.Text := TagText;
    filterBoxChange(Self);
  end
  else
    filterClearClick(Sender);
end;

procedure TformNotetask.tagsEditBeforeChange(Sender: TObject);
begin
  Tasks.CreateBackup;
end;

procedure TformNotetask.tagsEditChange(Sender: TObject);
begin
  SetFilter;
  SetChanged;
  taskGrid.Invalidate;
end;

procedure TformNotetask.tagsEditTagAdd(Sender: TObject; const TagText: string);
begin
  TagsAdd(taskGrid.Selection, TagText);
end;

procedure TformNotetask.tagsEditTagRemove(Sender: TObject; const TagText: string);
var
  i: integer;
begin
  //  Tasks.CreateBackup;
  for i := taskGrid.Selection.Top to taskGrid.Selection.Bottom do
    if Tasks.Map(i) > -1 then
      StringListRemove(Tasks.GetTask(i).Tags, TagText);
end;

procedure TformNotetask.tagsEditTagReorder(Sender: TObject; const TagText: string; const NewIndex: integer);
var
  i: integer;
begin
  Tasks.CreateBackup;
  for i := taskGrid.Selection.Top to taskGrid.Selection.Bottom do
    if Tasks.Map(i) > -1 then
      Tasks.GetTask(i).Tags.Assign(tagsEdit.Items);
end;

procedure TformNotetask.TagsAdd(const Rect: TRect; const TagText: string);
var
  i: integer;
begin
  //  Tasks.CreateBackup;
  if Rect.Height > 0 then
  begin
    for i := Rect.Top to Rect.Bottom do
      if Tasks.Map(i) > -1 then
        Tasks.GetTask(i).Tags.Add(TagText);
  end
  else
    Tasks.GetTask(Rect.Top).Tags.Add(TagText);
end;

procedure TformNotetask.tagsEditExit(Sender: TObject);
begin
  FLastGridSelection := taskGrid.Selection;
  Application.QueueAsyncCall(@DelayedFinishTagEdit, 0);
end;

procedure TformNotetask.DelayedFinishTagEdit(Data: PtrInt);
begin
  if (Trim(tagsEdit.EditBox.Text) <> string.Empty) then
  begin
    TagsAdd(FLastGridSelection, tagsEdit.EditBox.Text);
    tagsEdit.EditBox.Text := string.Empty;
    SetTags;
  end;
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

    FilterBox.Clear;
    if Assigned(Tasks) then
      Tasks.Free;

    new := TStringList.Create;
    new.Add('[ ]');
    Tasks := TTasks.Create(new);
    FFileName := string.Empty;
    panelTabs.Visible := False;

    FEncrypted := False;
    FreeBytesSecure(FKeyEnc);
    FreeBytesSecure(FKeyAuth);
    FreeBytesSecure(FSalt);

    // Encoding of new file
    FEncoding := TEncoding.UTF8;

    // Lineending
    {$IFDEF UNIX}
    FLineEnding := FLineEnding.UnixLF;
    {$ELSE}
    FLineEnding := FLineEnding.WindowsCRLF;
    {$ENDIF}

    FreeFile;
    ReadOnly := False;

    taskGrid.Clean;
    taskGrid.RowCount := 2;
    tagsEdit.SuggestedItems.Clear;

    // Load saved settings for new file
    LoadGridSettings(Self, taskGrid, string.Empty);
    ApplyGridSettings;
    SetZoom(FZoom);

    taskGrid.Selection := Rect(taskGrid.Selection.Left, taskGrid.Row, taskGrid.Selection.Right, taskGrid.Row);
    taskGrid.Row := 1;

    SetFilter;

    FLineEndingOriginal := FLineEnding;
    FEncodingOriginal := FEncoding;
  end;
end;

function TformNotetask.OpenFile(fileName: string; saveSettings: boolean = True; ShowTrigger: boolean = False): boolean;
var
  Content: string;
  Bytes: TBytes;
  FileNameOld: string;
  EncryptedOld: boolean;
  KeyEncOld: TBytes = nil;
  KeyAuthOld: TBytes = nil;
  SaltOld: TBytes = nil;
  Token: string;
begin
  Result := False;
  if not FileExists(fileName) then
  begin
    ShowMessage(rfilenotfound);
    exit;
  end;
  // Save settings for current file
  if (saveSettings) then
    SaveGridSettings(Self, taskGrid, ExtractFileName(FFileName));

  EncryptedOld := FEncrypted;
  FEncrypted := False;

  KeyEncOld := CopyBytes(FKeyEnc);
  KeyAuthOld := CopyBytes(FKeyAuth);
  SaltOld := CopyBytes(FSalt);
  FreeBytesSecure(FKeyEnc);
  FreeBytesSecure(FKeyAuth);
  FreeBytesSecure(FSalt);
  FileNameOld := FFileName;
  FFileName := fileName;
  EditComplite;

  FreeFile;
  ReadOnly := not TryLockFile(FFileName, FSReserved);

  if (CheckEncryptedFile(FFileName)) then
  begin
    FEncrypted := True;
    Bytes := DecryptData(LoadFileAsBytes(FFileName), string.Empty, FSalt, FKeyEnc, FKeyAuth);
    if Bytes = nil then
    begin
      // Create an instance of the form
      with formInputText do
      try
        if (ShowTrigger) then
        begin
          Left := Screen.Width div 2 - formInputText.Width div 2;
          Top := Screen.Height div 2 - formInputText.Height div 2;
        end
        else
        begin
          Left := self.Left + 14;
          Top := self.top + 52;
        end;
        SetMode(ReplaceStr(rpassword, ':', ''), rpassword, rok, string.Empty, False, True);

        // Show the form as a modal dialog
        if ShowModal = mrOk then
        begin
          FEncrypted := True;
          Token := editText.Text;
        end
        else
        begin
          FFileName := FileNameOld;
          if (ShowTrigger) then
          begin
            Application.Terminate;
            exit(False);
          end
          else
          begin
            FEncrypted := EncryptedOld;
            FKeyEnc := CopyBytes(KeyEncOld);
            FKeyAuth := CopyBytes(KeyAuthOld);
            FSalt := CopyBytes(SaltOld);
            exit(False);
          end;
        end;

        Bytes := DecryptData(LoadFileAsBytes(FFileName), Token, FSalt, FKeyEnc, FKeyAuth);
        if (Bytes <> nil) then
          ReadTextFile(Bytes, Content, FEncoding, FLineEnding, FLineCount)
        else
        begin
          FFileName := FileNameOld;
          ShowMessage(rincorrectpassword);
          if (ShowTrigger) then
          begin
            Application.Terminate;
            exit(False);
          end
          else
          begin
            FEncrypted := EncryptedOld;
            FKeyEnc := CopyBytes(KeyEncOld);
            FKeyAuth := CopyBytes(KeyAuthOld);
            FSalt := CopyBytes(SaltOld);
            exit(False);
          end;
        end;
      finally
        ClearStringSecure(Token);
        FreeBytesSecure(KeyEncOld);
        FreeBytesSecure(KeyAuthOld);
        FreeBytesSecure(SaltOld);
        Hide;
      end;
    end
    else
      ReadTextFile(Bytes, Content, FEncoding, FLineEnding, FLineCount);
  end
  else
    ReadTextFile(FFileName, Content, FEncoding, FLineEnding, FLineCount);

  tagsEdit.SuggestedItems.Clear;
  if Assigned(Tasks) then
    Tasks.Free;
  Tasks := TTasks.Create(TextToStringList(Content));

  // Load saved settings for file
  LoadGridSettings(Self, taskGrid, ExtractFileName(FFileName));
  ApplyGridSettings;
  SetZoom(FZoom);
  SetFilter;

  FLineEndingOriginal := FLineEnding;
  FEncodingOriginal := FEncoding;
  if (ReadOnly) and (not ShowTrigger) then ShowMessage(rfilereadonly);
  Result := True;
end;

function TformNotetask.SaveFile(fileName: string = string.Empty; saveAs: boolean = False; encrypt: boolean = False): boolean;
var
  TaskList: TStringList;
  Token: string = string.Empty;
  FileNameOld: string;
begin
  try
    if (fileName = string.Empty) and (FFileName = string.Empty) then
      exit(SaveFileAs);

    FileNameOld := FFileName;
    if (fileName = string.Empty) then
      fileName := FFileName
    else
      FFileName := fileName;

    if (fileName <> string.Empty) then
    begin
      if (encrypt) then
      begin
        // Create an instance of the form
        with formInputText do
        try
          Left := self.Left + 14;
          Top := self.top + 52;
          SetMode(ReplaceStr(rpassword, ':', ''), rpassword, rok, string.Empty, False, True, True);

          // Show the form as a modal dialog
          if ShowModal = mrOk then
          begin
            FEncrypted := True;
            Token := editText.Text;
            FreeBytesSecure(FSalt);
            FreeBytesSecure(FKeyEnc);
            FreeBytesSecure(FKeyAuth);
          end
          else
          begin
            FFileName := FileNameOld;
            exit(False);
          end;
        finally
          Hide;
        end;
      end
      else
      if saveAs then
      begin
        FEncrypted := False;
        FreeBytesSecure(FSalt);
        FreeBytesSecure(FKeyEnc);
        FreeBytesSecure(FKeyAuth);
      end;

      TaskList := Tasks.ToStringList;
      if Assigned(TaskList) and (TaskList <> nil) then
      begin
        try
          try
            EditComplite;
            FreeFile;
            SaveTextFile(fileName, TaskList, FEncoding, FLineEnding, FEncrypted, Token, FSalt, FKeyEnc, FKeyAuth);
            SetChanged(False);
            Tasks.CreateBackupInit;
            ReadOnly := not TryLockFile(fileName, FSReserved);
            Result := True;
          except
            on E: Exception do
            begin
              FFileName := FileNameOld;
              FreeFile;
              ShowMessage(E.Message);
            end;
          end;
        finally
          TaskList.Free;
        end;
      end;
    end
    else
      Result := False;

    SetInfo;
  finally
    ClearStringSecure(Token);
  end;
end;

function TformNotetask.SaveFileAs: boolean;
begin
  if FEncrypted then
    saveDialog.FilterIndex := 2;

  if FFileName <> string.Empty then
  begin
    saveDialog.FileName := ExtractFileName(FFileName); // file name only
    saveDialog.InitialDir := ExtractFileDir(FFileName); // set initial directory
  end;

  if saveDialog.Execute then
  begin
    Result := SaveFile(saveDialog.FileName, True, saveDialog.FilterIndex = 2);
  end
  else
    Result := False;
end;

procedure TformNotetask.ApplyGridSettings;
begin
  SetChanged(False);

  filterBox.Left := 0;
  SplitFilter.Left := 0;
  ShowNote := FShowNote;
  ShowTags := FShowTags;
  ShowStatusBar := FShowStatusBar;
  ShowArchived := FShowArchived;
  Showtime := FShowTime;
  HideNoteText := FHideNoteText;

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
  SetTags;
  SetTabs;
  if Self.Visible then
    RestoreSelectedState;
end;

procedure TformNotetask.AlignBottomControls;
var
  BottomPos: integer;
begin
  //if (StatusBar.Top < panelNote.Top) or (StatusBar.Top < panelTags.Top) then
  //  StatusBar.Top := ClientHeight - StatusBar.Height;

  // Start from the bottom of the client area
  BottomPos := ClientHeight;

  // Align StatusBar at the very bottom
  StatusBar.Top := BottomPos - StatusBar.Height;
  BottomPos := StatusBar.Top;

  // Align panelTags above SplitTags
  panelTags.Top := BottomPos - panelTags.Height;
  BottomPos := panelTags.Top;

  // Align SplitTags above panelTags
  SplitTags.Top := BottomPos - SplitTags.Height;
  BottomPos := SplitTags.Top;

  // Align panelNote above SplitTags
  panelNote.Top := BottomPos - panelNote.Height;
  BottomPos := panelNote.Top;

  // Align Splitter above panelNote
  Splitter.Top := BottomPos - Splitter.Height;
  // BottomPos := Splitter.Top; // not needed unless есть что-то сверху

  // Ensure none of the controls go above the top of the form
  if Splitter.Top < 0 then
  begin
    Splitter.Top := 0;
    panelNote.Top := Splitter.Top + Splitter.Height;
    SplitTags.Top := panelNote.Top + panelNote.Height;
    panelTags.Top := SplitTags.Top + SplitTags.Height;
    StatusBar.Top := panelTags.Top + panelTags.Height;
  end;
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
      if (Assigned(PanelMemo)) and (PanelMemo.Visible) and (Memo.SelLength > 0) then
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
    if (Assigned(PanelMemo)) and (PanelMemo.Visible) and (Memo.SelLength > 0) then
      Result := Memo.SelText
    else
      Result := Tasks.GetTask(aRow).Text;
  end;
end;

procedure TformNotetask.ExecuteChatGpt;
var
  Value: string;
begin
  Value := GetExecuteValue(taskGrid.Row);

  with formMemoText do
  try
    if not formMemoText.Showed then
    begin
      Left := self.Left + 14;
      Top := self.top + 52;
    end;
    SetMode(rapp, aChatGpt.Caption, rOK, TrimRight(sLineBreak + sLineBreak + Value), 400, 180, FWordWrap);

    // Show the form as a modal dialog
    if ShowModal = mrOk then
    begin
      OpenURL(rchatgpt + EncodeUrl(Trim(formMemoText.memoText.Text)));
    end;
  finally
    Hide;
  end;
end;

procedure TformNotetask.TryOpenAsUrl(Value: string);
begin
  if IsURL(Value) then
    OpenURL(IfThen(HasScheme(Value), Value, http + Value))
  else
  if IsEmail(Value) then
    OpenURL(IfThen(AnsiStartsText(mailto, Value), Value, mailto + Value));
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
  {$IFDEF UNIX}
  Terminal: string;
  {$ELSE}
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

  {$IFDEF UNIX}
  function FindTerminal: string;
  const
    Terminals: array[0..8] of string = (
      '/usr/bin/xterm',
      '/usr/bin/gnome-terminal',
      '/usr/bin/konsole',
      '/usr/bin/tilix',
      '/usr/bin/xfce4-terminal',
      '/usr/bin/alacritty',
      '/usr/bin/lxterminal',
      '/usr/bin/mate-terminal',
      '/usr/bin/x-terminal-emulator'
    );
  var
    i: Integer;
  begin
    for i := Low(Terminals) to High(Terminals) do
      if FileExists(Terminals[i]) then
        Exit(Terminals[i]);
    Result := '';
  end;
  {$ENDIF}
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
    Terminal := FindTerminal;
    Process.Options := [];

    if Terminal <> string.Empty then
    begin
      Process.Executable := Terminal;

      if Pos('gnome-terminal', Terminal) > 0 then
      begin
        Process.Parameters.Add('--');
        Process.Parameters.Add('/bin/bash');
        Process.Parameters.Add(TempFile);
      end
      else if Pos('xfce4-terminal', Terminal) > 0 then
      begin
        Process.Parameters.Add('-e');
        Process.Parameters.Add('/bin/bash -c "source ' + TempFile + '"');
      end
      else if Pos('mate-terminal', Terminal) > 0 then
      begin
        Process.Parameters.Add('--');
        Process.Parameters.Add('/bin/bash');
        Process.Parameters.Add(TempFile);
      end else
      begin
        Process.Parameters.Add('-e');
        Process.Parameters.Add('/bin/bash');
        Process.Parameters.Add(TempFile);
      end;
    end
    else
    begin
      // fallback — no terminal emulator found
      Process.Executable := '/bin/bash';
      Process.Parameters.Add('-e');
      Process.Parameters.Add(TempFile);
      Process.Options := [poNewConsole]; // Open in a new console window
    end;
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
    Process.Options := [poNewConsole]; // Open in a new console window
    {$ENDIF}

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
  Result: integer;
  RowMem: integer = -1;
begin
  if (Index = 1) and (Tasks.GroupNames[0] = string.Empty) then exit;

  Result := Tasks.MoveGroupLeft(FindGroupRealIndex(Index), ShowArchived, filterBox.Text, FShowTime);
  if (Length(FLastRowMem) > Result) then
    RowMem := FLastRowMem[Result];
  Result := FindGroupTabIndex(Result);
  if (Result >= 0) and (Result <> Index) then
  begin
    if (Length(FLastRowMem) > FindGroupRealIndex(Index)) and (RowMem >= 0) then
    begin
      if (Length(FLastRowMem) > FindGroupRealIndex(Result)) then
        FLastRowMem[FindGroupRealIndex(Result)] := FLastRowMem[FindGroupRealIndex(Index)];
      if (RowMem >= 0) then
        FLastRowMem[FindGroupRealIndex(Index)] := RowMem;

    end;
    SetTabs(False);
    if (FDragTab >= 0) then FDragTab := Result;
    ChangeGroup(Result);
    SetChanged;
  end;
end;

procedure TformNotetask.MoveTabRight(Index: integer);
var
  Result: integer;
  RowMem: integer = -1;
begin
  if (Index = 0) and (Tasks.GroupNames[0] = string.Empty) then exit;

  Result := Tasks.MoveGroupRight(FindGroupRealIndex(Index), ShowArchived, filterBox.Text, FShowTime);
  if (Length(FLastRowMem) > Result) then
    RowMem := FLastRowMem[Result];
  Result := FindGroupTabIndex(Result);
  if (Result >= 0) and (Result <> Index) then
  begin
    if (Length(FLastRowMem) > FindGroupRealIndex(Index)) then
    begin
      if (Length(FLastRowMem) > FindGroupRealIndex(Result)) then
        FLastRowMem[FindGroupRealIndex(Result)] := FLastRowMem[FindGroupRealIndex(Index)];
      if (RowMem >= 0) then
        FLastRowMem[FindGroupRealIndex(Index)] := RowMem;
    end;
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
        Memo.Text := FMemoOldText
      else
      if taskGrid.Col in [2, 3] then
        SetFilter;
    end;

    taskGrid.EditorMode := False;
    FIsEditing := False;
    ChangeLastText(taskGrid.Cells[taskGrid.Col, taskGrid.Row]);
    ResetRowHeight;
    if Visible and taskGrid.Visible and taskGrid.CanFocus then
      taskGrid.SetFocus;
  end;
end;

procedure TformNotetask.PasteWithLineEnding(AMemo: TMemo);
var
  s: string;
begin
  {$IFDEF UNIX}
  memoNote.Tag := memoNote.VertScrollBar.Position;
  {$ENDIF}
  if Clipboard.HasFormat(CF_TEXT) then
  begin
    s := Clipboard.AsText;

    s := StringReplace(s, #13#10, #10, [rfReplaceAll]); // Windows CRLF -> LF
    s := StringReplace(s, #13, #10, [rfReplaceAll]);   // Macintosh CR -> LF

    s := StringReplace(s, #10, FLineEnding.Value, [rfReplaceAll]);
    s := StringReplace(s, #9, IndentStr, [rfReplaceAll]);

    AMemo.SelText := s;
  end;
  {$IFDEF UNIX}
  if (memoNote.Tag > 0) then
    MemoNoteSetScrollPosition(memoNote.Tag);
  {$ENDIF}
end;

procedure TformNotetask.UpdateComboRegion(Combo: TComboBox; AInsetLeft: integer = 1; AInsetTop: integer = 1;
  AInsetRight: integer = 0; AInsetBottom: integer = 1);
{$IFDEF Windows}
var
  Rgn: HRGN;
{$ENDIF}
begin
  {$IFDEF Windows}
  // Define a client area without the border (inset pixels from each side)
  Rgn := CreateRectRgn(AInsetLeft, AInsetTop, Combo.Width - AInsetRight, Combo.Height - AInsetBottom);
  // Windows takes ownership of Rgn, so it must not be deleted manually.
  SetWindowRgn(Combo.Handle, Rgn, True);
  {$ENDIF}
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

procedure TformNotetask.DelayedSetMemoFocus(Data: PtrInt);
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
  Application.QueueAsyncCall(@DelayedSetMemoFocus, 0);
end;

procedure TformNotetask.PanelMemoUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  if UTF8Key = #8 then  // backspace
    Memo.SelText := string.Empty
  else
  if (taskGrid.Col <> 4) then
    Memo.SelText := UTF8Key
  else
    Memo.SelText := CleanNumericExpression(UTF8Key);
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

  if (FKeyPressed <> string.Empty) and (FKeyPressed <> #13) then
  begin
    if (taskGrid.Col = 4) then
      Memo.SelText := CleanNumericExpression(FKeyPressed)
    else
      Memo.SelText := FKeyPressed;
    FKeyPressed := string.Empty;
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

procedure TformNotetask.MemoKeyPress(Sender: TObject; var Key: char);
begin
  // Event KeyPress for Amount column only
  // Replace comma with dot for decimal input
  if Key in ['.', ','] then
    Key := DefaultFormatSettings.DecimalSeparator;

  // Allow digits and one decimal point
  if not (Key in ['0'..'9', DefaultFormatSettings.DecimalSeparator, '-', '+', '/', '*', '%', '^', '(', ')', ' ', #8, #13]) then
    Key := #0; // Block other keys
end;

procedure TformNotetask.MemoChange(Sender: TObject);
begin
  taskGrid.Cells[taskGrid.Col, taskGrid.Row] := TMemo(Sender).Text;
  Tasks.SetTask(taskGrid, taskGrid.Row, FMemoStartEdit and FBackup, FShowTime); // Backup only on begin edit
  FMemoStartEdit := False;
  SetChanged;
  CalcRowHeight(taskGrid.Row);
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

procedure TformNotetask.DuplicateTasks;
var
  Sel, Back, Original: TGridRect;
  Value: string;
begin
  if (ReadOnly) then exit;

  if (FBackup) then
  begin
    GridBackupSelection;
    Tasks.CreateBackup;
  end;

  DisableGridEvents;
  try
    Original := taskGrid.Selection;
    taskGrid.Selection := TGridRect.Create(0, taskGrid.Selection.Top, taskGrid.Columns.Count, taskGrid.Selection.Bottom);
    Tasks.CopyToClipboard(taskGrid, FShowNote, @Value);
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
    Tasks.PasteFromClipboard(taskGrid, SortOrder, False, @Value);
    if (SortOrder = soAscending) then
      Sel := TGridRect.Create(Original.Left, Back.Bottom + 1, Original.Right, Back.Bottom + Back.Height + 1)
    else
      Sel := TGridRect.Create(Original.Left, Back.Top, Original.Right, Back.Bottom);
  finally
    EnableGridEvents;
  end;
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
  CalcRowHeight(0, True);
  SetInfo;
  SetNote;
  SetTags;
  SetChanged;
end;

procedure TformNotetask.MergeTasks;
var
  i, Confirm: integer;
  Task, Target: TTask;
  Sel: TRect;
  MaxDate: TDateTime;
begin
  if (ReadOnly) then exit;

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

      DisableGridEvents;
      try
        Task := Tasks.GetTask(taskGrid.Selection.Top);
        MaxDate := Task.Date;
        for i := taskGrid.Selection.Top + 1 to taskGrid.Selection.Bottom do
        begin
          Target := Tasks.GetTask(i);
          if (task.Text <> Target.Text) then
            Task.Text := Task.Text + FLineEnding.Value + Target.Text;
          if (task.Note <> Target.Note) then
            Task.Note := Task.Note + FLineEnding.Value + Target.Note;
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
          if Task.Tags.Count > 0 then
          begin
            Task.Tags.Sorted := True;
            Task.Tags.AddStrings(Target.Tags);
            Task.Tags.Sorted := False;
          end;
        end;
        for i := taskGrid.Selection.Bottom downto taskGrid.Selection.Top + 1 do
          Tasks.DeleteTask(i);

        // Mem selection
        Sel := taskGrid.Selection;
      finally
        EnableGridEvents;
      end;

      FillGrid;
      SetNote;
      SetTags;
      SetChanged; // Mark that data has changed

      // Restore selection
      taskGrid.Row := Sel.Top;
      taskGrid.Selection := TGridRect.Create(Sel.Left, Sel.Top, Sel.Right, Sel.Top);
    end;
  end;
end;

procedure TformNotetask.SplitTasks;
var
  i, j, index, colToSplit, Confirm: integer;
  Sel: TGridRect;
  Task, NewTask: TTask;
  TasksToSplit: array of TTask = nil;
  Lines, Lines2: TStringList;
  Source, Source2: string;
begin
  if (ReadOnly) then Exit;

  // Check if the current column can be split
  colToSplit := -1;
  for i := taskGrid.Selection.Top to taskGrid.Selection.Bottom do
  begin
    if (taskGrid.Col = 2) and (Pos(FLineEnding.Value, Tasks.GetTask(i).Text) > 0) then
      colToSplit := 2
    else if (taskGrid.Col = 3) and (Pos(FLineEnding.Value, Tasks.GetTask(i).Note) > 0) then
      colToSplit := 3;
  end;

  if (colToSplit = -1) then
  begin
    ShowMessage(rsplitwarning);
    Exit;
  end;

  Confirm := MessageDlg(rsplitconfirm, mtConfirmation, [mbYes, mbNo], 0);
  if (Confirm <> mrYes) then Exit;

  // Create backup if enabled
  if (FBackup) then
  begin
    GridBackupSelection;
    Tasks.CreateBackup;
  end;

  Lines := TStringList.Create;
  Lines2 := TStringList.Create;
  DisableGridEvents;
  try
    Lines.LineBreak := FLineEnding.Value;
    Lines2.LineBreak := FLineEnding.Value;

    // Cache selected tasks (avoid accessing the grid during modifications)
    SetLength(TasksToSplit, taskGrid.Selection.Bottom - taskGrid.Selection.Top + 1);
    for i := 0 to High(TasksToSplit) do
      TasksToSplit[i] := Tasks.GetTask(taskGrid.Selection.Top + i);

    // Process tasks in order
    index := taskGrid.Selection.Top;
    for i := 0 to High(TasksToSplit) do
    begin
      Task := TasksToSplit[i];

      // Get fields for splitting
      if (colToSplit = 2) then
      begin
        Source := Task.Text;
        Source2 := Task.Note;
      end
      else
      begin
        Source := Task.Note;
        Source2 := Task.Text;
      end;

      if Pos(FLineEnding.Value, Source) = 0 then
        Continue; // No line breaks — skip

      Lines.Text := Source;
      Lines2.Text := Source2;

      // Update original task
      if (colToSplit = 2) then
        Task.Text := Trim(Lines[0])
      else
        Task.Note := Trim(Lines[0]);

      if Lines.Count = Lines2.Count then
      begin
        if (colToSplit = 2) then
          Task.Note := Trim(Lines2[0])
        else
          Task.Text := Trim(Lines2[0]);
      end;

      // Create new tasks
      for j := 1 to Lines.Count - 1 do
      begin
        NewTask := TTask.Create;
        try
          NewTask.Copy(Task); // Copy all properties

          if (colToSplit = 2) then
            NewTask.Text := Trim(Lines[j])
          else
            NewTask.Note := Trim(Lines[j]);

          if Lines.Count = Lines2.Count then
          begin
            if (colToSplit = 2) then
              NewTask.Note := Trim(Lines2[j])
            else
              NewTask.Text := Trim(Lines2[j]);
          end;

          if SortOrder = soAscending then
          begin
            Tasks.InsertTask(NewTask.ToString, index + i + j - 1, False);
            Tasks.InsertMap(index, Tasks.Map(index));
          end
          else
          begin
            Tasks.InsertTask(NewTask.ToString, index + i + j, False);
            Tasks.InsertMap(index, Tasks.Map(index), 0);
          end;
        finally
          NewTask.Free;
        end;
      end;
      index := index + Lines.Count - 1;
    end;
  finally
    EnableGridEvents;
    Lines.Free;
    Lines2.Free;
    SetLength(TasksToSplit, 0);
  end;

  // Refresh grid and UI
  Sel := taskGrid.Selection;
  FillGrid;
  CalcRowHeight(0, True);
  SetInfo;
  SetNote;
  SetTags;
  SetChanged;

  // Restore selection
  if (SortColumn = 0) then
    taskGrid.Selection := TGridRect.Create(sel.Left, sel.Top, Sel.Right, index + (Sel.Bottom - Sel.Top))
  else
    taskGrid.Selection := Sel;
end;

procedure TformNotetask.DeleteTask(aRow: integer = 0; ShowConfirm: boolean = True);
var
  RowIndex: integer;
  Confirm: integer;
begin
  if (ReadOnly) then exit;

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
      SetTags;
      SetChanged; // Mark that data has changed
    end;
  end;
end;

procedure TformNotetask.DeleteTasks(ShowConfirm: boolean = True);
var
  i, RowIndex, Confirm: integer;
begin
  if (ReadOnly) then exit;

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

      DisableGridEvents;
      try
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
      finally
        EnableGridEvents;
      end;

      taskGrid.ClearSelections;
      FillGrid;
      SetInfo;
      SetNote;
      SetTags;
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
  if (ReadOnly) then exit;

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
      SetTabs;
      SetInfo;
      SetNote;
      SetTags;
      SetChanged;
    end;
  end;
end;

procedure TformNotetask.ArchiveTasks;
var
  i, RowIndex, Confirm: integer;
begin
  if (ReadOnly) then exit;

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

      DisableGridEvents;
      try
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
      finally
        EnableGridEvents;
      end;
      FillGrid;
      ResetRowHeight;
      SetTabs;
      SetInfo;
      SetNote;
      SetTags;
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
  if (ReadOnly) or (not ShowColumnDone) then exit;

  // If multiple rows are selected
  if (taskGrid.Selection.Width > 0) or (taskGrid.Selection.Height > 0) then
  begin
    if FBackup then
    begin
      GridBackupSelection;
      Tasks.CreateBackup;
    end;

    DisableGridEvents;
    try
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
            if (taskGrid.Columns.Items[4].Visible) and (taskGrid.Cells[5, RowIndex] = string.Empty) then
              taskGrid.Cells[5, RowIndex] := DateTimeToString(Now, FShowTime);
          end
          else
            taskGrid.Cells[1, RowIndex] := '0';

          Tasks.SetTask(taskGrid, RowIndex, False, FShowTime); // Backup created on start
        end;
      end;
    finally
      EnableGridEvents;
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
        if (taskGrid.Columns.Items[4].Visible) and (taskGrid.Cells[5, RowIndex] = string.Empty) then
          taskGrid.Cells[5, RowIndex] := DateTimeToString(Now, FShowTime);
      end
      else
        taskGrid.Cells[1, RowIndex] := '0';

      Tasks.SetTask(taskGrid, RowIndex, False, FShowTime);
      if (ShowDuration) and (Check) then FillGrid;
      SetChanged; // Mark that data has changed
      SetInfo;
    end;
  end;
end;

procedure TformNotetask.StarTasks(aRow: integer = 0);
var
  Rows: TIntegerArray = nil;
  i, RowIndex: integer;
begin
  if (ReadOnly) then exit;

  // Detect selected rows
  if (taskGrid.Selection.Width > 0) or (taskGrid.Selection.Height > 0) then
  begin
    SetLength(Rows, taskGrid.Selection.Bottom - taskGrid.Selection.Top + 1);
    for i := 0 to High(Rows) do
      Rows[i] := taskGrid.Selection.Top + i;
  end
  else
  begin
    if aRow = 0 then
      RowIndex := taskGrid.Row
    else
      RowIndex := aRow;
    SetLength(Rows, 1);
    Rows[0] := RowIndex;
  end;

  if FBackup then
  begin
    GridBackupSelection;
    Tasks.CreateBackup;
  end;

  for i := 0 to High(Rows) do
  begin
    RowIndex := Rows[i];
    if (RowIndex > 0) and (RowIndex <= Tasks.Count) then
    begin
      Tasks.StarTask(RowIndex, False);

      if Tasks.GetTask(RowIndex).Star then
        taskGrid.Cells[6, RowIndex] := '1'
      else
        taskGrid.Cells[6, RowIndex] := '0';

      Tasks.SetTask(taskGrid, RowIndex, False, FShowTime);
    end;
  end;

  SetChanged;  // Mark that data has changed
  GridInvalidate;
end;

procedure TformNotetask.IndentTasks(Outdent: boolean = False);
var
  RowIndex: integer;
  i: integer;
begin
  if (ReadOnly) then exit;

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
        CalcRowHeight;
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
  if (ReadOnly) then exit;

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
  CaretPos: TPoint;
  Offset: integer;
begin
  MemoNoteBackup;
  if (memoNote.SelLength > 0) then
  begin
    SelStartPos := memoNote.SelStart;
    SelEndPos := SelStartPos + memoNote.SelLength;
    CaretPos := Point(memoNote.CaretPos.X, memoNote.CaretPos.Y);

    memoNote.Lines.BeginUpdate;

    // Calculate start line number of selection
    memoNote.SelStart := SelStartPos;
    StartLine := memoNote.CaretPos.Y;

    // Calculate end line number of selection
    memoNote.SelStart := SelEndPos;
    EndLine := memoNote.CaretPos.Y;

    // Restore selection
    memoNote.SelStart := SelStartPos;
    memoNote.SelLength := SelEndPos - SelStartPos;

    memoNote.Lines.EndUpdate;

    // If last line not selected decrement endline
    if (StartLine <> EndLine) and (SelEndPos - SelStartPos > 0) and (EndLine = CaretPos.Y) and
      ((CaretPos.X = 0) or ((CaretPos.X = (SelEndPos - SelStartPos)) and (MemoNote.SelText[Length(MemoNote.SelText)] in [#10, #13]))) then
      Dec(EndLine);

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
  CaretPos: TPoint;
  Offset: integer;
  line: string;
begin
  MemoNoteBackup;
  SelStartPos := memoNote.SelStart;
  SelEndPos := SelStartPos + memoNote.SelLength;
  CaretPos := Point(memoNote.CaretPos.X, memoNote.CaretPos.Y);

  memoNote.Lines.BeginUpdate;

  // Calculate start line number of selection
  memoNote.SelStart := SelStartPos;
  StartLine := memoNote.CaretPos.Y;

  // Calculate end line number of selection
  memoNote.SelStart := SelEndPos;
  EndLine := memoNote.CaretPos.Y;

  // Restore selection
  memoNote.SelStart := SelStartPos;
  memoNote.SelLength := SelEndPos - SelStartPos;

  memoNote.Lines.EndUpdate;

  // If last line not selected decrement endline
  if (StartLine <> EndLine) and (SelEndPos - SelStartPos > 0) and (EndLine = CaretPos.Y) and
    ((CaretPos.X = 0) or ((CaretPos.X = (SelEndPos - SelStartPos)) and (MemoNote.SelText[Length(MemoNote.SelText)] in [#10, #13]))) then
    Dec(EndLine);

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
  CaretPos: TPoint;
  line, trimmed, resultStr: string;
  AllCommented: boolean;
  MinIndent, CurrentIndent: integer;
  CommentOffset: integer;
  FirstCommentPos, j, wordWidth, Count: integer;
begin
  {$IFDEF UNIX}
  memoNote.Tag := memoNote.VertScrollBar.Position;
  {$ENDIF}
  MemoNoteBackup;

  // If no selection and the cursor is on an empty line -> insert a line of the comment character
  if (memoNote.SelLength = 0) then
  begin
    line := Trim(memoNote.Lines[memoNote.CaretPos.Y]);
    if line = string.Empty then
    begin
      // Create a string of the comment character, approximate length to fit the editor width
      // Calculate how many times we can repeat the full word
      wordWidth := Canvas.TextWidth(aComment);
      if wordWidth > 0 then
        Count := Min(memoNote.ClientWidth, 800) div wordWidth
      else
        Count := 60; // fallback value

      // Build the repeated string
      resultStr := string.Empty;
      for j := 1 to Count do
        resultStr := resultStr + aComment;

      memoNote.Lines[memoNote.CaretPos.Y] := resultStr;

      {$IFDEF UNIX}
    if (memoNote.Tag > 0) then
      MemoNoteSetScrollPosition(memoNote.Tag);
      {$ENDIF}

      Exit; // Stop method execution, nothing else to do
    end;
  end;

  FirstCommentPos := -1;
  SelStartPos := memoNote.SelStart;
  SelEndPos := SelStartPos + memoNote.SelLength;
  CaretPos := Point(memoNote.CaretPos.X, memoNote.CaretPos.Y);

  memoNote.Lines.BeginUpdate;

  // Calculate start and end lines of selection
  memoNote.SelStart := SelStartPos;
  StartLine := memoNote.CaretPos.Y;

  memoNote.SelStart := SelEndPos;
  EndLine := memoNote.CaretPos.Y;

  // Restore selection
  memoNote.SelStart := SelStartPos;
  memoNote.SelLength := SelEndPos - SelStartPos;

  memoNote.Lines.EndUpdate;

  // If last line not selected decrement endline
  if (StartLine <> EndLine) and (SelEndPos - SelStartPos > 0) and (EndLine = CaretPos.Y) and
    ((CaretPos.X = 0) or ((CaretPos.X = (SelEndPos - SelStartPos)) and (MemoNote.SelText[Length(MemoNote.SelText)] in [#10, #13]))) then
    Dec(EndLine);

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
          {$IFDEF WINDOWS}
          //if (FLineEnding = TLineEnding.WindowsCRLF) then
             Inc(FirstCommentPos);
          {$ENDIF}
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
  {$IFDEF UNIX}
  if (memoNote.Tag > 0) then
    MemoNoteSetScrollPosition(memoNote.Tag);
  {$ENDIF}
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

function TformNotetask.CalcDeleteCount(const S: string; SelStart: integer): integer;
var
  Len, DeleteCount, Pos: integer;
begin
  Len := Length(S);
  DeleteCount := 0;
  Pos := SelStart + 1; // 1-based indexing

  while Pos <= Len do
  begin
    if IsUTF8Char(S, Pos, ' ') then
    begin
      // If space, extend deletion
      Inc(DeleteCount);
      Inc(Pos);
    end
    else if (IsUTF8Char(S, Pos, #13)) or (IsUTF8Char(S, Pos, #10)) then
    begin
      // If CR, delete it and check for following LF
      Inc(DeleteCount);
      Inc(Pos);
      if (Pos <= Len) and (IsUTF8Char(S, Pos, #10)) then
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
  Result := DeleteCount;
end;

procedure TformNotetask.MemoDelKey(aMemoNote: boolean = True);
var
  TargetMemo: Tmemo;
  DeleteCount: integer;
begin
  if aMemoNote then
    TargetMemo := memoNote
  else
    TargetMemo := Memo;

  if TargetMemo.SelLength = 0 then
  begin
    DeleteCount := CalcDeleteCount(TargetMemo.Text, TargetMemo.SelStart);
    TargetMemo.SelLength := DeleteCount;
    TargetMemo.ClearSelection;
  end
  else
  begin
    if aMemoNote then
      MemoNoteBackup
    else
      MemoBackup;
    TargetMemo.ClearSelection;
  end;
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

procedure TformNotetask.BackupSelectedState(aRowMem: boolean = False);
begin
  if (aRowMem) then
    FLoadedRowMem := CloneArray(FLastRowMem);
  FLoadedSelectedTab := groupTabs.TabIndex;
  FLoadedSelectedRow := taskGrid.Row;
  FLoadedSelection := taskGrid.Selection;
  FLoadedMemoNoteSelStart := memoNote.SelStart;
  FLoadedMemoNoteSelLength := memoNote.SelLength;
  FLoadedMemoNoteScroll := memoNote.VertScrollBar.Position;
end;

procedure TformNotetask.RestoreSelectedState(aRowMem: boolean = True; aRowMemPriority: boolean = True; aFocusMemo: boolean = False);
var
  FirstTabRow, Index: integer;
begin
  // Restore rows memory
  if (aRowMem) and (Length(FLoadedRowMem) > 0) then
    CopyToArray(FLastRowMem, FLoadedRowMem);

  if (groupTabs.Tabs.Count > 0) and ((FLoadedSelectedTab < 0) or (FLoadedSelectedTab >= groupTabs.Tabs.Count)) then
    FLoadedSelectedTab := 0;

  // Restore last open tab and rows
  if (FLoadedSelectedTab >= 0) then
  begin
    FirstTabRow := -1;
    if (Length(FLastRowMem) > FindGroupRealIndex(0)) then
      FirstTabRow := FLastRowMem[FindGroupRealIndex(0)];
    if (FLoadedSelectedTab > 0) then
      groupTabs.TabIndex := FLoadedSelectedTab
    else
    if (FLoadedSelectedTab = 0) and (FindGroupRealIndex(0) > 0) then
      groupTabsChange(groupTabs);

    if (aRowMem) and (aRowMemPriority) and (Length(FLastRowMem) > FindGroupRealIndex(groupTabs.TabIndex)) then
      taskGrid.Row := FLastRowMem[FindGroupRealIndex(groupTabs.TabIndex)]
    else
    if (FLoadedSelectedRow > 0) then
      taskGrid.Row := FLoadedSelectedRow;

    // Set current row to mem
    if (Length(FLastRowMem) > 0) then
    begin
      if (FirstTabRow >= 0) then
        FLastRowMem[FindGroupRealIndex(0)] := FirstTabRow;
      Index := FindGroupRealIndex(FLoadedSelectedTab);
      if (Index >= Low(FLastRowMem)) and (Index <= High(FLastRowMem)) then
        FLastRowMem[Index] := FLoadedSelectedRow;
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
    SetTags;
  end;

  if (memoNote.Visible) and (Showing) then
  begin
    // Restore memo note SelStart
    if (FLoadedMemoNoteSelStart > 0) then
    begin
      memoNote.SelStart := FLoadedMemoNoteSelStart;
      FLoadedMemoNoteSelStart := 0;
    end;

    // Restore memo note SelLength
    if (FLoadedMemoNoteSelLength > 0) then
    begin
      if memoNote.CanFocus then memoNote.SetFocus;
      memoNote.SelLength := FLoadedMemoNoteSelLength;
      FLoadedMemoNoteSelLength := 0;
    end;

    // Restore memo note scroll position
    if FLoadedMemoNoteScroll > 0 then
    begin
      MemoNoteSetScrollPosition(FLoadedMemoNoteScroll);
      FLoadedMemoNoteScroll := 0;
    end;
  end;

  GridInvalidate;
end;

procedure TformNotetask.GridAdjustScrollBars;
var
  totalWidth, visibleWidth: integer;
  newStyle: TScrollStyle;
begin
  // Guard: if already adjusting — exit to avoid recursion
  if FAdjustingScrollBars then Exit;
  FAdjustingScrollBars := True;
  try
    // Calculate widths
    totalWidth := taskGrid.GridWidth;
    visibleWidth := taskGrid.ClientWidth;

    if totalWidth > visibleWidth then
      newStyle := ssAutoBoth
    else
      newStyle := ssAutoVertical;

    // Only change when necessary (prevents extra events)
    if taskGrid.ScrollBars <> newStyle then
    begin
      taskGrid.ScrollBars := newStyle;

      // If we disabled horizontal scrollbar, try to hide native scrollbar
      if (newStyle = ssAutoVertical) and (taskGrid.HandleAllocated) then
      begin
        // Try to hide native horizontal scrollbar (widgetset dependent)
        ShowScrollBar(taskGrid.Handle, SB_HORZ, False);
      end;
    end;
  finally
    FAdjustingScrollBars := False;
  end;
end;

procedure TformNotetask.GridInvalidate;
begin
  Application.ProcessMessages;
  taskGrid.Invalidate;
  Application.ProcessMessages;
end;

function TformNotetask.FreeFile: boolean;
begin
  // Release the reserved file stream if it exists
  if Assigned(FSReserved) then
  begin
    try
      FSReserved.Free;
    except
      // Ignore any unexpected error during destruction
      Result := False;
    end;
    FSReserved := nil;
  end;
  Result := True;
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
    filterBox.BiDiMode := bdRightToLeft;
    memoNote.BiDiMode := bdRightToLeft;
    memoNote.Alignment := taRightJustify;
    memoNote.BorderSpacing.Left := 0;
    memoNote.BorderSpacing.Right := 10;
    tagsEdit.BiDiMode := bdRightToLeft;
    SetCursorTo(panelNote, 'RIGHTARROW');
  end
  else
  begin
    taskGrid.BiDiMode := bdLeftToRight;
    groupTabs.BiDiMode := bdLeftToRight;
    for i := 1 to taskGrid.Columns.Count - 1 do
      taskGrid.Columns[i].Alignment := taLeftJustify;
    filterBox.BiDiMode := bdLeftToRight;
    memoNote.BiDiMode := bdLeftToRight;
    memoNote.Alignment := taLeftJustify;
    memoNote.BorderSpacing.Left := 10;
    memoNote.BorderSpacing.Right := 0;
    memoNote.BiDiMode := bdLeftToRight;
    tagsEdit.BiDiMode := bdLeftToRight;
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
var
  LastTask: integer;
  LastTab: integer;
begin
  LastTask := Tasks.Map(taskGrid.Row);
  LastTab := FindGroupRealIndex(groupTabs.TabIndex);
  FShowArchived := Value;
  aShowArchived.Checked := FShowArchived;
  SetTabs;
  FillGrid;
  if (LastTab = FindGroupRealIndex(groupTabs.TabIndex)) then
    taskGrid.Row := Tasks.ReverseMap(LastTask);
  ResetRowHeight;
  SetInfo;
  SetNote;
  SetTags;
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

procedure TformNotetask.SetShowTags(Value: boolean);
begin
  FShowTags := Value;

  aShowTags.Checked := FShowTags;
  panelTags.Visible := FShowTags;
  SplitTags.Visible := FShowTags;

  AlignBottomControls;

  SetTags;
end;

procedure TformNotetask.SetShowNote(Value: boolean);
begin
  FShowNote := Value;

  aShowNote.Checked := FShowNote;
  panelNote.Visible := FShowNote;
  Splitter.Visible := FShowNote;

  AlignBottomControls;

  SetNote;
end;

procedure TformNotetask.SetHideNoteText(Value: boolean);
begin
  aHideNoteText.Checked := Value;
  FHideNoteText := Value;
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

  FLastRow := taskGrid.Row;
  SetNote;
  SetTags;
end;

procedure TformNotetask.ApplySortingActions;
begin
  aMoveTaskTop.Enabled := (not ReadOnly) and (SortColumn = 0);
  aMoveTaskBottom.Enabled := (not ReadOnly) and (SortColumn = 0);
  aMoveTaskUp.Enabled := (not ReadOnly) and (SortColumn = 0);
  aMoveTaskDown.Enabled := (not ReadOnly) and (SortColumn = 0);
  aMoveTaskLeft.Enabled := (not ReadOnly) and (SortColumn = 0);
  aMoveTaskRight.Enabled := (not ReadOnly) and (SortColumn = 0);

  if (SortColumn = 0) then
    taskGrid.Options := taskGrid.Options + [goRowMoving]
  else
    taskGrid.Options := taskGrid.Options - [goRowMoving];
end;

procedure TformNotetask.FillGrid;
begin
  DisableGridEvents;
  Tasks.FillGrid(taskGrid, FShowArchived, FShowDuration, FShowTime, SortOrder, SortColumn, FilterBox.Text);
  CalcRowHeight;
  EnableGridEvents;
end;

procedure TformNotetask.CalcDefaultColWidth;
begin
  if (FShowDuration) then
    taskGrid.DefaultColWidth := Round((Canvas.TextWidth('10.10sec') + 10) * FZoom)
  else
    taskGrid.DefaultColWidth := Round(Canvas.TextWidth('10000') * FZoom);
end;

procedure TformNotetask.CalcRowHeight(aRow: integer = 0; aForce: boolean = False);
var
  FromRow, ToRow: integer;

  procedure CalcCol(col: integer; force: boolean = False);
  var
    row: integer;
    drawrect: TRect;
    Text: string;
    task: TTask;
    flags: cardinal;
    h: integer;
  begin
    for row := FromRow to ToRow do
    begin
      drawrect := taskGrid.CellRect(col, row);
      drawrect.Inflate(-4, 0);

      Text := taskGrid.Cells[col, row];

      // Reduce text area by TagsWidth for text measurement
      if (col = 2) then
      begin
        task := Tasks.GetTask(row);
        if task.TagsWidth < drawrect.Width then
        begin
          if FBiDiRightToLeft then
            drawrect.Left := drawrect.Left + task.TagsWidth  // For RTL: reserve space on the left
          else
            drawrect.Right := drawrect.Right - task.TagsWidth; // For LTR: reserve space on the right
        end;
      end;

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
  taskGrid.RowHeights[0] := Round(Max(Canvas.TextHeight('A') + 4, taskGrid.DefaultRowHeight) * FZoom);
  if (aForce) then
  begin
    {$IFDEF UNIX}
    panelTabs.Height := Canvas.TextHeight('A') + 11;
    {$ELSE}
    panelTabs.Height := Canvas.TextHeight('A') + 8;
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
  taskGrid.BeginUpdate;
  try
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
      begin
        if taskGrid.RowHeights[i] <> h then
          taskGrid.RowHeights[i] := h;
      end;
    end
    else // if valid row just that row
      taskGrid.RowHeights[aRow] := taskGrid.DefaultRowHeight;

    if (Assigned(Memo)) and ((aRow = 0) or (aRow = taskGrid.Row)) then
      Memo.Height := h;

    if (aCalcRowHeight) then
      CalcRowHeight(aRow);
  finally
    taskGrid.EndUpdate;
  end;
end;

function TformNotetask.LastRowHeight(aRow: integer): integer;
begin
  if (Length(FLastRowHeights) > aRow) then
    Result := FLastRowHeights[aRow]
  else
    Result := taskGrid.DefaultRowHeight;
end;

procedure TformNotetask.ChangeLastText(const Value: string; aCol: integer = -1; aRow: integer = -1);
begin
  if aCol < 0 then aCol := taskGrid.Col;
  if aRow < 0 then aRow := taskGrid.Row;
  if (aCol > 0) and (aRow > 0) then
  begin
    if FDuplicateHighlight and ((FLastText <> string.Empty) or (Value <> string.Empty)) then
    begin
      FLastText := Value;
      if Tasks.HasDuplicateMatches(FLastText) and (taskGrid.Selection.Height = 0) then
      begin
        GridInvalidate;
        FLastTextMatch := True;
      end
      else
      begin
        if FLastTextMatch then
          GridInvalidate;
        FLastTextMatch := False;
      end;
    end;
  end
  else
    FLastText := string.Empty;
end;

procedure TformNotetask.SetChanged(aChanged: boolean = True);
begin
  if (aChanged = False) then
    taskGrid.Modified := False;

  FChanged := taskGrid.Modified or aChanged;
  aSave.Enabled := FChanged and not FReadOnly;
  aUndo.Enabled := FChanged;
  aUndoAll.Enabled := FChanged;
  SetCaption;
end;

procedure TformNotetask.SetCaption;
var
  NewCaption: string;
begin
  if (FFileName <> '') then
    NewCaption := ExtractFileName(FFileName) + ifthen(FEncrypted, ' (' + rencrypted + ')', string.Empty) +
      ifthen(FReadOnly, ' (' + rreadonly + ')', string.Empty) + ' - ' + rapp
  else
    NewCaption := runtitled + ifthen(FEncrypted, ' (' + rencrypted + ')', string.Empty) +
      ifthen(FReadOnly, ' (' + rreadonly + ')', string.Empty) + ' - ' + rapp;

  if FChanged then
    NewCaption := '*' + NewCaption;

  if Caption <> NewCaption then
  begin
    Caption := NewCaption;
    Application.Title := NewCaption;
  end;
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

  statusBar.Panels[0].Text := ' ' + IntToStr(Round(FZoom * 100)) + '%';

  if Assigned(FEncoding) then
  begin
    statusBar.Panels[1].Text := UpperCase(GetEncodingName(FEncoding));

    // Encoding menuZoomIn check
    contextANSI.Checked := FEncoding = TEncoding.ANSI;
    contextASCII.Checked := FEncoding = TEncoding.ASCII;
    contextUTF8.Checked := FEncoding = TEncoding.UTF8;
    contextUTF8BOM.Checked := FEncoding = UTF8BOMEncoding;
    contextUTF16BEBOM.Checked := FEncoding = UTF16BEBOMEncoding;
    contextUTF16LEBOM.Checked := FEncoding = UTF16LEBOMEncoding;
  end;
  if Assigned(FLineEnding) then
  begin
    statusBar.Panels[2].Text := FLineEnding.ToString;

    // Line ending menuZoomIn check
    contextWindowsCRLF.Checked := FLineEnding = TLineEnding.WindowsCRLF;
    contextUnixLF.Checked := FLineEnding = TLineEnding.UnixLF;
    contextMacintoshCR.Checked := FLineEnding = TLineEnding.MacintoshCR;
  end;

  // Task counts
  if (taskGrid.Selection.Height = 0) then
  begin
    CurAll := Tasks.CalcCount(ShowArchived, False, FilterBox.Text, 0, 0, FShowTime);
    CurDone := Tasks.CalcCount(ShowArchived, True, FilterBox.Text, 0, 0, FShowTime);
  end
  else
  begin
    CurAll := Tasks.CalcCount(ShowArchived, False, FilterBox.Text, taskGrid.Selection.Top, taskGrid.Selection.Bottom, FShowTime);
    CurDone := Tasks.CalcCount(ShowArchived, True, FilterBox.Text, taskGrid.Selection.Top, taskGrid.Selection.Bottom, FShowTime);
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
      SumAll := Tasks.CalcSum(ShowArchived, False, FilterBox.Text, 0, 0, FShowTime);
      SumDone := Tasks.CalcSum(ShowArchived, True, FilterBox.Text, 0, 0, FShowTime);
    end
    else
    begin
      SumAll := Tasks.CalcSum(ShowArchived, False, FilterBox.Text, taskGrid.Selection.Top, taskGrid.Selection.Bottom, FShowTime);
      SumDone := Tasks.CalcSum(ShowArchived, True, FilterBox.Text, taskGrid.Selection.Top, taskGrid.Selection.Bottom, FShowTime);
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
      DurationAll := Tasks.CalcDuration(ShowArchived, False, FilterBox.Text, 0, 0, FShowTime);
      DurationCurrent := Tasks.CalcDuration(ShowArchived, True, FilterBox.Text, 0, 0, FShowTime);
    end
    else
    begin
      DurationAll := Tasks.CalcDuration(ShowArchived, False, FilterBox.Text, taskGrid.Selection.Top, taskGrid.Selection.Bottom, FShowTime);
      DurationCurrent := Tasks.CalcDuration(ShowArchived, True, FilterBox.Text, taskGrid.Selection.Top,
        taskGrid.Selection.Bottom, FShowTime);
    end;
    if (DurationAll = DurationCurrent) or (DurationCurrent = string.Empty) then
      statusBar.Panels[5].Text := DurationAll
    else
      statusBar.Panels[5].Text := DurationCurrent + ' / ' + DurationAll;
  end
  else
    statusBar.Panels[5].Text := string.empty;
end;

procedure TformNotetask.SetTags;
var
  i: integer;
  tags, curtags: TStringList;
  HasDiff: boolean = False;
begin
  if (not ShowTags) then exit;

  tags := TStringList.Create;
  tags.Sorted := True;
  tags.Duplicates := dupIgnore;
  curtags := TStringList.Create;
  curtags.Sorted := True;
  curtags.Duplicates := dupIgnore;
  try
    if Assigned(Tasks) and (taskGrid.RowCount > 1) then
    begin
      if taskGrid.Selection.Height > 0 then
      begin
        // Multiple rows selected — concatenate tags and set read-only
        for i := taskGrid.Selection.Top to taskGrid.Selection.Bottom do
          if Tasks.Map(i) > -1 then
          begin
            curtags.Assign(Tasks.GetTask(i).Tags);
            tags.AddStrings(curtags);
            if (i > taskGrid.Selection.Top) and (not StringListsEqual(tags, curtags)) then
              HasDiff := True;
          end;
        if (tags.Count > 0) then
        begin
          if (not HasDiff) and (Tasks.Map(taskGrid.Selection.Top) > -1) then
            tagsEdit.Items.Assign(Tasks.GetTask(taskGrid.Selection.Top).Tags)
          else
            tagsEdit.Items.Assign(tags);
        end
        else
          tagsEdit.Items.Clear;
        tagsEdit.ReadOnly := FReadOnly;
        tagsEdit.AllowReorder := not HasDiff;
        tagsEdit.Color := clDefault;
      end
      else if Tasks.Map(taskGrid.Row) > -1 then
      begin
        // Single row selected — set editable tag
        tagsEdit.Items.Assign(Tasks.GetTask(taskGrid.Row).Tags);
        tagsEdit.ReadOnly := FReadOnly;
        tagsEdit.AllowReorder := True;
        tagsEdit.Color := clDefault;
      end
      else
      begin
        tagsEdit.Items.Clear;
        tagsEdit.ReadOnly := True;
      end;
    end
    else
    begin
      tagsEdit.Items.Clear;
      tagsEdit.ReadOnly := True;
    end;
  finally
    tagsEdit.ClearSelection;
    tags.Free;
  end;
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
        memoNote.ReadOnly := FReadOnly;
        memoNote.Color := clDefault;
      end
      else
      begin
        memoNote.Text := string.Empty;
        memoNote.ReadOnly := True;
      end;
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

procedure TformNotetask.SetFilter(FillTags: boolean = True);
var
  i: integer;
  SortedState: boolean;
begin
  if FillTags and (taskGrid.Row > 0) then
    Tasks.FillTags;
  SortedState := filterBox.Sorted;
  filterBox.Sorted := False;
  filterBox.Items.Assign(Tasks.Tags);
  tagsEdit.SuggestedItems := Tasks.Tags;

  // Remove ` from each item directly
  for i := 0 to filterBox.Items.Count - 1 do
    filterBox.Items[i] := StringReplace(filterBox.Items[i], '`', '', [rfReplaceAll]);
  filterBox.Sorted := SortedState;

  UpdateComboRegion(filterBox);
end;

procedure TformNotetask.SetReadOnly(Value: boolean);
begin
  FReadOnly := Value;
  aUndo.Enabled := not Value;
  aUndoAll.Enabled := not Value;
  aCut.Enabled := not Value;
  aPaste.Enabled := not Value;
  aDelete.Enabled := not Value;
  aReplace.Enabled := not Value;
  aDateTime.Enabled := not Value;
  aInsertGroup.Enabled := not Value;
  aRenameGroup.Enabled := not Value;
  aEditGroupTooltip.Enabled := not Value;
  aDuplicateGroup.Enabled := not Value;
  aDeleteGroup.Enabled := not Value;
  aMoveGroupLeft.Enabled := not Value;
  aMoveGroupRight.Enabled := not Value;
  aMoveTaskLeft.Enabled := not Value;
  aMoveTaskRight.Enabled := not Value;
  aInsertTask.Enabled := not Value;
  aMergeTasks.Enabled := not Value;
  aSplitTasks.Enabled := not Value;
  aDuplicateTasks.Enabled := not Value;
  aDeleteTasks.Enabled := not Value;
  aArchiveTasks.Enabled := not Value;
  aMoveTaskTop.Enabled := not Value;
  aMoveTaskUp.Enabled := not Value;
  aMoveTaskDown.Enabled := not Value;
  aMoveTaskBottom.Enabled := not Value;
  aIndentTasks.Enabled := not Value;
  aOutdentTasks.Enabled := not Value;
  contextDeleteTags.Enabled := not Value;
end;

procedure TformNotetask.SetZoom(Value: float);
begin
  FZoom := Value;
  taskGrid.Font.Assign(Font);
  taskGrid.Font.Size := Round(Max(1, FOriginalFontSize * FZoom));
  memoNote.Font.Assign(Font);
  memoNote.Font.Size := taskGrid.Font.Size;
  if Assigned(Memo) then
  begin
    Memo.Font.Assign(Font);
    Memo.Font.Size := taskGrid.Font.Size;
  end;
  if Assigned(DatePicker) then
  begin
    DatePicker.Font.Assign(Font);
    DatePicker.Font.Size := taskGrid.Font.Size;
  end;

  contextZoom50.Checked := SameFloat(FZoom, 0.5, 0.001);
  contextZoom60.Checked := SameFloat(FZoom, 0.6, 0.001);
  contextZoom70.Checked := SameFloat(FZoom, 0.7, 0.001);
  contextZoom80.Checked := SameFloat(FZoom, 0.8, 0.001);
  contextZoom90.Checked := SameFloat(FZoom, 0.9, 0.001);
  contextZoom100.Checked := SameFloat(FZoom, 1.0, 0.001);
  contextZoom110.Checked := SameFloat(FZoom, 1.1, 0.001);
  contextZoom120.Checked := SameFloat(FZoom, 1.2, 0.001);
  contextZoom130.Checked := SameFloat(FZoom, 1.3, 0.001);
  contextZoom140.Checked := SameFloat(FZoom, 1.4, 0.001);
  contextZoom150.Checked := SameFloat(FZoom, 1.5, 0.001);

  CalcRowHeight(0, True);
  SetInfo;
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
        if not Tasks.GetGroupFiltered(i, ShowArchived, filterBox.Text, FShowTime) then
        begin
          Clean.Add(Tasks.GroupNames[i].TrimLeft([' ', '#']).Trim);
          SetLength(FGroupIndexMap, Length(FGroupIndexMap) + 1);
          FGroupIndexMap[High(FGroupIndexMap)] := i;
        end;
      end;
    end;

    groupTabs.Tabs := Clean;
    SetTabsVisible;

    if (LastRealIndex < 0) and (FLastTabFilter >= 0) then LastRealIndex := FLastTabFilter;

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

procedure TformNotetask.SetTabsVisible;
begin
  panelTabs.Visible := (filterbox.Text <> string.Empty) or (filterBox.Focused) or
    (not ((groupTabs.Tabs.Count = 1) and (Tasks.GroupNames[0] = string.Empty)));
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
    if not ApplicationTranslate(Language) then
      Language := 'en';
  end;

  openDialog.Filter := ropendialogfilter;
  saveDialog.Filter := rsavedialogfilter;
  if Assigned(tagsEdit) then
  begin
    tagsEdit.RemoveConfirmMessage := rremovetag;
    tagsEdit.RemoveConfirmTitle := rremovetagtitle;
    tagsEdit.TextHint := renternewtag;
    tagsEdit.EditBox.Hint := renternewtaghint;
  end;

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

procedure TformNotetask.DisableDrag;
begin
  if FDragTab >= 0 then
  begin
    FDragTab := -1;
    Screen.Cursor := crDefault;
  end;
end;

procedure TformNotetask.DisableGridEvents;
begin
  taskGrid.OnSelectCell := nil;
  taskGrid.OnSelection := nil;
  taskGrid.OnSelectEditor := nil;
end;

procedure TformNotetask.EnableGridEvents;
begin
  taskGrid.OnSelectEditor := @taskGridSelectEditor;
  taskGrid.OnSelection := @taskGridSelection;
  taskGrid.OnSelectCell := @taskGridSelectCell;
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
        // Reset group rows memory
        FLastRowMem := CloneArray(FLoadedRowMem);
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
  try
    if (groupTabs.Visible) and (groupTabs.TabIndex >= 0) and (Length(FLastRowMem) > FindGroupRealIndex(groupTabs.TabIndex)) then
      FLastRowMem[FindGroupRealIndex(groupTabs.TabIndex)] := GetSelectedRow;
  except
    // just insure
  end;
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
  FDuplicateHighlight := False;
  Enabled := False;
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
      (WrapAround and (Counter > taskGrid.RowCount)) or (not formFindText.Visible and not formReplaceText.Visible);

    if (WrapAround and (Counter > taskGrid.RowCount)) then
      exit(NotFound);

    Result := True;
  finally
    FDuplicateHighlight := True;
    FFindActive := False;
    Enabled := True;
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
  Enabled := False;
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
    while (Find(aText, aMatchCase, aWrapAround, True, RowsChanged, True)) and (formReplaceText.Visible) do
    begin
      FDuplicateHighlight := False;

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
    Enabled := True;
    FDuplicateHighlight := True;
  end;
end;

end.
