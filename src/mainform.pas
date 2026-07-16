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
  uDateTimePicker,
  GridPrn,
  TagEdit,
  task,
  lineending,
  arrayhelpers;

type
  { TformNotetask }
  TformNotetask = class(TForm)
    {%Region -fold Vars}
    aArchiveTasks: TAction;
    aAbout: TAction;
    aCopy: TAction;
    aCheckforupdates: TAction;
    aAutoCheckUpdates: TAction;
    aLangTurkish: TAction;
    aLangGreek: TAction;
    aLangHebrew: TAction;
    aLangIndonesian: TAction;
    aLangPolish: TAction;
    aLangRomanian: TAction;
    aLangSwedish: TAction;
    aLangCzech: TAction;
    aLangDanish: TAction;
    aLangDutch: TAction;
    aLangFinnish: TAction;
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
    FilterBox: TComboBox;
    filterClear: TSpeedButton;
    fontDialog: TFontDialog;
    TabsGroup: TTabControl;
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
    ImagesBtn: TImageList;
    menuPolish: TMenuItem;
    menuRomanian: TMenuItem;
    menuSwedish: TMenuItem;
    menuTurkish: TMenuItem;
    menuAutoCheckUpdates: TMenuItem;
    menuCzech: TMenuItem;
    menuDanish: TMenuItem;
    menuDutch: TMenuItem;
    menuFinnish: TMenuItem;
    menuGreek: TMenuItem;
    menuHebrew: TMenuItem;
    menuIndonesian: TMenuItem;
    menuZoomIn: TMenuItem;
    menuZoomOut: TMenuItem;
    menuDefaultZoom: TMenuItem;
    menuZoom: TMenuItem;
    ImagesMisc: TImageList;
    MainMenu: TMainMenu;
    MemoNote: TMemo;
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
    menuEditGroupTooltip: TMenuItem;
    contextEditGroupTooltip: TMenuItem;
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
    panelFunc: TPanel;
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
    PanelTags: TScrollBox;
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
    btnMulti: TSpeedButton;
    SplitFilter: TSplitter;
    SplitTags: TSplitter;
    statusBar: TStatusBar;
    Grid: TStringGrid;
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
    ImagesTitle: TImageList;
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
    contextInsertGroup: TMenuItem;
    contextInsertTask: TMenuItem;
    contextRenameGroup: TMenuItem;
    contextDuplicateGroup: TMenuItem;
    contextDeleteGroup: TMenuItem;
    {%EndRegion}
    {%Region -fold Events}
    // Form Events
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    // Application Events
    procedure ApplicationOnException(Sender: TObject; E: Exception);
    procedure ApplicationOnQueryEndSession(var CanEnd: boolean);
    procedure ApplicationOnShowHint(var HintStr: string; var CanShow: boolean; var HintInfo: THintInfo);
    // Actions Events
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
    procedure aSaveNotesAsExecute(Sender: TObject);
    procedure aAutoCheckUpdatesExecute(Sender: TObject);
    procedure aCheckforupdatesExecute(Sender: TObject);
    procedure aEditGroupTooltipExecute(Sender: TObject);
    procedure aFilterExecute(Sender: TObject);
    procedure aLangCzechExecute(Sender: TObject);
    procedure aLangDanishExecute(Sender: TObject);
    procedure aLangDutchExecute(Sender: TObject);
    procedure aLangFinnishExecute(Sender: TObject);
    procedure aLangGreekExecute(Sender: TObject);
    procedure aLangHebrewExecute(Sender: TObject);
    procedure aLangIndonesianExecute(Sender: TObject);
    procedure aLangPolishExecute(Sender: TObject);
    procedure aLangRomanianExecute(Sender: TObject);
    procedure aLangSwedishExecute(Sender: TObject);
    procedure aLangTurkishExecute(Sender: TObject);
    procedure aShowTagsExecute(Sender: TObject);
    procedure aSplitTasksExecute(Sender: TObject);
    procedure aZoomDefaultExecute(Sender: TObject);
    procedure aZoomInExecute(Sender: TObject);
    procedure aZoomOutExecute(Sender: TObject);
    // Context Menu Events
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
    // Memo Note Events
    procedure MemoNoteDblClick(Sender: TObject);
    procedure MemoNoteEnter(Sender: TObject);
    procedure MemoNoteExit(Sender: TObject);
    procedure MemoNoteChange(Sender: TObject);
    procedure MemoNoteKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure MemoNoteMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure MemoNoteKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure MemoNoteMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure MemoNoteMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure panelNoteMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure panelNoteMouseEnter(Sender: TObject);
    procedure panelNoteMouseLeave(Sender: TObject);
    procedure panelNoteMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure panelNoteMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    // TagEdit Events
    procedure TagEditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure TagEditTagClick(Sender: TObject; const TagText: string; const TagIndex: integer);
    procedure TagEditBeforeChange(Sender: TObject; Tags: string; Operation: TTagEditOperation; var AllowChange: boolean);
    procedure TagEditChange(Sender: TObject);
    procedure TagEditTagAdd(Sender: TObject; const TagText: string; const TagIndex: integer);
    procedure TagEditTagRemove(Sender: TObject; const TagText: string; const TagIndex: integer);
    procedure TagEditTagReorder(Sender: TObject; const TagText: string; const NewIndex: integer);
    procedure TagEditExit(Sender: TObject);
    // Tabs Group Events
    procedure TabsGroupChange(Sender: TObject);
    procedure TabsGroupMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure TabsGroupMouseLeave(Sender: TObject);
    procedure TabsGroupMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure TabsGroupMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    // Grid Events
    procedure GridCheckboxToggled(Sender: TObject; aCol, aRow: integer; aState: TCheckboxState);
    procedure GridColRowDeleted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
    procedure GridColRowInserted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure GridHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure GridHeaderSized(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure GridKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure GridMouseLeave(Sender: TObject);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure GridMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure GridResize(Sender: TObject);
    procedure GridSelectCell(Sender: TObject; aCol, aRow: integer; var CanSelect: boolean);
    procedure GridSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
    procedure GridTopLeftChanged(Sender: TObject);
    procedure GridUserCheckboxBitmap(Sender: TObject; const aCol, aRow: integer; const CheckedState: TCheckboxState;
      var ABitmap: TBitmap);
    procedure GridColRowMoved(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
    procedure GridSetCheckboxState(Sender: TObject; ACol, ARow: integer; const Value: TCheckboxState);
    procedure GridSelection(Sender: TObject; aCol, aRow: integer);
    procedure GridUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    // All Events
    procedure btnMultiClick(Sender: TObject);
    procedure FilterBoxChange(Sender: TObject);
    procedure FilterBoxKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure filterClearClick(Sender: TObject);
    procedure SplitFilterChangeBounds(Sender: TObject);
    procedure statusBarContextPopup(Sender: TObject; MousePos: TPoint; var Handled: boolean);
    {%EndRegion}
  private
    {%Region -fold Private Vars}
    Memo: TMemo;
    PanelMemo: TPanel;
    DatePicker: TDateTimePicker;
    TagEdit: TTagEdit;
    FChanged: boolean;
    FBackup: boolean;
    FReadOnly: boolean;
    FFormSettingsLoaded: boolean;
    FGridSettingsLoaded: boolean;
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
    FAutoCheckUpdates: boolean;
    FSortOrder: TSortOrder;
    FSortColumn: integer;
    FMatchCase: boolean;
    FWrapAround: boolean;
    FBiDiRightToLeft: boolean;
    FFindActive: boolean;
    FFindF3: boolean;
    FFindText: string;
    FFoundText: string;
    FLastGridSelection: TRect;
    FLastGridRow, FLastGridCol: integer;
    FLastSelectionHeight: integer;
    FLastSelection: TGridRect;
    FLastFoundRow, FLastFoundCol, FLastFoundSelStart, FLastFoundSelLength: integer;
    FLastRowHeights: array of integer;
    FLastRow, FLastCol: integer;
    FLastText: string;
    FLastFilter: string;
    FLastTextMatch: boolean;
    FLastRowMem: TIntegerArray;
    FLastTabMouseX: integer;
    FLastTabTarget: integer;
    FLastTabFilter: integer;
    FLastTabIndex: integer;
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
    FFitRowHeightToFont: boolean;
    {%EndRegion}
    {%Region -fold Private Mathods}
    procedure EditControlSetBounds(Sender: TWinControl; aCol, aRow: integer; OffsetLeft: integer = 4;
      OffsetTop: integer = 0; OffsetRight: integer = -8; OffsetBottom: integer = -1);
    procedure UpdateComboRegion(Combo: TComboBox; AInsetLeft: integer = 1; AInsetTop: integer = 1;
      AInsetRight: integer = 0; AInsetBottom: integer = 1);
    procedure PrinterPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    procedure PrinterBeforePrintCell(Sender: TObject; AGrid: TCustomGrid; ACanvas: TCanvas; ACol, ARow: integer; ARect: TRect);
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
    procedure MemoExit(Sender: TObject);
    procedure MemoChange(Sender: TObject);
    procedure MemoKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure MemoKeyPress(Sender: TObject; var Key: char);
    procedure DatePickerEnter(Sender: TObject);
    procedure DatePickerChange(Sender: TObject);
    procedure DatePickerKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure EditCell(aCol: integer = -1; aRow: integer = -1);
    procedure EditComplete(aEnter: boolean = False; aEscape: boolean = False);
    procedure DisableDrag;
    procedure DisableGridEvents;
    procedure EnableGridEvents;
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
    function TryOpenAsUrl(Value: string): boolean;
    procedure ExecuteTerminal(usePowershell: boolean = True);
    procedure MoveTabLeft(Index: integer);
    procedure MoveTabRight(Index: integer);
    procedure ChangeGroup(Index: integer);
    procedure CalcDefaultColWidth;
    procedure ResetRowHeight(aCalcRowHeight: boolean = True; aRow: integer = 0);
    procedure CalcRowHeight(aForce: boolean = False; aRow: integer = 0);
    procedure SwapRowHeights(RowIndex1, RowIndex2: integer);
    procedure BackupSelectedState(aRowMem: boolean = False);
    procedure RestoreSelectedState(aRowMem: boolean = True; aRowMemPriority: boolean = True; aFocusMemo: boolean = False);
    procedure GridAdjustScrollBars;
    procedure GridInvalidate;
    procedure AdjustMultiButton;
    procedure TagsAdd(const Rect: TRect; const TagText: string);
    function FreeFile: boolean;
    function LastRowHeight(aRow: integer): integer;
    procedure ChangeLastText(Value: string = string.Empty; aCol: integer = -1; aRow: integer = -1);
    function GetScrollPosition: integer;
    function GetIsEditing: boolean;
    function IsCanClose: boolean;
    procedure CorrectGridSelection;
    function GetSelectedTab: integer;
    function GetSelectedRow: integer;
    function GetSelection: TRect;
    function GetSelectedRows: TIntegerArray;
    function GetMemoNoteScroll: integer;
    function GetMemoNoteSelStart: integer;
    function GetMemoNoteSelLength: integer;
    procedure DelayedSetMemoFocus(Data: PtrInt);
    procedure DelayedFinishTagEdit(Data: PtrInt);
    procedure DelayedInvalidate(Data: PtrInt);
    procedure FixDatePickerFont(Data: PtrInt);
    procedure AlignBottomControls;
    procedure SetLanguage(aLanguage: string = string.Empty);
    procedure FillGrid;
    procedure NewFile(SaveSetting: boolean = True);
    function OpenFile(fileName: string; saveSettings: boolean = True; ShowTrigger: boolean = False): boolean;
    function SaveFile(fileName: string = string.Empty; saveAs: boolean = False; encrypt: boolean = False): boolean;
    function SaveFileAs: boolean;
    procedure ApplyGridSettings;
    {%EndRegion}
    {%Region -fold Private Setters}
    procedure SetChanged(Value: boolean);
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
    {%EndRegion}
  public
    {%Region -fold Public Vars}
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
    {%EndRegion}
    {%Region -fold Public Methods}
    function Find(aText: string; aMatchCase, aWrapAround, aDirectionDown: boolean; Silent: boolean = False): boolean; overload;
    function Find(aText: string; aMatchCase, aWrapAround, aDirectionDown: boolean; out aRowsChanged: integer; Silent: boolean): boolean;
      overload;
    function Replace(aText, aToText: string; aMatchCase, aWrapAround: boolean): boolean;
    function ReplaceAll(aText, aToText: string; aMatchCase, aWrapAround: boolean): boolean;
    {%EndRegion}
    {%Region -fold Public Properties}
    property Changed: boolean read FChanged write SetChanged;
    property Zoom: float read FZoom write SetZoom;
    property ReadOnly: boolean read FReadOnly write SetReadOnly;
    property WordWrap: boolean read FWordWrap write FWordWrap;
    property EnterSubmit: boolean read FEnterSubmit write FEnterSubmit;
    property AutoCheckUpdates: boolean read FAutoCheckUpdates write FAutoCheckUpdates;
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
    property FitRowHeightToFont: boolean read FFitRowHeightToFont write FFitRowHeightToFont;
    {%EndRegion}
  end;

var
  formNotetask: TformNotetask;
  Tasks: TTasks; // Tasks collection
  ResourceBitmapCheck: TBitmap;
  ResourceBitmapUncheck: TBitmap;
  ResourceBitmapStarGold: TBitmap;
  ResourceBitmapStarGray: TBitmap;

  {%Region -fold Resource String}

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

  {%EndRegion}

implementation

uses Consts, mathparser, filemngr, settings, controlshelper, cryptoutils, stringgridhelper, forminput, formmemo, formfind,
  formreplace, formabout, formdonate, osutils, stringhelper, stringshelper, darkutils, localize, checkupdates, hotkeyhelper,
  pascalutils;

  {$R *.lfm}

  {%Region -fold Form Events}

procedure TformNotetask.FormCreate(Sender: TObject);
begin
  // Init components
  TagEdit := TTagEdit.Create(Self);
  TagEdit.Parent := PanelTags;
  TagEdit.AutoSuggest := True;
  TagEdit.Align := alTop;
  TagEdit.AutoSizeHeight := True;
  TagEdit.DragIndicatorColor := clRed;
  TagEdit.SelectionRectColor := clSilver;
  TagEdit.TagHoverColor := clNone;
  TagEdit.TagSuffixColor := TDarkUtils.ThemeColor(clTagSuffix_Light, clTagSuffix_Dark);
  TagEdit.RoundCorners := 20;
  TagEdit.TagHeightFactor := 2;
  TagEdit.AutoColorSeed := 14;
  TagEdit.EditMinWidth := 150;
  TagEdit.AutoColorBrigtness := TagsColorBrigtness;
  TagEdit.AutoColorSaturation := TagsColorSaturation;
  TagEdit.BackSpaceEditTag := True;
  TagEdit.ShowHint := True;
  TagEdit.SuggestedButtonCaption := string.Empty;
  ImagesMisc.GetBitmap(TDarkUtils.ThemeValue(0, 1), TagEdit.SuggestedButtonGlyph);
  TagEdit.PopupMenu := PopupTags;
  TagEdit.OnKeyDown := @TagEditKeyDown;
  TagEdit.OnTagClick := @TagEditTagClick;
  TagEdit.OnBeforeChange := @TagEditBeforeChange;
  TagEdit.OnChange := @TagEditChange;
  TagEdit.OnTagAdd := @TagEditTagAdd;
  TagEdit.OnTagRemove := @TagEditTagRemove;
  TagEdit.OnTagReorder := @TagEditTagReorder;
  TagEdit.OnExit := @TagEditExit;

  // Initialize variables
  FZoom := 1;
  FBackup := True;
  FReadOnly := False;
  FWordWrap := True;
  FEnterSubmit := True;
  FAutoCheckUpdates := True;
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
  FSortColumn := COL_NUM;
  FMemoSelStartClicked := -1;
  FLastTabFilter := -1;
  FLastTextMatch := False;
  FSortOrder := soAscending;
  FKeyPressed := string.Empty;
  FEncrypted := False;
  TCrypto.FreeBytesSecure(FKeyEnc);
  TCrypto.FreeBytesSecure(FKeyAuth);
  TCrypto.FreeBytesSecure(FSalt);
  openDialog.Filter := ropendialogfilter;
  saveDialog.Filter := rsavedialogfilter;
  FFitRowHeightToFont := False;

  // Set colors
  Self.Color := clWindow;
  Grid.GridLineColor := TDarkUtils.ThemeColor(clGridLineColor_Light, clGridLineColor_Dark);
  Grid.FixedHotColor := TDarkUtils.ThemeColor(clSplitHighlight_Light, clSplitHighlight_Dark);
  panelNote.Color := TDarkUtils.ThemeColor(clSpit_Light, clSplit_Dark);
  Splitter.Color := TDarkUtils.ThemeColor(clSpit_Light, clSplit_Dark);
  SplitTags.Color := TDarkUtils.ThemeColor(clSpit_Light, clSplit_Dark);
  SplitFilter.Color := TDarkUtils.ThemeColor(clSplitFilter_Light, clSplitFilter_Dark);

  // Remove standart border
  UpdateComboRegion(FilterBox);

  Application.OnException := @ApplicationOnException;
  Application.OnQueryEndSession := @ApplicationOnQueryEndSession;
  Application.OnShowHint := @ApplicationOnShowHint;

  Grid.DefaultRowHeight := iif(FFitRowHeightToFont, Grid.Canvas.TextHeight('Wg') + 2, DefRowHeight);

  // Create TBitmap objects
  ResourceBitmapCheck := TBitmap.Create;
  ResourceBitmapUncheck := TBitmap.Create;
  ResourceBitmapStarGold := TBitmap.Create;
  ResourceBitmapStarGray := TBitmap.Create;

  // Load bitmaps from resources
  ResourceBitmapCheck.LoadFromResourceName(HInstance, 'CHECK');
  ResourceBitmapCheck.TransparentColor := clFuchsia;
  ResourceBitmapCheck.Transparent := True;
  ResourceBitmapUncheck.LoadFromResourceName(HInstance, 'UNCHECK');
  ResourceBitmapUncheck.TransparentColor := clFuchsia;
  ResourceBitmapUncheck.Transparent := True;
  ResourceBitmapStarGold.LoadFromResourceName(HInstance, 'STARGOLD');
  ResourceBitmapStarGold.TransparentColor := clFuchsia;
  ResourceBitmapStarGold.Transparent := True;
  ResourceBitmapStarGray.LoadFromResourceName(HInstance, 'STARGRAY');
  ResourceBitmapStarGray.TransparentColor := clFuchsia;
  ResourceBitmapStarGray.Transparent := True;

  FFormSettingsLoaded := LoadFormSettings(Self, TagEdit);
  FGridSettingsLoaded := LoadGridSettings(Self, Grid, string.Empty);

  // After load settings
  aWordWrap.Checked := FWordWrap;
  MemoNote.WordWrap := FWordWrap;
  aEnterSubmit.Checked := FEnterSubmit;
  aAutoCheckUpdates.Checked := FAutoCheckUpdates;
  aBidiRightToLeft.Checked := FBiDiRightToLeft;
  aShowArchived.Checked := FShowArchived;
  ShowTags := FShowTags;
  ShowNote := FShowNote;
  ShowStatusBar := FShowStatusBar;
  ShowTime := FShowTime;
  HideNoteText := FHideNoteText;

  // Zoom
  FOriginalFontSize := ifthen(Font.Size > 0, Font.Size, Screen.SystemFont.Size);
  if FOriginalFontSize = 0 then
  begin
    FOriginalFontSize := DefFontSize;
    {$IFDEF UNIX}
    Self.Font.Size := DefFontSize;
    {$ENDIF}
  end;

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
  if FFormSettingsLoaded then
    SaveFormSettings(Self, TagEdit);
  if FGridSettingsLoaded then
    SaveGridSettings(Self, Grid, ExtractFileName(FFileName));

  // Free allocated resources
  Tasks.Free;
  ResourceBitmapCheck.Free;
  ResourceBitmapUncheck.Free;
  ResourceBitmapStarGold.Free;
  ResourceBitmapStarGray.Free;

  FreeFile;

  TagEdit.Free;
end;

procedure TformNotetask.FormShow(Sender: TObject);
var
  FilePath: string;
  FileOpened: boolean;
  TagsHeight: integer;
  Th: TCheckUpdateThread;
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

  // Save panel height as it gets cleared during restore on Linux
  TagsHeight := PanelTags.Height;
  RestoreSelectedState(True, True, True);
  PanelTags.Height := TagsHeight;

  Tasks.CalcTagsWidths(-1, Grid.Columns[COL_TASK - 1].Width, TagEdit, Font);
  SetZoom(FZoom);
  CorrectGridSelection;

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
  AdjustMultiButton;

  if (ReadOnly) then ShowMessage(rfilereadonly);

  // Check new version if needed
  if AutoCheckUpdates then
  begin
    Th := TCheckUpdateThread.Create(REPO, APP_NAME, False);
    Th.FreeOnTerminate := True;
  end;
end;

procedure TformNotetask.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := IsCanClose;
end;

procedure TformNotetask.FormResize(Sender: TObject);
begin
  GridResize(Sender);

  AlignBottomControls;
end;

procedure TformNotetask.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if MemoNote.Focused or FilterBox.Focused or TagEdit.Focused then
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
        EditCell(Grid.Col, Grid.Row);
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
    EditComplete;
    ChangeGroup(Key - VK_0 - 1);
    Key := 0;
  end
  else
  if not (ssCtrl in Shift) and not (ssShift in Shift) and (Key = VK_ADD) and (TabsGroup.Tabs.Count > 1) and (not IsEditing) then // NUMPAD+
  begin
    EditComplete;
    ChangeGroup(TabsGroup.TabIndex + 1);
    Key := 0;
  end
  else if not (ssCtrl in Shift) and not (ssShift in Shift) and (Key = VK_SUBTRACT) and (TabsGroup.Tabs.Count > 1) and (not IsEditing) then
    // NUMPAD-
  begin
    EditComplete;
    ChangeGroup(TabsGroup.TabIndex - 1);
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
  if ((Shift = [ssCtrl, ssShift]) and (Key = VK_LEFT)) or (((Shift = [ssCtrl]) or (Shift = [ssShift])) and (Key = VK_SUBTRACT)) then
    // Ctrl + Shift + Left, Ctrl | Shift + NUMPAD-
  begin
    aMoveGroupLeft.Execute;
    Key := 0;
  end
  else
  if ((Shift = [ssCtrl, ssShift]) and (Key = VK_RIGHT)) or (((Shift = [ssCtrl]) or (Shift = [ssShift])) and (Key = VK_ADD)) then
    // Ctrl + Shift + Right, Ctrl | Shift + NUMPAD+
  begin
    aMoveGroupRight.Execute;
    Key := 0;
  end
  else
  if (Shift = [ssCtrl, ssShift]) and (Key = VK_UP) then // Ctrl + Shift + Up
  begin
    if IsEditing then
    begin
      EditComplete;
      if Grid.Row > 0 then
        Grid.Row := Grid.Row - 1;
      Key := 0;
    end;
  end
  else
  if (Shift = [ssCtrl, ssShift]) and (Key = VK_DOWN) then // Ctrl + Shift + Down
  begin
    if IsEditing then
    begin
      EditComplete;
      if (Grid.Row < Grid.RowCount - 1) then
        Grid.Row := Grid.Row + 1;
      Key := 0;
    end;
  end
  else
  if (Key in [VK_SPACE]) then // Space
  begin
    if (not IsEditing) and (Grid.Focused) then
    begin
      if (not Grid.Columns[COL_DONE - 1].Visible) or (Grid.Col = COL_STAR) then
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
      EditComplete(False, True)
    else
      Grid.ClearSelections;
    Key := 0;
  end
  else
  if (Key = VK_RETURN) then // Enter
  begin
    if IsEditing then
    begin
      if (Grid.Col in [COL_AMOUNT, COL_DATE]) or ((Grid.Col in [COL_TASK, COL_NOTE]) and
        ((FEnterSubmit and (Shift = [])) or (not FEnterSubmit and ((Shift = [ssCtrl]) or (Shift = [ssShift]))))) then
      begin
        EditComplete(True);
        Key := 0;
      end
      else
      if (Grid.Col in [COL_TASK, COL_NOTE]) and (FEnterSubmit) and (Shift = [ssCtrl]) then
      begin
        Memo.SelText := sLineBreak;
        Key := 0;
      end;
    end
    else
    begin
      if (Grid.Col in [COL_TASK, COL_NOTE, COL_AMOUNT]) then
        FMemoNeedSelectAll := False
      else
      if (Grid.Col = COL_DONE) then
        CompleteTasks
      else
      if (Grid.Col = COL_STAR) and (Grid.Selection.Height = 0) and (Grid.Selection.Width = 0) then
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

procedure TformNotetask.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  // Ignore drops from the memo itself
  if (Screen.ActiveControl = Memo) then
    Exit;

  if Length(FileNames) > 0 then
  begin
    if not FileExists(FileNames[0]) then Exit;

    if IsCanClose then
      OpenFile(FileNames[0]);
  end;
end;

{%EndRegion}

{%Region -fold Application Events}

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
    TabIndex := TabsGroup.IndexOfTabAt(HintInfo.CursorPos.X, 5);

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

{%EndRegion}

{%Region -fold Action Events}

procedure TformNotetask.aNewExecute(Sender: TObject);
begin
  NewFile;
end;

procedure TformNotetask.aNewWindowExecute(Sender: TObject);
var
  Process: TProcess;
begin
  if Screen.ActiveForm <> Self then exit;

  if FFormSettingsLoaded then
    SaveFormSettings(self, TagEdit); // Save setting for new process

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
  if FilterBox.Focused then exit;
  if MemoNote.Focused then
  begin
    MemoNoteUndo;
    exit;
  end
  else
  if TagEdit.Focused and not TagEdit.ReadOnly and TagEdit.EditBox.CanUndo then
  begin
    TagEdit.EditBox.Undo;
    exit;
  end;

  if Screen.ActiveForm <> Self then exit;

  if not IsEditing then
  begin
    TempTopRow := Grid.TopRow;
    TempRect := FLastGridSelection;
    TempLastRow := FLastGridRow;
    TempLastCol := FLastGridCol;
    GridBackupSelection;

    Tasks.UndoBackup;

    FillGrid;
    ResetRowHeight;
    Grid.Col := TempLastCol;
    if (TempLastRow > 1) then
      Grid.Row := TempLastRow;
    if (TempRect.Width > 0) or (TempRect.Height > 0) then
      Grid.Selection := TRect.Create(TempRect.Left, TempRect.Top, TempRect.Right, TempRect.Bottom);
    Grid.TopRow := TempTopRow;
    ChangeLastText;
    SetFilter;
    SetInfo;
    SetNote;
    SetTags;
  end
  else
  if (Grid.InplaceEditor.InheritsFrom(TPanel)) then
    MemoUndo; //(Grid.InplaceEditor as TCustomEdit).Undo;
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
      SetFilter;
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
  if FilterBox.Focused then exit;
  if MemoNote.Focused then
  begin
    MemoNoteBackup;
    MemoNote.CutToClipboard;
    exit;
  end
  else
  if TagEdit.Focused then
  begin
    TagEdit.EditBox.CutToClipboard;
    exit;
  end;

  if Screen.ActiveForm <> Self then exit;
  if Grid.RowCount < 2 then exit;

  if not IsEditing then
  begin
    Tasks.CopyToClipboard(Grid);
    if (Grid.Selection.Width < 3) then
      ClearSelected(False)
    else
      DeleteTasks(False);
  end
  else
  if (Grid.InplaceEditor.InheritsFrom(TPanel)) then
  begin
    MemoBackup;
    Memo.CutToClipboard;
  end;
end;

procedure TformNotetask.aCopyExecute(Sender: TObject);
begin
  if FilterBox.Focused then exit;
  if MemoNote.Focused then
  begin
    MemoNote.CopyToClipboard;
    exit;
  end
  else
  if TagEdit.SelectedTags.Count > 0 then
  begin
    Clipboard.AsText := TagEdit.SelectedTags.DelimitedText;
    exit;
  end
  else
  if TagEdit.HoveredTag <> string.Empty then
  begin
    Clipboard.AsText := TagEdit.HoveredTag;
    exit;
  end
  else
  if TagEdit.EditBox.Focused then
  begin
    TagEdit.EditBox.CopyToClipboard;
    exit;
  end;

  if Screen.ActiveForm <> Self then exit;
  if Grid.RowCount < 2 then exit;

  if not IsEditing then
    Tasks.CopyToClipboard(Grid, FShowNote)
  else
  if (Grid.InplaceEditor.InheritsFrom(TPanel)) then
    Memo.CopyToClipboard;
end;

procedure TformNotetask.aPasteExecute(Sender: TObject);
var
  Sel: TGridRect;
begin
  if FilterBox.Focused then exit;
  if MemoNote.Focused then
  begin
    if (not MemoNote.ReadOnly) then
    begin
      MemoNoteBackup;
      PasteWithLineEnding(MemoNote);
    end;
    exit;
  end
  else
  if TagEdit.Focused then
  begin
    if not TagEdit.ReadOnly then
      TagEdit.EditBox.PasteFromClipboard;
    exit;
  end;

  if Screen.ActiveForm <> Self then exit;

  if not IsEditing then
  begin
    Sel := Tasks.PasteFromClipboard(Grid, SortOrder);
    FillGrid;
    CalcRowHeight(True);
    if (Assigned(DatePicker)) then
      DatePicker.DateTime := Tasks.GetTask(Grid.Row).Date;
    if (SortColumn = COL_NUM) then
      Grid.Selection := Sel;
    Changed := True;
    SetInfo;
    SetNote;
    SetTags;
    SetFilter;
  end
  else
  if (Grid.InplaceEditor.InheritsFrom(TPanel)) then
  begin
    MemoBackup;
    PasteWithLineEnding(Memo);
  end;
end;

procedure TformNotetask.aDeleteExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if FilterBox.Focused then exit;

  if MemoNote.Focused then
  begin
    if (not MemoNote.ReadOnly) then
    begin
      MemoNoteBackup;
      MemoNote.ClearSelection;
    end;
    exit;
  end
  else
  if TagEdit.Focused then
  begin
    {$IFDEF UNIX}
    if not TagEdit.ReadOnly then
    begin
      if TagEdit.EditBox.SelLength = 0 then
        TagEdit.EditBox.SelLength := CalcDeleteCount(TagEdit.EditBox.Text, TagEdit.EditBox.SelStart);
      TagEdit.EditBox.ClearSelection;
    end;
    {$ENDIF}
    exit;
  end;

  if Grid.RowCount < 2 then exit;
  if not IsEditing then
  begin
    ClearSelected(False);
    if ShowDuration then FillGrid;
    SetInfo;
    SetNote;
    SetTags;
  end
  else
  if (Grid.InplaceEditor is TPanel) then
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
  if FilterBox.Focused then exit;
  if MemoNote.Focused then
  begin
    MemoNote.SelStart := 0;
    MemoNote.SelLength := Length(MemoNote.Text);
    exit;
  end
  else
  if TagEdit.Focused then
  begin
    TagEdit.EditBox.SelectAll;
    exit;
  end;

  if Screen.ActiveForm <> Self then exit;
  if Grid.RowCount < 2 then exit;

  if not IsEditing then
  begin
    Grid.Selection := TGridRect.Create(COL_NUM, 0, COL_STAR, Grid.RowCount);
    FLastSelectionHeight := Grid.Selection.Height;
    SetInfo;
    SetNote;
    SetTags;
  end
  else
  if (Grid.InplaceEditor.InheritsFrom(TPanel)) then
  begin
    Memo.SelStart := 0;
    Memo.SelLength := Length(Memo.Text);
  end;
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
    FOriginalFontSize := ifthen(Font.Size > 0, Font.Size, Screen.SystemFont.Size);
    if FOriginalFontSize = 0 then
    begin
      FOriginalFontSize := DefFontSize;
      {$IFDEF UNIX}
      Self.Font.Size := DefFontSize;
      {$ENDIF}
    end;
    SetZoom(FZoom);
  end;
end;

procedure TformNotetask.aInsertTaskExecute(Sender: TObject);
var
  Ind: integer;
  TaskText, Oper, Value: string;
begin
  if Screen.ActiveForm <> Self then exit;
  if TabsGroup.Tabs.Count = 0 then exit;

  EditComplete;
  GridBackupSelection;

  TaskText := '[ ]';
  if Length(FilterBox.Text) > 0 then
  begin
    string(FilterBox.Text).StartsWithOperator(Oper, Value);
    if (Length(Oper) = 0) or (Oper = '#') or (Oper = '=') then
    begin
      if Trim(Value) <> string.Empty then
        TaskText += ' `' + Trim(Value) + '`'
      else
      if Value <> string.Empty then
        TaskText += ' ' + Value;
    end;
  end;

  Ind := Tasks.InsertTask(TaskText, Grid.Row);
  FLastText := string.Empty;
  FillGrid;
  ResetRowHeight;
  if (Ind > 0) then
    Grid.Row := Tasks.ReverseMap(Ind)
  else
    Grid.Row := Grid.Row + 1;

  if Visible and Grid.Visible and Grid.CanFocus then
    Grid.SetFocus;
  AdjustMultiButton;
  ResetRowHeight;
  SetTabs;
  SetInfo;
  Changed := True;
  SetNote;
  SetTags;
end;

procedure TformNotetask.aDuplicateTasksExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if Grid.RowCount < 2 then exit;

  DuplicateTasks;
end;

procedure TformNotetask.aMergeTasksExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if Grid.RowCount < 2 then exit;

  MergeTasks;
end;

procedure TformNotetask.aSplitTasksExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if Grid.RowCount < 2 then exit;

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
var
  LatestVersion: string;
begin
  CheckGithubLatestVersion(LatestVersion, REPO, APP_NAME);
end;

procedure TformNotetask.aAutoCheckUpdatesExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;

  FAutoCheckUpdates := aAutoCheckUpdates.Checked;
end;

procedure TformNotetask.aDeleteTasksExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if Grid.RowCount < 2 then exit;

  DeleteTasks;
end;

procedure TformNotetask.aMoveTaskTopExecute(Sender: TObject);
var
  newRow, selLen, selCol, selLeft, selRight: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if Grid.RowCount < 3 then exit;

  GridBackupSelection;
  selLen := Grid.Selection.Bottom - Grid.Selection.Top + 1;
  selLeft := Grid.Selection.Left;
  selRight := Grid.Selection.Right;
  selCol := Grid.Col;

  if (SortOrder = soAscending) then
    newRow := Tasks.MoveTasksTop(Grid.Selection.Top, Grid.Selection.Bottom, FShowArchived)
  else
    newRow := Tasks.MoveTasksBottom(Grid.Selection.Bottom, Grid.Selection.Top, FShowArchived);

  FillGrid;
  if (newRow > -1) then
  begin
    ResetRowHeight;
    Grid.Row := 0;
    Grid.Col := selCol;
    Grid.Selection := TGridRect.Create(selLeft, 0, selRight, selLen);
  end;
  Changed := True;
  SetNote;
  SetTags;
  SetInfo;
end;

procedure TformNotetask.aMoveTaskBottomExecute(Sender: TObject);
var
  newRow, selLen, selCol, selLeft, selRight: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if Grid.RowCount < 3 then exit;

  GridBackupSelection;
  selLen := Grid.Selection.Bottom - Grid.Selection.Top + 1;
  selLeft := Grid.Selection.Left;
  selRight := Grid.Selection.Right;
  selCol := Grid.Col;

  if (SortOrder = soAscending) then
    newRow := Tasks.MoveTasksBottom(Grid.Selection.Top, Grid.Selection.Bottom, FShowArchived)
  else
    newRow := Tasks.MoveTasksTop(Grid.Selection.Bottom, Grid.Selection.Top, FShowArchived);

  FillGrid;
  if (newRow > -1) then
  begin
    ResetRowHeight;
    Grid.Row := Grid.RowCount - selLen;
    Grid.Col := selCol;
    Grid.Selection := TGridRect.Create(selLeft, Grid.RowCount - selLen, selRight, Grid.RowCount);
  end;
  Changed := True;
  SetNote;
  SetTags;
  SetInfo;
end;

procedure TformNotetask.aMoveTaskUpExecute(Sender: TObject);
var
  newRow, selLen, selCol, selLeft, selRight: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if Grid.RowCount < 3 then exit;

  GridBackupSelection;
  selLen := Grid.Selection.Bottom - Grid.Selection.Top + 1;
  selLeft := Grid.Selection.Left;
  selRight := Grid.Selection.Right;
  selCol := Grid.Col;

  if (SortOrder = soAscending) then
    newRow := Tasks.MoveTasksUp(Grid.Selection.Top, Grid.Selection.Bottom)
  else
    newRow := Tasks.MoveTasksDown(Grid.Selection.Bottom, Grid.Selection.Top);

  FillGrid;
  if (newRow > -1) then
  begin
    Grid.OnSelection := nil;
    try
      ResetRowHeight(True, -1);
      Grid.Row := newRow;
      Grid.Col := selCol;
      Grid.Selection := TGridRect.Create(selLeft, newRow, selRight, newRow + selLen - 1);
      ResetRowHeight(True, -1);
    finally
      Grid.OnSelection := @GridSelection;
    end;
  end;
  Changed := True;
  SetNote;
  SetTags;
  SetInfo;
end;

procedure TformNotetask.aMoveTaskDownExecute(Sender: TObject);
var
  newRow, selLen, selCol, selLeft, selRight: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if Grid.RowCount < 3 then exit;

  GridBackupSelection;
  selLen := Grid.Selection.Bottom - Grid.Selection.Top + 1;
  selLeft := Grid.Selection.Left;
  selRight := Grid.Selection.Right;
  selCol := Grid.Col;

  if (SortOrder = soAscending) then
    newRow := Tasks.MoveTasksDown(Grid.Selection.Top, Grid.Selection.Bottom)
  else
    newRow := Tasks.MoveTasksUp(Grid.Selection.Bottom, Grid.Selection.Top);

  FillGrid;
  if (newRow > -1) then
  begin
    Grid.OnSelection := nil;
    try
      ResetRowHeight(True, -1);
      Grid.Row := newRow;
      Grid.Col := selCol;
      Grid.Selection := TGridRect.Create(selLeft, newRow - selLen + 1, selRight, newRow);
      ResetRowHeight(True, -1);
    finally
      Grid.OnSelection := @GridSelection;
    end;
  end;
  Changed := True;
  SetNote;
  SetTags;
  SetInfo;
end;

procedure TformNotetask.aMoveTaskLeftExecute(Sender: TObject);
var
  newRow, selCol, selLen, selLeft, selRight, selEnd: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if (TabsGroup.TabIndex <= 0) then exit;
  if Grid.RowCount < 2 then exit;

  GridBackupSelection;
  selLen := Grid.Selection.Bottom - Grid.Selection.Top + 1;
  selLeft := Grid.Selection.Left;
  selRight := Grid.Selection.Right;
  selCol := Grid.Col;

  newRow := Tasks.MoveGroupTasks(Grid.Selection.Top, Grid.Selection.Bottom, Tasks.GetLeftGroup(
    Tasks.SelectedGroup, FShowArchived, FilterBox.Text, FShowTime));

  if (newRow > -1) then
  begin
    ChangeGroup(FindGroupTabIndex(Tasks.SelectedGroup));
    newRow := Tasks.ReverseMap(newRow);
    Grid.Row := newRow;
    if (SortOrder = soAscending) then
      selEnd := newRow + selLen - 1
    else
      selEnd := newRow - selLen - 1;

    ResetRowHeight;
    SetTabs;
    Changed := True;
    SetNote;
    SetTags;
    SetInfo;

    Grid.Col := selCol;
    Grid.Selection := TGridRect.Create(selLeft, newRow, selRight, selEnd);
  end;
end;

procedure TformNotetask.aMoveTaskRightExecute(Sender: TObject);
var
  newRow, selCol, selLen, selLeft, selRight, selEnd: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if (TabsGroup.TabIndex >= TabsGroup.Tabs.Count - 1) then exit;
  if Grid.RowCount < 2 then exit;

  GridBackupSelection;
  selLen := Grid.Selection.Bottom - Grid.Selection.Top + 1;
  selLeft := Grid.Selection.Left;
  selRight := Grid.Selection.Right;
  selCol := Grid.Col;

  newRow := Tasks.MoveGroupTasks(Grid.Selection.Top, Grid.Selection.Bottom, Tasks.GetRightGroup(
    Tasks.SelectedGroup, FShowArchived, FilterBox.Text, FShowTime));

  if (newRow > -1) then
  begin
    ChangeGroup(FindGroupTabIndex(Tasks.SelectedGroup));
    newRow := Tasks.ReverseMap(newRow);
    Grid.Row := newRow;
    if (SortOrder = soAscending) then
      selEnd := newRow + selLen - 1
    else
      selEnd := newRow - selLen - 1;

    ResetRowHeight;
    SetTabs;
    Changed := True;
    SetNote;
    SetTags;
    SetInfo;

    Grid.Col := selCol;
    Grid.Selection := TGridRect.Create(selLeft, newRow, selRight, selEnd);
  end;
end;

procedure TformNotetask.aIndentTasksExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if Grid.RowCount < 2 then exit;
  EditComplete;
  IndentTasks;
end;

procedure TformNotetask.aOutdentTasksExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if Grid.RowCount < 2 then exit;
  EditComplete;
  IndentTasks(True);
end;

procedure TformNotetask.aArchiveTasksExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if Grid.RowCount < 2 then exit;

  ArchiveTasks;
end;

procedure TformNotetask.aDateTimeExecute(Sender: TObject);
var
  PosStart: integer;
  CurrentDateTimeISO: string;
  CurrentDateTime: string;

  procedure InsertDateToCell(ACol, ARow: integer);
  begin
    if (Grid.RowCount > 1) then
    begin
      if (Grid.Cells[ACol, ARow].Trim = string.Empty) or (ACol = COL_DATE) then
        Grid.Cells[ACol, ARow] := CurrentDateTime
      else
        Grid.Cells[ACol, ARow] := Grid.Cells[ACol, ARow].Trim + ' ' + CurrentDateTime;
      Tasks.SetTask(Grid, Memo, ARow, False, FShowTime);
      if Assigned(DatePicker) then
        DatePicker.DateTime := Now;
      if (FShowDuration) and (ACol = COL_DATE) then
        FillGrid;
    end
    else
    begin
      Tasks.InsertTask('- [ ] ' + CurrentDateTimeISO + ',', ARow);
      FillGrid;
      Grid.Row := Grid.Row + 1;
    end;
    Changed := True;
    SetInfo;
  end;

var
  c, r: integer;
begin
  if Screen.ActiveForm <> Self then exit;

  CurrentDateTime := DateTimeToString(Now, FShowTime);
  CurrentDateTimeISO := DateTimeToStringISO(Now, FShowTime);

  if MemoNote.Focused then
  begin
    PosStart := MemoNote.SelStart;
    MemoNote.SelText := CurrentDateTime;
    MemoNote.SelStart := PosStart;
    MemoNote.SelLength := Length(CurrentDateTime);
  end
  else
  if IsEditing then
  begin
    if (Grid.Col = COL_DATE) then
    begin
      Grid.Cells[COL_DATE, Grid.Row] := CurrentDateTime;
      DatePicker.DateTime := Now;
    end
    else
    if (Grid.Col in [COL_TASK, COL_NOTE]) then
    begin
      PosStart := Memo.SelStart;
      Memo.SelText := CurrentDateTime;
      Memo.SelStart := PosStart;
      Memo.SelLength := Length(CurrentDateTime);
    end;
    Tasks.SetTask(Grid, Memo, Grid.Row, FBackup, FShowTime);
    Changed := True;
    SetInfo;
  end
  else
  begin
    Tasks.CreateBackup;
    for r := Grid.Selection.Top to Grid.Selection.Bottom do
      for c := Grid.Selection.Left to Grid.Selection.Right do
        if (c > 0) then
          InsertDateToCell(c, r);
  end;
end;

procedure TformNotetask.aFilterExecute(Sender: TObject);
begin
  panelTabs.Visible := (not panelTabs.Visible and not FilterBox.Focused) or
    (not ((TabsGroup.Tabs.Count = 1) and (Tasks.GroupNames[0] = string.Empty)));
  Invalidate;
  Application.ProcessMessages;
  if (panelTabs.Visible) then
  begin
    if FilterBox.Focused then
    begin
      if Visible and Grid.Visible and Grid.CanFocus then
      begin
        FilterBox.Text := string.Empty;
        FLastFilter := '-1';
        filterBoxChange(Self);
        Grid.SetFocus;
      end;
    end
    else
    begin
      if (Length(FilterBox.Text) = 0) then
      begin
        if Assigned(Memo) and (Memo.SelText <> string.Empty) then
        begin
          FilterBox.Text := Memo.SelText;
          FLastFilter := '-1';
          filterBoxChange(Self);
        end
        else
        if MemoNote.Visible and MemoNote.Focused and (MemoNote.SelText <> string.Empty) then
        begin
          FilterBox.Text := MemoNote.SelText;
          FLastFilter := '-1';
          filterBoxChange(Self);
        end;
      end;

      if Visible and FilterBox.Visible and FilterBox.CanFocus then
        FilterBox.SetFocus;
    end;
  end
  else
  begin
    FilterBox.Text := string.Empty;
    FLastFilter := '-1';
    filterBoxChange(Self);
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
      EditComplete;
      newName := editText.Text;
      if (newName = rgroupuntitled) then newName := string.Empty;

      Result := Tasks.InsertGroup(newName);
      if (Result <> FindGroupRealIndex(TabsGroup.TabIndex)) then
      begin
        FLastRowMem.InsertAtPos(Result, 0);
        SetTabs;
        ChangeGroup(FindGroupTabIndex(Result));
        Self.Changed := True;
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
  if TabsGroup.Tabs.Count < 1 then exit;

  // Create an instance of the form
  with formInputText do
  try
    Left := self.Left + 14;
    Top := self.top + 52;
    SetMode(aRenameGroup.Caption, rentergroupname, rOK, Tasks.GetGroupNameForTab(FindGroupRealIndex(TabsGroup.TabIndex), False));

    // Show the form as a modal dialog
    if (ShowModal = mrOk) {and (editText.Text <> TabsGroup.Tabs[TabsGroup.TabIndex])} then
    begin
      newName := editText.Text;
      if (newName = rgroupuntitled) and (TabsGroup.TabIndex = 0) then newName := string.Empty;

      if (Tasks.RenameGroup(FindGroupRealIndex(TabsGroup.TabIndex), newName)) then
      begin
        SetTabs;
        Self.Changed := True;
      end;
    end;
  finally
    Hide;
  end;
end;

procedure TformNotetask.aEditGroupTooltipExecute(Sender: TObject);
begin
  if (TabsGroup.TabIndex = 0) and (Tasks.GroupNames[0] = string.Empty) then
    exit;
  if TabsGroup.Tabs.Count < 1 then exit;

  with formMemoText do
  try
    if not formMemoText.Showed then
    begin
      Left := self.Left + 14;
      Top := self.top + 52;
    end;
    SetMode(rapp, aEditGroupTooltip.Caption, rOK, Tasks.GetGroupHint(FindGroupRealIndex(TabsGroup.TabIndex)), 400, 180, FWordWrap, True);

    // Show the form as a modal dialog
    if ShowModal = mrOk then
    begin
      Tasks.RehintGroup(FindGroupRealIndex(TabsGroup.TabIndex), formMemoText.memoText.Text);
      Self.Changed := True;
    end;
  finally
    Hide;
  end;
end;

procedure TformNotetask.aDuplicateGroupExecute(Sender: TObject);
begin
  if Screen.ActiveForm <> Self then exit;
  if TabsGroup.Tabs.Count < 1 then exit;

  // Create an instance of the form
  with formInputText do
  try
    Left := self.Left + 14;
    Top := self.top + 52;
    SetMode(aDuplicateGroup.Caption, rentergroupname, rOK, Tasks.GetGroupNameForTab(FindGroupRealIndex(TabsGroup.TabIndex), False));

    // Show the form as a modal dialog
    if (ShowModal = mrOk) then
    begin
      if (Tasks.CopyGroup(FindGroupRealIndex(TabsGroup.TabIndex), editText.Text)) then
      begin
        FLastRowMem.InsertAtPos(FindGroupRealIndex(TabsGroup.TabIndex) + 1, FLastRowMem[FindGroupRealIndex(TabsGroup.TabIndex)]);
        SetTabs;
        ChangeGroup(TabsGroup.TabIndex + 1);
        Self.Changed := True;
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
  if TabsGroup.Tabs.Count < 1 then exit;

  Confirm := MessageDlg(rdeletegroupconfirm, mtConfirmation, [mbYes, mbNo], 0);

  if (Confirm = mrYes) then
  begin
    if (Tasks.DeleteGroup(FindGroupRealIndex(TabsGroup.TabIndex))) then
    begin
      FLastRowMem.DeleteAtPos(FindGroupRealIndex(TabsGroup.TabIndex));
      Mem := FLastRowMem.CloneArray;
      SetTabs;
      ChangeGroup(FindGroupTabIndex(Tasks.SelectedGroup));
      FLastRowMem := Mem.CloneArray;
      if (Length(FLastRowMem) > Tasks.SelectedGroup) then
        Grid.Row := FLastRowMem[Tasks.SelectedGroup];
      Changed := True;
    end;
  end;
end;

procedure TformNotetask.aMoveGroupLeftExecute(Sender: TObject);
begin
  MoveTabLeft(TabsGroup.TabIndex);
end;

procedure TformNotetask.aMoveGroupRightExecute(Sender: TObject);
begin
  MoveTabRight(TabsGroup.TabIndex);
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
      gridPrinter.Grid := Grid;
      gridPrinter.OnGetCellText := @PrinterGetCellText;
      gridPrinter.OnPrepareCanvas := @PrinterPrepareCanvas;
      gridPrinter.OnBeforePrintCell := @PrinterBeforePrintCell;
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

  EditComplete;
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

  EditComplete;
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

  EditComplete;
  FWordWrap := aWordWrap.Checked;
  sel := MemoNote.SelLength;
  MemoNote.WordWrap := FWordWrap;
  if sel = 0 then
    MemoNote.SelLength := 0;
  CalcRowHeight(True);
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

  EditComplete;
  BiDiRightToLeft := aBidiRightToLeft.Checked;
  ResetRowHeight;
  Invalidate;
end;

procedure TformNotetask.aChatGptExecute(Sender: TObject);
begin
  if Grid.RowCount < 2 then exit;

  ExecuteChatGpt;
end;

procedure TformNotetask.aRunTerminalExecute(Sender: TObject);
begin
  if Grid.RowCount < 2 then exit;

  ExecuteTerminal(False);
end;

procedure TformNotetask.aRunPowershellExecute(Sender: TObject);
begin
  if Grid.RowCount < 2 then exit;

  ExecuteTerminal;
end;

procedure TformNotetask.aGoToExecute(Sender: TObject);
var
  rowNum: integer;
begin
  if Screen.ActiveForm <> Self then exit;
  if Grid.RowCount < 2 then exit;

  // Create an instance of the form
  with formInputText do
  try
    Left := self.Left + 14;
    Top := self.top + 52;
    SetMode(rgototask, rtasknumber, rgoto, IntToStr(Grid.Row), True);

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
          Grid.Row := Tasks.ReverseMap(rowNum - 1);
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
  if Grid.RowCount < 2 then exit;

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
  if Grid.RowCount < 2 then exit;

  formReplaceText.editFind.Text := FindText;
  if (formReplaceText.Left = 0) then
    formReplaceText.Left := self.Left + 80;
  if (formReplaceText.Top = 0) then
    formReplaceText.Top := self.top + 100;
  formReplaceText.Show;
end;

procedure TformNotetask.aFindNextExecute(Sender: TObject);
begin
  if Grid.RowCount < 2 then exit;

  if (assigned(formFindText)) and (formFindText.Visible) then
  begin
    MatchCase := formFindText.checkMatchCase.Checked;
    WrapAround := formFindText.checkWrapAround.Checked;
  end;

  if (FindText = string.Empty) and (Clipboard.AsText <> string.Empty) then
    FindText := Clipboard.AsText;

  if (FindText <> string.Empty) then
  begin
    FFindF3 := True;
    Find(FindText, MatchCase, WrapAround, True);
  end
  else
    aFind.Execute;
end;

procedure TformNotetask.aFindPrevExecute(Sender: TObject);
begin
  if Grid.RowCount < 2 then exit;

  if (FindText = string.Empty) and (Clipboard.AsText <> string.Empty) then
    FindText := Clipboard.AsText;

  if (FindText <> string.Empty) then
  begin
    FFindF3 := True;
    Find(FindText, MatchCase, WrapAround, False);
  end
  else
    aFind.Execute;
end;

procedure TformNotetask.aAboutExecute(Sender: TObject);
begin
  formAboutNotetask := TformAboutNotetask.Create(Application);
  try
    formAboutNotetask.Left := Self.Left + 100;
    formAboutNotetask.Top := Self.Top + 100;
    formAboutNotetask.ShowModal;
  finally
    formAboutNotetask.Free;
  end;
end;

procedure TformNotetask.aLangArabicExecute(Sender: TObject);
begin
  SetLanguage('ar');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangBelarusianExecute(Sender: TObject);
begin
  SetLanguage('be');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangChineseExecute(Sender: TObject);
begin
  SetLanguage('zh');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangCzechExecute(Sender: TObject);
begin
  SetLanguage('cs');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangDanishExecute(Sender: TObject);
begin
  SetLanguage('da');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangDutchExecute(Sender: TObject);
begin
  SetLanguage('nl');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangEnglishExecute(Sender: TObject);
begin
  SetLanguage('en');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangFinnishExecute(Sender: TObject);
begin
  SetLanguage('fi');
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

procedure TformNotetask.aLangGreekExecute(Sender: TObject);
begin
  SetLanguage('el');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangHebrewExecute(Sender: TObject);
begin
  SetLanguage('he');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangHindiExecute(Sender: TObject);
begin
  SetLanguage('hi');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangIndonesianExecute(Sender: TObject);
begin
  SetLanguage('id');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangItalianExecute(Sender: TObject);
begin
  SetLanguage('it');
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

procedure TformNotetask.aLangPolishExecute(Sender: TObject);
begin
  SetLanguage('pl');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangPortugueseExecute(Sender: TObject);
begin
  SetLanguage('pt');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangRomanianExecute(Sender: TObject);
begin
  SetLanguage('ro');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangRussianExecute(Sender: TObject);
begin
  SetLanguage('ru');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangSpanishExecute(Sender: TObject);
begin
  SetLanguage('es');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangSwedishExecute(Sender: TObject);
begin
  SetLanguage('sv');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangTurkishExecute(Sender: TObject);
begin
  SetLanguage('tr');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aLangUkrainianExecute(Sender: TObject);
begin
  SetLanguage('uk');
  if ShowDuration then FillGrid;
  SetInfo;
end;

procedure TformNotetask.aDonateExecute(Sender: TObject);
begin
  formDonateNotetask := TformDonateNotetask.Create(Application);
  try
    formDonateNotetask.Left := Self.Left + 100;
    formDonateNotetask.Top := Self.Top + 100;
    formDonateNotetask.ShowModal;
  finally
    formDonateNotetask.Free;
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

    if Grid.Selection.Height > 0 then
    begin
      // Multiple rows selected — concatenate notes
      for i := Grid.Selection.Top to Grid.Selection.Bottom do
        if Tasks.Map(i) > -1 then
        begin
          notes.Add(Tasks.GetTask(i).Note);
          if (i = Grid.Selection.Top) then
            fileName := Tasks.GetTask(i).Text;
        end;
    end
    else if Tasks.Map(Grid.Row) > -1 then
    begin
      // Single row selected
      notes.Add(Tasks.GetTask(Grid.Row).Note);
      fileName += Tasks.GetTask(Grid.Row).Text;
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

{%EndRegion}

{%Region -fold Context Menu}

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
  if TagEdit.SelectedTags.Count > 0 then
    Clipboard.AsText := TagEdit.SelectedTags.DelimitedText
  else
  if TagEdit.HoveredTag <> string.Empty then
    Clipboard.AsText := TagEdit.HoveredTag;
end;

procedure TformNotetask.contextDeleteTagsClick(Sender: TObject);
begin
  if TagEdit.SelectedTags.Count > 0 then
    TagEdit.RemoveSelectedTags
  else
  if TagEdit.HoveredTag <> string.Empty then
    TagEdit.RemoveTag(TagEdit.HoveredTag, True);
end;

procedure TformNotetask.contextColorClick(Sender: TObject);
var
  HoverTag: string;
  HoverIndex: integer;
begin
  HoverTag := LowerCase(TagEdit.HoveredTag).SubStringBeforeColon;
  HoverIndex := TagEdit.TagColors.IndexOf(HoverTag);

  if HoverIndex >= 0 then
    colorDialog.Color := TagEdit.TagColors.Items[HoverIndex].Color
  else
    colorDialog.Color := TagEdit.GetAutoColor(HoverTag);

  if (colorDialog.Execute) then
  begin
    if HoverIndex >= 0 then
      TagEdit.TagColors.Items[HoverIndex].Color := colorDialog.Color
    else
      TagEdit.TagColors.Add(HoverTag, colorDialog.Color);
    TagEdit.Invalidate;
    GridInvalidate;
  end;
end;

procedure TformNotetask.contextResetColorClick(Sender: TObject);
var
  HoverTag: string;
  HoverIndex: integer;
begin
  HoverTag := LowerCase(TagEdit.HoveredTag).SubStringBeforeColon;
  HoverIndex := TagEdit.TagColors.IndexOf(HoverTag);

  if HoverIndex >= 0 then
  begin
    TagEdit.TagColors.Delete(HoverIndex);

    TagEdit.Invalidate;
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
    Changed := True;
  end;
end;

procedure TformNotetask.contextUnixLFClick(Sender: TObject);
begin
  FLineEnding := TLineEnding.UnixLF;
  if (contextUnixLF.Checked = False) then
  begin
    contextUnixLF.Checked := True;
    SetInfo;
    Changed := True;
  end;
end;

procedure TformNotetask.contextMacintoshCRClick(Sender: TObject);
begin
  FLineEnding := TLineEnding.MacintoshCR;
  if (contextMacintoshCR.Checked = False) then
  begin
    contextMacintoshCR.Checked := True;
    SetInfo;
    Changed := True;
  end;
end;

procedure TformNotetask.contextANSIClick(Sender: TObject);
begin
  FEncoding := TEncoding.ANSI;
  if (contextANSI.Checked = False) then
  begin
    contextANSI.Checked := True;
    SetInfo;
    Changed := True;
  end;
end;

procedure TformNotetask.contextASCIIClick(Sender: TObject);
begin
  FEncoding := TEncoding.ASCII;
  if (contextASCII.Checked = False) then
  begin
    contextASCII.Checked := True;
    SetInfo;
    Changed := True;
  end;
end;

procedure TformNotetask.contextUTF8Click(Sender: TObject);
begin
  FEncoding := TEncoding.UTF8;
  if (contextUTF8.Checked = False) then
  begin
    contextUTF8.Checked := True;
    SetInfo;
    Changed := True;
  end;
end;

procedure TformNotetask.contextUTF8BOMClick(Sender: TObject);
begin
  FEncoding := UTF8BOMEncoding;
  if (contextUTF8BOM.Checked = False) then
  begin
    contextUTF8BOM.Checked := True;
    SetInfo;
    Changed := True;
  end;
end;

procedure TformNotetask.contextUTF16BEBOMClick(Sender: TObject);
begin
  FEncoding := UTF16BEBOMEncoding;
  if (contextUTF16BEBOM.Checked = False) then
  begin
    contextUTF16BEBOM.Checked := True;
    SetInfo;
    Changed := True;
  end;
end;

procedure TformNotetask.contextUTF16LEBOMClick(Sender: TObject);
begin
  FEncoding := UTF16LEBOMEncoding;
  if (contextUTF16LEBOM.Checked = False) then
  begin
    contextUTF16LEBOM.Checked := True;
    SetInfo;
    Changed := True;
  end;
end;

{%EndRegion}

{%Region -fold Memo Note Events}

procedure TformNotetask.MemoNoteEnter(Sender: TObject);
begin
  {$IFDEF UNIX}
     aDelete.ShortCut:=0;
  {$ELSE}
  ; // NOP
  {$ENDIF}
end;

procedure TformNotetask.MemoNoteExit(Sender: TObject);
begin
  {$IFDEF UNIX}
     aDelete.ShortCut:=VK_DELETE;
  {$ELSE}
  ; // NOP
  {$ENDIF}

  SetFilter;
end;

procedure TformNotetask.MemoNoteKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  LinesPerPage, NewPos: integer;
  Render: string;
begin
  // Test for letter, number, space, back, enter, shift or delete key for backup
  if (Shift * [ssCtrl, ssAlt] = []) and ((not THotKeyData.Create(Key).IsSystemKey) or (Key = VK_SPACE) or
    (Key = VK_BACK) or (Key = VK_RETURN) or (ssShift in Shift) or ((Key = VK_DELETE) and (MemoNote.SelLength = 0))) then
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
    LinesPerPage := MemoNote.ClientHeight div Canvas.TextHeight('Wg');
    NewPos := Max(0, MemoNote.CaretPos.Y - LinesPerPage);
    if (NewPos = 0) then
      MemoNote.SelStart := 0
    else
      MemoNote.CaretPos := Point(0, NewPos);
    MemoNote.VertScrollBar.Position := MemoNote.CaretPos.Y - (LinesPerPage div 2);
    MemoNote.Invalidate;
    Key := 0;
  end
  else
  if (not (ssShift in Shift)) and (Key = VK_NEXT) then
  begin
    LinesPerPage := MemoNote.ClientHeight div Canvas.TextHeight('Wg');
    NewPos := Min(MemoNote.Lines.Count - 1, MemoNote.CaretPos.Y + LinesPerPage);
    if NewPos >= MemoNote.Lines.Count - 1 then
      MemoNote.SelStart := MemoNote.GetTextLen - Length(unicodestring(MemoNote.Lines[MemoNote.Lines.Count - 1]))
    else
      MemoNote.CaretPos := Point(0, NewPos);
    MemoNote.VertScrollBar.Position := MemoNote.CaretPos.Y - (LinesPerPage div 2);
    MemoNote.Invalidate;
    Key := 0;
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
    MemoNote.SelStart := 0;
    MemoNote.SelLength := Length(MemoNote.Text);
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
  if MemoNote.ReadOnly then exit
  else
  if Key = VK_DELETE then // Delete
  begin
    {$IFDEF UNIX}
    if MemoNote.SelLength > 0 then
        MemoNoteBackup;
    {$ELSE}
    MemoDelKey;
    Key := 0;
    {$ENDIF}
  end
  else
  if (Key = VK_BACK) then // Backspace
  begin
    if MemoNote.SelLength > 0 then
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
  if (ssCtrl in Shift) and (Key = VK_RETURN) and (trim(MemoNote.SelText) <> string.Empty) then // Ctrl + Enter
  begin
    Render := MemoNote.SelText.ToASCIITextArt(Font.Name, Max(ifthen(Font.Size = 0, 10, Font.Size) - 2, 2));
    if (Render <> MemoNote.SelText) then
    begin
      MemoNoteBackup;
      MemoNote.SelText := Render;
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
    MemoNote.CutToClipboard;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_V) then // Ctrl + V
  begin
    MemoNoteBackup;
    PasteWithLineEnding(MemoNote);
    Key := 0;
  end
  else
  if Key = VK_ESCAPE then // Escape
    if Visible and Grid.Visible and Grid.CanFocus then
      Grid.SetFocus;
end;

procedure TformNotetask.MemoNoteKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  FNoteLastSelText := (Sender as TMemo).SelText;
  FNoteLastSelStart := (Sender as TMemo).SelStart;
  FNoteLastSelLength := (Sender as TMemo).SelLength;
end;

procedure TformNotetask.MemoNoteMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbMiddle) and (ssCtrl in Shift) then // Middle button + Ctrl
    aZoomDefault.Execute
  else
  if not (ssDouble in Shift) then
  begin
    if ssCtrl in Shift then
    begin
      // if no selection try select full url
      if (FNoteLastSelLength < 1) or (((Sender as TMemo).SelStart < FNoteLastSelStart) or
        ((Sender as TMemo).SelStart > FNoteLastSelStart + FNoteLastSelLength)) or (not TryOpenAsUrl(Trim(FNoteLastSelText))) then
      begin
        (Sender as TMemo).MemoTokenAtPos((Sender as TMemo).SelStart, ':/?#[]@!$&''()*+,;=-_.~%');
        FNoteLastSelText := (Sender as TMemo).SelText;
        FNoteLastSelStart := (Sender as TMemo).SelStart;
        FNoteLastSelLength := (Sender as TMemo).SelLength;
      end
      else
      begin
        (Sender as TMemo).SelStart := FNoteLastSelStart;
        (Sender as TMemo).SelLength := FNoteLastSelLength;
      end;
    end
    else
      FMemoSelStartClicked := (Sender as TMemo).SelStart;
  end;
  // Force set focus
  if (Sender as TMemo).Visible and (Sender as TMemo).CanFocus and not (Sender as TMemo).Focused then
    (Sender as TMemo).SetFocus;
end;

procedure TformNotetask.MemoNoteMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if not (ssCtrl in Shift) then
  begin
    FNoteLastSelText := (Sender as TMemo).SelText;
    FNoteLastSelStart := (Sender as TMemo).SelStart;
    FNoteLastSelLength := (Sender as TMemo).SelLength;
  end;
end;

procedure TformNotetask.MemoNoteMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer;
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

procedure TformNotetask.MemoNoteChange(Sender: TObject);
begin
  Grid.Cells[COL_NOTE, Grid.Row] := MemoNote.Text;
  Tasks.SetTask(Grid, Memo, Grid.Row, FBackup, FShowTime);
  CalcRowHeight(True, Grid.Row);
  Changed := True;
end;

procedure TformNotetask.MemoNoteDblClick(Sender: TObject);
var
  Pos: integer;
begin
  if FMemoSelStartClicked >= 0 then
    Pos := FMemoSelStartClicked
  else
    Pos := (Sender as TMemo).SelStart;

  (Sender as TMemo).MemoTokenAtPos(Pos, '_-@');
  FMemoSelStartClicked := -1;
end;

procedure TformNotetask.panelNoteMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbLeft) or (Button = mbRight) then
  begin
    if not MemoNote.Focused then MemoNote.SetFocus;
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
      //  if (MemoNote.VertScrollBar.Position > 0) then
      //  begin
      //    Application.ProcessMessages;
      //    MemoNote.VertScrollBar.Position := MemoNote.VertScrollBar.Position + Canvas.TextHeight('Wg');
      //  end;
      //  {$ELSE}
      //  if (MemoNote.VertScrollBar.Position > 0) then
      //    MemoNote.VertScrollBar.Position := MemoNote.VertScrollBar.Position - 1;
      //  {$ENDIF}
      //end
      //else
      //if (Index > IndexEnd) then
      //begin
      //  {$IFDEF UNIX}
      //  Application.ProcessMessages;
      //  MemoNote.VertScrollBar.Position := MemoNote.VertScrollBar.Position + Canvas.TextHeight('Wg');
      //  {$ELSE}
      //  MemoNote.VertScrollBar.Position := MemoNote.VertScrollBar.Position + 1;
      //  {$ENDIF}
      //end;
    end;
  end;

end;

procedure TformNotetask.panelNoteMouseEnter(Sender: TObject);
begin
  panelNote.Color := TDarkUtils.ThemeColor(clSplitHighlight_Light, clSplitHighlight_Dark);
end;

procedure TformNotetask.panelNoteMouseLeave(Sender: TObject);
begin
  FNoteSelecting := False;
  panelNote.Color := TDarkUtils.ThemeColor(clSpit_Light, clSplit_Dark);
end;

procedure TformNotetask.panelNoteMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FNoteSelecting := False;
end;

procedure TformNotetask.MemoNoteSetScrollPosition(Value: integer);
begin
  {$IFDEF UNIX}
  MemoNote.Visible := False;
  Application.ProcessMessages;
  MemoNote.VertScrollBar.Position;
  MemoNote.VertScrollBar.Position := Value;
  MemoNote.Visible := True;
  if (MemoNote.CanFocus) then MemoNote.SetFocus;
  {$ELSE}
  MemoNote.VertScrollBar.Position := 0;
  MemoNote.VertScrollBar.Position := Value;
  {$ENDIF}
end;

procedure TformNotetask.MemoNoteBackup;
begin
  FMemoNoteBackup := MemoNote.Text;
  FMemoNoteSelStartBackup := MemoNote.SelStart;
  FMemoNoteSelLengthBackup := MemoNote.SelLength;
  FMemoNoteCaretBackup := MemoNote.CaretPos;
  FMemoNoteVertScrollBackup := MemoNote.VertScrollBar.Position;
end;

procedure TformNotetask.MemoNoteUndo;
var
  newBackup: TCaption;
  SelStart, SelLength: integer;
begin
  // Save current selection and text
  newBackup := MemoNote.Text;
  SelStart := MemoNote.SelStart;
  SelLength := MemoNote.SelLength;

  // Restore from backup
  MemoNote.Text := FMemoNoteBackup;
  MemoNote.CaretPos := FMemoNoteCaretBackup;
  MemoNote.SelStart := FMemoNoteSelStartBackup;
  MemoNote.SelLength := FMemoNoteSelLengthBackup;

  // Adjust scroll position
  MemoNoteSetScrollPosition(FMemoNoteVertScrollBackup);

  // Option with scroll centering
  //if (FMemoNoteVertScrollBackup > 0) then
  //begin
  //  LinesPerPage := MemoNote.ClientHeight div Canvas.TextHeight('Wg');
  //  MemoNote.VertScrollBar.Position := MemoNote.VertScrollBar.Position + LinesPerPage div 2;
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
  if (MemoNote.SelLength > 0) then
  begin
    SelStartPos := MemoNote.SelStart;
    SelEndPos := SelStartPos + MemoNote.SelLength;
    CaretPos := Point(MemoNote.CaretPos.X, MemoNote.CaretPos.Y);

    MemoNote.Lines.BeginUpdate;

    // Calculate start line number of selection
    MemoNote.SelStart := SelStartPos;
    StartLine := MemoNote.CaretPos.Y;

    // Calculate end line number of selection
    MemoNote.SelStart := SelEndPos;
    EndLine := MemoNote.CaretPos.Y;

    // Restore selection
    MemoNote.SelStart := SelStartPos;
    MemoNote.SelLength := SelEndPos - SelStartPos;

    MemoNote.Lines.EndUpdate;

    // If last line not selected decrement endline
    if (StartLine <> EndLine) and (SelEndPos - SelStartPos > 0) and (EndLine = CaretPos.Y) and
      ((CaretPos.X = 0) or ((CaretPos.X = (SelEndPos - SelStartPos)) and (MemoNote.SelText[Length(MemoNote.SelText)] in [#10, #13]))) then
      Dec(EndLine);

    // Add IndentStr at the start of each selected line
    for i := StartLine to EndLine do
      MemoNote.Lines[i] := IndentStr + MemoNote.Lines[i];

    // Adjust selection length to include inserted spaces
    Offset := Length(IndentStr) * (EndLine - StartLine + 1);
    MemoNote.SelStart := SelStartPos;
    MemoNote.SelLength := SelEndPos - SelStartPos + Offset;
  end
  else
    MemoNote.SelText := IndentStr;
end;

procedure TformNotetask.MemoNoteOutdent;
var
  SelStartPos, SelEndPos, StartLine, EndLine, i: integer;
  CaretPos: TPoint;
  Offset: integer;
  line: string;
begin
  MemoNoteBackup;
  SelStartPos := MemoNote.SelStart;
  SelEndPos := SelStartPos + MemoNote.SelLength;
  CaretPos := Point(MemoNote.CaretPos.X, MemoNote.CaretPos.Y);

  MemoNote.Lines.BeginUpdate;

  // Calculate start line number of selection
  MemoNote.SelStart := SelStartPos;
  StartLine := MemoNote.CaretPos.Y;

  // Calculate end line number of selection
  MemoNote.SelStart := SelEndPos;
  EndLine := MemoNote.CaretPos.Y;

  // Restore selection
  MemoNote.SelStart := SelStartPos;
  MemoNote.SelLength := SelEndPos - SelStartPos;

  MemoNote.Lines.EndUpdate;

  // If last line not selected decrement endline
  if (StartLine <> EndLine) and (SelEndPos - SelStartPos > 0) and (EndLine = CaretPos.Y) and
    ((CaretPos.X = 0) or ((CaretPos.X = (SelEndPos - SelStartPos)) and (MemoNote.SelText[Length(MemoNote.SelText)] in [#10, #13]))) then
    Dec(EndLine);

  // Remove IndentStr at the start of each selected line if present
  Offset := 0;
  for i := StartLine to EndLine do
  begin
    line := MemoNote.Lines[i];
    if Length(line) >= Length(IndentStr) then
    begin
      if Copy(line, 1, Length(IndentStr)) = IndentStr then
      begin
        Delete(line, 1, Length(IndentStr));
        MemoNote.Lines[i] := line;
        Offset += Length(IndentStr);
      end;
    end;
  end;

  // Adjust selection length to account for removed spaces
  MemoNote.SelStart := SelStartPos;
  MemoNote.SelLength := SelEndPos - SelStartPos - Offset;
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
  MemoNote.Tag := MemoNote.VertScrollBar.Position;
  {$ENDIF}
  MemoNoteBackup;

  // If no selection and the cursor is on an empty line -> insert a line of the comment character
  if (MemoNote.SelLength = 0) then
  begin
    line := Trim(MemoNote.Lines[MemoNote.CaretPos.Y]);
    if line = string.Empty then
    begin
      // Create a string of the comment character, approximate length to fit the editor width
      // Calculate how many times we can repeat the full word
      wordWidth := Canvas.TextWidth(aComment);
      if wordWidth > 0 then
        Count := Min(MemoNote.ClientWidth, 800) div wordWidth
      else
        Count := 60; // fallback value

      // Build the repeated string
      resultStr := string.Empty;
      for j := 1 to Count do
        resultStr := resultStr + aComment;

      MemoNote.Lines[MemoNote.CaretPos.Y] := resultStr;

      {$IFDEF UNIX}
    if (MemoNote.Tag > 0) then
      MemoNoteSetScrollPosition(MemoNote.Tag);
      {$ENDIF}

      Exit; // Stop method execution, nothing else to do
    end;
  end;

  FirstCommentPos := -1;
  SelStartPos := MemoNote.SelStart;
  SelEndPos := SelStartPos + MemoNote.SelLength;
  CaretPos := Point(MemoNote.CaretPos.X, MemoNote.CaretPos.Y);

  MemoNote.Lines.BeginUpdate;

  // Calculate start and end lines of selection
  MemoNote.SelStart := SelStartPos;
  StartLine := MemoNote.CaretPos.Y;

  MemoNote.SelStart := SelEndPos;
  EndLine := MemoNote.CaretPos.Y;

  // Restore selection
  MemoNote.SelStart := SelStartPos;
  MemoNote.SelLength := SelEndPos - SelStartPos;

  MemoNote.Lines.EndUpdate;

  // If last line not selected decrement endline
  if (StartLine <> EndLine) and (SelEndPos - SelStartPos > 0) and (EndLine = CaretPos.Y) and
    ((CaretPos.X = 0) or ((CaretPos.X = (SelEndPos - SelStartPos)) and (MemoNote.SelText[Length(MemoNote.SelText)] in [#10, #13]))) then
    Dec(EndLine);

  // Find minimum IndentStr among non-empty lines
  MinIndent := MaxInt;
  for i := StartLine to EndLine do
  begin
    line := MemoNote.Lines[i];
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
    trimmed := TrimLeft(MemoNote.Lines[i]);
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
    line := MemoNote.Lines[i];
    trimmed := TrimLeft(line);

    if trimmed = '' then
      Continue; // skip empty lines

    if AllCommented then
    begin
      // Remove aComment, keep spaces after it
      if UpperCase(Copy(trimmed, 1, Length(aComment))) = UpperCase(aComment) then
      begin
        Delete(trimmed, 1, Length(aComment));
        MemoNote.Lines[i] := StringOfChar(' ', Length(line) - Length(TrimLeft(line))) + trimmed;
        CommentOffset -= Length(aComment);
      end;
    end
    else
    begin
      // Add aComment at MinIndent, keep extra spaces
      if Length(line) > MinIndent then
        MemoNote.Lines[i] := Copy(line, 1, MinIndent) + aComment + Copy(line, MinIndent + 1, MaxInt)
      else
        MemoNote.Lines[i] := StringOfChar(' ', MinIndent) + aComment;
      CommentOffset += Length(aComment);

      // Calculate first comment position
      if (i = startline) then
      begin
        FirstCommentPos := 0;
        for j := 0 to i - 1 do
        begin
          FirstCommentPos := FirstCommentPos + Length(unicodestring(MemoNote.Lines[j])) + 1;
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
    MemoNote.SelStart := FirstCommentPos
  else
    MemoNote.SelStart := SelStartPos;
  MemoNote.SelLength := (SelEndPos - SelStartPos) + CommentOffset;
  {$IFDEF UNIX}
  if (MemoNote.Tag > 0) then
    MemoNoteSetScrollPosition(MemoNote.Tag);
  {$ENDIF}
end;

{%EndRegion}

{%Region -fold Tags Edit Events}

procedure TformNotetask.TagEditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if TagEdit.ReadOnly then exit
  else
  if Key = VK_DELETE then // Delete
  begin
    {$IFDEF UNIX}
    {$ELSE}
    if (TagEdit.EditBox.SelLength = 0) then
      TagEdit.EditBox.SelLength := CalcDeleteCount(TagEdit.EditBox.Text, TagEdit.EditBox.SelStart);
    TagEdit.EditBox.ClearSelection;
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
  if (ssCtrl in Shift) and (Key = VK_Z) and (not TagEdit.EditBox.Focused) then // Ctrl + Z
  begin
    aUndo.Execute;
    Key := 0;
  end
  else
  if Key = VK_ESCAPE then // Escape
    if Grid.Visible and Grid.CanFocus then
      Grid.SetFocus;
end;

procedure TformNotetask.TagEditTagClick(Sender: TObject; const TagText: string; const TagIndex: integer);
begin
  if (FilterBox.Text <> TagText) then
  begin
    FilterBox.Text := TagText;
    FLastFilter := '-1';
    filterBoxChange(Self);
  end
  else
    filterClearClick(Sender);
end;

procedure TformNotetask.TagEditBeforeChange(Sender: TObject; Tags: string; Operation: TTagEditOperation; var AllowChange: boolean);
begin
  Tasks.CreateBackup;
end;

procedure TformNotetask.TagEditChange(Sender: TObject);
begin
  SetFilter;
  Changed := True;
  Grid.Invalidate;
  Application.ProcessMessages;
  CalcRowHeight(True);
end;

procedure TformNotetask.TagEditTagAdd(Sender: TObject; const TagText: string; const TagIndex: integer);
begin
  TagsAdd(Grid.Selection, TagText);
end;

procedure TformNotetask.TagEditTagRemove(Sender: TObject; const TagText: string; const TagIndex: integer);
var
  i: integer;
  task: TTask;
begin
  //  Tasks.CreateBackup;
  for i := Grid.Selection.Top to Grid.Selection.Bottom do
    if Tasks.Map(i) > -1 then
    begin
      task := Tasks.GetTask(i);
      if TagIndex >= 0 then
        task.Tags.Delete(TagIndex)
      else
        task.Tags.RemoveAll(TagText);
      if task.Tags.Count = 0 then
        task.TagsWidth := 0;
    end;
  SetTags;
end;

procedure TformNotetask.TagEditTagReorder(Sender: TObject; const TagText: string; const NewIndex: integer);
var
  i: integer;
begin
  Tasks.CreateBackup;
  for i := Grid.Selection.Top to Grid.Selection.Bottom do
    if Tasks.Map(i) > -1 then
      Tasks.GetTask(i).Tags.Assign(TagEdit.Items);
end;

procedure TformNotetask.TagEditExit(Sender: TObject);
begin
  FLastGridSelection := Grid.Selection;
  Application.QueueAsyncCall(@DelayedFinishTagEdit, 0);
end;

{%EndRegion}

{%Region -fold Group Tabs}

procedure TformNotetask.TabsGroupChange(Sender: TObject);
begin
  EditComplete;
  TagEdit.FinishEdit;

  if (Length(FLastRowMem) > Tasks.SelectedGroup) then
    FLastRowMem[Tasks.SelectedGroup] := Grid.Row;

  if (TabsGroup.TabIndex >= 0) then
    Tasks.ChangeGroup(FindGroupRealIndex(TabsGroup.TabIndex), True);

  FillGrid;

  if (Length(FLastRowMem) > Tasks.SelectedGroup) then
    Grid.Row := FLastRowMem[Tasks.SelectedGroup]
  else
    Grid.Row := 1;
  Grid.ClearSelections;

  if (TabsGroup.TabIndex <> FLastTabIndex) then
  begin
    Tasks.CreateBackup;
    GridBackupSelection;
  end;

  FLastTabIndex := TabsGroup.TabIndex;

  Tasks.CalcTagsWidths(-1, Grid.Columns[COL_TASK - 1].Width, TagEdit, Font);
  ChangeLastText;
  CalcRowHeight(True);
  SetNote;
  SetTags;
  SetInfo;
end;

procedure TformNotetask.TabsGroupMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbLeft) then
    if (TabsGroup.IndexOfTabAt(X, Y) = TabsGroup.TabIndex) and not ((TabsGroup.TabIndex = 0) and (Tasks.GroupNames[0] = string.Empty)) then
    begin
      FDragTab := TabsGroup.TabIndex;
      FLastTabMouseX := 0;
    end;
end;

procedure TformNotetask.TabsGroupMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  target: integer;
begin
  if not (ssLeft in Shift) then
  begin
    TabsGroupMouseLeave(Self);
    exit;
  end;

  target := TabsGroup.IndexOfTabAt(X, Y);
  if FDragTab >= 0 then
  begin
    if target >= 0 then
    begin
      if (FLastTabMouseX <> X) and (FLastTabMouseX > 0) then  Screen.Cursor := crDrag;
      if (target > FDragTab) and (FLastTabMouseX < X) then
        MoveTabRight(TabsGroup.TabIndex)
      else
      if (target < FDragTab) and (FLastTabMouseX > X) then
        MoveTabLeft(TabsGroup.TabIndex);
    end;
  end;
  // Hide hint if long move
  Application.HintPause := 100;
  if (target <> FLastTabTarget) then
    Application.HideHint;
  FLastTabMouseX := X;
  FLastTabTarget := target;
end;

procedure TformNotetask.TabsGroupMouseLeave(Sender: TObject);
begin
  FLastTabMouseX := 0;
  Application.HintPause := 500;
  DisableDrag;
end;

procedure TformNotetask.TabsGroupMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  TabIndex: integer;
begin
  FLastTabMouseX := 0;
  DisableDrag;

  if Button = mbRight then
  begin
    TabIndex := TabsGroup.IndexOfTabAt(X, Y);
    if TabIndex <> -1 then
      TabsGroup.TabIndex := TabIndex;
    PopupTabs.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

{%EndRegion}

{%Region -fold Task Grid}

procedure TformNotetask.GridDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
var
  TempGrid: TStringGrid;
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
  Indent: integer = 0;
begin
  TempGrid := Sender as TStringGrid;
  bgFill := TDarkUtils.ThemeColor(clWhite, clBlack);

  // Border for fixed cells
  if (aRow < TempGrid.FixedRows) or (aCol < TempGrid.FixedCols) then
  begin
    TempGrid.Canvas.Pen.Color := clSilver;
    TempGrid.Canvas.Pen.Style := psSolid;
    TempGrid.Canvas.Pen.Width := 1;
    TempGrid.Canvas.Pen.Width := 0;
    TempGrid.Canvas.Brush.Style := bsClear;
    TempGrid.Canvas.Rectangle(aRect.Left - 1, aRect.Top - 1, aRect.Right, aRect.Bottom);

    if (aRow = 0) and (aCol = COL_NUM) and (SortColumn = COL_NUM) and Assigned(TempGrid.TitleImageList) then
    begin
      if SortOrder = soAscending then
        ImgIndex := 0
      else
        ImgIndex := 1;

      ImgX := aRect.Right - TempGrid.TitleImageList.Width - 4;
      ImgY := aRect.Top + ((aRect.Bottom - aRect.Top - TempGrid.TitleImageList.Height) div 2);

      TempGrid.TitleImageList.Draw(TempGrid.Canvas, ImgX, ImgY, ImgIndex, True);
    end;
  end
  else
  begin
    // Determine background color
    if (gdFocused in aState) and (TempGrid.Selection.Height = 0) and (TempGrid.Selection.Width = 0) and
      ((IsEditing and ((Assigned(TempGrid.Editor) and TempGrid.Editor.Focused) or (Assigned(Memo) and Memo.Focused))) or
      (not IsEditing)) then
    begin
      bgFill := TDarkUtils.ThemeColor(clRowFocused_Light, clRowFocused_Dark);    // Focused
      TempGrid.Canvas.Font.Color := TDarkUtils.ThemeColor(clBlack, clWhite);
    end
    else
    if (gdSelected in aState) and ((TempGrid.Selection.Height > 0) or (TempGrid.Selection.Width > 0)) then
    begin
      bgFill := clHighlight;    // Multiselect
      TempGrid.Canvas.Font.Color := clWhite;
    end
    else
    if gdRowHighlight in aState then
    begin
      bgFill := TDarkUtils.ThemeColor(clRowHighlight_Light, clRowHighlight_Dark); // Highlight
      TempGrid.Canvas.Font.Color := TDarkUtils.ThemeColor(clBlack, clWhite);
    end
    else
    begin
      if (Assigned(Tasks)) and (Tasks.HasTask(ARow)) then
      begin
        Task := Tasks.GetTask(ARow);
        if (ShowColumnDate) and (not Task.Done) and (Task.Date > 0) and (Task.Date < Now) then // Color expired Task
        begin
          bgFill := TDarkUtils.ThemeColor(clRowExpired_Light, clRowExpired_Dark); // Expired warning red
          TempGrid.Canvas.Font.Color := TDarkUtils.ThemeColor(clBlack, clWhite);
        end
        else
        if (not Task.Done) and (Task.Archive) then
        begin
          bgFill := TDarkUtils.ThemeColor(clWhite, clBlack); // Not done but arhive warning color
          TempGrid.Canvas.Font.Color := TDarkUtils.ThemeColor(clRowNotDone_Light, clRowNotDone_Dark);
        end
        else
        begin
          bgFill := TDarkUtils.ThemeColor(clWhite, clBlack); // All other white
          TempGrid.Canvas.Font.Color := Font.Color;
        end;
      end;
    end;

    if (Assigned(Tasks)) and (Tasks.HasTask(ARow)) then
    begin
      Task := Tasks.GetTask(ARow);
      if Task.Star then
        TempGrid.Canvas.Font.Style := TempGrid.Canvas.Font.Style + [fsBold];

      if (aCol = COL_TASK) and (Task.Archive) then
        TempGrid.Canvas.Font.Style := TempGrid.Canvas.Font.Style + [fsStrikeOut];

      if (aCol = COL_NOTE) and (Task.NoteItalic) then
        TempGrid.Canvas.Font.Style := TempGrid.Canvas.Font.Style + [fsItalic];

      if (aCol = COL_DATE) and (Task.Date > Now) and (not (gdSelected in aState)) then
        TempGrid.Canvas.Font.Color := TDarkUtils.ThemeColor(clPlanned_Light, clPlanned_Dark);
    end;

    // Fill the cell background
    TempGrid.Canvas.Brush.Color := bgFill;
    TempGrid.canvas.Brush.Style := bsSolid;
    TempGrid.canvas.FillRect(aRect);

    if (aCol in [COL_DONE, COL_STAR]) then
    begin
      TempGrid.DefaultDrawCell(aCol, aRow, aRect, aState);
      exit;
    end;

    if (aCol = COL_AMOUNT) and (TryStrToFloat(TempGrid.Cells[ACol, ARow], Amount)) then
    begin
      FS := DefaultFormatSettings;
      FS.ThousandSeparator := ' ';
      Value := FormatFloat('#,##0.##########', StrToFloat(TempGrid.Cells[ACol, ARow]), FS);
    end
    else
      Value := TempGrid.Cells[ACol, ARow];

    if (Assigned(Tasks)) and (Tasks.HasTask(ARow)) then
    begin
      if (aCol = COL_TASK) then
      begin
        Task := Tasks.GetTask(ARow);
        Indent := Task.FIndentLevel * Canvas.TextWidth(' ') * 2;
        if Task.Tags.Count > 0 then
        begin
          BitTags := TagEdit.GetTagsBitmap(Task.Tags, Round(Max(Max(Font.Size div 2, 8) * FZoom, 1)),
            Min(ARect.Width, Round(500 * FZoom)), ARect.Height, 2, ifthen(gdSelected in aState, TagsDimnessSelected,
            ifthen(bgFill <> TDarkUtils.ThemeColor(clWhite, clBlack), TagsDimnessColor, TagsDimness)), ColorToRGB(bgFill));
          try
            BitTags.TransparentColor := clWhite;
            BitTags.Transparent := True;
            TagsWidth := BitTags.Width;
            Task.TagsWidth := TagsWidth;
            if TagsWidth < aRect.Width - 50 then
            begin
              if TempGrid.BiDiMode = bdLeftToRight then
                TempGrid.canvas.Draw(aRect.Right - TagsWidth - 5, aRect.Top, BitTags)
              else
                TempGrid.canvas.Draw(aRect.Left + 5, aRect.Top, BitTags);
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
        (Trim(Value) = Trim(FLastText)) and (TempGrid.Selection.Height = 0) and ((aCol <> FLastCol) or (aRow <> FLastRow)) then
      begin
        TempGrid.canvas.Brush.Style := bsSolid;
        TempGrid.canvas.Brush.Color := TDarkUtils.ThemeColor(clDuplicateHighlight_Light, clDuplicateHighlight_Dark);
      end
      else
        TempGrid.canvas.Brush.Style := bsClear;

      DrawRect := aRect;
      DrawRect.Inflate(-4, 0);
      if (aCol = COL_TASK) then
        DrawRect.Inflate(-Indent, 0, 0, 0);

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
        Flags := Flags or longword(ifthen(aCol in [COL_DATE], DT_LEFT, DT_RIGHT))
      else
        Flags := Flags or longword(ifthen(aCol in [COL_AMOUNT], DT_RIGHT, DT_LEFT));
      if FWordWrap then
        Flags := Flags or DT_WORDBREAK;

      DrawText(TempGrid.canvas.handle, PChar(Value), Length(Value), DrawRect, Flags);

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
        Flags := Flags or longword(ifthen(aCol in [COL_DATE], DT_LEFT, DT_RIGHT))
      else
        Flags := Flags or longword(ifthen(aCol in [COL_AMOUNT], DT_RIGHT, DT_LEFT));
      if FWordWrap then
        Flags := Flags or DT_WORDBREAK;

      if (FHideNoteText) and (aCol = COL_NOTE) then
        Value := Value.MaskTextWithBullets(TempGrid.Canvas, FLineEnding.Value);

      if (Value = string.Empty) or (FilterBox.Text = string.Empty) or (TempGrid.canvas.Brush.Color =
        TDarkUtils.ThemeColor(clDuplicateHighlight_Light, clDuplicateHighlight_Dark)) or
        (Pos(Trim(FilterBox.Text).UTF8Lower, ifthen(aCol = COL_AMOUNT, ReplaceStr(Value, ' ', ''), Value).UTF8Lower) = 0) or
        ((FHideNoteText) and (aCol = COL_NOTE)) then
      begin
        DrawText(TempGrid.canvas.handle, PChar(Value), Length(Value), DrawRect, Flags);
      end
      else
      begin
        if (aCol = COL_AMOUNT) then Value := ReplaceStr(Value, ' ', '');

        TempGrid.DrawHighlightedText(
          TempGrid.Canvas,                                 // ACanvas
          DrawRect,                                        // ARect
          GridDrawColors(ifthen(bgFill <> TDarkUtils.ThemeColor(clWhite, clBlack), TagEdit.BlendColors(
          TDarkUtils.ThemeColor(clDuplicateHighlight_Light, clDuplicateHighlight_Dark), bgFill, 50),
          TDarkUtils.ThemeColor(clDuplicateHighlight_Light, clDuplicateHighlight_Dark)), clNone, clNone, clNone), // Colors
          Value,                                           // AText
          Trim(FilterBox.Text),                            // AFilterText
          '',                                              // AHintText
          FWordWrap,                                       // AWordWrap
          False,                                           // AShowLineBreaks
          iif(aCol = COL_DATE, False, iif(aCol = COL_AMOUNT, True, FBiDiRightToLeft))  // ABiDiRightToLeft
          );
      end;
    end;
  end;
end;

procedure TformNotetask.GridSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
var
  sDateTime: TDateTime;
begin
  if FReadOnly then
  begin
    Editor := nil;  // disable editor — grid stays view-only
    exit;
  end;

  if (aCol in [COL_TASK, COL_NOTE, COL_AMOUNT]) then
  begin
    PanelMemo := TPanel.Create(Self);
    PanelMemo.Parent := Grid;
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
    if (Grid.IsCellSelected[aCol, aRow]) and ((Grid.Selection.Height > 0) or (Grid.Selection.Width > 0)) then
    begin
      Memo.Color := clHighlight;
      Memo.Font.Color := clWhite;
    end
    else
    begin
      Memo.Color := TDarkUtils.ThemeColor(clRowFocused_Light, clRowFocused_Dark);
    end;
    Memo.Font.Name := Grid.Font.Name;
    Memo.Font.Size := Grid.Font.Size;
    Memo.Font.Bold := Grid.Cells[COL_STAR, aRow] = '1';
    Memo.HideSelection := False;
    Memo.BorderStyle := bsNone;
    Memo.ScrollBars := ssNone;
    Memo.TabStop := False;
    Memo.WantTabs := True;
    Memo.WordWrap := (FWordWrap) and (aCol <> COL_AMOUNT);
    Memo.WantReturns := aCol in [COL_TASK, COL_NOTE];
    if (FBiDiRightToLeft) then
    begin
      if aCol = COL_AMOUNT then
      begin
        Memo.ParentBiDiMode := False;
        Memo.BiDiMode := bdLeftToRight;
        Memo.Alignment := taRightJustify;
      end
      else
        Memo.BiDiMode := bdRightToLeft;
    end
    else
    begin
      if aCol = COL_AMOUNT then
      begin
        Memo.ParentBiDiMode := False;
        Memo.BiDiMode := bdLeftToRight;
        Memo.Alignment := taRightJustify;
      end
      else
        Memo.BiDiMode := bdLeftToRight;
    end;
    EditControlSetBounds(PanelMemo, aCol, aRow);
    Memo.PopupMenu := PopupMemo;
    Memo.OnEnter := @MemoEnter; // Event Enter
    Memo.OnExit := @MemoExit; // Event Exit
    Memo.OnChange := @MemoChange; // Event Change
    Memo.OnKeyDown := @MemoKeyDown; // Event KeyDown
    Memo.OnMouseDown := @MemoNoteMouseDown; // Event MouseDown
    Memo.OnDblClick := @MemoNoteDblClick; // Event MouseDown
    Memo.OnKeyUp := @MemoNoteKeyUp; // Event KeyUp
    Memo.OnMouseUp := @MemoNoteMouseUp; // Event MouseUp
    if (aCol = COL_AMOUNT) then
      Memo.OnKeyPress := @MemoKeyPress; // Event KeyPress for amount column only
    Memo.Text := Grid.Cells[aCol, aRow];
    Memo.SelStart := Length(Memo.Text);
    Memo.SelLength := 0;
    MemoBackup;

    Editor := PanelMemo;

    if (FIsSelecting) or (Grid.Selection.Height > 0) or (Grid.Selection.Width > 0) then
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
  if (aCol = COL_DATE) then
  begin
    DatePicker := TDateTimePicker.Create(Self);
    DatePicker.Visible := False;
    DatePicker.AutoSize := False;
    DatePicker.BorderStyle := bsNone;
    DatePicker.ArrowShape := asModernLarger;
    DatePicker.Options := [dtpoFlatButton];
    if (FShowTime) then
      DatePicker.Kind := dtkDateTime
    else
      DatePicker.Kind := dtkDate;
    DatePicker.TimeDisplay := tdHMS;
    DatePicker.ParentBiDiMode := False;
    DatePicker.BiDiMode := bdLeftToRight;

    EditControlSetBounds(DatePicker, aCol, aRow, 2, -2, -2, 0);

    if (Grid.Cells[aCol, aRow] = string.Empty) then
      DatePicker.DateTime := Now
    else
    begin
      TryStrToDateTime(Grid.Cells[aCol, aRow], sDateTime);
      DatePicker.DateTime := sDateTime;
    end;

    DatePicker.OnChange := @DatePickerChange; // Event Change
    DatePicker.OnEnter := @DatePickerEnter; // Event Enter
    DatePicker.OnKeyDown := @DatePickerKeyDown; // Event KeyDown

    Editor := DatePicker;

    Application.QueueAsyncCall(@FixDatePickerFont, 0);

    if (FIsSelecting) or (Grid.Selection.Height > 0) or (Grid.Selection.Width > 0) then
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

procedure TformNotetask.GridHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
var
  LastTask: integer;
begin
  EditComplete;
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
    Grid.SortOrder := SortOrder;

    FSortColumn := Index;

    ApplySorting;

    Grid.Row := Tasks.ReverseMap(LastTask);
  end
  else
    // Set LastTask when clicked on begining of LastTask
  begin
    if (ssShift in GetKeyShiftState) and (Grid.Selection.Height = 0) and (Grid.Selection.Top <> index) then
    begin
      Grid.Selection := TGridRect.Create(COL_DONE, Grid.Selection.Top, COL_STAR, index);
    end
    else
    begin
      Grid.Row := index;
      Grid.Selection := TGridRect.Create(COL_DONE, index, COL_STAR, index);
    end;
    // Trigger event
    Grid.OnSelection(Grid, Grid.Col, Grid.Row);
  end;
end;

procedure TformNotetask.GridSetCheckboxState(Sender: TObject; ACol, ARow: integer; const Value: TCheckboxState);
var
  MousePosScreen, MousePosClient, CheckBoxCenter: TPoint;
  CheckBoxRect: TRect;
  CheckBoxSize: integer;
begin
  if (aCol = COL_DONE) then
  begin
    // Define checkbox area size (16x16)
    CheckBoxSize := 14;

    // Get mouse position in screen coordinates
    MousePosScreen := Mouse.CursorPos;

    // Convert screen coordinates to client coordinates (relative to the form)
    MousePosClient := Grid.ScreenToClient(MousePosScreen);

    // Get the center of the checkbox (approximately the center of the cell)
    CheckBoxCenter := Grid.CellRect(ACol, ARow).CenterPoint;

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
  if (aCol = COL_STAR) then
  begin
    // Get mouse position in screen coordinates
    MousePosScreen := Mouse.CursorPos;

    // Convert screen coordinates to client coordinates (relative to the form)
    MousePosClient := Grid.ScreenToClient(MousePosScreen);

    // Check if the mouse is within the 16x16 checkbox area
    if not PtInRect(Grid.CellRect(ACol, ARow), MousePosClient) then
    begin
      // If the mouse is outside the checkbox, prevent the state from being changed
      FDisableStarToggle := True;
      exit;
    end;
    FDisableStarToggle := False;
    exit;
  end;
end;

procedure TformNotetask.GridCheckboxToggled(Sender: TObject; aCol, aRow: integer; aState: TCheckboxState);
begin
  if (aCol = COL_DONE) then
  begin
    if (FDisableCheckToggle) then exit;

    CompleteTasks(aRow);
  end
  else
  if (aCol = COL_STAR) then
  begin
    if (FDisableStarToggle) then exit;

    StarTasks(aRow);
  end;
end;

procedure TformNotetask.GridColRowInserted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
begin
  if (not IsColumn) then
  begin
    if FBackup then
    begin
      GridBackupSelection;
      Tasks.CreateBackup;
    end;
    Tasks.AddMap(Tasks.AddTask('[ ]'));
    Grid.Cells[COL_DONE, tIndex] := '0';
    SetInfo;
    Changed := True;
    SetNote;
    SetTags;
  end;
end;

procedure TformNotetask.GridColRowDeleted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
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

procedure TformNotetask.GridHeaderSized(Sender: TObject; IsColumn: boolean; Index: integer);
begin
  GridResize(Sender);
  if IsColumn then
    CalcRowHeight(True);
  EditControlSetBounds(PanelMemo, Grid.Col, Grid.Row);
  EditControlSetBounds(DatePicker, Grid.Col, Grid.Row, 2, -2, -2, 0);
end;

procedure TformNotetask.GridSelectCell(Sender: TObject; aCol, aRow: integer; var CanSelect: boolean);
begin
  FIsSelecting := True;
  AdjustMultiButton;
end;

procedure TformNotetask.GridKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  TempGrid: TStringGrid;
  i: integer;
  //Sel:TGridRect;
  //Col: integer;
begin
  TempGrid := Sender as TStringGrid;

  // Remove due to conflict with standard selection behavior
  //// Shift + Home -> select from current position to first visible column
  //if (Key = VK_HOME) and (ssShift in Shift) and not (ssCtrl in Shift) then
  //begin
  //  Sel:=  TempGrid.Selection;
  //  Col := TempGrid.Col;
  //  for i := 1 to TempGrid.ColCount - 1 do
  //    if TempGrid.ColWidths[i] > 0 then
  //    begin
  //      TempGrid.Col := i;
  //      Break;
  //    end;
  //  TempGrid.ClearSelections;
  //  TempGrid.Selection := Rect(TempGrid.Col, Sel.Top, Sel.Right, Sel.Bottom);
  //  TempGrid.Update;
  //  Key := 0;
  //  Exit;
  //end
  //else
  //// Shift + End -> select from current position to last visible column
  //if (Key = VK_END) and (ssShift in Shift) and not (ssCtrl in Shift) then
  //begin
  //  Sel:=  TempGrid.Selection;
  //  Col := TempGrid.Col;

  //  for i := TempGrid.ColCount - 1 downto 0 do
  //    if TempGrid.ColWidths[i] > 0 then
  //    begin
  //      TempGrid.Col := i;
  //      Break;
  //    end;
  //  TempGrid.ClearSelections;
  //  TempGrid.Selection := Rect(Sel.Left, Sel.Top, TempGrid.Col, Sel.Bottom);
  //  TempGrid.Update;
  //  Key := 0;
  //  Exit;
  //end
  //else

  // Default HOME -> move to first visible column
  if (Key = VK_HOME) and not (ssCtrl in Shift) and not (ssShift in Shift) then
  begin
    for i := 1 to TempGrid.ColCount - 1 do
      if TempGrid.ColWidths[i] > 0 then
      begin
        TempGrid.Col := i;
        Break;
      end;
    Key := 0;
    Exit;
  end
  else
  // Default END -> move to last visible column
  if (Key = VK_END) and not (ssCtrl in Shift) and not (ssShift in Shift) then
  begin
    for i := TempGrid.ColCount - 1 downto 0 do
      if TempGrid.ColWidths[i] > 0 then
      begin
        TempGrid.Col := i;
        Break;
      end;
    Key := 0;
    Exit;
  end;
end;

procedure TformNotetask.GridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FIsSelecting := False;

  if (Button = mbMiddle) and (ssCtrl in Shift) then // Middle button + Ctrl
    aZoomDefault.Execute;
end;

procedure TformNotetask.GridMouseLeave(Sender: TObject);
begin
  FIsSelecting := False;
end;

procedure TformNotetask.GridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  Cell: TPoint;
begin
  if (Button = mbRight) and (not IsEditing) then
  begin
    if (Grid.Selection.Height = 0) then
    begin
      // Get the row index at the mouse coordinates
      Cell := Grid.MouseToCell(TPoint.Create(X, Y));

      // Check if the row index is valid
      if (Cell.Y >= 0) and (Cell.Y < Grid.RowCount) then
        Grid.Row := Cell.Y;
      if (Cell.X > 0) and (Cell.X < 5) then
        Grid.Col := Cell.X;

      if Visible and Grid.Visible and Grid.CanFocus then
        Grid.SetFocus;
    end;
    Popup.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;

  if (Button = mbLeft) and (ssCtrl in Shift) and (Grid.Col in [COL_TASK, COL_NOTE]) then
    TryOpenAsUrl(Trim(Grid.Cells[Grid.Col, Grid.Row]));

  if (not FRepaint) then
  begin
    FRepaint := True;
    GridInvalidate;
  end;
end;

procedure TformNotetask.GridMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  if IsEditing then
    EditComplete;

  if ssCtrl in Shift then
  begin
    if WheelDelta > 0 then
      aZoomIn.Execute
    else
      aZoomOut.Execute;
    Handled := True;
  end;
end;

procedure TformNotetask.GridResize(Sender: TObject);
var
  Rect: TRect;
begin
  {$IFDEF UNIX}
  GridAdjustScrollBars;
  {$ENDIF}

  // Get the cell dimensions
  Rect := Grid.CellRect(Grid.Col, Grid.Row);

  // Update the size and position of the Memo
  if Assigned(Grid.Editor) and (Grid.Editor is TPanel) then
    TPanel(Grid.Editor).SetBounds(Rect.Left + 5, Rect.Top + 1, Rect.Right - Rect.Left - 10, Rect.Bottom - Rect.Top - 3);

  // Align panelFunc to bottom-right of Grid
  if FBiDiRightToLeft then
  begin
    if Grid.GetActualScrollBarVisibility(ssVertical) then
      panelFunc.Left := Grid.Left + GetSystemMetrics(SM_CXVSCROLL) + 5
    else
      panelFunc.Left := Grid.Left + 5;
  end
  else
  begin
    if Grid.GetActualScrollBarVisibility(ssVertical) then
      panelFunc.Left := Grid.Left + Grid.Width - panelFunc.Width - GetSystemMetrics(SM_CXVSCROLL) - 5
    else
      panelFunc.Left := Grid.Left + Grid.Width - panelFunc.Width - 5;
  end;
  if Grid.GetActualScrollBarVisibility(ssHorizontal) then
    panelFunc.Top := Grid.Top + Grid.Height - panelFunc.Height - GetSystemMetrics(SM_CYHSCROLL) - 5
  else
    panelFunc.Top := Grid.Top + Grid.Height - panelFunc.Height - 5;
end;

procedure TformNotetask.GridTopLeftChanged(Sender: TObject);
begin
  EditComplete;

  if Grid.TopRow = 1 then
    Application.QueueAsyncCall(@DelayedInvalidate, 0);

  if Grid.TopRow + Grid.VisibleRowCount >= Grid.RowCount then
    Application.QueueAsyncCall(@DelayedInvalidate, 0);
end;

procedure TformNotetask.GridUserCheckboxBitmap(Sender: TObject; const aCol, aRow: integer;
  const CheckedState: TCheckboxState; var ABitmap: TBitmap);
begin
  // Check if we're in the correct column
  if aCol = COL_DONE then
  begin
    // Assign the appropriate bitmap based on the CheckedState
    if CheckedState = cbChecked then
      ABitmap := ResourceBitmapCheck // Use check bitmap
    else
      ABitmap := ResourceBitmapUncheck; // Use uncheck bitmap
  end
  else
  if aCol = COL_STAR then
  begin
    // Assign the appropriate bitmap based on the CheckedState
    if CheckedState = cbChecked then
      ABitmap := ResourceBitmapStarGold // Use check bitmap
    else
      ABitmap := ResourceBitmapStarGray; // Use uncheck bitmap
  end;
end;

procedure TformNotetask.GridColRowMoved(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
begin
  if (not IsColumn) then
  begin
    Tasks.MoveTask(sIndex, tIndex);
    FillGrid;
    Changed := True;
  end;
end;

procedure TformNotetask.GridSelection(Sender: TObject; aCol, aRow: integer);
var
  Modified: boolean = False;
begin
  if (Grid.Selection.Height > 0) or (FLastSelectionHeight > 0) then
    SetInfo;

  FLastText := string.Empty;
  AdjustMultiButton;

  if (aRow <> FLastRow) or (Grid.Selection.Top <> FLastSelection.Top) or (Grid.Selection.Bottom <> FLastSelection.Bottom) then
  begin
    FLastRow := aRow;
    Modified := True;
    SetNote;
    SetTags;
  end;

  if (aCol <> FLastCol) or (Grid.Selection.Left <> FLastSelection.Left) or (Grid.Selection.Right <> FLastSelection.Right) then
  begin
    FLastCol := aCol;
    Modified := True;
  end;
  if Modified then
    ChangeLastText(Grid.Cells[aCol, aRow], aCol, aRow);

  // Save row to mem
  if Length(FLastRowMem) > FindGroupRealIndex(TabsGroup.TabIndex) then
    FLastRowMem[FindGroupRealIndex(TabsGroup.TabIndex)] := aRow;

  FLastSelectionHeight := Grid.Selection.Height;
  FLastSelection := Grid.Selection;
end;

{%EndRegion}

{%Region -fold All Events}

procedure TformNotetask.btnMultiClick(Sender: TObject);
begin
  if btnMulti.ImageIndex in [0, 1] then aInsertTask.Execute
  else
  if btnMulti.ImageIndex in [2, 3] then aDuplicateTasks.Execute;
end;

procedure TformNotetask.FilterBoxChange(Sender: TObject);
var
  LastTask, LastTab: integer;
  LastSelTop, LastSelBottom: integer;
  LastRect, NewRect: TGridRect;
begin
  if FLastFilter = FilterBox.Text then Exit;
  FLastFilter := FilterBox.Text;

  LastTask := Tasks.Map(Grid.Row);
  LastTab := FindGroupRealIndex(TabsGroup.TabIndex);
  LastSelTop := Tasks.Map(Grid.Selection.Top);
  LastSelBottom := Tasks.Map(Grid.Selection.Bottom);
  LastRect := Grid.Selection;

  if (Trim(FilterBox.Text) <> string.Empty) and (FLastTabFilter < 0) then
  begin
    FLastTabFilter := LastTab;
  end;
  SetTabs;
  FillGrid;

  if (LastTab = FindGroupRealIndex(TabsGroup.TabIndex)) then
    Grid.Row := Tasks.ReverseMap(LastTask);

  if Trim(FilterBox.Text) = string.Empty then
    FLastTabFilter := -1;

  NewRect := Rect(LastRect.Left, Tasks.ReverseMap(LastSelTop), LastRect.Right, Tasks.ReverseMap(LastSelBottom));
  if (LastRect.Height = NewRect.Height) then
    Grid.Selection := NewRect
  else
    Grid.ClearSelections;
  CalcRowHeight;
  ChangeLastText;
  SetInfo;
  SetNote;
  SetTags;
end;

procedure TformNotetask.FilterBoxKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  SelStart, SelLen: integer;
  ClipText: string;
begin
  SelStart := FilterBox.SelStart;
  SelLen := FilterBox.SelLength;

  case Key of
    VK_DELETE: // Delete
    begin
      if SelLen > 0 then
        FilterBox.Text := string(Copy(unicodestring(FilterBox.Text), 1, SelStart) +
          Copy(unicodestring(FilterBox.Text), SelStart + SelLen + 1, MaxInt))
      else
        FilterBox.Text := string(Copy(unicodestring(FilterBox.Text), 1, SelStart) +
          Copy(unicodestring(FilterBox.Text), SelStart + 2, MaxInt));
      FilterBox.SelStart := SelStart;
      Key := 0;
    end;

    Ord('A'): if ssCtrl in Shift then // Ctrl + A
      begin
        FilterBox.SelectAll;
        Key := 0;
      end;

    Ord('C'): if ssCtrl in Shift then // Ctrl + C
      begin
        if SelLen > 0 then
        begin
          ClipText := string(Copy(unicodestring(FilterBox.Text), SelStart + 1, SelLen));
          Clipboard.AsText := ClipText;
        end;
        Key := 0;
      end;

    Ord('X'): if ssCtrl in Shift then // Ctrl + X
      begin
        if SelLen > 0 then
        begin
          ClipText := string(Copy(unicodestring(FilterBox.Text), SelStart + 1, SelLen));
          Clipboard.AsText := ClipText;
          FilterBox.Text := string(Copy(unicodestring(FilterBox.Text), 1, SelStart) +
            Copy(unicodestring(FilterBox.Text), SelStart + SelLen + 1, MaxInt));
        end;
        Key := 0;
      end;

    Ord('V'): if ssCtrl in Shift then // Ctrl + V
      begin
        ClipText := Clipboard.AsText;
        FilterBox.Text := string(Copy(unicodestring(FilterBox.Text), 1, SelStart) + unicodestring(ClipText) +
          Copy(unicodestring(FilterBox.Text), SelStart + SelLen + 1, MaxInt));
        FilterBox.SelStart := SelStart + Length(unicodestring(ClipText));
        Key := 0;
      end;

    VK_UP, VK_DOWN:
      if (((FilterBox.GetTextLen > 0) and (FilterBox.SelLength = 0)) or (FilterBox.Items.Count = 0)) and
        (Visible and Grid.Visible and Grid.CanFocus) then
      begin
        Grid.SetFocus;
        Key := 0;
      end;
    VK_ESCAPE:
      if Visible and Grid.Visible and Grid.CanFocus then
        Grid.SetFocus;
    else
  end;
  FilterBox.OnChange(Self);
end;

procedure TformNotetask.filterClearClick(Sender: TObject);
begin
  FilterBox.Text := string.Empty;
  FilterBox.OnChange(Self);
  if Visible and Grid.Visible and Grid.CanFocus then
    Grid.SetFocus;
end;

procedure TformNotetask.SplitFilterChangeBounds(Sender: TObject);
begin
  UpdateComboRegion(FilterBox);
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

{%EndRegion}

{%Region -fold Private Setters}

procedure TformNotetask.SetChanged(Value: boolean);
begin
  if (Value = False) then
    Grid.Modified := False;

  FChanged := Grid.Modified or Value;
  aSave.Enabled := FChanged and not FReadOnly;
  aUndo.Enabled := FChanged;
  aUndoAll.Enabled := FChanged;
  SetCaption;
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

  function SameFloat(A, B: double; Eps: double): boolean;
  begin
    // Compare floats with epsilon
    Result := Abs(A - B) < Eps;
  end;

begin
  FZoom := Value;
  Grid.Font.Assign(Font);
  Grid.Font.Size := Round(Max(1, FOriginalFontSize * FZoom));
  MemoNote.Font.Assign(Font);
  MemoNote.Font.Size := Grid.Font.Size;
  if Assigned(Memo) then
  begin
    Memo.Font.Assign(Font);
    Memo.Font.Size := Grid.Font.Size;
  end;
  if Assigned(DatePicker) then
  begin
    DatePicker.Font.Assign(Font);
    DatePicker.Font.Size := Grid.Font.Size;
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

  if FFitRowHeightToFont then
    Grid.DefaultRowHeight := Grid.Canvas.TextHeight('Wg') + 2;
  CalcRowHeight(True);
  SetInfo;
end;

procedure TformNotetask.SetBiDiRightToLeft(Value: boolean);
var
  i: integer;
begin
  FBiDiRightToLeft := Value;

  if (Value) then
  begin
    Grid.BiDiMode := bdRightToLeft;
    TabsGroup.BiDiMode := bdRightToLeft;
    for i := 1 to Grid.Columns.Count - 2 do
    begin
      if (i in [COL_AMOUNT - 1]) then
        Grid.Columns[i].Alignment := taLeftJustify
      else
        Grid.Columns[i].Alignment := taRightJustify;
    end;
    FilterBox.BiDiMode := bdRightToLeft;
    MemoNote.BiDiMode := bdRightToLeft;
    MemoNote.Alignment := taRightJustify;
    MemoNote.BorderSpacing.Left := 0;
    MemoNote.BorderSpacing.Right := 10;
    TagEdit.BiDiMode := bdRightToLeft;
    TOS.SetCursorTo(panelNote, 'RIGHTARROW');
  end
  else
  begin
    Grid.BiDiMode := bdLeftToRight;
    TabsGroup.BiDiMode := bdLeftToRight;
    FilterBox.BiDiMode := bdLeftToRight;
    MemoNote.BiDiMode := bdLeftToRight;
    MemoNote.Alignment := taLeftJustify;
    MemoNote.BorderSpacing.Left := 10;
    MemoNote.BorderSpacing.Right := 0;
    MemoNote.BiDiMode := bdLeftToRight;
    TagEdit.BiDiMode := bdLeftToRight;
    TOS.SetCursorTo(panelNote, 'LEFTARROW');
    for i := 1 to Grid.Columns.Count - 2 do
    begin
      if (i = COL_AMOUNT - 1) then
        Grid.Columns[i].Alignment := taRightJustify
      else
        Grid.Columns[i].Alignment := taLeftJustify;
    end;
  end;
  GridResize(Self);
end;

procedure TformNotetask.SetShowStatusBar(Value: boolean);
begin
  FShowStatusBar := Value;

  aShowStatusBar.Checked := FShowStatusBar;
  StatusBar.Visible := FShowStatusBar;
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
  PanelTags.Visible := FShowTags;
  SplitTags.Visible := FShowTags;

  AlignBottomControls;

  if Visible and PanelTags.Visible and TagEdit.CanFocus then
    TagEdit.SetFocus;

  SetTags;
end;

procedure TformNotetask.SetShowNote(Value: boolean);
begin
  FShowNote := Value;

  aShowNote.Checked := FShowNote;
  panelNote.Visible := FShowNote;
  Splitter.Visible := FShowNote;

  AlignBottomControls;

  if Visible and panelNote.Visible and MemoNote.CanFocus then
    MemoNote.SetFocus;

  SetNote;
end;

procedure TformNotetask.SetHideNoteText(Value: boolean);
begin
  aHideNoteText.Checked := Value;
  FHideNoteText := Value;
end;

procedure TformNotetask.SetShowArchived(Value: boolean);
var
  LastTask: integer;
  LastTab: integer;
begin
  LastTask := Tasks.Map(Grid.Row);
  LastTab := FindGroupRealIndex(TabsGroup.TabIndex);
  FShowArchived := Value;
  aShowArchived.Checked := FShowArchived;
  SetTabs;
  FillGrid;
  if (LastTab = FindGroupRealIndex(TabsGroup.TabIndex)) then
    Grid.Row := Tasks.ReverseMap(LastTask);
  ResetRowHeight;
  SetInfo;
  SetNote;
  SetTags;
end;

procedure TformNotetask.SetShowColumnDone(Value: boolean);
begin
  FShowColumnDone := Value;
  Grid.Columns.Items[COL_DONE - 1].Visible := FShowColumnDone;
end;

procedure TformNotetask.SetShowColumnTask(Value: boolean);
begin
  FShowColumnTask := Value;
  Grid.Columns.Items[COL_TASK - 1].Visible := FShowColumnTask;
  CalcRowHeight(True);
end;

procedure TformNotetask.SetShowColumnNote(Value: boolean);
begin
  FShowColumnNote := Value;
  Grid.Columns.Items[COL_NOTE - 1].Visible := FShowColumnNote;
  CalcRowHeight(True);
end;

procedure TformNotetask.SetShowColumnAmount(Value: boolean);
begin
  FShowColumnAmount := Value;
  Grid.Columns.Items[COL_AMOUNT - 1].Visible := FShowColumnAmount;
  SetInfo;
end;

procedure TformNotetask.SetShowColumnDate(Value: boolean);
begin
  FShowColumnDate := Value;
  Grid.Columns.Items[COL_DATE - 1].Visible := FShowColumnDate;
end;

procedure TformNotetask.SetShowColumnFavorite(Value: boolean);
begin
  FShowColumnFavorite := Value;
  Grid.Columns.Items[COL_STAR - 1].Visible := FShowColumnFavorite;
end;

{%EndRegion}

{%Region -fild Private Methods}

procedure TformNotetask.CalcDefaultColWidth;
begin
  if (FShowDuration) then
    Grid.DefaultColWidth := Round((Canvas.TextWidth('10.10sec') + 10) * FZoom)
  else
    Grid.DefaultColWidth := Round(Canvas.TextWidth('10000') * FZoom);
end;

procedure TformNotetask.ResetRowHeight(aCalcRowHeight: boolean = True; aRow: integer = 0);
var
  i: integer;
  h: integer;
begin
  Grid.BeginUpdate;
  try
    h := Canvas.TextHeight('A') + 2;

    // if -1 only selection
    if (aRow = -1) then
    begin
      for i := Grid.Selection.Top to Grid.Selection.Bottom do
        Grid.RowHeights[i] := h;
    end
    else
    // if 0 for all rows
    if (aRow = 0) then
    begin
      for i := 1 to Grid.RowCount - 1 do
      begin
        if Grid.RowHeights[i] <> h then
          Grid.RowHeights[i] := h;
      end;
    end
    else // if valid row just that row
      Grid.RowHeights[aRow] := Grid.DefaultRowHeight;

    if (Assigned(Memo)) and ((aRow = 0) or (aRow = Grid.Row)) then
      Memo.Height := h;

    if (aCalcRowHeight) then
      CalcRowHeight(False, aRow);
  finally
    Grid.EndUpdate;
  end;
end;

procedure TformNotetask.CalcRowHeight(aForce: boolean = False; aRow: integer = 0);
var
  FromRow, ToRow: integer;

  procedure CalcCol(col: integer; force: boolean = False);
  var
    row: integer;
    drawrect: TRect;
    Text: string;
    task: TTask;
    Flags: cardinal;
    h: integer;
    OldBold: boolean;
  begin
    OldBold := Grid.Canvas.Font.Bold;
    for row := FromRow to ToRow do
    begin
      task := Tasks.GetTask(row);
      if aForce or (task.FRowHeight = 0) then
      begin
        drawrect := Grid.CellRect(col, row);
        drawrect.Inflate(-4, 0);

        Text := Grid.Cells[col, row];
        if Text = string.Empty then Text := 'Wg';

        // Reduce text area by TagsWidth for text measurement
        if (col in [COL_TASK, COL_NOTE]) then
        begin
          if task.Star then
            Grid.Canvas.Font.Bold := True;

          if (col = COL_TASK) then
          begin
            if task.TagsWidth < drawrect.Width then
            begin
              if FBiDiRightToLeft then
                drawrect.Left := drawrect.Left + task.TagsWidth  // For RTL: reserve space on the left
              else
                drawrect.Right := drawrect.Right - task.TagsWidth; // For LTR: reserve space on the right
            end;
          end;
        end;

        Flags := DT_CALCRECT;
        if FBiDiRightToLeft then
          Flags := Flags or longword(ifthen(col in [COL_DATE], DT_LEFT, DT_RIGHT))
        else
          Flags := Flags or longword(ifthen(col in [COL_AMOUNT], DT_RIGHT, DT_LEFT));
        if FWordWrap then
          Flags := Flags or DT_WORDBREAK;

        {$IFDEF UNIX}
        Text := StringReplace(Text, #$0A, #$0A+ '+', [rfReplaceAll]);
        {$ENDIF}

        DrawText(Grid.canvas.handle, PChar(Text), Length(Text), drawrect, Flags);
        Grid.Canvas.Font.Bold := OldBold;

        // The greater than sign is important because values may differ across fields, need max
        if force or (abs(drawrect.bottom - drawrect.top) > Grid.RowHeights[row]) then
        begin
          h := drawrect.bottom - drawrect.top + 2;
          if (force) and (h < Grid.DefaultRowHeight) then
            h := Max(Grid.Canvas.TextHeight('Wg') + 2, integer(Round(Grid.DefaultRowHeight * FZoom)));
          FLastRowHeights[row] := h;
          Grid.RowHeights[row] := h;
          task.FRowHeight := h;
        end
        else
          FLastRowHeights[row] := Grid.RowHeights[row];
      end
      else
      begin
        FLastRowHeights[row] := task.FRowHeight;
        Grid.RowHeights[row] := task.FRowHeight;
      end;
    end;
  end;

begin
  Grid.BeginUpdate;
  try
    SetLength(FLastRowHeights, Grid.RowCount);

    // if -1 only selection
    if aRow = -1 then
    begin
      FromRow := Grid.Selection.Top;
      ToRow := Grid.Selection.Bottom;
    end
    else
    // if 0 for all rows
    if aRow = 0 then
    begin
      FromRow := 1;
      ToRow := Grid.RowCount - 1;
    end
    else // if valid row just that row
    begin
      FromRow := aRow;
      ToRow := aRow;
    end;

    // Force applies only to the priority column
    if (ShowColumnTask) then CalcCol(COL_TASK, aForce);
    if (ShowColumnNote) then CalcCol(COL_NOTE);

    // Header, tabs, first col
    Grid.RowHeights[0] := Round(Max(Canvas.TextHeight('Wg') + 4, Grid.DefaultRowHeight) * FZoom);
    if (aForce) then
    begin
      {$IFDEF UNIX}
      panelTabs.Height := Canvas.TextHeight('Wg') + 11;
      {$ELSE}
      panelTabs.Height := Canvas.TextHeight('Wg') + 8;
      {$ENDIF}
      CalcDefaultColWidth;
    end;

    EditControlSetBounds(PanelMemo, Grid.Col, Grid.Row);
  finally
    Grid.EndUpdate;
  end;
end;

function TformNotetask.LastRowHeight(aRow: integer): integer;
begin
  if (Length(FLastRowHeights) > aRow) then
    Result := FLastRowHeights[aRow]
  else
    Result := Grid.DefaultRowHeight;
end;

procedure TformNotetask.ChangeLastText(Value: string = string.Empty; aCol: integer = -1; aRow: integer = -1);
begin
  if aCol < 0 then aCol := Grid.Col;
  if aRow < 0 then aRow := Grid.Row;
  if Value = string.Empty then Value := Grid.Cells[aCol, aRow];
  if (aCol > 0) and (aRow > 0) then
  begin
    if FDuplicateHighlight and ((FLastText <> string.Empty) or (Value <> string.Empty)) then
    begin
      FLastText := Value;
      if Tasks.HasDuplicateMatches(FLastText) and (Grid.Selection.Height = 0) then
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
      exit;
    end;
  end;
  FLastText := string.Empty;
end;

procedure TformNotetask.DelayedInvalidate(Data: PtrInt);
begin
  Repaint;
end;

procedure TformNotetask.FixDatePickerFont(Data: PtrInt);
begin
  if Assigned(DatePicker) then
  begin
    DatePicker.ParentFont := False;
    DatePicker.Font.Name := Grid.Font.Name;
    DatePicker.Font.Size := Grid.Font.Size;
    DatePicker.Font.Bold := Grid.Cells[COL_STAR, Grid.Row] = '1';
  end;
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

procedure TformNotetask.DelayedFinishTagEdit(Data: PtrInt);
begin
  if (Trim(TagEdit.EditBox.Text) <> string.Empty) then
  begin
    TagsAdd(FLastGridSelection, TagEdit.EditBox.Text);
    TagEdit.EditBox.Text := string.Empty;
    SetTags;
  end;
end;

procedure TformNotetask.NewFile(SaveSetting: boolean = True);
var
  new: TStringList;
begin
  if IsCanClose then
  begin
    EditComplete;

    // Save settings for current file
    if SaveSetting and FGridSettingsLoaded then
      SaveGridSettings(Self, Grid, ExtractFileName(FFileName));

    Grid.Clean;
    Grid.RowCount := 2;
    TagEdit.SuggestedItems.Clear;
    FilterBox.Clear;
    FLastFilter := '-1';

    new := TStringList.Create;
    new.Add('[ ]');
    if Assigned(Tasks) then Tasks.Free;
    Tasks := TTasks.Create(new);

    FFileName := string.Empty;
    panelTabs.Visible := False;

    FEncrypted := False;
    TCrypto.FreeBytesSecure(FKeyEnc);
    TCrypto.FreeBytesSecure(FKeyAuth);
    TCrypto.FreeBytesSecure(FSalt);

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

    // Load saved settings for new file
    FGridSettingsLoaded := LoadGridSettings(Self, Grid, string.Empty);

    ApplyGridSettings;
    SetZoom(FZoom);

    SetFilter;

    Grid.Row := 1;
    Grid.Selection := Rect(Grid.Selection.Left, Grid.Row, Grid.Selection.Right, Grid.Row);

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
  if saveSettings and FGridSettingsLoaded then
    SaveGridSettings(Self, Grid, ExtractFileName(FFileName));

  EncryptedOld := FEncrypted;
  FEncrypted := False;

  KeyEncOld := TCrypto.CopyBytes(FKeyEnc);
  KeyAuthOld := TCrypto.CopyBytes(FKeyAuth);
  SaltOld := TCrypto.CopyBytes(FSalt);
  TCrypto.FreeBytesSecure(FKeyEnc);
  TCrypto.FreeBytesSecure(FKeyAuth);
  TCrypto.FreeBytesSecure(FSalt);
  FileNameOld := FFileName;
  FFileName := fileName;
  EditComplete;

  FreeFile;
  ReadOnly := not TFileManager.TryLockFile(FFileName, FSReserved);

  if (TCrypto.CheckEncryptedFile(FFileName)) then
  begin
    FEncrypted := True;
    Bytes := TCrypto.DecryptData(TCrypto.LoadFileAsBytes(FFileName), string.Empty, FSalt, FKeyEnc, FKeyAuth);
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
            FKeyEnc := TCrypto.CopyBytes(KeyEncOld);
            FKeyAuth := TCrypto.CopyBytes(KeyAuthOld);
            FSalt := TCrypto.CopyBytes(SaltOld);
            exit(False);
          end;
        end;

        Bytes := TCrypto.DecryptData(TCrypto.LoadFileAsBytes(FFileName), Token, FSalt, FKeyEnc, FKeyAuth);
        if (Bytes <> nil) then
          TFileManager.ReadTextFile(Bytes, Content, FEncoding, FLineEnding, FLineCount)
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
            FKeyEnc := TCrypto.CopyBytes(KeyEncOld);
            FKeyAuth := TCrypto.CopyBytes(KeyAuthOld);
            FSalt := TCrypto.CopyBytes(SaltOld);
            exit(False);
          end;
        end;
      finally
        TCrypto.ClearStringSecure(Token);
        TCrypto.FreeBytesSecure(KeyEncOld);
        TCrypto.FreeBytesSecure(KeyAuthOld);
        TCrypto.FreeBytesSecure(SaltOld);
        Hide;
      end;
    end
    else
      TFileManager.ReadTextFile(Bytes, Content, FEncoding, FLineEnding, FLineCount);
  end
  else
    TFileManager.ReadTextFile(FFileName, Content, FEncoding, FLineEnding, FLineCount);

  TagEdit.SuggestedItems.Clear;
  if Assigned(Tasks) then
    Tasks.Free;
  Tasks := TTasks.Create(Content.ToStringList);

  // Load saved settings for file
  FGridSettingsLoaded := LoadGridSettings(Self, Grid, ExtractFileName(FFileName));
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
            TCrypto.FreeBytesSecure(FSalt);
            TCrypto.FreeBytesSecure(FKeyEnc);
            TCrypto.FreeBytesSecure(FKeyAuth);
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
        TCrypto.FreeBytesSecure(FSalt);
        TCrypto.FreeBytesSecure(FKeyEnc);
        TCrypto.FreeBytesSecure(FKeyAuth);
      end;

      TaskList := Tasks.ToStringList;
      if Assigned(TaskList) and (TaskList <> nil) then
      begin
        try
          try
            EditComplete;
            FreeFile;
            TFileManager.SaveTextFile(fileName, TaskList, FEncoding, FLineEnding, FEncrypted, Token, FSalt, FKeyEnc, FKeyAuth);
            SetChanged(False);
            Tasks.CreateBackupInit;
            ReadOnly := not TFileManager.TryLockFile(fileName, FSReserved);
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
    TCrypto.ClearStringSecure(Token);
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

  FilterBox.Left := 0;
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

  Grid.Row := 1;
  Grid.Col := COL_TASK;
  TabsGroup.TabIndex := 0;
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
  //if (StatusBar.Top < panelNote.Top) or (StatusBar.Top < PanelTags.Top) then
  //  StatusBar.Top := ClientHeight - StatusBar.Height;

  // Start from the bottom of the client area
  BottomPos := ClientHeight;

  // Align StatusBar at the very bottom
  StatusBar.Top := BottomPos - StatusBar.Height;
  BottomPos := StatusBar.Top;

  // Align PanelTags above SplitTags
  PanelTags.Top := BottomPos - PanelTags.Height;
  BottomPos := PanelTags.Top;

  // Align SplitTags above PanelTags
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
    PanelTags.Top := SplitTags.Top + SplitTags.Height;
    StatusBar.Top := PanelTags.Top + PanelTags.Height;
  end;
end;

function TformNotetask.IsExecuteValueNote(memoPriority: boolean = False): boolean;
begin
  Result := (((Grid.Selection.Left = COL_NOTE) and (Grid.Selection.Right >= COL_NOTE)) or
    ((panelNote.Visible) and ((memoPriority) or (MemoNote.SelLength > 0) or (MemoNote.Focused)))) and
    (panelNote.Visible) and (MemoNote.SelLength > 0);
end;

function TformNotetask.GetExecuteValue(aRow: integer; memoPriority: boolean = False): string;
begin
  // If note column is selected or note panel visible
  if (((Grid.Selection.Left = COL_NOTE) and (Grid.Selection.Right >= COL_NOTE)) or ((panelNote.Visible) and
    ((memoPriority) or (MemoNote.SelLength > 0) or (MemoNote.Focused)))) then
  begin
    if (panelNote.Visible) and (MemoNote.SelLength > 0) then
      Result := MemoNote.SelText
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
  Value := GetExecuteValue(Grid.Row);

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
      OpenURL(rchatgpt + Trim(formMemoText.memoText.Text).AsEncodedUrl);
    end;
  finally
    Hide;
  end;
end;

function TformNotetask.TryOpenAsUrl(Value: string): boolean;
begin
  Result := False;
  if Value.IsUrlSimilar then
  begin
    OpenURL(IfThen(Value.HasUrlScheme, Value, http + Value));
    Result := True;
  end
  else
  if Value.IsEmail then
  begin
    OpenURL(IfThen(AnsiStartsText(mailto, Value), Value, mailto + Value));
    Result := True;
  end;
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
    ConsoleEncoding := TOS.GetConsoleEncoding;

    {$IFDEF UNIX}
    Script.Add('#!/bin/bash');
    {$ENDIF}

    for i := Grid.Selection.Top to Grid.Selection.Bottom do
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
        TFileManager.SaveTextFile(TempFile, Script, ScriptEncoding, TLineEnding.WindowsCRLF);
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
      PwshPath := TFileManager.FindPowerShellCore; // Search for pwsh.exe
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

procedure TformNotetask.MoveTabLeft(Index: integer);
var
  Result: integer;
  RowMem: integer = -1;
begin
  if (Index = 1) and (Tasks.GroupNames[0] = string.Empty) then exit;
  if TabsGroup.Tabs.Count < 1 then exit;

  Result := Tasks.MoveGroupLeft(FindGroupRealIndex(Index), ShowArchived, FilterBox.Text, FShowTime);
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
    Changed := True;
  end;
end;

procedure TformNotetask.MoveTabRight(Index: integer);
var
  Result: integer;
  RowMem: integer = -1;
begin
  if (Index = 0) and (Tasks.GroupNames[0] = string.Empty) then exit;
  if TabsGroup.Tabs.Count < 1 then exit;

  Result := Tasks.MoveGroupRight(FindGroupRealIndex(Index), ShowArchived, FilterBox.Text, FShowTime);
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
    Changed := True;
  end;
end;

procedure TformNotetask.ChangeGroup(Index: integer);
var
  force: boolean;
begin
  if (Index < 0) or (index > TabsGroup.Tabs.Count - 1) then exit;
  force := TabsGroup.TabIndex = Index;
  TabsGroup.TabIndex := Index;
  if (force) then TabsGroupChange(TabsGroup);
end;

procedure TformNotetask.PrinterPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
var
  task: TTask;
  ACanvas: TCanvas;
begin
  if not Tasks.HasTask(aRow) then exit;

  ACanvas := TGridPrinter(Sender).Canvas;
  task := Tasks.GetTask(aRow);

  // Default text color
  ACanvas.Font.Color := TDarkUtils.ThemeColor(clBlack, clWhite);
  ACanvas.Font.Style := [];

  // Color and style
  if (ShowColumnDate) and (not task.Done) and (task.Date > 0) and (task.Date < Now) then
    ACanvas.Font.Color := clRed
  else if (not task.Done) and task.Archive then
    ACanvas.Font.Color := clMaroon;

  if task.Star then
    ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];

  if (aCol = COL_TASK) and task.Archive then
    ACanvas.Font.Style := ACanvas.Font.Style + [fsStrikeOut];

  if (aCol = COL_NOTE) and task.NoteItalic then
    ACanvas.Font.Style := ACanvas.Font.Style + [fsItalic];

  if (aCol = COL_DATE) and (task.Date > Now) then
    ACanvas.Font.Color := TDarkUtils.ThemeColor(clPlanned_Light, clPlanned_Dark);

  // Text styles
  with ACanvas.TextStyle do
  begin
    SingleLine := not FWordWrap;
    WordBreak := FWordWrap;
    RightToLeft := FBiDiRightToLeft;
  end;
end;

procedure TformNotetask.PrinterBeforePrintCell(Sender: TObject; AGrid: TCustomGrid; ACanvas: TCanvas; ACol, ARow: integer; ARect: TRect);
var
  task: TTask;
  BitTags: TBitmap;
  mRoundCorners, mTagBorderWidth: integer;
begin
  if (Assigned(Tasks)) and (Tasks.HasTask(ARow)) then
  begin
    if (aCol = COL_TASK) then
    begin
      Task := Tasks.GetTask(ARow);
      if Task.Tags.Count > 0 then
      begin
        mRoundCorners := TagEdit.RoundCorners;
        mTagBorderWidth := TagEdit.TagBorderWidth;
        TagEdit.RoundCorners := TGridPrinter(Sender).ScaleY(TagEdit.RoundCorners);
        TagEdit.TagBorderWidth := TGridPrinter(Sender).ScaleY(TagEdit.TagBorderWidth);
        BitTags := TagEdit.GetTagsBitmap(Task.Tags, Round(TGridPrinter(Sender).ScaleY(Max(ACanvas.Font.Size, 10))),
          Min(ARect.Width, TGridPrinter(Sender).ScaleY(500)), ARect.Height, 2, TagsDimnessPrint);
        try
          BitTags.TransparentColor := TDarkUtils.ThemeColor(clWhite, clBlack);
          BitTags.Transparent := True;
          if BitTags.Width < aRect.Width - 50 then
          begin
            if Grid.BiDiMode = bdLeftToRight then
              ACanvas.Draw(aRect.Right - BitTags.Width - 5, aRect.Top + TagEdit.TagBorderWidth, BitTags)
            else
              ACanvas.Draw(aRect.Left + 5, aRect.Top + TagEdit.TagBorderWidth, BitTags);
          end;
        finally
          TagEdit.RoundCorners := mRoundCorners;
          TagEdit.TagBorderWidth := mTagBorderWidth;
          BitTags.Free;
        end;
      end;
    end;
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
  LineHeight := Canvas.TextHeight('Wg');
  if LineHeight <= 0 then Exit(0);
  {$IFDEF UNIX}
  FirstVisibleLine := MemoNote.VertScrollBar.Position div LineHeight;
  {$ELSE}
  FirstVisibleLine := MemoNote.VertScrollBar.Position;
  {$ENDIF}
  Result := FirstVisibleLine + (MemoNote.ClientHeight - MemoNote.ClientHeight mod LineHeight) div LineHeight;

  if Result < 0 then Result := 0;
  if Result >= MemoNote.Lines.Count then Result := MemoNote.Lines.Count - 1;
end;

function TformNotetask.GetLineAtPos(Y: integer): integer;
var
  LineHeight: integer;
  FirstVisibleLine: integer;
  {$IFDEF UNIX}
  PixelOffset: integer;
  {$ENDIF}
begin
  LineHeight := Canvas.TextHeight('Wg');
  if LineHeight <= 0 then Exit(0);
  {$IFDEF UNIX}
  FirstVisibleLine := MemoNote.VertScrollBar.Position div LineHeight;
  PixelOffset := MemoNote.VertScrollBar.Position mod LineHeight;
  Result := FirstVisibleLine + (Y + PixelOffset) div LineHeight;
  {$ELSE}
  FirstVisibleLine := MemoNote.VertScrollBar.Position;
  Result := FirstVisibleLine + Y div LineHeight;
  {$ENDIF}

  if (Y <= 0) then Result := -1
  else
  if Result < 0 then Result := 0
  else
  if Result >= MemoNote.Lines.Count then Result := MemoNote.Lines.Count;
end;

procedure TformNotetask.EditCell(aCol: integer = -1; aRow: integer = -1);
var
  Value: string;
begin
  if (aCol >= 0) then
    Grid.Col := aCol
  else
    aCol := Grid.Col;
  if (aRow >= 0) then
    Grid.Row := aRow
  else
    aRow := Grid.Row;
  FIsEditing := True;
  Grid.EditorMode := True; //Set editing mode

  if (Assigned(PanelMemo)) and (PanelMemo.Visible) then
  begin
    EditControlSetBounds(PanelMemo, Grid.Col, Grid.Row);
    Value := Tasks.GetTaskValue(aCol, aRow);
    if (aCol <> COL_AMOUNT) or (Value <> '0') then
      Memo.Text := Value;
    Memo.SelStart := 0;
    Memo.SelLength := Length(Memo.Text);
    Memo.SetFocus;
    FMemoStartEdit := True;
  end;
  if (Assigned(DatePicker)) and (DatePicker.Visible) then
  begin
    EditControlSetBounds(DatePicker, Grid.Col, Grid.Row, 2, -2, -2, 0);
  end;
end;

procedure TformNotetask.EditComplete(aEnter: boolean = False; aEscape: boolean = False);
begin
  if IsEditing then
  begin
    if (Grid.Col = COL_DATE) and (Assigned(DatePicker)) then
    begin
      if (aEnter) then
      begin
        if Grid.Cells[Grid.Col, Grid.Row] = string.empty then
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

    if (Grid.Col in [COL_TASK, COL_NOTE, COL_AMOUNT]) and (Assigned(Memo)) then
    begin
      // Pressing the Escape key cancels editing
      if (aEscape) then
        Memo.Text := FMemoOldText
      else
      if Grid.Col in [COL_TASK, COL_NOTE] then
        SetFilter;
    end;

    Grid.EditorMode := False;
    FIsEditing := False;
    ChangeLastText;
    ResetRowHeight;
    if Visible and Grid.Visible and Grid.CanFocus then
      Grid.SetFocus;
  end;
end;

procedure TformNotetask.PasteWithLineEnding(AMemo: TMemo);
var
  s: string;
begin
  {$IFDEF UNIX}
  MemoNote.Tag := MemoNote.VertScrollBar.Position;
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
  if (MemoNote.Tag > 0) then
    MemoNoteSetScrollPosition(MemoNote.Tag);
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
  MemoNote.Tag := MemoNote.VertScrollBar.Position;
  {$ENDIF}
  MemoNote.CaretPos := Point(0, LineIndex);
  MemoNote.SelLength := Length(unicodestring(MemoNote.Lines[LineIndex]));

  if (not Move) then
  begin
    FNoteSelStart := MemoNote.SelStart;
    FNoteSelLength := MemoNote.SelLength;
  end;

  if (Move) then
  begin
    newStart := MemoNote.SelStart;
    newLength := MemoNote.SelLength;

    if (newStart > FNoteSelStart) then
    begin
      MemoNote.SelStart := FNoteSelStart;
      MemoNote.SelLength := newStart + newLength - FNoteSelStart;
    end
    else
      MemoNote.SelLength := FNoteSelStart + FNoteSelLength - newStart;
  end;

  {$IFDEF UNIX}
  if (MemoNote.Tag > 0) then
    MemoNoteSetScrollPosition(MemoNote.Tag);
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
  if (Grid.Col <> COL_AMOUNT) then
    Memo.SelText := UTF8Key
  else
    Memo.SelText := TMathParser.CleanNumericExpression(UTF8Key);
end;

procedure TformNotetask.GridUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
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
  FMemoOldText := Grid.Cells[Grid.Col, Grid.Row];

  // If amount column selected then clean when edit
  if (FMemoNeedSelectAll) and (Grid.Col in [COL_TASK, COL_NOTE, COL_AMOUNT]) then
  begin
    Memo.SelStart := 0;
    Memo.SelLength := Length(Memo.Text);
  end;
  FMemoNeedSelectAll := True;

  if (FKeyPressed <> string.Empty) and (FKeyPressed <> #13) then
  begin
    if (Grid.Col = COL_AMOUNT) then
      Memo.SelText := TMathParser.CleanNumericExpression(FKeyPressed)
    else
      Memo.SelText := FKeyPressed;
    FKeyPressed := string.Empty;
  end;

  if (Grid.IsCellSelected[Grid.Col, Grid.Row]) and ((Grid.Selection.Height > 0) or (Grid.Selection.Width > 0)) then
  begin
    Memo.Color := clHighlight;
    Memo.Font.Color := clWhite;
  end
  else
  begin
    Memo.Color := TDarkUtils.ThemeColor(clRowFocused_Light, clRowFocused_Dark);
  end;
end;

procedure TformNotetask.MemoExit(Sender: TObject);
begin
  EditComplete;
end;

procedure TformNotetask.MemoChange(Sender: TObject);
begin
  Grid.Cells[Grid.Col, Grid.Row] := TMemo(Sender).Text;
  Tasks.SetTask(Grid, Memo, Grid.Row, FMemoStartEdit and FBackup, FShowTime); // Backup only on begin edit
  FMemoStartEdit := False;
  Changed := True;
  CalcRowHeight(True, Grid.Row);
  EditControlSetBounds(PanelMemo, Grid.Col, Grid.Row);
  if (Grid.Col = COL_NOTE) then
    SetNote;
  if (Grid.Col = COL_AMOUNT) then
    SetInfo;
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

procedure TformNotetask.MemoKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  nextCol: integer;
begin
  // Test for letter, number, space or back key for backup
  if (Shift * [ssCtrl, ssAlt] = []) and ((not THotKeyData.Create(Key).IsSystemKey) or (Key = VK_SPACE) or (Key = VK_BACK)) then
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
    EditComplete(True);

    with Grid do
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
        nextCol := COL_TASK;
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
  FDatePickerOldDate := Tasks.GetTask(Grid.Row).Date;
  FDatePickerDateSet := False;
  if (FBackup) then Tasks.CreateBackup;
  if (DatePicker.DateTime = 0) then DatePicker.DateTime := Now;
  if (Grid.IsCellSelected[Grid.Col, Grid.Row]) and ((Grid.Selection.Height > 0) or (Grid.Selection.Width > 0)) then
  begin
    DatePicker.Color := clHighlight;
    DatePicker.Font.Color := TDarkUtils.ThemeColor(clWhite, clBlack);
  end
  else
  begin
    DatePicker.Color := TDarkUtils.ThemeColor(clRowFocused_Light, clRowFocused_Dark);
    DatePicker.Font.Color := TDarkUtils.ThemeColor(clBlack, clWhite);
  end;
end;

procedure TformNotetask.DatePickerChange(Sender: TObject);
begin
  FDatePickerDateSet := True;
  Grid.Cells[Grid.Col, Grid.Row] := DateTimeToString(TDateTimePicker(Sender).DateTime, FShowTime);
  Tasks.SetTask(Grid, Memo, Grid.Row, False, FShowTime);
  Changed := True;
  EditControlSetBounds(DatePicker, Grid.Col, Grid.Row, 2, -2, -2, 0);
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
    EditComplete(FDatePickerDateSet, not FDatePickerDateSet);

    with Grid do
    begin
      if Row < RowCount - 1 then
      begin
        Row := Row + 1;
        nextCol := COL_TASK;
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
  Indent: integer = 0;
begin
  if Assigned(Sender) then
  begin
    if (Sender is TPanel) and (aCol = COL_TASK) and (aRow > 0) and (aRow < Grid.RowCount) then
      Indent := Tasks.GetTask(aRow).FIndentLevel * Canvas.TextWidth(' ') * 2;

    Rect := Grid.CellRect(aCol, aRow);
    Sender.SetBounds(Rect.Left + OffsetLeft + Indent, Max(Rect.Top, Grid.RowHeights[0]) + OffsetTop,
      Rect.Right - Rect.Left + OffsetRight - Indent,
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
    Tasks.ClearTasksInRect(Grid, Grid.Selection);
    if (Assigned(Memo)) then
    begin
      Memo.OnChange := nil;
      Memo.Clear;
      Memo.OnChange := @MemoChange;
    end;
    Changed := True;
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
    Original := Grid.Selection;
    Grid.Selection := TGridRect.Create(0, Grid.Selection.Top, Grid.Columns.Count, Grid.Selection.Bottom);
    Tasks.CopyToClipboard(Grid, FShowNote, @Value);
    Back := Grid.Selection;
    if (SortOrder = soAscending) then
    begin
      Grid.Row := Grid.Selection.Bottom;
      Grid.Selection := TGridRect.Create(0, Grid.Selection.Bottom, Grid.Columns.Count, Grid.Selection.Bottom);
    end
    else
    begin
      Grid.Row := Grid.Selection.Top;
      Grid.Selection := TGridRect.Create(0, Grid.Selection.Top, Grid.Columns.Count, Grid.Selection.Top);
    end;
    Tasks.PasteFromClipboard(Grid, SortOrder, False, @Value);
    if (SortOrder = soAscending) then
      Sel := TGridRect.Create(Original.Left, Back.Bottom + 1, Original.Right, Back.Bottom + Back.Height + 1)
    else
      Sel := TGridRect.Create(Original.Left, Back.Top, Original.Right, Back.Bottom);
  finally
    EnableGridEvents;
  end;
  FillGrid;
  if (SortColumn = COL_NUM) then
  begin
    if (SortOrder = soAscending) then
      Grid.Row := Sel.Top
    else
      Grid.Row := Sel.Bottom;
    Grid.Selection := Sel;
    FLastSelectionHeight := Sel.Height;
  end;
  CalcRowHeight(True);
  AdjustMultiButton;
  SetInfo;
  SetNote;
  SetTags;
  Changed := True;
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
  if (Grid.Selection.Height > 0) then
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
        Task := Tasks.GetTask(Grid.Selection.Top);
        MaxDate := Task.Date;
        for i := Grid.Selection.Top + 1 to Grid.Selection.Bottom do
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
        for i := Grid.Selection.Bottom downto Grid.Selection.Top + 1 do
          Tasks.DeleteTask(i);

        // Mem selection
        Sel := Grid.Selection;
      finally
        EnableGridEvents;
      end;

      FillGrid;
      SetNote;
      SetTags;
      CalcRowHeight(True);
      Changed := True;

      // Restore selection
      Grid.Row := Sel.Top;
      Grid.Selection := TGridRect.Create(Sel.Left, Sel.Top, Sel.Right, Sel.Top);
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
  for i := Grid.Selection.Top to Grid.Selection.Bottom do
  begin
    if (Grid.Col = COL_TASK) and (Pos(FLineEnding.Value, Tasks.GetTask(i).Text) > 0) then
      colToSplit := COL_TASK
    else if (Grid.Col = COL_NOTE) and (Pos(FLineEnding.Value, Tasks.GetTask(i).Note) > 0) then
      colToSplit := COL_NOTE;
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
    SetLength(TasksToSplit, Grid.Selection.Bottom - Grid.Selection.Top + 1);
    for i := 0 to High(TasksToSplit) do
      TasksToSplit[i] := Tasks.GetTask(Grid.Selection.Top + i);

    // Process tasks in order
    index := Grid.Selection.Top;
    for i := 0 to High(TasksToSplit) do
    begin
      Task := TasksToSplit[i];

      // Get fields for splitting
      if (colToSplit = COL_TASK) then
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
      if (colToSplit = COL_TASK) then
        Task.Text := Trim(Lines[0])
      else
        Task.Note := Trim(Lines[0]);

      if Lines.Count = Lines2.Count then
      begin
        if (colToSplit = COL_TASK) then
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

          if (colToSplit = COL_TASK) then
            NewTask.Text := Trim(Lines[j])
          else
            NewTask.Note := Trim(Lines[j]);

          if Lines.Count = Lines2.Count then
          begin
            if (colToSplit = COL_TASK) then
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
  Sel := Grid.Selection;
  FillGrid;
  CalcRowHeight(True);
  SetInfo;
  SetNote;
  SetTags;
  Changed := True;

  // Restore selection
  if (SortColumn = COL_NUM) then
    Grid.Selection := TGridRect.Create(sel.Left, sel.Top, Sel.Right, index + (Sel.Bottom - Sel.Top))
  else
    Grid.Selection := Sel;
end;

procedure TformNotetask.DeleteTask(aRow: integer = 0; ShowConfirm: boolean = True);
var
  RowIndex: integer;
  Confirm: integer;
begin
  if (ReadOnly) then exit;

  // Get current RowIndex selected
  if (aRow = 0) then
    RowIndex := Grid.Row
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
      Grid.DeleteRow(RowIndex);
      FillGrid;
      ResetRowHeight;
      SetTabs;
      SetInfo;
      SetNote;
      SetTags;
      Changed := True;
      FLastText := Grid.Cells[Grid.Col, Grid.Row];
    end;
  end;
end;

procedure TformNotetask.DeleteTasks(ShowConfirm: boolean = True);
var
  i, RowIndex, Confirm: integer;
begin
  if (ReadOnly) then exit;

  // If multiple rows are selected
  if (Grid.Selection.Height > 0) then
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
        for i := Grid.Selection.Bottom downto Grid.Selection.Top do
        begin
          RowIndex := i;
          if (RowIndex > 0) and (RowIndex <= Tasks.Count) and (Grid.RowCount > RowIndex) then
          begin
            // Remove the task from the collection
            Grid.DeleteRow(RowIndex);
          end;
        end;
      finally
        EnableGridEvents;
      end;

      Grid.ClearSelections;
      FillGrid;
      ResetRowHeight;
      SetTabs;
      SetInfo;
      SetNote;
      SetTags;
      Changed := True;
      FLastText := Grid.Cells[Grid.Col, Grid.Row];
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
    RowIndex := Grid.Row
  else
    RowIndex := aRow;
  if (RowIndex > 0) and (RowIndex <= Tasks.Count) then
  begin
    // Show confirm delete dialog
    Confirm := MessageDlg(rarchiveconfirm, mtConfirmation, [mbYes, mbNo], 0);

    if Confirm = mrYes then
    begin
      EditComplete;
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
      Changed := True;
    end;
  end;
end;

procedure TformNotetask.ArchiveTasks;
var
  i, RowIndex, Confirm: integer;
begin
  if (ReadOnly) then exit;

  // If multiple rows are selected
  if (Grid.Selection.Width > 0) or (Grid.Selection.Height > 0) then
  begin
    // Request confirmation for archiving
    Confirm := MessageDlg(rarchivesconfirm, mtConfirmation, [mbYes, mbNo], 0);

    if Confirm = mrYes then
    begin
      EditComplete;
      if (FBackup) then
      begin
        GridBackupSelection;
        Tasks.CreateBackup;
      end;

      DisableGridEvents;
      try
        // Archive tasks from the end to avoid index shifting
        for i := Grid.Selection.Bottom downto Grid.Selection.Top do
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
      Changed := True;
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
  if (Grid.Selection.Width > 0) or (Grid.Selection.Height > 0) then
  begin
    if FBackup then
    begin
      GridBackupSelection;
      Tasks.CreateBackup;
    end;

    DisableGridEvents;
    try
      // Mark tasks as completed from the end to avoid index shifting
      for i := Grid.Selection.Bottom downto Grid.Selection.Top do
      begin
        RowIndex := i;
        if (RowIndex > 0) and (RowIndex <= Tasks.Count) then
        begin
          // Mark the task as completed in the collection
          Tasks.CompleteTask(RowIndex, False);

          if Tasks.GetTask(RowIndex).Done then
          begin
            Grid.Cells[COL_DONE, RowIndex] := '1';
            if (Grid.Columns.Items[COL_DATE - 1].Visible) and (Grid.Cells[COL_DATE, RowIndex] = string.Empty) then
              Grid.Cells[COL_DATE, RowIndex] := DateTimeToString(Now, FShowTime);
          end
          else
            Grid.Cells[COL_DONE, RowIndex] := '0';

          Tasks.SetTask(Grid, Memo, RowIndex, False, FShowTime); // Backup created on start
        end;
      end;
    finally
      EnableGridEvents;
    end;
    if ShowDuration then FillGrid;
    Changed := True;
    SetInfo;
  end
  else
  begin
    // Get current RowIndex selected if no multiple selection
    if (aRow = 0) then
      RowIndex := Grid.Row
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
        Grid.Cells[COL_DONE, RowIndex] := '1';
        if (Grid.Columns.Items[COL_DATE - 1].Visible) and (Grid.Cells[COL_DATE, RowIndex] = string.Empty) then
          Grid.Cells[COL_DATE, RowIndex] := DateTimeToString(Now, FShowTime);
      end
      else
        Grid.Cells[COL_DONE, RowIndex] := '0';

      Tasks.SetTask(Grid, Memo, RowIndex, False, FShowTime);
      if (ShowDuration) and (Check) then FillGrid;
      Changed := True;
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
  if (Grid.Selection.Width > 0) or (Grid.Selection.Height > 0) then
  begin
    SetLength(Rows, Grid.Selection.Bottom - Grid.Selection.Top + 1);
    for i := 0 to High(Rows) do
      Rows[i] := Grid.Selection.Top + i;
  end
  else
  begin
    if aRow = 0 then
      RowIndex := Grid.Row
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
        Grid.Cells[COL_STAR, RowIndex] := '1'
      else
        Grid.Cells[COL_STAR, RowIndex] := '0';

      Tasks.SetTask(Grid, Memo, RowIndex, False, FShowTime);
    end;
  end;

  Changed := True;
  CalcRowHeight(True);
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
  for i := Grid.Selection.Bottom downto Grid.Selection.Top do
  begin
    RowIndex := i;
    if (RowIndex > 0) and (RowIndex <= Tasks.Count) then
    begin
      with Tasks.GetTask(RowIndex) do
        if not Outdent then
          Inc(FIndentLevel)
        else if FIndentLevel > 0 then
          Dec(FIndentLevel);

      CalcRowHeight;
    end;
  end;
  Changed := True;
end;

procedure TformNotetask.GridBackupSelection;
begin
  FLastGridSelection := Grid.Selection;
  FLastGridRow := Grid.Row;
  FLastGridCol := Grid.Col;
end;

procedure TformNotetask.GridClearSelection;
begin
  if (ReadOnly) then exit;

  FLastGridSelection := TRect.Empty;
  FLastGridRow := 1;
  FLastGridCol := COL_TASK;
  Grid.ClearSelections;
  Grid.Row := 1;
  Grid.Col := COL_TASK;
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
    if S.IsUTF8Char(Pos, ' ') then
    begin
      // If space, extend deletion
      Inc(DeleteCount);
      Inc(Pos);
    end
    else if (S.IsUTF8Char(Pos, #13)) or (S.IsUTF8Char(Pos, #10)) then
    begin
      // If CR, delete it and check for following LF
      Inc(DeleteCount);
      Inc(Pos);
      if (Pos <= Len) and (S.IsUTF8Char(Pos, #10)) then
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
    TargetMemo := MemoNote
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

  for i := 1 to Grid.TopRow do
  begin
    Result += Grid.RowHeights[i] + Grid.GridLineWidth;
  end;
end;

procedure TformNotetask.SwapRowHeights(RowIndex1, RowIndex2: integer);
var
  TempHeight: integer;
begin
  // Check if the row indices are valid
  if (RowIndex1 < 0) or (RowIndex1 >= Grid.RowCount) or (RowIndex2 < 0) or (RowIndex2 >= Grid.RowCount) then
    Exit; // Exit if the indices are invalid

  // Store the height of the first row
  TempHeight := Grid.RowHeights[RowIndex1];

  // Swap the heights of the two rows
  Grid.RowHeights[RowIndex1] := Grid.RowHeights[RowIndex2];
  Grid.RowHeights[RowIndex2] := TempHeight;
end;

procedure TformNotetask.BackupSelectedState(aRowMem: boolean = False);
begin
  if (aRowMem) then
    FLoadedRowMem := FLastRowMem.CloneArray;
  FLoadedSelectedTab := TabsGroup.TabIndex;
  FLoadedSelectedRow := Grid.Row;
  FLoadedSelection := Grid.Selection;
  FLoadedMemoNoteSelStart := MemoNote.SelStart;
  FLoadedMemoNoteSelLength := MemoNote.SelLength;
  FLoadedMemoNoteScroll := MemoNote.VertScrollBar.Position;
end;

procedure TformNotetask.RestoreSelectedState(aRowMem: boolean = True; aRowMemPriority: boolean = True; aFocusMemo: boolean = False);
var
  FirstTabRow, Index: integer;
begin
  // Restore rows memory
  if (aRowMem) and (Length(FLoadedRowMem) > 0) then
    FLoadedRowMem.CopyToArray(FLastRowMem);

  if (TabsGroup.Tabs.Count > 0) and ((FLoadedSelectedTab < 0) or (FLoadedSelectedTab >= TabsGroup.Tabs.Count)) then
    FLoadedSelectedTab := 0;

  // Restore last open tab and rows
  if (FLoadedSelectedTab >= 0) then
  begin
    FirstTabRow := -1;
    if (Length(FLastRowMem) > FindGroupRealIndex(0)) then
      FirstTabRow := FLastRowMem[FindGroupRealIndex(0)];
    if (FLoadedSelectedTab > 0) then
      TabsGroup.TabIndex := FLoadedSelectedTab
    else
    if (FLoadedSelectedTab = 0) and (FindGroupRealIndex(0) > 0) then
      TabsGroupChange(TabsGroup);

    if (aRowMem) and (aRowMemPriority) and (Length(FLastRowMem) > FindGroupRealIndex(TabsGroup.TabIndex)) then
      Grid.Row := FLastRowMem[FindGroupRealIndex(TabsGroup.TabIndex)]
    else
    if (FLoadedSelectedRow > 0) then
      Grid.Row := FLoadedSelectedRow;

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
    Grid.Col := FLoadedSelection.Left;
    Grid.Selection := TGridRect.Create(FLoadedSelection);
    FLoadedSelection := Rect(0, 0, 0, 0);
    SetNote;
    SetTags;
  end;

  if (MemoNote.Visible) and (Showing) then
  begin
    // Restore memo note SelStart
    if (FLoadedMemoNoteSelStart > 0) then
    begin
      MemoNote.SelStart := FLoadedMemoNoteSelStart;
      FLoadedMemoNoteSelStart := 0;
    end;

    // Restore memo note SelLength
    if (FLoadedMemoNoteSelLength > 0) then
    begin
      if MemoNote.CanFocus then MemoNote.SetFocus;
      MemoNote.SelLength := FLoadedMemoNoteSelLength;
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
    totalWidth := Grid.GridWidth;
    visibleWidth := Grid.ClientWidth;

    if totalWidth > visibleWidth then
      newStyle := ssAutoBoth
    else
      newStyle := ssAutoVertical;

    // Only change when necessary (prevents extra events)
    if Grid.ScrollBars <> newStyle then
    begin
      Grid.ScrollBars := newStyle;

      // If we disabled horizontal scrollbar, try to hide native scrollbar
      if (newStyle = ssAutoVertical) and (Grid.HandleAllocated) then
      begin
        // Try to hide native horizontal scrollbar (widgetset dependent)
        ShowScrollBar(Grid.Handle, SB_HORZ, False);
      end;
    end;
  finally
    FAdjustingScrollBars := False;
  end;
end;

procedure TformNotetask.GridInvalidate;
begin
  Application.ProcessMessages;
  Grid.Invalidate;
  Application.ProcessMessages;
end;

procedure TformNotetask.AdjustMultiButton;
begin
  if (Grid.Selection.Height > 0) or (FLastSelectionHeight > 0) or (Grid.Selection.Width > 0) then
  begin
    btnMulti.Hint := aDuplicateTasks.Caption + ' (Ctrl+D)';
    btnMulti.ImageIndex := TDarkUtils.ThemeValue(2, 3);
    btnMulti.HotImageIndex := TDarkUtils.ThemeValue(3, 2);
  end
  else
  begin
    btnMulti.Hint := aInsertTask.Caption + ' (Ins)';
    btnMulti.ImageIndex := TDarkUtils.ThemeValue(0, 1);
    btnMulti.HotImageIndex := TDarkUtils.ThemeValue(1, 0);
  end;
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
  Grid.Columns.Items[COL_DONE - 1].Visible := FShowColumnDone;
  Grid.Columns.Items[COL_TASK - 1].Visible := FShowColumnTask;
  Grid.Columns.Items[COL_NOTE - 1].Visible := FShowColumnNote;
  Grid.Columns.Items[COL_AMOUNT - 1].Visible := FShowColumnAmount;
  Grid.Columns.Items[COL_DATE - 1].Visible := FShowColumnDate;
  Grid.Columns.Items[COL_STAR - 1].Visible := FShowColumnFavorite;

  ApplySortArrow;
end;

procedure TformNotetask.ApplySortArrow;
var
  i: integer;
begin
  for i := 0 to Grid.Columns.Count - 1 do
    Grid.Columns[i].Title.ImageIndex := -1;
  if (SortColumn > COL_NUM) then
  begin
    if SortOrder = soAscending then
      Grid.Columns[SortColumn - 1].Title.ImageIndex := 0
    else
      Grid.Columns[SortColumn - 1].Title.ImageIndex := 1;
  end;
end;

procedure TformNotetask.ApplySorting;
var
  i: integer;
begin
  FillGrid;
  ResetRowHeight;

  for i := 0 to Grid.Columns.Count - 1 do
    Grid.Columns[i].Title.ImageIndex := -1;

  ApplySortingActions;

  FLastRow := Grid.Row;
  SetNote;
  SetTags;
end;

procedure TformNotetask.ApplySortingActions;
begin
  aMoveTaskTop.Enabled := (not ReadOnly) and (SortColumn = COL_NUM);
  aMoveTaskBottom.Enabled := (not ReadOnly) and (SortColumn = COL_NUM);
  aMoveTaskUp.Enabled := (not ReadOnly) and (SortColumn = COL_NUM);
  aMoveTaskDown.Enabled := (not ReadOnly) and (SortColumn = COL_NUM);
  aMoveTaskLeft.Enabled := (not ReadOnly) and (SortColumn = COL_NUM);
  aMoveTaskRight.Enabled := (not ReadOnly) and (SortColumn = COL_NUM);

  if (SortColumn = COL_NUM) then
    Grid.Options := Grid.Options + [goRowMoving]
  else
    Grid.Options := Grid.Options - [goRowMoving];
end;

procedure TformNotetask.FillGrid;
begin
  DisableGridEvents;
  Tasks.FillGrid(Grid, FShowArchived, FShowDuration, FShowTime, SortOrder, SortColumn, FilterBox.Text);
  CalcRowHeight;
  EnableGridEvents;
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
    statusBar.Panels[1].Text := UpperCase(TFileManager.GetEncodingName(FEncoding));

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
  if (Grid.Selection.Height = 0) then
  begin
    CurAll := Tasks.CalcCount(ShowArchived, False, FilterBox.Text, 0, 0, FShowTime);
    CurDone := Tasks.CalcCount(ShowArchived, True, FilterBox.Text, 0, 0, FShowTime);
  end
  else
  begin
    CurAll := Tasks.CalcCount(ShowArchived, False, FilterBox.Text, Grid.Selection.Top, Grid.Selection.Bottom, FShowTime);
    CurDone := Tasks.CalcCount(ShowArchived, True, FilterBox.Text, Grid.Selection.Top, Grid.Selection.Bottom, FShowTime);
  end;
  if (CurAll = CurDone) or (CurDone = 0) then
    statusBar.Panels[3].Text := CurAll.ToString + rrows
  else
    statusBar.Panels[3].Text := CurDone.ToString + ' / ' + CurAll.ToString + rrows;

  // Task amounts
  if (ShowColumnAmount) then
  begin
    if (Grid.Selection.Height = 0) then
    begin
      SumAll := Tasks.CalcSum(ShowArchived, False, FilterBox.Text, 0, 0, FShowTime);
      SumDone := Tasks.CalcSum(ShowArchived, True, FilterBox.Text, 0, 0, FShowTime);
    end
    else
    begin
      SumAll := Tasks.CalcSum(ShowArchived, False, FilterBox.Text, Grid.Selection.Top, Grid.Selection.Bottom, FShowTime);
      SumDone := Tasks.CalcSum(ShowArchived, True, FilterBox.Text, Grid.Selection.Top, Grid.Selection.Bottom, FShowTime);
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
    if (Grid.Selection.Height = 0) then
    begin
      DurationAll := Tasks.CalcDuration(ShowArchived, False, FilterBox.Text, 0, 0, FShowTime);
      DurationCurrent := Tasks.CalcDuration(ShowArchived, True, FilterBox.Text, 0, 0, FShowTime);
    end
    else
    begin
      DurationAll := Tasks.CalcDuration(ShowArchived, False, FilterBox.Text, Grid.Selection.Top, Grid.Selection.Bottom, FShowTime);
      DurationCurrent := Tasks.CalcDuration(ShowArchived, True, FilterBox.Text, Grid.Selection.Top,
        Grid.Selection.Bottom, FShowTime);
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
  tags, curtags, firstTags: TStringList;
  HasDiff: boolean = False;
begin
  if (not ShowTags) then exit;

  tags := TStringList.Create;
  tags.Sorted := True;
  tags.Duplicates := dupIgnore;
  curtags := TStringList.Create;
  curtags.Sorted := True;
  curtags.Duplicates := dupIgnore;
  firstTags := TStringList.Create;
  firstTags.Sorted := True;
  firstTags.Duplicates := dupIgnore;

  try
    if Assigned(Tasks) and (Grid.RowCount > 1) then
    begin
      if Grid.Selection.Height > 0 then
      begin
        // Multiple rows selected — concatenate tags and set read-only
        for i := Grid.Selection.Top to Grid.Selection.Bottom do
          if Tasks.Map(i) > -1 then
          begin
            curtags.Assign(Tasks.GetTask(i).Tags);
            tags.AddStrings(curtags);

            if i = Grid.Selection.Top then
              firstTags.Assign(curtags)
            else if not firstTags.Equal(curtags) then
              HasDiff := True;
          end;
        if (tags.Count > 0) then
        begin
          if (not HasDiff) and (Tasks.Map(Grid.Selection.Top) > -1) then
            TagEdit.Items.Assign(Tasks.GetTask(Grid.Selection.Top).Tags)
          else
            TagEdit.Items.Assign(tags);
        end
        else
          TagEdit.Items.Clear;
        TagEdit.ReadOnly := FReadOnly;
        TagEdit.AllowReorder := not HasDiff;
        TagEdit.Color := clDefault;
      end
      else if Tasks.Map(Grid.Row) > -1 then
      begin
        // Single row selected — set editable tag
        TagEdit.Items.Assign(Tasks.GetTask(Grid.Row).Tags);
        TagEdit.ReadOnly := FReadOnly;
        TagEdit.AllowReorder := True;
        TagEdit.Color := clDefault;
      end
      else
      begin
        TagEdit.Items.Clear;
        TagEdit.ReadOnly := True;
      end;
    end
    else
    begin
      TagEdit.Items.Clear;
      TagEdit.ReadOnly := True;
    end;
  finally
    TagEdit.ClearSelection;
    tags.Free;
    curtags.Free;
    firstTags.Free;
  end;
end;

procedure TformNotetask.SetNote;
var
  i: integer;
  notes: TStringList;
  note: string;
begin
  if (not ShowNote) then exit;

  MemoNote.OnChange := nil;
  notes := TStringList.Create;
  try
    if Assigned(Tasks) and (Grid.RowCount > 1) then
    begin
      if Grid.Selection.Height > 0 then
      begin
        // Multiple rows selected — concatenate notes and set read-only
        for i := Grid.Selection.Top to Grid.Selection.Bottom do
          if Tasks.Map(i) > -1 then
          begin
            note := Tasks.GetTask(i).Note;
            if note <> string.Empty then
              notes.Add(note);
          end;
        MemoNote.Lines.Text := notes.Text;
        MemoNote.ReadOnly := True;
        MemoNote.Color := TDarkUtils.ThemeColor(clReadOnly_Light, clReadOnly_Dark);
      end
      else if Tasks.Map(Grid.Row) > -1 then
      begin
        // Single row selected — set editable note
        MemoNote.Text := Tasks.GetTask(Grid.Row).Note;
        MemoNote.ReadOnly := FReadOnly;
        MemoNote.Color := clDefault;
      end
      else
      begin
        MemoNote.Text := string.Empty;
        MemoNote.ReadOnly := True;
      end;
    end
    else
    begin
      MemoNote.Text := string.Empty;
      MemoNote.ReadOnly := True;
    end;
  finally
    notes.Free;
    MemoNoteBackup;
    MemoNote.OnChange := @memoNoteChange;
  end;
end;

procedure TformNotetask.SetFilter(FillTags: boolean = True);
var
  i: integer;
  SortedState: boolean;
begin
  if FillTags and (Grid.Row > 0) then
    Tasks.FillTags;
  SortedState := FilterBox.Sorted;
  FilterBox.Sorted := False;
  FilterBox.Items.Assign(Tasks.Tags);
  TagEdit.SuggestedItemsSorted := False;
  TagEdit.SuggestedItems := Tasks.Tags;

  // Remove ` from each item directly
  for i := 0 to filterBox.Items.Count - 1 do
    filterBox.Items[i] := StringReplace(filterBox.Items[i], '`', '', [rfReplaceAll]);
  FilterBox.Sorted := SortedState;

  UpdateComboRegion(FilterBox);
end;

procedure TformNotetask.SetTabs(Change: boolean = True);
var
  Clean: TStringList;
  i: integer;
  LastIndex, LastRealIndex: integer;
  FoundTab: boolean;
begin
  LastRealIndex := FindGroupRealIndex(TabsGroup.TabIndex);
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
        if not Tasks.GetGroupFiltered(i, ShowArchived, FilterBox.Text, FShowTime) then
        begin
          Clean.Add(Tasks.GetGroupNameForTab(i));
          SetLength(FGroupIndexMap, Length(FGroupIndexMap) + 1);
          FGroupIndexMap[High(FGroupIndexMap)] := i;
        end;
      end;
    end;

    TabsGroup.Tabs := Clean;
    SetTabsVisible;

    if (LastRealIndex < 0) and (FLastTabFilter >= 0) then LastRealIndex := FLastTabFilter;

    if (Change) and (TabsGroup.Visible) and (LastRealIndex >= 0) then
    begin
      FoundTab := False;
      LastIndex := FindGroupTabIndex(LastRealIndex);
      if (LastIndex > 0) and (LastIndex < TabsGroup.Tabs.Count) then
        TabsGroup.TabIndex := LastIndex
      else
      if (LastIndex >= TabsGroup.Tabs.Count) then
        TabsGroup.TabIndex := TabsGroup.Tabs.Count - 1
      else
      if (LastIndex < 0) then
      begin
        i := LastRealIndex;
        while (i < Tasks.CountGroup) do
        begin
          Inc(i);
          LastIndex := FindGroupTabIndex(i);
          if (LastIndex >= 0) and (LastIndex < TabsGroup.Tabs.Count) then
          begin
            TabsGroup.TabIndex := LastIndex;
            FoundTab := True;
            break;
          end;
        end;
        if (not FoundTab) then
          TabsGroup.TabIndex := TabsGroup.Tabs.Count - 1;
      end;

      // Change group if tab was changed
      if (LastRealIndex <> FindGroupRealIndex(TabsGroup.TabIndex)) then
        TabsGroupChange(TabsGroup);
    end
    else
    if not TabsGroup.Visible then
    begin
      TabsGroup.TabIndex := 0;
      TabsGroupChange(TabsGroup);
    end
    else
    if LastRealIndex < 0 then
      TabsGroupChange(TabsGroup);

    // Set selected row memory for tabs
    SetLength(FLastRowMem, Tasks.CountGroup);
  finally
    Clean.Free;
  end;
end;

procedure TformNotetask.SetTabsVisible;
begin
  panelTabs.Visible := (FilterBox.Text <> string.Empty) or (FilterBox.Focused) or
    (not ((TabsGroup.Tabs.Count = 1) and (Tasks.GroupNames[0] = string.Empty)));
end;

procedure TformNotetask.SetLanguage(aLanguage: string = string.Empty);
begin
  aLangArabic.Checked := False;
  aLangBelarusian.Checked := False;
  aLangChinese.Checked := False;
  aLangCzech.Checked := False;
  aLangDanish.Checked := False;
  aLangDutch.Checked := False;
  aLangEnglish.Checked := False;
  aLangFinnish.Checked := False;
  aLangFrench.Checked := False;
  aLangGerman.Checked := False;
  aLangGreek.Checked := False;
  aLangHebrew.Checked := False;
  aLangHindi.Checked := False;
  aLangIndonesian.Checked := False;
  aLangItalian.Checked := False;
  aLangJapanese.Checked := False;
  aLangKorean.Checked := False;
  aLangPolish.Checked := False;
  aLangPortuguese.Checked := False;
  aLangRomanian.Checked := False;
  aLangRussian.Checked := False;
  aLangSpanish.Checked := False;
  aLangSwedish.Checked := False;
  aLangTurkish.Checked := False;
  aLangUkrainian.Checked := False;

  if (aLanguage <> string.Empty) then
  begin
    Language := aLanguage;
    if not TLocalize.ApplicationTranslate(APP_NAME, Language) then
      Language := 'en';

    TLocalize.UpdatePackageTranslations(APP_NAME, 'checkupdates', Language);
  end;

  openDialog.Filter := ropendialogfilter;
  saveDialog.Filter := rsavedialogfilter;
  if Assigned(TagEdit) then
  begin
    TagEdit.RemoveConfirmMessage := rremovetag;
    TagEdit.RemoveConfirmTitle := rremovetagtitle;
    TagEdit.TextHint := renternewtag;
    TagEdit.EditBox.Hint := renternewtaghint;
  end;

  if (Assigned(Tasks)) and (Tasks.GroupNames[0] = string.Empty) and (TabsGroup.Tabs.Count > 0) then
    TabsGroup.Tabs[0] := rgroupuntitled;

  case Language of
    'ar': aLangArabic.Checked := True;
    'be': aLangBelarusian.Checked := True;
    'zh': aLangChinese.Checked := True;
    'cs': aLangCzech.Checked := True;
    'da': aLangDanish.Checked := True;
    'nl': aLangDutch.Checked := True;
    'en': aLangEnglish.Checked := True;
    'fi': aLangFinnish.Checked := True;
    'fr': aLangFrench.Checked := True;
    'de': aLangGerman.Checked := True;
    'el': aLangGreek.Checked := True;
    'he': aLangHebrew.Checked := True;
    'hi': aLangHindi.Checked := True;
    'id': aLangIndonesian.Checked := True;
    'it': aLangItalian.Checked := True;
    'ja': aLangJapanese.Checked := True;
    'ko': aLangKorean.Checked := True;
    'pl': aLangPolish.Checked := True;
    'pt': aLangPortuguese.Checked := True;
    'ro': aLangRomanian.Checked := True;
    'ru': aLangRussian.Checked := True;
    'es': aLangSpanish.Checked := True;
    'sv': aLangSwedish.Checked := True;
    'tr': aLangTurkish.Checked := True;
    'uk': aLangUkrainian.Checked := True;
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
  Grid.OnSelectCell := nil;
  Grid.OnSelection := nil;
  Grid.OnSelectEditor := nil;
end;

procedure TformNotetask.EnableGridEvents;
begin
  Grid.OnSelectEditor := @GridSelectEditor;
  Grid.OnSelection := @GridSelection;
  Grid.OnSelectCell := @GridSelectCell;
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
        FLastRowMem := FLoadedRowMem.CloneArray;
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

procedure TformNotetask.CorrectGridSelection;
begin
  // Check and fix Row if it's out of bounds
  if Grid.Row >= Grid.RowCount then
    Grid.Row := Grid.RowCount - 1;

  // Only fix Selection.Bottom if it's out of bounds
  if (Grid.Selection.Bottom < 1) or (Grid.Selection.Bottom >= Grid.RowCount) then
    Grid.Selection := Rect(Grid.Selection.Left, Grid.Selection.Top, Grid.Selection.Right, Grid.RowCount - 1);

  // Only fix Selection.Top if it's out of bounds
  if (Grid.Selection.Top < 1) or (Grid.Selection.Top >= Grid.RowCount) then
    Grid.Selection := Rect(Grid.Selection.Left, Grid.RowCount - 1, Grid.Selection.Right, Grid.Selection.Bottom);
end;

function TformNotetask.GetSelectedTab: integer;
begin
  Result := TabsGroup.TabIndex;
end;

function TformNotetask.GetSelectedRow: integer;
begin
  Result := Grid.Row;
end;

function TformNotetask.GetSelectedRows: TIntegerArray;
begin
  try
    if (TabsGroup.Visible) and (TabsGroup.TabIndex >= 0) and (Length(FLastRowMem) > FindGroupRealIndex(TabsGroup.TabIndex)) then
      FLastRowMem[FindGroupRealIndex(TabsGroup.TabIndex)] := GetSelectedRow;
  except
    // just insure
  end;
  Result := FLastRowMem;
end;

function TformNotetask.GetSelection: TRect;
begin
  Result := Grid.Selection;
end;

function TformNotetask.GetMemoNoteScroll: integer;
begin
  Result := MemoNote.VertScrollBar.Position;
end;

function TformNotetask.GetMemoNoteSelStart: integer;
begin
  Result := MemoNote.SelStart;
end;

function TformNotetask.GetMemoNoteSelLength: integer;
begin
  Result := MemoNote.SelLength;
end;

function TformNotetask.GetIsEditing: boolean;
begin
  Result := (Grid.EditorMode) or (FIsEditing);
end;

{%EndRegion}

{%Region -fold Public Methods}

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
    Grid.Col := CurCol;
    Memo.SelStart := 0;
    Memo.SelLength := 0;
  end;

  procedure DecCurCol;
  begin
    Dec(CurCol);
    Grid.Col := CurCol;
    Memo.SelStart := Length(unicodestring(Memo.Text));
    Memo.SelLength := 0;
  end;

  function NotFound(messageOnly: boolean = False): boolean;
  begin
    if (not messageOnly) then
    begin
      if (aText = FFoundText) then
      begin
        Grid.Row := FLastFoundRow;
        Grid.Col := FLastFoundCol;
        Grid.EditorMode := True;
        FMemoOldText := Memo.Text;
        Memo.SelStart := FLastFoundSelStart;
        Memo.SelLength := FLastFoundSelLength;
        {$IFDEF UNIX}
          if Memo.Visible and Memo.CanSetFocus then
            Memo.SetFocus;
        {$ENDIF}
      end
      else
      begin
        FLastFoundRow := StartRow;
        FLastFoundCol := StartCol;
        Grid.Row := FLastFoundRow;
        Grid.Col := FLastFoundCol;
      end;
    end;
    if (not Silent) then
      ShowMessage(rcantfind + ' "' + string(aText) + '"');
    Result := False;
  end;

begin
  if (FFindActive) or (Grid.RowCount = 0) then exit;
  FFindActive := True;
  aRowsChanged := 0;
  FDuplicateHighlight := False;
  {$IFDEF WINDOWS}
  if not Silent then
    Enabled := False;
  {$ENDIF}
  try
    FindText := aText;
    MatchCase := aMatchCase;
    WrapAround := aWrapAround;

    // Search in Note if selected
    if self.ActiveControl = MemoNote then
    begin
      sValue := unicodestring(MemoNote.Text);
      sText := unicodestring(aText);
      if (Pos(UnicodeLowerCase(sText), UnicodeLowerCase(sValue)) > 0) then
      begin
        if (FindMemo(MemoNote)) then
        begin
          FFoundText := aText;
          Result := True;
        end
        else
        if (WrapAround) then
        begin
          if (aDirectionDown) then
          begin
            MemoNote.SelStart := 0;
            MemoNote.SelLength := 0;
          end
          else
          begin
            MemoNote.SelStart := Length(MemoNote.Text);
            MemoNote.SelLength := 0;
          end;
          FindMemo(MemoNote);
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

    StartRow := Grid.Row;
    StartCol := Grid.Col;
    LastDate := False;
    if Grid.Col = COL_DONE then Grid.Col := COL_TASK;
    FMemoNeedSelectAll := False;
    Grid.EditorMode := True;
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
    if (aDirectionDown) and (FLastFoundCol = COL_DATE) and (Grid.Col = COL_DATE) then
    begin
      if (Grid.Row < Grid.RowCount - 1) then
      begin
        Grid.Row := Grid.Row + 1;
        Inc(aRowsChanged);
        Grid.Col := COL_DONE;
      end
      else
        LastDate := True;
    end
    else
    if (not aDirectionDown) and (FLastFoundCol = COL_DATE) and (Grid.Col = COL_DATE) then
    begin
      if (Grid.Row > 1) then
      begin
        Grid.Row := Grid.Row - 1;
        Inc(aRowsChanged);
        Grid.Col := COL_DATE;
      end
      else
        LastDate := True;
    end;

    CurRow := Grid.Row;
    CurCol := Grid.Col;
    Counter := 0;

    repeat
      if (CurCol < COL_DATE) and (CurCol > COL_DONE) and (Assigned(Memo)) then
      begin
        sValue := unicodestring(Memo.Text);
        sText := unicodestring(aText);
        if (Pos(UnicodeLowerCase(sText), UnicodeLowerCase(sValue)) > 0) and (FindMemo(Memo)) then
        begin
          FMemoNeedSelectAll := False;
          Grid.EditorMode := True;
          FMemoOldText := Memo.Text;
          FLastFoundRow := Grid.Row;
          FLastFoundCol := Grid.Col;
          FLastFoundSelStart := Memo.SelStart;
          FLastFoundSelLength := Memo.SelLength;
          FFoundText := aText;
          {$IFDEF UNIX}
          if Memo.Visible and Memo.CanSetFocus then
            Memo.SetFocus;
          {$ENDIF}
          Counter := 0;
          Break;
        end;
      end
      else
      if (CurCol = COL_DATE) and (Assigned(DatePicker)) then
      begin
        sValue := unicodestring(DateTimeToString(DatePicker.DateTime));
        sText := unicodestring(aText);
        if (Pos(UnicodeLowerCase(sText), UnicodeLowercase(sValue)) > 0) and (Grid.Cells[Grid.Col, Grid.Row] <> string.Empty) and
          (not LastDate) then
        begin
          FMemoNeedSelectAll := False;
          Grid.EditorMode := True;
          FLastFoundRow := Grid.Row;
          FLastFoundCol := Grid.Col;
          FLastFoundSelStart := 0;
          FLastFoundSelLength := Length(sValue);
          FFoundText := aText;
          {$IFDEF UNIX}
          if Memo.Visible and Memo.CanSetFocus then
            Memo.SetFocus;
          {$ENDIF}
          Counter := 0;
          Break;
        end;
      end;

      // Move to col
      if ((aDirectionDown) and (CurCol < COL_DATE)) or ((not aDirectionDown) and (CurCol > COL_TASK)) then
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
        if ((aDirectionDown) and (CurRow < Grid.RowCount)) or ((not aDirectionDown) and (CurRow > 0)) then
        begin
          // Move to next row
          if (aDirectionDown) then
          begin
            Inc(CurRow);
            CurCol := COL_DONE;
            Grid.Row := Grid.Row + 1;
            Inc(aRowsChanged);
            Grid.Col := COL_TASK;
            Memo.SelStart := 0;
            Memo.SelLength := 0;
          end
          else
            // Move to prev row
          begin
            Dec(CurRow);
            CurCol := COL_DATE;
            Grid.Row := Grid.Row - 1;
            Inc(aRowsChanged);
            Grid.Col := COL_DATE;
            Memo.SelStart := Length(unicodestring(Memo.Text)) - 1;
            Memo.SelLength := 0;
          end;
          Inc(Counter);
        end;
      end;

      // Move to begin
      if ((aDirectionDown) and (CurRow >= Grid.RowCount)) or ((not aDirectionDown) and (CurRow = 0)) then
      begin
        if (WrapAround) then
        begin
          // Move to begin start
          if (aDirectionDown) then
          begin
            CurRow := 1;
            Grid.Row := 1;
            Inc(aRowsChanged);
            CurCol := COL_TASK;
            Grid.Col := COL_TASK;
            Memo.SelStart := 0;
          end
          else
            // Move to begin end
          begin
            CurRow := Grid.RowCount - 1;
            Grid.Row := Grid.RowCount - 1;
            Inc(aRowsChanged);
            CurCol := COL_DATE;
            Grid.Col := COL_DATE;
          end;
          Inc(Counter);
        end
        else
          exit(NotFound);
      end;

      // Skip hidden columns
      if (aDirectionDown) then
      begin
        if (CurCol = COL_TASK) and (not ShowColumnTask) then IncCurCol;
        if (CurCol = COL_NOTE) and (not ShowColumnNote) then IncCurCol;
        if (CurCol = COL_AMOUNT) and (not ShowColumnAmount) then IncCurCol;
        if (CurCol = COL_DATE) and (not ShowColumnDate) then IncCurCol;
      end
      else
      begin
        if (CurCol = COL_DATE) and (not ShowColumnDate) then DecCurCol;
        if (CurCol = COL_AMOUNT) and (not ShowColumnAmount) then DecCurCol;
        if (CurCol = COL_NOTE) and (not ShowColumnNote) then DecCurCol;
        if (CurCol = COL_TASK) and (not ShowColumnTask) then DecCurCol;
      end;

    until ((not WrapAround) and (((aDirectionDown) and (CurRow >= Grid.RowCount)) or ((not aDirectionDown) and (CurRow = 0)))) or
      (WrapAround and (Counter > Grid.RowCount)) or (not FFindF3 and not formFindText.Visible and not formReplaceText.Visible);

    if (WrapAround and (Counter > Grid.RowCount)) then
      exit(NotFound);

    Result := True;
  finally
    FDuplicateHighlight := True;
    FFindActive := False;
    FFindF3 := False;
    {$IFDEF WINDOWS}
    if not Silent then
      Enabled := True;
    {$ENDIF}
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
  if self.ActiveControl = MemoNote then
    Target := MemoNote
  else
    Target := Memo;

  sValue := unicodestring(Target.SelText);
  sText := unicodestring(aText);

  if (FFoundText = string.Empty) or ((aMatchCase) and (sValue <> sText)) or ((not aMatchCase) and
    (UnicodeLowerCase(sValue) <> UnicodeLowerCase(sText))) then
    FindNextExecute
  else
  begin
    if self.ActiveControl = MemoNote then
      Target := MemoNote
    else
    begin
      FMemoOldText := Memo.Text;
      Target := Memo;
    end;

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
  if self.ActiveControl <> MemoNote then
    FShowNote := False;
  GridBackupSelection;
  Tasks.CreateBackup; // FBackup = false here
  Enabled := False;
  try
    if self.ActiveControl = MemoNote then
    begin
      Target := MemoNote;
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

      if self.ActiveControl = MemoNote then
        Target := MemoNote
      else
      begin
        FMemoOldText := Memo.Text;
        Target := Memo;
      end;
      Target.SelText := aToText;
      FLastFoundSelLength := Length(unicodestring(aToText));
      Target.SelStart := Max(Target.SelStart - FLastFoundSelLength, 0);
      Target.SelLength := FLastFoundSelLength;

      // Safeguard to prevent infinite loop
      if aWrapAround then
      begin
        if self.ActiveControl = MemoNote then
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
          if (CounterRow > Grid.RowCount - 1) then break;
        end;
      end;
    end;

    Result := True;
  finally
    FBackup := True;
    FShowNote := sShowNote;
    FDuplicateHighlight := True;
    Enabled := True;
  end;
end;

{%EndRegion}

end.
