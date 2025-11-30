//-----------------------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the GNU General Public License, Version 3 (GPL-3.0)
//  You may obtain a copy of the License at https://www.gnu.org/licenses/gpl-3.0.html
//-----------------------------------------------------------------------------------

unit task;

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

uses
  Classes,
  SysUtils,
  Math,
  Graphics,
  Grids,
  Clipbrd,
  StrUtils,
  DateUtils,
  TagEdit,
  formattool;

type
  // Class representing a single task
  TTask = class
  public
    FDone: boolean; // Status of task completion
    FText: string; // Description of the task
    FNote: string; // Note for the task
    FAmount: double; // Amount for the task
    FDate: TDateTime; // Date of task completion
    FArchive: boolean; // Archive task
    FStar: boolean; // Starred
    FNoteItalic: boolean; // Note is italic
    FEmptyNote: boolean; // Has empty note symbols
    FSpaceBeforeNote: boolean; // Space before note in file
    FSpaceAfterNote: boolean; // Space after note in file
    FAmountOriginal: string; // Original sum line
    FDateOriginal: string; // Original date line
    FDateStart: TDateTime; // Calculated start time interval
    FDateEnd: TDateTime; // Calculated end time interval
    FTags: TStringList; // List of detected tags
    FTagsWidth: integer;
    constructor Create;
    constructor Create(const TaskString: string); // Constructor that takes a task string
    destructor Destroy; override; // Destructor
    function ToString(Col: integer = 0; AddEmptyCompletion: boolean = True): string; reintroduce;
    procedure Copy(Original: TTask);
    procedure FillTags;
    function MatchesFilter(const Filter: string; DisplayTime: boolean): boolean;
    function GetDate: string;
    function GetDateTime: string;
    function GetDateTimeISO: string;
    function GetAmount: string;
    function GetAmountDot: string;

    property Done: boolean read FDone write FDone;
    property Text: string read FText write FText;
    property Note: string read FNote write FNote;
    property Amount: double read FAmount write FAmount;
    property Date: TDateTime read FDate write FDate;
    property Archive: boolean read FArchive write FArchive;
    property Star: boolean read FStar write FStar;
    property NoteItalic: boolean read FNoteItalic write FNoteItalic;
    property EmptyNote: boolean read FEmptyNote write FEmptyNote;
    property SpaceBeforeNote: boolean read FSpaceBeforeNote write FSpaceBeforeNote;
    property SpaceAfterNote: boolean read FSpaceAfterNote write FSpaceAfterNote;
    property DateOriginal: string read FDateOriginal write FDateOriginal;
    property AmountOriginal: string read FAmountOriginal write FAmountOriginal;
    property AmountStr: string read GetAmount;
    property AmountStrDot: string read GetAmountDot;
    property DateStr: string read GetDate;
    property DateTimeStr: string read GetDateTime;
    property DateTimeStrISO: string read GetDateTimeISO;
    property Tags: TStringList read FTags write FTags;
    property TagsWidth: integer read FTagsWidth write FTagsWidth;
  end;

  // Class representing a collection of tasks
  TTasks = class
  private
    FGroupList: array of array of TTask; // Array of task groups
    FGroupNameList: TStringList; // List of group names
    FGroupHintList: TStringList; // List of group hints
    FTags: TStringList; // List of detected tags
    FTaskList: array of TTask; // Array of tasks
    FMapGrid: TIntegerArray; // Maps grid rows to tasks
    FBackupTaskList: array of TTask; // Backup array of tasks
    FInitGroupList: array of array of TTask; // Initial array of task groups
    FInitGroupNameList: TStringList; // List of group names
    FInitGroupHintList: TStringList; // List of group hints
    FInitTags: TStringList; // List of detected tags
    FSelectedGroup: integer;

    function GetGroupCount: integer;
    function GetCount: integer;
    function GetCountFilter(Archive: boolean; Filter: string; DisplayTime: boolean): integer;
  public
    constructor Create(const TaskStrings: TStringList = nil); // Constructor that takes a StringList
    destructor Destroy; override; // Destructor
    function ToStringList: TStringList;
    procedure AddGroup(const GroupName: string; const GroupHint: string; const TaskStrings: TStringList = nil);
    // Add new group from a StringList
    procedure UpdateGroup;
    procedure ChangeGroup(GroupIndex: integer; UpdateCurrent: boolean = False);
    function GetGroupName(Index: integer): string;
    function GetGroupHint(Index: integer): string;
    function GetGroupFiltered(Index: integer; ShowArchive: boolean; Filter: string; DisplayTime: boolean): boolean;
    function HasDuplicateMatches(const Value: string): boolean;
    function GetTaskCount(GroupIndex: integer): integer;
    function GetTaskInGroup(GroupIndex, TaskIndex: integer): TTask;
    procedure InitMap(Length: integer);
    function Map(Index: integer): integer;
    function ReverseMap(Value: integer): integer;
    procedure AddMap(Value: integer);
    procedure InsertMap(Pos, Value: integer; Delta: integer = 1);
    function AddTask(const TaskString: string): integer; // Method to add a task
    function AddGroupTask(const GroupIndex: integer; const TaskString: string): integer;
    function GetTask(Index: integer): TTask; // Method to get a task by row index
    function GetTaskValue(ACol, ARow: integer): string; // Method to get a task value by row col
    function HasTask(Index: integer): boolean;
    procedure SetTask(Grid: TStringGrid; Row: integer; Backup: boolean = True; DisplayTime: boolean = True);
    function InsertTask(const TaskString: string; Index: integer; Backup: boolean = True): integer;
    procedure DeleteTask(Index: integer);
    procedure ArchiveTask(Index: integer);
    procedure CompleteTask(Index: integer; Backup: boolean = True);
    procedure StarTask(Index: integer; Backup: boolean = True);
    procedure ClearTasksInRect(Grid: TStringGrid; Rect: TGridRect);
    function InsertGroup(aName: string): integer;
    function RenameGroup(aIndex: integer; aName: string): boolean;
    function RehintGroup(aIndex: integer; aHint: string): boolean;
    function CopyGroup(aIndex: integer; aName: string): boolean;
    function DeleteGroup(aIndex: integer): boolean;
    function GetLeftGroup(aIndex: integer; aShowArchived: boolean; aFilter: string; DisplayTime: boolean): integer;
    function GetRightGroup(aIndex: integer; aShowArchived: boolean; aFilter: string; DisplayTime: boolean): integer;
    function MoveGroupLeft(Index: integer; aShowArchived: boolean; aFilter: string; DisplayTime: boolean): integer;
    function MoveGroupRight(Index: integer; aShowArchived: boolean; aFilter: string; DisplayTime: boolean): integer;
    function MoveGroupTasks(Index1, Index2, NewGroup: integer): integer;
    function MoveTasksTop(Index1, Index2: integer; ShowArchived: boolean): integer;
    function MoveTasksBottom(Index1, Index2: integer; ShowArchived: boolean): integer;
    function MoveTasksUp(Index1, Index2: integer): integer;
    function MoveTasksDown(Index1, Index2: integer): integer;
    procedure SwapTasks(OldIndex, NewIndex: integer);
    procedure MoveTask(OldIndex, NewIndex: integer);
    procedure CopyToClipboard(Grid: TStringGrid; NoteVisible: boolean = False; Value: PString = nil);
    function PasteFromClipboard(Grid: TStringGrid; SortOrder: TSortOrder; Backup: boolean = True; Value: PString = nil): TGridRect;
    procedure FillTags;
    procedure CalcTagsWidths(Group: integer; ColWidth: integer; TagsEdit: TTagEdit; Font: TFont);
    procedure CreateBackup;
    procedure UndoBackup;
    procedure CreateBackupInit;
    procedure UndoBackupInit;
    function CalcDateDiff(const StartDate, EndDate: TDateTime): string;
    function CalcDateDiffAccuracy(const StartDate, EndDate: TDateTime): string;
    function CalcCount(Archive, Done: boolean; Filter: string; StartIndex: integer = 0; EndIndex: integer = 0;
      aDisplayTime: boolean = True): integer;
    function CalcSum(Archive, Done: boolean; Filter: string; StartIndex: integer = 0; EndIndex: integer = 0;
      aDisplayTime: boolean = True): double;
    function CalcDuration(Archive, Done: boolean; Filter: string; StartIndex: integer = 0; EndIndex: integer = 0;
      aDisplayTime: boolean = True): string;

    procedure FillGrid(Grid: TStringGrid; ShowArchive, ShowDuration, DisplayTime: boolean; SortOrder: TSortOrder;
      SortColumn: integer; Filter: string);

    property GroupNames: TStringList read FGroupNameList;
    property GroupHints: TStringList read FGroupHintList;
    property Tags: TStringList read FTags;
    property SelectedGroup: integer read FSelectedGroup;
    property CountGroup: integer read GetGroupCount;
    property Count: integer read GetCount;
    property MapGrid: TIntegerArray read FMapGrid;
  end;

resourcestring
  rseconds = 'sec';
  rminutes = 'min';
  rhours = 'h';
  rdays = 'd';
  rmonths = 'm';
  ryears = 'y';

implementation

uses mdformat;

  { TTask }

constructor TTask.Create;
begin
  FDone := False;
  FArchive := False;
  FDate := 0;
  FAmount := 0;
  FNote := string.Empty;
  FText := string.Empty;
  FStar := False;
  FNoteItalic := True;
  FEmptyNote := False;
  FSpaceBeforeNote := True;
  FSpaceAfterNote := True;
  FDateStart := 0;
  FDateEnd := 0;
  FAmountOriginal := string.Empty;
  FDateOriginal := string.Empty;
  FTags := TStringList.Create;
  FTags.Duplicates := dupIgnore;
  FTagsWidth := 0;
end;

constructor TTask.Create(const TaskString: string);
var
  Temp: TTask;
begin
  Temp := mdformat.TaskFromString(TaskString);
  Self.Copy(Temp); // Copy fields to new task
  Temp.Free;
end;

destructor TTask.Destroy;
begin
  FTags.Free;
  inherited;
end;

function TTask.ToString(Col: integer = 0; AddEmptyCompletion: boolean = True): string;
begin
  Result := mdformat.TaskToString(Self, Col, AddEmptyCompletion);
end;

procedure TTask.Copy(Original: TTask);
begin
  FDone := Original.FDone;
  FText := Original.FText;
  FNote := Original.FNote;
  FAmount := Original.FAmount;
  FDate := Original.FDate;
  FArchive := Original.FArchive;
  FStar := Original.FStar;
  FNoteItalic := Original.FNoteItalic;
  FEmptyNote := Original.FEmptyNote;
  FSpaceBeforeNote := Original.FSpaceBeforeNote;
  FSpaceAfterNote := Original.FSpaceAfterNote;
  FDateStart := Original.FDateStart;
  FDateEnd := Original.FDateEnd;
  FAmountOriginal := Original.FAmountOriginal;
  FDateOriginal := Original.FDateOriginal;
  if (not Assigned(FTags)) then
  begin
    FTags := TStringList.Create;
    FTags.Duplicates := dupIgnore;
  end;
  if (Assigned(Original.FTags)) then
    FTags.Assign(Original.FTags)
  else
    FTags.Clear;
  FTagsWidth := Original.FTagsWidth;
end;

procedure TTask.FillTags;
begin
  FillTagsFromString(FTags, FText, True);
end;

function TTask.MatchesFilter(const Filter: string; DisplayTime: boolean): boolean;
var
  TrimFilter, Oper, ValuePart, DateAsStr: string;

  function CompareWithOperator(const A: string; Op: string; const B: string): boolean;
  var
    NumA, NumB: double;
    DateA, DateB: TDateTime;
  begin
    // Try numeric comparison first
    if TryStrToFloat(B, NumB) then
    begin
      if TryStrToFloat(A, NumA) then
      begin
        if Op = '=' then Result := NumA = NumB
        else if Op = '>' then Result := NumA > NumB
        else if Op = '<' then Result := NumA < NumB
        else if Op = '>=' then Result := NumA >= NumB
        else if Op = '<=' then Result := NumA <= NumB
        else if (Op = '<>') or (Op = '!=') then Result := NumA <> NumB
        else
          Result := False;
      end
      else
        Result := False;
    end
    else
    if TryStrToDateTime(B, DateB) then
    begin
      if TryStrToDateTime(A, DateA) then
      begin
        // Date comparison
        if Op = '=' then Result := DateA = DateB
        else if Op = '>' then Result := DateA > DateB
        else if Op = '<' then Result := DateA < DateB
        else if Op = '>=' then Result := DateA >= DateB
        else if Op = '<=' then Result := DateA <= DateB
        else if (Op = '<>') or (Op = '!=') then Result := DateA <> DateB
        else
          Result := False;
      end
      else
        Result := False;
    end
    else
    begin
      // String comparison
      if Op = '=' then Result := ULower(A) = ULower(B)
      else if (Op = '<>') or (Op = '!=') then Result := ULower(A) <> ULower(B)
      else if Op = '!' then Result := Pos(ULower(B), ULower(A)) = 0
      else if Op = '#' then Result := Pos(ULower('#' + B), ULower(A)) > 0
      else
        Result := False;
    end;
  end;

  function CompareWithTags(const A: string; Op: string; OpAnd: boolean = False): boolean;
  var
    i: integer;
  begin
    Result := OpAnd;
    for i := 0 to FTags.Count - 1 do
    begin
      if Op = '#' then
      begin
        if Pos(ULower(A), ULower(FTags[i])) > 0 then
        begin
          if not OpAnd then exit(True);
        end
        else
        begin
          if OpAnd then exit(False);
        end;
      end
      else
      if Op = string.Empty then
      begin
        if Pos(ULower(A), ULower(FTags[i])) > 0 then
        begin
          if not OpAnd then exit(True);
        end
        else
        begin
          if OpAnd then exit(False);
        end;
      end
      else
      begin
        if CompareWithOperator(FTags[i], Op, A) then
        begin
          if not OpAnd then exit(True);
        end
        else
        begin
          if OpAnd then exit(False);
        end;
      end;
    end;
  end;

begin
  TrimFilter := Trim(Filter);
  if TrimFilter = string.Empty then
    Exit(True); // Empty filter matches everything

  if StartsWithOperator(TrimFilter, Oper, ValuePart) then
  begin
    DateAsStr := DateTimeToString(FDate, DisplayTime);
    // Compare with operator
    if (Oper = '#') then
    begin
      if ValuePart <> string.Empty then
      begin
        if CompareWithOperator(FText, Oper, ValuePart) or CompareWithOperator(FNote, Oper, ValuePart) or
          CompareWithTags(ValuePart, Oper) then
          Exit(True);
      end
      else
      begin
        if FTags.Count = 0 then Exit(True);
      end;
    end
    else
    if ValuePart = string.Empty then
      Exit(True) // Empty filter matches everything
    else
    if (Oper = '!') then
    begin
      // Special case for Star field (0/1)
      if (ValuePart = '*') then
        if FStar then
          Exit(False)
        else
          Exit(True);

      if CompareWithOperator(FText, Oper, ValuePart) and CompareWithOperator(FNote, Oper, ValuePart) and
        CompareWithOperator(FloatToStr(FAmount), Oper, ValuePart) and CompareWithOperator(DateAsStr, Oper, ValuePart) and
        CompareWithTags(ValuePart, Oper, True) then Exit(True);
    end
    else
    begin
      // Special case for Done field (0/1)
      if (Oper = '=') and ((ValuePart = '1') or (ValuePart = '0')) then
      begin
        if FDone = (ValuePart = '1') then
          Exit(True)
        else
          Exit(False);
      end;

      // Special case for Star field (0/1)
      if (Oper = '=') and (ValuePart = '*') then
      begin
        if FStar then
          Exit(True)
        else
          Exit(False);
      end;

      if CompareWithOperator(FText, Oper, ValuePart) then Exit(True);
      if CompareWithOperator(FNote, Oper, ValuePart) then Exit(True);
      if CompareWithOperator(FloatToStr(FAmount), Oper, ValuePart) then Exit(True);
      if CompareWithOperator(DateAsStr, Oper, ValuePart) then Exit(True);
      if CompareWithTags(ValuePart, Oper) then Exit(True);
    end;
  end
  else
  begin
    // Special case for Done field
    if (TrimFilter = '1') or (TrimFilter = '0') then
    begin
      if FDone = (TrimFilter = '1') then
        Exit(True)
      else
        Exit(False);
    end;

    // Special case for Star field (0/1)
    if (ValuePart = '*') then
    begin
      if FStar then
        Exit(True)
      else
        Exit(False);
    end;

    // No operator, simple substring search
    if (Pos(ULower(TrimFilter), ULower(FText)) > 0) then Exit(True);
    if (Pos(ULower(TrimFilter), ULower(FNote)) > 0) then Exit(True);
    if (Pos(ULower(TrimFilter), ULower(FloatToStr(FAmount))) > 0) then Exit(True);
    DateAsStr := DateTimeToString(FDate, DisplayTime);
    if (Pos(ULower(TrimFilter), ULower(DateAsStr)) > 0) then Exit(True);
    if CompareWithTags(TrimFilter, string.Empty) then Exit(True);
  end;
  Result := False;
end;

function TTask.GetDate: string;
begin
  Result := DateTimeToString(FDate, False);
end;

function TTask.GetDateTime: string;
begin
  Result := DateTimeToString(FDate);
end;

function TTask.GetDateTimeISO: string;
begin
  Result := DateTimeToStringISO(FDate);
end;

function TTask.GetAmount: string;
begin
  Result := FloatToString(FAmount);
end;

function TTask.GetAmountDot: string;
var
  FS: TFormatSettings;
begin
  FS.DecimalSeparator := '.';
  Result := FloatToString(FAmount, FS);
end;

{ TTasks }

constructor TTasks.Create(const TaskStrings: TStringList = nil);
begin
  SetLength(FGroupList, 0); // Initialize group array
  FSelectedGroup := -1;
  FGroupNameList := TStringList.Create;
  FGroupHintList := TStringList.Create;
  FTags := TStringList.Create;
  FTags.Duplicates := dupIgnore;

  if Assigned(TaskStrings) then
  begin
    mdformat.TasksFromStringList(TaskStrings, @AddGroup);
    TaskStrings.Free;
  end;

  CreateBackupInit;
  ChangeGroup(0, True);

  FillTags;
end;

destructor TTasks.Destroy;
var
  i, j: integer;
begin
  UpdateGroup;

  if Assigned(FGroupList) then
    for i := 0 to High(FGroupList) do
    begin
      for j := 0 to High(FGroupList[i]) do
      begin
        FGroupList[i, j].Free;
        FGroupList[i, j] := nil;
      end;
      SetLength(FGroupList[i], 0);
      FGroupList[i] := nil;
    end;
  SetLength(FGroupList, 0);
  FGroupList := nil;

  //if Assigned(FTaskList) then
  //  for i := 0 to High(FTaskList) do
  //    FTaskList[i].Free;
  SetLength(FTaskList, 0);
  FTaskList := nil;

  if Assigned(FBackupTaskList) then
    for i := 0 to High(FBackupTaskList) do
    begin
      FBackupTaskList[i].Free;
      FBackupTaskList[i] := nil;
    end;
  SetLength(FBackupTaskList, 0);
  FBackupTaskList := nil;

  if Assigned(FInitGroupList) then
    for i := 0 to High(FInitGroupList) do
    begin
      for j := 0 to High(FInitGroupList[i]) do
      begin
        FInitGroupList[i, j].Free;
        FInitGroupList[i, j] := nil;
      end;
      SetLength(FInitGroupList[i], 0);
      FInitGroupList[i] := nil;
    end;
  SetLength(FInitGroupList, 0);
  FInitGroupList := nil;

  if Assigned(FGroupNameList) then
    FGroupNameList.Free;

  if Assigned(FGroupHintList) then
    FGroupHintList.Free;

  if Assigned(FTags) then
    FTags.Free;

  if Assigned(FInitGroupNameList) then
    FInitGroupNameList.Free;

  if Assigned(FInitGroupHintList) then
    FInitGroupHintList.Free;

  if Assigned(FInitTags) then
    FInitTags.Free;

  SetLength(FMapGrid, 0);
  FMapGrid := nil;

  inherited;
end;

function TTasks.ToStringList: TStringList;
var
  i, j: integer;
  addCompleted: boolean;
begin
  try
    UpdateGroup;
    addCompleted := False;
    for i := 0 to CountGroup - 1 do
      for j := 0 to Length(FGroupList[i]) - 1 do
        if FGroupList[i, j].Done then addCompleted := True;

    Result := mdformat.TasksToStringList(CountGroup, addCompleted, @GetGroupName, @GetGroupHint, @GetTaskCount, @GetTaskInGroup);
  except
    Result.Free;
    raise;
  end;
end;

procedure TTasks.AddGroup(const GroupName: string; const GroupHint: string; const TaskStrings: TStringList = nil);
// Add new group from a StringList
var
  i: integer; // Index for iteration
begin
  SetLength(FGroupList, CountGroup + 1);
  FGroupNameList.Add(GroupName);
  FGroupHintList.Add(GroupHint);

  // Iterate through the StringList to create tasks
  if (Assigned(TaskStrings)) then
  begin
    for i := 0 to TaskStrings.Count - 1 do
    begin
      AddGroupTask(CountGroup - 1, TaskStrings[i]); // Create a new task from the string and add to the list
    end;
  end;
end;

procedure TTasks.UpdateGroup;
begin
  if (FSelectedGroup > -1) then
    FGroupList[SelectedGroup] := FTaskList;
end;

procedure TTasks.ChangeGroup(GroupIndex: integer; UpdateCurrent: boolean = False);
begin
  if (CountGroup <= 0) or (GroupIndex < 0) then exit;
  if (GroupIndex >= CountGroup) then
    GroupIndex := CountGroup - 1;

  if (UpdateCurrent) then UpdateGroup;
  SetLength(FMapGrid, 0); // Initialize task map
  FSelectedGroup := GroupIndex;
  FTaskList := FGroupList[GroupIndex];
  InitMap(Count + 1);
end;

function TTasks.GetGroupName(Index: integer): string;
begin
  Result := FGroupNameList[Index];
end;

function TTasks.GetGroupHint(Index: integer): string;
begin
  Result := FGroupHintList[Index];
end;

function TTasks.GetGroupFiltered(Index: integer; ShowArchive: boolean; Filter: string; DisplayTime: boolean): boolean;
var
  i: integer;
  TaskArray: array of TTask;
begin
  // Select the task array for the specified group
  if Index = SelectedGroup then
    TaskArray := FTaskList
  else
    TaskArray := FGroupList[Index];

  // If the group has no tasks, hide the tab
  if Length(TaskArray) = 0 then
    Exit(False);

  // Check if there is at least one task that is not archived (if ShowArchive = False)
  // and matches the filter
  for i := 0 to High(TaskArray) do
  begin
    if ((ShowArchive) or (not TaskArray[i].Archive)) and TaskArray[i].MatchesFilter(Filter, DisplayTime) then
      Exit(False); // Group should be visible
  end;

  // If no matching task is found, hide the group
  Result := True;
end;

function TTasks.HasDuplicateMatches(const Value: string): boolean;
var
  i: integer;
  MatchCount: integer;
begin
  MatchCount := 0;
  for i := 0 to High(FTaskList) do
  begin
    if (FTaskList[i].Text = Value) or (FTaskList[i].Note = Value) or (FTaskList[i].GetAmount = Value) or
      (FTaskList[i].GetDate = Value) then
    begin
      Inc(MatchCount);
      if MatchCount >= 2 then
      begin
        Result := True;
        Exit; // Exit early when second match is found
      end;
    end;
  end;
  Result := False;
end;

function TTasks.GetTaskCount(GroupIndex: integer): integer;
begin
  Result := Length(FGroupList[GroupIndex]);
end;

function TTasks.GetTaskInGroup(GroupIndex, TaskIndex: integer): TTask;
begin
  Result := FGroupList[GroupIndex, TaskIndex];
end;

procedure TTasks.InitMap(Length: integer);
var
  i: integer;
begin
  SetLength(FMapGrid, Length);
  if Length > 0 then
  begin
    FMapGrid[0] := -1;
    for i := 1 to Length - 1 do
      FMapGrid[i] := i - 1;
  end;
end;

function TTasks.Map(Index: integer): integer;
begin
  if (Index >= 0) and (Index < Length(FMapGrid)) then
    Result := FMapGrid[Index]
  else
    Result := -1;
end;

function TTasks.ReverseMap(Value: integer): integer;
var
  i: integer;
begin
  Result := -1; // Default value if not found
  for i := 0 to Length(FMapGrid) - 1 do
  begin
    if FMapGrid[i] = Value then
    begin
      Result := i;
      Exit; // Exit the loop as soon as the value is found
    end;
  end;
end;

procedure TTasks.AddMap(Value: integer);
begin
  SetLength(FMapGrid, Length(FMapGrid) + 1);
  FMapGrid[High(FMapGrid)] := Value;
end;

procedure TTasks.InsertMap(Pos, Value: integer; Delta: integer = 1);
begin
  InsertAtPos(FMapGrid, Pos, Value, Delta);
end;

function TTasks.AddTask(const TaskString: string): integer;
var
  Task: TTask;
begin
  Task := TTask.Create(TaskString); // Create a new task
  SetLength(FTaskList, Count + 1); // Resize the array
  FTaskList[Count - 1] := Task; // Add the task to the list
  Result := Count;
end;

function TTasks.AddGroupTask(const GroupIndex: integer; const TaskString: string): integer;
begin
  SetLength(FGroupList[GroupIndex], Length(FGroupList[GroupIndex]) + 1); // Resize the array
  FGroupList[GroupIndex, High(FGroupList[GroupIndex])] := TTask.Create(TaskString); // Add the task to the list
  Result := Length(FGroupList[GroupIndex]);
end;

function TTasks.GetTask(Index: integer): TTask;
var
  Ind: integer;
begin
  Ind := Map(Index);
  if (Ind < 0) or (Ind >= Count) then
    raise Exception.Create('Index out of bounds'); // Error handling for invalid index
  Result := FTaskList[Ind]; // Return the task by index
end;

function TTasks.GetTaskValue(ACol, aRow: integer): string;
begin
  Result := string.Empty;
  if ACol = 1 then
    if GetTask(aRow).FDone then Result := '1'
    else
      Result := '0'
  else
  if ACol = 2 then Result := GetTask(aRow).Text
  else
  if ACol = 3 then Result := GetTask(aRow).Note
  else
  if ACol = 4 then Result := GetTask(aRow).AmountStr
  else
  if ACol = 5 then Result := GetTask(aRow).DateTimeStrISO
  else
  if ACol = 6 then
    if GetTask(aRow).FStar then Result := '1'
    else
      Result := '0';
end;

function TTasks.HasTask(Index: integer): boolean;
var
  Ind: integer;
begin
  Ind := Map(Index);
  Result := (Ind >= 0) and (Ind < Count);
end;

procedure TTasks.SetTask(Grid: TStringGrid; Row: integer; Backup: boolean = True; DisplayTime: boolean = True);
var
  Task: TTask;
  pDate: TDateTime;
  pAmount: double;
begin
  if (Row > 0) and (Row <= Count) then
  begin
    if (Backup) then
      CreateBackup;

    Task := GetTask(Row);
    // Get the task by the row index (minus one, as rows start from 1)

    // Reading data from the grid
    Task.Done := StrToBoolDef(Grid.Cells[1, Row], False); // Convert to boolean
    Task.Text := Grid.Cells[2, Row].Replace(sLineBreak, '<br>').Replace(#10, string.Empty).Replace(
      #13, string.Empty).Replace('<br>', sLineBreak);
    Task.Note := Grid.Cells[3, Row].Replace(sLineBreak, '<br>').Replace(#10, string.Empty).Replace(
      #13, string.Empty).Replace('<br>', sLineBreak);

    if not TryStrToFloat(CleanNumeric(Grid.Cells[4, Row]), pAmount) then
    begin
      pAmount := 0; // If parsing the amount failed, set to 0
      Grid.Cells[4, Row] := string.Empty;
    end
    else
    if (pAmount = 0) then
      Grid.Cells[4, Row] := string.Empty
    else
      Grid.Cells[4, Row] := FloatToString(pAmount);
    Task.Amount := pAmount;
    if not TryStrToDateTime(Grid.Cells[5, Row], pDate) then
    begin
      pDate := 0; // If parsing the date failed, set to 0
      Grid.Cells[5, Row] := string.Empty;
    end
    else
      Grid.Cells[5, Row] := DateTimeToString(pDate, DisplayTime);
    Task.Date := pDate;
  end
  else
    raise Exception.Create('Invalid row or task index');
end;

function TTasks.InsertTask(const TaskString: string; Index: integer; Backup: boolean = True): integer;
var
  Task: TTask;
  i, Ind: integer;
begin
  if (Index = 0) then
    Ind := Count - 1
  else
    Ind := Map(Index);

  if (Ind < 0) and (Ind >= Count) then
    exit(-1);

  if (Backup) then
    CreateBackup;

  Ind := Ind + 1;
  Task := TTask.Create(TaskString); // Create a new task
  SetLength(FTaskList, Count + 1); // Resize the array

  // Shift tasks down to make space for the new task
  for i := Count - 1 downto Ind + 1 do
    FTaskList[i] := FTaskList[i - 1]; // Move tasks one position down

  FTaskList[Ind] := Task; // Insert the new task at the specified index

  Result := Ind;
end;

procedure TTasks.DeleteTask(Index: integer);
var
  i, Ind: integer;
begin
  Ind := Map(Index);
  if (Ind < 0) or (Ind >= Count) then
    exit;

  // Free the task that is being removed
  if (Assigned(FTaskList[Ind])) then
    FTaskList[Ind].Free;

  // Shift tasks down to fill the gap
  for i := Ind to Count - 2 do
  begin
    FTaskList[i] := FTaskList[i + 1]; // Move the next task to the current position
  end;

  // Resize the array to remove the last (now duplicate) element
  SetLength(FTaskList, Count - 1);

  // Delete record from FMapGrid
  for i := Index to Length(FMapGrid) - 2 do
  begin
    FMapGrid[i] := FMapGrid[i + 1];
  end;

  for i := 0 to Length(FMapGrid) - 1 do
    if (FMapGrid[i] > Ind) then Dec(FMapGrid[i]);

  // Resize FMapGrid to remove the last element
  SetLength(FMapGrid, Length(FMapGrid) - 1);
end;

procedure TTasks.ArchiveTask(Index: integer);
var
  Ind: integer;
begin
  Ind := Map(Index);
  if (Ind < 0) or (Ind >= Count) then
    exit;

  FTaskList[Ind].Archive := not FTaskList[Ind].Archive;
end;

procedure TTasks.CompleteTask(Index: integer; Backup: boolean = True);
var
  Ind: integer;
begin
  Ind := Map(Index);
  if (Ind < 0) or (Ind >= Count) then
    exit;

  if Backup then CreateBackup;

  FTaskList[Ind].Done := not FTaskList[Ind].Done;
end;

procedure TTasks.StarTask(Index: integer; Backup: boolean = True);
var
  Ind: integer;
begin
  Ind := Map(Index);
  if (Ind < 0) or (Ind >= Count) then
    exit;

  if Backup then CreateBackup;

  FTaskList[Ind].Star := not FTaskList[Ind].Star;
end;

procedure TTasks.ClearTasksInRect(Grid: TStringGrid; Rect: TGridRect);
var
  i, j: integer;
begin
  CreateBackup;

  for i := Rect.Top to Rect.Bottom do
  begin
    for j := Rect.Left to Rect.Right do
    begin
      // Clearing task fields based on the column
      if (j = 1) then
        GetTask(i).Done := False; // Reset completion status
      if (j = 2) then
        GetTask(i).Text := ''; // Clear task description
      if (j = 3) then
        GetTask(i).Note := ''; // Clear note
      if (j = 4) then
        GetTask(i).Amount := 0; // Reset amount
      if (j = 5) then
        GetTask(i).Date := 0; // Reset completion date
      if (j = 6) then
        GetTask(i).Star := False; // Reset starred

      // For grid column
      if (j = 1) then
        Grid.Cells[j, i] := '0'
      else
        Grid.Cells[j, i] := string.Empty;
    end;
  end;
end;

function TTasks.InsertGroup(aName: string): integer;
var
  i: integer;
begin
  UpdateGroup;

  // Increase the size of the group array
  SetLength(FGroupList, CountGroup + 1);
  FGroupNameList.Add(string.Empty);
  FGroupHintList.Add(string.Empty);

  // Check if the selected group is within bounds and shift groups to the right
  if (FSelectedGroup >= 0) and (FSelectedGroup < CountGroup - 2) then
  begin
    // Shift existing groups to the right to make space for the new group
    for i := CountGroup - 1 downto FSelectedGroup + 1 do
    begin
      FGroupList[i] := FGroupList[i - 1];
      FGroupNameList[i] := FGroupNameList[i - 1];
      FGroupHintList[i] := FGroupHintList[i - 1];
    end;
  end;

  // Increment the selected group index and set the name of the new group
  Inc(FSelectedGroup);
  SetLength(FGroupList[FSelectedGroup], 0);
  FGroupNameList[FSelectedGroup] := IfThen(aName = string.Empty, aName, '## ' + aName);
  FGroupHintList[FSelectedGroup] := string.Empty;

  // Change group to new one
  ChangeGroup(SelectedGroup);

  // Return the index of the newly inserted group
  Result := FSelectedGroup;
end;

function TTasks.RenameGroup(aIndex: integer; aName: string): boolean;
begin
  Result := False;
  if (aIndex < 0) or (aIndex >= CountGroup) then exit;
  FGroupNameList[aIndex] := IfThen(aName = string.Empty, aName, '## ' + aName);
  Result := True;
end;

function TTasks.RehintGroup(aIndex: integer; aHint: string): boolean;
begin
  Result := False;
  if (aIndex < 0) or (aIndex >= CountGroup) then exit;
  FGroupHintList[aIndex] := aHint;
  Result := True;
end;

function TTasks.CopyGroup(aIndex: integer; aName: string): boolean;
var
  i: integer;
begin
  Result := False;
  UpdateGroup;

  // Check if the index is within valid range
  if (aIndex < 0) or (aIndex >= CountGroup) then exit;

  FGroupNameList.Add(string.Empty);
  FGroupHintList.Add(string.Empty);
  SetLength(FGroupList, CountGroup + 1);

  // Shift groups to the right, overwriting the group at aIndex
  for i := CountGroup - 2 downto aIndex + 1 do
  begin
    FGroupList[i + 1] := FGroupList[i];
    FGroupNameList[i + 1] := FGroupNameList[i];
    FGroupHintList[i + 1] := FGroupHintList[i];
  end;

  // Copy tasks
  SetLength(FGroupList[aIndex + 1], Length(FGroupList[aIndex]));
  for i := 0 to Length(FGroupList[aIndex + 1]) - 1 do
  begin
    FGroupList[aIndex + 1, i] := TTask.Create;
    FGroupList[aIndex + 1, i].Copy(FGroupList[aIndex, i]);
  end;

  // Name new group
  FGroupNameList[aIndex + 1] := IfThen(aName = string.Empty, aName, '## ' + aName);
  FGroupHintList[aIndex + 1] := FGroupHintList[aIndex];

  Result := True;
end;

function TTasks.DeleteGroup(aIndex: integer): boolean;
var
  i: integer;
begin
  Result := False;
  UpdateGroup;

  // Check if the index is within valid range
  if (aIndex < 0) or (aIndex >= CountGroup) then exit;

  // Remove task from deleting group
  for i := 0 to High(FGroupList[aIndex]) do
  begin
    FGroupList[aIndex, i].Free;
    FGroupList[aIndex, i] := nil;
  end;
  FGroupList[aIndex] := nil;

  // Shift groups to the left, overwriting the group at aIndex
  for i := aIndex to CountGroup - 2 do
  begin
    FGroupList[i] := FGroupList[i + 1];
    FGroupNameList[i] := FGroupNameList[i + 1];
    FGroupHintList[i] := FGroupHintList[i + 1];
  end;

  // Decrease the size of the group array and name list
  FGroupNameList.Delete(CountGroup - 1);
  FGroupHintList.Delete(CountGroup - 1);
  SetLength(FGroupList, CountGroup - 1);

  // If groups don't exists then add new
  if (CountGroup = 0) then
  begin
    SetLength(FGroupList, CountGroup + 1);
    FGroupNameList.Add(string.Empty);
    FGroupHintList.Add(string.Empty);
  end;

  // Update the selected group if needed
  if FSelectedGroup >= CountGroup then
    FSelectedGroup := CountGroup - 1;

  ChangeGroup(FSelectedGroup);

  Result := True;
end;

function TTasks.GetLeftGroup(aIndex: integer; aShowArchived: boolean; aFilter: string; DisplayTime: boolean): integer;
var
  i, target: integer;
begin
  if (aShowArchived) then
  begin
    if (aIndex > 0) then
      target := aIndex - 1
    else
      target := -1;
  end
  else
  begin
    target := -1;
    for i := aIndex - 1 downto 0 do
      if (not GetGroupFiltered(i, aShowArchived, aFilter, DisplayTime)) then
      begin
        target := i;
        break;
      end;
  end;
  Result := target;
end;

function TTasks.GetRightGroup(aIndex: integer; aShowArchived: boolean; aFilter: string; DisplayTime: boolean): integer;
var
  i, target: integer;
begin
  if (aShowArchived) then
  begin
    if (aIndex < CountGroup - 1) then
      target := aIndex + 1
    else
      target := -1;
  end
  else
  begin
    target := -1;
    for i := aIndex + 1 to CountGroup - 1 do
      if (not GetGroupFiltered(i, aShowArchived, aFilter, DisplayTime)) then
      begin
        target := i;
        break;
      end;
  end;
  Result := target;
end;

function TTasks.MoveGroupLeft(Index: integer; aShowArchived: boolean; aFilter: string; DisplayTime: boolean): integer;
var
  tempGroup: array of TTask;
  tempName, tempHint: string;
  target, i: integer;
begin
  Result := Index;
  if (Index < 1) then exit;

  target := GetLeftGroup(Index, aShowArchived, aFilter, DisplayTime);
  if target < 0 then exit;

  // Save the group that will be moved
  tempGroup := FGroupList[Index];
  tempName := FGroupNameList[Index];
  tempHint := FGroupHintList[Index];

  // Shift all groups one position to the right (from target to Index-1)
  for i := Index downto target + 1 do
  begin
    FGroupList[i] := FGroupList[i - 1];
    FGroupNameList[i] := FGroupNameList[i - 1];
    FGroupHintList[i] := FGroupHintList[i - 1];
  end;

  // Place the saved group at the target position
  FGroupList[target] := tempGroup;
  FGroupNameList[target] := tempName;
  FGroupHintList[target] := tempHint;

  Result := target;
  FSelectedGroup := Result;
end;

function TTasks.MoveGroupRight(Index: integer; aShowArchived: boolean; aFilter: string; DisplayTime: boolean): integer;
var
  tempGroup: array of TTask;
  tempName, tempHint: string;
  target, i: integer;
begin
  Result := Index;
  if (Index >= CountGroup - 1) then exit;

  target := GetRightGroup(Index, aShowArchived, aFilter, DisplayTime);
  if target < 0 then exit;

  // Save the group that will be moved
  tempGroup := FGroupList[Index];
  tempName := FGroupNameList[Index];
  tempHint := FGroupHintList[Index];

  // Shift all groups one position to the left (from Index+1 to target)
  for i := Index to target - 1 do
  begin
    FGroupList[i] := FGroupList[i + 1];
    FGroupNameList[i] := FGroupNameList[i + 1];
    FGroupHintList[i] := FGroupHintList[i + 1];
  end;

  // Place the saved group at the target position
  FGroupList[target] := tempGroup;
  FGroupNameList[target] := tempName;
  FGroupHintList[target] := tempHint;

  Result := target;
  FSelectedGroup := Result;
end;

function TTasks.MoveGroupTasks(Index1, Index2, NewGroup: integer): integer;
var
  i, Len, Ind, IndStart, IndEnd, LastTask: integer;
begin
  Result := -1;
  if (NewGroup < 0) then exit;

  // Map the start and end indexes of the task range
  IndStart := Map(Index1);
  IndEnd := Map(Index2);
  if (IndStart > IndEnd) then
  begin
    i := IndEnd;
    IndEnd := IndStart;
    IndStart := i;
  end;

  // Calculate the number of tasks to move
  Len := IndEnd - IndStart + 1;

  // Get the last task index in the new group and resize the group array
  LastTask := Length(FGroupList[NewGroup]);
  SetLength(FGroupList[NewGroup], LastTask + Len);

  // Copy tasks from the current list to the new group
  Ind := IndStart;
  for i := LastTask to LastTask + Len - 1 do
  begin
    FGroupList[NewGroup, i] := TTask.Create;
    FGroupList[NewGroup, i].Copy(FTaskList[Ind]);
    Inc(Ind);
  end;

  // Remove tasks from the original group in reverse order
  for i := IndEnd downto IndStart do
    DeleteTask(ReverseMap(i));

  // Change the selected group to the new group
  ChangeGroup(NewGroup, True);

  // Return the index of the first moved task in the new group
  Result := LastTask;
end;

function TTasks.MoveTasksTop(Index1, Index2: integer; ShowArchived: boolean): integer;
var
  TempTasks: array of TTask = ();
  i, IndStart, IndEnd, Len, TargetIndex: integer;
begin
  Result := -1;
  IndStart := Map(Index1);
  IndEnd := Map(Index2);
  Len := IndEnd - IndStart + 1;

  // Check if the Index1 is valid
  if (IndStart >= 0) and (IndEnd < Count) then
  begin
    CreateBackup;

    // Determine target index based on ShowArchived
    if ShowArchived then
      TargetIndex := 0
    else
    begin
      // Find the first non-archived task from the top
      TargetIndex := 0;
      while (TargetIndex < Count) and FTaskList[TargetIndex].FArchive do
        Inc(TargetIndex);
    end;

    // Don't move if we're already at the correct position
    if IndStart = TargetIndex then
    begin
      Result := 0;
      Exit;
    end;

    // Save selected tasks to temporary array
    SetLength(TempTasks, Len);
    for i := IndStart to IndEnd do
      TempTasks[i - IndStart] := FTaskList[i];

    if IndStart > TargetIndex then
    begin
      // Moving up - shift tasks between TargetIndex and IndStart-1 down
      for i := IndStart - 1 downto TargetIndex do
        FTaskList[i + Len] := FTaskList[i];
    end
    else
    begin
      // Moving down - shift tasks between IndEnd+1 and TargetIndex-1 up
      for i := IndEnd + 1 to TargetIndex - 1 do
        FTaskList[i - Len] := FTaskList[i];
      TargetIndex := TargetIndex - Len;
    end;

    // Place saved tasks at the target position
    for i := 0 to High(TempTasks) do
      FTaskList[TargetIndex + i] := TempTasks[i];

    SetLength(TempTasks, 0);
    Result := 1;
  end;
end;

function TTasks.MoveTasksBottom(Index1, Index2: integer; ShowArchived: boolean): integer;
var
  TempTasks: array of TTask = ();
  i, IndStart, IndEnd, Len, TargetIndex: integer;
begin
  Result := -1;
  IndStart := Map(Index1);
  IndEnd := Map(Index2);
  Len := IndEnd - IndStart + 1;

  // Check if the Index1 is valid
  if (IndStart >= 0) and (IndEnd < Count) then
  begin
    CreateBackup;

    // Determine target index based on ShowArchived
    if ShowArchived then
      TargetIndex := Count - 1
    else
    begin
      // Find the last non-archived task from the bottom
      TargetIndex := Count - 1;
      while (TargetIndex >= 0) and FTaskList[TargetIndex].FArchive do
        Dec(TargetIndex);
    end;

    // Don't move if we're already at the correct position
    if IndEnd = TargetIndex then
    begin
      Result := TargetIndex;
      Exit;
    end;

    // Save selected tasks to temporary array
    SetLength(TempTasks, Len);
    for i := IndStart to IndEnd do
      TempTasks[i - IndStart] := FTaskList[i];

    if IndEnd < TargetIndex then
    begin
      // Moving down - shift tasks between IndEnd+1 and TargetIndex up
      for i := IndEnd + 1 to TargetIndex do
        FTaskList[i - Len] := FTaskList[i];

      // Place saved tasks at the target position
      for i := 0 to High(TempTasks) do
        FTaskList[TargetIndex - Len + 1 + i] := TempTasks[i];

      Result := TargetIndex - Len + 1;
    end
    else
    begin
      // Moving up - shift tasks between TargetIndex+1 and IndStart-1 down
      for i := IndStart - 1 downto TargetIndex + 1 do
        FTaskList[i + Len] := FTaskList[i];

      // Place saved tasks at the target position
      for i := 0 to High(TempTasks) do
        FTaskList[TargetIndex + 1 + i] := TempTasks[i];

      Result := TargetIndex + 1;
    end;

    SetLength(TempTasks, 0);
  end;
end;

function TTasks.MoveTasksUp(Index1, Index2: integer): integer;
var
  TempTasks: array of TTask; // Temporary array for selected tasks
  i, IndStart, IndEnd, Len: integer;
begin
  Result := -1;
  IndStart := Map(Index1);
  IndEnd := Map(Index2);
  Len := IndEnd - IndStart + 1;

  // Check if the selection is not at the top and valid
  if (IndStart > 0) and (IndEnd < Count) then
  begin
    CreateBackup;

    // Save selected tasks to temporary array
    TempTasks := [];
    SetLength(TempTasks, Len);
    for i := IndStart to IndEnd do
      TempTasks[i - IndStart] := FTaskList[i];

    // Shift tasks up by one position to fill the gap
    for i := IndEnd downto IndEnd - Len + 1 do
      if (i - Len >= 0) then
        FTaskList[i] := FTaskList[i - Len];

    // Place saved tasks at the new position
    for i := 0 to High(TempTasks) do
      FTaskList[IndStart - 1 + i] := TempTasks[i];

    Result := ReverseMap(IndStart - 1);
  end;
end;

function TTasks.MoveTasksDown(Index1, Index2: integer): integer;
var
  TempTasks: array of TTask; // Temporary array for selected tasks
  i, IndStart, IndEnd, Len: integer;
begin
  Result := -1;
  IndStart := Map(Index1);
  IndEnd := Map(Index2);
  Len := IndEnd - IndStart + 1;

  // Check if the selection is not at the bottom and valid
  if (IndEnd < Count - 1) and (IndStart >= 0) then
  begin
    CreateBackup;

    // Save selected tasks to temporary array
    TempTasks := [];
    SetLength(TempTasks, Len);
    for i := IndStart to IndEnd do
      TempTasks[i - IndStart] := FTaskList[i];

    // Shift tasks down by one position to fill the gap
    for i := IndStart to IndEnd do
      if (i + Len < Count) then
      begin
        FTaskList[i] := FTaskList[i + Len];
      end;

    // Place saved tasks at the new position
    for i := 0 to High(TempTasks) do
      FTaskList[IndStart + 1 + i] := TempTasks[i];

    Result := ReverseMap(IndEnd + 1);
  end;
end;

procedure TTasks.SwapTasks(OldIndex, NewIndex: integer);
var
  IndOld, IndNew: integer;
  TempTask: TTask;
begin
  // Get mapped indices for old and new positions
  IndOld := Map(OldIndex);
  IndNew := Map(NewIndex);

  // Check if both indices are valid
  if (IndOld < 0) or (IndNew < 0) or (IndOld >= Count) or (IndNew >= Count) then
    raise Exception.Create('Invalid index for swapping tasks');

  CreateBackup; // Backup the current state before swapping

  // Swap tasks in the task list
  TempTask := FTaskList[IndOld];
  FTaskList[IndOld] := FTaskList[IndNew];
  FTaskList[IndNew] := TempTask;

  // Swap the mapping in FMapGrid
  FMapGrid[OldIndex] := IndNew;
  FMapGrid[NewIndex] := IndOld;
end;

procedure TTasks.MoveTask(OldIndex, NewIndex: integer);
var
  Ind: integer;
  TempTask: TTask;
  i: integer;
begin
  Ind := Map(OldIndex);

  // Validate the indices
  if (Ind < 0) or (Ind >= Count) or (NewIndex < 0) or (NewIndex > Count) then
    Exit;

  CreateBackup; // Backup the current state before moving

  // Save the task to be moved
  TempTask := FTaskList[Ind];

  // If moving up
  if NewIndex < OldIndex then
  begin
    // Shift tasks down
    for i := OldIndex downto NewIndex + 1 do
    begin
      FTaskList[Map(i)] := FTaskList[Map(i - 1)];
    end;
  end
  // If moving down
  else if NewIndex > OldIndex then
  begin
    // Shift tasks up
    for i := OldIndex to NewIndex - 1 do
    begin
      FTaskList[Map(i)] := FTaskList[Map(i + 1)];
    end;
  end;

  // Insert the moved task into the new position
  FTaskList[Map(NewIndex)] := TempTask;

  // Update FMapGrid (if necessary)
  // This may require additional logic depending on how you want to track indices.
end;

procedure TTasks.CopyToClipboard(Grid: TStringGrid; NoteVisible: boolean = False; Value: PString = nil);
var
  SelectedText: TStringList;
  Rect: TGridRect;
  pDone, pText, pNote, pAmount, pDate: string;
  Row1, Row2, RowText: string;
  Str, Arh: boolean;
  i, j: integer;
begin
  SelectedText := TStringList.Create;
  SelectedText.Options := SelectedText.Options - [soTrailingLineBreak];

  try
    Rect := Grid.Selection; // Get grid selection rect

    if (Rect.Width = 0) and (Rect.Left > 1) then
    begin
      for i := Rect.Top to Rect.Bottom do
      begin
        SelectedText.Add(Grid.Cells[Rect.Left, i]);
      end;
    end
    else
      for i := Rect.Top to Rect.Bottom do
      begin
        for j := Rect.Left to Rect.Right do
        begin
          if (j = 1) and (Grid.Columns[j - 1].Visible) then pDone := GetTask(i).ToString(j).Trim;
          if (j = 2) and (Grid.Columns[j - 1].Visible) then
          begin
            pText := GetTask(i).ToString(j);
            Arh := False;
            if pText.StartsWith('~~') and pText.EndsWith('~~') then
            begin
              pText := RemoveFirstSubstring(pText, '~~');
              pText := RemoveFirstSubstring(pText, '~~', True);
              Arh := True;
            end;
            Str := False;
            if pText.StartsWith('**') and pText.EndsWith('**') then
            begin
              pText := RemoveFirstSubstring(pText, '**');
              pText := RemoveFirstSubstring(pText, '**', True);
              Str := True;
            end;
            if Rect.Width > 0 then pText := pText + StringListToBacktickString(GetTask(i).Tags, (pText <> string.Empty));
            if Str then pText := '**' + pText + '**';
            if Arh then pText := '~~' + pText + '~~';
          end;
          if (j = 3) and ((Grid.Columns[j - 1].Visible) or NoteVisible) then pNote := GetTask(i).ToString(j);
          if (j = 4) and (Grid.Columns[j - 1].Visible) then pAmount := GetTask(i).ToString(j).Trim;
          if (j = 5) and (Grid.Columns[j - 1].Visible) then pDate := GetTask(i).ToString(j).Trim;
        end;
        Row1 := (pDone + ' ' + pDate).Trim;
        Row2 := (pText + ifthen(StartsWith(pNote), '', ' ') + pNote);

        if (pDate <> string.Empty) and ((Row2 <> string.Empty) or (pAmount <> string.Empty)) then
          Row1 += ', '
        else
        if (Row1 <> string.Empty) and (Row2 <> string.Empty) then
          Row1 += ' ';

        if (pAmount <> string.Empty) then
        begin
          Row1 += pAmount;
          if (Row2 <> string.Empty) then Row1 += ', ';
        end;

        RowText := Row1 + Row2;

        if (Grid.Selection.Width = 0) and (Grid.Selection.Height = 0) then
          RowText := StringReplace(RowText, '<br>', sLineBreak, [rfReplaceAll]);

        SelectedText.Add(RowText);
      end;

    // Copy to clipboard
    if SelectedText.Count > 0 then
    begin
      if Assigned(Value) then
        Value^ := SelectedText.Text
      else
        Clipboard.AsText := SelectedText.Text;
    end;
  finally
    SelectedText.Free;
  end;
end;

function TTasks.PasteFromClipboard(Grid: TStringGrid; SortOrder: TSortOrder; Backup: boolean = True; Value: PString = nil): TGridRect;
var
  TempTasks: TTasks;
  TempTask: TTask;
  ListTasks: TStringList;
  TempDate: TDateTime;
  TempAmount: double;
  Rect: TGridRect;
  RowTask: TTask;
  IsRowEmpty, DoInsert: boolean;
  index, row, col: integer;

  procedure PasteSelecting;
  begin
    if col = 1 then // Done
      GetTask(row).Done := TempTask.Done
    else
    if col = 2 then // Task
    begin
      if (TempTask.Text <> string.Empty) then
      begin
        GetTask(row).Text := CleanString(TempTask.Text);
        if (Rect.Width > 0) then
        begin
          GetTask(row).Archive := TempTask.Archive;
          GetTask(row).Star := TempTask.Star;
        end;
      end
      else
      if Rect.Width = 0 then
      begin
        if (TempTask.Note <> string.Empty) then
          GetTask(row).Text := CleanString(TempTask.Note)
        else
        if (TempTask.Date <> 0) then
          GetTask(row).Text := TempTask.DateOriginal // DateTimeToStringISO(TempTask.Date)
        else
        if (TempTask.Amount <> 0) then
          GetTask(row).Text := TempTask.AmountOriginal // FloatToString(TempTask.Amount)
        else
          GetTask(row).Text := string.Empty;
      end
      else
        GetTask(row).Text := string.Empty;

      if TempTask.Tags.Count > 0 then
        GetTask(row).Tags.Assign(TempTask.Tags); // Tags
    end
    else
    if col = 3 then // Note
    begin
      if (TempTask.Note <> string.Empty) then
      begin
        GetTask(row).Note := CleanString(TempTask.Note);
        GetTask(row).NoteItalic := TempTask.NoteItalic;
      end
      else
      if Rect.Width = 0 then
      begin
        if (TempTask.Text <> string.Empty) then
          GetTask(row).Note := CleanString(TempTask.Text)
        else
        if (TempTask.Date <> 0) then
          GetTask(row).Note := TempTask.DateOriginal // DateTimeToStringISO(TempTask.Date)
        else
        if (TempTask.Amount <> 0) then
          GetTask(row).Note := TempTask.AmountOriginal // FloatToString(TempTask.Amount)
        else
          GetTask(row).Note := string.Empty;
      end
      else
        GetTask(row).Note := string.Empty;
    end
    else
    if col = 4 then // Amount
    begin
      if (TempTask.Amount <> 0) then
        GetTask(row).Amount := TempTask.Amount
      else
      if Rect.Width = 0 then
      begin
        if (TempTask.Text <> string.Empty) and (TryStrToFloatLimited(CleanNumeric(TempTask.Text), TempAmount)) then
          GetTask(row).Amount := TempAmount
        else
        if (TempTask.Note <> string.Empty) and (TryStrToFloatLimited(CleanNumeric(TempTask.Note), TempAmount)) then
          GetTask(row).Amount := TempAmount
        else
          GetTask(row).Amount := 0;
      end
      else
        GetTask(row).Amount := 0;
    end
    else
    if col = 5 then // Date
    begin
      if (TempTask.Date <> 0) then
        GetTask(row).Date := TempTask.Date
      else
      if Rect.Width = 0 then
      begin
        if (TempTask.Text <> string.Empty) and (TryStrToDateTimeISO(TempTask.Text, TempDate)) then
          GetTask(row).Date := TempDate
        else
        if (TempTask.Note <> string.Empty) and (TryStrToDateTimeISO(TempTask.Note, TempDate)) then
          GetTask(row).Date := TempDate
        else
          GetTask(row).Date := 0;
      end
      else
        GetTask(row).Date := 0;
    end
    else
    if col = 6 then // Star
    begin
      GetTask(row).Star := TempTask.Star;
    end;
  end;

begin
  Result := Grid.Selection;
  DoInsert := True;
  if (not Assigned(Value)) and (Clipboard.AsText = string.Empty) then exit;
  if Backup then CreateBackup;

  if (Grid.Row > 0) then
  begin
    RowTask := GetTask(Grid.Row);
    IsRowEmpty := (RowTask.Text = string.Empty) and (RowTask.Note = string.Empty) and (RowTask.Amount = 0) and (RowTask.Date = 0);
  end
  else
    IsRowEmpty := False;

  if (Assigned(Value)) then
    ListTasks := TextToStringList(Value^, True)
  else
    ListTasks := TextToStringList(Clipboard.AsText, True);

  // Handle SortOrder: reverse the list for descending order
  if (Grid.Selection.Height = 0) and (SortOrder = soDescending) then
    for row := 0 to ListTasks.Count div 2 - 1 do
      ListTasks.Exchange(row, ListTasks.Count - 1 - row);

  // Replace tabs in strings
  for row := 0 to ListTasks.Count - 1 do
    ListTasks[row] := CleanString(ListTasks[row]);

  TempTasks := TTasks.Create(ListTasks);
  try
    // If the row is not empty and only it is selected, and the number of tasks to insert is more than one,
    // if the grid is empty or grid selection is entry row, we insert as new tasks
    if ((not IsRowEmpty) and (TempTasks.Count > 1) and (Grid.Selection.Height = 0)) or (Grid.Row = 0) or
      ((Grid.Selection.Width = Grid.ColCount - 2) and (Grid.Selection.Height = 0)) then
    begin
      if (Grid.Row = 0) then
        index := 1
      else
        index := Grid.Row;
      for row := TempTasks.Count - 1 downto 0 do
        InsertTask(TempTasks.GetTask(row + 1).ToString(), index, False);
    end
    else
    begin
      index := 1;
      Rect := Grid.Selection;

      // If the line is empty and one line is selected, then we start with it
      if (IsRowEmpty) and (Rect.Height = 0) then
      begin
        TempTask := TempTasks.GetTask(index);
        if (Rect.Width = 0) and (TempTasks.Count = 1) then
        begin
          row := Rect.Top;
          col := Rect.Left;
          PasteSelecting;
        end
        else
          GetTask(Grid.Row).Copy(TempTask);

        Inc(index);
      end
      else
      begin
        // If the selection has height, then do not insert records below
        if (Grid.Selection.Height > 0) then
        begin
          DoInsert := False;
        end;

        for row := Rect.Top to Rect.Bottom do
        begin
          if (index > TempTasks.Count) then
          begin
            index := 1;
            DoInsert := False;
          end;

          TempTask := TempTasks.GetTask(index);
          for col := Rect.Left to Rect.Right do
            PasteSelecting;
          Inc(index);
        end;
      end;

      // Insert if clipboard is bigger than selection
      if (DoInsert) and (index - 1 < TempTasks.Count) then
      begin
        for row := TempTasks.Count - 1 downto index - 1 do
        begin
          InsertTask(TempTasks.GetTask(row + 1).ToString(), Grid.Row, False);
        end;
        Result := TGridRect.Create(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom + (TempTasks.Count - index) + 1);
      end;
    end;
  finally
    TempTasks.Free; // Free the temporary TTasks object
  end;
end;

procedure TTasks.FillTags;
var
  i, j: integer;
  Up, Down: TStringList;
begin
  Up := TStringList.Create;
  Up.Sorted := True;
  Up.Duplicates := dupIgnore;
  Down := TStringList.Create;
  Down.Sorted := True;
  Down.Duplicates := dupIgnore;
  try
    // Current group
    for i := 0 to High(FTaskList) do
    begin
      FTaskList[i].FillTags;
      Up.AddStrings(FTaskList[i].Tags);

      FillTagsFromString(Down, FTaskList[i].FText);
      FillTagsFromString(Down, FTaskList[i].FNote);
    end;

    // All Groups
    for i := 0 to High(FGroupList) do
      if SelectedGroup <> i then
        for j := 0 to High(FGroupList[i]) do
        begin
          FGroupList[i][j].FillTags;
          Up.AddStrings(FGroupList[i][j].Tags);

          FillTagsFromString(Down, FGroupList[i][j].FText);
          FillTagsFromString(Down, FGroupList[i][j].FNote);
        end;

    // Add prefixes
    AddPrefixTags(Up);

    // Add Tags
    FTags.Clear;
    FTags.AddStrings(Up);
    FTags.AddStrings(Down);
  finally
    Up.Free;
    Down.Free;
  end;
end;

procedure TTasks.CalcTagsWidths(Group: integer; ColWidth: integer; TagsEdit: TTagEdit; Font: TFont);
var
  i: integer;
  BitTags: TBitmap;

  procedure CalcTask(task: TTask);
  begin
    if task.Tags.Count > 0 then
    begin
      BitTags := TagsEdit.GetTagsBitmap(task.Tags, Max(Font.Size div 2 + 2, 8), Min(ColWidth, 500), 32, 2);
      try
        task.TagsWidth := BitTags.Width;
      finally
        BitTags.Free;
      end;
    end;
  end;

begin
  if Group < 0 then
    for i := 0 to High(FTaskList) do
      CalcTask(FTaskList[i])
  else
    for i := 0 to High(FGroupList[Group]) do
      CalcTask(FGroupList[Group][i]);
end;

procedure TTasks.CreateBackup;
var
  i: integer;
begin
  // Free old backup tasks if they exist
  for i := 0 to High(FBackupTaskList) do
    if Assigned(FBackupTaskList[i]) then
      FBackupTaskList[i].Free;

  // Create a backup of the task list and map grid
  SetLength(FBackupTaskList, Length(FTaskList));
  for i := 0 to High(FTaskList) do
  begin
    FBackupTaskList[i] := TTask.Create;
    FBackupTaskList[i].Copy(FTaskList[i]);
  end;
end;

procedure TTasks.UndoBackup;
var
  i: integer;
  TempTaskList: array of TTask;
begin
  TempTaskList := [];

  // Make an intermediate backup of the current task list
  SetLength(TempTaskList, Length(FTaskList));
  for i := 0 to High(TempTaskList) do
  begin
    TempTaskList[i] := TTask.Create;
    TempTaskList[i].Copy(FTaskList[i]);
  end;

  // Free old task list before resizing and replacing
  for i := 0 to High(FTaskList) do
  begin
    if Assigned(FTaskList[i]) then
    begin
      FTaskList[i].Free;
      FTaskList[i] := nil; // Set to nil after freeing
    end;
  end;
  SetLength(FTaskList, 0);

  // Restore task list from the original backup
  SetLength(FTaskList, Length(FBackupTaskList));
  for i := 0 to High(FTaskList) do
  begin
    FTaskList[i] := TTask.Create;
    FTaskList[i].Copy(FBackupTaskList[i]);
  end;

  // Free old backup task list before resizing and replacing
  for i := 0 to High(FBackupTaskList) do
  begin
    if Assigned(FBackupTaskList[i]) then
    begin
      FBackupTaskList[i].Free;
      FBackupTaskList[i] := nil; // Set to nil after freeing
    end;
  end;
  SetLength(FBackupTaskList, 0);

  // Update the backup with the intermediate state
  SetLength(FBackupTaskList, Length(TempTaskList));
  for i := 0 to High(FBackupTaskList) do
  begin
    FBackupTaskList[i] := TTask.Create;
    FBackupTaskList[i].Copy(TempTaskList[i]);

    // Free the temporary task after copying
    if Assigned(TempTaskList[i]) then
    begin
      TempTaskList[i].Free;
      TempTaskList[i] := nil; // Set to nil after freeing
    end;
  end;

  // Optionally, clear the TempTaskList array itself
  SetLength(TempTaskList, 0);
end;

procedure TTasks.CreateBackupInit;
var
  i, j: integer;
begin
  // Free old backup task list if it exists
  for i := 0 to High(FInitGroupList) do
    for j := 0 to High(FInitGroupList[i]) do
    begin
      if Assigned(FInitGroupList[i, j]) then
      begin
        FInitGroupList[i, j].Free; // Free the old task
        FInitGroupList[i, j] := nil; // Set to nil after freeing
      end;
    end;
  SetLength(FInitGroupList, 0);

  // Create a backup of the task list
  SetLength(FInitGroupList, Length(FGroupList));
  for i := 0 to High(FInitGroupList) do
  begin
    SetLength(FInitGroupList[i], Length(FGroupList[i]));
    for j := 0 to High(FInitGroupList[i]) do
    begin
      FInitGroupList[i, j] := TTask.Create; // Create a new task
      FInitGroupList[i, j].Copy(FGroupList[i, j]); // Copy the task data
    end;
  end;

  // Create a backup of tab names
  if Assigned(FInitGroupNameList) then
  begin
    FInitGroupNameList.Free;
    FInitGroupNameList := nil;
  end;
  if Assigned(FGroupNameList) then
  begin
    FInitGroupNameList := TStringList.Create;
    FInitGroupNameList.Assign(FGroupNameList);
  end;

  if Assigned(FInitGroupHintList) then
  begin
    FInitGroupHintList.Free;
    FInitGroupHintList := nil;
  end;
  if Assigned(FGroupHintList) then
  begin
    FInitGroupHintList := TStringList.Create;
    FInitGroupHintList.Assign(FGroupHintList);
  end;

  if Assigned(FInitTags) then
  begin
    FInitTags.Free;
    FInitTags := nil;
  end;
  if Assigned(FTags) then
  begin
    FInitTags := TStringList.Create;
    FInitTags.Duplicates := dupIgnore;
    FInitTags.Assign(FTags);
  end;
end;

procedure TTasks.UndoBackupInit;
var
  i, j: integer;
begin
  UpdateGroup;

  // Free existing objects in FGroupList before resizing
  for i := 0 to High(FGroupList) do
  begin
    for j := 0 to High(FGroupList[i]) do
    begin
      if Assigned(FGroupList[i, j]) then
      begin
        FGroupList[i, j].Free;
        FGroupList[i, j] := nil;
      end;
    end;
    SetLength(FGroupList[i], 0);
    FGroupList[i] := nil;
  end;
  SetLength(FGroupList, 0);
  FGroupList := nil;

  // Resize the task list to match the initial task list
  SetLength(FGroupList, Length(FInitGroupList));
  for i := 0 to High(FInitGroupList) do
  begin
    SetLength(FGroupList[i], Length(FInitGroupList[i]));

    for j := 0 to High(FInitGroupList[i]) do
    begin
      FGroupList[i, j] := TTask.Create;
      FGroupList[i, j].Copy(FInitGroupList[i, j]);
    end;
  end;

  // Restore a backup of tab names
  if Assigned(FGroupNameList) then
  begin
    FGroupNameList.Free;
    FGroupNameList := nil;
  end;
  if Assigned(FGroupHintList) then
  begin
    FGroupHintList.Free;
    FGroupHintList := nil;
  end;
  if Assigned(FTags) then
  begin
    FTags.Free;
    FTags := nil;
  end;
  if Assigned(FInitGroupNameList) then
  begin
    FGroupNameList := TStringList.Create;
    FGroupNameList.Assign(FInitGroupNameList);
  end;
  if Assigned(FInitGroupHintList) then
  begin
    FGroupHintList := TStringList.Create;
    FGroupHintList.Assign(FInitGroupHintList);
  end;
  if Assigned(FInitTags) then
  begin
    FTags := TStringList.Create;
    FTags.Duplicates := dupIgnore;
    FTags.Assign(FInitTags);
  end;

  ChangeGroup(SelectedGroup);
end;

function TTasks.CalcDateDiff(const StartDate, EndDate: TDateTime): string;
var
  YearsDiff, MonthsDiff, DaysDiff, HoursDiff, MinutesDiff, SecondsDiff: integer;
begin
  // Calculate difference in yeard
  YearsDiff := YearsBetween(StartDate, EndDate, True);
  if YearsDiff <> 0 then
  begin
    if (StartDate < EndDate) then
      Result := IntToStr(YearsDiff) + ryears
    else
      Result := '-';
    Exit;
  end;

  // Calculate difference in months
  MonthsDiff := MonthsBetween(StartDate, EndDate);
  if (MonthsDiff = 0) and (IncMonth(StartDate) <= EndDate) then
    MonthsDiff := 1;
  if MonthsDiff <> 0 then
  begin
    if (StartDate < EndDate) then
      Result := IntToStr(MonthsDiff) + rmonths
    else
      Result := '-';
    Exit;
  end;

  // Calculate difference in days
  DaysDiff := DaysBetween(StartDate, EndDate);
  if DaysDiff <> 0 then
  begin
    if (StartDate < EndDate) then
      Result := IntToStr(DaysDiff) + rdays
    else
      Result := '-';
    Exit;
  end;

  // Calculate difference in hours
  HoursDiff := HoursBetween(StartDate, EndDate);
  if HoursDiff <> 0 then
  begin
    if (StartDate < EndDate) then
      Result := IntToStr(HoursDiff) + rhours
    else
      Result := '-';
    Exit;
  end;

  // Calculate difference in minutes
  MinutesDiff := MinutesBetween(StartDate, EndDate);
  if MinutesDiff <> 0 then
  begin
    if (StartDate < EndDate) then
      Result := IntToStr(MinutesDiff) + rminutes
    else
      Result := '-';
    Exit;
  end;

  // Calculate difference in seconds
  SecondsDiff := SecondsBetween(StartDate, EndDate);
  if SecondsDiff <> 0 then
  begin
    if (StartDate < EndDate) then
      Result := IntToStr(SecondsDiff) + rseconds
    else
      Result := '-';
    Exit;
  end;

  Result := '0' + rseconds;
end;

function TTasks.CalcDateDiffAccuracy(const StartDate, EndDate: TDateTime): string;
var
  YearsDiff, MonthsDiff, DaysDiff, HoursDiff, MinutesDiff, SecondsDiff: integer;
  TempEndDate: TDateTime;
begin
  Result := '';
  TempEndDate := Abs(EndDate);

  // Calculate difference in years
  YearsDiff := YearsBetween(TempEndDate, StartDate);
  if (YearsDiff = 0) and (IncYear(StartDate) <= TempEndDate) then
    YearsDiff := 1;

  if YearsDiff <> 0 then
  begin
    Result := Result + IntToStr(YearsDiff) + ryears;
    TempEndDate := IncYear(TempEndDate, -YearsDiff); // Decrement TempEndDate by YearsDiff years
  end;

  // Calculate difference in months
  if (TempEndDate > StartDate) then
  begin
    MonthsDiff := MonthsBetween(TempEndDate, StartDate);
    if (MonthsDiff = 0) and (IncMonth(StartDate) <= TempEndDate) then
      MonthsDiff := 1;

    if MonthsDiff <> 0 then
    begin
      Result := Result + ' ' + IntToStr(MonthsDiff) + rmonths;
      TempEndDate := IncMonth(TempEndDate, -MonthsDiff); // Decrement TempEndDate by MonthsDiff months
    end;
  end;

  // Calculate difference in days
  if (TempEndDate > StartDate) then
  begin
    DaysDiff := DaysBetween(TempEndDate, StartDate);
    if (DaysDiff = 0) and (IncDay(StartDate) <= TempEndDate) then
      DaysDiff := 1;

    if DaysDiff <> 0 then
    begin
      Result := Result + ' ' + IntToStr(DaysDiff) + rdays;
      TempEndDate := IncDay(TempEndDate, -DaysDiff); // Decrement TempEndDate by DaysDiff days
    end;
  end;

  // Calculate difference in hours
  if (TempEndDate > StartDate) then
  begin
    HoursDiff := HoursBetween(TempEndDate, StartDate);
    if (HoursDiff = 0) and (IncHour(StartDate) <= TempEndDate) then
      HoursDiff := 1;

    if HoursDiff <> 0 then
    begin
      Result := Result + ' ' + IntToStr(HoursDiff) + rhours;
      TempEndDate := IncHour(TempEndDate, -HoursDiff); // Decrement TempEndDate by HoursDiff hours
    end;
  end;

  // Calculate difference in minutes
  if (TempEndDate > StartDate) then
  begin
    MinutesDiff := MinutesBetween(TempEndDate, StartDate);
    if (MinutesDiff = 0) and (IncMinute(StartDate) <= TempEndDate) then
      MinutesDiff := 1;

    if MinutesDiff <> 0 then
    begin
      Result := Result + ' ' + IntToStr(MinutesDiff) + rminutes;
      TempEndDate := IncMinute(TempEndDate, -MinutesDiff); // Decrement TempEndDate by MinutesDiff minutes
    end;
  end;

  // Calculate difference in seconds
  if (TempEndDate > StartDate) then
  begin
    SecondsDiff := SecondsBetween(TempEndDate, StartDate);
    if SecondsDiff <> 0 then
    begin
      Result := Result + ' ' + IntToStr(SecondsDiff) + rseconds;
    end;
  end;

  // Remove leading/trailing whitespace
  Result := Trim(Result);
end;

function TTasks.CalcCount(Archive, Done: boolean; Filter: string; StartIndex: integer = 0; EndIndex: integer = 0;
  aDisplayTime: boolean = True): integer;
var
  I, Ind: integer;
begin
  Result := 0;
  if (StartIndex = 0) and (EndIndex = 0) then
  begin
    for I := 0 to Count - 1 do
    begin
      if ((Archive = True) or (FTaskList[I].Archive = False)) and ((Done = False) or (FTaskList[I].Done = True)) and
        (FTaskList[I].MatchesFilter(Filter, aDisplayTime)) then
        Result += 1;
    end;
  end
  else
  begin
    for I := StartIndex to EndIndex do
    begin
      Ind := Map(I);
      if (Ind > -1) and ((Archive = True) or (FTaskList[Ind].Archive = False)) and ((Done = False) or (FTaskList[Ind].Done = True)) then
        Result += 1;
    end;
  end;
end;

function TTasks.CalcSum(Archive, Done: boolean; Filter: string; StartIndex: integer = 0; EndIndex: integer = 0;
  aDisplayTime: boolean = True): double;
var
  I, Ind: integer;
begin
  Result := 0;
  if (StartIndex = 0) and (EndIndex = 0) then
  begin
    for I := 0 to Count - 1 do
    begin
      if ((Archive = True) or (FTaskList[I].Archive = False)) and ((Done = False) or (FTaskList[I].Done = True)) and
        (FTaskList[I].MatchesFilter(Filter, aDisplayTime)) then
        Result += FTaskList[I].Amount;
    end;
  end
  else
  begin
    for I := StartIndex to EndIndex do
    begin
      Ind := Map(I);
      if (Ind > -1) and ((Archive = True) or (FTaskList[Ind].Archive = False)) and ((Done = False) or (FTaskList[Ind].Done = True)) then
        Result += FTaskList[Ind].Amount;
    end;
  end;
end;

function TTasks.CalcDuration(Archive, Done: boolean; Filter: string; StartIndex: integer = 0; EndIndex: integer = 0;
  aDisplayTime: boolean = True): string;
var
  I, Ind: integer;
  TotalDuration: TDateTime;
begin
  Result := string.Empty;
  TotalDuration := 0;
  if (StartIndex = 0) and (EndIndex = 0) then
  begin
    for I := 0 to Count - 1 do
    begin
      if ((Archive = True) or (FTaskList[I].Archive = False)) and ((Done = False) or (FTaskList[I].Done = True) or
        (FTaskList[I].Archive = True)) and (FTaskList[I].MatchesFilter(Filter, aDisplayTime)) then
        TotalDuration += (FTaskList[I].FDateEnd - FTaskList[I].FDateStart);
    end;
  end
  else
  begin
    for I := StartIndex to EndIndex do
    begin
      Ind := Map(I);
      if (Ind > -1) and ((Archive = True) or (FTaskList[Ind].Archive = False)) and ((Done = False) or
        (FTaskList[Ind].Done = True) or (FTaskList[Ind].Archive = True)) then
        TotalDuration += (FTaskList[Ind].FDateEnd - FTaskList[Ind].FDateStart);
    end;
  end;
  Result := CalcDateDiffAccuracy(0, TotalDuration).Replace('-', string.Empty);
end;

function TTasks.GetGroupCount: integer;
begin
  Result := Length(FGroupList);
end;

function TTasks.GetCount: integer;
begin
  Result := Length(FTaskList);
end;

function TTasks.GetCountFilter(Archive: boolean; Filter: string; DisplayTime: boolean): integer;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if (not Archive and FTaskList[I].Archive) or (not FTaskList[I].MatchesFilter(Filter, DisplayTime)) then Result += 1;
end;

procedure TTasks.FillGrid(Grid: TStringGrid; ShowArchive, ShowDuration, DisplayTime: boolean; SortOrder: TSortOrder;
  SortColumn: integer; Filter: string);
var
  I, J, ArhCount, RowIndex: integer;
  LastDate, MinDate, MaxDate: TDateTime;
  StartDates, EndDates: array of TDateTime;
  StartDate, EndDate: TDateTime;
  DateDiff: string;
  eventOnColRowInserted: TGridOperationEvent;
  eventOnColRowDeleted: TGridOperationEvent;

// Compare tasks for soring
  function CompareTasks(Index1, Index2: integer): integer;
  var
    Task1, Task2: TTask;
    Value1, Value2: string;
    DateTime1, DateTime2: TDateTime;
    Amount1, Amount2: double;
  begin
    Task1 := FTaskList[FMapGrid[Index1]];
    Task2 := FTaskList[FMapGrid[Index2]];

    case SortColumn of
      1: begin
        // Completed status
        Value1 := IntToStr(Ord(Task1.Done));
        Value2 := IntToStr(Ord(Task2.Done));
      end;
      2: begin
        // Task description
        Value1 := Task1.Text;
        Value2 := Task2.Text;
      end;
      3: begin
        // Note
        Value1 := Task1.Note;
        Value2 := Task2.Note;
      end;
      4: begin
        // Completion amount (compare as double)
        Amount1 := Task1.Amount;
        Amount2 := Task2.Amount;

        if Amount1 = Amount2 then
          Result := 0
        else if SortOrder = soAscending then
        begin
          if Amount1 < Amount2 then
            Result := -1
          else
            Result := 1;
        end
        else
        begin
          if Amount1 > Amount2 then
            Result := -1
          else
            Result := 1;
        end;
        Exit;
      end;
      5: begin
        // Completion date (compare as DateTime)
        DateTime1 := Task1.Date;
        DateTime2 := Task2.Date;

        if DateTime1 = DateTime2 then
          Result := 0
        else if SortOrder = soAscending then
        begin
          if DateTime1 < DateTime2 then
            Result := -1
          else
            Result := 1;
        end
        else
        begin
          if DateTime1 > DateTime2 then
            Result := -1
          else
            Result := 1;
        end;
        Exit;
      end;
      6: begin
        // Completed status
        Value1 := IntToStr(Ord(Task1.Star));
        Value2 := IntToStr(Ord(Task2.Star));
      end;
      else
        // Default comparison (index comparison)
        Result := FMapGrid[Index1] - FMapGrid[Index2];
        Exit;
    end;

    // Compare as strings for all columns except the date
    if SortOrder = soAscending then
      Result := CompareStr(Value1, Value2)
    else
      Result := CompareStr(Value2, Value1);
  end;

  // Swap rows in the grid and update FMapGrid
  procedure SwapRows(Row1, Row2: integer);
  var
    Temp: TStringList;
    Col, TempMap: integer;
  begin
    Temp := TStringList.Create;
    try
      // Copy Row1 to Temp
      for Col := 0 to Grid.ColCount - 1 do
        Temp.Add(Grid.Cells[Col, Row1]);

      // Move Row2 to Row1
      for Col := 0 to Grid.ColCount - 1 do
        Grid.Cells[Col, Row1] := Grid.Cells[Col, Row2];

      // Move Temp to Row2
      for Col := 0 to Grid.ColCount - 1 do
        Grid.Cells[Col, Row2] := Temp[Col];

      // Swap the corresponding values in FMapGrid
      TempMap := FMapGrid[Row1];
      FMapGrid[Row1] := FMapGrid[Row2];
      FMapGrid[Row2] := TempMap;
    finally
      Temp.Free;
    end;
  end;

  // Add new dates interval to calulate task time length
  procedure AddDatesInterval(Index: integer);
  begin
    SetLength(StartDates, Length(StartDates) + 1);
    SetLength(EndDates, Length(EndDates) + 1);
    StartDates[High(StartDates)] := StartDate;
    EndDates[High(EndDates)] := EndDate;
    FTaskList[Index].FDateStart := StartDate;
    FTaskList[Index].FDateEnd := EndDate;
  end;

  // Calculate total duration of all tasks
  function CalculateTotalDuration: TDateTime;
  var
    i: integer;
    TotalDuration: TDateTime;
  begin
    TotalDuration := 0;

    // We go through each interval
    for i := Low(StartDates) to High(StartDates) do
    begin
      TotalDuration += (EndDates[i] - StartDates[i]);
    end;

    Result := TotalDuration;
  end;

begin
  try
    Grid.BeginUpdate;
    eventOnColRowInserted := Grid.OnColRowInserted;
    eventOnColRowDeleted := Grid.OnColRowDeleted;
    Grid.OnColRowInserted := nil;
    Grid.OnColRowDeleted := nil;
    LastDate := IfThen(displaytime, Now, Date);
    MinDate := IfThen(displaytime, Now, Date);
    MaxDate := 0;
    StartDate := 0;
    EndDate := 0;
    SetLength(StartDates, 0);
    SetLength(EndDates, 0);

    // Task count
    ArhCount := GetCountFilter(ShowArchive, Filter, DisplayTime);
    Grid.RowCount := Count - ArhCount + 1;

    // Default row indexing based on sort order
    if SortOrder = soAscending then
      RowIndex := 1
    else
      RowIndex := Grid.RowCount - 1;

    InitMap(Grid.RowCount);

    // Fill the grid with tasks
    for I := 0 to Count - 1 do
    begin
      if ((ShowArchive = True) or (FTaskList[I].Archive = False)) and ((Filter = string.Empty) or
        (FTaskList[I].MatchesFilter(Filter, DisplayTime))) then
      begin
        if (Grid.RowCount > RowIndex) then
          Grid.Cells[0, RowIndex] := (I + 1).ToString;

        // Duration calculation
        if (ShowDuration) then
        begin
          // Clear task dates
          FTaskList[I].FDateStart := 0;
          FTaskList[I].FDateEnd := 0;

          // Check if the task's date is earlier than the current minimum date
          if (FTaskList[I].Date > 0) and (FTaskList[I].Date < MinDate) then
            MinDate := FTaskList[I].Date;

          // For non-initial tasks, check previous tasks to adjust the minimum date further if needed
          if (i > 0) and (FTaskList[I].Date > 0) and (FTaskList[I].Date < LastDate) then
          begin
            // Loop through previous tasks to find a suitable minimum date
            for j := i - 1 downto 0 do
            begin
              MinDate := FTaskList[J].Date;
              // Break the loop if a valid previous task date is found
              if (FTaskList[J].Date > 0) and (FTaskList[J].Date <= FTaskList[I].Date) then
                break;
            end;
          end;

          // Update the maximum date if the current task date is later than the current maximum
          if (FTaskList[I].Date > MaxDate) then
            MaxDate := FTaskList[I].Date;

          // Calculate and display the date difference if applicable
          if FTaskList[I].Date > 0 then
          begin
            if (LastDate > 0) then
            begin
              StartDate := LastDate;
              EndDate := FTaskList[I].Date;
              DateDiff := CalcDateDiff(StartDate, EndDate);

              // If the date difference is invalid, recalculate with the minimum date
              if (DateDiff = '-') and (MinDate > 0) and (LastDate <> IfThen(displaytime, Now, Date)) then
              begin
                StartDate := MinDate;
                EndDate := FTaskList[I].Date;
                DateDiff := CalcDateDiff(StartDate, EndDate);

                MinDate := FTaskList[I].Date;
              end;

              // Display the calculated date difference in the grid
              if (DateDiff <> '-') and (Grid.RowCount > RowIndex) and ((RowIndex > 1) or (DateDiff <> '0' + rseconds)) then
              begin
                AddDatesInterval(I);
                Grid.Cells[0, RowIndex] := (I + 1).ToString + '. ' + DateDiff;
              end;
            end;
          end;

          // Update LastDate to the current task's date for the next iteration
          if FTaskList[I].Date > 0 then
            LastDate := FTaskList[I].Date;
        end;

        // Fill task in grid for archive or not
        Grid.Cells[1, RowIndex] := IntToStr(Ord(FTaskList[I].Done));
        Grid.Cells[2, RowIndex] := FTaskList[I].Text;
        Grid.Cells[3, RowIndex] := FTaskList[I].Note;
        if FTaskList[I].Amount <> 0 then
        begin
          Grid.Cells[4, RowIndex] := FTaskList[I].AmountStr;
        end
        else
          Grid.Cells[4, RowIndex] := string.Empty;

        if FTaskList[I].Date > 0 then
        begin
          if (DisplayTime) then
            Grid.Cells[5, RowIndex] := FTaskList[I].DateTimeStr
          else
            Grid.Cells[5, RowIndex] := FTaskList[I].DateStr;
        end
        else
          Grid.Cells[5, RowIndex] := string.Empty;
        Grid.Cells[6, RowIndex] := IntToStr(Ord(FTaskList[I].Star));

        FMapGrid[RowIndex] := I;

        if SortOrder = soAscending then
          RowIndex += 1
        else
          RowIndex -= 1;
      end;
    end;

    // Total duration calculation
    Grid.Cells[0, 0] := string.Empty;
    if (Length(StartDates) > 0) and (Length(StartDates) = Length(EndDates)) then
    begin
      DateDiff := CalcDateDiff(0, CalculateTotalDuration);
      if (DateDiff <> '-') and (DateDiff <> '0' + rseconds) then
        Grid.Cells[0, 0] := DateDiff;
    end;

    // Perform sorting if SortColumn is between 1 and 4
    if (SortColumn >= 1) and (SortColumn <= 6) then
    begin
      // Bubble Sort logic to sort rows based on CompareTasks function
      for I := 1 to Grid.RowCount - 2 do
        for J := I + 1 to Grid.RowCount - 1 do
          if CompareTasks(I, J) > 0 then
            SwapRows(I, J);
    end;

  finally
    if (Assigned(eventOnColRowInserted)) then
      Grid.OnColRowInserted := eventOnColRowInserted;
    if (Assigned(eventOnColRowDeleted)) then
      Grid.OnColRowDeleted := eventOnColRowDeleted;
    Grid.EndUpdate;
  end;
end;

end.
