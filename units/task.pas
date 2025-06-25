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
  Grids,
  Clipbrd,
  StrUtils,
  DateUtils;

type
  TIntegerArray = array of integer;

  // Class representing a single task
  TTask = class
  private
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
    FDateStart, FDateEnd: TDateTime; // Calculated time interval
    function GetDate: string;
    function GetDateISO: string;
    function GetAmount: string;
  public
    constructor Create;
    constructor Create(const TaskString: string); // Constructor that takes a task string
    procedure Copy(Original: TTask);
    property Done: boolean read FDone write FDone;
    property Archive: boolean read FArchive write FArchive;
    property Date: TDateTime read FDate write FDate;
    property DateStrISO: string read GetDateISO;
    property DateStr: string read GetDate;
    property Note: string read FNote write FNote;
    property Text: string read FText write FText;
    property Amount: double read FAmount write FAmount;
    property AmountStr: string read GetAmount;
    property Star: boolean read FStar write FStar;
    property NoteItalic: boolean read FNoteItalic write FNoteItalic;
    property EmptyNote: boolean read FEmptyNote write FEmptyNote;
    property SpaceBeforeNote: boolean read FSpaceBeforeNote write FSpaceBeforeNote;
    property SpaceAfterNote: boolean read FSpaceAfterNote write FSpaceAfterNote;
    function ToString(Col: integer = 0; AddEmptyCompletion: boolean = True): string; reintroduce;
  end;

  // Class representing a collection of tasks
  TTasks = class
  private
    FGroupList: array of array of TTask; // Array of task groups
    FGroupNameList: TStringList; // List of group names
    FTaskList: array of TTask; // Array of tasks
    FMapGrid: TIntegerArray; // Maps grid rows to tasks
    FBackupTaskList: array of TTask; // Backup array of tasks
    FInitGroupList: array of array of TTask; // Initial array of task groups
    FInitGroupNameList: TStringList; // List of group names
    FSelectedGroup: integer;

    function GetGroupCount: integer;
    function GetCount: integer;
    function GetCountArchive: integer;
  public
    constructor Create(const TaskStrings: TStringList = nil); // Constructor that takes a StringList
    destructor Destroy; override; // Destructor
    procedure AddGroup(const GroupName: string; const TaskStrings: TStringList = nil); // Add new group from a StringList
    procedure UpdateGroup;
    procedure ChangeGroup(GroupIndex: integer; UpdateCurrent: boolean = False);
    function ToStringList: TStringList;
    procedure InitMap(Length: integer);
    function Map(Index: integer): integer;
    function ReverseMap(Value: integer): integer;
    procedure AddMap(Value: integer);
    function AddTask(const TaskString: string): integer; // Method to add a task
    function AddGroupTask(const GroupIndex: integer; const TaskString: string): integer;
    function GetTask(Index: integer): TTask; // Method to get a task by row index
    function GetTaskValue(ACol, ARow: integer): string; // Method to get a task value by row col
    function HasTask(Index: integer): boolean;
    procedure SetTask(Grid: TStringGrid; Row: integer; Backup: boolean = True);
    function InsertTask(const TaskString: string; Index: integer; Backup: boolean = True): integer;
    procedure DeleteTask(Index: integer);
    procedure ArchiveTask(Index: integer);
    procedure CompleteTask(Index: integer; Backup: boolean = True);
    procedure StarTask(Index: integer; Backup: boolean = True);
    procedure ClearTasksInRect(Grid: TStringGrid; Rect: TGridRect);
    function InsertGroup(aName: string): integer;
    function RenameGroup(aIndex: integer; aName: string): boolean;
    function CopyGroup(aIndex: integer; aName: string): boolean;
    function DeleteGroup(aIndex: integer): boolean;
    function MoveGroupLeft(Index: integer): integer;
    function MoveGroupRight(Index: integer): integer;
    function MoveGroupTasks(Index1, Index2, NewGroup: integer): integer;
    function MoveTasksTop(Index1, Index2: integer): integer;
    function MoveTasksBottom(Index1, Index2: integer): integer;
    function MoveTasksUp(Index1, Index2: integer): integer;
    function MoveTasksDown(Index1, Index2: integer): integer;
    procedure SwapTasks(OldIndex, NewIndex: integer);
    procedure MoveTask(OldIndex, NewIndex: integer);
    procedure CopyToClipboard(Grid: TStringGrid);
    function PasteFromClipboard(Grid: TStringGrid): TGridRect;
    procedure CreateBackup;
    procedure UndoBackup;
    procedure CreateBackupInit;
    procedure UndoBackupInit;
    function CalcDateDiff(const StartDate, EndDate: TDateTime): string;
    function CalcDateDiffAccuracy(const StartDate, EndDate: TDateTime): string;
    function CalcCount(Archive, Done: boolean; StartIndex: integer = 0; EndIndex: integer = 0): integer;
    function CalcSum(Archive, Done: boolean; StartIndex: integer = 0; EndIndex: integer = 0): double;
    function CalcDuration(Archive, Done: boolean; StartIndex: integer = 0; EndIndex: integer = 0): string;

    procedure FillGrid(Grid: TStringGrid; ShowArchive, ShowDuration: boolean; SortOrder: TSortOrder; SortColumn: integer);

    property GroupNames: TStringList read FGroupNameList;
    property SelectedGroup: integer read FSelectedGroup;
    property CountGroup: integer read GetGroupCount;
    property Count: integer read GetCount;
    property CountArhive: integer read GetCountArchive;
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

uses stringtool;

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
end;

constructor TTask.Create(const TaskString: string);
var
  PartNote, PartDate: TStringArray; // Use TStringArray for compatibility
  CompletedStr: string;

  procedure FillNote(start: integer = 1);
  var
    i: integer;
  begin
    FNote := string.Empty;
    for i := start to High(PartNote) do
    begin
      FNote += PartNote[i];
      if (i < High(PartNote)) then FNote += '//';
    end;
  end;

  procedure FillCompletedStr(start: integer = 0);
  var
    i: integer;
  begin
    CompletedStr := string.Empty;
    for i := start to High(PartNote) do
    begin
      CompletedStr += PartNote[i];
      if (i < High(PartNote)) then CompletedStr += '//';
    end;
  end;

  procedure FillText(start: integer = 1);
  var
    i: integer;
  begin
    FText := string.Empty;
    for i := start to High(PartDate) do
    begin
      FText += PartDate[i];
      if (i < High(PartDate)) then FText += ',';
    end;
  end;

begin
  // Format: - [x] 01.01.2000, 123, ~~**Task**~~ // Note
  FSpaceBeforeNote := True;
  FSpaceAfterNote := True;
  FNoteItalic := False;

  // Split the task string into PartNote
  PartNote := TaskString.Split(['//']);
  if (Length(PartNote) >= 2) and (not PartNote[0].EndsWith(':')) then // Url protection
  begin
    CompletedStr := PartNote[0];
    if (Length(CompletedStr) > 0) and (CompletedStr.EndsWith(' ')) then
    begin
      Delete(CompletedStr, Length(CompletedStr), 1);
      SpaceBeforeNote := True;
    end
    else
      SpaceBeforeNote := False;

    if (Length(PartNote[1]) > 0) and (PartNote[1].StartsWith(' ')) then
    begin
      Delete(PartNote[1], 0, 1);
      SpaceAfterNote := True;
    end
    else
      SpaceAfterNote := False;

    FillNote;

    // Test for empty Note symbols
    if (FNote.Trim = string.empty) then
      EmptyNote := True;
  end
  else
    FillCompletedStr;

  // Remove star in start and end of Note
  if (FNote.TrimLeft.StartsWith('*')) and (FNote.TrimRight.EndsWith('*')) then
  begin
    FNoteItalic := True;
    if (Length(FNote) > 0) and (FNote.StartsWith(' ')) then
    begin
      FNote := TrimLeft(FNote);
      SpaceAfterNote := True;
    end;
    Delete(FNote, 1, 1);
    if (TrimRight(FNote).EndsWith('*')) then
    begin
      FNote := TrimRight(FNote);
      Delete(FNote, Length(FNote), 1);
    end;
  end
  else
  begin
    if (Length(FNote) > 0) and (FNote.StartsWith(' ')) then
    begin
      SpaceAfterNote := True;
      Delete(FNote, 1, 1);
    end;
  end;

  PartDate := CompletedStr.Split([',']);

  // Check completion status based on the first character in the string
  FDone := DetectDone(PartDate[0]);
  PartDate[0] := RemoveBrackets(PartDate[0]);

  // Checks if the task is completed
  CompletedStr := RemoveBrackets(CompletedStr);

  if (TryStrToFloat(CleanAmount(CompletedStr), FAmount)) then
  else
  if (TryStrToDateTimeISO(CompletedStr, FDate)) then
  else
  if Length(PartDate) > 2 then
  begin
    // Extract and trim the date string
    if (TryStrToDateTimeISO(PartDate[0].Trim, FDate)) then
    begin
      // Extract and trim the amount string
      if (TryStrToFloat(PartDate[1].Trim, FAmount)) then
        FillText(2)
      else
      begin
        FillText(1);
        FAmount := 0;
      end;
      if (Length(FText) > 0) and (FText.StartsWith(' ')) then
        Delete(FText, 1, 1); // Delete space in begining
    end
    else
    // Extract and trim the amount string
    if (TryStrToFloat(PartDate[0].Trim, FAmount)) then
    begin
      FillText(1);
      if (Length(FText) > 0) and (FText.StartsWith(' ')) then
        Delete(FText, 1, 1); // Delete space in begining
    end
    else
    begin
      FText := CompletedStr;
      FDate := 0;
      FAmount := 0;
    end;
  end
  else
  if Length(PartDate) > 1 then
  begin
    // Extract and trim the amount string
    if (TryStrToFloat(PartDate[0].Trim, FAmount)) then
    begin
      FillText(1);
      if (Length(FText) > 0) and (FText.StartsWith(' ')) then
        Delete(FText, 1, 1); // Delete space in begining
    end
    else
    begin
      // Extract and trim the date string
      if (TryStrToDateTimeISO(PartDate[0].Trim, FDate)) then
      begin
        FillText(1);
        if (Length(FText) > 0) and (FText.StartsWith(' ')) then
          Delete(FText, 1, 1); // Delete space in begining
      end
      else
      begin
        FText := CompletedStr;
        FDate := 0;
      end;
      FAmount := 0;
    end;
  end
  else
  begin
    FText := CompletedStr;
    FDate := 0;
    FAmount := 0;
  end;

  FText := StringReplace(FText, '<br>', sLineBreak, [rfReplaceAll]);
  FNote := StringReplace(FNote, '<br>', sLineBreak, [rfReplaceAll]);

  // Check if Text starts and ends with '~~'
  if FText.TrimLeft.StartsWith('~~') and FText.TrimRight.EndsWith('~~') then
  begin
    FArchive := True;
    FText := Trim(FText);
    // Remove '~~' from the start and end of the Text
    FText := FText.Substring(2, Length(FText) - 4);
  end
  else
    FArchive := False;

  // Check if Text starts and ends with '**'
  if FText.TrimLeft.StartsWith('**') and FText.TrimRight.EndsWith('**') then
  begin
    FStar := True;
    FText := Trim(FText);
    // Remove '**' from the start and end of the Text
    FText := FText.Substring(2, Length(FText) - 4);
  end
  else
    FStar := False;
end;

function TTask.ToString(Col: integer = 0; AddEmptyCompletion: boolean = True): string;
var
  TextString: string;
  DoneString: string;
  NoteString: string;
begin
  // Replace line breaks from task description and Note
  TextString := StringReplace(Text, sLineBreak, '<br>', [rfReplaceAll]);
  TextString := StringReplace(TextString, #13, '<br>', [rfReplaceAll]);
  TextString := StringReplace(TextString, #10, '<br>', [rfReplaceAll]);
  NoteString := StringReplace(Note, sLineBreak, '<br>', [rfReplaceAll]);
  NoteString := StringReplace(NoteString, #13, '<br>', [rfReplaceAll]);
  NoteString := StringReplace(NoteString, #10, '<br>', [rfReplaceAll]);

  // Add '**' for starred tasks
  if FStar then
    TextString := '**' + TextString + '**';

  // Add '~~' for archived tasks
  if FArchive then
    TextString := '~~' + TextString + '~~';

  // Check completion
  if Done then
    DoneString := '- [x]'
  else if AddEmptyCompletion then
    DoneString := '- [ ]'
  else
    DoneString := string.Empty;

  // Check notes
  if (NoteString <> string.Empty) or (EmptyNote) then
  begin
    if (NoteString <> string.Empty) and (NoteItalic) then
      NoteString := '*' + NoteString + '*';

    if (SpaceAfterNote) then NoteString := ' ' + NoteString;
    NoteString := '//' + NoteString;
    if (SpaceBeforeNote) then NoteString := ' ' + NoteString;
  end
  else
    NoteString := string.Empty;

  // Form the task string based on the provided Col
  case Col of
    1: Result := DoneString; // Returning only the completion status
    2: Result := TextString; // Returning only the task string
    3: Result := NoteString; // Returning only the Note
    4:
      if Amount <> 0 then
        Result := FloatToString(Amount)
      else
        Result := string.Empty;
    5:
      if FDate > 0 then
        Result := DateStrISO.Trim
      else
        Result := string.Empty; // If the completion date is missing, return an empty string
    else
      // Forming the task string considering the completion date and Note
      if (DoneString = string.Empty) then
      begin
        if Amount <> 0 then
        begin
          if Date > 0 then
            Result := Format('%s, %s, %s%s', [DateStrISO, AmountStr, TextString, NoteString])
          else
          begin
            if (TextString + NoteString <> string.Empty) then
              Result := Format('%s, %s%s', [AmountStr, TextString, NoteString])
            else
              Result := Format('%s', [AmountStr]);
          end;
        end
        else
        begin
          if Date > 0 then
          begin
            if (TextString + NoteString <> string.Empty) then
              Result := Format('%s, %s%s', [DateStrISO, TextString, NoteString])
            else
              Result := Format('%s', [DateStrISO]);
          end
          else
            Result := Format('%s%s', [TextString, NoteString]);
        end;
      end
      else
      begin
        if Amount <> 0 then
        begin
          if Date > 0 then
            Result := Format('%s %s, %s, %s%s', [DoneString, DateStrISO, AmountStr, TextString, NoteString]).Trim
          else
          begin
            if (TextString + NoteString <> string.Empty) then
              Result := Format('%s %s, %s%s', [DoneString, AmountStr, TextString, NoteString]).Trim
            else
              Result := Format('%s %s', [DoneString, AmountStr]).Trim;
          end;
        end
        else
        begin
          if Date > 0 then
          begin
            if (TextString + NoteString <> string.Empty) then
              Result := Format('%s %s, %s%s', [DoneString, DateStrISO, TextString, NoteString]).Trim
            else
              Result := Format('%s %s', [DoneString, DateStrISO]).Trim;
          end
          else
            Result := Format('%s %s%s', [DoneString, TextString, NoteString]).Trim;
        end;
      end;
  end;
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
end;

function TTask.GetDate: string;
begin
  Result := DateTimeToString(FDate);
end;

function TTask.GetDateISO: string;
begin
  Result := DateTimeToStringISO(FDate);
end;

function TTask.GetAmount: string;
begin
  Result := FloatToString(FAmount);
end;

{ TTasks }

constructor TTasks.Create(const TaskStrings: TStringList = nil);
var
  i: integer; // Index for iteration
  Tab: TStringList;
  TabName, Value: string;
begin
  SetLength(FGroupList, 0); // Initialize group array
  FSelectedGroup := -1;
  FGroupNameList := TStringList.Create;
  try
    // Iterate through the StringList to create tasks
    if (Assigned(TaskStrings)) then
    begin
      Tab := TStringList.Create;
      TabName := string.Empty;

      for i := 0 to TaskStrings.Count - 1 do
      begin
        Value := TaskStrings[i];

        if (Value.TrimLeft.StartsWith('#')) then
        begin
          if (Tab.Count > 0) or (TabName <> string.Empty) then
          begin
            AddGroup(TabName, Tab);
            Tab.Clear;
          end;

          TabName := Value;
          Continue;
        end;

        Tab.Add(Value);
      end;

      // Add last group
      AddGroup(TabName, Tab);

      TaskStrings.Free;
    end;
  finally
    Tab.Free;
  end;

  CreateBackupInit;
  ChangeGroup(0, True);
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

  if Assigned(FInitGroupNameList) then
    FInitGroupNameList.Free;

  SetLength(FMapGrid, 0);
  FMapGrid := nil;

  inherited;
end;

procedure TTasks.AddGroup(const GroupName: string; const TaskStrings: TStringList = nil); // Add new group from a StringList
var
  i: integer; // Index for iteration
begin
  SetLength(FGroupList, CountGroup + 1);
  FGroupNameList.Add(GroupName);

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
  if (UpdateCurrent) then UpdateGroup;

  SetLength(FMapGrid, 0); // Initialize task map
  FSelectedGroup := GroupIndex;
  FTaskList := FGroupList[GroupIndex];
  InitMap(Count + 1);
end;

function TTasks.ToStringList: TStringList;
var
  i, j: integer;
  addCompleted: boolean;
begin
  Result := TStringList.Create;
  try
    UpdateGroup;
    addCompleted := False;
    for i := 0 to CountGroup - 1 do
      for j := 0 to Length(FGroupList[i]) - 1 do
        if FGroupList[i, j].Done then addCompleted := True;

    for i := 0 to CountGroup - 1 do
    begin
      if (FGroupNameList[i] <> string.Empty) then
        Result.Add(FGroupNameList[i]);

      for j := 0 to Length(FGroupList[i]) - 1 do
      begin
        Result.Add(FGroupList[i, j].ToString(0, addCompleted)); // Add task string to TStringList
      end;
    end;
  except
    Result.Free;
    raise;
  end;
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
  if ACol = 5 then Result := GetTask(aRow).DateStrISO
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

procedure TTasks.SetTask(Grid: TStringGrid; Row: integer; Backup: boolean = True);
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
    if not TryStrToFloat(Grid.Cells[4, Row], pAmount) then
    begin
      pAmount := 0; // If parsing the amount failed, set to 0
      Grid.Cells[4, Row] := '';
    end
    else
      Grid.Cells[4, Row] := FloatToString(pAmount);
    Task.Amount := pAmount;
    if not TryStrToDateTime(Grid.Cells[5, Row], pDate) then
    begin
      pDate := 0; // If parsing the date failed, set to 0
      Grid.Cells[5, Row] := '';
    end
    else
      Grid.Cells[5, Row] := DateTimeToString(pDate);
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
  begin
    FTaskList[i] := FTaskList[i - 1]; // Move tasks one position down
  end;

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

  // Check if the selected group is within bounds and shift groups to the right
  if (FSelectedGroup >= 0) and (FSelectedGroup < CountGroup - 2) then
  begin
    // Shift existing groups to the right to make space for the new group
    for i := CountGroup - 1 downto FSelectedGroup + 1 do
    begin
      FGroupList[i] := FGroupList[i - 1];
      FGroupNameList[i] := FGroupNameList[i - 1];
    end;
  end;

  // Increment the selected group index and set the name of the new group
  Inc(FSelectedGroup);
  SetLength(FGroupList[FSelectedGroup], 0);
  FGroupNameList[FSelectedGroup] := IfThen(aName = string.Empty, aName, '## ' + aName);

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

function TTasks.CopyGroup(aIndex: integer; aName: string): boolean;
var
  i: integer;
begin
  Result := False;
  if (aIndex < 0) or (aIndex >= CountGroup) then exit;

  FGroupNameList.Add(string.Empty);
  SetLength(FGroupList, CountGroup + 1);

  // Shift groups to the left, overwriting the group at aIndex
  for i := CountGroup - 2 downto aIndex + 1 do
  begin
    FGroupList[i + 1] := FGroupList[i];
    FGroupNameList[i + 1] := FGroupNameList[i];
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
  end;

  // Decrease the size of the group array and name list
  FGroupNameList.Delete(CountGroup - 1);
  SetLength(FGroupList, CountGroup - 1);

  // If groups don't exists then add new
  if (CountGroup = 0) then
  begin
    SetLength(FGroupList, CountGroup + 1);
    FGroupNameList.Add(string.Empty);
  end;

  // Update the selected group if needed
  if FSelectedGroup >= CountGroup then
    FSelectedGroup := CountGroup - 1;

  ChangeGroup(FSelectedGroup);

  Result := True;
end;

function TTasks.MoveGroupLeft(Index: integer): integer;
var
  tempGroup: array of TTask;
  tempName: string;
begin
  Result := Index;
  if (Index < 1) then exit;

  tempGroup := FGroupList[Index - 1];
  FGroupList[Index - 1] := FGroupList[Index];
  FGroupList[Index] := tempGroup;
  SetLength(tempGroup, 0);
  tempGroup := nil;

  tempName := FGroupNameList[Index - 1];
  FGroupNameList[Index - 1] := FGroupNameList[Index];
  FGroupNameList[Index] := tempName;
  Result := Index - 1;
  FSelectedGroup := Result;
end;

function TTasks.MoveGroupRight(Index: integer): integer;
var
  tempGroup: array of TTask;
  tempName: string;
begin
  Result := Index;
  if (Index >= CountGroup - 1) then exit;

  tempGroup := FGroupList[Index + 1];
  FGroupList[Index + 1] := FGroupList[Index];
  FGroupList[Index] := tempGroup;
  SetLength(tempGroup, 0);
  tempGroup := nil;

  tempName := FGroupNameList[Index + 1];
  FGroupNameList[Index + 1] := FGroupNameList[Index];
  FGroupNameList[Index] := tempName;
  Result := Index + 1;
  FSelectedGroup := Result;
end;

function TTasks.MoveGroupTasks(Index1, Index2, NewGroup: integer): integer;
var
  i, Len, Ind, IndStart, IndEnd, LastTask: integer;
begin
  Result := -1;

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

function TTasks.MoveTasksTop(Index1, Index2: integer): integer;
var
  TempTasks: array of TTask; // Temporary array for selected tasks
  i, IndStart, IndEnd, Len: integer;
begin
  Result := -1;
  IndStart := Map(Index1);
  IndEnd := Map(Index2);
  Len := IndEnd - IndStart + 1;

  // Check if the Index1 is valid and not already at the top
  if (IndStart > 0) and (IndEnd <= Count) then
  begin
    CreateBackup;

    // Save selected tasks to temporary array
    TempTasks := [];
    SetLength(TempTasks, Len);
    for i := IndStart to IndEnd do
      TempTasks[i - IndStart] := FTaskList[i];

    // Shift tasks down to make room at the top
    for i := IndEnd downto Len do
      FTaskList[i] := FTaskList[i - Len];

    // Place saved tasks at the top in the original order
    for i := 0 to High(TempTasks) do
      FTaskList[i] := TempTasks[i];

    SetLength(TempTasks, 0);
    TempTasks := nil;
    Result := 1;
  end;
end;

function TTasks.MoveTasksBottom(Index1, Index2: integer): integer;
var
  TempTasks: array of TTask; // Temporary array for selected tasks
  i, IndStart, IndEnd, Len: integer;
begin
  Result := -1;
  IndStart := Map(Index1);
  IndEnd := Map(Index2);
  Len := IndEnd - IndStart + 1;

  // Check if the Index1 is valid and not already at the bottom
  if (IndStart >= 0) and (IndEnd < Count) then
  begin
    CreateBackup;

    // Save selected tasks to temporary array
    TempTasks := [];
    SetLength(TempTasks, Len);
    for i := IndStart to IndEnd do
      TempTasks[i - IndStart] := FTaskList[i];

    // Shift all tasks down to fill the gap
    for i := IndStart to Count - 1 - Len do
      FTaskList[i] := FTaskList[i + Len];

    // Place the stored task at the end
    for i := 0 to High(TempTasks) do
      FTaskList[Count - len + i] := TempTasks[i];

    Result := Length(MapGrid) - 1; //ReverseMap(FCount);
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

procedure TTasks.CopyToClipboard(Grid: TStringGrid);
var
  SelectedText: TStringList;
  Rect: TGridRect;
  pDone, pText, pNote, pAmount, pDate: string;
  Row1, Row2, RowText: string;
  i, j: integer;
begin
  SelectedText := TStringList.Create;
  try
    Rect := Grid.Selection;     // Get grid selection rect

    for i := Rect.Top to Rect.Bottom do
    begin
      for j := Rect.Left to Rect.Right do
      begin
        if (j = 1) then pDone := GetTask(i).ToString(j).Trim;
        if (j = 2) then pText := GetTask(i).ToString(j).Trim;
        if (j = 3) then pNote := GetTask(i).ToString(j).Trim;
        if (j = 4) then pAmount := GetTask(i).ToString(j).Trim;
        if (j = 5) then pDate := GetTask(i).ToString(j).Trim;
      end;
      Row1 := (pDone + ' ' + pDate).Trim;
      Row2 := (pText + ' ' + pNote).Trim;

      if (pDate <> string.Empty) and (Row2 <> string.Empty) then
        Row1 += ', '
      else
      if (Row1 <> string.Empty) and (Row2 <> string.Empty) then
        Row1 += ' ';

      if (pAmount <> string.Empty) then
        Row1 += pAmount + ', ';

      RowText := Row1 + Row2;

      if (Grid.Selection.Width = 0) and (Grid.Selection.Height = 0) then
        RowText := StringReplace(RowText, '<br>', sLineBreak, [rfReplaceAll]);

      SelectedText.Add(RowText.Trim);
    end;

    // Copy to clipboard
    if SelectedText.Count > 0 then
    begin
      Clipboard.AsText := SelectedText.Text;
    end;
  finally
    SelectedText.Free;
  end;
end;

function TTasks.PasteFromClipboard(Grid: TStringGrid): TGridRect;
var
  TempTasks: TTasks;
  TempDate: TDateTime;
  TempAmount: double;
  Rect: TGridRect;
  Index, i, j: integer;
  RowTask: TTask;
  IsRowEmpty, DoInsert: boolean;
begin
  Result:= Grid.Selection;
  DoInsert := True;
  if Clipboard.AsText = string.Empty then exit;
  CreateBackup;

  if (Grid.Row > 0) then
  begin
    RowTask := GetTask(Grid.Row);
    IsRowEmpty := (RowTask.Text = string.Empty) and (RowTask.Note = string.Empty) and (RowTask.Amount = 0) and (RowTask.Date = 0);
  end
  else
    IsRowEmpty := False;

  TempTasks := TTasks.Create(TextToStringList(Clipboard.AsText, True));
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
      for i := TempTasks.Count - 1 downto 0 do
        InsertTask(TempTasks.GetTask(i + 1).ToString(), index, False);
    end
    else
    begin
      index := 1;

      // If the line is empty and one line is selected, then we start with it
      if (IsRowEmpty) and (Grid.Selection.Height = 0) then
      begin
        GetTask(Grid.Row).Copy(TempTasks.GetTask(index));
        Inc(index);
      end
      else
      begin
        Rect := Grid.Selection;

        // If the selection has height, then do not insert records below
        if (Grid.Selection.Height > 0) then
        begin
          DoInsert := False;
        end;

        for i := Rect.Top to Rect.Bottom do
        begin
          if (index > TempTasks.Count) then
          begin
            Index := 1;
            DoInsert := False;
          end;
          for j := Rect.Left to Rect.Right do
          begin
            if j = 1 then
              GetTask(i).Done := TempTasks.GetTask(index).Done
            else
            if j = 2 then
            begin
              if (TempTasks.GetTask(index).Text <> string.Empty) then
              begin
                GetTask(i).Text := CleanString(TempTasks.GetTask(index).Text);
                GetTask(i).Archive := TempTasks.GetTask(index).Archive;
                GetTask(i).Star := TempTasks.GetTask(index).Star;
              end
              else
              if Rect.Width = 0 then
              begin
                if (TempTasks.GetTask(index).Note <> string.Empty) then
                  GetTask(i).Text := CleanString(TempTasks.GetTask(index).Note)
                else
                if (TempTasks.GetTask(index).Date <> 0) then
                  GetTask(i).Text := DateTimeToStringISO(TempTasks.GetTask(index).Date)
                else
                if (TempTasks.GetTask(index).Amount <> 0) then
                  GetTask(i).Text := FloatToString(TempTasks.GetTask(index).Amount)
                else
                  GetTask(i).Text := string.Empty;
              end
              else
                GetTask(i).Text := string.Empty;
            end
            else
            if j = 3 then
            begin
              if (TempTasks.GetTask(index).Note <> string.Empty) then
              begin
                GetTask(i).Note := CleanString(TempTasks.GetTask(index).Note);
                GetTask(i).NoteItalic := TempTasks.GetTask(index).NoteItalic;
              end
              else
              if Rect.Width = 0 then
              begin
                if (TempTasks.GetTask(index).Text <> string.Empty) then
                  GetTask(i).Note := CleanString(TempTasks.GetTask(index).Text)
                else
                if (TempTasks.GetTask(index).Date <> 0) then
                  GetTask(i).Note := DateTimeToStringISO(TempTasks.GetTask(index).Date)
                else
                if (TempTasks.GetTask(index).Amount <> 0) then
                  GetTask(i).Note := FloatToString(TempTasks.GetTask(index).Amount)
                else
                  GetTask(i).Note := string.Empty;
              end
              else
                GetTask(i).Note := string.Empty;
            end
            else
            if j = 4 then
            begin
              if (TempTasks.GetTask(index).Amount <> 0) then
                GetTask(i).Amount := TempTasks.GetTask(index).Amount
              else
              if Rect.Width = 0 then
              begin
                if (TempTasks.GetTask(index).Text <> string.Empty) and
                  (TryStrToFloat(CleanAmount(TempTasks.GetTask(index).Text), TempAmount)) then
                  GetTask(i).Amount := TempAmount
                else
                if (TempTasks.GetTask(index).Note <> string.Empty) and
                  (TryStrToFloat(CleanAmount(TempTasks.GetTask(index).Note), TempAmount)) then
                  GetTask(i).Amount := TempAmount
                else
                  GetTask(i).Amount := 0;
              end
              else
                GetTask(i).Amount := 0;
            end
            else
            if j = 5 then
            begin
              if (TempTasks.GetTask(index).Date <> 0) then
                GetTask(i).Date := TempTasks.GetTask(index).Date
              else
              if Rect.Width = 0 then
              begin
                if (TempTasks.GetTask(index).Text <> string.Empty) and (TryStrToDateTimeISO(TempTasks.GetTask(index).Text, TempDate)) then
                  GetTask(i).Date := TempDate
                else
                if (TempTasks.GetTask(index).Note <> string.Empty) and
                  (TryStrToDateTimeISO(TempTasks.GetTask(index).Note, TempDate)) then
                  GetTask(i).Date := TempDate
                else
                  GetTask(i).Date := 0;
              end
              else
                GetTask(i).Date := 0;
            end
            else
            if j = 6 then
            begin
              GetTask(i).Star := TempTasks.GetTask(index).Star;
            end;
          end;
          Inc(index);
        end;
      end;

      // Insert if clipboard is bigger than selection
      if (DoInsert) and (index - 1 < TempTasks.Count) then
      begin
        for i := TempTasks.Count - 1 downto index - 1 do
        begin
          InsertTask(TempTasks.GetTask(i + 1).ToString(), Grid.Row, False);
        end;
        Result := TGridRect.Create(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom + (TempTasks.Count - Index) + 1);
      end;
    end;
  finally
    TempTasks.Free; // Free the temporary TTasks object
  end;
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
  if Assigned(FInitGroupNameList) then
  begin
    FGroupNameList := TStringList.Create;
    FGroupNameList.Assign(FInitGroupNameList);
  end;

  ChangeGroup(SelectedGroup);
end;

function TTasks.CalcDateDiff(const StartDate, EndDate: TDateTime): string;
var
  YearsDiff, MonthsDiff, DaysDiff, HoursDiff, MinutesDiff, SecondsDiff: integer;
begin
  // Calculate difference in yeard
  YearsDiff := YearsBetween(EndDate, StartDate, True);
  if YearsDiff <> 0 then
  begin
    if (StartDate > EndDate) then
      Result := IntToStr(YearsDiff) + ryears
    else
      Result := '-';
    Exit;
  end;

  // Calculate difference in months
  MonthsDiff := MonthsBetween(EndDate, StartDate);
  if MonthsDiff <> 0 then
  begin
    if (StartDate > EndDate) then
      Result := IntToStr(MonthsDiff) + rmonths
    else
      Result := '-';
    Exit;
  end;

  // Calculate difference in days
  DaysDiff := DaysBetween(EndDate, StartDate);
  if DaysDiff <> 0 then
  begin
    if (StartDate > EndDate) then
      Result := IntToStr(DaysDiff) + rdays
    else
      Result := '-';
    Exit;
  end;

  // Calculate difference in hours
  HoursDiff := HoursBetween(EndDate, StartDate);
  if HoursDiff <> 0 then
  begin
    if (StartDate > EndDate) then
      Result := IntToStr(HoursDiff) + rhours
    else
      Result := '-';
    Exit;
  end;

  // Calculate difference in minutes
  MinutesDiff := MinutesBetween(EndDate, StartDate);
  if MinutesDiff <> 0 then
  begin
    if (StartDate > EndDate) then
      Result := IntToStr(MinutesDiff) + rminutes
    else
      Result := '-';
    Exit;
  end;

  // Calculate difference in seconds
  SecondsDiff := SecondsBetween(EndDate, StartDate);
  if SecondsDiff <> 0 then
  begin
    if (StartDate > EndDate) then
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

function TTasks.CalcCount(Archive, Done: boolean; StartIndex: integer = 0; EndIndex: integer = 0): integer;
var
  I, Ind: integer;
begin
  Result := 0;
  if (StartIndex = 0) and (EndIndex = 0) then
  begin
    for I := 0 to Count - 1 do
    begin
      if ((Archive = True) or (FTaskList[I].Archive = False)) and ((Done = False) or (FTaskList[I].Done = True)) then
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

function TTasks.CalcSum(Archive, Done: boolean; StartIndex: integer = 0; EndIndex: integer = 0): double;
var
  I, Ind: integer;
begin
  Result := 0;
  if (StartIndex = 0) and (EndIndex = 0) then
  begin
    for I := 0 to Count - 1 do
    begin
      if ((Archive = True) or (FTaskList[I].Archive = False)) and ((Done = False) or (FTaskList[I].Done = True)) then
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

function TTasks.CalcDuration(Archive, Done: boolean; StartIndex: integer = 0; EndIndex: integer = 0): string;
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
      if ((Archive = True) or (FTaskList[I].Archive = False)) and ((Done = False) or (FTaskList[I].Done = True)) then
        TotalDuration += (FTaskList[I].FDateEnd - FTaskList[I].FDateStart);
    end;
  end
  else
  begin
    for I := StartIndex to EndIndex do
    begin
      Ind := Map(I);
      if (Ind > -1) and ((Archive = True) or (FTaskList[Ind].Archive = False)) and ((Done = False) or (FTaskList[Ind].Done = True)) then
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

function TTasks.GetCountArchive: integer;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if FTaskList[I].Archive then Result += 1;
end;

procedure TTasks.FillGrid(Grid: TStringGrid; ShowArchive, ShowDuration: boolean; SortOrder: TSortOrder; SortColumn: integer);
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

    // Проходим по каждому интервалу
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
    LastDate := Now;
    MinDate := Now;
    MaxDate := 0;
    StartDate := 0;
    EndDate := 0;
    SetLength(StartDates, 0);
    SetLength(EndDates, 0);

    // Archive tasks count
    ArhCount := GetCountArchive;

    if (ShowArchive) then
      Grid.RowCount := Count + 1
    else
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
            StartDate := FTaskList[I].Date;
            EndDate := LastDate;
            DateDiff := CalcDateDiff(StartDate, EndDate);

            // If the date difference is invalid, recalculate with the minimum date
            if (DateDiff = '-') and (MinDate > 0) and (LastDate <> Now) then
            begin
              StartDate := FTaskList[I].Date;
              EndDate := MinDate;
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
      if (ShowArchive = True) or (FTaskList[I].Archive = False) then
      begin
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
