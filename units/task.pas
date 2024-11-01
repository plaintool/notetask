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
  DateUtils;

type
  TIntegerArray = array of integer;

  // Class representing a single task
  TTask = class
  private
    FDone: boolean; // Status of task completion
    FArchive: boolean; // Archive task
    FDate: TDateTime; // Date of task completion
    FComment: string; // Comment for the task
    FText: string; // Description of the task
    FCommentItalic: boolean; // Comment is italic
    FEmptyComment: boolean; // Has empty comment symbols
    FSpaceBeforeComment: boolean; // Space before comment in file
    FSpaceAfterComment: boolean; // Space after comment in file
  public
    constructor Create;
    constructor Create(const TaskString: string); // Constructor that takes a task string
    procedure Copy(Original: TTask);
    property Done: boolean read FDone write FDone;
    property Archive: boolean read FArchive write FArchive;
    property Date: TDateTime read FDate write FDate;
    property Comment: string read FComment write FComment;
    property Text: string read FText write FText;
    property CommentItalic: boolean read FCommentItalic write FCommentItalic;
    property EmptyComment: boolean read FEmptyComment write FEmptyComment;
    property SpaceBeforeComment: boolean read FSpaceBeforeComment write FSpaceBeforeComment;
    property SpaceAfterComment: boolean read FSpaceAfterComment write FSpaceAfterComment;
    function ToString(Col: integer = 0; AddEmptyCompletion: boolean = True): string; reintroduce;
  end;

  // Class representing a collection of tasks
  TTasks = class
  private
    FTaskList: array of TTask; // Array of tasks
    FBackupTaskList: array of TTask; // Array of tasks
    FInitTaskList: array of TTask; // Array of tasks
    FMapGrid: TIntegerArray; // Map grid rows to tasks
    FCount: integer; // Current count of tasks
  public
    constructor Create(const TaskStrings: TStringList = nil); // Constructor that takes a StringList
    destructor Destroy; override; // Destructor
    function ToStringList: TStringList;
    procedure InitMap(Length: integer);
    function Map(Index: integer): integer;
    function ReverseMap(Value: integer): integer;
    procedure AddMap(Value: integer);
    function AddTask(const TaskString: string): integer; // Method to add a task
    function GetTask(Index: integer): TTask; // Method to get a task by row index
    function GetTaskValue(ACol, ARow: integer): string; // Method to get a task value by row col
    function HasTask(Index: integer): boolean;
    procedure SetTask(Grid: TStringGrid; Row: integer; Backup: boolean = True);
    function InsertTask(const TaskString: string; Index: integer; Backup: boolean = True): integer;
    procedure DeleteTask(Index: integer);
    procedure ArchiveTask(Index: integer);
    procedure CompleteTask(Index: integer; Backup: boolean = True);
    procedure ClearTasksInRect(Grid: TStringGrid; Rect: TGridRect);
    function MoveTaskTop(Index1, Index2: integer): integer;
    function MoveTaskBottom(Index1, Index2: integer): integer;
    function MoveTaskUp(Index1, Index2: integer): integer;
    function MoveTaskDown(Index1, Index2: integer): integer;
    procedure SwapTasks(OldIndex, NewIndex: integer);
    procedure MoveTask(OldIndex, NewIndex: integer);
    procedure CopyToClipboard(Grid: TStringGrid);
    function PasteFromClipboard(Grid: TStringGrid): TGridRect;
    procedure FillGrid(Grid: TStringGrid; ShowArchive, ShowDuration: boolean; SortOrder: TSortOrder; SortColumn: integer);
    procedure CreateBackup;
    procedure UndoBackup;
    procedure CreateBackupInit;
    procedure UndoBackupInit;
    function GetCountArchive: integer;
    function CalcDateDiff(const StartDate, EndDate: TDateTime): string;

    property Count: integer read FCount;
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
  FComment := string.Empty;
  FText := string.Empty;
  FCommentItalic := True;
  FEmptyComment := False;
  FSpaceAfterComment := False;
  FSpaceBeforeComment := False;
end;

constructor TTask.Create(const TaskString: string);
var
  PartComment, PartDate: TStringArray; // Use TStringArray for compatibility
  CompletedStr: string;

  procedure FillComment(start: integer = 1);
  var
    i: integer;
  begin
    FComment := string.Empty;
    for i := start to High(PartComment) do
    begin
      FComment += PartComment[i];
      if (i < High(PartComment)) then FComment += '//';
    end;
  end;

  procedure FillCompletedStr(start: integer = 0);
  var
    i: integer;
  begin
    CompletedStr := string.Empty;
    for i := start to High(PartComment) do
    begin
      CompletedStr += PartComment[i];
      if (i < High(PartComment)) then CompletedStr += '//';
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
  // Format: - [x] 01.01.2000, ~~Task~~ // *Comment*

  // Split the task string into PartComment
  FCommentItalic := False;

  PartComment := TaskString.Split(['//']);
  if (Length(PartComment) >= 2) and (not PartComment[0].EndsWith(':')) then
  begin
    CompletedStr := PartComment[0];
    if (Length(CompletedStr) > 0) and (CompletedStr.EndsWith(' ')) then
    begin
      Delete(CompletedStr, Length(CompletedStr), 1);
      SpaceBeforeComment := True;
    end;
    FillComment;

    // Test for empty comment symbols
    if (FComment.Trim = string.empty) then
      EmptyComment := True;
  end
  else
    FillCompletedStr;

  // Remove star in start and end of comment
  if (FComment.TrimLeft.StartsWith('*')) and (FComment.TrimRight.EndsWith('*')) then
  begin
    FCommentItalic := True;
    if (Length(FComment) > 0) and (FComment.StartsWith(' ')) then
    begin
      FComment := TrimLeft(FComment);
      SpaceAfterComment := True;
    end;
    Delete(FComment, 1, 1);
    if (TrimRight(FComment).EndsWith('*')) then
    begin
      FComment := TrimRight(FComment);
      Delete(FComment, Length(FComment), 1);
    end;
  end
  else
  begin
    if (Length(FComment) > 0) and (FComment.StartsWith(' ')) then
    begin
      SpaceAfterComment := True;
      Delete(FComment, 1, 1);
    end;
  end;

  PartDate := CompletedStr.Split([',']);

  // Check completion status based on the first character in the string
  FDone := DetectDone(PartDate[0]);
  PartDate[0] := RemoveBrackets(PartDate[0]);

  // Checks if the task is completed
  CompletedStr := RemoveBrackets(CompletedStr);

  if Length(PartDate) > 1 then
  begin
    // Extract and trim the date string
    if (Length(PartDate) > 1) and (TryStrToDateTime(PartDate[0].Trim, FDate)) then
    begin
      FillText(1);
      if (Length(FText) > 0) and (FText.StartsWith(' ')) then
      begin
        Delete(FText, 1, 1);
      end;
    end
    else
    begin
      FText := CompletedStr;
      FDate := 0;
    end;
  end
  else
  begin
    FText := CompletedStr;
    FDate := 0;
  end;

  FText := StringReplace(FText, '<br>', sLineBreak, [rfReplaceAll]);
  FComment := StringReplace(FComment, '<br>', sLineBreak, [rfReplaceAll]);

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
end;

function TTask.ToString(Col: integer = 0; AddEmptyCompletion: boolean = True): string;
var
  TextString: string;
  DoneString: string;
  CommentString: string;
begin
  // Replace line breaks from task description and comment
  TextString := StringReplace(Text, sLineBreak, '<br>', [rfReplaceAll]);
  Comment := StringReplace(Comment, sLineBreak, '<br>', [rfReplaceAll]);

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

  // Check comments
  if (Comment <> string.Empty) or (EmptyComment) then
  begin
    CommentString := string.Empty;
    if (SpaceBeforeComment) then CommentString += ' ';
    CommentString += '//';
    if (SpaceAfterComment) then CommentString += ' ';

    if (Comment <> string.Empty) then
    begin
      if CommentItalic then
        CommentString += '*' + Comment + '*'
      else
        CommentString += Comment;
    end;
  end
  else
    CommentString := string.Empty;

  // Form the task string based on the provided Col
  case Col of
    1: Result := DoneString; // Returning only the completion status
    2: Result := TextString; // Returning only the task string
    3: Result := CommentString; // Returning only the comment
    4:
      if Date > 0 then
        Result := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat, Date).Trim
      else
        Result := string.Empty; // If the completion date is missing, return an empty string
    else
      // Forming the task string considering the completion date and comment
      if (DoneString = string.Empty) then
      begin
        if Date > 0 then
          Result := Format('%s, %s%s', [FormatDateTime(FormatSettings.ShortDateFormat + ' ' +
            FormatSettings.LongTimeFormat, Date), TextString, CommentString])
        else
          Result := Format('%s%s', [TextString, CommentString]);
      end
      else
      begin
        if Date > 0 then
          Result := Format('%s %s, %s%s', [DoneString, FormatDateTime(FormatSettings.ShortDateFormat + ' ' +
            FormatSettings.LongTimeFormat, Date), TextString, CommentString]).Trim
        else
          Result := Format('%s %s%s', [DoneString, TextString, CommentString]).Trim;
      end;
  end;
end;

procedure TTask.Copy(Original: TTask);
begin
  FDone := Original.FDone;
  FArchive := Original.FArchive;
  FDate := Original.FDate;
  FComment := Original.FComment;
  FText := Original.FText;
  FCommentItalic := Original.FCommentItalic;
  FEmptyComment := Original.FEmptyComment;
  FSpaceBeforeComment := Original.FSpaceBeforeComment;
  FSpaceAfterComment := Original.FSpaceAfterComment;
end;

{ TTasks }

constructor TTasks.Create(const TaskStrings: TStringList = nil);
var
  i: integer; // Index for iteration
begin
  SetLength(FTaskList, 0); // Initialize task array
  SetLength(FMapGrid, 0); // Initialize task map
  FCount := 0; // Initialize task count

  // Iterate through the StringList to create tasks
  if (Assigned(TaskStrings)) then
  begin
    for i := 0 to TaskStrings.Count - 1 do
    begin
      AddTask(TaskStrings[i]); // Create a new task from the string and add to the list
    end;
    TaskStrings.Free;
  end;
  InitMap(FCount + 1);
  CreateBackupInit;
end;

destructor TTasks.Destroy;
var
  i: integer;
begin
  // Free each task in the array
  if (Assigned(FTaskList)) then
    for i := 0 to High(FTaskList) do
      FTaskList[i].Free;
  if (Assigned(FBackupTaskList)) then
    for i := 0 to High(FBackupTaskList) do
      FBackupTaskList[i].Free;
  if (Assigned(FInitTaskList)) then
    for i := 0 to High(FInitTaskList) do
      FInitTaskList[i].Free;

  SetLength(FMapGrid, 0);
  FMapGrid := nil;

  inherited;
end;

function TTasks.ToStringList: TStringList;
var
  i: integer;
  addCompleted: boolean;
begin
  Result := TStringList.Create;
  try
    addCompleted := False;
    for i := 0 to FCount - 1 do
      if FTaskList[i].Done then addCompleted := True;
    //if (FCount = 1) and (FTaskList[0].Text = string.Empty) and (FTaskList[0].Comment = string.Empty) and (FTaskList[0].Date = 0) then
    //  addCompleted := True;

    for i := 0 to FCount - 1 do
    begin
      Result.Add(FTaskList[i].ToString(0, addCompleted)); // Add task string to TStringList
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
  SetLength(FTaskList, FCount + 1); // Resize the array
  FTaskList[FCount] := Task; // Add the task to the list
  Result := FCount;
  Inc(FCount); // Increment the task count
end;

function TTasks.GetTask(Index: integer): TTask;
var
  Ind: integer;
begin
  Ind := Map(Index);
  if (Ind < 0) or (Ind >= FCount) then
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
  if ACol = 3 then Result := GetTask(aRow).Comment
  else
  if ACol = 4 then Result := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat, GetTask(aRow).Date);
end;

function TTasks.HasTask(Index: integer): boolean;
var
  Ind: integer;
begin
  Ind := Map(Index);
  Result := (Ind >= 0) and (Ind < FCount);
end;

procedure TTasks.SetTask(Grid: TStringGrid; Row: integer; Backup: boolean = True);
var
  Task: TTask;
  Date: TDateTime;
begin
  if (Row > 0) and (Row <= FCount) then
  begin
    if (Backup) then
      CreateBackup;

    Task := GetTask(Row);
    // Get the task by the row index (minus one, as rows start from 1)

    // Reading data from the grid
    Task.Done := StrToBoolDef(Grid.Cells[1, Row], False); // Convert to boolean
    Task.Text := Grid.Cells[2, Row];
    Task.Comment := Grid.Cells[3, Row];
    if not TryStrToDateTime(Grid.Cells[4, Row], Date) then
    begin
      Date := 0; // If parsing the date failed, set to 0
      Grid.Cells[4, Row] := '';
    end
    else
      Grid.Cells[4, Row] := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat, Date);
    Task.Date := Date;
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
    Ind := FCount - 1
  else
    Ind := Map(Index);

  if (Ind < 0) and (Ind >= FCount) then
    exit(-1);

  if (Backup) then
    CreateBackup;

  Ind := Ind + 1;
  Task := TTask.Create(TaskString); // Create a new task
  SetLength(FTaskList, FCount + 1); // Resize the array

  // Shift tasks down to make space for the new task
  for i := FCount downto Ind + 1 do
  begin
    FTaskList[i] := FTaskList[i - 1]; // Move tasks one position down
  end;

  FTaskList[Ind] := Task; // Insert the new task at the specified index
  Inc(FCount); // Increment the task count

  Result := Ind;
end;

procedure TTasks.DeleteTask(Index: integer);
var
  i, Ind: integer;
begin
  Ind := Map(Index);
  if (Ind < 0) or (Ind >= FCount) then
    exit;

  // Free the task that is being removed
  if (Assigned(FTaskList[Ind])) then
    FTaskList[Ind].Free;

  // Shift tasks down to fill the gap
  for i := Ind to FCount - 2 do
  begin
    FTaskList[i] := FTaskList[i + 1]; // Move the next task to the current position
  end;

  // Resize the array to remove the last (now duplicate) element
  SetLength(FTaskList, FCount - 1);
  Dec(FCount); // Decrease the task count

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
  if (Ind < 0) or (Ind >= FCount) then
    exit;

  FTaskList[Ind].Archive := not FTaskList[Ind].Archive;
end;

procedure TTasks.CompleteTask(Index: integer; Backup: boolean = True);
var
  Ind: integer;
begin
  Ind := Map(Index);
  if (Ind < 0) or (Ind >= FCount) then
    exit;

  if Backup then CreateBackup;

  FTaskList[Ind].Done := not FTaskList[Ind].Done;
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
        GetTask(i).Comment := ''; // Clear comment
      if (j = 4) then
        GetTask(i).Date := 0; // Reset completion date

      // For grid column
      if (j = 1) then
        Grid.Cells[j, i] := '0'
      else
        Grid.Cells[j, i] := string.Empty;
    end;
  end;
end;

function TTasks.MoveTaskTop(Index1, Index2: integer): integer;
var
  TempTasks: array of TTask; // Temporary array for selected tasks
  i, IndStart, IndEnd, Len: integer;
begin
  Result := -1;
  IndStart := Map(Index1);
  IndEnd := Map(Index2);
  Len := IndEnd - IndStart + 1;

  // Check if the Index1 is valid and not already at the top
  if (IndStart > 0) and (IndEnd <= FCount) then
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

function TTasks.MoveTaskBottom(Index1, Index2: integer): integer;
var
  TempTasks: array of TTask; // Temporary array for selected tasks
  i, IndStart, IndEnd, Len: integer;
begin
  Result := -1;
  IndStart := Map(Index1);
  IndEnd := Map(Index2);
  Len := IndEnd - IndStart + 1;

  // Check if the Index1 is valid and not already at the bottom
  if (IndStart >= 0) and (IndEnd < FCount) then
  begin
    CreateBackup;

    // Save selected tasks to temporary array
    TempTasks := [];
    SetLength(TempTasks, Len);
    for i := IndStart to IndEnd do
      TempTasks[i - IndStart] := FTaskList[i];

    // Shift all tasks down to fill the gap
    for i := IndStart to FCount - 1 - Len do
      FTaskList[i] := FTaskList[i + Len];

    // Place the stored task at the end
    for i := 0 to High(TempTasks) do
      FTaskList[FCount - len + i] := TempTasks[i];

    Result := Length(MapGrid) - 1; //ReverseMap(FCount);
  end;
end;

function TTasks.MoveTaskUp(Index1, Index2: integer): integer;
var
  TempTasks: array of TTask; // Temporary array for selected tasks
  i, IndStart, IndEnd, Len: integer;
begin
  Result := -1;
  IndStart := Map(Index1);
  IndEnd := Map(Index2);
  Len := IndEnd - IndStart + 1;

  // Check if the selection is not at the top and valid
  if (IndStart > 0) and (IndEnd < FCount) then
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

function TTasks.MoveTaskDown(Index1, Index2: integer): integer;
var
  TempTasks: array of TTask; // Temporary array for selected tasks
  i, IndStart, IndEnd, Len: integer;
begin
  Result := -1;
  IndStart := Map(Index1);
  IndEnd := Map(Index2);
  Len := IndEnd - IndStart + 1;

  // Check if the selection is not at the bottom and valid
  if (IndEnd < FCount - 1) and (IndStart >= 0) then
  begin
    CreateBackup;

    // Save selected tasks to temporary array
    TempTasks := [];
    SetLength(TempTasks, Len);
    for i := IndStart to IndEnd do
      TempTasks[i - IndStart] := FTaskList[i];

    // Shift tasks down by one position to fill the gap
    for i := IndStart to IndEnd do
      if (i + Len < FCount) then
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
  if (IndOld < 0) or (IndNew < 0) or (IndOld >= FCount) or (IndNew >= FCount) then
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
  if (Ind < 0) or (Ind >= FCount) or (NewIndex < 0) or (NewIndex > FCount) then
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
  Done, Text, Comment, Date: string;
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
        if (j = 1) then Done := GetTask(i).ToString(j).Trim;
        if (j = 2) then Text := GetTask(i).ToString(j).Trim;
        if (j = 3) then Comment := GetTask(i).ToString(j).Trim;
        if (j = 4) then Date := GetTask(i).ToString(j).Trim;
      end;
      Row1 := (Done + ' ' + Date).Trim;
      Row2 := (Text + ' ' + Comment).Trim;
      if (Date <> string.Empty) and (Row2 <> string.Empty) then
        Row1 += ', '
      else
      if (Row1 <> string.Empty) and (Row2 <> string.Empty) then
        Row1 += ' ';
      RowText := Row1 + Row2;
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
  Rect: TGridRect;
  Index, i, j: integer;
  RowTask: TTask;
  IsEmpty, DoInsert: boolean;
  //  Id, Id0, Id1: integer;
begin
  Result := Grid.Selection;
  DoInsert := True;
  if Clipboard.AsText = string.Empty then exit;
  CreateBackup;

  if (Grid.Row > 0) then
  begin
    RowTask := GetTask(Grid.Row);
    IsEmpty := (RowTask.Text = string.Empty) and (RowTask.Comment = string.Empty) and (RowTask.Date = 0);
  end
  else
    IsEmpty := False;

  TempTasks := TTasks.Create(TextToStringList(Clipboard.AsText, True));
  try
    if (Grid.Selection.Height = 0) and (Grid.Selection.Width = 0) and (not IsEmpty) then
    begin
      if (Grid.Row = 0) then
        index := 1
      else
        index := Grid.Row;
      for i := TempTasks.FCount - 1 downto 0 do
      begin
        InsertTask(TempTasks.GetTask(i + 1).ToString(), index, False);
        // if i = 0 then Id0 := Id;
        // if i = TempTasks.FCount - 1 then Id1 := Id;
      end;
      // Result := TGridRect.Create(1, ReverseMap(id0), 4, ReverseMap(id1));
    end
    else
    begin
      index := 1;

      if (Grid.Selection.Height = 0) and (Grid.Selection.Width = 0) and (IsEmpty) then
      begin
        GetTask(Grid.Row).Copy(TempTasks.GetTask(index));
        Inc(index);
      end
      else
      begin
        Rect := Grid.Selection; // Get grid selection rect
        if ((Grid.Selection.Height > 0) or (Grid.Selection.Width > 0)) and
          (not ((Grid.Selection.Left = 2) and (Grid.Selection.Right = 2))) then
        begin
          DoInsert := False;
        end;

        for i := Rect.Top to Rect.Bottom do
        begin
          if (index > TempTasks.FCount) then
          begin
            Index := 1;
            DoInsert := False;
          end;
          for j := Rect.Left to Rect.Right do
          begin
            if j = 1 then GetTask(i).Done := TempTasks.GetTask(index).Done;
            if j = 2 then
            begin
              if (TempTasks.GetTask(index).Text <> string.Empty) then
              begin
                GetTask(i).Text := TempTasks.GetTask(index).Text;
                GetTask(i).Archive := TempTasks.GetTask(index).Archive;
              end
              else
              if Rect.Width = 0 then
              begin
                if (TempTasks.GetTask(index).Comment <> string.Empty) then
                  GetTask(i).Text := TempTasks.GetTask(index).Comment
                else
                if (TempTasks.GetTask(index).Date <> 0) then
                  GetTask(i).Text := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat,
                    TempTasks.GetTask(index).Date)
                else
                  GetTask(i).Text := string.Empty;
              end
              else
                GetTask(i).Text := string.Empty;
            end;
            if j = 3 then
            begin
              if (TempTasks.GetTask(index).Comment <> string.Empty) then
              begin
                GetTask(i).Comment := TempTasks.GetTask(index).Comment;
                GetTask(i).CommentItalic := TempTasks.GetTask(index).CommentItalic;
              end
              else
              if Rect.Width = 0 then
              begin
                if (TempTasks.GetTask(index).Text <> string.Empty) then
                  GetTask(i).Comment := TempTasks.GetTask(index).Text
                else
                if (TempTasks.GetTask(index).Date <> 0) then
                  GetTask(i).Comment := FormatDateTime(FormatSettings.ShortDateFormat + ' ' +
                    FormatSettings.LongTimeFormat, TempTasks.GetTask(index).Date)
                else
                  GetTask(i).Comment := string.Empty;
              end
              else
                GetTask(i).Comment := string.Empty;
            end;
            if j = 4 then
            begin
              if (TempTasks.GetTask(index).Date <> 0) then
                GetTask(i).Date := TempTasks.GetTask(index).Date
              else
              if Rect.Width = 0 then
              begin
                if (TempTasks.GetTask(index).Text <> string.Empty) and (TryStrToDateTime(TempTasks.GetTask(index).Text, TempDate)) then
                  GetTask(i).Date := TempDate
                else
                if (TempTasks.GetTask(index).Comment <> string.Empty) and
                  (TryStrToDateTime(TempTasks.GetTask(index).Comment, TempDate)) then
                  GetTask(i).Date := TempDate
                else
                  GetTask(i).Date := 0;
              end
              else
                GetTask(i).Date := 0;
            end;
          end;
          Inc(index);
        end;
      end;

      // Insert if clipboard is bigger than selection
      if (DoInsert) and (index - 1 < TempTasks.FCount) then
      begin
        for i := TempTasks.FCount - 1 downto index - 1 do
        begin
          InsertTask(TempTasks.GetTask(i + 1).ToString(), Grid.Row, False);
        end;
        Result := TGridRect.Create(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom + (TempTasks.Count - Index) + 1);
      end
      else
        Result := Rect;
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

  FCount := Length(FTaskList); // Restore the task count

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
  i: integer;
begin
  // Free old backup task list if it exists
  for i := 0 to High(FInitTaskList) do
  begin
    if Assigned(FInitTaskList[i]) then
    begin
      FInitTaskList[i].Free; // Free the old task
      FInitTaskList[i] := nil; // Set to nil after freeing
    end;
  end;
  SetLength(FInitTaskList, 0);

  // Create a backup of the task list
  SetLength(FInitTaskList, Length(FTaskList));
  for i := 0 to High(FInitTaskList) do
  begin
    FInitTaskList[i] := TTask.Create; // Create a new task
    FInitTaskList[i].Copy(FTaskList[i]); // Copy the task data
  end;
end;

procedure TTasks.UndoBackupInit;
var
  i: integer;
begin
  // Resize the task list to match the initial task list
  SetLength(FTaskList, Length(FInitTaskList));

  for i := 0 to High(FInitTaskList) do
  begin
    // If there's already an object in FTaskList, free it
    if Assigned(FTaskList[i]) then
    begin
      FTaskList[i].Free;
      FTaskList[i] := nil; // Set to nil after freeing
    end;

    // Create a new task and copy the initial task data
    FTaskList[i] := TTask.Create;
    FTaskList[i].Copy(FInitTaskList[i]);
  end;

  FCount := Length(FTaskList); // Restore the task count
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
        // Comment
        Value1 := Task1.Comment;
        Value2 := Task2.Comment;
      end;
      4: begin
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
  procedure AddDatesInterval();
  begin
    SetLength(StartDates, Length(StartDates) + 1);
    SetLength(EndDates, Length(EndDates) + 1);
    StartDates[High(StartDates)] := StartDate;
    EndDates[High(EndDates)] := EndDate;
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
      TotalDuration := TotalDuration + (EndDates[i] - StartDates[i]);
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
              AddDatesInterval;
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
        Grid.Cells[3, RowIndex] := FTaskList[I].Comment;
        if FTaskList[I].Date > 0 then
        begin
          Grid.Cells[4, RowIndex] := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat,
            FTaskList[I].Date);
        end
        else
          Grid.Cells[4, RowIndex] := string.Empty;

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
    if (SortColumn >= 1) and (SortColumn <= 4) then
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
