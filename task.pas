unit task;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Grids, Clipbrd;

type
  TIntegerArray = array of integer;

  // Class representing a single task
  TTask = class
  private
    FIsCompleted: boolean; // Status of task completion
    FIsArchive: boolean; // Archive task
    FCompletionDate: TDateTime; // Date of task completion
    FComment: string; // Comment for the task
    FTaskDescription: string; // Description of the task
  public
    constructor Create(const TaskString: string); // Constructor that takes a task string
    property IsCompleted: boolean read FIsCompleted write FIsCompleted;
    property IsArchive: boolean read FIsArchive write FIsArchive;
    property CompletionDate: TDateTime read FCompletionDate write FCompletionDate;
    property Comment: string read FComment write FComment;
    property TaskDescription: string read FTaskDescription write FTaskDescription;
    function ToString(Index: integer = 0; AddEmptyCompletion: boolean = True): string;
  end;

  // Class representing a collection of tasks
  TTasks = class
  private
    FTaskList: array of TTask; // Array of tasks
    FCount: integer; // Current count of tasks
    FMapGrid: TIntegerArray;
  public
    // Constructor that takes a StringList
    constructor Create(const TaskStrings: TStringList = nil);
    destructor Destroy; override; // Destructor
    function Map(Index: integer): integer;
    function ReverseMap(Value: integer): integer;
    procedure AddTask(const TaskString: string); // Method to add a task
    function GetTask(Index: integer): TTask; // Method to get a task by index
    function HasTask(Index: integer): boolean;
    procedure SetTask(Grid: TStringGrid; Row, Col: integer);
    procedure InsertTask(const TaskString: string; Index: integer);
    procedure DeleteTask(Index: integer);
    procedure ArchiveTask(Index: integer);
    procedure CompleteTask(Index: integer);
    procedure ClearTasksInRect(Rect: TGridRect);
    function MoveTaskUp(Index: integer): integer;
    function MoveTaskDown(Index: integer): integer;
    function MoveTaskToTop(Index: integer): integer;
    function MoveTaskToBottom(Index: integer): integer;
    procedure CopyToClipboard(Grid: TStringGrid);
    procedure FillGrid(Grid: TStringGrid; ShowArchive: boolean; SortOrder: TSortOrder);
    function ToStringList: TStringList;

    property Count: integer read FCount;
    property MapGrid: TIntegerArray read FMapGrid;
  end;

implementation

{ TTask }

function RemoveBrackets(const S: string): string;
const
  Brackets: array[0..11] of string = ('- [x]', '- [X]', '- [ ]', '- []', '-[x]', '-[X]', '-[ ]', '-[]', '[x]', '[X]', '[ ]', '[]');
var
  I: integer;
begin
  Result := S;
  for I := Low(Brackets) to High(Brackets) do
  begin
    if Pos(Brackets[I], Result) = 1 then
    begin
      Delete(Result, 1, Length(Brackets[I]));
      Break;
    end;
  end;
  Result := TrimLeft(Result); // Remove spaces from begining of string
end;

constructor TTask.Create(const TaskString: string);
var
  Parts, PartsSub: TStringArray; // Use TStringArray for compatibility
  CompletedStr: string;
begin
  // Format [x] 01.01.2000, Task *// Comment*
  // Split the task string into parts
  Parts := TaskString.Split(['*//']);
  if Length(Parts) = 2 then
  begin
    CompletedStr := Parts[0].Trim;
    FComment := Parts[1].Trim;
    if FComment.EndsWith('*') then
      Delete(FComment, Length(FComment), 1);
  end
  else
    CompletedStr := Parts[0];

  PartsSub := CompletedStr.Split([',']);
  // Check completion status based on the first character in the string
  FIsCompleted := PartsSub[0].Trim.ToLower.StartsWith('- [x]') or PartsSub[0].Trim.ToLower.StartsWith('-[x]') or
    PartsSub[0].Trim.ToLower.StartsWith('[x]');

  // Checks if the task is completed
  PartsSub[0] := RemoveBrackets(PartsSub[0]);
  CompletedStr := RemoveBrackets(CompletedStr);

  if Length(PartsSub) > 1 then
  begin
    // Extract and trim the date string
    if (TryStrToDateTime(PartsSub[1].Trim, FCompletionDate)) and (Length(PartsSub) > 2) then
      FTaskDescription := PartsSub[2].Trim
    else
    if (TryStrToDateTime(PartsSub[0].Trim, FCompletionDate)) and (Length(PartsSub) > 1) then
      FTaskDescription := PartsSub[1].Trim
    else
      FTaskDescription := CompletedStr.Trim;
  end
  else
    FTaskDescription := CompletedStr.Trim;

  FTaskDescription := StringReplace(FTaskDescription, '<br>', sLineBreak, [rfReplaceAll]);
  FComment := StringReplace(FComment, '<br>', sLineBreak, [rfReplaceAll]);

  // Check if TaskDescription starts and ends with '~~'
  if FTaskDescription.StartsWith('~~') and FTaskDescription.EndsWith('~~') then
  begin
    FIsArchive := True;
    // Remove '~~' from the start and end of the TaskDescription
    FTaskDescription := FTaskDescription.Substring(2, Length(FTaskDescription) - 4);
  end;
end;

function TTask.ToString(Index: integer = 0; AddEmptyCompletion: boolean = True): string;
var
  TaskString: string;
  CompletionStatus: string;
  CommentPart: string;
begin
  // Replace line breaks from task desctioption and comment
  TaskString := StringReplace(TaskDescription, sLineBreak, '<br>', [rfReplaceAll]);
  Comment := StringReplace(Comment, sLineBreak, '<br>', [rfReplaceAll]);

  // Add '~~' for archived tasks
  if (FIsArchive) then
    TaskString := '~~' + TaskString + '~~';

  // Check completion
  if IsCompleted then
    CompletionStatus := '- [x]'
  else
  if AddEmptyCompletion then
    CompletionStatus := '- [ ]';

  // Check comments
  if Comment <> string.Empty then
    CommentPart := ' *// ' + Comment + '*'
  else
    CommentPart := string.Empty;

  // Form the task string based on the provided index
  case Index of
    1: Result := CompletionStatus; // Returning only the completion status
    2: Result := TaskString; // Returning only the task string
    3: Result := CommentPart; // Returning only the comment
    4:
      if CompletionDate > 0 then
        Result := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat, CompletionDate).Trim
      else
        Result := string.Empty; // If the completion date is missing, return an empty string
    else
      // Forming the task string considering the completion date and comment
      if CompletionDate > 0 then
        Result := Format('%s %s, %s%s', [CompletionStatus, FormatDateTime(FormatSettings.ShortDateFormat +
          ' ' + FormatSettings.LongTimeFormat, CompletionDate), TaskString, CommentPart]).Trim
      else
        Result := Format('%s %s%s', [CompletionStatus, TaskString, CommentPart]).Trim;
  end;
end;

{ TTasks }

constructor TTasks.Create(const TaskStrings: TStringList);
var
  i: integer; // Index for iteration
begin
  SetLength(FTaskList, 0); // Initialize task array
  FCount := 0; // Initialize task count

  // Iterate through the StringList to create tasks
  if (Assigned(TaskStrings)) then
    for i := 0 to TaskStrings.Count - 1 do
    begin
      AddTask(TaskStrings[i]); // Create a new task from the string and add to the list
    end;
end;

destructor TTasks.Destroy;
var
  i: integer;
begin
  // Free each task in the array
  for i := 0 to High(FTaskList) do
    FTaskList[i].Free;
  inherited;
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

procedure TTasks.AddTask(const TaskString: string);
var
  Task: TTask;
begin
  Task := TTask.Create(TaskString); // Create a new task
  SetLength(FTaskList, FCount + 1); // Resize the array
  FTaskList[FCount] := Task; // Add the task to the list
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

function TTasks.HasTask(Index: integer): boolean;
var
  Ind: integer;
begin
  Ind := Map(Index);
  Result := (Ind >= 0) and (Ind < FCount);
end;

procedure TTasks.SetTask(Grid: TStringGrid; Row, Col: integer);
var
  Task: TTask;
  IsCompleted: boolean;
  TaskDescription, Comment: string;
  CompletionDate: TDateTime;
begin
  if (Row > 0) and (Row <= FCount) then
  begin
    Task := GetTask(Row);
    // Get the task by the row index (minus one, as rows start from 1)

    // Reading data from the grid
    IsCompleted := StrToBoolDef(Grid.Cells[1, Row], False); // Convert to boolean
    TaskDescription := Grid.Cells[2, Row];
    Comment := Grid.Cells[3, Row];
    if not TryStrToDateTime(Grid.Cells[4, Row], CompletionDate) then
    begin
      CompletionDate := 0; // If parsing the date failed, set to 0
      Grid.Cells[4, Row] := '';
    end
    else
      Grid.Cells[4, Row] := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat, CompletionDate);

    // Update task properties
    Task.IsCompleted := IsCompleted;
    Task.TaskDescription := TaskDescription;
    Task.Comment := Comment;
    Task.CompletionDate := CompletionDate;
  end
  else
    raise Exception.Create('Invalid row or task index');
end;

procedure TTasks.InsertTask(const TaskString: string; Index: integer);
var
  Task: TTask;
  i: integer;
begin
  if (Index < 0) or (Index > FCount) then
    raise Exception.Create('Index out of bounds'); // Error handling for invalid index

  Task := TTask.Create(TaskString); // Create a new task
  SetLength(FTaskList, FCount + 1); // Resize the array

  // Shift tasks to the right to make space for the new task
  for i := FCount downto Index do
  begin
    FTaskList[i] := FTaskList[i - 1]; // Move tasks one position to the right
  end;

  FTaskList[Index] := Task; // Insert the new task at the specified index
  Inc(FCount); // Increment the task count
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
end;

procedure TTasks.ArchiveTask(Index: integer);
var
  i, Ind: integer;
begin
  Ind := Map(Index);
  if (Ind < 0) or (Ind >= FCount) then
    exit;

  FTaskList[Ind].IsArchive := not FTaskList[Ind].IsArchive;
end;

procedure TTasks.CompleteTask(Index: integer);
var
  i, Ind: integer;
begin
  Ind := Map(Index);
  if (Ind < 0) or (Ind >= FCount) then
    exit;

  FTaskList[Ind].IsCompleted := not FTaskList[Ind].IsCompleted;
end;

procedure TTasks.ClearTasksInRect(Rect: TGridRect);
var
  i, j: integer;
begin
  for i := Rect.Top to Rect.Bottom do
  begin
    for j := Rect.Left to Rect.Right do
    begin
      // Clearing task fields based on the column
      if (j = 1) then
        GetTask(i).IsCompleted := False; // Reset completion status
      if (j = 2) then
        GetTask(i).TaskDescription := ''; // Clear task description
      if (j = 3) then
        GetTask(i).Comment := ''; // Clear comment
      if (j = 4) then
        GetTask(i).CompletionDate := 0; // Reset completion date
    end;
  end;
end;

function TTasks.MoveTaskUp(Index: integer): integer;
var
  TempTask: TTask;   // Temporary variable for swapping tasks
  Ind: integer;
begin
  Ind := Map(Index);
  Result := -1;
  if (Ind > 0) and (Ind < FCount) then
  begin
    // Save the task in the temporary variable
    TempTask := FTaskList[Ind];

    // Shift the task at Index - 1 to Index
    FTaskList[Ind] := FTaskList[Ind - 1];

    // Place the temporary task in Index - 1
    FTaskList[Ind - 1] := TempTask;

    Result := ReverseMap(Ind - 1);
  end;
end;

function TTasks.MoveTaskDown(Index: integer): integer;
var
  TempTask: TTask;
  Ind: integer;
begin
  Ind := Map(Index);
  Result := -1;
  // Check if the index is valid and not the last task
  if (Ind >= 0) and (Ind < FCount - 1) then
  begin
    // Temporarily store the task at the current index
    TempTask := FTaskList[Ind];

    // Move the task below to the current index
    FTaskList[Ind] := FTaskList[Ind + 1];

    // Place the stored task below
    FTaskList[Ind + 1] := TempTask;

    Result := ReverseMap(Ind + 1);
  end;
end;

function TTasks.MoveTaskToTop(Index: integer): integer;
var
  TempTask: TTask; // Declare the temporary variable here
  i, Ind: integer;
begin
  Ind := Map(Index);
  Result := -1;
  // Check if the index is valid and not already at the top
  if (Ind > 0) and (Ind < FCount) then
  begin
    // Store the task at the given index
    TempTask := FTaskList[Ind];

    // Shift tasks down to make room at the top
    for i := Ind downto 1 do
      FTaskList[i] := FTaskList[i - 1];

    // Place the stored task at the top
    FTaskList[0] := TempTask;

    Result := ReverseMap(0);
  end;
end;

function TTasks.MoveTaskToBottom(Index: integer): integer;
var
  TempTask: TTask; // Declare the temporary variable here
  i, Ind: integer;
begin
  Ind := Map(Index);
  Result := -1;
  // Check if the index is valid and not already at the bottom
  if (Ind >= 0) and (Ind < FCount - 1) then
  begin
    // Store the task at the given index
    TempTask := FTaskList[Ind];

    // Shift all tasks down to fill the gap
    for i := Ind to FCount - 2 do
      FTaskList[i] := FTaskList[i + 1];

    // Place the stored task at the end
    FTaskList[FCount - 1] := TempTask;

    Result := ReverseMap(FCount - 1);
  end;
end;

procedure TTasks.CopyToClipboard(Grid: TStringGrid);
var
  SelectedText: TStringList;
  Rect: TGridRect;
  CompletionState, TaskDescription, Comment, CompletionDate: string;
  Row1, Row2, RowText: string;
  i, j: integer;
begin
  SelectedText := TStringList.Create;
  try
    // Get grid selection rect
    Rect := Grid.Selection;

    for i := Rect.Top to Rect.Bottom do
    begin
      for j := Rect.Left to Rect.Right do
      begin
        if (j = 1) then CompletionState := GetTask(i).ToString(j).Trim;
        if (j = 2) then TaskDescription := GetTask(i).ToString(j).Trim;
        if (j = 3) then Comment := GetTask(i).ToString(j).Trim;
        if (j = 4) then CompletionDate := GetTask(i).ToString(j).Trim;
      end;
      Row1 := (CompletionState + ' ' + CompletionDate).Trim;
      Row2 := (TaskDescription + ' ' + Comment).Trim;
      if (CompletionDate <> string.Empty) and (Row2 <> string.Empty) then
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

procedure TTasks.FillGrid(Grid: TStringGrid; ShowArchive: boolean; SortOrder: TSortOrder);
var
  I, ArhCount, RowIndex: integer;
  eventOnColRowInserted: TGridOperationEvent;
  eventOnColRowDeleted: TGridOperationEvent;
begin
  try
    Grid.BeginUpdate;
    eventOnColRowInserted := Grid.OnColRowInserted;
    eventOnColRowDeleted := Grid.OnColRowDeleted;
    Grid.OnColRowInserted := nil;
    Grid.OnColRowDeleted := nil;

    ArhCount := 0;
    for I := 0 to Count - 1 do
      if FTaskList[I].IsArchive then ArhCount += 1;

    if (ShowArchive) then
      Grid.RowCount := Count + 1 // Set the row count to the number of tasks
    else
      Grid.RowCount := Count - ArhCount + 1; // Set the row count to the number of tasks

    // Determine row index based on sort order
    if SortOrder = soAscending then
      RowIndex := 1
    else
      RowIndex := Grid.RowCount - 1;

    SetLength(FMapGrid, Grid.RowCount);
    FMapGrid[0] := -1;

    for I := 0 to Count - 1 do
    begin
      if (ShowArchive = True) or (FTaskList[I].IsArchive = False) then
      begin
        Grid.Cells[1, RowIndex] := IntToStr(Ord(FTaskList[I].IsCompleted)); // Convert Boolean to 1 or 0
        Grid.Cells[2, RowIndex] := FTaskList[I].TaskDescription; // Set task description
        Grid.Cells[3, RowIndex] := FTaskList[I].Comment; // Set comment

        if FTaskList[I].CompletionDate > 0 then
          Grid.Cells[4, RowIndex] := FormatDateTime(FormatSettings.ShortDateFormat + ' ' +
            FormatSettings.LongTimeFormat, FTaskList[I].CompletionDate) // Set completion date
        else
          Grid.Cells[4, RowIndex] := string.Empty;

        FMapGrid[RowIndex] := I;

        if SortOrder = soAscending then
          RowIndex += 1
        else
          RowIndex -= 1;
      end;
    end;

  finally
    if (Assigned(eventOnColRowInserted)) then
      Grid.OnColRowInserted := eventOnColRowInserted;
    if (Assigned(eventOnColRowDeleted)) then
      Grid.OnColRowDeleted := eventOnColRowDeleted;
    Grid.EndUpdate;
  end;
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
      if FTaskList[i].IsCompleted then addCompleted := True;
    if (FCount = 1) and (FTaskList[0].TaskDescription = string.Empty) and (FTaskList[0].Comment = string.Empty) and
      (FTaskList[0].CompletionDate = 0) then
      addCompleted := True;

    for i := 0 to FCount - 1 do
    begin
      Result.Add(FTaskList[i].ToString(0, addCompleted)); // Add task string to TStringList
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.
