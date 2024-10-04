unit task;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Grids, Clipbrd;

type
  // Class representing a single task
  TTask = class
  private
    FIsCompleted: boolean; // Status of task completion
    FCompletionDate: TDateTime; // Date of task completion
    FComment: string; // Comment for the task
    FTaskDescription: string; // Description of the task
  public
    constructor Create(const TaskString: string); // Constructor that takes a task string
    property IsCompleted: boolean read FIsCompleted write FIsCompleted;
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
  public
    constructor Create(const TaskStrings: TStringList = nil);
    // Constructor that takes a StringList
    destructor Destroy; override; // Destructor
    procedure AddTask(const TaskString: string); // Method to add a task
    function GetTask(Index: integer): TTask; // Method to get a task by index
    procedure SetTask(Grid: TStringGrid; Row, Col: integer);
    procedure RemoveTask(Index: integer);
    function Count: integer; // Method to get the number of tasks
    procedure CopyToClipboard(Grid: TStringGrid);
    procedure FillGrid(Grid: TStringGrid; SortOrder: TSortOrder = soAscending);
    function ToStringList: TStringList;
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

  // Формируем строку задачи в зависимости от переданного индекса
  case Index of
    1: Result := CompletionStatus; // Возвращаем только статус завершенности
    2: Result := TaskString; // Возвращаем только строку задачи
    3: Result := CommentPart; // Возвращаем только комментарий
    4:
      if CompletionDate > 0 then
        Result := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat, CompletionDate).Trim
      else
        Result := string.Empty; // Если даты завершения нет, возвращаем пустую строку
    else
      // Формируем строку задачи с учетом даты завершения и комментария
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
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.Create('Index out of bounds'); // Error handling for invalid index
  Result := FTaskList[Index]; // Return the task by index
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
    Task := GetTask(Row - 1);
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

procedure TTasks.RemoveTask(Index: integer);
var
  i: integer;
begin
  if (Index < 0) or (Index >= FCount) then
    exit;

  // Free the task that is being removed
  FTaskList[Index].Free;

  // Shift tasks down to fill the gap
  for i := Index to FCount - 2 do
  begin
    FTaskList[i] := FTaskList[i + 1]; // Move the next task to the current position
  end;

  // Resize the array to remove the last (now duplicate) element
  SetLength(FTaskList, FCount - 1);
  Dec(FCount); // Decrease the task count
end;

function TTasks.Count: integer;
begin
  Result := FCount; // Return the number of tasks
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
        if (j = 1) then CompletionState := GetTask(i - 1).ToString(j).Trim;
        if (j = 2) then TaskDescription := GetTask(i - 1).ToString(j).Trim;
        if (j = 3) then Comment := GetTask(i - 1).ToString(j).Trim;
        if (j = 4) then CompletionDate := GetTask(i - 1).ToString(j).Trim;
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

procedure TTasks.FillGrid(Grid: TStringGrid; SortOrder: TSortOrder = soAscending);
var
  I, RowIndex: integer;
  event: TGridOperationEvent;
begin
  try
    Grid.BeginUpdate;

    event := Grid.OnColRowInserted;
    Grid.OnColRowInserted := nil;

    Grid.RowCount := Count + 1; // Set the row count to the number of tasks

    for I := 0 to Count - 1 do
    begin
      // Determine row index based on sort order
      if SortOrder = soAscending then
        RowIndex := I + 1
      else
        RowIndex := Count - I;

      Grid.Cells[1, RowIndex] := IntToStr(Ord(FTaskList[I].IsCompleted)); // Convert Boolean to 1 or 0
      Grid.Cells[2, RowIndex] := FTaskList[I].TaskDescription; // Set task description
      Grid.Cells[3, RowIndex] := FTaskList[I].Comment; // Set comment

      if FTaskList[I].CompletionDate > 0 then
        Grid.Cells[4, RowIndex] := DateTimeToStr(FTaskList[I].CompletionDate); // Set completion date
    end;

  finally
    if (Assigned(event)) then
      Grid.OnColRowInserted := event;
    Grid.EndUpdate;
  end;
end;

function TTasks.ToStringList: TStringList;
var
  i: integer;
  isCompleted: boolean;
begin
  Result := TStringList.Create;
  try
    isCompleted := False;
    for i := 0 to FCount - 1 do
      if FTaskList[i].IsCompleted then isCompleted := True;

    for i := 0 to FCount - 1 do
    begin
      Result.Add(FTaskList[i].ToString(0, isCompleted)); // Add task string to TStringList
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.
