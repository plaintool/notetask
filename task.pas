unit task;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Grids;

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
    // Property for completion status
    property CompletionDate: TDateTime read FCompletionDate write FCompletionDate;
    // Property for completion date
    property Comment: string read FComment write FComment; // Property for comment
    property TaskDescription: string read FTaskDescription write FTaskDescription;
    // Property for task description
  end;

  // Class representing a collection of tasks
  TTasks = class
  private
    FTaskList: array of TTask; // Array of tasks
    FCount: integer; // Current count of tasks
  public
    constructor Create(const TaskStrings: TStringList);
    // Constructor that takes a StringList
    destructor Destroy; override; // Destructor
    procedure AddTask(const TaskString: string); // Method to add a task
    function GetTask(Index: integer): TTask; // Method to get a task by index
    function Count: integer; // Method to get the number of tasks
    procedure FillGrid(Grid: TStringGrid);
  end;

implementation

{ TTask }

function TryDateTime(const S: string; out DateTime: TDateTime): boolean;
const
  Formats: array[0..4] of string = (
    'dd.mm.yyyy', 'yyyy-mm-dd', 'dd/mm/yyyy', 'yyyy/mm/dd', 'mm/dd/yyyy'
    );
var
  I: integer;
  FormatSettings: TFormatSettings;
begin
  Result := False;
  for I := Low(Formats) to High(Formats) do
  begin
    FormatSettings.ShortDateFormat := Formats[I];
    FormatSettings.DateSeparator := Formats[I][3];
    // Предполагаем, что в 3-й позиции находится разделитель
    if TryStrToDateTime(S, DateTime, FormatSettings) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function RemoveBrackets(const S: string): string;
const
  Brackets: array[0..3] of string = ('[x]', '[X]', '[ ]', '[]');
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
  Result := TrimLeft(Result); // Удаление лишних пробелов в начале строки
end;

constructor TTask.Create(const TaskString: string);
var
  Parts, PartsSub: TStringArray; // Use TStringArray for compatibility
  CompletedStr: string;
  FormatSettings: TFormatSettings;
begin
  // Split the task string into parts
  Parts := TaskString.Split(['|']);
  if Length(Parts) = 2 then
  begin
    CompletedStr := Parts[0];
    FComment := Parts[1];
  end
  else
    CompletedStr := Parts[0];

  PartsSub := CompletedStr.Split([',']);
  // Check completion status based on the first character in the string
  FIsCompleted := PartsSub[0].Trim.StartsWith('[x]') or PartsSub[0].Trim.StartsWith('[X]'); // Checks if the task is completed
  CompletedStr := RemoveBrackets(CompletedStr);

  if Length(PartsSub) > 1 then
  begin
    // Extract and trim the date string
    if (TryDateTime(PartsSub[1].Trim, FCompletionDate)) and (Length(PartsSub) > 2) then
      FTaskDescription := PartsSub[2].Trim
    else
    if (TryDateTime(PartsSub[0].Trim, FCompletionDate)) and (Length(PartsSub) > 1) then
      FTaskDescription := PartsSub[1].Trim
    else
      FTaskDescription := CompletedStr;
  end
  else
    FTaskDescription := CompletedStr;
end;

{ TTasks }

constructor TTasks.Create(const TaskStrings: TStringList);
var
  i: integer; // Index for iteration
begin
  SetLength(FTaskList, 0); // Initialize task array
  FCount := 0; // Initialize task count

  // Iterate through the StringList to create tasks
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

function TTasks.Count: integer;
begin
  Result := FCount; // Return the number of tasks
end;

procedure TTasks.FillGrid(Grid: TStringGrid);
var
  I: integer;
begin
  Grid.RowCount := Count + 1; // Set the row count to the number of tasks

  for I := 0 to Count - 1 do
  begin
    Grid.Cells[1, I + 1] := IntToStr(Ord(FTaskList[I].IsCompleted)); // Convert Boolean to 1 or 0
    Grid.Cells[2, I + 1] := FTaskList[I].TaskDescription; // Set comment
    Grid.Cells[3, I + 1] := FTaskList[I].Comment; // Set comment
    if FTaskList[I].CompletionDate > 0 then
      Grid.Cells[4, I + 1] := DateTimeToStr(FTaskList[I].CompletionDate); // Set completion date
  end;
end;

end.
