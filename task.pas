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

constructor TTask.Create(const TaskString: string);
var
  Parts: TStringArray; // Use TStringArray for compatibility
  DateStr: string;
  FormatSettings: TFormatSettings;
begin
  // Split the task string into parts
  Parts := TaskString.Split([',']);
  if Length(Parts) < 3 then
    raise Exception.Create('Invalid task string format');

  // Check completion status based on the first character in the string
  FIsCompleted := Parts[0].Trim.StartsWith('[x]'); // Checks if the task is completed

  // Extract and trim the date string
  DateStr := Parts[0].Trim.Substring(4).Trim; // Remove "[ ] - " and trim whitespace

  // Set format settings for date parsing
  FormatSettings.ShortDateFormat := 'dd.mm.yyyy'; // Specify the expected date format
  FormatSettings.DateSeparator := '.'; // Specify date separator

  // Attempt to convert string to date, with error handling
  if DateStr.StartsWith('-') then
    DateStr := DateStr.Substring(1).Trim; // Remove leading hyphen if present

  try
    FCompletionDate := StrToDateTime(DateStr, FormatSettings); // Convert string to date
  except
    on E: EConvertError do
      raise Exception.CreateFmt('Invalid date format in task string: "%s"', [DateStr]);
  end;

  FComment := Parts[1].Trim; // Trim comment
  FTaskDescription := Parts[2].Trim; // Trim task description
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
  Grid.RowCount := Count; // Set the row count to the number of tasks

  for I := 0 to Count - 1 do
  begin
    Grid.Cells[1, I] := IntToStr(Ord(FTaskList[I].IsCompleted)); // Convert Boolean to 1 or 0
    Grid.Cells[2, I] := FTaskList[I].TaskDescription; // Set comment
    Grid.Cells[3, I] := FTaskList[I].Comment; // Set comment
    Grid.Cells[4, I] := DateTimeToStr(FTaskList[I].CompletionDate); // Set completion date
  end;
end;

end.
