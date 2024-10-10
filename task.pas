unit task;

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

uses
  Classes,
  SysUtils,
  Grids,
  Clipbrd;

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
  public
    constructor Create;
    constructor Create(const TaskString: string); // Constructor that takes a task string
    procedure Copy(Original: TTask);
    property Done: boolean read FDone write FDone;
    property Archive: boolean read FArchive write FArchive;
    property Date: TDateTime read FDate write FDate;
    property Comment: string read FComment write FComment;
    property Text: string read FText write FText;
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
    procedure InitMap(Length: integer);
    function Map(Index: integer): integer;
    function ReverseMap(Value: integer): integer;
    procedure AddMap(Value: integer);
    function AddTask(const TaskString: string): integer; // Method to add a task
    function GetTask(Index: integer): TTask; // Method to get a task by row index
    function GetTaskValue(ACol, ARow: integer): string; // Method to get a task value by row col
    function HasTask(Index: integer): boolean;
    procedure SetTask(Grid: TStringGrid; Row, Col: integer; Backup: boolean = True);
    function InsertTask(const TaskString: string; Index: integer; Backup: boolean = True): integer;
    procedure DeleteTask(Index: integer);
    procedure ArchiveTask(Index: integer);
    procedure CompleteTask(Index: integer);
    procedure ClearTasksInRect(Grid: TStringGrid; Rect: TGridRect);
    function MoveTaskTop(Index: integer): integer;
    function MoveTaskBottom(Index: integer): integer;
    function MoveTaskUp(Index: integer): integer;
    function MoveTaskDown(Index: integer): integer;
    procedure CopyToClipboard(Grid: TStringGrid);
    function PasteFromClipboard(Grid: TStringGrid): TGridRect;
    procedure FillGrid(Grid: TStringGrid; ShowArchive: boolean; SortOrder: TSortOrder; SortColumn: integer);
    function ToStringList: TStringList;
    procedure CreateBackup;
    procedure UndoBackup;
    procedure CreateBackupInit;
    procedure UndoBackupInit;

    property Count: integer read FCount;
    property MapGrid: TIntegerArray read FMapGrid;
  end;

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
end;

constructor TTask.Create(const TaskString: string);
var
  Parts, PartsSub: TStringArray; // Use TStringArray for compatibility
  CompletedStr: string;
begin
  // Format: - [x] 01.01.2000, ~~Task~~ *// Comment*
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
  FDone := PartsSub[0].Trim.ToLower.StartsWith('- [x]') or PartsSub[0].Trim.ToLower.StartsWith('-[x]') or PartsSub[0].Trim.ToLower.StartsWith('[x]');

  // Checks if the task is completed
  PartsSub[0] := RemoveBrackets(PartsSub[0]);
  CompletedStr := RemoveBrackets(CompletedStr);

  if Length(PartsSub) > 1 then
  begin
    // Extract and trim the date string
    if (TryStrToDateTime(PartsSub[1].Trim, FDate)) and (Length(PartsSub) > 2) then
      FText := PartsSub[2].Trim
    else
    if (TryStrToDateTime(PartsSub[0].Trim, FDate)) and (Length(PartsSub) > 1) then
      FText := PartsSub[1].Trim
    else
      FText := CompletedStr.Trim;
  end
  else
    FText := CompletedStr.Trim;

  FText := StringReplace(FText, '<br>', sLineBreak, [rfReplaceAll]);
  FComment := StringReplace(FComment, '<br>', sLineBreak, [rfReplaceAll]);

  // Check if Text starts and ends with '~~'
  if FText.StartsWith('~~') and FText.EndsWith('~~') then
  begin
    FArchive := True;
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
  // Replace line breaks from task desctioption and comment
  TextString := StringReplace(Text, sLineBreak, '<br>', [rfReplaceAll]);
  Comment := StringReplace(Comment, sLineBreak, '<br>', [rfReplaceAll]);

  // Add '~~' for archived tasks
  if (FArchive) then
    TextString := '~~' + TextString + '~~';

  // Check completion
  if Done then
    DoneString := '- [x]'
  else
  if AddEmptyCompletion then
    DoneString := '- [ ]'
  else
    DoneString := string.Empty;

  // Check comments
  if Comment <> string.Empty then
    CommentString := ' *// ' + Comment + '*'
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
      if Date > 0 then
        Result := Format('%s %s, %s%s', [DoneString, FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat, Date),
          TextString, CommentString]).Trim
      else
        Result := Format('%s %s%s', [DoneString, TextString, CommentString]).Trim;
  end;
end;

procedure TTask.Copy(Original: TTask);
begin
  FDone := Original.FDone;
  FArchive := Original.FArchive;
  FDate := Original.FDate;
  FComment := Original.FComment;
  FText := Original.FText;
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

procedure TTasks.SetTask(Grid: TStringGrid; Row, Col: integer; Backup: boolean = True);
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
    if (Col = 1) then
      Task.Done := StrToBoolDef(Grid.Cells[1, Row], False); // Convert to boolean
    if (Col = 2) then
      Task.Text := Grid.Cells[2, Row];
    if (Col = 3) then
      Task.Comment := Grid.Cells[3, Row];
    if (Col = 4) then
    begin
      if not TryStrToDateTime(Grid.Cells[4, Row], Date) then
      begin
        Date := 0; // If parsing the date failed, set to 0
        Grid.Cells[4, Row] := '';
      end
      else
        Grid.Cells[4, Row] := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat, Date);
      Task.Date := Date;
    end;
  end
  else
    raise Exception.Create('Invalid row or task index');
end;

function TTasks.InsertTask(const TaskString: string; Index: integer; Backup: boolean = True): integer;
var
  Task: TTask;
  i, Ind: integer;
begin
  Ind := Map(Index);
  if (Ind < 0) and (Ind >= FCount) then
    exit(-1);

  if (Backup) then
    CreateBackup;

  Ind := Ind + 1;
  Task := TTask.Create(TaskString); // Create a new task
  SetLength(FTaskList, FCount + 1); // Resize the array

  // Shift tasks to the right to make space for the new task
  for i := FCount downto Ind + 1 do
  begin
    FTaskList[i] := FTaskList[i - 1]; // Move tasks one position to the right
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

procedure TTasks.CompleteTask(Index: integer);
var
  Ind: integer;
begin
  Ind := Map(Index);
  if (Ind < 0) or (Ind >= FCount) then
    exit;

  CreateBackup;

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

function TTasks.MoveTaskTop(Index: integer): integer;
var
  TempTask: TTask; // Declare the temporary variable here
  i, Ind: integer;
begin
  Ind := Map(Index);
  Result := -1;
  // Check if the index is valid and not already at the top
  if (Ind >= 0) and (Ind < FCount) then
  begin
    CreateBackup;

    // Store the task at the given index
    TempTask := FTaskList[Ind];

    // Shift tasks down to make room at the top
    for i := Index downto 1 do
      FTaskList[Map(i)] := FTaskList[Map(i - 1)];

    // Place the stored task at the top
    FTaskList[Map(1)] := TempTask;

    Result := 1;// ReverseMap(0);
  end;
end;

function TTasks.MoveTaskBottom(Index: integer): integer;
var
  TempTask: TTask; // Declare the temporary variable here
  i, Ind: integer;
begin
  Ind := Map(Index);
  Result := -1;
  // Check if the index is valid and not already at the bottom
  if (Ind >= 0) and (Ind < FCount) then
  begin
    CreateBackup;

    // Store the task at the given index
    TempTask := FTaskList[Ind];

    // Shift all tasks down to fill the gap
    for i := Index to Length(MapGrid) - 1 do
      FTaskList[Map(i)] := FTaskList[Map(i + 1)];

    // Place the stored task at the end
    FTaskList[Map(FCount)] := TempTask;

    Result := Length(MapGrid) - 1;//ReverseMap(FCount);
  end;
end;

function TTasks.MoveTaskUp(Index: integer): integer;
var
  TempTask: TTask;   // Temporary variable for swapping tasks
  Ind: integer;
begin
  Ind := Map(Index);
  Result := -1;
  if (Index > 1) and (Ind >= 0) and (Ind < FCount) then
  begin
    CreateBackup;

    // Save the task in the temporary variable
    TempTask := FTaskList[Ind];

    // Shift the task at Index - 1 to Index
    FTaskList[Ind] := FTaskList[Map(Index - 1)];

    // Place the temporary task in Index - 1
    FTaskList[Map(Index - 1)] := TempTask;

    Result := Index - 1;//ReverseMap(Map(Index - 1));
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
  if (Index < Length(MapGrid) - 1) and (Ind >= 0) and (Ind < FCount) then
  begin
    CreateBackup;

    // Temporarily store the task at the current index
    TempTask := FTaskList[Ind];

    // Move the task below to the current index
    FTaskList[Ind] := FTaskList[Map(Index + 1)];

    // Place the stored task below
    FTaskList[Map(Index + 1)] := TempTask;

    Result := Index + 1;//ReverseMap(Map(Index + 1));
  end;
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
  //  Id, Id0, Id1: integer;
begin
  Result := Grid.Selection;
  if Clipboard.AsText = string.Empty then exit;
  CreateBackup;

  TempTasks := TTasks.Create(TextToStringList(Clipboard.AsText));
  if (Grid.Selection.Height = 0) and (Grid.Selection.Width = 0) then
  begin
    for i := TempTasks.FCount - 1 downto 0 do
    begin
      InsertTask(TempTasks.GetTask(i + 1).ToString(), Grid.Row, False);
      // if i = 0 then Id0 := Id;
      // if i = TempTasks.FCount - 1 then Id1 := Id;
    end;
    // Result := TGridRect.Create(1, ReverseMap(id0), 4, ReverseMap(id1));
  end
  else
  begin
    index := 1;
    Rect := Grid.Selection; // Get grid selection rect
    for i := Rect.Top to Rect.Bottom do
    begin
      if (index > TempTasks.FCount) then Index := 1;
      for j := Rect.Left to Rect.Right do
      begin
        if j = 1 then GetTask(i).Done := TempTasks.GetTask(index).Done;
        if j = 2 then
        begin
          if (TempTasks.GetTask(index).Text <> string.Empty) then
            GetTask(i).Text := TempTasks.GetTask(index).Text
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
            GetTask(i).Comment := TempTasks.GetTask(index).Comment
          else
          if Rect.Width = 0 then
          begin
            if (TempTasks.GetTask(index).Text <> string.Empty) then
              GetTask(i).Comment := TempTasks.GetTask(index).Text
            else
            if (TempTasks.GetTask(index).Date <> 0) then
              GetTask(i).Comment := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat,
                TempTasks.GetTask(index).Date)
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
            if (TempTasks.GetTask(index).Comment <> string.Empty) and (TryStrToDateTime(TempTasks.GetTask(index).Comment, TempDate)) then
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
    if (TempTasks.Count < Rect.Height) then
      Result := TGridRect.Create(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom - (Rect.Height - TempTasks.Count) - 1)
    else
      Result := Rect;
  end;
end;

procedure TTasks.FillGrid(Grid: TStringGrid; ShowArchive: boolean; SortOrder: TSortOrder; SortColumn: integer);
var
  I, J, ArhCount, RowIndex: integer;
  eventOnColRowInserted: TGridOperationEvent;
  eventOnColRowDeleted: TGridOperationEvent;

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

begin
  try
    Grid.BeginUpdate;
    eventOnColRowInserted := Grid.OnColRowInserted;
    eventOnColRowDeleted := Grid.OnColRowDeleted;
    Grid.OnColRowInserted := nil;
    Grid.OnColRowDeleted := nil;

    ArhCount := 0;
    for I := 0 to Count - 1 do
      if FTaskList[I].Archive then ArhCount += 1;

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
      if (ShowArchive = True) or (FTaskList[I].Archive = False) then
      begin
        Grid.Cells[1, RowIndex] := IntToStr(Ord(FTaskList[I].Done));
        Grid.Cells[2, RowIndex] := FTaskList[I].Text;
        Grid.Cells[3, RowIndex] := FTaskList[I].Comment;
        if FTaskList[I].Date > 0 then
          Grid.Cells[4, RowIndex] := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat, FTaskList[I].Date)
        else
          Grid.Cells[4, RowIndex] := string.Empty;

        FMapGrid[RowIndex] := I;

        if SortOrder = soAscending then
          RowIndex += 1
        else
          RowIndex -= 1;
      end;
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
    if (FCount = 1) and (FTaskList[0].Text = string.Empty) and (FTaskList[0].Comment = string.Empty) and (FTaskList[0].Date = 0) then
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
    if Assigned(FTaskList[i]) then
      FTaskList[i].Free;

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
    if Assigned(FBackupTaskList[i]) then
      FBackupTaskList[i].Free;

  // Update the backup with the intermediate state
  SetLength(FBackupTaskList, Length(TempTaskList));
  for i := 0 to High(FBackupTaskList) do
  begin
    FBackupTaskList[i] := TTask.Create;
    FBackupTaskList[i].Copy(TempTaskList[i]);
    TempTaskList[i].Free; // Free the temporary task after copying
  end;
end;

procedure TTasks.CreateBackupInit;
var
  i: integer;
begin
  // Create a backup of the task list and map grid
  SetLength(FInitTaskList, Length(FTaskList));
  for i := 0 to High(FInitTaskList) do
  begin
    FInitTaskList[i] := TTask.Create;
    FInitTaskList[i].Copy(FTaskList[i]);
  end;
end;

procedure TTasks.UndoBackupInit;
var
  i: integer;
begin
  SetLength(FTaskList, Length(FInitTaskList));
  for i := 0 to High(FInitTaskList) do
  begin
    FTaskList[i] := TTask.Create;
    FTaskList[i].Copy(FInitTaskList[i]);
  end;

  FCount := Length(FTaskList); // Restore the task count
end;

end.
