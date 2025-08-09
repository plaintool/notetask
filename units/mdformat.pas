//-----------------------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the GNU General Public License, Version 3 (GPL-3.0)
//  You may obtain a copy of the License at https://www.gnu.org/licenses/gpl-3.0.html
//-----------------------------------------------------------------------------------

unit mdformat;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Task;

type
  TAddGroupProc = procedure(const GroupName: string; const Lines: TStringList) of object;
  TGetGroupNameFunc = function(Index: integer): string of object;
  TGetTaskCountFunc = function(GroupIndex: integer): integer of object;
  TGetTaskFunc = function(GroupIndex, TaskIndex: integer): TTask of object;

function TaskFromString(const TaskString: string): TTask;
function TaskToString(Task: TTask; Col: integer = 0; AddEmptyCompletion: boolean = True): string;
procedure TasksFromStringList(const TaskStrings: TStringList; AddGroup: TAddGroupProc);
function TasksToStringList(GroupCount: integer; AddCompleted: boolean; GetGroupName: TGetGroupNameFunc;
  GetTaskCount: TGetTaskCountFunc; GetTask: TGetTaskFunc): TStringList;

implementation

uses stringtool;

function TaskFromString(const TaskString: string): TTask;
var
  PartNote, PartDate: TStringArray; // Use TStringArray for compatibility
  CompletedStr: string;

  procedure FillText(start: integer = 1);
  var
    i: integer;
  begin
    Result.FText := string.Empty;
    for i := start to High(PartDate) do
    begin
      Result.FText += PartDate[i];
      if (i < High(PartDate)) then Result.FText += ',';
    end;
  end;

  procedure FillNote(start: integer = 1);
  var
    i: integer;
  begin
    Result.FNote := string.Empty;
    for i := start to High(PartNote) do
    begin
      Result.FNote += PartNote[i];
      if (i < High(PartNote)) then Result.FNote += '//';
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

begin
  Result := TTask.Create;

  // Format: - [x] 01.01.2000, 123, ~~**Task**~~ // Note
  Result.FSpaceBeforeNote := True;
  Result.FSpaceAfterNote := True;
  Result.FNoteItalic := False;
  CompletedStr := string.Empty;

  // Split the task string into PartNote
  PartNote := TaskString.Split(['//']);
  if (Length(PartNote) >= 2) and (not PartNote[0].EndsWith(':')) then // Url protection
  begin
    CompletedStr := PartNote[0];
    if (Length(CompletedStr) > 0) and (CompletedStr.EndsWith(' ')) then
    begin
      Delete(CompletedStr, Length(CompletedStr), 1);
      Result.FSpaceBeforeNote := True;
    end
    else
      Result.FSpaceBeforeNote := False;

    if (Length(PartNote[1]) > 0) and (PartNote[1].StartsWith(' ')) then
    begin
      Delete(PartNote[1], 0, 1);
      Result.FSpaceAfterNote := True;
    end
    else
      Result.FSpaceAfterNote := False;

    FillNote;

    // Test for empty Note symbols
    if (Result.FNote.Trim = string.empty) then
      Result.FEmptyNote := True;
  end
  else
    FillCompletedStr;

  // Remove star in start and end of Note
  if (Result.FNote.TrimLeft.StartsWith('*')) and (Result.FNote.TrimRight.EndsWith('*')) then
  begin
    Result.FNoteItalic := True;
    if (Length(Result.FNote) > 0) and (Result.FNote.StartsWith(' ')) then
    begin
      Result.FNote := TrimLeft(Result.FNote);
      Result.FSpaceAfterNote := True;
    end;
    Delete(Result.FNote, 1, 1);
    if (TrimRight(Result.FNote).EndsWith('*')) then
    begin
      Result.FNote := TrimRight(Result.FNote);
      Delete(Result.FNote, Length(Result.FNote), 1);
    end;
  end
  else
  begin
    if (Length(Result.FNote) > 0) and (Result.FNote.StartsWith(' ')) then
    begin
      Result.FSpaceAfterNote := True;
      Delete(Result.FNote, 1, 1);
    end;
  end;

  PartDate := CompletedStr.Split([',']);

  // Check completion status based on the first character in the string
  Result.FDone := DetectDone(PartDate[0]);
  PartDate[0] := RemoveBrackets(PartDate[0]);

  // Checks if the task is completed
  CompletedStr := RemoveBrackets(CompletedStr);

  if (TryStrToFloatLimited(CleanAmount(CompletedStr), Result.FAmount)) then
  else
  if (TryStrToDateTimeISO(CompletedStr, Result.FDate)) then
  else
  if Length(PartDate) > 2 then
  begin
    // Extract and trim the date string
    if (TryStrToDateTimeISO(PartDate[0].Trim, Result.FDate)) then
    begin
      // Extract and trim the amount string
      if (TryStrToFloatLimited(PartDate[1].Trim, Result.FAmount)) then
        FillText(2)
      else
      begin
        FillText(1);
        Result.FAmount := 0;
      end;
      if (Length(Result.FText) > 0) and (Result.FText.StartsWith(' ')) then
        Delete(Result.FText, 1, 1); // Delete space in begining
    end
    else
    // Extract and trim the amount string
    if (TryStrToFloatLimited(PartDate[0].Trim, Result.FAmount)) then
    begin
      FillText(1);
      if (Length(Result.FText) > 0) and (Result.FText.StartsWith(' ')) then
        Delete(Result.FText, 1, 1); // Delete space in begining
    end
    else
    begin
      Result.FText := CompletedStr;
      Result.FDate := 0;
      Result.FAmount := 0;
    end;
  end
  else
  if Length(PartDate) > 1 then
  begin
    // Extract and trim the amount string
    if (TryStrToFloatLimited(PartDate[0].Trim, Result.FAmount)) then
    begin
      FillText(1);
      if (Length(Result.FText) > 0) and (Result.FText.StartsWith(' ')) then
        Delete(Result.FText, 1, 1); // Delete space in begining
    end
    else
    begin
      // Extract and trim the date string
      if (TryStrToDateTimeISO(PartDate[0].Trim, Result.FDate)) then
      begin
        FillText(1);
        if (Length(Result.FText) > 0) and (Result.FText.StartsWith(' ')) then
          Delete(Result.FText, 1, 1); // Delete space in begining
      end
      else
      begin
        Result.FText := CompletedStr;
        Result.FDate := 0;
      end;
      Result.FAmount := 0;
    end;
  end
  else
  begin
    Result.FText := CompletedStr;
    Result.FDate := 0;
    Result.FAmount := 0;
  end;

  Result.FText := StringReplace(Result.FText, '<br>', sLineBreak, [rfReplaceAll]);
  Result.FNote := StringReplace(Result.FNote, '<br>', sLineBreak, [rfReplaceAll]);

  // Check if Text starts and ends with '~~'
  if Result.FText.TrimLeft.StartsWith('~~') and Result.FText.TrimRight.EndsWith('~~') then
  begin
    Result.FArchive := True;
    Result.FText := Trim(Result.FText);
    // Remove '~~' from the start and end of the Text
    Result.FText := Result.FText.Substring(2, Length(Result.FText) - 4);
  end
  else
    Result.FArchive := False;

  // Check if Text starts and ends with '**'
  if Result.FText.TrimLeft.StartsWith('**') and Result.FText.TrimRight.EndsWith('**') then
  begin
    Result.FStar := True;
    Result.FText := Trim(Result.FText);
    // Remove '**' from the start and end of the Text
    Result.FText := Result.FText.Substring(2, Length(Result.FText) - 4);
  end
  else
    Result.FStar := False;
end;

function TaskToString(Task: TTask; Col: integer = 0; AddEmptyCompletion: boolean = True): string;
var
  TextString: string;
  DoneString: string;
  NoteString: string;
begin
  // Replace line breaks from task description and Note
  TextString := StringReplace(Task.FText, sLineBreak, '<br>', [rfReplaceAll]);
  TextString := StringReplace(TextString, #13, '<br>', [rfReplaceAll]);
  TextString := StringReplace(TextString, #10, '<br>', [rfReplaceAll]);
  NoteString := StringReplace(Task.FNote, sLineBreak, '<br>', [rfReplaceAll]);
  NoteString := StringReplace(NoteString, #13, '<br>', [rfReplaceAll]);
  NoteString := StringReplace(NoteString, #10, '<br>', [rfReplaceAll]);

  // Add '**' for starred tasks
  if Task.FStar then
    TextString := '**' + TextString + '**';

  // Add '~~' for archived tasks
  if Task.FArchive then
    TextString := '~~' + TextString + '~~';

  // Check completion
  if Task.FDone then
    DoneString := '- [x]'
  else if AddEmptyCompletion then
    DoneString := '- [ ]'
  else
    DoneString := string.Empty;

  // Check notes
  if (NoteString <> string.Empty) or (Task.FEmptyNote) then
  begin
    if (NoteString <> string.Empty) and (Task.FNoteItalic) then
      NoteString := '*' + NoteString + '*';

    if (Task.FSpaceAfterNote) then NoteString := ' ' + NoteString;
    NoteString := '//' + NoteString;
    if (Task.FSpaceBeforeNote) then NoteString := ' ' + NoteString;
  end
  else
    NoteString := string.Empty;

  // Form the task string based on the provided Col
  case Col of
    1: Result := DoneString; // Returning only the completion status
    2: Result := TextString; // Returning only the task string
    3: Result := NoteString; // Returning only the Note
    4:
      if Task.FAmount <> 0 then
        Result := FloatToString(Task.FAmount)
      else
        Result := string.Empty;
    5:
      if Task.FDate > 0 then
        Result := Task.DateTimeStrISO.Trim
      else
        Result := string.Empty; // If the completion date is missing, return an empty string
    else
      // Forming the task string considering the completion date and Note
      if (DoneString = string.Empty) then
      begin
        if Task.FAmount <> 0 then
        begin
          if Task.FDate > 0 then
            Result := Format('%s, %s, %s%s', [Task.DateTimeStrISO, Task.AmountStr, TextString, NoteString])
          else
          begin
            if (TextString + NoteString <> string.Empty) then
              Result := Format('%s, %s%s', [Task.AmountStr, TextString, NoteString])
            else
              Result := Format('%s', [Task.AmountStr]);
          end;
        end
        else
        begin
          if Task.FDate > 0 then
          begin
            if (TextString + NoteString <> string.Empty) then
              Result := Format('%s, %s%s', [Task.DateTimeStrISO, TextString, NoteString])
            else
              Result := Format('%s', [Task.DateTimeStrISO]);
          end
          else
            Result := Format('%s%s', [TextString, NoteString]);
        end;
      end
      else
      begin
        if Task.FAmount <> 0 then
        begin
          if Task.FDate > 0 then
            Result := Format('%s %s, %s, %s%s', [DoneString, Task.DateTimeStrISO, Task.AmountStr, TextString, NoteString]).Trim
          else
          begin
            if (TextString + NoteString <> string.Empty) then
              Result := Format('%s %s, %s%s', [DoneString, Task.AmountStr, TextString, NoteString]).Trim
            else
              Result := Format('%s %s', [DoneString, Task.AmountStr]).Trim;
          end;
        end
        else
        begin
          if Task.FDate > 0 then
          begin
            if (TextString + NoteString <> string.Empty) then
              Result := Format('%s %s, %s%s', [DoneString, Task.DateTimeStrISO, TextString, NoteString]).Trim
            else
              Result := Format('%s %s', [DoneString, Task.DateTimeStrISO]).Trim;
          end
          else
            Result := Format('%s %s%s', [DoneString, TextString, NoteString]).Trim;
        end;
      end;
  end;
end;

procedure TasksFromStringList(const TaskStrings: TStringList; AddGroup: TAddGroupProc);
var
  i: integer; // Index for iteration
  TabName, Value: string;
  TabContent: TStringList;
begin
  // Iterate through the StringList to create tasks
  TabContent := TStringList.Create;
  try
    TabName := string.Empty;

    for i := 0 to TaskStrings.Count - 1 do
    begin
      Value := TaskStrings[i];

      if (Value.TrimLeft.StartsWith('#')) then
      begin
        if (TabContent.Count > 0) or (TabName <> string.Empty) then
        begin
          AddGroup(TabName, TabContent);
          TabContent.Clear;
        end;

        TabName := Value;
        Continue;
      end;

      TabContent.Add(Value);
    end;

    // Add last group
    AddGroup(TabName, TabContent);
  finally
    TabContent.Free;
  end;
end;

function TasksToStringList(GroupCount: integer; AddCompleted: boolean; GetGroupName: TGetGroupNameFunc;
  GetTaskCount: TGetTaskCountFunc; GetTask: TGetTaskFunc): TStringList;
var
  i, j: integer;
begin
  Result := TStringList.Create;
  for i := 0 to GroupCount - 1 do
  begin
    if (GetGroupName(i) <> '') then
      Result.Add(GetGroupName(i));

    for j := 0 to GetTaskCount(i) - 1 do
      Result.Add(TaskToString(GetTask(i, j), 0, AddCompleted));
  end;
end;

end.
