//-----------------------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the GNU General Public License, Version 3 (GPL-3.0)
//  You may obtain a copy of the License at https://www.gnu.org/licenses/gpl-3.0.html
//-----------------------------------------------------------------------------------

unit stringtool;

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

uses
  Classes,
  SysUtils,
  Process;

function TextToStringList(const Content: string; TrimEnd: boolean = False): TStringList;

function TryStrToDateTimeISO(const S: string; out ADateTime: TDateTime): boolean;

function DateTimeToStringISO(Value: TDateTime; ADisplayTime: boolean = True): string;

function DateTimeToString(Value: TDateTime; ADisplayTime: boolean = True): string;

function FloatToString(Value: double): string;

function IsBracket(const Input: string): boolean;

function RemoveBrackets(const S: string): string;

function CleanString(const Value: string): string;

function CleanNumeric(Value: string): string;

function CleanAmount(const Value: string): string;

function DetectDone(const Input: string): boolean;

function TrimLeadingSpaces(const Input: string; MaxSpaces: integer = 4): string;

function PosExReverse(const SubStr, S: unicodestring; Offset: SizeUint): SizeInt;

function EncodeUrl(const url: string): string;

function GetConsoleEncoding: string;

const
  Brackets: array[0..11] of string = ('- [x]', '- [X]', '- [ ]', '- []', '-[x]', '-[X]', '-[ ]', '-[]', '[x]', '[X]', '[ ]', '[]');
  UnicodeMinusUTF8 = #$E2#$88#$92;

implementation

function TextToStringList(const Content: string; TrimEnd: boolean = False): TStringList;
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  // Create a new instance of TStringList
  try
    StringList.Text := Content; // Load text into TStringList

    // Check if the file ends with a new line and the last line is not empty
    if (Content <> '') and (Content[Length(Content)] in [#10, #13]) and (not TrimEnd) then
    begin
      // Add an extra empty line only if the last line is not already empty
      StringList.Add(string.Empty);
    end;

    Result := StringList; // Return TStringList
  except
    StringList.Free; // Free memory on error
    raise; // Re-throw the exception
  end;
end;

function TryStrToDateTimeISO(const S: string; out ADateTime: TDateTime): boolean;
var
  FS: TFormatSettings;
  SFixed: string;
begin
  SFixed := StringReplace(S, 'T', ' ', [rfReplaceAll]);
  SFixed := StringReplace(SFixed, 'Z', '', [rfReplaceAll]);
  SFixed := StringReplace(SFixed, '.', '-', [rfReplaceAll]);

  FS := DefaultFormatSettings;
  FS.DateSeparator := '-';
  FS.TimeSeparator := ':';
  FS.ShortDateFormat := 'yyyy-mm-dd';
  FS.ShortTimeFormat := 'hh:nn:ss';

  Result := TryStrToDateTime(SFixed, ADateTime, FS);
  if not Result then
  begin
    // If ISO parsing fails, try using local format
    FS := DefaultFormatSettings;
    Result := TryStrToDateTime(S, ADateTime, FS);
  end;
end;

function DateTimeToStringISO(Value: TDateTime; ADisplayTime: boolean = True): string;
var
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FS.DateSeparator := '-';
  FS.TimeSeparator := ':';
  FS.ShortDateFormat := 'yyyy-mm-dd';
  FS.ShortTimeFormat := 'hh:nn:ss';

  if (Frac(Value) = 0) or (not ADisplayTime) then
    Result := FormatDateTime('yyyy"-"mm"-"dd', Value, FS)
  else
    Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', Value, FS);
end;

function DateTimeToString(Value: TDateTime; ADisplayTime: boolean = True): string;
begin
  if (not ADisplayTime) then
    Result := FormatDateTime(FormatSettings.ShortDateFormat, Value)
  else
    Result := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat, Value);
end;

function FloatToString(Value: double): string;
begin
  Result := FloatToStr(Value);
end;

function IsBracket(const Input: string): boolean;
var
  TrimmedInput: string;
  I: integer;
begin
  // Trim the input string
  TrimmedInput := Trim(Input);

  // Check if the trimmed string is in the Brackets array
  Result := False;
  for I := Low(Brackets) to High(Brackets) do
  begin
    if Brackets[I] = TrimmedInput then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function RemoveBrackets(const S: string): string;
var
  I: integer;
begin
  Result := S;
  for I := Low(Brackets) to High(Brackets) do
  begin
    if Result.TrimLeft.StartsWith(Brackets[I]) then
    begin
      // Remove brackets
      Result := TrimLeft(Result);
      Delete(Result, 1, Length(Brackets[I]));

      // Remove first space
      if (Length(Result) > 0) and (Result.StartsWith(' ')) then
        Delete(Result, 1, 1);
      Break;
    end;
  end;
end;

function CleanString(const Value: string): string;
begin
  Result := Value.Replace(#9, ' ');
end;

function CleanNumeric(Value: string): string;
var
  i: integer;
  C: char;
begin
  Result := '';
  Value := Value.Replace(UnicodeMinusUTF8, '-');
  for i := 1 to Length(Value) do
  begin
    C := Value[i];
    // allow digits and ASCII minus
    if C in ['0'..'9', '-', '.'] then
      Result := Result + C
    else if C = ',' then
      Result := Result + '.'; // replace comma with dot
  end;
end;

function CleanAmount(const Value: string): string;
begin
  Result := Value.Replace(' ', string.empty).Replace(',', '.').Trim;
end;

function DetectDone(const Input: string): boolean;
var
  i: integer;
  LowerInput: string;
const
  Brackets: array[0..2] of string = ('- [x]', '-[x]', '[x]');
begin
  Result := False;
  LowerInput := Trim(LowerCase(Input));
  for i := 0 to High(Brackets) do
  begin
    if LowerInput.StartsWith(LowerCase(Brackets[i])) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TrimLeadingSpaces(const Input: string; MaxSpaces: integer = 4): string;
var
  i, SpaceCount: integer;
begin
  SpaceCount := 0;

  // Count leading spaces, up to MaxSpaces
  for i := 1 to Length(Input) do
  begin
    if (Input[i] = ' ') and (SpaceCount < MaxSpaces) then
      Inc(SpaceCount)
    else
      Break;
  end;

  // Remove the leading spaces
  Result := Copy(Input, SpaceCount + 1, Length(Input) - SpaceCount);
end;

function PosExReverse(const SubStr, S: unicodestring; Offset: SizeUint): SizeInt;
var
  i, MaxLen, SubLen: SizeInt;
  // SubFirst: widechar;
  pc: pwidechar;
begin
  Result := 0; // Initialize result to 0 (not found)
  SubLen := Length(SubStr); // Get length of the substring

  // Check if the substring is not empty and Offset is valid
  if (SubLen > 0) and (Offset > 0) and (Offset <= SizeUint(Length(S))) then
  begin
    MaxLen := Length(S) - SubLen + 1; // Adjust max starting index to include end of the string
    // SubFirst := SubStr[1]; // Get the first character of the substring

    // Search backwards, starting from Offset
    for i := Offset downto 1 do
    begin
      // Ensure there is enough space left for the substring
      if (i <= MaxLen) then
      begin
        pc := @S[i]; // Pointer to the current position

        // Check for a match with the substring
        if (CompareWord(SubStr[1], pc^, SubLen) = 0) then
        begin
          Result := i; // Return the found position
          Exit; // Exit the function
        end;
      end;
    end;
  end;
end;

function EncodeUrl(const url: string): string;
var
  x: integer;
  sBuff: string;
const
  SafeMask = ['A'..'Z', '0'..'9', 'a'..'z', '*', '@', '.', '_', '-', '+'];
begin
  // Init
  sBuff := '';

  for x := 1 to Length(url) do
  begin
    // Check if we have a safe char
    if url[x] in SafeMask then
    begin
      // Append all other chars
      sBuff := sBuff + url[x];
    end
    else
    begin
      // Convert to hex
      sBuff := sBuff + '%' + IntToHex(Ord(url[x]), 2);
    end;
  end;

  Result := sBuff;
end;

function GetConsoleEncoding: string;
var
  Output: TStringList;
  Process: TProcess;
  Encoding: string;
begin
  Result := 'utf-8'; // Default to UTF-8
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    {$IFDEF Windows}
        Process.Executable := 'cmd.exe';
        Process.Parameters.Add('/C chcp'); // Execute the chcp command
    {$ELSE}
    Process.Executable := '/bin/bash';
    Process.Parameters.Add('-c');
    Process.Parameters.Add('locale charmap'); // Check the character encoding
    {$ENDIF}

    Process.Options := [poUsePipes, poNoConsole];
    Process.Execute;

    Output.LoadFromStream(Process.Output);

    // Check the output of chcp or locale charmap command
    if Output.Count > 0 then
    begin
      {$IFDEF Windows}
        if Pos('866', Output[0]) > 0 then
          Encoding := 'CP866'           // Russian (Cyrillic)
        else if Pos('850', Output[0]) > 0 then
          Encoding := 'CP850'           // Western European
        else if Pos('437', Output[0]) > 0 then
          Encoding := 'CP437'           // United States
        else if Pos('1252', Output[0]) > 0 then
          Encoding := 'CP1252'          // Western European (Windows)
        else if Pos('65001', Output[0]) > 0 then
          Encoding := 'utf-8'           // UTF-8
        else if Pos('936', Output[0]) > 0 then
          Encoding := 'GB2312'          // Simplified Chinese
        else if Pos('950', Output[0]) > 0 then
          Encoding := 'Big5'            // Traditional Chinese
        else if Pos('932', Output[0]) > 0 then
          Encoding := 'Shift-JIS'       // Japanese
        else if Pos('949', Output[0]) > 0 then
          Encoding := 'CP949'           // Korean
        else if Pos('1251', Output[0]) > 0 then
          Encoding := 'CP1251';         // Cyrillic (Windows)
      {$ELSE}
      // For Linux and macOS, check the output of `locale charmap`
      Encoding := Trim(Output[0]);
      {$ENDIF}
    end;

    if Encoding <> '' then
      Result := Encoding;
  finally
    Output.Free;
    Process.Free;
  end;
end;

end.
