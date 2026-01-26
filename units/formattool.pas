//-----------------------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the GNU General Public License, Version 3 (GPL-3.0)
//  You may obtain a copy of the License at https://www.gnu.org/licenses/gpl-3.0.html
//-----------------------------------------------------------------------------------

unit formattool;

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

uses
  Classes,
  SysUtils,
  Graphics,
  Process,
  RegExpr,
  Math,
  StrUtils,
  LazUTF8,
  lineending;

type
  TIntegerArray = array of integer;

function TextToStringList(const Content: string; TrimEnd: boolean = False): TStringList;

function TryStrToDateTimeISO(const S: string; out ADateTime: TDateTime): boolean;

function DateTimeToStringISO(Value: TDateTime; ADisplayTime: boolean = True): string;

function DateTimeToString(Value: TDateTime; ADisplayTime: boolean = True): string;

function TryStrToFloatLimited(const S: string; out Value: double): boolean; overload;

function FloatToString(Value: double): string; overload;

function FloatToString(Value: double; FS: TFormatSettings): string; overload;

function SplitByFirstSpaces(const S: string; Count: integer = 1): TStringArray;

function StartsWithOperator(const S: string; out Op, Rest: string): boolean;

function StartsWithBracketAZ(const S: string): boolean;

function IsBracket(const Input: string): boolean;

function RemoveBrackets(const S: string): string;

function DetectDone(const Input: string): boolean;

function CleanString(const Value: string): string;

function CleanNumericExpression(Value: string): string;

function CleanNumeric(Value: string): string;

function CleanAmount(const Value: string): string;

function TrimLeadingSpaces(const Input: string; MaxSpaces: integer = 1): string;

function TrimTrailingSpaces(const Input: string; MaxSpaces: integer = 1): string;

function PosExReverse(const SubStr, S: unicodestring; Offset: SizeUint): SizeInt;

function EncodeUrl(const url: string): string;

function GetConsoleEncoding: string;

function IsUTF8Char(const S: string; CharIndex: integer; FindChar: string = ' '): boolean;

function ULower(Value: string): string;

function IsLetterOrDigit(ch: widechar): boolean;

function RepeatString(const S: string; Count: integer): string;

function MaskTextWithBullets(const AText: string; ACanvas: TCanvas; ALineEnding: TLineEnding): string;

function RenderWordCanvas(const AWord: string; const FontName: string = 'Monospace'; FontSize: integer = 12): string;

function IsEmail(const S: string): boolean;

function IsURL(const S: string): boolean;

function HasScheme(const URL: string): boolean;

function JoinArrayText(const Parts: TStringArray; StartIndex: integer = 0; const Separator: string = ','; EndIndex: integer = -1): string;

function RemoveBacktickBlocks(const S: string): string;

procedure FillTagsFromString(var List: TStringList; var S: string; Backtick: boolean = False);

procedure ReplaceListStartsWith(var List: TStringList; const S: string);

function StringListToBacktickString(List: TStringList; LeadingSpace: boolean = True): string;

procedure AddPrefixTags(List: TStringList);

function StringListsEqual(A, B: TStrings): boolean;

function GetBeforeColon(const S: string): string;

procedure ParseGroupName(const Value: string; out NameText, HintText: string);

function ReplaceLineBreaks(const S: string): string;

function DeleteFirstChar(const S: string; const Ch: char): string;

function EndsWith(const S: string; const Ch: char = ' '): boolean;

function StartsWith(const S: string; const Ch: char = ' '): boolean;

procedure StringListRemove(AList: TStringList; const AName: string);

function RemoveFirstSubstring(const S, SubStr: string; Reverse: boolean = False): string;

function ApplyCombiningChar(const AText: string; const ACombiningChar: string = #$0335): string;

function SameFloat(A, B: double; Eps: double): boolean;

{TIntegerArray}

procedure InsertAtPos(var A: TIntegerArray; Pos, Value: integer; Delta: integer = 0);

procedure DeleteAtPos(var A: TIntegerArray; Pos: integer);

function CloneArray(const Src: TIntegerArray): TIntegerArray;

procedure CopyToArray(var Dest: TIntegerArray; const Src: TIntegerArray);

const
  Brackets: array[0..17] of string = ('- [x]', '- [X]', '- [ ]', '- []', '-[x]', '-[X]', '-[ ]', '-[]', '[x]',
    '[X]', '[ ]', '[]', 'x ', '-x ', '- x ', 'X ', '-X ', '- X ');
  UnicodeMinusUTF8 = #$E2#$88#$92;
  MaxFloatStringLength = 15;
  MaxDT: TDateTime = 2958465.999988426; // 31.12.9999 23:59:59
  MaxTagLength = 500;

implementation

uses mathparser;

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
  ADateTime := 0;
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
  if (Value > MaxDT) then
    Value := 0;

  if (Value <> 0) then
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
  end
  else
    Result := string.Empty;
end;

function DateTimeToString(Value: TDateTime; ADisplayTime: boolean = True): string;
begin
  if (Value <> 0) then
  begin
    if (not ADisplayTime) then
      Result := FormatDateTime(FormatSettings.ShortDateFormat, Value)
    else
      Result := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat, Value);
  end
  else
    Result := string.Empty;
end;

function TryStrToFloatLimited(const S: string; out Value: double): boolean;
var
  FS: TFormatSettings;
begin
  if Length(S) > MaxFloatStringLength then
    Exit(False);
  Result := TryStrToFloat(S, Value);
  if (not Result) then
  begin
    FS.DecimalSeparator := '.';
    Result := TryStrToFloat(S, Value, FS);
    if (not Result) then
    begin
      FS.DecimalSeparator := ',';
      Result := TryStrToFloat(S, Value, FS);
    end;
  end;
end;

function FloatToString(Value: double): string;
begin
  Result := FloatToStr(Value);
end;

function FloatToString(Value: double; FS: TFormatSettings): string;
begin
  Result := FloatToStr(Value, FS);
end;

function SplitByFirstSpaces(const S: string; Count: integer = 1): TStringArray;
var
  SpacePos, i: integer;
  Remaining: string;
begin
  Result := nil;
  if Count < 1 then Count := 1;

  SetLength(Result, 0);
  Remaining := S;

  for i := 1 to Count do
  begin
    SpacePos := Pos(' ', Remaining);
    if SpacePos = 0 then
      Break;

    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Copy(Remaining, 1, SpacePos - 1);
    Remaining := Copy(Remaining, SpacePos + 1, Length(Remaining));
  end;

  // Add whatever is left as the last part
  SetLength(Result, Length(Result) + 1);
  Result[High(Result)] := Remaining;
end;

function StartsWithOperator(const S: string; out Op, Rest: string): boolean;
const
  Ops: array[0..8] of string = ('>=', '<=', '<>', '!=', '=', '>', '<', '!', '#');
var
  i: integer;
begin
  for i := 0 to High(Ops) do
    if StartsText(Ops[i], S) then
    begin
      Op := Ops[i];
      Rest := Trim(System.Copy(S, Length(Ops[i]) + 1, MaxInt));
      Exit(True);
    end;
  Op := string.Empty;
  Rest := S;
  Result := False;
end;

function StartsWithBracketAZ(const S: string): boolean;
var
  C: char;
begin
  Result := False;
  // String must be at least 4 characters long: "(A) "
  if Length(S) < 4 then
    Exit;

  // First char must be '('
  if S[1] <> '(' then
    Exit;

  // Second char must be A..Z
  C := S[2];
  if not (C in ['A'..'Z']) then
    Exit;

  // Third char must be ')'
  if S[3] <> ')' then
    Exit;

  // Fourth char must be space
  if S[4] <> ' ' then
    Exit;

  Result := True;
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

function DetectDone(const Input: string): boolean;
var
  i: integer;
  LowerInput: string;
const
  Brackets: array[0..5] of string = ('- [x]', '-[x]', '[x]', 'x ', '- x ', '-x ');
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

function CleanString(const Value: string): string;
begin
  Result := Value.Replace(#9, ' ');
end;

function CleanNumericExpression(Value: string): string;
var
  i: integer;
  C: char;
begin
  Result := string.Empty;
  Value := Value.Replace(UnicodeMinusUTF8, '-');
  Value := Value.Replace('.', DefaultFormatSettings.DecimalSeparator);
  Value := Value.Replace(',', DefaultFormatSettings.DecimalSeparator);
  for i := 1 to Length(Value) do
  begin
    C := Value[i];
    // allow digits and actions
    if C in ['0'..'9', '-', '+', '*', '/', '%', '^', '(', ')', DefaultFormatSettings.DecimalSeparator] then
      Result := Result + C;
  end;
end;

function CleanNumeric(Value: string): string;
var
  i: integer;
  C: char;
  ValTest: double;
begin
  Result := string.Empty;

  // Clean if expression
  Value := CleanNumericExpression(Value);

  // If numeric then simple return result
  if TryStrToFloat(Value, ValTest) then exit(Value);

  // Try to evaluate as a mathematical expression first
  Value := TMathParser.Eval(Value);

  // After evaluation clean manually
  for i := 1 to Length(Value) do
  begin
    C := Value[i];
    // allow digits, ASCII minus, and dot
    if C in ['0'..'9', '-', '.', ',', DefaultFormatSettings.DecimalSeparator] then
      Result := Result + C;
  end;
end;

function CleanAmount(const Value: string): string;
begin
  Result := Value.Replace(' ', string.empty).Trim;
end;

function TrimLeadingSpaces(const Input: string; MaxSpaces: integer = 1): string;
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

function TrimTrailingSpaces(const Input: string; MaxSpaces: integer = 1): string;
var
  i, SpaceCount, L: integer;
begin
  SpaceCount := 0;
  L := Length(Input);

  // Count trailing spaces, up to MaxSpaces
  for i := L downto 1 do
  begin
    if (Input[i] = ' ') and (SpaceCount < MaxSpaces) then
      Inc(SpaceCount)
    else
      Break;
  end;

  // Remove the trailing spaces
  Result := Copy(Input, 1, L - SpaceCount);
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

function IsUTF8Char(const S: string; CharIndex: integer; FindChar: string = ' '): boolean;
var
  ch: string;
begin
  Result := False;
  if (CharIndex < 1) or (CharIndex > UTF8Length(S)) then Exit;

  ch := UTF8Copy(S, CharIndex, 1);
  Result := (ch = FindChar);
end;

function ULower(Value: string): string;
begin
  Result := UTF8LowerCase(Value);
end;

function IsLetterOrDigit(ch: widechar): boolean;
begin
  Result := (ch in ['0'..'9', 'A'..'Z', 'a'..'z']) or (ch > #127);
end;

function RepeatString(const S: string; Count: integer): string;
var
  i: integer;
begin
  Result := string.Empty;
  for i := 1 to Count do
    Result := Result + S;
end;

function MaskTextWithBullets(const AText: string; ACanvas: TCanvas; ALineEnding: TLineEnding): string;
var
  Lines: TStringList;
  i, Count: integer;
  Bullet, Line: string;
begin
  Result := '';
  Lines := TStringList.Create;
  try
    // Split text into separate lines
    Lines.Text := AText;
    Bullet := #$2022 + ' '; // Unicode bullet with space

    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];
      if Trim(Line) <> string.Empty then
      begin
        // Calculate how many bullets fit in the width of this line
        Count := ACanvas.TextWidth(Line) div ACanvas.TextWidth(Bullet);
        if Count < 1 then Count := 1; // always at least one bullet
        Line := RepeatString(Bullet, Count);
      end;
      // Restore line breaks
      if Result = string.Empty then
        Result := Line
      else
        Result := Result + ALineEnding.Value + Line;
    end;
  finally
    Lines.Free;
  end;
end;

function RenderWordCanvas(const AWord: string; const FontName: string = 'Monospace'; FontSize: integer = 12): string;
var
  bmp: TBitmap;
  x, y: integer;
  line, res: unicodestring;
  col: TColor;
  r, g, b: byte;
  luminance: integer;
  char: boolean;
begin
  if Length(AWord) > 1024 then exit(AWord);

  bmp := TBitmap.Create;
  try
    bmp.Canvas.Font.Name := FontName;
    bmp.Canvas.Font.Size := FontSize;
    bmp.Canvas.Font.Color := clBlack;

    // use exact size
    bmp.SetSize(bmp.Canvas.TextWidth(AWord), bmp.Canvas.TextHeight(AWord));

    // fill background
    bmp.Canvas.Brush.Color := clWhite;
    bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);

    // draw text into rect
    bmp.Canvas.TextRect(Rect(0, 0, bmp.Width, bmp.Height), 0, 0, AWord);

    // scan pixels
    Res := string.Empty;
    for y := 0 to bmp.Height - 1 do
    begin
      line := string.Empty;
      char := False;
      for x := 0 to bmp.Width - 1 do
      begin
        col := bmp.Canvas.Pixels[x, y];
        r := Red(col);
        g := Green(col);
        b := Blue(col);
        luminance := (r + g + b) div 3;

        if luminance < 128 then
        begin
          line := line + #$2593;
          char := True;
        end
        else if luminance < 192 then
        begin
          line := line + #$2592;
          char := True;
        end
        else
        if luminance < 216 then
        begin
          line := line + #$2591;
          char := True;
        end
        else
        {$IFDEF UNIX}
          line := line + #$2591;
          {$ELSE}
          line := line + #$2003;
        {$ENDIF}
      end;

      if char then
        Res := Res + line + sLineBreak;
    end;

    Result := UTF8Encode(Res);
  finally
    bmp.Free;
  end;
end;

function IsEmail(const S: string): boolean;
var
  RE: TRegExpr;
  EmailToCheck: string;
begin
  // Remove mailto: prefix if present
  if LowerCase(Copy(S, 1, 7)) = 'mailto:' then
    EmailToCheck := Copy(S, 8, MaxInt)
  else
    EmailToCheck := S;

  // Improved regular expression for email validation
  RE := TRegExpr.Create;
  try
    RE.Expression := '^(?i)[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}$';
    Result := RE.Exec(EmailToCheck);
  finally
    RE.Free;
  end;
end;

function IsURL(const S: string): boolean;
var
  RE: TRegExpr;
begin
  RE := TRegExpr.Create;
  try
    RE.Expression :=
      '^(?i)' + // case-insensitive
      '(' + '(https?|ftp)://[^\s/$.?#].[^\s]*' + '|' + '.*/.*' + ')$';
    Result := RE.Exec(S);
  finally
    RE.Free;
  end;
end;

function HasScheme(const URL: string): boolean;
begin
  // Check if URL starts with any scheme (protocol)
  Result := (Pos('://', URL) > 0) or (Pos('mailto:', LowerCase(URL)) = 1) or (Pos('tel:', LowerCase(URL)) = 1) or
    (Pos('sms:', LowerCase(URL)) = 1);
end;

function JoinArrayText(const Parts: TStringArray; StartIndex: integer = 0; const Separator: string = ','; EndIndex: integer = -1): string;
var
  i: integer;
begin
  Result := string.Empty;
  if Length(Parts) = 0 then Exit;

  if StartIndex < 0 then StartIndex := 0;
  if StartIndex > High(Parts) then Exit;

  for i := StartIndex to ifthen(EndIndex < 0, High(Parts), EndIndex) do
  begin
    Result += Parts[i];
    if i < High(Parts) then
      Result += Separator;
  end;
end;

function RemoveBacktickBlocks(const S: string): string;
const
  MaxTagLength = 50; // Maximum allowed length for backtick blocks
var
  i, Start: integer;
  WordStr: string;
  NewS: string;
  HasRelevantChars: boolean;
begin
  Result := S; // Initialize result with original string

  // Optimization: skip processing if string doesn't contain backticks
  HasRelevantChars := Pos('`', S) > 0;
  if not HasRelevantChars then
    Exit;

  i := 1;
  NewS := string.Empty; // Initialize new string for building cleaned result

  while i <= Length(S) do
  begin
    // Check for backtick character
    if S[i] = '`' then
    begin
      Start := i; // Mark the start position of potential block
      Inc(i);

      // Scan until closing backtick, line break, or max length reached
      while (i <= Length(S)) and (S[i] <> '`') and (i - Start < MaxTagLength) do
      begin
        if S[i] in [#13, #10] then
          Break; // Stop scanning at line breaks
        Inc(i);
      end;

      // Check if we found a valid closing backtick
      if (i <= Length(S)) and (S[i] = '`') then
      begin
        WordStr := Copy(S, Start, i - Start + 1); // Extract the block content

        // Validate block: length requirements and no line breaks
        if (Length(WordStr) > 2) and (Length(WordStr) <= MaxTagLength) and (Pos(#13, WordStr) = 0) and
          (Pos(#10, WordStr) = 0) then
        begin
          // Remove single preceding space from NewS if present
          if (Length(NewS) > 0) and (NewS[Length(NewS)] = ' ') then
            SetLength(NewS, Length(NewS) - 1);

          // Skip adding this block to NewS (effectively removing it)
          Inc(i); // Move past the closing backtick
          Continue; // Skip to next iteration without processing current position
        end;
      end;
      // If no valid backtick block found, fall through to normal character processing
    end;

    // Add current character to result string (not part of valid backtick block)
    NewS := NewS + S[i];
    Inc(i);
  end;

  // Return cleaned version
  Result := NewS;
end;

procedure FillTagsFromString(var List: TStringList; var S: string; Backtick: boolean = False);
var
  i, Start: integer;
  WordStr: string;
  HasAlphaNum: boolean;
  Num: double;
  NewS: string; // Used to build modified string when Backtick is True
  HasRelevantChars: boolean; // Optimization flag
begin
  // Initial optimization check: skip processing if string doesn't contain relevant characters
  if Backtick then
    HasRelevantChars := Pos('`', S) > 0
  else
    HasRelevantChars := (Pos('@', S) > 0) or (Pos('#', S) > 0) or (Pos('%', S) > 0) or (Pos('+', S) > 0) or (Pos('$', S) > 0);

  if not HasRelevantChars then
    Exit;

  i := 1;
  NewS := string.Empty; // Initialize new string for Backtick mode

  while i <= Length(S) do
  begin
    // Check for backtick tag first
    if (S[i] = '`') and Backtick then
    begin
      Start := i;
      Inc(i);
      // Scan until closing backtick, line break, or max length
      while (i <= Length(S)) and (S[i] <> '`') and (i - Start < MaxTagLength) do
      begin
        if S[i] in [#13, #10] then
          Break;
        Inc(i);
      end;

      // Check if we found a valid closing backtick
      if (i <= Length(S)) and (S[i] = '`') then
      begin
        WordStr := Copy(S, Start, i - Start + 1);
        // Add the tag if it meets length requirements and has no line breaks
        if (Length(WordStr) > 2) and (Length(WordStr) <= MaxTagLength) and (Pos(#13, WordStr) = 0) and
          (Pos(#10, WordStr) = 0) then
        begin
          List.Add(StringReplace(WordStr, '`', '', [rfReplaceAll]));

          // Remove single preceding space from NewS if present
          if (Length(NewS) > 0) and (NewS[Length(NewS)] = ' ') then
            SetLength(NewS, Length(NewS) - 1);

          // Skip adding this content to NewS (effectively removing it)
          Inc(i); // Move past the closing backtick
          Continue; // Skip to next iteration without adding to NewS
        end;
      end;
      // If no valid backtick tag found, fall through to normal processing
    end;

    // Process regular prefix tags when Backtick is False, or when in Backtick mode but no valid backtick tag was found
    if (not Backtick) and (S[i] in ['@', '#', '%', '+', '$']) and ((i = 1) or (S[i - 1] in [#32, #13, #10])) then
    begin
      Start := i;
      Inc(i);
      // Collect alphanumeric characters, hyphens, and underscores
      // Also check that we have at least one alphanumeric character
      HasAlphaNum := False;
      while (i <= Length(S)) and (S[i] in ['0'..'9', 'A'..'Z', 'a'..'z', '-', '_']) and (i - Start < MaxTagLength) do
      begin
        if S[i] in ['0'..'9', 'A'..'Z', 'a'..'z'] then
          HasAlphaNum := True;
        Inc(i);
      end;

      WordStr := Copy(S, Start, i - Start);
      // Add tag if length is valid and contains at least one alphanumeric character
      if (Length(WordStr) > 1) and (Length(WordStr) <= MaxTagLength) and HasAlphaNum and not TryStrToFloat(WordStr, Num) then
        List.Add(WordStr);
    end
    else
    begin
      // Add current character to NewS when in Backtick mode
      if Backtick then
        NewS := NewS + S[i];
      Inc(i);
    end;
  end;

  // Update original string with modified version when in Backtick mode
  if Backtick then
    S := NewS;
end;

procedure ReplaceListStartsWith(var List: TStringList; const S: string);
var
  i: integer;
  LowerS, LowerItem: string;
begin
  LowerS := LowerCase(S);
  // Iterate backwards to safely remove items while looping
  for i := List.Count - 1 downto 0 do
  begin
    LowerItem := LowerCase(List[i]);
    // Check if string S starts with the list item (case insensitive)
    if (Length(LowerItem) <= Length(LowerS)) and (Copy(LowerS, 1, Length(LowerItem)) = LowerItem) then
      List.Delete(i);
  end;

  // Add the string to the list
  List.Add(S);
end;

function StringListToBacktickString(List: TStringList; LeadingSpace: boolean = True): string;
var
  i: integer;
  SB: TStringBuilder;
begin
  if List.Count = 0 then
  begin
    Result := string.Empty;
    Exit;
  end;

  SB := TStringBuilder.Create;
  try
    for i := 0 to List.Count - 1 do
    begin
      if LeadingSpace then
        SB.Append(' ');
      SB.Append('`');
      SB.Append(List[i]);
      SB.Append('`');
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

procedure AddPrefixTags(List: TStringList);
var
  i: integer;
  Tag, Prefix: string;
  PrefixesToAdd: TStringList;
begin
  // Create temporary list to store prefixes that need to be added
  PrefixesToAdd := TStringList.Create;
  try
    // First, collect all prefixes that need to be added
    for i := 0 to List.Count - 1 do
    begin
      Tag := List[i];
      // Check if tag contains colon (indicates it has a prefix)
      if Pos(':', Tag) > 0 then
      begin
        // Extract prefix (text before the colon)
        Prefix := Copy(Tag, 1, Pos(':', Tag) - 1);
        // Add prefix to temporary list if it doesn't exist in original list
        // and hasn't been already added to the temporary list
        if (List.IndexOf(Prefix) = -1) and (PrefixesToAdd.IndexOf(Prefix) = -1) then
          PrefixesToAdd.Add(Prefix);
      end;
    end;

    // Add all collected prefixes to the original list at once
    List.AddStrings(PrefixesToAdd);
  finally
    // Always free the temporary list to avoid memory leaks
    PrefixesToAdd.Free;
  end;
end;

function StringListsEqual(A, B: TStrings): boolean;
var
  i: integer;
begin
  // Compare count first
  if A.Count <> B.Count then
    Exit(False);

  // Compare each line
  for i := 0 to A.Count - 1 do
  begin
    // Compare text values
    if A[i] <> B[i] then
      Exit(False);
  end;

  Result := True;
end;

function GetBeforeColon(const S: string): string;
var
  p: integer;
begin
  // Find ":" position
  p := Pos(':', S); // returns 0 if not found

  if p > 0 then
    Result := Copy(S, 1, p - 1)
  else
    Result := S; // no ":" → return whole string
end;

procedure ParseGroupName(const Value: string; out NameText, HintText: string);
var
  PosCommentStart: integer;
begin
  NameText := string.Empty;
  HintText := string.Empty;

  // find the start of the comment
  PosCommentStart := Pos('//', Value);
  if PosCommentStart > 0 then
  begin
    // name is everything before //
    NameText := Trim(Copy(Value, 1, PosCommentStart - 1));
    HintText := Trim(Copy(Value, PosCommentStart + 2, MaxInt));
  end
  else
  begin
    // if // not found, the whole Value is the name
    NameText := Trim(Value);
  end;
end;

function ReplaceLineBreaks(const S: string): string;
var
  ResultStr: string;
begin
  ResultStr := StringReplace(S, sLineBreak, '<br>', [rfReplaceAll]);
  ResultStr := StringReplace(ResultStr, #13, '<br>', [rfReplaceAll]);
  ResultStr := StringReplace(ResultStr, #10, '<br>', [rfReplaceAll]);
  Result := ResultStr;
end;

function DeleteFirstChar(const S: string; const Ch: char): string;
begin
  if (S <> '') and (S[1] = Ch) then
    Result := Copy(S, 2, MaxInt)
  else
    Result := S;
end;

function EndsWith(const S: string; const Ch: char = ' '): boolean;
begin
  // Return True if string ends with specified char
  Result := (S <> string.Empty) and (S[Length(S)] = Ch);
end;

function StartsWith(const S: string; const Ch: char = ' '): boolean;
begin
  // Return True if string starts with specified char
  Result := (S <> string.Empty) and (S[1] = Ch);
end;

procedure StringListRemove(AList: TStringList; const AName: string);
var
  Index: integer;
begin
  Index := AList.IndexOf(AName);
  while Index <> -1 do
  begin
    AList.Delete(Index);
    Index := AList.IndexOf(AName);
  end;
end;

function RemoveFirstSubstring(const S, SubStr: string; Reverse: boolean = False): string;
var
  Position, I: integer;
begin
  Result := S;

  // If substring is empty, return original string
  if SubStr = '' then
    Exit;

  if not Reverse then
  begin
    // Find first occurrence from start
    Position := Pos(SubStr, S);
    if Position > 0 then
      Result := Copy(S, 1, Position - 1) + Copy(S, Position + Length(SubStr), MaxInt);
  end
  else
  begin
    // Find first occurrence from end
    Position := 0;
    // Search backwards through the string
    for I := Length(S) - Length(SubStr) + 1 downto 1 do
    begin
      if Copy(S, I, Length(SubStr)) = SubStr then
      begin
        Position := I;
        Break;
      end;
    end;

    if Position > 0 then
      Result := Copy(S, 1, Position - 1) + Copy(S, Position + Length(SubStr), MaxInt);
  end;
end;

function ApplyCombiningChar(const AText: string; const ACombiningChar: string = #$0335): string;
var
  I, Len: integer;
  Ch: string;
begin
  Result := '';
  Len := UTF8Length(AText);

  for I := 1 to Len do
  begin
    Ch := UTF8Copy(AText, I, 1);
    Result := Result + Ch;

    if (Ch <> #10) and (Ch <> #13) then
      Result := Result + ACombiningChar;
  end;
end;

function SameFloat(A, B: double; Eps: double): boolean;
begin
  // Compare floats with epsilon
  Result := Abs(A - B) < Eps;
end;

procedure InsertAtPos(var A: TIntegerArray; Pos, Value: integer; Delta: integer = 0);
var
  i, Len: integer;
begin
  Len := Length(A);
  if (Pos < 0) or (Pos > Len) then
    Exit; // Out of bounds

  // Increase array size
  SetLength(A, Len + 1);

  // Shift elements to the right
  for i := Len - 1 downto Pos do
    A[i + 1] := A[i];

  // Insert new value
  A[Pos] := Value;

  // Increase all following elements by Delta
  for i := Pos + 1 to High(A) do
    A[i] := A[i] + Delta;
end;

procedure DeleteAtPos(var A: TIntegerArray; Pos: integer);
var
  i, Len: integer;
begin
  Len := Length(A);
  if (Pos < 0) or (Pos >= Len) then
    Exit; // Out of bounds

  // Shift left
  for i := Pos to Len - 2 do
    A[i] := A[i + 1];

  // Decrease array size
  SetLength(A, Len - 1);
end;

function CloneArray(const Src: TIntegerArray): TIntegerArray;
begin
  Result := Copy(Src, 0, Length(Src));
end;

procedure CopyToArray(var Dest: TIntegerArray; const Src: TIntegerArray);
var
  CopyCount, i: integer;
begin
  // Determine how many elements to copy: take the smaller of Dest length and Src length
  CopyCount := Min(Length(Dest), Length(Src));
  for i := 0 to CopyCount - 1 do
    Dest[i] := Src[i];
end;

end.
