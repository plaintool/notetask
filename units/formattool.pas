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
  LazUTF8,
  lineending;

type
  TIntegerArray = array of integer;

function TextToStringList(const Content: string; TrimEnd: boolean = False): TStringList;

function TryStrToDateTimeISO(const S: string; out ADateTime: TDateTime): boolean;

function DateTimeToStringISO(Value: TDateTime; ADisplayTime: boolean = True): string;

function DateTimeToString(Value: TDateTime; ADisplayTime: boolean = True): string;

function TryStrToFloatLimited(const S: string; out Value: double): boolean;

function FloatToString(Value: double): string;

function SplitByFirstSpaces(const S: string; Count: integer = 1): TStringArray;

function StartsWithBracketAZ(const S: string): boolean;

function IsBracket(const Input: string): boolean;

function RemoveBrackets(const S: string): string;

function DetectDone(const Input: string): boolean;

function CleanString(const Value: string): string;

function CleanNumericExpression(Value: string): string;

function CleanNumeric(Value: string): string;

function CleanAmount(const Value: string): string;

function TrimLeadingSpaces(const Input: string; MaxSpaces: integer = 2): string;

function PosExReverse(const SubStr, S: unicodestring; Offset: SizeUint): SizeInt;

function EncodeUrl(const url: string): string;

function GetConsoleEncoding: string;

function IsUTF8Char(const S: string; CharIndex: integer; FindChar: string = ' '): boolean;

function IsLetterOrDigit(ch: widechar): boolean;

function RepeatString(const S: string; Count: integer): string;

function MaskTextWithBullets(const AText: string; ACanvas: TCanvas; ALineEnding: TLineEnding): string;

function JoinArrayText(const Parts: TStringArray; StartIndex: integer = 0; const Separator: string = ','): string;

procedure InsertAtPos(var A: TIntegerArray; Pos, Value: integer);

procedure DeleteAtPos(var A: TIntegerArray; Pos: integer);

function CloneArray(const Src: TIntegerArray): TIntegerArray;

function RenderWordCanvas(const AWord: string; const FontName: string = 'Monospace'; FontSize: integer = 12): string;

const
  Brackets: array[0..17] of string = ('- [x]', '- [X]', '- [ ]', '- []', '-[x]', '-[X]', '-[ ]', '-[]', '[x]',
    '[X]', '[ ]', '[]', 'x ', '-x ', '- x ', 'X ', '-X ', '- X ');
  UnicodeMinusUTF8 = #$E2#$88#$92;
  MaxFloatStringLength = 15;
  MaxDT: TDateTime = 2958465.999988426; // 31.12.9999 23:59:59

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
begin
  if Length(S) > MaxFloatStringLength then
    Exit(False);
  Result := TryStrToFloat(S, Value);
end;

function FloatToString(Value: double): string;
begin
  Result := FloatToStr(Value);
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
  for i := 1 to Length(Value) do
  begin
    C := Value[i];
    // allow digits and actions
    if C in ['0'..'9', '-', '+', '*', '/', '%', '^', '(', ')', '.'] then
      Result := Result + C
    else if C = ',' then
      Result := Result + '.'; // replace comma with dot
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

function TrimLeadingSpaces(const Input: string; MaxSpaces: integer = 2): string;
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

function IsUTF8Char(const S: string; CharIndex: integer; FindChar: string = ' '): boolean;
var
  ch: string;
begin
  Result := False;
  if (CharIndex < 1) or (CharIndex > UTF8Length(S)) then Exit;

  ch := UTF8Copy(S, CharIndex, 1);
  Result := (ch = FindChar);
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

function JoinArrayText(const Parts: TStringArray; StartIndex: integer = 0; const Separator: string = ','): string;
var
  i: integer;
begin
  Result := string.Empty;
  if Length(Parts) = 0 then Exit;

  if StartIndex < 0 then StartIndex := 0;
  if StartIndex > High(Parts) then Exit;

  for i := StartIndex to High(Parts) do
  begin
    Result += Parts[i];
    if i < High(Parts) then
      Result += Separator;
  end;
end;

procedure InsertAtPos(var A: TIntegerArray; Pos, Value: integer);
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

        if luminance < 150 then
        begin
          line := line + #$2593;
          char := True;
        end
        else if luminance < 200 then
        begin
          line := line + #$2592;
          char := True;
        end
        else
          line := line + #$2591;
      end;

      if char then
        Res := Res + line + sLineBreak;
    end;

    Result := UTF8Encode(Res);
  finally
    bmp.Free;
  end;
end;

end.
