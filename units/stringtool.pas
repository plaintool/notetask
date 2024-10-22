//---------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the MIT License
//---------------------------------------------------------------------

unit stringtool;

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

uses
  Classes,
  SysUtils;

function TextToStringList(const Content: string): TStringList;

function IsBracket(const Input: string): Boolean;

function RemoveBrackets(const S: string): string;

function DetectDone(const Input: string): boolean;

function PosExReverse(const SubStr, S: unicodestring; Offset: SizeUint): SizeInt;

function EncodeUrl(const url: string): string;

const
  Brackets: array[0..11] of string = ('- [x]', '- [X]', '- [ ]', '- []', '-[x]', '-[X]', '-[ ]', '-[]', '[x]', '[X]', '[ ]', '[]');

implementation

function TextToStringList(const Content: string): TStringList;
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  // Create a new instance of TStringList
  try
    StringList.Text := Content; // Load text into TStringList

    // Check if the file ends with a new line and the last line is not empty
    if (Content <> '') and (Content[Length(Content)] in [#10, #13]) then
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

function IsBracket(const Input: string): Boolean;
var
  TrimmedInput: string;
  I: Integer;
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

end.
