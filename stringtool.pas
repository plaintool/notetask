unit stringtool;

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

uses
  Classes,
  SysUtils;

function TextToStringList(const TextContent: string): TStringList;

function RemoveBrackets(const S: string): string;

function PosExReverse(const SubStr, S: unicodestring; Offset: SizeUint): SizeInt;

implementation

function TextToStringList(const TextContent: string): TStringList;
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  // Create a new instance of TStringList
  try
    StringList.Text := TextContent; // Load text into TStringList
    Result := StringList; // Return TStringList
  except
    StringList.Free; // Free memory on error
    raise; // Re-throw the exception
  end;
end;

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

function PosExReverse(const SubStr, S: unicodestring; Offset: SizeUint): SizeInt;
var
  i, MaxLen, SubLen: SizeInt;
  SubFirst: widechar;
  pc: pwidechar;
begin
  Result := 0; // Initialize result to 0 (not found)
  SubLen := Length(SubStr); // Get length of the substring

  // Check if the substring is not empty and Offset is valid
  if (SubLen > 0) and (Offset > 0) and (Offset <= SizeUint(Length(S))) then
  begin
    MaxLen := Length(S) - SubLen + 1; // Adjust max starting index to include end of the string
    SubFirst := SubStr[1]; // Get the first character of the substring

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

end.
