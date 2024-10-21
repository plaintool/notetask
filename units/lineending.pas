//----------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under CC BY-NC-SA 4.0
//  Full license text: https://creativecommons.org/licenses/by-nc-sa/4.0/
//----------------------------------------------------------------------

unit lineending;

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

type
  TLineEnding = class
  private
  class var FWindowsCRLF: TLineEnding;
  class var FUnixLF: TLineEnding;
  class var FMacintoshCR: TLineEnding;
  class var FUnknown: TLineEnding;
    class function GetWindowsCRLF: TLineEnding; static;
    class function GetUnixLF: TLineEnding; static;
    class function GetMacintoshCR: TLineEnding; static;
    class function GetUnknown: TLineEnding; static;
  public
    class property WindowsCRLF: TLineEnding read GetWindowsCRLF;
    class property UnixLF: TLineEnding read GetUnixLF;
    class property MacintoshCR: TLineEnding read GetMacintoshCR;
    class property Unknown: TLineEnding read GetUnknown;
    function ToString: string; override;
  end;

implementation

class function TLineEnding.GetWindowsCRLF: TLineEnding;
begin
  if not Assigned(FWindowsCRLF) then
    FWindowsCRLF := TLineEnding.Create;
  Result := FWindowsCRLF;
end;

class function TLineEnding.GetUnixLF: TLineEnding;
begin
  if not Assigned(FUnixLF) then
    FUnixLF := TLineEnding.Create;
  Result := FUnixLF;
end;

class function TLineEnding.GetMacintoshCR: TLineEnding;
begin
  if not Assigned(FMacintoshCR) then
    FMacintoshCR := TLineEnding.Create;
  Result := FMacintoshCR;
end;

class function TLineEnding.GetUnknown: TLineEnding;
begin
  if not Assigned(FUnknown) then
    FUnknown := TLineEnding.Create;
  Result := FUnknown;
end;

function TLineEnding.ToString: string;
begin
  if Self = WindowsCRLF then
    Result := 'Windows (CRLF)'
  else if Self = UnixLF then
    Result := 'Unix (LF)'
  else if Self = MacintoshCR then
    Result := 'Macintosh (CR)'
  else
    Result := 'Unknown';
end;

end.
