//-----------------------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the GNU General Public License, Version 3 (GPL-3.0)
//  You may obtain a copy of the License at https://www.gnu.org/licenses/gpl-3.0.html
//-----------------------------------------------------------------------------------

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
    function Value: string;
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

function TLineEnding.Value: string;
begin
  if Self = WindowsCRLF then
    Result := sLineBreak // Windows: CRLF
  else if Self = UnixLF then
    Result := #10 // Unix: LF
  else if Self = MacintoshCR then
    Result := #13 // Macintosh: CR
  else
    Result := ''; // Unknown
end;

end.
