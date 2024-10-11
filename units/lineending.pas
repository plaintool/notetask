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
  public
  class var WindowsCRLF: TLineEnding;
  class var UnixLF: TLineEnding;
  class var MacintoshCR: TLineEnding;
  class var Unknown: TLineEnding;
    class constructor Create;
    function ToString: string; override;
  end;

implementation

class constructor TLineEnding.Create;
begin
  WindowsCRLF := TLineEnding.Create;
  UnixLF := TLineEnding.Create;
  MacintoshCR := TLineEnding.Create;
  Unknown := TLineEnding.Create;
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
