//-----------------------------------------------------------------------------------
//  Notetask © 2024 by Alexander Tverskoy
//  Licensed under the GNU General Public License, Version 3 (GPL-3.0)
//  You may obtain a copy of the License at https://www.gnu.org/licenses/gpl-3.0.html
//-----------------------------------------------------------------------------------

unit mathparser;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  Math;

type
  TMathParser = class
  public
    class function MaxPrecision(const Expr: string): integer;
    class function Eval(const Expr: string): string;
  end;

implementation

class function TMathParser.MaxPrecision(const Expr: string): integer;
var
  i, DotPos, Digits: integer;
begin
  Result := 0;
  i := 1;
  while i <= Length(Expr) do
  begin
    if Expr[i] in ['0'..'9'] then
    begin
      // Find number end
      DotPos := 0;
      Digits := 0;
      while (i <= Length(Expr)) and (Expr[i] in ['0'..'9', '.', ',', DefaultFormatSettings.DecimalSeparator]) do
      begin
        if Expr[i] in ['.', ',', DefaultFormatSettings.DecimalSeparator] then
          DotPos := i
        else if DotPos > 0 then
          Inc(Digits);
        Inc(i);
      end;
      if Digits > Result then
        Result := Digits;
    end
    else
      Inc(i);
  end;
end;

class function TMathParser.Eval(const Expr: string): string;
var
  i: integer;

// Forward declarations for recursive functions
  function ParseExpr: double; forward;
  function ParseTerm: double; forward;
  function ParseFactor: double; forward;

  // Skip spaces
  procedure SkipSpaces;
  begin
    while (i <= Length(Expr)) and (Expr[i] = ' ') do Inc(i);
  end;

  // Parse number (integer or float)
  function ParseNumber: double;
  var
    StartPos: integer;
    S: string;
  begin
    StartPos := i;
    while (i <= Length(Expr)) and (Expr[i] in ['0'..'9', '.', ',', DefaultFormatSettings.DecimalSeparator]) do
      Inc(i);
    S := Copy(Expr, StartPos, i - StartPos);
    //S := StringReplace(S, ',', '.', [rfReplaceAll]);
    Result := StrToFloatDef(S, 0);
  end;

  // Primary factor: number, parentheses, unary minus
  function ParsePrimary: double;
  var
    Sign: double;
  begin
    SkipSpaces;
    Sign := 1;
    if (i <= Length(Expr)) and (Expr[i] = '-') then
    begin
      Sign := -1;
      Inc(i);
    end;

    SkipSpaces;
    if (i <= Length(Expr)) and (Expr[i] = '(') then
    begin
      Inc(i);
      Result := ParseExpr;
      if (i <= Length(Expr)) and (Expr[i] = ')') then Inc(i)
      else
        exit;
    end
    else
      Result := ParseNumber;

    Result := Result * Sign;

    SkipSpaces;

    // Handle percent %
    if (i <= Length(Expr)) and (Expr[i] = '%') then
    begin
      Result := Result / 100;
      Inc(i);
    end;
  end;

  // Factor: handles exponentiation
  function ParseFactor: double;
  var
    Base, Exponent: double;
  begin
    Base := ParsePrimary; // only number or parentheses
    SkipSpaces;
    while (i <= Length(Expr)) and (Expr[i] = '^') do
    begin
      Inc(i);
      Exponent := ParsePrimary; // right operand for ^
      Base := Power(Base, Exponent);
      SkipSpaces;
    end;
    Result := Base;
  end;

  // Parse term (* and /)
  function ParseTerm: double;
  var
    Tmp: double;
  begin
    Result := ParseFactor;
    SkipSpaces;
    while i <= Length(Expr) do
    begin
      case Expr[i] of
        '*': begin
          Inc(i);
          Result := Result * ParseFactor;
        end;
        '/': begin
          Inc(i);
          Tmp := ParseFactor;
          if Tmp = 0 then exit;
          Result := Result / Tmp;
        end;
        else
          Exit;
      end;
      SkipSpaces;
    end;
  end;

  // Parse expression (+ and -)
  function ParseExpr: double;
  begin
    Result := ParseTerm;
    SkipSpaces;
    while i <= Length(Expr) do
    begin
      case Expr[i] of
        '+': begin
          Inc(i);
          Result := Result + ParseTerm;
        end;
        '-': begin
          Inc(i);
          Result := Result - ParseTerm;
        end;
        else
          Exit;
      end;
      SkipSpaces;
    end;
  end;

var
  R: double;
  Precision: integer;
  FormatStr: string;
begin
  Result := string.Empty;
  i := 1;
  try
    R := ParseExpr;
    SkipSpaces;

    Precision := Max(MaxPrecision(Expr), 5);
    FormatStr := '0.' + StringOfChar('#', Precision);

    Result := FormatFloat(FormatStr, R);
  except
    Result := string.Empty; // Return empty string on any error
  end;
end;

end.
