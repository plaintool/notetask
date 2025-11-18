//-----------------------------------------------------------------------------------
//  TTagEdit © 2025 by Alexander Tverskoy
//  https://github.com/plaintool/TagEdit
//  Licensed under the MIT License
//  You may obtain a copy of the License at https://opensource.org/licenses/MIT
//-----------------------------------------------------------------------------------

unit TagColorItems;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  { TTagColorItem }
  TTagColorItem = class(TCollectionItem)
  private
    FTagName: string;
    FColor: TColor;
    procedure SetTag(const AValue: string);
    procedure SetColor(AValue: TColor);
  published
    property TagName: string read FTagName write SetTag;
    property Color: TColor read FColor write SetColor default clNone;
  end;

  { TTagColorItems }
  TTagColorItems = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TTagColorItem;
    procedure SetItem(Index: integer; const Value: TTagColorItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add(const ATag: string; AColor: TColor): TTagColorItem;
    function IndexOf(const ATag: string): integer;
    property Items[Index: integer]: TTagColorItem read GetItem write SetItem; default;
  end;

implementation

{ TTagColorItem }

procedure TTagColorItem.SetTag(const AValue: string);
begin
  if FTagName <> AValue then
    FTagName := AValue;
end;

procedure TTagColorItem.SetColor(AValue: TColor);
begin
  if FColor <> AValue then
    FColor := AValue;
end;

{ TTagColorItems }

constructor TTagColorItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TTagColorItem);
end;

function TTagColorItems.GetItem(Index: integer): TTagColorItem;
begin
  Result := TTagColorItem(inherited GetItem(Index));
end;

procedure TTagColorItems.SetItem(Index: integer; const Value: TTagColorItem);
begin
  inherited SetItem(Index, Value);
end;

function TTagColorItems.Add(const ATag: string; AColor: TColor): TTagColorItem;
begin
  Result := TTagColorItem(inherited Add);
  Result.FTagName := ATag;
  Result.FColor := AColor;
end;

function TTagColorItems.IndexOf(const ATag: string): integer;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].TagName = ATag then
      Exit(i);
  Result := -1;
end;

end.
