//-----------------------------------------------------------------------------------
//  TTagEdit © 2025 by Alexander Tverskoy
//  Licensed under the MIT License
//  You may obtain a copy of the License at https://opensource.org/licenses/MIT
//-----------------------------------------------------------------------------------

unit TagEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Controls,
  StdCtrls,
  Graphics,
  Math,
  Types,
  Forms,
  Dialogs,
  LCLType,
  LCLIntf,
  TagColorItems;

type
  TTagEvent = procedure(Sender: TObject; const TagText: string) of object;
  TTagPopupEvent = procedure(Sender: TObject; const TagText: string; var Handled: boolean) of object;

  TTagEdit = class(TCustomControl)
  private
    FTags: TStringList;
    FTagRects: array of TRect;

    FEdit: TEdit;
    FColor: TColor;
    FTagColor: TColor;
    FTagSuffixColor: TColor;
    FTagBorderColor: TColor;
    FBorderColor: TColor;
    FTagHoverColor: TColor;
    FDragIndicatorColor: TColor;
    FTagColors: TTagColorItems;

    FAutoSizeHeight: boolean;
    FAllowReorder: boolean;
    FTagHoverUnderline: boolean;
    FCloseButtons: boolean;
    FCloseButtonOnHover: boolean;
    FReadOnly: boolean;
    FEnabled: boolean;

    FAutoColorBrigtness: integer;
    FBorderWidth: integer;
    FRoundCorners: integer;
    FTagBorderWidth: integer;
    FEditMinWidth: integer;

    FDragging: boolean;
    FDragIndex: integer;
    FDropIndex: integer;
    FUpdatingEdit: boolean;
    FHoverIndex: integer;
    FMouseDownPos: TPoint;
    FMouseDownIndex: integer;

    FRemoveConfirm: boolean;
    FRemoveConfirmMessage: string;
    FRemoveConfirmTitle: string;

    FFont: TFont;
    FParentFont: boolean;

    FOnTagAdd: TTagEvent;
    FOnTagRemove: TTagEvent;
    FOnTagClick: TTagEvent;
    FOnTagPopup: TTagPopupEvent;
    FOnChange: TNotifyEvent;

    procedure EditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure EditExit(Sender: TObject);
    function GetTags: TStringList;
    procedure SetTags(Value: TStringList);
    procedure SetFont(Value: TFont);
    procedure SetParentFont(Value: boolean);
    procedure SetReadOnly(Value: boolean);
    procedure TagsChanged(Sender: TObject);
    function RemovalConfirmed(idx: integer): boolean;
    procedure DrawTags;
    function GetTagHeight: integer;
    function GetTagRect(Index: integer): TRect;
    function TagAtPos(const P: TPoint): integer;
    procedure UpdateEditPosition;
    procedure UpdateHoverState(X, Y: integer);
    function CoalesceInt(const A, B: integer; const C: integer = 0): integer;
    procedure SetAutoSizeHeight(Value: boolean);
    procedure UpdateAutoHeight;
    procedure SetTagColors(Value: TTagColorItems);
    function Scale(const AValue: integer): integer;
    function RandTagColor(const TagName: string; BrightnessPercent: integer = 70): TColor;
    function FindTagColor(const S: string): TColor;
    function GetContrastTextColor(BackColor, FontColor: TColor; MidLevel: integer = 128): TColor;
    procedure HandlePopupMenu(Sender: TObject);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseLeave; override;
    procedure SetEnabled(Value: boolean); override;
    procedure Resize; override;
    procedure SetColor(Value: TColor); override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddTag(const ATag: string);
    procedure RemoveTag(const ATag: string);
    property EditBox: TEdit read FEdit;
    procedure ParentFontChange(Sender: TObject);
    procedure FontChanged(Sender: TObject); override;
    procedure SetParent(AParent: TWinControl); override;
    function CalculateAutoHeight: integer;
    procedure ScaleFontsPPI(const AToPPI: integer; const AProportion: double); override;
    procedure FixDesignFontsPPI(const ADesignTimePPI: integer); override;
    function Focused: boolean; override;
  published
    property Align;
    property Anchors;
    property Visible;
    property ShowHint;
    property PopupMenu;
    property DoubleBuffered;
    property ParentDoubleBuffered;
    property Height default 32;
    property Width default 300;
    property Tag default 0;
    property Color read FColor write SetColor default clWindow;
    property Font: TFont read FFont write SetFont;
    property AutoSizeHeight: boolean read FAutoSizeHeight write SetAutoSizeHeight default False;
    property AllowReorder: boolean read FAllowReorder write FAllowReorder default True;
    property TagHoverUnderline: boolean read FTagHoverUnderline write FTagHoverUnderline default True;
    property CloseButtons: boolean read FCloseButtons write FCloseButtons default True;
    property CloseButtonOnHover: boolean read FCloseButtonOnHover write FCloseButtonOnHover default True;
    property TagColor: TColor read FTagColor write FTagColor default clNone;
    property TagSuffixColor: TColor read FTagSuffixColor write FTagSuffixColor default clWhite;
    property TagHoverColor: TColor read FTagHoverColor write FTagHoverColor default clMenuHighlight;
    property TagBorderColor: TColor read FTagBorderColor write FTagBorderColor default clNone;
    property DragIndicatorColor: TColor read FDragIndicatorColor write FDragIndicatorColor default clHighlight;
    property AutoColorBrigtness: integer read FAutoColorBrigtness write FAutoColorBrigtness default 80;
    property TagBorderWidth: integer read FTagBorderWidth write FTagBorderWidth default 2;
    property BorderColor: TColor read FBorderColor write FBorderColor default clWindowFrame;
    property BorderWidth: integer read FBorderWidth write FBorderWidth default 0;
    property RoundCorners: integer read FRoundCorners write FRoundCorners default 5;
    property EditMinWidth: integer read FEditMinWidth write FEditMinWidth default 50;
    property RemoveConfirm: boolean read FRemoveConfirm write FRemoveConfirm default True;
    property RemoveConfirmTitle: string read FRemoveConfirmTitle write FRemoveConfirmTitle;
    property RemoveConfirmMessage: string read FRemoveConfirmMessage write FRemoveConfirmMessage;
    property ParentFont: boolean read FParentFont write SetParentFont default True;
    property ReadOnly: boolean read FReadOnly write SetReadOnly default False;
    property Enabled: boolean read FEnabled write SetEnabled;
    property TagColors: TTagColorItems read FTagColors write SetTagColors;

    property Items: TStringList read GetTags write SetTags;

    property OnTagAdd: TTagEvent read FOnTagAdd write FOnTagAdd;
    property OnTagRemove: TTagEvent read FOnTagRemove write FOnTagRemove;
    property OnTagClick: TTagEvent read FOnTagClick write FOnTagClick;
    property OnTagPopup: TTagPopupEvent read FOnTagPopup write FOnTagPopup;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
  end;

implementation

{ TTagEdit }

constructor TTagEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTagColors := TTagColorItems.Create(Self);

  FTags := TStringList.Create;
  FTags.OnChange := @TagsChanged;
  FTags.CaseSensitive := True;
  FTags.Add('IDE:Lazarus');
  FTags.Add('Free Pascal');

  Height := Scale(32);
  Width := Scale(300);
  FColor := clWindow;
  ParentColor := False;

  FReadOnly := False;
  FEnabled := True;
  FAutoSizeHeight := False;
  FAllowReorder := True;
  FFont := TFont.Create;
  FFont.OnChange := @FontChanged;
  FParentFont := True;
  FTagHoverUnderline := True;
  FCloseButtons := True;
  FCloseButtonOnHover := True;
  PopupMenu := nil;

  FTagColor := clNone;
  FTagSuffixColor := clWhite;
  FTagHoverColor := clMenuHighlight;
  FTagBorderColor := clNone;
  FTagBorderWidth := 2;
  FDragIndicatorColor := clHighlight;
  FBorderColor := clWindowFrame;
  FEditMinWidth := Scale(50);
  FRoundCorners := Scale(5);
  FAutoColorBrigtness := 80;

  // Create inner edit control
  FEdit := TEdit.Create(Self);
  if not (csDesigning in ComponentState) then
  begin
    FEdit.Parent := Self;
    FEdit.DoubleBuffered := FDoubleBuffered;
    FEdit.BiDiMode := BiDiMode;
    FEdit.ParentFont := True;
    FEdit.BorderStyle := bsNone;
    FEdit.OnKeyDown := @EditKeyDown;
    FEdit.OnExit := @EditExit;
    FEdit.OnKeyUp := OnKeyUp;
    FEdit.OnKeyPress := OnKeyPress;
    FEdit.Color := Color;
    FEdit.Left := 4;
    FEdit.Top := 4;
  end;

  FRemoveConfirm := True;
  FRemoveConfirmMessage := 'Are you sure you want to remove tag';
  FRemoveConfirmTitle := 'Remove tag';

  FDragging := False;
  FDragIndex := -1;
  FDropIndex := -1;
  FUpdatingEdit := False;
  FHoverIndex := -1;
  FMouseDownIndex := -1;
end;

destructor TTagEdit.Destroy;
begin
  FTags.Free;
  FFont.Free;
  FEdit.Free;
  FTagColors.Free;
  inherited Destroy;
end;

procedure TTagEdit.SetTagColors(Value: TTagColorItems);
begin
  FTagColors.Assign(Value);
  Invalidate;
end;

function TTagEdit.GetTags: TStringList;
begin
  Result := FTags;
end;

procedure TTagEdit.SetTags(Value: TStringList);
begin
  FTags.Assign(Value);
  Invalidate;
  UpdateAutoHeight;
end;

procedure TTagEdit.SetColor(Value: TColor);
begin
  inherited;
  FColor := Value;
  if Assigned(FEdit) then
    FEdit.Color := Value;
end;

procedure TTagEdit.SetFont(Value: TFont);
begin
  if Value <> nil then
  begin
    FFont.Assign(Value);
    FEdit.Font.Assign(Value);
    FParentFont := False;
    Invalidate;
    UpdateAutoHeight;
  end;
end;

procedure TTagEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FParentFont and (AParent <> nil) then
  begin
    FFont.Assign(AParent.Font);
    FEdit.Font.Assign(AParent.Font);
    SetParentFont(True);
  end;
end;

procedure TTagEdit.SetParentFont(Value: boolean);
begin
  FParentFont := Value;

  if FParentFont and (Parent <> nil) then
  begin
    // Detach previous event
    if Assigned(FFont.OnChange) then
      FFont.OnChange := nil;

    // Copy parent's font
    FFont.Assign(Parent.Font);
    FEdit.Font.Assign(Parent.Font);

    // Subscribe to parent's font change manually
    Parent.Font.OnChange := @ParentFontChange;

    // Subscribe to font change
    FFont.OnChange := @FontChanged;
  end
  else
  begin
    // Remove parent font change hook
    if Assigned(Parent) then
      Parent.Font.OnChange := nil;

    // Copy font
    FEdit.Font.Assign(FFont);

    // Subscribe to font change
    FFont.OnChange := @FontChanged;
  end;
  Invalidate;
  UpdateAutoHeight;
end;

procedure TTagEdit.ParentFontChange(Sender: TObject);
begin
  if FParentFont and (Parent <> nil) then
  begin
    FFont.Assign(Parent.Font);
    FEdit.Font.Assign(Parent.Font);
    Invalidate;
    UpdateAutoHeight;
  end;
end;

procedure TTagEdit.FontChanged(Sender: TObject);
begin
  inherited;
  FEdit.Font.Assign(Font);
  if (Assigned(Parent)) and (not FFont.IsEqual(Parent.Font)) and (not FFont.IsDefault) then
    FParentFont := False;
  Invalidate;
  UpdateAutoHeight;
end;

function TTagEdit.Scale(const AValue: integer): integer;
begin
  Result := Scale96ToScreen(AValue);
end;

procedure TTagEdit.ScaleFontsPPI(const AToPPI: integer; const AProportion: double);
begin
  inherited ScaleFontsPPI(AToPPI, AProportion);
  if ((not FParentFont) and (FFont.Size <= 0)) or ((FParentFont) and (Assigned(Parent)) and (Parent.Font.Size <= 0)) then
    FFont.Size := CoalesceInt(Screen.SystemFont.Size, 8);
  DoScaleFontPPI(FFont, AToPPI, AProportion);
  if (Assigned(Parent)) and (FParentFont) then
    DoScaleFontPPI(Parent.Font, AToPPI, AProportion);
end;

procedure TTagEdit.FixDesignFontsPPI(const ADesignTimePPI: integer);
begin
  inherited FixDesignFontsPPI(ADesignTimePPI);
  if ((not FParentFont) and (FFont.Size <= 0)) or ((FParentFont) and (Assigned(Parent)) and (Parent.Font.Size <= 0)) then
    FFont.Size := CoalesceInt(Screen.SystemFont.Size, 8);
  DoFixDesignFontPPI(FFont, ADesignTimePPI);
  if (Assigned(Parent)) and (FParentFont) then
    DoFixDesignFontPPI(Parent.Font, ADesignTimePPI);
end;

function TTagEdit.Focused: boolean;
begin
  Result := FEdit.Focused;
end;

procedure TTagEdit.SetReadOnly(Value: boolean);
begin
  FReadOnly := Value;
  FEdit.Visible := not Value and FEnabled;
  Invalidate;
end;

procedure TTagEdit.SetEnabled(Value: boolean);
begin
  inherited;
  FEnabled := Value;
  FEdit.Visible := Value and not FReadOnly;
end;

procedure TTagEdit.SetAutoSizeHeight(Value: boolean);
begin
  if FAutoSizeHeight <> Value then
  begin
    FAutoSizeHeight := Value;
    if FAutoSizeHeight then
      UpdateAutoHeight;
  end;
end;

procedure TTagEdit.TagsChanged(Sender: TObject);
begin
  FHoverIndex := -1; // Reset hover state when Items change
  Invalidate;
  UpdateAutoHeight;
end;

function TTagEdit.RemovalConfirmed(idx: integer): boolean;
begin
  if not RemoveConfirm then
    exit(True);
  Result := MessageDlg(FRemoveConfirmTitle, FRemoveConfirmMessage + ' "' + FTags[idx] + '"?', mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

procedure TTagEdit.AddTag(const ATag: string);
begin
  if (ATag <> string.Empty) and (FTags.IndexOf(ATag) = -1) then
  begin
    FTags.Add(ATag);
    FEdit.Text := string.Empty;
    if Assigned(FOnTagAdd) then
      FOnTagAdd(Self, ATag);
    if Assigned(FOnChange) then
      FOnChange(Self);
    Invalidate;
    UpdateAutoHeight;
  end;
end;

procedure TTagEdit.RemoveTag(const ATag: string);
var
  i: integer;
begin
  i := FTags.IndexOf(ATag);
  if i >= 0 then
  begin
    FTags.Delete(i);
    if Assigned(FOnTagRemove) then
      FOnTagRemove(Self, ATag);
    if Assigned(FOnChange) then
      FOnChange(Self);
    Invalidate;
    UpdateAutoHeight;
  end;
end;

function TTagEdit.GetTagHeight: integer;
begin
  Result := Scale(CoalesceInt(Font.Size, Screen.SystemFont.Size, 8) * 2 + 6);
end;

function TTagEdit.GetTagRect(Index: integer): TRect;
begin
  if (Index >= 0) and (Index <= High(FTagRects)) then
    Result := FTagRects[Index]
  else
    Result := Rect(0, 0, 0, 0);
end;

procedure TTagEdit.UpdateEditPosition;
var
  LastRect: TRect;
  NewLeft, NewTop: integer;
  AvailWidth: integer;
begin
  if FUpdatingEdit then Exit;

  FUpdatingEdit := True;
  try
    // Calculate available width considering borders
    AvailWidth := ClientWidth - Scale(8);
    if FBorderWidth > 0 then
      Dec(AvailWidth, 2 * FBorderWidth);

    if FTags.Count > 0 then
    begin
      LastRect := GetTagRect(FTags.Count - 1);

      // Check if sufficient space exists for Edit after last tag
      if (LastRect.Right + Scale(4) + FEditMinWidth) <= AvailWidth then
      begin
        // If space permits, place on the same line
        NewLeft := LastRect.Right + Scale(4);
        NewTop := LastRect.Top;
      end
      else
      begin
        // If insufficient space - move to new line
        NewLeft := Scale(4);
        NewTop := LastRect.Bottom + Scale(4);
      end;
    end
    else
    begin
      NewLeft := Scale(4);
      NewTop := Scale(4);
    end;

    // Set position only when changed
    if (FEdit.Left <> NewLeft) or (FEdit.Top <> NewTop) then
    begin
      FEdit.Left := NewLeft;
      {$IFDEF UNIX}
      FEdit.Top := NewTop +Scale(1);
      {$ELSE}
      FEdit.Top := NewTop + Scale(3);
      {$ENDIF}
    end;

    // Set Edit width
    FEdit.Width := ClientWidth - FEdit.Left - Scale(4);
    if FEdit.Width < FEditMinWidth then
      FEdit.Width := FEditMinWidth;
    FEdit.Height := GetTagHeight;
  finally
    FUpdatingEdit := False;
  end;
end;

function TTagEdit.CoalesceInt(const A, B: integer; const C: integer = 0): integer;
begin
  if A > 0 then
    Result := A
  else
  if B > 0 then
    Result := B
  else
    Result := C;
end;

function TTagEdit.CalculateAutoHeight: integer;
var
  I: integer;
  X, Y, W, H: integer;
  AvailWidth: integer;
  LastRect: TRect;
  EditBottom: integer;
begin
  if FTags.Count = 0 then
  begin
    // Minimum height for empty control (edit box + padding)
    Result := Scale(4) + GetTagHeight + Scale(4);
    if FBorderWidth > 0 then
      Inc(Result, 2 * FBorderWidth);
    Exit;
  end;

  // Calculate available width
  AvailWidth := ClientWidth - Scale(8);
  if FBorderWidth > 0 then
    Dec(AvailWidth, 2 * FBorderWidth);

  Canvas.Font.Assign(Font);
  H := GetTagHeight;

  X := Scale(4);
  Y := Scale(4);

  // Simulate tag layout to find bottom position
  for I := 0 to FTags.Count - 1 do
  begin
    W := Canvas.TextWidth(FTags[I]) + GetTagHeight;

    // Wrap to next line if tag doesn't fit
    if (I > 0) and ((X + W) > AvailWidth) then
    begin
      X := Scale(4);
      Y := Y + H + Scale(4);
    end;

    LastRect := Rect(X, Y, X + W, Y + H);
    Inc(X, W + Scale(4));
  end;

  // Calculate edit box position
  if (LastRect.Right + 4 + FEditMinWidth) <= AvailWidth then
    EditBottom := LastRect.Bottom + Scale(4) // Edit on same line
  else
    EditBottom := LastRect.Bottom + Scale(4) + H + Scale(4); // Edit on new line

  // Return total height including borders
  Result := EditBottom;
  if FBorderWidth > 0 then
    Inc(Result, 2 * FBorderWidth);
end;

procedure TTagEdit.UpdateAutoHeight;
begin
  if FAutoSizeHeight and (not (Align in [alClient, alRight, alLeft])) then
  begin
    Height := CalculateAutoHeight;
  end;
end;

procedure TTagEdit.Resize;
begin
  inherited Resize;
  UpdateAutoHeight;
end;

function TTagEdit.RandTagColor(const TagName: string; BrightnessPercent: integer = 70): TColor;
var
  Hash: longword;
  R, G, B: byte;
  S: single;
  i, MaxVal, MinVal: integer;
begin
  // Clamp brightness range to [0..100]
  if BrightnessPercent < 0 then BrightnessPercent := 0;
  if BrightnessPercent > 100 then BrightnessPercent := 100;

  // Generate stable hash (FNV-1a)
  Hash := 2166136267;
  if TagName <> string.Empty then
    for i := 1 to Length(TagName) do
      Hash := longword(uint64(Hash xor Ord(TagName[i])) * 16777619);

  // Extract RGB components from hash
  R := (Hash and $FF);
  G := (Hash shr 8) and $FF;
  B := (Hash shr 16) and $FF;

  // Boost saturation slightly if too gray
  MaxVal := MaxIntValue([R, G, B]);
  MinVal := MinIntValue([R, G, B]);
  if (MaxVal - MinVal) < 60 then
  begin
    if MaxVal < 200 then
    begin
      R := Min(255, R + 60);
      G := Min(255, G + 40);
      B := Min(255, B + 40);
    end;
  end;

  // --- Apply brightness adjustment ---
  // Convert BrightnessPercent (0–100) to S (0.0–2.0)
  // 50% = original color, >50 brighten, <50 darken
  S := 0.5 + (BrightnessPercent / 100);

  R := Min(255, Round(R * S));
  G := Min(255, Round(G * S));
  B := Min(255, Round(B * S));

  Result := RGBToColor(R, G, B);
end;

function TTagEdit.FindTagColor(const S: string): TColor;
var
  i: integer;
begin
  Result := clNone;
  for i := 0 to TagColors.Count - 1 do
  begin
    if Pos(UpperCase(TagColors[i].TagName), UpperCase(S)) > 0 then
      Exit(TagColors[i].Color);
  end;
end;

function TTagEdit.GetContrastTextColor(BackColor, FontColor: TColor; MidLevel: integer = 128): TColor;
var
  Rb, Gb, Bb: byte; // background
  Rf, Gf, Bf: byte; // font
  Brightness: double;
  InvR, InvG, InvB: integer;
begin
  // Ensure MidLevel in 0..255
  if MidLevel < 0 then MidLevel := 0;
  if MidLevel > 255 then MidLevel := 255;

  // Extract RGB from both colors
  BackColor := ColorToRGB(BackColor);
  FontColor := ColorToRGB(FontColor);

  Rb := GetRValue(BackColor);
  Gb := GetGValue(BackColor);
  Bb := GetBValue(BackColor);

  Rf := GetRValue(FontColor);
  Gf := GetGValue(FontColor);
  Bf := GetBValue(FontColor);

  // Calculate perceived brightness of background
  Brightness := 0.299 * Rb + 0.587 * Gb + 0.114 * Bb;

  // Invert the given font color
  InvR := 255 - Rf;
  InvG := 255 - Gf;
  InvB := 255 - Bf;

  // If background is dark — make inverted color lighter (toward white)
  // If background is light — make inverted color darker (toward black)
  if Brightness < MidLevel then
  begin
    InvR := EnsureRange(Round(InvR), 0, 255);
    InvG := EnsureRange(Round(InvG), 0, 255);
    InvB := EnsureRange(Round(InvB), 0, 255);
    Result := RGBToColor(InvR, InvG, InvB);
  end
  else
    Result := FontColor;
end;

procedure TTagEdit.DrawTags;
var
  i: integer;
  R: TRect;
  s, Part1, Part2: string;
  X, Y, W, H, M, SepW: integer;
  AvailWidth: integer;
  Color1, Color2: TColor;
  HasColon, Hover: boolean;
begin
  AvailWidth := ClientWidth - Scale(8);
  if FBorderWidth > 0 then
    Dec(AvailWidth, 2 * FBorderWidth);

  Canvas.Font.Assign(Font);
  SetLength(FTagRects, FTags.Count);

  X := Scale(4);
  Y := Scale(4);
  H := GetTagHeight;

  for i := 0 to FTags.Count - 1 do
  begin
    s := FTags[i];
    Hover := (i = FHoverIndex) and ((FTagHoverColor <> clNone) or FTagHoverUnderline);

    // Split tag by colon
    HasColon := Pos(':', s) > 0;
    if HasColon then
    begin
      Part1 := Trim(Copy(s, 1, Pos(':', s) - 1)) + ' ';
      Part2 := Trim(Copy(s, Pos(':', s) + 1, MaxInt));
    end
    else
    begin
      Part1 := s;
      Part2 := '';
    end;

    M := GetTagHeight;
    W := Canvas.TextWidth(s) + M;

    if (i > 0) and ((X + W) > AvailWidth) then
    begin
      X := Scale(4);
      Y := Y + H + Scale(4);
    end;

    R := Rect(X, Y, X + W, Y + H);
    FTagRects[i] := R;

    Canvas.Pen.Width := FTagBorderWidth;
    Canvas.Pen.Color := FTagBorderColor;
    if FTagBorderWidth <= 0 then
      Canvas.Pen.Style := psClear
    else
      Canvas.Pen.Style := psSolid;

    if HasColon then
    begin
      if Hover and (FTagHoverColor <> clNone) then
        Color1 := FTagHoverColor
      else
      begin
        Color1 := FindTagColor(Part1);
        if Color1 = clNone then
        begin
          if FTagColor <> clNone then
            Color1 := FTagColor
          else
            Color1 := RandTagColor(Part1, FAutoColorBrigtness);
        end;
      end;

      Color2 := FindTagColor(Part2);
      if Color2 = clNone then
      begin
        if FTagSuffixColor <> clNone then
          Color2 := FTagSuffixColor
        else
          Color2 := RandTagColor(Part2, FAutoColorBrigtness);
      end;

      if Hover and (FTagHoverColor <> clNone) then
        Canvas.Pen.Color := FTagHoverColor
      else
      if FTagBorderColor = clNone then
        Canvas.Pen.Color := Color1;

      SepW := Canvas.TextWidth(Part1) + Scale(6); // width of first part + left padding

      // Left part background
      Canvas.Brush.Color := Color1;
      Canvas.RoundRect(R.Left, R.Top, R.Left + SepW, R.Bottom, FRoundCorners, FRoundCorners);

      // Right part background
      Canvas.Brush.Color := Color2;
      Canvas.RoundRect(R.Left + SepW, R.Top, R.Right, R.Bottom, FRoundCorners, FRoundCorners);

      // Fill junction to avoid double-rounded corner visual
      Canvas.Brush.Color := Color1;
      Canvas.FillRect(R.Left + SepW - FRoundCorners, R.Top, R.Left + SepW, R.Bottom - 1);
      Canvas.Brush.Color := Color2;
      Canvas.FillRect(R.Left + SepW, R.Top, R.Left + SepW + FRoundCorners, R.Bottom - 1);

      if (FTagBorderWidth > 0) then
      begin
        Canvas.Brush.Color := Canvas.Pen.Color;
        Canvas.FillRect(R.Left + SepW - FRoundCorners, R.Top - (FTagBorderWidth div 2), R.Left + SepW + FRoundCorners,
          R.Top + FTagBorderWidth - (FTagBorderWidth div 2));
        Canvas.FillRect(R.Left + SepW - FRoundCorners, R.Bottom - (FTagBorderWidth div 2) - 1, R.Left + SepW +
          FRoundCorners, R.Bottom + FTagBorderWidth - (FTagBorderWidth div 2) - 1);
      end;
    end
    else
    begin
      if Hover and (FTagHoverColor <> clNone) then
        Canvas.Brush.Color := FTagHoverColor
      else
      begin
        Canvas.Brush.Color := FindTagColor(s);
        if Canvas.Brush.Color = clNone then
        begin
          if FTagColor <> clNone then
            Canvas.Brush.Color := FTagColor
          else
            Canvas.Brush.Color := RandTagColor(s, FAutoColorBrigtness);
        end;
      end;

      if Hover and (FTagHoverColor <> clNone) then
        Canvas.Pen.Color := FTagHoverColor
      else
      if FTagBorderColor = clNone then
        Canvas.Pen.Color := Canvas.Brush.Color;

      Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, FRoundCorners, FRoundCorners);
    end;

    // Calculate shift
    if (FReadOnly) or (not CloseButtons) or (FCloseButtonOnHover and not Hover) then
      M := Scale(Round(CoalesceInt(Font.Size, Screen.SystemFont.Size, 8) * 1.3) + 2)
    else
      M := 0;

    // Draw text
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Underline := FTagHoverUnderline and Hover;
    if HasColon then
    begin
      Canvas.Font.Color := GetContrastTextColor(Color1, Font.Color);
      Canvas.TextOut(R.Left + Scale(4), R.Top + Scale(3), Part1);

      Canvas.Font.Color := GetContrastTextColor(Color2, Font.Color);
      Canvas.TextOut(R.Left + SepW + Scale(2) + M div 3, R.Top + Scale(3), Part2);
    end
    else
    begin
      Canvas.Font.Color := GetContrastTextColor(Canvas.Brush.Color, Font.Color, 128);
      Canvas.TextOut(R.Left + Scale(4) + M div 2, R.Top + Scale(3), s);
    end;

    // Draw '×' button if not read-only
    if (not FReadOnly) and (FCloseButtons) and (not FCloseButtonOnHover or Hover) then
    begin
      Canvas.Font.Underline := False;
      M := Scale(Round(CoalesceInt(Font.Size, Screen.SystemFont.Size, 8) * 1.3) + 2);
      Canvas.TextOut(R.Right - M, R.Top + Scale(4), '×');
    end;

    Inc(X, W + Scale(4));
  end;

  if not (csDesigning in ComponentState) then
    UpdateEditPosition;
end;

procedure TTagEdit.Paint;
var
  BorderRect, R: TRect;
begin
  // Draw component background
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  // Draw component border if needed
  if FBorderWidth > 0 then
  begin
    Canvas.Pen.Width := FBorderWidth;
    Canvas.Pen.Color := FBorderColor;
    Canvas.Pen.Style := psSolid;
    BorderRect := ClientRect;
    Canvas.Rectangle(BorderRect);
  end;

  DrawTags;

  // Draw drag indicator
  if not (csDesigning in ComponentState) and FDragging and (FDropIndex >= 0) and (FDropIndex <= FTags.Count) then
  begin
    Canvas.Pen.Width := 2;
    Canvas.Pen.Color := FDragIndicatorColor;
    Canvas.Pen.Style := psSolid;
    if FDropIndex < FTags.Count then
    begin
      R := GetTagRect(FDropIndex);
      Canvas.MoveTo(R.Left - Scale(2), R.Top);
      Canvas.LineTo(R.Left - Scale(2), R.Bottom);
    end
    else if FTags.Count > 0 then
    begin
      R := GetTagRect(FTags.Count - 1);
      Canvas.MoveTo(R.Right + Scale(2), R.Top);
      Canvas.LineTo(R.Right + Scale(2), R.Bottom);
    end
    else
    begin
      // No Items - show indicator at start
      Canvas.MoveTo(Scale(2), Scale(4));
      Canvas.LineTo(Scale(2), Scale(4) + GetTagHeight);
    end;
  end;
end;

function TTagEdit.TagAtPos(const P: TPoint): integer;
var
  i: integer;
  R: TRect;
begin
  for i := 0 to FTags.Count - 1 do
  begin
    R := GetTagRect(i);
    if PtInRect(R, P) then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TTagEdit.UpdateHoverState(X, Y: integer);
var
  NewHoverIndex: integer;
begin
  NewHoverIndex := TagAtPos(Point(X, Y));

  if NewHoverIndex <> FHoverIndex then
  begin
    FHoverIndex := NewHoverIndex;
    Invalidate;
  end;
end;

procedure TTagEdit.EditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  ATag: string;
begin
  if csDesigning in ComponentState then exit;

  // Enter adds a new tag
  if Key = VK_RETURN then
  begin
    if FEdit.Text <> string.Empty then
      AddTag(Trim(FEdit.Text));
    Key := 0;
  end;

  // Backspace removes last tag if edit is empty
  if (Key = VK_BACK) and (FEdit.Text = string.Empty) and (FTags.Count > 0) and RemovalConfirmed(FTags.Count - 1) then
  begin
    ATag := FTags[FTags.Count - 1];
    FTags.Delete(FTags.Count - 1);
    if Assigned(FOnTagRemove) then
      FOnTagRemove(Self, ATag);
    if Assigned(FOnChange) then
      FOnChange(Self);
    Invalidate;
    UpdateAutoHeight;
  end;

  if Assigned(OnKeyDown) then
    OnKeyDown(Sender, Key, Shift);
end;

procedure TTagEdit.EditExit(Sender: TObject);
begin
  // When leaving the edit, add tag if not empty
  if FEdit.Text <> string.Empty then
    AddTag(Trim(FEdit.Text));
end;

procedure TTagEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  idx, M: integer;
  R: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if csDesigning in ComponentState then exit;

  if Button = mbLeft then
  begin
    idx := TagAtPos(Point(X, Y));
    FMouseDownIndex := idx;
    FMouseDownPos := Point(X, Y);

    if (not FReadOnly) and (FCloseButtons) and (idx >= 0) then
    begin
      R := GetTagRect(idx);
      M := Scale(Round(CoalesceInt(Font.Size, Screen.SystemFont.Size, 8) * 1.3) + 2);
      // Click near right edge removes the tag - do it immediately
      if (X > R.Right - M) and RemovalConfirmed(idx) then
      begin
        RemoveTag(FTags[idx]);
        FMouseDownIndex := -1; // Reset since we handled it
      end;
    end;
    if FEdit.Visible and FEdit.CanFocus then
      FEdit.SetFocus;
  end;
end;

procedure TTagEdit.MouseMove(Shift: TShiftState; X, Y: integer);
var
  idx: integer;
  NewDropIndex: integer;
  DragThreshold: integer;
  i: integer;
  R: TRect;
  ClosestIndex: integer;
  MinDistance: integer;
  Distance: integer;
  FoundInTag: boolean;
begin
  inherited MouseMove(Shift, X, Y);
  if csDesigning in ComponentState then exit;

  // Update hover state
  if not FDragging then
    UpdateHoverState(X, Y);

  // Start dragging only if mouse moved beyond threshold and we have a valid tag index
  if FAllowReorder and not FReadOnly and not FDragging and (ssLeft in Shift) and (FMouseDownIndex >= 0) then
  begin
    DragThreshold := Scale(5); // pixels
    if (Abs(X - FMouseDownPos.X) > DragThreshold) or (Abs(Y - FMouseDownPos.Y) > DragThreshold) then
    begin
      FDragging := True;
      FDragIndex := FMouseDownIndex;
      FDropIndex := FMouseDownIndex;
      Invalidate;
    end;
  end;

  if FDragging then
  begin
    // First check if cursor is directly over a tag
    idx := TagAtPos(Point(X, Y));
    FoundInTag := (idx >= 0);

    if FoundInTag then
    begin
      // If cursor is over a tag, place before that tag
      NewDropIndex := idx;
    end
    else
    begin
      // Overfloaw last tag, place at the end
      R := GetTagRect(FTags.Count - 1);
      if (Y >= R.Bottom) or ((Y >= R.Top) and (X > R.Right)) then
        NewDropIndex := FTags.Count
      else
      begin
        // If cursor is between Items, find the closest tag
        ClosestIndex := -1;
        MinDistance := MaxInt;

        for i := 0 to FTags.Count - 1 do
        begin
          R := GetTagRect(i);

          // Calculate distance to tag center
          Distance := Abs(X - R.Left) + Abs(Y - (R.Top + R.Height div 2));

          if Distance < MinDistance then
          begin
            MinDistance := Distance;
            ClosestIndex := i;
          end;
        end;

        if ClosestIndex >= 0 then
        begin
          NewDropIndex := ClosestIndex;
        end
        else
        begin
          // No Items found, place at the end
          NewDropIndex := FTags.Count;
        end;
      end;
    end;

    // Only update and invalidate if position changed
    if NewDropIndex <> FDropIndex then
    begin
      FDropIndex := NewDropIndex;
      Invalidate;
    end;
  end;
end;

procedure TTagEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  TempTag: string;
  idx, M: integer;
  R: TRect;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if csDesigning in ComponentState then exit;

  if Button = mbLeft then
  begin
    // Handle click event if we didn't drag and it's the same tag we pressed
    if not FDragging and (FMouseDownIndex >= 0) then
    begin
      idx := TagAtPos(Point(X, Y));
      if (idx = FMouseDownIndex) and (idx >= 0) then
      begin
        R := GetTagRect(idx);
        if FReadOnly or not FCloseButtons then
          M := 0
        else
          M := Scale(Round(CoalesceInt(Font.Size, Screen.SystemFont.Size, 8) * 1.3) + 2);
        // Don't trigger click for remove button area (already handled in MouseDown)
        if not (X > R.Right - M) then
        begin
          // Generate OnTagClick event
          if Assigned(FOnTagClick) then
            FOnTagClick(Self, FTags[idx]);
        end;
      end;
    end;

    // Handle drag completion
    if FDragging then
    begin
      FDragging := False;
      if (FDragIndex >= 0) and (FDropIndex >= 0) and (FDragIndex <> FDropIndex) then
      begin
        // Adjust drop index if dragging forward
        if FDropIndex > FDragIndex then
          Dec(FDropIndex);

        TempTag := FTags[FDragIndex];
        FTags.Delete(FDragIndex);
        if FDropIndex >= FTags.Count then
          FTags.Add(TempTag)
        else
          FTags.Insert(FDropIndex, TempTag);

        if Assigned(FOnChange) then
          FOnChange(Self);
      end;
      FDragIndex := -1;
      FDropIndex := -1;
      Invalidate;
      UpdateAutoHeight;
    end;

    // Reset mouse down state
    FMouseDownIndex := -1;
  end;
end;

procedure TTagEdit.MouseLeave;
var
  Form: TCustomForm;
begin
  inherited MouseLeave;
  if csDesigning in ComponentState then exit;

  Form := GetParentForm(Self);
  if (Form <> nil) and (not Form.Active) then Exit;

  if (FHoverIndex <> -1) then
  begin
    FHoverIndex := -1;
    Invalidate;
  end;
end;

procedure TTagEdit.HandlePopupMenu(Sender: TObject);
var
  MousePos: TPoint;
  TagIndex: integer;
  Handled: boolean;
begin
  if not Assigned(FOnTagPopup) then
    Exit;

  // Get mouse position relative to the component
  MousePos := ScreenToClient(Mouse.CursorPos);

  // Find tag under cursor position
  TagIndex := TagAtPos(MousePos);

  if TagIndex >= 0 then
  begin
    Handled := False;
    FOnTagPopup(Self, FTags[TagIndex], Handled);

    // If event is handled, don't show standard popup menu
    if Handled then
      PopupMenu := nil;
  end
  else
  begin
    Handled := False;
    FOnTagPopup(Self, '', Handled);

    // If event is handled, don't show standard popup menu
    if Handled then
      PopupMenu := nil;
  end;
end;

procedure TTagEdit.DoContextPopup(MousePos: TPoint; var Handled: boolean);
var
  TagIndex: integer;
begin
  if Assigned(FOnTagPopup) then
  begin
    // Find tag under cursor position
    TagIndex := TagAtPos(MousePos);

    if TagIndex >= 0 then
    begin
      Handled := False;
      FOnTagPopup(Self, FTags[TagIndex], Handled);
    end
    else
    begin
      Handled := False;
      FOnTagPopup(Self, '', Handled);
    end;
  end
  else
    inherited DoContextPopup(MousePos, Handled);
end;

end.
