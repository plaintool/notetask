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
  Clipbrd,
  LCLType,
  LCLIntf,
  TagColorItems;

type
  TTagEvent = procedure(Sender: TObject; const TagText: string) of object;
  TTagReorderEvent = procedure(Sender: TObject; const TagText: string; const NewIndex: integer) of object;
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
    FBackspaceEditTag: boolean;
    FReadOnly: boolean;
    FEnabled: boolean;

    FAutoColorBrigtness: integer;
    FAutoColorSaturation: integer;
    FAutoColorSeed: longword;
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
    FRemoveConfirmMessage: TTranslateString;
    FRemoveConfirmTitle: TTranslateString;
    FTextHint: TTranslateString;

    FFont: TFont;
    FParentFont: boolean;

    FOnTagAdd: TTagEvent;
    FOnTagRemove: TTagEvent;
    FOnTagReorder: TTagReorderEvent;
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
    procedure SetTextHint(Value: TTranslateString);
    procedure TagsChanged(Sender: TObject);
    function RemovalConfirmed(idx: integer): boolean;
    function GetTagHeight(AFontSize: integer = -1): integer;
    function GetTagRect(Index: integer): TRect;
    function GetHoveredTag: string;
    function TagAtPos(const P: TPoint): integer;
    procedure UpdateEditPosition;
    procedure UpdateHoverState(X, Y: integer);
    function CoalesceInt(const A, B: integer; const C: integer = 0): integer;
    procedure SetAutoSizeHeight(Value: boolean);
    procedure UpdateAutoHeight;
    procedure SetTagColors(Value: TTagColorItems);
    function Scale(const AValue: integer): integer;
    function GetContrastTextColor(BackColor, FontColor: TColor; MidLevel: integer = 128): TColor;
    procedure HandlePopupMenu(Sender: TObject);
    function RandTagColor(const TagName: string; Brightness, Saturation: integer): TColor;
    function FindTagColor(const S: string): TColor;
    procedure DrawTags;
    procedure DrawTagsToCanvas(const ATags: TStringList; ACanvas: TCanvas; ATagHeight: integer;
      AAvailWidth: integer; AHoverIndex: integer = -1; AShowCloseButtons: boolean = True; var ATagRects: array of TRect;
      AFontSize: integer = -1; AIndent: integer = 4; ABlend: integer = 0; ABlendColor: TColor = clNone);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseLeave; override;
    procedure SetEnabled(Value: boolean); override;
    procedure Resize; override;
    procedure SetColor(Value: TColor); override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: boolean); override;
    procedure Paint; override;
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
    procedure CopyHoverText;
    procedure FinishEdit;
    function BlendColors(Color1, Color2: TColor; Intensity: integer): TColor;
    function GetTagsBitmap(ATags: TStringList; AFontSize, AWidth, AHeight: integer; ATagHeightDelta: integer = 0;
      ABlend: integer = 0; ABlendColor: TColor = clWhite): TBitmap;
    property HoveredTag: string read GetHoveredTag;
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
    property BackSpaceEditTag: boolean read FBackspaceEditTag write FBackspaceEditTag default False;
    property TagColor: TColor read FTagColor write FTagColor default clNone;
    property TagSuffixColor: TColor read FTagSuffixColor write FTagSuffixColor default clWhite;
    property TagHoverColor: TColor read FTagHoverColor write FTagHoverColor default clHighlight;
    property TagBorderColor: TColor read FTagBorderColor write FTagBorderColor default clNone;
    property DragIndicatorColor: TColor read FDragIndicatorColor write FDragIndicatorColor default clRed;
    property AutoColorBrigtness: integer read FAutoColorBrigtness write FAutoColorBrigtness default 80;
    property AutoColorSaturation: integer read FAutoColorSaturation write FAutoColorSaturation default 80;
    property AutoColorSeed: longword read FAutoColorSeed write FAutoColorSeed default 0;
    property TagBorderWidth: integer read FTagBorderWidth write FTagBorderWidth default 2;
    property BorderColor: TColor read FBorderColor write FBorderColor default clWindowFrame;
    property BorderWidth: integer read FBorderWidth write FBorderWidth default 0;
    property RoundCorners: integer read FRoundCorners write FRoundCorners default 5;
    property EditMinWidth: integer read FEditMinWidth write FEditMinWidth default 50;
    property RemoveConfirm: boolean read FRemoveConfirm write FRemoveConfirm default True;
    property RemoveConfirmTitle: TTranslateString read FRemoveConfirmTitle write FRemoveConfirmTitle;
    property RemoveConfirmMessage: TTranslateString read FRemoveConfirmMessage write FRemoveConfirmMessage;
    property TextHint: TTranslateString read FTextHint write SetTextHint;
    property ParentFont: boolean read FParentFont write SetParentFont default True;
    property ReadOnly: boolean read FReadOnly write SetReadOnly default False;
    property Enabled: boolean read FEnabled write SetEnabled;
    property TagColors: TTagColorItems read FTagColors write SetTagColors;

    property Items: TStringList read GetTags write SetTags;

    property OnTagClick: TTagEvent read FOnTagClick write FOnTagClick;
    property OnTagPopup: TTagPopupEvent read FOnTagPopup write FOnTagPopup;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnTagAdd: TTagEvent read FOnTagAdd write FOnTagAdd;
    property OnTagRemove: TTagEvent read FOnTagRemove write FOnTagRemove;
    property OnTagReorder: TTagReorderEvent read FOnTagReorder write FOnTagReorder;
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
  // 2166136267
  Randomize;
  FAutoColorSeed := Random(High(longword));
  FAllowReorder := True;
  FFont := TFont.Create;
  FFont.OnChange := @FontChanged;
  FParentFont := True;
  FTagHoverUnderline := True;
  FBackSpaceEditTag := False;
  FCloseButtons := True;
  FCloseButtonOnHover := True;
  PopupMenu := nil;

  FTagColor := clNone;
  FTagSuffixColor := clWhite;
  FTagHoverColor := clHighlight;
  FTagBorderColor := clNone;
  FTagBorderWidth := 2;
  FDragIndicatorColor := clRed;
  FBorderColor := clWindowFrame;
  FEditMinWidth := Scale(50);
  FRoundCorners := Scale(5);
  FAutoColorBrigtness := 80;
  FAutoColorSaturation := 80;
  FTextHint := 'Enter new tag...';

  // Create inner edit control
  FEdit := TEdit.Create(Self);
  if not (csDesigning in ComponentState) then
  begin
    FEdit.Parent := Self;
    FEdit.DoubleBuffered := FDoubleBuffered;
    FEdit.TextHint := FTextHint;
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

procedure TTagEdit.SetTextHint(Value: TTranslateString);
begin
  FTextHint := Value;
  if Assigned(FEdit) then
    FEdit.TextHint := Value;
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

function TTagEdit.GetTagHeight(AFontSize: integer = -1): integer;
begin
  Result := Scale(ifthen(AFontSize > -1, AFontSize, CoalesceInt(Font.Size, Screen.SystemFont.Size, 8)) * 2 + 6);
end;

function TTagEdit.GetTagRect(Index: integer): TRect;
begin
  if (Index >= 0) and (Index <= High(FTagRects)) then
    Result := FTagRects[Index]
  else
    Result := Rect(0, 0, 0, 0);
end;

function TTagEdit.GetHoveredTag: string;
begin
  if (FHoverIndex >= 0) then
    Result := FTags[FHoverIndex]
  else
    Result := string.Empty;
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
  LastRect := Rect(0, 0, 0, 0);
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

procedure TTagEdit.CopyHoverText;
var
  MousePos: TPoint;
  TagIndex: integer;
begin
  // Get current mouse position relative to the control
  MousePos := ScreenToClient(Mouse.CursorPos);

  // Find which tag is under the cursor
  TagIndex := TagAtPos(MousePos);

  // Check if we found a valid tag
  if (TagIndex >= 0) and (TagIndex < FTags.Count) then
  begin
    // Copy tag text to clipboard
    Clipboard.AsText := FTags[TagIndex];

    // Optional: Provide visual feedback
    // ShowMessage('Copied: ' + FTags[TagIndex]);
  end
  else
  begin
    // Optional: Handle case when no tag is under cursor
    // ShowMessage('No tag under cursor to copy');
  end;
end;

procedure TTagEdit.FinishEdit;
begin
  if FEdit.Text <> string.Empty then
    AddTag(Trim(FEdit.Text));
end;

procedure TTagEdit.EditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  ATag: string;
  SL: TStringList;
  i: integer;
  s: string;
begin
  if csDesigning in ComponentState then exit;

  // Enter adds a new tag (handles multiple tags separated by ';')
  if Key = VK_RETURN then
  begin
    if FEdit.Text <> string.Empty then
    begin
      SL := TStringList.Create;
      try
        // Split by semicolon
        ExtractStrings([';'], [], PChar(FEdit.Text), SL);

        for i := 0 to SL.Count - 1 do
        begin
          s := Trim(SL[i]);
          if s <> '' then
            AddTag(s); // add every tag
        end;
      finally
        SL.Free;
      end;
      FEdit.Clear; // clear input after adding
    end;
    Key := 0;
  end;

  // Backspace removes last tag if edit is empty
  if (Key = VK_BACK) and (FEdit.Text = string.Empty) and (FTags.Count > 0) and (FBackspaceEditTag or
    RemovalConfirmed(FTags.Count - 1)) then
  begin
    ATag := FTags[FTags.Count - 1];
    FTags.Delete(FTags.Count - 1);
    if FBackSpaceEditTag then
    begin
      FEdit.Text := ATag;
      FEdit.SelStart := FEdit.GetTextLen;
      UpdateEditPosition;
      Repaint;
    end;
    if Assigned(FOnTagRemove) then
      FOnTagRemove(Sender, ATag);
    if Assigned(FOnChange) then
      FOnChange(Sender);
    UpdateAutoHeight;
    Invalidate;
    Key := 0;
  end;

  if Assigned(OnKeyDown) then
    OnKeyDown(Sender, Key, Shift);
end;

procedure TTagEdit.EditExit(Sender: TObject);
begin
  // When leaving the edit, add tag if not empty
  FinishEdit;
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
      M := Scale(Round(CoalesceInt(Font.Size, Screen.SystemFont.Size, 8) * 1.3) + 4);
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

        if Assigned(FOnTagReorder) then
          FOnTagReorder(Self, TempTag, FDropIndex);
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

  // Don't clear hover if form inactive or popup menu opened
  Form := GetParentForm(Self);
  if (Assigned(Form) and (not Form.Active)) then Exit;
  if Assigned(PopupMenu) and (PopupMenu.PopupComponent = Self) then Exit;

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

function TTagEdit.RandTagColor(const TagName: string; Brightness, Saturation: integer): TColor;
var
  Hash: longword;
  R, G, B: byte;
  H, S_val, L: single;
  i: integer;

// Helper functions for RGB/HSL conversion
  procedure RGBToHSL(R, G, B: byte; out H, S, L: single);
  var
    Max, Min: integer;
    Delta: integer;
  begin
    R := R;
    G := G;
    B := B;

    Max := MaxIntValue([R, G, B]);
    Min := MinIntValue([R, G, B]);

    L := (Max + Min) / 2 / 255;

    if Max = Min then
    begin
      H := 0;
      S := 0;
    end
    else
    begin
      Delta := Max - Min;

      if L < 0.5 then
        S := Delta / (Max + Min)
      else
        S := Delta / (510 - Max - Min);

      if R = Max then
        H := (G - B) / Delta
      else if G = Max then
        H := 2 + (B - R) / Delta
      else
        H := 4 + (R - G) / Delta;

      H := H / 6;
      if H < 0 then H := H + 1;
    end;
  end;

  procedure HSLToRGB(H, S, L: single; out R, G, B: byte);
  var
    M1, M2: single;

    function HueToRGB(Hue: single): single;
    begin
      Hue := Hue - Floor(Hue);
      if 6 * Hue < 1 then
        Result := M1 + (M2 - M1) * Hue * 6
      else if 2 * Hue < 1 then
        Result := M2
      else if 3 * Hue < 2 then
        Result := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
      else
        Result := M1;
    end;

  begin
    if S = 0 then
    begin
      R := Round(L * 255);
      G := R;
      B := R;
    end
    else
    begin
      if L < 0.5 then
        M2 := L * (1 + S)
      else
        M2 := L + S - L * S;

      M1 := 2 * L - M2;

      R := Round(255 * HueToRGB(H + 1 / 3));
      G := Round(255 * HueToRGB(H));
      B := Round(255 * HueToRGB(H - 1 / 3));
    end;
  end;

begin
  // Generate stable hash (FNV-1a)
  Hash := FAutoColorSeed;
  if TagName <> string.Empty then
    for i := 1 to Length(TagName) do
      Hash := longword(uint64(Hash xor Ord(TagName[i])) * 16777619);

  // Extract RGB components from hash
  R := (Hash and $FF);
  G := (Hash shr 8) and $FF;
  B := (Hash shr 16) and $FF;

  // Convert RGB to HSL color space
  RGBToHSL(R, G, B, H, S_val, L);

  // Apply brightness and saturation parameters
  // Brightness: 0-100% (0 = dark, 100 = light)
  L := Brightness / 100;

  // Saturation: 0-100% (0 = grayscale, 100 = full color)
  S_val := Saturation / 100;

  // Convert back to RGB
  HSLToRGB(H, S_val, L, R, G, B);

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

procedure TTagEdit.DrawTags;
var
  AvailWidth: integer;
begin
  // Calculate available width considering borders
  AvailWidth := ClientWidth - Scale(8);
  if FBorderWidth > 0 then
    Dec(AvailWidth, 2 * FBorderWidth);

  // Initialize FTagRects array
  SetLength(FTagRects, FTags.Count);

  DrawTagsToCanvas(FTags, Canvas, GetTagHeight, AvailWidth, FHoverIndex, FCloseButtons, FTagRects);

  if not (csDesigning in ComponentState) then
    UpdateEditPosition;
end;

procedure TTagEdit.DrawTagsToCanvas(const ATags: TStringList; ACanvas: TCanvas; ATagHeight: integer;
  AAvailWidth: integer; AHoverIndex: integer = -1; AShowCloseButtons: boolean = True; var ATagRects: array of TRect;
  AFontSize: integer = -1; AIndent: integer = 4; ABlend: integer = 0; ABlendColor: TColor = clNone);
var
  i: integer;
  R: TRect;
  s, Part1, Part2: string;
  X, Y, W, H, M: integer;
  SepW: integer = 0;
  Color1: Tcolor = clNone;
  Color2: TColor = clNone;
  FontColor1: Tcolor = clNone;
  FontColor2: TColor = clNone;
  HasColon, Hover: boolean;
begin
  ACanvas.Font.Assign(Font);
  if AFontSize > -1 then
    ACanvas.Font.Size := AFontSize;
  ACanvas.AntialiasingMode := amOn;

  // If ATagRects is passed, ensure it has the correct length
  if Length(ATagRects) <> ATags.Count then
    Exit;

  X := Scale(AIndent);
  Y := Scale(AIndent);
  H := ATagHeight;

  for i := 0 to ATags.Count - 1 do
  begin
    s := ATags[i];
    Hover := (i = AHoverIndex) and ((FTagHoverColor <> clNone) or FTagHoverUnderline);

    // Split tag by colon
    HasColon := Pos(':', s) > 0;
    if HasColon then
    begin
      Part1 := Trim(Copy(s, 1, Pos(':', s) - 1)) + ' ';
      Part2 := Trim(Copy(s, Pos(':', s) + 1, MaxInt));
      if Trim(Part2) = string.Empty then
        HasColon := False;
    end
    else
    begin
      Part1 := s;
      Part2 := '';
    end;

    M := ATagHeight;
    W := ACanvas.TextWidth(s) + M;
    if HasColon and (ACanvas.TextWidth(Part2) < ACanvas.TextWidth('...')) then W += ACanvas.TextWidth('.....');

    if (i > 0) and ((X + W) > AAvailWidth) then
    begin
      X := Scale(AIndent);
      Y := Y + H + Scale(AIndent);
    end;

    R := Rect(X, Y, X + W, Y + H);

    // Store the calculated rect in the provided array
    if i < Length(ATagRects) then
      ATagRects[i] := R;

    ACanvas.Pen.Width := FTagBorderWidth;
    ACanvas.Pen.Color := FTagBorderColor;
    if FTagBorderWidth <= 0 then
      ACanvas.Pen.Style := psClear
    else
      ACanvas.Pen.Style := psSolid;

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
            Color1 := RandTagColor(Trim(Part1), FAutoColorBrigtness, FAutoColorSaturation);
        end;
      end;

      Color2 := FindTagColor(Part2);
      if Color2 = clNone then
      begin
        if FTagSuffixColor <> clNone then
          Color2 := FTagSuffixColor
        else
          Color2 := RandTagColor(Trim(Part2), FAutoColorBrigtness, FAutoColorSaturation);
      end;

      if Hover and (FTagHoverColor <> clNone) then
        ACanvas.Pen.Color := FTagHoverColor
      else
      if FTagBorderColor = clNone then
        ACanvas.Pen.Color := Color1;

      FontColor1 := GetContrastTextColor(Color1, Font.Color, 150);
      FontColor2 := GetContrastTextColor(Color2, Font.Color, 150);

      if ABlendColor <> clNone then
      begin
        Color1 := BlendColors(Color1, ABlendColor, ABlend);
        Color2 := BlendColors(Color2, ABlendColor, ABlend);
        ACanvas.Pen.Color := BlendColors(ACanvas.Pen.Color, ABlendColor, ABlend);
        FontColor1 := BlendColors(FontColor1, ABlendColor, ABlend);
        FontColor2 := BlendColors(FontColor2, ABlendColor, ABlend);
      end;

      SepW := ACanvas.TextWidth(Part1) + Scale(6); // width of first part + left padding

      // Left part background
      ACanvas.Brush.Color := Color1;
      ACanvas.RoundRect(R.Left, R.Top, R.Left + SepW, R.Bottom, FRoundCorners, FRoundCorners);

      // Right part background
      ACanvas.Brush.Color := Color2;
      ACanvas.RoundRect(R.Left + SepW, R.Top, R.Right, R.Bottom, FRoundCorners, FRoundCorners);

      // Fill junction to avoid double-rounded corner visual
      ACanvas.Brush.Color := Color1;
      ACanvas.FillRect(R.Left + SepW - FRoundCorners, R.Top, R.Left + SepW, R.Bottom - 1);
      ACanvas.Brush.Color := Color2;
      ACanvas.FillRect(R.Left + SepW, R.Top, R.Left + SepW + FRoundCorners, R.Bottom - 1);

      if (FTagBorderWidth > 0) then
      begin
        ACanvas.Brush.Color := ACanvas.Pen.Color;
        ACanvas.FillRect(R.Left + SepW - FRoundCorners, R.Top - (FTagBorderWidth div 2), R.Left + SepW + FRoundCorners,
          R.Top + FTagBorderWidth - (FTagBorderWidth div 2));
        ACanvas.FillRect(R.Left + SepW - FRoundCorners, R.Bottom - (FTagBorderWidth div 2) - 1, R.Left +
          SepW + FRoundCorners, R.Bottom + FTagBorderWidth - (FTagBorderWidth div 2) - 1);
      end;
    end
    else
    begin
      if Hover and (FTagHoverColor <> clNone) then
        ACanvas.Brush.Color := FTagHoverColor
      else
      begin
        ACanvas.Brush.Color := FindTagColor(s);
        if ACanvas.Brush.Color = clNone then
        begin
          if FTagColor <> clNone then
            ACanvas.Brush.Color := FTagColor
          else
            ACanvas.Brush.Color := RandTagColor(Trim(s), FAutoColorBrigtness, FAutoColorSaturation);
        end;
      end;

      if Hover and (FTagHoverColor <> clNone) then
        ACanvas.Pen.Color := FTagHoverColor
      else
      if FTagBorderColor = clNone then
        ACanvas.Pen.Color := ACanvas.Brush.Color;

      ACanvas.Font.Color := GetContrastTextColor(ACanvas.Brush.Color, Font.Color, 150);

      if ABlendColor <> clNone then
      begin
        ACanvas.Brush.Color := BlendColors(ACanvas.Brush.Color, ABlendColor, ABlend);
        ACanvas.Pen.Color := BlendColors(ACanvas.Pen.Color, ABlendColor, ABlend);
        ACanvas.Font.Color := BlendColors(ACanvas.Font.Color, ABlendColor, ABlend);
      end;

      ACanvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, FRoundCorners, FRoundCorners);
    end;

    // Calculate shift
    if (FReadOnly) or (not AShowCloseButtons) or (FCloseButtonOnHover and not Hover) then
      M := Scale(Round(CoalesceInt(Font.Size, Screen.SystemFont.Size, 8) * 1.3) + 2)
    else
      M := 0;

    // Draw text
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Underline := FTagHoverUnderline and Hover;
    if HasColon then
    begin
      ACanvas.Font.Color := FontColor1;
      ACanvas.TextOut(R.Left + Scale(Max(AIndent, 4)), R.Top + Scale(3), Part1);

      ACanvas.Font.Color := FontColor2;
      ACanvas.TextOut(R.Left + SepW + Scale(AIndent div 2) + M div 3, R.Top + Scale(3), Part2);
    end
    else
      ACanvas.TextOut(R.Left + Scale(AIndent) + M div 2, R.Top + Scale(3), s);

    // Draw '×' button if enabled
    if AShowCloseButtons and (not FReadOnly) and (FCloseButtons) and (not FCloseButtonOnHover or Hover) then
    begin
      ACanvas.Font.Underline := False;
      M := Scale(Round(CoalesceInt(Font.Size, Screen.SystemFont.Size, 8) * 1.3) + 2);
      ACanvas.TextOut(R.Right - M, R.Top + Scale(AIndent), '×');
    end;

    Inc(X, W + Scale(AIndent));
  end;
end;

function TTagEdit.GetTagsBitmap(ATags: TStringList; AFontSize, AWidth, AHeight: integer; ATagHeightDelta: integer = 0;
  ABlend: integer = 0; ABlendColor: TColor = clWhite): TBitmap;
var
  TempBitmap: TBitmap;
  TempTagRects: array of TRect = ();
  I: integer;
  MaxX, UsedWidth: integer;
  MaxY: integer;
  SingleLine: boolean;
begin
  Result := TBitmap.Create;
  try
    // Set font size for rendering
    Result.Canvas.Font.Assign(Font);
    Result.Canvas.Font.Size := AFontSize;

    // Calculate tag height based on font size
    TempBitmap := TBitmap.Create;
    try
      // Use larger temporary bitmap to accommodate all tags
      TempBitmap.Width := AWidth;
      TempBitmap.Height := AHeight;
      TempBitmap.PixelFormat := pf24bit;
      TempBitmap.Canvas.Brush.Color := clWhite;
      TempBitmap.Canvas.FillRect(0, 0, TempBitmap.Width, TempBitmap.Height);
      TempBitmap.Canvas.Font.Assign(Result.Canvas.Font);

      // Initialize temporary tag rects array
      SetLength(TempTagRects, ATags.Count);

      // Draw tags to temporary bitmap
      DrawTagsToCanvas(
        ATags,
        TempBitmap.Canvas,
        GetTagHeight(AFontSize) - ATagHeightDelta, // Calculate tag height based on font size
        AWidth - Scale(8), -1, // No hover effects
        False, // No close buttons for preview
        TempTagRects,
        AFontSize,
        1,
        ABlend,
        ABlendColor
        );

      // Calculate actual used width and height
      MaxX := 0;
      MaxY := 0;
      SingleLine := True;

      for I := 0 to ATags.Count - 1 do
      begin
        // Find the rightmost point
        if TempTagRects[I].Right > MaxX then
          MaxX := TempTagRects[I].Right;

        // Find the bottommost point
        if TempTagRects[I].Bottom > MaxY then
          MaxY := TempTagRects[I].Bottom;

        // Check if any tag is on second line (Y > initial position)
        if (I > 0) and (TempTagRects[I].Top > TempTagRects[0].Top) then
          SingleLine := False;
      end;

      // Add padding
      UsedWidth := MaxX + Scale(4);

      // Determine final dimensions
      if SingleLine and (UsedWidth < AWidth) then
      begin
        Result.Width := UsedWidth;
      end
      else
      begin
        Result.Width := AWidth;
      end;

      Result.Height := AHeight;
      Result.PixelFormat := pf24bit;

      // Draw background
      Result.Canvas.Brush.Color := clWhite;
      Result.Canvas.FillRect(0, 0, Result.Width, Result.Height);

      // Copy content from temp bitmap
      Result.Canvas.CopyRect(Rect(0, 0, Result.Width, Result.Height),
        TempBitmap.Canvas, Rect(0, 0, Result.Width, Result.Height));
    finally
      TempBitmap.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TTagEdit.BlendColors(Color1, Color2: TColor; Intensity: integer): TColor;
var
  R1, G1, B1: byte;
  R2, G2, B2: byte;
  Alpha: double;
begin
  // Return original color if no blending needed
  if Intensity <= 0 then
    Exit(Color1);

  // Return full blend color if maximum intensity
  if Intensity >= 100 then
    Exit(Color2);

  // Calculate blend factor (0.0 to 1.0)
  Alpha := Intensity / 100.0;

  // Extract RGB components from first color
  Color1 := ColorToRGB(Color1);
  R1 := GetRValue(Color1);
  G1 := GetGValue(Color1);
  B1 := GetBValue(Color1);

  // Extract RGB components from second color
  Color2 := ColorToRGB(Color2);
  R2 := GetRValue(Color2);
  G2 := GetGValue(Color2);
  B2 := GetBValue(Color2);

  // Linear interpolation: result = color1 * (1-alpha) + color2 * alpha
  Result := RGBToColor(Round(R1 * (1 - Alpha) + R2 * Alpha), Round(G1 * (1 - Alpha) + G2 * Alpha),
    Round(B1 * (1 - Alpha) + B2 * Alpha));
end;

end.
