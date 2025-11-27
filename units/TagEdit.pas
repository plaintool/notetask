//-----------------------------------------------------------------------------------
//  TagEdit © 2025 by Alexander Tverskoy
//  https://github.com/plaintool/TagEdit
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
  TagColorItems,
  TagCheckPopup;

type
  TTagEvent = procedure(Sender: TObject; const TagText: string) of object;
  TTagReorderEvent = procedure(Sender: TObject; const TagText: string; const NewIndex: integer) of object;
  TTagPopupEvent = procedure(Sender: TObject; const TagText: string; var Handled: boolean) of object;

  { TCustomTagEdit }

  TCustomTagEdit = class(TCustomControl)
  private
    FTags: TStringList;
    FTagRects: array of TRect;
    FTagColors: TTagColorItems;
    FSelectedTags: TStringList;
    FSuggestedTags: TStringList;

    FEdit: TEdit;
    FCheckListButton: TCheckListButton;

    FColor: TColor;
    FTagColor: TColor;
    FTagSuffixColor: TColor;
    FTagHoverColor: TColor;
    FTagBorderColor: TColor;
    FBorderColor: TColor;
    FSelectionColor: TColor;
    FSelectionRectColor: TColor;
    FDragIndicatorColor: TColor;

    FAutoSizeHeight: boolean;
    FAllowReorder: boolean;
    FAllowSelect: boolean;
    FAutoSuggest: boolean;
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
    FCloseBtnWidth: integer;

    FSelecting: boolean;
    FSelectionRectPenStyle: TPenStyle;
    FSelectionRectWidth: integer;
    FSelectionStart: TPoint;
    FSelectionRect: TRect;

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
    FSuggestionButtonCaption: TTranslateString;
    FTagEditing: string;

    FFont: TFont;
    FParentFont: boolean;

    FOnTagAdd: TTagEvent;
    FOnTagRemove: TTagEvent;
    FOnTagReorder: TTagReorderEvent;
    FOnTagClick: TTagEvent;
    FOnTagPopup: TTagPopupEvent;
    FOnChange: TNotifyEvent;

    function GetTags: TStringList;
    function GetTagHeight(AFontSize: integer = -1): integer;
    function GetTagRect(Index: integer): TRect;
    function GetHoveredTag: string;
    function GetSuggestedItemsSorted: boolean;
    function GetSuggestedButtonGlyph: TBitmap;
    function GetSuggestedButtonCaption: string;

    procedure SetTags(Value: TStringList);
    procedure SetSuggestedTags(Value: TStringList);
    procedure SetFont(Value: TFont);
    procedure SetParentFont(Value: boolean);
    procedure SetReadOnly(Value: boolean);
    procedure SetTextHint(Value: TTranslateString);
    procedure SetSelectionColor(Value: TColor);
    procedure SetTagColors(Value: TTagColorItems);
    procedure SetSuggestedItemsSorted(Value: boolean);
    procedure SetSuggestedButtonGlyph(AValue: TBitmap);
    procedure SetSuggestedButtonCaption(const AValue: string);

    function RemovalConfirmed(idx: integer = -1): boolean;
    function TagAtPos(const P: TPoint): integer;
    procedure UpdateEditPosition;
    procedure UpdateCheckList(AddSuggestions: boolean = True);
    procedure UpdateHoverState(X, Y: integer);
    function CoalesceInt(const A, B: integer; const C: integer = 0): integer;
    procedure SetAutoSizeHeight(Value: boolean);
    function CalculateAutoHeight: integer;
    function IsInSelectionRect(const R: TRect): boolean;
    procedure UpdateSelection(X, Y: integer);
    procedure UpdateAutoHeight;
    function Scale(const AValue: integer): integer;
    function IndexOf(const AName: string; AItems: TStrings; ACaseSensitive: boolean = True): integer;
    function GetContrastTextColor(BackColor, FontColor: TColor; MidLevel: integer = 128): TColor;
    function RandTagColor(const ATag: string; Brightness, Saturation: integer): TColor;
    function FindTagColor(const S: string): TColor;
    procedure DrawTags;
    procedure DrawTagsToCanvas(const ATags: TStringList; ACanvas: TCanvas; ATagHeight: integer;
      AAvailWidth: integer; AHoverIndex: integer = -1; AShowCloseButtons: boolean = True; var ATagRects: array of TRect;
      AFontSize: integer = -1; AIndent: integer = 4; ABlend: integer = 0; ABlendColor: TColor = clNone; ADrawSelection: boolean = True);
  protected
    procedure SetEnabled(Value: boolean); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure Resize; override;
    procedure SetColor(Value: TColor); override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: boolean); override;
    procedure Paint; override;

    // Event handlers
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseLeave; override;
    procedure EditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure EditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure EditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure EditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure SuggestedChanged(Sender: TObject);
    procedure CheckListItemChecked(Sender: TObject; Index: integer; Checked: boolean);
    procedure TagsChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject); override;
    procedure ParentFontChange(Sender: TObject);
    procedure HandlePopupMenu(Sender: TObject);

    // Properties
    property Height default 32;
    property Width default 300;
    property Tag default 0;
    property Color read FColor write SetColor default clWindow;
    property Font: TFont read FFont write SetFont;
    property AllowReorder: boolean read FAllowReorder write FAllowReorder default True;
    property AllowSelect: boolean read FAllowSelect write FAllowSelect default True;
    property AutoSuggest: boolean read FAutoSuggest write FAutoSuggest default False;
    property AutoSizeHeight: boolean read FAutoSizeHeight write SetAutoSizeHeight default False;
    property AutoColorBrigtness: integer read FAutoColorBrigtness write FAutoColorBrigtness default 80;
    property AutoColorSaturation: integer read FAutoColorSaturation write FAutoColorSaturation default 80;
    property AutoColorSeed: longword read FAutoColorSeed write FAutoColorSeed default 0;
    property CloseButtons: boolean read FCloseButtons write FCloseButtons default True;
    property CloseButtonOnHover: boolean read FCloseButtonOnHover write FCloseButtonOnHover default True;
    property TagHoverUnderline: boolean read FTagHoverUnderline write FTagHoverUnderline default True;
    property BackSpaceEditTag: boolean read FBackspaceEditTag write FBackspaceEditTag default False;
    property TagColor: TColor read FTagColor write FTagColor default clNone;
    property TagSuffixColor: TColor read FTagSuffixColor write FTagSuffixColor default clWhite;
    property TagHoverColor: TColor read FTagHoverColor write FTagHoverColor default clHighlight;
    property TagBorderColor: TColor read FTagBorderColor write FTagBorderColor default clNone;
    property DragIndicatorColor: TColor read FDragIndicatorColor write FDragIndicatorColor default clRed;
    property BorderColor: TColor read FBorderColor write FBorderColor default clWindowFrame;
    property BorderWidth: integer read FBorderWidth write FBorderWidth default 0;
    property RoundCorners: integer read FRoundCorners write FRoundCorners default 5;
    property TagBorderWidth: integer read FTagBorderWidth write FTagBorderWidth default 2;
    property EditMinWidth: integer read FEditMinWidth write FEditMinWidth default 50;
    property RemoveConfirm: boolean read FRemoveConfirm write FRemoveConfirm default True;
    property RemoveConfirmTitle: TTranslateString read FRemoveConfirmTitle write FRemoveConfirmTitle;
    property RemoveConfirmMessage: TTranslateString read FRemoveConfirmMessage write FRemoveConfirmMessage;
    property TextHint: TTranslateString read FTextHint write SetTextHint;
    property ParentFont: boolean read FParentFont write SetParentFont default True;
    property ReadOnly: boolean read FReadOnly write SetReadOnly default False;
    property Enabled: boolean read FEnabled write SetEnabled;
    property TagColors: TTagColorItems read FTagColors write SetTagColors;
    property HoveredTag: string read GetHoveredTag;
    property SuggestedItemsSorted: boolean read GetSuggestedItemsSorted write SetSuggestedItemsSorted default False;
    property SuggestedButtonGlyph: TBitmap read GetSuggestedButtonGlyph write SetSuggestedButtonGlyph;
    property SuggestedButtonCaption: string read GetSuggestedButtonCaption write SetSuggestedButtonCaption;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor default clHighlight;
    property SelectionRectColor: TColor read FSelectionRectColor write FSelectionRectColor default clHighlight;
    property SelectionRectPenStyle: TPenStyle read FSelectionRectPenStyle write FSelectionRectPenStyle default psDash;
    property SelectionRectWidth: integer read FSelectionRectWidth write FSelectionRectWidth default 1;

    property EditBox: TEdit read FEdit;
    property Items: TStringList read GetTags write SetTags;
    property SelectedTags: TStringList read FSelectedTags;
    property SuggestedItems: TStringList read FSuggestedTags write SetSuggestedTags;

    property OnTagClick: TTagEvent read FOnTagClick write FOnTagClick;
    property OnTagPopup: TTagPopupEvent read FOnTagPopup write FOnTagPopup;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnTagAdd: TTagEvent read FOnTagAdd write FOnTagAdd;
    property OnTagRemove: TTagEvent read FOnTagRemove write FOnTagRemove;
    property OnTagReorder: TTagReorderEvent read FOnTagReorder write FOnTagReorder;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddTag(const ATag: string);
    procedure RemoveTag(const ATag: string; AConfirm: boolean = False);
    function Focused: boolean; override;
    procedure CopyHoverText;
    function GetAutoColor(const ATag: string): TColor;
    procedure RemoveSelectedTags;
    procedure FinishEdit;
    procedure SelectAll;
    procedure ClearSelection;
    procedure ScaleFontsPPI(const AToPPI: integer; const AProportion: double); override;
    procedure FixDesignFontsPPI(const ADesignTimePPI: integer); override;
    function BlendColors(Color1, Color2: TColor; Intensity: integer): TColor;
    function GetTagsBitmap(ATags: TStringList; AFontSize, AWidth, AHeight: integer; ATagHeightDelta: integer = 0;
      ABlend: integer = 0; ABlendColor: TColor = clWhite): TBitmap;
  end;

  { TTagEdit }

  TTagEdit = class(TCustomTagEdit)
  public
    property EditBox;
    property HoveredTag;
    property SelectedTags;
  published
    property Align;
    property Anchors;
    property AllowReorder;
    property AllowSelect;
    property AutoSuggest;
    property AutoSizeHeight;
    property BidiMode;
    property BorderSpacing;
    property BorderWidth;
    property BorderColor;
    property RoundCorners;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property Font;
    property Visible;
    property DoubleBuffered;
    property ParentDoubleBuffered;
    property ParentBiDiMode;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Height;
    property Width;
    property Tag;
    property Color;
    property TagHoverUnderline;
    property CloseButtons;
    property CloseButtonOnHover;
    property BackSpaceEditTag;
    property TagColor;
    property TagSuffixColor;
    property TagHoverColor;
    property TagBorderColor;
    property TagBorderWidth;
    property DragIndicatorColor;
    property AutoColorBrigtness;
    property AutoColorSaturation;
    property AutoColorSeed;
    property EditMinWidth;
    property RemoveConfirm;
    property RemoveConfirmTitle;
    property RemoveConfirmMessage;
    property TextHint;
    property ParentFont;
    property ReadOnly;
    property TagColors;
    property SelectionColor;
    property SelectionRectColor;
    property SelectionRectPenStyle;
    property SelectionRectWidth;
    property SuggestedItems;
    property SuggestedItemsSorted;
    property SuggestedButtonCaption;
    property SuggestedButtonGlyph;
    property Items;
    // events
    property OnTagClick;
    property OnTagPopup;
    property OnChange;
    property OnTagAdd;
    property OnTagRemove;
    property OnTagReorder;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnShowHint;
    property OnUTF8KeyPress;
  end;

implementation

{ TCustomTagEdit }

constructor TCustomTagEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTagColors := TTagColorItems.Create(Self);

  FTags := TStringList.Create;
  FTags.Add('IDE:Lazarus');
  FTags.Add('Free Pascal');
  FTags.CaseSensitive := True;
  FTags.UseLocale := True;
  FTags.Options := [soUseLocale];
  FTags.OnChange := @TagsChanged;

  // Initialize selection
  FSelectedTags := TStringList.Create;
  FSelectedTags.CaseSensitive := True;
  FSelectedTags.Delimiter := ';';
  FSelectionColor := clHighlight;
  FSelectionRectColor := clHighlight;
  FSelectionRectPenStyle := psDash;
  FSelectionRectWidth := 1;
  FSelecting := False;
  FSelectionRect := Rect(0, 0, 0, 0);
  FMouseDownPos := Point(-1, -1);

  FSuggestedTags := TStringList.Create;
  FSuggestedTags.CaseSensitive := True;
  FSuggestedTags.Sorted := SuggestedItemsSorted;
  FSuggestedTags.Duplicates := dupIgnore;
  FSuggestedTags.Delimiter := ';';
  FSuggestedTags.OnChange := @SuggestedChanged;
  FSuggestionButtonCaption := '...';

  Height := Scale(32);
  Width := Scale(300);
  FColor := clWindow;
  ParentColor := False;
  FTagEditing := string.Empty;
  FReadOnly := False;
  FEnabled := True;
  FAutoSizeHeight := False;

  Randomize;
  FAutoColorSeed := Random(High(longword));
  FAllowReorder := True;
  FAllowSelect := True;
  FAutoSuggest := False;
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

  // Create inner edit control
  FEdit := TEdit.Create(Self);
  FCheckListButton := TCheckListButton.Create(Self);
  if not (csDesigning in ComponentState) then
  begin
    FEdit.Parent := Self;
    FEdit.DoubleBuffered := FDoubleBuffered;
    FEdit.AutoSize := False;
    FEdit.TextHint := FTextHint;
    FEdit.BiDiMode := BiDiMode;
    FEdit.ParentFont := True;
    FEdit.BorderStyle := bsNone;
    FEdit.OnKeyDown := @EditKeyDown;
    FEdit.OnKeyUp := OnKeyUp;
    FEdit.OnKeyPress := OnKeyPress;
    FEdit.Color := Color;
    FEdit.Left := 4;
    FEdit.Top := 4;

    FCheckListButton.Parent := Self;
    FCheckListButton.Flat := True;
    FCheckListButton.Sorted := SuggestedItemsSorted;
    FCheckListButton.Visible := False;
    FCheckListButton.MultiSelect := True;
    FCheckListButton.DropDownCount := 15;
    FCheckListButton.AttachedControl := FEdit;
    FCheckListButton.OnItemChecked := @CheckListItemChecked;

    FEdit.OnMouseDown := @EditMouseDown;
    FEdit.OnMouseMove := @EditMouseMove;
    FEdit.OnMouseUp := @EditMouseUp;
  end;

  FRemoveConfirm := True;
  FRemoveConfirmMessage := 'Are you sure you want to remove tag(s)';
  FRemoveConfirmTitle := 'Remove tag(s)';

  FDragging := False;
  FDragIndex := -1;
  FDropIndex := -1;
  FUpdatingEdit := False;
  FHoverIndex := -1;
  FMouseDownIndex := -1;
end;

destructor TCustomTagEdit.Destroy;
begin
  FTags.Free;
  FFont.Free;
  FEdit.Free;
  FTagColors.Free;
  FSelectedTags.Free;
  FCheckListButton.Free;
  FSuggestedTags.Free;
  inherited Destroy;
end;

function TCustomTagEdit.GetTags: TStringList;
begin
  Result := FTags;
end;

function TCustomTagEdit.GetTagHeight(AFontSize: integer = -1): integer;
begin
  Result := Scale(ifthen(AFontSize > -1, AFontSize, CoalesceInt(Font.Size, Screen.SystemFont.Size, 8)) * 2 + 6);
end;

function TCustomTagEdit.GetTagRect(Index: integer): TRect;
begin
  if (Index >= 0) and (Index <= High(FTagRects)) then
    Result := FTagRects[Index]
  else
    Result := Rect(0, 0, 0, 0);
end;

function TCustomTagEdit.GetHoveredTag: string;
begin
  if (FHoverIndex >= 0) then
    Result := FTags[FHoverIndex]
  else
    Result := string.Empty;
end;

function TCustomTagEdit.GetSuggestedItemsSorted: boolean;
begin
  Result := FSuggestedTags.Sorted;
end;

function TCustomTagEdit.GetSuggestedButtonGlyph: TBitmap;
begin
  // return glyph of internal button
  Result := FCheckListButton.Glyph;
end;

function TCustomTagEdit.GetSuggestedButtonCaption: string;
begin
  // return caption of internal button
  Result := FSuggestionButtonCaption;
end;

procedure TCustomTagEdit.SetTags(Value: TStringList);
begin
  FTags.Assign(Value);
  Invalidate;
  UpdateAutoHeight;
end;

procedure TCustomTagEdit.SetSuggestedTags(Value: TStringList);
begin
  FSuggestedTags.Assign(Value);
  SuggestedChanged(Self);
end;

procedure TCustomTagEdit.SetColor(Value: TColor);
begin
  inherited;
  FColor := Value;
  if Assigned(FEdit) then
    FEdit.Color := Value;
end;

procedure TCustomTagEdit.SetTagColors(Value: TTagColorItems);
begin
  FTagColors.Assign(Value);
  Invalidate;
end;

procedure TCustomTagEdit.SetSelectionColor(Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    Invalidate;
  end;
end;

procedure TCustomTagEdit.SetTextHint(Value: TTranslateString);
begin
  FTextHint := Value;
  if Assigned(FEdit) then
    FEdit.TextHint := Value;
end;

procedure TCustomTagEdit.SetReadOnly(Value: boolean);
begin
  FReadOnly := Value;
  FEdit.Visible := not Value and FEnabled;
  Invalidate;
end;

procedure TCustomTagEdit.SetEnabled(Value: boolean);
begin
  inherited;
  FEnabled := Value;
  FEdit.Visible := Value and not FReadOnly;
end;

procedure TCustomTagEdit.SetAutoSizeHeight(Value: boolean);
begin
  if FAutoSizeHeight <> Value then
  begin
    FAutoSizeHeight := Value;
    if FAutoSizeHeight then
      UpdateAutoHeight;
  end;
end;

procedure TCustomTagEdit.SetFont(Value: TFont);
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

procedure TCustomTagEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FParentFont and (AParent <> nil) then
  begin
    FFont.Assign(AParent.Font);
    FEdit.Font.Assign(AParent.Font);
    SetParentFont(True);
  end;
end;

procedure TCustomTagEdit.SetParentFont(Value: boolean);
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

procedure TCustomTagEdit.SetSuggestedItemsSorted(Value: boolean);
begin
  FSuggestedTags.Sorted := Value;
  FCheckListButton.Sorted := Value;
end;

procedure TCustomTagEdit.SetSuggestedButtonCaption(const AValue: string);
begin
  // assign caption to internal button
  FSuggestionButtonCaption := AValue;
  FCheckListButton.Caption := FSuggestionButtonCaption;
end;

procedure TCustomTagEdit.SetSuggestedButtonGlyph(AValue: TBitmap);
begin
  // assign glyph to internal button
  if Assigned(AValue) then
    FCheckListButton.Glyph.Assign(AValue)
  else
    FCheckListButton.Glyph.Clear;
end;

procedure TCustomTagEdit.UpdateEditPosition;
var
  LastRect: TRect;
  NewLeft, NewTop: integer;
  AvailWidth: integer;
begin
  if FUpdatingEdit or (csDesigning in ComponentState) then Exit;

  FUpdatingEdit := True;
  try
    // Calculate available width considering borders
    AvailWidth := ClientWidth - FBorderWidth - Scale(8);
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
        NewLeft := FBorderWidth + Scale(4);
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

    if (not FEdit.Visible) or (FSuggestedTags.Count = 0) then
    begin
      FCheckListButton.Width := 0;
      FCheckListButton.Visible := False;
    end
    else
    begin
      if not FCheckListButton.Visible then
      begin
        FCheckListButton.Width := Scale(30);
        FCheckListButton.Visible := True;
      end;
    end;

    // Set Edit width
    FEdit.Width := ClientWidth - FBorderWidth - FEdit.Left - FCheckListButton.Width - Scale(4);
    if FEdit.Width < FEditMinWidth - FCheckListButton.Width then
      FEdit.Width := FEditMinWidth - FCheckListButton.Width;
    FEdit.Height := GetTagHeight - Scale(2);

    // Set CheckListButton
    if (FSuggestedTags.Count > 0) then
    begin
      FCheckListButton.Left := FEdit.Left + FEdit.Width;
      FCheckListButton.Top := FEdit.Top;
      FCheckListButton.Height := FEdit.Height;
    end;
  finally
    FUpdatingEdit := False;
  end;
end;

procedure TCustomTagEdit.UpdateCheckList(AddSuggestions: boolean = True);
var
  i: integer;
begin
  if not Assigned(FCheckListButton) or (csDesigning in ComponentState) then exit;

  FCheckListButton.UncheckAll;
  if AddSuggestions and FAutoSuggest then
    for i := 0 to FTags.Count - 1 do
    begin
      if (IndexOf(FTags[i], FSuggestedTags) < 0) then
        FSuggestedTags.Add(FTags[i]);

      if (IndexOf(FTags[i], FCheckListButton.Items) < 0) then
        FCheckListButton.Items.Add(FTags[i]);
    end;

  for i := 0 to FTags.Count - 1 do
    FCheckListButton.CheckedByName[FTags[i]] := True;
end;

procedure TCustomTagEdit.UpdateHoverState(X, Y: integer);
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

procedure TCustomTagEdit.UpdateSelection(X, Y: integer);
var
  I: integer;
  TagRect: TRect;
begin
  // Update selection rectangle
  FSelectionRect := Rect(Min(FSelectionStart.X, X), Min(FSelectionStart.Y, Y), Max(FSelectionStart.X, X), Max(FSelectionStart.Y, Y));

  // Update selected tags based on selection rectangle
  for I := 0 to FTags.Count - 1 do
  begin
    TagRect := GetTagRect(I);
    if IsInSelectionRect(TagRect) then
    begin
      // Add to selection if not already selected
      if IndexOf(FTags[I], FSelectedTags) = -1 then
        FSelectedTags.Add(FTags[I]);
    end
    else
    begin
      // Remove from selection if no longer in selection rect
      if IndexOf(FTags[I], FSelectedTags) <> -1 then
        FSelectedTags.Delete(IndexOf(FTags[I], FSelectedTags));
    end;
  end;
  Invalidate;
end;

procedure TCustomTagEdit.UpdateAutoHeight;
begin
  if FAutoSizeHeight and (not (Align in [alClient, alRight, alLeft])) then
  begin
    Height := CalculateAutoHeight;
  end;
end;

function TCustomTagEdit.Scale(const AValue: integer): integer;
begin
  Result := Scale96ToScreen(AValue);
end;

function TCustomTagEdit.IndexOf(const AName: string; AItems: TStrings; ACaseSensitive: boolean = True): integer;
var
  i: integer;
  s1, s2: string;
begin
  for i := 0 to AItems.Count - 1 do
  begin
    if ACaseSensitive then
    begin
      if AItems[i] = AName then
        Exit(i);
    end
    else
    begin
      s1 := AItems[i];
      s2 := AName;
      if CompareText(s1, s2) = 0 then
        Exit(i);
    end;
  end;
  Result := -1;
end;

procedure TCustomTagEdit.ScaleFontsPPI(const AToPPI: integer; const AProportion: double);
begin
  inherited ScaleFontsPPI(AToPPI, AProportion);
  if ((not FParentFont) and (FFont.Size <= 0)) or ((FParentFont) and (Assigned(Parent)) and (Parent.Font.Size <= 0)) then
    FFont.Size := CoalesceInt(Screen.SystemFont.Size, 8);
  DoScaleFontPPI(FFont, AToPPI, AProportion);
  if (Assigned(Parent)) and (FParentFont) then
    DoScaleFontPPI(Parent.Font, AToPPI, AProportion);
end;

procedure TCustomTagEdit.FixDesignFontsPPI(const ADesignTimePPI: integer);
begin
  inherited FixDesignFontsPPI(ADesignTimePPI);
  if ((not FParentFont) and (FFont.Size <= 0)) or ((FParentFont) and (Assigned(Parent)) and (Parent.Font.Size <= 0)) then
    FFont.Size := CoalesceInt(Screen.SystemFont.Size, 8);
  DoFixDesignFontPPI(FFont, ADesignTimePPI);
  if (Assigned(Parent)) and (FParentFont) then
    DoFixDesignFontPPI(Parent.Font, ADesignTimePPI);
end;

function TCustomTagEdit.Focused: boolean;
begin
  Result := FEdit.Focused;
end;

procedure TCustomTagEdit.AddTag(const ATag: string);
var
  SL: TStringList;
  i: integer;
  s: string;
  TagsAdded: boolean;
  EditValue: string;
begin
  if ATag = string.Empty then
    Exit;

  TagsAdded := False;
  EditValue := FEdit.Text;
  FEdit.Clear;
  SL := TStringList.Create;
  try
    // Split by semicolon if present, otherwise just add the single tag
    if Pos(';', ATag) > 0 then
      ExtractStrings([';'], [], PChar(ATag), SL)
    else
      SL.Add(ATag);

    for i := 0 to SL.Count - 1 do
    begin
      s := Trim(SL[i]);
      // Skip empty or duplicate tags
      if (s <> '') and (IndexOf(s, FTags) = -1) then
      begin
        FTags.Add(s);
        TagsAdded := True;
        if Assigned(FOnTagAdd) then
          FOnTagAdd(Self, s);
      end;
    end;
  finally
    SL.Free;
  end;

  // Update UI and call events if tags were added
  if TagsAdded then
  begin
    if Assigned(FOnChange) then
      FOnChange(Self);
    Invalidate;
    UpdateAutoHeight;
  end
  else
    FEdit.Text := EditValue;
end;

procedure TCustomTagEdit.RemoveTag(const ATag: string; AConfirm: boolean = False);
var
  i: integer;
begin
  i := IndexOf(ATag, FTags);

  if AConfirm and not RemovalConfirmed(i) then exit;

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

procedure TCustomTagEdit.RemoveSelectedTags;
var
  I: integer;
  TagToRemove: string;
begin
  if FSelectedTags.Count = 0 then
    Exit;

  if not RemovalConfirmed then exit;

  // Remove tags from the main list
  for I := FSelectedTags.Count - 1 downto 0 do
  begin
    TagToRemove := FSelectedTags[I];

    // Call OnTagRemove event if assigned
    if Assigned(FOnTagRemove) then
      FOnTagRemove(Self, TagToRemove);

    // Remove from main tags list
    FTags.Delete(IndexOf(TagToRemove, FTags));
  end;

  // Clear selection
  FSelectedTags.Clear;

  // Call OnChange event if assigned
  if Assigned(FOnChange) then
    FOnChange(Self);

  // Update visual
  Invalidate;
  UpdateAutoHeight;
end;

function TCustomTagEdit.RemovalConfirmed(idx: integer = -1): boolean;
begin
  if not RemoveConfirm then
    exit(True);
  if (idx >= 0) then
    Result := MessageDlg(FRemoveConfirmTitle, FRemoveConfirmMessage + ' "' + FTags[idx] + '"?', mtConfirmation, [mbYes, mbNo], 0) = mrYes
  else
    Result := MessageDlg(FRemoveConfirmTitle, FRemoveConfirmMessage + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

function TCustomTagEdit.CalculateAutoHeight: integer;
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

  X := Scale(4) + FBorderWidth;
  Y := Scale(4) + FBorderWidth;

  // Simulate tag layout to find bottom position
  for I := 0 to FTags.Count - 1 do
  begin
    W := Canvas.TextWidth(FTags[I]) + GetTagHeight;

    // Wrap to next line if tag doesn't fit
    if (I > 0) and ((X + W) > AvailWidth) then
    begin
      X := Scale(4) + FBorderWidth;
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
    Inc(Result, FBorderWidth);
end;

function TCustomTagEdit.CoalesceInt(const A, B: integer; const C: integer = 0): integer;
begin
  if A <> 0 then
    Result := A
  else
  if B <> 0 then
    Result := B
  else
    Result := C;
end;

procedure TCustomTagEdit.Resize;
begin
  inherited Resize;
  UpdateAutoHeight;
end;

function TCustomTagEdit.TagAtPos(const P: TPoint): integer;
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

function TCustomTagEdit.IsInSelectionRect(const R: TRect): boolean;
begin
  Result := (FSelectionRect.Left <= R.Right) and (FSelectionRect.Right >= R.Left) and (FSelectionRect.Top <= R.Bottom) and
    (FSelectionRect.Bottom >= R.Top);
end;

procedure TCustomTagEdit.CopyHoverText;
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
  end;
end;

function TCustomTagEdit.GetAutoColor(const ATag: string): TColor;
begin
  Result := RandTagColor(ATag, FAutoColorBrigtness, FAutoColorSaturation);
end;

procedure TCustomTagEdit.FinishEdit;
begin
  if FEdit.Text <> string.Empty then
    AddTag(Trim(FEdit.Text));
  FTagEditing := string.Empty;
end;

procedure TCustomTagEdit.ClearSelection;
begin
  if FSelectedTags.Count > 0 then
  begin
    FSelectedTags.Clear;
    Invalidate;
  end;
end;

procedure TCustomTagEdit.SelectAll;
var
  I: integer;
begin
  FSelectedTags.Clear;
  for I := 0 to FTags.Count - 1 do
    FSelectedTags.Add(FTags[I]);
  Invalidate;
end;

procedure TCustomTagEdit.ParentFontChange(Sender: TObject);
begin
  if FParentFont and (Parent <> nil) then
  begin
    FFont.Assign(Parent.Font);
    FEdit.Font.Assign(Parent.Font);
    Invalidate;
    UpdateAutoHeight;
  end;
end;

procedure TCustomTagEdit.FontChanged(Sender: TObject);
begin
  inherited;
  FEdit.Font.Assign(Font);
  if (Assigned(FCheckListButton)) then
    FCheckListButton.Font.Assign(Font);
  if (Assigned(Parent)) and (not FFont.IsEqual(Parent.Font)) and (not FFont.IsDefault) then
    FParentFont := False;
  Invalidate;
  UpdateAutoHeight;
end;

procedure TCustomTagEdit.TagsChanged(Sender: TObject);
begin
  FHoverIndex := -1; // Reset hover state when Items change
  Invalidate;
  UpdateAutoHeight;
  UpdateCheckList;
end;

procedure TCustomTagEdit.SuggestedChanged(Sender: TObject);
var
  i, idx: integer;
  NewValue: TStringList;
begin
  if csDesigning in ComponentState then exit;

  if (FSuggestedTags.Count = 0) then
    FCheckListButton.Clear
  else
  // Reassign new items
  if FCheckListButton.Items.Count = 0 then
    FCheckListButton.Items.Assign(FSuggestedTags)
  else
  begin
    NewValue := TStringList.Create;
    try
      NewValue.Sorted := True;
      NewValue.Assign(FSuggestedTags);

      // Insert items at the same positions if they don't exist
      for i := 0 to NewValue.Count - 1 do
      begin
        // get index of the tag in target list
        // comment: search tag in destination list
        idx := IndexOf(NewValue[i], FCheckListButton.Items);
        if idx < 0 then
        begin
          // comment: insert at the same position (or at the end if index is out of range)
          if i <= FCheckListButton.Items.Count then
            FCheckListButton.Items.Insert(i, NewValue[i])
          else
            FCheckListButton.Items.Add(NewValue[i]);
        end;
      end;
    finally
      NewValue.Free;
    end;

    // remove items not present in FSuggestedTags — iterate backwards to avoid index shift
    for i := FCheckListButton.Items.Count - 1 downto 0 do
    begin
      if IndexOf(FCheckListButton.Items[i], FSuggestedTags) < 0 then
        FCheckListButton.Items.Delete(i);
    end;
  end;

  UpdateCheckList(False);
  UpdateEditPosition;
  Invalidate;
end;

procedure TCustomTagEdit.CheckListItemChecked(Sender: TObject; Index: integer; Checked: boolean);
begin
  if csDesigning in ComponentState then exit;

  if Checked then
    AddTag(FCheckListButton.Items[Index])
  else
    RemoveTag(FCheckListButton.Items[Index]);
end;

procedure TCustomTagEdit.EditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  ATag: string;
begin
  if csDesigning in ComponentState then exit;

  // Enter adds a new tag (handles multiple tags separated by ';')
  if Key = VK_RETURN then
  begin
    if FEdit.Text <> string.Empty then
      AddTag(FEdit.Text)
    else
    if Assigned(FOnChange) and (FTagEditing <> string.Empty) then
      FOnChange(Sender);

    FTagEditing := string.Empty;
    Key := 0;
  end
  else
  // Backspace removes last tag if edit is empty
  if (Key = VK_BACK) and (FEdit.Text = string.Empty) and (FTags.Count > 0) and (FBackspaceEditTag or
    RemovalConfirmed(FTags.Count - 1)) then
  begin
    ATag := FTags[FTags.Count - 1];
    FTags.Delete(FTags.Count - 1);
    if FBackSpaceEditTag then
    begin
      FTagEditing := ATag;
      FEdit.Text := FTagEditing;
      FEdit.SelStart := FEdit.GetTextLen;
      UpdateEditPosition;
      Repaint;
    end
    else
    begin
      if Assigned(FOnTagRemove) then
        FOnTagRemove(Sender, ATag);
      if Assigned(FOnChange) then
        FOnChange(Sender);
    end;
    UpdateAutoHeight;
    Invalidate;
    Key := 0;
  end
  else
  if (Key = VK_DELETE) and (SelectedTags.Count > 0) then
  begin
    RemoveSelectedTags;
    Key := 0;
  end
  else
  if (Key = VK_ESCAPE) then
  begin
    if (SelectedTags.Count > 0) then
    begin
      ClearSelection;
      Key := 0;
    end;
    if (FEdit.Text <> string.Empty) or (FTagEditing <> string.Empty) then
    begin
      if FTagEditing = string.Empty then
        FEdit.Text := string.Empty
      else
      begin
        FEdit.Text := FTagEditing;
        FinishEdit;
      end;
      Key := 0;
    end;
  end
  else if (FAllowSelect) and (ssCtrl in Shift) and (Key = VK_A) and (FEdit.Text = string.Empty) then
  begin
    SelectAll;
    Key := 0;
  end;

  if Assigned(OnKeyDown) then
    OnKeyDown(Sender, Key, Shift);
end;

procedure TCustomTagEdit.EditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  GlobalPos: TPoint;
begin
  if csDesigning in ComponentState then exit;

  // RemoveSelectedTags selection if exists
  if FSelectedTags.Count > 0 then ClearSelection;

  // If Edit has text, let it handle the click normally for text editing
  if FEdit.Text <> string.Empty then
  begin
    // Just ensure Edit has focus for text editing
    if FEdit.CanFocus then
      FEdit.SetFocus;
    Exit;
  end;

  // Convert coordinates from FEdit coordinate system to TCustomTagEdit coordinate system
  GlobalPos := FEdit.ClientToScreen(Point(X, Y));
  GlobalPos := Self.ScreenToClient(GlobalPos);

  // Store mouse down position for drag detection
  FMouseDownPos := GlobalPos;

  // Don't start selection immediately - wait for mouse movement
  // Just store the position for potential selection start
end;

procedure TCustomTagEdit.EditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  GlobalPos: TPoint;
  DragThreshold: integer;
begin
  if csDesigning in ComponentState then exit;

  // If Edit has text, let it handle mouse movement normally
  if FEdit.Text <> '' then
    Exit;

  // Convert coordinates from FEdit coordinate system to TCustomTagEdit coordinate system
  GlobalPos := FEdit.ClientToScreen(Point(X, Y));
  GlobalPos := Self.ScreenToClient(GlobalPos);

  // Check if we should start selection (mouse moved with button pressed beyond threshold)
  if (FAllowSelect) and (ssLeft in Shift) and (FMouseDownPos.X <> -1) then
  begin
    DragThreshold := Scale(2); // pixels threshold to start selection

    if (Abs(GlobalPos.X - FMouseDownPos.X) > DragThreshold) or (Abs(GlobalPos.Y - FMouseDownPos.Y) > DragThreshold) then
    begin
      // Start selection in parent component
      FSelecting := True;
      FEdit.Visible := False;

      FSelectionStart := FMouseDownPos;
      FSelectionRect := Rect(FMouseDownPos.X, FMouseDownPos.Y, GlobalPos.X, GlobalPos.Y);

      // RemoveSelectedTags previous selection unless Ctrl is pressed
      if not (ssCtrl in Shift) then
        ClearSelection;
    end;
  end;

  // Update selection if we're in selection mode
  if FSelecting then
  begin
    UpdateSelection(GlobalPos.X, GlobalPos.Y);
  end
  else
  begin
    // Call the MouseMove method for hover effects
    MouseMove(Shift, GlobalPos.X, GlobalPos.Y);
  end;
end;

procedure TCustomTagEdit.EditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  GlobalPos: TPoint;
begin
  if csDesigning in ComponentState then exit;

  // Convert coordinates from FEdit coordinate system to TCustomTagEdit coordinate system
  GlobalPos := FEdit.ClientToScreen(Point(X, Y));
  GlobalPos := Self.ScreenToClient(GlobalPos);

  // If it was a simple click (not a drag) and Edit is empty, focus the Edit
  if (FEdit.Text = string.Empty) and (not FSelecting) and (FMouseDownPos.X <> -1) then
  begin
    // Check if it was a simple click (minimal movement)
    if (Abs(GlobalPos.X - FMouseDownPos.X) <= Scale(2)) and (Abs(GlobalPos.Y - FMouseDownPos.Y) <= Scale(2)) then
    begin
      if FEdit.CanFocus then
        FEdit.SetFocus;
    end;
  end;

  // End selection if we were selecting
  if FSelecting then
  begin
    FSelecting := False;
    FEdit.Visible := Enabled and not ReadOnly;
    if FEdit.Visible and FEdit.CanFocus then FEdit.SetFocus;

    FSelectionRect := Rect(0, 0, 0, 0);
    Invalidate;
  end;

  // Reset mouse down position
  FMouseDownPos := Point(-1, -1);

  // Call the MouseUp method for any cleanup
  MouseUp(Button, Shift, GlobalPos.X, GlobalPos.Y);
end;

procedure TCustomTagEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  idx: integer;
  R: TRect;
  IsEditArea: boolean;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if csDesigning in ComponentState then exit;

  FMouseDownPos := Point(X, Y);

  if Button = mbLeft then
  begin
    idx := TagAtPos(Point(X, Y));
    FMouseDownIndex := idx;
    FMouseDownPos := Point(X, Y);

    // Check if mouse down is in edit area or empty space (not on a tag)
    IsEditArea := (idx = -1) and (FEdit.Visible) and (X >= FEdit.Left) and (X <= FEdit.Left + FEdit.Width) and
      (Y >= FEdit.Top) and (Y <= FEdit.Top + FEdit.Height);

    if (not FReadOnly) and (FCloseButtons) and (idx >= 0) then
    begin
      // Existing close button logic
      R := GetTagRect(idx);

      // Click near right edge removes the tag - do it immediately
      if (X > R.Right - FCloseBtnWidth - Scale(2)) and RemovalConfirmed(idx) then
      begin
        RemoveTag(FTags[idx]);
        FMouseDownIndex := -1; // Reset since we handled it
        if FEdit.Visible and FEdit.CanFocus then FEdit.SetFocus;
        exit;
      end;
    end;

    // Start selection if clicking in empty space or edit area
    if (FAllowSelect) and (not FAllowReorder or (idx = -1) or IsEditArea) then
    begin
      FSelecting := True;
      FEdit.Visible := False;

      FSelectionStart := Point(X, Y);
      FSelectionRect := Rect(X, Y, X, Y);

      // RemoveSelectedTags previous selection unless Ctrl is pressed
      if not (ssCtrl in Shift) then
        ClearSelection;

      if FEdit.Visible and FEdit.CanFocus then FEdit.SetFocus;
    end;

  end;
end;

procedure TCustomTagEdit.MouseMove(Shift: TShiftState; X, Y: integer);
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
  if not FDragging and not FSelecting then
    UpdateHoverState(X, Y);

  // Handle selection
  if FSelecting and (ssLeft in Shift) then
  begin
    UpdateSelection(X, Y);
    Exit;
  end;

  // Start dragging only if mouse moved beyond threshold and we have a valid tag index
  if FAllowReorder and not FReadOnly and not FDragging and (ssLeft in Shift) and (FMouseDownIndex >= 0) then
  begin
    DragThreshold := Scale(5); // pixels
    if (Abs(X - FMouseDownPos.X) > DragThreshold) or (Abs(Y - FMouseDownPos.Y) > DragThreshold) then
    begin
      FDragging := True;
      FDragIndex := FMouseDownIndex;
      FDropIndex := FMouseDownIndex;
      // RemoveSelectedTags selection when starting drag
      ClearSelection;
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

procedure TCustomTagEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  TempTag: string;
  idx, M: integer;
  R: TRect;
  DragThreshold: integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if csDesigning in ComponentState then exit;

  if Button = mbLeft then
  begin
    // Handle selection completion
    if FSelecting then
    begin
      FSelecting := False;
      FEdit.Visible := FEnabled and not FReadOnly;
      if FEdit.Visible and FEdit.CanFocus then FEdit.SetFocus;

      // If selection rectangle is very small, treat as click
      DragThreshold := Scale(2);
      if (Abs(X - FSelectionStart.X) < DragThreshold) and (Abs(Y - FSelectionStart.Y) < DragThreshold) then
      begin
        // Single click in empty space - RemoveSelectedTags selection
        ClearSelection;
      end;

      FSelectionRect := Rect(0, 0, 0, 0);
      Invalidate;
    end
    // Handle click event if we didn't drag and it's the same tag we pressed
    else if not FDragging and (FMouseDownIndex >= 0) then
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
          // Handle selection on click
          if (FAllowSelect) and (ssCtrl in Shift) then
          begin
            // Toggle selection with Ctrl
            if IndexOf(FTags[idx], FSelectedTags) = -1 then
              FSelectedTags.Add(FTags[idx])
            else
              FSelectedTags.Delete(IndexOf(FTags[idx], FSelectedTags));
          end
          else
          begin
            // Select single tag without Ctrl
            if (FSelectedTags.Count > 0) then
              ClearSelection;

            // Generate OnTagClick event
            if Assigned(FOnTagClick) and (not FSelecting) then
              FOnTagClick(Self, FTags[idx]);
          end;

          Invalidate;
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

procedure TCustomTagEdit.MouseLeave;
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

  {$IFDEF UNIX}
  if FSelecting then
  begin
    FSelecting := False;
    FEdit.Visible := FEnabled and not FReadOnly;
    if FEdit.Visible and FEdit.CanFocus then FEdit.SetFocus;
    FSelectionRect := Rect(0, 0, 0, 0);
    Invalidate;
  end
  {$ENDIF}
end;

procedure TCustomTagEdit.HandlePopupMenu(Sender: TObject);
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

procedure TCustomTagEdit.DoContextPopup(MousePos: TPoint; var Handled: boolean);
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

function TCustomTagEdit.GetContrastTextColor(BackColor, FontColor: TColor; MidLevel: integer = 128): TColor;
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

function TCustomTagEdit.RandTagColor(const ATag: string; Brightness, Saturation: integer): TColor;
var
  Hash: longword;
  R, G, B: byte;
  H, S_val, L: single;
  i: integer;
  TagName: string;

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
  TagName := lowercase(ATag);
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

function TCustomTagEdit.FindTagColor(const S: string): TColor;
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

procedure TCustomTagEdit.Paint;
var
  R: TRect;
begin
  // Draw component background
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  // Draw component border if needed
  if FBorderWidth > 0 then
  begin
    Canvas.Brush.Color := FBorderColor;
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Style := psClear;

    // Top side
    Canvas.FillRect(
      Rect(ClientRect.Left, ClientRect.Top, ClientRect.Right, ClientRect.Top + FBorderWidth)
      );

    // Bottom side
    Canvas.FillRect(
      Rect(ClientRect.Left, ClientRect.Bottom - FBorderWidth, ClientRect.Right, ClientRect.Bottom)
      );

    // Left side
    Canvas.FillRect(
      Rect(ClientRect.Left, ClientRect.Top + FBorderWidth, ClientRect.Left + FBorderWidth, ClientRect.Bottom - FBorderWidth)
      );

    // Right side
    Canvas.FillRect(
      Rect(ClientRect.Right - FBorderWidth, ClientRect.Top + FBorderWidth, ClientRect.Right, ClientRect.Bottom - FBorderWidth)
      );
  end;

  DrawTags;

  // Draw selection rectangle during selection
  if FSelecting and not IsRectEmpty(FSelectionRect) then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := FSelectionRectColor;
    Canvas.Pen.Style := FSelectionRectPenStyle;
    Canvas.Pen.Width := FSelectionRectWidth;
    Canvas.Rectangle(FSelectionRect);
  end;

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

procedure TCustomTagEdit.DrawTags;
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

procedure TCustomTagEdit.DrawTagsToCanvas(const ATags: TStringList; ACanvas: TCanvas; ATagHeight: integer;
  AAvailWidth: integer; AHoverIndex: integer = -1; AShowCloseButtons: boolean = True; var ATagRects: array of TRect;
  AFontSize: integer = -1; AIndent: integer = 4; ABlend: integer = 0; ABlendColor: TColor = clNone; ADrawSelection: boolean = True);
var
  i: integer;
  R: TRect;
  s, Part1, Part2: string;
  X, Y, W, H, CloseBtnWidth: integer;
  SepW: integer = 0;
  Color1: Tcolor = clNone;
  Color2: TColor = clNone;
  FontColor1: Tcolor = clNone;
  FontColor2: TColor = clNone;
  HasColon, Hover: boolean;
  IsSelected: boolean;
begin
  ACanvas.Font.Assign(Font);
  if AFontSize > -1 then
    ACanvas.Font.Size := AFontSize;
  ACanvas.AntialiasingMode := amOn;

  // If ATagRects is passed, ensure it has the correct length
  if Length(ATagRects) <> ATags.Count then
    Exit;

  X := Scale(AIndent) + FBorderWidth;
  Y := Scale(AIndent) + FBorderWidth;
  H := ATagHeight;

  for i := 0 to ATags.Count - 1 do
  begin
    s := ATags[i];
    Hover := (i = AHoverIndex) and ((FTagHoverColor <> clNone) or FTagHoverUnderline);
    IsSelected := IndexOf(s, FSelectedTags) <> -1;

    // Split tag by colon
    HasColon := Pos(':', s) > 0;
    if HasColon then
    begin
      Part1 := Trim(Copy(s, 1, Pos(':', s) - 1));
      Part2 := Trim(Copy(s, Pos(':', s) + 1, MaxInt));
      if Trim(Part1) = string.Empty then
      begin
        HasColon := False;
        Part1 := Part2;
      end;
      if Trim(Part2) = string.Empty then
        HasColon := False;
    end
    else
    begin
      Part1 := s;
      Part2 := string.Empty;
    end;

    W := ACanvas.TextWidth(s) + ATagHeight;

    if (i > 0) and ((X + W) > AAvailWidth) then
    begin
      X := Scale(AIndent) + FBorderWidth;
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

      SepW := ACanvas.TextWidth(Part1 + ' ') + Scale(6); // width of first part + left padding

      if ADrawSelection and IsSelected then
      begin
        ACanvas.Brush.Color := FSelectionColor;
        ACanvas.FillRect(R.Left - Scale(3), R.Top - Scale(3), R.Right + Scale(3), R.Bottom + Scale(3));
        Color1 := BlendColors(Color1, FSelectionColor, 30);
        Color2 := BlendColors(Color2, FSelectionColor, 30);
        ACanvas.Pen.Color := BlendColors(ACanvas.Pen.Color, FSelectionColor, 30);
      end;

      // Left part background
      ACanvas.Brush.Color := Color1;
      ACanvas.RoundRect(R.Left, R.Top, R.Left + SepW, R.Bottom, FRoundCorners, FRoundCorners);

      // Right part background
      ACanvas.Brush.Color := Color2;
      ACanvas.RoundRect(R.Left + SepW, R.Top, R.Right, R.Bottom, FRoundCorners, FRoundCorners);

      // Fill junction to avoid double-rounded corner visual
      ACanvas.Brush.Color := Color1;
      ACanvas.FillRect(R.Left + Min(FRoundCorners, SepW div 2), R.Top, R.Left + SepW, R.Bottom - 1);
      ACanvas.Brush.Color := Color2;
      ACanvas.FillRect(R.Left + SepW, R.Top, R.Right - Min(FRoundCorners, R.Height div 2), R.Bottom - 1);

      if (FTagBorderWidth > 0) then
      begin
        ACanvas.Brush.Color := ACanvas.Pen.Color;

        ACanvas.FillRect(
          R.Left + Min(FRoundCorners, SepW div 2),
          R.Top - (FTagBorderWidth div 2),
          R.Right - Min(FRoundCorners, R.Height div 2),
          R.Top + FTagBorderWidth - (FTagBorderWidth div 2));

        ACanvas.FillRect(
          R.Left + Min(FRoundCorners, SepW div 2),
          R.Bottom - (FTagBorderWidth div 2) - 1,
          R.Right - Min(FRoundCorners, R.Height div 2),
          R.Bottom + FTagBorderWidth - (FTagBorderWidth div 2) - 1);
      end;
    end
    else
    begin
      if Hover and (FTagHoverColor <> clNone) then
        Color1 := FTagHoverColor
      else
      begin
        Color1 := FindTagColor(s);
        if Color1 = clNone then
        begin
          if FTagColor <> clNone then
            Color1 := FTagColor
          else
            Color1 := RandTagColor(Trim(s), FAutoColorBrigtness, FAutoColorSaturation);
        end;
      end;

      if Hover and (FTagHoverColor <> clNone) then
        ACanvas.Pen.Color := FTagHoverColor
      else
      if FTagBorderColor = clNone then
        ACanvas.Pen.Color := Color1;

      ACanvas.Font.Color := GetContrastTextColor(Color1, Font.Color, 150);

      if ABlendColor <> clNone then
      begin
        Color1 := BlendColors(Color1, ABlendColor, ABlend);
        ACanvas.Pen.Color := BlendColors(ACanvas.Pen.Color, ABlendColor, ABlend);
        ACanvas.Font.Color := BlendColors(ACanvas.Font.Color, ABlendColor, ABlend);
      end;

      if ADrawSelection and IsSelected then
      begin
        ACanvas.Brush.Color := FSelectionColor;
        ACanvas.FillRect(R.Left - Scale(3), R.Top - Scale(3), R.Right + Scale(3), R.Bottom + Scale(3));
        Color1 := BlendColors(Color1, FSelectionColor, 30);
        ACanvas.Pen.Color := BlendColors(ACanvas.Pen.Color, FSelectionColor, 30);
      end;

      ACanvas.Brush.Color := Color1;
      ACanvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, FRoundCorners, FRoundCorners);
    end;

    // Calculate shift
    FCloseBtnWidth := ACanvas.TextWidth('×') + Scale(5);
    if (FReadOnly) or (not AShowCloseButtons) or (FCloseButtonOnHover and not Hover) then
      CloseBtnWidth := 0
    else
      CloseBtnWidth := ACanvas.TextWidth('×') + Scale(2);

    // Draw text
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Underline := FTagHoverUnderline and Hover;
    if HasColon then
    begin
      ACanvas.Font.Color := FontColor1;
      ACanvas.TextOut(R.Left + SepW div 2 - ACanvas.TextWidth(Part1) div 2, R.Top + R.Height div 2 -
        ACanvas.TextHeight(Part1) div 2 - Scale(1), Part1);

      ACanvas.Font.Color := FontColor2;
      ACanvas.TextOut(R.Left + SepW - Scale(AIndent) + (R.Width - SepW) div 2 - ACanvas.TextWidth(Part2) div
        2 - CloseBtnWidth div 2, R.Top + R.Height div 2 - ACanvas.TextHeight(Part2) div 2 - Scale(1), Part2);
    end
    else
      ACanvas.TextOut(R.Left + R.Width div 2 - ACanvas.TextWidth(s) div 2 - CloseBtnWidth div 2, R.Top +
        R.Height div 2 - ACanvas.TextHeight(s) div 2 - Scale(1), s);

    // Draw '×' button if enabled
    if AShowCloseButtons and (not FReadOnly) and (FCloseButtons) and (not FCloseButtonOnHover or Hover) then
    begin
      ACanvas.Font.Underline := False;
      ACanvas.TextOut(R.Right - FCloseBtnWidth, R.Top + R.Height div 2 - ACanvas.TextHeight('×') div 2 - Scale(2), '×');
    end;

    Inc(X, W + Scale(AIndent));
  end;
end;

function TCustomTagEdit.GetTagsBitmap(ATags: TStringList; AFontSize, AWidth, AHeight: integer;
  ATagHeightDelta: integer = 0; ABlend: integer = 0; ABlendColor: TColor = clWhite): TBitmap;
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
        ABlendColor,
        False
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

function TCustomTagEdit.BlendColors(Color1, Color2: TColor; Intensity: integer): TColor;
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
