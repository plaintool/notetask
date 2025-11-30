//-----------------------------------------------------------------------------------
//  TagCheckPopup © 2025 by Alexander Tverskoy
//  https://github.com/plaintool/TagEdit
//  Licensed under the MIT License
//  You may obtain a copy of the License at https://opensource.org/licenses/MIT
//-----------------------------------------------------------------------------------

unit TagCheckPopup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Clipbrd, Math, Graphics, Dialogs, StdCtrls,
  Buttons, CheckLst, ExtCtrls, Types, LCLType;

const
  MinHeight = 10;
  {$IFDEF UNIX}
  DefaultItemHeight = 23;
  {$ELSE}
  DefaultItemHeight = 17;
  {$ENDIF}

type
  TItemCheckedEvent = procedure(Sender: TObject; Index: integer; Checked: boolean) of object;

  { TCheckListForm }

  TCheckListForm = class(TForm)
  private
    FCheckListBox: TCheckListBox;
    FSpeedButton: TControl;
    FAppDeactivateHandler: TNotifyEvent;
    procedure CheckListBoxClickCheck(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure CheckListBoxKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure CheckListBoxKeyPress(Sender: TObject; var Key: char);
    procedure AppDeactivate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateStylesFromButton;
    property CheckListBox: TCheckListBox read FCheckListBox;
    property SpeedButton: TControl read FSpeedButton write FSpeedButton;
  protected
    procedure DoShow; override;
    procedure DoHide; override;
  end;

  { TCheckListButton }

  TCheckListButton = class(TSpeedButton)
  private
    FPopupForm: TCheckListForm;
    FItems: TStrings;
    FAttachedControl: TControl;
    FOnItemChecked: TItemCheckedEvent;
    FOnBeforeBulkChange: TNotifyEvent;
    FOnAfterBulkChange: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    FClosing: boolean;
    FPopupEmpty: boolean;
    FDropDownCount: integer;
    FItemHeight: integer;
    FAllowGrayed: boolean;
    FMultiSelect: boolean;
    FPopupOpenTopDelta: integer;
    FPopupOpenBottomDelta: integer;
    FPopupWidthDelta: integer;
    FSorted: boolean;
    FParentColor: boolean;
    FParentFont: boolean;
    procedure SetAttachedControl(Value: TControl);
    procedure SetDropDownCount(Value: integer);
    procedure SetItemHeight(Value: integer);
    procedure SetAllowGrayed(Value: boolean);
    procedure SetMultiSelect(Value: boolean);
    procedure SetSorted(Value: boolean);
    procedure SetParentColor(Value: boolean);
    procedure SetParentFont(Value: boolean);
    function GetItems: TStrings;
    procedure SetItems(Value: TStrings);
    function GetChecked(Index: integer): boolean;
    procedure SetChecked(Index: integer; Value: boolean);
    function GetCheckedByName(const AName: string): boolean;
    procedure SetCheckedByName(const AName: string; Value: boolean);
    function GetItemEnabled(Index: integer): boolean;
    procedure SetItemEnabled(Index: integer; Value: boolean);
    function GetCount: integer;
    function GetState(Index: integer): TCheckBoxState;
    procedure SetState(Index: integer; Value: TCheckBoxState);
    procedure UpdatePopupStyles;
    function IndexOf(const AName: string; AItems: TStrings; ACaseSensitive: boolean = True): integer;
    procedure InitializePopupForm; // Lazy initialization
  protected
    procedure Loaded; override;
    procedure ParentFontChanged; override;
    procedure FontChanged(Sender: TObject); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowPopupForm;
    procedure HidePopupForm;
    procedure ToggleSelectedChecks;
    procedure CheckAll;
    procedure UncheckAll;
    procedure Clear;
    function Scale(const AValue: integer): integer;
    property Checked[Index: integer]: boolean read GetChecked write SetChecked;
    property CheckedByName[AName: string]: boolean read GetCheckedByName write SetCheckedByName;
    property ItemEnabled[Index: integer]: boolean read GetItemEnabled write SetItemEnabled;
    property State[Index: integer]: TCheckBoxState read GetState write SetState;
    property Count: integer read GetCount;
  published
    property AttachedControl: TControl read FAttachedControl write SetAttachedControl;
    property Items: TStrings read GetItems write SetItems;
    property DropDownCount: integer read FDropDownCount write SetDropDownCount default 8;
    property ItemHeight: integer read FItemHeight write SetItemHeight default 0;
    property AllowGrayed: boolean read FAllowGrayed write SetAllowGrayed default False;
    property MultiSelect: boolean read FMultiSelect write SetMultiSelect default False;
    property PopupOpenTopDelta: integer read FPopupOpenTopDelta write FPopupOpenTopDelta default 0;
    property PopupOpenBottomDelta: integer read FPopupOpenBottomDelta write FPopupOpenBottomDelta default 0;
    property PopupWidthDelta: integer read FPopupWidthDelta write FPopupWidthDelta default 2;
    property Sorted: boolean read FSorted write SetSorted default False;
    property ParentColor: boolean read FParentColor write SetParentColor default True;
    property ParentFont: boolean read FParentFont write SetParentFont default True;
    property OnItemChecked: TItemCheckedEvent read FOnItemChecked write FOnItemChecked;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnBeforeBulkChange: TNotifyEvent read FOnBeforeBulkChange write FOnBeforeBulkChange;
    property OnAfterBulkChange: TNotifyEvent read FOnAfterBulkChange write FOnAfterBulkChange;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property PopupEmpty: boolean read FPopupEmpty write FPopupEmpty default True;
    property ParentBackground default False;
    property Color;
    property Font;
  end;

implementation

{ TCheckListForm }

constructor TCheckListForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);

  // Set form properties
  BorderStyle := bsNone;
  FormStyle := fsStayOnTop;

  // Create and configure CheckListBox
  FCheckListBox := TCheckListBox.Create(Self);
  with FCheckListBox do
  begin
    Parent := Self;
    Align := alClient;
    Items.UseLocale := True;
    TStringList(Items).CaseSensitive := True;
    OnClickCheck := @CheckListBoxClickCheck;
    OnKeyDown := @CheckListBoxKeyDown;
    OnKeyPress := @CheckListBoxKeyPress;
  end;

  // Set event handlers
  OnDeactivate := @FormDeactivate;
  OnMouseDown := @HandleMouseDown;

  FAppDeactivateHandler := nil;
end;

destructor TCheckListForm.Destroy;
begin
  // Remove application deactivate handler if still assigned
  if Assigned(FAppDeactivateHandler) then
    Application.RemoveOnDeactivateHandler(FAppDeactivateHandler);

  inherited Destroy;
end;

procedure TCheckListForm.DoShow;
begin
  inherited DoShow;
  // Add application deactivate handler when form is shown
  FAppDeactivateHandler := @AppDeactivate;
  Application.AddOnDeactivateHandler(FAppDeactivateHandler);
end;

procedure TCheckListForm.DoHide;
begin
  // Remove application deactivate handler when form is hidden
  if Assigned(FAppDeactivateHandler) then
  begin
    Application.RemoveOnDeactivateHandler(FAppDeactivateHandler);
    FAppDeactivateHandler := nil;
  end;
  inherited DoHide;
end;

procedure TCheckListForm.AppDeactivate(Sender: TObject);
begin
  // Hide immediately when application loses focus (Alt+Tab, etc.)
  if Assigned(FSpeedButton) and (FSpeedButton is TCheckListButton) then
    TCheckListButton(FSpeedButton).HidePopupForm
  else
    Hide;
end;

procedure TCheckListForm.CheckListBoxClickCheck(Sender: TObject);
begin
  // Propagate the click event to the speed button
  if Assigned(FSpeedButton) and (FSpeedButton is TCheckListButton) then
  begin
    with TCheckListButton(FSpeedButton) do
    begin
      if Assigned(FOnItemChecked) then
        FOnItemChecked(Self, FCheckListBox.ItemIndex, FCheckListBox.Checked[FCheckListBox.ItemIndex]);

      if Assigned(FOnChange) then
        FOnChange(Self);
    end;
  end;
end;

procedure TCheckListForm.CheckListBoxKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  i, SelCount: integer;
  HasMultiSelection: boolean;
  SelectedItems: TStringList;
  CopyText: string;
begin
  // Handle Ctrl+A to select all items
  if (Shift = [ssCtrl]) and (Key = VK_A) then  // 65 = 'A'
  begin
    if FCheckListBox.MultiSelect then
    begin
      for i := FCheckListBox.Items.Count - 1 downto 0 do
        FCheckListBox.Selected[i] := True;
      Key := 0; // Prevent default handling
    end;
  end
  // Handle Ctrl+C to copy selected items
  else if (Shift = [ssCtrl]) and (Key = VK_C) then // 67 = 'C'
  begin
    SelectedItems := TStringList.Create;
    try
      // Collect all selected items
      for i := 0 to FCheckListBox.Items.Count - 1 do
      begin
        if FCheckListBox.Selected[i] then
          SelectedItems.Add(FCheckListBox.Items[i]);
      end;

      // If no items selected but there is focused item, use it
      if (SelectedItems.Count = 0) and (FCheckListBox.ItemIndex >= 0) then
        SelectedItems.Add(FCheckListBox.Items[FCheckListBox.ItemIndex]);

      // If we have items to copy
      if SelectedItems.Count > 0 then
      begin
        // Join items with semicolon separator
        CopyText := SelectedItems.DelimitedText;
        SelectedItems.Delimiter := ';';
        CopyText := SelectedItems.DelimitedText;

        // Copy to clipboard
        Clipboard.AsText := CopyText;
        Key := 0; // Prevent default handling
      end;
    finally
      SelectedItems.Free;
    end;
  end
  // Handle Escape key to clear selection
  else if (Key = VK_ESCAPE) then // Escape
  begin
    if FCheckListBox.MultiSelect then
    begin
      // Check if there are any selected items
      HasMultiSelection := False;
      SelCount := 0;
      for i := 0 to FCheckListBox.Items.Count - 1 do
      begin
        if FCheckListBox.Selected[i] then
        begin
          Inc(SelCount);
          if SelCount > 1 then
          begin
            HasMultiSelection := True;
            Break;
          end;
        end;
      end;

      if HasMultiSelection then
      begin
        // Clear selection only if there are selected items
        for i := 0 to FCheckListBox.Items.Count - 1 do
          FCheckListBox.Selected[i] := False;
        Key := 0; // Prevent default handling
        Exit;
      end;
    end;
    // In single select mode, always hide the popup on Escape
    if Assigned(FSpeedButton) and (FSpeedButton is TCheckListButton) then
      TCheckListButton(FSpeedButton).HidePopupForm;
    Key := 0;
  end
  // Handle Space key to toggle checks for selected items
  else if (Key = VK_SPACE) then // Space
  begin
    if Assigned(FSpeedButton) and (FSpeedButton is TCheckListButton) then
    begin
      TCheckListButton(FSpeedButton).ToggleSelectedChecks;
      Key := 0; // Prevent default handling
    end;
  end;
end;

procedure TCheckListForm.CheckListBoxKeyPress(Sender: TObject; var Key: char);
begin
  // Prevent default Space handling which only toggles focused item
  if Key = #32 then
    Key := #0;
end;

procedure TCheckListForm.FormDeactivate(Sender: TObject);
var
  MousePos: TPoint;
  ButtonRect: TRect;
  ActiveForm: TCustomForm;
begin
  // Get the currently active form
  ActiveForm := Screen.ActiveCustomForm;

  // If our popup form is becoming active, don't hide - this happens when clicking the button
  if (ActiveForm = Self) or (ActiveForm is TCheckListForm) then
    Exit;

  // Check if the click was specifically on the button
  if Assigned(FSpeedButton) then
  begin
    MousePos := Mouse.CursorPos;
    ButtonRect := FSpeedButton.BoundsRect;
    ButtonRect.TopLeft := FSpeedButton.Parent.ClientToScreen(ButtonRect.TopLeft);
    ButtonRect.BottomRight := FSpeedButton.Parent.ClientToScreen(ButtonRect.BottomRight);

    // If click was on the button - don't hide, let the button handle it
    if ButtonRect.Contains(MousePos) then
      Exit;
  end;

  // In all other cases hide immediately
  if Assigned(FSpeedButton) and (FSpeedButton is TCheckListButton) then
    TCheckListButton(FSpeedButton).HidePopupForm
  else
    Hide;
end;

procedure TCheckListForm.HandleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  Pt: TPoint;
begin
  Pt := Point(X, Y);

  // Hide the form if clicked outside the CheckListBox
  if not FCheckListBox.BoundsRect.Contains(Pt) then
  begin
    if Assigned(FSpeedButton) and (FSpeedButton is TCheckListButton) then
      TCheckListButton(FSpeedButton).HidePopupForm
    else
      Hide;
  end;
end;

procedure TCheckListForm.UpdateStylesFromButton;
var
  SpeedBtn: TCheckListButton;
begin
  if Assigned(FSpeedButton) and (FSpeedButton is TCheckListButton) then
  begin
    SpeedBtn := TCheckListButton(FSpeedButton);

    // Update font - respect ParentFont setting
    if SpeedBtn.ParentFont and Assigned(SpeedBtn.Parent) then
      Font := SpeedBtn.Parent.Font
    else
      Font := SpeedBtn.Font;

    // Update color - respect ParentColor setting
    if SpeedBtn.ParentColor and Assigned(SpeedBtn.Parent) then
      Color := SpeedBtn.Parent.Color
    else
      Color := SpeedBtn.Color;

    if Assigned(FCheckListBox) then
    begin
      FCheckListBox.Font := Font;
      FCheckListBox.Color := Color;
    end;
  end;
end;

{ TCheckListButton }

constructor TCheckListButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Create local storage for items in designer
  FItems := TStringList.Create;

  // Don't create form in constructor to avoid showing it in designer
  FPopupForm := nil;
  FAttachedControl := nil;
  FClosing := False;
  FDropDownCount := 8;
  FItemHeight := 0;
  FAllowGrayed := False;
  FMultiSelect := False;
  FPopupWidthDelta := 2;
  FPopupOpenTopDelta := 0;
  FPopupOpenBottomDelta := 0;
  FSorted := False;
  FParentColor := True;
  FParentFont := True;
  FPopupEmpty := True;
  GroupIndex := 1;
  AllowAllUp := True;
end;

destructor TCheckListButton.Destroy;
begin
  FClosing := True;

  // Free local items storage
  FItems.Free;

  // Free form if it was created
  if Assigned(FPopupForm) then
  begin
    if FPopupForm.Visible then
      FPopupForm.Hide;
    FPopupForm.Free;
  end;

  inherited Destroy;
end;

procedure TCheckListButton.InitializePopupForm;
begin
  // Create form only when needed and only in runtime
  if not Assigned(FPopupForm) and not (csDesigning in ComponentState) then
  begin
    FPopupForm := TCheckListForm.Create(Self);
    FPopupForm.SpeedButton := Self;

    // Copy items from local storage to form
    if FItems.Count > 0 then
      FPopupForm.CheckListBox.Items.Assign(FItems);

    // Apply properties to form
    FPopupForm.CheckListBox.AllowGrayed := FAllowGrayed;
    FPopupForm.CheckListBox.MultiSelect := FMultiSelect;
    FPopupForm.CheckListBox.Sorted := FSorted;

    UpdatePopupStyles;
  end;
end;

procedure TCheckListButton.Loaded;
begin
  inherited Loaded;
  // Initialize form when component is loaded in runtime
  if not (csDesigning in ComponentState) then
    InitializePopupForm;
end;

procedure TCheckListButton.ParentFontChanged;
begin
  inherited ParentFontChanged;
  if FParentFont then
    UpdatePopupStyles;
end;

procedure TCheckListButton.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  UpdatePopupStyles;
end;

procedure TCheckListButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  // Only handle left mouse button
  if Button <> mbLeft then Exit;

  // Initialize form if needed
  InitializePopupForm;
  if not Assigned(FPopupForm) then Exit;

  // Simple toggle - the form knows when to hide itself
  if FPopupForm.Visible then
    HidePopupForm
  else
    ShowPopupForm;
end;

procedure TCheckListButton.UpdatePopupStyles;
begin
  if Assigned(FPopupForm) then
    FPopupForm.UpdateStylesFromButton;
end;

procedure TCheckListButton.ShowPopupForm;
var
  Control: TControl;
  P: TPoint;
  ScreenHeight: integer;
  FormHeight: integer;
  ActualItemHeight: integer;
  ItemCount: integer;
  MaxVisibleItems: integer;
  BorderDelta: integer;
begin
  // Initialize form if needed
  InitializePopupForm;
  if not Assigned(FPopupForm) then Exit;

  // Prevent multiple calls
  if FClosing or ((FPopupEmpty = False) and (Items.Count = 0)) then
    Exit;

  Control := FAttachedControl;
  if not Assigned(Control) then
    Control := Self;

  // Update styles before show
  UpdatePopupStyles;

  // Calculate position below attached control
  P := Control.ClientToScreen(Point(0, Control.Height));

  // Check available space below
  ScreenHeight := Screen.Height;

  // Determine actual item height
  if FItemHeight > 0 then
  begin
    ActualItemHeight := FItemHeight;
    FPopupForm.CheckListBox.ItemHeight := FItemHeight;
  end
  else
  begin
    ActualItemHeight := FPopupForm.CheckListBox.ItemHeight;
    if ActualItemHeight <= 0 then
    begin
      ActualItemHeight := Scale(DefaultItemHeight); // Default item height
    end;
  end;

  // Calculate form height based on item count and DropDownCount
  ItemCount := FPopupForm.CheckListBox.Items.Count;

  if ItemCount > 0 then
  begin
    // Determine how many items to show
    if (FDropDownCount > 0) and (ItemCount > FDropDownCount) then
      MaxVisibleItems := FDropDownCount
    else
      MaxVisibleItems := ItemCount;

    FormHeight := ActualItemHeight * MaxVisibleItems + 8; // 8 pixels for border

    // Ensure minimum height
    if FormHeight < Scale(MinHeight) then
      FormHeight := Scale(MinHeight);
  end
  else
    FormHeight := Scale(MinHeight); // Minimum height when no items

  // Show above control if not enough space below
  if P.Y + FormHeight > ScreenHeight then
  begin
    P := Control.ClientToScreen(Point(0, -FormHeight));
    BorderDelta := ifthen(FPopupOpenTopDelta <> 0, FPopupOpenTopDelta, -2);
  end
  else
  begin
    BorderDelta := ifthen(FPopupOpenBottomDelta <> 0, -FPopupOpenBottomDelta, 1);
  end;

  // Set form position and size
  FPopupForm.Top := P.Y + BorderDelta;
  if not assigned(FAttachedControl) then
  begin
    FPopupForm.Left := P.X + 1;
    FPopupForm.Width := Control.Parent.Width;
  end
  else
  begin
    FPopupForm.Left := P.X - 2;
    FPopupForm.Width := Control.Width + Self.Width + Scale(FPopupWidthDelta);
  end;
  FPopupForm.Height := FormHeight;

  Down := True;
  FClosing := False;
  FpopupForm.Font := self.Font;
  FPopupForm.Show;
  FPopupForm.BringToFront;

  // Trigger OnDropDown event
  if Assigned(FOnDropDown) then
    FOnDropDown(Self);
end;

procedure TCheckListButton.HidePopupForm;
begin
  if not Assigned(FPopupForm) then Exit;

  Down := False;
  FClosing := True;
  if FPopupForm.Visible then
    FPopupForm.Hide;
  FClosing := False;

  // Trigger OnCloseUp event
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self);
end;

procedure TCheckListButton.ToggleSelectedChecks;
var
  i: integer;
  HasSelection: boolean;
begin
  // Initialize form if needed
  InitializePopupForm;
  if not Assigned(FPopupForm) then Exit;

  HasSelection := False;

  // Check if any items are selected
  for i := 0 to FPopupForm.CheckListBox.Items.Count - 1 do
  begin
    if FPopupForm.CheckListBox.Selected[i] then
    begin
      HasSelection := True;
      Break;
    end;
  end;

  // Event before change
  if HasSelection or (FPopupForm.CheckListBox.ItemIndex >= 0) then
  begin
    if Assigned(FOnBeforeBulkChange) then
      FOnBeforeBulkChange(Self);
  end;

  // If no items selected, work with focused item only
  if not HasSelection and (FPopupForm.CheckListBox.ItemIndex >= 0) then
  begin
    FPopupForm.CheckListBox.Selected[FPopupForm.CheckListBox.ItemIndex] := True;
  end;

  // Toggle state for all selected items
  for i := 0 to FPopupForm.CheckListBox.Items.Count - 1 do
  begin
    if FPopupForm.CheckListBox.Selected[i] then
    begin
      if FAllowGrayed then
      begin
        // Cycle through three states: Unchecked -> Checked -> Grayed -> Unchecked
        case FPopupForm.CheckListBox.State[i] of
          cbUnchecked: FPopupForm.CheckListBox.State[i] := cbChecked;
          cbChecked: FPopupForm.CheckListBox.State[i] := cbGrayed;
          cbGrayed: FPopupForm.CheckListBox.State[i] := cbUnchecked;
        end;
      end
      else
      begin
        // Toggle between checked and unchecked
        FPopupForm.CheckListBox.Checked[i] := not FPopupForm.CheckListBox.Checked[i];
      end;

      // Trigger events
      if Assigned(FOnItemChecked) then
        FOnItemChecked(Self, i, FPopupForm.CheckListBox.Checked[i]);
    end;
  end;

  // Event after change
  if HasSelection or (FPopupForm.CheckListBox.ItemIndex >= 0) then
  begin
    if Assigned(FOnChange) then
      FOnChange(Self);
    if Assigned(FOnAfterBulkChange) then
      FOnAfterBulkChange(Self);
  end;
end;

procedure TCheckListButton.CheckAll;
var
  i: integer;
  OldOnItemChecked: TItemCheckedEvent;
  OldOnChange: TNotifyEvent;
begin
  // Initialize form if needed
  InitializePopupForm;
  if not Assigned(FPopupForm) or (Count = 0) then Exit;

  // Event before change
  if Assigned(FOnBeforeBulkChange) then
    FOnBeforeBulkChange(Self);

  // Save old event handlers
  OldOnItemChecked := FOnItemChecked;
  OldOnChange := FOnChange;

  try
    // Temporarily disable events to prevent multiple calls
    FOnItemChecked := nil;
    FOnChange := nil;

    // Set all items as checked
    for i := 0 to Count - 1 do
    begin
      if FAllowGrayed then
        FPopupForm.CheckListBox.State[i] := cbChecked
      else
        FPopupForm.CheckListBox.Checked[i] := True;
    end;

  finally
    // Restore event handlers
    FOnItemChecked := OldOnItemChecked;
    FOnChange := OldOnChange;
  end;

  // Event after change
  if Assigned(FOnChange) then
    FOnChange(Self);
  if Assigned(FOnAfterBulkChange) then
    FOnAfterBulkChange(Self);
end;

procedure TCheckListButton.UncheckAll;
var
  i: integer;
  OldOnItemChecked: TItemCheckedEvent;
  OldOnChange: TNotifyEvent;
begin
  // Initialize form if needed
  InitializePopupForm;
  if not Assigned(FPopupForm) or (Count = 0) then Exit;

  // Event before change
  if Assigned(FOnBeforeBulkChange) then
    FOnBeforeBulkChange(Self);

  // Save old event handlers
  OldOnItemChecked := FOnItemChecked;
  OldOnChange := FOnChange;

  try
    // Temporarily disable events to prevent multiple calls
    FOnItemChecked := nil;
    FOnChange := nil;

    // Set all items as unchecked
    for i := 0 to Count - 1 do
    begin
      if FAllowGrayed then
        FPopupForm.CheckListBox.State[i] := cbUnchecked
      else
        FPopupForm.CheckListBox.Checked[i] := False;
    end;

  finally
    // Restore event handlers
    FOnItemChecked := OldOnItemChecked;
    FOnChange := OldOnChange;
  end;

  // Event after change
  if Assigned(FOnChange) then
    FOnChange(Self);
  if Assigned(FOnAfterBulkChange) then
    FOnAfterBulkChange(Self);
end;

procedure TCheckListButton.Clear;
var
  OldOnItemChecked: TItemCheckedEvent;
  OldOnChange: TNotifyEvent;
begin
  // Initialize form if needed
  InitializePopupForm;
  if not Assigned(FPopupForm) or (Count = 0) then Exit;

  // Save old event handlers
  OldOnItemChecked := FOnItemChecked;
  OldOnChange := FOnChange;

  try
    // Temporarily disable events to prevent multiple calls
    FOnItemChecked := nil;
    FOnChange := nil;

    // Clear all items from the checklist
    FPopupForm.CheckListBox.Items.Clear;
    FItems.Clear; // Also clear local storage

  finally
    // Restore event handlers
    FOnItemChecked := OldOnItemChecked;
    FOnChange := OldOnChange;
  end;

  // Event after change
  if Assigned(FOnChange) then
    FOnChange(Self);
  if Assigned(FOnAfterBulkChange) then
    FOnAfterBulkChange(Self);
end;

function TCheckListButton.GetCheckedByName(const AName: string): boolean;
var
  Index: integer;
begin
  // In designer, always return False since we don't have form states
  if csDesigning in ComponentState then
    Result := False
  else
  begin
    InitializePopupForm;
    if not Assigned(FPopupForm) then Exit(False);

    Result := False;
    Index := IndexOf(AName, TStringList(FPopupForm.CheckListBox.Items));
    if Index >= 0 then
      Result := FPopupForm.CheckListBox.Checked[Index];
  end;
end;

procedure TCheckListButton.SetCheckedByName(const AName: string; Value: boolean);
var
  Index: integer;
begin
  // In designer, do nothing since we don't have form states
  if csDesigning in ComponentState then
    Exit;

  InitializePopupForm;
  if not Assigned(FPopupForm) then Exit;

  Index := IndexOf(AName, TStringList(FPopupForm.CheckListBox.Items));
  if Index >= 0 then
  begin
    FPopupForm.CheckListBox.Checked[Index] := Value;

    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

function TCheckListButton.Scale(const AValue: integer): integer;
begin
  Result := Scale96ToScreen(AValue);
end;

procedure TCheckListButton.SetAttachedControl(Value: TControl);
begin
  if FAttachedControl <> Value then
  begin
    FAttachedControl := Value;
    if not Assigned(FAttachedControl) then
      FAttachedControl := Self;
  end;
end;

procedure TCheckListButton.SetDropDownCount(Value: integer);
begin
  if Value < 0 then Value := 0;
  if FDropDownCount <> Value then
    FDropDownCount := Value;
end;

procedure TCheckListButton.SetItemHeight(Value: integer);
begin
  if Value < 0 then Value := 0;
  if FItemHeight <> Value then
    FItemHeight := Value;
end;

procedure TCheckListButton.SetAllowGrayed(Value: boolean);
begin
  if FAllowGrayed <> Value then
  begin
    FAllowGrayed := Value;
    if Assigned(FPopupForm) then
      FPopupForm.CheckListBox.AllowGrayed := Value;
  end;
end;

procedure TCheckListButton.SetMultiSelect(Value: boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    if Assigned(FPopupForm) then
      FPopupForm.CheckListBox.MultiSelect := Value;
  end;
end;

procedure TCheckListButton.SetSorted(Value: boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    if Assigned(FPopupForm) then
      FPopupForm.CheckListBox.Sorted := Value;
  end;
end;

procedure TCheckListButton.SetParentColor(Value: boolean);
begin
  if FParentColor <> Value then
  begin
    FParentColor := Value;
    if FParentColor then
      UpdatePopupStyles;
  end;
end;

procedure TCheckListButton.SetParentFont(Value: boolean);
begin
  if FParentFont <> Value then
  begin
    FParentFont := Value;
    if FParentFont then
      UpdatePopupStyles;
  end;
end;

function TCheckListButton.GetItems: TStrings;
begin
  // In designer, use local storage
  if csDesigning in ComponentState then
    Result := FItems
  else
  begin
    // In runtime, use form's items
    InitializePopupForm;
    if Assigned(FPopupForm) then
      Result := FPopupForm.CheckListBox.Items
    else
      Result := FItems; // Fallback
  end;
end;

procedure TCheckListButton.SetItems(Value: TStrings);
begin
  // Always update local storage
  FItems.Assign(Value);

  // Also update form if it exists
  if Assigned(FPopupForm) then
    FPopupForm.CheckListBox.Items.Assign(Value);
end;

function TCheckListButton.GetChecked(Index: integer): boolean;
begin
  // In designer, always return False since we don't have form states
  if csDesigning in ComponentState then
    Result := False
  else
  begin
    InitializePopupForm;
    if not Assigned(FPopupForm) then Exit(False);

    if (Index >= 0) and (Index < FPopupForm.CheckListBox.Items.Count) then
      Result := FPopupForm.CheckListBox.Checked[Index]
    else
      Result := False;
  end;
end;

procedure TCheckListButton.SetChecked(Index: integer; Value: boolean);
begin
  // In designer, do nothing since we don't have form states
  if csDesigning in ComponentState then
    Exit;

  InitializePopupForm;
  if not Assigned(FPopupForm) then Exit;

  if (Index >= 0) and (Index < FPopupForm.CheckListBox.Items.Count) then
  begin
    FPopupForm.CheckListBox.Checked[Index] := Value;

    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

function TCheckListButton.GetState(Index: integer): TCheckBoxState;
begin
  // In designer, always return unchecked since we don't have form states
  if csDesigning in ComponentState then
    Result := cbUnchecked
  else
  begin
    InitializePopupForm;
    if not Assigned(FPopupForm) then Exit(cbUnchecked);

    if (Index >= 0) and (Index < FPopupForm.CheckListBox.Items.Count) then
      Result := FPopupForm.CheckListBox.State[Index]
    else
      Result := cbUnchecked;
  end;
end;

procedure TCheckListButton.SetState(Index: integer; Value: TCheckBoxState);
begin
  // In designer, do nothing since we don't have form states
  if csDesigning in ComponentState then
    Exit;

  InitializePopupForm;
  if not Assigned(FPopupForm) then Exit;

  if (Index >= 0) and (Index < FPopupForm.CheckListBox.Items.Count) then
  begin
    FPopupForm.CheckListBox.State[Index] := Value;

    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

function TCheckListButton.GetItemEnabled(Index: integer): boolean;
begin
  // In designer, always return True since we don't have form states
  if csDesigning in ComponentState then
    Result := True
  else
  begin
    InitializePopupForm;
    if not Assigned(FPopupForm) then Exit(True);

    if (Index >= 0) and (Index < FPopupForm.CheckListBox.Items.Count) then
      Result := FPopupForm.CheckListBox.ItemEnabled[Index]
    else
      Result := False;
  end;
end;

procedure TCheckListButton.SetItemEnabled(Index: integer; Value: boolean);
begin
  // In designer, do nothing since we don't have form states
  if csDesigning in ComponentState then
    Exit;

  InitializePopupForm;
  if not Assigned(FPopupForm) then Exit;

  if (Index >= 0) and (Index < FPopupForm.CheckListBox.Items.Count) then
    FPopupForm.CheckListBox.ItemEnabled[Index] := Value;
end;

function TCheckListButton.GetCount: integer;
begin
  // Always use local storage count in designer
  if csDesigning in ComponentState then
    Result := FItems.Count
  else
  begin
    InitializePopupForm;
    if Assigned(FPopupForm) then
      Result := FPopupForm.CheckListBox.Items.Count
    else
      Result := FItems.Count; // Fallback
  end;
end;

function TCheckListButton.IndexOf(const AName: string; AItems: TStrings; ACaseSensitive: boolean = True): integer;
var
  i: integer;
  s1, s2: string;
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.Assign(AItems);
    List.CaseSensitive := False;
    List.Sorted := FSorted;
    if FSorted then List.Sort;
    List.Sorted := False;
    List.CaseSensitive := ACaseSensitive;
    for i := 0 to List.Count - 1 do
    begin
      if ACaseSensitive then
      begin
        if List[i] = AName then
          Exit(i);
      end
      else
      begin
        s1 := List[i];
        s2 := AName;
        if CompareText(s1, s2) = 0 then
          Exit(i);
      end;
    end;
    Result := -1;
  finally
    List.Free;
  end;
end;

end.
