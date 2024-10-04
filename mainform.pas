unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Types, CheckLst, ValEdit, Grids, Menus, ActnList, ComCtrls, PrintersDlgs,
  LCLIntf, LCLType, ExtDlgs, LMessages, Clipbrd, Process, task, lineending;

type

  { TformNotetask }
  TformNotetask = class(TForm)
    aWordWrap: TAction;
    aFont: TAction;
    aPrinterSetup: TAction;
    aExit: TAction;
    aPrint: TAction;
    aPageProperties: TAction;
    aSaveAs: TAction;
    aSave: TAction;
    aNew: TAction;
    aNewWindow: TAction;
    aOpen: TAction;
    ActionList: TActionList;
    calendarDialog: TCalendarDialog;
    fontDialog: TFontDialog;
    MainMenu: TMainMenu;
    menuFile: TMenuItem;
    menuFormat: TMenuItem;
    menuFont: TMenuItem;
    MenuItem1: TMenuItem;
    menuWordWrap: TMenuItem;
    menuTask: TMenuItem;
    menuNewWindow: TMenuItem;
    menuOpen: TMenuItem;
    menuSave: TMenuItem;
    menuSaveAs: TMenuItem;
    menuPrinterSetup: TMenuItem;
    menuPageProperties: TMenuItem;
    menuPrint: TMenuItem;
    menuNew: TMenuItem;
    openDialog: TOpenDialog;
    pageSetupDialog: TPageSetupDialog;
    printDialog: TPrintDialog;
    printerSetupDialog: TPrinterSetupDialog;
    saveDialog: TSaveDialog;
    Separator1: TMenuItem;
    menuExit: TMenuItem;
    Separator3: TMenuItem;
    statusBar: TStatusBar;
    taskGrid: TStringGrid;
    procedure aExitExecute(Sender: TObject);
    procedure aFontExecute(Sender: TObject);
    procedure aNewExecute(Sender: TObject);
    procedure aNewWindowExecute(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure aPagePropertiesExecute(Sender: TObject);
    procedure aPrinterSetupExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aSaveAsExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aWordWrapExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure taskGridCheckboxToggled(Sender: TObject; aCol, aRow: integer; aState: TCheckboxState);
    procedure taskGridColRowDeleted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
    procedure taskGridColRowInserted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
    procedure taskGridDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure taskGridEditButtonClick(Sender: TObject);
    procedure taskGridHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure taskGridHeaderSized(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure taskGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure taskGridMouseLeave(Sender: TObject);
    procedure taskGridMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure taskGridPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    procedure taskGridResize(Sender: TObject);
    procedure taskGridSelectCell(Sender: TObject; aCol, aRow: integer; var CanSelect: boolean);
    procedure taskGridSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
    procedure taskGridValidateEntry(Sender: TObject; aCol, aRow: integer; const OldValue: string; var NewValue: string);
  private
    Memo: TMemo;
    FChanged: boolean;
    IsEditing: boolean;
    IsSelecting: boolean;
    FFileName: string;
    FEncoding: TEncoding;
    FLineEnding: TLineEnding;
    FLineCount: integer;
    FWordWrap: boolean;
    procedure MemoChange(Sender: TObject);
    procedure MemoSetBounds(aCol: integer; aRow: integer);
    procedure SetChanged(aChanged: boolean = True);
    procedure EditCell(aCol, aRow: integer);
    procedure EditComplite;
    function IsCanClose: boolean;
  public
    procedure OpenFile(fileName: string);
    procedure SaveFile(fileName: string = string.Empty);
    procedure InfoFile();

    property WordWrap: boolean read FWordWrap write FWordWrap;
  end;

var
  formNotetask: TformNotetask;
  Tasks: TTasks; // Tasks collection
  clRowHighlight: TColor;

resourcestring
  rapp = 'Notetask';
  runtitled = 'Untitled';
  rrows = ' rows';
  rdeleteconfirm = 'Are you sure you want to delete this task?';
  rsavechanges = 'Do you want to save the changes?';
  rclearconfirm = 'Are you sure you want to clear this section?';

implementation

uses filemanager, settings;

  {$R *.lfm}

  { TformNotetask }

procedure TformNotetask.FormCreate(Sender: TObject);
var
  FilePath: string;
begin
  FWordWrap := True;
  aWordWrap.Checked := FWordWrap;
  clRowHighlight := RGBToColor(224, 224, 224);

  Caption := runtitled + ' - ' + rapp;

  LoadFormSettings(self);
  LoadGridSettings(taskGrid);

  // Check if a command line argument is passed
  if ParamCount > 0 then
  begin
    FilePath := ParamStr(1); // Get the file path
    if (not FilePath.StartsWith('--')) then
    begin
      OpenFile(FilePath); // Your function to load a task from the file
      exit;
    end;
  end;

  aNew.Execute;
end;

procedure TformNotetask.FormDestroy(Sender: TObject);
begin
  SaveFormSettings(self);
end;

procedure TformNotetask.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  Row: integer;
  Confirm: integer;
  i, j: integer;
  Rect: TRect;
begin
  if (Key = VK_DELETE) then
  begin
    if (not taskGrid.EditorMode) and (not IsEditing) then
    begin
      if (taskGrid.Selection.Width > 0) or (taskGrid.Selection.Height > 0) then
      begin
        // Show confirm delete dialog
        Confirm := MessageDlg(rclearconfirm, mtConfirmation, [mbYes, mbNo], 0);

        if Confirm = mrYes then
        begin
          Rect := taskGrid.Selection;
          for i := Rect.Top to Rect.Bottom do
          begin
            for j := Rect.Left to Rect.Right do
            begin
              if (j = 1) then Tasks.GetTask(i - 1).IsCompleted := False;
              if (j = 2) then Tasks.GetTask(i - 1).TaskDescription := '';
              if (j = 3) then Tasks.GetTask(i - 1).Comment := '';
              if (j = 4) then Tasks.GetTask(i - 1).CompletionDate := 0;
              if (j = 1) then
                taskGrid.Cells[j, i] := '0'
              else
                taskGrid.Cells[j, i] := string.Empty;
            end;
          end;
          SetChanged;
        end;
        Abort;
      end
      else
      begin
        // Get current row selected
        Row := taskGrid.Row;
        if (Row > 0) and (Row <= Tasks.Count) then
        begin
          // Show confirm delete dialog
          Confirm := MessageDlg(rdeleteconfirm, mtConfirmation, [mbYes, mbNo], 0);

          if Confirm = mrYes then
          begin
            // RemoveTask from collection
            Tasks.RemoveTask(Row - 1);
            taskGrid.DeleteRow(Row);
            SetChanged;
          end;
          Abort;
        end;
      end;
    end
    else
      Abort;
  end
  else
  if (Key = VK_ESCAPE) then
  begin
    if (taskGrid.EditorMode) or (IsEditing) then
      EditComplite;
    Abort;
  end
  else
  if (Key = VK_F2) then
  begin
    EditComplite;
    EditCell(taskGrid.Col, taskGrid.Row);
    Abort;
  end
  else
  if (ssCtrl in Shift) and (Key = VK_C) then
  begin
    Tasks.CopyToClipboard(taskGrid);
    Abort;
  end;
end;

procedure TformNotetask.aExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TformNotetask.aFontExecute(Sender: TObject);
begin
  fontDialog.Font := Font;
  if fontDialog.Execute then  // Open the font dialog
  begin
    // Apply the selected font to the form
    Self.Font := fontDialog.Font;
  end;
end;

procedure TformNotetask.aNewExecute(Sender: TObject);
begin
  Tasks := TTasks.Create();
  if IsCanClose then
  begin
    SetChanged(False);
    FFileName := string.Empty;
    Caption := runtitled + ' - ' + rapp;
    taskGrid.Clean;
    taskGrid.RowCount := 2;
    Tasks.AddTask('[ ]');
    taskGrid.Cells[1, 1] := '0';
  end;
end;

procedure TformNotetask.aNewWindowExecute(Sender: TObject);
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil); // Create a new process
  try
    Process.Executable := ParamStr(0); // Set the executable to the current application
    Process.Options := []; // No wait, open and forget
    Process.Execute; // Execute the new instance
  finally
    Process.Free; // Free the process object
  end;
end;

procedure TformNotetask.aOpenExecute(Sender: TObject);
begin
  if openDialog.Execute then
  begin
    OpenFile(openDialog.FileName);
  end;
end;

procedure TformNotetask.aPagePropertiesExecute(Sender: TObject);
begin
  printerSetupDialog.Execute;
end;

procedure TformNotetask.aPrinterSetupExecute(Sender: TObject);
begin

end;

procedure TformNotetask.aPrintExecute(Sender: TObject);
begin

end;

procedure TformNotetask.aSaveAsExecute(Sender: TObject);
begin
  if (saveDialog.Execute) then
  begin
    SaveFile(saveDialog.FileName);
  end;
end;

procedure TformNotetask.aSaveExecute(Sender: TObject);
begin
  SaveFile(FFileName);
end;

procedure TformNotetask.aWordWrapExecute(Sender: TObject);
var
  i: integer;
begin
  EditComplite;
  FWordWrap := aWordWrap.Checked;
  for i := 0 to taskGrid.RowCount - 1 do
  begin
    taskGrid.RowHeights[i] := taskGrid.DefaultRowHeight;
  end;
  Invalidate;
end;

function TformNotetask.IsCanClose: boolean;
var
  UserResponse: integer;
begin
  if FChanged then
  begin
    // Show message with Yes, No, and Cancel options
    UserResponse := MessageDlg(rsavechanges, mtConfirmation, [mbYes, mbNo, mbCancel], 0);

    case UserResponse of
      mrYes:
      begin
        // Call save method and allow form to close
        aSave.Execute;
        Result := True;
      end;
      mrNo:
      begin
        // Do not save, but allow form to close
        Result := True;
      end;
      else
        Result := False;
    end;
  end
  else
    Result := True; // No changes, just close the form
end;

procedure TformNotetask.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := IsCanClose;
end;

procedure TformNotetask.OpenFile(fileName: string);
var
  Content: string;
  i: integer;
begin
  FFileName := fileName;
  ReadTextFile(FFileName, Content, FEncoding, FLineEnding, FLineCount);
  InfoFile;

  Tasks := TTasks.Create(TextToStringList(Content));
  Tasks.FillGrid(taskGrid);

  for i := 0 to taskGrid.RowCount - 1 do
  begin
    taskGrid.RowHeights[i] := taskGrid.DefaultRowHeight;
  end;
end;

procedure TformNotetask.SaveFile(fileName: string = string.Empty);
begin
  if (fileName = string.Empty) then
    fileName := FFileName
  else
    FFileName := fileName;

  if (fileName <> string.Empty) then
  begin
    SaveTextFile(fileName, Tasks.ToStringList, FEncoding, FLineEnding);
    SetChanged(False);
  end;

  InfoFile;
end;

procedure TformNotetask.InfoFile();
begin
  Caption := ExtractFileName(FFileName) + ' - ' + rapp;
  statusBar.Panels[1].Text := UpperCase(FEncoding.EncodingName);
  statusBar.Panels[2].Text := FLineEnding.ToString;
  statusBar.Panels[3].Text := FLineCount.ToString + rrows;
end;

procedure TformNotetask.SetChanged(aChanged: boolean = True);
begin
  if (aChanged = False) then
    taskGrid.Modified := False;

  FChanged := taskGrid.Modified;
  aSave.Enabled := FChanged;
  if (FChanged) and (Caption <> '') and (Caption[1] <> '*') then
    Caption := '*' + Caption
  else if not FChanged and (Caption <> '') and (Caption[1] = '*') then
    Caption := Copy(Caption, 2, Length(Caption) - 1);
end;

procedure TformNotetask.taskGridPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
var
  MyTextStyle: TTextStyle;
begin
  if (aCol in [2, 3]) then
  begin
    MyTextStyle := taskGrid.Canvas.TextStyle;
    MyTextStyle.SingleLine := False;
    MyTextStyle.Wordbreak := True;
    taskGrid.Canvas.TextStyle := MyTextStyle;
  end;
end;

procedure TformNotetask.taskGridEditButtonClick(Sender: TObject);
var
  Col, Row: integer;
  CellRect: TRect;
  CalendarX, CalendarY: integer;
begin
  // Get the current column and row
  Col := taskGrid.Col;
  Row := taskGrid.Row;

  // Get the rectangle of the cell
  CellRect := taskGrid.CellRect(Col, Row);

  // Calculate the position for the calendar dialog
  CalendarX := CellRect.CenterPoint.X; // Position to the left of the cell
  CalendarY := CellRect.CenterPoint.Y; // Position below the cell

  // Set the position of the calendar dialog
  calendarDialog.Left := CalendarX + Left;
  calendarDialog.Top := CalendarY + Top;

  if calendarDialog.Execute then
  begin
    // Set the selected date in the corresponding cell
    taskGrid.Cells[Col, Row] := DateToStr(calendarDialog.Date);
  end;
end;

procedure TformNotetask.taskGridValidateEntry(Sender: TObject; aCol, aRow: integer; const OldValue: string; var NewValue: string);
var
  DateTime: TDateTime;
begin
  if (aCol = 4) then
  begin
    if (NewValue <> '') and (not TryStrToDateTime(NewValue, DateTime)) then
      abort;

    if (OldValue <> NewValue) then
    begin
      SetChanged;

      Tasks.SetTask(taskGrid, taskGrid.Row, taskGrid.Col);
      EditComplite;
    end;
  end;
end;

procedure TformNotetask.taskGridHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
begin
  EditComplite;
  if IsColumn then
  begin
    if Index = 0 then
      Tasks.FillGrid(taskGrid, taskGrid.SortOrder);
  end
  else
  begin
    taskGrid.Row := index;
  end;
end;

procedure TformNotetask.FormResize(Sender: TObject);
begin
  taskGridResize(Sender);
end;

procedure TformNotetask.taskGridCheckboxToggled(Sender: TObject; aCol, aRow: integer; aState: TCheckboxState);
begin
  SetChanged;
  if (aState = cbChecked) and (taskGrid.Cells[4, aRow] = '') then
    taskGrid.Cells[4, aRow] := FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat, Now)
  else
    taskGrid.Cells[4, aRow] := string.Empty;
  Tasks.SetTask(taskGrid, aRow, aCol);
end;

procedure TformNotetask.taskGridColRowInserted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
begin
  if (not IsColumn) then
  begin
    Tasks.AddTask('[ ]');
    taskGrid.Cells[1, tIndex] := '0';
  end;
end;

procedure TformNotetask.taskGridColRowDeleted(Sender: TObject; IsColumn: boolean; sIndex, tIndex: integer);
begin
  if (not IsColumn) then
    Tasks.RemoveTask(tIndex);
end;

procedure TformNotetask.taskGridHeaderSized(Sender: TObject; IsColumn: boolean; Index: integer);
begin
  taskGridResize(Sender);
  SaveGridSettings(taskGrid);
end;

procedure TformNotetask.taskGridSelectCell(Sender: TObject; aCol, aRow: integer; var CanSelect: boolean);
begin
  IsSelecting := True;
end;

procedure TformNotetask.taskGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  IsSelecting := False;
end;

procedure TformNotetask.taskGridMouseLeave(Sender: TObject);
begin
  IsSelecting := False;
end;

procedure TformNotetask.taskGridMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint; var Handled: boolean);
begin
  EditComplite;
end;

procedure TformNotetask.taskGridResize(Sender: TObject);
var
  Rect: TRect;
begin
  // Get the cell dimensions
  Rect := taskGrid.CellRect(taskGrid.Col, taskGrid.Row);

  // Update the size and position of the Memo
  if Assigned(taskGrid.Editor) and (taskGrid.Editor is TMemo) then
    TMemo(taskGrid.Editor).SetBounds(Rect.Left + 5, Rect.Top + 1, Rect.Right - Rect.Left - 10, Rect.Bottom - Rect.Top - 3);
end;

procedure TformNotetask.MemoChange(Sender: TObject);
begin
  taskGrid.Cells[taskGrid.Col, taskGrid.Row] := TMemo(Sender).Text;
  MemoSetBounds(taskGrid.Col, taskGrid.Row);
  Tasks.SetTask(taskGrid, taskGrid.Row, taskGrid.Col);
  SetChanged;
end;

procedure TformNotetask.MemoSetBounds(aCol: integer; aRow: integer);
var
  Rect: TRect;
begin
  Rect := taskGrid.CellRect(aCol, aRow);
  Memo.SetBounds(Rect.Left + 5, Rect.Top + 1, Rect.Right - Rect.Left - 10, Rect.Bottom - Rect.Top - 3);
end;

procedure TformNotetask.taskGridDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
var
  grid: TStringGrid;
  S: string;
  drawrect: TRect;
  bgFill: TColor;
  flags: cardinal;
begin
  if (aCol in [0, 1]) or (aRow = 0) then exit;
  grid := Sender as TStringGrid;

  // Determine background color
  if (gdSelected in aState) and ((taskGrid.Selection.Height > 0) or (taskGrid.Selection.Width > 0)) then
  begin
    bgFill := clHighlight;
    grid.Canvas.Font.Color := clWhite; // Set font color to white when selected
  end
  else
  if gdRowHighlight in aState then
  begin
    bgFill := clRowHighlight;
    grid.Canvas.Font.Color := clBlack;
  end
  else
  begin
    bgFill := clWhite;
    grid.Canvas.Font.Color := clBlack;
  end;

  // Fill the cell background
  grid.Canvas.Brush.Color := bgFill;
  grid.canvas.Brush.Style := bsSolid;
  grid.canvas.fillrect(aRect);

  S := grid.Cells[ACol, ARow];
  if Length(S) > 0 then
  begin
    drawrect := aRect;
    drawrect.Inflate(-4, 0);
    if (FWordWrap) then
      flags := dt_calcrect or dt_wordbreak or dt_left
    else
      flags := dt_calcrect or dt_left;
    DrawText(grid.canvas.handle, PChar(S), Length(S), drawrect, flags);

    //if gdFocused in aState then
    //  DrawFocusRect(grid.canvas.handle, drawrect);

    if (drawrect.bottom - drawrect.top) > grid.RowHeights[ARow] then
      grid.RowHeights[ARow] := (drawrect.bottom - drawrect.top + 2) // changing the row height fires the event again!
    else
    begin
      drawrect.Right := aRect.Right;
      // grid.canvas.fillrect(drawrect);
      if (FWordWrap) then
        flags := dt_wordbreak or dt_left
      else
        flags := dt_left;
      DrawText(grid.canvas.handle, PChar(S), Length(S), drawrect, dt_wordbreak or dt_left);
    end;
  end;
end;

procedure TformNotetask.taskGridSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
var
  h, w: integer;
begin
  if (aCol in [1, 4]) then exit;
  if (Assigned(Memo)) then Memo.Free;

  Memo := TMemo.Create(Self);
  Memo.Visible := False;
  w := taskGrid.Selection.Width;
  h := taskGrid.Selection.Height;
  if (taskGrid.IsCellSelected[aCol, aRow]) and ((taskGrid.Selection.Height > 0) or (taskGrid.Selection.Width > 0)) then
  begin
    Memo.Color := clHighlight;
    Memo.Font.Color := clWhite;
  end
  else
  begin
    Memo.Color := clRowHighlight;
    Memo.Font.Color := clBlack;
  end;
  Memo.Font.Name := taskGrid.Font.Name;
  Memo.Font.Size := taskGrid.Font.Size;
  Memo.HideSelection := True;
  Memo.TabStop := False;
  Memo.WantTabs := True;
  Memo.BorderStyle := bsNone;
  Memo.WordWrap := FWordWrap;
  Memo.WantReturns := FWordWrap;
  Memo.ScrollBars := ssAutoVertical;

  MemoSetBounds(aCol, aRow);
  Memo.Parent := taskGrid;
  Memo.OnChange := @MemoChange; // Event
  Memo.Text := taskGrid.Cells[aCol, aRow];
  Memo.CaretPos := Point(Length(Memo.Text), 0);

  Editor := Memo;

  if (IsSelecting) or (taskGrid.Selection.Height > 0) or (taskGrid.Selection.Width > 0) then
  begin
    IsSelecting := False;
    Memo.Visible := False;
    IsEditing := False;
  end
  else
  begin
    Memo.Visible := True;
    IsEditing := True;
  end;
end;

procedure TformNotetask.EditCell(aCol, aRow: integer);
begin
  taskGrid.Row := aRow;
  taskGrid.Col := aCol;
  IsEditing := True;
  taskGrid.EditorMode := True; //Set editing mode

  if (Assigned(Memo)) and (Memo.Visible) then
  begin
    Memo.SelectAll;
    Memo.SetFocus;
  end;
end;

procedure TformNotetask.EditComplite;
begin
  taskGrid.EditorMode := False;
  IsEditing := False; // Reset editing flag when exiting
end;

end.
