unit GridPrn;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Types,
  Graphics,
  StdCtrls,
  Grids,
  Printers;

type
  TGridPrnGetCellTextEvent = procedure(Sender: TObject; AGrid: TCustomGrid; ACol, ARow: integer; var AText: string) of object;

  TGridPrnOrder = (poRowsFirst, poColsFirst);

  TGridPrnHeaderFooterPart = (hfpLeft, hfpCenter, hfpRight);

  TGridPrnMargins = class(TPersistent)
  private
    FMargins: array[0..5] of double;
    function GetMargin(AIndex: integer): double;
    function IsStoredMargin(AIndex: integer): boolean;
    procedure SetMargin(AIndex: integer; AValue: double);
  public
    constructor Create;
  published
    property LeftMargin: double index 0 read GetMargin write SetMargin stored IsStoredMargin;
    property TopMargin: double index 1 read GetMargin write SetMargin stored IsStoredMargin;
    property RightMargin: double index 2 read GetMargin write SetMargin stored IsStoredMargin;
    property BottomMargin: double index 3 read GetMargin write SetMargin stored IsStoredMargin;
    property HeaderMargin: double index 4 read GetMargin write SetMargin stored IsStoredMargin;
    property FooterMargin: double index 5 read GetMargin write SetMargin stored IsStoredMargin;
  end;

  { TGridPrinter }

  TGridPrinter = class(TComponent)
  private
    FBorderLineColor: integer;
    FBorderLineWidth: integer;
    FFixedLineColor: TColor;
    FFixedLineWidth: integer;
    FGrid: TCustomGrid;
    FGridLineWidth: integer;
    FGridLineColor: TColor;
    FHeaderFont: TFont;
    FHeaderLine: boolean;
    FHeaderLineColor: TColor;
    FHeaderLineWidth: integer;
    FHeaderText: array[TGridPrnHeaderFooterPart] of string;
    FFooterFont: TFont;
    FFooterLine: boolean;
    FFooterLineColor: TColor;
    FFooterLineWidth: integer;
    FFooterText: array[TGridPrnHeaderFooterPart] of string;
    FMargins: TGridPrnMargins;
    FMonochrome: boolean;
    FOrientation: TPrinterOrientation;
    FPadding: integer;
    FPageHeight: integer;
    FPageWidth: integer;
    FPrintOrder: TGridPrnOrder;
    FOnGetCellText: TGridPrnGetCellTextEvent;
    FOnPrepareCanvas: TOnPrepareCanvasEvent;
    function GetCanvas: TCanvas;
    function GetFooter: string;
    function GetFooterPart(AIndex: TGridPrnHeaderFooterPart): string;
    function GetHeader: string;
    function GetHeaderPart(AIndex: TGridPrnHeaderFooterPart): string;
    function GetPageCount: integer;
    procedure SetFooter(AValue: string);
    procedure SetFooterPart(AIndex: TGridPrnHeaderFooterPart; AValue: string);
    procedure SetGrid(AValue: TCustomGrid);
    procedure SetHeader(AValue: string);
    procedure SetHeaderPart(AIndex: TGridPrnHeaderFooterPart; AValue: string);
  protected
  type
    TOutputDevice = (odPrinter, odPreview);
  protected
    FFactorX: double;              // Multiply to convert screen to printer pixels
    FFactorY: double;
    FLeftMarginPx: integer;         // Page margins, in printer pixels
    FTopMarginPx: integer;
    FRightMarginPx: integer;
    FBottomMarginPx: integer;
    FHeaderMarginPx: integer;
    FFooterMarginPx: integer;
    FColWidths: array of integer;   // Array of grid column widts, in printer pixels
    FRowHeights: array of integer;  // Array of grid row heights, in printer pixels
    FFixedColPos: integer;          // Right end of the fixed cols, in printer pixels
    FFixedRowPos: integer;          // Bottom end of the fixed rows, in printer pixels
    FOutputDevice: TOutputDevice;
    FPageBreakRows: array of integer;  // Indices of first row on new page
    FPageBreakCols: array of integer;  // Indices of first columns on new page
    FPageNumber: integer;
    FPageCount: integer;
    FPixelsPerInchX: integer;
    FPixelsPerInchY: integer;
    FPreviewBitmap: TBitmap;           // Bitmap to which the preview image is printed
    FPreviewPage: integer;             // Page request for the preview bitmap
    FPreviewPercent: integer;          // Scaling factor for preview bitmap
    FColCount: integer;
    FRowCount: integer;
    FFixedCols: integer;
    FFixedRows: integer;
    FPrinting: boolean;
    procedure DoPrepareCanvas(ACol, ARow: integer); virtual;
    procedure Execute(ACanvas: TCanvas);
    function GetHeaderFooterText(AText: string): string;
    procedure LayoutPagebreaks;
    procedure NewPage;
    procedure Prepare;
    procedure PrepareCanvas(ACanvas: TCanvas; ACol, ARow: integer); virtual;
    procedure PrintByCols(ACanvas: TCanvas);
    procedure PrintByRows(ACanvas: TCanvas);
    procedure PrintCell(ACanvas: TCanvas; ACol, ARow: integer; ARect: TRect); virtual;
    procedure PrintCheckbox(ACanvas: TCanvas; ACol, ARow: integer; ARect: TRect; ACheckState: TCheckboxstate); virtual;
    procedure PrintColHeaders(ACanvas: TCanvas; ACol1, ACol2: integer);
    procedure PrintFooter(ACanvas: TCanvas);
    procedure PrintHeader(ACanvas: TCanvas);
    procedure PrintGridLines(ACanvas: TCanvas; AFirstCol, AFirstRow, XEnd, YEnd: integer);
    procedure PrintPage(ACanvas: TCanvas; AStartCol, AStartRow, AEndCol, AEndRow: integer);
    procedure PrintRowHeader(ACanvas: TCanvas; ARow, Y: integer);
    procedure ScaleColWidths;
    procedure ScaleRowHeights;
    procedure SelectFont(ACanvas: TCanvas; AFont: TFont);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreatePreviewBitmap(APageNo, APercentage: integer): TBitmap;
    function GetCellText(ACol, ARow: integer): string; virtual;
    procedure Print;
    function ScaleX(AValue: integer): integer; inline;
    function ScaleY(AValue: integer): integer; inline;
    property Canvas: TCanvas read GetCanvas;
    property FooterPart[AIndex: TGridPrnHeaderFooterPart]: string read GetFooterPart write SetFooterPart;
    property HeaderPart[AIndex: TGridPrnHeaderFooterPart]: string read GetHeaderPart write SetHeaderPart;
    property PageCount: integer read GetPageCount;
  published
    property Grid: TCustomGrid read FGrid write SetGrid;
    property BorderLineColor: TColor read FBorderLineColor write FBorderLineColor default clDefault;
    property BorderLineWidth: integer read FBorderLineWidth write FBorderLineWidth default 0;
    property FixedLineColor: TColor read FFixedLineColor write FFixedLineColor default clDefault;
    property FixedLineWidth: integer read FFixedLineWidth write FFixedLineWidth default 0;
    property Footer: string read GetFooter write SetFooter;
    property FooterFont: TFont read FFooterFont write FFooterFont;
    property FooterLine: boolean read FFooterLine write FFooterline default True;
    property FooterLineColor: TColor read FFooterLineColor write FFooterLineColor default clDefault;
    property FooterLineWidth: integer read FFooterLineWidth write FFooterLineWidth default 0;
    property GridLineColor: TColor read FGridLineColor write FGridLineColor default clDefault;
    property GridLineWidth: integer read FGridLineWidth write FGridLineWidth default 0;
    property Header: string read GetHeader write SetHeader;
    property HeaderLine: boolean read FHeaderLine write FHeaderline default True;
    property HeaderLineColor: TColor read FHeaderLineColor write FHeaderLineColor default clDefault;
    property HeaderLineWidth: integer read FHeaderLineWidth write FHeaderLineWidth default 0;
    property HeaderFont: TFont read FHeaderFont write FHeaderFont;
    property Monochrome: boolean read FMonochrome write FMonochrome default False;
    property Margins: TGridPrnMargins read FMargins write FMargins;
    property Orientation: TPrinterOrientation read FOrientation write FOrientation default poPortrait;
    property PrintOrder: TGridPrnOrder read FPrintOrder write FPrintOrder default poRowsFirst;
    property OnGetCellText: TGridPrnGetCellTextEvent read FOnGetCellText write FOnGetCellText;
    property OnPrepareCanvas: TOnPrepareCanvasEvent read FOnPrepareCanvas write FOnPrepareCanvas;
  end;

implementation

uses
  LCLIntf, LCLType, OSPrinters, Themes;

type
  TGridAccess = class(TCustomGrid);

const
  INCH = 25.4;    // 1" = 25.4 mm

  DefaultTextStyle: TTextStyle = (
    Alignment: taLeftJustify;
    Layout: tlCenter;
    SingleLine: True;
    Clipping: True;
    ExpandTabs: False;
    ShowPrefix: False;
    WordBreak: False;
    Opaque: False;
    SystemFont: False;
    RightToLeft: False;
    EndEllipsis: False
    );

function IfThen(cond: boolean; a, b: integer): integer;
begin
  if cond then Result := a
  else
    Result := b;
end;

function IfThen(cond: boolean; a, b: TColor): TColor;
begin
  if cond then Result := a
  else
    Result := b;
end;

function DefaultFontSize(AFont: TFont): integer;
var
  fontData: TFontData;
begin
  fontData := GetFontData(AFont.Handle);
  Result := abs(fontData.Height) * 72 div ScreenInfo.PixelsPerInchY;
end;

procedure FixFontSize(AFont: TFont);
begin
  if AFont.Size = 0 then
    AFont.Size := DefaultFontSize(AFont);
end;

function mm2px(mm: double; dpi: integer): integer;
begin
  Result := round(mm / INCH * dpi);
end;

function px2mm(px: integer; dpi: integer): double;
begin
  Result := px * INCH / dpi;
end;


{ TGridPrnMargins }

constructor TGridPrnMargins.Create;
var
  i: integer;
begin
  inherited Create;
  for i := 0 to 3 do FMargins[i] := 20.0;
  for i := 4 to 5 do FMargins[i] := 10.0;
end;

function TGridPrnMargins.GetMargin(AIndex: integer): double;
begin
  Result := FMargins[AIndex];
end;

function TGridPrnMargins.IsStoredMargin(AIndex: integer): boolean;
begin
  case AIndex of
    0..3: Result := FMargins[AIndex] <> 20.0;
    4..5: Result := FMargins[AIndex] <> 10.0;
  end;
end;

procedure TGridPrnMargins.SetMargin(AIndex: integer; AValue: double);
begin
  FMargins[AIndex] := AValue;
end;

{ TGridPrinter }

constructor TGridPrinter.Create(AOwner: TComponent);
begin
  inherited;

  FMargins := TGridPrnMargins.Create;
  FPrintOrder := poRowsFirst;

  FHeaderFont := TFont.Create;
  FixFontSize(FHeaderFont);
  FHeaderFont.Size := FHeaderFont.Size - 2;
  FHeaderLine := True;
  FHeaderLineColor := clDefault;

  FFooterFont := TFont.Create;
  FixFontSize(FFooterFont);
  FFooterFont.Size := FFooterFont.Size - 2;
  FFooterLine := True;
  FFooterLineColor := clDefault;

  FBorderLineColor := clDefault;
  FFixedLineColor := clDefault;
  FGridLineColor := clDefault;
end;

destructor TGridPrinter.Destroy;
begin
  FHeaderFont.Free;
  FFooterFont.Free;
  FMargins.Free;
  inherited;
end;

function TGridPrinter.CreatePreviewBitmap(APageNo, APercentage: integer): TBitmap;
begin
  if FGrid = nil then
  begin
    Result := nil;
    exit;
  end;

  FOutputDevice := odPreview;

  FPreviewPercent := APercentage;
  FPreviewPage := APageNo;  // out-of-range values are handled by Prepare
  Prepare;

  FPreviewBitmap := TBitmap.Create;
  FPreviewBitmap.SetSize(FPageWidth, FPageHeight);
  FPreviewBitmap.Canvas.Brush.Color := clWhite;
  FPreviewBitmap.Canvas.FillRect(0, 0, FPageWidth, FPageHeight);

  Execute(FPreviewBitmap.Canvas);

  Result := FPreviewBitmap;
end;

procedure TGridPrinter.DoPrepareCanvas(ACol, ARow: integer);
begin
  if Assigned(FOnPrepareCanvas) then
    FOnPrepareCanvas(Self, ACol, ARow, []);
end;

procedure TGridPrinter.Execute(ACanvas: TCanvas);
begin
  FPrinting := True;
  case FPrintOrder of
    poRowsFirst: PrintByRows(ACanvas);
    poColsFirst: PrintByCols(ACanvas);
  end;
  FPrinting := False;
end;

function TGridPrinter.GetCanvas: TCanvas;
begin
  if FPrinting then
    case FOutputDevice of
      odPrinter: Result := Printer.Canvas;
      odPreview: Result := FPreviewBitmap.Canvas;
    end
  else
    Result := nil;
end;

function TGridPrinter.GetCellText(ACol, ARow: integer): string;
var
  col: TGridColumn;
  lGrid: TGridAccess;
begin
  Result := '';
  if FGrid = nil then
    exit;

  lGrid := TGridAccess(FGrid);
  if lGrid.Columns.Enabled and (ACol >= FFixedCols) and (ARow = 0) then
  begin
    col := lGrid.Columns[ACol - FFixedCols];
    Result := col.Title.Caption;
    exit;
  end;

  if Assigned(FOnGetCellText) then
    FOnGetCellText(self, FGrid, ACol, ARow, Result)
  else
    Result := lGrid.GetCells(Acol, ARow);
end;

function TGridPrinter.GetFooter: string;
begin
  Result := FFooterText[hfpLeft] + '|' + FFooterText[hfpCenter] + '|' + FFooterText[hfpRight];
end;

function TGridPrinter.GetFooterPart(AIndex: TGridPrnHeaderFooterPart): string;
begin
  Result := FFooterText[AIndex];
end;

function TGridPrinter.GetHeader: string;
begin
  Result := FHeaderText[hfpLeft] + '|' + FHeaderText[hfpCenter] + '|' + FHeaderText[hfpRight];
end;

function TGridPrinter.GetHeaderFooterText(AText: string): string;
begin
  Result := StringReplace(AText, '$DATE', DateToStr(Now), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '$TIME', TimeToStr(Now), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '$PAGECOUNT', IntToStr(FPageCount), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '$PAGE', IntToStr(FPageNumber), [rfReplaceAll, rfIgnoreCase]);
end;

function TGridPrinter.GetHeaderPart(AIndex: TGridPrnHeaderFooterPart): string;
begin
  Result := FHeaderText[AIndex];
end;

function TGridPrinter.GetPageCount: integer;
begin
  if FPageCount = 0 then
    Prepare;
  Result := FPageCount;
end;

{ Find the column and row indices before which page breaks are occuring.
  Store them in the arrays FPageBreakCols and FPageBreakRows.
  Note that the indices do not contain the fixed columns/rows. }
procedure TGridPrinter.LayoutPageBreaks;
var
  col, row: integer;
  n: integer;
  totalWidth, totalHeight: integer;
begin
  // Scanning horizontally --> get page break column indices
  SetLength(FPageBreakCols, FColCount);
  n := 0;
  totalWidth := FFixedColPos;
  FPageBreakCols[0] := FFixedCols;
  for col := FFixedCols to FColCount - 1 do
  begin
    totalWidth := totalWidth + FColWidths[col];
    if totalWidth >= FPageWidth - FRightMarginPx then
    begin
      Inc(n);
      FPageBreakCols[n] := col;
      totalWidth := FFixedColPos + FColWidths[col];
    end;
  end;
  SetLength(FPageBreakCols, n + 1);

  // Scanning vertically --> get page break row indices
  SetLength(FPageBreakRows, FRowCount);
  n := 0;
  totalHeight := FFixedRowPos;
  FPageBreakRows[0] := FFixedRows;
  for row := FFixedRows to FRowCount - 1 do
  begin
    totalHeight := totalHeight + FRowHeights[row];
    if totalHeight > FPageHeight - FBottomMarginPx then
    begin
      Inc(n);
      FPageBreakRows[n] := row;
      totalHeight := FFixedRowPos + FRowHeights[row];
    end;
  end;
  SetLength(FPageBreakRows, n + 1);

  FPageCount := Length(FPageBreakCols) * Length(FPageBreakRows);
end;

procedure TGridPrinter.NewPage;
begin
  if FOutputDevice = odPrinter then
    Printer.NewPage;
end;

procedure TGridPrinter.Prepare;
const
  Wmm = 210;  // A4 page size
  Hmm = 297;
begin
  Printer.Orientation := FOrientation;

  case FOutputDevice of
    odPrinter:
    begin
      FPixelsPerInchX := Printer.XDPI;
      FPixelsPerInchY := Printer.YDPI;
      FPageWidth := Printer.PageWidth;
      FPageHeight := Printer.PageHeight;
    end;
    odPreview:
    begin
      FPixelsPerInchX := ScreenInfo.PixelsPerInchX * FPreviewPercent div 100;
      FPixelsPerInchY := ScreenInfo.PixelsPerInchY * FPreviewPercent div 100;
      FPageWidth := round(Printer.PageWidth * FPixelsPerInchX / Printer.XDPI);
      FPageHeight := round(Printer.PageHeight * FPixelsPerInchY / Printer.YDPI);
    end;
  end;

  FFactorX := FPixelsPerInchX / ScreenInfo.PixelsPerInchX;
  FFactorY := FPixelsPerInchY / ScreenInfo.PixelsPerInchY;

  FLeftMarginPx := mm2px(FMargins.LeftMargin, FPixelsPerInchX);
  FTopMarginPx := mm2px(FMargins.TopMargin, FPixelsPerInchY);
  FRightMarginPx := mm2px(FMargins.RightMargin, FPixelsPerInchX);
  FBottomMarginPx := mm2px(FMargins.BottomMargin, FPixelsPerInchY);
  FHeaderMarginPx := mm2px(FMargins.HeaderMargin, FPixelsPerInchY);
  FFooterMarginPx := mm2px(FMargins.FooterMargin, FPixelsPerInchY);
  FPadding := ScaleX(varCellPadding);

  ScaleColWidths;
  ScaleRowHeights;
  LayoutPageBreaks;
end;

procedure TGridPrinter.PrepareCanvas(ACanvas: TCanvas; ACol, ARow: integer);
var
  lGrid: TGridAccess;
  color, alternateColor: TColor;
  textStyle: TTextStyle;
begin
  lGrid := TGridAccess(FGrid);

  // Background color
  ACanvas.Brush.Style := bsSolid;
  if (ACol < FFixedCols) or (ARow < FFixedRows) then
    ACanvas.Brush.Color := ColorToRGB(lGrid.FixedColor)
  else
  begin
    color := ColorToRGB(lGrid.Color);
    alternateColor := ColorToRGB(lGrid.AlternateColor);
    if (color <> alternateColor) and Odd(ARow) then
      ACanvas.Brush.Color := alternateColor
    else
      ACanvas.Brush.Color := color;
  end;
  // Font
  SelectFont(ACanvas, lGrid.Font);
  // Text style
  textStyle := DefaultTextStyle;
  if (goCellEllipsis in lGrid.Options) then
    textStyle.EndEllipsis := True;
  ACanvas.TextStyle := textStyle;

  // Fire the event OnPrepareCanvas
  DoPrepareCanvas(ACol, ARow);

  // Fix zero font size and monochrome text color
  FixFontSize(ACanvas.Font);
  if FMonochrome then
    ACanvas.Font.Color := clBlack;
end;

procedure TGridPrinter.Print;
begin
  if FGrid = nil then
    exit;

  FOutputDevice := odPrinter;
  Prepare;
  Printer.BeginDoc;
  try
    Execute(Printer.Canvas);
  finally
    Printer.EndDoc;
  end;
end;

{ Advances first along rows when handling page-breaks. }
procedure TGridPrinter.PrintByCols(ACanvas: TCanvas);
var
  vertPage, horPage: integer;
  col1, col2: integer;
  row1, row2: integer;
begin
  SelectFont(ACanvas, FGrid.Font);
  FPageNumber := 1;

  for horPage := 0 to High(FPageBreakCols) do
  begin
    col1 := FPageBreakCols[horPage];
    if horPage < High(FPageBreakCols) then
      col2 := FPageBreakCols[horPage + 1] - 1
    else
      col2 := FColCount - 1;

    for vertPage := 0 to High(FPageBreakRows) do
    begin
      row1 := FPageBreakRows[vertPage];
      if vertPage < High(FPageBreakRows) then
        row2 := FPageBreakRows[vertPage + 1] - 1
      else
        row2 := FRowCount - 1;
      // Print page beginning at col1/row1
      if (FOutputDevice = odPrinter) or  // Printer renders all pages
        (FPageNumber = FPreviewPage)    // Preview can render only a single page
      then
        PrintPage(ACanvas, col1, row1, col2, row2);
      Inc(FPageNumber);
    end;
  end;
end;

{ Advances first along columns when handling page-breaks. }
procedure TGridPrinter.PrintByRows(ACanvas: TCanvas);
var
  vertPage, horPage: integer;
  col1, col2: integer;
  row1, row2: integer;
begin
  SelectFont(ACanvas, FGrid.Font);
  FPageNumber := 1;

  for vertPage := 0 to High(FPageBreakRows) do
  begin
    row1 := FPageBreakRows[vertPage];
    if vertPage < High(FPageBreakRows) then
      row2 := FPageBreakRows[vertPage + 1] - 1
    else
      row2 := FRowCount - 1;

    for horPage := 0 to High(FPageBreakCols) do
    begin
      col1 := FPageBreakCols[horPage];
      if horPage < High(FPageBreakCols) then
        col2 := FPageBreakCols[horPage + 1] - 1
      else
        col2 := FColCount - 1;
      // Print the page beginning at col1/row1
      if (FOutputDevice = odPrinter) or  // Printer renders all pages
        (FPageNumber = FPreviewPage)    // Preview can render only a single page
      then
        PrintPage(ACanvas, col1, row1, col2, row2);
      Inc(FPageNumber);
    end;
  end;
end;

{ Prints the cell at ACol/ARow. The cell will appear in the given rectangle. }
procedure TGridPrinter.PrintCell(ACanvas: TCanvas; ACol, ARow: integer; ARect: TRect);
var
  s: string;
  col: TGridColumn;
  lGrid: TGridAccess;
  checkedState: TCheckboxState;
begin
  lGrid := TGridAccess(FGrid);

  PrepareCanvas(ACanvas, ACol, ARow);
  if not FMonochrome then
    ACanvas.FillRect(ARect);

  s := GetCellText(ACol, ARow);
  InflateRect(ARect, -FPadding, -FPadding);

  // Handle checkbox columns
  if lGrid.Columns.Enabled and (ACol >= FFixedCols) and (ARow >= FFixedRows) then
  begin
    col := lGrid.Columns[ACol - FFixedCols];
    if col.Buttonstyle = cbsCheckboxColumn then
    begin
      if s = col.ValueChecked then
        checkedState := cbChecked
      else
      if s = col.ValueUnChecked then
        checkedState := cbUnchecked
      else
        checkedState := cbGrayed;
      PrintCheckbox(ACanvas, ACol, ARow, ARect, checkedState);
      exit;
    end;
  end;

  // Normal text output
  ACanvas.TextRect(ARect, ARect.Left, ARect.Top, s);
end;

procedure TGridPrinter.PrintCheckbox(ACanvas: TCanvas; ACol, ARow: integer; ARect: TRect; ACheckState: TCheckboxstate);
const
  arrtb: array[TCheckboxState] of TThemedButton =
    (tbCheckBoxUncheckedNormal, tbCheckBoxCheckedNormal, tbCheckBoxMixedNormal);
var
  details: TThemedElementDetails;
  cSize: TSize;
  R: TRect;
  P: array[0..2] of TPoint;
begin
  details := ThemeServices.GetElementDetails(arrtb[ACheckState]);
  cSize := ThemeServices.GetDetailSize(Details);
  cSize.cx := ScaleX(cSize.cx);
  cSize.cy := ScaleY(cSize.cy);
  R.Left := (ARect.Left + ARect.Right - cSize.cx) div 2;
  R.Top := (ARect.Top + ARect.Bottom - cSize.cy) div 2;
  R.BottomRight := Point(R.Left + cSize.cx, R.Top + cSize.cy);
  ACanvas.Pen.Width := ScaleX(1);
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Style := psSolid;
  if ACheckState = cbGrayed then
    ACanvas.Brush.Color := clSilver
  else
    ACanvas.Brush.Color := clWhite;
  ACanvas.Brush.Style := bsSolid;
  InflateRect(R, -ACanvas.Pen.Width div 2, -ACanvas.Pen.Width div 2);
  ACanvas.Rectangle(R);
  InflateRect(R, -ACanvas.Pen.Width div 2, -ACanvas.Pen.Width div 2);
  if ACheckState in [cbChecked, cbGrayed] then
  begin
    if ACheckState = cbGrayed then ACanvas.Pen.Color := clGray;
    ACanvas.Pen.Width := ScaleX(2);
    P[0] := Point(R.Left + cSize.cx div 6, R.Top + cSize.cy div 2);
    P[1] := Point(R.Left + cSize.cx div 3, R.Bottom - cSize.cy div 6);
    P[2] := Point(R.Right - cSize.cx div 6, R.Top + cSize.cy div 6);
    ACanvas.PolyLine(P);
  end;
end;

{ Prints the column headers: at first the fixed column headers, then the
  headers between ACol1 and ACol2. }
procedure TGridPrinter.PrintColHeaders(ACanvas: TCanvas; ACol1, ACol2: integer);
var
  R: TRect;
  col, row: integer;
  x, y, x2, y2: integer;
begin
  x := FLeftMarginPx;
  y := FTopMarginPx;
  for row := 0 to FFixedRows - 1 do
  begin
    y2 := FTopMarginPx + FRowHeights[row];
    for col := 0 to FFixedCols - 1 do
    begin
      x2 := x + FColWidths[col];
      R := Rect(x, y, x2, y2);
      PrintCell(ACanvas, col, row, R);
      x := x2;
    end;
    for col := ACol1 to ACol2 do
    begin
      x2 := x + FColWidths[col];
      R := Rect(x, y, x2, y2);
      PrintCell(ACanvas, col, row, R);
      x := x2;
    end;
    y := y2;
  end;
end;

procedure TGridPrinter.PrintFooter(ACanvas: TCanvas);
var
  Width: array[TGridPrnHeaderFooterPart] of integer = (0, 0, 0);
  w, h: integer;
  x, y: integer;
  s: string;
  R: TRect;
  textStyle: TTextStyle;
begin
  if (FFooterText[hfpLeft] = '') and (FFooterText[hfpCenter] = '') and (FFooterText[hfpRight] = '') then
    exit;

  SelectFont(ACanvas, FFooterFont);
  w := FPageWidth - FLeftMarginPx - FRightMarginPx;
  if (FFooterText[hfpLeft] <> '') and (FFooterText[hfpCenter] = '') and (FFooterText[hfpRight] = '') then
    Width[hfpLeft] := w
  else
  if (FFooterText[hfpLeft] = '') and (FFooterText[hfpCenter] <> '') and (FFooterText[hfpRight] = '') then
    Width[hfpCenter] := w
  else
  if (FFooterText[hfpLeft] = '') and (FFooterText[hfpCenter] = '') and (FFooterText[hfpRight] <> '') then
    Width[hfpRight] := w
  else
  begin
    Width[hfpLeft] := w div 3;
    Width[hfpCenter] := w div 3;
    Width[hfpRight] := w div 3;
  end;

  h := ACanvas.TextHeight('Rg');
  textStyle := DefaultTextStyle;

  y := FPageHeight - FHeaderMarginPx - h;
  if FFooterText[hfpLeft] <> '' then
  begin
    s := GetHeaderFooterText(FFooterText[hfpLeft]);
    x := FLeftMarginPx;
    R := Rect(x, y, x + Width[hfpLeft], y + h);
    ACanvas.TextRect(R, R.Left, R.Top, s);
  end;
  if FFooterText[hfpCenter] <> '' then
  begin
    s := GetHeaderFooterText(FFooterText[hfpCenter]);
    x := FLeftMarginPx + (FPageWidth - FLeftMarginPx - FRightMarginPx - Width[hfpCenter]) div 2;
    R := Rect(x, y, x + Width[hfpCenter], y + h);
    textStyle.Alignment := taCenter;
    ACanvas.TextRect(R, R.Left, R.Top, s, textStyle);
  end;
  if FFooterText[hfpRight] <> '' then
  begin
    s := GetHeaderFooterText(FFooterText[hfpRight]);
    x := FPageWidth - FRightMarginPx - Width[hfpRight];
    R := Rect(x, y, x + Width[hfpRight], y + h);
    textStyle.Alignment := taRightJustify;
    ACanvas.TextRect(R, R.Left, R.Top, s, textStyle);
  end;

  if FFooterLine then
  begin
    ACanvas.Pen.Color := IfThen(FMonochrome or (FFooterLineColor = clDefault), clBlack, FFooterLineColor);
    ACanvas.Pen.Width := IfThen(FFooterLineWidth = 0, ScaleY(1), FFooterlineWidth);
    ACanvas.Pen.Style := psSolid;
    ACanvas.Line(FLeftMarginPx, y, FPageWidth - FRightMarginPx, y);
  end;

end;

procedure TGridPrinter.PrintGridLines(ACanvas: TCanvas; AFirstCol, AFirstRow, XEnd, YEnd: integer);
const
  HEADERBORDER_LINEWIDTH = 1;
  OUTERBORDER_LINEWIDTH = 2;
var
  x, y: integer;
  col, row: integer;
  lGrid: TGridAccess;
begin
  lGrid := TGridAccess(FGrid);

  // Print inner grid lines
  ACanvas.Pen.Style := lGrid.GridLineStyle;
  ACanvas.Pen.Width := IfThen(FGridLineWidth = 0, lGrid.GridLineWidth, FGridLineWidth);
  ACanvas.Pen.Color := IfThen(FMonoChrome, clBlack, IfThen(FGridLineColor = clDefault, lGrid.GridLineColor, FGridLineColor));
  // ... vertical fixed cell lines
  if (goFixedVertLine in lGrid.Options) then
  begin
    col := 1;
    x := FLeftMarginPx;
    while col < lGrid.FixedCols do
    begin
      x := x + FColWidths[col - 1];
      ACanvas.Line(x, FTopMarginPx, x, YEnd);
      Inc(col);
    end;
    col := AFirstCol;
    x := FFixedColPos;
    while (x < XEnd) and (col < lGrid.ColCount) do
    begin
      x := x + FColWidths[col];
      ACanvas.Line(x, FTopMarginPx, x, FFixedRowPos);
      Inc(col);
    end;
  end;
  // ... vertical grid lines
  if (goVertLine in lGrid.Options) then
  begin
    col := AFirstCol;
    x := FFixedColPos;
    while (x < XEnd) and (col < lGrid.ColCount) do
    begin
      x := x + FColWidths[col];
      ACanvas.Line(x, FFixedRowPos, x, YEnd);
      Inc(col);
    end;
  end;
  // ... horizontal fixed cell lines
  if (goFixedHorzLine in lGrid.Options) then
  begin
    row := 1;
    y := FTopMarginPx;
    while row < lGrid.FixedRows do
    begin
      y := y + FRowHeights[row];
      ACanvas.Line(FLeftMarginPx, y, XEnd, y);
      Inc(row);
    end;
    row := AFirstRow;
    y := FFixedRowPos;
    while (y < YEnd) and (row < lGrid.RowCount) do
    begin
      y := y + FRowHeights[row];
      ACanvas.Line(FLeftMarginPx, y, FFixedColPos, y);
      Inc(row);
    end;
  end;
  // ... horizontal grid lines
  if (goHorzLine in lGrid.Options) then
  begin
    row := AFirstRow;
    y := FFixedRowPos;
    while (y < YEnd) and (row < lGrid.RowCount) do
    begin
      y := y + FRowHeights[row];
      ACanvas.Line(FFixedColPos, y, XEnd, y);
      Inc(row);
    end;
  end;

  // Print header border lines between fixed and normal cells
  // ... horizontal
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := IfThen(FMonochrome or (FFixedLineColor = clDefault), clBlack, FFixedLineColor);
  ACanvas.Pen.Width := IfThen(FFixedLineWidth = 0, ScaleY(HEADERBORDER_LINEWIDTH), FFixedLineWidth);
  ACanvas.Line(FLeftMarginPx, FFixedRowPos, XEnd, FFixedRowPos);
  // ... vertical
  ACanvas.Pen.Width := IfThen(FFixedLineWidth = 0, ScaleX(HEADERBORDER_LINEWIDTH), FFixedLineWidth);
  ACanvas.Line(FFixedColPos, FTopMarginPx, FFixedColPos, YEnd);

  // Print outer border lines
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := IfThen(FMonochrome, clBlack, IfThen(FBorderLineColor = clDefault, clBlack, ColorToRGB(FBorderLineColor)));
  // ... horizontal
  ACanvas.Pen.Width := IfThen(FBorderLineWidth = 0, ScaleY(OUTERBORDER_LINEWIDTH), FBorderLineWidth);
  ACanvas.Line(FLeftMarginPx, FTopMarginPx, XEnd, FTopMarginPx);
  ACanvas.Line(FLeftMarginPx, YEnd, XEnd, YEnd);
  // ... vertical
  ACanvas.Pen.Width := IfThen(FBorderLineWidth = 0, ScaleX(OUTERBORDER_LINEWIDTH), FBorderLineWidth);
  ACanvas.Line(FLeftMarginPx, FTopMarginPx, FLeftMarginPx, YEnd);
  ACanvas.Line(XEnd, FTopMarginPx, XEnd, YEnd);
end;

procedure TGridPrinter.PrintHeader(ACanvas: TCanvas);
var
  Width: array[TGridPrnHeaderFooterPart] of integer = (0, 0, 0);
  w, h: integer;
  x, y: integer;
  s: string;
  R: TRect;
  textStyle: TTextStyle;
begin
  if (FHeaderText[hfpLeft] = '') and (FHeaderText[hfpCenter] = '') and (FHeaderText[hfpRight] = '') then
    exit;

  SelectFont(ACanvas, FHeaderFont);
  w := FPageWidth - FLeftMarginPx - FRightMarginPx;
  if (FHeaderText[hfpLeft] <> '') and (FHeaderText[hfpCenter] = '') and (FHeaderText[hfpRight] = '') then
    Width[hfpLeft] := w
  else
  if (FHeaderText[hfpLeft] = '') and (FHeaderText[hfpCenter] <> '') and (FHeaderText[hfpRight] = '') then
    Width[hfpCenter] := w
  else
  if (FHeaderText[hfpLeft] = '') and (FHeaderText[hfpCenter] = '') and (FHeaderText[hfpRight] <> '') then
    Width[hfpRight] := w
  else
  begin
    Width[hfpLeft] := w div 3;
    Width[hfpCenter] := w div 3;
    Width[hfpRight] := w div 3;
  end;

  h := ACanvas.TextHeight('Rg');
  textStyle := DefaultTextStyle;

  y := FHeaderMarginPx;
  if FHeaderText[hfpLeft] <> '' then
  begin
    s := GetHeaderFooterText(FHeaderText[hfpLeft]);
    x := FLeftMarginPx;
    R := Rect(x, y, x + Width[hfpLeft], y + h);
    ACanvas.TextRect(R, R.Left, R.Top, s);
  end;
  if FHeaderText[hfpCenter] <> '' then
  begin
    s := GetHeaderFooterText(FHeaderText[hfpCenter]);
    x := FLeftMarginPx + (FPageWidth - FLeftMarginPx - FRightMarginPx - Width[hfpCenter]) div 2;
    R := Rect(x, y, x + Width[hfpCenter], y + h);
    textStyle.Alignment := taCenter;
    ACanvas.TextRect(R, R.Left, R.Top, s, textStyle);
  end;
  if FHeaderText[hfpRight] <> '' then
  begin
    s := GetHeaderFooterText(FHeaderText[hfpRight]);
    x := FPageWidth - FRightMarginPx - Width[hfpRight];
    R := Rect(x, y, x + Width[hfpRight], y + h);
    textStyle.Alignment := taRightJustify;
    ACanvas.TextRect(R, R.Left, R.Top, s, textStyle);
  end;

  if FHeaderLine then
  begin
    ACanvas.Pen.Color := IfThen(FMonochrome or (FHeaderLineColor = clDefault), clBlack, FHeaderLineColor);
    ACanvas.Pen.Width := IfThen(FHeaderLineWidth = 0, ScaleY(1), FHeaderlineWidth);
    ACanvas.Pen.Style := psSolid;
    ACanvas.Line(FLeftMarginPx, y + h, FPageWidth - FRightMarginPx, y + h);
  end;
end;

procedure TGridPrinter.PrintPage(ACanvas: TCanvas; AStartCol, AStartRow, AEndCol, AEndRow: integer);
var
  x, y: integer;
  x2, y2: integer;
  row, col: integer;
  lastPagePrinted: boolean;
begin
  // Print column headers
  PrintColHeaders(ACanvas, AStartCol, AEndCol);

  // Print grid cells
  y := FFixedRowPos;
  for row := AStartRow to AEndRow do
  begin
    y2 := y + FRowHeights[row];
    PrintRowHeader(ACanvas, row, y);
    x := FFixedColPos;
    for col := AStartCol to AEndCol do
    begin
      x2 := x + FColWidths[col];
      PrintCell(ACanvas, col, row, Rect(x, y, x2, y2));
      x := x2;
    end;
    y := y2;
  end;

  // Print cell grid lines
  PrintGridLines(ACanvas, AStartCol, AStartRow, x2, y2);

  // Print header and footer
  PrintHeader(ACanvas);
  PrintFooter(ACanvas);

  // Unless we printed the last cell we must send a pagebreak to the printer.
  lastPagePrinted := (AEndCol = FColCount - 1) and (AEndRow = FRowCount - 1);
  if not lastPagePrinted then
    NewPage;
end;

{ Prints the row headers of the specified row. Row headers are the cells in the
  FixedCols of that row. The row is positioned at the given y coordinate on
  the canvas. }
procedure TGridPrinter.PrintRowHeader(ACanvas: TCanvas; ARow, Y: integer);
var
  R: TRect;
  col: integer;
  x, x2, y2: integer;
begin
  x := FLeftMarginPx;            // left side of the row
  y2 := Y + FRowHeights[ARow];   // lower end of the row
  for col := 0 to FFixedCols - 1 do
  begin
    x2 := x + FColWidths[col];
    R := Rect(x, Y, x2, y2);
    PrintCell(ACanvas, col, ARow, R);
    x := x2;
  end;
end;

procedure TGridPrinter.ScaleColWidths;
var
  i: integer;
  w, sum: integer;
begin
  SetLength(FColWidths, FColCount);
  sum := 0;
  for i := 0 to FColCount - 1 do
  begin
    w := ScaleX(TGridAccess(FGrid).ColWidths[i]);
    FColWidths[i] := w;
    sum := sum + w;
    if i < FFixedCols then
      FFixedColPos := FLeftMarginPx + sum;
  end;
end;

procedure TGridPrinter.ScaleRowHeights;
var
  i: integer;
  h, sum: integer;
begin
  SetLength(FRowHeights, FRowCount);
  sum := 0;
  for i := 0 to FRowCount - 1 do
  begin
    h := ScaleY(TGridAccess(FGrid).RowHeights[i]);
    FRowHeights[i] := h;
    sum := sum + h;
    if i < FFixedRows then
      FFixedRowPos := FTopMarginPx + sum;
  end;
end;

function TGridPrinter.ScaleX(AValue: integer): integer;
begin
  Result := Round(FFactorX * AValue);
end;

function TGridPrinter.ScaleY(AValue: integer): integer;
begin
  Result := Round(FFactorY * AValue);
end;

procedure TGridPrinter.SelectFont(ACanvas: TCanvas; AFont: TFont);
var
  fd: TFontData;
begin
  ACanvas.Font.Assign(AFont);
  ACanvas.Font.PixelsPerInch := FPixelsPerInchY;
  if AFont.Size = 0 then
  begin
    fd := GetFontData(AFont.Handle);
    ACanvas.Font.Size := abs(fd.Height) * 72 div ScreenInfo.PixelsPerInchY;
  end;
end;

procedure TGridPrinter.SetFooter(AValue: string);
var
  sa: TStringArray;
begin
  sa := AValue.Split('|');
  if Length(sa) > 0 then FFooterText[hfpLeft] := sa[0]
  else
    FFooterText[hfpLeft] := '';
  if Length(sa) > 1 then FFooterText[hfpCenter] := sa[1]
  else
    FFooterText[hfpCenter] := '';
  if Length(sa) > 2 then FFooterText[hfpRight] := sa[2]
  else
    FFooterText[hfpRight] := '';
end;

procedure TGridPrinter.SetFooterPart(AIndex: TGridPrnHeaderFooterPart; AValue: string);
begin
  FFooterText[AIndex] := AValue;
end;

procedure TGridPrinter.SetGrid(AValue: TCustomGrid);
begin
  FGrid := AValue;
  FColCount := TGridAccess(FGrid).ColCount;
  FRowCount := TGridAccess(FGrid).RowCount;
  FFixedCols := TGridAccess(FGrid).FixedCols;
  FFixedRows := TGridAccess(Fgrid).FixedRows;
  FPageNumber := 0;
  FPageCount := 0;
end;

procedure TGridPrinter.SetHeader(AValue: string);
var
  sa: TStringArray;
begin
  sa := AValue.Split('|');
  if Length(sa) > 0 then FHeaderText[hfpLeft] := sa[0]
  else
    FHeaderText[hfpLeft] := '';
  if Length(sa) > 1 then FHeaderText[hfpCenter] := sa[1]
  else
    FHeaderText[hfpCenter] := '';
  if Length(sa) > 2 then FHeaderText[hfpRight] := sa[2]
  else
    FHeaderText[hfpRight] := '';
end;

procedure TGridPrinter.SetHeaderPart(AIndex: TGridPrnHeaderFooterPart; AValue: string);
begin
  FHeaderText[AIndex] := AValue;
end;

end.
