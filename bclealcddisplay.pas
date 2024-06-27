{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in your Lazarus installation
  for details about the license.
 *****************************************************************************
 Based on LCDLine by Yuriy Tereshchenko
 Initial Lazarus port and multi-line extension: Boban Spasic (spasic@gmail.com)
 Further optimizations and extensions: Werner Pamler
 Published under the name indLCDDisplay as part of the IndustrialStuff package

 Conversion to BGRABitmap and published as part of the BComponents package
 under the name BLCDDisplay: Boban Spasic 2024
}
unit BCLeaLCDDisplay;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, fgl, Graphics, LCLIntf,
  laz2_dom, laz2_xmlwrite, laz2_xmlread,
  BGRABitmapTypes, BGRABitmap, BGRAGradients, BCLeaTheme, BCLeaTypes;

type
  TBCLeaLCDDisplay = class;

  TDotRow = integer;
  TDotRows = array of TDotRow;
  TDotMatrixList = specialize TFPGMap<string, TDotRows>;

  TBCLeaCharDefs = class(TPersistent)
  private
    FBCLeaLCDDisplay: TBCLeaLCDDisplay;
    FCharList: TDotMatrixList;
    FColCount: integer;
    FRowCount: integer;
    function GetCharByIndex(AIndex: integer): string;
    function GetCount: integer;
    function GetDotRows(AChar: string): TDotRows;
    function GetDotRowsByIndex(AIndex: integer): TDotRows;
    procedure SetColCount(AValue: integer);
    procedure SetDotRows(AChar: string; const AValue: TDotRows);
    procedure SetRowCount(AValue: integer);
    function EmptyRows: TDotRows;
    procedure ReadCharDefs(Reader: TReader);
    procedure WriteCharDefs(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(ADisplay: TBCLeaLCDDisplay);
    destructor Destroy; override;
    procedure Add(AChar: string; const ADotRows: TDotRows);
    procedure Assign(ASource: TPersistent); override;
    procedure Clear;
    procedure Delete(AChar: string);
    function DotRowsToString(AChar: string): string;
    function Find(const AChar: string): boolean;
    procedure LoadFromFile(const AFileName: string);
    function SameDotRows(const AChar: string; const ADotRows: TDotRows): boolean;
    procedure SaveToFile(const AFileName: string);
    property Count: integer read GetCount;
    property CharByIndex[AIndex: integer]: string read GetCharByIndex;
    property DotRows[AChar: string]: TDotRows read GetDotRows write SetDotRows;
    property DotRowsByIndex[AIndex: integer]: TDotRows read GetDotRowsByIndex;
  published
    property ColCount: integer read FColCount write SetColCount;
    property RowCount: integer read FRowCount write SetRowCount;
  end;

  TBCLeaLCDDisplay = class(TCustomControl)
  private
    FBitmap: TBGRABitmap;
    FTheme: TBCLeaTheme;
    {
    one char consists of Col x Row of dots
    dots have size and space between dots
    }
    FDotSize: integer;  // dot size in pixels
    FDotsSpace: integer; // inter-dots space in pixels
    FCharCount: integer;
    FGlobalDotColsCount: integer;
    FCharWidth: integer;
    FFrameSize: integer;
    FBoardWidth: integer;
    FBoardHeight: integer;
    FLEDWidth: integer;
    FLEDHeight: integer;
    FFrameColor: TColor;
    FBoardColor: TColor;
    FDotColorOn: TColor;
    FLenText: integer;
    FDisplayLineCount: integer;
    FDisplayCharCount: integer;
    FLines: TStrings;
    FCountOn: integer;
    FFrameStyle: TZStyle;
    FFrameHeight: integer;
    FFrameAltitude: integer;
    FDotShape: TDotShape;
    FCharDefs: TBCLeaCharDefs;
    FOnChange: TNotifyEvent;
    FDotBlend: boolean;
    FDotBlendOperation: TBlendOperation;
    FDotBlur: boolean;
    FDotBlurRadius: single;
    FBoardShadow: TBoardShadow;
    //global intensity of the light
    FLightSourceIntensity: single;
    //minimum distance always added (positive value)
    FLightSourceDistanceTerm: single;
    //how much actual distance is taken into account (usually 0 or 1)
    FLightSourceDistanceFactor: single;
    //how much the location of the lightened pixel is taken into account (usually 0 or 1)
    FLightDestFactor: single;
    //color of the light reflection
    FLightColor: TColor;
    //how much light is reflected (0..1)
    FSpecularFactor: single;
    //how concentrated reflected light is (positive value)
    FSpecularIndex: single;
    //ambiant lighting whereever the point is (0..1)
    FAmbientFactor: single;
    //diffusion, i.e. how much pixels are lightened by light source (0..1)
    FDiffusionFactor: single;
    //how much hidden surface are darkened (0..1)
    FNegativeDiffusionFactor: single;
    //when diffusion saturates, use light color to show it
    FDiffuseSaturation: boolean;
    FLightPositionX: integer;
    FLightPositionY: integer;
    FLightPositionZ: integer;
    function GetDotColCount: integer;
    function GetDotRowCount: integer;
    procedure SetDotColCount(AValue: integer);
    procedure SetDotRowCount(AValue: integer);
    procedure SetDotShape(const Value: TDotShape);
    procedure SetFrameHeight(const Value: integer);
    procedure SeTZStyle(const Value: TZStyle);
    procedure SetFrameAltitude(const Value: integer);
    function GetCharCount: longint;
    function GetGlobalDotColsCount: longint;
    procedure SetDisplayLineCount(const Value: integer);
    procedure SetDisplayCharCount(const Value: integer);
    procedure SetLines(const Value: TStrings);
    procedure SetDotColorOn(const Value: TColor);
    procedure SetBoardColor(const Value: TColor);
    procedure SetFrameColor(const Value: TColor);
    procedure SetFrameSize(const Value: integer);
    procedure SetDotSize(const Value: integer);
    procedure SetDotsSpace(const Value: integer);
    function CalcCharCount: integer;
    procedure InitCharDefs(ACharDefs: TBCLeaCharDefs; AHorDots, AVertDots: integer);
    function IsCharDefsStored: boolean;
    procedure LinesChanged(Sender: TObject);
    procedure SetTheme(AValue: TBCLeaTheme);
    //calculate widths and heights of the display matrix, background border and frame
    procedure Prepare();
    //draw frame
    procedure DrawBorder();
    //call DrawChar for every char
    procedure DrawText();
    //call DrawDot for every dot that is on
    procedure DrawChar(Row, Col, NChar: integer);
    procedure DrawDot(Row, Col: integer; DotColor: TColor);
    procedure DrawBitmapToCanvas();
  protected
    // Basic method of auto-size calculation
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
    {%H-}WithThemeSpace: boolean); override;
    // Takes care of high-dpi scaling
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: double); override;
    procedure DoChange; virtual;
    // inherited painting routine
    procedure Paint; override;
    // Recalculates the geometry if a related property has been changed.
    procedure UpdateSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Adds a user-defined character and its dot matrix.
    procedure AddCharDef(AChar: string; const Dots: TDotRows);
    property CharCount: longint read GetCharCount;
    property DotColCount: integer read GetDotColCount;
    property DotRowCount: integer read GetDotRowCount;
    property GlobalDotColsCount: longint read GetGlobalDotColsCount;
    procedure UpdateTheme;
    procedure ApplyTheme;
    procedure SaveThemeToFile(AFileName: string);
    procedure LoadThemeFromFile(AFileName: string);
    procedure ApplyDefaultTheme;
  published
    property AutoSize default True;
    property BorderSpacing;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property CharDefs: TBCLeaCharDefs read FCharDefs write FCharDefs stored IsCharDefsStored;
    property DotSize: integer read FDotSize write SetDotSize default 4;
    property DotsSpace: integer read FDotsSpace write SetDotsSpace default 1;
    property FrameSize: integer read FFrameSize write SetFrameSize default 8;
    property FrameAltitude: integer read FFrameAltitude write SetFrameAltitude default 2;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clBtnFace;
    // To use BoardColor, ColorScheme must be set to csCustom
    property BoardColor: TColor read FBoardColor write SetBoardColor default clBlack;
    // To use DotColorOn, ColorScheme must be set to csCustom
    property DotColorOn: TColor read FDotColorOn write SetDotColorOn default clSkyBlue;
    // Vertical screen size in chars
    // Without AutoSize, if the frame is too small
    // a part off the text will not be visible
    // e.g. frame is big enough for one line
    // and the text contains 3 lines
    // - just the middle line will be visible
    property DisplayLineCount: integer read FDisplayLineCount write SetDisplayLineCount default 2;
    // Horizontal screen size in chars
    // Set to <=0 (zero) to have a real AutoSize
    // Has no effect without AutoSize
    property DisplayCharCount: integer read FDisplayCharCount write SetDisplayCharCount default 10;
    // The text to display
    // It will be truncated according
    // to ScreenRowCount and ScreenColCount
    property Lines: TStrings read FLines write SetLines;
    property FrameStyle: TZStyle read FFrameStyle write SeTZStyle default zsRaised;
    property FrameHeight: integer read FFrameHeight write SetFrameHeight default 8;
    property DotShape: TDotShape read FDotShape write SetDotShape default stSquare;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Theme: TBCLeaTheme read FTheme write SetTheme;
  end;

function CopyDotRows(const ADotRows: array of TDotRow): TDotRows;

procedure Register;

implementation

uses
  Dialogs, LazUTF8, LazUnicode;

const
  DEFAULT_DOT_COL_COUNT = 5;
  DEFAULT_DOT_ROW_COUNT = 7;

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCLeaLCDDisplay]);
end;

{ Create a "real" copy to avoid reference counter issues. }
function CopyDotRows(const ADotRows: array of TDotRow): TDotRows;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, Length(ADotRows));
  for i := 0 to High(ADotRows) do
    Result[i] := ADotRows[i];
end;


{ TBCLeaCharDefs }

constructor TBCLeaCharDefs.Create(ADisplay: TBCLeaLCDDisplay);
begin
  inherited Create;
  FBCLeaLCDDisplay := ADisplay;
  FCharList := TDotMatrixList.Create;
  FCharList.Sorted := True;
end;

destructor TBCLeaCharDefs.Destroy;
begin
  FCharList.Free;
  inherited;
end;

{ Adds a new character dot matrix. }
procedure TBCLeaCharDefs.Add(AChar: string; const ADotRows: TDotRows);
begin
  if Length(ADotRows) <> FRowCount then
    raise Exception.Create('Incorrect number of rows.');
  // Make sure to reset the reference counter --> use a local copy of ADotRows!
  FCharList.Add(AChar, CopyDotRows(ADotRows));
end;

procedure TBCLeaCharDefs.Assign(ASource: TPersistent);
var
  i: integer;
begin
  if (ASource is TBCLeaCharDefs) then
  begin
    FColCount := TBCLeaCharDefs(ASource).ColCount;
    FRowCount := TBCLeaCharDefs(ASource).RowCount;
    Clear;
    for i := 0 to TBCLeaCharDefs(ASource).Count - 1 do
      Add(TBCLeaCharDefs(ASource).CharByIndex[i], TBCLeaCharDefs(ASource).DotRowsByIndex[i]);
  end
  else
    inherited;
end;

{ Clears all characters and their dot matrices. }
procedure TBCLeaCharDefs.Clear;
begin
  FCharList.Clear;
end;

{ Prepares streaming of the dot matrices }
procedure TBCLeaCharDefs.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('CharDefs', @ReadCharDefs, @WriteCharDefs, True);
end;

{ Deletes the specified character and its dot matrix from the list. }
procedure TBCLeaCharDefs.Delete(AChar: string);
var
  idx: integer;
begin
  if FCharList.Find(AChar, idx) then
    FCharList.Delete(idx);
end;

{ Display the elements of the RowDots as a string. For debugging purposes. }
function TBCLeaCharDefs.DotRowsToString(AChar: string): string;
var
  lDotRows: TDotRows;
  i: integer;
begin
  lDotRows := DotRows[AChar];
  Result := IntToStr(lDotRows[0]);
  for i := 1 to High(lDotRows) do
    Result := Result + ',' + IntToStr(lDotRows[i]);
end;

{ Creates an empty row in which not dots are set. }
function TBCLeaCharDefs.EmptyRows: TDotRows;
var
  row: integer;
begin
  Result := nil;
  SetLength(Result, FRowCount);
  for row := 0 to FRowCount - 1 do
    Result[row] := 0;
end;

function TBCLeaCharDefs.GetCharByIndex(AIndex: integer): string;
begin
  Result := FCharList.Keys[AIndex];
end;

{ Returns the number of characters contained. }
function TBCLeaCharDefs.GetCount: integer;
begin
  Result := FCharList.Count;
end;

{ Returns the dot matrix rows for the specified character. Each row is an 
  integer in which each bit is interpreted as a dot. }
function TBCLeaCharDefs.GetDotRows(AChar: string): TDotRows;
var
  idx: integer;
begin
  if FCharList.Find(AChar, idx) then
    Result := FCharList.Data[idx]
  else
    Result := EmptyRows;
end;

function TBCLeaCharDefs.GetDotRowsByIndex(AIndex: integer): TDotRows;
begin
  Result := FCharList.Data[AIndex];
end;

function TBCLeaCharDefs.Find(const AChar: string): boolean;
var
  idx: integer;
begin
  Result := FCharList.Find(AChar, idx);
end;

{ Reads the list of character name and dot matrices from the LFM file. The data
  are stored in the LFM as a comma-separated list beginning with the character 
  name. }
procedure TBCLeaCharDefs.ReadCharDefs(Reader: TReader);
var
  i: integer;
  s: string;
  ch: string;
  sa: TStringArray;
  rows: TDotRows = nil;
begin
  Clear;
  Reader.ReadListBegin;
  while not Reader.EndOfList do
  begin
    s := Reader.ReadString;
    if s[1] = ',' then
    begin
      ch := ',';
      System.Delete(s, 1, 2);
    end
    else
    begin
      i := pos(',', s);
      ch := copy(s, 1, i - 1);
      System.Delete(s, 1, i);
    end;
    sa := s.Split(',');
    SetLength(rows, Length(sa));
    for i := 0 to High(sa) do
      rows[i] := StrToInt(sa[i]);
    Add(ch, rows);
  end;
  Reader.ReadListEnd;
end;

function TBCLeaCharDefs.SameDotRows(const AChar: string; const ADotRows: TDotRows): boolean;
var
  i: integer;
  lDotRows: TDotRows;
begin
  Result := False;
  lDotRows := DotRows[AChar];
  if (Length(lDotRows) <> Length(ADotRows)) then
    exit;
  for i := 0 to High(lDotRows) do
    if lDotRows[i] <> ADotRows[i] then exit;
  Result := True;
end;

function DotRowsToStr(ADotRows: TDotRows): string;
var
  i: integer;
begin
  Result := IntToStr(ADotRows[0]);
  for i := 1 to High(ADotRows) do
    Result := Result + ',' + IntToStr(ADotRows[i]);
end;

function StrToDotRows(AString: string): TDotRows;
var
  sa: TStringArray;
  i: integer;
begin
  Result := nil;
  sa := AString.Split(',');
  SetLength(Result, Length(sa));
  for i := 0 to High(sa) do
    Result[i] := StrToInt(sa[i]);
end;

procedure TBCLeaCharDefs.LoadFromFile(const AFileName: string);
var
  doc: TXMLDocument = nil;
  rootNode, parentNode, node, childNode: TDOMNode;
  nodeName: string;
  s: string;
  ch: string;
  dots: TDotRows;
begin
  FCharList.Clear;

  try
    ReadXMLFile(doc, AFileName);
    rootNode := doc.DocumentElement;
    parentNode := rootNode.FirstChild;
    while Assigned(parentNode) do
    begin
      nodeName := parentNode.NodeName;
      if nodeName = 'DotColCount' then
      begin
        s := TDOMElement(parentNode).GetAttribute('Value');
        if s <> '' then
          FColCount := StrToInt(s)
        else
          raise Exception.Create('DotColCount missing');
      end
      else
      if nodeName = 'DotRowCount' then
      begin
        s := TDOMElement(parentNode).GetAttribute('Value');
        if s <> '' then
          FRowCount := StrToInt(s)
        else
          raise Exception.Create('DotRowCount missing');
      end
      else
      if nodeName = 'Chars' then
      begin
        node := parentNode.FirstChild;
        while Assigned(node) do
        begin
          childnode := node.FirstChild;
          ch := '';
          dots := nil;
          while Assigned(childnode) do
          begin
            nodeName := childNode.NodeName;
            if nodeName = 'Name' then
              ch := childNode.TextContent
            else
            if nodeName = 'DotRows' then
            begin
              s := childNode.TextContent;
              dots := StrToDotRows(s);
            end;
            childNode := childNode.NextSibling;
          end;
          if ch = '' then
            raise Exception.Create('Char "Name" missing.');
          if dots = nil then
            raise Exception.Create('Char "DotRows" missing.');
          Add(ch, dots);
          node := node.NextSibling;
        end;
      end;
      parentNode := parentNode.NextSibling;
    end;

    with FBCLeaLCDDisplay do
    begin
      if AutoSize then
      begin
        InvalidatePreferredSize;
        AdjustSize;
      end;
      Invalidate;
    end;

  finally
    doc.Free;
  end;
end;

{ Saves the list (character plus dot matrix for each) to an xml file. }
procedure TBCLeaCharDefs.SaveToFile(const AFileName: string);
var
  doc: TXMLDocument;
  rootNode, parentNode, node, childNode, textNode: TDOMNode;
  i: integer;
begin
  doc := TXMLDocument.Create;
  try
    rootNode := doc.CreateElement('LCD-CharDefs');
    doc.AppendChild(rootNode);

    rootNode := doc.DocumentElement;
    node := doc.CreateElement('DotColCount');
    TDOMElement(node).SetAttribute('Value', IntToStr(ColCount));
    rootNode.AppendChild(node);

    node := doc.CreateElement('DotRowCount');
    TDOMElement(node).SetAttribute('Value', IntToStr(RowCount));
    rootNode.AppendChild(node);

    parentNode := doc.CreateElement('Chars');
    rootNode.AppendChild(parentNode);

    for i := 0 to Count - 1 do
    begin
      node := doc.CreateElement('Char');
      parentNode.AppendChild(node);
      childNode := doc.CreateElement('Name');
      node.AppendChild(childNode);
      textNode := doc.CreateTextNode(CharByIndex[i]);
      childnode.AppendChild(textNode);
      childNode := doc.CreateElement('DotRows');
      node.AppendChild(childNode);
      textNode := doc.CreateTextNode(DotRowsToStr(DotRowsByIndex[i]));
      childNode.Appendchild(textNode);
    end;

    WriteXMLFile(doc, AFileName);
  finally
    doc.Free;
  end;
end;

{ Returns the number of columns of the dot matrix. }
procedure TBCLeaCharDefs.SetColCount(AValue: integer);
begin
  if FColCount = AValue then
    exit;
  FColCount := AValue;
  FCharList.Clear;
end;

{ Sets the dot matrix for the specified character }
procedure TBCLeaCharDefs.SetDotRows(AChar: string; const AValue: TDotRows);
var
  idx: integer;
begin
  if FCharList.Find(AChar, idx) then
    Delete(AChar);
  Add(AChar, AValue);
end;

{ Returns the number of rows of the dot matrix. }
procedure TBCLeaCharDefs.SetRowCount(AValue: integer);
begin
  if FRowCount = AValue then
    exit;
  FRowCount := AValue;
  FCharList.Clear;
end;

{ Write the character and its dot matrix to the LFM. Each character is stored
  as a comma-separated list of character and dotrow values. }
procedure TBCLeaCharDefs.WriteCharDefs(Writer: TWriter);
var
  i, j: integer;
  ch: string;
  rows: TDotRows;
  s: string;
begin
  Writer.WriteListBegin;
  for i := 0 to Count - 1 do
  begin
    ch := FCharList.Keys[i];
    rows := DotRows[ch];
    s := ch;
    for j := 0 to FRowCount - 1 do
      s := s + ',' + IntToStr(rows[j]);
    Writer.WriteString(s);
  end;
  Writer.WriteListEnd;
end;

(*
{ TBCLeaLCDDisplayStrings }

type
  TBCLeaLCDDisplayStrings = class(TStrings)
  private
    FOnChange: TNotifyEvent;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
*)

{ TBCLeaLCDDisplay}

constructor TBCLeaLCDDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];

  Width := 156;
  Height := 76;

  FDisplayLineCount := 2;
  FDisplayCharCount := 10;
  FBoardWidth := 4;

  FCharDefs := TBCLeaCharDefs.Create(self);
  FCharDefs.ColCount := DEFAULT_DOT_COL_COUNT;
  FCharDefs.RowCount := DEFAULT_DOT_ROW_COUNT;
  InitCharDefs(FCharDefs, DotColCount, DotRowCount);

  FBitmap := TBGRABitmap.Create;
  FCountOn := 255;

  FLines := TStringList.Create;
  TStringList(FLines).OnChange := @LinesChanged;
  AutoSize := True;

  ApplyDefaultTheme;
end;

destructor TBCLeaLCDDisplay.Destroy;
begin
  FreeAndNil(FBitmap);
  FCharDefs.Free;
  FLines.Free;
  inherited Destroy;
end;

procedure TBCLeaLCDDisplay.AddCharDef(AChar: string; const Dots: TDotRows);
begin
  FCharDefs.Add(AChar, Dots);
end;

procedure TBCLeaLCDDisplay.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
var
  nDotCols: integer;
  nDotRows: integer;
begin
  nDotCols := DotColCount;
  nDotRows := DotRowCount;

  FCharWidth := (DotSize * nDotCols) + (DotsSpace * (nDotCols + 1)); //pixels
  FCharCount := CalcCharCount;
  FGlobalDotColsCount := (FCharCount * nDotCols) + (FCharCount - 1);      //dots

  // total matrix width
  FLEDWidth := (FGlobalDotColsCount * DotSize) + ((FGlobalDotColsCount - 1) * DotsSpace);
  // total matrix height
  FLEDHeight := (FDisplayLineCount * nDotRows * (DotSize + DotsSpace)) +
    ((FDisplayLineCount - 1) * (DotSize + DotsSpace));

  //background around text matrix - left/right pixels
  FBoardWidth := DotSize + DotsSpace;
  //background around text matrix - up/down pixels
  FBoardHeight := DotSize + DotsSpace;

  //Total size incl. frame
  PreferredWidth := FLEDWidth + (2 * FrameSize) + (2 * FBoardWidth);
  PreferredHeight := FLEDHeight + (2 * FrameSize) + (2 * FBoardWidth);
end;

procedure TBCLeaLCDDisplay.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: double);
begin
  inherited;
  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
  begin
    FDotSize := round(FDotSize * AXProportion);
    FDotsSpace := round(FDotsSpace * AXProportion);
    FFrameSize := round(FFrameSize * AXProportion);
  end;
end;

procedure TBCLeaLCDDisplay.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TBCLeaLCDDisplay.Prepare();
var
  nDotCols: integer;
  nDotRows: integer;
begin
  nDotCols := DotColCount;
  nDotRows := DotRowCount;

  FCharWidth := (DotSize * nDotCols) + (DotsSpace * (nDotCols + 1)); //pixels
  FCharCount := ((Width - (2 * FrameSize)) + DotSize) div (FCharWidth + DotSize);
  FGlobalDotColsCount := (FCharCount * nDotCols) + (FCharCount - 1);      //dots

  // total matrix width
  FLEDWidth := (FGlobalDotColsCount * DotSize) + ((FGlobalDotColsCount - 1) * DotsSpace);
  // total matrix height
  FLEDHeight := (FDisplayLineCount * nDotRows * (DotSize + DotsSpace)) +
    ((FDisplayLineCount - 1) * (DotSize + DotsSpace));

  FBoardWidth := (Width - 2 * FrameSize - FLEDWidth) div 2;
  FBoardHeight := (Height - 2 * FrameSize - FLEDHeight) div 2;

  FBitmap.SetSize(Width, Height);
  FBitmap.Fill(FBoardColor);
  FBitmap.Canvas2D.resetTransform;
end;

procedure TBCLeaLCDDisplay.DrawBorder();
var
  ScaledPhongSize: integer;
  EffectiveLineWidth: single;
  Blur, Mask, TempBMP, ShadowBoard: TBGRABitmap;
  Phong: TPhongShading;
begin

  if FFrameHeight = 0 then
    EffectiveLineWidth := Height / 12
  else
    EffectiveLineWidth := FFrameHeight;

  FBitmap.Canvas2D.lineWidth := EffectiveLineWidth;

  FBitmap.Canvas2D.lineCapLCL := pecRound;
  FBitmap.Canvas2D.strokeStyle(FrameColor);
  FBitmap.Canvas2D.beginPath;
  FBitmap.Canvas2D.rect(FFrameHeight / 2, FFrameHeight / 2, Width - FFrameHeight, Height - FFrameHeight);
  FBitmap.Canvas2D.stroke;

  if (FFrameStyle = zsLowered) or (FFRameStyle = zsRaised) then
  begin
    //make a copy for bsOwn
    ShadowBoard := TBGRABitmap.Create(Width, Height, BGRABlack);
    ShadowBoard.PutImage(0, 0, FBitmap, dmSet);
    //draw frame inclusive board
    ScaledPhongSize := round(EffectiveLineWidth / 2);
    Mask := FBitmap.FilterGrayscale as TBGRABitmap;
    if FFRameStyle = zsLowered then
      Mask.Negative;
    Blur := Mask.FilterBlurRadial(ScaledPhongSize, ScaledPhongSize, rbFast) as TBGRABitmap;
    Blur.FillMask(0, 0, Mask, BGRAPixelTransparent, dmSet);
    Mask.Free;
    //make it 3D
    Phong := TPhongShading.Create;
    if assigned(FTheme) then
    begin
      Phong.AmbientFactor := FAmbientFactor;
      Phong.SpecularIndex := FSpecularIndex;
      Phong.LightDestFactor := FLightDestFactor;
      Phong.LightPosition := Point(FLightPositionX, FLightPositionY);
      Phong.LightPositionZ := FLightPositionZ;
      Phong.LightSourceIntensity := FLightSourceIntensity;
      Phong.LightSourceDistanceTerm := FLightSourceDistanceTerm;
      Phong.LightSourceDistanceFactor := FLightSourceDistanceFactor;
      Phong.NegativeDiffusionFactor := FNegativeDiffusionFactor;
      Phong.SpecularFactor := FSpecularFactor;
      Phong.DiffusionFactor := FDiffusionFactor;
      Phong.DiffuseSaturation := FDiffuseSaturation;
      Phong.LightColor := FLightColor;
    end;

    Phong.Draw(FBitmap, Blur, FFrameAltitude, 0, 0, FBitmap);
    Phong.Free;
    //now are both the frame and the board processed with phong effect and stored in FBitmap

    //re-draw board (without frame)
    if FBoardShadow = bsNone then
    begin
      //create mask for the frame
      Mask := TBGRABitmap.Create(Width, Height, BGRABlack);
      Mask.Canvas2D.lineWidth := EffectiveLineWidth;
      Mask.Canvas2D.lineCapLCL := pecRound;
      Mask.Canvas2D.strokeStyle(BGRAWhite);
      Mask.Canvas2D.beginPath;
      Mask.Canvas2D.rect(FFrameHeight / 2, FFrameHeight / 2, Width - FFrameHeight, Height - FFrameHeight);
      Mask.Canvas2D.stroke;
      //create new bitmap in BoardColor and add just the frame
      TempBMP := TBGRABitmap.Create(Width, Height, ColorToBGRA(ColorToRGB(FBoardColor)));
      TempBMP.PutImage(0, 0, FBitmap, dmSet);
      TempBMP.ApplyMask(Mask);
      Mask.Free;
      FBitmap.Fill(FBoardColor);
      FBitmap.PutImage(0, 0, TempBMP, dmDrawWithTransparency);
      TempBMP.Free;
    end;
    if FBoardShadow = bsOwn then
    begin
      //ToDo - not sure about this
      Phong := TPhongShading.Create;
      if assigned(FTheme) then
      begin
        Phong.AmbientFactor := FAmbientFactor;
        Phong.SpecularIndex := FSpecularIndex;
        Phong.LightDestFactor := FLightDestFactor;
        Phong.LightPosition := Point(FLightPositionX, FLightPositionY);
        Phong.LightPositionZ := FLightPositionZ;
        Phong.LightSourceIntensity := FLightSourceIntensity;
        Phong.LightSourceDistanceTerm := FLightSourceDistanceTerm;
        Phong.LightSourceDistanceFactor := FLightSourceDistanceFactor;
        Phong.NegativeDiffusionFactor := FNegativeDiffusionFactor;
        Phong.SpecularFactor := FSpecularFactor;
        Phong.DiffusionFactor := FDiffusionFactor;
        Phong.DiffuseSaturation := FDiffuseSaturation;
        Phong.LightColor := FLightColor;
      end;
      Phong.Draw(ShadowBoard, Blur, 1, 0, 0, ShadowBoard);
      Phong.Free;
      //create mask for the frame
      Mask := TBGRABitmap.Create(Width, Height, BGRAWhite);
      Mask.Canvas2D.lineWidth := EffectiveLineWidth;
      Mask.Canvas2D.lineCapLCL := pecRound;
      Mask.Canvas2D.strokeStyle(BGRABlack);
      Mask.Canvas2D.beginPath;
      Mask.Canvas2D.rect(FFrameHeight / 2, FFrameHeight / 2, Width - FFrameHeight, Height - FFrameHeight);
      Mask.Canvas2D.stroke;
      ShadowBoard.ApplyMask(Mask);
      Mask.Free;
      FBitmap.PutImage(0, 0, ShadowBoard, dmDrawWithTransparency);
    end;
    ShadowBoard.Free;
    Blur.Free;
  end;
end;

procedure TBCLeaLCDDisplay.DrawText();
var
  x, y, c: integer;
  dots: TDotRows;
  dotRow: TDotRow;
  dotOn: boolean;
  i: integer;
  line: string;
  ch: string;
begin
  for i := 0 to FDisplayLineCount - 1 do
  begin
    if i < FLines.Count then
    begin
      line := FLines[i];
      FLenText := UTF8Length(line);

      c := 0;
      for ch in line do
      begin
        Inc(c);
        dots := FCharDefs.DotRows[ch];
        for y := 0 to DotRowCount - 1 do
        begin
          DotRow := dots[y];
          for x := 0 to 4 do
          begin
            DotOn := DotRow and (1 shl (5 - x - 1)) > 0;
            if DotOn then
              DrawChar(y + 8 * i, x, c);
          end; // for x
        end;  // for y
      end;  // for ch
    end;
  end;
end;


procedure TBCLeaLCDDisplay.DrawChar(Row, Col, NChar: integer);
begin

  Col := Col + ((DotColCount + 1) * (NChar - 1));
  if Col > FGlobalDotColsCount - 1 then
    Exit;
  if Col < 0 then
    Exit;
  DrawDot(Row, Col, FDotColorOn);

end;

procedure TBCLeaLCDDisplay.DrawDot(Row, Col: integer; DotColor: TColor);
var
  DotR: TRect;
  Mask: TBGRABitmap;
begin
  DotR.Left := FrameSize + FBoardWidth + (DotSize + DotsSpace) * Col;
  DotR.Top := FrameSize + FBoardHeight + (DotSize + DotsSpace) * Row;
  DotR.Right := DotR.Left + DotSize;
  DotR.Bottom := DotR.Top + DotSize;
  //todo
  if FFrameHeight = 4 then
  begin
    if (DotR.Top <= FFrameSize) or (DotR.Bottom >= Height - FFrameSize) then
      exit;
    if (DotR.Left <= FFrameSize) or (DotR.Right >= Width - FFrameSize) then
      exit;
  end
  else
  begin
    if (DotR.Top <= FFrameSize + 1) or (DotR.Bottom >= Height - FFrameSize - 1) then
      exit;
    if (DotR.Left <= FFrameSize + 1) or (DotR.Right >= Width - FFrameSize - 1) then
      exit;
  end;

  Mask := TBGRABitmap.Create(Width, Height);
  if DotShape = stSquare then
    Mask.FillRect(DotR, DotColor)
  else
    Mask.FillEllipseAntialias(DotR.Left + DotSize / 2, DotR.Top + DotSize / 2, DotSize / 2, DotSize / 2, DotColor);
  if FDotBlur then
    Mask := Mask.FilterBlurRadial(DotSize * FDotBlurRadius, DotSize * FDotBlurRadius, rbFast);
  if FDotBlend then
    FBitmap.BlendImageOver(0, 0, Mask, FDotBlendOperation)
  else
    FBitmap.PutImage(0, 0, Mask, dmDrawWithTransparency);
  Mask.Free;
end;

procedure TBCLeaLCDDisplay.DrawBitmapToCanvas();
begin
  FBitmap.Draw(Canvas, 0, 0, True);
end;

procedure TBCLeaLCDDisplay.Paint();
begin
  Prepare();
  DrawBorder();
  DrawText();
  DrawBitmapToCanvas();
end;

// Find the longest's line length
function TBCLeaLCDDisplay.CalcCharCount: integer;
var
  len: integer;
  i, tmp: integer;
begin
  len := 1;
  if FDisplayCharCount > 0 then
    Result := FDisplayCharCount
  else
  begin
    for i := 0 to FDisplayLineCount - 1 do
    begin
      if i < Lines.Count then
      begin
        tmp := UTF8Length(Lines[i]);
        if tmp > Len then Len := tmp;
      end;
    end;
    Result := Len;
  end;
end;

procedure TBCLeaLCDDisplay.InitCharDefs(ACharDefs: TBCLeaCharDefs; AHorDots, AVertDots: integer);
begin
  ACharDefs.Clear;
  if (AHorDots = DEFAULT_DOT_COL_COUNT) and (AVertDots = DEFAULT_DOT_ROW_COUNT) then
  begin
    // Note: Passing the array via CopyDotRows is for compilation with FPC before v3.2
    ACharDefs.Add('!', CopyDotRows([4, 4, 4, 4, 4, 0, 4]));        // #33
    ACharDefs.Add('"', CopyDotRows([10, 10, 0, 0, 0, 0, 0]));      // #34
    ACharDefs.Add('#', CopyDotRows([0, 10, 31, 10, 31, 10, 0]));   // #35
    ACharDefs.Add('$', CopyDotRows([4, 15, 20, 14, 5, 30, 4]));    // #36
    ACharDefs.Add('%', CopyDotRows([25, 26, 2, 4, 8, 11, 19]));    // #37
    ACharDefs.Add('&', CopyDotRows([12, 18, 20, 8, 21, 18, 13]));  // #38
    ACharDefs.Add('''', CopyDotRows([4, 4, 0, 0, 0, 0, 0]));       // #39
    ACharDefs.Add('(', CopyDotRows([2, 4, 8, 8, 8, 4, 2]));        // #40
    ACharDefs.Add(')', CopyDotRows([8, 4, 2, 2, 2, 4, 8]));        // #41
    ACharDefs.Add('*', CopyDotRows([0, 4, 21, 14, 21, 4, 0]));     // #42
    ACharDefs.Add('+', CopyDotRows([0, 4, 4, 31, 4, 4, 0]));       // #43
    ACharDefs.Add(',', CopyDotRows([0, 0, 0, 0, 12, 4, 8]));       // #44
    ACharDefs.Add('-', CopyDotRows([0, 0, 0, 14, 0, 0, 0]));       // #45
    ACharDefs.Add('.', CopyDotRows([0, 0, 0, 0, 0, 12, 12]));      // #46
    ACharDefs.Add('/', CopyDotRows([1, 1, 2, 4, 8, 16, 16]));      // #47
    ACharDefs.Add('0', CopyDotRows([14, 17, 19, 21, 25, 17, 14])); // #48
    ACharDefs.Add('1', CopyDotRows([4, 12, 4, 4, 4, 4, 14]));      // #49
    ACharDefs.Add('2', CopyDotRows([14, 17, 1, 2, 4, 8, 31]));     // #50
    ACharDefs.Add('3', CopyDotRows([14, 17, 1, 6, 1, 17, 14]));    // #51
    ACharDefs.Add('4', CopyDotRows([2, 6, 10, 18, 31, 2, 2]));     // #52
    ACharDefs.Add('5', CopyDotRows([31, 16, 30, 1, 1, 17, 14]));   // #53
    ACharDefs.Add('6', CopyDotRows([14, 17, 16, 30, 17, 17, 14])); // #54
    ACharDefs.Add('7', CopyDotRows([31, 1, 1, 2, 4, 4, 4]));       // #55
    ACharDefs.Add('8', CopyDotRows([14, 17, 17, 14, 17, 17, 14])); // #56
    ACharDefs.Add('9', CopyDotRows([14, 17, 17, 15, 1, 17, 14]));  // #57
    ACharDefs.Add(':', CopyDotRows([0, 12, 12, 0, 12, 12, 0]));    // #58
    ACharDefs.Add(';', CopyDotRows([0, 12, 12, 0, 12, 4, 8]));     // #59
    ACharDefs.Add('<', CopyDotRows([2, 4, 8, 16, 8, 4, 2]));       // #60
    ACharDefs.Add('=', CopyDotRows([0, 0, 31, 0, 31, 0, 0]));      // #61
    ACharDefs.Add('>', CopyDotRows([8, 4, 2, 1, 2, 4, 8]));        // #62
    ACharDefs.Add('?', CopyDotRows([14, 17, 1, 2, 4, 0, 4]));      // #63
    ACharDefs.Add('@', CopyDotRows([14, 17, 19, 21, 23, 16, 15])); // #64
    ACharDefs.Add('A', CopyDotRows([14, 17, 17, 31, 17, 17, 17])); // #65
    ACharDefs.Add('B', CopyDotRows([30, 17, 17, 30, 17, 17, 30])); // #66
    ACharDefs.Add('C', CopyDotRows([14, 17, 16, 16, 16, 17, 14])); // #67
    ACharDefs.Add('D', CopyDotRows([30, 17, 17, 17, 17, 17, 30])); // #68
    ACharDefs.Add('E', CopyDotRows([31, 16, 16, 30, 16, 16, 31])); // #69
    ACharDefs.Add('F', CopyDotRows([31, 16, 16, 30, 16, 16, 16])); // #70
    ACharDefs.Add('G', CopyDotRows([14, 17, 16, 19, 17, 17, 14])); // #71
    ACharDefs.Add('H', CopyDotRows([17, 17, 17, 31, 17, 17, 17])); // #72
    ACharDefs.Add('I', CopyDotRows([14, 4, 4, 4, 4, 4, 14]));      // #73
    ACharDefs.Add('J', CopyDotRows([1, 1, 1, 1, 17, 17, 14]));     // #74
    ACharDefs.Add('K', CopyDotRows([17, 18, 20, 24, 20, 18, 17])); // #75
    ACharDefs.Add('L', CopyDotRows([16, 16, 16, 16, 16, 16, 31])); // #76
    ACharDefs.Add('M', CopyDotRows([17, 27, 21, 21, 17, 17, 17])); // #77
    ACharDefs.Add('N', CopyDotRows([17, 25, 21, 19, 17, 17, 17])); // #78
    ACharDefs.Add('O', CopyDotRows([14, 17, 17, 17, 17, 17, 14])); // #79
    ACharDefs.Add('P', CopyDotRows([30, 17, 17, 30, 16, 16, 16])); // #80
    ACharDefs.Add('Q', CopyDotRows([14, 17, 17, 17, 17, 14, 1]));  // #81
    ACharDefs.Add('R', CopyDotRows([30, 17, 17, 30, 17, 17, 17])); // #82
    ACharDefs.Add('S', CopyDotRows([14, 17, 16, 14, 1, 17, 14]));  // #83
    ACharDefs.Add('T', CopyDotRows([31, 4, 4, 4, 4, 4, 4]));       // #84
    ACharDefs.Add('U', CopyDotRows([17, 17, 17, 17, 17, 17, 14])); // #85
    ACharDefs.Add('V', CopyDotRows([17, 17, 17, 17, 17, 10, 4]));  // #86
    ACharDefs.Add('W', CopyDotRows([17, 17, 17, 17, 21, 27, 17])); // #87
    ACharDefs.Add('X', CopyDotRows([17, 10, 4, 4, 4, 10, 17]));    // #88
    ACharDefs.Add('Y', CopyDotRows([17, 17, 17, 10, 4, 4, 4]));    // #89
    ACharDefs.Add('Z', CopyDotRows([31, 1, 2, 4, 8, 16, 31]));     // #90
    ACharDefs.Add('[', CopyDotRows([12, 8, 8, 8, 8, 8, 12]));      // #91
    ACharDefs.Add('\', CopyDotRows([0, 16, 8, 4, 2, 1, 0]));       // #92
    ACharDefs.Add(']', CopyDotRows([6, 2, 2, 2, 2, 2, 6]));        // #93
    ACharDefs.Add('^', CopyDotRows([4, 10, 17, 0, 0, 0, 0]));      // #94
    ACharDefs.Add('_', CopyDotRows([0, 0, 0, 0, 0, 0, 31]));       // #95
    ACharDefs.Add('`', CopyDotRows([6, 4, 2, 0, 0, 0, 0]));        // #96
    ACharDefs.Add('a', CopyDotRows([0, 0, 14, 1, 15, 17, 15]));    // #97
    ACharDefs.Add('b', CopyDotRows([16, 16, 30, 17, 17, 17, 30])); // #98
    ACharDefs.Add('c', CopyDotRows([0, 0, 15, 16, 16, 16, 15]));   // #99
    ACharDefs.Add('d', CopyDotRows([1, 1, 15, 17, 17, 17, 15]));  // #100
    ACharDefs.Add('e', CopyDotRows([0, 0, 14, 17, 31, 16, 14]));  // #101
    ACharDefs.Add('f', CopyDotRows([3, 4, 31, 4, 4, 4, 4]));      // #102
    ACharDefs.Add('g', CopyDotRows([0, 0, 15, 17, 15, 1, 14]));   // #103
    ACharDefs.Add('h', CopyDotRows([16, 16, 22, 25, 17, 17, 17]));// #104
    ACharDefs.Add('i', CopyDotRows([4, 0, 12, 4, 4, 4, 14]));     // #105
    ACharDefs.Add('j', CopyDotRows([2, 0, 6, 2, 2, 18, 12]));     // #106
    ACharDefs.Add('k', CopyDotRows([16, 16, 18, 20, 24, 20, 18]));// #107
    ACharDefs.Add('l', CopyDotRows([12, 4, 4, 4, 4, 4, 14]));     // #108
    ACharDefs.Add('m', CopyDotRows([0, 0, 26, 21, 21, 21, 21]));  // #109
    ACharDefs.Add('n', CopyDotRows([0, 0, 22, 25, 17, 17, 17]));  // #110
    ACharDefs.Add('o', CopyDotRows([0, 0, 14, 17, 17, 17, 14]));  // #111
    ACharDefs.Add('p', CopyDotRows([0, 0, 30, 17, 30, 16, 16]));  // #112
    ACharDefs.Add('q', CopyDotRows([0, 0, 15, 17, 15, 1, 1]));    // #113
    ACharDefs.Add('r', CopyDotRows([0, 0, 11, 12, 8, 8, 8]));     // #114
    ACharDefs.Add('s', CopyDotRows([0, 0, 14, 16, 14, 1, 30]));   // #115
    ACharDefs.Add('t', CopyDotRows([4, 4, 31, 4, 4, 4, 3]));      // #116
    ACharDefs.Add('u', CopyDotRows([0, 0, 17, 17, 17, 19, 13]));  // #117
    ACharDefs.Add('v', CopyDotRows([0, 0, 17, 17, 17, 10, 4]));   // #118
    ACharDefs.Add('w', CopyDotRows([0, 0, 17, 17, 21, 21, 10]));  // #119
    ACharDefs.Add('x', CopyDotRows([0, 0, 17, 10, 4, 10, 17]));   // #120
    ACharDefs.Add('y', CopyDotRows([0, 0, 17, 17, 15, 1, 14]));   // #121
    ACharDefs.Add('z', CopyDotRows([0, 0, 31, 2, 4, 8, 31]));     // #122
    ACharDefs.Add('{', CopyDotRows([3, 4, 4, 8, 4, 4, 3]));       // #123
    ACharDefs.Add('|', CopyDotRows([4, 4, 4, 4, 4, 4, 4]));       // #124
    ACharDefs.Add('}', CopyDotRows([24, 4, 4, 2, 4, 4, 24]));     // #125
    ACharDefs.Add('~', CopyDotRows([8, 21, 2, 0, 0, 0, 0]));      // #126
  end;
end;

{ Determines whether the character definitons must be stored in the lfm file.
  They are NOT stored when the currently used defs are exactly the same as those
  generated by IniTBCLeaCharDefs. }
function TBCLeaLCDDisplay.IsCharDefsStored: boolean;
var
  defs: TBCLeaCharDefs;
  i: integer;
  ch1, ch2: string;
  dotRows1: TDotRows;
begin
  Result := True;
  if (DotRowCount <> DEFAULT_DOT_ROW_COUNT) or (DotColCount <> DEFAULT_DOT_COL_COUNT) then
    exit;
  defs := TBCLeaCharDefs.Create(self);
  try
    defs.ColCount := DEFAULT_DOT_COL_COUNT;
    defs.RowCount := DEFAULT_DOT_ROW_COUNT;
    InitCharDefs(defs, defs.ColCount, defs.RowCount);
    if defs.Count <> FCharDefs.Count then
      exit;
    for i := 0 to defs.Count - 1 do
    begin
      ch1 := defs.CharByIndex[i];
      ch2 := FCharDefs.CharByIndex[i];
      ;
      if (ch1 <> ch2) then
        exit;
      dotRows1 := defs.DotRowsByIndex[i];
      if not FCharDefs.SameDotRows(ch1, dotRows1) then
        exit;
    end;
    Result := False;
  finally
    defs.Free;
  end;
end;

procedure TBCLeaLCDDisplay.SetBoardColor(const Value: TColor);
begin
  if Value = FBoardColor then
    Exit;
  FBoardColor := Value;
end;

procedure TBCLeaLCDDisplay.SetDotColorOn(const Value: TColor);
begin
  if Value = FDotColorOn then
    Exit;
  FDotColorOn := Value;
end;

procedure TBCLeaLCDDisplay.SetDotSize(const Value: integer);
begin
  if Value = DotSize then
    Exit;
  FDotSize := Value;
  UpdateSize;
end;

procedure TBCLeaLCDDisplay.SetDotsSpace(const Value: integer);
begin
  if Value = DotsSpace then
    Exit;
  FDotsSpace := Value;
  UpdateSize;
end;

procedure TBCLeaLCDDisplay.SetDotShape(const Value: TDotShape);
begin
  if Value = DotShape then
    Exit;
  FDotShape := Value;
  Invalidate;
end;

procedure TBCLeaLCDDisplay.SetFrameColor(const Value: TColor);
begin
  if Value = FrameColor then
    Exit;
  FFrameColor := Value;
  Invalidate;
end;

procedure TBCLeaLCDDisplay.SetFrameHeight(const Value: integer);
begin
  if Value = FrameHeight then
    Exit;
  FFrameHeight := Value;
  if FFrameSize < FFrameHeight then FFrameSize := FFrameHeight;
  UpdateSize;
end;

procedure TBCLeaLCDDisplay.SetFrameSize(const Value: integer);
begin
  if Value = FrameSize then
    Exit;
  FFrameSize := Value;
  if FFrameSize < FFrameHeight then FFrameHeight := FFrameSize;
  UpdateSize;
end;

//Added with conversion to BGRABitmap
procedure TBCLeaLCDDisplay.SetFrameAltitude(const Value: integer);
begin
  if Value = FrameAltitude then
    Exit;
  FFrameAltitude := Value;
  Invalidate;
end;

procedure TBCLeaLCDDisplay.SeTZStyle(const Value: TZStyle);
begin
  if Value = FrameStyle then
    Exit;
  FFrameStyle := Value;
  UpdateSize;
end;

procedure TBCLeaLCDDisplay.SetDisplayLineCount(const Value: integer);
begin
  if Value = FDisplayLineCount then
    Exit;
  FDisplayLineCount := Value;
  UpdateSize;
end;

procedure TBCLeaLCDDisplay.SetDisplayCharCount(const Value: integer);
begin
  if Value = FDisplayCharCount then
    Exit;
  FDisplayCharCount := Value;
  UpdateSize;
end;

procedure TBCLeaLCDDisplay.SetLines(const Value: TStrings);
var
  i: integer;
begin
  FLines.Clear;
  for i := 0 to FDisplayLineCount - 1 do
  begin
    if i < Value.Count then
      FLines.Add(Value[i])
    else
      FLines.Add(' ');
  end;
  LinesChanged(self);
end;

function TBCLeaLCDDisplay.GetCharCount: longint;
begin
  Prepare();
  Result := FCharCount;
end;

function TBCLeaLCDDisplay.GetGlobalDotColsCount: longint;
begin
  Prepare();
  Result := FGlobalDotColsCount;
end;

function TBCLeaLCDDisplay.GetDotColCount: integer;
begin
  Result := FCharDefs.ColCount;
end;

function TBCLeaLCDDisplay.GetDotRowCount: integer;
begin
  Result := FCharDefs.RowCount;
end;

procedure TBCLeaLCDDisplay.SetDotColCount(AValue: integer);
begin
  if AValue = DotColCount then
    exit;
  FCharDefs.ColCount := AValue;
  UpdateSize;
end;

procedure TBCLeaLCDDisplay.SetDotRowCount(AValue: integer);
begin
  if AValue = DotRowCount then
    exit;
  FCharDefs.RowCount := AValue;
  UpdateSize;
end;

procedure TBCLeaLCDDisplay.LinesChanged(Sender: TObject);
begin
  UpdateSize;
  DoChange;
end;

procedure TBCLeaLCDDisplay.UpdateSize;
begin
  if AutoSize then
  begin
    InvalidatePreferredSize;
    AdjustSize;
  end;
  Invalidate;
end;

procedure TBCLeaLCDDisplay.SetTheme(AValue: TBCLeaTheme);
begin
  if FTheme = AValue then
    Exit;
  if Assigned(FTheme) then
    FTheme := nil;
  FTheme := AValue;
  ApplyTheme;
end;

procedure TBCLeaLCDDisplay.UpdateTheme;
begin
  if Assigned(FTheme) then
  begin
    FTheme.LCD_FrameColor := FFrameColor;
    FTheme.LCD_BoardColor := FBoardColor;
    FTheme.LCD_DotColorOn := FDotColorOn;
    FTheme.LCD_DotShape := FDotShape;
    FTheme.LCD_DotSize := FDotSize;
    FTheme.LCD_DotsSpace := FDotsSpace;
    FTheme.LCD_DotBlend := FDotBlend;
    FTheme.LCD_DotBlendOperation := FDotBlendOperation;
    FTheme.LCD_DotBlur := FDotBlur;
    FTheme.LCD_DotBlurRadius := FDotBlurRadius;
    FTheme.LCD_FrameStyle := FrameStyle;
    FTheme.LCD_FrameHeight := FFrameHeight;
    FTheme.LCD_FrameAltitude := FFrameAltitude;
    FTheme.LCD_FrameSize := FFrameSize;
    FTheme.LCD_BoardShadow := FBoardShadow;
  end;
end;

procedure TBCLeaLCDDisplay.ApplyTheme;
begin
  if Assigned(FTheme) then
  begin
    FFrameColor := FTheme.LCD_FrameColor;
    FBoardColor := FTheme.LCD_BoardColor;
    FDotColorOn := FTheme.LCD_DotColorOn;
    FDotShape := FTheme.LCD_DotShape;
    FDotSize := FTheme.LCD_DotSize;
    FDotsSpace := FTheme.LCD_DotsSpace;
    FDotBlend := FTheme.LCD_DotBlend;
    FDotBlendOperation := FTheme.LCD_DotBlendOperation;
    FDotBlur := FTheme.LCD_DotBlur;
    FDotBlurRadius := FTheme.LCD_DotBlurRadius;
    FFrameStyle := FTheme.LCD_FrameStyle;
    FFrameHeight := FTheme.LCD_FrameHeight;
    FFrameAltitude := FTheme.LCD_FrameAltitude;
    FFrameSize := FTheme.LCD_FrameSize;
    FBoardShadow := FTheme.LCD_BoardShadow;
    FLightSourceIntensity := FTheme.COM_LightSourceIntensity;
    FLightSourceDistanceTerm := FTheme.COM_LightSourceDistanceTerm;
    FLightSourceDistanceFactor := FTheme.COM_LightSourceDistanceFactor;
    FLightDestFactor := FTheme.COM_LightDestFactor;
    FLightColor := FTheme.COM_LightColor;
    FSpecularFactor := FTheme.COM_SpecularFactor;
    FSpecularIndex := FTheme.COM_SpecularIndex;
    FAmbientFactor := FTheme.COM_AmbientFactor;
    FDiffusionFactor := FTheme.COM_DiffusionFactor;
    FNegativeDiffusionFactor := FTheme.COM_NegativeDiffusionFactor;
    FDiffuseSaturation := FTheme.COM_DiffuseSaturation;
    FLightPositionX := FTheme.COM_LightPositionX;
    FLightPositionY := FTheme.COM_LightPositionY;
    FLightPositionZ := FTheme.COM_LightPositionZ;
    UpdateSize;
    Invalidate;
  end
  else
  begin
    ApplyDefaultTheme;
  end;
end;

procedure TBCLeaLCDDisplay.SaveThemeToFile(AFileName: string);
begin
  if Assigned(FTheme) then
    FTheme.SaveThemeToFile(AFileName);
end;

procedure TBCLeaLCDDisplay.LoadThemeFromFile(AFileName: string);
begin
  if Assigned(FTheme) then
    FTheme.LoadThemeFromFile(AFileName);
end;

procedure TBCLeaLCDDisplay.ApplyDefaultTheme;
begin
  FDotSize := 4;
  FDotsSpace := 1;
  FDotShape := stRound;
  FDotBlend := False;
  FDotBlendOperation := boGlow;
  FDotBlur := False;
  FDotBlurRadius := 0.8;
  FFrameSize := 8;
  FFrameHeight := 8;
  FFrameAltitude := 2;
  FFrameStyle := zsRaised;
  FBoardShadow := bsFrame;
  FFrameColor := clBtnFace;
  FBoardColor := clBlack;
  FDotColorOn := clSkyBlue;
  FAmbientFactor := 0.3;
  FSpecularIndex := 10;
  FSpecularFactor := 0.6;
  FLightDestFactor := 1;
  FLightPositionX := -100;
  FLightPositionY := -100;
  FLightPositionZ := 100;
  FLightSourceIntensity := 500;
  FLightSourceDistanceTerm := 150;
  FLightSourceDistanceFactor := 1;
  FNegativeDiffusionFactor := 0.1;
  FLightColor := clWhite;
  FDiffuseSaturation := False;
  FDiffusionFactor := 0.9;
end;

end.
