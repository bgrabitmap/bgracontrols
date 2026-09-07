unit BGRASVGImageList;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, ImgList, Dialogs, FGL,
  XMLConf, BGRABitmap, BGRABitmapTypes, BGRASVG;

type
  TListOfTStringList = TFPGObjectList<TStringList>;
  TListOfTBGRASVG = TFPGObjectList<TBGRASVG>;
  TIntegerDynArray = array of integer;

  { TBGRASVGImageList }

  // Inherit directly from TImageList so standard LCL controls can consume it natively
  TBGRASVGImageList = class(TImageList)
  private
    FItems: TListOfTStringList;
    FSVGCache: TListOfTBGRASVG; // Parsed TBGRASVG cache, index-aligned with FItems
    FReferenceDPI: integer;
    FUseSVGAlignment: boolean;
    FHorizontalAlignment: TAlignment;
    FVerticalAlignment: TTextLayout;
    FRasterized: boolean;
    FRasterizing: boolean; // Flag to prevent infinite recursive loops
    FDataLineBreak: TTextLineBreakStyle;
    procedure ReadSVGData(Stream: TStream);
    procedure WriteSVGData(Stream: TStream);
    procedure CheckIndex(AIndex: integer);
    procedure ClearSVGCache;
    procedure InvalidateSVGCache(AIndex: integer);
    function GetCachedSVG(AIndex: integer): TBGRASVG;
    function DedupSortedWidths(const AWidths: array of integer): TIntegerDynArray;
  protected
    procedure Load(const XMLConf: TXMLConfig);
    procedure Save(const XMLConf: TXMLConfig);
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    function GetSVGCount: integer;
    // Get SVG string
    function GetSVGString(AIndex: integer): string; overload;
    procedure Rasterize;
    procedure RasterizeIfNeeded;
    procedure QueryRasterize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(ASVG: string): integer;
    procedure Remove(AIndex: integer);
    procedure Exchange(AIndex1, AIndex2: integer);
    procedure Replace(AIndex: integer; ASVG: string);
    function GetScaledSize(ATargetDPI: integer): TSize;
    // Get TBGRABitmap with custom width and height
    function GetBGRABitmap(AIndex: integer; AWidth, AHeight: integer): TBGRABitmap; overload;
    function GetBGRABitmap(AIndex: integer; AWidth, AHeight: integer;
      AUseSVGAlignment: boolean): TBGRABitmap; overload;
    // Get TBitmap with custom width and height
    function GetBitmap(AIndex: integer; AWidth, AHeight: integer): TBitmap; overload;
    function GetBitmap(AIndex: integer; AWidth, AHeight: integer;
      AUseSVGAlignment: boolean): TBitmap; overload;
    // Draw image with custom width and height. The Width and
    // Height property are in LCL coordinates.
    procedure Draw(AIndex: integer; AControl: TControl; ACanvas: TCanvas;
      ALeft, ATop, AWidth, AHeight: integer); overload;
    procedure Draw(AIndex: integer; AControl: TControl; ACanvas: TCanvas;
      ALeft, ATop, AWidth, AHeight: integer; AUseSVGAlignment: boolean;
      AOpacity: byte = 255); overload;
    // Draw image with custom width, height and canvas scale. The Width and
    // Height property are in LCL coordinates. CanvasScale is useful on MacOS
    // where LCL coordinates do not match actual pixels.
    procedure Draw(AIndex: integer; ACanvasScale: single; ACanvas: TCanvas;
      ALeft, ATop, AWidth, AHeight: integer); overload;
    procedure Draw(AIndex: integer; ACanvasScale: single; ACanvas: TCanvas;
      ALeft, ATop, AWidth, AHeight: integer; AUseSVGAlignment: boolean;
      AOpacity: byte = 255); overload;
    // Draw on the target BGRABitmap with specified Width and Height.
    procedure Draw(AIndex: integer; ABitmap: TBGRABitmap; const ARectF: TRectF); overload;
    procedure Draw(AIndex: integer; ABitmap: TBGRABitmap; const ARectF: TRectF;
      AUseSVGAlignment: boolean); overload;

    // Populates self with standard raster sizes for DPI scaling
    procedure PopulateImageList(AWidths: array of integer);

    property SVGString[AIndex: integer]: string read GetSVGString;
    property SVGCount: integer read GetSVGCount; // Renamed to avoid hiding standard raster Count
  published
    // Note: Width and Height are already natively published by TImageList
    property ReferenceDPI: integer read FReferenceDPI write FReferenceDPI default 96;
    property UseSVGAlignment: boolean read FUseSVGAlignment write FUseSVGAlignment default False;
    property HorizontalAlignment: TAlignment read FHorizontalAlignment write FHorizontalAlignment default taCenter;
    property VerticalAlignment: TTextLayout read FVerticalAlignment write FVerticalAlignment default tlCenter;
  end;

procedure Register;

implementation

uses LCLType, XMLRead;

procedure Register;
begin
  RegisterComponents('BGRA Themes', [TBGRASVGImageList]);
end;

{$IF FPC_FULLVERSION < 30203}
type

  { TPatchedXMLConfig }

  TPatchedXMLConfig = class(TXMLConfig)
    public
      procedure LoadFromStream(S : TStream); reintroduce;
  end;


{ TPatchedXMLConfig }

procedure TPatchedXMLConfig.LoadFromStream(S: TStream);
begin
  FreeAndNil(Doc);
  ReadXMLFile(Doc,S);
  FModified := False;
  if (Doc.DocumentElement.NodeName<>RootName) then
    raise EXMLConfigError.CreateFmt(SWrongRootName,[RootName,Doc.DocumentElement.NodeName]);
end;
{$ENDIF}

{ TBGRASVGImageList }

procedure TBGRASVGImageList.ReadSVGData(Stream: TStream);
  // Detects EOL marker used in the text stream
  function GetLineEnding(AStream: TStream; AMaxLookAhead: integer = 4096): TTextLineBreakStyle;
  var c: char;
    i: integer;
  begin
    c := #0;
    for i := 0 to AMaxLookAhead-1 do
    begin
      if AStream.Read(c, sizeof(c)) = 0 then break;
      Case c of
      #10: exit(tlbsLF);
      #13: begin
          if AStream.Read(c, sizeof(c)) = 0 then c := #0;
          if c = #10 then
            exit(tlbsCRLF)
          else
            exit(tlbsCR);
        end;
      end;
    end;
    // no marker found, return system default
    exit(DefaultTextLineBreakStyle);
  end;

var
  FXMLConf: TXMLConfig;
begin
  FXMLConf := TXMLConfig.Create(Self);
  try
    // Detect the line EOL marker
    Stream.Position := 0;
    FDataLineBreak:= GetLineEnding(Stream);
    // Actually load the XML file
    Stream.Position := 0;
    {$IF FPC_FULLVERSION < 30203}TPatchedXMLConfig(FXMLConf){$ELSE}FXMLConf{$ENDIF}.LoadFromStream(Stream);
    Load(FXMLConf);
  finally
    FXMLConf.Free;
  end;
end;

procedure TBGRASVGImageList.WriteSVGData(Stream: TStream);
var
  FXMLConf: TXMLConfig;
  FTempStream: TStringStream;
  FNormalizedData: string;
begin
  FXMLConf := TXMLConfig.Create(Self);
  FTempStream := TStringStream.Create;
  try
    Save(FXMLConf);
    // Save to temporary string stream.
    // EOL marker will depend on OS (#13#10 or #10),
    // because TXMLConfig automatically changes EOL to platform default.
    FXMLConf.SaveToStream(FTempStream);
    // Normalize EOL marker, as data will be saved as binary data.
    // Saving without normalization would lead to different binary
    // data when saving on different platforms.
    FNormalizedData := AdjustLineBreaks(FTempStream.DataString, FDataLineBreak);
    if FNormalizedData <> '' then
      Stream.WriteBuffer(FNormalizedData[1], Length(FNormalizedData));
  finally
    FXMLConf.Free;
    FTempStream.Free;
  end;
end;

procedure TBGRASVGImageList.Load(const XMLConf: TXMLConfig);
var
  i, j, index: integer;
begin
  try
    FItems.Clear;
    ClearSVGCache;
    j := XMLConf.GetValue('Count', 0);
    for i := 0 to j - 1 do
    begin
      index := FItems.Add(TStringList.Create);
      FItems[index].Text := XMLConf.GetValue('Item' + i.ToString + '/SVG', '');
      FSVGCache.Add(nil); // Parsed lazily on first use
    end;
  finally
  end;
end;

procedure TBGRASVGImageList.Save(const XMLConf: TXMLConfig);
var
  i: integer;
begin
  try
    XMLConf.SetValue('Count', FItems.Count);
    for i := 0 to FItems.Count - 1 do
      XMLConf.SetValue('Item' + i.ToString + '/SVG', AdjustLineBreaks(FItems[i].Text, FDataLineBreak));
  finally
  end;
end;

procedure TBGRASVGImageList.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Items', ReadSVGData, WriteSVGData, True);
end;

procedure TBGRASVGImageList.Loaded;
begin
  inherited Loaded;
  QueryRasterize; // Guarantee items load properly at runtime when created from LFM
end;

constructor TBGRASVGImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TListOfTStringList.Create(True);
  FSVGCache := TListOfTBGRASVG.Create(True);
  FReferenceDPI := 96;
  FUseSVGAlignment:= false;
  FHorizontalAlignment := taCenter;
  FVerticalAlignment := tlCenter;
  FDataLineBreak := DefaultTextLineBreakStyle;
end;

destructor TBGRASVGImageList.Destroy;
begin
  FSVGCache.Free;
  FItems.Free;
  inherited Destroy;
end;

function TBGRASVGImageList.Add(ASVG: string): integer;
var
  list: TStringList;
begin
  list := TStringList.Create;
  list.Text := ASVG;
  Result := FItems.Add(list);
  FSVGCache.Add(nil); // Parsed lazily on first use
  QueryRasterize;
end;

procedure TBGRASVGImageList.Remove(AIndex: integer);
begin
  CheckIndex(AIndex);
  FItems.Delete(AIndex);
  FSVGCache.Delete(AIndex);
  QueryRasterize;
end;

procedure TBGRASVGImageList.Exchange(AIndex1, AIndex2: integer);
begin
  CheckIndex(AIndex1);
  CheckIndex(AIndex2);
  FItems.Exchange(AIndex1, AIndex2);
  FSVGCache.Exchange(AIndex1, AIndex2);
  QueryRasterize;
end;

procedure TBGRASVGImageList.CheckIndex(AIndex: integer);
begin
  if (AIndex < 0) or (AIndex >= FItems.Count) then
    raise ERangeError.CreateFmt('TBGRASVGImageList: index %d out of range (SVGCount = %d)',
      [AIndex, FItems.Count]);
end;

procedure TBGRASVGImageList.ClearSVGCache;
begin
  FSVGCache.Clear; // FSVGCache owns its objects, so this frees any parsed SVGs
end;

procedure TBGRASVGImageList.InvalidateSVGCache(AIndex: integer);
begin
  FSVGCache[AIndex].Free;
  FSVGCache[AIndex] := nil; // Re-parsed lazily next time it's needed
end;

function TBGRASVGImageList.GetCachedSVG(AIndex: integer): TBGRASVG;
begin
  CheckIndex(AIndex);
  Result := FSVGCache[AIndex];
  if Result = nil then
  begin
    Result := TBGRASVG.CreateFromString(FItems[AIndex].Text);
    FSVGCache[AIndex] := Result;
  end;
end;

function TBGRASVGImageList.DedupSortedWidths(const AWidths: array of integer): TIntegerDynArray;
var
  i, j, n, v: integer;
begin
  SetLength(Result, Length(AWidths));
  n := 0;
  for i := 0 to High(AWidths) do
  begin
    if AWidths[i] <= 0 then Continue; // ignore invalid widths defensively
    j := 0;
    while (j < n) and (Result[j] <> AWidths[i]) do Inc(j);
    if j = n then // not already present
    begin
      Result[n] := AWidths[i];
      Inc(n);
    end;
  end;
  SetLength(Result, n);
  // Small arrays (typically <= 4 entries), so a simple insertion sort is fine
  for i := 1 to n - 1 do
  begin
    v := Result[i];
    j := i - 1;
    while (j >= 0) and (Result[j] > v) do
    begin
      Result[j + 1] := Result[j];
      Dec(j);
    end;
    Result[j + 1] := v;
  end;
end;

function TBGRASVGImageList.GetSVGString(AIndex: integer): string;
begin
  CheckIndex(AIndex);
  Result := FItems[AIndex].Text;
end;

procedure TBGRASVGImageList.Rasterize;
var
  BaseWidth: Integer;
begin
  // Safety guard against endless recursive loops triggered by properties changing
  if FRasterizing or (csLoading in ComponentState) then Exit;
  FRasterizing := True;
  try
    BaseWidth := Width;
    if BaseWidth <= 0 then BaseWidth := 16; // default fail-safe

    {$IFDEF DARWIN}
    PopulateImageList([BaseWidth, BaseWidth*2]);
    {$ELSE}
    // Automatically generate 100%, 125%, 150%, and 200% scales for standard High-DPI support,
    // relative to ReferenceDPI rather than a hardcoded 96
    PopulateImageList([BaseWidth, MulDiv(BaseWidth, 120, FReferenceDPI),
      MulDiv(BaseWidth, 144, FReferenceDPI), BaseWidth*2]);
    {$ENDIF}
  finally
    FRasterizing := False;
  end;
end;

procedure TBGRASVGImageList.RasterizeIfNeeded;
begin
  if not FRasterized then
  begin
    Rasterize;
    FRasterized := true;
  end;
end;

procedure TBGRASVGImageList.QueryRasterize;
var method: TThreadMethod;
begin
  if csLoading in ComponentState then Exit;
  FRasterized := false;

  // If designing in IDE, execute immediately so previews work. Otherwise, queue it safely.
  if csDesigning in ComponentState then
    RasterizeIfNeeded
  else
  begin
    method := RasterizeIfNeeded;
    TThread.ForceQueue(nil, method);
  end;
end;

procedure TBGRASVGImageList.Replace(AIndex: integer; ASVG: string);
begin
  CheckIndex(AIndex);
  FItems[AIndex].Text := ASVG;
  InvalidateSVGCache(AIndex);
  QueryRasterize;
end;

function TBGRASVGImageList.GetSVGCount: integer;
begin
  Result := FItems.Count;
end;

function TBGRASVGImageList.GetScaledSize(ATargetDPI: integer): TSize;
begin
  result.cx := MulDiv(Width, ATargetDPI, ReferenceDPI);
  result.cy := MulDiv(Height, ATargetDPI, ReferenceDPI);
end;

function TBGRASVGImageList.GetBGRABitmap(AIndex: integer; AWidth,
  AHeight: integer): TBGRABitmap;
begin
  result := GetBGRABitmap(AIndex, AWidth, AHeight, UseSVGAlignment);
end;

function TBGRASVGImageList.GetBGRABitmap(AIndex: integer; AWidth, AHeight: integer;
  AUseSVGAlignment: boolean): TBGRABitmap;
var
  svg: TBGRASVG;
begin
  // CheckIndex/parsing happens here, before the bitmap is allocated, so a bad
  // index or malformed SVG can never leak an orphaned TBGRABitmap.
  svg := GetCachedSVG(AIndex);
  Result := TBGRABitmap.Create(AWidth, AHeight);
  try
    svg.StretchDraw(Result.Canvas2D, 0, 0, AWidth, AHeight, AUseSVGAlignment);
  except
    Result.Free;
    raise;
  end;
end;

function TBGRASVGImageList.GetBitmap(AIndex: integer; AWidth, AHeight: integer): TBitmap;
begin
  result := GetBitmap(AIndex, AWidth, AHeight, UseSVGAlignment);
end;

function TBGRASVGImageList.GetBitmap(AIndex: integer; AWidth, AHeight: integer;
  AUseSVGAlignment: boolean): TBitmap;
var
  bmp: TBGRABitmap;
begin
  bmp := GetBGRABitmap(AIndex, AWidth, AHeight, AUseSVGAlignment);
  try
    Result := TBitmap.Create;
    Result.Assign(bmp.Bitmap);
  finally
    bmp.Free;
  end;
end;

procedure TBGRASVGImageList.Draw(AIndex: integer; AControl: TControl;
  ACanvas: TCanvas; ALeft, ATop, AWidth, AHeight: integer);
begin
  Draw(AIndex, AControl, ACanvas, ALeft, ATop, AWidth, AHeight, UseSVGAlignment);
end;

procedure TBGRASVGImageList.Draw(AIndex: integer; AControl: TControl; ACanvas: TCanvas;
  ALeft, ATop, AWidth, AHeight: integer; AUseSVGAlignment: boolean; AOpacity: byte);
begin
  Draw(AIndex, AControl.GetCanvasScaleFactor, ACanvas, ALeft, ATop, AWidth, AHeight,
       AUseSVGAlignment, AOpacity);
end;

procedure TBGRASVGImageList.Draw(AIndex: integer; ACanvasScale: single;
  ACanvas: TCanvas; ALeft, ATop, AWidth, AHeight: integer);
begin
  Draw(AIndex, ACanvasScale, ACanvas, ALeft, ATop, AWidth, AHeight, UseSVGAlignment);
end;

procedure TBGRASVGImageList.Draw(AIndex: integer; ACanvasScale: single; ACanvas: TCanvas;
  ALeft, ATop, AWidth, AHeight: integer; AUseSVGAlignment: boolean; AOpacity: byte);
var
  bmp: TBGRABitmap;
begin
  if (AWidth = 0) or (AHeight = 0) or (ACanvasScale = 0) then
    Exit;
  bmp := TBGRABitmap.Create(round(AWidth * ACanvasScale), round(AHeight * ACanvasScale));
  try
    Draw(AIndex, bmp, rectF(0, 0, bmp.Width, bmp.Height), AUseSVGAlignment);
    bmp.ApplyGlobalOpacity(AOpacity);
    bmp.Draw(ACanvas, RectWithSize(ALeft, ATop, AWidth, AHeight), False);
  finally
    bmp.Free;
  end;
end;

procedure TBGRASVGImageList.Draw(AIndex: integer; ABitmap: TBGRABitmap; const ARectF: TRectF);
begin
  Draw(AIndex, ABitmap, ARectF, UseSVGAlignment);
end;

procedure TBGRASVGImageList.Draw(AIndex: integer; ABitmap: TBGRABitmap; const ARectF: TRectF;
  AUseSVGAlignment: boolean);
var
  svg: TBGRASVG;
begin
  svg := GetCachedSVG(AIndex);
  if AUseSVGAlignment then
    svg.StretchDraw(ABitmap.Canvas2D, ARectF, true)
  else
    svg.StretchDraw(ABitmap.Canvas2D, HorizontalAlignment, VerticalAlignment,
      ARectF.Left, ARectF.Top, ARectF.Width, ARectF.Height);
end;

procedure TBGRASVGImageList.PopulateImageList(AWidths: array of integer);
var
  i, j: integer;
  arr: array of TCustomBitmap;
  BaseWidth, BaseHeight: Integer;
  widths: TIntegerDynArray;
begin
  // Dedup + sort defensively: duplicate widths (e.g. from rounding at small
  // sizes, or a caller-supplied list) can make RegisterResolutions fail.
  widths := DedupSortedWidths(AWidths);
  if Length(widths) = 0 then Exit;

  BaseWidth := Width;
  BaseHeight := Height;
  if BaseWidth <= 0 then BaseWidth := 16;
  if BaseHeight <= 0 then BaseHeight := 16;

  // BeginUpdate disables layout recalculations during population
  Self.BeginUpdate;
  try
    Self.Clear;
    Self.Width := widths[0];
    Self.Height := MulDiv(widths[0], BaseHeight, BaseWidth);
    Self.Scaled := True;
    Self.RegisterResolutions(widths);

    SetLength(arr, Length(widths));
    for j := 0 to SVGCount - 1 do // Using our renamed SVGCount here
    begin
      for i := 0 to Length(arr) - 1 do
        arr[i] := GetBitmap(j, widths[i], MulDiv(widths[i], BaseHeight, BaseWidth), True);

      Self.AddMultipleResolutions(arr);

      for i := 0 to Length(arr) - 1 do
        TBitmap(arr[i]).Free;
    end;
  finally
    Self.EndUpdate;
  end;
end;

end.
