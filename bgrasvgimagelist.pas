unit BGRASVGImageList;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, FGL,
  XMLConf, BGRABitmap, BGRABitmapTypes, BGRASVG;

type

  TListOfTStringList = TFPGObjectList<TStringList>;

  { TBGRASVGImageList }

  TBGRASVGImageList = class(TComponent)
  private
    FHeight: integer;
    FHorizontalAlignment: TAlignment;
    FItems: TListOfTStringList;
    FReferenceDPI: integer;
    FTargetRasterImageList: TImageList;
    FUseSVGAlignment: boolean;
    FVerticalAlignment: TTextLayout;
    FWidth: integer;
    FRasterized: boolean;
    FDataLineBreak: TTextLineBreakStyle;
    procedure ReadData(Stream: TStream);
    procedure SetHeight(AValue: integer);
    procedure SetTargetRasterImageList(AValue: TImageList);
    procedure SetWidth(AValue: integer);
    procedure WriteData(Stream: TStream);
  protected
    procedure Load(const XMLConf: TXMLConfig);
    procedure Save(const XMLConf: TXMLConfig);
    procedure DefineProperties(Filer: TFiler); override;
    function GetCount: integer;
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

    // Generate bitmaps for an image list
    procedure PopulateImageList(const AImageList: TImageList; AWidths: array of integer);
    property SVGString[AIndex: integer]: string read GetSVGString;
    property Count: integer read GetCount;
  published
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property ReferenceDPI: integer read FReferenceDPI write FReferenceDPI default 96;
    property UseSVGAlignment: boolean read FUseSVGAlignment write FUseSVGAlignment default False;
    property HorizontalAlignment: TAlignment read FHorizontalAlignment write FHorizontalAlignment default taCenter;
    property VerticalAlignment: TTextLayout read FVerticalAlignment write FVerticalAlignment default tlCenter;
    property TargetRasterImageList: TImageList read FTargetRasterImageList write SetTargetRasterImageList default nil;
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

procedure TBGRASVGImageList.ReadData(Stream: TStream);

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

procedure TBGRASVGImageList.SetHeight(AValue: integer);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;
  QueryRasterize;
end;

procedure TBGRASVGImageList.SetTargetRasterImageList(AValue: TImageList);
begin
  if FTargetRasterImageList=AValue then Exit;
  if Assigned(FTargetRasterImageList) then FTargetRasterImageList.Clear;
  FTargetRasterImageList:=AValue;
  QueryRasterize;
end;

procedure TBGRASVGImageList.SetWidth(AValue: integer);
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;
  QueryRasterize;
end;

procedure TBGRASVGImageList.WriteData(Stream: TStream);
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
    FXMLConf.Flush;
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
    j := XMLConf.GetValue('Count', 0);
    for i := 0 to j - 1 do
    begin
      index := FItems.Add(TStringList.Create);
      FItems[index].Text := XMLConf.GetValue('Item' + i.ToString + '/SVG', '');
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
  Filer.DefineBinaryProperty('Items', ReadData, WriteData, True);
end;

constructor TBGRASVGImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TListOfTStringList.Create(True);
  FWidth := 16;
  FHeight := 16;
  FReferenceDPI := 96;
  FUseSVGAlignment:= false;
  FHorizontalAlignment := taCenter;
  FVerticalAlignment := tlCenter;
  FDataLineBreak := DefaultTextLineBreakStyle;
end;

destructor TBGRASVGImageList.Destroy;
begin
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
  QueryRasterize;
end;

procedure TBGRASVGImageList.Remove(AIndex: integer);
begin
  FItems.Remove(FItems[AIndex]);
  QueryRasterize;
end;

procedure TBGRASVGImageList.Exchange(AIndex1, AIndex2: integer);
begin
  FItems.Exchange(AIndex1, AIndex2);
  QueryRasterize;
end;

function TBGRASVGImageList.GetSVGString(AIndex: integer): string;
begin
  Result := FItems[AIndex].Text;
end;

procedure TBGRASVGImageList.Rasterize;
begin
  if Assigned(FTargetRasterImageList) then
  begin
    FTargetRasterImageList.Clear;
    FTargetRasterImageList.Width := Width;
    FTargetRasterImageList.Height := Height;
    {$IFDEF DARWIN}
    PopulateImageList(FTargetRasterImageList, [Width, Width*2]);
    {$ELSE}
    PopulateImageList(FTargetRasterImageList, [Width]);
    {$ENDIF}
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
  FRasterized := false;
  method := RasterizeIfNeeded;
  TThread.ForceQueue(nil, method);
end;

procedure TBGRASVGImageList.Replace(AIndex: integer; ASVG: string);
begin
  FItems[AIndex].Text := ASVG;
  QueryRasterize;
end;

function TBGRASVGImageList.GetCount: integer;
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
  bmp: TBGRABitmap;
  svg: TBGRASVG;
begin
  bmp := TBGRABitmap.Create(AWidth, AHeight);
  svg := TBGRASVG.CreateFromString(FItems[AIndex].Text);
  try
    svg.StretchDraw(bmp.Canvas2D, 0, 0, AWidth, AHeight, AUseSVGAlignment);
  finally
    svg.Free;
  end;
  Result := bmp;
end;

function TBGRASVGImageList.GetBitmap(AIndex: integer; AWidth, AHeight: integer): TBitmap;
begin
  result := GetBitmap(AIndex, AWidth, AHeight, UseSVGAlignment);
end;

function TBGRASVGImageList.GetBitmap(AIndex: integer; AWidth, AHeight: integer;
  AUseSVGAlignment: boolean): TBitmap;
var
  bmp: TBGRABitmap;
  ms: TMemoryStream;
begin
  bmp := GetBGRABitmap(AIndex, AWidth, AHeight, AUseSVGAlignment);
  ms := TMemoryStream.Create;
  bmp.Bitmap.SaveToStream(ms);
  bmp.Free;
  Result := TBitmap.Create;
  ms.Position := 0;
  Result.LoadFromStream(ms);
  ms.Free;
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
  svg := TBGRASVG.CreateFromString(FItems[AIndex].Text);
  try
    if AUseSVGAlignment then
      svg.StretchDraw(ABitmap.Canvas2D, ARectF, true)
      else svg.StretchDraw(ABitmap.Canvas2D, HorizontalAlignment, VerticalAlignment, ARectF.Left, ARectF.Top, ARectF.Width, ARectF.Height);
  finally
    svg.Free;
  end;
end;

procedure TBGRASVGImageList.PopulateImageList(const AImageList: TImageList;
  AWidths: array of integer);
var
  i, j: integer;
  arr: array of TCustomBitmap;
begin
  AImageList.Width := AWidths[0];
  AImageList.Height := MulDiv(AWidths[0], Height, Width);
  AImageList.Scaled := True;
  AImageList.RegisterResolutions(AWidths);
  SetLength({%H-}arr, Length(AWidths));
  for j := 0 to Count - 1 do
  begin
    for i := 0 to Length(arr) - 1 do
      arr[i] := GetBitmap(j, AWidths[i], MulDiv(AWidths[i], Height, Width), True);
    AImageList.AddMultipleResolutions(arr);
    for i := 0 to Length(arr) - 1 do
      TBitmap(Arr[i]).Free;
  end;
end;

end.
