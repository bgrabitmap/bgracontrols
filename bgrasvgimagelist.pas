unit BGRASVGImageList;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, FGL,
  XMLConf, BGRABitmap, BGRABitmapTypes, BGRASVG;

type

  TListOfTStringList = TFPGObjectList<TStringList>;
  TListOfTBGRASVG = TFPGObjectList<TBGRASVG>;

  { TBGRASVGImageList }

  TBGRASVGImageList = class(TComponent)
  private
    FHeight: integer;
    FHorizontalAlignment: TAlignment;
    FItems: TListOfTStringList;
    FSVGCache: TListOfTBGRASVG;
    FReferenceDPI: integer;
    FTargetRasterImageList: TImageList;
    FUseSVGAlignment: boolean;
    FVerticalAlignment: TTextLayout;
    FWidth: integer;
    FRasterized: boolean;
    FRasterizeQueued: boolean;
    FDataLineBreak: TTextLineBreakStyle;
    procedure ReadData(Stream: TStream);
    procedure SetHeight(AValue: integer);
    procedure SetTargetRasterImageList(AValue: TImageList);
    procedure SetWidth(AValue: integer);
    procedure WriteData(Stream: TStream);
    procedure CheckSVGIndex(AIndex: integer);
    function GetCachedSVG(AIndex: integer): TBGRASVG;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Load(const XMLConf: TXMLConfig);
    procedure Save(const XMLConf: TXMLConfig);
    procedure DefineProperties(Filer: TFiler); override;
    function GetCount: integer;
    // Get SVG string
    function GetSVGString(AIndex: integer): string; overload;
    procedure Rasterize; virtual;
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
  if Assigned(FTargetRasterImageList) then
  begin
    FTargetRasterImageList.RemoveFreeNotification(Self);
    FTargetRasterImageList.Clear;
  end;
  FTargetRasterImageList:=AValue;
  if Assigned(FTargetRasterImageList) then
    FTargetRasterImageList.FreeNotification(Self)
  else
  begin
    TThread.RemoveQueuedEvents(nil, RasterizeIfNeeded);
    FRasterizeQueued := false;
  end;
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
  finally
    FXMLConf.Free;
    FTempStream.Free;
  end;
end;

procedure TBGRASVGImageList.Load(const XMLConf: TXMLConfig);
var
  i, j, index: integer;
begin
  FSVGCache.Clear;
  FItems.Clear;
  j := XMLConf.GetValue('Count', 0);
  for i := 0 to j - 1 do
  begin
    index := FItems.Add(TStringList.Create);
    FSVGCache.Add(nil);
    FItems[index].Text := XMLConf.GetValue('Item' + i.ToString + '/SVG', '');
  end;
  QueryRasterize;
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
  FSVGCache := TListOfTBGRASVG.Create(True);
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
  TThread.RemoveQueuedEvents(nil, RasterizeIfNeeded);
  if Assigned(FTargetRasterImageList) then
    FTargetRasterImageList.RemoveFreeNotification(Self);
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
  FSVGCache.Add(nil);
  QueryRasterize;
end;

procedure TBGRASVGImageList.Remove(AIndex: integer);
begin
  CheckSVGIndex(AIndex);
  FSVGCache.Delete(AIndex);
  FItems.Delete(AIndex);
  QueryRasterize;
end;

procedure TBGRASVGImageList.Exchange(AIndex1, AIndex2: integer);
begin
  CheckSVGIndex(AIndex1);
  CheckSVGIndex(AIndex2);
  FItems.Exchange(AIndex1, AIndex2);
  FSVGCache.Exchange(AIndex1, AIndex2);
  QueryRasterize;
end;

function TBGRASVGImageList.GetSVGString(AIndex: integer): string;
begin
  CheckSVGIndex(AIndex);
  Result := FItems[AIndex].Text;
end;

procedure TBGRASVGImageList.Rasterize;
begin
  if Assigned(FTargetRasterImageList) then
  begin
    FTargetRasterImageList.BeginUpdate;
    try
      FTargetRasterImageList.Clear;
      FTargetRasterImageList.Width := Width;
      FTargetRasterImageList.Height := Height;
      {$IFDEF DARWIN}
      PopulateImageList(FTargetRasterImageList, [Width, Width*2]);
      {$ELSE}
      PopulateImageList(FTargetRasterImageList, [Width]);
      {$ENDIF}
    finally
      FTargetRasterImageList.EndUpdate;
    end;
  end;
end;

procedure TBGRASVGImageList.RasterizeIfNeeded;
begin
  FRasterizeQueued := false;
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
  if not Assigned(FTargetRasterImageList) or FRasterizeQueued or
     (csLoading in ComponentState) or (csDestroying in ComponentState) then Exit;
  FRasterizeQueued := true;
  method := RasterizeIfNeeded;
  TThread.ForceQueue(nil, method);
end;

procedure TBGRASVGImageList.Loaded;
begin
  inherited Loaded;
  QueryRasterize;
end;

procedure TBGRASVGImageList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FTargetRasterImageList) then
  begin
    FTargetRasterImageList := nil;
    TThread.RemoveQueuedEvents(nil, RasterizeIfNeeded);
    FRasterizeQueued := false;
    FRasterized := false;
  end;
end;

procedure TBGRASVGImageList.CheckSVGIndex(AIndex: integer);
begin
  if (AIndex < 0) or (AIndex >= FItems.Count) then
    raise ERangeError.CreateFmt('TBGRASVGImageList: index %d out of range (Count = %d)',
      [AIndex, FItems.Count]);
end;

function TBGRASVGImageList.GetCachedSVG(AIndex: integer): TBGRASVG;
begin
  CheckSVGIndex(AIndex);
  Result := FSVGCache[AIndex];
  if Result = nil then
  begin
    Result := TBGRASVG.CreateFromString(FItems[AIndex].Text);
    FSVGCache[AIndex] := Result;
  end;
end;

procedure TBGRASVGImageList.Replace(AIndex: integer; ASVG: string);
begin
  CheckSVGIndex(AIndex);
  FItems[AIndex].Text := ASVG;
  // The owning list frees the old SVG when replacing its entry.
  FSVGCache[AIndex] := nil;
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
  svg: TBGRASVG;
begin
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
  stream: TMemoryStream;
begin
  bmp := GetBGRABitmap(AIndex, AWidth, AHeight, AUseSVGAlignment);
  try
    stream := TMemoryStream.Create;
    try
      // Keep the stream conversion: Assign does not copy lazy native bitmaps
      // correctly on every widgetset.
      bmp.Bitmap.SaveToStream(stream);
      stream.Position := 0;
      Result := TBitmap.Create;
      try
        Result.LoadFromStream(stream);
      except
        Result.Free;
        raise;
      end;
    finally
      stream.Free;
    end;
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

procedure TBGRASVGImageList.PopulateImageList(const AImageList: TImageList;
  AWidths: array of integer);
var
  i, j: integer;
  arr: array of TCustomBitmap;
begin
  if Length(AWidths) = 0 then Exit;
  if (Width <= 0) or (Height <= 0) then
    raise EArgumentException.Create('SVG image dimensions must be positive');
  for i := 0 to High(AWidths) do
    if AWidths[i] <= 0 then
      raise EArgumentException.Create('Image list resolution widths must be positive');
  AImageList.BeginUpdate;
  try
    AImageList.Width := AWidths[0];
    AImageList.Height := MulDiv(AWidths[0], Height, Width);
    AImageList.Scaled := True;
    AImageList.RegisterResolutions(AWidths);
    SetLength({%H-}arr, Length(AWidths));
    for j := 0 to Count - 1 do
    begin
      try
        for i := 0 to Length(arr) - 1 do
          arr[i] := GetBitmap(j, AWidths[i], MulDiv(AWidths[i], Height, Width), True);
        AImageList.AddMultipleResolutions(arr);
      finally
        for i := 0 to Length(arr) - 1 do
          FreeAndNil(arr[i]);
      end;
    end;
  finally
    AImageList.EndUpdate;
  end;
end;

end.
