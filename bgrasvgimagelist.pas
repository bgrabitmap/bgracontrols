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
    FUseSVGAlignment: boolean;
    FVerticalAlignment: TTextLayout;
    FWidth: integer;
    procedure ReadData(Stream: TStream);
    procedure SetHeight(AValue: integer);
    procedure SetWidth(AValue: integer);
    procedure WriteData(Stream: TStream);
  protected
    procedure Load(const XMLConf: TXMLConfig);
    procedure Save(const XMLConf: TXMLConfig);
    procedure DefineProperties(Filer: TFiler); override;
    function GetCount: integer;
    // Get SVG string
    function GetSVGString(AIndex: integer): string; overload;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(ASVG: string): integer;
    procedure Remove(AIndex: integer);
    procedure Exchange(AIndex1, AIndex2: integer);
    procedure Replace(AIndex: integer; ASVG: string);
    function GetSize(ATargetDPI: integer): TSize;
    // Get TBGRABitmap with custom width and height
    function GetBGRABitmap(AIndex: integer; AWidth, AHeight: integer): TBGRABitmap; overload;
    function GetBGRABitmap(AIndex: integer; AWidth, AHeight: integer;
      AUseSVGAlignment: boolean): TBGRABitmap; overload;
    // Get TBitmap with custom width and height
    function GetBitmap(AIndex: integer; AWidth, AHeight: integer): TBitmap; overload;
    function GetBitmap(AIndex: integer; AWidth, AHeight: integer;
      AUseSVGAlignment: boolean): TBitmap; overload;
    // Draw image with default width and height scaled to the DPI of the control.
    procedure Draw(AIndex: integer; AControl: TControl; ACanvas: TCanvas; ALeft, ATop: integer); overload;
    procedure Draw(AIndex: integer; AControl: TControl; ACanvas: TCanvas; ALeft, ATop: integer;
      AUseSVGAlignment: boolean; AOpacity: byte = 255); overload;
    // Draw image with default width and height scaled to a custom DPI in LCL coordinates.
    procedure Draw(AIndex: integer; AControl: TControl; ATargetDPI: integer;
      ACanvas: TCanvas; ALeft, ATop: integer); overload;
    procedure Draw(AIndex: integer; AControl: TControl; ATargetDPI: integer; ACanvas: TCanvas;
      ALeft, ATop: integer; AUseSVGAlignment: boolean; AOpacity: byte = 255); overload;
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
  end;

procedure Register;

implementation

uses LCLType;

procedure Register;
begin
  RegisterComponents('BGRA Themes', [TBGRASVGImageList]);
end;

{ TBGRASVGImageList }

procedure TBGRASVGImageList.ReadData(Stream: TStream);
var
  FXMLConf: TXMLConfig;
begin
  FXMLConf := TXMLConfig.Create(Self);
  try
    Stream.Position := 0;
    FXMLConf.LoadFromStream(Stream);
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
end;

procedure TBGRASVGImageList.SetWidth(AValue: integer);
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;
end;

procedure TBGRASVGImageList.WriteData(Stream: TStream);
var
  FXMLConf: TXMLConfig;
begin
  FXMLConf := TXMLConfig.Create(Self);
  try
    Save(FXMLConf);
    FXMLConf.SaveToStream(Stream);
    FXMLConf.Flush;
  finally
    FXMLConf.Free;
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
      XMLConf.SetValue('Item' + i.ToString + '/SVG', FItems[i].Text);
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
end;

procedure TBGRASVGImageList.Remove(AIndex: integer);
begin
  FItems.Remove(FItems[AIndex]);
end;

procedure TBGRASVGImageList.Exchange(AIndex1, AIndex2: integer);
begin
  FItems.Exchange(AIndex1, AIndex2);
end;

function TBGRASVGImageList.GetSVGString(AIndex: integer): string;
begin
  Result := FItems[AIndex].Text;
end;

procedure TBGRASVGImageList.Replace(AIndex: integer; ASVG: string);
begin
  FItems[AIndex].Text := ASVG;
end;

function TBGRASVGImageList.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TBGRASVGImageList.GetSize(ATargetDPI: integer): TSize;
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
  ACanvas: TCanvas; ALeft, ATop: integer);
begin
  Draw(AIndex, AControl, ACanvas, ALeft, ATop, UseSVGAlignment);
end;

procedure TBGRASVGImageList.Draw(AIndex: integer; AControl: TControl; ACanvas: TCanvas;
  ALeft, ATop: integer; AUseSVGAlignment: boolean; AOpacity: byte);
var
  parentForm: TCustomForm;
  targetDPI: Integer;
begin
  parentForm := GetParentForm(AControl);
  if parentForm <> nil then targetDPI := parentForm.PixelsPerInch
  else targetDPI := Screen.PixelsPerInch;
  Draw(AIndex, AControl, targetDPI, ACanvas, ALeft, ATop, AUseSVGAlignment, AOpacity);
end;

procedure TBGRASVGImageList.Draw(AIndex: integer; AControl: TControl;
  ATargetDPI: integer; ACanvas: TCanvas; ALeft, ATop: integer);
begin
  Draw(AIndex, AControl, ATargetDPI, ACanvas, ALeft, ATop, UseSVGAlignment);
end;

procedure TBGRASVGImageList.Draw(AIndex: integer; AControl: TControl;
  ATargetDPI: integer; ACanvas: TCanvas; ALeft, ATop: integer;
  AUseSVGAlignment: boolean; AOpacity: byte);
begin
  with GetSize(ATargetDPI) do
    Draw(AIndex, AControl, ACanvas, ALeft, ATop, cx, cy, AUseSVGAlignment, AOpacity);
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
  svg: TBGRASVG;
begin
  if (AWidth = 0) or (AHeight = 0) or (ACanvasScale = 0) then
    Exit;
  bmp := TBGRABitmap.Create(round(AWidth * ACanvasScale), round(AHeight * ACanvasScale));
  svg := TBGRASVG.CreateFromString(FItems[AIndex].Text);
  try
    if AUseSVGAlignment then
      svg.StretchDraw(bmp.Canvas2D, 0, 0, bmp.Width, bmp.Height, true)
      else svg.StretchDraw(bmp.Canvas2D, HorizontalAlignment, VerticalAlignment, 0, 0, bmp.Width, bmp.Height);
    bmp.ApplyGlobalOpacity(AOpacity);
    bmp.Draw(ACanvas, RectWithSize(ALeft, ATop, AWidth, AHeight), False);
  finally
    bmp.Free;
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
  SetLength(arr, Length(AWidths));
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
