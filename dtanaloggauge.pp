unit DTAnalogGauge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DTAnalogCommon,
  BGRABitmap, BGRABitmapTypes;

type

  TDTGaugeStyle = (gsCustom, gsDark, gsLight);


  { TDTCustomAnalogGauge }

  TDTCustomAnalogGauge = class(TGraphicControl)
  private
    FFaceSettings: TDTFaceSettings;
    FGaugeStyle: TDTGaugeStyle;
    FNeedleSettings: TDTNeedleSettings;
    FPosition: integer;
    FResized: boolean;
    FGaugeBitmap: TBGRABitmap;
    FGaugeBodyBitmap: TBGRABitmap;
    FGaugeScaleBitmap: TBGRABitmap;
    FGaugeNeedleBitmap: TBGRABitmap;
    FScaleSettings: TDTScaleSettings;
    procedure SetFaceSettings(AValue: TDTFaceSettings);
    procedure DoChange(Sender: TObject);
    procedure SetGaugeStyle(AValue: TDTGaugeStyle);
    procedure SetNeedleSettings(AValue: TDTNeedleSettings);
    procedure SetPosition(AValue: integer);
    procedure SetScaleSettings(AValue: TDTScaleSettings);
    { Private declarations }
  protected
    { Protected declarations }
    procedure ResizeEvent(Sender: TObject);
    procedure ClearBitMap(var BitMap: TBGRABitmap);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure DrawGauge; virtual;
    procedure DrawGaugeBody; virtual;
    procedure DrawGaugeRange; virtual;
    procedure DrawGaugeFace; virtual;
    procedure DrawGaugeScale; virtual;
    procedure DrawGaugeNeedle; virtual;
  published
    { Published declarations }
    property Position: integer read FPosition write SetPosition;
    property FaceSettings: TDTFaceSettings read FFaceSettings write SetFaceSettings;
    property ScaleSettings: TDTScaleSettings read FScaleSettings write SetScaleSettings;
    property NeedleSettings: TDTNeedleSettings read FNeedleSettings write SetNeedleSettings;
    //property GaugeStyle: TDTGaugeStyle read FGaugeStyle write SetGaugeStyle;
  end;

  { TDTAnalogGauge }

  TDTAnalogGauge = class(TDTCustomAnalogGauge)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    property FaceSettings;
    property ScaleSettings;
    property NeedleSettings;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TDTAnalogGauge]);
end;

{ TDTCustomAnalogGauge }

procedure TDTCustomAnalogGauge.ClearBitMap(var BitMap: TBGRABitmap);
begin
  BitMap.Fill(BGRA(0, 0, 0, 0));
end;

constructor TDTCustomAnalogGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 240;
  Height := 240;

  FScaleSettings := TDTScaleSettings.Create;
  ScaleSettings.OnChange := @DoChange;

  FFaceSettings := TDTFaceSettings.Create;
  FaceSettings.OnChange := @DoChange;

  FNeedleSettings := TDTNeedleSettings.Create;
  NeedleSettings.OnChange := @DoChange;

  FGaugeBitmap := TBGRABitmap.Create(Width, Height);
  FGaugeBodyBitmap := TBGRABitmap.Create(Width, Height);
  FGaugeScaleBitmap := TBGRABitmap.Create(Width, Height);
  FGaugeNeedleBitmap := TBGRABitmap.Create(Width, Height);

end;

destructor TDTCustomAnalogGauge.Destroy;
begin
  FScaleSettings.OnChange:=nil;
  FScaleSettings.Free;
  FFaceSettings.OnChange:=nil;
  FFaceSettings.Free;
  FGaugeBitmap.Free;
  FGaugeBodyBitmap.Free;
  FGaugeScaleBitmap.Free;
  FGaugeNeedleBitmap.Free;
  FNeedleSettings.OnChange:=nil;
  FNeedleSettings.Free;
  inherited Destroy;
end;

procedure TDTCustomAnalogGauge.DoChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TDTCustomAnalogGauge.SetGaugeStyle(AValue: TDTGaugeStyle);
begin
  if FGaugeStyle = AValue then
    Exit;
  FGaugeStyle := AValue;

  DoChange(self);

end;

procedure TDTCustomAnalogGauge.SetNeedleSettings(AValue: TDTNeedleSettings);
begin
  if FNeedleSettings = AValue then
    Exit;
  FNeedleSettings := AValue;

  DoChange(self);

end;

procedure TDTCustomAnalogGauge.DrawGauge;
begin
  DrawGaugeBody;
  DrawGaugeFace;

  if FScaleSettings.EnableRangeIndicator then
    DrawGaugeRange;

  DrawGaugeScale;
  DrawGaugeNeedle;
end;

procedure TDTCustomAnalogGauge.DrawGaugeBody;
var
  w, h, r, Xo, Yo: integer;
  origin: TDTOrigin;
begin

  origin := Initializebitmap(FGaugeBodyBitmap, Width, Height);

  //// Keep circle insde frame
  r := round(origin.Radius * 0.95);

  // Draw Bitmap frame
  FGaugeBodyBitmap.FillEllipseAntialias(origin.CenterPoint.x,
    origin.CenterPoint.y,
    r, r, ColorToBGRA(FFaceSettings.ColorFrame));

  // Draw thin antialiased border to smooth against background
  FGaugeBodyBitmap.EllipseAntialias(origin.CenterPoint.x,
    origin.CenterPoint.y,
    r, r, ColorToBGRA(clBlack, 120), 1);

end;

procedure TDTCustomAnalogGauge.DrawGaugeRange;
var
  w, h, r, Xo, Yo, X, Y, Xt, Yt: integer;
  n: integer;
  j: single;
begin

  ClearBitMap(FGaugeScaleBitmap);

  w := Width;
  h := Height;

  FGaugeScaleBitmap.SetSize(w, h);

  { Set center point }
  Xo := w div 2;
  Yo := h div 2;

  // Determine radius. If canvas is rectangular then r = shortest length w or h
  r := yo;

  if xo > yo then
    r := yo;

  if xo < yo then
    r := xo;

  //j := (180 - FScaleSettings.Angle) / 2;

end;

procedure TDTCustomAnalogGauge.DrawGaugeFace;
var
  w, h, r, Xo, Yo: integer;
  origin: TDTOrigin;
begin
  ClearBitMap(FGaugeScaleBitmap);

  w := Width;
  h := Height;

  FGaugeBodyBitmap.SetSize(w, h);

  //{ Set center point }
  Xo := w div 2;
  Yo := h div 2;

  //  // Determine radius. If canvas is rectangular then r = shortest length w or h
  r := yo;

  if xo > yo then
    r := yo;

  if xo < yo then
    r := xo;

  // Keep circle insde frame
  r := round(r * 0.95) - 5;

  // Draw face background
  case FFaceSettings.FillStyle of
    fsGradient:
      FGaugeBodyBitmap.FillEllipseLinearColorAntialias(Xo, Yo, r, r, ColorToBGRA(FFaceSettings.ColorStart), ColorToBGRA(FFaceSettings.ColorEnd));
    fsnone:
      FGaugeBodyBitmap.FillEllipseAntialias(Xo, Yo, r, r, ColorToBGRA(FFaceSettings.ColorStart));
  end;


  //origin := Initializebitmap(FGaugeBodyBitmap, Width, Height);

  //// Keep circle insde frame
  //r := round(origin.Radius * 0.95) - 5;

  //// Draw face background
  //case FFaceSettings.FillStyle of
  //  fsGradient:
  //    FGaugeBodyBitmap.FillEllipseLinearColorAntialias(origin.CenterPoint.x, origin.CenterPoint.y, r, r, ColorToBGRA(FFaceSettings.ColorStart), ColorToBGRA(FFaceSettings.ColorEnd));
  //  fsnone:
  //    FGaugeBodyBitmap.FillEllipseAntialias(origin.CenterPoint.x, origin.CenterPoint.y, r, r, ColorToBGRA(FFaceSettings.ColorStart));
  //end;

end;

procedure TDTCustomAnalogGauge.DrawGaugeScale;
var
  w, h, r, Xo, Yo, X, Y, Xt, Yt: integer;
  i, n: integer;
  j: single;
begin

  w := Width;
  h := Height;

  FGaugeScaleBitmap.SetSize(w, h);

  ClearBitMap(FGaugeScaleBitmap);

  { Set center point }
  Xo := w div 2;
  Yo := h div 2;

  // Determine radius. If canvas is rectangular then r = shortest length w or h
  r := yo;

  if xo > yo then
    r := yo;

  if xo < yo then
    r := xo;

  j := (180 - FScaleSettings.Angle) / 2;

  // Draw SubTicks
  if FScaleSettings.EnableSubTicks then
  begin

    n := FScaleSettings.MainTickCount * FScaleSettings.SubTickCount;

    for i := 0 to n do
    begin
      // Calculate draw from point
      X := xo - Round(r * 0.85 * cos((j + i * FScaleSettings.Angle / n) * Pi / 180));
      Y := yo - Round(r * 0.85 * sin((j + i * FScaleSettings.Angle / n) * Pi / 180));

      // Calculate draw to point
      Xt := xo - Round(((r * 0.85) - FScaleSettings.LengthSubTick) * cos((j + i * FScaleSettings.Angle / n) * Pi / 180));
      Yt := yo - Round(((r * 0.85) - FScaleSettings.LengthSubTick) * sin((j + i * FScaleSettings.Angle / n) * Pi / 180));

      FGaugeScaleBitmap.DrawLineAntialias(x, y, xt, yt, ColorToBGRA(FScaleSettings.TickColor), FScaleSettings.ThicknessSubTick);

    end;
  end;

  if FScaleSettings.EnableMainTicks then
  begin

    FGaugeScaleBitmap.FontName := FScaleSettings.TextFont;
    FGaugeScaleBitmap.FontHeight := FScaleSettings.TextSize;
    FGaugeScaleBitmap.FontQuality := fqFineAntialiasing;

    n := FScaleSettings.MainTickCount;

    for i := 0 to n do
    begin
      // Draw main ticks
      // Calculate draw from point
      X := xo - Round(r * 0.85 * cos((j + i * FScaleSettings.Angle / n) * Pi / 180));
      Y := yo - Round(r * 0.85 * sin((j + i * FScaleSettings.Angle / n) * Pi / 180));

      // Calculate draw to point
      Xt := xo - Round(((r * 0.85) - FScaleSettings.LengthMainTick) * cos((j + i * FScaleSettings.Angle / n) * Pi / 180));
      Yt := yo - Round(((r * 0.85) - FScaleSettings.LengthMainTick) * sin((j + i * FScaleSettings.Angle / n) * Pi / 180));

      FGaugeScaleBitmap.DrawLineAntialias(x, y, xt, yt, ColorToBGRA(FScaleSettings.TickColor), FScaleSettings.ThicknessMainTick);

      // Draw text for main ticks
      Xt := xo - Round((r - FScaleSettings.LengthMainTick) * 0.7 * cos((j + i * FScaleSettings.Angle / n) * Pi / 180));
      Yt := yo - Round((r - FScaleSettings.LengthMainTick) * 0.7 * sin((j + i * FScaleSettings.Angle / n) * Pi / 180));

      FGaugeScaleBitmap.TextOut(Xt, Yt - (FGaugeScaleBitmap.FontHeight / 1.7),
        IntToStr(i * FScaleSettings.Maximum div FScaleSettings.MainTickCount),
        //ColorToBGRA(FScaleSettings.TickColor),
        ColorToBGRA(FScaleSettings.TextColor),
        taCenter);
    end;
  end;
end;

procedure TDTCustomAnalogGauge.DrawGaugeNeedle;
var
  w, h, Xo, Yo, X, Y: integer;
  j: single;
begin

  ClearBitMap(FGaugeNeedleBitmap);

  w := Width;
  h := Height;

  FGaugeNeedleBitmap.SetSize(w, h);

  { Set center point }
  Xo := w div 2;
  Yo := h div 2;

  j := (180 - FScaleSettings.Angle) / 2;

  // Draw needle
  case FNeedleSettings.NeedleStyle of
    nsLine:
    begin

      X := xo - Round(FNeedleSettings.NeedleLength * cos((j + Position * FScaleSettings.Angle / FScaleSettings.Maximum) * Pi / 180));
      Y := yo - Round(FNeedleSettings.NeedleLength * sin((j + Position * FScaleSettings.Angle / FScaleSettings.Maximum) * Pi / 180));

      FGaugeNeedleBitmap.DrawLineAntialias(xo, yo, x, y,
        ColorToBGRA(FNeedleSettings.NeedleColor),
        FScaleSettings.ThicknessMainTick);
    end;
    nsTriangle:
    begin

    end;
  end;

  // Draw cap over needle
  FGaugeNeedleBitmap.EllipseAntialias(Xo, Yo, FNeedleSettings.CapRadius,
    FNeedleSettings.CapRadius,
    ColorToBGRA(FNeedleSettings.CapEdgeColor),
    2, ColorToBGRA(FNeedleSettings.CapColor));

end;

procedure TDTCustomAnalogGauge.SetFaceSettings(AValue: TDTFaceSettings);
begin
  if FFaceSettings = AValue then
    Exit;
  FFaceSettings := AValue;

  DoChange(self);

end;

procedure TDTCustomAnalogGauge.SetPosition(AValue: integer);
begin
  if FPosition = AValue then
    Exit;
  FPosition := AValue;

  DoChange(self);

end;

procedure TDTCustomAnalogGauge.SetScaleSettings(AValue: TDTScaleSettings);
begin
  if FScaleSettings = AValue then
    Exit;
  FScaleSettings := AValue;

  DoChange(self);

end;

procedure TDTCustomAnalogGauge.ResizeEvent(Sender: TObject);
begin
  FResized := True;
end;

procedure TDTCustomAnalogGauge.Paint;
begin
  inherited Paint;

  ClearBitMap(FGaugeBitmap);

  FGaugeBitmap.SetSize(Width, Height);

  DrawGauge;

  FGaugeBitmap.BlendImage(0, 0, FGaugeBodyBitmap, boLinearBlend);
  FGaugeBitmap.BlendImage(0, 0, FGaugeScaleBitmap, boLinearBlend);
  FGaugeBitmap.BlendImage(0, 0, FGaugeNeedleBitmap, boLinearBlend);

  FGaugeBitmap.Draw(Canvas, 0, 0, False);

end;

end.
