{
  Part of BGRA Controls. Made by third party.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit DTAnalogGauge;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, Graphics, {$IFDEF FPC}LResources, {$ELSE} BGRAGraphics, {$ENDIF}Forms, Controls, Dialogs, DTAnalogCommon,
  BCBaseCtrls, BGRABitmap, BGRABitmapTypes;

type

  TDTGaugeStyle = (gsCustom, gsDark, gsLight);


  { TDTCustomAnalogGauge }

  TDTCustomAnalogGauge = class(TBGRAGraphicCtrl)
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
    procedure DoChange({%H-}Sender: TObject);
    procedure SetGaugeStyle(AValue: TDTGaugeStyle);
    procedure SetNeedleSettings(AValue: TDTNeedleSettings);
    procedure SetPosition(AValue: integer);
    procedure SetScaleSettings(AValue: TDTScaleSettings);
    { Private declarations }
  protected
    { Protected declarations }
    procedure ResizeEvent({%H-}Sender: TObject);
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

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

{$IFDEF FPC}
procedure Register;
begin
  //{$I icons\dtanaloggauge_icon.lrs}
  RegisterComponents('BGRA Controls', [TDTAnalogGauge]);
end;
{$ENDIF}

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
  ScaleSettings.OnChange := DoChange;

  FFaceSettings := TDTFaceSettings.Create;
  FaceSettings.OnChange := DoChange;

  FNeedleSettings := TDTNeedleSettings.Create;
  NeedleSettings.OnChange := DoChange;

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
  r: integer;
  origin: TDTOrigin;
begin

  origin := Initializebitmap(FGaugeBodyBitmap, Width, Height);

  //// Keep circle insde frame
  r := round(origin.Radius * 0.95);

  // Draw Bitmap frame
  FGaugeBodyBitmap.FillEllipseAntialias(origin.CenterPoint.x,
    origin.CenterPoint.y,
    r, r, FFaceSettings.ColorFrame);

  // Draw thin antialiased border to smooth against background
  FGaugeBodyBitmap.EllipseAntialias(origin.CenterPoint.x,
    origin.CenterPoint.y,
    r, r, ColorToBGRA(clBlack, 120), 1);

end;

procedure TDTCustomAnalogGauge.DrawGaugeRange;
var
  {%H-}r, w, h, Xo, Yo: integer;
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
      FGaugeBodyBitmap.FillEllipseLinearColorAntialias(Xo, Yo, r, r, FFaceSettings.ColorStart, ColorToBGRA(FFaceSettings.ColorEnd));
    fsnone:
      FGaugeBodyBitmap.FillEllipseAntialias(Xo, Yo, r, r, FFaceSettings.ColorStart);
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

      FGaugeScaleBitmap.DrawLineAntialias(x, y, xt, yt, FScaleSettings.TickColor, FScaleSettings.ThicknessSubTick);

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

      FGaugeScaleBitmap.DrawLineAntialias(x, y, xt, yt, FScaleSettings.TickColor, FScaleSettings.ThicknessMainTick);

      // Draw text for main ticks
      Xt := xo - Round((r - FScaleSettings.LengthMainTick) * 0.7 * cos((j + i * FScaleSettings.Angle / n) * Pi / 180));
      Yt := yo - Round((r - FScaleSettings.LengthMainTick) * 0.7 * sin((j + i * FScaleSettings.Angle / n) * Pi / 180));

      FGaugeScaleBitmap.TextOut(Xt, Yt - (FGaugeScaleBitmap.FontHeight / 1.7),
        IntToStr(i * FScaleSettings.Maximum div FScaleSettings.MainTickCount),
        //ColorToBGRA(FScaleSettings.TickColor),
        FScaleSettings.TextColor,
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
        FNeedleSettings.NeedleColor,
        FScaleSettings.ThicknessMainTick);
    end;
    nsTriangle:
    begin

    end;
  end;

  // Draw cap over needle
  FGaugeNeedleBitmap.EllipseAntialias(Xo, Yo, FNeedleSettings.CapRadius,
    FNeedleSettings.CapRadius,
    FNeedleSettings.CapEdgeColor,
    2, FNeedleSettings.CapColor);

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
