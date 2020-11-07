// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
unit BGRATheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes;

type
  TBGRAThemeButtonState = (btbsNormal, btbsHover, btbsActive, btbsDisabled);

  { TBGRATheme }

  TBGRATheme = class(TComponent)
  private

  protected

  public
    procedure DrawButton(Caption: string; State: TBGRAThemeButtonState;
      Focused: boolean; ARect: TRect; DestCanvas: TCanvas; CanvasScale: single); virtual;
    procedure DrawRadioButton(Caption: string; State: TBGRAThemeButtonState;
    {%H-}Focused: boolean; Checked: boolean; ARect: TRect;
      DestCanvas: TCanvas; CanvasScale: single); virtual;
    procedure DrawCheckBox(Caption: string; State: TBGRAThemeButtonState;
    {%H-}Focused: boolean; Checked: boolean; ARect: TRect;
      DestCanvas: TCanvas; CanvasScale: single); virtual;
  public
    procedure Colorize(Source, Dest: TBGRABitmap; c: TBGRAPixel);
  published

  end;

var
  BGRADefaultTheme: TBGRATheme;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Themes', [TBGRATheme]);
end;

{ TBGRATheme }

procedure TBGRATheme.Colorize(Source, Dest: TBGRABitmap; c: TBGRAPixel);
var
  psource: PBGRAPixel;
  pdest: PBGRAPixel;
  ec: TExpandedPixel;
  n: integer;
begin
  psource := Source.Data;
  pdest := Dest.Data;
  ec := GammaExpansion(c);
  for n := Source.NbPixels - 1 downto 0 do
  begin
    pdest^.red := GammaCompressionTab[
      ((GammaExpansionTab[psource^.red] * ec.red + 65535) shr 16)];
    pdest^.green := GammaCompressionTab[
      ((GammaExpansionTab[psource^.green] * ec.green + 65535) shr 16)];
    pdest^.blue := GammaCompressionTab[
      ((GammaExpansionTab[psource^.blue] * ec.blue + 65535) shr 16)];
    pdest^.alpha := (psource^.alpha * ec.alpha + 255) shr 16;
    Inc(pdest);
    Inc(psource);
  end;
end;

procedure TBGRATheme.DrawButton(Caption: string; State: TBGRAThemeButtonState;
  Focused: boolean; ARect: TRect; DestCanvas: TCanvas; CanvasScale: single);
var
  Style: TTextStyle;
begin
  DestCanvas.Font.Color := clBlack;
  case State of
    btbsNormal: DestCanvas.Brush.Color := RGBToColor(225, 225, 225);
    btbsHover: DestCanvas.Brush.Color := RGBToColor(229, 241, 251);
    btbsActive: DestCanvas.Brush.Color := RGBToColor(204, 228, 247);
    btbsDisabled: DestCanvas.Brush.Color := RGBToColor(204, 204, 204);
  end;

  DestCanvas.Pen.Color := DestCanvas.Brush.Color;
  DestCanvas.Rectangle(ARect);

  if Focused then
  begin
    DestCanvas.Pen.Color := clBlack;
    DestCanvas.Rectangle(ARect);
  end;

  if Caption <> '' then
  begin
    Style.Alignment := taCenter;
    Style.Layout := tlCenter;
    Style.Wordbreak := True;
    Style.SystemFont := False;
    Style.Clipping := True;
    Style.Opaque := False;
    DestCanvas.TextRect(ARect, 0, 0, Caption, Style);
  end;
end;

procedure TBGRATheme.DrawRadioButton(Caption: string; State: TBGRAThemeButtonState;
  Focused: boolean; Checked: boolean; ARect: TRect; DestCanvas: TCanvas; CanvasScale: single);
var
  Style: TTextStyle;
  Bitmap: TBGRABitmap;
  Color: TBGRAPixel;
begin
  DestCanvas.Font.Color := clBlack;
  case State of
    btbsHover: Color := BGRA(0, 120, 215);
    btbsActive: Color := BGRA(0, 84, 153);
    btbsDisabled:
    begin
      DestCanvas.Font.Color := clGray;
      Color := BGRA(204, 204, 204);
    end;
    else {btbsNormal}
      Color := BGRABlack;
  end;

  Bitmap := TBGRABitmap.Create(round(ARect.Height*CanvasScale),
            round(ARect.Height));
  Bitmap.FillEllipseAntialias(Bitmap.Height / 2, Bitmap.Height / 2,
    Bitmap.Height / 2 - 2, Bitmap.Height / 2 - 2, BGRAWhite);
  Bitmap.EllipseAntialias(Bitmap.Height / 2, Bitmap.Height / 2,
    Bitmap.Height / 2 - 2, Bitmap.Height / 2 - 2, Color{%H-}, 1);
  if Checked then
    Bitmap.FillEllipseAntialias(Bitmap.Height / 2, Bitmap.Height /
      2, Bitmap.Height / 4, Bitmap.Height / 4, Color);
  Bitmap.Draw(DestCanvas, RectWithSize(Arect.Left, Arect.Top,
    round(Bitmap.Width/CanvasScale), round(Bitmap.Height/CanvasScale)), False);
  Bitmap.Free;

  if Caption <> '' then
  begin
    Style.Alignment := taLeftJustify;
    Style.Layout := tlCenter;
    Style.Wordbreak := True;
    Style.SystemFont := False;
    Style.Clipping := True;
    Style.Opaque := False;
    DestCanvas.TextRect(Rect(Arect.Height, 0, ARect.Right, ARect.Bottom),
      ARect.Height, 0, Caption, Style);
  end;
end;

procedure TBGRATheme.DrawCheckBox(Caption: string; State: TBGRAThemeButtonState;
  Focused: boolean; Checked: boolean; ARect: TRect; DestCanvas: TCanvas; CanvasScale: single);
var
  Style: TTextStyle;
  Bitmap: TBGRABitmap;
  Color: TBGRAPixel;
  aleft, atop, aright, abottom: integer;
begin
  DestCanvas.Font.Color := clBlack;
  case State of
    btbsHover: Color := BGRA(0, 120, 215);
    btbsActive: Color := BGRA(0, 84, 153);
    btbsDisabled:
    begin
      DestCanvas.Font.Color := clGray;
      Color := BGRA(204, 204, 204);
    end;
    else {btbsNormal}
      Color := BGRABlack;
  end;

  Bitmap := TBGRABitmap.Create(round(ARect.Height*CanvasScale), round(ARect.Height*CanvasScale));
  Bitmap.Rectangle(0, 0, Bitmap.Height, Bitmap.Height, Color, BGRAWhite);
  aleft := 0;
  aright := Bitmap.Height;
  atop := 0;
  abottom := Bitmap.Height;
  if Checked then
    Bitmap.DrawPolyLineAntialias(Bitmap.ComputeBezierSpline(
      [BezierCurve(pointF(aleft + 2, atop + 3), PointF((aleft + aright - 1) / 2, abottom - 3)),
      BezierCurve(PointF((aleft + aright - 1) / 2, abottom - 3), PointF(
      (aleft + aright - 1) / 2, (atop * 2 + abottom - 1) / 3), PointF(aright - 2, atop - 2))]),
      Color, 1.5);
  Bitmap.Draw(DestCanvas, RectWithSize(Arect.Left, Arect.Top,
    round(Bitmap.Width/CanvasScale), round(Bitmap.Height/CanvasScale)), False);
  Bitmap.Free;

  if Caption <> '' then
  begin
    Style.Alignment := taLeftJustify;
    Style.Layout := tlCenter;
    Style.Wordbreak := True;
    Style.SystemFont := False;
    Style.Clipping := True;
    Style.Opaque := False;
    DestCanvas.TextRect(Rect(Arect.Height, 0, ARect.Right, ARect.Bottom),
      ARect.Height, 0, Caption, Style);
  end;
end;

var
  BasicTheme: TBGRATheme;

initialization

  BasicTheme := TBGRATheme.Create(nil);
  BGRADefaultTheme := BasicTheme;

finalization
  FreeAndNil(BasicTheme);

end.
