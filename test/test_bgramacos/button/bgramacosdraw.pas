unit bgramacosdraw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, BGRAGradientScanner, Math;

type

  { TBGRAMacOS }

  TBGRAMacOS = class
    class var BUTTONLINEWIDTH: single;
    // Generic gradient button that's used by the other methods
    class procedure GradientButton(
      const lineTop, lineBottom, fillTop, fillBottom: TBGRAPixel;
      const ADest: TBGRABitmap; const ARect: TRect);

    // Button
    class procedure Button(const ADest: TBGRABitmap; const ARect: TRect);
    class procedure ButtonActive(const ADest: TBGRABitmap; const ARect: TRect);
    class procedure ButtonPressed(const ADest: TBGRABitmap; const ARect: TRect);
  end;

implementation

{ TBGRAMacOS }

class procedure TBGRAMacOS.GradientButton(
  const lineTop, lineBottom, fillTop, fillBottom: TBGRAPixel;
  const ADest: TBGRABitmap; const ARect: TRect);
var
  gradient, gradientfill: TBGRAGradientScanner;
  halflinewidth: integer;
begin
  if (BUTTONLINEWIDTH * 2) + 3 > ARect.Height then
    Exit;
  if (BUTTONLINEWIDTH * 2) + 3 > ARect.Width then
    Exit;

  gradient := TBGRAGradientScanner.Create(lineTop, lineBottom, gtLinear,
    PointF(0, 0), PointF(0, ARect.Bottom));

  gradientfill := TBGRAGradientScanner.Create(fillTop, fillBottom,
    gtLinear, PointF(0, BUTTONLINEWIDTH), PointF(0, ARect.Bottom - BUTTONLINEWIDTH));

  halflinewidth := ceil(BUTTONLINEWIDTH * 0.5);

  ADest.RoundRectAntialias(ARect.Left + halflinewidth,
    ARect.Top + halflinewidth, ARect.Right - halflinewidth - 1,
    ARect.Bottom - halflinewidth - 1,
    4, 4, gradient, BUTTONLINEWIDTH, gradientfill);

  FreeAndNil(gradient);
  FreeAndNil(gradientfill);
end;

class procedure TBGRAMacOS.Button(const ADest: TBGRABitmap; const ARect: TRect);
begin
  GradientButton(BGRA(210, 210, 210), BGRA(180, 180, 180), BGRAWhite,
    BGRAWhite, ADest, ARect);
end;

class procedure TBGRAMacOS.ButtonActive(const ADest: TBGRABitmap; const ARect: TRect);
begin
  GradientButton(BGRA(83, 160, 246), BGRA(43, 93, 251), BGRA(111, 177, 248),
    BGRA(45, 127, 252), ADest, ARect);
end;

class procedure TBGRAMacOS.ButtonPressed(const ADest: TBGRABitmap; const ARect: TRect);
begin
  GradientButton(BGRA(55, 124, 251), BGRA(36, 60, 218), BGRA(84, 149, 250),
    BGRA(39, 102, 225), ADest, ARect);
end;

initialization
  TBGRAMacOS.BUTTONLINEWIDTH := 1;

end.
