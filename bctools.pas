{ General framework methods for rendering background, borders, text, etc.

  Copyright (C) 2012 Krzysztof Dibowski dibowski at interia.pl

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

unit BCTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, BGRABitmap, BGRABitmapTypes, bctypes,
  Controls, BGRAGradientScanner;

// This method prepare BGRABitmap for rendering BCFont type
procedure AssignBCFont(AFont: TBCFont; out ATargetBGRA: TBGRABitmap);
// Calculate text height and width (doesn't include wordwrap - just single line)
procedure CalculateTextSize(const AText: String; AFont: TBCFont;
  out ANewWidth, ANewHeight: integer);
// This method correct TRect to border width. As far as border width is bigger,
// BGRA drawing rectangle with offset (half border width)
procedure CalculateBorderRect(ABorder: TBCBorder; var ARect: TRect);
// This returns a rectangle that is inside the border outline
procedure CalculateInnerRect(ABorder: TBCBorder; var ARect: TRect);
// Create BGRA Gradient Scanner based on BCGradient properties
function CreateGradient(AGradient: TBCGradient; ARect: TRect): TBGRAGradientScanner;
// Render arrow (used by BCButton with DropDownMenu style)
procedure RenderArrow(ATargetBGRA: TBGRABitmap; const ARect: TRect;
  ASize: Integer; ADirection: TBCArrowDirection; AColor: TColor = clBlack;
  AOpacity: Byte = 255);
// Render customizable backgroud (used e.g. by TBCButton, TBCPanel, TBCLabel)
procedure RenderBackground(const ARect: TRect; ABackground: TBCBackground;
  ATargetBGRA: TBGRABitmap; ARounding: TBCRounding = nil; AHasNoBorder: boolean = false);
procedure RenderBackgroundF(x1,y1,x2,y2: single; ABackground: TBCBackground;
  ATargetBGRA: TBGRABitmap; ARounding: TBCRounding = nil);
procedure RenderBackgroundAndBorder(const ARect: TRect; ABackground: TBCBackground;
  ATargetBGRA: TBGRABitmap; ARounding: TBCRounding; ABorder: TBCBorder; AInnerMargin: single = 0);
// Render customizable border (used e.g. by TBCButton, TBCPanel, TBCLabel)
procedure RenderBorder(const ARect: TRect; ABorder: TBCBorder;
  ATargetBGRA: TBGRABitmap; ARounding: TBCRounding = nil);
procedure RenderBorderF(x1,y1,x2,y2: single; ABorder: TBCBorder;
  ATargetBGRA: TBGRABitmap; ARounding: TBCRounding = nil);
// Render BCFont (used e.g. by TBCButton, TBCPanel, TBCLabel)
procedure RenderText(const ARect: TRect; AFont: TBCFont;
  const AText: String; ATargetBGRA: TBGRABitmap);
// Return LCL horizontal equivalent for BCAlignment
function BCAlign2HAlign(AAlign: TBCAlignment): TAlignment;
// Return LCL vertical equivalent for BCAlignment
function BCAlign2VAlign(AAlign: TBCAlignment): TTextLayout;

implementation

uses Types, BGRAPolygon, BGRAFillInfo, BGRAText, math, LCLType, LCLIntf;

procedure CalculateBorderRect(ABorder: TBCBorder; var ARect: TRect);
var w: integer;
begin
  if ABorder = nil then Exit;
  w := ABorder.Width div 2;
  Inc(ARect.Left, w);
  Inc(ARect.Top, w);
  Dec(ARect.Right, w);
  Dec(ARect.Bottom, w);
end;

procedure CalculateInnerRect(ABorder: TBCBorder; var ARect: TRect);
var w: integer;
begin
  if (ABorder = nil) or (ABorder.Style = bboNone) then Exit;
  w := ABorder.Width;
  Inc(ARect.Left, w);
  Inc(ARect.Top, w);
  Dec(ARect.Right, w);
  Dec(ARect.Bottom, w);
end;

function CreateGradient(AGradient: TBCGradient; ARect: TRect): TBGRAGradientScanner;
begin
  Result := TBGRAGradientScanner.Create(
    ColorToBGRA(ColorToRGB(AGradient.StartColor), AGradient.StartColorOpacity),
    ColorToBGRA(ColorToRGB(AGradient.EndColor), AGradient.EndColorOpacity),
    AGradient.GradientType, PointF(ARect.Left + Round(
    ((ARect.Right - ARect.Left) / 100) * AGradient.Point1XPercent),
    ARect.Top + Round(((ARect.Bottom - ARect.Top) / 100) * AGradient.Point1YPercent)),
    PointF(ARect.Left + Round(((ARect.Right - ARect.Left) / 100) *
    AGradient.Point2XPercent), ARect.Top + Round(
    ((ARect.Bottom - ARect.Top) / 100) * AGradient.Point2YPercent)),
    AGradient.ColorCorrection, AGradient.Sinus);
end;

procedure RenderBackgroundAndBorder(const ARect: TRect;
  ABackground: TBCBackground; ATargetBGRA: TBGRABitmap;
  ARounding: TBCRounding; ABorder: TBCBorder; AInnerMargin: single);
var w: single;
begin
  if ABorder.Style = bboNone then
  begin
    w := AInnerMargin-0.5;
    RenderBackgroundF(ARect.Left+w, ARect.Top+w, ARect.Right-1-w,
          ARect.Bottom-1-w,ABackground,ATargetBGRA,ARounding);
  end
  else
  begin
    w := (ABorder.Width-1)/2+AInnerMargin;
    RenderBackgroundF(ARect.Left+w,ARect.Top+w,ARect.Right-1-w,ARect.Bottom-1-w,ABackground,ATargetBGRA,ARounding);
    RenderBorderF(ARect.Left+w,ARect.Top+w,ARect.Right-1-w,ARect.Bottom-1-w,ABorder,ATargetBGRA,ARounding);
  end;
end;

procedure RenderBorder(const ARect: TRect; ABorder: TBCBorder;
  ATargetBGRA: TBGRABitmap; ARounding: TBCRounding = nil);
begin
  RenderBorderF(ARect.Left,ARect.Top,ARect.Right-1,ARect.Bottom-1,ABorder,
  ATargetBGRA,ARounding);
end;

procedure RenderBorderF(x1,y1,x2,y2: single; ABorder: TBCBorder;
  ATargetBGRA: TBGRABitmap; ARounding: TBCRounding = nil);
var
  fiLight: TFillBorderRoundRectInfo;
  rx,ry: Byte;
  ropt: TRoundRectangleOptions;
begin
  if (x1>x2) or (y1>y2) then exit;
  if ABorder.Style=bboNone then Exit;

  if ARounding = nil then
  begin
    rx   := 0;
    ry   := 0;
    ropt := [];
  end else
  begin
    rx   := ARounding.RoundX;
    ry   := ARounding.RoundY;
    ropt := ARounding.RoundOptions;
  end;

  ATargetBGRA.RoundRectAntialias(x1,y1,x2,y2,
    rx, ry, ColorToBGRA(ColorToRGB(ABorder.Color),ABorder.ColorOpacity),
    ABorder.Width, ropt);

  if ABorder.LightWidth > 0 then
  begin
    //compute light position
    fiLight := TFillBorderRoundRectInfo.Create(
      x1,y1,x2,y2, rx,
      ry, ABorder.Width + ABorder.LightWidth, ropt);
    //check if there is an inner position
    if fiLight.InnerBorder <> nil then
      with fiLight.InnerBorder do //fill with light
        ATargetBGRA.RoundRectAntialias(topleft.x, topleft.y, bottomright.x,
          bottomright.y, radiusx, radiusY,
          ColorToBGRA(ColorToRGB(ABorder.LightColor), ABorder.LightOpacity),
          ABorder.LightWidth, ropt);
    fiLight.Free;
  end;
end;

procedure RenderText(const ARect: TRect; AFont: TBCFont;
  const AText: String; ATargetBGRA: TBGRABitmap);
var
  shd: TBGRABitmap;
  hal: TAlignment;
  val: TTextLayout;
  st: TTextStyle;
begin
  AssignBCFont(AFont,ATargetBGRA);

  hal := BCAlign2HAlign(AFont.TextAlignment);
  val := BCAlign2VAlign(AFont.TextAlignment);

  FillChar({%H-}st, SizeOf({%H-}st),0);

  st.Wordbreak   := AFont.WordBreak;
  st.Alignment   := hal;
  st.Layout      := val;
  st.SingleLine  := AFont.SingleLine;
  st.EndEllipsis := AFont.EndEllipsis;

  if AFont.Shadow then
  begin
    shd := TBGRABitmap.Create(ATargetBGRA.Width,ATargetBGRA.Height,BGRAPixelTransparent);
    shd.FontName      := ATargetBGRA.FontName;
    shd.FontStyle     := ATargetBGRA.FontStyle;
    shd.FontQuality   := ATargetBGRA.FontQuality;
    shd.FontHeight    := ATargetBGRA.FontHeight;
    shd.TextRect(ARect, ARect.Left, ARect.Top, AText, st, ColorToBGRA(ColorToRGB(AFont.ShadowColor),
      AFont.ShadowColorOpacity));
    BGRAReplace(shd, shd.FilterBlurRadial(AFont.ShadowRadius, rbFast));
    ATargetBGRA.BlendImage(AFont.ShadowOffsetX, AFont.ShadowOffsetY,
      shd, boLinearBlend);
    shd.Free;
  end;

  ATargetBGRA.TextRect(ARect,ARect.Left,ARect.Top,AText,st,AFont.Color);

end;

function BCAlign2HAlign(AAlign: TBCAlignment): TAlignment;
begin
  if AAlign in [bcaCenter, bcaCenterTop, bcaCenterBottom] then
    Result := taCenter
  else if AAlign in [bcaRightCenter, bcaRightTop, bcaRightBottom] then
    Result := taRightJustify
  else
    Result := taLeftJustify;
end;

function BCAlign2VAlign(AAlign: TBCAlignment): TTextLayout;
begin
  if AAlign in [bcaCenter, bcaLeftCenter, bcaRightCenter] then
    Result := tlCenter
  else if AAlign in [bcaCenterBottom, bcaLeftBottom, bcaRightBottom] then
    Result := tlBottom
  else
    Result := tlTop;
end;

procedure AssignBCFont(AFont: TBCFont; out ATargetBGRA: TBGRABitmap);
var c: TBitmap;
begin
  // Canvas is need for calculate font height
  c := TBitmap.Create;
  c.Canvas.Font.Name := AFont.Name;
  c.Canvas.Font.Style := AFont.Style;
  case AFont.FontQuality of
    fqSystem: c.Canvas.Font.Quality := fqNonAntialiased;
    fqFineAntialiasing: c.Canvas.Font.Quality := fqAntialiased;
    fqFineClearTypeRGB: c.Canvas.Font.Quality := fqProof;
    fqSystemClearType: c.Canvas.Font.Quality := fqCleartype;
  end;
  // FontAntialias is only backward compability for FontQuality property.
  // FontQuality is published in TBCFont so we don't need FontAntialias anymore.
  //ATargetBGRA.FontAntialias := AFont.FontAntialias;
  {%H-}ATargetBGRA.FontStyle     := AFont.Style;

  // If font quality is system, then we can leave default values. LCL will
  // handle everything (when name is "default" or height 0)
  if AFont.FontQuality in [fqSystem,fqSystemClearType] then
  begin
    ATargetBGRA.FontName   := AFont.Name;
    ATargetBGRA.FontHeight := AFont.Height;
  end
  else
  begin
    // Getting real font name
    if SameText(AFont.Name,'default')
    then ATargetBGRA.FontName := GetFontData(c.Canvas.Font.Handle).Name
    else ATargetBGRA.FontName := AFont.Name;

    // Calculate default height, because when font quality is <> fqSystemXXX
    // then if height is 0 then it is 0 for real
    if (AFont.Height=0) then
      ATargetBGRA.FontHeight := -c.Canvas.TextHeight('Bgra')
    else
      ATargetBGRA.FontHeight := AFont.Height;
  end;
  ATargetBGRA.FontQuality   := AFont.FontQuality;
  c.Free;
end;

procedure CalculateTextSize(const AText: String; AFont: TBCFont; out ANewWidth,
  ANewHeight: integer);
var
  s: TSize;
  tmp: TBGRABitmap;
begin
  if (AText = '') or (AFont = nil) then
  begin
    ANewWidth := 0;
    ANewHeight := 0;
    Exit;
  end;

  {TODO: Check why BGRATextSize doesn't work. BGRABitmap call this method
         and it work. Temporary I'm creating temp bitmap
  }
  {s := BGRAText.BGRATextSize(AFont,AFont.FontQuality,AText,FontAntialiasingLevel);
  if (s.cy >= 24) and AFont.FontAntialias then
    s := BGRAText.BGRATextSize(AFont,AFont.FontQuality,AText,4);}
  tmp := TBGRABitmap.Create(0,0);
  AssignBCFont(AFont, tmp);

  s := tmp.TextSize(AText);
  tmp.Free;

  { shadow offset }
  if AFont.Shadow then
  begin
    Inc(s.cx, 2 * Abs(AFont.ShadowOffsetX) + 2 * AFont.ShadowRadius);
    Inc(s.cy, 2 * Abs(AFont.ShadowOffsetY) + 2 * AFont.ShadowRadius);
  end;

  ANewWidth := s.cx;
  ANewHeight := s.cy;
end;

procedure RenderArrow(ATargetBGRA: TBGRABitmap; const ARect: TRect;
  ASize: Integer; ADirection: TBCArrowDirection; AColor: TColor; AOpacity: Byte);
var
  p: ArrayOfTPointF;
  n: byte;
  temp: TBGRABitmap;
  w: Integer;
begin
  // We can't draw outside rect
  w := Min(ASize, ARect.Right - ARect.Left);

  { Poly }
  SetLength(p, 3);

  temp := TBGRABitmap.Create(w+1, w+1,BGRAPixelTransparent);

  case ADirection of
    badDown:
      begin;
        p[0].x := 0;
        p[0].y := 0;

        p[1].x := w;
        p[1].y := 0;

        p[2].x := Round(w/2);
        p[2].y := w;
      end;
    badUp:
      begin
        p[0].x := Round(w/2);
        p[0].y := 0;

        p[1].x := 0;
        p[1].y := w;

        p[2].x := w;
        p[2].y := w;
      end;
    badLeft:
      begin
        p[0].x := 0;
        p[0].y := Round(w/2);

        p[1].x := w;
        p[1].y := 0;

        p[2].x := w;
        p[2].y := w;
      end;
    badRight:
      begin
        p[0].x := w;
        p[0].y := Round(w/2);

        p[1].x := 0;
        p[1].y := 0;

        p[2].x := 0;
        p[2].y := w;
      end;
  end;

  // Fill n times to get best quality
  for n := 1 to 6 do
    temp.FillPolyAntialias(p, ColorToBGRA(ColorToRGB(AColor),AOpacity));

  ATargetBGRA.BlendImage(
    ARect.Right-Round( ((ARect.Right-ARect.Left)/2) + (w/2) ),
    ARect.Bottom-Round( ((ARect.Bottom-ARect.Top)/2) + (w/2) ),
    temp,
    boLinearBlend
  );
  temp.Free;
end;

procedure RenderBackgroundF(x1,y1,x2,y2: single; ABackground: TBCBackground;
  ATargetBGRA: TBGRABitmap; ARounding: TBCRounding = nil);
var
  backcolor: TBGRAPixel;
  multi: TBGRAMultishapeFiller;
  back: TBGRABitmap;
  grect1, grect2: TRect;
  gra: TBGRAGradientScanner;
  rx,ry: Byte;
  ropt: TRoundRectangleOptions;
begin
  if (x1>=x2) or (y1>=y2) then exit;
  if ARounding = nil then
  begin
    rx   := 0;
    ry   := 0;
    ropt := [];
  end else
  begin
    rx   := ARounding.RoundX;
    ry   := ARounding.RoundY;
    ropt := ARounding.RoundOptions;
  end;

  { Background color }
  case ABackground.Style of
    bbsClear: backcolor := BGRAPixelTransparent;
    // TODO: Why if I use some system colors like clBtnFace, clActiveCaption etc.
    //       without ColorToRGB, I always get Black? Interface: QT
    bbsColor: backcolor := ColorToBGRA(ColorToRGB(ABackground.Color), ABackground.ColorOpacity);
  end;

  case ABackground.Style of
    bbsClear, bbsColor:
      { Solid background color }
      ATargetBGRA.FillRoundRectAntialias(x1,y1,x2,y2, rx, ry, {%H-}backcolor, ropt);
    bbsGradient:
    begin
      { Using multishape filler to merge background gradient and border }
      multi := TBGRAMultishapeFiller.Create;
      multi.PolygonOrder := poFirstOnTop; { Border will replace background }

      { Gradients }
      back := TBGRABitmap.Create(ATargetBGRA.Width, ATargetBGRA.Height, BGRAPixelTransparent);
      grect1 := rect(floor(x1),floor(y1),ceil(x2)+1,ceil(y2)+1);
      grect2 := grect1;
      { Gradient 1 }
      if ABackground.Gradient1EndPercent > 0 then
      begin
        grect1.Bottom := grect1.top + Round(((grect1.Bottom-grect1.Top) / 100) * ABackground.Gradient1EndPercent);
        gra := CreateGradient(ABackground.Gradient1, grect1);
        back.FillRect(grect1.Left, grect1.Top, grect1.Right, grect1.Bottom,
          gra, dmSet
          );
        gra.Free;
      end;
      { Gradient 2 }
      if ABackground.Gradient1EndPercent < 100 then
      begin
        grect2.Top := grect1.Bottom;
        gra := CreateGradient(ABackground.Gradient2, grect2);
        back.FillRect(grect2.Left, grect2.Top, grect2.Right, grect2.Bottom,
          gra, dmSet
          );
        gra.Free;
      end;

      multi.AddRoundRectangle(x1,y1,x2,y2, rx, ry, back, ropt);

      multi.Draw(ATargetBGRA);
      multi.Free;
      back.Free;
    end;
  end;
end;

procedure RenderBackground(const ARect: TRect; ABackground: TBCBackground;
  ATargetBGRA: TBGRABitmap; ARounding: TBCRounding = nil; AHasNoBorder: boolean = false);
var
  extraSize: single;
begin
  if AHasNoBorder then extraSize := 0.5
    else extraSize := 0;
  RenderBackgroundF(ARect.Left-extraSize, ARect.Top-extraSize, ARect.Right-1+extraSize,
        ARect.Bottom-1+extraSize,ABackground,ATargetBGRA,ARounding);
end;

end.

