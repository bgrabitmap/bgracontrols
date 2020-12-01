// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ General framework methods for rendering background, borders, text, etc.

  originally written in 2012 by Krzysztof Dibowski dibowski at interia.pl
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCTools;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, Types, Graphics,
  {$IFDEF FPC}LCLType, LCLIntf,{$ENDIF} {$IFNDEF FPC}BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BGRABitmap, BGRABitmapTypes, bctypes, Controls, BGRAGradientScanner;

function ScaleRect(ARect: TRect; AScale: Single): TRect;
// This method prepare BGRABitmap for rendering BCFont type
procedure AssignBCFont(AFont: TBCFont; var ATargetBGRA: TBGRABitmap);
// Calculate text height and width (doesn't include wordwrap - just single line)
procedure CalculateTextSize(const AText: String; AFont: TBCFont; out ANewWidth, ANewHeight: integer;
  AShadowMargin: boolean = true);
// Calculate text height and width (handles wordwrap and end ellipsis)
procedure CalculateTextSizeEx(const AText: String; AFont: TBCFont; out ANewWidth, ANewHeight: integer;
  AAvailableWidth: integer; AShadowMargin: boolean = false);
// Determines the layout of the glyph
procedure GetGlyphActualLayout(ACaption: string; AFont: TBCFont;
  AGlyphAlignment: TBCAlignment; AGlyphMargin: integer; out AHorizAlign: TAlignment;
  out AVertAlign: TTextLayout; out AGlyphRelativeHorizAlign: TAlignment;
  out AGlyphRelativeVertAlign: TTextLayout; out AGlyphHorizMargin: integer;
  out AGlyphVertMargin: integer);
// Computes the position the glyph and update rAvail with the space dedicated to text.
// Specify the flag AOldPlacement to have the old (buggy) version
function ComputeGlyphPosition(var rAvail: TRect;
  AGlyph: TBitmap; AGlyphAlignment: TBCAlignment; AGlyphMargin: integer;
  ACaption: string; AFont: TBCFont; AOldPlacement: boolean = false;
  AGlyphScale: Single = 1): TRect; overload;
function ComputeGlyphPosition(var rAvail: TRect;
  gw, gh: integer; AGlyphAlignment: TBCAlignment; AGlyphMargin: integer;
  ACaption: string; AFont: TBCFont; AOldPlacement: boolean = false): TRect; overload;
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

uses BGRAPolygon, BGRAFillInfo, BGRAText, math, BGRAUTF8, LazUTF8;

function ComputeGlyphPosition(var rAvail: TRect; AGlyph: TBitmap;
  AGlyphAlignment: TBCAlignment; AGlyphMargin: integer; ACaption: string;
  AFont: TBCFont; AOldPlacement: boolean; AGlyphScale: Single): TRect;
var gw, gh: integer;
begin
  if Assigned(AGlyph) and not AGlyph.Empty then
  begin
    gw := round(AGlyph.Width * AGlyphScale);
    gh := round(AGlyph.Height * AGlyphScale);
  end else
  begin
    gw := 0;
    gh := 0;
  end;
  result := ComputeGlyphPosition(rAvail, gw, gh, AGlyphAlignment, AGlyphMargin, ACaption,
    AFont, AOldPlacement);
end;

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
  r: TRect;
begin
  if AText = '' then exit;

  AssignBCFont(AFont,ATargetBGRA);

  hal := BCAlign2HAlign(AFont.TextAlignment);
  val := BCAlign2VAlign(AFont.TextAlignment);

  FillChar({%H-}st, SizeOf({%H-}st),0);

  st.Wordbreak   := AFont.WordBreak;
  st.Alignment   := hal;
  st.Layout      := val;
  st.SingleLine  := AFont.SingleLine;
  st.EndEllipsis := AFont.EndEllipsis;
  r := ARect;
  r.Left += AFont.PaddingLeft;
  r.Right -= AFont.PaddingRight;
  r.Top += AFont.PaddingTop;
  r.Bottom -= AFont.PaddingBottom;

  if AFont.Shadow then
  begin
    shd := TBGRABitmap.Create(ATargetBGRA.Width,ATargetBGRA.Height,BGRAPixelTransparent);
    shd.FontName      := ATargetBGRA.FontName;
    shd.FontStyle     := ATargetBGRA.FontStyle;
    shd.FontQuality   := ATargetBGRA.FontQuality;
    shd.FontHeight    := ATargetBGRA.FontHeight;
    shd.TextRect(r, r.Left, r.Top, AText, st, ColorToBGRA(ColorToRGB(AFont.ShadowColor),
      AFont.ShadowColorOpacity));
    BGRAReplace(shd, shd.FilterBlurRadial(AFont.ShadowRadius, rbFast));
    ATargetBGRA.BlendImage(AFont.ShadowOffsetX, AFont.ShadowOffsetY,
      shd, boLinearBlend);
    shd.Free;
  end;

  ATargetBGRA.TextRect(r,r.Left,r.Top,AText,st,AFont.Color);

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

function ScaleRect(ARect: TRect; AScale: Single): TRect;
begin
  with ARect do
    result := rect(round(Left*AScale), round(Top*AScale),
      round(Right*AScale), round(Bottom*AScale));
end;

procedure AssignBCFont(AFont: TBCFont; var ATargetBGRA: TBGRABitmap);
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
    then ATargetBGRA.FontName := string(GetFontData(c.Canvas.Font.Handle).Name)
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
  ANewHeight: integer; AShadowMargin: boolean);
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

  tmp := TBGRABitmap.Create(0,0);
  AssignBCFont(AFont, tmp);
  s := tmp.TextSize(AText);
  tmp.Free;

  { shadow offset }
  if AShadowMargin and AFont.Shadow then
  begin
    Inc(s.cx, 2 * Abs(AFont.ShadowOffsetX) + 2 * AFont.ShadowRadius);
    Inc(s.cy, 2 * Abs(AFont.ShadowOffsetY) + 2 * AFont.ShadowRadius);
  end;

  inc(s.cx, AFont.PaddingLeft+Afont.PaddingRight);
  inc(s.cy, AFont.PaddingTop+Afont.PaddingBottom);

  ANewWidth := s.cx;
  ANewHeight := s.cy;
end;

procedure CalculateTextSizeEx(const AText: String; AFont: TBCFont; out
  ANewWidth, ANewHeight: integer; AAvailableWidth: integer; AShadowMargin: boolean);
var
  s: TSize;
  tmp: TBGRABitmap;
  extraX,extraY, fitCount: integer;
  dotSize: LongInt;
begin
  if (AText = '') or (AFont = nil) then
  begin
    ANewWidth := 0;
    ANewHeight := 0;
    Exit;
  end;

  extraX := 0;
  extraY := 0;
  { shadow offset }
  if AShadowMargin and AFont.Shadow then
  begin
    Inc(extraX, 2 * Abs(AFont.ShadowOffsetX) + 2 * AFont.ShadowRadius);
    Inc(extraY, 2 * Abs(AFont.ShadowOffsetY) + 2 * AFont.ShadowRadius);
  end;

  inc(extraX, AFont.PaddingLeft+Afont.PaddingRight);
  inc(extraY, AFont.PaddingTop+Afont.PaddingBottom);

  dec(AAvailableWidth, extraX);
  tmp := TBGRABitmap.Create(0,0);
  AssignBCFont(AFont, tmp);
  if AFont.WordBreak then
    s := tmp.TextSize(AText, AAvailableWidth)
  else
  begin
    s := tmp.TextSize(AText);
    if AFont.EndEllipsis and (s.cx > AAvailableWidth) then
    begin
      dotSize := tmp.TextSize('...').cx;
      fitCount := tmp.TextFitInfo(AText, AAvailableWidth-dotSize);
      s.cx := tmp.TextSize(UTF8Copy(AText, 1, fitCount)).cx + dotSize;
    end;
  end;
  tmp.Free;

  ANewWidth := s.cx+extraX;
  ANewHeight := s.cy+extraY;
end;

procedure GetGlyphActualLayout(ACaption: string; AFont: TBCFont;
  AGlyphAlignment: TBCAlignment; AGlyphMargin: integer; out AHorizAlign: TAlignment;
  out AVertAlign: TTextLayout; out AGlyphRelativeHorizAlign: TAlignment;
  out AGlyphRelativeVertAlign: TTextLayout; out AGlyphHorizMargin: integer;
  out AGlyphVertMargin: integer);
begin
  if AGlyphAlignment in [bcaLeftTop,bcaLeftCenter,bcaLeftBottom] then AHorizAlign := taLeftJustify
  else if AGlyphAlignment  in [bcaRightTop,bcaRightCenter,bcaRightBottom] then AHorizAlign:= taRightJustify
  else AHorizAlign:= taCenter;
  if AGlyphAlignment in [bcaCenter,bcaLeftCenter,bcaRightCenter] then AVertAlign := tlCenter
  else if AGlyphAlignment in [bcaLeftBottom,bcaCenterBottom,bcaRightBottom] then AVertAlign := tlBottom
  else AVertAlign := tlTop;

  if ACaption<>'' then
  begin
    AGlyphRelativeVertAlign:= AVertAlign;
    if AVertAlign <> tlCenter then
      AGlyphRelativeHorizAlign:= AHorizAlign else
    begin
      if AHorizAlign = taCenter then
      begin
        if IsRightToLeftUTF8(ACaption) then AGlyphRelativeHorizAlign := taRightJustify
        else AGlyphRelativeHorizAlign := taLeftJustify;
      end else
        AGlyphRelativeHorizAlign:= AHorizAlign;
    end;

    if AFont.TextAlignment in [bcaLeftTop,bcaLeftCenter,bcaLeftBottom] then AHorizAlign := taLeftJustify
    else if AFont.TextAlignment in [bcaRightTop,bcaRightCenter,bcaRightBottom] then AHorizAlign:= taRightJustify
    else AHorizAlign := taCenter;
    if AFont.TextAlignment in [bcaLeftTop,bcaCenterTop,bcaRightTop] then AVertAlign := tlTop
    else if AFont.TextAlignment in [bcaLeftBottom,bcaCenterBottom,bcaRightBottom] then AVertAlign:= tlBottom
    else AVertAlign:= tlCenter;

    if AGlyphRelativeVertAlign in[tlTop,tlBottom] then
    begin
      if AGlyphRelativeHorizAlign <> taCenter then AGlyphHorizMargin:= AGlyphMargin
      else AGlyphHorizMargin:= 0;
      if AGlyphRelativeVertAlign = AVertAlign then AGlyphVertMargin:= AGlyphMargin
      else AGlyphVertMargin:= 0;
    end else
    begin
      AGlyphHorizMargin:= AGlyphMargin;
      AGlyphVertMargin:= 0;
    end;
  end else
  begin
    case AHorizAlign of
      taCenter: AGlyphRelativeHorizAlign:= taCenter;
      taRightJustify: AGlyphRelativeHorizAlign:= taLeftJustify;
    else AGlyphRelativeHorizAlign:= taRightJustify;
    end;
    if AHorizAlign <> taCenter then AGlyphHorizMargin := AGlyphMargin
    else AGlyphHorizMargin := 0;
    case AVertAlign of
      tlCenter: AGlyphRelativeVertAlign:= tlCenter;
      tlBottom: AGlyphRelativeVertAlign:= tlTop;
    else AGlyphRelativeVertAlign:= tlBottom;
    end;
    if AVertAlign <> tlCenter then AGlyphVertMargin := AGlyphMargin
    else AGlyphVertMargin := 0;
  end;
end;

function ComputeGlyphPosition(var rAvail: TRect;
  gw, gh: integer; AGlyphAlignment: TBCAlignment; AGlyphMargin: integer;
  ACaption: string; AFont: TBCFont; AOldPlacement: boolean): TRect;
var
  w, h, w2,h2, glyphHorzMargin, glyphVertMargin: integer;
  horizAlign, relHorizAlign: TAlignment;
  vertAlign, relVertAlign: TTextLayout;
  rText, rAll, rGlyph: TRect;
  l,t: integer;

  procedure AlignRect(var ARect: TRect; const ABounds: TRect; AHorizAlign: TAlignment;
    AVertAlign: TTextLayout; AHorizMargin: integer = 0; AVertMargin: integer = 0);
  begin
    case AHorizAlign of
      taCenter: ARect.Offset((ABounds.Left+ABounds.Right - (ARect.Right-ARect.Left)) div 2,0);
      taRightJustify: ARect.Offset(ABounds.Right - AHorizMargin - (ARect.Right-ARect.Left),0);
      else ARect.Offset(ABounds.Left + AHorizMargin,0);
    end;
    case AVertAlign of
      tlCenter: ARect.Offset(0, (ABounds.Top+ABounds.Bottom - (ARect.Bottom-ARect.Top)) div 2);
      tlBottom: ARect.Offset(0, ABounds.Bottom - AVertMargin - (ARect.Bottom-ARect.Top));
      else ARect.Offset(0, ABounds.Top + AVertMargin);
    end;
  end;

begin
  if (gw = 0) or (gh = 0) then exit(EmptyRect);

  if AOldPlacement then
  begin
    if ACaption = '' then
    begin
      w := 0;
      h := 0;
    end else
      CalculateTextSize(ACaption, AFont, w, h);
    l := rAvail.Right - Round(((rAvail.Right - rAvail.Left) + w + gw) / 2);
    t := rAvail.Bottom - Round(((rAvail.Bottom - rAvail.Top) + gh) / 2);
    result := rect(l,t,l+gw,t+gh);
    Inc(rAvail.Left, l + gw + AGlyphMargin);
    exit;
  end;

  GetGlyphActualLayout(ACaption, AFont, AGlyphAlignment, AGlyphMargin,
    horizAlign, vertAlign, relHorizAlign, relVertAlign, glyphHorzMargin, glyphVertMargin);

  if ACaption = '' then
  begin
    rGlyph := rect(0,0,gw,gh);
    AlignRect(rGlyph, rAvail, horizAlign, vertAlign, glyphHorzMargin, glyphVertMargin);
    exit(rGlyph);
  end else
    CalculateTextSizeEx(ACaption, AFont, w, h, rAvail.Right-rAvail.Left);

  if relVertAlign in[tlTop,tlBottom] then
  begin
    w2 := max(w,gw+glyphHorzMargin);
    h2 := h+gh+glyphVertMargin;
  end else
  begin
    w2 := w+gw+glyphHorzMargin;
    if (ACaption <> '') and (w2 > rAvail.Right-rAvail.Left) then
    begin
      CalculateTextSizeEx(ACaption, AFont, w, h, rAvail.Right-rAvail.Left - (gw+glyphHorzMargin));
      w2 := w+gw+glyphHorzMargin;
    end;
    h2 := max(h,gh+glyphVertMargin);
  end;
  rAll := rect(0,0,w2,h2);
  AlignRect(rAll, rAvail, horizAlign, vertAlign);

  rText := rect(0,0,w,h);
  rGlyph := rect(0,0,gw,gh);
  case relVertAlign of
    tlTop: begin
      AlignRect(rGlyph, rAll, relHorizAlign, tlTop,
        glyphHorzMargin, glyphVertMargin);
      AlignRect(rText, rAll, horizAlign, tlBottom);
    end;
    tlBottom: begin
      AlignRect(rGlyph, rAll, relHorizAlign, tlBottom,
        glyphHorzMargin, glyphVertMargin);
      AlignRect(rText, rAll, horizAlign, tlTop);
    end;
    else begin
      if relHorizAlign = taRightJustify then
      begin
        AlignRect(rGlyph, rAll, taRightJustify, tlCenter,
          glyphHorzMargin, glyphHorzMargin);
        AlignRect(rText, rAll, taLeftJustify, tlCenter);
      end else
      begin
        AlignRect(rGlyph, rAll, taLeftJustify, tlCenter,
          glyphHorzMargin, glyphHorzMargin);
        AlignRect(rText, rAll, taRightJustify, tlCenter);
      end;
    end;
  end;
  result := rGlyph;
  if AFont.WordBreak and (rText.Right < rAvail.Right) then inc(rText.Right); //word-break computation may be one pixel off
  rAvail := rText;
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

