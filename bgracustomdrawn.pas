unit BGRACustomDrawn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, FPCanvas, Graphics, Controls, Math, LazUTF8,
  { CustomDrawn }
  CustomDrawnControls, CustomDrawnDrawers, CustomDrawn_Common,
  { BGRABitmap }
  BGRABitmap, BGRABitmapTypes, BGRAGradients;

type
  TBCDButton = class(TCDButton)

  end;

  TBCDEdit = class(TCDEdit)

  end;

  TBCDStaticText = class(TCDStaticText)

  end;

  { TBCDProgressBar }

  TBCDProgressBar = class(TCDProgressBar)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TBCDSpinEdit = class(TCDSpinEdit)

  end;

  { TBGRADrawer }

  TBGRADrawer = class(TCDDrawerCommon)
  private
    FRandSeed: integer;
  protected
    procedure AssignFont(Bitmap: TBGRABitmap; Font: TFont);
  public
    constructor Create; override;
    { Button }
    procedure DrawButton(ADest: TFPCustomCanvas; ADestPos: TPoint;
      ASize: TSize; AState: TCDControlState; AStateEx: TCDButtonStateEx); override;
    { Edit }
    procedure DrawEditBackground(ADest: TCanvas; ADestPos: TPoint;
      ASize: TSize; AState: TCDControlState; AStateEx: TCDEditStateEx); override;
    procedure DrawEditFrame(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;
    procedure DrawCaret(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;
    procedure DrawEdit(ADest: TCanvas; ASize: TSize; AState: TCDControlState;
      AStateEx: TCDEditStateEx); override;
    { Panel }
    procedure DrawPanel(ADest: TCanvas; ASize: TSize; AState: TCDControlState;
      AStateEx: TCDPanelStateEx); override;
    { Static Text }
    procedure DrawStaticText(ADest: TCanvas; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    { Progress Bar }
    procedure DrawProgressBar(ADest: TCanvas; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDProgressBarStateEx); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Custom Drawn', [TBCDButton, TBCDEdit,
    TBCDStaticText, TBCDProgressBar, TBCDSpinEdit]);
end;

{ TBCDProgressBar }

constructor TBCDProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := BGRAToColor(BGRA(102, 163, 226));
end;

{ TBGRADrawer }

procedure TBGRADrawer.AssignFont(Bitmap: TBGRABitmap; Font: TFont);
begin
  Bitmap.FontName := Font.Name;
  Bitmap.FontStyle := Font.Style;
  Bitmap.FontHeight := Font.Height;
  Bitmap.FontQuality := fqSystemClearType;
end;

constructor TBGRADrawer.Create;
begin
  inherited Create;
  randomize;
  FRandSeed := randseed;
end;

procedure TBGRADrawer.DrawButton(ADest: TFPCustomCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDButtonStateEx);
var
  Bitmap: TBGRABitmap;
  ts: TSize;
begin
  Bitmap := TBGRABitmap.Create(ASize.cx, ASize.cy);

  if csfEnabled in AState then
  begin
    if csfSunken in AState then
    begin
      { Button Down }
      Bitmap.Rectangle(0, 0, ASize.cx, ASize.cy - 1, BGRA(48, 48, 48),
        BGRA(61, 61, 61), dmSet);
      Bitmap.Rectangle(1, 1, ASize.cx - 1, ASize.cy - 2, BGRA(55, 55, 55),
        BGRA(61, 61, 61), dmSet);
      Bitmap.SetHorizLine(0, ASize.cy - 1, ASize.cx - 1, BGRA(115, 115, 115));
    end
    else
    begin
      if csfMouseOver in AState then
      begin
        { Button Hovered }
        Bitmap.GradientFill(0, 0, ASize.cx, ASize.cy, BGRA(132, 132, 132),
          BGRA(109, 109, 109), gtLinear, PointF(0, 0), PointF(0, ASize.cy), dmSet);
        Bitmap.Rectangle(0, 0, ASize.cx, ASize.cy - 1, BGRA(48, 48, 48), dmSet);
        Bitmap.SetHorizLine(1, 1, ASize.cx - 2, BGRA(160, 160, 160));
        Bitmap.SetHorizLine(0, ASize.cy - 1, ASize.cx - 1, BGRA(115, 115, 115));
      end
      else
      begin
        { Button Normal }
        Bitmap.GradientFill(0, 0, ASize.cx, ASize.cy, BGRA(107, 107, 107),
          BGRA(84, 84, 84), gtLinear, PointF(0, 0), PointF(0, ASize.cy), dmSet);
        Bitmap.Rectangle(0, 0, ASize.cx, ASize.cy - 1, BGRA(48, 48, 48), dmSet);
        Bitmap.SetHorizLine(1, 1, ASize.cx - 2, BGRA(130, 130, 130));
        Bitmap.SetHorizLine(0, ASize.cy - 1, ASize.cx - 1, BGRA(115, 115, 115));
        { Button Focused }
        if csfHasFocus in AState then
          Bitmap.Rectangle(1, 2, ASize.cx - 1, ASize.cy - 2, BGRA(80, 111, 172), dmSet);
      end;
    end;
  end
  else
  begin
    { Button Disabled }
    Bitmap.Rectangle(0, 0, ASize.cx, ASize.cy - 1, BGRA(66, 66, 66),
      BGRA(71, 71, 71), dmSet);
    Bitmap.SetHorizLine(0, ASize.cy - 1, ASize.cx - 1, BGRA(94, 94, 94));
  end;

  AssignFont(Bitmap, AStateEx.Font);
  ts := Bitmap.TextSize(AStateEx.Caption);

  if csfEnabled in AState then
  begin
    { Text Enabled }
    Bitmap.TextOut((ASize.cx - ts.cx) div 2, ((ASize.cy - ts.cy) div 2) -
      1, AStateEx.Caption, BGRA(47, 47, 47));
    Bitmap.TextOut((ASize.cx - ts.cx) div 2, (ASize.cy - ts.cy) div 2,
      AStateEx.Caption, BGRA(229, 229, 229));
  end
  else
    { Text Disabled }
    Bitmap.TextOut((ASize.cx - ts.cx) div 2, (ASize.cy - ts.cy) div 2,
      AStateEx.Caption, BGRA(170, 170, 170));

  Bitmap.Draw(TCanvas(ADest), ADestPos.x, ADestPos.y, True);
  Bitmap.Free;
end;

procedure TBGRADrawer.DrawEditBackground(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDEditStateEx);
var
  Bitmap: TBGRABitmap;
begin
  Bitmap := TBGRABitmap.Create(ASize.cx, ASize.cy);

  if csfEnabled in AState then
    if csfHasFocus in AState then
      { Focused }
      Bitmap.Fill(BGRAWhite)
    else
      { Normal }
      Bitmap.Rectangle(1, 1, ASize.cx - 1, ASize.cy - 1, BGRA(41, 41, 41),
        BGRA(58, 58, 58), dmSet)
  else
    { Disabled }
    Bitmap.Rectangle(1, 1, ASize.cx - 1, ASize.cy - 1, BGRA(66, 66, 66),
      BGRA(71, 71, 71), dmSet);

  Bitmap.Draw(TCanvas(ADest), ADestPos.x, ADestPos.y, True);
  Bitmap.Free;
end;

procedure TBGRADrawer.DrawEditFrame(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDEditStateEx);
var
  Bitmap: TBGRABitmap;
begin
  Bitmap := TBGRABitmap.Create(ASize.cx, ASize.cy);

  if csfEnabled in AState then
  begin
    if csfHasFocus in AState then
    begin
      { Focused }
      Bitmap.Rectangle(0, 0, ASize.cx, ASize.cy, BGRA(80, 111, 172), dmSet);
      Bitmap.Rectangle(1, 1, ASize.cx - 1, ASize.cy - 1, BGRA(41, 41, 41), dmSet);
    end
    else
    begin
      { Normal }
      Bitmap.Rectangle(0, 0, ASize.cx, ASize.cy, BGRA(83, 83, 83), dmSet);
      Bitmap.Rectangle(1, 1, ASize.cx - 1, ASize.cy - 1, BGRA(41, 41, 41), dmSet);
      Bitmap.SetHorizLine(1, ASize.cy - 1, ASize.cx - 2, BGRA(105, 105, 105));
    end;
  end
  else
  begin
    { Disabled }
    Bitmap.Rectangle(0, 0, ASize.cx, ASize.cy, BGRA(83, 83, 83), dmSet);
    Bitmap.Rectangle(1, 1, ASize.cx - 1, ASize.cy - 1, BGRA(66, 66, 66), dmSet);
    Bitmap.SetHorizLine(1, ASize.cy - 1, ASize.cx - 2, BGRA(94, 94, 94));
  end;

  Bitmap.Draw(TCanvas(ADest), ADestPos.x, ADestPos.y, False);
  Bitmap.Free;
end;

procedure TBGRADrawer.DrawCaret(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDEditStateEx);
begin
  inherited DrawCaret(ADest, ADestPos, ASize, AState, AStateEx);
end;

procedure TBGRADrawer.DrawEdit(ADest: TCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDEditStateEx);
var
  lVisibleText, lControlText: TCaption;
  lSelLeftPos, lSelLeftPixelPos, lSelLength, lSelRightPos: integer;
  lTextWidth, lLineHeight, lLineTop: integer;
  lControlTextLen: PtrInt;
  lTextLeftSpacing, lTextTopSpacing, lTextBottomSpacing: integer;
  lTextColor: TColor;
  i, lVisibleLinesCount: integer;
begin
  // Background
  DrawEditBackground(ADest, Point(0, 0), ASize, AState, AStateEx);

  // General text configurations which apply to all lines
  // Configure the text color
  if csfEnabled in AState then
    lTextColor := $00E5E5E5
  else
    lTextColor := $00AAAAAA;
  if csfHasFocus in AState then
    lTextColor := clBlack;

  ADest.Brush.Style := bsClear;
  ADest.Font.Assign(AStateEx.Font);
  ADest.Font.Color := lTextColor;
  lTextLeftSpacing := GetMeasures(TCDEDIT_LEFT_TEXT_SPACING);
  //lTextRightSpacing := GetMeasures(TCDEDIT_RIGHT_TEXT_SPACING);
  lTextTopSpacing := GetMeasures(TCDEDIT_TOP_TEXT_SPACING);
  lTextBottomSpacing := GetMeasures(TCDEDIT_BOTTOM_TEXT_SPACING);

  lLineHeight := ADest.TextHeight(cddTestStr) + 2;
  lLineHeight := Min(ASize.cy - lTextBottomSpacing, lLineHeight);

  // Fill this to be used in other parts
  AStateEx.LineHeight := lLineHeight;
  AStateEx.FullyVisibleLinesCount := ASize.cy - lTextTopSpacing - lTextBottomSpacing;
  AStateEx.FullyVisibleLinesCount := AStateEx.FullyVisibleLinesCount div lLineHeight;
  AStateEx.FullyVisibleLinesCount :=
    Min(AStateEx.FullyVisibleLinesCount, AStateEx.Lines.Count);

  // Calculate how many lines to draw
  if AStateEx.Multiline then
    lVisibleLinesCount := AStateEx.FullyVisibleLinesCount + 1
  else
    lVisibleLinesCount := 1;
  lVisibleLinesCount := Min(lVisibleLinesCount, AStateEx.Lines.Count);

  // Now draw each line
  for i := 0 to lVisibleLinesCount - 1 do
  begin
    lControlText := AStateEx.Lines.Strings[AStateEx.VisibleTextStart.Y + i];
    lControlText := VisibleText(lControlText, AStateEx.PasswordChar);
    lControlTextLen := UTF8Length(lControlText);
    lLineTop := lTextTopSpacing + i * lLineHeight;

    // The text
    ADest.Pen.Style := psClear;
    ADest.Brush.Style := bsClear;
    // ToDo: Implement multi-line selection
    if (AStateEx.SelLength = 0) or (AStateEx.SelStart.Y <>
      AStateEx.VisibleTextStart.Y + i) then
    begin
      lVisibleText := UTF8Copy(lControlText, AStateEx.VisibleTextStart.X,
        lControlTextLen);
      ADest.TextOut(lTextLeftSpacing, lLineTop, lVisibleText);
    end
    // Text and Selection
    else
    begin
      lSelLeftPos := AStateEx.SelStart.X;
      if AStateEx.SelLength < 0 then
        lSelLeftPos := lSelLeftPos + AStateEx.SelLength;

      lSelRightPos := AStateEx.SelStart.X;
      if AStateEx.SelLength > 0 then
        lSelRightPos := lSelRightPos + AStateEx.SelLength;

      lSelLength := AStateEx.SelLength;
      if lSelLength < 0 then
        lSelLength := lSelLength * -1;

      // Text left of the selection
      lVisibleText := UTF8Copy(lControlText, AStateEx.VisibleTextStart.X,
        lSelLeftPos - AStateEx.VisibleTextStart.X + 1);
      ADest.TextOut(lTextLeftSpacing, lLineTop, lVisibleText);
      lSelLeftPixelPos := ADest.TextWidth(lVisibleText) + lTextLeftSpacing;

      // The selection background
      lVisibleText := UTF8Copy(lControlText, lSelLeftPos + 1, lSelLength);
      lTextWidth := ADest.TextWidth(lVisibleText);
      ADest.Brush.Color := $00C3C3C3;
      ADest.Brush.Style := bsSolid;
      ADest.Rectangle(Bounds(lSelLeftPixelPos, lLineTop, lTextWidth, lLineHeight));
      ADest.Brush.Style := bsClear;

      // The selection text
      ADest.Font.Color := clWhite;
      ADest.TextOut(lSelLeftPixelPos, lLineTop, lVisibleText);
      lSelLeftPixelPos := lSelLeftPixelPos + lTextWidth;

      // Text right of the selection
      ADest.Brush.Color := clWhite;
      ADest.Font.Color := lTextColor;
      lVisibleText := UTF8Copy(lControlText, lSelLeftPos + lSelLength +
        1, lControlTextLen);
      ADest.TextOut(lSelLeftPixelPos, lLineTop, lVisibleText);
    end;
  end;

  // And the caret
  DrawCaret(ADest, Point(0, 0), ASize, AState, AStateEx);

  // In the end the frame, because it must be on top of everything
  DrawEditFrame(ADest, Point(0, 0), ASize, AState, AStateEx);
end;

procedure TBGRADrawer.DrawPanel(ADest: TCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDPanelStateEx);
var
  Bitmap: TBGRABitmap;
begin
  Bitmap := TBGRABitmap.Create(ASize.cx, ASize.cy);
  Bitmap.Rectangle(0, 0, ASize.cx, ASize.cy, BGRA(40, 40, 40), BGRA(83, 83, 83), dmSet);
  Bitmap.SetHorizLine(1, 1, ASize.cx - 2, BGRA(106, 106, 106));
  Bitmap.Draw(TCanvas(ADest), ASize.cx, ASize.cy, True);
  Bitmap.Free;
end;

procedure TBGRADrawer.DrawStaticText(ADest: TCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDControlStateEx);
var
  lColor: TColor;
begin
  // Background
  lColor := $00535353; //AStateEx.ParentRGBColor;
  ADest.Brush.Color := lColor;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psClear;
  ADest.FillRect(0, 0, ASize.cx, ASize.cy);

  // Now the text
  ADest.Brush.Style := bsClear;
  ADest.Font.Assign(AStateEx.Font);
  if csfEnabled in AState then
    ADest.Font.Color := $00E5E5E5
  else
    ADest.Font.Color := $00AAAAAA;
  ADest.TextOut(0, 0, AStateEx.Caption);
end;

procedure TBGRADrawer.DrawProgressBar(ADest: TCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDProgressBarStateEx);

  function ApplyLightness(c: TBGRAPixel; lightness: word): TBGRAPixel;
  begin
    Result := GammaCompression(SetLightness(GammaExpansion(c), lightness));
  end;

  procedure DrawBar(Bitmap: TBGRABitmap; bounds: TRect);
  var
    lCol: TBGRAPixel;
  begin
    lCol := ColorToBGRA(ColorToRGB(AStateEx.RGBColor));

    DoubleGradientAlphaFill(Bitmap, bounds,
      ApplyLightness(lCol, 37000), ApplyLightness(lCol, 29000),
      ApplyLightness(lCol, 26000), ApplyLightness(lCol, 18000),
      gdVertical, gdVertical, gdVertical, 0.53);

    InflateRect(bounds, -1, -1);

    DoubleGradientAlphaFill(Bitmap, bounds,
      ApplyLightness(lCol, 28000), ApplyLightness(lCol, 22000),
      ApplyLightness(lCol, 19000), ApplyLightness(lCol, 11000),
      gdVertical, gdVertical, gdVertical, 0.53);
  end;

var
  content: TRect;
  xpos, y, tx, ty: integer;
  grayValue: integer;
  Bitmap: TBGRABitmap;
begin
  Bitmap := TBGRABitmap.Create(ASize.cx, ASize.cy);

  tx := ASize.cx;
  ty := ASize.cy;

  Bitmap.Fill(BGRA(83, 83, 83));
  Bitmap.Rectangle(0, 0, tx, ty, BGRA(255, 255, 255, 6), dmDrawWithTransparency);
  if (tx > 2) and (ty > 2) then
    Bitmap.Rectangle(1, 1, tx - 1, ty - 1, BGRA(29, 29, 29), dmSet);

  if (tx > 4) and (ty > 4) then
  begin
    content := Rect(2, 2, tx - 2, ty - 2);
    randseed := FRandSeed;
    for y := content.Top to content.Bottom - 1 do
    begin
      if y = content.Top then
        grayValue := 33
      else
      if y = content.Top + 1 then
        grayValue := 43
      else
        grayValue := 47 + random(50 - 47 + 1);
      Bitmap.SetHorizLine(content.Left, y, content.Right - 1, BGRA(
        grayValue, grayValue, grayValue));
    end;
    if tx >= 6 then
      Bitmap.DrawVertLine(content.Right - 1, content.Top, content.Bottom - 1,
        BGRA(0, 0, 0, 32));
    xpos := round(AStateEx.PercentPosition * (content.right - content.left)) +
      content.left;
    if xpos > content.left then
    begin
      DrawBar(Bitmap, rect(content.left, content.top, xpos, content.bottom));
      if xpos < content.right then
      begin
        Bitmap.SetPixel(xpos, content.top, BGRA(62, 62, 62));
        Bitmap.SetVertLine(xpos, content.top + 1, content.bottom -
          1, BGRA(40, 40, 40));
      end;
    end;
  end;
  Bitmap.Draw(TCanvas(ADest), 0, 0, True);
  Bitmap.Free;
end;

initialization
  RegisterDrawer(TBGRADrawer.Create, dsCommon);

end.
