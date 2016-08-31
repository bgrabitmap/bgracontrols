unit BGRACustomDrawn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, FPCanvas, Graphics,
  { CustomDrawn }
  CustomDrawnControls, CustomDrawnDrawers, Customdrawn_Common,
  { BGRABitmap }
  BGRABitmap, BGRABitmapTypes;

type
  TBCDButton = class(TCDButton)

  end;

  { TBGRADrawer }

  TBGRADrawer = class(TCDDrawerCommon)
  protected
    procedure AssignFont(Bitmap: TBGRABitmap; Font: TFont);
  public
    procedure DrawButton(ADest: TFPCustomCanvas; ADestPos: TPoint;
      ASize: TSize; AState: TCDControlState; AStateEx: TCDButtonStateEx); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Custom Drawn', [TBCDButton]);
end;

{ TBGRADrawer }

procedure TBGRADrawer.AssignFont(Bitmap: TBGRABitmap; Font: TFont);
begin
  Bitmap.FontName := Font.Name;
  Bitmap.FontStyle := Font.Style;
  Bitmap.FontHeight := Font.Height;
  Bitmap.FontQuality := fqSystemClearType;
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
end;

initialization
  RegisterDrawer(TBGRADrawer.Create, dsCommon);

end.

