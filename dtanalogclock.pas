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
unit DTAnalogClock;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF}
  Forms, Controls, Graphics, Dialogs, ExtCtrls,
  {$IFNDEF FPC}Types, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCBaseCtrls, BGRABitmap, BGRABitmapTypes, BGRAGradients;

type
  TClockStyle = (stlBlue, stlGreen, stlWhite);

  { TDTCustomAnalogClock }

  TDTCustomAnalogClock = class(TBGRAGraphicCtrl)
  private
    FClockStyle: TClockStyle;
    FBitmap: TBGRABitmap;
    FClockFace: TBGRABitmap;
    FEnabled: boolean;
    FMovingParts: TBGRABitmap;
    FTimer: TTimer;
    FResized: boolean;
    procedure SetClockStyle(AValue: TClockStyle);
    { Private declarations }
  protected
    procedure SetEnabled(AValue: boolean); override;
    { Protected declarations }
    procedure Paint; override;
    procedure DrawClock; virtual;
    procedure DrawClockFace; virtual;
    procedure DrawMovingParts; virtual;
    procedure SwitchTimer;

    procedure TimerEvent({%H-}Sender: TObject);
    procedure ResizeEvent({%H-}Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Enabled: boolean read FEnabled write SetEnabled;// default False;
  end;

  TDTAnalogClock = class(TDTCustomAnalogClock)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    //property ClockStyle;
    property Enabled;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

{ TDTCustomAnalogClock }

constructor TDTCustomAnalogClock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  OnResize := ResizeEvent;

  Width := 128;
  Height := 128;

  FBitmap := TBGRABitmap.Create;
  FClockFace := TBGRABitmap.Create;
  FMovingParts := TBGRABitmap.Create;

  FBitmap.SetSize(Width, Height);

  DrawClockFace;
  DrawMovingParts;


  FTimer := TTimer.Create(Self);
  FTimer.Interval := 1000;
  FTimer.Enabled := FEnabled;
  FTimer.OnTimer := TimerEvent;

end;

destructor TDTCustomAnalogClock.Destroy;
begin
  FTimer.Enabled:=False;
  FTimer.OnTimer:=nil;
  FBitmap.Free;
  FClockFace.Free;
  FMovingParts.Free;

  inherited Destroy;
end;

procedure TDTCustomAnalogClock.DrawClock;
begin

end;

procedure TDTCustomAnalogClock.DrawClockFace;
var
  img: TBGRABitmap;
  A: integer;
  w, h, r, Xo, Yo, X, Y, Xt, Yt: integer;
  phong: TPhongShading;
begin
  w := Width;
  h := Height;

  { Set center point }
  Xo := w div 2;
  Yo := h div 2;

  // Determine radius. If canvas is rectangular then r = shortest length w or h
  r := yo;

  if xo > yo then
    r := yo;

  if xo < yo then
    r := xo;

  img := TBGRABitmap.Create(w, h);

  // Draw Bitmap frame
  img.FillEllipseAntialias(Xo, Yo, r * 0.99, r * 0.99, BGRA(175, 175, 175));

  // Draw Rounded/RIng type border using shading
  phong := TPhongShading.Create;
  phong.LightPosition := point(Xo, Yo);
  phong.DrawSphere(img, rect(round(Xo - r * 0.98), round(Yo - r * 0.98), round(Xo + r * 0.98) + 1, round(Yo + r * 0.98) + 1), 4, BGRA(245, 245, 245));
  phong.Free;
  img.EllipseAntialias(Xo, Yo, r * 0.99, r * 0.99, ColorToBGRA(clBlack, 110), 1);

  img.FillEllipseLinearColorAntialias(Xo, Yo, r * 0.88, r * 0.88, BGRA(0, 58, 81), BGRA(2, 94, 131));

  // Draw Face frame
  img.FillEllipseAntialias(Xo, Yo, r * 0.90, r * 0.90, BGRA(175, 175, 175));

  // Draw face background
  img.FillEllipseLinearColorAntialias(Xo, Yo, r * 0.88, r * 0.88, BGRA(0, 58, 81), BGRA(2, 94, 131));

  // Draw Bitmap face
  for A := 1 to 12 do
  begin
    X := Xo + Round(r * 0.80 * sin(30 * A * Pi / 180));
    Y := Yo - Round(r * 0.80 * cos(30 * A * Pi / 180));
    Xt := Xo + Round(r * 0.70 * sin(30 * A * Pi / 180));
    Yt := Yo - Round(r * 0.70 * cos(30 * A * Pi / 180));
    img.EllipseAntialias(x, y, (r * 0.02), (r * 0.02), BGRA(255, 255, 255, 200), 2, BGRA(2, 94, 131));

    img.FontName := 'Calibri';
    img.FontHeight := r div 8;
    img.FontQuality := fqFineAntialiasing;
    img.TextOut(Xt, Yt - (img.FontHeight / 1.7), IntToStr(A), BGRA(245, 245, 245), taCenter);
  end;

  FClockFace.Fill(BGRA(0, 0, 0, 0));
  FClockFace.Assign(img);

  img.Free;

end;

procedure TDTCustomAnalogClock.DrawMovingParts;
var
  img: TBGRABitmap;
  w, h, r, Xo, Yo: integer;
  Xs, Ys, Xm, Ym, Xh, Yh: integer;
  th, tm, ts, tn: word;
begin

  w := Width;
  h := Height;

  { Set center point }
  Xo := w div 2;
  Yo := h div 2;

  // Determine radius. If canvas is rectangular then r = shortest length w or h
  r := yo;

  if xo > yo then
    r := yo;

  if xo < yo then
    r := xo;

  //// Convert current time to integer values
  decodetime(Time, th, tm, ts, tn);

  //{ Set coordinates (length of arm) for seconds }
  Xs := Xo + Round(r * 0.78 * Sin(ts * 6 * Pi / 180));
  Ys := Yo - Round(r * 0.78 * Cos(ts * 6 * Pi / 180));

  //{ Set coordinates (length of arm) for minutes }
  Xm := Xo + Round(r * 0.68 * Sin(tm * 6 * Pi / 180));
  Ym := Yo - Round(r * 0.68 * Cos(tm * 6 * Pi / 180));

  //{ Set coordinates (length of arm) for hours }
  Xh := Xo + Round(r * 0.50 * Sin((th * 30 + tm / 2) * Pi / 180));
  Yh := Yo - Round(r * 0.50 * Cos((th * 30 + tm / 2) * Pi / 180));

  img := TBGRABitmap.Create(w, h);

  // Draw time hands
  img.DrawLineAntialias(xo, yo, xs, ys, BGRA(255, 0, 0), r * 0.02);
  img.DrawLineAntialias(xo, yo, xm, ym, BGRA(245, 245, 245), r * 0.03);
  img.DrawLineAntialias(xo, yo, xh, yh, BGRA(245, 245, 245), r * 0.07);
  img.DrawLineAntialias(xo, yo, xh, yh, BGRA(2, 94, 131), r * 0.04);

  // Draw Bitmap centre dot
  img.EllipseAntialias(Xo, Yo, r * 0.04, r * 0.04, BGRA(245, 245, 245, 255), r * 0.02, BGRA(210, 210, 210, 255));

  // Clear bitmap first
  FMovingParts.Fill(BGRA(0, 0, 0, 0));
  FMovingParts.Assign(img);

  img.Free;
end;

procedure TDTCustomAnalogClock.SwitchTimer;
begin
  FTimer.Enabled := Enabled;
end;

procedure TDTCustomAnalogClock.Paint;
begin
  inherited Paint;

  FBitmap.SetSize(Width, Height);
  FBitMap.Fill(BGRA(0, 0, 0, 0));

  if FResized then
  begin
    DrawClockFace;
    DrawMovingParts;
    FResized := False;
  end;

  FBitmap.BlendImage(0, 0, FClockFace, boLinearBlend);
  FBitmap.BlendImage(0, 0, FMovingParts, boLinearBlend);

  FBitmap.Draw(Canvas, 0, 0, False);

end;

procedure TDTCustomAnalogClock.ResizeEvent(Sender: TObject);
begin
  FResized := True;
end;

procedure TDTCustomAnalogClock.SetClockStyle(AValue: TClockStyle);
begin
  if FClockStyle = AValue then
    Exit;
  FClockStyle := AValue;
end;

procedure TDTCustomAnalogClock.SetEnabled(AValue: boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
  SwitchTimer;
end;

procedure TDTCustomAnalogClock.TimerEvent(Sender: TObject);
begin
  DrawMovingParts;
  Refresh;
end;

{$IFDEF FPC}
procedure Register;
begin
  //{$I icons\dtanalogclock_icon.lrs}
  RegisterComponents('BGRA Controls', [TDTAnalogClock]);
end;
{$ENDIF}


end.
