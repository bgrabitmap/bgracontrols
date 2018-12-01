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
unit dtthemedclock;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, ExtCtrls, {$IFDEF FPC}LResources,{$ENDIF} Forms, Controls, Graphics, Dialogs, DTAnalogCommon,
  {$IFNDEF FPC}Types, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BGRABitmap, BGRABitmapTypes;

type

  { TDTCustomThemedClock }

  TDTCustomThemedClock = class(TDTBaseAnalogDevice)
  private
    FClockFace: TBGRABitmap;
    FPointerBitmap: TBGRABitmap;
    FEnabled: boolean;
    FHoursPointerSettings: TDTPointerSettings;
    FMinutesPointerSettings: TDTPointerSettings;
    FPointerCapSettings: TDTPointerCapSettings;
    FPosition: integer;
    FSecondsPointerSettings: TDTPointerSettings;
    FTimer: TTimer;
    procedure SetHoursPointerSettings(AValue: TDTPointerSettings);
    procedure SetMinutesPointerSettings(AValue: TDTPointerSettings);
    procedure SetPointerCapSettings(AValue: TDTPointerCapSettings);
    procedure SetPosition(AValue: integer);
    procedure SetSecondsPointerSettings(AValue: TDTPointerSettings);
    { Private declarations }
  protected
    procedure SetEnabled(AValue: boolean); override;
    { Protected declarations }
    property SecondsPointerSettings: TDTPointerSettings read FSecondsPointerSettings write SetSecondsPointerSettings;
    property MinutesPointerSettings: TDTPointerSettings read FMinutesPointerSettings write SetMinutesPointerSettings;
    property HoursPointerSettings: TDTPointerSettings read FHoursPointerSettings write SetHoursPointerSettings;
    property PointerCapSettings: TDTPointerCapSettings read FPointerCapSettings write SetPointerCapSettings;
    property Position: integer read FPosition write SetPosition;
    property Enabled: boolean read FEnabled write SetEnabled;
    procedure TimerEvent({%H-}Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure DrawClock;
    procedure DrawPointers;
  end;

  TDTThemedClock = class(TDTCustomThemedClock)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Public declarations }
    property SecondsPointerSettings;
    property MinutesPointerSettings;
    property HoursPointerSettings;
    property PointerCapSettings;
    property ScaleSettings;
    property Position;
    property Enabled;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

{$IFDEF FPC}
procedure Register;
begin
  //{$I icons\dtthemedclock_icon.lrs}
  RegisterComponents('BGRA Controls', [TDTThemedClock]);
end;
{$ENDIF}

{ TDTCustomThemedClock }

procedure TDTCustomThemedClock.SetPointerCapSettings(AValue: TDTPointerCapSettings);
begin
  if FPointerCapSettings = AValue then
    Exit;
  FPointerCapSettings := AValue;

  DoChange(self);
end;

procedure TDTCustomThemedClock.SetHoursPointerSettings(AValue: TDTPointerSettings);
begin
  if FHoursPointerSettings = AValue then
    Exit;
  FHoursPointerSettings := AValue;

  DoChange(self);
end;

procedure TDTCustomThemedClock.SetEnabled(AValue: boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;

  FTimer.Enabled := FEnabled;

  DoChange(self);
end;

procedure TDTCustomThemedClock.SetMinutesPointerSettings(AValue: TDTPointerSettings);
begin
  if FMinutesPointerSettings = AValue then
    Exit;
  FMinutesPointerSettings := AValue;

  DoChange(self);
end;

procedure TDTCustomThemedClock.SetPosition(AValue: integer);
begin
  if FPosition = AValue then
    Exit;
  FPosition := AValue;

  DoChange(self);
end;

procedure TDTCustomThemedClock.SetSecondsPointerSettings(AValue: TDTPointerSettings);
begin
  if FSecondsPointerSettings = AValue then
    Exit;
  FSecondsPointerSettings := AValue;

  DoChange(self);
end;

procedure TDTCustomThemedClock.TimerEvent(Sender: TObject);
begin
  DrawPointers;
  DoChange(self);
end;

constructor TDTCustomThemedClock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSecondsPointerSettings := TDTPointerSettings.Create;
  FSecondsPointerSettings.OnChange := DoChange;
  FMinutesPointerSettings := TDTPointerSettings.Create;
  FMinutesPointerSettings.OnChange := DoChange;
  FHoursPointerSettings := TDTPointerSettings.Create;
  FHoursPointerSettings.OnChange := DoChange;

  FPointerCapSettings := TDTPointerCapSettings.Create;
  FPointerCapSettings.OnChange := DoChange;

  FClockFace := TBGRABitmap.Create;
  FPointerBitmap := TBGRABitmap.Create;

  FSecondsPointerSettings.Color := BGRA(255, 81, 81);
  FSecondsPointerSettings.Length := 80;
  FSecondsPointerSettings.Thickness := 3;

  FMinutesPointerSettings.Color := BGRA(199, 199, 173);
  FMinutesPointerSettings.Length := 80;
  FMinutesPointerSettings.Thickness := 3;

  FHoursPointerSettings.Color := BGRA(199, 199, 173);
  FHoursPointerSettings.Length := 60;
  FHoursPointerSettings.Thickness := 5;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 1000;
  FTimer.Enabled := FEnabled;
  FTimer.OnTimer := TimerEvent;

end;

destructor TDTCustomThemedClock.Destroy;
begin
  FSecondsPointerSettings.OnChange:=nil;
  FSecondsPointerSettings.Free;
  FMinutesPointerSettings.OnChange:=nil;
  FMinutesPointerSettings.Free;
  FHoursPointerSettings.OnChange:=nil;
  FHoursPointerSettings.Free;
  FTimer.Enabled:=False;
  FTimer.OnTimer:=nil;
  FPointerCapSettings.OnChange:=nil;
  FPointerCapSettings.Free;

  FClockFace.Free;
  FPointerBitmap.Free;

  inherited Destroy;
end;

procedure TDTCustomThemedClock.Paint;
begin
  inherited Paint;

  DrawClock;
  DrawPointers;

  FGaugeBitmap.BlendImage(0, 0, FClockFace, boLinearBlend);
  FGaugeBitmap.BlendImage(0, 0, FPointerBitmap, boLinearBlend);

  FGaugeBitmap.Draw(Canvas, 0, 0, False);
end;

procedure TDTCustomThemedClock.DrawClock;
var
  Origin: TDTOrigin;
  r, i, x, y, xt, yt: integer;
begin

  Origin := Initializebitmap(FClockFace, Width, Height);

  r := round(Origin.Radius * 0.85);

  // Draw minor tick marks
  if ScaleSettings.EnableSubTicks then
  begin
    for i := 1 to 60 do
    begin
      // Calculate draw from point
      X := Origin.CenterPoint.x + Round(r * sin(6 * i * Pi / 180));
      Y := Origin.CenterPoint.y - Round(r * cos(6 * i * Pi / 180));

      // Calculate draw to point
      xt := Origin.CenterPoint.x + Round((r - ScaleSettings.LengthSubTick) * sin(6 * i * Pi / 180));
      yt := Origin.CenterPoint.y - Round((r - ScaleSettings.LengthSubTick) * cos(6 * i * Pi / 180));

      FClockFace.DrawLineAntialias(x, y, xt, yt, ScaleSettings.TickColor, ScaleSettings.ThicknessSubTick);
    end;
  end;

  // Draw major tick marks
  if ScaleSettings.EnableMainTicks then
  begin
    for i := 1 to 12 do
    begin
      // Calculate draw from point
      X := Origin.CenterPoint.x + Round(r * sin(30 * i * Pi / 180));
      Y := Origin.CenterPoint.y - Round(r * cos(30 * i * Pi / 180));

      // Calculate draw to point
      xt := Origin.CenterPoint.x + Round((r - ScaleSettings.LengthMainTick) * sin(30 * i * Pi / 180));
      yt := Origin.CenterPoint.y - Round((r - ScaleSettings.LengthMainTick) * cos(30 * i * Pi / 180));

      FClockFace.DrawLineAntialias(x, y, xt, yt, ScaleSettings.TickColor, ScaleSettings.ThicknessMainTick);

      if ScaleSettings.EnableScaleText then
      begin
        FClockFace.FontName := ScaleSettings.TextFont;
        FClockFace.FontHeight := ScaleSettings.TextSize;
        FClockFace.FontQuality := fqFineAntialiasing;

        // Draw text for main ticks
        xt := Origin.CenterPoint.x + Round(ScaleSettings.TextRadius * sin(30 * i * Pi / 180));
        yt := Origin.CenterPoint.y - Round(ScaleSettings.TextRadius * cos(30 * i * Pi / 180));

        FClockFace.TextOut(Xt, Yt - (FClockFace.FontHeight / 1.7), IntToStr(i), ScaleSettings.TextColor, taCenter);
      end;

    end;
  end;

end;

procedure TDTCustomThemedClock.DrawPointers;
var
  Origin: TDTOrigin;
  {%H-}r: integer;
  Xs, Ys, Xm, Ym, Xh, Yh: integer;
  th, tm, ts, tn: word;
begin

  Origin := Initializebitmap(FPointerBitmap, Width, Height);

  r := round(Origin.Radius * 0.85);

  //// Convert current time to integer values
  decodetime(Time, th, tm, ts, tn);

  //{ Set coordinates (length of arm) for seconds }
  Xs := Origin.CenterPoint.x + Round(SecondsPointerSettings.Length * Sin(ts * 6 * Pi / 180));
  Ys := Origin.CenterPoint.y - Round(SecondsPointerSettings.Length * Cos(ts * 6 * Pi / 180));

  //{ Set coordinates (length of arm) for minutes }
  Xm := Origin.CenterPoint.x + Round(MinutesPointerSettings.Length * Sin(tm * 6 * Pi / 180));
  Ym := Origin.CenterPoint.y - Round(MinutesPointerSettings.Length * Cos(tm * 6 * Pi / 180));

  //{ Set coordinates (length of arm) for hours }
  Xh := Origin.CenterPoint.x + Round(HoursPointerSettings.Length * Sin((th * 30 + tm / 2) * Pi / 180));
  Yh := Origin.CenterPoint.y - Round(HoursPointerSettings.Length * Cos((th * 30 + tm / 2) * Pi / 180));

  FPointerBitmap.DrawLineAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y, xs, ys, SecondsPointerSettings.Color, SecondsPointerSettings.Thickness);
  FPointerBitmap.DrawLineAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y, xm, ym, MinutesPointerSettings.Color, MinutesPointerSettings.Thickness);
  FPointerBitmap.DrawLineAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y, xh, yh, HoursPointerSettings.Color, HoursPointerSettings.Thickness);

  // Draw cap over needle
  FPointerBitmap.EllipseAntialias(origin.CenterPoint.x, origin.CenterPoint.y, PointerCapSettings.Radius, PointerCapSettings.Radius, PointerCapSettings.EdgeColor, 2, ColorToBGRA(PointerCapSettings.FillColor));


end;

end.
