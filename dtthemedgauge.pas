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
unit dtthemedgauge;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, Graphics, {$IFDEF FPC}LResources,{$ELSE} BGRAGraphics, {$ENDIF} Forms, Controls, Dialogs, DTAnalogCommon,
  BGRABitmap, BGRABitmapTypes;

type

  { TDTCustomThemedGauge }

  TDTCustomThemedGauge = class(TDTBaseAnalogDevice)
  private
    FPointerCapSettings: TDTPointerCapSettings;
    FPointerSettings: TDTPointerSettings;
    FScaleBitmap: TBGRABitmap;
    FPointerBitmap: TBGRABitmap;
    FPosition: integer;
    procedure SetPointerCapSettings(AValue: TDTPointerCapSettings);
    procedure SetPointerSettings(AValue: TDTPointerSettings);
    procedure SetPosition(AValue: integer);
    { Private declarations }
  protected
    { Protected declarations }
    property PointerSettings: TDTPointerSettings read FPointerSettings write SetPointerSettings;
    property PointerCapSettings: TDTPointerCapSettings read FPointerCapSettings write SetPointerCapSettings;
    property Position: integer read FPosition write SetPosition;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure DrawScale;
    procedure DrawPointer;
  end;

  { TDTThemedGauge }

  TDTThemedGauge = class(TDTCustomThemedGauge)
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
    property PointerSettings;
    property PointerCapSettings;
    property Position;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

{$IFDEF FPC}
procedure Register;
begin
  //{$I icons\dtthemedgauge_icon.lrs}
  RegisterComponents('BGRA Controls', [TDTThemedGauge]);
end;
{$ENDIF}

{ TDTCustomThemedGauge }

//procedure TDTCustomThemedGauge.SetNeedleSettings(AValue: TDTNeedleSettings);
//begin
//  if FNeedleSettings = AValue then
//    Exit;
//  FNeedleSettings := AValue;
//
//  DoChange(self);
//end;
//
procedure TDTCustomThemedGauge.SetPointerCapSettings(AValue: TDTPointerCapSettings);
begin
  if FPointerCapSettings = AValue then
    Exit;
  FPointerCapSettings := AValue;

  DoChange(self);
end;

procedure TDTCustomThemedGauge.SetPointerSettings(AValue: TDTPointerSettings);
begin
  if FPointerSettings = AValue then
    Exit;
  FPointerSettings := AValue;

  DoChange(self);
end;


procedure TDTCustomThemedGauge.SetPosition(AValue: integer);
begin
  if FPosition = AValue then
    Exit;
  FPosition := AValue;

  DoChange(self);
end;

constructor TDTCustomThemedGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPointerSettings := TDTPointerSettings.Create;
  FPointerSettings.OnChange := DoChange;
  FPointerSettings.Color := BGRA(255, 81, 81);

  FPointerCapSettings := TDTPointerCapSettings.Create;
  FPointerCapSettings.OnChange := DoChange;

  FScaleBitmap := TBGRABitmap.Create;
  FPointerBitmap := TBGRABitmap.Create;

end;

destructor TDTCustomThemedGauge.Destroy;
begin
  FPointerCapSettings.OnChange:=nil;
  FPointerCapSettings.Free;
  FPointerSettings.OnChange:=nil;
  FPointerSettings.Free;

  FScaleBitmap.Free;
  FPointerBitmap.Free;
  inherited Destroy;
end;

procedure TDTCustomThemedGauge.Paint;
begin
  inherited Paint;
  DrawScale;
  DrawPointer;

  FGaugeBitmap.BlendImage(0, 0, FScaleBitmap, boLinearBlend);
  FGaugeBitmap.BlendImage(0, 0, FPointerBitmap, boLinearBlend);

  FGaugeBitmap.Draw(Canvas, 0, 0, False);

end;

procedure TDTCustomThemedGauge.DrawScale;
var
  Origin: TDTOrigin;
  r, i, n, x, y, xt, yt: integer;
  j: single;
begin

  Origin := Initializebitmap(FScaleBitmap, Width, Height);

  r := round(Origin.Radius * 0.85);

  //j := (180 - ScaleSettings.Angle) / 2;
  j := (180 - 270) / 2;

  // Draw SubTicks
  if ScaleSettings.EnableSubTicks then
  begin

    n := ScaleSettings.MainTickCount * ScaleSettings.SubTickCount;

    for i := 0 to n do
    begin
      // Calculate draw from point
      X := Origin.CenterPoint.x - Round(r * cos((j + i * 270 / n) * Pi / 180));
      Y := Origin.CenterPoint.y - Round(r * sin((j + i * 270 / n) * Pi / 180));

      // Calculate draw to point
      Xt := Origin.CenterPoint.x - Round((r - ScaleSettings.LengthSubTick) * cos((j + i * 270 / n) * Pi / 180));
      Yt := Origin.CenterPoint.y - Round((r - ScaleSettings.LengthSubTick) * sin((j + i * 270 / n) * Pi / 180));

      FScaleBitmap.DrawLineAntialias(x, y, xt, yt, ScaleSettings.TickColor, ScaleSettings.ThicknessSubTick);

    end;
  end;

  if ScaleSettings.EnableMainTicks then
  begin

    FScaleBitmap.FontName := ScaleSettings.TextFont;
    FScaleBitmap.FontHeight := ScaleSettings.TextSize;
    FScaleBitmap.FontQuality := fqFineAntialiasing;

    n := ScaleSettings.MainTickCount;

    for i := 0 to n do
    begin
      // Draw main ticks
      // Calculate draw from point
      X := Origin.CenterPoint.x - Round(r * cos((j + i * 270 / n) * Pi / 180));
      Y := Origin.CenterPoint.y - Round(r * sin((j + i * 270 / n) * Pi / 180));

      // Calculate draw to point
      Xt := Origin.CenterPoint.x - Round((r - ScaleSettings.LengthMainTick) * cos((j + i * 270 / n) * Pi / 180));
      Yt := Origin.CenterPoint.y - Round((r - ScaleSettings.LengthMainTick) * sin((j + i * 270 / n) * Pi / 180));

      FScaleBitmap.DrawLineAntialias(x, y, xt, yt, ScaleSettings.TickColor, ScaleSettings.ThicknessMainTick);

      if ScaleSettings.EnableScaleText then
      begin
        // Draw text for main ticks
        Xt := Origin.CenterPoint.x - Round(ScaleSettings.TextRadius * cos((j + i * 270 / n) * Pi / 180));
        Yt := Origin.CenterPoint.y - Round(ScaleSettings.TextRadius * sin((j + i * 270 / n) * Pi / 180));

        FScaleBitmap.TextOut(Xt, Yt - (FScaleBitmap.FontHeight / 1.7), IntToStr(i * ScaleSettings.Maximum div ScaleSettings.MainTickCount), ScaleSettings.TextColor, taCenter);
      end;
    end;
  end;
end;

procedure TDTCustomThemedGauge.DrawPointer;
var
  Origin: TDTOrigin;
  {%H-}r, x, y: integer;
  j: single;
begin

  Origin := Initializebitmap(FPointerBitmap, Width, Height);

  r := round(Origin.Radius * 0.85);

  j := (180 - 270) / 2;

  X := origin.CenterPoint.x - Round(PointerSettings.Length * cos((j + Position * 270 / ScaleSettings.Maximum) * Pi / 180));
  Y := origin.CenterPoint.y - Round(PointerSettings.Length * sin((j + Position * 270 / ScaleSettings.Maximum) * Pi / 180));

  FPointerBitmap.DrawLineAntialias(origin.CenterPoint.y, origin.CenterPoint.y, x, y, PointerSettings.Color, PointerSettings.Thickness);

  // Draw cap over needle
  FPointerBitmap.EllipseAntialias(origin.CenterPoint.x, origin.CenterPoint.y, PointerCapSettings.Radius, PointerCapSettings.Radius, PointerCapSettings.EdgeColor, 2, ColorToBGRA(PointerCapSettings.FillColor));
end;

end.
