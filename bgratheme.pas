unit BGRATheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  TBGRAThemeButtonState = (btbsNormal, btbsHover, btbsActive, btbsDisabled);

  { TBGRATheme }

  TBGRATheme = class(TComponent)
  private

  protected

  public
    procedure DrawButton(Caption: string; State: TBGRAThemeButtonState;
      Focused: boolean; ARect: TRect; DestCanvas: TCanvas); virtual;
  published

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Themes', [TBGRATheme]);
end;

{ TBGRATheme }

procedure TBGRATheme.DrawButton(Caption: string; State: TBGRAThemeButtonState;
  Focused: boolean; ARect: TRect; DestCanvas: TCanvas);
var
  Style: TTextStyle;
begin
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

end.
