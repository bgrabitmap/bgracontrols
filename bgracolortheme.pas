unit BGRAColorTheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, BGRATheme,
  BGRABitmap, BGRABitmapTypes;

type

  { TBGRAColorTheme }

  TBGRAColorTheme = class(TBGRATheme)
  private
    FColorActive: TColor;
    FColorDisabled: TColor;
    FColorFocused: TColor;
    FColorHover: TColor;
    FColorNormal: TColor;
    FColorText: TColor;
    procedure SetFColorActive(AValue: TColor);
    procedure SetFColorDisabled(AValue: TColor);
    procedure SetFColorFocused(AValue: TColor);
    procedure SetFColorHover(AValue: TColor);
    procedure SetFColorNormal(AValue: TColor);
    procedure SetFColorText(AValue: TColor);

  protected

  public
    procedure DrawButton(Caption: string; State: TBGRAThemeButtonState;
      Focused: boolean; ARect: TRect; DestCanvas: TCanvas); override;
    procedure DrawRadioButton(Caption: string; State: TBGRAThemeButtonState;
      {%H-}Focused: boolean; Checked: boolean; ARect: TRect; DestCanvas: TCanvas); override;
  published
    property ColorNormal: TColor read FColorNormal write SetFColorNormal;
    property ColorHover: TColor read FColorHover write SetFColorHover;
    property ColorActive: TColor read FColorActive write SetFColorActive;
    property ColorDisabled: TColor read FColorDisabled write SetFColorDisabled;
    property ColorFocused: TColor read FColorFocused write SetFColorFocused;
    property ColorText: TColor read FColorText write SetFColorText;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Themes', [TBGRAColorTheme]);
end;

{ TBGRAColorTheme }

procedure TBGRAColorTheme.SetFColorActive(AValue: TColor);
begin
  if FColorActive = AValue then
    Exit;
  FColorActive := AValue;
end;

procedure TBGRAColorTheme.SetFColorDisabled(AValue: TColor);
begin
  if FColorDisabled = AValue then
    Exit;
  FColorDisabled := AValue;
end;

procedure TBGRAColorTheme.SetFColorFocused(AValue: TColor);
begin
  if FColorFocused = AValue then
    Exit;
  FColorFocused := AValue;
end;

procedure TBGRAColorTheme.SetFColorHover(AValue: TColor);
begin
  if FColorHover = AValue then
    Exit;
  FColorHover := AValue;
end;

procedure TBGRAColorTheme.SetFColorNormal(AValue: TColor);
begin
  if FColorNormal = AValue then
    Exit;
  FColorNormal := AValue;
end;

procedure TBGRAColorTheme.SetFColorText(AValue: TColor);
begin
  if FColorText = AValue then
    Exit;
  FColorText := AValue;
end;

procedure TBGRAColorTheme.DrawButton(Caption: string; State: TBGRAThemeButtonState;
  Focused: boolean; ARect: TRect; DestCanvas: TCanvas);
var
  Style: TTextStyle;
begin
  case State of
    btbsNormal: DestCanvas.Brush.Color := ColorNormal;
    btbsHover: DestCanvas.Brush.Color := ColorHover;
    btbsActive: DestCanvas.Brush.Color := ColorActive;
    btbsDisabled: DestCanvas.Brush.Color := ColorDisabled;
  end;

  DestCanvas.Pen.Color := DestCanvas.Brush.Color;
  DestCanvas.Rectangle(ARect);

  if Focused then
  begin
    DestCanvas.Pen.Color := ColorFocused;
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
    if ColorText <> clDefault then
      DestCanvas.Font.Color := ColorText;
    DestCanvas.TextRect(ARect, 0, 0, Caption, Style);
  end;
end;

procedure TBGRAColorTheme.DrawRadioButton(Caption: string;
  State: TBGRAThemeButtonState; Focused: boolean; Checked: boolean;
  ARect: TRect; DestCanvas: TCanvas);
var
  Style: TTextStyle;
  Bitmap: TBGRABitmap;
  Color: TBGRAPixel;
begin
  DestCanvas.Font.Color := ColorText;
  case State of
    btbsHover: Color := ColorHover;
    btbsActive: Color := ColorActive;
    btbsDisabled:
    begin
      DestCanvas.Font.Color := ColorDisabled;
      Color := ColorDisabled;
    end;
  else {btbsNormal}
    Color := ColorNormal;
  end;

  Bitmap := TBGRABitmap.Create(ARect.Height, ARect.Height);
  Bitmap.FillEllipseAntialias(Bitmap.Height / 2, Bitmap.Height / 2,
    Bitmap.Height / 2 - 2, Bitmap.Height / 2 - 2, BGRAWhite);
  Bitmap.EllipseAntialias(Bitmap.Height / 2, Bitmap.Height / 2,
    Bitmap.Height / 2 - 2, Bitmap.Height / 2 - 2, Color{%H-}, 1);
  if Checked then
    Bitmap.FillEllipseAntialias(Bitmap.Height / 2, Bitmap.Height /
      2, Bitmap.Height / 4, Bitmap.Height / 4, Color);
  Bitmap.Draw(DestCanvas, Arect.Left, Arect.Top, False);
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

end.
