unit BGRASVGTheme;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRATheme, BGRABitmap, BGRABitmapTypes, BGRASVG;

type

  { TBGRASVGTheme }

  TBGRASVGTheme = class(TBGRATheme)
  private
    FCheckBoxChecked: string;
    FCheckBoxUnchecked: string;
    FColorizeActive: TBGRAPixel;
    FColorizeDisabled: TBGRAPixel;
    FColorizeHover: TBGRAPixel;
    FColorizeNormal: TBGRAPixel;
    FRadioButtonChecked: string;
    FRadioButtonUnchecked: string;
    procedure SetCheckBoxChecked(AValue: string);
    procedure SetCheckBoxUnchecked(AValue: string);
    procedure SetColorizeActive(AValue: TBGRAPixel);
    procedure SetColorizeDisabled(AValue: TBGRAPixel);
    procedure SetColorizeHover(AValue: TBGRAPixel);
    procedure SetColorizeNormal(AValue: TBGRAPixel);
    procedure SetRadioButtonChecked(AValue: string);
    procedure SetRadioButtonUnchecked(AValue: string);

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawRadioButton(Caption: string; State: TBGRAThemeButtonState;
    {%H-}Focused: boolean; Checked: boolean; ARect: TRect; ASurface: TBGRAThemeSurface); override;
    procedure DrawCheckBox(Caption: string; State: TBGRAThemeButtonState;
    {%H-}Focused: boolean; Checked: boolean; ARect: TRect; ASurface: TBGRAThemeSurface); override;
  public
    property ColorizeNormal: TBGRAPixel read FColorizeNormal write SetColorizeNormal;
    property ColorizeHover: TBGRAPixel read FColorizeHover write SetColorizeHover;
    property ColorizeActive: TBGRAPixel read FColorizeActive write SetColorizeActive;
    property ColorizeDisabled: TBGRAPixel read FColorizeDisabled
      write SetColorizeDisabled;
  published
    property CheckBoxUnchecked: string read FCheckBoxUnchecked
      write SetCheckBoxUnchecked;
    property CheckBoxChecked: string read FCheckBoxChecked write SetCheckBoxChecked;
    property RadioButtonUnchecked: string read FRadioButtonUnchecked
      write SetRadioButtonUnchecked;
    property RadioButtonChecked: string read FRadioButtonChecked
      write SetRadioButtonChecked;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Themes', [TBGRASVGTheme]);
end;

{ TBGRASVGTheme }

procedure TBGRASVGTheme.SetCheckBoxUnchecked(AValue: string);
begin
  if FCheckBoxUnchecked = AValue then
    Exit;
  FCheckBoxUnchecked := AValue;
end;

procedure TBGRASVGTheme.SetColorizeActive(AValue: TBGRAPixel);
begin
  if FColorizeActive = AValue then
    Exit;
  FColorizeActive := AValue;
end;

procedure TBGRASVGTheme.SetColorizeDisabled(AValue: TBGRAPixel);
begin
  if FColorizeDisabled = AValue then
    Exit;
  FColorizeDisabled := AValue;
end;

procedure TBGRASVGTheme.SetColorizeHover(AValue: TBGRAPixel);
begin
  if FColorizeHover = AValue then
    Exit;
  FColorizeHover := AValue;
end;

procedure TBGRASVGTheme.SetColorizeNormal(AValue: TBGRAPixel);
begin
  if FColorizeNormal = AValue then
    Exit;
  FColorizeNormal := AValue;
end;

procedure TBGRASVGTheme.SetRadioButtonChecked(AValue: string);
begin
  if FRadioButtonChecked = AValue then
    Exit;
  FRadioButtonChecked := AValue;
end;

procedure TBGRASVGTheme.SetRadioButtonUnchecked(AValue: string);
begin
  if FRadioButtonUnchecked = AValue then
    Exit;
  FRadioButtonUnchecked := AValue;
end;

constructor TBGRASVGTheme.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // https://material.io/resources/icons/
  FCheckBoxUnchecked :=
    '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M19 5v14H5V5h14m0-2H5c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h14c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2z"/></svg>';
  FCheckBoxChecked :=
    '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M19 3H5c-1.11 0-2 .9-2 2v14c0 1.1.89 2 2 2h14c1.11 0 2-.9 2-2V5c0-1.1-.89-2-2-2zm-9 14l-5-5 1.41-1.41L10 14.17l7.59-7.59L19 8l-9 9z"/></svg>';
  FRadioButtonUnchecked :=
    '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.42 0-8-3.58-8-8s3.58-8 8-8 8 3.58 8 8-3.58 8-8 8z"/></svg>';
  FRadioButtonChecked :=
    '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M12 7c-2.76 0-5 2.24-5 5s2.24 5 5 5 5-2.24 5-5-2.24-5-5-5zm0-5C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.42 0-8-3.58-8-8s3.58-8 8-8 8 3.58 8 8-3.58 8-8 8z"/></svg>';
  // Colorize
  FColorizeNormal := BGRAPixelTransparent;
  FColorizeHover := ColorToBGRA(clWhite, 100);
  FColorizeActive := BGRAPixelTransparent;
  FColorizeDisabled := BGRAPixelTransparent;
end;

destructor TBGRASVGTheme.Destroy;
begin
  inherited Destroy;
end;

procedure TBGRASVGTheme.DrawRadioButton(Caption: string;
  State: TBGRAThemeButtonState; Focused: boolean; Checked: boolean;
  ARect: TRect; ASurface: TBGRAThemeSurface);
var
  Style: TTextStyle;
  svg: TBGRASVG;
  color: TBGRAPixel;
begin
  with ASurface do
  begin
    BitmapRect := RectWithSize(ARect.Left, ARect.Top, ARect.Height, ARect.Height);
    if Checked then
      svg := TBGRASVG.CreateFromString(FRadioButtonChecked)
    else
      svg := TBGRASVG.CreateFromString(FRadioButtonUnchecked);
    svg.StretchDraw(Bitmap.Canvas2D, 0, 0, Bitmap.Width, Bitmap.Height);
    svg.Free;
    case State of
      btbsNormal: color := FColorizeNormal;
      btbsHover: color := FColorizeHover;
      btbsActive: color := FColorizeActive;
      btbsDisabled: color := FColorizeDisabled;
    end;
    BitmapColorOverlay(color);
    DrawBitmap;

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
end;

procedure TBGRASVGTheme.SetCheckBoxChecked(AValue: string);
begin
  if FCheckBoxChecked = AValue then
    Exit;
  FCheckBoxChecked := AValue;
end;

procedure TBGRASVGTheme.DrawCheckBox(Caption: string; State: TBGRAThemeButtonState;
  Focused: boolean; Checked: boolean; ARect: TRect; ASurface: TBGRAThemeSurface);
var
  Style: TTextStyle;
  svg: TBGRASVG;
  color: TBGRAPixel;
begin
  with ASurface do
  begin
    BitmapRect := RectWithSize(ARect.Left, ARect.Top, ARect.Height, ARect.Height);
    if Checked then
      svg := TBGRASVG.CreateFromString(FCheckBoxChecked)
    else
      svg := TBGRASVG.CreateFromString(FCheckBoxUnchecked);
    svg.StretchDraw(Bitmap.Canvas2D, 0, 0, Bitmap.Width, Bitmap.Height);
    svg.Free;
    case State of
      btbsNormal: color := FColorizeNormal;
      btbsHover: color := FColorizeHover;
      btbsActive: color := FColorizeActive;
      btbsDisabled: color := FColorizeDisabled;
    end;
    BitmapColorOverlay(color);
    DrawBitmap;

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
end;

end.
