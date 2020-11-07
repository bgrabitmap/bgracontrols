// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
unit BGRATheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes;

type
  TBGRAThemeButtonState = (btbsNormal, btbsHover, btbsActive, btbsDisabled);

  { TBGRAThemeSurface }

  TBGRAThemeSurface = class
  private
    FBitmap: TBGRABitmap;
    FBitmapRect: TRect;
    FCanvasScale: single;
    FDestCanvas: TCanvas;
    FLclDPI: integer;
    function GetBitmap: TBGRABitmap;
    function GetBitmapDPI: integer;
    procedure SetBitmapRect(AValue: TRect);
  public
    constructor Create(AControl: TCustomControl);
    constructor Create(ADestRect: TRect; ADestCanvas: TCanvas; ACanvasScale: single; ALclDPI: integer);
    destructor Destroy; override;
    procedure DrawBitmap;
    procedure DiscardBitmap;
    procedure BitmapColorOverlay(AColor: string; AOperation: TBlendOperation = boTransparent); overload;
    procedure BitmapColorOverlay(AColor: TBGRAPixel; AOperation: TBlendOperation = boTransparent); overload;
    property DestCanvas: TCanvas read FDestCanvas;
    property DestCanvasDPI: integer read FLclDPI;
    property Bitmap: TBGRABitmap read GetBitmap;
    property BitmapRect: TRect read FBitmapRect write SetBitmapRect;
    property BitmapDPI: integer read GetBitmapDPI;
  end;

  { TBGRATheme }

  TBGRATheme = class(TComponent)
  private

  protected

  public
    procedure DrawButton(Caption: string; State: TBGRAThemeButtonState;
      Focused: boolean; ARect: TRect; ASurface: TBGRAThemeSurface); virtual;
    procedure DrawRadioButton(Caption: string; State: TBGRAThemeButtonState;
    {%H-}Focused: boolean; Checked: boolean; ARect: TRect; ASurface: TBGRAThemeSurface); virtual;
    procedure DrawCheckBox(Caption: string; State: TBGRAThemeButtonState;
    {%H-}Focused: boolean; Checked: boolean; ARect: TRect; ASurface: TBGRAThemeSurface); virtual;
  published

  end;

var
  BGRADefaultTheme: TBGRATheme;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Themes', [TBGRATheme]);
end;

{ TBGRAThemeSurface }

function TBGRAThemeSurface.GetBitmap: TBGRABitmap;
begin
  if FBitmap = nil then
    FBitmap := TBGRABitmap.Create(round(FBitmapRect.Width * FCanvasScale),
               round(FBitmapRect.Height * FCanvasScale));
  result := FBitmap;
end;

function TBGRAThemeSurface.GetBitmapDPI: integer;
begin
  result := round(FLclDPI*FCanvasScale);
end;

procedure TBGRAThemeSurface.SetBitmapRect(AValue: TRect);
begin
  if FBitmapRect=AValue then Exit;
  DiscardBitmap;
  FBitmapRect:=AValue;
end;

constructor TBGRAThemeSurface.Create(AControl: TCustomControl);
var
  parentForm: TCustomForm;
  lclDPI: Integer;
begin
  parentForm := GetParentForm(AControl, False);
  if Assigned(parentForm) then
    lclDPI := parentForm.PixelsPerInch
    else lclDPI := Screen.PixelsPerInch;
  Create(AControl.ClientRect, AControl.Canvas, AControl.GetCanvasScaleFactor, lclDPI);
end;

constructor TBGRAThemeSurface.Create(ADestRect: TRect; ADestCanvas: TCanvas;
  ACanvasScale: single; ALclDPI: integer);
begin
  FBitmap := nil;
  FBitmapRect := ADestRect;
  FDestCanvas := ADestCanvas;
  FCanvasScale:= ACanvasScale;
end;

destructor TBGRAThemeSurface.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TBGRAThemeSurface.DrawBitmap;
begin
  if FBitmap = nil then exit;
  FBitmap.Draw(FDestCanvas, FBitmapRect, false);
end;

procedure TBGRAThemeSurface.DiscardBitmap;
begin
  FreeAndNil(FBitmap);
end;

procedure TBGRAThemeSurface.BitmapColorOverlay(AColor: string;
  AOperation: TBlendOperation);
begin
  BitmapColorOverlay(StrToBGRA(AColor), AOperation);
end;

procedure TBGRAThemeSurface.BitmapColorOverlay(AColor: TBGRAPixel;
  AOperation: TBlendOperation);
begin
  if AColor.alpha <> 0 then
    Bitmap.BlendOver(AColor, AOperation, AColor.alpha, false, true);
end;

{ TBGRATheme }

procedure TBGRATheme.DrawButton(Caption: string; State: TBGRAThemeButtonState;
  Focused: boolean; ARect: TRect; ASurface: TBGRAThemeSurface);
var
  Style: TTextStyle;
begin
  With ASurface do
  begin
    DestCanvas.Font.Color := clBlack;
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
end;

procedure TBGRATheme.DrawRadioButton(Caption: string; State: TBGRAThemeButtonState;
  Focused: boolean; Checked: boolean; ARect: TRect; ASurface: TBGRAThemeSurface);
var
  Style: TTextStyle;
  Color: TBGRAPixel;
begin
  with ASurface do
  begin
    DestCanvas.Font.Color := clBlack;
    case State of
      btbsHover: Color := BGRA(0, 120, 215);
      btbsActive: Color := BGRA(0, 84, 153);
      btbsDisabled:
      begin
        DestCanvas.Font.Color := clGray;
        Color := BGRA(204, 204, 204);
      end;
      else {btbsNormal}
        Color := BGRABlack;
    end;

    BitmapRect := RectWithSize(ARect.Left, ARect.Top, ARect.Height, ARect.Height);
    Bitmap.FillEllipseAntialias(Bitmap.Height / 2, Bitmap.Height / 2,
      Bitmap.Height / 2 - 2, Bitmap.Height / 2 - 2, BGRAWhite);
    Bitmap.EllipseAntialias(Bitmap.Height / 2, Bitmap.Height / 2,
      Bitmap.Height / 2 - 2, Bitmap.Height / 2 - 2, Color{%H-}, 1);
    if Checked then
      Bitmap.FillEllipseAntialias(Bitmap.Height / 2, Bitmap.Height /
        2, Bitmap.Height / 4, Bitmap.Height / 4, Color);
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

procedure TBGRATheme.DrawCheckBox(Caption: string; State: TBGRAThemeButtonState;
  Focused: boolean; Checked: boolean; ARect: TRect; ASurface: TBGRAThemeSurface);
var
  Style: TTextStyle;
  Bitmap: TBGRABitmap;
  Color: TBGRAPixel;
  aleft, atop, aright, abottom: integer;
begin
  with ASurface do
  begin
    DestCanvas.Font.Color := clBlack;
    case State of
      btbsHover: Color := BGRA(0, 120, 215);
      btbsActive: Color := BGRA(0, 84, 153);
      btbsDisabled:
      begin
        DestCanvas.Font.Color := clGray;
        Color := BGRA(204, 204, 204);
      end;
      else {btbsNormal}
        Color := BGRABlack;
    end;

    BitmapRect := RectWithSize(ARect.Left, ARect.Top, ARect.Height, ARect.Height);
    Bitmap.Rectangle(0, 0, Bitmap.Height, Bitmap.Height, Color, BGRAWhite);
    aleft := 0;
    aright := Bitmap.Height;
    atop := 0;
    abottom := Bitmap.Height;
    if Checked then
      Bitmap.DrawPolyLineAntialias(Bitmap.ComputeBezierSpline(
        [BezierCurve(pointF(aleft + 2, atop + 3), PointF((aleft + aright - 1) / 2, abottom - 3)),
        BezierCurve(PointF((aleft + aright - 1) / 2, abottom - 3), PointF(
        (aleft + aright - 1) / 2, (atop * 2 + abottom - 1) / 3), PointF(aright - 2, atop - 2))]),
        Color, 1.5);
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

var
  BasicTheme: TBGRATheme;

initialization

  BasicTheme := TBGRATheme.Create(nil);
  BGRADefaultTheme := BasicTheme;

finalization
  FreeAndNil(BasicTheme);

end.
