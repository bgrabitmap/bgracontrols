// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRATheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRASVGImageList;

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
    procedure BitmapColorOverlay(AColor: TBGRAPixel; AOperation: TBlendOperation = boTransparent); overload;
    function ScaleForCanvas(AValue: integer; AFromDPI: integer = 96): integer;
    function ScaleForBitmap(AValue: integer; AFromDPI: integer = 96): integer;
    function ScaleForBitmap(const ARect: TRect; AFromDPI: integer = 96): TRect;
    property DestCanvas: TCanvas read FDestCanvas;
    property DestCanvasDPI: integer read FLclDPI;
    property Bitmap: TBGRABitmap read GetBitmap;
    property BitmapRect: TRect read FBitmapRect write SetBitmapRect;
    property BitmapDPI: integer read GetBitmapDPI;
  end;

  TBGRATheme = class;

  { TBGRAThemeControl }

  TBGRAThemeControl = class(TCustomControl)
  private
    FTheme: TBGRATheme;
    procedure SetTheme(AValue: TBGRATheme);
  public
    destructor Destroy; override;
  published
    property Theme: TBGRATheme read FTheme write SetTheme;
  end;

  { TBGRATheme }

  TBGRATheme = class(TComponent)
  private
    FThemedControls: TList;
    function GetThemedControl(AIndex: integer): TBGRAThemeControl;
    function GetThemedControlCount: integer;
    procedure AddThemedControl(AControl: TBGRAThemeControl);
    procedure RemoveThemedControl(AControl: TBGRAThemeControl);

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateThemedControls;

    procedure DrawButton(Caption: string; State: TBGRAThemeButtonState;
      Focused: boolean; ARect: TRect; ASurface: TBGRAThemeSurface; AImageIndex: Integer = -1; AImageList: TBGRASVGImageList = nil); virtual;
    procedure DrawRadioButton(Caption: string; State: TBGRAThemeButtonState;
    {%H-}Focused: boolean; Checked: boolean; ARect: TRect; ASurface: TBGRAThemeSurface); virtual;
    procedure DrawCheckBox(Caption: string; State: TBGRAThemeButtonState;
    {%H-}Focused: boolean; Checked: boolean; ARect: TRect; ASurface: TBGRAThemeSurface); virtual;

    property ThemedControlCount: integer read GetThemedControlCount;
    property ThemedControl[AIndex: integer]: TBGRAThemeControl read GetThemedControl;
  published

  end;

var
  BGRADefaultTheme: TBGRATheme;

procedure Register;

implementation

uses LCLType;

procedure Register;
begin
  RegisterComponents('BGRA Themes', [TBGRATheme]);
end;

{ TBGRAThemeControl }

procedure TBGRAThemeControl.SetTheme(AValue: TBGRATheme);
begin
  if FTheme=AValue then Exit;
  if Assigned(AValue) then AValue.RemoveThemedControl(self);
  FTheme:=AValue;
  if Assigned(AValue) then AValue.AddThemedControl(self);
  Invalidate;
end;

destructor TBGRAThemeControl.Destroy;
begin
  if Assigned(FTheme) then FTheme.RemoveThemedControl(self);
  inherited Destroy;
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
  FLclDPI:= ALclDPI;
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

procedure TBGRAThemeSurface.BitmapColorOverlay(AColor: TBGRAPixel;
  AOperation: TBlendOperation);
begin
  if AColor.alpha <> 0 then
    Bitmap.BlendOver(AColor, AOperation, AColor.alpha, false, true);
end;

function TBGRAThemeSurface.ScaleForCanvas(AValue: integer; AFromDPI: integer): integer;
begin
  result := MulDiv(AValue, DestCanvasDPI, AFromDPI);
end;

function TBGRAThemeSurface.ScaleForBitmap(AValue: integer; AFromDPI: integer): integer;
begin
  result := MulDiv(AValue, BitmapDPI, AFromDPI);
end;

function TBGRAThemeSurface.ScaleForBitmap(const ARect: TRect; AFromDPI: integer): TRect;
begin
  result.Left := ScaleForBitmap(ARect.Left, AFromDPI);
  result.Top := ScaleForBitmap(ARect.Top, AFromDPI);
  result.Right := ScaleForBitmap(ARect.Right, AFromDPI);
  result.Bottom := ScaleForBitmap(ARect.Bottom, AFromDPI);
end;

{ TBGRATheme }

function TBGRATheme.GetThemedControl(AIndex: integer): TBGRAThemeControl;
begin
  result := TBGRAThemeControl(FThemedControls[AIndex]);
end;

function TBGRATheme.GetThemedControlCount: integer;
begin
  result := FThemedControls.Count;
end;

procedure TBGRATheme.AddThemedControl(AControl: TBGRAThemeControl);
begin
  if FThemedControls.IndexOf(AControl) = -1 then
    FThemedControls.Add(AControl);
end;

procedure TBGRATheme.RemoveThemedControl(AControl: TBGRAThemeControl);
begin
  FThemedControls.Remove(AControl);
end;

constructor TBGRATheme.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThemedControls := TList.Create;
end;

destructor TBGRATheme.Destroy;
var i: integer;
begin
  for i := ThemedControlCount-1 downto 0 do
    ThemedControl[i].Theme := nil;
  FThemedControls.Free;
  inherited Destroy;
end;

procedure TBGRATheme.InvalidateThemedControls;
var
  i: Integer;
begin
  for i := 0 to ThemedControlCount-1 do
    ThemedControl[i].Invalidate;
end;

procedure TBGRATheme.DrawButton(Caption: string; State: TBGRAThemeButtonState;
  Focused: boolean; ARect: TRect; ASurface: TBGRAThemeSurface;
  AImageIndex: Integer; AImageList: TBGRASVGImageList);
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
      fillchar(Style, sizeof(Style), 0);
      Style.Alignment := taCenter;
      Style.Layout := tlCenter;
      Style.Wordbreak := True;
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
      fillchar(Style, sizeof(Style), 0);
      Style.Alignment := taLeftJustify;
      Style.Layout := tlCenter;
      Style.Wordbreak := True;
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
      fillchar(Style, sizeof(Style), 0);
      Style.Alignment := taLeftJustify;
      Style.Layout := tlCenter;
      Style.Wordbreak := True;
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
