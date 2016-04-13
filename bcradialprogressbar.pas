unit BCRadialProgressBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, BCBaseCtrls,
  BGRABitmap, BGRABitmapTypes, BGRATextFX;

type

  { TBCRadialProgressBar }

  TBCRadialProgressBar = class(TBCGraphicControl)
  private
    { Private declarations }
    FMaxValue: integer;
    FMinValue: integer;
    FValue: integer;
    FBitmap: TBGRABitmap;
    FLineColor: TColor;
    FLineBkgColor: TColor;
    FFontShadowColor: TColor;
    FFontShadowOffsetX: integer;
    FFontShadowOffsetY: integer;
    FFontShadowRadius: integer;
    procedure SetFFontShadowColor(AValue: TColor);
    procedure SetFFontShadowOffsetX(AValue: integer);
    procedure SetFFontShadowOffsetY(AValue: integer);
    procedure SetFFontShadowRadius(AValue: integer);
    procedure SetFLineBkgColor(AValue: TColor);
    procedure SetFLineColor(AValue: TColor);
    procedure SetMaxValue(AValue: integer);
    procedure SetMinValue(AValue: integer);
    procedure SetValue(AValue: integer);
  protected
    { Protected declarations }
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: boolean); override;
    procedure DrawControl; override;
    procedure RenderControl; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property MinValue: integer read FMinValue write SetMinValue default 0;
    property MaxValue: integer read FMaxValue write SetMaxValue default 100;
    property Value: integer read FValue write SetValue default 0;
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property Color default clWhite;
    property LineColor: TColor read FLineColor write SetFLineColor default clBlack;
    property LineBkgColor: TColor read FLineBkgColor write SetFLineBkgColor default
      clSilver;
    property FontShadowColor: TColor read FFontShadowColor
      write SetFFontShadowColor default clBlack;
    property FontShadowOffsetX: integer read FFontShadowOffsetX
      write SetFFontShadowOffsetX default 2;
    property FontShadowOffsetY: integer read FFontShadowOffsetY
      write SetFFontShadowOffsetY default 2;
    property FontShadowRadius: integer read FFontSHadowRadius
      write SetFFontShadowRadius default 4;
    property Font;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I icons\bcradialprogressbar_icon.lrs}
  RegisterComponents('BGRA Controls', [TBCRadialProgressBar]);
end;

{ TBCRadialProgressBar }

procedure TBCRadialProgressBar.SetMaxValue(AValue: integer);
begin
  if FMaxValue = AValue then
    exit;
  FMaxValue := AValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  if FMinValue > FMaxValue then
    FMinValue := FMaxValue;
  RenderControl;
  Invalidate;
end;

procedure TBCRadialProgressBar.SetFLineBkgColor(AValue: TColor);
begin
  if FLineBkgColor = AValue then
    Exit;
  FLineBkgColor := AValue;
  RenderControl;
  Invalidate;
end;

procedure TBCRadialProgressBar.SetFFontShadowColor(AValue: TColor);
begin
  if FFontShadowColor = AValue then
    Exit;
  FFontShadowColor := AValue;
  RenderControl;
  Invalidate;
end;

procedure TBCRadialProgressBar.SetFFontShadowOffsetX(AValue: integer);
begin
  if FFontShadowOffsetX = AValue then
    Exit;
  FFontShadowOffsetX := AValue;
  RenderControl;
  Invalidate;
end;

procedure TBCRadialProgressBar.SetFFontShadowOffsetY(AValue: integer);
begin
  if FFontShadowOffsetY = AValue then
    Exit;
  FFontShadowOffsetY := AValue;
  RenderControl;
  Invalidate;
end;

procedure TBCRadialProgressBar.SetFFontShadowRadius(AValue: integer);
begin
  if FFontSHadowRadius = AValue then
    Exit;
  FFontSHadowRadius := AValue;
  RenderControl;
  Invalidate;
end;

procedure TBCRadialProgressBar.SetFLineColor(AValue: TColor);
begin
  if FLineColor = AValue then
    Exit;
  FLineColor := AValue;
  RenderControl;
  Invalidate;
end;

procedure TBCRadialProgressBar.SetMinValue(AValue: integer);
begin
  if FMinValue = AValue then
    exit;
  FMinValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FMaxValue < FMinValue then
    FMaxValue := FMinValue;
  RenderControl;
  Invalidate;
end;

procedure TBCRadialProgressBar.SetValue(AValue: integer);
begin
  if FValue = AValue then
    exit;
  FValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  RenderControl;
  Invalidate;
end;

procedure TBCRadialProgressBar.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
begin
  PreferredWidth := 200;
  PreferredHeight := 200;
end;

procedure TBCRadialProgressBar.DrawControl;
begin
  FBitmap.Draw(Canvas, 0, 0, False);
end;

procedure TBCRadialProgressBar.RenderControl;
var
  textBmp: TBGRABitmap;
  textStr: string;
begin
  FreeAndNil(FBitmap);
  FBitmap := TBGRABitmap.Create(Width, Height);

  FBitmap.Canvas2D.beginPath;
  FBitmap.Canvas2D.arc(Width / 2, Height / 2, Height / 2.5, 0, pi * 2, False);
  FBitmap.Canvas2D.fillStyle(Color);
  FBitmap.Canvas2D.fill;

  FBitmap.Canvas2D.lineWidth := Height / 50;
  FBitmap.Canvas2D.strokeStyle(LineBkgColor);
  FBitmap.Canvas2D.stroke;

  FBitmap.Canvas2D.beginPath;
  if Value <> MinValue then
    FBitmap.Canvas2D.arc(Width / 2, Height / 2, Height / 2.5, pi * 1.5,
      (pi * 1.5) + ((pi * 2) * Value / MaxValue), False);
  FBitmap.Canvas2D.fillStyle(BGRAPixelTransparent);
  FBitmap.Canvas2D.fill;

  FBitmap.Canvas2D.lineWidth := Height / 50;
  FBitmap.Canvas2D.strokeStyle(LineColor);
  FBitmap.Canvas2D.stroke;

  textStr := FloatToStr((Value / MaxValue) * 100) + '%';

  textBmp := TextShadow(Width, Height, textStr, Font.Height,
    ColorToBGRA(Font.Color), ColorToBGRA(FontShadowColor), FontShadowOFfsetX,
    FontShadowOffsetY, FontSHadowRadius, Font.Style, Font.Name) as TBGRABitmap;
  FBitmap.PutImage(0, 0, textBmp, dmDrawWithTransparency);
  textBmp.Free;
end;

constructor TBCRadialProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, 200, 200);
  FMaxValue := 100;
  FMinValue := 0;
  FValue := 0;
  FLineColor := clBlack;
  FLineBkgColor := clSilver;
  FFontShadowColor := clBlack;
  FFontShadowOffsetX := 2;
  FFontShadowOffsetY := 2;
  FFontShadowRadius := 4;
  Font.Color := clBlack;
  Font.Height := 20;
  Color := clWhite;
end;

destructor TBCRadialProgressBar.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

end.
