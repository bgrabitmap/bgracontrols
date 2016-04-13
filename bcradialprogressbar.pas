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
    property MinValue: integer read FMinValue write SetMinValue;
    property MaxValue: integer read FMaxValue write SetMaxValue;
    property Value: integer read FValue write SetValue;
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property Color;
  end;

procedure Register;

implementation

procedure Register;
begin
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
  FBitmap.Canvas2D.arc(Width / 2, Height / 2, Height / 3, 0, pi * 2, False);
  FBitmap.Canvas2D.fillStyle(BGRAWhite);
  FBitmap.Canvas2D.fill;

  FBitmap.Canvas2D.lineWidth := 5;
  FBitmap.Canvas2D.strokeStyle(BGRA(200, 200, 200));
  FBitmap.Canvas2D.stroke;

  FBitmap.Canvas2D.beginPath;
  if Value <> MinValue then
    FBitmap.Canvas2D.arc(Width / 2, Height / 2, Height / 3, pi * 1.5,
      (pi * 1.5) + ((pi * 2) * Value / MaxValue), False);
  FBitmap.Canvas2D.fillStyle(BGRAPixelTransparent);
  FBitmap.Canvas2D.fill;

  FBitmap.Canvas2D.lineWidth := 5;
  FBitmap.Canvas2D.strokeStyle(BGRABlack);
  FBitmap.Canvas2D.stroke;

  textStr := FloatToStr((Value / MaxValue) * 100) + '%';

  textBmp := TextShadow(Width, Height, textStr, 20, BGRABlack, BGRABlack, 2, 2, 4) as
    TBGRABitmap;
  FBitmap.PutImage(0, 0, textBmp, dmDrawWithTransparency);
  textBmp.Free;
end;

constructor TBCRadialProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, 200, 200);
  FMinValue := 0;
  FMaxValue := 100;
  FValue := 30;
  FBitmap := nil;
end;

destructor TBCRadialProgressBar.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

end.
