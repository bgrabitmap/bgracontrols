unit BGRAFlashProgressBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LMessages, Forms, Controls, Graphics,
  Dialogs, BGRABitmap;

type

  { TBGRAFlashProgressBar }

  TBGRAFlashProgressBar = class(TGraphicControl)
  private
    FMaxValue: integer;
    FMinValue: integer;
    FValue:    integer;
    FBmp:      TBGRABitmap;
    FRandSeed: integer;
    procedure SetMaxValue(const AValue: integer);
    procedure SetMinValue(const AValue: integer);
    procedure SetValue(const AValue: integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: boolean); override;
    procedure Paint; override;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property MinValue: integer Read FMinValue Write SetMinValue;
    property MaxValue: integer Read FMaxValue Write SetMaxValue;
    property Value: integer Read FValue Write SetValue;
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property Color;
  end;

procedure Register;

implementation

uses BGRABitmapTypes, BGRAGradients, Types;

procedure Register;
begin
  {$I bgraflashprogressbar_icon.lrs}
  RegisterComponents('BGRA Controls', [TBGRAFlashProgressBar]);
end;

{ TBGRAFlashProgressBar }

procedure TBGRAFlashProgressBar.SetMinValue(const AValue: integer);
begin
  if FMinValue = AValue then
    exit;
  FMinValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FMaxValue < FMinValue then
    FMaxValue := FMinValue;
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetValue(const AValue: integer);
begin
  if FValue = AValue then
    exit;
  FValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  Invalidate;
end;

{$hints off}
procedure TBGRAFlashProgressBar.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
begin
  PreferredWidth  := 379;
  PreferredHeight := 33;
end;

{$hints on}

procedure TBGRAFlashProgressBar.Paint;
var
  content: TRect;
  xpos, y, tx, ty: integer;
  grayValue: integer;

  function ApplyLightness(c: TBGRAPixel; lightness: word): TBGRAPixel;
  begin
    Result := GammaCompression(SetLightness(GammaExpansion(c), lightness));
  end;

  procedure DrawBar(bounds: TRect);
  var
    lCol: TBGRAPixel;
  begin
    lCol := ColorToBGRA(ColorToRGB(Color));

    DoubleGradientAlphaFill(FBmp, bounds,
      ApplyLightness(lCol, 37000), ApplyLightness(lCol, 29000),
      ApplyLightness(lCol, 26000), ApplyLightness(lCol, 18000),
      gdVertical, gdVertical, gdVertical, 0.53);

    InflateRect(bounds, -1, -1);

    DoubleGradientAlphaFill(FBmp, bounds,
      ApplyLightness(lCol, 28000), ApplyLightness(lCol, 22000),
      ApplyLightness(lCol, 19000), ApplyLightness(lCol, 11000),
      gdVertical, gdVertical, gdVertical, 0.53);
  end;

begin
  tx := ClientWidth;
  ty := ClientHeight;
  if Assigned(FBmp) and ((FBmp.Width <> tx) or (FBmp.Height <> ty)) then
    FreeAndNil(FBmp);

  if not Assigned(FBmp) then
    FBmp := TBGRABitmap.Create(tx, ty)
  else
    FBmp.FillTransparent;

  FBmp.Rectangle(0, 0, tx, ty, BGRA(255, 255, 255, 6), dmSet);
  if (tx > 2) and (ty > 2) then
    FBmp.Rectangle(1, 1, tx - 1, ty - 1, BGRA(29, 29, 29), dmSet);

  if (tx > 4) and (ty > 4) then
  begin
    content  := Rect(2, 2, tx - 2, ty - 2);
    randseed := FRandSeed;
    for y := content.Top to content.Bottom - 1 do
    begin
      if y = content.Top then
        grayValue := 33
      else
      if y = content.Top + 1 then
        grayValue := 43
      else
        grayValue := 47 + random(50 - 47 + 1);
      FBmp.SetHorizLine(content.Left, y, content.Right - 1, BGRA(
        grayValue, grayValue, grayValue));
    end;
    if tx >= 6 then
      FBmp.DrawVertLine(content.Right - 1, content.Top, content.Bottom - 1,
        BGRA(0, 0, 0, 32));
    if FMaxValue > FMinValue then
    begin
      xpos := round((FValue - FMinValue) / (FMaxValue - FMinValue) *
        (content.right - content.left)) + content.left;
      if xpos > content.left then
      begin
        DrawBar(rect(content.left, content.top, xpos, content.bottom));
        if xpos < content.right then
        begin
          FBmp.SetPixel(xpos, content.top, BGRA(62, 62, 62));
          FBmp.SetVertLine(xpos, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
        end;
      end;
    end;
  end;
  FBmp.Draw(Canvas, 0, 0, False);
end;

{$hints off}
procedure TBGRAFlashProgressBar.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  //do nothing
end;

{$hints on}

constructor TBGRAFlashProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, 33);
  FMinValue := 0;
  FMaxValue := 100;
  FValue := 30;
  FBmp := nil;
  randomize;
  FRandSeed := randseed;
  Color := BGRAToColor(BGRA(102, 163, 226));
end;

destructor TBGRAFlashProgressBar.Destroy;
begin
  FreeAndNil(FBmp);
  inherited Destroy;
end;

procedure TBGRAFlashProgressBar.SetMaxValue(const AValue: integer);
begin
  if FMaxValue = AValue then
    exit;
  FMaxValue := AValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  if FMinValue > FMaxValue then
    FMinValue := FMaxValue;
  Invalidate;
end;

end.
