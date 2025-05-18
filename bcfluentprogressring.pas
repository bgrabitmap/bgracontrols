{
 2024 by hedgehog
}

unit BCFluentProgressRing;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls,
  BGRAGraphicControl, BGRABitmapTypes;

type

  { TBCFluentProgressRing }

  TBCFluentProgressRing = class(TBGRAGraphicControl)
  private
    FPeriod: Int64;
    FIndeterminate: boolean;
    FStartTickCount: QWord;
    FAnimationTime: Int64;
    FTimer: TTimer;
    FMaxValue: integer;
    FMinValue: integer;
    FValue: integer;
    FLineColor: TColor;
    FLineBkgColor: TColor;
    FLineWidth: integer;

    procedure SetIndeterminate(AValue: boolean);
    procedure SetLineBkgColor(AValue: TColor);
    procedure SetLineColor(AValue: TColor);
    procedure SetMaxValue(AValue: integer);
    procedure SetMinValue(AValue: integer);
    procedure SetValue(AValue: integer);
    procedure SetLineWidth(AValue: integer);
  protected
    procedure SetEnabled(Value: Boolean); override;
    procedure SetVisible(Value: Boolean); override;
    procedure RedrawBitmapContent; override;

    procedure TimerEvent({%H-}Sender: TObject);
    procedure TimerStart({%H-}Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MinValue: integer read FMinValue write SetMinValue default 0;
    property MaxValue: integer read FMaxValue write SetMaxValue default 100;
    property Value: integer read FValue write SetValue default 0;
    property LineColor: TColor read FLineColor write SetLineColor default
      TColor($009E5A00);
    property LineBkgColor: TColor read FLineBkgColor write SetLineBkgColor default
      TColor($00D3D3D3);
    property LineWidth: integer read FLineWidth write SetLineWidth default 0;
    property Indeterminate: boolean read FIndeterminate write SetIndeterminate default false;
  end;

procedure Register;

implementation

uses Math;

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCFluentProgressRing]);
end;

{ TBCFluentProgressRing }

procedure TBCFluentProgressRing.SetMaxValue(AValue: integer);
begin
  if FMaxValue = AValue then
    exit;
  FMaxValue := AValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  if FMinValue > FMaxValue then
    FMinValue := FMaxValue;
  DiscardBitmap;
end;

procedure TBCFluentProgressRing.SetLineBkgColor(AValue: TColor);
begin
  if FLineBkgColor = AValue then
    Exit;
  FLineBkgColor := AValue;
  DiscardBitmap;
end;

procedure TBCFluentProgressRing.SetIndeterminate(AValue: boolean);
begin
  if FIndeterminate=AValue then Exit;
  FIndeterminate:=AValue;
  if Enabled and Visible then
  begin
    FTimer.Enabled:= FIndeterminate;
    DiscardBitmap;
  end;
end;

procedure TBCFluentProgressRing.SetLineColor(AValue: TColor);
begin
  if FLineColor = AValue then
    Exit;
  FLineColor := AValue;
  DiscardBitmap;
end;

procedure TBCFluentProgressRing.SetMinValue(AValue: integer);
begin
  if FMinValue = AValue then
    exit;
  FMinValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FMaxValue < FMinValue then
    FMaxValue := FMinValue;
  DiscardBitmap;
end;

procedure TBCFluentProgressRing.SetValue(AValue: integer);
begin
  if FValue = AValue then
    exit;
  FValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  DiscardBitmap;
end;

procedure TBCFluentProgressRing.SetLineWidth(AValue: integer);
begin
  if FLineWidth = AValue then exit;
  FLineWidth := AValue;
  if Visible then DiscardBitmap;
end;

procedure TBCFluentProgressRing.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  FTimer.Enabled := Value and Visible and FIndeterminate;
  DiscardBitmap;
end;

procedure TBCFluentProgressRing.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  FTimer.Enabled := Enabled and Value and FIndeterminate;
  DiscardBitmap;
end;

procedure TBCFluentProgressRing.RedrawBitmapContent;
const
  pi2= 2*pi;
  pi15 = pi*1.5;
var
  EffectiveSize: integer;
  EffectiveLineWidth: single;
  a, da, r: single;

  procedure DoDrawArc(a, b: single; c: TColor);
  begin
    Bitmap.Canvas2D.strokeStyle(c);
    Bitmap.Canvas2D.beginPath;
    Bitmap.Canvas2D.arc(0, 0, r, a, b, false);
    Bitmap.Canvas2D.stroke;
  end;

begin
  if Width< Height then
    EffectiveSize:= Width
  else
    EffectiveSize:= Height;

  if EffectiveSize<2 then exit;

  Bitmap.FillTransparent;
  Bitmap.Canvas2D.resetTransform;
  Bitmap.Canvas2D.translate(Bitmap.Width/2, Bitmap.Height/2);
  Bitmap.Canvas2D.rotate(pi15);


  if FLineWidth=0 then
    EffectiveLineWidth:=EffectiveSize / 12
  else
    EffectiveLineWidth:= FLineWidth;
  r:= (EffectiveSize -EffectiveLineWidth)/2;

   Bitmap.Canvas2D.lineWidth:= EffectiveLineWidth;
  // background line
  if (FValue < FMaxValue) and (FLineBkgColor<>clNone) then
    DoDrawArc(0, pi2, FLineBkgColor);
  Bitmap.Canvas2D.lineCapLCL:= pecRound;

  if FIndeterminate and FTimer.Enabled then
  begin
    FAnimationTime:= (GetTickCount64 - FStartTickCount) mod FPeriod;
    a:= 3*FAnimationTime*pi2/FPeriod - pi;
    da:= max(2*abs(1 - 2*FAnimationTime/FPeriod), 0.01);
    DoDrawArc(a-da, a+da, FLineColor);
  end
  else if FValue > FMinValue then
  begin
    if Enabled then
      DoDrawArc(0, pi2 * FValue / FMaxValue, FLineColor)
    else
      DoDrawArc(0, pi2 * FValue / FMaxValue, clGray);
  end;
end;

procedure TBCFluentProgressRing.TimerEvent(Sender: TObject);
begin
  DiscardBitmap;
end;

procedure TBCFluentProgressRing.TimerStart(Sender: TObject);
begin
  FStartTickCount:= GetTickCount64;
  FAnimationTime:=0;
end;

constructor TBCFluentProgressRing.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPeriod:= 2400;
  FTimer:= TTimer.Create(self);
  FTimer.Interval := 15;
  FTimer.Enabled := false;
  FTimer.OnTimer := @TimerEvent;
  FTimer.OnStartTimer:= @TimerStart;

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, 100, 100);
  FMaxValue := 100;
  FMinValue := 0;
  FValue := 0;
  FLineWidth:=0;
  FLineColor := TColor($009E5A00);
  FLineBkgColor := TColor($00D3D3D3);
end;


end.

