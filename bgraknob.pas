unit BGRAKnob;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRAGradients, BGRABitmap, BGRABitmapTypes;

type
  TBGRAKnobPositionType = (kptLineSquareCap, kptLineRoundCap, kptFilledCircle,
    kptHollowCircle);
  TBGRAKnobValueChangedEvent = procedure(Sender: TObject; Value: single) of object;

  { TBGRAKnob }

  TBGRAKnob = class(TGraphicControl)
  private
    { Private declarations }
    FPhong: TPhongShading;
    FCurveExponent: single;
    FKnobBmp: TBGRABitmap;
    FKnobColor: TColor;
    FAngularPos: single;
    FPositionColor: TColor;
    FPositionMargin: single;
    FPositionOpacity: byte;
    FPositionType: TBGRAKnobPositionType;
    FPositionWidth: single;
    FSettingAngularPos: boolean;
    FUsePhongLighting: boolean;
    FMinValue, FMaxValue: single;
    FOnKnobValueChange: TBGRAKnobValueChangedEvent;
    FStartFromBottom: boolean;
    procedure CreateKnobBmp;
    function GetLightIntensity: integer;
    function GetValue: single;
    procedure SetCurveExponent(const AValue: single);
    procedure SetLightIntensity(const AValue: integer);
    procedure SetStartFromBottom(const AValue: boolean);
    procedure SetValue(AValue: single);
    procedure SetMaxValue(AValue: single);
    procedure SetMinValue(AValue: single);
    procedure SetPositionColor(const AValue: TColor);
    procedure SetPositionMargin(AValue: single);
    procedure SetPositionOpacity(const AValue: byte);
    procedure SetPositionType(const AValue: TBGRAKnobPositionType);
    procedure SetPositionWidth(const AValue: single);
    procedure SetUsePhongLighting(const AValue: boolean);
    procedure UpdateAngularPos(X, Y: integer);
    procedure SetKnobColor(const AValue: TColor);
  protected
    { Protected declarations }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure Paint; override;
    procedure Resize; override;
    function ValueCorrection(var AValue: single): boolean; virtual; overload;
    function ValueCorrection: boolean; virtual; overload;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    { Streaming }
    procedure SaveToFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);
    procedure OnFindClass(Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
  published
    { Published declarations }
    property Anchors;
    property CurveExponent: single read FCurveExponent write SetCurveExponent;
    property KnobColor: TColor read FKnobColor write SetKnobColor;
    property LightIntensity: integer read GetLightIntensity write SetLightIntensity;
    property PositionColor: TColor read FPositionColor write SetPositionColor;
    property PositionWidth: single read FPositionWidth write SetPositionWidth;
    property PositionOpacity: byte read FPositionOpacity write SetPositionOpacity;
    property PositionMargin: single read FPositionMargin write SetPositionMargin;
    property PositionType: TBGRAKnobPositionType
      read FPositionType write SetPositionType;
    property UsePhongLighting: boolean read FUsePhongLighting write SetUsePhongLighting;
    property MinValue: single read FMinValue write SetMinValue;
    property MaxValue: single read FMaxValue write SetMaxValue;
    property Value: single read GetValue write SetValue;
    property OnValueChanged: TBGRAKnobValueChangedEvent
      read FOnKnobValueChange write FOnKnobValueChange;
    property StartFromBottom: boolean read FStartFromBottom write SetStartFromBottom;
  end;


procedure Register;

implementation

uses Math;

procedure Register;
begin
  {$I icons\bgraknob_icon.lrs}
  RegisterComponents('BGRA Controls', [TBGRAKnob]);
end;

{ TBGRAKnob }

procedure TBGRAKnob.CreateKnobBmp;
var
  tx, ty: integer;
  h: single;
  d2: single;
  v: TPointF;
  p: PBGRAPixel;
  center: TPointF;
  yb: integer;
  xb: integer;
  mask: TBGRABitmap;
  Map: TBGRABitmap;
  BGRAKnobColor: TBGRAPixel;
begin
  tx := ClientWidth;
  ty := ClientHeight;
  if (tx = 0) or (ty = 0) then
    exit;

  FreeAndNil(FKnobBmp);

  FKnobBmp := TBGRABitmap.Create(tx, ty);
  center := PointF((tx - 1) / 2, (ty - 1) / 2);
  BGRAKnobColor := ColorToBGRA(ColorToRGB(KnobColor));

  if UsePhongLighting then
  begin
    //compute knob height map
    Map := TBGRABitmap.Create(tx, ty);
    for yb := 0 to ty - 1 do
    begin
      p := map.ScanLine[yb];
      for xb := 0 to tx - 1 do
      begin
        //compute vector between center and current pixel
        v := PointF(xb, yb) - center;
        //scale down to unit circle (with 1 pixel margin for soft border)
        v.x /= tx / 2 + 1;
        v.y /= ty / 2 + 1;
        //compute squared distance with scalar product
        d2 := v * v;
        //interpolate as quadratic curve and apply power function
        if d2 > 1 then
          h := 0
        else
          h := power(1 - d2, FCurveExponent);
        p^ := MapHeightToBGRA(h, 255);
        Inc(p);
      end;
    end;
    //antialiased border
    mask := TBGRABitmap.Create(tx, ty, BGRABlack);
    Mask.FillEllipseAntialias(center.x, center.y, tx / 2, ty / 2, BGRAWhite);
    map.ApplyMask(mask);
    Mask.Free;

    FPhong.Draw(FKnobBmp, Map, 30, 0, 0, BGRAKnobColor);
    Map.Free;
  end
  else
  begin
    FKnobBmp.FillEllipseAntialias(center.x, center.y, tx / 2, ty / 2, BGRAKnobColor);
  end;

end;

function TBGRAKnob.GetLightIntensity: integer;
begin
  Result := round(FPhong.LightSourceIntensity);
end;

function TBGRAKnob.GetValue: single;
begin
  Result := FAngularPos * 180 / Pi;
  if Result < 0 then
    Result += 360;
  Result := 270 - Result;
  if Result < 0 then
    Result += 360;
end;

procedure TBGRAKnob.SetCurveExponent(const AValue: single);
begin
  if FCurveExponent = AValue then
    exit;
  FCurveExponent := AValue;
  FreeAndNil(FKnobBmp);
  Invalidate;
end;

procedure TBGRAKnob.SetKnobColor(const AValue: TColor);
begin
  if FKnobColor = AValue then
    exit;
  FKnobColor := AValue;
  FreeAndNil(FKnobBmp);
  Invalidate;
end;

procedure TBGRAKnob.SetLightIntensity(const AValue: integer);
begin
  if AValue <> FPhong.LightSourceIntensity then
  begin
    FPhong.LightSourceIntensity := AValue;
    FreeAndNil(FKnobBmp);
    Invalidate;
  end;
end;

procedure TBGRAKnob.SetStartFromBottom(const AValue: boolean);
begin
  if FStartFromBottom = AValue then
    exit;
  FStartFromBottom := AValue;
  Invalidate;
end;

procedure TBGRAKnob.SetValue(AValue: single);
var
  NewAngularPos: single;
begin
  ValueCorrection(AValue);
  NewAngularPos := 3 * Pi / 2 - AValue * Pi / 180;
  if NewAngularPos > Pi then
    NewAngularPos -= 2 * Pi;
  if NewAngularPos < -Pi then
    NewAngularPos += 2 * Pi;
  if NewAngularPos <> FAngularPos then
  begin
    FAngularPos := NewAngularPos;
    Invalidate;
  end;
end;

procedure TBGRAKnob.SetMaxValue(AValue: single);
begin
  if AValue < 0 then
    AValue := 0;
  if AValue > 360 then
    AValue := 360;
  if FMaxValue = AValue then
    exit;
  FMaxValue := AValue;
  if FMinValue > FMaxValue then
    FMinValue := FMaxValue;
  if ValueCorrection then
    Invalidate;
end;

procedure TBGRAKnob.SetMinValue(AValue: single);
begin
  if AValue < 0 then
    AValue := 0;
  if AValue > 360 then
    AValue := 360;
  if FMinValue = AValue then
    exit;
  FMinValue := AValue;
  if FMaxValue < FMinValue then
    FMaxValue := FMinValue;
  if ValueCorrection then
    Invalidate;
end;

procedure TBGRAKnob.SetPositionColor(const AValue: TColor);
begin
  if FPositionColor = AValue then
    exit;
  FPositionColor := AValue;
  Invalidate;
end;

procedure TBGRAKnob.SetPositionMargin(AValue: single);
begin
  if FPositionMargin = AValue then
    exit;
  FPositionMargin := AValue;
  Invalidate;
end;

procedure TBGRAKnob.SetPositionOpacity(const AValue: byte);
begin
  if FPositionOpacity = AValue then
    exit;
  FPositionOpacity := AValue;
  Invalidate;
end;

procedure TBGRAKnob.SetPositionType(const AValue: TBGRAKnobPositionType);
begin
  if FPositionType = AValue then
    exit;
  FPositionType := AValue;
  Invalidate;
end;

procedure TBGRAKnob.SetPositionWidth(const AValue: single);
begin
  if FPositionWidth = AValue then
    exit;
  FPositionWidth := AValue;
  Invalidate;
end;

procedure TBGRAKnob.SetUsePhongLighting(const AValue: boolean);
begin
  if FUsePhongLighting = AValue then
    exit;
  FUsePhongLighting := AValue;
  FreeAndNil(FKnobBmp);
  Invalidate;
end;

procedure TBGRAKnob.UpdateAngularPos(X, Y: integer);
var
  FPreviousPos, Sign: single;
begin
  FPreviousPos := FAngularPos;
  if FStartFromBottom then
    Sign := 1
  else
    Sign := -1;
  FAngularPos := ArcTan2((-Sign) * (Y - ClientHeight / 2) / ClientHeight,
    Sign * (X - ClientWidth / 2) / ClientWidth);
  ValueCorrection;
  Invalidate;
  if (FPreviousPos <> FAngularPos) and Assigned(FOnKnobValueChange) then
    FOnKnobValueChange(Self, Value);
end;

procedure TBGRAKnob.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FSettingAngularPos := True;
    UpdateAngularPos(X, Y);
  end;
end;

procedure TBGRAKnob.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
    FSettingAngularPos := False;
end;

procedure TBGRAKnob.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FSettingAngularPos then
    UpdateAngularPos(X, Y);
end;

procedure TBGRAKnob.Paint;
var
  Bmp: TBGRABitmap;
  Center, Pos: TPointF;
  PosColor: TBGRAPixel;
  PosLen: single;
begin
  if (ClientWidth = 0) or (ClientHeight = 0) then
    exit;
  if FKnobBmp = nil then
  begin
    CreateKnobBmp;
    if FKnobBmp = nil then
      Exit;
  end;
  Bmp := TBGRABitmap.Create(ClientWidth, ClientHeight);
  Bmp.BlendImage(0, 0, FKnobBmp, boLinearBlend);

  //draw current position
  PosColor := ColorToBGRA(ColorToRGB(FPositionColor), FPositionOpacity);
  Center := PointF(ClientWidth / 2, ClientHeight / 2);
  Pos.X := Cos(FAngularPos) * (ClientWidth / 2);
  Pos.Y := -Sin(FAngularPos) * (ClientHeight / 2);
  if not FStartFromBottom then
    Pos := -Pos;
  PosLen := sqrt(Pos * Pos);

  Pos := Pos * ((PosLen - PositionMargin - FPositionWidth) / PosLen);
  Pos := Center + Pos;

  case PositionType of
    kptLineSquareCap:
    begin
      Bmp.LineCap := pecSquare;
      Bmp.DrawLineAntialias(Center.X, Center.Y, Pos.X, Pos.Y, PosColor, FPositionWidth);
    end;
    kptLineRoundCap:
    begin
      Bmp.LineCap := pecRound;
      Bmp.DrawLineAntialias(Center.X, Center.Y, Pos.X, Pos.Y, PosColor, FPositionWidth);
    end;
    kptFilledCircle:
    begin
      Bmp.FillEllipseAntialias(Pos.X, Pos.Y, FPositionWidth, FPositionWidth, PosColor);
    end;
    kptHollowCircle:
    begin
      Bmp.EllipseAntialias(Pos.X, Pos.Y, FPositionWidth * 2 / 3,
        FPositionWidth * 2 / 3, PosColor, FPositionWidth / 3);
    end;
  end;

  Bmp.Draw(Canvas, 0, 0, False);
  Bmp.Free;
end;

procedure TBGRAKnob.Resize;
begin
  inherited Resize;
  if (FKnobBmp <> nil) and ((ClientWidth <> FKnobBmp.Width) or
    (ClientHeight <> FKnobBmp.Height)) then
    FreeAndNil(FKnobBmp);
end;

function TBGRAKnob.ValueCorrection(var AValue: single): boolean;
begin
  if AValue < MinValue then
  begin
    AValue := MinValue;
    Result := True;
  end
  else
  if AValue > MaxValue then
  begin
    AValue := MaxValue;
    Result := True;
  end
  else
    Result := False;
end;

function TBGRAKnob.ValueCorrection: boolean;
var
  LValue: single;
begin
  LValue := Value;
  Result := ValueCorrection(LValue);
  if Result then
    Value := LValue;
end;

constructor TBGRAKnob.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  FPhong := TPhongShading.Create;
  FPhong.LightPositionZ := 100;
  FPhong.LightSourceIntensity := 300;
  FPhong.NegativeDiffusionFactor := 0.8;
  FPhong.AmbientFactor := 0.5;
  FPhong.DiffusionFactor := 0.6;
  FKnobBmp := nil;
  FCurveExponent := 0.2;
  FKnobColor := clBtnFace;
  FPositionColor := clBtnText;
  FPositionOpacity := 192;
  FPositionWidth := 4;
  FPositionMargin := 4;
  FPositionType := kptLineSquareCap;
  FUsePhongLighting := True;
  FOnKnobValueChange := nil;
  FStartFromBottom := True;
  FMinValue := 30;
  FMaxValue := 330;
end;

destructor TBGRAKnob.Destroy;
begin
  FPhong.Free;
  FKnobBmp.Free;
  inherited Destroy;
end;

procedure TBGRAKnob.SaveToFile(AFileName: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    WriteComponentAsTextToStream(AStream, Self);
    AStream.SaveToFile(AFileName);
  finally
    AStream.Free;
  end;
end;

procedure TBGRAKnob.LoadFromFile(AFileName: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFileName);
    ReadComponentFromTextStream(AStream, TComponent(Self), @OnFindClass);
  finally
    AStream.Free;
  end;
end;

procedure TBGRAKnob.OnFindClass(Reader: TReader; const AClassName: string;
  var ComponentClass: TComponentClass);
begin
  if CompareText(AClassName, 'TBGRAKnob') = 0 then
    ComponentClass := TBGRAKnob;
end;

end.

