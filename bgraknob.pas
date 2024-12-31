// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Initially written by Circular.
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)
- Sandy Ganz | sganz@pacbell.net
  Added range, sector, and other features
  12/30/2024 - Added option for audio taper, and no position draw (kptNone)
***************************** END CONTRIBUTOR(S) *****************************}

unit BGRAKnob;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Forms, Controls, Graphics,
  {$IFNDEF FPC}BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCBaseCtrls, BGRAGradients, BGRABitmap, BGRABitmapTypes;

type
  TBGRAKnobPositionType = (kptLineSquareCap, kptLineRoundCap, kptFilledCircle,
    kptHollowCircle, kptNone);
  TKnobType = (ktRange, ktSector);
  TKnobTaperType = (kttLinear, kttAudioSlow, kttAudioFast);
  TBGRAKnobValueChangedEvent = procedure(Sender: TObject; Value: single) of object;

  { TBGRAKnob }

  TBGRAKnob = class(TBGRAGraphicCtrl)
  private
    { Private declarations }
    FPhong: TPhongShading;
    FCurveExponent: single;
    FKnobBmp: TBGRABitmap;
    FKnobColor: TColor;
    FAngularPos: single;  // In RADIANS
    FPositionColor: TColor;
    FPositionMargin: single;
    FPositionOpacity: byte;
    FPositionType: TBGRAKnobPositionType;
    FPositionWidth: single;
    FSettingAngularPos: boolean;
    FTaperType: TKnobTaperType;
    FUsePhongLighting: boolean;
    FMinValue, FMaxValue: single;        // Knob Values
    FStartAngle, FEndAngle: single;      // Knob Angles
    FKnobType: TKnobType;
    FOnKnobValueChange: TBGRAKnobValueChangedEvent;
    FStartFromBottom: boolean;
    FWheelSpeed: byte;                   // 0 : no wheel, 1 slowest, 255 fastest
    FWheelWrap: boolean;
    FSlowSnap: boolean;
    FReverseScale: boolean;
    FSectorDivisions: integer;           // Computed internally from FMinValue/FMaxValue

    function AudioTaperMapping(x, K : single): single;
    function InverseAudioTaperMapping(y, K : single): single;
    procedure CreateKnobBmp;
    function GetLightIntensity: integer;
    function GetValue: single;
    function AngularPosToDeg(RadPos: single): single;
    function DegPosToAngular(DegPos: single): single;
    procedure SetCurveExponent(const AValue: single);
    procedure SetLightIntensity(const AValue: integer);
    procedure SetStartFromBottom(const AValue: boolean);
    procedure SetValue(AValue: single);
    procedure SetMaxValue(AValue: single);
    procedure SetMinValue(AValue: single);
    procedure SetStartAngle(AValue: single);
    procedure SetEndAngle(AValue: single);
    procedure SetKnobType(const AValue: TKnobType);
    procedure SetPositionColor(const AValue: TColor);
    procedure SetPositionMargin(AValue: single);
    procedure SetPositionOpacity(const AValue: byte);
    procedure SetPositionType(const AValue: TBGRAKnobPositionType);
    procedure SetPositionWidth(const AValue: single);
    procedure SetUsePhongLighting(const AValue: boolean);
    procedure UpdateAngularPos(X, Y: integer);
    procedure SetKnobColor(const AValue: TColor);
    procedure SetWheelSpeed(AValue: byte);
    procedure SetReverseScale(AValue: boolean);
    procedure SetTaperType(AValue: TKnobTaperType);

  protected
    { Protected declarations }

    class function GetControlClassDefaultSize: TSize; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure Paint; override;
    procedure Resize; override;
    function ValueCorrection(var AValue: single): boolean; overload; virtual;
    function ValueCorrection: boolean; overload; virtual;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: integer; MousePos: TPoint): boolean; override;
    procedure MouseWheelPos({%H-}Shift: TShiftState; WheelDelta: integer); virtual;
    function RemapRange(OldValue: single; OldMin, OldMax, NewMin, NewMax: single): single;
    function AngularPosSector(AValue: single): single;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    { Streaming }
    {$IFDEF FPC}
    procedure SaveToFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);
    {$ENDIF}
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
  published
    { Published declarations }
    property Anchors;
    property CurveExponent: single read FCurveExponent write SetCurveExponent nodefault;
    property KnobColor: TColor read FKnobColor write SetKnobColor default clBtnFace;
    property LightIntensity: integer read GetLightIntensity write SetLightIntensity default 300;
    property PositionColor: TColor read FPositionColor write SetPositionColor default clBtnText;
    property PositionWidth: single read FPositionWidth write SetPositionWidth default 4;
    property PositionOpacity: byte read FPositionOpacity write SetPositionOpacity default 192;
    property PositionMargin: single read FPositionMargin write SetPositionMargin default 4;
    property PositionType: TBGRAKnobPositionType
      read FPositionType write SetPositionType default kptLineSquareCap;
    property UsePhongLighting: boolean read FUsePhongLighting write SetUsePhongLighting default true;
    property MinValue: single read FMinValue write SetMinValue nodefault;
    property MaxValue: single read FMaxValue write SetMaxValue nodefault;
    property StartFromBottom: boolean read FStartFromBottom write SetStartFromBottom default true;
    property StartAngle: single read FStartAngle write SetStartAngle default 30;
    property EndAngle: single read FEndAngle write SetEndAngle default 330;
    property KnobType: TKnobType read FKnobType write SetKnobType default ktRange;
    property TaperType: TKnobTaperType read FTaperType write SetTaperType default kttLinear;
    property Value: single read GetValue write SetValue nodefault;
    property OnValueChanged: TBGRAKnobValueChangedEvent
      read FOnKnobValueChange write FOnKnobValueChange;
    property WheelSpeed: byte read FWheelSpeed write SetWheelSpeed default 0;
    property WheelWrap: boolean read FWheelWrap write FWheelWrap default false;
    property SlowSnap: boolean read FSlowSnap write FSlowSnap default false;
    property ReverseScale: boolean read FReverseScale write SetReverseScale default false;
    property OnMouseWheel;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

  {$IFDEF FPC}
        procedure Register;
  {$ENDIF}

const
  VERSIONSTR = '2.2';      // knob version

implementation

uses Math;

const
  WHEELSPEEDFACTOR = 20.0;  // used to calculate mouse wheel speed
  WHEELSPEEDBASE = 300;
  AUDIO_TAPER_SLOW_K = 8;
  AUDIO_TAPER_FAST_K = 4;

{$IFDEF FPC}
procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBGRAKnob]);
end;
{$ENDIF}

{ TBGRAKnob }

// AudioTaperMapping will estimate the curve of an Audio Taper
// potentiometer. The value of 'x' typically from a linear set
// and is mapped to a curve that will simulate the curve
// of an Audio taper potentiometer. A few types of exists, but
// for here we are looking at 10% of the Max Value as 'AudioSlow'
// when knob at 50%. 'AudioFast' is the same but at 50% the
// value is at 15% of Max.
// Typically the Max should be at 100 and Min at 0 for this
// to make sense. Other values may not do what you think.
//
// The value to be mapped is 'x', and the factor 'K' is
// how 'curvey' the line is.
//
// For MinValue = 0 and MaxValue = 100 Below are the goal
//
// For values of K = 8, this gives a slow acting curve
// where at mid position (50%) the value is around 10% of
// the Max.
//
// For values of K = 4, this gives a faster acting curve
// where at mid position (50%) the value is around 15% of
// the Max.
//
// The Mapping/Inverse must both use the same 'K'
//
// While MinValue and MaxValue can be anything, typically
// MinValue = 0, and MaxValue 100. Think in percent. Experiment
// and see. MinValue = 0, and MaxValue = 1.0 also works well.

// Linear to AudioTaper
function TBGRAKnob.AudioTaperMapping(x, K : single): single;
var
  sign_change : single;
begin
  // simple version
  sign_change := 1;
  if x < 0 then
  begin
    x := abs(x);
    sign_change := -1;
  end;
  x := x / FMaxValue; // scale

  // Simulate the curve from a linear space
  Result := x / (1 + (1 - x) * K) * FMaxValue * sign_change;
end;

// Same Idea here but the inverse so we can map back an Audio taper
// value back to a linear one for the knob to be set.

function TBGRAKnob.InverseAudioTaperMapping(y, K : single): single;
var
  sign_change : single;
begin
  sign_change := 1;
  if y < 0 then
  begin
    y := abs(y);
    sign_change := -1;
  end;
  y := y / FMaxValue; // scale

  // reverse the curve to a linear space

  Result :=  (y + y * K) / (1 + y * K) * FMaxValue * sign_change;
end;

// Override the base class which has a rectangular dimension, odd for a knob

class function TBGRAKnob.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 50;
  Result.CY := 50;
end;

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
  BGRAKnobColor := KnobColor;

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

        v.x := v.x / (tx / 2 + 1);
        v.y := v.y / (ty / 2 + 1);

        //compute squared distance with scalar product

        d2 := v {$if FPC_FULLVERSION < 30203}*{$ELSE}**{$ENDIF} v;

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
  // Maintains the correct value range based on knobtype, result in terms of
  // FMinValue and FMaxValue

  Result := RemapRange(AngularPosToDeg(FAngularPos), FStartAngle,
    FEndAngle, FMinValue, FMaxValue);

  // Check to Reverse the scale and fix value

  if FReverseScale then
    Result := FMaxValue + FMinValue - Result;

  if FKnobType = ktSector then
    Result := Round(Result);

  // After all the mess above, map it to AudioTaper curves if needed.

  if FTaperType = kttAudioSlow THEN
    Result := AudioTaperMapping(Result, AUDIO_TAPER_SLOW_K)
  else
      if FTaperType = kttAudioFast THEN
        Result := AudioTaperMapping(Result, AUDIO_TAPER_FAST_K)
end;

function TBGRAKnob.AngularPosToDeg(RadPos: single): single;
begin
  // helper to convert AnglePos in radians to degrees, wraps as needed

  Result := RadPos * 180 / Pi;

  if Result < 0 then
    Result := Result + 360;

  Result := 270 - Result;

  if Result < 0 then
    Result := Result + 360;
end;

function TBGRAKnob.DegPosToAngular(DegPos: single): single;
begin
  // helper to convert Angle in degrees to radians

  Result := 3 * Pi / 2 - DegPos * Pi / 180;
  if Result > Pi then
    Result := Result - (2 * Pi);
  if Result < -Pi then
    Result := Result + (2 * Pi);
end;

function TBGRAKnob.AngularPosSector(AValue: single): single;
var
  sector: integer;
begin
  // AValue is the degree angle of FAngularPos of where the mouse is
  // typically. So no restrictions on values, 0 to < 360

  if AValue > FEndAngle then
    Avalue := FEndAngle;
  if AValue < FStartAngle then
    Avalue := FStartAngle;

  // from the current angular pos get the value
  sector := Round(RemapRange(AValue, FStartAngle, FEndAngle, FMinValue, FMaxValue));

  // now get back the FAngularPos after mapping
  Result := DegPosToAngular(RemapRange(sector, FMinValue, FMaxValue, FStartAngle, FEndAngle));
end;

function TBGRAKnob.ValueCorrection(var AValue: single): boolean;
begin
  if AValue < FStartAngle then
  begin
    AValue := FStartAngle;
    Result := True;
  end
  else
  if AValue > FEndAngle then
  begin
    AValue := FEndAngle;
    Result := True;
  end
  else
    Result := False;
end;

function TBGRAKnob.ValueCorrection: boolean;
var
  LValue: single;
begin
  LValue := AngularPosToDeg(FAngularPos);

  // this always needs to be in Degrees of position (NOT VALUE)

  Result := ValueCorrection(LValue);        // LValue modified by call

  if Result then
    FAngularPos := DegPosToAngular(LValue); // Back to Radians
end;

function TBGRAKnob.RemapRange(OldValue: single;
  OldMin, OldMax, NewMin, NewMax: single): single;
begin
  // Generic mapping of ranges. Value is the number to remap, returns number
  // in the new range. Looks for odd div by 0 condition and fixes

  if OldMin = OldMax then
  begin
    if OldValue <= OldMin then
      exit(NewMin)
    else
      exit(NewMax);
  end;

  Result := (((OldValue - OldMin) * (NewMax - NewMin)) / (OldMax - OldMin)) + NewMin;
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

procedure TBGRAKnob.SetWheelSpeed(AValue: byte);
begin
  // Sets the mouse wheel speed

  FWheelSpeed := AValue;
end;

procedure TBGRAKnob.SetReverseScale(AValue: boolean);
var
  oldVal: single;
begin
  // Sets the direction of the scale

  if FReverseScale = AValue then
    exit;

  oldVal := GetValue;
  FReverseScale := AValue;
  SetValue(oldVal);
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

  // first things, if we are doing audio taper, then inverse map it

  if FTaperType = kttAudioSlow THEN
    AValue := InverseAudioTaperMapping(AValue, AUDIO_TAPER_SLOW_K)
  else
      if FTaperType = kttAudioFast THEN
        AValue := InverseAudioTaperMapping(AValue, AUDIO_TAPER_FAST_K);

  // carry on with range checks, AValue is in user space not degrees until later

  if AValue > FMaxValue then
    AValue := FMaxValue;

  if AValue < FMinValue then
    AValue := FMinValue;

  // Get the integeral value from given sector,

  if FKnobType = ktSector then
    AValue := Round(AValue);  // Round to sector

  AValue := RemapRange(AValue, FMinValue, FMaxValue, FStartAngle, FEndAngle);

  // Reverse the scale if needed

  if FReverseScale then
    AValue := FEndAngle + FStartAngle - AValue;

  ValueCorrection(AValue);

  NewAngularPos := 3 * Pi / 2 - AValue * Pi / 180;

  if NewAngularPos > Pi then
    NewAngularPos := NewAngularPos - (2 * Pi);

  if NewAngularPos < -Pi then
    NewAngularPos := NewAngularPos + (2 * Pi);

  if NewAngularPos <> FAngularPos then
  begin
    FAngularPos := NewAngularPos;
    Invalidate;
  end;
end;

procedure TBGRAKnob.SetEndAngle(AValue: single);
var
  oldValue: single;
begin
  // degrees for position of start position

  if (FEndAngle = AValue) or (FStartAngle >= AValue) or (AValue < 0) or
    (AValue >= 360) then
    exit;

  // If we are going to change the angle, we need to save off the current value
  // as it will change it if we don't reset it

  oldValue := GetValue;
  FEndAngle := AValue;

  if FStartAngle > FEndAngle then
    FStartAngle := FEndAngle;

  SetValue(oldValue); // Invalidate the hard way, preserve Value for user
end;

procedure TBGRAKnob.SetStartAngle(AValue: single);
var
  oldValue: single;
begin
  // Start angle in degrees

  if (FStartAngle = AValue) or (FEndAngle <= AValue) or (AValue < 0) or
    (AValue >= 360) then
    exit;

  oldValue := GetValue;
  FStartAngle := AValue;

  if FEndAngle < FStartAngle then
    FEndAngle := FStartAngle;

  SetValue(oldValue); // Invalidate the hard way, preserve Value for user
end;

procedure TBGRAKnob.SetMaxValue(AValue: single);
var
  oldValue: single;
  IntMinVal, IntMaxVal: integer;
begin
  // Note : MinValue and MaxValue can span negative ranges and be increasing
  //        decreasing

  // If sector mode do some math, set number of sector divisions

  if FKnobType = ktSector then
  begin

    IntMinVal := Round(FMinValue);
    IntMaxVal := Round(AValue);
    FSectorDivisions := IntMaxVal - IntMinVal;

    // Just to be safe, ensure at least 1 sector division

    if FSectorDivisions < 1 then
      FSectorDivisions := 1;

    FMinValue := IntMinVal;   // force to an integeral value if in sector mode
    AValue := IntMaxVal;
  end;

  // Min and Max Can't be the same in any case

  if (FMinValue >= AValue) then
    exit;

  oldValue := GetValue;
  FMaxValue := AValue;
  SetValue(oldValue);
end;

procedure TBGRAKnob.SetMinValue(AValue: single);
var
  oldValue: single;
  IntMinVal, IntMaxVal: integer;
begin
  // Note : MinValue and MaxValue can span negative ranges and be increasing
  //        decreasing

  // If sector mode do some math, set number of sector divisions

  if FKnobType = ktSector then
  begin
    IntMinVal := Round(AValue);
    IntMaxVal := Round(FMaxValue);
    FSectorDivisions := IntMaxVal - IntMinVal;

    // Just to be safe, ensure at least 1 sector division

    if FSectorDivisions < 1 then
      FSectorDivisions := 1;

    FMaxValue := IntMaxVal;   // force to an integeral value if in sector mode
    AValue := IntMinVal;
  end;

  // Min and Max Can't be the same in any case, rounding can also cause this

  if (FMaxValue <= AValue) then
    exit;

  // Save and refresh with proper value

  oldValue := GetValue;
  FMinValue := AValue;
  SetValue(oldValue);
end;

procedure TBGRAKnob.SetKnobType(const AValue: TKnobType);
var
  IntMinVal, IntMaxVal: integer;
begin
  // Set the knobtype, if ktRange nothing really needed,
  // if ktSector then calc and check value for divisions.

  FKnobType := AValue;

  if FKnobType = ktSector then
  begin
    IntMinVal := Round(FMinValue);
    IntMaxVal := Round(FMaxValue);
    FSectorDivisions := IntMaxVal - IntMinVal;

    if FSectorDivisions < 1 then
      FSectorDivisions := 1;
  end;

  // No other changes for ktRange mode
end;
procedure TBGRAKnob.SetTaperType(AValue: TKnobTaperType);
begin
  if FTaperType = AValue then
    Exit;

  FTaperType := AValue;
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
  prevAngle, currAngle: single;
begin
  // Saves a previous position for the SlowSnap functionality.
  // Uses that to see how far we have moved to see if we should move

  FPreviousPos := FAngularPos;
  prevAngle := AngularPosToDeg(FAngularPos); // Need these in degrees!

  if FStartFromBottom then
    Sign := 1
  else
    Sign := -1;

  FAngularPos := ArcTan2((-Sign) * (Y - ClientHeight / 2) / ClientHeight,
    Sign * (X - ClientWidth / 2) / ClientWidth);

  currAngle := AngularPosToDeg(FAngularPos);

  // If sector mode then we need to translate angle to sector and back to simulate each sector

  if FKnobType = ktSector then
    FAngularPos := AngularPosSector(currAngle);

  ValueCorrection;

  // If SlowSnap mode make sure past the Min/Max angles before snapping.
  // This is less twitchy behavior near endpoints. This may not make sense
  // when in ktSector mode so skip if that

  if FSlowSnap and (FKnobType <> ktSector) then
    if ((currAngle <= FStartAngle) and (prevAngle = FEndAngle)) or
      ((CurrAngle >= FEndAngle) and (PrevAngle = FStartAngle)) then
      FAngularPos := FPreviousPos;

  Invalidate;

  if (FPreviousPos <> FAngularPos) and Assigned(FOnKnobValueChange) then
    FOnKnobValueChange(Self, Value); // Value passes back with data based on knobtype

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

  // draw current position

  PosColor := ColorToBGRA(ColorToRGB(FPositionColor), FPositionOpacity);
  Center := PointF(ClientWidth / 2, ClientHeight / 2);
  Pos.X := Cos(FAngularPos) * (ClientWidth / 2);
  Pos.Y := -Sin(FAngularPos) * (ClientHeight / 2);
  if not FStartFromBottom then
    Pos := -Pos;
  PosLen := VectLen(Pos);

  Pos := Pos * ((PosLen - PositionMargin - FPositionWidth) / PosLen);
  Pos := Center + Pos;

  case PositionType of
    kptLineSquareCap:
    begin
      Bmp.LineCap := pecSquare;
      Bmp.DrawLineAntialias(Center.X, Center.Y, Pos.X, Pos.Y,
        PosColor, FPositionWidth);
    end;
    kptLineRoundCap:
    begin
      Bmp.LineCap := pecRound;
      Bmp.DrawLineAntialias(Center.X, Center.Y, Pos.X, Pos.Y,
        PosColor, FPositionWidth);
    end;
    kptFilledCircle:
    begin
      Bmp.FillEllipseAntialias(Pos.X, Pos.Y, FPositionWidth,
        FPositionWidth, PosColor);
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
  FTaperType := kttLinear;  // Should be default for compatibility
  FUsePhongLighting := True;
  FOnKnobValueChange := nil;
  FStartFromBottom := True;
  FWheelSpeed := 0;        // 0, no wheel, 1 slowest, 255 fastest
  FWheelWrap := False;     // don't allow the mouse wheel to wrap around
  FSlowSnap := False;      // True : less snap around on min/max
  FReverseScale := False;  // Flips direction around if True
  FSectorDivisions := 1;   // Number of divisions for sector knob, computed
  FKnobType := ktRange;    // Defaults ranges to match orig knob
  FStartAngle := 30;
  FEndAngle := 330;
  FMinValue := 30;
  FMaxValue := 330;
  SetValue(30);
end;

destructor TBGRAKnob.Destroy;
begin
  FPhong.Free;
  FKnobBmp.Free;
  inherited Destroy;
end;

{$IFDEF FPC}
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
    ReadComponentFromTextStream(AStream, TComponent(Self), OnFindClass);
  finally
    AStream.Free;
  end;
end;
{$ENDIF}

procedure TBGRAKnob.OnFindClass(Reader: TReader; const AClassName: string;
  var ComponentClass: TComponentClass);
begin
  if CompareText(AClassName, 'TBGRAKnob') = 0 then
    ComponentClass := TBGRAKnob;
end;

function TBGRAKnob.DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint): boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  MouseWheelPos(Shift, WheelDelta);
end;

procedure TBGRAKnob.MouseWheelPos(Shift: TShiftState; WheelDelta: integer);
var
  newValue: single;
begin
  // WheelSpeed is a Base Value and a factor to slow or speed up the wheel affect.
  // FWheelSpeed = 0 then no wheel, 1 slowest movement, 255 fastest movement
  //
  // Note if Mouse Wheel is used in AudioSlow or AudioFast mode, the wheel
  // will not be compensated so will seem faster at 0 side, and slower as
  // it gets to the MaxValue since it's values curved. (assumes 0min, 100max)
  // Setting the wheel speed to a low value (like 1) will help these modes

  if FWheelSpeed > 0 then
  begin

    if FKnobType = ktRange then
    begin
      newValue := Value + (FMaxValue - FMinValue) * WheelDelta /
        ((WHEELSPEEDBASE - FWheelSpeed) * WHEELSPEEDFACTOR);

      // Check for wrap in either direction

      if FWheelWrap then
      begin
        if newValue > FMaxValue then
          newValue := FMinValue
        else
          if newValue < FMinValue then
            newValue := FMaxValue;
      end;
    end
    else
    begin
      // ktSector
      // Jumps are now always 1 or -1, in terms of sectors, note wheel speed
      // does not make any difference in ktSector mode since we can only bump 1/-1
      // value or it will rounded back to an integral value an not move

      if WheelDelta < 0 then
      begin
        // Move Backwards, check for wrap

        newValue := Value - 1.0;

        if newValue < FMinValue then
        begin
          if FWheelWrap then
            newValue := FMaxValue
          else
            newValue := FMinValue;
        end;
      end
      else
      begin
        // Move Forward, check for wrap

        newValue := Value + 1.0;

        if newValue >= FMaxValue then
        begin
          if FWheelWrap then
            newValue := FMinValue
          else
            newValue := FMaxValue;
        end;
      end;
    end;

    SetValue(newValue);
  end;

  if Assigned(FOnKnobValueChange) then
    FOnKnobValueChange(Self, Value);
end;

end.
