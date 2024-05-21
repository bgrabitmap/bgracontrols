// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Iintially written by Circular.
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)
- Sandy Ganz | sganz@pacbell.net
  (added range, sector, and other features)
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
        kptHollowCircle);
    TKnobType = (ktRange, ktSector);
    TBGRAKnobValueChangedEvent = procedure(Sender : TObject; Value : Single) of object;

    { TBGRAKnob }

    TBGRAKnob = class(TBGRAGraphicCtrl)
    private
        { Private declarations }
        FPhong : TPhongShading;
        FCurveExponent : Single;
        FKnobBmp : TBGRABitmap;
        FKnobColor : TColor;
        FAngularPos : Single;  // In RADIANS
        FPositionColor : TColor;
        FPositionMargin : Single;
        FPositionOpacity : Byte;
        FPositionType : TBGRAKnobPositionType;
        FPositionWidth : Single;
        FSettingAngularPos : Boolean;
        FUsePhongLighting : Boolean;
        FMinValue, FMaxValue : Single;        // Knob Values
        FStartAngle, FEndAngle : Single;      // Knob Angles
        FKnobType : TKnobType;
        FOnKnobValueChange : TBGRAKnobValueChangedEvent;
        FStartFromBottom : Boolean;
        FWheelSpeed : Byte;                   // 0 : no wheel, 1 slowest, 255 fastest
        FWheelSpeedFactor : Single;
        FWheelWrap : Boolean;
        FSlowSnap : Boolean;
        FReverseScale : Boolean;
        FSectorDivisions : Integer;

        procedure CreateKnobBmp;
        function GetLightIntensity : Integer;
        function GetValue : Single;
        function AngularPosToDeg(RadPos : Single) : Single;
        function DegPosToAngular(DegPos : Single) : Single;
        procedure SetCurveExponent(const AValue : Single);
        procedure SetLightIntensity(const AValue : Integer);
        procedure SetStartFromBottom(const AValue : Boolean);
        procedure SetValue(AValue : Single);
        procedure SetMaxValue(AValue : Single);
        procedure SetMinValue(AValue : Single);
        procedure SetStartAngle(AValue : Single);
        procedure SetEndAngle(AValue : Single);
        procedure SetKnobType(const AValue : TKnobType);
        procedure SetPositionColor(const AValue : TColor);
        procedure SetPositionMargin(AValue : Single);
        procedure SetPositionOpacity(const AValue : Byte);
        procedure SetPositionType(const AValue : TBGRAKnobPositionType);
        procedure SetPositionWidth(const AValue : Single);
        procedure SetUsePhongLighting(const AValue : Boolean);
        procedure UpdateAngularPos(X, Y : Integer);
        procedure SetKnobColor(const AValue : TColor);
        procedure SetWheelSpeed(AValue : Byte);
        procedure SetReverseScale(AValue : Boolean);
        procedure SetSectorDivisions(AValue : Integer);

    protected
        { Protected declarations }

        class function GetControlClassDefaultSize : TSize; override;

        procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
        procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
        procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
        procedure Paint; override;
        procedure Resize; override;
        function ValueCorrection(var AValue : Single) : Boolean; overload; virtual;
        function ValueCorrection : Boolean; overload; virtual;
        function DoMouseWheel(Shift : TShiftState; WheelDelta : Integer; MousePos : TPoint) : Boolean; override;
        procedure MouseWheelPos({%H-}Shift : TShiftState; WheelDelta : Integer); virtual;
        function RemapRange(OldValue : Single; OldMin, OldMax, NewMin, NewMax : Single) : Single;
        function CalcValueFromSector(SectorIdx : Integer) : Single;
        function CalcSectorFromValue(AValue : Single) : Integer;
        function AngularPosSector(AValue : Single) : Single;
    public
        { Public declarations }
        constructor Create(AOwner : TComponent); override;
        destructor Destroy; override;
    public
        { Streaming }
        {$IFDEF FPC}
    procedure SaveToFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);
        {$ENDIF}
        procedure OnFindClass({%H-}Reader : TReader; const AClassName : String; var ComponentClass : TComponentClass);
    published
        { Published declarations }
        property Anchors;
        property CurveExponent : Single read FCurveExponent write SetCurveExponent;
        property KnobColor : TColor read FKnobColor write SetKnobColor;
        property LightIntensity : Integer read GetLightIntensity write SetLightIntensity;
        property PositionColor : TColor read FPositionColor write SetPositionColor;
        property PositionWidth : Single read FPositionWidth write SetPositionWidth;
        property PositionOpacity : Byte read FPositionOpacity write SetPositionOpacity;
        property PositionMargin : Single read FPositionMargin write SetPositionMargin;
        property PositionType : TBGRAKnobPositionType read FPositionType write SetPositionType;
        property UsePhongLighting : Boolean read FUsePhongLighting write SetUsePhongLighting;
        property MinValue : Single read FMinValue write SetMinValue;
        property MaxValue : Single read FMaxValue write SetMaxValue;
        property StartFromBottom : Boolean read FStartFromBottom write SetStartFromBottom;
        property StartAngle : Single read FStartAngle write SetStartAngle;
        property EndAngle : Single read FEndAngle write SetEndAngle;
        property KnobType : TKnobType read FKnobType write SetKnobType;
        property Value : Single read GetValue write SetValue;
        property OnValueChanged : TBGRAKnobValueChangedEvent read FOnKnobValueChange write FOnKnobValueChange;
        property WheelSpeed : Byte read FWheelSpeed write SetWheelSpeed;
        property WheelWrap : Boolean read FWheelWrap write FWheelWrap;
        property SlowSnap : Boolean read FSlowSnap write FSlowSnap;
        property ReverseScale : Boolean read FReverseScale write SetReverseScale;
        property SectorDivisions : Integer read FSectorDivisions write SetSectorDivisions;
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
    WHEELSPEEDFACTOR = 20.0;  // used to calculate mouse wheel speed
    WHEELSPEEDBASE   = 300;
    VERSION = 2.00;           // knob version

implementation

uses Math;

{$IFDEF FPC}
procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBGRAKnob]);
end;
{$ENDIF}

    { TBGRAKnob }

// Override the base class which has a rectangular dimension, odd for a knob
class function TBGRAKnob.GetControlClassDefaultSize : TSize;
begin
    Result.CX := 50;
    Result.CY := 50;
end;

procedure TBGRAKnob.CreateKnobBmp;
var
    tx, ty : Integer;
    h :      Single;
    d2 :     Single;
    v :      TPointF;
    p :      PBGRAPixel;
    center : TPointF;
    yb :     Integer;
    xb :     Integer;
    mask :   TBGRABitmap;
    Map :    TBGRABitmap;
    BGRAKnobColor : TBGRAPixel;
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
                d2 := v {$if FPC_FULLVERSION < 030301} * {$ELSE} ** {$ENDIF} v;

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

function TBGRAKnob.GetLightIntensity : Integer;
begin
    Result := round(FPhong.LightSourceIntensity);
end;

function TBGRAKnob.AngularPosToDeg(RadPos : Single) : Single;
begin
    // helper to convert AnglePos in radians to degrees, wraps as needed

    Result := RadPos * 180 / Pi;

    if Result < 0 then
        Result := Result + 360;

    Result := 270 - Result;

    if Result < 0 then
        Result := Result + 360;
end;

function TBGRAKnob.DegPosToAngular(DegPos : Single) : Single;
begin
    // helper to convert Angle in degrees to radians

    Result := 3 * Pi / 2 - DegPos * Pi / 180;
    if Result > Pi then
        Result := Result - (2 * Pi);
    if Result < -Pi then
        Result := Result + (2 * Pi);
end;

function TBGRAKnob.AngularPosSector(AValue : Single) : Single;
var
    valueMapped : Single;
    sectorIdx :   Integer;
begin
    // AValue is the degree angle of FAngularPos of where the mouse is
    // typically. So no restrictions on values, 0 to < 360

    if AValue > FEndAngle then
        Avalue := FEndAngle;
    if AValue < FStartAngle then
        Avalue := FStartAngle;

    // from the current angular pos get the value
    valueMapped := RemapRange(AValue, FStartAngle, FEndAngle, FMinValue, FMaxValue);

    // now with that value we can see what sector is returned
    sectorIdx := CalcSectorFromValue(valueMapped);

    // once we have the sector we need to get back to the value for that sector
    valueMapped := CalcValueFromSector(sectorIdx);

    // now get back the FAngularPos after mapping
    Result := DegPosToAngular(RemapRange(valueMapped, FMinValue, FMaxValue, FStartAngle, FEndAngle));
end;

function TBGRAKnob.ValueCorrection(var AValue : Single) : Boolean;
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

function TBGRAKnob.ValueCorrection : Boolean;
var
    LValue : Single;
begin
    LValue := AngularPosToDeg(FAngularPos);   // this always needs to be in Degrees of position (NOT VALUE)
    Result := ValueCorrection(LValue);        // LValue modified by call

    if Result then
      FAngularPos := DegPosToAngular(LValue); // Back to Radians
end;

function TBGRAKnob.GetValue : Single;
begin
    // Maintains the correct value range based on knobtype, result in terms of FMinValue and MaxValue

    Result := RemapRange(AngularPosToDeg(FAngularPos), FStartAngle, FEndAngle, FMinValue, FMaxValue); // user range

    // Check to Reverse the scale and fix value

    if FReverseScale then
        Result := FMaxValue + FMinValue - Result;

    if FKnobType = ktSector then
        Result := CalcSectorFromValue(Result);

end;

procedure TBGRAKnob.SetValue(AValue : Single);
var
    NewAngularPos : Single;
begin
    // AValue in the range of FStartAngle and FEndAngles after the mapping

    if FKnobType = ktSector then
      begin
        // Range check for ktSector mode

        if (AValue < 0) then
            AValue := 0;

        if (AValue > 255) then
            AValue := 255;

        AValue := CalcValueFromSector(Round(AValue));    // Round to sector
      end;

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

function TBGRAKnob.RemapRange(OldValue : Single; OldMin, OldMax, NewMin, NewMax : Single) : Single;
begin
    // Generic mapping of ranges. Value is the number to remap, returns number in the new range
    // Simple version will toss an exception IF OldMax = OldMin (Div 0) or NewMax = NewMin (no range)
    // Can't happen with properties in the IDE, but can possibly progamatically

    Result := (((OldValue - OldMin) * (NewMax - NewMin)) / (OldMax - OldMin)) + NewMin;
end;

function TBGRAKnob.CalcValueFromSector(SectorIdx : Integer) : Single;
var
    sectorSpan, secValue : Single;
begin
    // Given a sector offset get the value where it's at.

    // Check for some sane values, note - range of ktSector is 0-255 inclusive at this time
    // setting to knob type to ktSector sets these, and user can't reset

    if SectorIdx < MinValue then
      begin
        Result := FMinValue;
        exit;
      end;

    if SectorIdx > MaxValue then
      begin
        Result := FMaxValue;
        exit;
      end;

    sectorSpan := (FMaxValue - FMinValue) / FSectorDivisions;
    secValue := (SectorIdx) * SectorSpan;

    Result := secValue;
end;

function TBGRAKnob.CalcSectorFromValue(AValue : Single) : Integer;
var
    sectorSpan : Single;
    secValue :   Byte;
begin
    // for this we need to get the matching sector that the value lands in.
    // If we are PAST the previous sector (ends of a sector range is the NEXT Sector), we are in that
    // next sector, so sector endpoints are the sector starts, For 2 sectors
    // angles of 0-178 (In first) 179-360 (In second) etc.

    sectorSpan := (FMaxValue - FMinValue) / FSectorDivisions;
    secValue := Round(AValue / sectorSpan);
    Result := secValue;
end;

procedure TBGRAKnob.SetCurveExponent(const AValue : Single);
begin
    if FCurveExponent = AValue then
        exit;

    FCurveExponent := AValue;
    FreeAndNil(FKnobBmp);
    Invalidate;
end;

procedure TBGRAKnob.SetKnobColor(const AValue : TColor);
begin
    if FKnobColor = AValue then
        exit;

    FKnobColor := AValue;
    FreeAndNil(FKnobBmp);
    Invalidate;
end;

procedure TBGRAKnob.SetWheelSpeed(AValue : Byte);
begin
    // Sets the mouse wheel speed

    FWheelSpeed := AValue;
end;

procedure TBGRAKnob.SetReverseScale(AValue : Boolean);
var
    oldVal : Single;
begin
    // Sets the state of the scale, will move to match value

    if FReverseScale = AValue then
        exit;
    oldVal := GetValue;
    FReverseScale := AValue;
    SetValue(oldVal);
end;

procedure TBGRAKnob.SetSectorDivisions(AValue : Integer);
begin
    // Sets the number of sector divisions, only used if knob is ktSector
    // Valid range for sectors 1-255 inclusive

    if (FSectorDivisions = AValue) or (AValue < 1) or (Avalue > 255) then
        exit;

    FSectorDivisions := AValue;
    invalidate;
end;

procedure TBGRAKnob.SetLightIntensity(const AValue : Integer);
begin
    if AValue <> FPhong.LightSourceIntensity then
      begin
        FPhong.LightSourceIntensity := AValue;
        FreeAndNil(FKnobBmp);
        Invalidate;
      end;
end;

procedure TBGRAKnob.SetStartFromBottom(const AValue : Boolean);
begin
    if FStartFromBottom = AValue then
        exit;
    FStartFromBottom := AValue;
    Invalidate;
end;

procedure TBGRAKnob.SetEndAngle(AValue : Single);
var
    oldValue : Single;
begin
    // degrees for position of start position

    if (FEndAngle = AValue) or (FStartAngle >= AValue) or (AValue < 0) or (AValue >= 360) then
        exit;

    // If we are going to change the angle, we need to save off the value
    // as it will change the value if we don't reset it

    oldValue := GetValue;
    FEndAngle := AValue;

    if FStartAngle > FEndAngle then
        FStartAngle := FEndAngle;

    SetValue(oldValue); // Invalidate the hard way, preserve Value for user
end;

procedure TBGRAKnob.SetStartAngle(AValue : Single);
var
    oldValue : Single;
begin
    // degrees for position of start position

    if (FStartAngle = AValue) or (FEndAngle <= AValue) or (AValue < 0) or (AValue >= 360) then
        exit;

    oldValue := GetValue;
    FStartAngle := AValue;

    if FEndAngle < FStartAngle then
        FEndAngle := FStartAngle;

    SetValue(oldValue); // Invalidate the hard way, preserve Value for user
end;

procedure TBGRAKnob.SetMaxValue(AValue : Single);
var
    oldValue : Single;
begin
    // Min and Max Can't be the same or possible exception in RemapRange()
    // Also only allow set if knobtype is ktRange, the rest are fixed values
    // Note : MinValue and MaxValue can span negative ranges and be increasing
    //        decreasing

    if (FMinValue = AValue) or (FKnobType <> ktRange) then
        exit;

    oldValue := GetValue;
    FMaxValue := AValue;
    SetValue(oldValue);
end;

procedure TBGRAKnob.SetMinValue(AValue : Single);
var
    oldValue : Single;
begin
    // Min and Max Can't be the same or possible exception in RemapRange()
    // Also only allow set if knobtype is ktRange, the rest are fixed values
    // Note : MinValue and MaxValue can span negative ranges and be increasing
    //        decreasing

    if (FMaxValue = AValue) or (FKnobType <> ktRange) then
        exit;

    oldValue := GetValue;
    FMinValue := AValue;
    SetValue(oldValue);
end;

procedure TBGRAKnob.SetKnobType(const AValue : TKnobType);
begin
    // Set the knobtype, and set min and max values as needed. If
    // this is ktRange type, we don't need to do anything as it's
    // will use the FMinValue/FMaxValue as needed to compute stuff.

    FKnobType := AValue;

    if FKnobType = ktSector then
      begin
        FMinValue := 0;         // MUST start at 0
        FMaxValue := 255;       // MUST end at 255

        // if sector was set and in range, leave it. Otherwise default it

        if (FSectorDivisions < 1) or (FSectorDivisions > 255) then
           FSectorDivisions := 3;
      end;

    // No other changes for ktRange mode
end;

procedure TBGRAKnob.SetPositionColor(const AValue : TColor);
begin
    if FPositionColor = AValue then
        exit;
    FPositionColor := AValue;
    Invalidate;
end;

procedure TBGRAKnob.SetPositionMargin(AValue : Single);
begin
    if FPositionMargin = AValue then
        exit;
    FPositionMargin := AValue;
    Invalidate;
end;

procedure TBGRAKnob.SetPositionOpacity(const AValue : Byte);
begin
    if FPositionOpacity = AValue then
        exit;
    FPositionOpacity := AValue;
    Invalidate;
end;

procedure TBGRAKnob.SetPositionType(const AValue : TBGRAKnobPositionType);
begin
    if FPositionType = AValue then
        exit;
    FPositionType := AValue;
    Invalidate;
end;

procedure TBGRAKnob.SetPositionWidth(const AValue : Single);
begin
    if FPositionWidth = AValue then
        exit;
    FPositionWidth := AValue;
    Invalidate;
end;

procedure TBGRAKnob.SetUsePhongLighting(const AValue : Boolean);
begin
    if FUsePhongLighting = AValue then
        exit;
    FUsePhongLighting := AValue;
    FreeAndNil(FKnobBmp);
    Invalidate;
end;

procedure TBGRAKnob.UpdateAngularPos(X, Y : Integer);
var
    FPreviousPos, Sign : Single;
    prevAngle, currAngle : Single;
begin
    // Saves a previous position for the SlowSnap functionality.
    // Uses that to see how far we have moved to see if we should move

    FPreviousPos := FAngularPos;
    prevAngle := AngularPosToDeg(FAngularPos); // Need these in degrees!

    if FStartFromBottom then
        Sign := 1
    else
        Sign := -1;

    // possibly save off the existing sector IDX before so we can see if
    // we should move off of it

    FAngularPos := ArcTan2((-Sign) * (Y - ClientHeight / 2) / ClientHeight, Sign *
        (X - ClientWidth / 2) / ClientWidth);

    currAngle := AngularPosToDeg(FAngularPos);

    // If sector mode then we need to translate angle to sector and back to simulate each sector

    if FKnobType = ktSector then
        FAngularPos := AngularPosSector(currAngle);

    ValueCorrection;

    // If SlowSnap mode make sure past the Min/Max angles before snapping.
    // This leasts to less twitchy behavior near endpoints. This may not make sense
    // when in ktSector mode so skip if that

    if FSlowSnap and (FKnobType <> ktSector) then
        if ((currAngle <= FStartAngle) and (prevAngle = FEndAngle)) or
            ((CurrAngle >= FEndAngle) and (PrevAngle = FStartAngle)) then
            FAngularPos := FPreviousPos;

    Invalidate;

    if (FPreviousPos <> FAngularPos) and Assigned(FOnKnobValueChange) then
        FOnKnobValueChange(Self, Value); // Value passes back with data based on knobtype

end;

procedure TBGRAKnob.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
    inherited MouseDown(Button, Shift, X, Y);
    if Button = mbLeft then
      begin
        FSettingAngularPos := True;
        UpdateAngularPos(X, Y);
      end;
end;

procedure TBGRAKnob.MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
    inherited MouseUp(Button, Shift, X, Y);
    if Button = mbLeft then
        FSettingAngularPos := False;
end;

procedure TBGRAKnob.MouseMove(Shift : TShiftState; X, Y : Integer);
begin
    inherited MouseMove(Shift, X, Y);
    if FSettingAngularPos then
        UpdateAngularPos(X, Y);
end;

procedure TBGRAKnob.Paint;
var
    Bmp :      TBGRABitmap;
    Center, Pos : TPointF;
    PosColor : TBGRAPixel;
    PosLen :   Single;
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
        kptLineSquareCap :
          begin
            Bmp.LineCap := pecSquare;
            Bmp.DrawLineAntialias(Center.X, Center.Y, Pos.X, Pos.Y, PosColor, FPositionWidth);
          end;
        kptLineRoundCap :
          begin
            Bmp.LineCap := pecRound;
            Bmp.DrawLineAntialias(Center.X, Center.Y, Pos.X, Pos.Y, PosColor, FPositionWidth);
          end;
        kptFilledCircle :
          begin
            Bmp.FillEllipseAntialias(Pos.X, Pos.Y, FPositionWidth, FPositionWidth, PosColor);
          end;
        kptHollowCircle :
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
    if (FKnobBmp <> nil) and ((ClientWidth <> FKnobBmp.Width) or (ClientHeight <> FKnobBmp.Height)) then
        FreeAndNil(FKnobBmp);
end;

constructor TBGRAKnob.Create(AOwner : TComponent);
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
    FWheelSpeed := 0;        // 0, no wheel, 1 slowest, 255 fastest
    FWheelSpeedFactor := WHEELSPEEDFACTOR;   // factor for the mousewheel speed
    FWheelWrap := False;     // don't allow the mouse wheel to wrap around
    FSlowSnap := False;      // True  : no immediate snap around on min/max
    FReverseScale := False;  // False : FMinValue to FMaxValue, True : FMaxValue to FMinValue
    FSectorDivisions := 1;   // Number of divisions for sector knob
    FKnobType := ktRange;    // Defaults ranges to match orig knob
    FStartAngle := 30;
    FEndAngle := 330;
    FMinValue := 30;
    FMaxValue := 330;
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

procedure TBGRAKnob.OnFindClass(Reader : TReader; const AClassName : String; var ComponentClass : TComponentClass);
begin
    if CompareText(AClassName, 'TBGRAKnob') = 0 then
        ComponentClass := TBGRAKnob;
end;

function TBGRAKnob.DoMouseWheel(Shift : TShiftState; WheelDelta : Integer; MousePos : TPoint) : Boolean;
begin
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
    MouseWheelPos(Shift, WheelDelta);
end;

procedure TBGRAKnob.MouseWheelPos(Shift : TShiftState; WheelDelta : Integer);
var
    newValue : Single;
begin
    // WheelSpeed is a Base Value and a factor to slow or speed up the wheel affect.
    // FWheelSpeed => 0, no wheel, 1 slowest movement, 255 fastest movement

    if FWheelSpeed > 0 then
      begin

        if FKnobType = ktRange then
          begin
            newValue := Value + (FMaxValue - FMinValue) * WheelDelta /
                ((WHEELSPEEDBASE - FWheelSpeed) * FWheelSpeedFactor);
            // Check for wrap in either direction

            if FWheelWrap then
              begin
                if newValue > FMaxValue then
                    newValue := FMinValue;

                if newValue < FMinValue then
                    newValue := FMaxValue;
              end;
          end
        else
          begin
            // Jumps are now always 1 or -1, in terms of sectors, note wheel speed
            // does not make any difference in ktSector mode since we can only bump 1/-1
            // value or it will rounded back to an integral value an not move

            if WheelDelta < 0 then
              begin
                newValue := Value - 1.0;
                if newValue < 0 then    // Going backwards
                  begin
                    if FWheelWrap then
                      begin
                        newValue := FSectorDivisions;
                      end
                    else
                        newValue := 0;  // Back to start
                  end;
              end
            else
              begin
                // Move Forward, check for wrap

                newValue := Value + 1.0;

                if FWheelWrap then
                  begin
                    if newValue > FSectorDivisions then // Going Forward
                        newValue := 0;

                    if newValue < 0 then // Going backwards
                        newValue := FSectorDivisions;
                  end;
              end;
          end;

        SetValue(newValue);
      end;

    if Assigned(FOnKnobValueChange) then
        FOnKnobValueChange(Self, Value);
end;

end.
