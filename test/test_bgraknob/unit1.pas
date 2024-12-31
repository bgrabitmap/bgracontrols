unit Unit1;

{$mode objfpc}{$H+}

{
  v1.0 05-21-2024 Sandy Ganz - Begat. sganz@pacbell.net
  v2.0 05-26-2024 Sandy Ganz - Removed SectorDivision stuff, Knob now computes it.
  v3.0 12-30-2024 Sandy Ganz - Added options for Audio taper, color settings, etc.

  Hacked up test progam to test the enhanced BGRAKnob.
}

interface

uses
  Classes, LCLType, SysUtils, Forms, Controls, Graphics, StdCtrls, Buttons, MaskEdit,
  ExtCtrls, ColorBox, SpinEx, BGRAKnob, BCTrackbarUpdown;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    CurveExponentLbl: TLabel;
    CurveExponentSpe: TFloatSpinEditEx;
    Label33: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    LightIntensityLbl: TLabel;
    LightIntensitySpe: TSpinEditEx;
    PositionColorCb: TColorBox;
    PositionColorLbl: TLabel;
    KnobColorCb: TColorBox;
    KnobColorLbl: TLabel;
    GroupBox2: TGroupBox;
    Label37: TLabel;
    PositionTypeLbl: TLabel;
    PositionMarginTB: TBCTrackbarUpdown;
    PositionMarginLbl: TLabel;
    TaperTypeLbl: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    TaperTypeCb: TComboBox;
    PositionWidthLbl: TLabel;
    Label27: TLabel;
    Label34: TLabel;
    PositionWidthTB: TBCTrackbarUpdown;
    PositionTypeCb: TComboBox;
    Label22: TLabel;
    Label23: TLabel;
    KnobVerLbl: TLabel;
    Label9: TLabel;
    MouseWheelWrapCb: TCheckBox;
    ReverseScaleCb: TCheckBox;
    Label1: TLabel;
    KnobTypeLbl: TLabel;
    MouseWheelSpeedTB: TBCTrackbarUpdown;
    BGRAKnob1: TBGRAKnob;
    CDVDBtn: TBitBtn;
    ResetRangesBtn1: TBitBtn;
    Set1000Btn: TButton;
    SetNeg10Btn: TButton;
    SetNeg5Btn: TButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    StartFromBottomCb: TCheckBox;
    DVAPGb: TGroupBox;
    MinValueEdt: TEdit;
    MaxValueEdt: TEdit;
    StartAngleEdt: TEdit;
    EndAngleEdt: TEdit;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    RangesGb: TGroupBox;
    Label10: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    MinValueLbl: TLabel;
    MaxValueLbl: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    StartAngleLbl: TLabel;
    EndAngleLbl: TLabel;
    Label28: TLabel;
    Label7: TLabel;
    ResetGeneralBtn: TBitBtn;
    Set50Btn: TButton;
    Set180Btn: TButton;
    Set100Btn: TButton;
    Set360Btn: TButton;
    Set25Btn: TButton;
    Set0Btn: TButton;
    SlowSnapCb: TCheckBox;
    KnobTypeCb: TComboBox;
    ValueEdt: TEdit;
    GroupBox1: TGroupBox;
    GeneralSettingsGb: TGroupBox;
    ValueLbl: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    MouseWheelSpeedLbl: TLabel;
    SetValueBtn: TSpeedButton;
    SetMinValueBtn: TSpeedButton;
    SetMaxValueBtn: TSpeedButton;
    SetStartAngleBtn: TSpeedButton;
    SetEndAngleBtn: TSpeedButton;
    Timer1: TTimer;

    procedure BitBtn1Click(Sender: TObject);
    procedure CurveExponentSpeChange(Sender: TObject);
    procedure KnobColorCbChange(Sender: TObject);
    function KnobTypeToStr(kt : TKnobType) : string;
    function KnobTaperTypeToStr(ktt : TKnobTaperType) : string;
    function KnobPositionTypeToStr(kpt : TBGRAKnobPositionType) : string;

    procedure EndAngleEdtKeyDown(Sender: TObject; var Key: Word;
      {%H-}Shift: TShiftState);
    procedure LightIntensitySpeChange(Sender: TObject);
    procedure MaxValueEdtKeyDown(Sender: TObject; var Key: Word;
      {%H-}Shift: TShiftState);
    procedure MinValueEdtKeyDown(Sender: TObject; var Key: Word;
      {%H-}Shift: TShiftState);
    procedure MouseWheelSpeedTBChange(Sender: TObject; {%H-}AByUser: boolean);
    procedure BGRAKnob1Click(Sender: TObject);
    procedure BGRAKnob1DblClick(Sender: TObject);
    procedure BGRAKnob1MouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure BGRAKnob1MouseEnter(Sender: TObject);
    procedure BGRAKnob1MouseLeave(Sender: TObject);
    procedure BGRAKnob1MouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure BGRAKnob1MouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure BGRAKnob1MouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
      WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
    procedure BGRAKnob1ValueChanged(Sender: TObject; Value: single);
    procedure CDVDBtnClick(Sender: TObject);
    procedure MouseWheelWrapCbChange(Sender: TObject);
    procedure PositionColorCbChange(Sender: TObject);
    procedure PositionMarginTBChange(Sender: TObject; AByUser: boolean);
    procedure PositionTypeCbChange(Sender: TObject);
    procedure PositionWidthTBChange(Sender: TObject; AByUser: boolean);
    procedure ResetRangesBtn1Click(Sender: TObject);
    procedure ReverseScaleCbChange(Sender: TObject);
    procedure Set1000BtnClick(Sender: TObject);
    procedure SetNeg10BtnClick(Sender: TObject);
    procedure SetNeg5BtnClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure StartAngleEdtKeyDown(Sender: TObject; var Key: Word;
      {%H-}Shift: TShiftState);
    procedure StartFromBottomCbChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ResetGeneralBtnClick(Sender: TObject);
    procedure Set50BtnClick(Sender: TObject);
    procedure Set180BtnClick(Sender: TObject);
    procedure Set100BtnClick(Sender: TObject);
    procedure Set360BtnClick(Sender: TObject);
    procedure Set25BtnClick(Sender: TObject);
    procedure Set0BtnClick(Sender: TObject);
    procedure SlowSnapCbChange(Sender: TObject);
    procedure KnobTypeCbChange(Sender: TObject);
    procedure SetEndAngleBtnClick(Sender: TObject);
    procedure SetMaxValueBtnClick(Sender: TObject);
    procedure SetMinValueBtnClick(Sender: TObject);
    procedure SetStartAngleBtnClick(Sender: TObject);
    procedure SetValueBtnClick(Sender: TObject);
    procedure TaperTypeCbChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ValueEdtKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
  private

  public

  end;

  Const
    VERSIONSTR = 'v3.0';

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'BGRAKnob Test ' + VERSIONSTR;

  ValueLbl.Caption := FloatToStr(BGRAKnob1.Value);
  MinValueLbl.Caption := FloatToStr(BGRAKnob1.MinValue);
  MinValueEdt.Text := MinValueLbl.Caption;
  MaxValueLbl.Caption := FloatToStr(BGRAKnob1.MaxValue);
  MaxValueEdt.Text := MaxValueLbl.Caption;
  StartAngleLbl.Caption := FloatToStr(BGRAKnob1.StartAngle);
  EndAngleLbl.Caption := FloatToStr(BGRAKnob1.EndAngle);
  SlowSnapCb.Checked := BGRAKnob1.SlowSnap;
  StartFromBottomCb.Checked := BGRAKnob1.StartFromBottom;
  ReverseScaleCb.Checked := BGRAKnob1.ReverseScale;

  MouseWheelSpeedTB.Value := BGRAKnob1.WheelSpeed;
  MouseWheelSpeedLbl.Caption := IntToStr(MouseWheelSpeedTB.Value);

  KnobTypeLbl.Caption := KnobTypeToStr(BGRAKnob1.KnobType);
  PositionTypeLbl.Caption := KnobPositionTypeToStr(BGRAKnob1.PositionType);
  PositionTypeCb.ItemIndex := ord(BGRAKnob1.PositionType);
  TaperTypeLbl.Caption := KnobTaperTypeToStr(BGRAKnob1.TaperType);
  TaperTypeCb.ItemIndex := ord(BGRAKnob1.TaperType);
  PositionWidthTB.Value := Round(BGRAKnob1.PositionWidth);
  PositionWidthLbl.Caption := FloatToStr(BGRAKnob1.PositionWidth);
  PositionMarginTB.Value := Round(BGRAKnob1.PositionMargin);
  PositionMarginLbl.Caption := FloatToStr(BGRAKnob1.PositionMargin);
  KnobColorCb.Selected := BGRAKnob1.KnobColor;
  PositionColorCb.Selected := BGRAKnob1.PositionColor;
  LightIntensitySpe.Value := BGRAKnob1.LightIntensity;
  CurveExponentSpe.Value := BGRAKnob1.CurveExponent;

  KnobVerLbl.Caption := 'BGRAKnob ' + BGRAKnob.VERSIONSTR;
end;

function TForm1.KnobTypeToStr(kt : TKnobType) : string;
begin
  case kt of
     ktRange   : Result := 'ktRange';
     ktSector  : Result := 'ktSector';
  else
     Result := 'UNKNOWN';
  end;
end;

function TForm1.KnobTaperTypeToStr(ktt : TKnobTaperType) : string;
begin
   // kttLinear, kttAudioSlow, kttAudioFast

   case ktt of
      kttLinear : Result := 'kttLinear';
      kttAudioSlow : Result := 'kttAudioSlow';
      kttAudioFast : Result :='kttAudioFast';
   else
      Result := 'UNKNOWN';
  end;
end;

function TForm1.KnobPositionTypeToStr(kpt : TBGRAKnobPositionType) : string;
begin
   // kptLineSquareCap, kptLineRoundCap, kptFilledCircle, kptHollowCircle, kptNone

   case kpt of
      kptLineSquareCap : Result := 'kptLineSquareCap';
      kptLineRoundCap : Result := 'kptLineRoundCap';
      kptFilledCircle : Result :='kptFilledCircle';
      kptHollowCircle : Result :='kptHollowCircle';
      kptNone : Result :='kptNone';
   else
      Result := 'UNKNOWN';
  end;
end;


procedure TForm1.BitBtn1Click(Sender: TObject);
begin
    ValueLbl.Caption:=FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.CurveExponentSpeChange(Sender: TObject);
begin
    BGRAKnob1.CurveExponent := CurveExponentSpe.Value;
end;

procedure TForm1.KnobColorCbChange(Sender: TObject);
begin
    BGRAKnob1.KnobColor := KnobColorCb.Selected;
end;

procedure TForm1.BGRAKnob1ValueChanged(Sender: TObject; Value: single);
begin
  ValueLbl.Caption:=FloatToStr(Value);
  MouseWheelSpeedLbl.Caption := IntToStr(BGRAKnob1.WheelSpeed);
end;

procedure TForm1.CDVDBtnClick(Sender: TObject);
begin
  // just clear some of the data values

  ValueLbl.Caption := '';
  label2.Caption := '';
  label3.Caption := '';
  label4.Caption := '';
  label5.Caption := '';
  label6.Caption := '';
  label8.Caption := '';
end;

procedure TForm1.MouseWheelWrapCbChange(Sender: TObject);
begin
  BGRAKnob1.WheelWrap := MouseWheelWrapCb.Checked;
end;

procedure TForm1.PositionColorCbChange(Sender: TObject);
begin
  BGRAKnob1.PositionColor := PositionColorCb.Selected;
end;

procedure TForm1.PositionMarginTBChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser then
  begin
    BGRAKnob1.PositionMargin := PositionMarginTB.Value;
    PositionMarginLbl.Caption := FloatToStr(BGRAKnob1.PositionMargin);
  end;
end;

procedure TForm1.PositionTypeCbChange(Sender: TObject);
begin
  if Sender is TComboBox then
    with Sender as TComboBox do
    begin
         // kptLineSquareCap, kptLineRoundCap, kptFilledCircle, kptHollowCircle, kptNone

         case ItemIndex of
            0 : begin
                  // kptLineSquareCap
                  BGRAKnob1.PositionType := kptLineSquareCap;
                end;
            1 : begin
                  // kptLineRoundCap
                  BGRAKnob1.PositionType := kptLineRoundCap;
                end;
            2 : begin
                  // kptFilledCircle
                  BGRAKnob1.PositionType := kptFilledCircle;
                end;
            3 : begin
                  // kptHollowCircle
                  BGRAKnob1.PositionType := kptHollowCircle;
                end;
            4 : begin
                  // kptNone
                  BGRAKnob1.PositionType := kptNone;
                end;
         end;
    end;
    PositionTypeLbl.Caption := KnobPositionTypeToStr(BGRAKnob1.PositionType);
end;

procedure TForm1.PositionWidthTBChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser then // not sure why this is needed but segfault if not
  begin
    BGRAKnob1.PositionWidth := PositionWidthTB.Value;
    PositionWidthLbl.Caption := IntToStr(Round(BGRAKnob1.PositionWidth));
  end;
end;

procedure TForm1.StartFromBottomCbChange(Sender: TObject);
begin
  BGRAKnob1.StartFromBottom := StartFromBottomCb.Checked;
end;

procedure TForm1.ReverseScaleCbChange(Sender: TObject);
begin
  BGRAKnob1.ReverseScale := ReverseScaleCb.Checked;
  ValueLbl.Caption := FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.ResetRangesBtn1Click(Sender: TObject);
begin
  SetMinValueBtnClick(Nil);
  SetMaxValueBtnClick(Nil);
  SetStartAngleBtnClick(Nil);
  SetEndAngleBtnClick(Nil);
end;

procedure TForm1.ResetGeneralBtnClick(Sender: TObject);
begin
  BGRAKnob1.SlowSnap := True;
  SlowSnapCb.Checked := True;
  ReverseScaleCb.Checked := False;
  MouseWheelWrapCb.Checked := False;
  BGRAKnob1.StartFromBottom := True;          // Normal Orientation
  StartFromBottomCb.Checked := True;
  MouseWheelSpeedTB.Value := 100;
  BGRAKnob1.PositionWidth := 4;
  PositionWidthTB.Value := Round(BGRAKnob1.PositionWidth);
  BGRAKnob1.PositionMargin := 4;
  PositionMarginTB.Value := Round(BGRAKnob1.PositionMargin);
  BGRAKnob1.KnobType := ktRange;
  KnobTypeCb.ItemIndex := ord(BGRAKnob1.KnobType);
  BGRAKnob1.TaperType := kttLinear;
  TaperTypeCb.ItemIndex := ord(BGRAKnob1.TaperType);
  BGRAKnob1.PositionType := kptLineRoundCap;
  PositionTypeCb.ItemIndex := ord(BGRAKnob1.PositionType);
  MinValueLbl.Caption := FloatToStr(BGRAKnob1.MinValue);
  MaxValueLbl.Caption := FloatToStr(BGRAKnob1.MaxValue);
  StartAngleLbl.Caption := FloatToStr(BGRAKnob1.StartAngle);
  EndAngleLbl.Caption := FloatToStr(BGRAKnob1.EndAngle);
  KnobTypeLbl.Caption := KnobTypeToStr(BGRAKnob1.KnobType);
  PositionWidthLbl.Caption := FloatToStr(BGRAKnob1.PositionWidth);
  LightIntensitySpe.Value := 300;
  CurveExponentSpe.Value := 0.2;
  KnobColorCb.Selected := clSilver;
  PositionColorCb.Selected := clBlack;
end;

procedure TForm1.StartAngleEdtKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if (Key = VK_RETURN) then
      SetStartAngleBtnClick(nil);
end;

procedure TForm1.SetNeg5BtnClick(Sender: TObject);
begin
  BGRAKnob1.Value:= -5.0;
  ValueLbl.Caption := FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  BGRAKnob1.Value := BGRAKnob1.Value + 1.0;
  ValueLbl.Caption := FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  BGRAKnob1.Value := BGRAKnob1.Value - 1.0;
  ValueLbl.Caption := FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.SetNeg10BtnClick(Sender: TObject);
begin
  BGRAKnob1.Value:= -10.0;
  ValueLbl.Caption := FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.Set0BtnClick(Sender: TObject);
begin
  BGRAKnob1.Value:=0.0;
  ValueLbl.Caption := FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.Set25BtnClick(Sender: TObject);
begin
    BGRAKnob1.Value:=25.0;
    ValueLbl.Caption := FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.Set50BtnClick(Sender: TObject);
begin
  BGRAKnob1.Value:=50.0;
  ValueLbl.Caption := FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.Set100BtnClick(Sender: TObject);
begin
  BGRAKnob1.Value:=100.0;
  ValueLbl.Caption := FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.Set180BtnClick(Sender: TObject);
begin
  BGRAKnob1.Value:=180.0;
  ValueLbl.Caption := FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.Set360BtnClick(Sender: TObject);
begin
  BGRAKnob1.Value:=360.0;
  ValueLbl.Caption := FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.Set1000BtnClick(Sender: TObject);
begin
  BGRAKnob1.Value:=1000.0;
  ValueLbl.Caption := FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.SlowSnapCbChange(Sender: TObject);
begin
  // Toggle the slow snap feature, makes wrap around more deliberate

  BGRAKnob1.SlowSnap:=SlowSnapCb.Checked;
end;

procedure TForm1.KnobTypeCbChange(Sender: TObject);
begin
    if Sender is TComboBox then
    with Sender as TComboBox do
    begin
         case ItemIndex of
            0 : begin
                  // ktRange
                  BGRAKnob1.KnobType:= ktRange;
                end;
            1 : begin
                  // ktSector
                  BGRAKnob1.KnobType:= ktSector;
                end;
         end;
    end;

  KnobTypeLbl.Caption := KnobTypeToStr(BGRAKnob1.KnobType);
  MinValueLbl.Caption := FloatToStr(BGRAKnob1.MinValue);
  MinValueEdt.Text := MinValueLbl.Caption;
  MaxValueLbl.Caption := FloatToStr(BGRAKnob1.MaxValue);
  MaxValueEdt.Text := MaxValueLbl.Caption;
  StartAngleLbl.Caption := FloatToStr(BGRAKnob1.StartAngle);
  StartAngleEdt.Text := StartAngleLbl.Caption;
  EndAngleLbl.Caption := FloatToStr(BGRAKnob1.EndAngle);
  EndAngleEdt.Text := EndAngleLbl.Caption;
  ValueLbl.Caption := FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.SetStartAngleBtnClick(Sender: TObject);
begin
  BGRAKnob1.StartAngle := StrToFloat(StartAngleEdt.Text);
  StartAngleLbl.Caption := FloatToStr(BGRAKnob1.StartAngle);
  ValueLbl.Caption:=FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.SetEndAngleBtnClick(Sender: TObject);
begin
  BGRAKnob1.EndAngle := StrToFloat(EndAngleEdt.Text);
  EndAngleLbl.Caption := FloatToStr(BGRAKnob1.EndAngle);
  ValueLbl.Caption:=FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.SetMinValueBtnClick(Sender: TObject);
begin
  BGRAKnob1.MinValue := StrToFloat(MinValueEdt.Text);
  MinValueLbl.Caption := FloatToStr(BGRAKnob1.MinValue);
  ValueLbl.Caption:=FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.SetMaxValueBtnClick(Sender: TObject);
begin
  BGRAKnob1.MaxValue := StrToFloat(MaxValueEdt.Text);
  MaxValueLbl.Caption := FloatToStr(BGRAKnob1.MaxValue);
  ValueLbl.Caption:=FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.SetValueBtnClick(Sender: TObject);
begin
  BGRAKnob1.Value:= StrToFloat(ValueEdt.Text);
  ValueLbl.Caption := FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.TaperTypeCbChange(Sender: TObject);
begin
    if Sender is TComboBox then
    with Sender as TComboBox do
    begin
         // kttLinear, kttAudioSlow, kttAudioFast;
         case ItemIndex of
            0 : begin
                  // kttLinear
                  BGRAKnob1.TaperType := kttLinear;
                end;
            1 : begin
                  // kttAudioSlow
                  BGRAKnob1.TaperType := kttAudioSlow;
                end;
            2 : begin
                  // kttAudioFast
                  BGRAKnob1.TaperType:= kttAudioFast;
                end;
         end;
    end;
    TaperTypeLbl.Caption := KnobTaperTypeToStr(BGRAKnob1.TaperType);
    ValueLbl.Caption:=FloatToStr(BGRAKnob1.Value); // update just in case!
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // When the timer fires (after leaving knob area) clear stuff out

  label3.Caption := '';
  label4.Caption := '';
  label5.Caption := '';
  label6.Caption := '';
  label8.Caption := '';
end;

procedure TForm1.ValueEdtKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
    SetValueBtnClick(nil);
end;

procedure TForm1.BGRAKnob1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
     label2.Caption:= IntToStr(WheelDelta);
end;

procedure TForm1.BGRAKnob1Click(Sender: TObject);
begin
  label3.Caption := 'OnClick';
end;

procedure TForm1.MouseWheelSpeedTBChange(Sender: TObject; AByUser: boolean);
begin
  BGRAKnob1.WheelSpeed := MouseWheelSpeedTB.Value;
  MouseWheelSpeedLbl.Caption := IntToStr(BGRAKnob1.WheelSpeed);
end;

procedure TForm1.MinValueEdtKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
    SetMinValueBtnClick(nil);
end;

procedure TForm1.MaxValueEdtKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
    SetMaxValueBtnClick(nil);
end;

procedure TForm1.EndAngleEdtKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
    SetEndAngleBtnClick(nil);
end;

procedure TForm1.LightIntensitySpeChange(Sender: TObject);
begin
  BGRAKnob1.LightIntensity := LightIntensitySpe.Value;
end;

procedure TForm1.BGRAKnob1DblClick(Sender: TObject);
begin
  label4.Caption := 'DoubleClick';
end;

procedure TForm1.BGRAKnob1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  label5.Caption := 'MouseDown';
end;

procedure TForm1.BGRAKnob1MouseEnter(Sender: TObject);
begin
  label6.Caption := 'MouseEnter';
  Timer1.Enabled := False; // While in the knob, turn off the reset timer
end;

procedure TForm1.BGRAKnob1MouseLeave(Sender: TObject);
begin
  label6.Caption := 'MouseLeave';
  Timer1.Enabled := True;  // mouse leaves the knob space, turn on reset timer
end;

procedure TForm1.BGRAKnob1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  label8.Caption := 'X: ' + IntToStr(x) + ' Y: ' + IntToStr(Y);
end;

procedure TForm1.BGRAKnob1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  label5.Caption := 'MouseUp';
end;

end.

