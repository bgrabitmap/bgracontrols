unit Unit1;

{$mode objfpc}{$H+}

{
  v1.0 05-21-2024 Sandy Ganz - Begat. sganz@pacbell.net
  v2.0 05-26-2024 Sandy Ganz - Removed SectorDivision stuff, Knob now computes it.

  Hacked up test progam to test the enhanced BGRAKnob.
}

interface

uses
  Classes, LCLType, SysUtils, Forms, Controls, Graphics, StdCtrls, Buttons, MaskEdit,
  ExtCtrls, BGRAKnob, BCTrackbarUpdown;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Label22: TLabel;
    Label23: TLabel;
    Label26: TLabel;
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
    function KnobTypeToStr(kt : TKnobType) : string;
    procedure EndAngleEdtKeyDown(Sender: TObject; var Key: Word;
      {%H-}Shift: TShiftState);
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
    procedure Timer1Timer(Sender: TObject);
    procedure ValueEdtKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  ValueLbl.Caption := FloatToStr(BGRAKnob1.Value);
  MinValueLbl.Caption := FloatToStr(BGRAKnob1.MinValue);
  MaxValueLbl.Caption := FloatToStr(BGRAKnob1.MaxValue);
  StartAngleLbl.Caption := FloatToStr(BGRAKnob1.StartAngle);
  EndAngleLbl.Caption := FloatToStr(BGRAKnob1.EndAngle);
  SlowSnapCb.Checked := BGRAKnob1.SlowSnap;
  StartFromBottomCb.Checked := BGRAKnob1.StartFromBottom;
  ReverseScaleCb.Checked := BGRAKnob1.ReverseScale;

  MouseWheelSpeedTB.Value := BGRAKnob1.WheelSpeed;
  MouseWheelSpeedLbl.Caption := IntToStr(MouseWheelSpeedTB.Value);

  KnobTypeLbl.Caption := KnobTypeToStr(BGRAKnob1.KnobType);

end;

function TForm1.KnobTypeToStr(kt : TKnobType) : string;
begin
  case kt of
     ktRange   : Result := 'ktRange';
     ktSector  : Result := 'ktRange';
  else
     Result := 'UNKNOWN';
  end;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
    ValueLbl.Caption:=FloatToStr(BGRAKnob1.Value);
end;

procedure TForm1.BGRAKnob1ValueChanged(Sender: TObject; Value: single);
begin
  ValueLbl.Caption:=FloatToStr(Value);
  MouseWheelSpeedLbl.Caption := IntToStr(BGRAKnob1.WheelSpeed);
end;

procedure TForm1.CDVDBtnClick(Sender: TObject);
begin
  ValueLbl.Caption := '';
  label2.Caption := '';
  label3.Caption := '';
  label4.Caption := '';
  label5.Caption := '';
  label6.Caption := '';
  label8.Caption := '';
  MouseWheelSpeedLbl.Caption := '';
  MinValueLbl.Caption := '';
  MaxValueLbl.Caption := '';
  StartAngleLbl.Caption := '';
  EndAngleLbl.Caption := '';
end;

procedure TForm1.MouseWheelWrapCbChange(Sender: TObject);
begin
  BGRAKnob1.WheelWrap := MouseWheelWrapCb.Checked;
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
  KnobTypeCb.ItemIndex := 0; // ktRange type of knob
  BGRAKnob1.KnobType:= ktRange;
  MinValueLbl.Caption := FloatToStr(BGRAKnob1.MinValue);
  MaxValueLbl.Caption := FloatToStr(BGRAKnob1.MaxValue);
  StartAngleLbl.Caption := FloatToStr(BGRAKnob1.StartAngle);
  EndAngleLbl.Caption := FloatToStr(BGRAKnob1.EndAngle);
  KnobTypeLbl.Caption := KnobTypeToStr(BGRAKnob1.KnobType);
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
    BGRAKnob1.MinValue := 0;               // Must Set values AFTER setting ktRange
    BGRAKnob1.MaxValue := 15;

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
  BGRAKnob1.Value := 0;
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
  BGRAKnob1.KnobColor:=clSilver;
  Timer1.Enabled := False; // While in the knob, turn off the reset timer
end;

procedure TForm1.BGRAKnob1MouseLeave(Sender: TObject);
begin
  label6.Caption := 'MouseLeave';
  BGRAKnob1.KnobColor:=clMedGray;
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

