unit sstest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Menus, Buttons, SuperSpinner, ColorBox,
  SpinEx, ueLED, BGRAImageList, BCTrackbarUpdown, SuperSpinnerCommon, about, Types;

const
  VERSIONSTR = '1.00';            // SG TEST version, Should ALWAYS show as a delta when merging!

type
  { TSSTestFrm }

  TSSTestFrm = class(TForm)
    AboutSubMenu: TMenuItem;
    AngleTb: TBCTrackbarUpdown;
    AutoScaleCb: TCheckBox;
    DirectionLbl: TLabel;
    GroupBox3: TGroupBox;
    MouseWheelDisabledLbl: TLabel;
    Memo3: TMemo;
    AutoScaleEnabledLbl: TLabel;
    PresetsCb: TComboBox;
    Label2: TLabel;
    OnPosChangeValueLbl: TLabel;
    PositonFillOpacityLbl: TLabel;
    PositionFillOpacitySpe: TSpinEditEx;
    PositionLineCountLbl: TLabel;
    PositonLineWidthLbl: TLabel;
    PositionLineCountSpe: TSpinEditEx;
    PositionLineWidthSpe: TSpinEditEx;
    PositionEdgeThicknessLbl: TLabel;
    PositionEdgeThicknessSpe: TSpinEditEx;
    PositionEdgeColorCb: TColorBox;
    KnobEdgeColorLbl1: TLabel;
    KnobEdgeThicknessLbl: TLabel;
    KnobEdgeThicknessSpe: TSpinEditEx;
    PositionCenterMarginLbl: TLabel;
    PositionCenterMarginSpe: TSpinEditEx;
    PositionStyleCb: TComboBox;
    PositionStyleLbl: TLabel;
    MouseWheelLbl: TLabel;
    MouseBtnLbl: TLabel;
    ResetSpinnerLAFBtn: TBitBtn;
    ShiftLbl: TLabel;
    ShiftValueLbl: TLabel;
    OnMouseCapEnterValueLbl: TLabel;
    OnDblClickValueLbl: TLabel;
    OnKnobClickValueLbl: TLabel;
    OnClickValueLbl: TLabel;
    OnCapClickValueLbl: TLabel;
    MouseBtnValueLbl: TLabel;
    OnMouseUpValueLbl: TLabel;
    OnMouseEnterValueLbl: TLabel;
    OnMouseDownValueLbl: TLabel;
    OnMouseLeaveValueLbl: TLabel;
    OnMouseKnobEnterValueLbl: TLabel;
    OnMouseKnobLeaveValueLbl: TLabel;
    OnMouseCapLeaveValueLbl: TLabel;
    OnMouseMoveValueLbl: TLabel;
    MouseWheelValueLbl: TLabel;
    PositionSnapCb: TCheckBox;
    TimerLED: TuELED;
    WrappedValueLbl: TLabel;
    DirectionValueLbl: TLabel;
    AngleValueLbl: TLabel;
    BackgroundColorCb: TColorBox;
    BackgroundColorLbl: TLabel;
    BasicTab: TTabSheet;
    CapCurveExponentLbl: TLabel;
    CapCurveExponentSpe: TFloatSpinEditEx;
    CapEdgeColorCb: TColorBox;
    CapEdgeColorLbl: TLabel;
    CapEdgeThicknessLbl: TLabel;
    CapEdgeThicknessSpe: TSpinEditEx;
    CapFillColorCb: TColorBox;
    CapFillColorLbl: TLabel;
    CapLightIntensityLbl: TLabel;
    CapLightIntensitySpe: TSpinEditEx;
    CapMemo: TMemo;
    CapRadiusLbl: TLabel;
    CapRadiusSpe: TSpinEditEx;
    CapStyleCb: TComboBox;
    CapStyleLbl: TLabel;
    CapTab: TTabSheet;
    UseProcessMsgCb: TCheckBox;
    Label1: TLabel;
    Label14: TLabel;
    AngleLbl: TLabel;
    TotalizerValueLbl: TLabel;
    MouseWheelSpeedTB: TBCTrackbarUpdown;
    SpinnerLockedCb: TCheckBox;
    ExitSubMenu: TMenuItem;
    KnobCurveExponentLbl: TLabel;
    KnobCurveExponentSpe: TFloatSpinEditEx;
    KnobStyleCb: TComboBox;
    SpinResolutionCb: TComboBox;
    KnobStyleLbl: TLabel;
    FaceFillStyleLbl1: TLabel;
    KnobFillColorCb: TColorBox;
    KnobFillColorLbl: TLabel;
    KnobLightIntensityLbl: TLabel;
    KnobLightIntensitySpe: TSpinEditEx;
    FaceMemo: TMemo;
    KnobEdgeColorCb: TColorBox;
    KnobEdgeColorLbl: TLabel;
    FaceTab: TTabSheet;
    FileMenu: TMenuItem;
    FrameBorderColorCb: TColorBox;
    FrameBorderColorLbl: TLabel;
    FrameBorderWidthLbl: TLabel;
    FrameRadiusSpe: TSpinEditEx;
    FrameTab: TTabSheet;
    GaugeTs: TPageControl;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    SpinnerMovementGb: TGroupBox;
    GroupBox5: TGroupBox;
    HeightAddBtn: TBitBtn;
    WidthHeightAddBtn: TBitBtn;
    HeightLbl: TLabel;
    HeightSubBtn: TBitBtn;
    WidthHeightSubBtn: TBitBtn;
    HeightValLbl: TLabel;
    ImageList1: TBGRAImageList;
    LeftAddBtn: TBitBtn;
    LeftLbl: TLabel;
    LeftSubBtn: TBitBtn;
    LeftValLbl: TLabel;
    MainMenu1: TMainMenu;
    Memo2: TMemo;
    PerfTestBtn: TBitBtn;
    PositionFillColorCb: TColorBox;
    PointerFillColorLbl: TLabel;
    PositionMarginLbl: TLabel;
    PositionMarginSpe: TSpinEditEx;
    PositionRadiusLbl: TLabel;
    PositionRadiusSpe: TSpinEditEx;
    PositonMemo: TMemo;
    PointerTab: TTabSheet;
    ResetPositionBtn: TBitBtn;
    ResetSizeBtn: TBitBtn;
    Separator1: TMenuItem;
    SuperSpinner: TSuperSpinner;
    Timer1: TTimer;
    TopAddBtn: TBitBtn;
    TopLbl: TLabel;
    TopSubBtn: TBitBtn;
    TopValLbl: TLabel;
    ValueMinus10Btn: TBitBtn;
    ValueMinus50Btn: TBitBtn;
    ValueMinus1Btn: TBitBtn;
    ValuePlus10Btn: TBitBtn;
    ValuePlus50Btn: TBitBtn;
    ValuePlus1Btn: TBitBtn;
    ValueZeroBtn: TBitBtn;
    WidthAddBtn: TBitBtn;
    WidthLbl: TLabel;
    WidthSubBtn: TBitBtn;
    WidthValLbl: TLabel;
    procedure AboutSubMenuClick(Sender: TObject);
    procedure AutoScaleCbChange(Sender: TObject);
    procedure BackgroundColorCbChange(Sender: TObject);
    procedure AngleTbChange(Sender: TObject; {%H-}AByUser: boolean);
    procedure CapCurveExponentSpeChange(Sender: TObject);
    procedure CapEdgeColorCbChange(Sender: TObject);
    procedure CapEdgeThicknessSpeChange(Sender: TObject);
    procedure CapFillColorCbChange(Sender: TObject);
    procedure CapLightIntensitySpeChange(Sender: TObject);
    procedure CapRadiusSpeChange(Sender: TObject);
    procedure CapStyleCbChange(Sender: TObject);
    procedure ExitSubMenuClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FrameBorderColorCbChange(Sender: TObject);
    procedure FrameRadiusSpeChange(Sender: TObject);
    procedure KnobCurveExponentSpeChange(Sender: TObject);
    procedure KnobEdgeColorCbChange(Sender: TObject);
    procedure KnobEdgeThicknessSpeChange(Sender: TObject);
    procedure KnobFillColorCbChange(Sender: TObject);
    procedure KnobLightIntensitySpeChange(Sender: TObject);
    procedure KnobStyleCbChange(Sender: TObject);
    procedure MouseWheelSpeedTBChange(Sender: TObject; {%H-}AByUser: boolean);
    procedure PerfTestBtnClick(Sender: TObject);
    procedure PositionCenterMarginSpeChange(Sender: TObject);
    procedure PositionRadiusSpeChange(Sender: TObject);
    procedure PositionEdgeColorCbChange(Sender: TObject);
    procedure PositionEdgeThicknessSpeChange(Sender: TObject);
    procedure PositionFillColorCbChange(Sender: TObject);
    procedure PositionLineCountSpeChange(Sender: TObject);
    procedure PositionLineWidthSpeChange(Sender: TObject);
    procedure PositionMarginSpeChange(Sender: TObject);
    procedure PositionStyleCbChange(Sender: TObject);
    procedure PositionFillOpacitySpeChange(Sender: TObject);
    procedure PositionSnapCbChange(Sender: TObject);
    procedure PresetsCbChange(Sender: TObject);
    procedure ResetSpinnerLAFBtnClick(Sender: TObject);
    procedure SpinnerLockedCbChange(Sender: TObject);
    procedure SpinResolutionCbChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SuperSpinnerCapClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState);
    procedure SuperSpinnerClick(Sender: TObject);
    procedure SuperSpinnerDblClick(Sender: TObject);
    procedure SuperSpinnerKnobClick(Sender: TObject; {%H-}Button: TMouseButton;
      Shift: TShiftState);
    procedure SuperSpinnerMouseCapEnter(Sender: TObject; Shift: TShiftState; {%H-}X,
      {%H-}Y: Integer);
    procedure SuperSpinnerMouseCapLeave(Sender: TObject; Shift: TShiftState; {%H-}X,
      {%H-}Y: Integer);
    procedure SuperSpinnerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure SuperSpinnerMouseEnter(Sender: TObject);
    procedure SuperSpinnerMouseKnobEnter(Sender: TObject; Shift: TShiftState;
      {%H-}X, {%H-}Y: Integer);
    procedure SuperSpinnerMouseKnobLeave(Sender: TObject; Shift: TShiftState;
      {%H-}X, {%H-}Y: Integer);
    procedure SuperSpinnerMouseLeave(Sender: TObject);
    procedure SuperSpinnerMouseMove(Sender: TObject; Shift: TShiftState; {%H-}X,
      {%H-}Y: Integer);
    procedure SuperSpinnerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure SuperSpinnerMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var {%H-}Handled: Boolean);
    procedure SuperSpinnerPosChanged(Sender: TObject; Shift: TShiftState;
      Value: single; MoveDir: TSSDirection);
    procedure SuperSpinnerWrapped(Sender: TObject; {%H-}Shift: TShiftState;
      OldAngle, NewAngle: single; {%H-}MoveDir: TSSDirection);
    procedure Timer1Timer(Sender: TObject);
    procedure TimerLEDClick(Sender: TObject);
    procedure ValueMinus50BtnClick(Sender: TObject);
    procedure ValueMinus10BtnClick(Sender: TObject);
    procedure ValueMinus1BtnClick(Sender: TObject);
    procedure ValuePlus50BtnClick(Sender: TObject);
    procedure ValuePlus10BtnClick(Sender: TObject);
    procedure ValuePlus1BtnClick(Sender: TObject);
    procedure ValueZeroBtnClick(Sender: TObject);
    procedure WidthHeightAddBtnClick(Sender: TObject);
    procedure HeightAddBtnClick(Sender: TObject);
    procedure HeightSubBtnClick(Sender: TObject);
    procedure LeftAddBtnClick(Sender: TObject);
    procedure LeftSubBtnClick(Sender: TObject);
    procedure ResetPositionBtnClick(Sender: TObject);
    procedure ResetSizeBtnClick(Sender: TObject);
    procedure TopAddBtnClick(Sender: TObject);
    procedure TopSubBtnClick(Sender: TObject);
    procedure WidthAddBtnClick(Sender: TObject);
    procedure WidthHeightSubBtnClick(Sender: TObject);
    procedure WidthSubBtnClick(Sender: TObject);
  private
    FTotalizer: integer;
    FSavedSpinner: TSuperSpinner;

  public
    procedure PresetDefault;
    procedure PresetLines1;
    procedure PresetLines2;
    procedure PresetLines3;
    procedure PresetFinger1;
    procedure PresetFinger2;

    procedure UpdateBasicStats;
    procedure UpdateWHStats;
    procedure UpdateLTStats;
    procedure UpdateFrameStats;
    procedure UpdateKnobStats;
    procedure UpdatePositionStats;
    procedure UpdateCapStats;
    procedure UpdateSpinnerMovementStats; // only some

    function MouseButtonToStr(Button: TMouseButton): string;
    function MouseShiftToStr(Shift: TShiftState): string;
    procedure SetShiftState(Highlight: boolean; Shift: TShiftState);
    procedure SetMouseButtonState(Highlight: boolean; Button: TMouseButton);
  end;

var
  SSTestFrm: TSSTestFrm;

implementation

{$R *.lfm}

{ SSTestFrm }

procedure TSSTestFrm.FormCreate(Sender: TObject);
begin
  Caption := 'Super Spinner Test Application ' + VERSIONSTR;

  UpdateBasicStats;
  UpdateWHStats;
  UpdateLTStats;
  UpdateFrameStats;
  UpdateKnobStats;
  UpdatePositionStats;
  UpdateCapStats;
  UpdateSpinnerMovementStats;

  // Create a Spinner to have defaults, clean up in form destroy

  FSavedSpinner := TSuperSpinner.Create(nil);

  FTotalizer := 0;
end;

procedure TSSTestFrm.FormDestroy(Sender: TObject);
begin
  // Free anything created like the temp Spinner

  FSavedSpinner.Free;
end;

function TSSTestFrm.MouseButtonToStr(Button: TMouseButton): string;
begin
  // Decode any mouse buttons that might be pushed. Only one at a time
  // can be in the Button, not like Shift

  case Button of
    mbLeft:
      Exit('mbLeft');
    mbRight:
      Exit('mbRight');
    mbMiddle:
      Exit('mbMiddle');
    mbExtra1:
      Exit('mbExtra1');
    mbExtra2:
      Exit('mbExtra2');
  end;
  Result := 'None';
end;

function TSSTestFrm.MouseShiftToStr(Shift: TShiftState): string;
var
  shiftStr: string;

begin
  // Decode the keyboard shift states, multiple can exist
  // Also interesting is that Mouse Buttons will show up as
  // ssLeft/ssRight/etc. Not sure why but seems like it's
  // supposed to

  shiftStr := '';

  if ssShift in Shift then
    shiftStr += 'ssShift ';

  if ssAlt in Shift then
    shiftStr += 'ssAlt ';

  if ssCtrl in Shift then
    shiftStr += 'ssCtrl ';

  if ssLeft in Shift then
    shiftStr += 'ssLeft ';

  if ssRight in Shift then
    shiftStr += 'ssRight ';

  if ssMiddle in Shift then
    shiftStr += 'ssMiddle ';

  if ssDouble in Shift then
    shiftStr += 'ssDouble ';

  if ssMeta in Shift then
    shiftStr += 'ssMeta ';

  if ssSuper in Shift then
    shiftStr += 'ssSuper ';

  if ssHyper in Shift then
    shiftStr += 'ssHyper ';

  if ssAltGr in Shift then
    ShiftStr += 'ssAltGr ';

  if ssCaps in Shift then
    ShiftStr += 'ssCaps ';

  if ssNum in Shift then
    ShiftStr += 'ssNum ';

  if ssScroll in Shift then
    ShiftStr += 'ssScroll ';

  if ssTriple in Shift then
    ShiftStr += 'ssTriple ';

  if ssQuad in Shift then
    ShiftStr += 'ssQuad ';

  if ssExtra1 in Shift then
    ShiftStr += 'ssExtra1 ';

  if ssExtra2 in Shift then
    ShiftStr += 'ssExtra2 ';

  if Length(ShiftStr) = 0 then
    ShiftStr := 'None';

  Result := trim(ShiftStr);
end;

procedure TSSTestFrm.SetShiftState(Highlight: boolean; Shift: TShiftState);
begin
  // Helper to Set and Highlight Shift State Display

  ShiftValueLbl.Caption := MouseShiftToStr(Shift);

  if Highlight then
  begin
    ShiftValueLbl.Font.Color := clGreen;
    ShiftValueLbl.Font.Style := [fsBold];
  end
    else
      begin
        ShiftValueLbl.Font.Color := clDefault;
        ShiftValueLbl.Font.Style := [];
      end;
end;

procedure TSSTestFrm.SetMouseButtonState(Highlight: boolean; Button: TMouseButton);
begin
  // Helper to Set and Hightlight Mouse Button State Display

  MouseBtnValueLbl.Caption := MouseButtonToStr(Button);

  if Highlight then
  begin
    MouseBtnValueLbl.Caption := MouseButtonToStr(Button);
    MouseBtnValueLbl.Font.Color := clGreen;
    MouseBtnValueLbl.Font.Style := [fsBold];
  end
    else
      begin
        MouseBtnValueLbl.Caption := 'None';
        MouseBtnValueLbl.Font.Color := clDefault;
        MouseBtnValueLbl.Font.Style := [];
      end;
end;

procedure TSSTestFrm.SuperSpinnerCapClick(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState);
begin
  // If the Spinner Cap is visable it can have it's own Click Handler
  // This is disabled if the Spinner is LOCKED

  OnCapClickValueLbl.Font.Color := clGreen;
  OnCapClickValueLbl.Font.Style := [fsBold];
  SetMouseButtonState(True, Button);
  SetShiftState(True, Shift);
end;

procedure TSSTestFrm.SuperSpinnerClick(Sender: TObject);
begin
  // Normal TControl type of click, it's NOT subjected to being controled
  // by the spinners lock state since its a generic click

  OnClickValueLbl.Font.Color := clGreen;
  OnClickValueLbl.Font.Style := [fsBold];
end;

procedure TSSTestFrm.SuperSpinnerDblClick(Sender: TObject);
begin
  OnDblClickValueLbl.Font.Color := clGreen;
  OnDblClickValueLbl.Font.Style := [fsBold];
end;

procedure TSSTestFrm.SuperSpinnerKnobClick(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState);
begin
  // This is another Spinner specific event, and only triggered
  // when the KNOB portion is clicked. It will NOT trigger on the
  // Cap if that is enabled, and will not trigger on the Frame of the
  // Spinner or other parts of the client area

  OnKnobClickValueLbl.Font.Color := clGreen;
  OnKnobClickValueLbl.Font.Style := [fsBold];
  SetShiftState(True, Shift);
  UpdateSpinnerMovementStats;
end;

procedure TSSTestFrm.SuperSpinnerMouseCapEnter(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  // Will track Mouse Entering the Cap if the Cap is enabled

  OnMouseCapEnterValueLbl.Font.Color := clGreen;
  OnMouseCapEnterValueLbl.Font.Style := [fsBold];
  SetShiftState(True, Shift);
end;

procedure TSSTestFrm.SuperSpinnerMouseCapLeave(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  // Will track Mouse Leaving the Cap if the Cap is Enabled

  OnMouseCapLeaveValueLbl.Font.Color := clGreen;
  OnMouseCapLeaveValueLbl.Font.Style := [fsBold];
  SetShiftState(True, Shift);
end;

procedure TSSTestFrm.SuperSpinnerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // General Mouse Down event, will trigger anywhere in the control

  OnMouseDownValueLbl.Font.Color := clGreen;
  OnMouseDownValueLbl.Font.Style := [fsBold];
  SetMouseButtonState(True, Button);
  SetShiftState(True, Shift);
end;

procedure TSSTestFrm.SuperSpinnerMouseEnter(Sender: TObject);
begin
  // General Mouse Enter, will trigger anywhere in the control

  OnMouseEnterValueLbl.Font.Color := clGreen;
  OnMouseEnterValueLbl.Font.Style := [fsBold];
end;

procedure TSSTestFrm.SuperSpinnerMouseKnobEnter(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  // Will trigger when the mouse enters the Knob, Spinner specific
  // If the cap is enabled, and the mouse is in the Cap, and moves
  // to the Knob, it will also trigger.

  OnMouseKnobEnterValueLbl.Font.Color := clGreen;
  OnMouseKnobEnterValueLbl.Font.Style := [fsBold];
  SetShiftState(True, Shift);
end;

procedure TSSTestFrm.SuperSpinnerMouseKnobLeave(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  // Triggers anytime the mouse leaves the knob portion of the spinner. This
  // includes leaving the knob area into the Cap if that is enabled.

  OnMouseKnobLeaveValueLbl.Font.Color := clGreen;
  OnMouseKnobLeaveValueLbl.Font.Style := [fsBold];
  SetShiftState(True, Shift);
end;

procedure TSSTestFrm.SuperSpinnerMouseLeave(Sender: TObject);
begin
  OnMouseLeaveValueLbl.Font.Color := clGreen;
  OnMouseLeaveValueLbl.Font.Style := [fsBold];
end;

procedure TSSTestFrm.SuperSpinnerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  // General mouse movement capture, will capture anywhere in the component

  OnMouseMoveValueLbl.Font.Color := clGreen;
  OnMouseMoveValueLbl.Font.Style := [fsBold];
  SetShiftState(True, Shift);
end;

procedure TSSTestFrm.SuperSpinnerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // General mouse movement capture, will capture anywhere in the component

  OnMouseUpValueLbl.Font.Color := clGreen;
  OnMouseUpValueLbl.Font.Style := [fsBold];
  SetMouseButtonState(True, Button);
  SetShiftState(True, Shift);
end;

procedure TSSTestFrm.SuperSpinnerMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  // General mouse movement capture, will capture anywhere in the component

  MouseWheelValueLbl.Caption := 'Mouse: ' + MouseShiftToStr(Shift) + ', Delta:' + IntToStr(WheelDelta) + ', Mouse: '
    + IntToStr(MousePos.X) + ',' + IntToStr(MousePos.Y);
  MouseWheelValueLbl.Font.Color := clGreen;
  MouseWheelValueLbl.Font.Style := [fsBold];
end;

// Rest stuff ever timer tick, this just flashes
// stuff and not synced to anythings so flashes
// may be short or longer depending when you click

procedure TSSTestFrm.Timer1Timer(Sender: TObject);
begin
  // called every timer interval to reset the display

  OnCapClickValueLbl.Font.Color := clDefault;
  OnCapClickValueLbl.Font.Style := [];
  OnClickValueLbl.Font.Color := clDefault;
  OnClickValueLbl.Font.Style := [];
  OnDblClickValueLbl.Font.Color := clDefault;
  OnDblClickValueLbl.Font.Style := [];
  OnKnobClickValueLbl.Font.Color := clDefault;
  OnKnobClickValueLbl.Font.Style := [];
  OnPosChangeValueLbl.Font.Color := clDefault;
  OnPosChangeValueLbl.Font.Style := [];
  OnMouseEnterValueLbl.Font.Color := clDefault;
  OnMouseEnterValueLbl.Font.Style := [];
  OnMouseLeaveValueLbl.Font.Color := clDefault;
  OnMouseLeaveValueLbl.Font.Style := [];
  OnMouseCapEnterValueLbl.Font.Color := clDefault;
  OnMouseCapEnterValueLbl.Font.Style := [];
  OnMouseCapLeaveValueLbl.Font.Color := clDefault;
  OnMouseCapLeaveValueLbl.Font.Style := [];
  OnMouseKnobEnterValueLbl.Font.Color := clDefault;
  OnMouseKnobEnterValueLbl.Font.Style := [];
  OnMouseKnobLeaveValueLbl.Font.Color := clDefault;
  OnMouseKnobLeaveValueLbl.Font.Style := [];
  OnMouseUpValueLbl.Font.Color := clDefault;
  OnMouseUpValueLbl.Font.Style := [];
  OnMouseDownValueLbl.Font.Color := clDefault;
  OnMouseDownValueLbl.Font.Style := [];
  OnMouseMoveValueLbl.Font.Color := clDefault;
  OnMouseMoveValueLbl.Font.Style := [];
  SetShiftState(False, []);
  SetMouseButtonState(False, TMouseButton(0)); // fake out a mouse button, not used if False
  MouseWheelValueLbl.Caption := '';
  MouseWheelValueLbl.Font.Color := clDefault;
  MouseWheelValueLbl.Font.Style := [];

  WrappedValueLbl.Font.Style := [];

  TimerLED.Active := not TimerLED.Active;
end;

procedure TSSTestFrm.TimerLEDClick(Sender: TObject);
begin
  // Click on the LED for a forced reset of the Event display

  Timer1Timer(self);
end;

procedure TSSTestFrm.HeightAddBtnClick(Sender: TObject);
begin
  SuperSpinner.Height := SuperSpinner.Height + 10;
  UpdateWHStats;
end;

procedure TSSTestFrm.BackgroundColorCbChange(Sender: TObject);
begin
  SuperSpinner.Color := BackgroundColorCb.Selected;
end;

procedure TSSTestFrm.AboutSubMenuClick(Sender: TObject);
begin
  AboutFrm.VersionStr := VERSIONSTR;
  AboutFrm.show;
end;

procedure TSSTestFrm.AutoScaleCbChange(Sender: TObject);
begin
  SuperSpinner.AutoScale := AutoScaleCb.Checked;
  AutoScaleEnabledLbl.Visible := SuperSpinner.AutoScale;
end;

procedure TSSTestFrm.AngleTbChange(Sender: TObject; AByUser: boolean);
begin
  SuperSpinner.Angle := AngleTb.Value;  // This will NOT call OnPosChanged internally to the spinner
  AngleValueLbl.Caption := FloatToStrF(SuperSpinner.Angle, ffFixed, 3,3); // Update display
  // again, setting the Angle will not do anything except reposition the spinner
  // No events will be called
end;

procedure TSSTestFrm.CapCurveExponentSpeChange(Sender: TObject);
begin
  SuperSpinner.CapSettings.CurveExponent := CapCurveExponentSpe.Value;
end;

procedure TSSTestFrm.CapEdgeColorCbChange(Sender: TObject);
begin
  SuperSpinner.CapSettings.EdgeColor := CapEdgeColorCb.Selected;
end;

procedure TSSTestFrm.CapEdgeThicknessSpeChange(Sender: TObject);
begin
  SuperSpinner.CapSettings.EdgeThickness := CapEdgeThicknessSpe.Value;
end;

procedure TSSTestFrm.CapFillColorCbChange(Sender: TObject);
begin
  SuperSpinner.CapSettings.FillColor := CapFillColorCb.Selected;
end;

procedure TSSTestFrm.CapLightIntensitySpeChange(Sender: TObject);
begin
  SuperSpinner.CapSettings.LightIntensity := CapLightIntensitySpe.Value;
end;

procedure TSSTestFrm.CapRadiusSpeChange(Sender: TObject);
begin
  SuperSpinner.CapSettings.Radius := CapRadiusSpe.Value;
end;

procedure TSSTestFrm.CapStyleCbChange(Sender: TObject);
begin
  // csNone, csFlat, csShaded, csPhong, csOutline

  case CapStyleCb.ItemIndex of
    0 : {csNone}
      SuperSpinner.CapSettings.Style := csNone;
    1 : {csFlat}
      SuperSpinner.CapSettings.Style := csFlat;
    2: {csShaded}
      SuperSpinner.CapSettings.Style := csShaded;
    3: {csPhong}
      SuperSpinner.CapSettings.Style := csPhong;
    4: {csOutline}
      SuperSpinner.CapSettings.Style := csOutline;
  else
    // Unknown type, warn somewhere...
  end;

  UpdateCapStats;
end;

procedure TSSTestFrm.ExitSubMenuClick(Sender: TObject);
begin
  Close;
end;

procedure TSSTestFrm.FrameBorderColorCbChange(Sender: TObject);
begin
  SuperSpinner.FrameSettings.BorderColor := FrameBorderColorCb.Selected;
end;

procedure TSSTestFrm.FrameRadiusSpeChange(Sender: TObject);
begin
  SuperSpinner.FrameSettings.BorderWidth := FrameRadiusSpe.Value;
end;

procedure TSSTestFrm.KnobCurveExponentSpeChange(Sender: TObject);
begin
  SuperSpinner.KnobSettings.CurveExponent := KnobCurveExponentSpe.Value;
end;

procedure TSSTestFrm.KnobEdgeColorCbChange(Sender: TObject);
begin
  SuperSpinner.KnobSettings.EdgeColor := KnobEdgeColorCb.Selected;
end;

procedure TSSTestFrm.KnobEdgeThicknessSpeChange(Sender: TObject);
begin
  SuperSpinner.KnobSettings.EdgeThickness := KnobEdgeThicknessSpe.Value;
end;

procedure TSSTestFrm.KnobFillColorCbChange(Sender: TObject);
begin
  SuperSpinner.KnobSettings.FillColor := KnobFillColorCb.Selected;
end;

procedure TSSTestFrm.KnobLightIntensitySpeChange(Sender: TObject);
begin
  SuperSpinner.KnobSettings.LightIntensity := KnobLightIntensitySpe.Value;
end;

procedure TSSTestFrm.KnobStyleCbChange(Sender: TObject);
begin
  // ssFlat, ssShaded, ssPhong

  case KnobStyleCb.ItemIndex of
    0 : {ssFlat}
      SuperSpinner.KnobSettings.Style := ssFlat;
    1 : {ssShaded}
      SuperSpinner.KnobSettings.Style  := ssShaded;
    2: {ssPhong}
      SuperSpinner.KnobSettings.Style := ssPhong;
  else
    // Unknown type, warn somewhere...
  end;

  UpdateKnobStats;
end;

procedure TSSTestFrm.MouseWheelSpeedTBChange(Sender: TObject; AByUser: boolean);
begin
  SuperSpinner.WheelSpeed:= MouseWheelSpeedTB.Value;
  MouseWheelDisabledLbl.Visible := (SuperSpinner.WheelSpeed = 0);
end;

procedure TSSTestFrm.PerfTestBtnClick(Sender: TObject);
var
  i, j : integer;
begin
  // Just move the knove, no event's, mainly for paint tests

  for j := 0 to 10 do
  begin
      for i := 0 to 100 do
      begin
        SuperSpinner.Angle := i;
        Application.ProcessMessages;
      end;
  end;
  beep;
end;

procedure TSSTestFrm.PositionCenterMarginSpeChange(Sender: TObject);
begin
  SuperSpinner.PositionSettings.CenterMargin := PositionCenterMarginSpe.Value;
end;

procedure TSSTestFrm.PositionRadiusSpeChange(Sender: TObject);
begin
  SuperSpinner.PositionSettings.Radius := PositionRadiusSpe.Value;
end;

procedure TSSTestFrm.PositionEdgeColorCbChange(Sender: TObject);
begin
  SuperSpinner.PositionSettings.EdgeColor := PositionEdgeColorCb.Selected;
end;

procedure TSSTestFrm.PositionEdgeThicknessSpeChange(Sender: TObject);
begin
  SuperSpinner.PositionSettings.EdgeThickness := PositionEdgeThicknessSpe.Value;
end;

procedure TSSTestFrm.PositionFillColorCbChange(Sender: TObject);
begin
  SuperSpinner.PositionSettings.FillColor := PositionFillColorCb.Selected;
end;

procedure TSSTestFrm.PositionLineCountSpeChange(Sender: TObject);
begin
  SuperSpinner.PositionSettings.LineCount := PositionLineCountSpe.Value;
end;

procedure TSSTestFrm.PositionLineWidthSpeChange(Sender: TObject);
begin
  SuperSpinner.PositionSettings.LineWidth := PositionLineWidthSpe.Value;
end;

procedure TSSTestFrm.PositionMarginSpeChange(Sender: TObject);
begin
  SuperSpinner.PositionSettings.Margin := PositionMarginSpe.Value;
end;

procedure TSSTestFrm.PositionStyleCbChange(Sender: TObject);
begin
  // Update Position Style
  // psNone, psFilledCircle, psHollowCircle, psIndentCircle, psLines

  case PositionStyleCb.ItemIndex of
    0 : {psNone}
      SuperSpinner.PositionSettings.Style := psNone;
    1 : {psFilledCircle}
      SuperSpinner.PositionSettings.Style := psFilledCircle;
    2 : {psHollowCircle}
      SuperSpinner.PositionSettings.Style := psHollowCircle;
    3 : {psShaded}
      SuperSpinner.PositionSettings.Style := psShaded;
    4 : {psIndentCircle}
      SuperSpinner.PositionSettings.Style := psIndentCircle;
    5 : {psLines}
      SuperSpinner.PositionSettings.Style := psLines;

  else
    // Unknown type, Warn
  end;
end;

procedure TSSTestFrm.PositionFillOpacitySpeChange(Sender: TObject);
begin
  SuperSpinner.PositionSettings.Opacity := PositionFillOpacitySpe.Value;
end;

procedure TSSTestFrm.PositionSnapCbChange(Sender: TObject);
begin
  SuperSpinner.PositionSnap := PositionSnapCb.Checked;
  UpdateSpinnerMovementStats;
end;

procedure TSSTestFrm.PresetsCbChange(Sender: TObject);
begin
  case PresetsCb.ItemIndex of
    0 : {default}
      PresetDefault;
    1 : {Lines 1}
      PresetLines1;
    2 : {Lines 2}
      PresetLines2;
    3 : {Lines 3}
      PresetLines3;
    4 : {Finger 1}
      PresetFinger1;
    5 : {Finger 2}
      PresetFinger2;
  end;
end;

procedure TSSTestFrm.PresetDefault;
begin
  // Resets mostly everything back to default Look and Feel
  // If adding or removing props don't forget to fix here!

  // Basic Settings

  with FSavedSpinner do
  begin
    SuperSpinner.Angle := Angle;
    SuperSpinner.Color := Color;
    SuperSpinner.Locked := Locked;
    SuperSpinner.PositionSnap := PositionSnap;
    SuperSpinner.SpinResolution := SpinResolution;
    SuperSpinner.WheelSpeed := WheelSpeed;
  end;
  UpdateBasicStats;

  // Cap Settings

  with FSavedSpinner.CapSettings do
  begin
    SuperSpinner.CapSettings.CurveExponent := CurveExponent;
    SuperSpinner.CapSettings.EdgeColor := EdgeColor;
    SuperSpinner.CapSettings.EdgeThickness := EdgeThickness;
    SuperSpinner.CapSettings.FillColor := FillColor;
    SuperSpinner.CapSettings.LightIntensity := LightIntensity;
    SuperSpinner.CapSettings.Radius := Radius;
    SuperSpinner.CapSettings.Style := Style;
  end;
  UpdateCapStats;

  // Frame Settings

  with FSavedSpinner.FrameSettings do
  begin
    SuperSpinner.FrameSettings.BorderColor := BorderColor;
    SuperSpinner.FrameSettings.BorderWidth := BorderWidth;
  end;
  UpdateFrameStats;

  // Knob Settings

  with FSavedSpinner.KnobSettings do
  begin
    SuperSpinner.KnobSettings.CurveExponent := CurveExponent;
    SuperSpinner.KnobSettings.EdgeColor := EdgeColor;
    SuperSpinner.KnobSettings.EdgeThickness := EdgeThickness;
    SuperSpinner.KnobSettings.FillColor := FillColor;
    SuperSpinner.KnobSettings.LightIntensity := LightIntensity;
    SuperSpinner.KnobSettings.Style := Style;
  end;
  UpdateKnobStats;

  // Position Settings

  with FSavedSpinner.PositionSettings do
  begin
    SuperSpinner.PositionSettings.CenterMargin := CenterMargin;
    SuperSpinner.PositionSettings.Radius := Radius;  // This may need to be renamed to RADIUS for consistancy
    SuperSpinner.PositionSettings.EdgeColor := EdgeColor;
    SuperSpinner.PositionSettings.EdgeThickness := EdgeThickness;
    SuperSpinner.PositionSettings.FillColor := FillColor;
    SuperSpinner.PositionSettings.LineCount := LineCount;
    SuperSpinner.PositionSettings.LineWidth := LineWidth;
    SuperSpinner.PositionSettings.Margin := Margin;
    SuperSpinner.PositionSettings.Opacity := Opacity;
    SuperSpinner.PositionSettings.Style := Style;
  end;
  UpdatePositionStats;

end;
procedure TSSTestFrm.PresetLines1;
begin
  // Lines 1 type, just different in a few aspects to show settings

  with SuperSpinner.PositionSettings do
  begin
    Style := psLines;
    FillColor := clGray;
    LineCount := 6;
    LineWidth := 3;
    Margin := 15;
    CenterMargin := 40;
  end;

  with SuperSpinner.CapSettings do
  begin
    Style := csShaded;
    FillColor := clGray;
    EdgeColor := clSilver;
    EdgeThickness := 2;
  end;

  with SuperSpinner.KnobSettings do
  begin
    Style := ssFlat;
    FillColor := clSilver;
    EdgeColor := clMedGray;
    EdgeThickness := 4;
  end;

  UpdatePositionStats;
  UpdateCapStats;
  UpdateKnobStats;
end;

procedure TSSTestFrm.PresetLines2;
begin
  // Lines 2 type, just different in a few aspects to show settings

  with SuperSpinner.PositionSettings do
  begin
    Style := psLines;
    FillColor := clRed;
    LineCount := 5;
    LineWidth := 6;
    Margin := 15;
    CenterMargin := 40;
  end;

  with SuperSpinner.CapSettings do
  begin
    Style := csFlat;
    FillColor := clRed;
    EdgeColor := clBlue;
    EdgeThickness := 4;
  end;

  with SuperSpinner.KnobSettings do
  begin
    Style := ssFlat;
    FillColor := clMedGray;
    EdgeColor := clGray;
    EdgeThickness := 4;
  end;

  UpdatePositionStats;
  UpdateCapStats;
  UpdateKnobStats;
end;

procedure TSSTestFrm.PresetLines3;
begin
  // Lines 3 type, just different in a few aspects to show settings

  with SuperSpinner.PositionSettings do
  begin
    Style := psLines;
    FillColor := clRed;
    LineCount := 15;
    LineWidth := 4;
    CenterMargin := 0;
  end;

  with SuperSpinner.CapSettings do
  begin
    Style := csNone;
    FillColor := clRed;
    EdgeColor := clBlue;
    EdgeThickness := 4;
  end;

  with SuperSpinner.KnobSettings do
  begin
    Style := ssShaded;
    FillColor := clWhite;
    EdgeColor := clSilver;
    EdgeThickness := 4;
  end;

  UpdatePositionStats;
  UpdateCapStats;
  UpdateKnobStats;
end;

procedure TSSTestFrm.PresetFinger1;
begin
  // Finger Hole type of spinner

  with SuperSpinner.PositionSettings do
  begin
    Style := psIndentCircle;
    FillColor := clBlack;
    EdgeColor := clGray;
    Radius := 20;
    EdgeThickness := 2;
  end;

  with SuperSpinner.CapSettings do
  begin
    Style := csNone;
  end;

  with SuperSpinner.KnobSettings do
  begin
    Style := ssPhong;
    FillColor := clWhite;
    EdgeColor := clMedGray;
    EdgeThickness := 2;
  end;

  UpdatePositionStats;
  UpdateCapStats;
  UpdateKnobStats;
end;

procedure TSSTestFrm.PresetFinger2;
begin
  // Another Finger Hole type

  with SuperSpinner.PositionSettings do
  begin
    Style := psHollowCircle;
    FillColor := clBlack;
    EdgeColor := clGray;
    Radius := 20;
    EdgeThickness := 2;
    LineWidth := 2;
  end;

  with SuperSpinner.CapSettings do
  begin
    Style := csNone;
  end;

  with SuperSpinner.KnobSettings do
  begin
    Style := ssPhong;
    FillColor := clWhite;
    EdgeColor := clMedGray;
    EdgeThickness := 2;
  end;

  UpdatePositionStats;
  UpdateCapStats;
  UpdateKnobStats;
end;

procedure TSSTestFrm.ResetSpinnerLAFBtnClick(Sender: TObject);
begin
  PresetDefault;
  PresetsCb.ItemIndex := 0;
end;

procedure TSSTestFrm.SpinnerLockedCbChange(Sender: TObject);
begin
  // If we lock the spinner, set a few colors to Red, if unlocked
  // restore to what the form has them set to. This is dumb as it
  // just doesn't update the contols on the test page so can just be restored.
  // So DON'T UpdateStats on these or you will have to save off the state
  // and that's more work!

  SuperSpinner.Locked := SpinnerLockedCb.Checked;

  if SuperSpinner.Locked then
  begin
    SuperSpinner.KnobSettings.FillColor := clRed;
    SuperSpinner.KnobSettings.EdgeColor := clRed;
    SuperSpinner.CapSettings.FillColor := clRed;
    SuperSpinner.CapSettings.EdgeColor := clBlack;
    SuperSpinner.FrameSettings.BorderColor := clBlack;
  end
    else
      begin
        SuperSpinner.KnobSettings.FillColor := KnobFillColorCb.Selected;
        SuperSpinner.KnobSettings.EdgeColor := KnobEdgeColorCb.Selected;
        SuperSpinner.CapSettings.FillColor := CapFillColorCb.Selected;
        SuperSpinner.CapSettings.EdgeColor := CapEdgeColorCb.Selected;
        SuperSpinner.FrameSettings.BorderColor := FrameBorderColorCb.Selected;
      end;
end;

procedure TSSTestFrm.SpinResolutionCbChange(Sender: TObject);
begin
  // SpinResolution
  // TSSResolution = (srHighest = 0, srHigh = 1, srHighMedium =2, srMedium = 3,
  //                  srMediumLow = 4, srLow = 5, srLowest = 6)
  //
  // These are abstracted to not get into any trouble with actual events per
  // revolution of the spinner. srHighest is full resolution of the mouse movement
  // and it gets more of a detent feel as you go to lower settings.
  //
  // Also an interesting note if using a Position type of lines it may not look
  // like the spinner is moving, switch to a different style to see the movement.

  case SpinResolutionCb.ItemIndex of
    0 : {srHighest}
      SuperSpinner.SpinResolution := srHighest;
    1 : {srHigh}
      SuperSpinner.SpinResolution := srHigh;
    2: {srHighMedium}
      SuperSpinner.SpinResolution := srHighMedium;
    3: {srMedium}
      SuperSpinner.SpinResolution := srMedium;
    4: {srMediumLow}
      SuperSpinner.SpinResolution := srMediumLow;
    5: {srLow}
      SuperSpinner.SpinResolution := srLow;
    6: {srLowest}
      SuperSpinner.SpinResolution := srLowest;
  else
    // Unknown type, warn somewhere...
  end;

  UpdateBasicStats;
end;

procedure TSSTestFrm.SuperSpinnerPosChanged(Sender: TObject;
  Shift: TShiftState; Value: single; MoveDir: TSSDirection);
var
  Direction: integer;
  DirectionStr: string;

begin
  // This gets called anytime the Spinner is moved by the user EXCEPT if the
  // Angle() method is called as it do NOT call this event handler
  //
  // This handler is where you would, for example, increase/decrease a value
  // as the user spins. Radio Frequency, Song Selection (like iPod), etc.
  // Typically the Angle should NOT be used other than to set the Position
  // Visually.

  // See which way we are moving, get the direction string decoded as well

  if MoveDir = sdCW then
  begin
      Direction := 1;
      DirectionStr := 'sdCW';
  end
    else
      begin
        Direction := -1;
        DirectionStr := 'sdCCW';
      end;

  // If moving the Spinner and the SHIFT is held down, it will just 10x the
  // bump in the Totalizer. This is arbitrary and just to show some examples
  // of a FAST way to move the spinner if needed.

  if ssShift in Shift then
  begin
    Direction := Direction * 10; // if shifting just 10x the count
  end;

  DirectionValueLbl.Caption := DirectionStr;
  AngleValueLbl.Caption := FloatToStrF(Value, ffFixed, 3,3);
  AngleTb.Value := Round(Value);
  FTotalizer := FTotalizer + Direction;
  TotalizerValueLbl.Caption := IntToStr(FTotalizer);
  ShiftValueLbl.Caption := MouseShiftToStr(Shift);
  ShiftValueLbl.Font.Color := clGreen;
  ShiftValueLbl.Font.Style := [fsBold];
  OnPosChangeValueLbl.Font.Color := clGreen;
  OnPosChangeValueLbl.Font.Style := [fsBold];
end;

procedure TSSTestFrm.SuperSpinnerWrapped(Sender: TObject; Shift: TShiftState;
  OldAngle, NewAngle: single; MoveDir: TSSDirection);
begin
  // If the spinners angle passes over 0 degrees by any direction or step it
  // trigger this. This could come in handy for something, but not sure what...

  WrappedValueLbl.Caption := 'Last Wrap@ Old: ' + FloatToStrF(OldAngle, ffFixed, 3,3) + ' New: ' + FloatToStrF(NewAngle, ffFixed, 3,3);
  WrappedValueLbl.Font.Style := [fsBold];
end;

procedure TSSTestFrm.ValueMinus50BtnClick(Sender: TObject);
begin
  SuperSpinner.Spin(sdCCW, 1, 60, UseProcessMsgCb.Checked);
end;

procedure TSSTestFrm.ValueMinus10BtnClick(Sender: TObject);
begin
  SuperSpinner.Spin(sdCCW, 1, 10, UseProcessMsgCb.Checked);
end;

procedure TSSTestFrm.ValueMinus1BtnClick(Sender: TObject);
begin
    SuperSpinner.Bump(sdCCW, 1);
end;

procedure TSSTestFrm.ValuePlus50BtnClick(Sender: TObject);
begin
  SuperSpinner.Spin(sdCW, 1, 60, UseProcessMsgCb.Checked);
end;

procedure TSSTestFrm.ValuePlus10BtnClick(Sender: TObject);
begin
  // Spin the wheel Clockwise 1 degree, 10 time. If UseProcessMsgCb is
  // True, then between each step ProcessMessages will be called to let
  // the screen update at each move. Otherwise it will just jump to the
  // end position depening on how the O/S does screen updates.
  //
  // Will cause the OnPosChange Event to fire 10 times in this case as
  // internally the Spin method will call bump as many times as needed.

  // To change speed or smoothness you can change the Angle and count, for
  // example you can have the Angle = 0.1 and the Step be 100. Will be slow
  // and smooth. You will however get 100 calls to OnPosChange() but you
  // can compensate by scaling your totalizer or other schemes. Play...

    SuperSpinner.Spin(sdCW, 1, 10, UseProcessMsgCb.Checked);
end;

procedure TSSTestFrm.ValuePlus1BtnClick(Sender: TObject);
begin
  // Bump the wheel Clockwise 1 degree, Will cause the
  // OnPosChange Event to fire

  SuperSpinner.Bump(sdCW, 1);
end;

procedure TSSTestFrm.ValueZeroBtnClick(Sender: TObject);
begin
  SuperSpinner.Angle :=0;
  FTotalizer := 0;
  TotalizerValueLbl.Caption := '0';
  AngleValueLbl.Caption := '0';
  DirectionValueLbl.Caption := 'N/A';
  WrappedValueLbl.Caption := 'Wrapped : N/A';
  AngleTb.Value := 0;
end;

procedure TSSTestFrm.WidthHeightAddBtnClick(Sender: TObject);
begin
  SuperSpinner.Width := SuperSpinner.Width + 10;
  SuperSpinner.Height := SuperSpinner.Height + 10;
  UpdateWHStats;
end;

procedure TSSTestFrm.HeightSubBtnClick(Sender: TObject);
begin
  SuperSpinner.Height := SuperSpinner.Height - 10;
  UpdateWHStats;
end;

procedure TSSTestFrm.LeftAddBtnClick(Sender: TObject);
begin
  SuperSpinner.Left := SuperSpinner.Left + 10;
  UpdateLTStats;
end;

procedure TSSTestFrm.LeftSubBtnClick(Sender: TObject);
begin
  SuperSpinner.Left := SuperSpinner.Left - 10;
  UpdateLTStats;
end;

procedure TSSTestFrm.ResetPositionBtnClick(Sender: TObject);
begin
  SuperSpinner.Top := 50;
  SuperSpinner.Left := 50;
  UpdateLTStats;
end;

procedure TSSTestFrm.ResetSizeBtnClick(Sender: TObject);
begin
    SuperSpinner.Width := 150;
    SuperSpinner.Height := 150;
    UpdateWHStats;
end;

procedure TSSTestFrm.TopAddBtnClick(Sender: TObject);
begin
    SuperSpinner.Top := SuperSpinner.Top + 10;
    UpdateLTStats;
end;

procedure TSSTestFrm.TopSubBtnClick(Sender: TObject);
begin
  SuperSpinner.Top := SuperSpinner.Top - 10;
  UpdateLTStats;
end;

procedure TSSTestFrm.WidthAddBtnClick(Sender: TObject);
begin
  SuperSpinner.Width := SuperSpinner.Width + 10;
  UpdateWHStats;
end;

procedure TSSTestFrm.WidthHeightSubBtnClick(Sender: TObject);
begin
  SuperSpinner.Width := SuperSpinner.Width - 10;
  SuperSpinner.Height := SuperSpinner.Height - 10;
  UpdateWHStats;
end;

procedure TSSTestFrm.WidthSubBtnClick(Sender: TObject);
begin
  SuperSpinner.Width := SuperSpinner.Width - 10;
  UpdateWHStats;
end;

procedure TSSTestFrm.UpdateSpinnerMovementStats;
begin
   // Only updates a few things since OnPosChange has more data

   AngleValueLbl.Caption := FloatToStrF(SuperSpinner.Angle, ffFixed, 3,3);
   AngleTb.Value := Round(SuperSpinner.Angle);
   TotalizerValueLbl.Caption := IntToStr(FTotalizer);
end;

procedure TSSTestFrm.UpdateBasicStats;
begin
  BackgroundColorCb.Selected := SuperSpinner.Color;
  SpinResolutionCb.ItemIndex := ord(SuperSpinner.SpinResolution);
  MouseWheelSpeedTB.Value := SuperSpinner.WheelSpeed;
  SpinnerLockedCb.Checked := SuperSpinner.Locked;
  PositionSnapCb.Checked := SuperSpinner.PositionSnap;
  AutoScaleCb.Checked := SuperSpinner.AutoScale;
  AutoScaleEnabledLbl.Visible := SuperSpinner.AutoScale;
  AngleTb.Value := Round(SuperSpinner.Angle);

end;

procedure TSSTestFrm.UpdateWHStats;
begin
    WidthValLbl.Caption := IntToStr(SuperSpinner.Width);
    HeightValLbl.Caption := IntToStr(SuperSpinner.Height);
end;

procedure TSSTestFrm.UpdateLTStats;
begin
    LeftValLbl.Caption := IntToStr(SuperSpinner.Left);
    TopValLbl.Caption := IntToStr(SuperSpinner.Top);
end;

procedure TSSTestFrm.UpdateFrameStats;
begin
  FrameBorderColorCb.Selected := SuperSpinner.FrameSettings.BorderColor;
  FrameRadiusSpe.Value := SuperSpinner.FrameSettings.BorderWidth;
end;

procedure TSSTestFrm.UpdateKnobStats;
begin
  KnobStyleCb.ItemIndex := ord(SuperSpinner.KnobSettings.Style);
  KnobFillColorCb.Selected := SuperSpinner.KnobSettings.FillColor;
  KnobEdgeColorCb.Selected := SuperSpinner.KnobSettings.EdgeColor;
  KnobEdgeThicknessSpe.Value := SuperSpinner.KnobSettings.EdgeThickness;
  KnobLightIntensitySpe.Value := SuperSpinner.KnobSettings.LightIntensity;
  KnobCurveExponentSpe.Value := SuperSpinner.KnobSettings.CurveExponent;
end;

procedure TSSTestFrm.UpdatePositionStats;
begin
  PositionStyleCb.ItemIndex := ord(SuperSpinner.PositionSettings.Style);
  PositionFillColorCb.Selected := SuperSpinner.PositionSettings.FillColor;
  PositionEdgeColorCb.Selected := SuperSpinner.PositionSettings.EdgeColor;
  PositionFillOpacitySpe.Value := SuperSpinner.PositionSettings.Opacity;
  PositionRadiusSpe.Value := SuperSpinner.PositionSettings.Radius;
  PositionEdgeThicknessSpe.Value := SuperSpinner.PositionSettings.EdgeThickness;
  PositionMarginSpe.Value := SuperSpinner.PositionSettings.Margin;
  PositionCenterMarginSpe.Value := SuperSpinner.PositionSettings.CenterMargin;
  PositionLineCountSpe.Value := SuperSpinner.PositionSettings.LineCount;
  PositionLineWidthSpe.Value := SuperSpinner.PositionSettings.LineWidth;
end;

procedure TSSTestFrm.UpdateCapStats;
begin
  CapStyleCb.ItemIndex := ord(SuperSpinner.CapSettings.Style);
  CapFillColorCb.Selected := SuperSpinner.CapSettings.FillColor;
  CapEdgeColorCb.Selected := SuperSpinner.CapSettings.EdgeColor;
  CapEdgeThicknessSpe.Value := SuperSpinner.CapSettings.EdgeThickness;
  CapRadiusSpe.Value := SuperSpinner.CapSettings.Radius;
  CapLightIntensitySpe.Value := SuperSpinner.CapSettings.LightIntensity;
  CapCurveExponentSpe.Value := SuperSpinner.CapSettings.CurveExponent;
end;

end.

