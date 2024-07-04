{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Author: Boban Spasic
}

unit untThemeBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, Menus, ComCtrls, TypInfo, BCLeaLCDDisplay, BCLeaTheme, BGRABitmapTypes,
  BCLeaTypes, BCLeaLED, BCLeaSelector, BCLeaRingSlider, BCLeaQLED, BCLeaBoard;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    BCLeaBoard: TBCLeaBoard;
    BTheme: TBCLeaTheme;
    Bevel1: TBevel;
    BLEDZoom: TBCLeaLED;
    BLED: TBCLeaLED;
    BQLED: TBCLeaQLED;
    BRingSlider: TBCLeaRingSlider;
    BSelector: TBCLeaSelector;
    cbLCDAutoSize: TCheckBox;
    cbBRSDrawText: TCheckBox;
    cbBRSDrawTextPhong: TCheckBox;
    cbBRSDrawPointer: TCheckBox;
    cbLEDClickable: TCheckBox;
    cbBSELDrawText: TCheckBox;
    cbQLEDClickable: TCheckBox;
    cbLEDValue: TCheckBox;
    cbLCDBlended: TCheckBox;
    cbLCDBlendOperations: TComboBox;
    cbLCDBlured: TCheckBox;
    cbBSELDrawTicks: TCheckBox;
    cbBSELDrawTextPhong: TCheckBox;
    cbQLEDValue: TCheckBox;
    cbL_DiffSat: TCheckBox;
    cbtLCDBoardColor: TColorButton;
    cbtBSELBkgColor: TColorButton;
    cbtBRSBkgColor: TColorButton;
    cbtBRSFontColor: TColorButton;
    cbtBSELFontShadowColor: TColorButton;
    cbtBSELFontColor: TColorButton;
    cbtBRSFontShadowColor: TColorButton;
    cbtBRSPointerColor: TColorButton;
    cbtBRSLineBkgColor: TColorButton;
    cbtBRSLineColor: TColorButton;
    cbtBoardBkgColor: TColorButton;
    cbtBoardBoard: TColorButton;
    cbtBoardFrame: TColorButton;
    cbtQLEDBkgColor: TColorButton;
    cbtLEDColorOff: TColorButton;
    cbtLCDDotOnColor: TColorButton;
    cbtLEDBkgColor: TColorButton;
    cbtLCDFrameColor: TColorButton;
    cbtBSELLineBkgColor: TColorButton;
    cbtQLEDColorOff: TColorButton;
    cbtLEDColorOn: TColorButton;
    cbtBSELLineColor: TColorButton;
    cbtQLEDColorOn: TColorButton;
    cbtLightColor: TColorButton;
    cbtPanelColor: TColorButton;
    GroupBox1: TGroupBox;
    GroupBox10: TGroupBox;
    GroupBox11: TGroupBox;
    GroupBox12: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    GroupBox9: TGroupBox;
    Label1: TLabel;
    lblBoardFrDistance: TLabel;
    lblBoardFrAltitude: TLabel;
    lblBoardFrRounding: TLabel;
    lblBRSAltitude: TLabel;
    lblBSELFontHeight: TLabel;
    lblBRSFontHeight: TLabel;
    lblBRSHeight: TLabel;
    lblBRSLineWidth: TLabel;
    lblBSELPointerSize: TLabel;
    lblBRSPointerSize: TLabel;
    lblBSELShadowOffsetX: TLabel;
    lblBRSShadowOffsetX: TLabel;
    lblBSELShadowOffsetY: TLabel;
    lblBRSShadowOffsetY: TLabel;
    lblBSELShadowRadius: TLabel;
    lblBRSShadowRadius: TLabel;
    lblBSELTextFont: TLabel;
    lblBRSTextFont: TLabel;
    lblBRSWidth: TLabel;
    lblLCDDisplayCharCount: TLabel;
    lblLCDDisplayLineCount: TLabel;
    lblLCDDotsBlended: TLabel;
    lblLCDDotSize: TLabel;
    lblLCDDotSpace: TLabel;
    lblLCDFrameAltitude: TLabel;
    lblLCDFrameHeight: TLabel;
    lblBSELAltitude: TLabel;
    lblBoardFrHeight: TLabel;
    lblQLEDAltitude: TLabel;
    lblQLEDRounding: TLabel;
    lblLEDHeight: TLabel;
    lblBSELHeight: TLabel;
    lblQLEDHeight: TLabel;
    lblLEDSize: TLabel;
    lblLCDFrameSize: TLabel;
    lblLCDHeight: TLabel;
    lblLEDAltitude: TLabel;
    lblBSELLineWidth: TLabel;
    lblBSELWidth: TLabel;
    lblLCDLinesText: TLabel;
    lblQLEDSize: TLabel;
    lblQLEDWidth: TLabel;
    lblL_1: TLabel;
    lblL_10: TLabel;
    lblL_11: TLabel;
    lblL_12: TLabel;
    lblL_2: TLabel;
    lblL_3: TLabel;
    lblL_4: TLabel;
    lblL_5: TLabel;
    lblL_6: TLabel;
    lblL_7: TLabel;
    lblL_8: TLabel;
    lblL_9: TLabel;
    lblLCDWidth: TLabel;
    lblLEDWidth: TLabel;
    LCDDisplay: TBCLeaLCDDisplay;
    MainMenu: TMainMenu;
    LoadTheme: TMenuItem;
    mmLCDText: TMemo;
    PageControl1: TPageControl;
    pnlComponents: TPanel;
    rgLCDBoardShadow: TRadioGroup;
    rgBRSZStyle: TRadioGroup;
    rgLCDDotShape: TRadioGroup;
    rgLCDFrameStyle: TRadioGroup;
    rgBoardFrameStyle: TRadioGroup;
    rgBoardBoardStyle: TRadioGroup;
    rgLEDZStyle: TRadioGroup;
    rgBSELZStyle: TRadioGroup;
    rgQLEDZStyle: TRadioGroup;
    SaveTheme: TMenuItem;
    seBoardFrDistance: TSpinEdit;
    seBoardFrAltitude: TSpinEdit;
    seBoardFrRounding: TSpinEdit;
    seBRSAltitude: TSpinEdit;
    seBSELFontHeigth: TSpinEdit;
    seBRSFontHeigth: TSpinEdit;
    seBRSHeigth: TSpinEdit;
    seBRSLineWidth: TSpinEdit;
    seBSELPointerSize: TSpinEdit;
    seBRSPointerSize: TSpinEdit;
    seBSELShadowOffsetX: TSpinEdit;
    seBRSShadowOffsetX: TSpinEdit;
    seBSELShadowOffsetY: TSpinEdit;
    seBRSShadowOffsetY: TSpinEdit;
    seBSELShadowRadius: TSpinEdit;
    seBRSShadowRadius: TSpinEdit;
    seBRSWidth: TSpinEdit;
    seLCDDisplayCharCount: TSpinEdit;
    seLCDDisplayLineCount: TSpinEdit;
    seLCDDotBlured: TFloatSpinEdit;
    seLCDDotSize: TSpinEdit;
    seLCDDotsSpace: TSpinEdit;
    seLCDFrameAltitude: TSpinEdit;
    seLCDFrameHeight: TSpinEdit;
    seBSELAltitude: TSpinEdit;
    seBoardFrHeight: TSpinEdit;
    seQLEDAltitude: TSpinEdit;
    seQLEDRounding: TSpinEdit;
    seLEDHeigth: TSpinEdit;
    seBSELHeigth: TSpinEdit;
    seQLEDHeigth: TSpinEdit;
    seLEDSize: TSpinEdit;
    seLCDFrameSize: TSpinEdit;
    seLCDHeigth: TSpinEdit;
    seLEDAltitude: TSpinEdit;
    seBSELLineWidth: TSpinEdit;
    seBSELWidth: TSpinEdit;
    seQLEDSize: TSpinEdit;
    seQLEDWidth: TSpinEdit;
    seL_Ambient: TFloatSpinEdit;
    seL_DiffFact: TFloatSpinEdit;
    seL_DistFact: TFloatSpinEdit;
    seL_NegDiffFact: TFloatSpinEdit;
    seL_PosX: TSpinEdit;
    seL_PosY: TSpinEdit;
    seL_PosZ: TSpinEdit;
    seL_SpecFact: TFloatSpinEdit;
    seL_SpecIndex: TFloatSpinEdit;
    seL_SrcDistFact: TFloatSpinEdit;
    seL_SrcDistTerm: TFloatSpinEdit;
    seL_SrcIntens: TFloatSpinEdit;
    Separator3: TMenuItem;
    miFile: TMenuItem;
    LoadChar: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveChar: TMenuItem;
    SaveDialog1: TSaveDialog;
    Quit: TMenuItem;
    Separator1: TMenuItem;
    pnlProperties: TPanel;
    seLCDWidth: TSpinEdit;
    seLEDWidth: TSpinEdit;
    tsBoard: TTabSheet;
    tsQLED: TTabSheet;
    tsLCD: TTabSheet;
    tsLED: TTabSheet;
    tsSelector: TTabSheet;
    tsRingSlider: TTabSheet;
    procedure BCLeaBoardClick(Sender: TObject);
    procedure BThemeChange(Sender: TObject);
    procedure BLEDChangeValue(Sender: TObject);
    procedure BLEDClick(Sender: TObject);
    procedure BQLEDChangeValue(Sender: TObject);
    procedure BQLEDClick(Sender: TObject);
    procedure BRingSliderClick(Sender: TObject);
    procedure BSelectorClick(Sender: TObject);
    procedure btnLoadCharDefsClick(Sender: TObject);
    procedure btnSaveCharDefsClick(Sender: TObject);
    procedure cbLCDAutoSizeChange(Sender: TObject);
    procedure cbLCDBlendedChange(Sender: TObject);
    procedure cbLCDBlendOperationsChange(Sender: TObject);
    procedure cbLCDBluredChange(Sender: TObject);
    procedure cbBRSDrawPointerChange(Sender: TObject);
    procedure cbBRSDrawTextChange(Sender: TObject);
    procedure cbBRSDrawTextPhongChange(Sender: TObject);
    procedure cbBSELDrawTextChange(Sender: TObject);
    procedure cbBSELDrawTextPhongChange(Sender: TObject);
    procedure cbBSELDrawTicksChange(Sender: TObject);
    procedure cbLEDClickableChange(Sender: TObject);
    procedure cbLEDValueChange(Sender: TObject);
    procedure cbL_DiffSatChange(Sender: TObject);
    procedure cbQLEDClickableChange(Sender: TObject);
    procedure cbQLEDValueChange(Sender: TObject);
    procedure cbtBoardBkgColorColorChanged(Sender: TObject);
    procedure cbtBoardBoardColorChanged(Sender: TObject);
    procedure cbtBoardFrameColorChanged(Sender: TObject);
    procedure cbtBRSBkgColorColorChanged(Sender: TObject);
    procedure cbtBRSFontColorColorChanged(Sender: TObject);
    procedure cbtBRSFontShadowColorColorChanged(Sender: TObject);
    procedure cbtBRSLineBkgColorColorChanged(Sender: TObject);
    procedure cbtBRSLineColorColorChanged(Sender: TObject);
    procedure cbtBRSPointerColorColorChanged(Sender: TObject);
    procedure cbtBSELBkgColorColorChanged(Sender: TObject);
    procedure cbtBSELFontColorColorChanged(Sender: TObject);
    procedure cbtBSELFontShadowColorColorChanged(Sender: TObject);
    procedure cbtBSELLineBkgColorColorChanged(Sender: TObject);
    procedure cbtBSELLineColorColorChanged(Sender: TObject);
    procedure cbtLEDBkgColorColorChanged(Sender: TObject);
    procedure cbtLEDColorOffColorChanged(Sender: TObject);
    procedure cbtLEDColorOnColorChanged(Sender: TObject);
    procedure cbtPanelColorColorChanged(Sender: TObject);
    procedure cbtLCDFrameColorColorChanged(Sender: TObject);
    procedure cbtLCDBoardColorColorChanged(Sender: TObject);
    procedure cbtLCDDotOnColorColorChanged(Sender: TObject);
    procedure cbtLightColorColorChanged(Sender: TObject);
    procedure cbtQLEDBkgColorColorChanged(Sender: TObject);
    procedure cbtQLEDColorOffColorChanged(Sender: TObject);
    procedure cbtQLEDColorOnColorChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LCDDisplayClick(Sender: TObject);
    procedure LCDDisplayMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure LCDDisplayMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure LCDDisplayMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure LoadThemeClick(Sender: TObject);
    procedure mmLCDTextChange(Sender: TObject);
    procedure QuitClick(Sender: TObject);
    procedure rgBoardBoardStyleClick(Sender: TObject);
    procedure rgBoardFrameStyleClick(Sender: TObject);
    procedure rgBRSZStyleClick(Sender: TObject);
    procedure rgLCDBoardShadowClick(Sender: TObject);
    procedure rgBSELZStyleClick(Sender: TObject);
    procedure rgLCDFrameStyleClick(Sender: TObject);
    procedure rgLCDDotShapeClick(Sender: TObject);
    procedure rgLEDZStyleClick(Sender: TObject);
    procedure rgQLEDZStyleClick(Sender: TObject);
    procedure SaveThemeClick(Sender: TObject);
    procedure seBoardFrAltitudeChange(Sender: TObject);
    procedure seBoardFrDistanceChange(Sender: TObject);
    procedure seBoardFrHeightChange(Sender: TObject);
    procedure seBoardFrRoundingChange(Sender: TObject);
    procedure seBRSAltitudeChange(Sender: TObject);
    procedure seBRSFontHeigthChange(Sender: TObject);
    procedure seBRSHeigthChange(Sender: TObject);
    procedure seBRSLineWidthChange(Sender: TObject);
    procedure seBRSPointerSizeChange(Sender: TObject);
    procedure seBRSShadowOffsetXChange(Sender: TObject);
    procedure seBRSShadowOffsetYChange(Sender: TObject);
    procedure seBRSShadowRadiusChange(Sender: TObject);
    procedure seBRSWidthChange(Sender: TObject);
    procedure seBSELAltitudeChange(Sender: TObject);
    procedure seBSELFontHeigthChange(Sender: TObject);
    procedure seBSELHeigthChange(Sender: TObject);
    procedure seBSELLineWidthChange(Sender: TObject);
    procedure seBSELPointerSizeChange(Sender: TObject);
    procedure seBSELShadowOffsetXChange(Sender: TObject);
    procedure seBSELShadowOffsetYChange(Sender: TObject);
    procedure seBSELShadowRadiusChange(Sender: TObject);
    procedure seBSELWidthChange(Sender: TObject);
    procedure seLCDDisplayCharCountChange(Sender: TObject);
    procedure seLCDDotBluredChange(Sender: TObject);
    procedure seLCDDotSizeChange(Sender: TObject);
    procedure seLCDDotsSpaceChange(Sender: TObject);
    procedure seLCDFrameAltitudeChange(Sender: TObject);
    procedure seLCDFrameHeightChange(Sender: TObject);
    procedure seLCDFrameSizeChange(Sender: TObject);
    procedure seLCDHeigthChange(Sender: TObject);
    procedure seLCDDisplayLineCountChange(Sender: TObject);
    procedure seLEDAltitudeChange(Sender: TObject);
    procedure seLEDHeigthChange(Sender: TObject);
    procedure seLEDSizeChange(Sender: TObject);
    procedure seLEDWidthChange(Sender: TObject);
    procedure seL_AmbientChange(Sender: TObject);
    procedure seL_DiffFactChange(Sender: TObject);
    procedure seL_DistFactChange(Sender: TObject);
    procedure seL_NegDiffFactChange(Sender: TObject);
    procedure seL_PosXChange(Sender: TObject);
    procedure seL_PosYChange(Sender: TObject);
    procedure seL_PosZChange(Sender: TObject);
    procedure seL_SpecFactChange(Sender: TObject);
    procedure seL_SpecIndexChange(Sender: TObject);
    procedure seL_SrcDistFactChange(Sender: TObject);
    procedure seL_SrcDistTermChange(Sender: TObject);
    procedure seL_SrcIntensChange(Sender: TObject);
    procedure seLCDWidthChange(Sender: TObject);
    procedure ReRead;
    procedure ApplyTheme;
    procedure seQLEDAltitudeChange(Sender: TObject);
    procedure seQLEDHeigthChange(Sender: TObject);
    procedure seQLEDRoundingChange(Sender: TObject);
    procedure seQLEDSizeChange(Sender: TObject);
    procedure seQLEDWidthChange(Sender: TObject);
  private
    FMouseDown: boolean;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.ApplyTheme;
begin
  pnlComponents.Color := BTheme.TestPanelColor;
  cbtPanelColor.ButtonColor:=BTheme.TestPanelColor;
  pnlComponents.Invalidate;
  LCDDisplay.ApplyTheme;
  BLEDZoom.ApplyTheme;
  BLED.ApplyTheme;
  BLEDZoom.Size := 45;
  BSelector.ApplyTheme;
  BRingSlider.ApplyTheme;
  BQLED.ApplyTheme;
  BCLeaBoard.ApplyTheme;
end;

procedure TfrmMain.seQLEDAltitudeChange(Sender: TObject);
begin
  BQLED.Altitude := seQLEDAltitude.Value;
  BTheme.QLED_Altitude := seQLEDAltitude.Value;
end;

procedure TfrmMain.seQLEDHeigthChange(Sender: TObject);
begin
  BQLED.Height := seQLEDHeigth.Value;
end;

procedure TfrmMain.seQLEDRoundingChange(Sender: TObject);
begin
  BQLED.Rounding := seQLEDRounding.Value;
  BTheme.QLED_Rounding := seQLEDRounding.Value;
end;

procedure TfrmMain.seQLEDSizeChange(Sender: TObject);
begin
  BQLED.Size := seQLEDSize.Value;
  BTheme.QLED_Size := seQLEDSize.Value;
end;

procedure TfrmMain.seQLEDWidthChange(Sender: TObject);
begin
  BQLED.Width := seQLEDWidth.Value;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: integer;
  bo: TBlendOperation;
begin
  cbLCDBlendOperations.Items.Clear;
  for bo := Low(TBlendOperation) to High(TBlendOperation) do
    cbLCDBlendOperations.Items.Add(GetEnumName(typeinfo(TBlendOperation), integer(bo)));

  FMouseDown := False;
  // Fix TLabel transparency issue in Laz 2.2.2+
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TLabel then
      TLabel(Components[i]).Transparent := True;

  PageControl1.ActivePage := tsLCD;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if pnlComponents.Color <> clDefault then
    cbtPanelColor.ButtonColor := pnlComponents.Color
  else
    cbtPanelColor.ButtonColor := clBtnFace;
  BLEDZoom.Size := 45;
  ReRead;
end;

procedure TfrmMain.cbtLCDFrameColorColorChanged(Sender: TObject);
begin
  LCDDisplay.FrameColor := cbtLCDFrameColor.ButtonColor;
  BTheme.LCD_FrameColor := cbtLCDFrameColor.ButtonColor;
  LCDDisplay.Invalidate;
end;

procedure TfrmMain.cbtLCDBoardColorColorChanged(Sender: TObject);
begin
  LCDDisplay.BoardColor := cbtLCDBoardColor.ButtonColor;
  BTheme.LCD_BoardColor := cbtLCDBoardColor.ButtonColor;
  LCDDisplay.Invalidate;
end;

procedure TfrmMain.cbtLCDDotOnColorColorChanged(Sender: TObject);
begin
  LCDDisplay.DotColorOn := cbtLCDDotOnColor.ButtonColor;
  BTheme.LCD_DotColorOn := cbtLCDDotOnColor.ButtonColor;
  LCDDisplay.Invalidate;
end;

procedure TfrmMain.cbtLightColorColorChanged(Sender: TObject);
begin
  BTheme.COM_LightColor := cbtLightColor.ButtonColor;
  ApplyTheme;
end;

procedure TfrmMain.cbtQLEDBkgColorColorChanged(Sender: TObject);
begin
  BQLED.BackgroundColor := cbtQLEDBkgColor.ButtonColor;
  BTheme.QLED_BkgColor := cbtQLEDBkgColor.ButtonColor;
  BQLED.Invalidate;
end;

procedure TfrmMain.cbtQLEDColorOffColorChanged(Sender: TObject);
begin
  BQLED.ColorOff := cbtQLEDColorOff.ButtonColor;
  BTheme.QLED_ColorOff := cbtQLEDColorOff.ButtonColor;
  BQLED.Invalidate;
end;

procedure TfrmMain.cbtQLEDColorOnColorChanged(Sender: TObject);
begin
  BQLED.ColorOn := cbtQLEDColorOn.ButtonColor;
  BTheme.QLED_ColorOn := cbtQLEDColorOn.ButtonColor;
  BQLED.Invalidate;
end;

procedure TfrmMain.LCDDisplayClick(Sender: TObject);
begin
  PageControl1.ActivePage := tsLCD;
end;

procedure TfrmMain.LCDDisplayMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
    FMouseDown := True;
end;

procedure TfrmMain.LCDDisplayMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FMouseDown then
  begin
    BTheme.COM_LightPositionY := Y;
    BTheme.COM_LightPositionX := X;
    ApplyTheme;
  end;
end;

procedure TfrmMain.LCDDisplayMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FMouseDown := False;
  end;
end;

procedure TfrmMain.LoadThemeClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'BCLea Theme|*.bclea';
  if OpenDialog1.Execute then
  begin
    LCDDisplay.LoadThemeFromFile(OpenDialog1.FileName);
    ApplyTheme;
    ReRead;
    frmMain.Caption := 'Theme Builder - ' + ExtractFileName(OpenDialog1.FileName);
    SaveDialog1.FileName := OpenDialog1.FileName;
  end;
end;

procedure TfrmMain.ReRead;
begin
  seL_Ambient.Value := BTheme.COM_AmbientFactor;
  seL_SpecIndex.Value := BTheme.COM_SpecularIndex;
  seL_SpecFact.Value := BTheme.COM_SpecularFactor;
  seL_DistFact.Value := BTheme.COM_LightDestFactor;
  seL_SrcIntens.Value := BTheme.COM_LightSourceIntensity;
  seL_SrcDistTerm.Value := BTheme.COM_LightSourceDistanceTerm;
  seL_SrcDistFact.Value := BTheme.COM_LightSourceDistanceFactor;
  seL_DiffFact.Value := BTheme.COM_DiffusionFactor;
  seL_NegDiffFact.Value := BTheme.COM_NegativeDiffusionFactor;
  seL_PosX.Value := BTheme.COM_LightPositionX;
  seL_PosY.Value := BTheme.COM_LightPositionY;
  seL_PosZ.Value := BTheme.COM_LightPositionZ;
  cbtLightColor.ButtonColor := BTheme.COM_LightColor;
  cbL_DiffSat.Checked := BTheme.COM_DiffuseSaturation;
  //BLCDDisplay
  mmLCDText.Lines := LCDDisplay.Lines;
  seLCDDisplayLineCount.Value := LCDDisplay.DisplayLineCount;
  seLCDDisplayCharCount.Value := LCDDisplay.DisplayCharCount;
  seLCDFrameSize.Value := LCDDisplay.FrameSize;
  seLCDDotSize.Value := LCDDisplay.DotSize;
  seLCDDotsSpace.Value := LCDDisplay.DotsSpace;
  seLCDWidth.Value := LCDDisplay.Width;
  seLCDHeigth.Value := LCDDisplay.Height;
  cbLCDAutoSize.Checked := LCDDisplay.AutoSize;
  cbtLCDFrameColor.ButtonColor := LCDDisplay.FrameColor;
  cbtLCDBoardColor.ButtonColor := LCDDisplay.BoardColor;
  cbtLCDDotOnColor.ButtonColor := LCDDisplay.DotColorOn;
  rgLCDFrameStyle.ItemIndex := integer(LCDDisplay.FrameStyle);
  rgLCDDotShape.ItemIndex := integer(LCDDisplay.DotShape);

  rgLCDBoardShadow.ItemIndex := integer(BTheme.LCD_BoardShadow);
  cbLCDBlendOperations.ItemIndex := integer(BTheme.LCD_DotBlendOperation);
  cbLCDBlured.Checked := boolean(BTheme.LCD_DotBlur);
  seLCDDotBlured.Value := single(BTheme.LCD_DotBlurRadius);
  cbLCDBlended.Checked := boolean(BTheme.LCD_DotBlend);
  seLCDFrameAltitude.Value := integer(LCDDisplay.FrameAltitude);
  seLCDFrameHeight.Value := LCDDisplay.FrameHeight;
  //BLED
  cbtLEDColorOn.ButtonColor := BTheme.LED_ColorOn;
  cbtLEDColorOff.ButtonColor := BTheme.LED_ColorOff;
  cbtLEDBkgColor.ButtonColor := BTheme.LED_BkgColor;
  seLEDSize.Value := BTheme.LED_Size;
  cbLEDClickable.Checked := BLED.Clickable;
  cbLEDValue.Checked := BLED.Value;
  seLEDHeigth.Value := BLED.Height;
  seLEDWidth.Value := BLED.Width;
  rgLEDZStyle.ItemIndex := integer(BTheme.LED_Style);
  seLEDAltitude.Value := BTheme.LED_Altitude;
  //BCLeaSelector
  seBSELLineWidth.Value := BTheme.SEL_LineWidth;
  cbtBSELLineColor.ButtonColor := BTheme.SEL_LineColor;
  cbtBSELLineBkgColor.ButtonColor := BTheme.SEL_LineBkgColor;
  cbtBSELBkgColor.ButtonColor := BTheme.SEL_BkgColor;
  cbtBSELFontShadowColor.ButtonColor := BTheme.SEL_FontShadowColor;
  seBSELShadowOffsetX.Value := BTheme.SEL_FontShadowOffsetX;
  seBSELShadowOffsetY.Value := BTheme.SEL_FontShadowOffsetY;
  seBSELShadowRadius.Value := BTheme.SEL_FontShadowRadius;
  seBSELPointerSize.Value := BTheme.SEL_PointerSize;
  rgBSELZStyle.ItemIndex := integer(BTheme.SEL_Style);
  cbBSELDrawTextPhong.Checked := BTheme.SEL_DrawTextPhong;
  seBSELAltitude.Value := BTheme.SEL_Altitude;
  cbBSELDrawText.Checked := BSelector.DrawText;
  cbBSELDrawTicks.Checked := BSelector.DrawTicks;
  seBSELWidth.Value := BSelector.Width;
  seBSELHeigth.Value := BSelector.Height;
  cbtBSELFontColor.ButtonColor := BSelector.Font.Color;
  seBSELFontHeigth.Value := BSelector.Font.Height;
  //RingSlider
  seBRSLineWidth.Value := BTheme.RS_LineWidth;
  cbtBRSLineColor.ButtonColor := BTheme.RS_LineColor;
  cbtBRSLineBkgColor.ButtonColor := BTheme.RS_LineBkgColor;
  cbtBRSBkgColor.ButtonColor := BTheme.RS_BkgColor;
  cbtBRSFontShadowColor.ButtonColor := BTheme.RS_FontShadowColor;
  seBRSShadowOffsetX.Value := BTheme.RS_FontShadowOffsetX;
  seBRSShadowOffsetY.Value := BTheme.RS_FontShadowOffsetY;
  seBRSShadowRadius.Value := BTheme.RS_FontShadowRadius;
  seBRSPointerSize.Value := BTheme.RS_PointerSize;
  cbtBRSPointerColor.ButtonColor := BTheme.RS_PointerColor;
  rgBRSZStyle.ItemIndex := integer(BTheme.RS_Style);
  cbBRSDrawTextPhong.Checked := BTheme.RS_DrawTextPhong;
  seBRSAltitude.Value := BTheme.RS_Altitude;
  cbBRSDrawText.Checked := BRingSlider.DrawText;
  cbBRSDrawPointer.Checked := BRingSlider.DrawPointer;
  seBRSWidth.Value := BRingSlider.Width;
  seBRSHeigth.Value := BRingSlider.Height;
  cbtBRSFontColor.ButtonColor := BRingSlider.Font.Color;
  seBRSFontHeigth.Value := BRingSlider.Font.Height;
  //BCLeaQLED
  cbtQLEDColorOn.ButtonColor := BTheme.QLED_ColorOn;
  cbtQLEDColorOff.ButtonColor := BTheme.QLED_ColorOff;
  cbtQLEDBkgColor.ButtonColor := BTheme.QLED_BkgColor;
  seQLEDSize.Value := BTheme.QLED_Size;
  cbQLEDClickable.Checked := BQLED.Clickable;
  cbQLEDValue.Checked := BQLED.Value;
  seQLEDHeigth.Value := BQLED.Height;
  seQLEDWidth.Value := BQLED.Width;
  rgQLEDZStyle.ItemIndex := integer(BTheme.QLED_Style);
  seQLEDAltitude.Value := BTheme.QLED_Altitude;
  seQLEDRounding.Value := BTheme.QLED_Rounding;
  //BCLeaBoard
  cbtBoardFrame.ButtonColor := BTHeme.BRD_FrameColor;
  cbtBoardBoard.ButtonColor := BTHeme.BRD_BoardColor;
  cbtBoardBkgColor.ButtonColor := BTHeme.BRD_BkgColor;
  seBoardFrHeight.Value := BTheme.BRD_FrameHeight;
  seBoardFrDistance.Value := BTheme.BRD_FrameDistance;
  seBoardFrRounding.Value := BTheme.BRD_Rounding;
  seBoardFrAltitude.Value := BTheme.BRD_Altitude;
  rgBoardFrameStyle.ItemIndex := integer(BTheme.BRD_FrameStyle);
  rgBoardBoardStyle.ItemIndex := integer(BTheme.BRD_BoardStyle);
end;

procedure TfrmMain.cbLCDAutoSizeChange(Sender: TObject);
begin
  LCDDisplay.AutoSize := cbLCDAutoSize.Checked;
  if not cbLCDAutoSize.Checked then
  begin
    LCDDisplay.Width := seLCDWidth.Value;
    LCDDisplay.Height := seLCDHeigth.Value;
  end;
  ReRead;
end;

procedure TfrmMain.cbLCDBlendedChange(Sender: TObject);
begin
  BTheme.LCD_DotBlend := cbLCDBlended.Checked;
  ApplyTheme;
end;

procedure TfrmMain.cbLCDBlendOperationsChange(Sender: TObject);
begin
  BTheme.LCD_DotBlendOperation := TBlendOperation(cbLCDBlendOperations.ItemIndex);
  ApplyTheme;
end;

procedure TfrmMain.cbLCDBluredChange(Sender: TObject);
begin
  BTheme.LCD_DotBlur := cbLCDBlured.Checked;
  ApplyTheme;
end;

procedure TfrmMain.cbBRSDrawPointerChange(Sender: TObject);
begin
  BRingSlider.DrawPointer := cbBRSDrawPointer.Checked;
end;

procedure TfrmMain.cbBRSDrawTextChange(Sender: TObject);
begin
  BRingSlider.DrawText := cbBRSDrawText.Checked;
end;

procedure TfrmMain.cbBRSDrawTextPhongChange(Sender: TObject);
begin
  BRingSlider.DrawTextPhong := cbBRSDrawTextPhong.Checked;
  BTheme.RS_DrawTextPhong := cbBRSDrawTextPhong.Checked;
end;

procedure TfrmMain.cbBSELDrawTextChange(Sender: TObject);
begin
  BSelector.DrawText := cbBSELDrawText.Checked;
end;

procedure TfrmMain.cbBSELDrawTextPhongChange(Sender: TObject);
begin
  BSelector.DrawTextPhong := cbBSELDrawTextPhong.Checked;
  BTheme.SEL_DrawTextPhong := cbBSELDrawTextPhong.Checked;
end;

procedure TfrmMain.cbBSELDrawTicksChange(Sender: TObject);
begin
  BSelector.DrawTicks := cbBSELDrawTicks.Checked;
end;

procedure TfrmMain.btnSaveCharDefsClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'BCLeaLCDDisplay CharDefs|*.char.xml';
  if SaveDialog1.Execute then
    LCDDisplay.CharDefs.SaveToFile(SaveDialog1.FileName);
end;

procedure TfrmMain.btnLoadCharDefsClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'BCLeaLCDDisplay CharDefs|*.char.xml';
  if OpenDialog1.Execute then
    LCDDisplay.CharDefs.LoadFromFile(OpenDialog1.FileName);
end;

procedure TfrmMain.BThemeChange(Sender: TObject);
begin
  //ApplyTheme;
  //ReRead;
end;

procedure TfrmMain.BCLeaBoardClick(Sender: TObject);
begin
  PageControl1.ActivePage := tsBoard;
end;

procedure TfrmMain.BLEDChangeValue(Sender: TObject);
begin
  BLEDZoom.Value := BLED.Value;
  ReRead;
end;

procedure TfrmMain.BLEDClick(Sender: TObject);
begin
  PageControl1.ActivePage := tsLED;
end;

procedure TfrmMain.BQLEDChangeValue(Sender: TObject);
begin
  ReRead;
end;

procedure TfrmMain.BQLEDClick(Sender: TObject);
begin
  PageControl1.ActivePage := tsQLED;
end;

procedure TfrmMain.BRingSliderClick(Sender: TObject);
begin
  PageControl1.ActivePage := tsRingSlider;
end;

procedure TfrmMain.BSelectorClick(Sender: TObject);
begin
  PageControl1.ActivePage := tsSelector;
end;

procedure TfrmMain.cbLEDClickableChange(Sender: TObject);
begin
  BLED.Clickable := cbLEDClickable.Checked;
end;

procedure TfrmMain.cbLEDValueChange(Sender: TObject);
begin
  BLED.Value := cbLEDValue.Checked;
  BLEDZoom.Value := cbLEDValue.Checked;
end;

procedure TfrmMain.cbL_DiffSatChange(Sender: TObject);
begin
  BTheme.COM_DiffuseSaturation := cbL_DiffSat.Checked;
  ApplyTheme;
end;

procedure TfrmMain.cbQLEDClickableChange(Sender: TObject);
begin
  BQLED.Clickable := cbQLEDClickable.Checked;
end;

procedure TfrmMain.cbQLEDValueChange(Sender: TObject);
begin
  BQLED.Value := cbQLEDValue.Checked;
end;

procedure TfrmMain.cbtBoardBkgColorColorChanged(Sender: TObject);
begin
  BCLeaBoard.BackgroundColor := cbtBoardBkgColor.ButtonColor;
  BTheme.BRD_BkgColor := cbtBoardBkgColor.ButtonColor;
  BCLeaBoard.Invalidate;
end;

procedure TfrmMain.cbtBoardBoardColorChanged(Sender: TObject);
begin
  BCLeaBoard.BoardColor := cbtBoardBoard.ButtonColor;
  BTheme.BRD_BoardColor := cbtBoardBoard.ButtonColor;
  BCLeaBoard.Invalidate;
end;

procedure TfrmMain.cbtBoardFrameColorChanged(Sender: TObject);
begin
  BCLeaBoard.FrameColor := cbtBoardFrame.ButtonColor;
  BTheme.BRD_FrameColor := cbtBoardFrame.ButtonColor;
  BCLeaBoard.Invalidate;
end;

procedure TfrmMain.cbtBRSBkgColorColorChanged(Sender: TObject);
begin
  BRingSlider.BackgroundColor := cbtBRSBkgColor.ButtonColor;
  BTheme.RS_BkgColor := cbtBRSBkgColor.ButtonColor;
  BRingSlider.Invalidate;
end;

procedure TfrmMain.cbtBRSFontColorColorChanged(Sender: TObject);
begin
  BRingSlider.Font.Color := cbtBRSFontColor.ButtonColor;
  BRingSlider.Invalidate;
end;

procedure TfrmMain.cbtBRSFontShadowColorColorChanged(Sender: TObject);
begin
  BRingSlider.FontShadowColor := cbtBRSFontShadowColor.ButtonColor;
  BTheme.RS_FontShadowColor := cbtBRSFontShadowColor.ButtonColor;
  BRingSlider.Invalidate;
end;

procedure TfrmMain.cbtBRSLineBkgColorColorChanged(Sender: TObject);
begin
  BRingSlider.LineBkgColor := cbtBRSLineBkgColor.ButtonColor;
  BTheme.RS_LineBkgColor := cbtBRSLineBkgColor.ButtonColor;
  BRingSlider.Invalidate;
end;

procedure TfrmMain.cbtBRSLineColorColorChanged(Sender: TObject);
begin
  BRingSlider.LineColor := cbtBRSLineColor.ButtonColor;
  BTheme.RS_LineColor := cbtBRSLineColor.ButtonColor;
  BRingSlider.Invalidate;
end;

procedure TfrmMain.cbtBRSPointerColorColorChanged(Sender: TObject);
begin
  BRingSlider.PointerColor := cbtBRSPointerColor.ButtonColor;
  BTheme.RS_PointerColor := cbtBRSPointerColor.ButtonColor;
  BRingSlider.Invalidate;
end;

procedure TfrmMain.cbtBSELBkgColorColorChanged(Sender: TObject);
begin
  BSelector.BackgroundColor := cbtBSELBkgColor.ButtonColor;
  BTheme.SEL_BkgColor := cbtBSELBkgColor.ButtonColor;
  BSelector.Invalidate;
end;

procedure TfrmMain.cbtBSELFontColorColorChanged(Sender: TObject);
begin
  BSelector.Font.Color := cbtBSELFontColor.ButtonColor;
  BSelector.Invalidate;
end;

procedure TfrmMain.cbtBSELFontShadowColorColorChanged(Sender: TObject);
begin
  BSelector.FontShadowColor := cbtBSELFontShadowColor.ButtonColor;
  BTheme.SEL_FontShadowColor := cbtBSELFontShadowColor.ButtonColor;
  BSelector.Invalidate;
end;

procedure TfrmMain.cbtBSELLineBkgColorColorChanged(Sender: TObject);
begin
  BSelector.LineBkgColor := cbtBSELLineBkgColor.ButtonColor;
  BTheme.SEL_LineBkgColor := cbtBSELLineBkgColor.ButtonColor;
  BSelector.Invalidate;
end;

procedure TfrmMain.cbtBSELLineColorColorChanged(Sender: TObject);
begin
  BSelector.LineColor := cbtBSELLineColor.ButtonColor;
  BTheme.SEL_LineColor := cbtBSELLineColor.ButtonColor;
  BSelector.Invalidate;
end;

procedure TfrmMain.cbtLEDBkgColorColorChanged(Sender: TObject);
begin
  BLED.BackgroundColor := cbtLEDBkgColor.ButtonColor;
  BLEDZoom.BackgroundColor := cbtLEDBkgColor.ButtonColor;
  BTheme.LED_BkgColor := cbtLEDBkgColor.ButtonColor;
  BLED.Invalidate;
  BLEDZoom.Invalidate;
end;

procedure TfrmMain.cbtLEDColorOffColorChanged(Sender: TObject);
begin
  BLED.ColorOff := cbtLEDColorOff.ButtonColor;
  BLEDZoom.ColorOff := cbtLEDColorOff.ButtonColor;
  BTheme.LED_ColorOff := cbtLEDColorOff.ButtonColor;
  BLED.Invalidate;
  BLEDZoom.Invalidate;
end;

procedure TfrmMain.cbtLEDColorOnColorChanged(Sender: TObject);
begin
  BLED.ColorOn := cbtLEDColorOn.ButtonColor;
  BLEDZoom.ColorOn := cbtLEDColorOn.ButtonColor;
  BTheme.LED_ColorOn := cbtLEDColorOn.ButtonColor;
  BLED.Invalidate;
  BLEDZoom.Invalidate;
end;

procedure TfrmMain.cbtPanelColorColorChanged(Sender: TObject);
begin
  pnlComponents.Color := cbtPanelColor.ButtonColor;
  BTheme.TestPanelColor := cbtPanelColor.ButtonColor;
  pnlComponents.Invalidate;
end;

procedure TfrmMain.mmLCDTextChange(Sender: TObject);
begin
  LCDDisplay.Lines.Assign(mmLCDText.Lines);
end;

procedure TfrmMain.QuitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.rgBoardBoardStyleClick(Sender: TObject);
begin
  BCLeaBoard.BoardStyle := TZStyle(rgBoardBoardStyle.ItemIndex);
  BTheme.BRD_BoardStyle := TZStyle(rgBoardBoardStyle.ItemIndex);
end;

procedure TfrmMain.rgBoardFrameStyleClick(Sender: TObject);
begin
  BCLeaBoard.FrameStyle := TZStyle(rgBoardFrameStyle.ItemIndex);
  BTheme.BRD_FrameStyle := TZStyle(rgBoardFrameStyle.ItemIndex);
end;

procedure TfrmMain.rgBRSZStyleClick(Sender: TObject);
begin
  BRingSlider.Style := TZStyle(rgBRSZStyle.ItemIndex);
  BTheme.RS_Style := TZStyle(rgBRSZStyle.ItemIndex);
end;

procedure TfrmMain.rgLCDBoardShadowClick(Sender: TObject);
begin
  BTheme.LCD_BoardShadow := TBoardShadow(rgLCDBoardShadow.ItemIndex);
  ApplyTheme;
end;

procedure TfrmMain.rgBSELZStyleClick(Sender: TObject);
begin
  BSelector.Style := TZStyle(rgBSELZStyle.ItemIndex);
  BTheme.SEL_Style := TZStyle(rgBSELZStyle.ItemIndex);
end;

procedure TfrmMain.rgLCDFrameStyleClick(Sender: TObject);
begin
  LCDDisplay.FrameStyle := TZStyle(rgLCDFrameStyle.ItemIndex);
  BTheme.LCD_FrameStyle := TZStyle(rgLCDFrameStyle.ItemIndex);
end;

procedure TfrmMain.rgLCDDotShapeClick(Sender: TObject);
begin
  LCDDisplay.DotShape := TDotShape(rgLCDDotShape.ItemIndex);
  BTheme.LCD_DotShape := TDotShape(rgLCDDotShape.ItemIndex);
end;

procedure TfrmMain.rgLEDZStyleClick(Sender: TObject);
begin
  BLED.Style := TZStyle(rgLEDZStyle.ItemIndex);
  BLEDZoom.Style := TZStyle(rgLEDZStyle.ItemIndex);
  BTheme.LED_Style := TZStyle(rgLEDZStyle.ItemIndex);
end;

procedure TfrmMain.rgQLEDZStyleClick(Sender: TObject);
begin
  BQLED.Style := TZStyle(rgQLEDZStyle.ItemIndex);
  BTheme.QLED_Style := TZStyle(rgQLEDZStyle.ItemIndex);
end;

procedure TfrmMain.SaveThemeClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'BCLea Theme|*.bclea';
  if SaveDialog1.Execute then
  begin
    LCDDisplay.UpdateTheme;
    LCDDisplay.SaveThemeToFile(SaveDialog1.FileName);
    frmMain.Caption := 'Theme Builder - ' + ExtractFileName(SaveDialog1.FileName);
  end;
end;

procedure TfrmMain.seBoardFrAltitudeChange(Sender: TObject);
begin
  BCLeaBoard.Altitude := seBoardFrAltitude.Value;
  BTheme.BRD_Altitude := seBoardFrAltitude.Value;
end;

procedure TfrmMain.seBoardFrDistanceChange(Sender: TObject);
begin
  BCLeaBoard.FrameDistance := seBoardFrDistance.Value;
  BTheme.BRD_FrameDistance := seBoardFrDistance.Value;
end;

procedure TfrmMain.seBoardFrHeightChange(Sender: TObject);
begin
  BCLeaBoard.FrameHeight := seBoardFrHeight.Value;
  BTheme.BRD_FrameHeight := seBoardFrHeight.Value;
end;

procedure TfrmMain.seBoardFrRoundingChange(Sender: TObject);
begin
  BCLeaBoard.Rounding := seBoardFrRounding.Value;
  BTheme.BRD_Rounding := seBoardFrRounding.Value;
end;

procedure TfrmMain.seBRSAltitudeChange(Sender: TObject);
begin
  BRingSlider.Altitude := seBRSAltitude.Value;
  BTheme.RS_Altitude := seBRSAltitude.Value;
end;

procedure TfrmMain.seBRSFontHeigthChange(Sender: TObject);
begin
  BRingSlider.Font.Height := seBRSFontHeigth.Value;
end;

procedure TfrmMain.seBRSHeigthChange(Sender: TObject);
begin
  BRingSlider.Height := seBRSHeigth.Value;
end;

procedure TfrmMain.seBRSLineWidthChange(Sender: TObject);
begin
  BRingSlider.LineWidth := seBRSLineWidth.Value;
  BTheme.RS_LineWidth := seBRSLineWidth.Value;
end;

procedure TfrmMain.seBRSPointerSizeChange(Sender: TObject);
begin
  BRingSlider.PointerSize := seBRSPointerSize.Value;
  BTheme.RS_PointerSize := seBRSPointerSize.Value;
end;

procedure TfrmMain.seBRSShadowOffsetXChange(Sender: TObject);
begin
  BRingSlider.FontShadowOffsetX := seBRSShadowOffsetX.Value;
  BTheme.RS_FontShadowOffsetX := seBRSShadowOffsetX.Value;
end;

procedure TfrmMain.seBRSShadowOffsetYChange(Sender: TObject);
begin
  BRingSlider.FontShadowOffsetY := seBRSShadowOffsetY.Value;
  BTheme.RS_FontShadowOffsetY := seBRSShadowOffsetY.Value;
end;

procedure TfrmMain.seBRSShadowRadiusChange(Sender: TObject);
begin
  BRingSlider.FontShadowRadius := seBRSShadowRadius.Value;
  BTheme.RS_FontShadowRadius := seBRSShadowRadius.Value;
end;

procedure TfrmMain.seBRSWidthChange(Sender: TObject);
begin
  BRingSlider.Width := seBRSWidth.Value;
end;

procedure TfrmMain.seBSELAltitudeChange(Sender: TObject);
begin
  BSelector.Altitude := seBSELAltitude.Value;
  BTheme.SEL_Altitude := seBSELAltitude.Value;
end;

procedure TfrmMain.seBSELFontHeigthChange(Sender: TObject);
begin
  BSelector.Font.Height := seBSELFontHeigth.Value;
end;

procedure TfrmMain.seBSELHeigthChange(Sender: TObject);
begin
  BSelector.Height := seBSELHeigth.Value;
end;

procedure TfrmMain.seBSELLineWidthChange(Sender: TObject);
begin
  BSelector.LineWidth := seBSELLineWidth.Value;
  BTheme.SEL_LineWidth := seBSELLineWidth.Value;
end;

procedure TfrmMain.seBSELPointerSizeChange(Sender: TObject);
begin
  BSelector.PointerSize := seBSELPointerSize.Value;
  BTheme.SEL_PointerSize := seBSELPointerSize.Value;
end;

procedure TfrmMain.seBSELShadowOffsetXChange(Sender: TObject);
begin
  BSelector.FontShadowOffsetX := seBSELShadowOffsetX.Value;
  BTheme.SEL_FontShadowOffsetX := seBSELShadowOffsetX.Value;
end;

procedure TfrmMain.seBSELShadowOffsetYChange(Sender: TObject);
begin
  BSelector.FontShadowOffsetY := seBSELShadowOffsetY.Value;
  BTheme.SEL_FontShadowOffsetY := seBSELShadowOffsetY.Value;
end;

procedure TfrmMain.seBSELShadowRadiusChange(Sender: TObject);
begin
  BSelector.FontShadowRadius := seBSELShadowRadius.Value;
  BTheme.SEL_FontShadowRadius := seBSELShadowRadius.Value;
end;

procedure TfrmMain.seBSELWidthChange(Sender: TObject);
begin
  BSelector.Width := seBSELWidth.Value;
end;

procedure TfrmMain.seLCDDisplayCharCountChange(Sender: TObject);
begin
  LCDDisplay.DisplayCharCount := seLCDDisplayCharCount.Value;
  ReRead;
end;

procedure TfrmMain.seLCDDotBluredChange(Sender: TObject);
begin
  BTheme.LCD_DotBlurRadius := seLCDDotBlured.Value;
  ApplyTheme;
end;

procedure TfrmMain.seLCDDotSizeChange(Sender: TObject);
begin
  LCDDisplay.DotSize := seLCDDotSize.Value;
  BTheme.LCD_DotSize := seLCDDotSize.Value;
end;

procedure TfrmMain.seLCDDotsSpaceChange(Sender: TObject);
begin
  LCDDisplay.DotsSpace := seLCDDotsSpace.Value;
  BTheme.LCD_DotsSpace := seLCDDotsSpace.Value;
end;

procedure TfrmMain.seLCDFrameAltitudeChange(Sender: TObject);
begin
  LCDDisplay.FrameAltitude := seLCDFrameAltitude.Value;
  BTheme.LCD_FrameAltitude := seLCDFrameAltitude.Value;
end;

procedure TfrmMain.seLCDFrameHeightChange(Sender: TObject);
begin
  LCDDisplay.FrameHeight := seLCDFrameHeight.Value;
  BTheme.LCD_FrameHeight := seLCDFrameHeight.Value;
end;

procedure TfrmMain.seLCDFrameSizeChange(Sender: TObject);
begin
  LCDDisplay.FrameSize := seLCDFrameSize.Value;
  BTheme.LCD_FrameSize := seLCDFrameSize.Value;
end;

procedure TfrmMain.seLCDHeigthChange(Sender: TObject);
begin
  LCDDisplay.Height := seLCDHeigth.Value;
end;

procedure TfrmMain.seLCDDisplayLineCountChange(Sender: TObject);
begin
  LCDDisplay.DisplayLineCount := seLCDDisplayLineCount.Value;
  ReRead;
end;

procedure TfrmMain.seLEDAltitudeChange(Sender: TObject);
begin
  BLED.Altitude := seLEDAltitude.Value;
  BLEDZoom.Altitude := seLEDAltitude.Value;
  BTheme.LED_Altitude := seLEDAltitude.Value;
end;

procedure TfrmMain.seLEDHeigthChange(Sender: TObject);
begin
  BLED.Height := seLEDHeigth.Value;
end;

procedure TfrmMain.seLEDSizeChange(Sender: TObject);
begin
  BLED.Size := seLEDSize.Value;
  BTheme.LED_Size := seLEDSize.Value;
  BLEDZoom.Size := 45;
end;

procedure TfrmMain.seLEDWidthChange(Sender: TObject);
begin
  BLED.Width := seLEDWidth.Value;
end;

procedure TfrmMain.seL_AmbientChange(Sender: TObject);
begin
  BTheme.COM_AmbientFactor := seL_Ambient.Value;
  ApplyTheme;
end;

procedure TfrmMain.seL_DiffFactChange(Sender: TObject);
begin
  BTheme.COM_DiffusionFactor := seL_DiffFact.Value;
  ApplyTheme;
end;

procedure TfrmMain.seL_DistFactChange(Sender: TObject);
begin
  BTheme.COM_LightSourceDistanceFactor := seL_DistFact.Value;
  ApplyTheme;
end;

procedure TfrmMain.seL_NegDiffFactChange(Sender: TObject);
begin
  BTheme.COM_NegativeDiffusionFactor := seL_NegDiffFact.Value;
  ApplyTheme;
end;

procedure TfrmMain.seL_PosXChange(Sender: TObject);
begin
  BTheme.COM_LightPositionX := seL_PosX.Value;
  ApplyTheme;
end;

procedure TfrmMain.seL_PosYChange(Sender: TObject);
begin
  BTheme.COM_LightPositionY := seL_PosY.Value;
  ApplyTheme;
end;

procedure TfrmMain.seL_PosZChange(Sender: TObject);
begin
  if seL_PosZ.Value > 0 then
  begin
    BTheme.COM_LightPositionZ := seL_PosZ.Value;
    ApplyTheme;
  end;
end;

procedure TfrmMain.seL_SpecFactChange(Sender: TObject);
begin
  BTheme.COM_SpecularFactor := seL_SpecFact.Value;
  ApplyTheme;
end;

procedure TfrmMain.seL_SpecIndexChange(Sender: TObject);
begin
  BTheme.COM_SpecularIndex := seL_SpecIndex.Value;
  ApplyTheme;
end;

procedure TfrmMain.seL_SrcDistFactChange(Sender: TObject);
begin
  BTheme.COM_LightSourceDistanceFactor := seL_SrcDistFact.Value;
  ApplyTheme;
end;

procedure TfrmMain.seL_SrcDistTermChange(Sender: TObject);
begin
  BTheme.COM_LightSourceDistanceTerm := seL_SrcDistTerm.Value;
  ApplyTheme;
end;

procedure TfrmMain.seL_SrcIntensChange(Sender: TObject);
begin
  BTheme.COM_LightSourceIntensity := seL_SrcIntens.Value;
  ApplyTheme;
end;

procedure TfrmMain.seLCDWidthChange(Sender: TObject);
begin
  LCDDisplay.Width := seLCDWidth.Value;
  ReRead;
end;

end.
