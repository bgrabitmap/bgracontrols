// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Part of BGRA Controls. Made by third party.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html

}
{******************************* CONTRIBUTOR(S) ******************************
- Sandy Ganz | sganz@pacbell.net
  Test Program for SuperGauge
***************************** END CONTRIBUTOR(S) *****************************}
{******************************** CHANGE LOG *********************************
v2.01 - Swapped uELED component with SuperLED component
******************************* END CHANGE LOG *******************************}

unit sgtest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Buttons, ColorBox, ComboEx, ExtDlgs, Menus, LazHelpHTML, HelpIntfs,
  SpinEx, BGRAKnob, BGRAShape, BGRAImageList, SuperGaugeCommon,
  SuperGauge, SuperLED, about;

const
  VERSIONSTR = '2.01';            // SG TEST version, Should ALWAYS show as a delta when merging!

type
  { TSGTestFrm }

  TSGTestFrm = class(TForm)
    AutoScaleCb: TCheckBox;
    AuxMaxValLbl: TLabel;
    AuxMaxValueSpe: TFloatSpinEditEx;
    AuxMinValueLbl: TLabel;
    AuxMinValueSpe: TFloatSpinEditEx;
    AuxPointerColorCb: TColorBox;
    AuxPointerColorLbl: TLabel;
    AuxPointerEnabledCb: TCheckBox;
    AuxPointerExtensionLengthLbl: TLabel;
    AuxPointerExtensionLengthSpe: TSpinEditEx;
    AuxPointerLengthLbl: TLabel;
    AuxPointerLengthSpe: TSpinEditEx;
    AuxPointerStyleCb: TComboBox;
    AuxPointerStyleLbl: TLabel;
    AuxPointerWidthLbl: TLabel;
    AuxPointerWidthSpe: TSpinEditEx;
    AuxScaleEnabledCb: TCheckBox;
    BackgroundColorCb: TColorBox;
    BackgroundColorLbl: TLabel;
    BGRAKnob: TBGRAKnob;
    AuxDispMaxLbl: TLabel;
    BGRAKnobAux: TBGRAKnob;
    AuxPointerHighlightCb: TCheckBox;
    HTMLBrowserHelpViewer1: THTMLBrowserHelpViewer;
    HTMLHelpDatabase1: THTMLHelpDatabase;
    MenuItem1: TMenuItem;
    RangeLEDActiveCb: TCheckBox;
    ComboBox1: TComboBox;
    EnableScaleTextCb: TCheckBox;
    AuxEnableScaleTextCb: TCheckBox;
    GroupBox8: TGroupBox;
    GroupBox9: TGroupBox;
    PointerHighlightColorCb: TColorBox;
    PointerHighlightCb: TCheckBox;
    DispAuxMaxValLbl: TLabel;
    AuxDispMinLbl: TLabel;
    DispAuxMinValLbl: TLabel;
    AuxKnobValLbl: TLabel;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    AuxGaugeValLbl: TLabel;
    AuxPointerHighlightColorCb: TColorBox;
    AuxPointerHighlightColorLbl: TLabel;
    PointerHighlightThicknessLbl: TLabel;
    PointerHighlightColorLbl: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    FrameMiddleFrameThicknessLbl: TLabel;
    FrameInnerFrameThicknessLbl: TLabel;
    CapEdgeWidthLbl: TLabel;
    CapEdgeWidthSpe: TSpinEditEx;
    FrameMiddleFrameColorCb: TColorBox;
    FrameInnerFrameColorCb: TColorBox;
    FrameMiddleFrameColorLbl: TLabel;
    FrameInnerFrameColorLbl: TLabel;
    FrameMiddleFrameThicknessSpe: TSpinEditEx;
    FrameInnerFrameThicknessSpe: TSpinEditEx;
    PointerColorCb: TColorBox;
    PointerColorLbl: TLabel;
    PointerEnabledCb: TCheckBox;
    PointerExtensionLengthLbl: TLabel;
    PointerExtensionLengthSpe: TSpinEditEx;
    AuxPointerHighlightThicknessLbl: TLabel;
    AuxPointerHighlightThicknessSpe: TSpinEditEx;
    PointerLengthLbl: TLabel;
    PointerLengthSpe: TSpinEditEx;
    PointerHighlightThicknessSpe: TSpinEditEx;
    PointerStyleCb: TComboBox;
    PointerStyleLbl: TLabel;
    PointerWidthLbl: TLabel;
    PointerWidthSpe: TSpinEditEx;
    FaceCurveExponentLbl: TLabel;
    FaceCurveExponentSpe: TFloatSpinEditEx;
    FaceLightIntensityLbl: TLabel;
    FaceLightIntensitySpe: TSpinEditEx;
    DisableAllMarkersBtn: TBitBtn;
    CapMemo: TMemo;
    FaceMemo: TMemo;
    GroupBox5: TGroupBox;
    MaxValueSpe: TFloatSpinEditEx;
    MinValueSpe: TFloatSpinEditEx;
    PointerMemo: TMemo;
    MaxValLbl: TLabel;
    MinValLbl: TLabel;
    RangeLEDCallBackLED: TSuperLED;
    RangeLEDCallbackNameLbl: TLabel;
    RangeLEDCallbackNameValLbl: TLabel;
    ResetMinMaxBtn: TBitBtn;
    ScaleEnabledCb: TCheckBox;
    ScaleEnableMainTicksCb: TCheckBox;
    AuxScaleEnableMainTicksCb: TCheckBox;
    ScaleEnableSubTicksCb: TCheckBox;
    AuxScaleEnableSubTicksCb: TCheckBox;
    ScaleInnerTickArcThicknessLbl: TLabel;
    AuxScaleInnerTickArcThicknessLbl: TLabel;
    ScaleInnerTickArcThicknessSpe: TSpinEditEx;
    AuxScaleInnerTickArcThicknessSpe: TSpinEditEx;
    ScaleLenghtMainTickLbl: TLabel;
    AuxScaleLenghtMainTickLbl: TLabel;
    ScaleLenghtSubTickLbl: TLabel;
    AuxScaleLenghtSubTickLbl: TLabel;
    ScaleMainTickCountLbl: TLabel;
    AuxScaleMainTickCountLbl: TLabel;
    ScaleMainTickCountSpe: TSpinEditEx;
    AuxScaleMainTickCountSpe: TSpinEditEx;
    ScaleMainTickLengthSpe: TSpinEditEx;
    AuxScaleMainTickLengthSpe: TSpinEditEx;
    ScaleMainTickThicknessLbl: TLabel;
    AuxScaleMainTickThicknessLbl: TLabel;
    ScaleMainTickThicknessSpe: TSpinEditEx;
    AuxScaleMainTickThicknessSpe: TSpinEditEx;
    ScaleMainTickUseDotsCb: TCheckBox;
    AuxScaleMainTickUseDotsCb: TCheckBox;
    ScaleOuterTickArcThicknessLbl: TLabel;
    AuxScaleOuterTickArcThicknessLbl: TLabel;
    ScaleOuterTickArcThicknessSpe: TSpinEditEx;
    AuxScaleOuterTickArcThicknessSpe: TSpinEditEx;
    ScaleRadiusLbl: TLabel;
    ScaleTextSizeLbl: TLabel;
    ScaleRadiusLbol2: TLabel;
    AuxScaleRadiusLbl: TLabel;
    AuxScaleTextRadiusLbl: TLabel;
    AuxScaleTextSizeLbl: TLabel;
    ScaleRadiusSpe: TSpinEditEx;
    AuxScaleRadiusSpe: TSpinEditEx;
    ScaleReversedCb: TCheckBox;
    AuxScaleReversedCb: TCheckBox;
    ScaleSetFontBtn: TBitBtn;
    AuxScaleSetFontBtn: TBitBtn;
    ScaleStartLbl: TLabel;
    AuxScaleStartLbl: TLabel;
    ScaleStartSpe: TSpinEditEx;
    AuxScaleStartSpe: TSpinEditEx;
    ScaleStepLbl: TLabel;
    AuxScaleStepLbl: TLabel;
    ScaleStepSpe: TSpinEditEx;
    AuxScaleStepSpe: TSpinEditEx;
    ScaleSubTickCountLbl: TLabel;
    AuxScaleSubTickCountLbl: TLabel;
    ScaleSubTickCountSpe: TSpinEditEx;
    AuxScaleSubTickCountSpe: TSpinEditEx;
    ScaleSubTickLengthSpe: TSpinEditEx;
    AuxScaleSubTickLengthSpe: TSpinEditEx;
    ScaleSubTickThicknessLbl: TLabel;
    AuxScaleSubTickThicknessLbl: TLabel;
    ScaleSubTickThicknessSpe: TSpinEditEx;
    AuxScaleSubTickThicknessSpe: TSpinEditEx;
    ScaleSubTickUseDotsCb: TCheckBox;
    AuxScaleSubTickUseDotsCb: TCheckBox;
    ScaleTextColorColorCb: TColorBox;
    AuxScaleTextColorColorCb: TColorBox;
    ScaleTextColorLbl: TLabel;
    AuxScaleTextColorLbl: TLabel;
    ScaleTextRadiusSpe: TSpinEditEx;
    AuxScaleTextRadiusSpe: TSpinEditEx;
    ScaleTextSizeSpe: TSpinEditEx;
    AuxScaleTextSizeSpe: TSpinEditEx;
    ScaleTextStyleDDCb: TCheckComboBox;
    AuxScaleTextStyleDDCb: TCheckComboBox;
    ScaleTextStyleLbl: TLabel;
    AuxScaleTextStyleLbl: TLabel;
    ScaleTickArcColorColorCb: TColorBox;
    AuxScaleTickArcColorColorCb: TColorBox;
    ScaleTickArcColorLbl: TLabel;
    AuxScaleTickArcColorLbl: TLabel;
    ScaleTickArcStyleCb: TComboBox;
    AuxScaleTickArcStyleCb: TComboBox;
    ScaleTickColorColorCb: TColorBox;
    AuxScaleTickColorColorCb: TColorBox;
    ScaleTickColorLbl: TLabel;
    AuxScaleTickColorLbl: TLabel;
    SuperGauge: TSuperGauge;
    EnableAllMarkersBtn: TBitBtn;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    AboutSubMenu: TMenuItem;
    ExitSubMenu: TMenuItem;
    Separator1: TMenuItem;
    TestLedShape: TBGRAShape;
    TickArcStyleLbl: TLabel;
    AuxTickArcStyleLbl: TLabel;
    TryToRoundValueCb: TCheckBox;
    DispMaxLbl: TLabel;
    DispMaxValLbl: TLabel;
    RoundBtn: TButton;
    Label3: TLabel;
    DispMinLbl: TLabel;
    DispMinValLbl: TLabel;
    uELED1: TSuperLED;
    uELED2: TSuperLED;
    ValuePlus1Btn: TBitBtn;
    ValueMinus1Btn: TBitBtn;
    ValuePlus10Btn: TBitBtn;
    ValueMinus10Btn: TBitBtn;
    ValueZeroBtn: TBitBtn;
    UserToGaugeLbl: TLabel;
    MinValueBtn: TBitBtn;
    MaxValueBtn: TBitBtn;
    GaugeLbl: TLabel;
    GaugeValLbl: TLabel;
    GroupBox4: TGroupBox;
    KnobValLbl: TLabel;
    Label1: TLabel;
    Label6: TLabel;
    MarkerZeroAllBtn: TBitBtn;
    Marker1ValueClrBtn: TBitBtn;
    Marker3ValueClrBtn: TBitBtn;
    Marker2ValueClrBtn: TBitBtn;
    MarkerMemo: TMemo;
    MarkerRandomTestBtn: TBitBtn;
    Marker2ColorLbl: TLabel;
    Marker3ColorLbl: TLabel;
    Marker2HeightLbl: TLabel;
    Marker3HeightLbl: TLabel;
    Marker2WidthLbl: TLabel;
    Marker3WidthLbl: TLabel;
    LoadImageBtn: TBitBtn;
    FrameOuterFrameColorCb: TColorBox;
    FrameOuterFrameColorLbl: TLabel;
    FaceOuterColorCb: TColorBox;
    FaceOuterColorLbl: TLabel;
    FaceInnerColorCb: TColorBox;
    FaceInnerColorLbl: TLabel;
    FaceFillStyleLbl: TLabel;
    BorderWidthLbl: TLabel;
    FrameOuterFrameThicknessSpe: TSpinEditEx;
    Marker2ColorCb: TColorBox;
    Marker3ColorCb: TColorBox;
    Marker2EnabledCb: TCheckBox;
    Marker3EnabledCb: TCheckBox;
    Marker1Gb2: TGroupBox;
    Marker1Gb3: TGroupBox;
    Marker1HeightSpe: TSpinEditEx;
    Marker2HeightSpe: TSpinEditEx;
    Marker3HeightSpe: TSpinEditEx;
    Marker1RadiusLbl: TLabel;
    Marker2RadiusLbl: TLabel;
    Marker3RadiusLbl: TLabel;
    Marker1RadiusSpe: TSpinEditEx;
    Marker1ColorCb: TColorBox;
    Marker1ColorLbl: TLabel;
    Marker1EnabledCb: TCheckBox;
    Marker1Gb1: TGroupBox;
    Marker1HeightLbl: TLabel;
    Marker1WidthLbl: TLabel;
    FrameTab: TTabSheet;
    Marker2RadiusSpe: TSpinEditEx;
    Marker3RadiusSpe: TSpinEditEx;
    Marker1StyleCb: TComboBox;
    Marker2StyleCb: TComboBox;
    Marker3StyleCb: TComboBox;
    Marker1StyleLbl: TLabel;
    Marker2StyleLbl: TLabel;
    Marker3StyleLbl: TLabel;
    Marker1ValueLbl: TLabel;
    Marker2ValueLbl: TLabel;
    Marker3ValueLbl: TLabel;
    Marker2ValueSpe: TFloatSpinEditEx;
    Marker3ValueSpe: TFloatSpinEditEx;
    Marker1WidthSpe: TSpinEditEx;
    FaceFillStyleCb: TComboBox;
    FacePictureOffsetXLbl: TLabel;
    FacePictureOffsetXSpe: TSpinEditEx;
    FacePictureOffsetYLbl: TLabel;
    FacePictureOffsetYSpe: TSpinEditEx;
    FacePictureResetOffsetBtn: TBitBtn;
    Marker2WidthSpe: TSpinEditEx;
    Marker3WidthSpe: TSpinEditEx;
    OutOfRangeLbl: TLabel;
    OverloadNegBtn: TBitBtn;
    OverloadPosBtn: TBitBtn;
    PerfTestBtn: TBitBtn;
    RandomBtn: TBitBtn;
    OpenPictureDialog1: TOpenPictureDialog;
    Marker1ValueSpe: TFloatSpinEditEx;
    TabSheet1: TTabSheet;
    FacePictureEnabledCb: TCheckBox;
    Band3BandRadiusLbl: TLabel;
    CapEdgeColorCb: TColorBox;
    CapFillColorCb: TColorBox;
    CapEdgeColorLbl: TLabel;
    CapFillColorLbl: TLabel;
    CapPositionLbl: TLabel;
    CapStyleLbl: TLabel;
    RangeLEDStyleCb: TComboBox;
    CapCurveExponentLbl: TLabel;
    CapCurveExponentSpe: TFloatSpinEditEx;
    CapPositionCb: TComboBox;
    CapStyleCb: TComboBox;
    Text2ColorCb: TColorBox;
    Text3ColorCb: TColorBox;
    Text2EnabledCb: TCheckBox;
    Text3EnabledCb: TCheckBox;
    Text1OffsetXLbl: TLabel;
    Text2OffsetXLbl: TLabel;
    Text3OffsetXLbl: TLabel;
    Text1OffsetXSpe: TSpinEditEx;
    Text2OffsetXSpe: TSpinEditEx;
    CapLightIntensityLbl: TLabel;
    CapRadiusLbl: TLabel;
    Text3OffsetXSpe: TSpinEditEx;
    Text1OffsetYLbl: TLabel;
    Text2OffsetYLbl: TLabel;
    CapLightIntensitySpe: TSpinEditEx;
    CapRadiusSpe: TSpinEditEx;
    Text3OffsetYLbl: TLabel;
    Text1OffsetYSpe: TSpinEditEx;
    Text2OffsetYSpe: TSpinEditEx;
    Text3OffsetYSpe: TSpinEditEx;
    Text2SetFontBtn: TBitBtn;
    Text3SetFontBtn: TBitBtn;
    Text2TextColorLbl: TLabel;
    Text3TextColorLbl: TLabel;
    Text1TextEdt: TEdit;
    Text2TextEdt: TEdit;
    Text3TextEdt: TEdit;
    Text1TextLbl: TLabel;
    Text1ColorCb: TColorBox;
    Text1TextColorLbl: TLabel;
    Text2TextLbl: TLabel;
    Text3TextLbl: TLabel;
    Text1TextSizeLbl: TLabel;
    Text1TextSizeLbl1: TLabel;
    Text3TextSizeLbl: TLabel;
    Text1TextSizeSpe: TSpinEditEx;
    Text2TextSizeSpe: TSpinEditEx;
    Text3TextSizeSpe: TSpinEditEx;
    Text1TextStyleDDCb: TCheckComboBox;
    Text2TextStyleDDCb: TCheckComboBox;
    Text3TextStyleDDCb: TCheckComboBox;
    Text1TextStyleLbl: TLabel;
    Text1SetFontBtn1: TBitBtn;

    BandRadiusLbl: TLabel;
    Band4BandRadiusLbl: TLabel;
    Band3BandRadiusSpe: TSpinEditEx;
    Band4BandRadiusSpe: TSpinEditEx;
    Band3ColorCb: TColorBox;
    Band4ColorCb: TColorBox;
    Band3ColorLbl: TLabel;
    Band4ColorLbl: TLabel;
    Band3EnabledCb: TCheckBox;
    Band4EnabledCb: TCheckBox;
    Band3EnabledTextCb: TCheckBox;
    Band4EnabledTextCb: TCheckBox;
    Band3EndValueSpe: TFloatSpinEditEx;
    Band4EndValueSpe: TFloatSpinEditEx;
    Band4Gb: TGroupBox;
    Band4RangeEndLbl: TLabel;
    Band4SetFontBtn: TBitBtn;
    Band4StartLbl: TLabel;
    Band4StartValueSpe: TFloatSpinEditEx;
    Band4TextColorCb: TColorBox;
    Band4TextColorLbl: TLabel;
    Band4TextEdt: TEdit;
    Band4TextLbl: TLabel;
    Band4TextRadiusLbl: TLabel;
    Band4TextRadiusSpe: TSpinEditEx;
    Band4TextSizeLbl: TLabel;
    Band4TextSizeSpe: TSpinEditEx;
    Band4TextStyleDDCb: TCheckComboBox;
    Band4TextStyleLbl: TLabel;
    Band4WidthLbl: TLabel;
    Band4ThicknessSpe: TSpinEditEx;
    Band3Gb: TGroupBox;
    Band3RangeEndLbl: TLabel;
    Band3SetFontBtn: TBitBtn;
    Band3StartLbl: TLabel;
    Band3StartValueSpe: TFloatSpinEditEx;
    Band3TextColorCb: TColorBox;
    Band3TextColorLbl: TLabel;
    Band3TextEdt: TEdit;
    Band3TextLbl: TLabel;
    Band3TextRadiusLbl: TLabel;
    Band3TextRadiusSpe: TSpinEditEx;
    Band3TextSizeLbl: TLabel;
    Band3TextSizeSpe: TSpinEditEx;
    Band3TextStyleDDCb: TCheckComboBox;
    Band1TextStyleLbl1: TLabel;
    Band3WidthLbl: TLabel;
    Band3ThicknessSpe: TSpinEditEx;
    Band2BandRadiusLbl: TLabel;
    Band2BandRadiusSpe: TSpinEditEx;
    Band2ColorCb: TColorBox;
    Band2ColorLbl: TLabel;
    Band2EnabledCb: TCheckBox;
    Band2EnabledTextCb: TCheckBox;
    Band2EndValueSpe: TFloatSpinEditEx;
    Band2Gb: TGroupBox;
    Band2RangeEndLbl: TLabel;
    Band2SetFontBtn: TBitBtn;
    BandStartLbl: TLabel;
    Band2StartValueSpe: TFloatSpinEditEx;
    Band2TextColorCb: TColorBox;
    Band2TextColorLbl: TLabel;
    Band2TextEdt: TEdit;
    Band2TextLbl: TLabel;
    Band1TextRadiusLbl: TLabel;
    Band2TextRadiusLbl: TLabel;
    Band1TextRadiusSpe: TSpinEditEx;
    Band1TextColorCb: TColorBox;
    Band1TextColorLbl: TLabel;
    Band1EnabledCb: TCheckBox;
    Band1EnabledTextCb: TCheckBox;
    Band1SetFontBtn: TBitBtn;
    Band2TextRadiusSpe: TSpinEditEx;
    Band2TextSizeLbl: TLabel;
    Band2TextSizeSpe: TSpinEditEx;
    Band2TextStyleDDCb: TCheckComboBox;
    Band2TextStyleLbl: TLabel;
    Band2ThicknessLbl: TLabel;
    Band2ThicknessSpe: TSpinEditEx;
    Band1Gb: TGroupBox;
    Band1ColorLbl: TLabel;
    Band1ColorCb: TColorBox;
    Band1TextLbl: TLabel;
    Band1TextEdt: TEdit;
    Band1TextStyleDDCb: TCheckComboBox;
    FontDialog1: TFontDialog;
    Band1TextSizeLbl: TLabel;
    Band1ThicknessLbl: TLabel;
    Band1TextStyleLbl: TLabel;
    Band1BandRadiusLbl: TLabel;
    Text1EnabledCb: TCheckBox;
    Text2TextStyleLbl: TLabel;
    Text3TextStyleLbl: TLabel;
    TextGb1: TGroupBox;
    ImageList1: TBGRAImageList;
    RangeLEDOffsetXLbl1: TLabel;
    Band1BandRadiusSpe: TSpinEditEx;
    Band1TextSizeSpe: TSpinEditEx;
    Band1ThicknessSpe: TSpinEditEx;
    Band1RangeEndLbl: TLabel;
    Band1EndValueSpe: TFloatSpinEditEx;
    Band1StartLbl: TLabel;
    Band1StartValueSpe: TFloatSpinEditEx;
    RangeLEDSizeSpe: TSpinEditEx;
    ShapeLbl: TLabel;
    RangeLEDRangeEnd: TLabel;
    RangeLEDRangeStartSpe: TFloatSpinEditEx;
    RangeLEDRangeEndSpe: TFloatSpinEditEx;
    RangeLEDRangeTypeLbl: TLabel;
    RangeLEDRangeStartLbl: TLabel;
    RangeLEDRangeTypeCb: TComboBox;
    RangeLEDOffsetYLbl: TLabel;
    RangeLEDOffsetXLbl: TLabel;
    RangeLEDActiveColorCb: TColorBox;
    RangeLEDInactiveColorCb: TColorBox;
    RangeLEDBorderColorCb: TColorBox;
    ColorDialog1: TColorDialog;
    Label2: TLabel;
    FillStyleLbl: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    RangeLEDOffsetXSpe: TSpinEditEx;
    RangeLEDOffsetYSpe: TSpinEditEx;
    RangeLEDResetOffsetBtn: TBitBtn;
    RangeLEDResetRangesBtn: TBitBtn;
    RangeLEDShapeCb: TComboBox;
    HeightLbl: TLabel;
    Text2Gb: TGroupBox;
    Text3Gb: TGroupBox;
    Timer2: TTimer;
    TopLbl: TLabel;
    TopValLbl: TLabel;
    LeftLbl: TLabel;
    WidthHeightAddBtn: TBitBtn;
    WidthHeightSubBtn: TBitBtn;
    WidthValLbl: TLabel;
    WidthLbl: TLabel;
    LeftAddBtn: TBitBtn;
    LeftSubBtn: TBitBtn;
    TopAddBtn: TBitBtn;
    TopSubBtn: TBitBtn;
    HeightAddBtn: TBitBtn;
    HeightSubBtn: TBitBtn;
    ResetSizeBtn: TBitBtn;
    GroupBox2: TGroupBox;
    ResetPositionBtn: TBitBtn;
    WidthSubBtn: TBitBtn;
    WidthAddBtn: TBitBtn;
    GroupBox1: TGroupBox;
    GaugeTs: TPageControl;
    BasicTab: TTabSheet;
    LEDTab: TTabSheet;
    FaceTab: TTabSheet;
    ScaleTab: TTabSheet;
    PointerTab: TTabSheet;
    CapTab: TTabSheet;
    BandTab: TTabSheet;
    TextTab: TTabSheet;
    Timer1: TTimer;
    HeightValLbl: TLabel;
    LeftValLbl: TLabel;

    procedure AboutSubMenuClick(Sender: TObject);
    procedure AutoScaleCbChange(Sender: TObject);
    procedure AuxEnableScaleTextCbChange(Sender: TObject);
    procedure AuxMaxValueSpeChange(Sender: TObject);
    procedure AuxMinValueSpeChange(Sender: TObject);
    procedure AuxPointerColorCbChange(Sender: TObject);
    procedure AuxPointerEnabledCbChange(Sender: TObject);
    procedure AuxPointerExtensionLengthSpeChange(Sender: TObject);
    procedure AuxPointerHighlightCbChange(Sender: TObject);
    procedure AuxPointerLengthSpeChange(Sender: TObject);
    procedure AuxPointerStyleCbChange(Sender: TObject);
    procedure AuxPointerWidthSpeChange(Sender: TObject);
    procedure AuxScaleEnableMainTicksCbChange(Sender: TObject);
    procedure AuxScaleEnableSubTicksCbChange(Sender: TObject);
    procedure AuxScaleInnerTickArcThicknessSpeChange(Sender: TObject);
    procedure AuxScaleMainTickCountSpeChange(Sender: TObject);
    procedure AuxScaleMainTickLengthSpeChange(Sender: TObject);
    procedure AuxScaleMainTickThicknessSpeChange(Sender: TObject);
    procedure AuxScaleMainTickUseDotsCbChange(Sender: TObject);
    procedure AuxScaleOuterTickArcThicknessSpeChange(Sender: TObject);
    procedure AuxScaleRadiusSpeChange(Sender: TObject);
    procedure AuxScaleReversedCbChange(Sender: TObject);
    procedure AuxScaleSetFontBtnClick(Sender: TObject);
    procedure AuxScaleStartSpeChange(Sender: TObject);
    procedure AuxScaleStepSpeChange(Sender: TObject);
    procedure AuxScaleSubTickCountSpeChange(Sender: TObject);
    procedure AuxScaleSubTickLengthSpeChange(Sender: TObject);
    procedure AuxScaleSubTickThicknessSpeChange(Sender: TObject);
    procedure AuxScaleSubTickUseDotsCbChange(Sender: TObject);
    procedure AuxScaleTextColorColorCbChange(Sender: TObject);
    procedure AuxScaleTextRadiusSpeChange(Sender: TObject);
    procedure AuxScaleTextSizeSpeChange(Sender: TObject);
    procedure AuxScaleTextStyleDDCbItemChange(Sender: TObject; AIndex: Integer);
    procedure AuxScaleTickArcColorColorCbChange(Sender: TObject);
    procedure AuxScaleTickArcStyleCbChange(Sender: TObject);
    procedure AuxScaleTickColorColorCbChange(Sender: TObject);
    procedure BackgroundColorCbChange(Sender: TObject);
    procedure Band1ColorCbChange(Sender: TObject);
    procedure Band1EndValueSpeChange(Sender: TObject);
    procedure Band1SetFontBtnClick(Sender: TObject);
    procedure Band1StartValueSpeChange(Sender: TObject);
    procedure Band1TextColorCbChange(Sender: TObject);
    procedure Band1TextEdtChange(Sender: TObject);
    procedure Band1EnabledTextCbChange(Sender: TObject);
    procedure Band1BandRadiusSpeChange(Sender: TObject);
    procedure Band1TextRadiusSpeChange(Sender: TObject);
    procedure Band1TextSizeSpeChange(Sender: TObject);
    procedure Band1TextStyleDDCbItemChange(Sender: TObject; AIndex: Integer);
    procedure Band2BandRadiusSpeChange(Sender: TObject);
    procedure Band2EnabledCbChange(Sender: TObject);
    procedure Band2EnabledTextCbChange(Sender: TObject);
    procedure Band2EndValueSpeChange(Sender: TObject);
    procedure Band2SetFontBtnClick(Sender: TObject);
    procedure Band2StartValueSpeChange(Sender: TObject);
    procedure Band2TextColorCbChange(Sender: TObject);
    procedure Band2TextEdtChange(Sender: TObject);
    procedure Band2TextRadiusSpeChange(Sender: TObject);
    procedure Band2TextSizeSpeChange(Sender: TObject);
    procedure Band2TextStyleDDCbItemChange(Sender: TObject; AIndex: Integer);
    procedure Band2ThicknessSpeChange(Sender: TObject);
    procedure Band2ColorCbChange(Sender: TObject);
    procedure Band3BandRadiusSpeChange(Sender: TObject);
    procedure Band3ColorCbChange(Sender: TObject);
    procedure Band3EnabledCbChange(Sender: TObject);
    procedure Band3EnabledTextCbChange(Sender: TObject);
    procedure Band3EndValueSpeChange(Sender: TObject);
    procedure Band3SetFontBtnClick(Sender: TObject);
    procedure Band3StartValueSpeChange(Sender: TObject);
    procedure Band3TextColorCbChange(Sender: TObject);
    procedure Band3TextEdtChange(Sender: TObject);
    procedure Band3TextRadiusSpeChange(Sender: TObject);
    procedure Band3TextSizeSpeChange(Sender: TObject);
    procedure Band3TextStyleDDCbItemChange(Sender: TObject; AIndex: Integer);
    procedure Band3ThicknessSpeChange(Sender: TObject);
    procedure Band4BandRadiusSpeChange(Sender: TObject);
    procedure Band4ColorCbChange(Sender: TObject);
    procedure Band4EnabledCbChange(Sender: TObject);
    procedure Band4EnabledTextCbChange(Sender: TObject);
    procedure Band4EndValueSpeChange(Sender: TObject);
    procedure Band4SetFontBtnClick(Sender: TObject);
    procedure Band4StartValueSpeChange(Sender: TObject);
    procedure Band4TextColorCbChange(Sender: TObject);
    procedure Band4TextEdtChange(Sender: TObject);
    procedure Band4TextRadiusSpeChange(Sender: TObject);
    procedure Band4TextSizeSpeChange(Sender: TObject);
    procedure Band4TextStyleDDCbItemChange(Sender: TObject; AIndex: Integer);
    procedure Band4ThicknessSpeChange(Sender: TObject);
    procedure BGRAKnobAuxDblClick(Sender: TObject);
    procedure BGRAKnobAuxValueChanged(Sender: TObject; Value: single);
    procedure BGRAKnobDblClick(Sender: TObject);
    procedure BGRAKnobValueChanged(Sender: TObject; Value: single);
    procedure BandEnabledCbChange(Sender: TObject);
    procedure CapEdgeWidthSpeChange(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FrameInnerFrameColorCbChange(Sender: TObject);
    procedure FrameInnerFrameThicknessSpeChange(Sender: TObject);
    procedure FrameMiddleFrameColorCbChange(Sender: TObject);
    procedure FrameMiddleFrameThicknessSpeChange(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure PointerEnabledCbChange(Sender: TObject);
    procedure PointerHighlightCbChange(Sender: TObject);
    procedure AuxPointerHighlightColorCbChange(Sender: TObject);
    procedure PointerHighlightColorCbChange(Sender: TObject);
    procedure AuxPointerHighlightThicknessSpeChange(Sender: TObject);
    procedure PointerHighlightThicknessSpeChange(Sender: TObject);
    procedure AuxScaleEnabledCbChange(Sender: TObject);
    procedure RangeLEDActiveCbChange(Sender: TObject);
    procedure ScaleEnabledCbChange(Sender: TObject);
    procedure ScaleMainTickUseDotsCbChange(Sender: TObject);
    procedure ScaleSubTickUseDotsCbChange(Sender: TObject);
    procedure DisableAllMarkersBtnClick(Sender: TObject);
    procedure EnableAllMarkersBtnClick(Sender: TObject);
    procedure ExitSubMenuClick(Sender: TObject);
    procedure FaceCurveExponentSpeChange(Sender: TObject);
    procedure FaceLightIntensitySpeChange(Sender: TObject);
    procedure RoundBtnClick(Sender: TObject);
    procedure LoadImageBtnClick(Sender: TObject);
    procedure MarkerZeroAllBtnClick(Sender: TObject);
    procedure Marker1ValueClrBtnClick(Sender: TObject);
    procedure Marker3ValueClrBtnClick(Sender: TObject);
    procedure Marker2ValueClrBtnClick(Sender: TObject);
    procedure MaxValueBtnClick(Sender: TObject);
    procedure MaxValueSpeChange(Sender: TObject);
    procedure MinValueBtnClick(Sender: TObject);
    procedure MinValueSpeChange(Sender: TObject);
    procedure RandomBtnClick(Sender: TObject);
    procedure MarkerRandomTestBtnClick(Sender: TObject);
    procedure CapCurveExponentSpeChange(Sender: TObject);
    procedure CapEdgeColorCbChange(Sender: TObject);
    procedure CapFillColorCbChange(Sender: TObject);
    procedure CapLightIntensitySpeChange(Sender: TObject);
    procedure CapPositionCbChange(Sender: TObject);
    procedure CapRadiusSpeChange(Sender: TObject);
    procedure CapStyleCbChange(Sender: TObject);
    procedure EnableScaleTextCbChange(Sender: TObject);
    procedure FaceInnerColorCbChange(Sender: TObject);
    procedure FaceOuterColorCbChange(Sender: TObject);
    procedure FaceFillStyleCbChange(Sender: TObject);
    procedure FacePictureEnabledCbChange(Sender: TObject);
    procedure FacePictureOffsetXSpeChange(Sender: TObject);
    procedure FacePictureOffsetYSpeChange(Sender: TObject);
    procedure FrameOuterFrameColorCbChange(Sender: TObject);
    procedure FrameOuterFrameThicknessSpeChange(Sender: TObject);
    procedure Marker1ColorCbChange(Sender: TObject);
    procedure Marker1EnabledCbChange(Sender: TObject);
    procedure Marker1HeightSpeChange(Sender: TObject);
    procedure Marker1RadiusSpeChange(Sender: TObject);
    procedure Marker1StyleCbChange(Sender: TObject);
    procedure Marker1ValueSpeChange(Sender: TObject);
    procedure Marker1WidthSpeChange(Sender: TObject);
    procedure Marker2ColorCbChange(Sender: TObject);
    procedure Marker2EnabledCbChange(Sender: TObject);
    procedure Marker2HeightSpeChange(Sender: TObject);
    procedure Marker2RadiusSpeChange(Sender: TObject);
    procedure Marker2StyleCbChange(Sender: TObject);
    procedure Marker2ValueSpeChange(Sender: TObject);
    procedure Marker2WidthSpeChange(Sender: TObject);
    procedure Marker3ColorCbChange(Sender: TObject);
    procedure Marker3EnabledCbChange(Sender: TObject);
    procedure Marker3HeightSpeChange(Sender: TObject);
    procedure Marker3RadiusSpeChange(Sender: TObject);
    procedure Marker3StyleCbChange(Sender: TObject);
    procedure Marker3ValueSpeChange(Sender: TObject);
    procedure Marker3WidthSpeChange(Sender: TObject);
    procedure PointerColorCbChange(Sender: TObject);
    procedure PointerExtensionLengthSpeChange(Sender: TObject);
    procedure PointerLengthSpeChange(Sender: TObject);
    procedure PointerStyleCbChange(Sender: TObject);
    procedure PointerWidthSpeChange(Sender: TObject);
    procedure FacePictureResetOffsetBtnClick(Sender: TObject);
    procedure ResetMinMaxBtnClick(Sender: TObject);
    procedure ScaleEnableMainTicksCbChange(Sender: TObject);
    procedure ScaleEnableSubTicksCbChange(Sender: TObject);
    procedure ScaleInnerTickArcThicknessSpeChange(Sender: TObject);
    procedure ScaleOuterTickArcThicknessSpeChange(Sender: TObject);
    procedure ScaleSubTickLengthSpeChange(Sender: TObject);
    procedure ScaleMainTickLengthSpeChange(Sender: TObject);
    procedure ScaleMainTickCountSpeChange(Sender: TObject);
    procedure ScaleMainTickThicknessSpeChange(Sender: TObject);
    procedure ScaleReversedCbChange(Sender: TObject);
    procedure ScaleStartSpeChange(Sender: TObject);
    procedure ScaleRadiusSpeChange(Sender: TObject);
    procedure ScaleSetFontBtnClick(Sender: TObject);
    procedure ScaleStepSpeChange(Sender: TObject);
    procedure ScaleSubTickCountSpeChange(Sender: TObject);
    procedure ScaleSubTickThicknessSpeChange(Sender: TObject);
    procedure ScaleTextColorColorCbChange(Sender: TObject);
    procedure ScaleTextRadiusSpeChange(Sender: TObject);
    procedure ScaleTextSizeSpeChange(Sender: TObject);
    procedure ScaleTextStyleDDCbItemChange(Sender: TObject; AIndex: Integer);
    procedure ScaleTickArcColorColorCbChange(Sender: TObject);
    procedure ScaleTickArcStyleCbChange(Sender: TObject);
    procedure ScaleTickColorColorCbChange(Sender: TObject);
    procedure SuperGaugeAuxOverloadRecovered(Sender: TObject; RangeValue: single
      );
    procedure SuperGaugeAuxOverloadTriggered(Sender: TObject;
      OutOfRangeValue: single);
    procedure SuperGaugeOverloadRecovered(Sender: TObject; RangeValue: single);
    procedure SuperGaugeOverloadTriggered(Sender: TObject;
      OutOfRangeValue: single);
    procedure Text1EnabledCbChange(Sender: TObject);
    procedure SuperGaugeRangeLEDActive(Sender: TObject; Value: single);
    procedure SuperGaugeRangeLEDInactive(Sender: TObject; Value: single);
    procedure RangeLEDActiveColorCbChange(Sender: TObject);
    procedure RangeLEDBorderColorCbChange(Sender: TObject);
    procedure Band1ThicknessSpeChange(Sender: TObject);
    procedure RangeLEDRangeTypeCbChange(Sender: TObject);
    procedure RangeLEDInactiveColorCbChange(Sender: TObject);
    procedure RangeLEDRangeEndSpeChange(Sender: TObject);
    procedure RangeLEDRangeStartSpeChange(Sender: TObject);
    procedure RangeLEDShapeCbChange(Sender: TObject);
    procedure RangeLEDSizeSpeChange(Sender: TObject);
    procedure RangeLEDStyleCbChange(Sender: TObject);
    procedure LeftAddBtnClick(Sender: TObject);
    procedure HeightSubBtnClick(Sender: TObject);
    procedure HeightAddBtnClick(Sender: TObject);
    procedure LeftSubBtnClick(Sender: TObject);
    procedure RangeLEDOffsetXSpeChange(Sender: TObject);
    procedure RangeLEDOffsetYSpeChange(Sender: TObject);
    procedure ResetPositionBtnClick(Sender: TObject);
    procedure RangeLEDResetOffsetBtnClick(Sender: TObject);
    procedure RangeLEDResetRangesBtnClick(Sender: TObject);
    procedure ResetSizeBtnClick(Sender: TObject);
    procedure Text1OffsetXSpeChange(Sender: TObject);
    procedure Text1OffsetYSpeChange(Sender: TObject);
    procedure Text1SetFontBtn1Click(Sender: TObject);
    procedure Text1ColorCbChange(Sender: TObject);
    procedure Text1TextEdtChange(Sender: TObject);
    procedure Text1TextSizeSpeChange(Sender: TObject);
    procedure Text1TextStyleDDCbItemChange(Sender: TObject; AIndex: Integer);
    procedure Text2ColorCbChange(Sender: TObject);
    procedure Text2EnabledCbChange(Sender: TObject);
    procedure Text2OffsetXSpeChange(Sender: TObject);
    procedure Text2OffsetYSpeChange(Sender: TObject);
    procedure Text2SetFontBtnClick(Sender: TObject);
    procedure Text2TextEdtChange(Sender: TObject);
    procedure Text2TextSizeSpeChange(Sender: TObject);
    procedure Text2TextStyleDDCbItemChange(Sender: TObject; AIndex: Integer);
    procedure Text3ColorCbChange(Sender: TObject);
    procedure Text3EnabledCbChange(Sender: TObject);
    procedure Text3OffsetXSpeChange(Sender: TObject);
    procedure Text3OffsetYSpeChange(Sender: TObject);
    procedure Text3SetFontBtnClick(Sender: TObject);
    procedure Text3TextEdtChange(Sender: TObject);
    procedure Text3TextSizeSpeChange(Sender: TObject);
    procedure Text3TextStyleDDCbItemChange(Sender: TObject; AIndex: Integer);
    procedure Timer2Timer(Sender: TObject);
    procedure TopAddBtnClick(Sender: TObject);
    procedure TopSubBtnClick(Sender: TObject);
    procedure ValueMinus10BtnClick(Sender: TObject);
    procedure ValueMinus1BtnClick(Sender: TObject);
    procedure ValuePlus10BtnClick(Sender: TObject);
    procedure ValuePlus1BtnClick(Sender: TObject);
    procedure ValueZeroBtnClick(Sender: TObject);
    procedure WidthAddBtnClick(Sender: TObject);
    procedure PerfTestBtnClick(Sender: TObject);
    procedure OverloadPosBtnClick(Sender: TObject);
    procedure OverloadNegBtnClick(Sender: TObject);
    procedure SuperGaugeDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure WidthHeightAddBtnClick(Sender: TObject);
    procedure WidthHeightSubBtnClick(Sender: TObject);
    procedure WidthSubBtnClick(Sender: TObject);
  private
    FSavedGauge: TSuperGauge;

  public
    TimerState: boolean;
    GaugeRangeError : boolean;
    FrameTest : TSGFrameSettings;
    ResetTics : integer;
    ResetLoTics : integer;
    ResetHiTics : integer;
    procedure UpdateBasicStats;
    procedure UpdateWHStats;
    procedure UpdateMinMaxStats;
    procedure UpdateLTStats;
    procedure UpdatePositionStats;
    procedure UpdateRangeLEDStats;
    procedure UpdateBandStats;
    procedure UpdateTextStats;
    procedure UpdateCapStats;
    procedure UpdatePointerStats;
    procedure UpdateAuxPointerStats;
    procedure UpdateScaleStats;
    procedure UpdateAuxScaleStats;
    procedure UpdateMarkerStats;
    procedure UpdateFaceStats;
    procedure UpdateFrameStats;
    procedure UpdateAllStats;

    procedure GaugeDefaults;

  end;

var
  SGTestFrm: TSGTestFrm;

implementation

{$R *.lfm}

{ TSGTestFrm }

procedure TSGTestFrm.FormCreate(Sender: TObject);
begin
      Caption := 'Super Gauge Test Application ' + VERSIONSTR;
      TimerState := false;
      GaugeRangeError := false;
      ResetTics := 0;
      ResetLoTics := 0;
      ResetHiTics := 0;

      UpdateAllStats;

      // Create a SuperGauge to have defaults, clean up in form destroy

      FSavedGauge := TSuperGauge.Create(nil);
end;

procedure TSGTestFrm.FormDestroy(Sender: TObject);
begin
  FSavedGauge.Free;
end;


procedure TSGTestFrm.GaugeDefaults;
begin
  // Reset anything from the saved default gauge back to the displayed gauge
  // Might not include size and positions and auto scale...
  // Lot's O' stuff here to do the reset

  with SuperGauge do
  begin

    // Basic Settings

    Color := FSavedGauge.Color;
    MinValue := FSavedGauge.MinValue;
    MaxValue := FSavedGauge.MaxValue;
    AuxMinValue := FSavedGauge.AuxMinValue;
    AuxMaxValue := FSavedGauge.AuxMaxValue;

    // Frame Settings

    with FSavedGauge.FrameSettings do
    begin
      FrameSettings.OuterFrameColor := OuterFrameColor;
      FrameSettings.MiddleFrameColor := MiddleFrameColor;
      FrameSettings.InnerFrameColor := InnerFrameColor;
      FrameSettings.OuterFrameThickness := OuterFrameThickness;
      FrameSettings.MiddleFrameThickness := MiddleFrameThickness;
      FrameSettings.InnerFrameThickness := InnerFrameThickness;
    end;

    // Face Settings

    with FSavedGauge.FaceSettings do
    begin
      FaceSettings.FillStyle := FillStyle;
      FaceSettings.InnerColor := InnerColor;
      FaceSettings.OuterColor := OuterColor;
      FaceSettings.LightIntensity := LightIntensity;
      FaceSettings.CurveExponent := CurveExponent;
      FaceSettings.PictureEnabled := PictureEnabled;
      FaceSettings.PictureOffsetX := PictureOffsetX;
      FaceSettings.PictureOffsetY := PictureOffsetY;
    end;

    // Main Scale

    with FSavedGauge.ScaleSettings do
    begin
      ScaleSettings.Enabled := Enabled;
      ScaleSettings.EnableMainTicks := EnableMainTicks;
      ScaleSettings.EnableSubTicks := EnableSubTicks;
      ScaleSettings.MainTickUseDots := MainTickUseDots;
      ScaleSettings.SubTickUseDots := SubTickUseDots;
      ScaleSettings.EnableScaleText := EnableScaleText;
      ScaleSettings.ReverseScale := ReverseScale;
      ScaleSettings.Start := Start;
      ScaleSettings.Step := Step;
      ScaleSettings.MainTickCount := MainTickCount;
      ScaleSettings.SubTickCount := SubTickCount;
      ScaleSettings.MainTickLength := MainTickLength;
      ScaleSettings.SubTickLength := SubTickLength;
      ScaleSettings.MainTickThickness := MainTickThickness;
      ScaleSettings.SubTickThickness := SubTickThickness;
      ScaleSettings.TickColor := TickColor;
      ScaleSettings.ScaleRadius := ScaleRadius;
      ScaleSettings.TickArcStyle := TickArcStyle;
      ScaleSettings.InnerTickArcThickness := InnerTickArcThickness;
      ScaleSettings.OuterTickArcThickness := OuterTickArcThickness;
      ScaleSettings.TickArcColor := TickArcColor;
      ScaleSettings.TextRadius := TextRadius;
      ScaleSettings.TextSize := TextSize;
      ScaleSettings.TextFont := TextFont;
      ScaleSettings.TextColor := TextColor;
      ScaleSettings.TextStyle := TextStyle;
    end;

    // Aux Scale

    with FSavedGauge.AuxScaleSettings do
    begin
      AuxScaleSettings.Enabled := Enabled;
      AuxScaleSettings.EnableMainTicks := EnableMainTicks;
      AuxScaleSettings.EnableSubTicks := EnableSubTicks;
      AuxScaleSettings.MainTickUseDots := MainTickUseDots;
      AuxScaleSettings.SubTickUseDots := SubTickUseDots;
      AuxScaleSettings.EnableScaleText := EnableScaleText;
      AuxScaleSettings.ReverseScale := ReverseScale;
      AuxScaleSettings.Start := Start;
      AuxScaleSettings.Step := Step;
      AuxScaleSettings.MainTickCount := MainTickCount;
      AuxScaleSettings.SubTickCount := SubTickCount;
      AuxScaleSettings.MainTickLength := MainTickLength;
      AuxScaleSettings.SubTickLength := SubTickLength;
      AuxScaleSettings.MainTickThickness := MainTickThickness;
      AuxScaleSettings.SubTickThickness := SubTickThickness;
      AuxScaleSettings.TickColor := TickColor;
      AuxScaleSettings.ScaleRadius := ScaleRadius;
      AuxScaleSettings.TickArcStyle := TickArcStyle;
      AuxScaleSettings.InnerTickArcThickness := InnerTickArcThickness;
      AuxScaleSettings.OuterTickArcThickness := OuterTickArcThickness;
      AuxScaleSettings.TickArcColor := TickArcColor;
      AuxScaleSettings.TextRadius := TextRadius;
      AuxScaleSettings.TextSize := TextSize;
      AuxScaleSettings.TextFont := TextFont;
      AuxScaleSettings.TextColor := TextColor;
      AuxScaleSettings.TextStyle := TextStyle;
    end;

    // Main Pointer

    with FSavedGauge.PointerSettings do
    begin
      PointerSettings.Enabled := Enabled;
      PointerSettings.Color := Color;
      PointerSettings.Length := Length;
      PointerSettings.ExtensionLength := ExtensionLength;
      PointerSettings.Style := Style;
      PointerSettings.Width := Width;
      PointerSettings.HighlightLine := HighlightLine;
      PointerSettings.HighlightColor := HighlightColor;
      PointerSettings.HighlightThickness := HighlightThickness;
    end;

    // Aux Pointer

    with FSavedGauge.AuxPointerSettings do
    begin
      AuxPointerSettings.Enabled := Enabled;
      AuxPointerSettings.Color := Color;
      AuxPointerSettings.Length := Length;
      AuxPointerSettings.ExtensionLength := ExtensionLength;
      AuxPointerSettings.Style := Style;
      AuxPointerSettings.Width := Width;
      AuxPointerSettings.HighlightLine := HighlightLine;
      AuxPointerSettings.HighlightColor := HighlightColor;
      AuxPointerSettings.HighlightThickness := HighlightThickness;
    end;

    with FSavedGauge.PointerCapSettings do
    begin
      PointerCapSettings.CapPosition := CapPosition;
      PointerCapSettings.CapStyle := CapStyle;
      PointerCapSettings.EdgeColor := EdgeColor;
      PointerCapSettings.EdgeWidth := EdgeWidth;
      PointerCapSettings.FillColor := FillColor;
      PointerCapSettings.Radius := Radius;
      PointerCapSettings.LightIntensity := LightIntensity;
      PointerCapSettings.CurveExponent := CurveExponent;
    end;

    // Text 1,2,3

    with FSavedGauge.TextSettings1 do
    begin
      TextSettings1.Enabled := Enabled;
      TextSettings1.FontEx := FontEx;
      TextSettings1.OffsetX := OffsetX;
      TextSettings1.OffsetY := OffsetY;
      TextSettings1.Text := Text;
    end;

    with FSavedGauge.TextSettings2 do
    begin
      TextSettings2.Enabled := Enabled;
      TextSettings2.FontEx := FontEx;
      TextSettings2.OffsetX := OffsetX;
      TextSettings2.OffsetY := OffsetY;
      TextSettings2.Text := Text;
    end;

    with FSavedGauge.TextSettings3 do
    begin
      TextSettings3.Enabled := Enabled;
      TextSettings3.FontEx := FontEx;
      TextSettings3.OffsetX := OffsetX;
      TextSettings3.OffsetY := OffsetY;
      TextSettings3.Text := Text;
    end;

    // Bands 1,2,3,4

    with FSavedGauge.BandSettings1 do
    begin
      BandSettings1.Enabled := Enabled;
      BandSettings1.EnableText := EnableText;
      BandSettings1.BandColor := BandColor;
      BandSettings1.StartValue := StartValue;
      BandSettings1.EndValue := EndValue;
      BandSettings1.Text := Text;
      BandSettings1.TextStyle := TextStyle;
      BandSettings1.TextColor := TextColor;
      BandSettings1.TextSize := TextSize;
      BandSettings1.TextRadius := TextRadius;
    end;

    with FSavedGauge.BandSettings2 do
    begin
      BandSettings2.Enabled := Enabled;
      BandSettings2.EnableText := EnableText;
      BandSettings2.BandColor := BandColor;
      BandSettings2.StartValue := StartValue;
      BandSettings2.EndValue := EndValue;
      BandSettings2.Text := Text;
      BandSettings2.TextStyle := TextStyle;
      BandSettings2.TextColor := TextColor;
      BandSettings2.TextSize := TextSize;
      BandSettings2.TextRadius := TextRadius;
    end;

    with FSavedGauge.BandSettings3 do
    begin
      BandSettings3.Enabled := Enabled;
      BandSettings3.EnableText := EnableText;
      BandSettings3.BandColor := BandColor;
      BandSettings3.StartValue := StartValue;
      BandSettings3.EndValue := EndValue;
      BandSettings3.Text := Text;
      BandSettings3.TextStyle := TextStyle;
      BandSettings3.TextColor := TextColor;
      BandSettings3.TextSize := TextSize;
      BandSettings3.TextRadius := TextRadius;
    end;

    with FSavedGauge.BandSettings4 do
    begin
      BandSettings4.Enabled := Enabled;
      BandSettings4.EnableText := EnableText;
      BandSettings4.BandColor := BandColor;
      BandSettings4.StartValue := StartValue;
      BandSettings4.EndValue := EndValue;
      BandSettings4.Text := Text;
      BandSettings4.TextStyle := TextStyle;
      BandSettings4.TextColor := TextColor;
      BandSettings4.TextSize := TextSize;
      BandSettings4.TextRadius := TextRadius;
    end;

    // Range LED

    with FSavedGauge.RangeLedSettings do
    begin
      RangeLEDSettings.Active := Active;
      RangeLEDSettings.Style := Style;
      RangeLEDSettings.Shape := Shape;
      RangeLEDSettings.ActiveColor := ActiveColor;
      RangeLEDSettings.InactiveColor := InactiveColor;
      RangeLEDSettings.BorderColor := BorderColor;
      RangeLEDSettings.Size := Size;
      RangeLEDSettings.OffsetX := OffsetX;
      RangeLEDSettings.OffsetY := OffsetY;
      RangeLEDSettings.RangeType := RangeType;
      RangeLEDSettings.RangeStartValue := RangeStartValue;
      RangeLEDSettings.RangeEndValue := RangeEndValue;
    end;

    // Markers 1,2,3

    with FSavedGauge.MarkerSettings1 do
    begin
      MarkerSettings1.Enabled := Enabled;
      MarkerSettings1.Color := Color;
      MarkerSettings1.Width := Width;
      MarkerSettings1.Height := Height;
      MarkerSettings1.Radius := Radius;
      MarkerSettings1.Style := Style;
      MarkerSettings1.Value := Value;
    end;

    with FSavedGauge.MarkerSettings2 do
    begin
      MarkerSettings2.Enabled := Enabled;
      MarkerSettings2.Color := Color;
      MarkerSettings2.Width := Width;
      MarkerSettings2.Height := Height;
      MarkerSettings2.Radius := Radius;
      MarkerSettings2.Style := Style;
      MarkerSettings2.Value := Value;
    end;

    with FSavedGauge.MarkerSettings3 do
    begin
      MarkerSettings3.Enabled := Enabled;
      MarkerSettings3.Color := Color;
      MarkerSettings3.Width := Width;
      MarkerSettings3.Height := Height;
      MarkerSettings3.Radius := Radius;
      MarkerSettings3.Style := Style;
      MarkerSettings3.Value := Value;
    end;

  end; // with supergauge
end;

procedure TSGTestFrm.PerfTestBtnClick(Sender: TObject);
var
i,j: integer;
begin

  for j := 0 to 10 do
  begin
      for i := 0 to 100 do
      begin
        SuperGauge.Value := i;
        Application.ProcessMessages;
      end;
  end;
  beep;
end;

procedure TSGTestFrm.UpdateAllStats;
begin
  UpdateBasicStats;
  UpdateWHStats;
  UpdateMinMaxStats;
  UpdateLTStats;
  UpdateRangeLEDStats;
  UpdateMarkerStats;
  UpdateBandStats;
  UpdateTextStats;
  UpdateCapStats;
  UpdatePointerStats;
  UpdateAuxPointerStats;
  UpdateScaleStats;
  UpdateAuxScaleStats;
  UpdateFaceStats;
  UpdateFrameStats;
  UpdatePositionStats;

  // hack to sync combo boxes

  RangeLEDRangeTypeCbChange(Nil); // force a quick call so can update

  if SuperGauge.AuxPointerSettings.Enabled then
    BGRAKnobAux.KnobColor := clBtnFace
  else
    BGRAKnobAux.KnobColor := clRed;

  if SuperGauge.PointerSettings.Enabled then
    BGRAKnob.KnobColor := clBtnFace
  else
    BGRAKnob.KnobColor := clRed;
end;

procedure TSGTestFrm.UpdateWHStats;
begin
    WidthValLbl.Caption := IntToStr(SuperGauge.Width);
    HeightValLbl.Caption := IntToStr(SuperGauge.Height);
end;

procedure TSGTestFrm.UpdateMinMaxStats;
begin
  MinValueSpe.Value := SuperGauge.MinValue;
  MaxValueSpe.Value := SuperGauge.MaxValue;
  AuxMinValueSpe.Value := SuperGauge.AuxMinValue;
  AuxMaxValueSpe.Value := SuperGauge.AuxMaxValue;
end;

procedure TSGTestFrm.UpdateBasicStats;
begin
  BackgroundColorCb.Selected := SuperGauge.Color;
end;

procedure TSGTestFrm.UpdateLTStats;
begin
    LeftValLbl.Caption := IntToStr(SuperGauge.Left);
    TopValLbl.Caption := IntToStr(SuperGauge.Top);
end;

procedure TSGTestFrm.UpdatePositionStats;
begin
  GaugeValLbl.Caption := FloatToStr(SuperGauge.Value);
  KnobValLbl.Caption := FloatToStr(BGRAKnob.Value);
  UserToGaugeLbl.Caption := FloatToStr(SuperGauge.UserToGauge(BGRAKnob.Value, SuperGauge.MinValue, SuperGauge.MaxValue));
  DispMinValLbl.Caption := FloatToStr(SuperGauge.MinValue);
  DispMaxValLbl.Caption := FloatToStr(SuperGauge.MaxValue);
  DispAuxMinValLbl.Caption := FloatToStr(SuperGauge.AuxMinValue);
  DispAuxMaxValLbl.Caption := FloatToStr(SuperGauge.AuxMaxValue);
  AuxKnobValLbl.Caption := FloatToStr(BGRAKnobAux.Value);
  AuxGaugeValLbl.Caption := FloatToStr(SuperGauge.AuxValue);
  SuperGauge.TextSettings2.Text:= FormatFloat('###0', SuperGauge.Value * 100);
  Text2TextEdt.Text := SuperGauge.TextSettings2.Text; // update the edit box to be nice!
end;

procedure TSGTestFrm.UpdateRangeLEDStats;
begin
  // RangeLED Settings get the State

  RangeLEDActiveCb.Checked := SuperGauge.RangeLEDSettings.Active;

  // Get the LED Style and stuff the ComboBox

  RangeLEDStyleCb.ItemIndex := ord(SuperGauge.RangeLEDSettings.Style);
  RangeLEDShapeCb.ItemIndex := ord(SuperGauge.RangeLEDSettings.Shape);

  // Read settings from the gauge and set to the shape as our makeshift
  // LED for testings.
  // Be Fancy and if setting LED as shaded do the same for the test

  TestLEDShape.UseFillGradient := (SuperGauge.RangeLEDSettings.Style = lsShaded);

  case SuperGauge.RangeLEDSettings.Shape of
    lshRound:
        begin
          TestLEDShape.ShapeType := stEllipse;
        end;
    lshSquare:
        begin
          TestLEDShape.ShapeType := stRegularPolygon;
          TestLEDShape.SideCount := 4;
          TestLEDShape.Angle := 45;
        end;
    lshTriangle:
        begin
          TestLEDShape.ShapeType := stRegularPolygon;
          TestLEDShape.SideCount := 3;
          TestLEDShape.Angle := 0;
        end;
    lshDownTriangle:
        begin
          TestLEDShape.ShapeType := stRegularPolygon;
          TestLEDShape.SideCount := 3;
          TestLEDShape.Angle := 180;
        end
  else
    TestLEDShape.ShapeType := stEllipse;
  end;

  if SuperGauge.RangeLEDSettings.Active then
    begin
      TestLEDShape.FillColor := SuperGauge.RangeLEDSettings.ActiveColor;
      TestLEDShape.FillGradient.StartColor := SuperGauge.RangeLEDSettings.ActiveColor;
      TestLEDShape.FillGradient.EndColor := SuperGauge.RangeLEDSettings.InactiveColor;
    end
  else
    begin
       TestLEDShape.FillColor := clBlack;
       TestLEDShape.FillGradient.StartColor := SuperGauge.RangeLEDSettings.InactiveColor;
       TestLEDShape.FillGradient.EndColor := clGray;
    end;

  // Now the colors

  RangeLEDActiveColorCb.Selected := SuperGauge.RangeLEDSettings.ActiveColor;
  RangeLEDInactiveColorCb.Selected := SuperGauge.RangeLEDSettings.InactiveColor;
  RangeLEDBorderColorCb.Selected := SuperGauge.RangeLEDSettings.BorderColor;

  // Position and size

  RangeLEDSizeSpe.Value := SuperGauge.RangeLEDSettings.Size;
  RangeLEDOffsetXSpe.Value := SuperGauge.RangeLEDSettings.OffsetX;
  RangeLEDOffsetYSpe.Value := SuperGauge.RangeLEDSettings.OffsetY;

  // Range Check Stuff

  RangeLEDRangeTypeCb.ItemIndex := ord(SuperGauge.RangeLEDSettings.RangeType);
  RangeLEDRangeStartSpe.Value := SuperGauge.RangeLEDSettings.RangeStartValue;
  RangeLEDRangeEndSpe.Value := SuperGauge.RangeLEDSettings.RangeEndValue;
end;

procedure TSGTestFrm.UpdateMarkerStats;
begin
  Marker1EnabledCb.Checked := SuperGauge.MarkerSettings1.Enabled;
  Marker1ColorCb.Selected := SuperGauge.MarkerSettings1.Color;
  Marker1WidthSpe.Value := SuperGauge.MarkerSettings1.Width;
  Marker1HeightSpe.Value := SuperGauge.MarkerSettings1.Height;
  Marker1RadiusSpe.Value := SuperGauge.MarkerSettings1.Radius;
  Marker1ValueSpe.Value := SuperGauge.MarkerSettings1.Value;
  Marker1StyleCb.ItemIndex := ord(SuperGauge.MarkerSettings1.Style);

  Marker2EnabledCb.Checked := SuperGauge.MarkerSettings2.Enabled;
  Marker2ColorCb.Selected := SuperGauge.MarkerSettings2.Color;
  Marker2WidthSpe.Value := SuperGauge.MarkerSettings2.Width;
  Marker2HeightSpe.Value := SuperGauge.MarkerSettings2.Height;
  Marker2RadiusSpe.Value := SuperGauge.MarkerSettings2.Radius;
  Marker2ValueSpe.Value := SuperGauge.MarkerSettings2.Value;
  Marker2StyleCb.ItemIndex := ord(SuperGauge.MarkerSettings2.Style);

  Marker3EnabledCb.Checked := SuperGauge.MarkerSettings3.Enabled;
  Marker3ColorCb.Selected := SuperGauge.MarkerSettings3.Color;
  Marker3WidthSpe.Value := SuperGauge.MarkerSettings3.Width;
  Marker3HeightSpe.Value := SuperGauge.MarkerSettings3.Height;
  Marker3RadiusSpe.Value := SuperGauge.MarkerSettings3.Radius;
  Marker3ValueSpe.Value := SuperGauge.MarkerSettings3.Value;
  Marker3StyleCb.ItemIndex := ord(SuperGauge.MarkerSettings3.Style);
end;

procedure TSGTestFrm.UpdateBandStats;
begin
  // Band Settings

  // Band 1 edits setup

  Band1EnabledCb.Checked := SuperGauge.BandSettings1.Enabled;
  Band1EnabledTextCb.Checked := SuperGauge.BandSettings1.EnableText;
  Band1ColorCb.Selected := SuperGauge.BandSettings1.BandColor;
  Band1StartValueSpe.Value := SuperGauge.BandSettings1.StartValue;
  Band1EndValueSpe.Value := SuperGauge.BandSettings1.EndValue;
  Band1ThicknessSpe.Value := SuperGauge.BandSettings1.BandThickness;
  Band1BandRadiusSpe.Value := SuperGauge.BandSettings1.BandRadius;
  Band1TextEdt.Text := SuperGauge.BandSettings1.Text;
  Band1TextColorCb.Selected := SuperGauge.BandSettings1.TextColor;
  Band1TextSizeSpe.Value := SuperGauge.BandSettings1.TextSize;
  Band1TextRadiusSpe.Value := SuperGauge.BandSettings1.TextRadius;

  // 0 = Bold=fsBold, 1 = Italic=fsItalic, 2 = Underline=fsUnderline 3 = Strike Out=fsStrikeOut,
  // if the order in the dropdown changes this is hosed

  // Setting each of these will call the onChange handler!

  Band1TextStyleDDCb.Checked[0] := fsBold in SuperGauge.BandSettings1.TextStyle;
  Band1TextStyleDDCb.Checked[1] := fsItalic in SuperGauge.BandSettings1.TextStyle;
  Band1TextStyleDDCb.Checked[2] := fsUnderline in SuperGauge.BandSettings1.TextStyle;
  Band1TextStyleDDCb.Checked[3] := fsStrikeOut in SuperGauge.BandSettings1.TextStyle;

  // Band 2 edits setup

  Band2EnabledCb.Checked := SuperGauge.BandSettings2.Enabled;
  Band2EnabledTextCb.Checked := SuperGauge.BandSettings2.EnableText;
  Band2ColorCb.Selected := SuperGauge.BandSettings2.BandColor;
  Band2StartValueSpe.Value := SuperGauge.BandSettings2.StartValue;
  Band2EndValueSpe.Value := SuperGauge.BandSettings2.EndValue;
  Band2ThicknessSpe.Value := SuperGauge.BandSettings2.BandThickness;
  Band2BandRadiusSpe.Value := SuperGauge.BandSettings2.BandRadius;
  Band2TextEdt.Text := SuperGauge.BandSettings2.Text;
  Band2TextColorCb.Selected := SuperGauge.BandSettings2.TextColor;
  Band2TextSizeSpe.Value := SuperGauge.BandSettings2.TextSize;
  Band2TextRadiusSpe.Value := SuperGauge.BandSettings2.TextRadius;
  Band2TextStyleDDCb.Checked[0] := fsBold in SuperGauge.BandSettings2.TextStyle;
  Band2TextStyleDDCb.Checked[1] := fsItalic in SuperGauge.BandSettings2.TextStyle;
  Band2TextStyleDDCb.Checked[2] := fsUnderline in SuperGauge.BandSettings2.TextStyle;
  Band2TextStyleDDCb.Checked[3] := fsStrikeOut in SuperGauge.BandSettings2.TextStyle;

  // Band 3 edits setup

  Band3EnabledCb.Checked := SuperGauge.BandSettings3.Enabled;
  Band3EnabledTextCb.Checked := SuperGauge.BandSettings3.EnableText;
  Band3ColorCb.Selected := SuperGauge.BandSettings3.BandColor;
  Band3StartValueSpe.Value := SuperGauge.BandSettings3.StartValue;
  Band3EndValueSpe.Value := SuperGauge.BandSettings3.EndValue;
  Band3ThicknessSpe.Value := SuperGauge.BandSettings3.BandThickness;
  Band3BandRadiusSpe.Value := SuperGauge.BandSettings3.BandRadius;
  Band3TextEdt.Text := SuperGauge.BandSettings3.Text;
  Band3TextColorCb.Selected := SuperGauge.BandSettings3.TextColor;
  Band3TextSizeSpe.Value := SuperGauge.BandSettings3.TextSize;
  Band3TextRadiusSpe.Value := SuperGauge.BandSettings3.TextRadius;
  Band3TextStyleDDCb.Checked[0] := fsBold in SuperGauge.BandSettings3.TextStyle;
  Band3TextStyleDDCb.Checked[1] := fsItalic in SuperGauge.BandSettings3.TextStyle;
  Band3TextStyleDDCb.Checked[2] := fsUnderline in SuperGauge.BandSettings3.TextStyle;
  Band3TextStyleDDCb.Checked[3] := fsStrikeOut in SuperGauge.BandSettings3.TextStyle;

  // Band 4 edits setup

  Band4EnabledCb.Checked := SuperGauge.BandSettings4.Enabled;
  Band4EnabledTextCb.Checked := SuperGauge.BandSettings4.EnableText;
  Band4ColorCb.Selected := SuperGauge.BandSettings4.BandColor;
  Band4StartValueSpe.Value := SuperGauge.BandSettings4.StartValue;
  Band4EndValueSpe.Value := SuperGauge.BandSettings4.EndValue;
  Band4ThicknessSpe.Value := SuperGauge.BandSettings4.BandThickness;
  Band4BandRadiusSpe.Value := SuperGauge.BandSettings4.BandRadius;
  Band4TextEdt.Text := SuperGauge.BandSettings4.Text;
  Band4TextColorCb.Selected := SuperGauge.BandSettings4.TextColor;
  Band4TextSizeSpe.Value := SuperGauge.BandSettings4.TextSize;
  Band4TextRadiusSpe.Value := SuperGauge.BandSettings4.TextRadius;
  Band4TextStyleDDCb.Checked[0] := fsBold in SuperGauge.BandSettings4.TextStyle;
  Band4TextStyleDDCb.Checked[1] := fsItalic in SuperGauge.BandSettings4.TextStyle;
  Band4TextStyleDDCb.Checked[2] := fsUnderline in SuperGauge.BandSettings4.TextStyle;
  Band4TextStyleDDCb.Checked[3] := fsStrikeOut in SuperGauge.BandSettings4.TextStyle;

end;

procedure TSGTestFrm.UpdateTextStats;
begin
  // Text Settings
  // Band 1 edits setup

  Text1EnabledCb.Checked := SuperGauge.TextSettings1.Enabled;
  Text1ColorCb.Selected := SuperGauge.TextSettings1.FontEx.Color;
  Text1TextEdt.Text := SuperGauge.TextSettings1.Text;
  Text1TextSizeSpe.Value := SuperGauge.TextSettings1.FontEx.Height;
  Text1OffsetXSpe.Value := SuperGauge.TextSettings1.OffsetX;
  Text1OffsetYSpe.Value := SuperGauge.TextSettings1.OffsetY;

  // 0 = Bold=fsBold, 1 = Italic=fsItalic, 2 = Underline=fsUnderline 3 = Strike Out=fsStrikeOut
  // if the order in the dropdown changes this is hosed

  // Setting each of these will call the onChange handler!

  Text1TextStyleDDCb.Checked[0] := fsBold in SuperGauge.TextSettings1.FontEx.Style;
  Text1TextStyleDDCb.Checked[1] := fsItalic in SuperGauge.TextSettings1.FontEx.Style;
  Text1TextStyleDDCb.Checked[2] := fsUnderline in SuperGauge.TextSettings1.FontEx.Style;
  Text1TextStyleDDCb.Checked[3] := fsStrikeOut in SuperGauge.TextSettings1.FontEx.Style;

  Text2EnabledCb.Checked := SuperGauge.TextSettings2.Enabled;
  Text2ColorCb.Selected := SuperGauge.TextSettings2.FontEx.Color;
  Text2TextEdt.Text := SuperGauge.TextSettings2.Text;
  Text2TextSizeSpe.Value := SuperGauge.TextSettings2.FontEx.Height;
  Text2OffsetXSpe.Value := SuperGauge.TextSettings2.OffsetX;
  Text2OffsetYSpe.Value := SuperGauge.TextSettings2.OffsetY;
  Text2TextStyleDDCb.Checked[0] := fsBold in SuperGauge.TextSettings2.FontEx.Style;
  Text2TextStyleDDCb.Checked[1] := fsItalic in SuperGauge.TextSettings2.FontEx.Style;
  Text2TextStyleDDCb.Checked[2] := fsUnderline in SuperGauge.TextSettings2.FontEx.Style;
  Text2TextStyleDDCb.Checked[3] := fsStrikeOut in SuperGauge.TextSettings2.FontEx.Style;

  Text3EnabledCb.Checked := SuperGauge.TextSettings3.Enabled;
  Text3ColorCb.Selected := SuperGauge.TextSettings3.FontEx.Color;
  Text3TextEdt.Text := SuperGauge.TextSettings3.Text;
  Text3TextSizeSpe.Value := SuperGauge.TextSettings3.FontEx.Height;
  Text3OffsetXSpe.Value := SuperGauge.TextSettings3.OffsetX;
  Text3OffsetYSpe.Value := SuperGauge.TextSettings3.OffsetY;
  Text3TextStyleDDCb.Checked[0] := fsBold in SuperGauge.TextSettings3.FontEx.Style;
  Text3TextStyleDDCb.Checked[1] := fsItalic in SuperGauge.TextSettings3.FontEx.Style;
  Text3TextStyleDDCb.Checked[2] := fsUnderline in SuperGauge.TextSettings3.FontEx.Style;
  Text3TextStyleDDCb.Checked[3] := fsStrikeOut in SuperGauge.TextSettings3.FontEx.Style;
end;

procedure TSGTestFrm.UpdateCapStats;
begin
  // Pointer Cap Settings

  CapPositionCb.ItemIndex := ord(SuperGauge.PointerCapSettings.CapPosition);
  CapStyleCb.ItemIndex := ord(SuperGauge.PointerCapSettings.CapStyle);
  CapCurveExponentSpe.Value := SuperGauge.PointerCapSettings.CurveExponent;
  CapEdgeColorCb.Selected := SuperGauge.PointerCapSettings.EdgeColor;
  CapFillColorCb.Selected := SuperGauge.PointerCapSettings.FillColor;
  CapLightIntensitySpe.Value := SuperGauge.PointerCapSettings.LightIntensity;
  CapRadiusSpe.Value := SuperGauge.PointerCapSettings.Radius;
  CapEdgeWidthSpe.Value := SuperGauge.PointerCapSettings.EdgeWidth;
end;

procedure TSGTestFrm.UpdatePointerStats;
begin
  // Pointer Settings

  PointerColorCb.Selected := SuperGauge.PointerSettings.Color;
  PointerLengthSpe.Value := SuperGauge.PointerSettings.Length;
  PointerExtensionLengthSpe.Value := SuperGauge.PointerSettings.ExtensionLength;
  PointerStyleCb.ItemIndex := ord(SuperGauge.PointerSettings.Style);
  PointerWidthSpe.Value := SuperGauge.PointerSettings.Width;
  PointerEnabledCb.Checked := SuperGauge.PointerSettings.Enabled;
  PointerHighlightCb.Checked := SuperGauge.PointerSettings.HighlightLine;
  PointerHighlightColorCb.Selected := SuperGauge.PointerSettings.HighlightColor;
  PointerHighlightThicknessSpe.Value := SuperGauge.PointerSettings.HighlightThickness;
end;

procedure TSGTestFrm.UpdateAuxPointerStats;
begin
  // AuxPointer Settings

  AuxPointerColorCb.Selected := SuperGauge.AuxPointerSettings.Color;
  AuxPointerLengthSpe.Value := SuperGauge.AuxPointerSettings.Length;
  AuxPointerExtensionLengthSpe.Value := SuperGauge.AuxPointerSettings.ExtensionLength;
  AuxPointerStyleCb.ItemIndex := ord(SuperGauge.AuxPointerSettings.Style);
  AuxPointerWidthSpe.Value := SuperGauge.AuxPointerSettings.Width;
  AuxPointerEnabledCb.Checked := SuperGauge.AuxPointerSettings.Enabled;
  AuxPointerHighlightCb.Checked := SuperGauge.AuxPointerSettings.HighlightLine;
  AuxPointerHighlightColorCb.Selected := SuperGauge.AuxPointerSettings.HighlightColor;
  AuxPointerHighlightThicknessSpe.Value := SuperGauge.AuxPointerSettings.HighlightThickness;
end;

procedure TSGTestFrm.UpdateScaleStats;
begin
  // Scale Settings

  ScaleEnabledCb.Checked := SuperGauge.ScaleSettings.Enabled;
  ScaleEnableMainTicksCb.Checked := SuperGauge.ScaleSettings.EnableMainTicks;
  ScaleEnableSubTicksCb.Checked := SuperGauge.ScaleSettings.EnableSubTicks;
  EnableScaleTextCb.Checked := SuperGauge.ScaleSettings.EnableScaleText;
  ScaleReversedCb.Checked := SuperGauge.ScaleSettings.ReverseScale;
  ScaleMainTickLengthSpe.Value := SuperGauge.ScaleSettings.MainTickLength;
  ScaleSubTickLengthSpe.Value := SuperGauge.ScaleSettings.SubTickLength;
  ScaleMainTickCountSpe.Value := SuperGauge.ScaleSettings.MainTickCount;
  ScaleSubTickCountSpe.Value := SuperGauge.ScaleSettings.SubTickCount;
  ScaleStartSpe.Value := SuperGauge.ScaleSettings.Start;
  ScaleStepSpe.Value := SuperGauge.ScaleSettings.Step;
  ScaleInnerTickArcThicknessSpe.Value := SuperGauge.ScaleSettings.InnerTickArcThickness;
  ScaleOuterTickArcThicknessSpe.Value := SuperGauge.ScaleSettings.OuterTickArcThickness;
  ScaleTickArcColorColorCb.Selected := SuperGauge.ScaleSettings.TickArcColor;

  ScaleSubTickCountSpe.Value := SuperGauge.ScaleSettings.SubTickCount;
  ScaleRadiusSpe.Value := SuperGauge.ScaleSettings.ScaleRadius;
  ScaleTickColorColorCb.Selected := SuperGauge.ScaleSettings.TickColor;
  ScaleTextRadiusSpe.Value := SuperGauge.ScaleSettings.TextRadius;
  ScaleTextColorColorCb.Selected := SuperGauge.ScaleSettings.TextColor;

  ScaleTextStyleDDCb.Checked[0] := fsBold in SuperGauge.ScaleSettings.TextStyle;
  ScaleTextStyleDDCb.Checked[1] := fsItalic in SuperGauge.ScaleSettings.TextStyle;
  ScaleTextStyleDDCb.Checked[2] := fsUnderline in SuperGauge.ScaleSettings.TextStyle;
  ScaleTextStyleDDCb.Checked[3] := fsStrikeOut in SuperGauge.ScaleSettings.TextStyle;

  ScaleTextSizeSpe.Value := SuperGauge.ScaleSettings.TextSize;
  ScaleMainTickThicknessSpe.Value := SuperGauge.ScaleSettings.MainTickThickness;
  ScaleSubTickThicknessSpe.Value := SuperGauge.ScaleSettings.SubTickThickness;
  ScaleTickArcStyleCb.ItemIndex := ord(SuperGauge.ScaleSettings.TickArcStyle);
  ScaleMainTickUseDotsCb.Checked := SuperGauge.ScaleSettings.MainTickUseDots;
  ScaleSubTickUseDotsCb.Checked := SuperGauge.ScaleSettings.SubTickUseDots;
end;

procedure TSGTestFrm.UpdateAuxScaleStats;
begin
  // Aux Scale Settings

  AuxScaleEnabledCb.Checked := SuperGauge.AuxScaleSettings.Enabled;
  AuxScaleEnableMainTicksCb.Checked := SuperGauge.AuxScaleSettings.EnableMainTicks;
  AuxScaleEnableSubTicksCb.Checked := SuperGauge.AuxScaleSettings.EnableSubTicks;
  AuxEnableScaleTextCb.Checked := SuperGauge.AuxScaleSettings.EnableScaleText;
  AuxScaleReversedCb.Checked := SuperGauge.AuxScaleSettings.ReverseScale;
  AuxScaleMainTickLengthSpe.Value := SuperGauge.AuxScaleSettings.MainTickLength;
  AuxScaleSubTickLengthSpe.Value := SuperGauge.AuxScaleSettings.SubTickLength;
  AuxScaleMainTickCountSpe.Value := SuperGauge.AuxScaleSettings.MainTickCount;
  AuxScaleSubTickCountSpe.Value := SuperGauge.AuxScaleSettings.SubTickCount;
  AuxScaleStartSpe.Value := SuperGauge.AuxScaleSettings.Start;
  AuxScaleStepSpe.Value := SuperGauge.AuxScaleSettings.Step;
  AuxScaleInnerTickArcThicknessSpe.Value := SuperGauge.AuxScaleSettings.InnerTickArcThickness;
  AuxScaleOuterTickArcThicknessSpe.Value := SuperGauge.AuxScaleSettings.OuterTickArcThickness;
  AuxScaleTickArcColorColorCb.Selected := SuperGauge.AuxScaleSettings.TickArcColor;

  AuxScaleSubTickCountSpe.Value := SuperGauge.AuxScaleSettings.SubTickCount;
  AuxScaleRadiusSpe.Value := SuperGauge.AuxScaleSettings.ScaleRadius;
  AuxScaleTickColorColorCb.Selected := SuperGauge.AuxScaleSettings.TickColor;
  AuxScaleTextRadiusSpe.Value := SuperGauge.AuxScaleSettings.TextRadius;
  AuxScaleTextColorColorCb.Selected := SuperGauge.AuxScaleSettings.TextColor;

  AuxScaleTextStyleDDCb.Checked[0] := fsBold in SuperGauge.AuxScaleSettings.TextStyle;
  AuxScaleTextStyleDDCb.Checked[1] := fsItalic in SuperGauge.AuxScaleSettings.TextStyle;
  AuxScaleTextStyleDDCb.Checked[2] := fsUnderline in SuperGauge.AuxScaleSettings.TextStyle;
  AuxScaleTextStyleDDCb.Checked[3] := fsStrikeOut in SuperGauge.AuxScaleSettings.TextStyle;

  AuxScaleTextSizeSpe.Value := SuperGauge.AuxScaleSettings.TextSize;
  AuxScaleMainTickThicknessSpe.Value := SuperGauge.AuxScaleSettings.MainTickThickness;
  AuxScaleSubTickThicknessSpe.Value := SuperGauge.AuxScaleSettings.SubTickThickness;
  AuxScaleTickArcStyleCb.ItemIndex := ord(SuperGauge.AuxScaleSettings.TickArcStyle);
  AuxScaleMainTickUseDotsCb.Checked := SuperGauge.AuxScaleSettings.MainTickUseDots;
  AuxScaleSubTickUseDotsCb.Checked := SuperGauge.AuxScaleSettings.SubTickUseDots;
end;

procedure TSGTestFrm.UpdateFaceStats;
begin
  // Face Settings

  FaceFillStyleCb.ItemIndex := ord(SuperGauge.FaceSettings.FillStyle);
  FaceOuterColorCb.Selected := SuperGauge.FaceSettings.OuterColor;
  FaceInnerColorCb.Selected := SuperGauge.FaceSettings.InnerColor;
  FacePictureEnabledCb.Checked := SuperGauge.FaceSettings.PictureEnabled;
  FacePictureOffsetXSpe.Value := SuperGauge.FaceSettings.PictureOffsetX;
  FacePictureOffsetYSpe.Value := SuperGauge.FaceSettings.PictureOffsetY;
  FaceCurveExponentSpe.Value := SuperGauge.FaceSettings.CurveExponent;
  FaceLightIntensitySpe.Value := SuperGauge.FaceSettings.LightIntensity;
end;

procedure TSGTestFrm.UpdateFrameStats;
begin
  // Frame Settings

  FrameOuterFrameColorCb.Selected := SuperGauge.FrameSettings.OuterFrameColor;
  FrameOuterFrameThicknessSpe.Value := SuperGauge.FrameSettings.OuterFrameThickness;
  FrameMiddleFrameColorCb.Selected := SuperGauge.FrameSettings.MiddleFrameColor;
  FrameMiddleFrameThicknessSpe.Value := SuperGauge.FrameSettings.MiddleFrameThickness;
  FrameInnerFrameColorCb.Selected := SuperGauge.FrameSettings.InnerFrameColor;
  FrameInnerFrameThicknessSpe.Value := SuperGauge.FrameSettings.InnerFrameThickness;
end;

procedure TSGTestFrm.RangeLEDOffsetXSpeChange(Sender: TObject);
begin
  SuperGauge.RangeLEDSettings.OffsetX := RangeLEDOffsetXSpe.Value;
  UpdateRangeLEDStats;
end;

procedure TSGTestFrm.RangeLEDOffsetYSpeChange(Sender: TObject);
begin
  SuperGauge.RangeLEDSettings.OffsetY := RangeLEDOffsetYSpe.Value;
  UpdateRangeLEDStats;
end;

procedure TSGTestFrm.RangeLEDActiveColorCbChange(Sender: TObject);
begin
  SuperGauge.RangeLEDSettings.ActiveColor := RangeLEDActiveColorCb.Selected;
  UpdateRangeLEDStats;
end;

procedure TSGTestFrm.RangeLEDBorderColorCbChange(Sender: TObject);
begin
  SuperGauge.RangeLEDSettings.BorderColor := RangeLEDBorderColorCb.Selected;
  UpdateRangeLEDStats;
end;

procedure TSGTestFrm.Band1ThicknessSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings1.BandThickness := Band1ThicknessSpe.Value;
end;

procedure TSGTestFrm.RangeLEDRangeTypeCbChange(Sender: TObject);
begin
  // Range Types -
  //          rcNone, rcGaugeOverload, rcBetween, rcBothInclusive,
  //          rcStartInclusive, rcEndInclusive, rcBothBetweenOutside,
  //          rcBothInclusiveOutside, rcGreaterStart, rcLessEnd,
  //          rcGreaterStartInclusive, rcLessEndInclusive

  RangeLEDRangeStartSpe.ReadOnly := False;
  RangeLEDRangeEndSpe.ReadOnly := False;
  RangeLEDRangeStartSpe.Color := clWindow;
  RangeLEDRangeEndSpe.Color := clWindow;

  case RangeLEDRangeTypeCb.ItemIndex of
    0 : {rcNone}
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcNone;
          RangeLEDRangeStartSpe.ReadOnly := True;
          RangeLEDRangeEndSpe.ReadOnly := True;
          RangeLEDRangeStartSpe.Color := clInactiveCaption;
          RangeLEDRangeEndSpe.Color := clInactiveCaption;
        end;

    1 : {rcGaugeOverload} // uses gauge range only, ignores RangeLED start/end
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcGaugeOverload;
          RangeLEDRangeStartSpe.ReadOnly := True;
          RangeLEDRangeEndSpe.ReadOnly := True;
          RangeLEDRangeStartSpe.Color := clInactiveCaption;
          RangeLEDRangeEndSpe.Color := clInactiveCaption;
        end;
    2 : {rcBetween}
        begin
            SuperGauge.RangeLEDSettings.RangeType := rcBetween;
        end;
    3 : {rcBothInclusive}
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcBothInclusive;
        end;
    4 : {rcStartInclusive}
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcStartInclusive;
        end;
    5 : {rcEndInclusive}
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcEndInclusive;
        end;
    6 : {rcBothBetweenOutside}
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcBothBetweenOutside;
        end;
    7 : {rcBothInclusiveOutside}
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcBothInclusiveOutside;
        end;

    // These last cases are really just making it so you only look at start or end
    // You could use the above with a LARGE number (Max integer) for the end, or Min Integer
    // and get the same results, but this just ensures it to work looking only at one value
    // The Inclusive will 'include' the start or end value if not obvious

    8 : {rcGreaterStart}
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcGreaterStart;
          RangeLEDRangeEndSpe.ReadOnly := True;
          RangeLEDRangeEndSpe.Color := clInactiveCaption;
        end;
    9 : {rcLessEnd}
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcLessEnd;
          RangeLEDRangeStartSpe.ReadOnly := True;
          RangeLEDRangeStartSpe.Color := clInactiveCaption;
        end;
    10 : {rcGreaterStartInclusive}
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcGreaterStartInclusive;
          RangeLEDRangeEndSpe.ReadOnly := True;
          RangeLEDRangeEndSpe.Color := clInactiveCaption;
        end;
    11 : {rcLessEndInclusive}
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcLessEndInclusive;
          RangeLEDRangeStartSpe.ReadOnly := True;
          RangeLEDRangeStartSpe.Color := clInactiveCaption;
        end;
  else
      // warn...
  end;

  UpdateRangeLEDStats;

end;

procedure TSGTestFrm.RangeLEDInactiveColorCbChange(Sender: TObject);
begin
  SuperGauge.RangeLEDSettings.InactiveColor := RangeLEDInactiveColorCb.Selected;
  UpdateRangeLEDStats;
end;

procedure TSGTestFrm.RangeLEDRangeStartSpeChange(Sender: TObject);
begin
  SuperGauge.RangeLEDSettings.RangeStartValue := RangeLEDRangeStartSpe.Value;
  UpdateRangeLEDStats;
end;

procedure TSGTestFrm.RangeLEDRangeEndSpeChange(Sender: TObject);
begin
  SuperGauge.RangeLEDSettings.RangeEndValue := RangeLEDRangeEndSpe.Value;
  UpdateRangeLEDStats;
end;

procedure TSGTestFrm.RangeLEDStyleCbChange(Sender: TObject);
begin
  // Set the Fill Style of the LED, update the stats
  // lsNone = 0, lsFlat = 1, lsShaded = 2

  case RangeLEDStyleCb.ItemIndex of
    0 : {lsNone}
        begin
          SuperGauge.RangeLEDSettings.Style := lsNone;
        end;
    1 : {lsFlat}
        begin
            SuperGauge.RangeLEDSettings.Style := lsFlat;
        end;
    2 : {lsShaded}
        begin
          SuperGauge.RangeLEDSettings.Style := lsShaded;
        end
  else
    // warn
  end;

  UpdateRangeLEDStats;
end;

procedure TSGTestFrm.RangeLEDShapeCbChange(Sender: TObject);
begin
  // Set the Shape of the LED, update the stats
  // lshRound = 0, lshSquare = 1, lshTriangle = 2, lshDownTriangle = 3

  case RangeLEDShapeCb.ItemIndex of
    0 : {lshRound}
        begin
          SuperGauge.RangeLEDSettings.Shape := lshRound;
        end;
    1 : {lshSquare}
        begin
            SuperGauge.RangeLEDSettings.Shape := lshSquare;
        end;
    2 : {lshTriangle}
        begin
          SuperGauge.RangeLEDSettings.Shape := lshTriangle;
        end;
    3 : {lshDownTriangle}
        begin
          SuperGauge.RangeLEDSettings.Shape := lshDownTriangle;
        end
  else
    // warn
  end;

  UpdateRangeLEDStats;
end;

procedure TSGTestFrm.RangeLEDSizeSpeChange(Sender: TObject);
begin
  SuperGauge.RangeLEDSettings.Size := RangeLEDSizeSpe.Value;
  UpdateRangeLEDStats;
end;


procedure TSGTestFrm.RangeLEDResetOffsetBtnClick(Sender: TObject);
begin
  SuperGauge.RangeLEDSettings.OffsetX := 50;
  SuperGauge.RangeLEDSettings.OffsetY := 90;
  UpdateRangeLEDStats;
end;

procedure TSGTestFrm.RangeLEDResetRangesBtnClick(Sender: TObject);
begin
  SuperGauge.RangeLEDSettings.RangeStartValue := 0.0;
  SuperGauge.RangeLEDSettings.RangeEndValue := 100.0;
  UpdateRangeLEDStats;
end;

procedure TSGTestFrm.OverloadPosBtnClick(Sender: TObject);
begin
  // overload value, but only if MaxValue is obviously less than 250

  BGRAKnobValueChanged(Sender, 250);
end;

procedure TSGTestFrm.OverloadNegBtnClick(Sender: TObject);
begin
  // overload value, but only if MinValue is obviously geater than -250

  BGRAKnobValueChanged(Sender, -250);
end;

procedure TSGTestFrm.SuperGaugeDblClick(Sender: TObject);
begin
  Beep;
end;

procedure TSGTestFrm.SuperGaugeRangeLEDInactive(Sender: TObject; Value: single);
begin
  RangeLEDCallBackLED.Active := True;
  RangeLEDCallBackLED.ActiveColor := SuperGauge.RangeLEDSettings.InactiveColor;
  RangeLEDCallbackNameValLbl.Caption := 'Inactive at ' + FloatToStr(Value);
  RangeLEDActiveCb.Checked := False;
end;

procedure TSGTestFrm.SuperGaugeRangeLEDActive(Sender: TObject; Value: single);
begin
  RangeLEDCallBackLED.Active := True;
  RangeLEDCallBackLED.ActiveColor := SuperGauge.RangeLEDSettings.ActiveColor;
  RangeLEDCallbackNameValLbl.Caption := 'Active at ' + FloatToStr(Value);
  RangeLEDActiveCb.Checked := True;
end;

procedure TSGTestFrm.AutoScaleCbChange(Sender: TObject);
begin
  SuperGauge.AutoScale := AutoScaleCb.checked;
end;

procedure TSGTestFrm.AuxEnableScaleTextCbChange(Sender: TObject);
begin
    SuperGauge.AuxScaleSettings.EnableScaleText := AuxEnableScaleTextCb.Checked;
end;

procedure TSGTestFrm.AuxMaxValueSpeChange(Sender: TObject);
begin
  SuperGauge.AuxMaxValue := AuxMaxValueSpe.Value;
  UpdatePositionStats;
end;

procedure TSGTestFrm.AuxMinValueSpeChange(Sender: TObject);
begin
  SuperGauge.AuxMinValue := AuxMinValueSpe.Value;
  UpdatePositionStats;
end;

procedure TSGTestFrm.AuxPointerColorCbChange(Sender: TObject);
begin
  SuperGauge.AuxPointerSettings.Color := AuxPointerColorCb.Selected;
end;

procedure TSGTestFrm.AuxPointerEnabledCbChange(Sender: TObject);
begin
  SuperGauge.AuxPointerSettings.Enabled := AuxPointerEnabledCb.Checked;

  if SuperGauge.AuxPointerSettings.Enabled then
    BGRAKnobAux.KnobColor := clBtnFace
  else
    BGRAKnobAux.KnobColor := clRed;
end;

procedure TSGTestFrm.AuxPointerExtensionLengthSpeChange(Sender: TObject);
begin
  SuperGauge.AuxPointerSettings.ExtensionLength := AuxPointerExtensionLengthSpe.Value;
  UpdateAuxPointerStats;
end;

procedure TSGTestFrm.AuxPointerHighlightCbChange(Sender: TObject);
begin
  SuperGauge.AuxPointerSettings.HighlightLine := AuxPointerHighlightCb.Checked;
end;

procedure TSGTestFrm.AuxPointerLengthSpeChange(Sender: TObject);
begin
  SuperGauge.AuxPointerSettings.Length := AuxPointerLengthSpe.Value;
  UpdateAuxPointerStats;
end;

procedure TSGTestFrm.AuxPointerStyleCbChange(Sender: TObject);
begin
  // Set the AuxPointer Style
  // psLine = 0, psLineExt = 1, psArc = 2, 3 = psTriangle

  case AuxPointerStyleCb.ItemIndex of
    0 : {psLine}
        begin
          SuperGauge.AuxPointerSettings.Style := psLine;
        end;
    1 : {psLineExt}
        begin
          SuperGauge.AuxPointerSettings.Style := psLineExt;
        end;
    2 : {psArc}
        begin
          SuperGauge.AuxPointerSettings.Style := psArc;
        end;
    3: {psTriangle}
        begin
          SuperGauge.AuxPointerSettings.Style := psTriangle;
        end;

  else
    // Unknown type, warn somewhere...
  end;

  UpdateAuxPointerStats;
end;

procedure TSGTestFrm.AuxPointerWidthSpeChange(Sender: TObject);
begin
  SuperGauge.AuxPointerSettings.Width := AuxPointerWidthSpe.Value;
  UpdateAuxPointerStats;
end;

procedure TSGTestFrm.AuxScaleEnableMainTicksCbChange(Sender: TObject);
begin
      SuperGauge.AuxScaleSettings.EnableMainTicks := AuxScaleEnableMainTicksCb.Checked;
end;

procedure TSGTestFrm.AuxScaleEnableSubTicksCbChange(Sender: TObject);
begin
    SuperGauge.AuxScaleSettings.EnableSubTicks := AuxScaleEnableSubTicksCb.Checked;
end;

procedure TSGTestFrm.AuxScaleInnerTickArcThicknessSpeChange(Sender: TObject);
begin
  SuperGauge.AuxScaleSettings.InnerTickArcThickness := AuxScaleInnerTickArcThicknessSpe.Value;
end;

procedure TSGTestFrm.AuxScaleMainTickCountSpeChange(Sender: TObject);
begin
    SuperGauge.AuxScaleSettings.MainTickCount := AuxScaleMainTickCountSpe.Value;
end;

procedure TSGTestFrm.AuxScaleMainTickLengthSpeChange(Sender: TObject);
begin
    SuperGauge.AuxScaleSettings.MainTickLength := AuxScaleMainTickLengthSpe.Value;
end;

procedure TSGTestFrm.AuxScaleMainTickThicknessSpeChange(Sender: TObject);
begin
    SuperGauge.AuxScaleSettings.MainTickThickness := AuxScaleMainTickThicknessSpe.Value;
end;

procedure TSGTestFrm.AuxScaleMainTickUseDotsCbChange(Sender: TObject);
begin
    SuperGauge.AuxScaleSettings.MainTickUseDots := AuxScaleMainTickUseDotsCb.Checked;
end;

procedure TSGTestFrm.AuxScaleOuterTickArcThicknessSpeChange(Sender: TObject);
begin
    SuperGauge.AuxScaleSettings.OuterTickArcThickness := AuxScaleOuterTickArcThicknessSpe.Value;
end;

procedure TSGTestFrm.AuxScaleRadiusSpeChange(Sender: TObject);
begin
       SuperGauge.AuxScaleSettings.ScaleRadius := AuxScaleRadiusSpe.Value;
end;

procedure TSGTestFrm.AuxScaleReversedCbChange(Sender: TObject);
begin
    SuperGauge.AuxScaleSettings.ReverseScale := AuxScaleReversedCb.Checked;
end;

procedure TSGTestFrm.AuxScaleSetFontBtnClick(Sender: TObject);
begin
    if FontDialog1.Execute then
    SuperGauge.AuxScaleSettings.TextFont := FontDialog1.Font.Name;
end;

procedure TSGTestFrm.AuxScaleStartSpeChange(Sender: TObject);
begin
    SuperGauge.AuxScaleSettings.Start := AuxScaleStartSpe.Value;
end;

procedure TSGTestFrm.AuxScaleStepSpeChange(Sender: TObject);
begin
    SuperGauge.AuxScaleSettings.Step := AuxScaleStepSpe.Value;
end;

procedure TSGTestFrm.AuxScaleSubTickCountSpeChange(Sender: TObject);
begin
    SuperGauge.AuxScaleSettings.SubTickCount := AuxScaleSubTickCountSpe.Value;
end;

procedure TSGTestFrm.AuxScaleSubTickLengthSpeChange(Sender: TObject);
begin
    SuperGauge.AuxScaleSettings.SubTickLength := AuxScaleSubTickLengthSpe.Value;
end;

procedure TSGTestFrm.AuxScaleSubTickThicknessSpeChange(Sender: TObject);
begin
    SuperGauge.AuxScaleSettings.SubTickThickness := AuxScaleSubTickThicknessSpe.Value;
end;

procedure TSGTestFrm.AuxScaleSubTickUseDotsCbChange(Sender: TObject);
begin
    SuperGauge.AuxScaleSettings.SubTickUseDots := AuxScaleSubTickUseDotsCb.Checked;
end;

procedure TSGTestFrm.AuxScaleTextColorColorCbChange(Sender: TObject);
begin
    SuperGauge.AuxScaleSettings.TextColor := AuxScaleTextColorColorCb.Selected;
end;

procedure TSGTestFrm.AuxScaleTextRadiusSpeChange(Sender: TObject);
begin
    SuperGauge.AuxScaleSettings.TextRadius := AuxScaleTextRadiusSpe.Value;
end;

procedure TSGTestFrm.AuxScaleTextSizeSpeChange(Sender: TObject);
begin
    SuperGauge.AuxScaleSettings.TextSize := AuxScaleTextSizeSpe.Value;
end;

procedure TSGTestFrm.AuxScaleTextStyleDDCbItemChange(Sender: TObject;   AIndex: Integer);
var
  TextStyle: TFontStyles;

begin
  // Bold=fsBold, Italic=fsItalic, Strike Out=fsStrikeOut, Underline=fsUnderline
  // Changing the order in the dropdown will break this

  TextStyle := SuperGauge.ScaleSettings.TextStyle;

    if AuxScaleTextStyleDDCb.Checked[AIndex] then
      case AuxScaleTextStyleDDCb.Items[AIndex] of
        'Bold'    : Include(TextStyle, fsBold);
        'Italic'  :  Include(TextStyle, fsItalic);
        'Strike Out' :  Include(TextStyle, fsStrikeOut);
        'Underline'  :  Include(TextStyle, fsUnderline); // may not work bug in BGRA??
      end
    else
      case AuxScaleTextStyleDDCb.Items[AIndex] of
        'Bold'    : Exclude(TextStyle, fsBold);
        'Italic'  :  Exclude(TextStyle, fsItalic);
        'Strike Out' :  Exclude(TextStyle, fsStrikeOut);
        'Underline'  :  Exclude(TextStyle, fsUnderline);
      end;

  SuperGauge.AuxScaleSettings.TextStyle := TextStyle;
end;

procedure TSGTestFrm.AuxScaleTickArcColorColorCbChange(Sender: TObject);
begin
    SuperGauge.AuxScaleSettings.TickArcColor := AuxScaleTickArcColorColorCb.Selected;
end;

procedure TSGTestFrm.AuxScaleTickArcStyleCbChange(Sender: TObject);
begin
    // Set Arc type 0=taNone, 1=taOuter, 2=taInner, 3=taBoth

  case AuxScaleTickArcStyleCb.ItemIndex of
    0 {taNone} : SuperGauge.AuxScaleSettings.TickArcStyle := taNone;
    1 {taOuter}: SuperGauge.AuxScaleSettings.TickArcStyle := taOuter;
    2 {taInner}: SuperGauge.AuxScaleSettings.TickArcStyle := taInner;
    3 {taBoth} : SuperGauge.AuxScaleSettings.TickArcStyle := taBoth;
  else
    // Unknown type, warn somewhere...
  end;
end;

procedure TSGTestFrm.AuxScaleTickColorColorCbChange(Sender: TObject);
begin
      SuperGauge.AuxScaleSettings.TickColor := AuxScaleTickColorColorCb.Selected;
end;

procedure TSGTestFrm.BackgroundColorCbChange(Sender: TObject);
begin
  SuperGauge.Color := BackgroundColorCb.Selected;
end;

procedure TSGTestFrm.AboutSubMenuClick(Sender: TObject);
begin
  AboutFrm.VersionStr := VERSIONSTR;
  AboutFrm.show;
end;

procedure TSGTestFrm.Band1ColorCbChange(Sender: TObject);
begin
    SuperGauge.BandSettings1.BandColor := Band1ColorCb.Selected;
end;

procedure TSGTestFrm.Band1EndValueSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings1.EndValue := Band1EndValueSpe.Value;
end;

procedure TSGTestFrm.Band1SetFontBtnClick(Sender: TObject);
begin
  if FontDialog1.Execute then
    SuperGauge.BandSettings1.TextFont := FontDialog1.Font.Name;
end;

procedure TSGTestFrm.Band1StartValueSpeChange(Sender: TObject);
begin
    SuperGauge.BandSettings1.StartValue := Band1StartValueSpe.Value;
end;

procedure TSGTestFrm.Band1TextColorCbChange(Sender: TObject);
begin
  SuperGauge.BandSettings1.TextColor := Band1TextColorCb.Selected;
end;

procedure TSGTestFrm.Band1TextEdtChange(Sender: TObject);
begin
  SuperGauge.BandSettings1.Text := Band1TextEdt.Text;
end;

procedure TSGTestFrm.Band1EnabledTextCbChange(Sender: TObject);
begin
    SuperGauge.BandSettings1.EnableText := Band1EnabledTextCb.Checked;
end;

procedure TSGTestFrm.Band1BandRadiusSpeChange(Sender: TObject);
begin
    SuperGauge.BandSettings1.BandRadius := Band1BandRadiusSpe.Value;
end;

procedure TSGTestFrm.Band1TextRadiusSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings1.TextRadius := Band1TextRadiusSpe.Value;
end;

procedure TSGTestFrm.Band1TextSizeSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings1.TextSize := Band1TextSizeSpe.Value;
end;

procedure TSGTestFrm.Band1TextStyleDDCbItemChange(Sender: TObject; AIndex: Integer);
var
  TextStyle: TFontStyles;
begin

  // Bold=fsBold, Italic=fsItalic, Strike Out=fsStrikeOut, Underline=fsUnderline
  // Changing the order in the dropdown will break this

  TextStyle := SuperGauge.BandSettings1.TextStyle;

    if Band1TextStyleDDCb.Checked[AIndex] then
      case Band1TextStyleDDCb.Items[AIndex] of
        'Bold'    : Include(TextStyle, fsBold);
        'Italic'  :  Include(TextStyle, fsItalic);
        'Strike Out' :  Include(TextStyle, fsStrikeOut);
        'Underline'  :  Include(TextStyle, fsUnderline); // may not work bug in BGRA??
      end
    else
      case Band1TextStyleDDCb.Items[AIndex] of
        'Bold'    : Exclude(TextStyle, fsBold);
        'Italic'  :  Exclude(TextStyle, fsItalic);
        'Strike Out' :  Exclude(TextStyle, fsStrikeOut);
        'Underline'  :  Exclude(TextStyle, fsUnderline);
      end;

  SuperGauge.BandSettings1.TextStyle := TextStyle;
end;

procedure TSGTestFrm.Band2BandRadiusSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings2.BandRadius := Band2BandRadiusSpe.Value;
end;

procedure TSGTestFrm.Band2EnabledCbChange(Sender: TObject);
begin
  SuperGauge.BandSettings2.Enabled := Band2EnabledCb.Checked;
end;

procedure TSGTestFrm.Band2EnabledTextCbChange(Sender: TObject);
begin
  SuperGauge.BandSettings2.EnableText := Band2EnabledTextCb.Checked;
end;

procedure TSGTestFrm.Band2EndValueSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings2.EndValue := Band2EndValueSpe.Value;
end;

procedure TSGTestFrm.Band2SetFontBtnClick(Sender: TObject);
begin
  if FontDialog1.Execute then
    SuperGauge.BandSettings2.TextFont := FontDialog1.Font.Name;
end;

procedure TSGTestFrm.Band2StartValueSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings2.StartValue := Band2StartValueSpe.Value;
end;

procedure TSGTestFrm.Band2TextColorCbChange(Sender: TObject);
begin
  SuperGauge.BandSettings2.TextColor := Band2TextColorCb.Selected;
end;

procedure TSGTestFrm.Band2TextEdtChange(Sender: TObject);
begin
  SuperGauge.BandSettings2.Text := Band2TextEdt.Text;
end;

procedure TSGTestFrm.Band2TextRadiusSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings2.TextRadius := Band2TextRadiusSpe.Value;
end;

procedure TSGTestFrm.Band2TextSizeSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings2.TextSize := Band2TextSizeSpe.Value;
end;

procedure TSGTestFrm.Band2TextStyleDDCbItemChange(Sender: TObject; AIndex: Integer);
var
  TextStyle: TFontStyles;
begin

  // Bold=fsBold, Italic=fsItalic, Strike Out=fsStrikeOut, Underline=fsUnderline
  // Changing the order in the dropdown will break this

  TextStyle := SuperGauge.BandSettings2.TextStyle;

    if Band1TextStyleDDCb.Checked[AIndex] then
      case Band1TextStyleDDCb.Items[AIndex] of
        'Bold'    : Include(TextStyle, fsBold);
        'Italic'  :  Include(TextStyle, fsItalic);
        'Strike Out' :  Include(TextStyle, fsStrikeOut);
        'Underline'  :  Include(TextStyle, fsUnderline); // may not work bug in BGRA??
      end
    else
      case Band1TextStyleDDCb.Items[AIndex] of
        'Bold'    : Exclude(TextStyle, fsBold);
        'Italic'  :  Exclude(TextStyle, fsItalic);
        'Strike Out' :  Exclude(TextStyle, fsStrikeOut);
        'Underline'  :  Exclude(TextStyle, fsUnderline);
      end;

  SuperGauge.BandSettings2.TextStyle := TextStyle;
end;

procedure TSGTestFrm.Band2ThicknessSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings2.BandThickness := Band2ThicknessSpe.Value;
end;

procedure TSGTestFrm.Band2ColorCbChange(Sender: TObject);
begin
  SuperGauge.BandSettings2.BandColor := Band2ColorCb.Selected;
end;

procedure TSGTestFrm.Band3BandRadiusSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings3.BandRadius := Band3BandRadiusSpe.Value;
end;

procedure TSGTestFrm.Band3ColorCbChange(Sender: TObject);
begin
  SuperGauge.BandSettings3.BandColor := Band3ColorCb.Selected;
end;

procedure TSGTestFrm.Band3EnabledCbChange(Sender: TObject);
begin
  SuperGauge.BandSettings3.Enabled := Band3EnabledCb.Checked;
end;

procedure TSGTestFrm.Band3EnabledTextCbChange(Sender: TObject);
begin
  SuperGauge.BandSettings3.EnableText := Band3EnabledTextCb.Checked;
end;

procedure TSGTestFrm.Band3EndValueSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings3.EndValue := Band3EndValueSpe.Value;
end;

procedure TSGTestFrm.Band3SetFontBtnClick(Sender: TObject);
begin
  if FontDialog1.Execute then
    SuperGauge.BandSettings3.TextFont := FontDialog1.Font.Name;
end;

procedure TSGTestFrm.Band3StartValueSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings3.StartValue := Band3StartValueSpe.Value;
end;

procedure TSGTestFrm.Band3TextColorCbChange(Sender: TObject);
begin
  SuperGauge.BandSettings3.TextColor := Band3TextColorCb.Selected;
end;

procedure TSGTestFrm.Band3TextEdtChange(Sender: TObject);
begin
  SuperGauge.BandSettings3.Text := Band3TextEdt.Text;
end;

procedure TSGTestFrm.Band3TextRadiusSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings3.TextRadius := Band3TextRadiusSpe.Value;
end;

procedure TSGTestFrm.Band3TextSizeSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings3.TextSize := Band3TextSizeSpe.Value;
end;

procedure TSGTestFrm.Band3TextStyleDDCbItemChange(Sender: TObject; AIndex: Integer);
var
  TextStyle: TFontStyles;
begin

  // Bold=fsBold, Italic=fsItalic, Strike Out=fsStrikeOut, Underline=fsUnderline
  // Changing the order in the dropdown will break this

  TextStyle := SuperGauge.BandSettings3.TextStyle;

    if Band1TextStyleDDCb.Checked[AIndex] then
      case Band1TextStyleDDCb.Items[AIndex] of
        'Bold'    : Include(TextStyle, fsBold);
        'Italic'  :  Include(TextStyle, fsItalic);
        'Strike Out' :  Include(TextStyle, fsStrikeOut);
        'Underline'  :  Include(TextStyle, fsUnderline); // may not work bug in BGRA??
      end
    else
      case Band1TextStyleDDCb.Items[AIndex] of
        'Bold'    : Exclude(TextStyle, fsBold);
        'Italic'  :  Exclude(TextStyle, fsItalic);
        'Strike Out' :  Exclude(TextStyle, fsStrikeOut);
        'Underline'  :  Exclude(TextStyle, fsUnderline);
      end;

  SuperGauge.BandSettings3.TextStyle := TextStyle;
end;

procedure TSGTestFrm.Band3ThicknessSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings3.BandThickness := Band3ThicknessSpe.Value;
end;

procedure TSGTestFrm.Band4BandRadiusSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings4.BandRadius := Band4BandRadiusSpe.Value;
end;

procedure TSGTestFrm.Band4ColorCbChange(Sender: TObject);
begin
  SuperGauge.BandSettings4.BandColor := Band4ColorCb.Selected;
end;

procedure TSGTestFrm.Band4EnabledCbChange(Sender: TObject);
begin
  SuperGauge.BandSettings4.Enabled := Band4EnabledCb.Checked;
end;

procedure TSGTestFrm.Band4EnabledTextCbChange(Sender: TObject);
begin
  SuperGauge.BandSettings4.EnableText := Band4EnabledTextCb.Checked;
end;

procedure TSGTestFrm.Band4EndValueSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings4.EndValue := Band4EndValueSpe.Value;
end;

procedure TSGTestFrm.Band4SetFontBtnClick(Sender: TObject);
begin
  if FontDialog1.Execute then
    SuperGauge.BandSettings4.TextFont := FontDialog1.Font.Name;
end;

procedure TSGTestFrm.Band4StartValueSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings4.StartValue := Band4StartValueSpe.Value;
end;

procedure TSGTestFrm.Band4TextColorCbChange(Sender: TObject);
begin
  SuperGauge.BandSettings4.TextColor := Band4TextColorCb.Selected;
end;

procedure TSGTestFrm.Band4TextEdtChange(Sender: TObject);
begin
  SuperGauge.BandSettings4.Text := Band4TextEdt.Text;
end;

procedure TSGTestFrm.Band4TextRadiusSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings4.TextRadius := Band4TextRadiusSpe.Value;
end;

procedure TSGTestFrm.Band4TextSizeSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings4.TextSize := Band4TextSizeSpe.Value;
end;

procedure TSGTestFrm.Band4TextStyleDDCbItemChange(Sender: TObject; AIndex: Integer);
var
  TextStyle: TFontStyles;
begin

  // Bold=fsBold, Italic=fsItalic, Strike Out=fsStrikeOut, Underline=fsUnderline
  // Changing the order in the dropdown will break this

  TextStyle := SuperGauge.BandSettings4.TextStyle;

    if Band1TextStyleDDCb.Checked[AIndex] then
      case Band1TextStyleDDCb.Items[AIndex] of
        'Bold'    : Include(TextStyle, fsBold);
        'Italic'  :  Include(TextStyle, fsItalic);
        'Strike Out' :  Include(TextStyle, fsStrikeOut);
        'Underline'  :  Include(TextStyle, fsUnderline); // may not work bug in BGRA??
      end
    else
      case Band1TextStyleDDCb.Items[AIndex] of
        'Bold'    : Exclude(TextStyle, fsBold);
        'Italic'  :  Exclude(TextStyle, fsItalic);
        'Strike Out' :  Exclude(TextStyle, fsStrikeOut);
        'Underline'  :  Exclude(TextStyle, fsUnderline);
      end;

  SuperGauge.BandSettings4.TextStyle := TextStyle;
end;
procedure TSGTestFrm.Band4ThicknessSpeChange(Sender: TObject);
begin
  SuperGauge.BandSettings4.BandThickness := Band4ThicknessSpe.Value;
end;

procedure TSGTestFrm.BGRAKnobAuxDblClick(Sender: TObject);
begin
  SuperGauge.AuxPointerSettings.Enabled := not SuperGauge.AuxPointerSettings.Enabled;
  UpdateAuxPointerStats;
end;

procedure TSGTestFrm.BGRAKnobAuxValueChanged(Sender: TObject; Value: single);
begin
  
  if TryToRoundValueCb.State = cbChecked then
    SuperGauge.AuxValue := Round(Value)
  else
    SuperGauge.AuxValue := Value;

  UpdatePositionStats;
end;

procedure TSGTestFrm.BGRAKnobDblClick(Sender: TObject);
begin
  SuperGauge.PointerSettings.Enabled := not SuperGauge.PointerSettings.Enabled;
  UpdatePointerStats;
end;

procedure TSGTestFrm.BGRAKnobValueChanged(Sender: TObject; Value: single);
begin

  if TryToRoundValueCb.State = cbChecked then
    SuperGauge.Value := Round(Value)
  else
    SuperGauge.Value := Value;

  // Move markers, the timer tics allow the markers have a peak
  // and hold effect. The reset counters will reset if the values
  // have updated. If no updates the counters will eventually
  // trigger a reset and the markers will snap back to the pointer.
  //
  // Why is this not part of the gauge? It might be but I was concerned
  // with a situation with having a bunch of gauges each with a timer. The
  // better solution is to have an external controller doing the reset with
  // one timer reseting all the gauges. Was thinking about a TSGController or
  // something like that TBD, but for now here is what I did here.

  // Move the High side valve (Red)

  if SuperGauge.Value > SuperGauge.MarkerSettings1.Value  then
  begin
    SuperGauge.MarkerSettings1.Value := SuperGauge.Value;
    ResetHiTics := 0; // reset since we just bumped
  end;

  // Move Sticky Peak (Yellow)

  if SuperGauge.Value > SuperGauge.MarkerSettings3.Value  then
  begin
    SuperGauge.MarkerSettings3.Value := SuperGauge.Value;
  end;

  // Move the Low side valve (Green)

  if SuperGauge.Value < SuperGauge.MarkerSettings2.Value  then
  begin
    SuperGauge.MarkerSettings2.Value := SuperGauge.Value;
    ResetLoTics := 0; // reset since we just bumped
  end;

  // if we have 10 tics without a bump, reset them

  if ResetHiTics > 9 then
  begin
    ResetHiTics := 0;
    SuperGauge.MarkerSettings1.Value := SuperGauge.Value;
  end;

  if ResetLoTics > 9 then
  begin
    ResetLoTics := 0;
    SuperGauge.MarkerSettings2.Value := SuperGauge.Value;
  end;

  UpdatePositionStats;

  if (SuperGauge.MarkerSettings1.Enabled)
    or (SuperGauge.MarkerSettings2.Enabled)
    or (SuperGauge.MarkerSettings3.Enabled)
  then
    UpdateMarkerStats;
end;

procedure TSGTestFrm.BandEnabledCbChange(Sender: TObject);
begin
  SuperGauge.BandSettings1.Enabled := Band1EnabledCb.Checked;
end;

procedure TSGTestFrm.CapEdgeWidthSpeChange(Sender: TObject);
begin
  SuperGauge.PointerCapSettings.EdgeWidth := CapEdgeWidthSpe.Value;
  UpdateCapStats;
end;

procedure TSGTestFrm.ComboBox1Change(Sender: TObject);
var
  saveValue, saveAuxValue: single;

begin
  // Always reset to GaugeDefault values then modify from that point. If
  // defaults change need to fix up here. Save a lot of typing!

  saveValue := SuperGauge.Value; // Save off the value, if we do change MinValue/MaxValue it could reset the gauges value!
  saveAuxValue := SuperGauge.AuxValue;

  GaugeDefaults;

  with SuperGauge do
  begin
    case ComboBox1.ItemIndex of

    0 : // Reset to default values
        begin
        end;

    1,  // Simple
    2:  // Fancy Pointer
        begin
         PointerCapSettings.CapPosition := cpOver;
         PointerSettings.Style := psLine;
         if ComboBox1.ItemIndex = 2 then
          begin
            PointerSettings.Width := 8;
            PointerSettings.HighlightLine := True;
            PointerSettings.HighlightColor := clSilver;
            PointerSettings.HighlightThickness := 3;
          end;
        end;

    3 : // Simple Arc style with 2 'pointer arc'
        begin
          PointerSettings.Style := psArc;
          PointerSettings.Width := 10;
          PointerSettings.Length := 130;
          AuxPointerSettings.Style := psArc;
          AuxPointerSettings.Width := 10;
          AuxPointerSettings.Length := 115;
          AuxPointerSettings.Enabled := True;
          ScaleSettings.MainTickUseDots := True;
          ScaleSettings.MainTickLength := 10;
          ScaleSettings.EnableSubTicks := False;
          ScaleSettings.ScaleRadius := 105;
          ScaleSettings.TextRadius := 80;
          ScaleSettings.MainTickLength := 10;
          ScaleSettings.TickArcStyle := taNone;
          PointerCapSettings.CapStyle := csNone;
          TextSettings1.Text := 'Pressure';
          TextSettings1.OffsetX := 0;
          TextSettings1.OffsetY := -25;
          TextSettings1.Enabled := True;
          TextSettings1.FontEx.Height := 24;
          TextSettings1.FontEx.Color := $004080FF;
          TextSettings3.Text := 'Temperature';
          TextSettings3.OffsetX := 0;
          TextSettings3.OffsetY := 5;
          TextSettings3.Enabled := True;
          TextSettings3.FontEx.Height := 24;
          TextSettings3.FontEx.Color := clRed;
        end;

    4,  // Simple Bands and Markers (Single Pointer and other stuff)
    5 : // Same but set up second pointer
        begin

          if ComboBox1.ItemIndex = 4 then
            AuxPointerSettings.Enabled := False
          else
            begin
              AuxPointerSettings.Length := 100;
              AuxPointerSettings.Width := 7;
              AuxPointerSettings.Enabled := True;
              AuxPointerSettings.Color := clRed;
            end;

          AuxScaleSettings.Enabled := False;

          BandSettings1.BandColor := clGreen;
          BandSettings1.Text := 'GOOD';
          BandSettings1.TextColor := clWhite;
          BandSettings1.StartValue := 0;
          BandSettings1.EndValue := 45;
          BandSettings1.TextStyle := [fsBold];
          BandSettings1.EnableText := True;
          BandSettings1.BandRadius := 85;
          BandSettings1.BandThickness := 25;

          BandSettings2.BandColor := clYellow;
          BandSettings2.Text := '?';
          BandSettings2.TextColor := clBlack;
          BandSettings2.StartValue := 45;
          BandSettings2.EndValue := 55;
          BandSettings2.TextStyle := [fsBold];
          BandSettings2.EnableText := True;
          BandSettings2.BandRadius := 85;
          BandSettings2.BandThickness := 25;

          BandSettings3.BandColor := clRed;
          BandSettings3.Text := 'BAD';
          BandSettings3.TextColor := clWhite;
          BandSettings3.StartValue := 55;
          BandSettings3.EndValue := 100;
          BandSettings3.TextStyle := [fsBold];
          BandSettings3.EnableText := True;
          BandSettings3.BandRadius := 85;
          BandSettings3.BandThickness := 25;

          // Save for last so only redraws less

          BandSettings1.Enabled := True;
          BandSettings2.Enabled := True;
          BandSettings3.Enabled := True;
          BandSettings4.Enabled := False;

          MarkerSettings1.Color := clLime;
          MarkerSettings1.Style := msRight;
          MarkerSettings1.Value := SuperGauge.Value;
          MarkerSettings2.Color := clRed;
          MarkerSettings2.Style := msLeft;
          MarkerSettings2.Value := SuperGauge.Value;
          MarkerSettings3.Color := clYellow;
          MarkerSettings3.Style := msCenter;
          MarkerSettings3.Width := 5;
          MarkerSettings3.Radius := 135;
          MarkerSettings3.Value := SuperGauge.Value;

          MarkerSettings1.Enabled := True;
          MarkerSettings2.Enabled := True;
          MarkerSettings3.Enabled := True;

          RangeLEDSettings.Style := lsShaded;
          RangeLEDSettings.RangeStartValue := 55;
          RangeLEDSettings.RangeEndValue := 100;
          RangeLEDSettings.RangeType := rcGreaterStart;

          TextSettings1.Text := 'RPM';
          TextSettings1.OffsetX := 0;
          TextSettings1.OffsetY := -35;
          TextSettings2.Text := '--';
          TextSettings2.OffsetX := 0;
          TextSettings2.OffsetY := 40;
          TextSettings2.FontEx.Color := $004080FF; // match pointer

          TextSettings1.Enabled := True;
          TextSettings2.Enabled := True;
        end;

    6: // Dual Pointers Dual Scales
        Begin
          PointerSettings.Width := 8;
          PointerSettings.HighlightLine := True;
          PointerSettings.HighlightThickness := 3;
          PointerSettings.Color := $004080FF; // orange
          PointerSettings.HighlightColor := $007DC4DF; // goldish
          AuxPointerSettings.Width := 8;
          AuxPointerSettings.HighlightLine := True;
          AuxPointerSettings.HighlightColor := clMedGray;
          AuxPointerSettings.HighlightThickness := 3;
          AuxPointerSettings.Color := clRed;
          AuxPointerSettings.Length := 75;

          AuxScaleSettings.ScaleRadius := 70;
          AuxScaleSettings.TextColor := clWhite;
          AuxScaleSettings.TextSize := 14;
          AuxScaleSettings.TextRadius := 45;
          AuxScaleSettings.TickColor := clRed;
          AuxScaleSettings.TickArcColor := clRed;
          AuxScaleSettings.Start := -5; // do a -5 to 5 scale for the heck of it

          AuxPointerSettings.Enabled := True;
          AuxScaleSettings.Enabled := True;
        end;

    7: // Cartoonish
        Begin
          PointerSettings.Style := psTriangle;
          PointerSettings.Width := 15;
          PointerSettings.HighlightLine := True;
          PointerSettings.HighlightColor := clYellow;
          PointerSettings.HighlightThickness := 4;

          PointerCapSettings.CapStyle := csFlat;
          PointerCapSettings.CapPosition := cpOver;
          PointerCapSettings.Radius := 25;
          PointerCapSettings.EdgeColor := clRed;
          PointerCapSettings.EdgeWidth := 5;

          ScaleSettings.MainTickThickness := 5;
          ScaleSettings.SubTickThickness := 3;
          ScaleSettings.TextRadius := 85;
          ScaleSettings.TextSize := 30;
          ScaleSettings.TextColor := clLime;
          ScaleSettings.TickArcColor := clRed;
          ScaleSettings.TickColor := clYellow;

          FaceSettings.FillStyle := fsFlat;
          FaceSettings.InnerColor := clNavy;

          TextSettings2.Enabled := True;
          TextSettings2.FontEx.Color := clAqua;

          BandSettings1.BandColor := clRed;
          BandSettings1.StartValue := 75;
          BandSettings1.BandThickness := 15;
          BandSettings1.BandRadius := 125;
          BandSettings1.Enabled := True;

          RangeLEDSettings.Style := lsFlat;
          RangeLEDSettings.Shape := lshSquare;
          RangeLEDSettings.InactiveColor := clGray;
          RangeLEDSettings.BorderColor := clAqua;
          RangeLEDSettings.RangeStartValue := 75;
          RangeLEDSettings.RangeType := rcGreaterStart;

          MarkerSettings1.Color := clLime;
          MarkerSettings1.Width := 10;
          MarkerSettings1.Height := 23;
          MarkerSettings1.Radius := 128;
          MarkerSettings1.Style := msCenter;

          MarkerSettings2.Color := clRed;
          MarkerSettings2.Width := 10;
          MarkerSettings2.Height := 23;
          MarkerSettings2.Radius := 128;
          MarkerSettings2.Style := msCenter;

          MarkerSettings3.Color := clYellow;
          MarkerSettings3.Width := 6;
          MarkerSettings3.Height := 32;
          MarkerSettings3.Radius := 132;
          MarkerSettings3.Style := msCenter;

          MarkerSettings1.Enabled := True; // Local Peak indicator
          MarkerSettings2.Enabled := True;
          MarkerSettings3.Enabled := True; // Max Value Indicator
        end;
    8: // Backwards Scale
        begin
          ScaleSettings.ReverseScale := True;

          // Tricky bit, reverse the range too! Note that since we may
          // have the current settings of MinValue=0, and MaxValue=100
          // When we change either and both end up being the same we will
          // get a RESET of the gauges value since a MinValue=MaxValue is
          // an internal soft exception case and will set the Gauges value so
          // we need to flim flam a bit to restore it later.

          MinValue := 100;  // This triggers the reset of the value, remember MaxValue = 100 too!
          MaxValue := 0;
        end;
    end;

    // Restor the value of the gauge. This is needed since we may be changing
    // the Min/MaxValue and this could reset it. So use the saved value!

    Value := saveValue;
    AuxValue := SaveAuxValue;

  end;

  UpdateAllStats;
end;

procedure TSGTestFrm.FrameInnerFrameColorCbChange(Sender: TObject);
begin
    SuperGauge.FrameSettings.InnerFrameColor := FrameInnerFrameColorCb.Selected;
end;

procedure TSGTestFrm.FrameInnerFrameThicknessSpeChange(Sender: TObject);
begin
  SuperGauge.FrameSettings.InnerFrameThickness := FrameInnerFrameThicknessSpe.Value;
end;

procedure TSGTestFrm.FrameMiddleFrameColorCbChange(Sender: TObject);
begin
    SuperGauge.FrameSettings.MiddleFrameColor := FrameMiddleFrameColorCb.Selected;
end;

procedure TSGTestFrm.FrameMiddleFrameThicknessSpeChange(Sender: TObject);
begin
  SuperGauge.FrameSettings.MiddleFrameThickness := FrameMiddleFrameThicknessSpe.Value;
end;

procedure TSGTestFrm.MenuItem1Click(Sender: TObject);
begin
  // help, you need a default browser in your O/S for this to work

  ShowHelpOrErrorForKeyword('', 'HTML/index.html');
end;

procedure TSGTestFrm.PointerEnabledCbChange(Sender: TObject);
begin
  SuperGauge.PointerSettings.Enabled := PointerEnabledCb.Checked;

  if SuperGauge.PointerSettings.Enabled then
    BGRAKnob.KnobColor := clBtnFace
  else
    BGRAKnob.KnobColor := clRed;
end;

procedure TSGTestFrm.PointerHighlightCbChange(Sender: TObject);
begin
  SuperGauge.PointerSettings.HighlightLine := PointerHighlightCb.Checked;
end;

procedure TSGTestFrm.AuxPointerHighlightColorCbChange(Sender: TObject);
begin
  SuperGauge.AuxPointerSettings.HighlightColor := AuxPointerHighlightColorCb.Selected;
end;

procedure TSGTestFrm.PointerHighlightColorCbChange(Sender: TObject);
begin
    SuperGauge.PointerSettings.HighlightColor := PointerHighlightColorCb.Selected;
end;

procedure TSGTestFrm.AuxPointerHighlightThicknessSpeChange(Sender: TObject);
begin
    SuperGauge.AuxPointerSettings.HighlightThickness := AuxPointerHighlightThicknessSpe.Value;
end;

procedure TSGTestFrm.PointerHighlightThicknessSpeChange(Sender: TObject);
begin
  SuperGauge.PointerSettings.HighlightThickness := PointerHighlightThicknessSpe.Value;
end;

procedure TSGTestFrm.AuxScaleEnabledCbChange(Sender: TObject);
begin
  SuperGauge.AuxScaleSettings.Enabled := AuxScaleEnabledCb.Checked;
end;

procedure TSGTestFrm.RangeLEDActiveCbChange(Sender: TObject);
begin
  SuperGauge.RangeLEDSettings.Active := RangeLEDActiveCb.Checked;
  UpdateRangeLEDStats;
end;

procedure TSGTestFrm.ScaleEnabledCbChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.Enabled := ScaleEnabledCb.Checked;
end;

procedure TSGTestFrm.ScaleMainTickUseDotsCbChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.MainTickUseDots := ScaleMainTickUseDotsCb.Checked;
end;

procedure TSGTestFrm.ScaleSubTickUseDotsCbChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.SubTickUseDots := ScaleSubTickUseDotsCb.Checked;
end;

procedure TSGTestFrm.DisableAllMarkersBtnClick(Sender: TObject);
begin
  SuperGauge.MarkerSettings1.Enabled := False;
  SuperGauge.MarkerSettings2.Enabled := False;
  SuperGauge.MarkerSettings3.Enabled := False;
  UpdateMarkerStats;
end;

procedure TSGTestFrm.EnableAllMarkersBtnClick(Sender: TObject);
begin
  SuperGauge.MarkerSettings1.Enabled := True;
  SuperGauge.MarkerSettings2.Enabled := True;
  SuperGauge.MarkerSettings3.Enabled := True;
  UpdateMarkerStats;
end;

procedure TSGTestFrm.ExitSubMenuClick(Sender: TObject);
begin
  Close;
end;

procedure TSGTestFrm.FaceCurveExponentSpeChange(Sender: TObject);
begin
  SuperGauge.FaceSettings.CurveExponent := FaceCurveExponentSpe.Value;
  UpdateFaceStats;
end;

procedure TSGTestFrm.FaceLightIntensitySpeChange(Sender: TObject);
begin
  SuperGauge.FaceSettings.LightIntensity := FaceLightIntensitySpe.Value;
  UpdateFaceStats;
end;

procedure TSGTestFrm.RoundBtnClick(Sender: TObject);
begin
  // try to round floating point, not always possible

  BGRAKnobValueChanged(Sender, Round(SuperGauge.Value));
  BGRAKnobAuxValueChanged(Sender, Round(SuperGauge.AuxValue));
end;

procedure TSGTestFrm.LoadImageBtnClick(Sender: TObject);
begin
  // Load face picture

  if OpenPictureDialog1.Execute then
  begin
    if fileExists(OpenPictureDialog1.Filename) then
    begin
      SuperGauge.FaceSettings.Picture.LoadFromFile(OpenPictureDialog1.Filename);
    end;
  end
else
  ShowMessage('No Picture Selected');
end;

procedure TSGTestFrm.MarkerZeroAllBtnClick(Sender: TObject);
begin
  SuperGauge.MarkerSettings1.Value := 0;
  SuperGauge.MarkerSettings2.Value := 0;
  SuperGauge.MarkerSettings3.Value := 0;
  UpdateMarkerStats;
end;

procedure TSGTestFrm.Marker1ValueClrBtnClick(Sender: TObject);
begin
  Marker1ValueSpe.Value := 0.0;
  SuperGauge.MarkerSettings1.Value := 0.0;
end;

procedure TSGTestFrm.Marker2ValueClrBtnClick(Sender: TObject);
begin
    Marker2ValueSpe.Value := 0.0;
    SuperGauge.MarkerSettings2.Value := 0.0;
end;

procedure TSGTestFrm.MaxValueSpeChange(Sender: TObject);
begin
  SuperGauge.MaxValue := MaxValueSpe.Value;
  UpdatePositionStats;
end;

procedure TSGTestFrm.MinValueBtnClick(Sender: TObject);
begin
  BGRAKnobValueChanged(Sender, SuperGauge.MinValue);
end;

procedure TSGTestFrm.MaxValueBtnClick(Sender: TObject);
begin
  BGRAKnobValueChanged(Sender, SuperGauge.MaxValue);
end;

procedure TSGTestFrm.MinValueSpeChange(Sender: TObject);
begin
  SuperGauge.MinValue := MinValueSpe.Value;
  UpdatePositionStats;
end;

procedure TSGTestFrm.Marker3ValueClrBtnClick(Sender: TObject);
begin
  Marker3ValueSpe.Value := 0.0;
  SuperGauge.MarkerSettings3.Value := 0.0;
end;

procedure TSGTestFrm.RandomBtnClick(Sender: TObject);
begin
  // similar to the MarkerRandomTestBTN code except it doesn't force markers
  // to be on. By default they are in the gauge props

  SuperGauge.Value := 0.0;              // Always reset
  SuperGauge.AuxValue := 0;             // Same!
  MarkerZeroAllBtnClick(nil);
  ResetLoTics := 10;                    // 10 forces update now unless timer stopped
  ResetHiTics := 10;
  Timer2.Enabled := not Timer2.Enabled; // toggle timer
  uELED1.Active:=Timer2.Enabled;
end;

procedure TSGTestFrm.MarkerRandomTestBtnClick(Sender: TObject);
begin
  // turn on marks and then random it up, like RandomBtnClick

  SuperGauge.MarkerSettings1.Enabled := True;
  SuperGauge.MarkerSettings2.Enabled := True;
  SuperGauge.MarkerSettings3.Enabled := True;
  MarkerZeroAllBtnClick(Sender);
  RandomBtn.Click;
end;

procedure TSGTestFrm.CapCurveExponentSpeChange(Sender: TObject);
begin
  SuperGauge.PointerCapSettings.CurveExponent := CapCurveExponentSpe.Value;
end;

procedure TSGTestFrm.CapEdgeColorCbChange(Sender: TObject);
begin
  SuperGauge.PointerCapSettings.EdgeColor := CapEdgeColorCb.Selected;
end;

procedure TSGTestFrm.CapFillColorCbChange(Sender: TObject);
begin
  SuperGauge.PointerCapSettings.FillColor := CapFillColorCb.Selected;
end;

procedure TSGTestFrm.CapLightIntensitySpeChange(Sender: TObject);
begin
  SuperGauge.PointerCapSettings.LightIntensity := CapLightIntensitySpe.Value;
  UpdateCapStats;
end;

procedure TSGTestFrm.CapPositionCbChange(Sender: TObject);
begin
  // Set the Pointer Cap position over the pointer or under it
  // cpUnder = 0, cpOver = 1

  case CapPositionCb.ItemIndex of
    0 : {cpUnder}
        begin
          SuperGauge.PointerCapSettings.CapPosition := cpUnder;
        end;
    1 : {cpOver}
        begin
          SuperGauge.PointerCapSettings.CapPosition := cpOver;
        end;
  else
    // Unknown type, warn somewhere...
  end;

  UpdateCapStats;
end;

procedure TSGTestFrm.CapRadiusSpeChange(Sender: TObject);
begin
  SuperGauge.PointerCapSettings.Radius := CapRadiusSpe.Value;
  UpdateCapStats;
end;

procedure TSGTestFrm.CapStyleCbChange(Sender: TObject);
begin
  // csNone = 0, csFlat = 1, csShaded = 2, csPhong = 3

  case CapStyleCb.ItemIndex of
    0 : {csNone}
        begin
          SuperGauge.PointerCapSettings.CapStyle := csNone;
        end;
    1 : {csFlat}
        begin
          SuperGauge.PointerCapSettings.CapStyle := csFlat;
        end;
    2 : {csShaded}
        begin
          SuperGauge.PointerCapSettings.CapStyle := csShaded;
        end;
    3 : {csPhong}
        begin
          SuperGauge.PointerCapSettings.CapStyle := csPhong;
        end;
  else
    // Unknown type, warn somewhere...
  end;

  UpdateCapStats;
end;

procedure TSGTestFrm.EnableScaleTextCbChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.EnableScaleText := EnableScaleTextCb.Checked;
end;

procedure TSGTestFrm.FaceInnerColorCbChange(Sender: TObject);
begin
  // update end color

  SuperGauge.FaceSettings.InnerColor := FaceInnerColorCb.Selected;
end;

procedure TSGTestFrm.FaceOuterColorCbChange(Sender: TObject);
begin
  // update start color

  SuperGauge.FaceSettings.OuterColor := FaceOuterColorCb.Selected;
end;

procedure TSGTestFrm.FaceFillStyleCbChange(Sender: TObject);
begin
  // update Face fill style
  // fsNone = 0, fsGradient = 1, fsFlat = 2, fsPhong = 3

  case FaceFillStyleCb.ItemIndex of
    0 : {fsNone}
        begin
          SuperGauge.FaceSettings.FillStyle := fsNone;
        end;
    1 : {fsGradient}
        begin
          SuperGauge.FaceSettings.FillStyle := fsGradient;
        end;
    2: {fsFlat}
        begin
          SuperGauge.FaceSettings.FillStyle := fsFlat;
        end;
    3: {fsPhong}
        begin
          SuperGauge.FaceSettings.FillStyle := fsPhong;
        end
  else
    // Unknown type, warn somewhere...
  end;

  UpdateFaceStats;

end;

procedure TSGTestFrm.FacePictureEnabledCbChange(Sender: TObject);
begin
  // Enable/Disable Picture on Face

  SuperGauge.FaceSettings.PictureEnabled := FacePictureEnabledCb.Checked;
end;

procedure TSGTestFrm.FacePictureOffsetXSpeChange(Sender: TObject);
begin
  // update face picture x offset

  SuperGauge.FaceSettings.PictureOffsetX := FacePictureOffsetXSpe.Value;
end;

procedure TSGTestFrm.FacePictureOffsetYSpeChange(Sender: TObject);
begin
  // update face picture y offset

  SuperGauge.FaceSettings.PictureOffsetY := FacePictureOffsetYSpe.Value;
end;

procedure TSGTestFrm.FrameOuterFrameColorCbChange(Sender: TObject);
begin
  SuperGauge.FrameSettings.OuterFrameColor := FrameOuterFrameColorCb.Selected;
end;

procedure TSGTestFrm.FrameOuterFrameThicknessSpeChange(Sender: TObject);
begin
  SuperGauge.FrameSettings.OuterFrameThickness := FrameOuterFrameThicknessSpe.Value;
end;

procedure TSGTestFrm.Marker1ColorCbChange(Sender: TObject);
begin
  SuperGauge.MarkerSettings1.Color := Marker1ColorCb.Selected;
  UpdateMarkerStats;
end;

procedure TSGTestFrm.Marker1EnabledCbChange(Sender: TObject);
begin
    SuperGauge.MarkerSettings1.Enabled := Marker1EnabledCb.Checked;
    UpdateMarkerStats;
end;

procedure TSGTestFrm.Marker1HeightSpeChange(Sender: TObject);
begin
  SuperGauge.MarkerSettings1.Height := Marker1HeightSpe.Value;
  UpdateMarkerStats;
end;

procedure TSGTestFrm.Marker1RadiusSpeChange(Sender: TObject);
begin
  SuperGauge.MarkerSettings1.Radius := Marker1RadiusSpe.Value;
  UpdateMarkerStats;
end;

procedure TSGTestFrm.Marker1StyleCbChange(Sender: TObject);
begin
  // msCenter = 0, msLeft = 1, msRight = 2

  case Marker1StyleCb.ItemIndex of
    0 : {msCenter}
        begin
          SuperGauge.MarkerSettings1.Style := msCenter;
        end;
    1 : {msLeft}
        begin
            SuperGauge.MarkerSettings1.Style := msLeft;
        end;
    2 : {msRight}
        begin
          SuperGauge.MarkerSettings1.Style := msRight;
        end
  else
      // some error message with invalid marker setting
  end;

  UpdateMarkerStats;

end;

procedure TSGTestFrm.Marker1ValueSpeChange(Sender: TObject);
begin
  SuperGauge.MarkerSettings1.Value := Marker1ValueSpe.Value;
end;

procedure TSGTestFrm.Marker1WidthSpeChange(Sender: TObject);
begin
  SuperGauge.MarkerSettings1.Width := Marker1WidthSpe.Value;
end;

procedure TSGTestFrm.Marker2ColorCbChange(Sender: TObject);
begin
  SuperGauge.MarkerSettings2.Color := Marker2ColorCb.Selected;
  UpdateMarkerStats;
end;

procedure TSGTestFrm.Marker2EnabledCbChange(Sender: TObject);
begin
  SuperGauge.MarkerSettings2.Enabled := Marker2EnabledCb.Checked;
  UpdateMarkerStats;
end;

procedure TSGTestFrm.Marker2HeightSpeChange(Sender: TObject);
begin
  SuperGauge.MarkerSettings2.Height := Marker2HeightSpe.Value;
  UpdateMarkerStats;
end;

procedure TSGTestFrm.Marker2RadiusSpeChange(Sender: TObject);
begin
  SuperGauge.MarkerSettings2.Radius := Marker2RadiusSpe.Value;
  UpdateMarkerStats;
end;

procedure TSGTestFrm.Marker2StyleCbChange(Sender: TObject);
begin
  // msCenter = 0, msLeft = 1, msRight = 2

  case Marker2StyleCb.ItemIndex of
    0 : {msCenter}
        begin
          SuperGauge.MarkerSettings2.Style := msCenter;
        end;
    1 : {msLeft}
        begin
            SuperGauge.MarkerSettings2.Style := msLeft;
        end;
    2 : {msRight}
        begin
          SuperGauge.MarkerSettings2.Style := msRight;
        end
  else
      // some error message with invalid marker setting
  end;

  UpdateMarkerStats;
end;

procedure TSGTestFrm.Marker2ValueSpeChange(Sender: TObject);
begin
  SuperGauge.MarkerSettings2.Value := Marker2ValueSpe.Value;
end;

procedure TSGTestFrm.Marker2WidthSpeChange(Sender: TObject);
begin
  SuperGauge.MarkerSettings2.Width := Marker2WidthSpe.Value;
end;

procedure TSGTestFrm.Marker3ColorCbChange(Sender: TObject);
begin
  SuperGauge.MarkerSettings3.Color := Marker3ColorCb.Selected;
  UpdateMarkerStats;
end;

procedure TSGTestFrm.Marker3EnabledCbChange(Sender: TObject);
begin
  SuperGauge.MarkerSettings3.Enabled := Marker3EnabledCb.Checked;
  UpdateMarkerStats;
end;

procedure TSGTestFrm.Marker3HeightSpeChange(Sender: TObject);
begin
  SuperGauge.MarkerSettings3.Height := Marker3HeightSpe.Value;
  UpdateMarkerStats;
end;

procedure TSGTestFrm.Marker3RadiusSpeChange(Sender: TObject);
begin
  SuperGauge.MarkerSettings3.Radius := Marker3RadiusSpe.Value;
  UpdateMarkerStats;
end;

procedure TSGTestFrm.Marker3StyleCbChange(Sender: TObject);
begin
  // msCenter = 0, msLeft = 1, msRight = 2

  case Marker3StyleCb.ItemIndex of
    0 : {msCenter}
        begin
          SuperGauge.MarkerSettings3.Style := msCenter;
        end;
    1 : {msLeft}
        begin
            SuperGauge.MarkerSettings3.Style := msLeft;
        end;
    2 : {msRight}
        begin
          SuperGauge.MarkerSettings3.Style := msRight;
        end
  else
      // some error message with invalid marker setting
  end;

  UpdateMarkerStats;
end;

procedure TSGTestFrm.Marker3ValueSpeChange(Sender: TObject);
begin
  SuperGauge.MarkerSettings3.Value := Marker3ValueSpe.Value;
end;

procedure TSGTestFrm.Marker3WidthSpeChange(Sender: TObject);
begin
  SuperGauge.MarkerSettings3.Width := Marker3WidthSpe.Value;
end;

procedure TSGTestFrm.PointerColorCbChange(Sender: TObject);
begin
  SuperGauge.PointerSettings.Color := PointerColorCb.Selected;
end;

procedure TSGTestFrm.PointerExtensionLengthSpeChange(Sender: TObject);
begin
  SuperGauge.PointerSettings.ExtensionLength := PointerExtensionLengthSpe.Value;
end;

procedure TSGTestFrm.PointerLengthSpeChange(Sender: TObject);
begin
  SuperGauge.PointerSettings.Length := PointerLengthSpe.Value;
end;

procedure TSGTestFrm.PointerStyleCbChange(Sender: TObject);
begin
  // Set the Pointer Style
  // psLine = 0, psLineExt = 1, psArc = 2, 3 = psTriangle

  case PointerStyleCb.ItemIndex of
    0 : {psLine}
        begin
          SuperGauge.PointerSettings.Style := psLine;
        end;
    1 : {psLineExt}
        begin
          SuperGauge.PointerSettings.Style := psLineExt;
        end;
    2 : {psArc}
        begin
          SuperGauge.PointerSettings.Style := psArc;
        end;
    3: {psTriangle}
        begin
          SuperGauge.PointerSettings.Style := psTriangle;
        end;

  else
    // Unknown type, warn somewhere...
  end;

  UpdatePointerStats;
end;

procedure TSGTestFrm.PointerWidthSpeChange(Sender: TObject);
begin
  SuperGauge.PointerSettings.Width := PointerWidthSpe.Value;
  UpdatePointerStats;
end;

procedure TSGTestFrm.FacePictureResetOffsetBtnClick(Sender: TObject);
begin
  SuperGauge.FaceSettings.PictureOffsetX := 0;
  SuperGauge.FaceSettings.PictureOffsetY := 0;
  UpdateFaceStats;
end;

procedure TSGTestFrm.ResetMinMaxBtnClick(Sender: TObject);
begin
  SuperGauge.MinValue := 0.0;
  SuperGauge.MaxValue := 100.0;
  SuperGauge.AuxMinValue := 0.0;
  SuperGauge.AuxMaxValue := 100;
  UpdateMinMaxStats;
end;

procedure TSGTestFrm.ScaleEnableMainTicksCbChange(Sender: TObject);
begin
    SuperGauge.ScaleSettings.EnableMainTicks := ScaleEnableMainTicksCb.Checked;
end;

procedure TSGTestFrm.ScaleEnableSubTicksCbChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.EnableSubTicks := ScaleEnableSubTicksCb.Checked;
end;

procedure TSGTestFrm.ScaleInnerTickArcThicknessSpeChange(Sender: TObject);
begin
    SuperGauge.ScaleSettings.InnerTickArcThickness := ScaleInnerTickArcThicknessSpe.Value;
end;

procedure TSGTestFrm.ScaleOuterTickArcThicknessSpeChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.OuterTickArcThickness := ScaleOuterTickArcThicknessSpe.Value;
end;

procedure TSGTestFrm.ScaleSubTickLengthSpeChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.SubTickLength := ScaleSubTickLengthSpe.Value;
end;

procedure TSGTestFrm.ScaleMainTickLengthSpeChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.MainTickLength := ScaleMainTickLengthSpe.Value;
end;

procedure TSGTestFrm.ScaleMainTickCountSpeChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.MainTickCount := ScaleMainTickCountSpe.Value;
end;

procedure TSGTestFrm.ScaleMainTickThicknessSpeChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.MainTickThickness := ScaleMainTickThicknessSpe.Value;
end;

procedure TSGTestFrm.ScaleReversedCbChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.ReverseScale := ScaleReversedCb.Checked;
end;

procedure TSGTestFrm.ScaleSubTickCountSpeChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.SubTickCount := ScaleSubTickCountSpe.Value;
end;

procedure TSGTestFrm.ScaleSubTickThicknessSpeChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.SubTickThickness := ScaleSubTickThicknessSpe.Value;
end;

procedure TSGTestFrm.ScaleTextColorColorCbChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.TextColor := ScaleTextColorColorCb.Selected;
end;

procedure TSGTestFrm.ScaleTextRadiusSpeChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.TextRadius := ScaleTextRadiusSpe.Value;
end;

procedure TSGTestFrm.ScaleTextSizeSpeChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.TextSize := ScaleTextSizeSpe.Value;
end;

procedure TSGTestFrm.ScaleTextStyleDDCbItemChange(Sender: TObject; AIndex: Integer);
var
  TextStyle: TFontStyles;

begin
  // Bold=fsBold, Italic=fsItalic, Strike Out=fsStrikeOut, Underline=fsUnderline
  // Changing the order in the dropdown will break this

  TextStyle := SuperGauge.ScaleSettings.TextStyle;

    if ScaleTextStyleDDCb.Checked[AIndex] then
      case ScaleTextStyleDDCb.Items[AIndex] of
        'Bold'    : Include(TextStyle, fsBold);
        'Italic'  :  Include(TextStyle, fsItalic);
        'Strike Out' :  Include(TextStyle, fsStrikeOut);
        'Underline'  :  Include(TextStyle, fsUnderline); // may not work bug in BGRA??
      end
    else
      case ScaleTextStyleDDCb.Items[AIndex] of
        'Bold'    : Exclude(TextStyle, fsBold);
        'Italic'  :  Exclude(TextStyle, fsItalic);
        'Strike Out' :  Exclude(TextStyle, fsStrikeOut);
        'Underline'  :  Exclude(TextStyle, fsUnderline);
      end;

  SuperGauge.ScaleSettings.TextStyle := TextStyle;
end;

procedure TSGTestFrm.ScaleTickArcColorColorCbChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.TickArcColor := ScaleTickArcColorColorCb.Selected;
end;

procedure TSGTestFrm.ScaleTickArcStyleCbChange(Sender: TObject);
begin
  // Set Arc type 0=taNone, 1=taOuter, 2=taInner, 3=taBoth

  case ScaleTickArcStyleCb.ItemIndex of
    0 {taNone} : SuperGauge.ScaleSettings.TickArcStyle := taNone;
    1 {taOuter}: SuperGauge.ScaleSettings.TickArcStyle := taOuter;
    2 {taInner}: SuperGauge.ScaleSettings.TickArcStyle := taInner;
    3 {taBoth} : SuperGauge.ScaleSettings.TickArcStyle := taBoth;
  else
    // Unknown type, warn somewhere...
  end;
end;

procedure TSGTestFrm.ScaleTickColorColorCbChange(Sender: TObject);
begin
    SuperGauge.ScaleSettings.TickColor := ScaleTickColorColorCb.Selected;
end;

procedure TSGTestFrm.SuperGaugeAuxOverloadRecovered(Sender: TObject;
  RangeValue: single);
begin
  // ONLY called once when the previous state of the Aux Value if the gauge was Overload.
  // This is done to eliminate calling this on every change of the gauge value

  OutOfRangeLbl.Caption := FloatToStr(RangeValue) + ' Aux Recovered' ;
  SuperGauge.BandSettings3.TextColor := clWhite;
  GaugeRangeError := False;
  Timer1.EnabLED:=False;
  TimerState := True;
  Timer1Timer(self);
end;

procedure TSGTestFrm.SuperGaugeAuxOverloadTriggered(Sender: TObject;
  OutOfRangeValue: single);
begin
  // Called any time the guage value is Overloaded MinValue/MaxValue is the Range.
  // Will only be reset once the value is in range, and that is handled by the
  // OverloadRecovered event. This is called only once until the gauge Recovers
  //
  // NOTE : this is typically called when the Min/Max value is exceeded on the
  // Aux Pointer (Value).

  OutOfRangeLbl.Caption := FloatToStr(OutOfRangeValue) + ' Aux Overload';
  GaugeRangeError := True;
  TimerState := False;
  Timer1.EnabLED := True;
  TimerState := False; // force call to always set, next tic will be turning off
  Timer1Timer(self);
end;

procedure TSGTestFrm.SuperGaugeOverloadRecovered(Sender: TObject;
  RangeValue: single);
begin
  // ONLY calLED once when the previous state of the Value if the gauge was Overload.
  // This is done to eliminate calling this on every change of the gauge value

  OutOfRangeLbl.Caption := FloatToStr(RangeValue) + ' Recovered' ;
  SuperGauge.BandSettings3.TextColor := clWhite;
  GaugeRangeError := False;
  Timer1.EnabLED:=False;
  TimerState := True;
  Timer1Timer(self);
end;

procedure TSGTestFrm.SuperGaugeOverloadTriggered(Sender: TObject;
  OutOfRangeValue: single);
begin
  // Called any time the guage value is Overloaded MinValue/MaxValue is the Range.
  // Will only be reset once the value is in range, and that is handled by the
  // OverloadRecovered event. This is called only once until the gauge Recovers
  //
  // NOTE : this is typically called when the Min/Max value is exceeded on the
  // Main Pointer (Value).

  OutOfRangeLbl.Caption := FloatToStr(OutOfRangeValue) + ' Overload';
  GaugeRangeError := True;
  TimerState := False;
  Timer1.EnabLED := True;
  TimerState := False; // force call to always set, next tic will be turning off
  Timer1Timer(self);
end;

procedure TSGTestFrm.ScaleStartSpeChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.Start := ScaleStartSpe.Value;
end;

procedure TSGTestFrm.ScaleRadiusSpeChange(Sender: TObject);
begin
     SuperGauge.ScaleSettings.ScaleRadius := ScaleRadiusSpe.Value;
end;

procedure TSGTestFrm.ScaleSetFontBtnClick(Sender: TObject);
begin
  if FontDialog1.Execute then
    SuperGauge.ScaleSettings.TextFont := FontDialog1.Font.Name;
end;

procedure TSGTestFrm.ScaleStepSpeChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.Step := ScaleStepSpe.Value;
end;

procedure TSGTestFrm.Text1EnabledCbChange(Sender: TObject);
begin
  SuperGauge.TextSettings1.Enabled := Text1EnabledCb.Checked;
end;

procedure TSGTestFrm.LeftAddBtnClick(Sender: TObject);
begin
  SuperGauge.Left := SuperGauge.Left + 5;
  UpdateLTStats;
end;

procedure TSGTestFrm.LeftSubBtnClick(Sender: TObject);
begin
  SuperGauge.Left := SuperGauge.Left - 5;
  UpdateLTStats;
end;

procedure TSGTestFrm.TopAddBtnClick(Sender: TObject);
begin
  SuperGauge.Top := SuperGauge.Top + 5;
  UpdateLTStats;
end;

procedure TSGTestFrm.TopSubBtnClick(Sender: TObject);
begin
  SuperGauge.Top := SuperGauge.Top - 5;
  UpdateLTStats;
end;

procedure TSGTestFrm.ValueMinus10BtnClick(Sender: TObject);
begin
  BGRAKnobValueChanged(Sender, SuperGauge.Value - 10.0);
end;

procedure TSGTestFrm.ValueMinus1BtnClick(Sender: TObject);
begin
  BGRAKnobValueChanged(Sender, SuperGauge.Value - 1.0);
end;

procedure TSGTestFrm.ValuePlus10BtnClick(Sender: TObject);
begin
  BGRAKnobValueChanged(Sender, SuperGauge.Value + 10.0);
end;

procedure TSGTestFrm.ValuePlus1BtnClick(Sender: TObject);
begin
  BGRAKnobValueChanged(Sender, SuperGauge.Value + 1.0);
end;

procedure TSGTestFrm.ValueZeroBtnClick(Sender: TObject);
begin
BGRAKnob.Value := 0;
  BGRAKnobAux.Value := 0; BGRAKnobValueChanged(Sender, 0);
  BGRAKnobAuxValueChanged(Sender, 0);
  SuperGauge.MarkerSettings1.Value := 0;
  SuperGauge.MarkerSettings2.Value := 0;
  SuperGauge.MarkerSettings3.Value := 0;

end;

procedure TSGTestFrm.ResetPositionBtnClick(Sender: TObject);
begin
  // where placed on form

  SuperGauge.Left := 40;
  SuperGauge.Top := 25;
  UpdateLTStats;
end;

procedure TSGTestFrm.WidthAddBtnClick(Sender: TObject);
begin
    SuperGauge.Width := SuperGauge.Width + 25;
    UpdateWHStats;
end;

procedure TSGTestFrm.WidthSubBtnClick(Sender: TObject);
begin
  SuperGauge.Width := SuperGauge.Width - 25;
  UpdateWHStats;
end;

procedure TSGTestFrm.HeightAddBtnClick(Sender: TObject);
begin
  SuperGauge.Height := SuperGauge.Height + 25;
  UpdateWHStats;
end;

procedure TSGTestFrm.HeightSubBtnClick(Sender: TObject);
begin
  SuperGauge.Height := SuperGauge.Height - 25;
  UpdateWHStats;
end;

procedure TSGTestFrm.ResetSizeBtnClick(Sender: TObject);
begin
  // default size

  // Could use FSavedGauge.Width but it might not always
  // be what is expected due to highDPI Scaling.

  SuperGauge.Width := 300;  // FSavedGauge.Width;
  SuperGauge.Height := 300; // FSavedGauge.Height;
  UpdateWHStats;
end;

procedure TSGTestFrm.Text1OffsetXSpeChange(Sender: TObject);
begin
  SuperGauge.TextSettings1.OffsetX := Text1OffsetXSpe.Value;
  UpdateTextStats;
end;

procedure TSGTestFrm.Text1OffsetYSpeChange(Sender: TObject);
begin
  SuperGauge.TextSettings1.OffsetY := Text1OffsetYSpe.Value;
  UpdateTextStats;
end;

procedure TSGTestFrm.Text1SetFontBtn1Click(Sender: TObject);
begin
  // try to preload the dialog, not sure it matters

  FontDialog1.Font.Name := SuperGauge.TextSettings1.FontEx.Name;
  FontDialog1.Font.Height := SuperGauge.TextSettings1.FontEx.Height;

  if FontDialog1.Execute then
    begin
      // Just set these for now

      SuperGauge.TextSettings1.FontEx.Name := FontDialog1.Font.Name;
      SuperGauge.TextSettings1.FontEx.Height := FontDialog1.Font.Height;
    end;
  UpdateTextStats;
end;

procedure TSGTestFrm.Text1ColorCbChange(Sender: TObject);
begin
    SuperGauge.TextSettings1.FontEx.Color := Text1ColorCb.Selected;
end;

procedure TSGTestFrm.Text1TextEdtChange(Sender: TObject);
begin
  SuperGauge.TextSettings1.Text := Text1TextEdt.Text;
end;

procedure TSGTestFrm.Text1TextSizeSpeChange(Sender: TObject);
begin
  SuperGauge.TextSettings1.FontEx.Height := Text1TextSizeSpe.Value;
end;

procedure TSGTestFrm.Text1TextStyleDDCbItemChange(Sender: TObject; AIndex: Integer);
var
  TextStyle: TFontStyles;
begin
  // Bold=fsBold, Italic=fsItalic, Strike Out=fsStrikeOut, Underline=fsUnderline
  // Changing the order in the dropdown will break this

  TextStyle := SuperGauge.TextSettings1.FontEx.Style;

    if Text1TextStyleDDCb.Checked[AIndex] then
      case Text1TextStyleDDCb.Items[AIndex] of
        'Bold'    : Include(TextStyle, fsBold);
        'Italic'  :  Include(TextStyle, fsItalic);
        'Strike Out' :  Include(TextStyle, fsStrikeOut);
        'Underline'  :  Include(TextStyle, fsUnderline); // may not work bug in BGRA??
      end
    else
      case Text1TextStyleDDCb.Items[AIndex] of
        'Bold'    : Exclude(TextStyle, fsBold);
        'Italic'  :  Exclude(TextStyle, fsItalic);
        'Strike Out' :  Exclude(TextStyle, fsStrikeOut);
        'Underline'  :  Exclude(TextStyle, fsUnderline);
      end;

  SuperGauge.TextSettings1.FontEx.Style := TextStyle;
end;

procedure TSGTestFrm.Text2ColorCbChange(Sender: TObject);
begin
  SuperGauge.TextSettings2.FontEx.Color := Text2ColorCb.Selected;
end;

procedure TSGTestFrm.Text2EnabledCbChange(Sender: TObject);
begin
  SuperGauge.TextSettings2.Enabled := Text2EnabledCb.Checked;
end;

procedure TSGTestFrm.Text2OffsetXSpeChange(Sender: TObject);
begin
  SuperGauge.TextSettings2.OffsetX := Text2OffsetXSpe.Value;
  UpdateTextStats;
end;

procedure TSGTestFrm.Text2OffsetYSpeChange(Sender: TObject);
begin
  SuperGauge.TextSettings2.OffsetY := Text2OffsetYSpe.Value;
  UpdateTextStats;
end;

procedure TSGTestFrm.Text2SetFontBtnClick(Sender: TObject);
begin
    // try to preload the dialog, not sure it matters

  FontDialog1.Font.Name := SuperGauge.TextSettings2.FontEx.Name;
  FontDialog1.Font.Height := SuperGauge.TextSettings2.FontEx.Height;

  if FontDialog1.Execute then
    begin
      // Just set these for now

      SuperGauge.TextSettings2.FontEx.Name := FontDialog1.Font.Name;
      SuperGauge.TextSettings2.FontEx.Height := FontDialog1.Font.Height;
    end;
  UpdateTextStats;

end;

procedure TSGTestFrm.Text2TextEdtChange(Sender: TObject);
begin
  SuperGauge.TextSettings2.Text := Text2TextEdt.Text;
end;

procedure TSGTestFrm.Text2TextSizeSpeChange(Sender: TObject);
begin
  SuperGauge.TextSettings2.FontEx.Height := Text2TextSizeSpe.Value;
end;

procedure TSGTestFrm.Text2TextStyleDDCbItemChange(Sender: TObject; AIndex: Integer);
var
  TextStyle: TFontStyles;
begin
  // Bold=fsBold, Italic=fsItalic, Strike Out=fsStrikeOut, Underline=fsUnderline
  // Changing the order in the dropdown will break this

  TextStyle := SuperGauge.TextSettings2.FontEx.Style;

    if Text2TextStyleDDCb.Checked[AIndex] then
      case Text2TextStyleDDCb.Items[AIndex] of
        'Bold'    : Include(TextStyle, fsBold);
        'Italic'  :  Include(TextStyle, fsItalic);
        'Strike Out' :  Include(TextStyle, fsStrikeOut);
        'Underline'  :  Include(TextStyle, fsUnderline); // may not work bug in BGRA??
      end
    else
      case Text2TextStyleDDCb.Items[AIndex] of
        'Bold'    : Exclude(TextStyle, fsBold);
        'Italic'  :  Exclude(TextStyle, fsItalic);
        'Strike Out' :  Exclude(TextStyle, fsStrikeOut);
        'Underline'  :  Exclude(TextStyle, fsUnderline);
      end;

  SuperGauge.TextSettings2.FontEx.Style := TextStyle;
end;

procedure TSGTestFrm.Text3ColorCbChange(Sender: TObject);
begin
  SuperGauge.TextSettings3.FontEx.Color := Text3ColorCb.Selected;
end;

procedure TSGTestFrm.Text3EnabledCbChange(Sender: TObject);
begin
  SuperGauge.TextSettings3.Enabled := Text3EnabledCb.Checked;
end;

procedure TSGTestFrm.Text3OffsetXSpeChange(Sender: TObject);
begin
  SuperGauge.TextSettings3.OffsetX := Text3OffsetXSpe.Value;
  UpdateTextStats;
end;

procedure TSGTestFrm.Text3OffsetYSpeChange(Sender: TObject);
begin
  SuperGauge.TextSettings3.OffsetY := Text3OffsetYSpe.Value;
  UpdateTextStats;
end;

procedure TSGTestFrm.Text3SetFontBtnClick(Sender: TObject);
begin
    // try to preload the dialog, not sure it matters

  FontDialog1.Font.Name := SuperGauge.TextSettings3.FontEx.Name;
  FontDialog1.Font.Height := SuperGauge.TextSettings3.FontEx.Height;

  if FontDialog1.Execute then
    begin
      // Just set these for now

      SuperGauge.TextSettings3.FontEx.Name := FontDialog1.Font.Name;
      SuperGauge.TextSettings3.FontEx.Height := FontDialog1.Font.Height;
    end;
  UpdateTextStats;
end;

procedure TSGTestFrm.Text3TextEdtChange(Sender: TObject);
begin
    SuperGauge.TextSettings3.Text := Text3TextEdt.Text;
end;

procedure TSGTestFrm.Text3TextSizeSpeChange(Sender: TObject);
begin
  SuperGauge.TextSettings3.FontEx.Height := Text3TextSizeSpe.Value;
end;

procedure TSGTestFrm.Text3TextStyleDDCbItemChange(Sender: TObject; AIndex: Integer);
var
  TextStyle: TFontStyles;
begin
  // Bold=fsBold, Italic=fsItalic, Strike Out=fsStrikeOut, Underline=fsUnderline
  // Changing the order in the dropdown will break this

  TextStyle := SuperGauge.TextSettings3.FontEx.Style;

    if Text3TextStyleDDCb.Checked[AIndex] then
      case Text3TextStyleDDCb.Items[AIndex] of
        'Bold'    : Include(TextStyle, fsBold);
        'Italic'  :  Include(TextStyle, fsItalic);
        'Strike Out' :  Include(TextStyle, fsStrikeOut);
        'Underline'  :  Include(TextStyle, fsUnderline); // may not work bug in BGRA??
      end
    else
      case Text3TextStyleDDCb.Items[AIndex] of
        'Bold'    : Exclude(TextStyle, fsBold);
        'Italic'  :  Exclude(TextStyle, fsItalic);
        'Strike Out' :  Exclude(TextStyle, fsStrikeOut);
        'Underline'  :  Exclude(TextStyle, fsUnderline);
      end;

  SuperGauge.TextSettings1.FontEx.Style := TextStyle;
end;

procedure TSGTestFrm.Timer2Timer(Sender: TObject);
var
  AuxGaugeValue, GaugeValue, offset : single;
  minVal, maxVal : single;

begin
  // Generate some numbers to move the pointer up and down
  // IF marker1 and marker2 are enabled will cause them to track
  // and reset after 10 tics. Current timer2 is 250ms.
  // Marker 3 will just always remain the MAX value hit, no reset

  offset := Random() * 10.0 - 5.0;

  // Sort if needed

  if SuperGauge.MaxValue > SuperGauge.MinValue then
  begin
    minVal := SuperGauge.MinValue;
    maxVal := SuperGauge.MaxValue;
  end
  else
    begin
      maxVal := SuperGauge.MinValue;
      minVal := SuperGauge.MaxValue;
    end;

  // some range checks to keep things moving

  if SuperGauge.Value + offset < minVal then
    GaugeValue := minVal
  else
    if SuperGauge.Value + offset > maxVal then
      GaugeValue := maxVal
    else
      GaugeValue := SuperGauge.Value + offset;

  // same for the aux pointer

  offset := Random() * 12.0 - 6.0;

  // same for Aux

  if SuperGauge.AuxMaxValue > SuperGauge.AuxMinValue then
  begin
    minVal := SuperGauge.AuxMinValue;
    maxVal := SuperGauge.AuxMaxValue;
  end
  else
    begin
      maxVal := SuperGauge.AuxMinValue;
      minVal := SuperGauge.AuxMaxValue;
    end;

  if SuperGauge.AuxValue + offset < minVal then
    AuxGaugeValue := minVal
  else
    if SuperGauge.AuxValue + offset > maxVal then
      AuxGaugeValue := maxVal
    else
      AuxGaugeValue := SuperGauge.AuxValue + offset;

  // Bump counters to reset

  inc(ResetHiTics);
  inc(ResetLoTics);

  // Call common update to catch markers and value change for gauge

  BGRAKnobValueChanged(Sender, GaugeValue);
  BGRAKnobAuxValueChanged(Sender, AuxGaugeValue);
end;

// Blink stuff when activated, typically on Overload
// NOTE : Overload is not the same as RangeLED different events

procedure TSGTestFrm.Timer1Timer(Sender: TObject);
begin
  TimerState := not TimerState;
  uELED2.Active := TimerState;

  if TimerState then
    begin
      SuperGauge.ScaleSettings.TickColor := clRed;
      SuperGauge.ScaleSettings.TickArcColor := clRed;
      SuperGauge.PointerCapSettings.FillColor := clRed;
      SuperGauge.BandSettings3.TextColor := clBlack;
      SuperGauge.FrameSettings.OuterFrameColor := clRed;
      SuperGauge.FaceSettings.PictureEnabled := True;
    end
  else
    begin
      SuperGauge.ScaleSettings.TickColor := $007DC4DF;
      SuperGauge.ScaleSettings.TickArcColor := $007DC4DF;
      SuperGauge.PointerCapSettings.FillColor := clBlack;
      SuperGauge.BandSettings3.TextColor := clWhite;
      SuperGauge.FrameSettings.OuterFrameColor := clGray;
      SuperGauge.FaceSettings.PictureEnabled := False;
  end;
end;

procedure TSGTestFrm.WidthHeightAddBtnClick(Sender: TObject);
begin
  SuperGauge.Width := SuperGauge.Width + 10;
  SuperGauge.Height := SuperGauge.Height + 10;
  UpdateWHStats;
end;

procedure TSGTestFrm.WidthHeightSubBtnClick(Sender: TObject);
begin
  SuperGauge.Width := SuperGauge.Width - 10;
  SuperGauge.Height := SuperGauge.Height - 10;
  UpdateWHStats;
end;

end.

