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

unit sgtest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Buttons, ColorBox, ComboEx, ExtDlgs, Menus, SpinEx, ueLED, BGRAKnob,
  BGRAShape, BGRAImageList, SuperGaugeCommon, SuperGauge,about;

const
  VERSIONSTR = '1.02';            // SG TEST version, Should ALWAYS show as a delta when merging!

type
  { TSGTestFrm }

  TSGTestFrm = class(TForm)
    BGRAKnob: TBGRAKnob;
    CapEdgeThicknessLbl: TLabel;
    CapEdgeThicknessSpe: TSpinEditEx;
    FaceCurveExponentLbl: TLabel;
    FaceCurveExponentSpe: TFloatSpinEditEx;
    FaceLightIntensityLbl: TLabel;
    FaceLightIntensitySpe: TSpinEditEx;
    DisableAllMarkersBtn: TBitBtn;
    CapMemo: TMemo;
    FaceMemo: TMemo;
    BackgroundColorCb: TColorBox;
    BackgroundColorLbl: TLabel;
    PointerMemo: TMemo;
    SuperGauge: TSuperGauge;
    EnableAllMarkersBtn: TBitBtn;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    Memo2: TMemo;
    FileMenu: TMenuItem;
    AboutSubMenu: TMenuItem;
    ExitSubMenu: TMenuItem;
    ScaleReversedCb: TCheckBox;
    ScaleStepLbl: TLabel;
    ScaleStepSpe: TSpinEditEx;
    Separator1: TMenuItem;
    TryToRoundValueCb: TCheckBox;
    MaxLbl: TLabel;
    MaxValLbl: TLabel;
    RoundBtn: TButton;
    Label3: TLabel;
    MinLbl: TLabel;
    MinValLbl: TLabel;
    uELED1: TuELED;
    uELED2: TuELED;
    ValuePlus1Btn: TBitBtn;
    ValueMinus1Btn: TBitBtn;
    ValuePlus10Btn: TBitBtn;
    ValueMinus10Btn: TBitBtn;
    ValueZeroBtn: TBitBtn;
    UserToGaugeLbl: TLabel;
    ResetMinMaxBtn: TBitBtn;
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
    FrameFrameColorCb: TColorBox;
    FrameFrameColorLbl: TLabel;
    FaceOuterColorCb: TColorBox;
    FrameBorderColorCb: TColorBox;
    FaceOuterColorLbl: TLabel;
    FaceInnerColorCb: TColorBox;
    FaceInnerColorLbl: TLabel;
    FaceFillStyleLbl: TLabel;
    FrameBorderColorLbl: TLabel;
    FrameRadiusLbl: TLabel;
    FrameRadiusSpe: TSpinEditEx;
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
    RangeLEDMemo: TMemo;
    OpenPictureDialog1: TOpenPictureDialog;
    Marker1ValueSpe: TFloatSpinEditEx;
    RangeLEDRangeEnd1: TLabel;
    MaxValueSpe: TFloatSpinEditEx;
    RangeLEDRangeStartLbl2: TLabel;
    MinValueSpe: TFloatSpinEditEx;
    TabSheet1: TTabSheet;
    FacePictureEnabledCb: TCheckBox;
    TickArcStyleLbl: TLabel;
    ScaleTickArcStyleCb: TComboBox;
    ScaleMainTickThicknessLbl: TLabel;
    ScaleMainTickThicknessSpe: TSpinEditEx;
    ScaleRadiusLbol2: TLabel;
    ScaleSubTickThicknessLbl: TLabel;
    ScaleSubTickThicknessSpe: TSpinEditEx;
    ScaleTextSizeSpe: TSpinEditEx;
    ScaleTextStyleDDCb: TCheckComboBox;
    ScaleTextStyleLbl: TLabel;
    ScaleSetFontBtn: TBitBtn;
    Band3BandRadiusLbl: TLabel;
    CapEdgeColorCb: TColorBox;
    CapFillColorCb: TColorBox;
    CapEdgeColorLbl: TLabel;
    ScaleRadiusLbol1: TLabel;
    ScaleTextRadiusSpe: TSpinEditEx;
    ScaleTickColorColorCb: TColorBox;
    ScaleTextColorColorCb: TColorBox;
    ScaleTickColorLbl: TLabel;
    ScaleLenghtMainTickLbl: TLabel;
    ScaleMainTickCountLbl: TLabel;
    ScaleLenghtSubTickLbl: TLabel;
    ScaleStartLbl: TLabel;
    ScaleRadiusLbol: TLabel;
    ScaleStartSpe: TSpinEditEx;
    ScaleRadiusSpe: TSpinEditEx;
    ScaleSubTickCountLbl: TLabel;
    ScaleLengthMainTickSpe: TSpinEditEx;
    ScaleEnableSubTicsCb: TCheckBox;
    EnableScaleTextCb: TCheckBox;
    ScaleEnableMainTicsCb: TCheckBox;
    PointerStyleCb: TComboBox;
    PointerStyleLbl: TLabel;
    PointerLengthLbl: TLabel;
    PointerExtensionLengthLbl: TLabel;
    PointerLengthSpe: TSpinEditEx;
    PointerColorCb: TColorBox;
    CapFillColorLbl: TLabel;
    PointerColorLbl: TLabel;
    PointerThicknessLbl: TLabel;
    PointerThicknessSpe: TSpinEditEx;
    CapPositionLbl: TLabel;
    CapStyleLbl: TLabel;
    PointerExtensionLengthSpe: TSpinEditEx;
    RangeLEDStyleCb: TComboBox;
    CapCurveExponentLbl: TLabel;
    CapCurveExponentSpe: TFloatSpinEditEx;
    CapPositionCb: TComboBox;
    CapStyleCb: TComboBox;
    ScaleMainTickCountSpe: TSpinEditEx;
    ScaleLengthSubTickSpe: TSpinEditEx;
    ScaleSubTickCountSpe: TSpinEditEx;
    ScaleTextColorLbl: TLabel;
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
    Band4ThicknessLbl: TLabel;
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
    Band3ThicknessLbl: TLabel;
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
    RangeLEDCallBackLED: TuELED;
    RangeLEDCallbackNameValLbl: TLabel;
    RangeLEDCallbackValueLbl: TLabel;
    RangeLEDCallbackNameLbl: TLabel;
    RangeLEDCallbackValLbl: TLabel;
    RangeLEDShapeValLbl: TLabel;
    RangeLEDShapeLbl: TLabel;
    RangeLEDRangeStartLbl1: TLabel;
    RangeLEDRangeTypeLbl1: TLabel;
    RangeLEDRangeStartValLbl1: TLabel;
    RangeLEDRangeEndLbl1: TLabel;
    RangeLEDRangeEndValLbl1: TLabel;
    RangeLEDRangeEnd: TLabel;
    RangeLEDRangeStartSpe: TFloatSpinEditEx;
    RangeLEDRangeEndSpe: TFloatSpinEditEx;
    RangeLEDRangeTypeValLbl1: TLabel;
    RangeLEDRangeTypeLbl: TLabel;
    RangeLEDRangeStartLbl: TLabel;
    RangeLEDRangeTypeCb: TComboBox;
    RangeLEDOffsetYValLbl: TLabel;
    RangeLEDOffsetX1Lbl1: TLabel;
    RangeLEDOffsetXValLbl: TLabel;
    RangeLEDOffsetYLbl1: TLabel;
    RangeLEDOffsetYLbl: TLabel;
    AutoScaleCb: TCheckBox;
    GroupBox3: TGroupBox;
    RangeLEDOffsetXLbl: TLabel;
    RangeLEDActiveColorCb: TColorBox;
    RangeLEDActiveColorLbl: TLabel;
    RangeLEDBorderColorLbl: TLabel;
    RangeLEDActiveColorValLbl: TLabel;
    RangeLEDBorderColorValLbl: TLabel;
    RangeLEDInactiveColorCb: TColorBox;
    RangeLEDBorderColorCb: TColorBox;
    ColorDialog1: TColorDialog;
    Label2: TLabel;
    FillStyleLbl: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    RangeLEDInactiveColorLbl: TLabel;
    RangeLEDInactiveColorValLbl: TLabel;
    RangeLEDFillStyleLbl: TLabel;
    RangeLEDStateLbl: TLabel;
    RangeLEDStateValLbl: TLabel;
    RangeLEDFillStyleValLbl: TLabel;
    RangeLEDOffsetXSpe: TSpinEditEx;
    RangeLEDOffsetYSpe: TSpinEditEx;
    RangeLEDResetOffsetBtn: TBitBtn;
    RangeLEDResetRangesBtn: TBitBtn;
    RangeLEDShapeCb: TComboBox;
    TestLEDShape: TBGRAShape;
    LEDOnBtn: TBitBtn;
    LEDOffBtn: TBitBtn;
    HeightLbl: TLabel;
    Text2Gb: TGroupBox;
    Text3Gb: TGroupBox;
    Timer2: TTimer;
    TopLbl: TLabel;
    TopValLbl: TLabel;
    LeftLbl: TLabel;
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
    procedure BGRAKnobValueChanged(Sender: TObject; Value: single);
    procedure BandEnabledCbChange(Sender: TObject);
    procedure CapEdgeThicknessSpeChange(Sender: TObject);
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
    procedure FrameBorderColorCbChange(Sender: TObject);
    procedure FrameFrameColorCbChange(Sender: TObject);
    procedure FrameRadiusSpeChange(Sender: TObject);
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
    procedure PointerThicknessSpeChange(Sender: TObject);
    procedure FacePictureResetOffsetBtnClick(Sender: TObject);
    procedure ResetMinMaxBtnClick(Sender: TObject);
    procedure ScaleEnableMainTicsCbChange(Sender: TObject);
    procedure ScaleEnableSubTicsCbChange(Sender: TObject);
    procedure ScaleLengthSubTickSpeChange(Sender: TObject);
    procedure ScaleLengthMainTickSpeChange(Sender: TObject);
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
    procedure ScaleTickArcStyleCbChange(Sender: TObject);
    procedure ScaleTickColorColorCbChange(Sender: TObject);
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
    procedure LEDOnBtnClick(Sender: TObject);
    procedure LEDOffBtnClick(Sender: TObject);
    procedure OverloadPosBtnClick(Sender: TObject);
    procedure OverloadNegBtnClick(Sender: TObject);
    procedure SuperGaugeBackInRange(Sender: TObject; RangeValue: single);
    procedure SuperGaugeDblClick(Sender: TObject);
    procedure SuperGaugeOutOfRange(Sender: TObject;OutOfRangeValue: single);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure WidthSubBtnClick(Sender: TObject);
  private

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
    procedure UpdateScaleStats;
    procedure UpdateMarkerStats;
    procedure UpdateFaceStats;
    procedure UpdateFrameStats;

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
      UpdateScaleStats;
      UpdateFaceStats;
      UpdateFrameStats;
      UpdatePositionStats;

      // hack to sync combo boxes
      RangeLEDRangeTypeCbChange(Nil); // force a quick call so can update
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

procedure TSGTestFrm.UpdateWHStats;
begin
    WidthValLbl.Caption := IntToStr(SuperGauge.Width);
    HeightValLbl.Caption := IntToStr(SuperGauge.Height);
end;

procedure TSGTestFrm.UpdateMinMaxStats;
begin
  MinValueSpe.Value := SuperGauge.MinValue;
  MaxValueSpe.Value := SuperGauge.MaxValue;
end;

procedure TSGTestFrm.UpdateBasicStats;
begin
  // could consolidate WHStats and MinMaxStats here
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
  UserToGaugeLbl.Caption := FloatToStr(SuperGauge.UserToGauge(BGRAKnob.Value));
  MinValLbl.Caption := FloatToStr(SuperGauge.MinValue);
  MaxValLbl.Caption := FloatToStr(SuperGauge.MaxValue);

  // extra stuff if needed, might be better to attach to text page to enable/disable
  SuperGauge.TextSettings2.Text:= FormatFloat('###0', SuperGauge.Value * 100);
  Text2TextEdt.Text := SuperGauge.TextSettings2.Text; // update the edit box to be nice!

end;

procedure TSGTestFrm.UpdateRangeLEDStats;
begin
  // RangeLED Settings
  // Get the State

  if SuperGauge.RangeLEDSettings.Active then
    RangeLEDStateValLbl.Caption := 'True'
  else
    RangeLEDStateValLbl.Caption := 'False';

  // Get the LED Style and stuff the ComboBox

  RangeLEDStyleCb.ItemIndex := ord(SuperGauge.RangeLEDSettings.Style);
  RangeLEDFillStyleValLbl.Caption := IntToStr(ord(SuperGauge.RangeLEDSettings.Style)); // just the int for now

  RangeLEDShapeCb.ItemIndex := ord(SuperGauge.RangeLEDSettings.Shape);
  RangeLEDShapeValLbl.Caption := IntToStr(ord(SuperGauge.RangeLEDSettings.Shape)); // just the int for now

  // read settings from the gauge and set to the shape as our makeshift
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

  // Now the colors, note Lazarus is ordered $00BBGGRR, ColorToString is your friend!

  RangeLEDActiveColorCb.Selected := SuperGauge.RangeLEDSettings.ActiveColor;
  RangeLEDActiveColorValLbl.Caption := ColorToString(SuperGauge.RangeLEDSettings.ActiveColor);
  RangeLEDInactiveColorCb.Selected := SuperGauge.RangeLEDSettings.InactiveColor;
  RangeLEDInactiveColorValLbl.Caption := ColorToString(SuperGauge.RangeLEDSettings.InactiveColor);
  RangeLEDBorderColorCb.Selected := SuperGauge.RangeLEDSettings.BorderColor;
  RangeLEDBorderColorValLbl.Caption := ColorToString(SuperGauge.RangeLEDSettings.BorderColor);

  RangeLEDSizeSpe.Value := SuperGauge.RangeLEDSettings.Size;
  RangeLEDOffsetXSpe.Value := SuperGauge.RangeLEDSettings.OffsetX;
  RangeLEDOffsetYSpe.Value := SuperGauge.RangeLEDSettings.OffsetY;
  RangeLEDOffsetXValLbl.Caption := IntToStr(SuperGauge.RangeLEDSettings.OffsetX);
  RangeLEDOffsetYValLbl.Caption := IntToStr(SuperGauge.RangeLEDSettings.OffsetY);

  // Range Check Stuff

  RangeLEDRangeTypeCb.ItemIndex := ord(SuperGauge.RangeLEDSettings.RangeType);
  RangeLEDRangeTypeValLbl1.Caption := IntToStr(ord(SuperGauge.RangeLEDSettings.RangeType));
  RangeLEDRangeStartSpe.Value := SuperGauge.RangeLEDSettings.RangeStartValue;
  RangeLEDRangeEndSpe.Value := SuperGauge.RangeLEDSettings.RangeEndValue;
  RangeLEDRangeStartValLbl1.Caption := FloatToStr(SuperGauge.RangeLEDSettings.RangeStartValue);
  RangeLEDRangeEndValLbl1.Caption := FloatToStr(SuperGauge.RangeLEDSettings.RangeEndValue);
end;

procedure TSGTestFrm.UpdateMarkerStats;
begin
  Marker1EnabledCb.Checked := SuperGauge.MarkerSettings1.Enabled;
  Marker1ColorCb.Selected := SuperGauge.MarkerSettings1.Color;
  Marker1WidthSpe.Value := SuperGauge.MarkerSettings1.Width;
  Marker1HeightSpe.Value := SuperGauge.MarkerSettings1.Height;
  Marker1RadiusSpe.Value := SuperGauge.MarkerSettings1.Radius;
  Marker1ValueSpe.Value := SuperGauge.MarkerSettings1.Value;

  // msCenter = 0, msLeft = 1, msRight = 2
  Marker1StyleCb.ItemIndex := ord(SuperGauge.MarkerSettings1.Style);

  Marker2EnabledCb.Checked := SuperGauge.MarkerSettings2.Enabled;
  Marker2ColorCb.Selected := SuperGauge.MarkerSettings2.Color;
  Marker2WidthSpe.Value := SuperGauge.MarkerSettings2.Width;
  Marker2HeightSpe.Value := SuperGauge.MarkerSettings2.Height;
  Marker2RadiusSpe.Value := SuperGauge.MarkerSettings2.Radius;
  Marker2ValueSpe.Value := SuperGauge.MarkerSettings2.Value;

  // msCenter = 0, msLeft = 1, msRight = 2
  Marker2StyleCb.ItemIndex := ord(SuperGauge.MarkerSettings2.Style);

  Marker3EnabledCb.Checked := SuperGauge.MarkerSettings3.Enabled;
  Marker3ColorCb.Selected := SuperGauge.MarkerSettings3.Color;
  Marker3WidthSpe.Value := SuperGauge.MarkerSettings3.Width;
  Marker3HeightSpe.Value := SuperGauge.MarkerSettings3.Height;
  Marker3RadiusSpe.Value := SuperGauge.MarkerSettings3.Radius;
  Marker3ValueSpe.Value := SuperGauge.MarkerSettings3.Value;

  // msCenter = 0, msLeft = 1, msRight = 2
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
  Band1ThicknessSpe.Value := SuperGauge.BandSettings1.Thickness;
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
  Band2ThicknessSpe.Value := SuperGauge.BandSettings2.Thickness;
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
  Band3ThicknessSpe.Value := SuperGauge.BandSettings3.Thickness;
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
  Band4ThicknessSpe.Value := SuperGauge.BandSettings4.Thickness;
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
  CapEdgeThicknessSpe.Value := SuperGauge.PointerCapSettings.EdgeThickness;
end;

procedure TSGTestFrm.UpdatePointerStats;
begin
  // Pointer Settings

  PointerColorCb.Selected := SuperGauge.PointerSettings.Color;
  PointerLengthSpe.Value := SuperGauge.PointerSettings.Length;
  PointerExtensionLengthSpe.Value := SuperGauge.PointerSettings.ExtensionLength;
  PointerStyleCb.ItemIndex := ord(SuperGauge.PointerSettings.Style);
  PointerThicknessSpe.Value := SuperGauge.PointerSettings.Thickness;
end;

procedure TSGTestFrm.UpdateScaleStats;
begin
  // Scale Settings

  ScaleEnableMainTicsCb.Checked := SuperGauge.ScaleSettings.EnableMainTicks;
  ScaleEnableSubTicsCb.Checked := SuperGauge.ScaleSettings.EnableSubTicks;
  EnableScaleTextCb.Checked := SuperGauge.ScaleSettings.EnableScaleText;
  ScaleReversedCb.Checked := SuperGauge.ScaleSettings.ReverseScale;
  ScaleLengthMainTickSpe.Value := SuperGauge.ScaleSettings.LengthMainTick;
  ScaleLengthSubTickSpe.Value := SuperGauge.ScaleSettings.LengthSubTick;
  ScaleMainTickCountSpe.Value := SuperGauge.ScaleSettings.MainTickCount;
  ScaleSubTickCountSpe.Value := SuperGauge.ScaleSettings.SubTickCount;
  ScaleStartSpe.Value := SuperGauge.ScaleSettings.Start;
  ScaleStepSpe.Value := SuperGauge.ScaleSettings.Step;

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
  ScaleMainTickThicknessSpe.Value := SuperGauge.ScaleSettings.ThicknessMainTick;
  ScaleSubTickThicknessSpe.Value := SuperGauge.ScaleSettings.ThicknessSubTick;
  ScaleTickArcStyleCb.ItemIndex := ord(SuperGauge.ScaleSettings.TickArcStyle);
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
  FrameBorderColorCb.Selected := SuperGauge.FrameSettings.BorderColor;
  FrameFrameColorCb.Selected :=  SuperGauge.FrameSettings.FrameColor;
  FrameRadiusSpe.Value := SuperGauge.FrameSettings.BorderRadius;
end;

procedure TSGTestFrm.LEDOnBtnClick(Sender: TObject);
begin
  SuperGauge.RangeLEDSettings.Active:=True;
  UpdateRangeLEDStats;
end;

procedure TSGTestFrm.LEDOffBtnClick(Sender: TObject);
begin
    SuperGauge.RangeLEDSettings.Active:=False;
    UpdateRangeLEDStats;
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
  SuperGauge.BandSettings1.Thickness := Band1ThicknessSpe.Value;
end;

procedure TSGTestFrm.RangeLEDRangeTypeCbChange(Sender: TObject);
begin
  // Range Types -
  //          rcNone, rcGaugeOutOfRange, rcBetween, rcBothInclusive,
  //          rcStartInclusive, rcEndInclusive, rcBothBetweenOutside,
  //          rcBothInclusiveOutside, rcGreaterStart, rcLessEnd

  RangeLEDRangeStartSpe.ReadOnly := False;
  RangeLEDRangeEndSpe.ReadOnly := False;
  RangeLEDRangeStartSpe.Color := clWindow;
  RangeLEDRangeEndSpe.Color := clWindow;

  case RangeLEDRangeTypeCb.ItemIndex of
    0 : {rcNone}
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcNone;
          RangeLEDFillStyleValLbl.Caption := 'rcNone';
          RangeLEDRangeStartSpe.ReadOnly := True;
          RangeLEDRangeEndSpe.ReadOnly := True;
          RangeLEDRangeStartSpe.Color := clInactiveCaption;
          RangeLEDRangeEndSpe.Color := clInactiveCaption;
        end;

    1 : {rcGaugeOutOfRange} // uses gauge range only, ignores RangeLED start/end
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcGaugeOutOfRange;
          RangeLEDFillStyleValLbl.Caption := 'rcGaugeOutOfRange';
          RangeLEDRangeStartSpe.ReadOnly := True;
          RangeLEDRangeEndSpe.ReadOnly := True;
          RangeLEDRangeStartSpe.Color := clInactiveCaption;
          RangeLEDRangeEndSpe.Color := clInactiveCaption;
        end;
    2 : {rcBetween}
        begin
            SuperGauge.RangeLEDSettings.RangeType := rcBetween;
            RangeLEDFillStyleValLbl.Caption := 'rcBetween';
        end;
    3 : {rcBothInclusive}
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcBothInclusive;
          RangeLEDFillStyleValLbl.Caption := 'rcBothInclusive';
        end;
    4 : {rcStartInclusive}
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcStartInclusive;
          RangeLEDFillStyleValLbl.Caption := 'rcStartInclusive';
        end;
    5 : {rcEndInclusive}
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcEndInclusive;
          RangeLEDFillStyleValLbl.Caption := 'rcEndInclusive';
        end;
    6 : {rcBothBetweenOutside}
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcBothBetweenOutside;
          RangeLEDFillStyleValLbl.Caption := 'rcBothBetweenOutside';
        end;
    7 : {rcBothInclusiveOutside}
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcBothInclusiveOutside;
          RangeLEDFillStyleValLbl.Caption := 'rcBothInclusiveOutside';
        end;

    // These last cases are really just making it so you only look at start or end
    // You could use the above with a LARGE number (Max integer) for the end, or Min Integer
    // and get the same results, but this just ensures it to work looking only at one value

    8 : {rcGreaterStart}
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcGreaterStart;
          RangeLEDFillStyleValLbl.Caption := 'rcGreaterStart';
          RangeLEDRangeEndSpe.ReadOnly := True;
          RangeLEDRangeEndSpe.Color := clInactiveCaption;
        end;
    9 : {rcLessEnd}
        begin
          SuperGauge.RangeLEDSettings.RangeType := rcLessEnd;
          RangeLEDFillStyleValLbl.Caption := 'rcLessEnd';
          RangeLEDRangeStartSpe.ReadOnly := True;
          RangeLEDRangeStartSpe.Color := clInactiveCaption;
        end;
  else
    RangeLEDFillStyleValLbl.Caption := 'Gauge Has Unknown Range Type';
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
          RangeLEDFillStyleValLbl.Caption := 'lsNone';
        end;
    1 : {lsFlat}
        begin
            SuperGauge.RangeLEDSettings.Style := lsFlat;
            RangeLEDFillStyleValLbl.Caption := 'lsFlat';
        end;
    2 : {lsShaded}
        begin
          SuperGauge.RangeLEDSettings.Style := lsShaded;
          RangeLEDFillStyleValLbl.Caption := 'lsShaded';
        end
  else
    RangeLEDFillStyleValLbl.Caption := 'Gauge Has Unknown LED Fill Type';
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
          RangeLEDShapeValLbl.Caption := 'lshRound';
        end;
    1 : {lshSquare}
        begin
            SuperGauge.RangeLEDSettings.Shape := lshSquare;
            RangeLEDShapeValLbl.Caption := 'lshSquare';
        end;
    2 : {lshTriangle}
        begin
          SuperGauge.RangeLEDSettings.Shape := lshTriangle;
          RangeLEDShapeValLbl.Caption := 'lshTriangle';
        end;
    3 : {lshDownTriangle}
        begin
          SuperGauge.RangeLEDSettings.Shape := lshDownTriangle;
          RangeLEDShapeValLbl.Caption := 'lshDownTriangle';
        end
  else
    RangeLEDShapeValLbl.Caption := 'Gauge Has Unknown LED Shape Type';
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
  SuperGauge.RangeLEDSettings.OffsetX := 90;
  SuperGauge.RangeLEDSettings.OffsetY := 120;
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
  BGRAKnobValueChanged(Sender, 250);
end;

procedure TSGTestFrm.OverloadNegBtnClick(Sender: TObject);
begin
   BGRAKnobValueChanged(Sender, -250);
end;

procedure TSGTestFrm.SuperGaugeDblClick(Sender: TObject);
begin
  Beep;
end;

procedure TSGTestFrm.SuperGaugeBackInRange(Sender: TObject; RangeValue: single);
begin
  // ONLY calLED once when the previous state of the gauge was out of range.
  // This is done to eliminate calling this on every change of the gauge value

  OutOfRangeLbl.Caption := FloatToStr(RangeValue) + ' In Range' ;
//  SuperGauge.RangeLEDSettings.Active := False; // Turn off the LED
  SuperGauge.BandSettings3.TextColor := clWhite;
  GaugeRangeError := False;
  Timer1.EnabLED:=False;
  TimerState := True; // force call to always clean
  Timer1Timer(self);
end;

procedure TSGTestFrm.SuperGaugeOutOfRange(Sender: TObject; OutOfRangeValue: single);
begin
  // CalLED any time the guage value is out of range. Will only be reset
  // once the value is in range, and that is handled by the BackInRange event, that
  // by the way if you read above is only callled once on state change to a valid value.

  OutOfRangeLbl.Caption := FloatToStr(OutOfRangeValue) + ' Out of Range';
//  SuperGauge.RangeLEDSettings.Active := True; // Turn on the LED
  GaugeRangeError := True;
  TimerState := False;
  Timer1.EnabLED := True;
  TimerState := False; // force call to always set, next tic will be turning off
  Timer1Timer(self);
end;

procedure TSGTestFrm.SuperGaugeRangeLEDInactive(Sender: TObject; Value: single);
begin
  RangeLEDCallBackLED.Color := SuperGauge.RangeLEDSettings.InactiveColor;
  RangeLEDCallbackNameValLbl.Caption := 'Inactive at ' + FloatToStr(Value);;
end;

procedure TSGTestFrm.SuperGaugeRangeLEDActive(Sender: TObject; Value: single);
begin
  RangeLEDCallBackLED.Color := SuperGauge.RangeLEDSettings.ActiveColor;
  RangeLEDCallbackNameValLbl.Caption := 'Active at ' + FloatToStr(Value);
end;

procedure TSGTestFrm.AutoScaleCbChange(Sender: TObject);
begin
  SuperGauge.AutoScale := AutoScaleCb.checked;
end;

procedure TSGTestFrm.BackgroundColorCbChange(Sender: TObject);
begin
  SuperGauge.Color := BackgroundColorCb.Selected;
end;

procedure TSGTestFrm.AboutSubMenuClick(Sender: TObject);
begin
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
  SuperGauge.BandSettings2.Thickness := Band2ThicknessSpe.Value;
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
  SuperGauge.BandSettings3.Thickness := Band3ThicknessSpe.Value;
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
  SuperGauge.BandSettings4.Thickness := Band4ThicknessSpe.Value;
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
end;

procedure TSGTestFrm.BandEnabledCbChange(Sender: TObject);
begin
  SuperGauge.BandSettings1.Enabled := Band1EnabledCb.Checked;
end;

procedure TSGTestFrm.CapEdgeThicknessSpeChange(Sender: TObject);
begin
  SuperGauge.PointerCapSettings.EdgeThickness := CapEdgeThicknessSpe.Value;
  UpdateCapStats;
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

procedure TSGTestFrm.FrameBorderColorCbChange(Sender: TObject);
begin
  // update frame border color
  SuperGauge.FrameSettings.BorderColor := FrameBorderColorCb.Selected;
end;

procedure TSGTestFrm.FrameFrameColorCbChange(Sender: TObject);
begin
  // update frame color
  SuperGauge.FrameSettings.FrameColor := FrameFrameColorCb.Selected;
end;

procedure TSGTestFrm.FrameRadiusSpeChange(Sender: TObject);
begin
  // update frame radius
  SuperGauge.FrameSettings.BorderRadius := FrameRadiusSpe.Value;
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
  UpdatePointerStats;
end;

procedure TSGTestFrm.PointerLengthSpeChange(Sender: TObject);
begin
  SuperGauge.PointerSettings.Length := PointerLengthSpe.Value;
  UpdatePointerStats;
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

procedure TSGTestFrm.PointerThicknessSpeChange(Sender: TObject);
begin
  SuperGauge.PointerSettings.Thickness := PointerThicknessSpe.Value;
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
  UpdateMinMaxStats;
end;

procedure TSGTestFrm.ScaleEnableMainTicsCbChange(Sender: TObject);
begin
    SuperGauge.ScaleSettings.EnableMainTicks := ScaleEnableMainTicsCb.Checked;
end;

procedure TSGTestFrm.ScaleEnableSubTicsCbChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.EnableSubTicks := ScaleEnableSubTicsCb.Checked;
end;

procedure TSGTestFrm.ScaleLengthSubTickSpeChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.LengthSubTick := ScaleLengthSubTickSpe.Value;
end;

procedure TSGTestFrm.ScaleLengthMainTickSpeChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.LengthMainTick := ScaleLengthMainTickSpe.Value;
end;

procedure TSGTestFrm.ScaleMainTickCountSpeChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.MainTickCount := ScaleMainTickCountSpe.Value;
end;

procedure TSGTestFrm.ScaleMainTickThicknessSpeChange(Sender: TObject);
begin
  SuperGauge.ScaleSettings.ThicknessMainTick := ScaleMainTickThicknessSpe.Value;
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
  SuperGauge.ScaleSettings.ThicknessSubTick := ScaleSubTickThicknessSpe.Value;
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

  UpdateScaleStats;
end;

procedure TSGTestFrm.ScaleTickColorColorCbChange(Sender: TObject);
begin
    SuperGauge.ScaleSettings.TickColor := ScaleTickColorColorCb.Selected;
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
  BGRAKnobValueChanged(Sender, 0);
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

  SuperGauge.Width := 360;
  SuperGauge.Height := 360;
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
  GaugeValue, offset : single;

begin
  // Generate some numbers to move the pointer up and down
  // IF marker1 and marker2 are enabled will cause them to track
  // and reset after 10 tics. Current timer2 is 250ms.
  // Marker 3 will just always remain the MAX value hit, no reset

  offset := Random() * 10.0 - 5.0;

  // some range checks to keep things moving

  if SuperGauge.Value + offset < 0 then
    GaugeValue := 10
  else
    if SuperGauge.Value + offset > 100 then
      GaugeValue := 90
    else
      GaugeValue := SuperGauge.Value + offset;

  // Bump counters to reset

  inc(ResetHiTics);
  inc(ResetLoTics);

  // Call common update to catch markers and value change for gauge

  BGRAKnobValueChanged(Sender, GaugeValue);

end;

// Blink stuff when activated, typically on overrangem
// NOTE : Overange is not the same as RangeLED different events

procedure TSGTestFrm.Timer1Timer(Sender: TObject);
begin
  TimerState := not TimerState;
  uELED2.Active := TimerState;

  if TimerState then
    begin
      SuperGauge.ScaleSettings.TickColor := clRed;
      SuperGauge.PointerCapSettings.FillColor := clRed;
      SuperGauge.BandSettings3.TextColor := clBlack;
      SuperGauge.FrameSettings.BorderColor := clRed;
      SuperGauge.FaceSettings.PictureEnabled := True;
    end
  else
    begin
      SuperGauge.ScaleSettings.TickColor := $007DC4DF;
      SuperGauge.PointerCapSettings.FillColor := clBlack;
      SuperGauge.BandSettings3.TextColor := clWhite;
      SuperGauge.FrameSettings.BorderColor := clGray;
      SuperGauge.FaceSettings.PictureEnabled := False;
end;
end;

end.

