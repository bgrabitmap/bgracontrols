{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Author: Boban Spasic
 Credits to: alpine from Lazarus forum
}

unit BCLeaTheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  laz2_dom, laz2_xmlwrite, laz2_xmlread, StrUtils, LCLIntf,
  BGRABitmapTypes, BGRABitmap, BGRAGradients, BCLeaTypes;

type
  TBCLeaThemeCommon = class(TPersistent)
  public
    //Common
    FLightSourceIntensity: single;
    FLightSourceDistanceTerm: single;
    FLightSourceDistanceFactor: single;
    FLightDestFactor: single;
    FLightColor: TColor;
    FSpecularFactor: single;
    FSpecularIndex: single;
    FAmbientFactor: single;
    FDiffusionFactor: single;
    FNegativeDiffusionFactor: single;
    FDiffuseSaturation: boolean;
    FLightPositionX: integer;
    FLightPositionY: integer;
    FLightPositionZ: integer;
  end;

  TBCLeaThemeLCD = class(TPersistent)
  public
    //BLCDDisplay
    FFrameColor: TColor;
    FBoardColor: TColor;
    FDotColorOn: TColor;
    FFrameStyle: TZStyle;
    FFrameHeight: integer;
    FFrameAltitude: integer;
    FFrameSize: integer;
    FDotShape: TDotShape;
    FDotSize: integer;
    FDotsSpace: integer;
    FDotBlend: boolean;
    FDotBlendOperation: TBlendOperation;
    FDotBlur: boolean;
    FDotBlurRadius: single;
    FBoardShadow: TBoardShadow;
  end;

  TBCLeaThemeLED = class(TPersistent)
  public
    //BCLeaLED
    FColorOn: TColor;
    FColorOff: TColor;
    FBkgColor: TColor;
    FStyle: TZStyle;
    FSize: integer;
    FAltitude: integer;
  end;

  TBCLeaThemeQLED = class(TPersistent)
  public
    //BCLeaQLED
    FColorOn: TColor;
    FColorOff: TColor;
    FBkgColor: TColor;
    FStyle: TZStyle;
    FSize: integer;
    FAltitude: integer;
    FRounding: integer;
  end;

  TBCLeaThemeSelector = class(TPersistent)
  public
    //BSelector
    FLineColor: TColor;
    FLineBkgColor: TColor;
    FLineWidth: integer;
    FFontShadowColor: TColor;
    FFontShadowOffsetX: integer;
    FFontShadowOffsetY: integer;
    FFontShadowRadius: integer;
    FBkgColor: TColor;
    FPointerSize: integer;
    FStyle: TZStyle;
    FDrawTextPhong: boolean;
    FAltitude: integer;
  end;

  TBCLeaThemeRingSlider = class(TPersistent)
  public
    //BRingSlider
    FLineColor: TColor;
    FLineBkgColor: TColor;
    FLineWidth: integer;
    FFontShadowColor: TColor;
    FFontShadowOffsetX: integer;
    FFontShadowOffsetY: integer;
    FFontShadowRadius: integer;
    FBkgColor: TColor;
    FPointerSize: integer;
    FPointerColor: TColor;
    FStyle: TZStyle;
    FDrawTextPhong: boolean;
    FAltitude: integer;
  end;

  TBCLeaTheme = class(TComponent)
  private
    FThemeSetCommon: TBCLeaThemeCommon;
    FThemeSetLCD: TBCLeaThemeLCD;
    FThemeSetLED: TBCLeaThemeLED;
    FThemeSetSelector: TBCLeaThemeSelector;
    FThemeSetRingSlider: TBCLeaThemeRingSlider;
    FThemeSetQLED: TBCLeaThemeQLED;
    FOnChange: TNotifyEvent;
    procedure DoChange;
    //Common
    procedure SetLightSourceIntensity(const AValue: single);
    procedure SetLightSourceDistanceTerm(const AValue: single);
    procedure SetLightSourceDistanceFactor(const AValue: single);
    procedure SetLightDestFactor(const AValue: single);
    procedure SetLightColor(const AValue: TColor);
    procedure SetSpecularFactor(const AValue: single);
    procedure SetSpecularIndex(const AValue: single);
    procedure SetAmbientFactor(const AValue: single);
    procedure SetDiffusionFactor(const AValue: single);
    procedure SetNegativeDiffusionFactor(const AValue: single);
    procedure SetDiffuseSaturation(const AValue: boolean);
    procedure SetLightPositionX(const AValue: integer);
    procedure SetLightPositionY(const AValue: integer);
    procedure SetLightPositionZ(const AValue: integer);
    function GetLightSourceIntensity: single;
    function GetLightSourceDistanceTerm: single;
    function GetLightSourceDistanceFactor: single;
    function GetLightDestFactor: single;
    function GetLightColor: TColor;
    function GetSpecularFactor: single;
    function GetSpecularIndex: single;
    function GetAmbientFactor: single;
    function GetDiffusionFactor: single;
    function GetNegativeDiffusionFactor: single;
    function GetDiffuseSaturation: boolean;
    function GetLightPositionX: integer;
    function GetLightPositionY: integer;
    function GetLightPositionZ: integer;
    //BLCDDisplay
    procedure SetBLCDFrameColor(const AValue: TColor);
    procedure SetBLCDBoardColor(const AValue: TColor);
    procedure SetBLCDDotColorOn(const AValue: TColor);
    procedure SetBLCDFrameAltitude(const AValue: integer);
    procedure SetBLCDFrameHeight(const AValue: integer);
    procedure SetBLCDFrameSize(const AValue: integer);
    procedure SetBLCDFrameStyle(const AValue: TZStyle);
    procedure SetBLCDDotShape(const AValue: TDotShape);
    procedure SetBLCDDotSize(const AValue: integer);
    procedure SetBLCDDotsSpace(const AValue: integer);
    procedure SetBLCDDotBlend(const AValue: boolean);
    procedure SetBLCDDotBlendOperation(const AValue: TBlendOperation);
    procedure SetBLCDDotBlur(const AValue: boolean);
    procedure SetBLCDDotBlurRadius(const AValue: single);
    procedure SetBLCDBoardShadow(const AValue: TBoardShadow);
    function GetBLCDFrameColor: TColor;
    function GetBLCDBoardColor: TColor;
    function GetBLCDDotColorOn: TColor;
    function GetBLCDFrameAltitude: integer;
    function GetBLCDFrameHeight: integer;
    function GetBLCDFrameSize: integer;
    function GetBLCDFrameStyle: TZStyle;
    function GetBLCDDotShape: TDotShape;
    function GetBLCDDotSize: integer;
    function GetBLCDDotsSpace: integer;
    function GetBLCDDotBlend: boolean;
    function GetBLCDDotBlendOperation: TBlendOperation;
    function GetBLCDDotBlur: boolean;
    function GetBLCDDotBlurRadius: single;
    function GetBLCDBoardShadow: TBoardShadow;
    //BCLeaLED
    procedure SetBCLeaLEDColorOn(AValue: TColor);
    procedure SetBCLeaLEDColorOff(AValue: TColor);
    procedure SetBCLeaLEDBkgColor(AValue: TColor);
    procedure SetBCLeaLEDStyle(AValue: TZStyle);
    procedure SetBCLeaLEDSize(AValue: integer);
    procedure SetBCLeaLEDAltitude(AValue: integer);
    function GetBCLeaLEDColorOn: TColor;
    function GetBCLeaLEDColorOff: TColor;
    function GetBCLeaLEDBkgColor: TColor;
    function GetBCLeaLEDStyle: TZStyle;
    function GetBCLeaLEDSize: integer;
    function GetBCLeaLEDAltitude: integer;
    //BCLeaQLED
    procedure SetBCLeaQLEDColorOn(AValue: TColor);
    procedure SetBCLeaQLEDColorOff(AValue: TColor);
    procedure SetBCLeaQLEDBkgColor(AValue: TColor);
    procedure SetBCLeaQLEDStyle(AValue: TZStyle);
    procedure SetBCLeaQLEDSize(AValue: integer);
    procedure SetBCLeaQLEDAltitude(AValue: integer);
    procedure SetBCLeaQLEDRounding(AValue: integer);
    function GetBCLeaQLEDColorOn: TColor;
    function GetBCLeaQLEDColorOff: TColor;
    function GetBCLeaQLEDBkgColor: TColor;
    function GetBCLeaQLEDStyle: TZStyle;
    function GetBCLeaQLEDSize: integer;
    function GetBCLeaQLEDAltitude: integer;
    function GetBCLeaQLEDRounding: integer;
    //BSelector
    procedure SetBSELLineColor(AValue: TColor);
    procedure SetBSELLineBkgColor(AValue: TColor);
    procedure SetBSELLineWidth(AValue: integer);
    procedure SetBSELFontShadowColor(AValue: TColor);
    procedure SetBSELFontShadowOffsetX(AValue: integer);
    procedure SetBSELFontShadowOffsetY(AValue: integer);
    procedure SetBSELFontShadowRadius(AValue: integer);
    procedure SetBSELBkgColor(AValue: TColor);
    procedure SetBSELPointerSize(AValue: integer);
    procedure SetBSELStyle(AValue: TZStyle);
    procedure SetBSELDrawTextPhong(AValue: boolean);
    procedure SetBSELAltitude(AValue: integer);
    function GetBSELLineColor: TColor;
    function GetBSELLineBkgColor: TColor;
    function GetBSELLineWidth: integer;
    function GetBSELFontShadowColor: TColor;
    function GetBSELFontShadowOffsetX: integer;
    function GetBSELFontShadowOffsetY: integer;
    function GetBSELFontShadowRadius: integer;
    function GetBSELBkgColor: TColor;
    function GetBSELPointerSize: integer;
    function GetBSELStyle: TZStyle;
    function GetBSELDrawTextPhong: boolean;
    function GetBSELAltitude: integer;
    //BRingSlider
    procedure SetBRSLineColor(AValue: TColor);
    procedure SetBRSLineBkgColor(AValue: TColor);
    procedure SetBRSLineWidth(AValue: integer);
    procedure SetBRSFontShadowColor(AValue: TColor);
    procedure SetBRSFontShadowOffsetX(AValue: integer);
    procedure SetBRSFontShadowOffsetY(AValue: integer);
    procedure SetBRSFontShadowRadius(AValue: integer);
    procedure SetBRSBkgColor(AValue: TColor);
    procedure SetBRSPointerSize(AValue: integer);
    procedure SetBRSPointerColor(AValue: TColor);
    procedure SetBRSStyle(AValue: TZStyle);
    procedure SetBRSDrawTextPhong(AValue: boolean);
    procedure SetBRSAltitude(AValue: integer);
    function GetBRSLineColor: TColor;
    function GetBRSLineBkgColor: TColor;
    function GetBRSLineWidth: integer;
    function GetBRSFontShadowColor: TColor;
    function GetBRSFontShadowOffsetX: integer;
    function GetBRSFontShadowOffsetY: integer;
    function GetBRSFontShadowRadius: integer;
    function GetBRSBkgColor: TColor;
    function GetBRSPointerSize: integer;
    function GetBRSPointerColor: TColor;
    function GetBRSStyle: TZStyle;
    function GetBRSDrawTextPhong: boolean;
    function GetBRSAltitude: integer;
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //load default theme
    procedure Initialize;
    //do not use direct, please use the procedures of the parent component
    procedure SaveThemeToFile(AFileName: string);
    //do not use direct, please use the procedures of the parent component
    procedure LoadThemeFromFile(AFileName: string);
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    //BLCDDisplay
    property LCD_FrameColor: TColor read GetBLCDFrameColor write SetBLCDFrameColor default clBtnFace;
    property LCD_BoardColor: TColor read GetBLCDBoardColor write SetBLCDBoardColor default clBlack;
    property LCD_DotColorOn: TColor read GetBLCDDotColorOn write SetBLCDDotColorOn default clSkyBlue;
    property LCD_FrameAltitude: integer read GetBLCDFrameAltitude write SetBLCDFrameAltitude default 2;
    property LCD_FrameHeight: integer read GetBLCDFrameHeight write SetBLCDFrameHeight default 8;
    property LCD_FrameSize: integer read GetBLCDFrameSize write SetBLCDFrameSize default 8;
    property LCD_FrameStyle: TZStyle read GetBLCDFrameStyle write SetBLCDFrameStyle default zsRaised;
    property LCD_DotShape: TDotShape read GetBLCDDotShape write SetBLCDDotShape default stSquare;
    property LCD_DotSize: integer read GetBLCDDotSize write SetBLCDDotSize default 4;
    property LCD_DotsSpace: integer read GetBLCDDotsSpace write SetBLCDDotsSpace default 1;
    property LCD_DotBlend: boolean read GetBLCDDotBlend write SetBLCDDotBlend default False;
    property LCD_DotBlendOperation: TBlendOperation read GetBLCDDotBlendOperation write SetBLCDDotBlendOperation default boGlow;
    property LCD_DotBlur: boolean read GetBLCDDotBlur write SetBLCDDotBlur default False;
    property LCD_DotBlurRadius: single read GetBLCDDotBlurRadius write SetBLCDDotBlurRadius default 0.8;
    property LCD_BoardShadow: TBoardShadow read GetBLCDBoardShadow write SetBLCDBoardShadow default bsFrame;
    property COM_LightSourceIntensity: single read GetLightSourceIntensity write SetLightSourceIntensity default 500;
    property COM_LightSourceDistanceTerm: single read GetLightSourceDistanceTerm write SetLightSourceDistanceTerm default 150;
    property COM_LightSourceDistanceFactor: single read GetLightSourceDistanceFactor write SetLightSourceDistanceFactor default 1.0;
    property COM_LightDestFactor: single read GetLightDestFactor write SetLightDestFactor default 1.0;
    property COM_LightColor: TColor read GetLightColor write SetLightColor default clWhite;
    property COM_SpecularFactor: single read GetSpecularFactor write SetSpecularFactor default 0.6;
    property COM_SpecularIndex: single read GetSpecularIndex write SetSpecularIndex default 10;
    property COM_AmbientFactor: single read GetAmbientFactor write SetAmbientFactor default 0.3;
    property COM_DiffusionFactor: single read GetDiffusionFactor write SetDiffusionFactor default 0.9;
    property COM_NegativeDiffusionFactor: single read GetNegativeDiffusionFactor write SetNegativeDiffusionFactor default 0.1;
    property COM_DiffuseSaturation: boolean read GetDiffuseSaturation write SetDiffuseSaturation default False;
    property COM_LightPositionX: integer read GetLightPositionX write SetLightPositionX default -100;
    property COM_LightPositionY: integer read GetLightPositionY write SetLightPositionY default -100;
    property COM_LightPositionZ: integer read GetLightPositionZ write SetLightPositionZ default 100;
    property LED_ColorOn: TColor read GetBCLeaLEDColorOn write SetBCLeaLEDColorOn default TColor($00FF9C15);
    property LED_ColorOff: TColor read GetBCLeaLEDColorOff write SetBCLeaLEDColorOff default TColor($009E5A00);
    property LED_BkgColor: TColor read GetBCLeaLEDBkgColor write SetBCLeaLEDBkgColor default clBtnFace;
    property LED_Style: TZStyle read GetBCLeaLEDStyle write SetBCLeaLEDStyle default zsRaised;
    property LED_Size: integer read GetBCLeaLEDSize write SetBCLeaLEDSize default 15;
    property LED_Altitude: integer read GetBCLeaLEDAltitude write SetBCLeaLEDAltitude default 2;
    property QLED_ColorOn: TColor read GetBCLeaQLEDColorOn write SetBCLeaQLEDColorOn default TColor($00FF9C15);
    property QLED_ColorOff: TColor read GetBCLeaQLEDColorOff write SetBCLeaQLEDColorOff default TColor($009E5A00);
    property QLED_BkgColor: TColor read GetBCLeaQLEDBkgColor write SetBCLeaQLEDBkgColor default clBtnFace;
    property QLED_Style: TZStyle read GetBCLeaQLEDStyle write SetBCLeaQLEDStyle default zsRaised;
    property QLED_Size: integer read GetBCLeaQLEDSize write SetBCLeaQLEDSize default 20;
    property QLED_Altitude: integer read GetBCLeaQLEDAltitude write SetBCLeaQLEDAltitude default 2;
    property QLED_Rounding: integer read GetBCLeaQLEDRounding write SetBCLeaQLEDRounding default 3;
    property SEL_LineColor: TColor read GetBSELLineColor write SetBSELLineColor default TColor($009E5A00);
    property SEL_LineBkgColor: TColor read GetBSELLineBkgColor write SetBSELLineBkgColor default TColor($00D3D3D3);
    property SEL_LineWidth: integer read GetBSELLineWidth write SetBSELLineWidth default 0;
    property SEL_FontShadowColor: TColor read GetBSELFontShadowColor write SetBSELFontShadowColor default clBlack;
    property SEL_FontShadowOffsetX: integer read GetBSELFontShadowOffsetX write SetBSELFontShadowOffsetX default 2;
    property SEL_FontShadowOffsetY: integer read GetBSELFontShadowOffsetY write SetBSELFontShadowOffsetY default 2;
    property SEL_FontShadowRadius: integer read GetBSELFontShadowRadius write SetBSELFontShadowRadius default 4;
    property SEL_BkgColor: TColor read GetBSELBkgColor write SetBSELBkgColor default clBtnFace;
    property SEL_PointerSize: integer read GetBSELPointerSize write SetBSELPointerSize default 2;
    property SEL_Style: TZStyle read GetBSELStyle write SetBSELStyle default zsRaised;
    property SEL_DrawTextPhong: boolean read GetBSELDrawTextPhong write SetBSELDrawTextPhong default False;
    property SEL_Altitude: integer read GetBSELAltitude write SetBSELAltitude default 2;
    property RS_LineColor: TColor read GetBRSLineColor write SetBRSLineColor default TColor($009E5A00);
    property RS_LineBkgColor: TColor read GetBRSLineBkgColor write SetBRSLineBkgColor default TColor($00D3D3D3);
    property RS_LineWidth: integer read GetBRSLineWidth write SetBRSLineWidth default 0;
    property RS_FontShadowColor: TColor read GetBRSFontShadowColor write SetBRSFontShadowColor default clBlack;
    property RS_FontShadowOffsetX: integer read GetBRSFontShadowOffsetX write SetBRSFontShadowOffsetX default 2;
    property RS_FontShadowOffsetY: integer read GetBRSFontShadowOffsetY write SetBRSFontShadowOffsetY default 2;
    property RS_FontShadowRadius: integer read GetBRSFontShadowRadius write SetBRSFontShadowRadius default 4;
    property RS_BkgColor: TColor read GetBRSBkgColor write SetBRSBkgColor default clBtnFace;
    property RS_PointerSize: integer read GetBRSPointerSize write SetBRSPointerSize default 2;
    property RS_PointerColor: TColor read GetBRSPointerColor write SetBRSPointerColor default TColor($00FF9C15);
    property RS_Style: TZStyle read GetBRSStyle write SetBRSStyle default zsRaised;
    property RS_DrawTextPhong: boolean read GetBRSDrawTextPhong write SetBRSDrawTextPhong default False;
    property RS_Altitude: integer read GetBRSAltitude write SetBRSAltitude default 2;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCLeaTheme]);
end;

constructor TBCLeaTheme.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThemeSetCommon := TBCLeaThemeCommon.Create;
  FThemeSetLCD := TBCLeaThemeLCD.Create;
  FThemeSetLED := TBCLeaThemeLED.Create;
  FThemeSetSelector := TBCLeaThemeSelector.Create;
  FThemeSetRingSlider := TBCLeaThemeRingSlider.Create;
  FThemeSetQLED := TBCLeaThemeQLED.Create;
  Initialize;
end;

destructor TBCLeaTheme.Destroy;
begin
  FreeAndNil(FThemeSetCommon);
  FreeAndNil(FThemeSetLCD);
  FreeAndNil(FThemeSetLED);
  FreeAndNil(FThemeSetSelector);
  FreeAndNil(FThemeSetRingSlider);
  FreeAndNil(FThemeSetQLED);
  inherited Destroy;
end;

procedure TBCLeaTheme.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(self);
end;

function TBCLeaTheme.GetLightSourceIntensity: single;
begin
  Result := FThemeSetCommon.FLightSourceIntensity;
end;

function TBCLeaTheme.GetLightSourceDistanceTerm: single;
begin
  Result := FThemeSetCommon.FLightSourceDistanceTerm;
end;

function TBCLeaTheme.GetLightSourceDistanceFactor: single;
begin
  Result := FThemeSetCommon.FLightSourceDistanceFactor;
end;

function TBCLeaTheme.GetLightDestFactor: single;
begin
  Result := FThemeSetCommon.FLightDestFactor;
end;

function TBCLeaTheme.GetLightColor: TColor;
begin
  Result := FThemeSetCommon.FLightColor;
end;

function TBCLeaTheme.GetSpecularFactor: single;
begin
  Result := FThemeSetCommon.FSpecularFactor;
end;

function TBCLeaTheme.GetSpecularIndex: single;
begin
  Result := FThemeSetCommon.FSpecularIndex;
end;

function TBCLeaTheme.GetAmbientFactor: single;
begin
  Result := FThemeSetCommon.FAmbientFactor;
end;

function TBCLeaTheme.GetDiffusionFactor: single;
begin
  Result := FThemeSetCommon.FDiffusionFactor;
end;

function TBCLeaTheme.GetNegativeDiffusionFactor: single;
begin
  Result := FThemeSetCommon.FNegativeDiffusionFactor;
end;

function TBCLeaTheme.GetDiffuseSaturation: boolean;
begin
  Result := FThemeSetCommon.FDiffuseSaturation;
end;

function TBCLeaTheme.GetLightPositionX: integer;
begin
  Result := FThemeSetCommon.FLightPositionX;
end;

function TBCLeaTheme.GetLightPositionY: integer;
begin
  Result := FThemeSetCommon.FLightPositionY;
end;

function TBCLeaTheme.GetLightPositionZ: integer;
begin
  Result := FThemeSetCommon.FLightPositionZ;
end;

function TBCLeaTheme.GetBLCDFrameColor: TColor;
begin
  Result := FThemeSetLCD.FFrameColor;
end;

function TBCLeaTheme.GetBLCDBoardColor: TColor;
begin
  Result := FThemeSetLCD.FBoardColor;
end;

function TBCLeaTheme.GetBLCDDotColorOn: TColor;
begin
  Result := FThemeSetLCD.FDotColorOn;
end;

function TBCLeaTheme.GetBLCDFrameAltitude: integer;
begin
  Result := FThemeSetLCD.FFrameAltitude;
end;

function TBCLeaTheme.GetBLCDFrameHeight: integer;
begin
  Result := FThemeSetLCD.FFrameHeight;
end;

function TBCLeaTheme.GetBLCDFrameSize: integer;
begin
  Result := FThemeSetLCD.FFrameSize;
end;

function TBCLeaTheme.GetBLCDFrameStyle: TZStyle;
begin
  Result := FThemeSetLCD.FFrameStyle;
end;

function TBCLeaTheme.GetBLCDDotShape: TDotShape;
begin
  Result := FThemeSetLCD.FDotShape;
end;

function TBCLeaTheme.GetBLCDDotSize: integer;
begin
  Result := FThemeSetLCD.FDotSize;
end;

function TBCLeaTheme.GetBLCDDotsSpace: integer;
begin
  Result := FThemeSetLCD.FDotsSpace;
end;

function TBCLeaTheme.GetBLCDDotBlend: boolean;
begin
  Result := FThemeSetLCD.FDotBlend;
end;

function TBCLeaTheme.GetBLCDDotBlendOperation: TBlendOperation;
begin
  Result := FThemeSetLCD.FDotBlendOperation;
end;

function TBCLeaTheme.GetBLCDDotBlur: boolean;
begin
  Result := FThemeSetLCD.FDotBlur;
end;

function TBCLeaTheme.GetBLCDDotBlurRadius: single;
begin
  Result := FThemeSetLCD.FDotBlurRadius;
end;

function TBCLeaTheme.GetBLCDBoardShadow: TBoardShadow;
begin
  Result := FThemeSetLCD.FBoardShadow;
end;

function TBCLeaTheme.GetBCLeaLEDColorOn: TColor;
begin
  Result := FThemeSetLED.FColorOn;
end;

function TBCLeaTheme.GetBCLeaLEDColorOff: TColor;
begin
  Result := FThemeSetLED.FColorOff;
end;

function TBCLeaTheme.GetBCLeaLEDBkgColor: TColor;
begin
  Result := FThemeSetLED.FBkgColor;
end;

function TBCLeaTheme.GetBCLeaLEDStyle: TZStyle;
begin
  Result := FThemeSetLED.FStyle;
end;

function TBCLeaTheme.GetBCLeaLEDSize: integer;
begin
  Result := FThemeSetLED.FSize;
end;

function TBCLeaTheme.GetBCLeaLEDAltitude: integer;
begin
  Result := FThemeSetLED.FAltitude;
end;

function TBCLeaTheme.GetBCLeaQLEDColorOn: TColor;
begin
  Result := FThemeSetQLED.FColorOn;
end;

function TBCLeaTheme.GetBCLeaQLEDColorOff: TColor;
begin
  Result := FThemeSetQLED.FColorOff;
end;

function TBCLeaTheme.GetBCLeaQLEDBkgColor: TColor;
begin
  Result := FThemeSetQLED.FBkgColor;
end;

function TBCLeaTheme.GetBCLeaQLEDStyle: TZStyle;
begin
  Result := FThemeSetQLED.FStyle;
end;

function TBCLeaTheme.GetBCLeaQLEDSize: integer;
begin
  Result := FThemeSetQLED.FSize;
end;

function TBCLeaTheme.GetBCLeaQLEDAltitude: integer;
begin
  Result := FThemeSetQLED.FAltitude;
end;

function TBCLeaTheme.GetBCLeaQLEDRounding: integer;
begin
  Result := FThemeSetQLED.FRounding;
end;

function TBCLeaTheme.GetBSELLineColor: TColor;
begin
  Result := FThemeSetSelector.FLineColor;
end;

function TBCLeaTheme.GetBSELLineBkgColor: TColor;
begin
  Result := FThemeSetSelector.FLineBkgColor;
end;

function TBCLeaTheme.GetBSELLineWidth: integer;
begin
  Result := FThemeSetSelector.FLineWidth;
end;

function TBCLeaTheme.GetBSELFontShadowColor: TColor;
begin
  Result := FThemeSetSelector.FFontShadowColor;
end;

function TBCLeaTheme.GetBSELFontShadowOffsetX: integer;
begin
  Result := FThemeSetSelector.FFontShadowOffsetX;
end;

function TBCLeaTheme.GetBSELFontShadowOffsetY: integer;
begin
  Result := FThemeSetSelector.FFontShadowOffsetY;
end;

function TBCLeaTheme.GetBSELFontShadowRadius: integer;
begin
  Result := FThemeSetSelector.FFontShadowRadius;
end;

function TBCLeaTheme.GetBSELBkgColor: TColor;
begin
  Result := FThemeSetSelector.FBkgColor;
end;

function TBCLeaTheme.GetBSELPointerSize: integer;
begin
  Result := FThemeSetSelector.FPointerSize;
end;

function TBCLeaTheme.GetBSELStyle: TZStyle;
begin
  Result := FThemeSetSelector.FStyle;
end;

function TBCLeaTheme.GetBSELDrawTextPhong: boolean;
begin
  Result := FThemeSetSelector.FDrawTextPhong;
end;

function TBCLeaTheme.GetBSELAltitude: integer;
begin
  Result := FThemeSetSelector.FAltitude;
end;

function TBCLeaTheme.GetBRSLineColor: TColor;
begin
  Result := FThemeSetRingSlider.FLineColor;
end;

function TBCLeaTheme.GetBRSLineBkgColor: TColor;
begin
  Result := FThemeSetRingSlider.FLineBkgColor;
end;

function TBCLeaTheme.GetBRSLineWidth: integer;
begin
  Result := FThemeSetRingSlider.FLineWidth;
end;

function TBCLeaTheme.GetBRSFontShadowColor: TColor;
begin
  Result := FThemeSetRingSlider.FFontShadowColor;
end;

function TBCLeaTheme.GetBRSFontShadowOffsetX: integer;
begin
  Result := FThemeSetRingSlider.FFontShadowOffsetX;
end;

function TBCLeaTheme.GetBRSFontShadowOffsetY: integer;
begin
  Result := FThemeSetRingSlider.FFontShadowOffsetY;
end;

function TBCLeaTheme.GetBRSFontShadowRadius: integer;
begin
  Result := FThemeSetRingSlider.FFontShadowRadius;
end;

function TBCLeaTheme.GetBRSBkgColor: TColor;
begin
  Result := FThemeSetRingSlider.FBkgColor;
end;

function TBCLeaTheme.GetBRSPointerSize: integer;
begin
  Result := FThemeSetRingSlider.FPointerSize;
end;

function TBCLeaTheme.GetBRSPointerColor: TColor;
begin
  Result := FThemeSetRingSlider.FPointerColor;
end;

function TBCLeaTheme.GetBRSStyle: TZStyle;
begin
  Result := FThemeSetRingSlider.FStyle;
end;

function TBCLeaTheme.GetBRSDrawTextPhong: boolean;
begin
  Result := FThemeSetRingSlider.FDrawTextPhong;
end;

function TBCLeaTheme.GetBRSAltitude: integer;
begin
  Result := FThemeSetRingSlider.FAltitude;
end;
//============================================================================
procedure TBCLeaTheme.SetLightSourceIntensity(const AValue: single);
begin
  if AValue = FThemeSetCommon.FLightSourceIntensity then
    Exit;
  FThemeSetCommon.FLightSourceIntensity := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetLightSourceDistanceTerm(const AValue: single);
begin
  if AValue = FThemeSetCommon.FLightSourceDistanceTerm then
    Exit;
  FThemeSetCommon.FLightSourceDistanceTerm := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetLightSourceDistanceFactor(const AValue: single);
begin
  if AValue = FThemeSetCommon.FLightSourceDistanceFactor then
    Exit;
  FThemeSetCommon.FLightSourceDistanceFactor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetLightDestFactor(const AValue: single);
begin
  if AValue = FThemeSetCommon.FLightDestFactor then
    Exit;
  FThemeSetCommon.FLightDestFactor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetLightColor(const AValue: TColor);
begin
  if AValue = FThemeSetCommon.FLightColor then
    Exit;
  FThemeSetCommon.FLightColor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetSpecularFactor(const AValue: single);
begin
  if AValue = FThemeSetCommon.FSpecularFactor then
    Exit;
  FThemeSetCommon.FSpecularFactor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetSpecularIndex(const AValue: single);
begin
  if AValue = FThemeSetCommon.FSpecularIndex then
    Exit;
  FThemeSetCommon.FSpecularIndex := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetAmbientFactor(const AValue: single);
begin
  if AValue = FThemeSetCommon.FAmbientFactor then
    Exit;
  FThemeSetCommon.FAmbientFactor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetDiffusionFactor(const AValue: single);
begin
  if AValue = FThemeSetCommon.FDiffusionFactor then
    Exit;
  FThemeSetCommon.FDiffusionFactor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetNegativeDiffusionFactor(const AValue: single);
begin
  if AValue = FThemeSetCommon.FNegativeDiffusionFactor then
    Exit;
  FThemeSetCommon.FNegativeDiffusionFactor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetDiffuseSaturation(const AValue: boolean);
begin
  if AValue = FThemeSetCommon.FDiffuseSaturation then
    Exit;
  FThemeSetCommon.FDiffuseSaturation := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetLightPositionX(const AValue: integer);
begin
  if AValue = FThemeSetCommon.FLightPositionX then
    Exit;
  FThemeSetCommon.FLightPositionX := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetLightPositionY(const AValue: integer);
begin
  if AValue = FThemeSetCommon.FLightPositionY then
    Exit;
  FThemeSetCommon.FLightPositionY := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetLightPositionZ(const AValue: integer);
begin
  if AValue = FThemeSetCommon.FLightPositionZ then
    Exit;
  FThemeSetCommon.FLightPositionZ := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBLCDFrameColor(const AValue: TColor);
begin
  if AValue = FThemeSetLCD.FFrameColor then
    Exit;
  FThemeSetLCD.FFrameColor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBLCDBoardColor(const AValue: TColor);
begin
  if AValue = FThemeSetLCD.FBoardColor then
    Exit;
  FThemeSetLCD.FBoardColor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBLCDDotColorOn(const AValue: TColor);
begin
  if AValue = FThemeSetLCD.FDotColorOn then
    Exit;
  FThemeSetLCD.FDotColorOn := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBLCDFrameAltitude(const AValue: integer);
begin
  if AValue = FThemeSetLCD.FFrameAltitude then
    Exit;
  FThemeSetLCD.FFrameAltitude := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBLCDFrameHeight(const AValue: integer);
begin
  if AValue = FThemeSetLCD.FFrameHeight then
    Exit;
  FThemeSetLCD.FFrameHeight := AValue;
  if FThemeSetLCD.FFrameSize < FThemeSetLCD.FFrameHeight then FThemeSetLCD.FFrameSize := FThemeSetLCD.FFrameHeight;
  DoChange;
end;

procedure TBCLeaTheme.SetBLCDFrameSize(const AValue: integer);
begin
  if AValue = FThemeSetLCD.FFrameSize then
    Exit;
  FThemeSetLCD.FFrameSize := AValue;
  if FThemeSetLCD.FFrameSize < FThemeSetLCD.FFrameHeight then FThemeSetLCD.FFrameHeight := FThemeSetLCD.FFrameSize;
  DoChange;
end;

procedure TBCLeaTheme.SetBLCDFrameStyle(const AValue: TZStyle);
begin
  if AValue = FThemeSetLCD.FFrameStyle then
    Exit;
  FThemeSetLCD.FFrameStyle := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBLCDDotShape(const AValue: TDotShape);
begin
  if AValue = FThemeSetLCD.FDotShape then
    Exit;
  FThemeSetLCD.FDotShape := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBLCDDotSize(const AValue: integer);
begin
  if AValue = FThemeSetLCD.FDotSize then
    Exit;
  FThemeSetLCD.FDotSize := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBLCDDotsSpace(const AValue: integer);
begin
  if AValue = FThemeSetLCD.FDotsSpace then
    Exit;
  FThemeSetLCD.FDotsSpace := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBLCDDotBlend(const AValue: boolean);
begin
  if AValue = FThemeSetLCD.FDotBlend then
    exit;
  FThemeSetLCD.FDotBlend := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBLCDDotBlendOperation(const AValue: TBlendOperation);
begin
  if AValue = FThemeSetLCD.FDotBlendOperation then
    exit;
  FThemeSetLCD.FDotBlendOperation := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBLCDDotBlur(const AValue: boolean);
begin
  if AValue = FThemeSetLCD.FDotBlur then
    exit;
  FThemeSetLCD.FDotBlur := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBLCDDotBlurRadius(const AValue: single);
begin
  if AValue = FThemeSetLCD.FDotBlurRadius then
    exit;
  FThemeSetLCD.FDotBlurRadius := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBLCDBoardShadow(const AValue: TBoardShadow);
begin
  if AValue = FThemeSetLCD.FBoardShadow then
    exit;
  FThemeSetLCD.FBoardShadow := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBCLeaLEDColorOn(AValue: TColor);
begin
  if AValue = FThemeSetLED.FColorOn then
    exit;
  FThemeSetLED.FColorOn := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBCLeaLEDColorOff(AValue: TColor);
begin
  if AValue = FThemeSetLED.FColorOff then
    exit;
  FThemeSetLED.FColorOff := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBCLeaLEDBkgColor(AValue: TColor);
begin
  if AValue = FThemeSetLED.FColorOn then
    exit;
  FThemeSetLED.FBkgColor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBCLeaLEDStyle(AValue: TZStyle);
begin
  if AValue = FThemeSetLED.FStyle then
    exit;
  FThemeSetLED.FStyle := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBCLeaLEDSize(AValue: integer);
begin
  if AValue = FThemeSetLED.FSize then
    exit;
  FThemeSetLED.FSize := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBCLeaLEDAltitude(AValue: integer);
begin
  if AValue = FThemeSetLED.FAltitude then
    exit;
  FThemeSetLED.FAltitude := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBCLeaQLEDColorOn(AValue: TColor);
begin
  if AValue = FThemeSetQLED.FColorOn then
    exit;
  FThemeSetQLED.FColorOn := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBCLeaQLEDColorOff(AValue: TColor);
begin
  if AValue = FThemeSetQLED.FColorOff then
    exit;
  FThemeSetQLED.FColorOff := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBCLeaQLEDBkgColor(AValue: TColor);
begin
  if AValue = FThemeSetQLED.FColorOn then
    exit;
  FThemeSetQLED.FBkgColor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBCLeaQLEDStyle(AValue: TZStyle);
begin
  if AValue = FThemeSetQLED.FStyle then
    exit;
  FThemeSetQLED.FStyle := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBCLeaQLEDSize(AValue: integer);
begin
  if AValue = FThemeSetQLED.FSize then
    exit;
  FThemeSetQLED.FSize := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBCLeaQLEDAltitude(AValue: integer);
begin
  if AValue = FThemeSetQLED.FAltitude then
    exit;
  FThemeSetQLED.FAltitude := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBCLeaQLEDRounding(AValue: integer);
begin
  if AValue = FThemeSetQLED.FRounding then
    exit;
  FThemeSetQLED.FRounding := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBSELLineColor(AValue: TColor);
begin
  if AValue = FThemeSetSelector.FLineColor then
    exit;
  FThemeSetSelector.FLineColor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBSELLineBkgColor(AValue: TColor);
begin
  if AValue = FThemeSetSelector.FLineBkgColor then
    exit;
  FThemeSetSelector.FLineBkgColor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBSELLineWidth(AValue: integer);
begin
  if AValue = FThemeSetSelector.FLineWidth then
    exit;
  FThemeSetSelector.FLineWidth := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBSELFontShadowColor(AValue: TColor);
begin
  if AValue = FThemeSetSelector.FFontShadowColor then
    exit;
  FThemeSetSelector.FFontShadowColor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBSELFontShadowOffsetX(AValue: integer);
begin
  if AValue = FThemeSetSelector.FFontShadowOffsetX then
    exit;
  FThemeSetSelector.FFontShadowOffsetX := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBSELFontShadowOffsetY(AValue: integer);
begin
  if AValue = FThemeSetSelector.FFontShadowOffsetY then
    exit;
  FThemeSetSelector.FFontShadowOffsetY := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBSELFontShadowRadius(AValue: integer);
begin
  if AValue = FThemeSetSelector.FFontShadowRadius then
    exit;
  FThemeSetSelector.FFontShadowRadius := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBSELBkgColor(AValue: TColor);
begin
  if AValue = FThemeSetSelector.FBkgColor then
    exit;
  FThemeSetSelector.FBkgColor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBSELPointerSize(AValue: integer);
begin
  if AValue = FThemeSetSelector.FPointerSize then
    exit;
  FThemeSetSelector.FPointerSize := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBSELStyle(AValue: TZStyle);
begin
  if AValue = FThemeSetSelector.FStyle then
    exit;
  FThemeSetSelector.FStyle := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBSELDrawTextPhong(AValue: boolean);
begin
  if AValue = FThemeSetSelector.FDrawTextPhong then
    exit;
  FThemeSetSelector.FDrawTextPhong := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBSELAltitude(AValue: integer);
begin
  if AValue = FThemeSetSelector.FAltitude then
    exit;
  FThemeSetSelector.FAltitude := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBRSLineColor(AValue: TColor);
begin
  if AValue = FThemeSetRingSlider.FLineColor then
    exit;
  FThemeSetRingSlider.FLineColor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBRSLineBkgColor(AValue: TColor);
begin
  if AValue = FThemeSetRingSlider.FLineBkgColor then
    exit;
  FThemeSetRingSlider.FLineBkgColor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBRSLineWidth(AValue: integer);
begin
  if AValue = FThemeSetRingSlider.FLineWidth then
    exit;
  FThemeSetRingSlider.FLineWidth := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBRSFontShadowColor(AValue: TColor);
begin
  if AValue = FThemeSetRingSlider.FFontShadowColor then
    exit;
  FThemeSetRingSlider.FFontShadowColor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBRSFontShadowOffsetX(AValue: integer);
begin
  if AValue = FThemeSetRingSlider.FFontShadowOffsetX then
    exit;
  FThemeSetRingSlider.FFontShadowOffsetX := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBRSFontShadowOffsetY(AValue: integer);
begin
  if AValue = FThemeSetRingSlider.FFontShadowOffsetY then
    exit;
  FThemeSetRingSlider.FFontShadowOffsetY := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBRSFontShadowRadius(AValue: integer);
begin
  if AValue = FThemeSetRingSlider.FFontShadowRadius then
    exit;
  FThemeSetRingSlider.FFontShadowRadius := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBRSBkgColor(AValue: TColor);
begin
  if AValue = FThemeSetRingSlider.FBkgColor then
    exit;
  FThemeSetRingSlider.FBkgColor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBRSPointerSize(AValue: integer);
begin
  if AValue = FThemeSetRingSlider.FPointerSize then
    exit;
  FThemeSetRingSlider.FPointerSize := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBRSPointerColor(AValue: TColor);
begin
  if AValue = FThemeSetRingSlider.FPointerColor then
    exit;
  FThemeSetRingSlider.FPointerColor := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBRSStyle(AValue: TZStyle);
begin
  if AValue = FThemeSetRingSlider.FStyle then
    exit;
  FThemeSetRingSlider.FStyle := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBRSDrawTextPhong(AValue: boolean);
begin
  if AValue = FThemeSetRingSlider.FDrawTextPhong then
    exit;
  FThemeSetRingSlider.FDrawTextPhong := AValue;
  DoChange;
end;

procedure TBCLeaTheme.SetBRSAltitude(AValue: integer);
begin
  if AValue = FThemeSetRingSlider.FAltitude then
    exit;
  FThemeSetRingSlider.FAltitude := AValue;
  DoChange;
end;

procedure TBCLeaTheme.Initialize;
begin
  FThemeSetCommon.FAmbientFactor := 0.3;
  FThemeSetCommon.FSpecularIndex := 10;
  FThemeSetCommon.FSpecularFactor := 0.6;
  FThemeSetCommon.FLightDestFactor := 1;
  FThemeSetCommon.FLightPositionX := -100;
  FThemeSetCommon.FLightPositionY := -100;
  FThemeSetCommon.FLightPositionZ := 100;
  FThemeSetCommon.FLightSourceIntensity := 500;
  FThemeSetCommon.FLightSourceDistanceTerm := 150;
  FThemeSetCommon.FLightSourceDistanceFactor := 1;
  FThemeSetCommon.FNegativeDiffusionFactor := 0.1;
  FThemeSetCommon.FLightColor := clWhite;
  FThemeSetCommon.FDiffuseSaturation := False;
  FThemeSetCommon.FDiffusionFactor := 0.9;
  FThemeSetLCD.FDotSize := 4;
  FThemeSetLCD.FDotsSpace := 1;
  FThemeSetLCD.FDotShape := stRound;
  FThemeSetLCD.FDotBlend := False;
  FThemeSetLCD.FDotBlendOperation := boGlow;
  FThemeSetLCD.FDotBlur := False;
  FThemeSetLCD.FDotBlurRadius := 0.8;
  FThemeSetLCD.FFrameSize := 8;
  FThemeSetLCD.FFrameHeight := 8;
  FThemeSetLCD.FFrameAltitude := 2;
  FThemeSetLCD.FFrameStyle := zsRaised;
  FThemeSetLCD.FBoardShadow := bsFrame;
  FThemeSetLCD.FFrameColor := clBtnFace;
  FThemeSetLCD.FBoardColor := clBlack;
  FThemeSetLCD.FDotColorOn := clSkyBlue;
  FThemeSetLED.FColorOn := TColor($00FF9C15);
  FThemeSetLED.FColorOff := TColor($009E5A00);
  FThemeSetLED.FBkgColor := clBtnFace;
  FThemeSetLED.FStyle := zsRaised;
  FThemeSetLED.FSize := 15;
  FThemeSetLED.FAltitude := 2;
  FThemeSetSelector.FLineWidth := 8;
  FThemeSetSelector.FLineColor := TColor($009E5A00);
  FThemeSetSelector.FLineBkgColor := TColor($00D3D3D3);
  FThemeSetSelector.FBkgColor := clBtnFace;
  FThemeSetSelector.FFontShadowColor := clBlack;
  FThemeSetSelector.FFontShadowOffsetX := 2;
  FThemeSetSelector.FFontShadowOffsetY := 2;
  FThemeSetSelector.FFontShadowRadius := 4;
  FThemeSetSelector.FPointerSize := 3;
  FThemeSetSelector.FStyle := zsRaised;
  FThemeSetSelector.FDrawTextPhong := False;
  FThemeSetSelector.FAltitude := 2;
  FThemeSetRingSlider.FLineWidth := 8;
  FThemeSetRingSlider.FLineColor := TColor($009E5A00);
  FThemeSetRingSlider.FLineBkgColor := TColor($00D3D3D3);
  FThemeSetRingSlider.FBkgColor := clBtnFace;
  FThemeSetRingSlider.FFontShadowColor := clBlack;
  FThemeSetRingSlider.FFontShadowOffsetX := 2;
  FThemeSetRingSlider.FFontShadowOffsetY := 2;
  FThemeSetRingSlider.FFontShadowRadius := 4;
  FThemeSetRingSlider.FPointerSize := 3;
  FThemeSetRingSlider.FPointerColor := TColor($00FF9C15);
  FThemeSetRingSlider.FStyle := zsRaised;
  FThemeSetRingSlider.FDrawTextPhong := False;
  FThemeSetRingSlider.FAltitude := 2;
  FThemeSetQLED.FColorOn := TColor($00FF9C15);
  FThemeSetQLED.FColorOff := TColor($009E5A00);
  FThemeSetQLED.FBkgColor := clBtnFace;
  FThemeSetQLED.FStyle := zsRaised;
  FThemeSetQLED.FSize := 20;
  FThemeSetQLED.FAltitude := 2;
  FThemeSetQLED.FRounding := 3;
end;

procedure TBCLeaTheme.LoadThemeFromFile(AFileName: string);
var
  doc: TXMLDocument = nil;
  rootNode, componentNode, parentNode, node: TDOMNode;
  nodeName: string;
  version: string;
begin
  try
    ReadXMLFile(doc, AFileName);
    rootNode := doc.DocumentElement;
    if rootNode.NodeName = 'BCLea_Theme' then
    begin
      parentNode := rootNode.FirstChild;
      while Assigned(parentNode) do
      begin
        nodeName := parentNode.NodeName;
        if nodeName = 'Version' then
        begin
          node := parentNode.FirstChild;
          while Assigned(node) do
          begin
            nodeName := node.NodeName;
            if nodeName = 'Version' then
              Version := node.TextContent;
            node := node.NextSibling;
          end;
        end;
        parentNode := parentNode.NextSibling;
      end;
      if version = '1.0' then
      begin
        componentNode := rootNode.FirstChild;
        while Assigned(componentNode) do
        begin
          nodeName := componentNode.NodeName;
          if nodeName = 'BCLeaLCD' then
          begin
            parentNode := componentNode.FirstChild;
            while Assigned(parentNode) do
            begin
              nodeName := parentNode.NodeName;
              if nodeName = 'Frame' then
              begin
                node := parentNode.FirstChild;
                while Assigned(node) do
                begin
                  nodeName := node.NodeName;
                  if nodeName = 'Height' then
                    FThemeSetLCD.FFrameHeight := StrToInt(node.TextContent)
                  else
                  if nodeName = 'Size' then
                    FThemeSetLCD.FFrameSize := StrToInt(node.TextContent)
                  else
                  if nodeName = 'Altitude' then
                    FThemeSetLCD.FFrameAltitude := StrToInt(node.TextContent)
                  else
                  if nodeName = 'Style' then
                    FThemeSetLCD.FFrameStyle := TZStyle(StrToInt(node.TextContent))
                  else
                  if nodeName = 'BoardShadow' then
                    FThemeSetLCD.FBoardShadow := TBoardShadow(StrToInt(node.TextContent));
                  node := node.NextSibling;
                end;
              end
              else
              if nodeName = 'Dot' then
              begin
                node := parentNode.FirstChild;
                while Assigned(node) do
                begin
                  nodeName := node.NodeName;
                  if nodeName = 'Shape' then
                    FThemeSetLCD.FDotShape := TDotShape(StrToInt(node.TextContent))
                  else
                  if nodeName = 'Size' then
                    FThemeSetLCD.FDotSize := StrToInt(node.TextContent)
                  else
                  if nodeName = 'Space' then
                    FThemeSetLCD.FDotsSpace := StrToInt(node.TextContent)
                  else
                  if nodeName = 'Blured' then
                    FThemeSetLCD.FDotBlur := StrToBool(node.TextContent)
                  else
                  if nodeName = 'BlurRadius' then
                    FThemeSetLCD.FDotBlurRadius := StrToFloat(node.TextContent)
                  else
                  if nodeName = 'Blended' then
                    FThemeSetLCD.FDotBlend := StrToBool(node.TextContent)
                  else
                  if nodeName = 'BlendOperation' then
                    FThemeSetLCD.FDotBlendOperation := TBlendOperation(StrToInt(node.TextContent));
                  node := node.NextSibling;
                end;
              end
              else
              if nodeName = 'Colors' then
              begin
                node := parentNode.FirstChild;
                while Assigned(node) do
                begin
                  nodeName := node.NodeName;
                  if nodeName = 'Board' then
                    FThemeSetLCD.FBoardColor := TColor(Hex2Dec(node.TextContent))
                  else
                  if nodeName = 'Frame' then
                    FThemeSetLCD.FFrameColor := TColor(Hex2Dec(node.TextContent))
                  else
                  if nodeName = 'DotOn' then
                    FThemeSetLCD.FDotColorOn := TColor(Hex2Dec(node.TextContent));
                  node := node.NextSibling;
                end;
              end;
              parentNode := parentNode.NextSibling;
            end;
          end
          else
          if nodeName = 'COMMON' then
          begin
            parentNode := componentNode.FirstChild;
            while Assigned(parentNode) do
            begin
              nodeName := parentNode.NodeName;
              if nodeName = 'Light' then
              begin
                node := parentNode.FirstChild;
                while Assigned(node) do
                begin
                  nodeName := node.NodeName;
                  if nodeName = 'SourceIntensity' then
                    FThemeSetCommon.FLightSourceIntensity := StrToFloat(node.TextContent)
                  else
                  if nodeName = 'SourceDistanceTerm' then
                    FThemeSetCommon.FLightSourceDistanceTerm := StrToFloat(node.TextContent)
                  else
                  if nodeName = 'SourceDistanceFactor' then
                    FThemeSetCommon.FLightSourceDistanceFactor := StrToFloat(node.TextContent)
                  else
                  if nodeName = 'DestFactor' then
                    FThemeSetCommon.FLightDestFactor := StrToFloat(node.TextContent)
                  else
                  if nodeName = 'Color' then
                    FThemeSetCommon.FLightColor := TColor(Hex2Dec(node.TextContent))
                  else
                  if nodeName = 'SpecularFacor' then
                    FThemeSetCommon.FSpecularFactor := StrToFloat(node.TextContent)
                  else
                  if nodeName = 'SpecularIndex' then
                    FThemeSetCommon.FSpecularIndex := StrToFloat(node.TextContent)
                  else
                  if nodeName = 'AmbientFactor' then
                    FThemeSetCommon.FAmbientFactor := StrToFloat(node.TextContent)
                  else
                  if nodeName = 'DiffusionFactor' then
                    FThemeSetCommon.FDiffusionFactor := StrToFloat(node.TextContent)
                  else
                  if nodeName = 'NegativeDiffusionFactor' then
                    FThemeSetCommon.FNegativeDiffusionFactor := StrToFloat(node.TextContent)
                  else
                  if nodeName = 'DiffuseSaturation' then
                    FThemeSetCommon.FDiffuseSaturation := StrToBool(node.TextContent)
                  else
                  if nodeName = 'PositionX' then
                    FThemeSetCommon.FLightPositionX := StrToInt(node.TextContent)
                  else
                  if nodeName = 'PositionY' then
                    FThemeSetCommon.FLightPositionY := StrToInt(node.TextContent)
                  else
                  if nodeName = 'PositionZ' then
                    FThemeSetCommon.FLightPositionZ := StrToInt(node.TextContent);
                  node := node.NextSibling;
                end;
              end;
              parentNode := parentNode.NextSibling;
            end;
          end
          else
          if nodeName = 'BCLeaLED' then
          begin
            parentNode := componentNode.FirstChild;
            while Assigned(parentNode) do
            begin
              nodeName := parentNode.NodeName;
              if nodeName = 'Geometry' then
              begin
                node := parentNode.FirstChild;
                while Assigned(node) do
                begin
                  nodeName := node.NodeName;
                  if nodeName = 'Style' then
                    FThemeSetLED.FStyle := TZStyle(StrToInt(node.TextContent))
                  else
                  if nodeName = 'Size' then
                    FThemeSetLED.FSize := StrToInt(node.TextContent)
                  else
                  if nodeName = 'Altitude' then
                    FThemeSetLED.FAltitude := StrToInt(node.TextContent);
                  node := node.NextSibling;
                end;
              end
              else
              if nodeName = 'Colors' then
              begin
                node := parentNode.FirstChild;
                while Assigned(node) do
                begin
                  nodeName := node.NodeName;
                  if nodeName = 'ColorOn' then
                    FThemeSetLED.FColorOn := TColor(Hex2Dec(node.TextContent))
                  else
                  if nodeName = 'ColorOff' then
                    FThemeSetLED.FColorOff := TColor(Hex2Dec(node.TextContent))
                  else
                  if nodeName = 'BkgColor' then
                    FThemeSetLED.FBkgColor := TColor(Hex2Dec(node.TextContent));
                  node := node.NextSibling;
                end;
              end;
              parentNode := parentNode.NextSibling;
            end;
          end
          else
          if nodeName = 'BCLeaQLED' then
          begin
            parentNode := componentNode.FirstChild;
            while Assigned(parentNode) do
            begin
              nodeName := parentNode.NodeName;
              if nodeName = 'Geometry' then
              begin
                node := parentNode.FirstChild;
                while Assigned(node) do
                begin
                  nodeName := node.NodeName;
                  if nodeName = 'Style' then
                    FThemeSetQLED.FStyle := TZStyle(StrToInt(node.TextContent))
                  else
                  if nodeName = 'Size' then
                    FThemeSetQLED.FSize := StrToInt(node.TextContent)
                  else
                  if nodeName = 'Altitude' then
                    FThemeSetQLED.FAltitude := StrToInt(node.TextContent)
                  else
                  if nodeName = 'Rounding' then
                    FThemeSetQLED.FRounding := StrToInt(node.TextContent);
                  node := node.NextSibling;
                end;
              end
              else
              if nodeName = 'Colors' then
              begin
                node := parentNode.FirstChild;
                while Assigned(node) do
                begin
                  nodeName := node.NodeName;
                  if nodeName = 'ColorOn' then
                    FThemeSetQLED.FColorOn := TColor(Hex2Dec(node.TextContent))
                  else
                  if nodeName = 'ColorOff' then
                    FThemeSetQLED.FColorOff := TColor(Hex2Dec(node.TextContent))
                  else
                  if nodeName = 'BkgColor' then
                    FThemeSetQLED.FBkgColor := TColor(Hex2Dec(node.TextContent));
                  node := node.NextSibling;
                end;
              end;
              parentNode := parentNode.NextSibling;
            end;
          end
          else
          if nodeName = 'BCLeaSelector' then
          begin
            parentNode := componentNode.FirstChild;
            while Assigned(parentNode) do
            begin
              nodeName := parentNode.NodeName;
              if nodeName = 'Geometry' then
              begin
                node := parentNode.FirstChild;
                while Assigned(node) do
                begin
                  nodeName := node.NodeName;
                  if nodeName = 'Style' then
                    FThemeSetSelector.FStyle := TZStyle(StrToInt(node.TextContent))
                  else
                  if nodeName = 'LineWidth' then
                    FThemeSetSelector.FLineWidth := StrToInt(node.TextContent)
                  else
                  if nodeName = 'Altitude' then
                    FThemeSetSelector.FAltitude := StrToInt(node.TextContent)
                  else
                  if nodeName = 'PointerSize' then
                    FThemeSetSelector.FPointerSize := StrToInt(node.TextContent);
                  node := node.NextSibling;
                end;
              end
              else
              if nodeName = 'Colors' then
              begin
                node := parentNode.FirstChild;
                while Assigned(node) do
                begin
                  nodeName := node.NodeName;
                  if nodeName = 'Line' then
                    FThemeSetSelector.FLineColor := TColor(Hex2Dec(node.TextContent))
                  else
                  if nodeName = 'LineBkg' then
                    FThemeSetSelector.FLineBkgColor := TColor(Hex2Dec(node.TextContent))
                  else
                  if nodeName = 'BkgColor' then
                    FThemeSetSelector.FBkgColor := TColor(Hex2Dec(node.TextContent));
                  node := node.NextSibling;
                end;
              end
              else
              if nodeName = 'Text' then
              begin
                node := parentNode.FirstChild;
                while Assigned(node) do
                begin
                  nodeName := node.NodeName;
                  if nodeName = 'ShadowColor' then
                    FThemeSetSelector.FFontShadowColor := TColor(Hex2Dec(node.TextContent))
                  else
                  if nodeName = 'ShadowOffsetX' then
                    FThemeSetSelector.FFontShadowOffsetX := StrToInt(node.TextContent)
                  else
                  if nodeName = 'ShadowOffsetY' then
                    FThemeSetSelector.FFontShadowOffsetY := StrToInt(node.TextContent)
                  else
                  if nodeName = 'ShadowRadius' then
                    FThemeSetSelector.FFontShadowRadius := StrToInt(node.TextContent)
                  else
                  if nodeName = 'Draw3D' then
                    FThemeSetSelector.FDrawTextPhong := StrToBool(node.TextContent);
                  node := node.NextSibling;
                end;
              end;
              parentNode := parentNode.NextSibling;
            end;
          end
          else
          if nodeName = 'BCLeaRingSlider' then
          begin
            parentNode := componentNode.FirstChild;
            while Assigned(parentNode) do
            begin
              nodeName := parentNode.NodeName;
              if nodeName = 'Geometry' then
              begin
                node := parentNode.FirstChild;
                while Assigned(node) do
                begin
                  nodeName := node.NodeName;
                  if nodeName = 'Style' then
                    FThemeSetRingSlider.FStyle := TZStyle(StrToInt(node.TextContent))
                  else
                  if nodeName = 'LineWidth' then
                    FThemeSetRingSlider.FLineWidth := StrToInt(node.TextContent)
                  else
                  if nodeName = 'Altitude' then
                    FThemeSetRingSlider.FAltitude := StrToInt(node.TextContent)
                  else
                  if nodeName = 'PointerSize' then
                    FThemeSetRingSlider.FPointerSize := StrToInt(node.TextContent);
                  node := node.NextSibling;
                end;
              end
              else
              if nodeName = 'Colors' then
              begin
                node := parentNode.FirstChild;
                while Assigned(node) do
                begin
                  nodeName := node.NodeName;
                  if nodeName = 'Line' then
                    FThemeSetRingSlider.FLineColor := TColor(Hex2Dec(node.TextContent))
                  else
                  if nodeName = 'LineBkg' then
                    FThemeSetRingSlider.FLineBkgColor := TColor(Hex2Dec(node.TextContent))
                  else
                  if nodeName = 'BkgColor' then
                    FThemeSetRingSlider.FBkgColor := TColor(Hex2Dec(node.TextContent))
                  else
                  if nodeName = 'PointerColor' then
                    FThemeSetRingSlider.FPointerColor := TColor(Hex2Dec(node.TextContent));
                  node := node.NextSibling;
                end;
              end
              else
              if nodeName = 'Text' then
              begin
                node := parentNode.FirstChild;
                while Assigned(node) do
                begin
                  nodeName := node.NodeName;
                  if nodeName = 'ShadowColor' then
                    FThemeSetRingSlider.FFontShadowColor := TColor(Hex2Dec(node.TextContent))
                  else
                  if nodeName = 'ShadowOffsetX' then
                    FThemeSetRingSlider.FFontShadowOffsetX := StrToInt(node.TextContent)
                  else
                  if nodeName = 'ShadowOffsetY' then
                    FThemeSetRingSlider.FFontShadowOffsetY := StrToInt(node.TextContent)
                  else
                  if nodeName = 'ShadowRadius' then
                    FThemeSetRingSlider.FFontShadowRadius := StrToInt(node.TextContent)
                  else
                  if nodeName = 'Draw3D' then
                    FThemeSetRingSlider.FDrawTextPhong := StrToBool(node.TextContent);
                  node := node.NextSibling;
                end;
              end;
              parentNode := parentNode.NextSibling;
            end;
          end;
          { here comes the next component
          if nodeName = 'BLCD' then  }
          componentNode := componentNode.NextSibling;
        end;
      end
      else
        raise Exception.Create('Not version 1.0 file');
    end
    else
      raise Exception.Create('Not a BCLea theme file');
  finally
    doc.Free;
  end;
  DoChange;
end;

procedure TBCLeaTheme.SaveThemeToFile(AFileName: string);
var
  doc: TXMLDocument;
  rootNode, componentNode, parentNode, node, textNode: TDOMNode;
begin
  doc := TXMLDocument.Create;
  try
    rootNode := doc.CreateElement('BCLea_Theme');
    doc.AppendChild(rootNode);

    rootNode := doc.DocumentElement;

    componentNode := doc.CreateElement('Version');
    rootNode.AppendChild(componentNode);

    node := doc.CreateElement('Version');
    componentNode.AppendChild(node);
    textNode := doc.CreateTextNode('1.0');
    node.AppendChild(textNode);

    //COMMON
    componentNode := doc.CreateElement('COMMON');
    rootNode.AppendChild(componentNode);

    parentNode := doc.CreateElement('Light');
    componentNode.AppendChild(parentNode);

    node := doc.CreateElement('SourceIntensity');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(FloatToStr(FThemeSetCommon.FLightSourceIntensity));
    node.AppendChild(textNode);

    node := doc.CreateElement('SourceDistanceTerm');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(FloatToStr(FThemeSetCommon.FLightSourceDistanceTerm));
    node.AppendChild(textNode);

    node := doc.CreateElement('SourceDistanceFactor');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(FloatToStr(FThemeSetCommon.FLightSourceDistanceFactor));
    node.AppendChild(textNode);

    node := doc.CreateElement('DestFactor');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(FloatToStr(FThemeSetCommon.FLightDestFactor));
    node.AppendChild(textNode);

    node := doc.CreateElement('Color');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToHex(FThemeSetCommon.FLightColor));
    node.AppendChild(textNode);

    node := doc.CreateElement('SpecularFactor');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(FloatToStr(FThemeSetCommon.FSpecularFactor));
    node.AppendChild(textNode);

    node := doc.CreateElement('SpecularIndex');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(FloatToStr(FThemeSetCommon.FSpecularIndex));
    node.AppendChild(textNode);

    node := doc.CreateElement('AmbientFactor');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(FloatToStr(FThemeSetCommon.FAmbientFactor));
    node.AppendChild(textNode);

    node := doc.CreateElement('DiffusionFactor');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(FloatToStr(FThemeSetCommon.FDiffusionFactor));
    node.AppendChild(textNode);

    node := doc.CreateElement('NegativeDiffusionFactor');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(FloatToStr(FThemeSetCommon.FNegativeDiffusionFactor));
    node.AppendChild(textNode);

    node := doc.CreateElement('DiffuseSaturation');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(BoolToStr(FThemeSetCommon.FDiffuseSaturation));
    node.AppendChild(textNode);

    node := doc.CreateElement('PositionX');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(FloatToStr(FThemeSetCommon.FLightPositionX));
    node.AppendChild(textNode);

    node := doc.CreateElement('PositionY');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(FloatToStr(FThemeSetCommon.FLightPositionY));
    node.AppendChild(textNode);

    node := doc.CreateElement('PositionZ');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(FloatToStr(FThemeSetCommon.FLightPositionZ));
    node.AppendChild(textNode);

    //BCLeaLCD
    componentNode := doc.CreateElement('BCLeaLCD');
    rootNode.AppendChild(componentNode);

    parentNode := doc.CreateElement('Frame');
    componentNode.AppendChild(parentNode);

    node := doc.CreateElement('Height');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetLCD.FFrameHeight));
    node.AppendChild(textNode);

    node := doc.CreateElement('Size');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetLCD.FFrameSize));
    node.AppendChild(textNode);

    node := doc.CreateElement('Altitude');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetLCD.FFrameAltitude));
    node.AppendChild(textNode);

    node := doc.CreateElement('Style');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(integer(FThemeSetLCD.FFrameStyle)));
    node.AppendChild(textNode);

    node := doc.CreateElement('BoardShadow');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(integer(FThemeSetLCD.FBoardShadow)));
    node.AppendChild(textNode);

    parentNode := doc.CreateElement('Dot');
    componentNode.AppendChild(parentNode);

    node := doc.CreateElement('Shape');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(integer(FThemeSetLCD.FDotShape)));
    node.AppendChild(textNode);

    node := doc.CreateElement('Size');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetLCD.FDotSize));
    node.AppendChild(textNode);

    node := doc.CreateElement('Space');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetLCD.FDotsSpace));
    node.AppendChild(textNode);

    node := doc.CreateElement('Blured');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(BoolToStr(FThemeSetLCD.FDotBlur));
    node.AppendChild(textNode);

    node := doc.CreateElement('BlurRadius');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(FloatToStr(FThemeSetLCD.FDotBlurRadius));
    node.AppendChild(textNode);

    node := doc.CreateElement('Blended');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(BoolToStr(FThemeSetLCD.FDotBlend));
    node.AppendChild(textNode);

    node := doc.CreateElement('BlendOperation');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(integer(FThemeSetLCD.FDotBlendOperation)));
    node.AppendChild(textNode);

    parentNode := doc.CreateElement('Colors');
    componentNode.AppendChild(parentNode);

    node := doc.CreateElement('Board');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToHex(integer(FThemeSetLCD.FBoardColor)));
    node.AppendChild(textNode);

    node := doc.CreateElement('Frame');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToHex(integer(FThemeSetLCD.FFrameColor)));
    node.AppendChild(textNode);

    node := doc.CreateElement('DotOn');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToHex(integer(FThemeSetLCD.FDotColorOn)));
    node.AppendChild(textNode);

    //BCLeaLED
    componentNode := doc.CreateElement('BCLeaLED');
    rootNode.AppendChild(componentNode);

    parentNode := doc.CreateElement('Geometry');
    componentNode.AppendChild(parentNode);

    node := doc.CreateElement('Style');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(integer(FThemeSetLED.FStyle)));
    node.AppendChild(textNode);

    node := doc.CreateElement('Size');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetLED.FSize));
    node.AppendChild(textNode);

    node := doc.CreateElement('Altitude');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetLED.FAltitude));
    node.AppendChild(textNode);

    parentNode := doc.CreateElement('Colors');
    componentNode.AppendChild(parentNode);

    node := doc.CreateElement('ColorOn');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToHex(integer(FThemeSetLED.FColorOn)));
    node.AppendChild(textNode);

    node := doc.CreateElement('ColorOff');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToHex(integer(FThemeSetLED.FColorOff)));
    node.AppendChild(textNode);

    node := doc.CreateElement('BkgColor');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToHex(integer(FThemeSetLED.FBkgColor)));
    node.AppendChild(textNode);

    //BCLeaQLED
    componentNode := doc.CreateElement('BCLeaQLED');
    rootNode.AppendChild(componentNode);

    parentNode := doc.CreateElement('Geometry');
    componentNode.AppendChild(parentNode);

    node := doc.CreateElement('Style');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(integer(FThemeSetQLED.FStyle)));
    node.AppendChild(textNode);

    node := doc.CreateElement('Size');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetQLED.FSize));
    node.AppendChild(textNode);

    node := doc.CreateElement('Altitude');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetQLED.FAltitude));
    node.AppendChild(textNode);

    node := doc.CreateElement('Rounding');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetQLED.FRounding));
    node.AppendChild(textNode);

    parentNode := doc.CreateElement('Colors');
    componentNode.AppendChild(parentNode);

    node := doc.CreateElement('ColorOn');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToHex(integer(FThemeSetQLED.FColorOn)));
    node.AppendChild(textNode);

    node := doc.CreateElement('ColorOff');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToHex(integer(FThemeSetQLED.FColorOff)));
    node.AppendChild(textNode);

    node := doc.CreateElement('BkgColor');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToHex(integer(FThemeSetQLED.FBkgColor)));
    node.AppendChild(textNode);

    //BCLeaSelector
    componentNode := doc.CreateElement('BCLeaSelector');
    rootNode.AppendChild(componentNode);

    parentNode := doc.CreateElement('Geometry');
    componentNode.AppendChild(parentNode);

    node := doc.CreateElement('Style');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(integer(FThemeSetSelector.FStyle)));
    node.AppendChild(textNode);

    node := doc.CreateElement('LineWidth');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetSelector.FLineWidth));
    node.AppendChild(textNode);

    node := doc.CreateElement('Altitude');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetSelector.FAltitude));
    node.AppendChild(textNode);

    node := doc.CreateElement('PointerSize');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetSelector.FPointerSize));
    node.AppendChild(textNode);

    parentNode := doc.CreateElement('Colors');
    componentNode.AppendChild(parentNode);

    node := doc.CreateElement('Line');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToHex(integer(FThemeSetSelector.FLineColor)));
    node.AppendChild(textNode);

    node := doc.CreateElement('LineBkg');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToHex(integer(FThemeSetSelector.FLineBkgColor)));
    node.AppendChild(textNode);

    node := doc.CreateElement('BkgColor');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToHex(integer(FThemeSetSelector.FBkgColor)));
    node.AppendChild(textNode);

    parentNode := doc.CreateElement('Text');
    componentNode.AppendChild(parentNode);

    node := doc.CreateElement('ShadowColor');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToHex(integer(FThemeSetSelector.FFontShadowColor)));
    node.AppendChild(textNode);

    node := doc.CreateElement('ShadowOffsetX');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetSelector.FFontShadowOffsetX));
    node.AppendChild(textNode);

    node := doc.CreateElement('ShadowOffsetY');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetSelector.FFontShadowOffsetY));
    node.AppendChild(textNode);

    node := doc.CreateElement('ShadowRadius');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetSelector.FFontShadowRadius));
    node.AppendChild(textNode);

    node := doc.CreateElement('Draw3D');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(BoolToStr(FThemeSetSelector.FDrawTextPhong));
    node.AppendChild(textNode);

    //BCLeaRingSlider
    componentNode := doc.CreateElement('BCLeaRingSlider');
    rootNode.AppendChild(componentNode);

    parentNode := doc.CreateElement('Geometry');
    componentNode.AppendChild(parentNode);

    node := doc.CreateElement('Style');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(integer(FThemeSetRingSlider.FStyle)));
    node.AppendChild(textNode);

    node := doc.CreateElement('LineWidth');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetRingSlider.FLineWidth));
    node.AppendChild(textNode);

    node := doc.CreateElement('Altitude');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetRingSlider.FAltitude));
    node.AppendChild(textNode);

    node := doc.CreateElement('PointerSize');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetRingSlider.FPointerSize));
    node.AppendChild(textNode);

    parentNode := doc.CreateElement('Colors');
    componentNode.AppendChild(parentNode);

    node := doc.CreateElement('Line');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToHex(integer(FThemeSetRingSlider.FLineColor)));
    node.AppendChild(textNode);

    node := doc.CreateElement('LineBkg');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToHex(integer(FThemeSetRingSlider.FLineBkgColor)));
    node.AppendChild(textNode);

    node := doc.CreateElement('BkgColor');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToHex(integer(FThemeSetRingSlider.FBkgColor)));
    node.AppendChild(textNode);

    node := doc.CreateElement('PointerColor');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToHex(integer(FThemeSetRingSlider.FPointerColor)));
    node.AppendChild(textNode);

    parentNode := doc.CreateElement('Text');
    componentNode.AppendChild(parentNode);

    node := doc.CreateElement('ShadowColor');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToHex(integer(FThemeSetRingSlider.FFontShadowColor)));
    node.AppendChild(textNode);

    node := doc.CreateElement('ShadowOffsetX');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetRingSlider.FFontShadowOffsetX));
    node.AppendChild(textNode);

    node := doc.CreateElement('ShadowOffsetY');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetRingSlider.FFontShadowOffsetY));
    node.AppendChild(textNode);

    node := doc.CreateElement('ShadowRadius');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(IntToStr(FThemeSetRingSlider.FFontShadowRadius));
    node.AppendChild(textNode);

    node := doc.CreateElement('Draw3D');
    parentNode.AppendChild(node);
    textNode := doc.CreateTextNode(BoolToStr(FThemeSetRingSlider.FDrawTextPhong));
    node.AppendChild(textNode);
    WriteXMLFile(doc, AFileName);
  finally
    doc.Free;
  end;
end;

end.
