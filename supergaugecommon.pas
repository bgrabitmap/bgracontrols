// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Part of BGRA Controls. Made by third party.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html

}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)
- Sandy Ganz | sganz@pacbell.net
  Evolved from DTAnalogCommon, specific for New Gauge Work
  Massive overhaul, fixes and features, begat Super Gauge
  Needed to split off as changes broke compatibility badly

Changes
  Renamed Frame property to BorderWidth, breaking change in gauge if property
  is not default.

***************************** END CONTRIBUTOR(S) *****************************}
{******************************** CHANGE LOG *********************************
v2.00 - Sandy Ganz
        Breaking change on Frame Border Width Property, Scale Tic
        Major and Minor Width and Scale Tic Major and Minor Length. Added
        missing property for TickArc width (inner and outer).
        Added Dots option for Tick Marks, and a few related Dot settings.
        Changed RangeLED type of rcGaugeOutOfRange to rcGaugeOverload and events


******************************* END CHANGE LOG *******************************}

unit SuperGaugeCommon;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ELSE}Types, {$ENDIF} Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRAGradients, BCTypes;

type
  TSGFillStyle = (fsNone, fsGradient, fsFlat, fsPhong);   // Add more if needed here
  TSGPointerStyle = (psLine, psLineExt, psArc , psTriangle { psTriangleExt}); // Todo : Add others at some point
  TSGLEDStyle = (lsNone, lsFlat, lsShaded);
  TSGLEDShape = (lshRound, lshSquare, lshTriangle, lshDownTriangle);
  TSGPointerCapPosition = (cpUnder, cpOver);
  TSGCapStyle = (csNone, csFlat, csShaded, csPhong);
  TSGTickArc = (taNone, taOuter, taInner, taBoth);  // Arc above or below ticks, inner/both is automatic on inner, main if exist, minor othewise
  TSGRangeCheckType = (rcNone, rcGaugeOverload, rcBetween, rcBothInclusive, rcStartInclusive,
                      rcEndInclusive, rcBothBetweenOutside, rcBothInclusiveOutside,
                      rcGreaterStart, rcLessEnd, rcGreaterStartInclusive, rcLessEndInclusive);   // added for range check led, see code for details
  TSGMarkerStyle = (msCenter, msLeft, msRight);

  { TSGOrigin }

  TSGOrigin = packed record
    CenterPoint: TPoint;
    Radius: integer;
  end;

  { TSGPointerCapSettings }

  TSGPointerCapSettings = class(TPersistent)
  private
    FEdgeColor: TColor;
    FEdgeWidth: integer;
    FFillColor: TColor;
    FOnChange: TNotifyEvent;
    FRadius: integer;
    FCurveExponent: single;
    FCapStyle: TSGCapStyle;
    FCapPosition: TSGPointerCapPosition;
    FDirty: boolean;

    procedure SetEdgeColor(AValue: TColor);
    procedure SetEdgeWidth(AValue: integer);
    procedure SetFillColor(AValue: TColor);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetRadius(AValue: integer);
    procedure SetLightIntensity(const AValue: integer);
    function GetLightIntensity: integer;
    procedure SetCurveExponent(const AValue: single);
    procedure SetCapStyle(const AValue: TSGCapStyle);
    procedure SetPointerCapPos(const AValue: TSGPointerCapPosition);
    procedure DirtyOnChange;
  protected
  public
    FPhong: TPhongShading;
    property Dirty: boolean read FDirty write FDirty;
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;

  published
    property EdgeColor: TColor read FEdgeColor write SetEdgeColor default clGray;
    property FillColor: TColor read FFillColor write SetFillColor default clBlack;
    property Radius: integer read FRadius write SetRadius default 30;
    property EdgeWidth: integer read FEdgeWidth write SetEdgeWidth default 1;
    property LightIntensity: integer read GetLightIntensity write SetLightIntensity default 300;
    property CurveExponent: single read FCurveExponent write SetCurveExponent default 0.05;
    property CapStyle: TSGCapStyle read FCapStyle write SetCapStyle default csPhong;
    property CapPosition: TSGPointerCapPosition read FCapPosition write SetPointerCapPos default cpUnder;
  end;

  { TSGPointerSettings }

  TSGPointerSettings = class(TPersistent)
  private
    FColor: TColor;
    FLength: integer;
    FExtensionLength: integer;
    FOnChange: TNotifyEvent;
    FWidth: integer;
    FStyle: TSGPointerStyle;
    FHighlightLine: boolean;
    FHighlightColor: TColor;
    FHighlightThickness: integer;
    FEnabled: boolean;
    FDirty: boolean;

    procedure SetColor(AValue: TColor);
    procedure SetLength(AValue: integer);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetWidth(AValue: integer);
    procedure SetStyle(AValue: TSGPointerStyle);
    procedure SetHighlightLine(AValue: boolean);
    procedure SetHighlightColor(AValue: TColor);
    procedure SetHighlightThickness(AValue: integer);
    procedure SetExtensionLength(AValue: integer);
    procedure SetEnabled(AValue: boolean);
    procedure DirtyOnChange;

  protected
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Dirty: boolean read FDirty write FDirty;

  published
    property Color: TColor read FColor write SetColor;
    property Length: integer read FLength write SetLength default 130;
    property ExtensionLength: integer read FExtensionLength write SetExtensionLength default 10;
    property Width: integer read FWidth write SetWidth default 5;
    property Style: TSGPointerStyle read FStyle write SetStyle default psLineExt;
    property HighlightLine: boolean read FHighlightLine write SetHighlightLine default False;
    property HighlightColor: TColor read FHighlightColor write SetHighlightColor default clGray;
    property HighlightThickness: integer read FHighlightThickness write SetHighlightThickness default 1;
    property Enabled: boolean read FEnabled write SetEnabled;
  end;

  { TSGScaleSettings }

  TSGScaleSettings = class(TPersistent)
  private
    FEnableScaleText: boolean;
    FStart: integer;
    FStep: integer;
    FMaximum: integer;
    FMinimum: integer;
    FTextFont: string;
    FTextRadius: integer;
    FTextSize: integer;
    FTextStyle: TFontStyles;
    FTickColor: TColor;
    FEnableMainTicks: boolean;
    FEnableSubTicks: boolean;
    FReverseScale: boolean;
    FMainTickThickness: integer;
    FSubTickThickness: integer;
    FMainTickLength: integer;
    FSubTickLength: integer;
    FMainTickCount: integer;
    FMainTickUseDots: boolean;
    FSubTickUseDots: boolean;
    FOnChange: TNotifyEvent;
    FSubTickCount: integer;
    FTextColor: TColor;
    FScaleRadius: integer;
    FTickArcStyle: TSGTickArc;
    FOuterTickArcThickness: integer;
    FInnerTickArcThickness: integer;
    FTickArcColor: TColor;
    FEnabled: boolean;
    FDirty: boolean;

    procedure SetEnableScaleText(AValue: boolean);
    procedure SetStart(AValue: integer);
    procedure SetStep(AValue: integer);
    procedure SetMaximum(AValue: integer);
    procedure SetMinimum(AValue: integer);
    procedure SetTextFont(AValue: string);
    procedure SetTextRadius(AValue: integer);
    procedure SetTextSize(AValue: integer);
    procedure SetTextStyle(AValue: TFontStyles);
    procedure SetTickColor(AValue: TColor);
    procedure SetEnableMainTicks(AValue: boolean);
    procedure SetEnableSubTicks(AValue: boolean);
    procedure SetReverseScale(AValue: boolean);
    procedure SetMainTickLength(AValue: integer);
    procedure SetSubTickLength(AValue: integer);
    procedure SetMainTickCount(AValue: integer);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetSubTickCount(AValue: integer);
    procedure SetTextColor(AValue: TColor);
    procedure SetMainTickThickness(AValue: integer);
    procedure SetSubTickThickness(AValue: integer);
    procedure SetTickArcStyle(AValue: TSGTickArc);
    procedure SetInnerTickArcThickness(AValue: integer);
    procedure SetOuterTickArcThickness(AValue: integer);
    procedure SetTickArcColor(AValue: TColor);
    procedure SetScaleRadius(AValue: integer);
    procedure SetMainTickUseDots(AValue: boolean);
    procedure SetSubTickUseDots(AValue: boolean);
    procedure SetEnabled(AValue: boolean);
    procedure DirtyOnChange;

  protected
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Dirty: boolean read FDirty write FDirty;

  published
    property TickColor: TColor read FTickColor write SetTickColor;
    property TextColor: TColor read FTextColor write SetTextColor;
    property TextSize: integer read FTextSize write SetTextSize default 24;
    property TextStyle: TFontStyles read FTextStyle write SetTextStyle default [fsBold];
    property TextFont: string read FTextFont write SetTextFont;
    property EnableMainTicks: boolean read FEnableMainTicks write SetEnableMainTicks default True;
    property EnableSubTicks: boolean read FEnableSubTicks write SetEnableSubTicks default True;
    property EnableScaleText: boolean read FEnableScaleText write SetEnableScaleText default True;
    property ReverseScale: boolean read FReverseScale write SetReverseScale default False;
    property Start: integer read FStart write SetStart default 0;
    property Step: integer read FStep write SetStep default 1;
    property MainTickCount: integer read FMainTickCount write SetMainTickCount default 10;
    property SubTickCount: integer read FSubTickCount write SetSubTickCount default 5;
    property MainTickLength: integer read FMainTickLength write SetMainTickLength default 15;
    property SubTickLength: integer read FSubTickLength write SetSubTickLength default 8;
    property MainTickThickness: integer read FMainTickThickness write SetMainTickThickness default 3;
    property SubTickThickness: integer read FSubTickThickness write SetSubTickThickness default 1;
    property TextRadius: integer read FTextRadius write SetTextRadius default 100;
    property ScaleRadius: integer read FScaleRadius write SetScaleRadius default 125;
    property TickArcStyle: TSGTickArc read FTickArcStyle write SetTickArcStyle default taOuter;
    property InnerTickArcThickness: integer read FInnerTickArcThickness write SetInnerTickArcThickness default 3;
    property OuterTickArcThickness: integer read FOuterTickArcThickness write SetOuterTickArcThickness default 3;
    property TickArcColor: TColor read FTickArcColor write SetTickArcColor;
    property MainTickUseDots: boolean read FMainTickUseDots write SetMainTickUseDots default False;
    property SubTickUseDots: boolean read FSubTickUseDots write SetSubTickUseDots default False;
    property Enabled: boolean read FEnabled write SetEnabled;
end;

  { TSGBandSettings }

  TSGBandSettings = class(TPersistent)
  private
    FEnabled: boolean;
    FStartValue: single;
    FEndValue: single;
    FEnableText: boolean;
    FText: TCaption;
    FTextFont: string;
    FTextStyle: TFontStyles;
    FTextRadius: integer;
    FTextSize: integer;
    FTextColor: TColor;
    FOnChange: TNotifyEvent;
    FBandThickness: integer;
    FBandRadius: integer;       // defines the outer Radius length in pixels, likely center of width/thickness
    FBandColor: TColor;
    FDirty: boolean;

    procedure SetEnabled(AValue: boolean);
    procedure SetStartValue(AValue: single);
    procedure SetEndValue(AValue: single);
    procedure SetEnableText(AValue: boolean);
    procedure SetText(AValue: TCaption);
    procedure SetTextSize(AValue: integer);
    procedure SetTextFont(AValue: string);
    procedure SetTextStyle(AValue: TFontStyles);
    procedure SetTextRadius(AValue: integer);
    procedure SetTextColor(AValue: TColor);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetBandThickness(AValue: integer);
    procedure SetBandRadius(AValue: integer);
    procedure SetBandColor(AValue: TColor);
    procedure DirtyOnChange;

  protected
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Dirty: boolean read FDirty write FDirty;

  published
    property Enabled: boolean read FEnabled write SetEnabled default False;
    property StartValue: single read FStartValue write SetStartValue default 0.0;
    property EndValue: single read FEndValue write SetEndValue default 100.0;
    property EnableText: boolean read FEnableText write SetEnableText default False;
    property Text: TCaption read FText write SetText;
    property TextSize: integer read FTextSize write SetTextSize default 14;
    property TextFont: string read FTextFont write SetTextFont;
    property TextStyle: TFontStyles read FTextStyle write SetTextStyle;
    property TextRadius: integer read FTextRadius write SetTextRadius default 90;
    property TextColor: TColor read FTextColor write SetTextColor;
    property BandThickness: integer read FBandThickness write SetBandThickness default 25;
    property BandRadius: integer read FBandRadius write SetBandRadius default 85;
    Property BandColor: TColor read FBandColor write SetBandColor default clGreen;
  end;

  { TSGFaceSettings }

  TSGFaceSettings = class(TPersistent)
  private
    FInnerColor: TColor;
    FOuterColor: TColor;
    FFillStyle: TSGFillStyle;
    FPicture: TPicture;
    FPictureEnabled: boolean;
    FPictureOffsetX, FPictureOffsetY: integer;
    FCurveExponent: single;
    FOnChange: TNotifyEvent;
    FDirty: boolean;

    procedure SetInnerColor(AValue: TColor);
    procedure SetOuterColor(AValue: TColor);
    procedure SetFillStyle(AValue: TSGFillStyle);
    procedure SetPicture(AValue: TPicture);
    procedure SetPictureEnabled(AValue: boolean);
    procedure SetPictureOffsetX(AValue: integer);
    procedure SetPictureOffsetY(AValue: integer);
    procedure SetLightIntensity(const AValue: integer);
    function GetLightIntensity: integer;
    procedure SetCurveExponent(const AValue: single);

    procedure SetOnChange(AValue: TNotifyEvent);
    procedure DirtyOnChange;
  protected
  public
    FPhong: TPhongShading;

    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Dirty: boolean read FDirty write FDirty;

  published
    property FillStyle: TSGFillStyle read FFillStyle write SetFillStyle default fsGradient;
    property InnerColor: TColor read FInnerColor write SetInnerColor default clGray;
    property OuterColor: TColor read FOuterColor write SetOuterColor default clBlack;
    property Picture: TPicture read FPicture write SetPicture;
    property PictureEnabled: boolean read FPictureEnabled write SetPictureEnabled default False;
    property PictureOffsetX: integer read FPictureOffsetX write SetPictureOffsetX default -30;
    property PictureOffsetY: integer read FPictureOffsetY write SetPictureOffsetY default 60;
    property LightIntensity: integer read GetLightIntensity write SetLightIntensity default 300;
    property CurveExponent: single read FCurveExponent write SetCurveExponent default 0.05;
  end;

  { TSGFrameSettings }

  TSGFrameSettings = class(TPersistent)
  private
    FOuterFrameColor: TColor;
    FMiddleFrameColor: TColor;
    FInnerFrameColor: TColor;
    FOuterFrameThickness: integer;
    FMiddleFrameThickness: integer;
    FInnerFrameThickness: integer;

    FOnChange: TNotifyEvent;
    FDirty: boolean;

    procedure SetOuterFrameColor(AValue: TColor);
    procedure SetMiddleFrameColor(AValue: TColor);
    procedure SetInnerFrameColor(AValue: TColor);
    procedure SetOuterFrameThickness(AValue: integer);
    procedure SetMiddleFrameThickness(AValue: integer);
    procedure SetInnerFrameThickness(AValue: integer);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure DirtyOnChange;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Dirty: boolean read FDirty write FDirty;

  published
    property OuterFrameColor: TColor read FOuterFrameColor write SetOuterFrameColor;
    property MiddleFrameColor: TColor read FMiddleFrameColor write SetMiddleFrameColor;
    property InnerFrameColor: TColor read FInnerFrameColor write SetInnerFrameColor;

    property OuterFrameThickness: integer read FOuterFrameThickness write SetOuterFrameThickness;
    property MiddleFrameThickness: integer read FMiddleFrameThickness write SetMiddleFrameThickness;
    property InnerFrameThickness: integer read FInnerFrameThickness write SetInnerFrameThickness;
  end;

  { TSGLEDSettings }

  TSGLEDSettings = class(TPersistent)
  private
    FActiveColor: TColor;
    FInactiveColor: TColor;
    FBorderColor: TColor;
    FSize: integer;
    FOffsetX, FOffsetY: integer;
    FStyle: TSGLEDStyle;
    FShape: TSGLEDShape;
    FOnChange: TNotifyEvent;
    FActive: boolean;
    FDirty: boolean;

    procedure SetActive(AValue: boolean);
    procedure SetActiveNoDoChange(AValue: boolean);
    procedure SetActiveColor(AValue: TColor);
    procedure SetInactiveColor(AValue: TColor);
    procedure SetBorderColor(AValue: TColor);
    procedure SetSize(AValue: integer);
    procedure SetOffsetX(AValue: integer);
    procedure SetOffsetY(AValue: integer);
    procedure SetStyle(AValue: TSGLEDStyle);
    procedure SetShape(AValue: TSGLEDShape);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure DirtyOnChange;

  protected
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Dirty: boolean read FDirty write FDirty;
    property ActiveNoDoChange: boolean read FActive write SetActiveNoDoChange;

  published
    property ActiveColor: TColor read FActiveColor write SetActiveColor;
    property InactiveColor: TColor read FInactiveColor write SetInactiveColor;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property Size: integer read FSize write SetSize default 10;
    property OffsetX: integer read FOffsetX write SetOffsetX default 50;
    property OffsetY: integer read FOffsetY write SetOffsetY default 90;
    property Style: TSGLEDStyle read FStyle write SetStyle default lsNone;
    property Shape: TSGLEDShape read FShape write SetShape default lshRound;
    property Active: boolean read FActive write SetActive default False;
  end;

  { TSGRangeCheckLEDSettings }

  TSGRangeCheckLEDSettings = class(TSGLEDSettings)
    private
      FRangeStartValue : single;
      FRangeEndValue : single;
      FRangeType: TSGRangeCheckType;
      procedure SetRangeStartValue(AValue: single);
      procedure SetRangeEndValue(AValue: single);
      procedure SetRangeType(AValue: TSGRangeCheckType);

    protected
    public
      constructor Create;
      destructor Destroy; override;

    published
      property RangeStartValue: single read FRangeStartValue write SetRangeStartValue default 0;
      property RangeEndValue: single read FRangeEndValue write SetRangeEndValue default 100;
      property RangeType: TSGRangeCheckType read FRangeType write SetRangeType;
      property ActiveColor;
      property InactiveColor;
      property BorderColor;
      property Size;
      property OffsetX;   // origin at center based offset
      property OffsetY;
      property Style;
  end;

{ TSGTextSettings }

TSGTextSettings = class(TPersistent)
private
  FEnabled: boolean;
  FFontEx: TBCFont;
  FText : TCaption;
  FOffsetX, FOffsetY: integer;
  FOnChange: TNotifyEvent;
  FDirty: boolean;

  procedure SetEnabled(AValue: boolean);
  procedure SetOffsetX(AValue: integer);
  procedure SetOffsetY(AValue: integer);
  procedure SetOnChange(AValue: TNotifyEvent);
  procedure DirtyOnChange;
  procedure SetText(AValue: TCaption);
  procedure SetFontEx(AValue: TBCFont);

protected
public
  constructor Create;
  destructor Destroy; override;
  property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  property Dirty: boolean read FDirty write FDirty;

published
  property Enabled: boolean read FEnabled write SetEnabled default False;
  property FontEx: TBCFont read FFontEx write SetFontEx;
  property Text: TCaption read FText write SetText;
  property OffsetX: integer read FOffsetX write SetOffsetX;
  property OffsetY: integer read FOffsetY write SetOffsetY;
end;

{ TSGMarkerSettings }

//  Marker can be left or right or centered. The flat side should
//  be aligned with the markers value or in the case of centered, will
//  be the center of the marker. Like -  \| - Left, |/ - Right, \/ - Centered

TSGMarkerSettings = class(TPersistent)
private
  FValue: single;     // this may be the scaled (Min/Max) value in the SuperGauge
  FEnabled: boolean;
  FColor: TColor;
  FHeight: integer;
  FRadius: integer;
  FWidth: integer;
  FStyle: TSGMarkerStyle;
  FOnChange: TNotifyEvent;
  FDirty: boolean;

  procedure SetValue(AValue: single);
  procedure SetEnabled(AValue: boolean);
  procedure SetColor(AValue: TColor);
  procedure SetHeight(AValue: integer);
  procedure SetRadius(AValue: integer);
  procedure SetWidth(AValue: integer);
  procedure SetOnChange(AValue: TNotifyEvent);
  procedure SetStyle(AValue: TSGMarkerStyle);
  procedure DirtyOnChange;

protected
public
  constructor Create;
  destructor Destroy; override;
  property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  property Dirty: boolean read FDirty write FDirty;

published
  property Value: single read FValue write SetValue default 0.0;
  property Enabled: boolean read FEnabled write SetEnabled default False;
  property Color: TColor read FColor write SetColor default clGreen;
  property Height: integer read FHeight write SetHeight default 20;
  property Radius: integer read FRadius write SetRadius default 130;
  property Width: integer read FWidth write SetWidth default 15;
  property Style: TSGMarkerStyle read FStyle write SetStyle default msCenter;
end;

function Initializebitmap(var Bitmap: TBGRABitmap; Width, Height: integer): TSGOrigin;

implementation

function Initializebitmap(var Bitmap: TBGRABitmap; Width, Height: integer): TSGOrigin;
begin
  Bitmap.SetSize(Width, Height);

  // Clear bitmap to transparent

  BitMap.Fill(BGRA(0, 0, 0, 0));

  // Get origin information

  Result.CenterPoint.x := Width div 2;
  Result.CenterPoint.y := Height div 2;

  // Take the smallest so radius will always fit

  if Result.CenterPoint.x < Result.CenterPoint.y then
    Result.Radius := Result.CenterPoint.x
  else
    Result.Radius := Result.CenterPoint.y;
end;

{ TSGPointerCapSettings }

procedure TSGPointerCapSettings.SetCapStyle(const AValue: TSGCapStyle);
begin
  if FCapStyle = AValue then
    Exit;

  FCapStyle := AValue;
  DirtyOnChange;
end;

procedure TSGPointerCapSettings.SetPointerCapPos(const AValue: TSGPointerCapPosition);
begin
  if FCapPosition = AValue then
    Exit;

  FCapPosition := AValue;
  DirtyOnChange;
end;

procedure TSGPointerCapSettings.SetLightIntensity(const AValue: integer);
begin
  if AValue = FPhong.LightSourceIntensity then
    Exit;

  FPhong.LightSourceIntensity := AValue;
  DirtyOnChange;
end;

function TSGPointerCapSettings.GetLightIntensity: integer;
begin
  Result := round(FPhong.LightSourceIntensity);
end;

procedure TSGPointerCapSettings.SetCurveExponent(const AValue: single);
begin
  if FCurveExponent = AValue then
    Exit;

  FCurveExponent := AValue;
  DirtyOnChange;
end;

procedure TSGPointerCapSettings.SetEdgeColor(AValue: TColor);
begin
  if FEdgeColor = AValue then
    Exit;

  FEdgeColor := AValue;
  DirtyOnChange;
end;

procedure TSGPointerCapSettings.SetEdgeWidth(AValue: integer);
begin
  if (FEdgeWidth = AValue) or (AValue < 0) then
    Exit;

  FEdgeWidth := AValue;
  DirtyOnChange;
end;

procedure TSGPointerCapSettings.SetFillColor(AValue: TColor);
begin
  if FFillColor = AValue then
    Exit;

  FFillColor := AValue;
  DirtyOnChange;
end;

procedure TSGPointerCapSettings.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;

  // no dirty needed possibly, call directly

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSGPointerCapSettings.SetRadius(AValue: integer);
begin
  if FRadius = AValue then
    Exit;

  FRadius := AValue;
  DirtyOnChange;
end;

constructor TSGPointerCapSettings.Create;
begin
  // create a phong shader, will need to delete on clean up

  FPhong := TPhongShading.Create;
  FPhong.LightPositionZ := 100;
  FPhong.LightSourceIntensity := 300;
  FPhong.NegativeDiffusionFactor := 0.8;
  FPhong.AmbientFactor := 0.5;
  FPhong.DiffusionFactor := 0.6;

  FCurveExponent := 0.05;
  FCapStyle := csPhong;
  FCapPosition := cpUnder;
  FEdgeColor := clGray;
  FFillColor := clBlack;
  FRadius := 20;
  FEdgeWidth := 1;
  FDirty := True;
end;

destructor TSGPointerCapSettings.Destroy;
begin
  FPhong.Free;
  inherited Destroy;
end;

procedure TSGPointerCapSettings.DirtyOnChange;
begin
  FDirty := True;   // if we get here some props must have changed, mark dirty

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TSGPointerSettings }
constructor TSGPointerSettings.Create;
begin
  FColor := BGRA(255, 127, 63); // Orange pointer
  FLength := 130;
  FWidth := 5;
  FExtensionLength := 10;
  FStyle := psLineExt;
  FHighlightLine := False;
  FHighlightColor := clGray;
  FHighlightThickness := 1;
  FDirty := True;
  FEnabled := True;
end;

destructor TSGPointerSettings.Destroy;
begin
  inherited Destroy;
end;

procedure TSGPointerSettings.SetColor(AValue: TColor);
begin
  if FColor = AValue then
    Exit;

  FColor := AValue;
  DirtyOnChange;
end;

procedure TSGPointerSettings.SetLength(AValue: integer);
begin
  if (FLength = AValue) or (AValue < 0) then
    Exit;

  FLength := AValue;
  DirtyOnChange;
end;

procedure TSGPointerSettings.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSGPointerSettings.SetWidth(AValue: integer);
begin
  if (FWidth = AValue) or (AValue < 0)then
    Exit;

  FWidth := AValue;
  DirtyOnChange;
end;

procedure TSGPointerSettings.SetStyle(AValue: TSGPointerStyle);
begin
  if FStyle = AValue then
    Exit;

  FStyle := AValue;
  DirtyOnChange;
end;

procedure TSGPointerSettings.SetExtensionLength(AValue: integer);
begin
  if (FExtensionLength = AValue) or (AValue < 0) then
    Exit;

  FExtensionLength := AValue;
  DirtyOnChange;
end;

procedure TSGPointerSettings.SetHighlightLine(AValue: boolean);
begin
  if FHighlightLine = AValue then
    Exit;

  FHighlightLine := AValue;
  DirtyOnChange;
end;


procedure TSGPointerSettings.SetHighlightColor(AValue: TColor);
begin
  if (FHighlightColor = AValue) then
    Exit;

  FHighlightColor := AValue;
  DirtyOnChange;
end;

procedure TSGPointerSettings.SetHighlightThickness(AValue: integer);
begin
  if (FHighlightThickness = AValue) or (AValue < 0) then
    Exit;

  FHighlightThickness := AValue;
  DirtyOnChange;
end;

procedure TSGPointerSettings.SetEnabled(AValue: boolean);
begin
  if FEnabled = AValue then
    Exit;

  FEnabled := AValue;
  DirtyOnChange;
end;

procedure TSGPointerSettings.DirtyOnChange;
begin
  FDirty := True;   // if we get here a prop must have changed, mark dirty

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TSGScaleSettings }

constructor TSGScaleSettings.Create;
begin
  FTickColor := BGRA(223, 196, 125);  // Tan
  FTextColor := BGRA(140, 208, 211);  // Light Blue
  FTextFont := 'default';
  FTextSize := 20;
  FTextStyle := [fsBold];
  FTextRadius := 100;
  FEnableMainTicks := True;
  FEnableSubTicks := True;
  FEnableScaleText := True;
  FReverseScale := False;
  FMainTickCount := 10;
  FSubTickCount := 5;
  FStart := 0;
  FStep := 1;
  FMainTickLength := 15;
  FSubTickLength := 8;
  FMainTickThickness := 3;
  FSubTickThickness := 1;
  FTickArcStyle := taOuter;
  FMainTickUseDots:= False;
  FSubTickUseDots:= False;
  FInnerTickArcThickness := 3;
  FOuterTickArcThickness := 3;
  FTickArcColor := FTickColor;  // Same as the ticks
  FScaleRadius := 125;
  FEnabled := True;
  FDirty := True;
end;

destructor TSGScaleSettings.Destroy;
begin
  inherited Destroy;
end;

procedure TSGScaleSettings.SetEnabled(AValue: boolean);
begin
  if FEnabled = AValue then
    Exit;

  FEnabled := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetTextFont(AValue: string);
begin
  if FTextFont = AValue then
    Exit;
  FTextFont := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetEnableScaleText(AValue: boolean);
begin
  if FEnableScaleText = AValue then
    Exit;
  FEnableScaleText := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetReverseScale(AValue: boolean);
begin
  if FReverseScale = AValue then
    Exit;
  FReverseScale := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetScaleRadius(AValue: integer);
begin
  if (FScaleRadius = AValue) or (AValue < 1) then
    Exit;

  FScaleRadius := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetMainTickUseDots(AValue: boolean);
begin
  if FMainTickUseDots = AValue then
    Exit;

  FMainTickUseDots := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetSubTickUseDots(AValue: boolean);
begin
  if FSubTickUseDots = AValue then
    Exit;

  FSubTickUseDots := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetMaximum(AValue: integer);
begin
  if (FMaximum = AValue) or (AValue <= FMinimum) then
    Exit;

  FMaximum := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetStart(AValue: integer);
begin
  if (FStart = AValue)then
    Exit;

  FStart := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetStep(AValue: integer);
begin
  if (FStep = AValue)then
    Exit;

  FStep := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetMinimum(AValue: integer);
begin
  if (FMinimum = AValue) then
    Exit;

  FMinimum := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetTextRadius(AValue: integer);
begin
  if (FTextRadius = AValue) or (AValue < 1) then
    Exit;

  FTextRadius := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetTextSize(AValue: integer);
begin
  if (FTextSize = AValue) or (AValue < 1) then
    Exit;

  FTextSize := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetTickColor(AValue: TColor);
begin
  if FTickColor = AValue then
    Exit;

  FTickColor := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetTickArcStyle(AValue: TSGTickArc);
begin
  if FTickArcStyle = AValue then
     exit;

  FTickArcStyle := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetInnerTickArcThickness(AValue: integer);
begin
  if FInnerTickArcThickness = AValue then
     exit;

  FInnerTickArcThickness := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetOuterTickArcThickness(AValue: integer);
begin
  if FOuterTickArcThickness = AValue then
     exit;

  FOuterTickArcThickness := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetTickArcColor(AValue: TColor);
begin
  if FTickArcColor = AValue then
    Exit;

  FTickArcColor := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetEnableMainTicks(AValue: boolean);
begin
  if FEnableMainTicks = AValue then
    Exit;

  FEnableMainTicks := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetEnableSubTicks(AValue: boolean);
begin
  if FEnableSubTicks = AValue then
    Exit;

  FEnableSubTicks := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetMainTickLength(AValue: integer);
begin
  if (FMainTickLength = AValue) or (AValue < 0) then
    Exit;

  FMainTickLength := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetSubTickLength(AValue: integer);
begin
  if (FSubTickLength = AValue) or (AValue < 0) then
    Exit;

  FSubTickLength := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetMainTickCount(AValue: integer);
begin
  if (FMainTickCount = AValue) or (AValue < 1) then
    Exit;

  FMainTickCount := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSGScaleSettings.SetSubTickCount(AValue: integer);
begin
  if (FSubTickCount = AValue) or (AValue < 1) then
    Exit;

  FSubTickCount := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetTextColor(AValue: TColor);
begin
  if FTextColor = AValue then
    Exit;

  FTextColor := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetTextStyle(AValue: TFontStyles);
begin
  if FTextStyle = AValue then
    Exit;

  FTextStyle := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetMainTickThickness(AValue: integer);
begin
  if (FMainTickThickness = AValue) or (AValue < 0) then
    Exit;

  FMainTickThickness := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.SetSubTickThickness(AValue: integer);
begin
  if (FSubTickThickness = AValue) or (AValue < 0)  then
    Exit;

  FSubTickThickness := AValue;
  DirtyOnChange;
end;

procedure TSGScaleSettings.DirtyOnChange;
begin
  FDirty := True;   // if we get here a prop must have changed, mark dirty

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TSGBandSettings}

constructor TSGBandSettings.Create;
begin
  FEnabled := False;
  FEnableText := False;
  FText := '';
  FTextColor := clWhite;
  FTextFont := 'default';
  FTextStyle := [];
  FTextSize := 14;
  FTextRadius := 90;
  FStartValue := 0;
  FEndValue := 20;
  FBandRadius := 85;
  FBandColor := clGreen;
  FBandThickness := 25;
  FStartValue := 0;
  FEndValue := 100;

  FDirty := True;
end;

destructor TSGBandSettings.Destroy;
begin
  inherited Destroy;
end;

procedure TSGBandSettings.SetEnabled(AValue: boolean);
begin
  if FEnabled = AValue then
    Exit;

  FEnabled := AValue;
  DirtyOnChange;
end;

procedure TSGBandSettings.SetTextFont(AValue: string);
begin
  if FTextFont = AValue then
    Exit;

  FTextFont := AValue;
  DirtyOnChange;
end;

procedure TSGBandSettings.SetTextStyle(AValue: TFontStyles);
begin
  if FTextStyle = AValue then
    Exit;

  FTextStyle := AValue;
  DirtyOnChange;
end;

procedure TSGBandSettings.SetEnableText(AValue: boolean);
begin
  if FEnableText = AValue then
    Exit;

  FEnableText := AValue;
  DirtyOnChange;
end;

procedure TSGBandSettings.SetText(AValue: TCaption);
begin
  if FText = AValue then
    Exit;

  FText := AValue;
  DirtyOnChange;
end;

procedure TSGBandSettings.SetTextRadius(AValue: integer);
begin
  if (FTextRadius = AValue) or (AValue < 1) then
    Exit;

  FTextRadius := AValue;
  DirtyOnChange;
end;

procedure TSGBandSettings.SetBandRadius(AValue: integer);
begin
  if (FBandRadius = AValue) or (AValue < 1) then
    Exit;

  FBandRadius := AValue;
  DirtyOnChange;
end;

procedure TSGBandSettings.SetStartValue(AValue: single);
begin
  if (FStartValue = AValue) or (AValue >= FEndValue) then
    Exit;

  FStartValue := AValue;
  DirtyOnChange;
end;

procedure TSGBandSettings.SetEndValue(AValue: single);
begin
  if (FEndValue = AValue) or (AValue <= FStartValue) then
    Exit;

  FEndValue := AValue;
  DirtyOnChange;
end;

procedure TSGBandSettings.SetTextSize(AValue: integer);
begin
  if (FTextSize = AValue) or (AValue < 1) then
    Exit;

  FTextSize := AValue;
  DirtyOnChange;
end;

procedure TSGBandSettings.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSGBandSettings.SetTextColor(AValue: TColor);
begin
  if FTextColor = AValue then
    Exit;

  FTextColor := AValue;
  DirtyOnChange;
end;

procedure TSGBandSettings.SetBandColor(AValue: TColor);
begin
  if FBandColor = AValue then
    Exit;

  FBandColor := AValue;
  DirtyOnChange;
end;

procedure TSGBandSettings.SetBandThickness(AValue: integer);
begin
  if (FBandThickness = AValue) or (AValue < 1) then
    Exit;

  FBandThickness := AValue;
  DirtyOnChange;
end;

procedure TSGBandSettings.DirtyOnChange;
begin
  FDirty := True;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TSGFaceSettings }

constructor TSGFaceSettings.Create;
begin
  // create a Phong shader, will need to delete on clean up

  FPhong := TPhongShading.Create;

  FPhong.LightPositionZ := 100;
  FPhong.LightSourceIntensity := 300;
  FPhong.NegativeDiffusionFactor := 0.8;
  FPhong.AmbientFactor := 0.5;
  FPhong.DiffusionFactor := 0.6;

  FCurveExponent := 0.05;
  FOuterColor := clBlack;
  FInnerColor := clGray;
  FFillStyle := fsGradient;
  FPicture := TPicture.Create;
  FPictureEnabled := False;
  FPictureOffsetX := -30;
  FPictureOffsetY := 60;

  FDirty := True;
end;

destructor TSGFaceSettings.Destroy;
begin
  FPhong.Free;
  FPicture.Free;

  inherited Destroy;
end;

procedure TSGFaceSettings.SetInnerColor(AValue: TColor);
begin
  if FInnerColor = AValue then
    Exit;

  FInnerColor := AValue;
  DirtyOnChange;
end;

procedure TSGFaceSettings.SetOuterColor(AValue: TColor);
begin
  if FOuterColor = AValue then
    Exit;

  FOuterColor := AValue;
  DirtyOnChange;
end;

procedure TSGFaceSettings.SetFillStyle(AValue: TSGFillStyle);
begin
  if FFillStyle = AValue then
    Exit;

  FFillStyle := AValue;
  DirtyOnChange;
end;

procedure TSGFaceSettings.SetPicture(AValue: TPicture);
begin
  if FPicture = AValue then
    Exit;

  FPicture := AValue;
  DirtyOnChange;
end;

procedure TSGFaceSettings.SetPictureEnabled(AValue: boolean);
begin

  if FPictureEnabled = AValue then
    Exit;

  FPictureEnabled := AValue;
  DirtyOnChange;
end;

procedure TSGFaceSettings.SetPictureOffsetX(AValue: integer);
begin
  if FPictureOffsetX = AValue then
    Exit;

  FPictureOffsetX := AValue;
  DirtyOnChange;
end;

procedure TSGFaceSettings.SetPictureOffsetY(AValue: integer);
begin
  if FPictureOffsetY = AValue then
    Exit;

  FPictureOffsetY := AValue;
  DirtyOnChange;
end;

procedure TSGFaceSettings.SetLightIntensity(const AValue: integer);
begin
  if AValue = FPhong.LightSourceIntensity then
    Exit;

  FPhong.LightSourceIntensity := AValue;
  DirtyOnChange;
end;

function TSGFaceSettings.GetLightIntensity: integer;
begin
  Result := round(FPhong.LightSourceIntensity);
end;

procedure TSGFaceSettings.SetCurveExponent(const AValue: single);
begin
  if FCurveExponent = AValue then
    Exit;

  FCurveExponent := AValue;
  DirtyOnChange;
end;

procedure TSGFaceSettings.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSGFaceSettings.DirtyOnChange;
begin
  FDirty := True;   // if we get here a prop must have changed, mark dirty

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TSGFrameSettings }

constructor TSGFrameSettings.Create;
begin
  FOuterFrameColor := clGray;
  FMiddleFrameColor := clSilver;
  FInnerFrameColor := clMedGray;
  FOuterFrameThickness := 5;
  FMiddleFrameThickness := 5;
  FInnerFrameThickness := 5;

  FDirty := True;
end;

destructor TSGFrameSettings.Destroy;
begin
  inherited Destroy;
end;

procedure TSGFrameSettings.SetOuterFrameColor(AValue: TColor);
begin
  if FOuterFrameColor = AValue then
    Exit;

  FOuterFrameColor := AValue;
  DirtyOnChange;
end;

procedure TSGFrameSettings.SetMiddleFrameColor(AValue: TColor);
begin
  if FMiddleFrameColor = AValue then
    Exit;

  FMiddleFrameColor := AValue;
  DirtyOnChange;
end;

procedure TSGFrameSettings.SetInnerFrameColor(AValue: TColor);
begin
  if FInnerFrameColor = AValue then
    Exit;

  FInnerFrameColor := AValue;
  DirtyOnChange;
end;

procedure TSGFrameSettings.SetOuterFrameThickness(AValue: integer);
begin
  if (FOuterFrameThickness = AValue) or (AValue < 0) then
    Exit;

  FOuterFrameThickness := AValue;
  DirtyOnChange;
end;

procedure TSGFrameSettings.SetMiddleFrameThickness(AValue: integer);
begin
  if (FMiddleFrameThickness = AValue) or (AValue < 0) then
    Exit;

  FMiddleFrameThickness := AValue;
  DirtyOnChange;
end;

procedure TSGFrameSettings.SetInnerFrameThickness(AValue: integer);
begin
  if (FInnerFrameThickness = AValue) or (AValue < 0) then
    Exit;

  FInnerFrameThickness := AValue;
  DirtyOnChange;
end;

procedure TSGFrameSettings.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSGFrameSettings.DirtyOnChange;
begin
  FDirty := True;   // if we get here a prop must have changed, mark dirty

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TSGLEDSettings }

constructor TSGLEDSettings.Create;
begin
  FActiveColor := clRed;
  FInActiveColor := clBlack;
  FBorderColor := clGray;
  FSize := 15;
  FOffsetX := 80;
  FOffsetY := 120;
  FStyle := lsNone;
  FDirty := True;
end;

destructor TSGLEDSettings.Destroy;
begin
  inherited Destroy;
end;

procedure TSGLEDSettings.SetActive(AValue: boolean);
begin
  if FActive = AValue then
    Exit;

  FActive := AValue;
  DirtyOnChange;
end;

// HACK, need to have a way to NOT call DirtyOnChange, This is it
// Need to expose this so the Gauge can use it and not cause a refresh

procedure TSGLEDSettings.SetActiveNoDoChange(AValue: boolean);
begin
  if FActive = AValue then
    Exit;

  FActive := AValue;
end;

procedure TSGLEDSettings.SetActiveColor(AValue: TColor);
begin
  if FActiveColor = AValue then
    Exit;

  FActiveColor := AValue;
  DirtyOnChange;
end;

procedure TSGLEDSettings.SetInactiveColor(AValue: TColor);
begin
  if FInactiveColor = AValue then
    Exit;

  FInActiveColor := AValue;
  DirtyOnChange;
end;

procedure TSGLEDSettings.SetBorderColor(AValue: TColor);
begin
  if FBorderColor = AValue then
    Exit;

  FBorderColor := AValue;
  DirtyOnChange;
end;

procedure TSGLEDSettings.SetSize(AValue: integer);
begin
  if FSize = AValue then
    Exit;

  FSize := AValue;
  DirtyOnChange;
end;

procedure TSGLEDSettings.SetOffsetX(AValue: integer);
begin
  if FOffsetX = AValue then
    Exit;

  FOffsetX := AValue;
  DirtyOnChange;
end;

procedure TSGLEDSettings.SetOffsetY(AValue: integer);
begin
  if FOffsetY = AValue then
    Exit;

  FOffsetY := AValue;
  DirtyOnChange;
end;

procedure TSGLEDSettings.SetStyle(AValue: TSGLEDStyle);
begin
  if FStyle = AValue then
    Exit;

  FStyle := AValue;
  DirtyOnChange;
end;

procedure TSGLEDSettings.SetShape(AValue: TSGLEDShape);
begin
  if FShape = AValue then
    Exit;

  FShape := AValue;
  DirtyOnChange;
end;

procedure TSGLEDSettings.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;

  // this will not dirty it, may need to not sure

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSGLEDSettings.DirtyOnChange;
begin
  FDirty := True;   // if we get here a prop must have changed, mark dirty

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TSGRangeCheckLEDSettings }

constructor TSGRangeCheckLEDSettings.Create;
begin
  inherited Create;

  FRangeStartValue := 0;
  FRangeEndValue := 100;
  FRangeType := rcNone;
  FSize := 10;
  FOffsetX := 50;
  FOffsetY := 90;
end;

destructor TSGRangeCheckLEDSettings.Destroy;
begin
  inherited Destroy;
end;

procedure TSGRangeCheckLEDSettings.SetRangeStartValue(AValue: single);
begin
  if (FRangeStartValue = AValue) or (AValue > FRangeEndValue) then
    Exit;

  FRangeStartValue := AValue;
  DirtyOnChange;
end;

procedure TSGRangeCheckLEDSettings.SetRangeEndValue(AValue: single);
begin
  if (FRangeEndValue = AValue) or (AValue < FRangeStartValue) then
    Exit;

  FRangeEndValue := AValue;
  DirtyOnChange;
end;

procedure TSGRangeCheckLEDSettings.SetRangeType(AValue: TSGRangeCheckType);
begin
  if FRangeType = AValue then
    Exit;

  FRangeType := AValue;
  DirtyOnChange;
end;

{ TSGTextSettings }

constructor TSGTextSettings.Create;
begin
  FText := 'Gauge';
  OffsetX := 0;
  OffsetY := 50; // default should be clear of default cap radius when it's drawn
  FDirty := True;

  // create font, must free in dtor

  FFontEx := TBCFont.Create(nil);
  FFontEx.Color := clWhite;
  FFontEx.Style := [fsBold];
  FFontEx.Height := 24;
end;

destructor TSGTextSettings.Destroy;
begin
  FFontEx.Free;

  inherited Destroy;
end;

procedure TSGTextSettings.SetEnabled(AValue: boolean);
begin
  if FEnabled = AValue then
    Exit;

  FEnabled := AValue;
  DirtyOnChange;
end;

procedure TSGTextSettings.SetText(AValue: TCaption);
begin
  if FText = AValue then
    Exit;

  FText := AValue;
  DirtyOnChange;
end;

procedure TSGTextSettings.SetOffsetX(AValue: integer);
begin
  if FOffsetX = AValue then
    Exit;

  FOffsetX := AValue;
  DirtyOnChange;
end;

procedure TSGTextSettings.SetOffsetY(AValue: integer);
begin
  if FOffsetY = AValue then
    Exit;

  FOffsetY := AValue;
  DirtyOnChange;
end;

procedure TSGTextSettings.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;

  // this will not dirty it, may need to not sure

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSGTextSettings.DirtyOnChange;
begin
  FDirty := True;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSGTextSettings.SetFontEx(AValue: TBCFont);
begin
  FFontEx.Assign(AValue);
  FDirty := True;
  DirtyOnChange;
end;

{TSGMarkerSettings}

constructor TSGMarkerSettings.Create;
begin
  FEnabled := False;
  FColor := clLime;
  FHeight := 20;
  FWidth := 15;
  FRadius := 130;
  FStyle := msCenter;
  FDirty := True;
  FValue := 0.0;
end;

destructor TSGMarkerSettings.Destroy;
begin
  inherited Destroy;
end;

procedure TSGMarkerSettings.SetValue(AValue: single);
begin
  if FValue = AValue then
    Exit;

  FValue := AValue;
  DirtyOnChange;
end;

procedure TSGMarkerSettings.SetEnabled(AValue: boolean);
begin
  if FEnabled = AValue then
    Exit;

  FEnabled := AValue;
  DirtyOnChange;
end;

procedure TSGMarkerSettings.SetColor(AValue: TColor);
begin
  if FColor = AValue then
    Exit;

  FColor := AValue;
  DirtyOnChange;
end;

procedure TSGMarkerSettings.SetHeight(AValue: integer);
begin
  if FHeight = AValue then
    Exit;

  FHeight := AValue;
  DirtyOnChange;
end;

procedure TSGMarkerSettings.SetRadius(AValue: integer);
begin
  if FRadius = AValue then
    Exit;

  FRadius := AValue;
  DirtyOnChange;
end;

procedure TSGMarkerSettings.SetWidth(AValue: integer);
begin
  if FWidth = AValue then
    Exit;

  FWidth := AValue;
  DirtyOnChange;
end;

procedure TSGMarkerSettings.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;

  // no dirty needed possibly, call directly

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSGMarkerSettings.SetStyle(AValue: TSGMarkerStyle);
begin
  if FStyle = AValue then
    Exit;

  FStyle := AValue;
  DirtyOnChange;
end;

procedure TSGMarkerSettings.DirtyOnChange;
begin
  FDirty := True;   // if we get here some props must have changed, mark dirty

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
