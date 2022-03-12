// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Common types for BGRA Controls package

  originally written in 2011 by Krzysztof Dibowski dibowski at interia.pl
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCTypes;

{$I bgracontrols.inc}

interface

uses
  Classes, Controls, {$IFNDEF FPC}Types, Windows, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BGRABitmap, BGRABitmapTypes, Graphics, BCBasectrls;

type

  {$IFDEF FPC}
    {$IFDEF CPU64}
    BGRAPtrInt      = PtrInt;           //    Cardinal;//PtrInt;
    BGRAPtrUInt     = PtrUInt;          //    Cardinal;//PtrUInt;
    {$ELSE}
    BGRAPtrInt      = PtrInt;           //    LongInt;//PtrInt;
    BGRAPtrUInt     = PtrUInt;          //    Cardinal;//PtrUInt;
    BGRAQWord       = Int64;            //    Cardinal;//QWord;
    {$ENDIF}
    BGRAWord        = Word;                        //    Word;
    PBGRAWord       = PWord;                       //    PWord;
    BGRADWord       = DWord;          //    Cardinal;  //DWord;
    BGRALongWord    = LongWord;       //    Cardinal;  //LongWord;
    PBGRAQWord      = PQWord;         //    PCardinal; //PQWord;
    PBGRADWord      = PDWord;         //    PCardinal; //PDWord;
    PBGRALongWord   = PLongWord;      //    PCardinal; //PLongWord;
    BGRANativeInt   = NativeInt;      //    NativeInt;  //NativeInt;
    BGRANativeUInt  = NativeUInt;     //    Cardinal;  //NativeUInt;
    BGRALongInt     = LongInt;        //    Cardinal;  //LongInt;
    BGRAInt64       = Int64;                       //    Int64;
    BGRAUInt64      = Int64;                      //    UInt64;
    BGRACardinal    = Cardinal;                    //    Cardinal;
    PBGRACardinal   = PCardinal;                   //    PCardinal;

    HDC             = {$IFDEF BGRABITMAP_USE_LCL}LCLType.HDC{$ELSE}BGRAPtrUInt{$ENDIF};
    PPTrint         = ^PtrInt;
  {$ELSE}
    ValReal         = Extended;
    {$IFDEF CPU64}
    BGRAPtrInt      = Int64;
    BGRAPtrUInt     = QWord;
    {$ELSE}
    BGRAPtrInt      = LongInt;        //      LongInt;//LongInt;
    BGRAPtrUInt     = LongWord;       //      Cardinal;//LongWord;
    BGRAQWord       = Int64;          //      Cardinal;//LongWord;
    {$ENDIF}
    BGRAWord        = Word;           //                 Word;
    PBGRAWord       = PWord;          //                 PWord;
    BGRADWord       = DWord;          //      Cardinal;//DWord;
    BGRALongWord    = LongWord;       //      Cardinal;//LongWord;
    PBGRAPtrInt     = ^BGRAPtrInt;    //     PCardinal;//^BGRAPtrInt;
    PBGRAPtrUInt    = ^BGRAPtrUInt;   //     PCardinal;//^BGRAPtrUInt;
    PBGRAQWord      = ^BGRAQWord;     //     PCardinal;//^BGRAQWord;
    PBGRADWord      = PDWord;         //     PCardinal;//PDWord;
    PBGRALongWord   = PLongWord;      //     PCardinal;//PLongWord;
    BGRANativeInt   = NativeInt;      //     NativeInt;//NativeInt;
    BGRANativeUInt  = NativeUInt;     //      Cardinal;//NativeUInt;
    BGRALongInt     = LongInt;        //                 LongInt;
    BGRAInt64       = Int64;          //                 Int64;
    BGRAUInt64      = Int64;          //                 UInt64;
    BGRACardinal    = Cardinal;       //                 Cardinal;
    PBGRACardinal   = PCardinal;      //                 PCardinal;

    HDC             = Windows.HDC;    //
    PUnicodeChar    = Windows.PWChar; //
    UnicodeChar     = Windows.WCHAR;  //
(*    ValReal         = FPImage.ValReal;
    {$IFDEF CPU64}     //WORD = 2 bytes = 4 nybbles = 16 bits    for 32bits
    BGRAPtrInt      = FPImage.BGRAPtrInt;
    BGRAPtrUInt     = FPImage.BGRAPtrUInt;  //QWORD = 2 DWORDs = 4 WORDs = ….. = 64 bits       for 32bits
    {$ELSE}               //BGRADWord = 2 WORDs = 4 bytes = 8 nybbles = 32 bits   for 32bits
    BGRAPtrInt      = FPImage.BGRAPtrInt;
    BGRAPtrUInt     = FPImage.BGRAPtrUInt;
    BGRAQWord       = FPImage.BGRAQWord;
    {$ENDIF}
    BGRADWord       = FPImage.BGRADWord;
    BGRALongWord    = FPImage.BGRALongWord;
    PBGRAPtrInt     = FPImage.PBGRAPtrInt;
    PBGRAPtrUInt    = FPImage.PBGRAPtrUInt;
    PBGRAQWord      = FPImage.PBGRAQWord;
    PBGRADWord      = FPImage.PBGRADWord;
    HDC             = FPImage.HDC;
    BGRANativeInt   = FPImage.BGRANativeInt;
    PBGRALongWord   = FPImage.PBGRALongWord;

    PUnicodeChar    = FPImage.PUnicodeChar;
    UnicodeChar     = FPImage.UnicodeChar;    *)
  {$ENDIF}


  TBCMouseState = (msNone, msHover, msClicked);
  TBCAlignment = (bcaLeftTop, bcaLeftCenter, bcaLeftBottom,
    bcaCenterTop, bcaCenter, bcaCenterBottom, bcaRightTop, bcaRightCenter,
    bcaRightBottom);
  TBCBackgroundStyle = (bbsClear, bbsColor, bbsGradient);
  TBCBorderStyle = (bboNone, bboSolid);
  TBCArrowDirection = (badLeft, badRight, badUp, badDown);
  TBCStretchMode = (smNone, smShrink, smStretch, smCover);
  TBCCanvasScaleMode = (csmAuto, csmScaleBitmap, csmFullResolution);
  TBGRATextAlign = (btaLeft, btaCenter, btaRight); // deprecated
  TBGRATextVAlign = (btvaTop, btvaCenter, btvaBottom); // deprecated
  TBGRARedrawEvent = procedure(Sender: TObject; Bitmap: TBGRABitmap) of object;

type
  { TBCGradient }

  TBCGradient = class(TBCProperty)
  private
    FColorCorrection: boolean;
    FDrawMode: TDrawMode;
    FGradientType: TGradientType;
    FEndColor: TColor;
    FEndColorOpacity: byte;
    FPoint1XPercent: single;
    FPoint1YPercent: single;
    FPoint2XPercent: single;
    FPoint2YPercent: single;
    FSinus: boolean;
    FStartColor: TColor;
    FStartColorOpacity: byte;
    procedure SetColorCorrection(const AValue: boolean);
    procedure SetDrawMode(const AValue: TDrawMode);
    procedure SetEndColor(const AValue: TColor);
    procedure SetEndColorOpacity(const AValue: byte);
    procedure SetGradientType(const AValue: TGradientType);
    procedure SetPoint1XPercent(const AValue: single);
    procedure SetPoint1YPercent(const AValue: single);
    procedure SetPoint2XPercent(const AValue: single);
    procedure SetPoint2YPercent(const AValue: single);
    procedure SetSinus(const AValue: boolean);
    procedure SetStartColor(const AValue: TColor);
    procedure SetStartColorOpacity(const AValue: byte);
  public
    constructor Create(AControl: TControl); override;

    procedure Assign(Source: TPersistent); override;
    procedure Scale(AScale: single);
  published
    property StartColor: TColor read FStartColor write SetStartColor;
    property StartColorOpacity: byte read FStartColorOpacity write SetStartColorOpacity default 255;
    property DrawMode: TDrawMode read FDrawMode write SetDrawMode default dmSet;
    property EndColor: TColor read FEndColor write SetEndColor;
    property EndColorOpacity: byte read FEndColorOpacity write SetEndColorOpacity default 255;
    property ColorCorrection: boolean read FColorCorrection write SetColorCorrection default true;
    property GradientType: TGradientType read FGradientType write SetGradientType;
    property Point1XPercent: single read FPoint1XPercent write SetPoint1XPercent default EmptySingle;
    property Point1YPercent: single read FPoint1YPercent write SetPoint1YPercent default EmptySingle;
    property Point2XPercent: single read FPoint2XPercent write SetPoint2XPercent default EmptySingle;
    property Point2YPercent: single read FPoint2YPercent write SetPoint2YPercent default EmptySingle;
    property Sinus: boolean read FSinus write SetSinus default false;
  end;

  { TBCFont }

  TBCFont = class(TBCProperty)
  private
    FColor, FDisabledColor: TColor;
    FEndEllipsis: boolean;
    FFontQuality: TBGRAFontQuality;
    FHeight: integer;
    FName: string;
    FPaddingBottom: integer;
    FPaddingLeft: integer;
    FPaddingRight: integer;
    FPaddingTop: integer;
    FShadow: boolean;
    FShadowColor: TColor;
    FShadowColorOpacity: byte;
    FShadowOffsetX: shortint;
    FShadowOffsetY: shortint;
    FShadowRadius: byte;
    FSingleLine: boolean;
    FStyle: TFontStyles;
    FTextAlignment: TBCAlignment;
    FWordBreak: boolean;
    function IsNameStored: boolean;
    procedure SetColor(AValue: TColor);
    procedure SetDisabledColor(AValue: TColor);
    procedure SetEndEllipsis(AValue: boolean);
    procedure SetFontQuality(AValue: TBGRAFontQuality);
    procedure SetHeight(AValue: integer);
    procedure SetName(AValue: string);
    procedure SetPaddingBottom(AValue: integer);
    procedure SetPaddingLeft(AValue: integer);
    procedure SetPaddingRight(AValue: integer);
    procedure SetPaddingTop(AValue: integer);
    procedure SetShadow(AValue: boolean);
    procedure SetShadowColor(AValue: TColor);
    procedure SetShadowColorOpacity(AValue: byte);
    procedure SetShadowOffsetX(AValue: shortint);
    procedure SetShadowOffsetY(AValue: shortint);
    procedure SetShadowRadius(AValue: byte);
    procedure SetSingleLine(AValue: boolean);
    procedure SetStyle(AValue: TFontStyles);
    procedure SetTextAlignment(AValue: TBCAlignment);
    procedure SetWordBreak(AValue: boolean);
  public
    constructor Create(AControl: TControl); override;
    procedure Assign(Source: TPersistent); override;
    procedure Scale(AScale: single; APreserveDefaultHeight: boolean = true);
  published
    property Color: TColor read FColor write SetColor;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clNone;
    property EndEllipsis: boolean read FEndEllipsis write SetEndEllipsis default false;
    property FontQuality: TBGRAFontQuality read FFontQuality write SetFontQuality;
    property Height: integer read FHeight write SetHeight default 0;
    property Name: string read FName write SetName stored IsNameStored;
    property SingleLine: boolean read FSingleLine write SetSingleLine default true;
    property Shadow: boolean read FShadow write SetShadow;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBlack;
    property ShadowColorOpacity: byte read FShadowColorOpacity
      write SetShadowColorOpacity default 255;
    property ShadowRadius: byte read FShadowRadius write SetShadowRadius;
    property ShadowOffsetX: shortint read FShadowOffsetX write SetShadowOffsetX;
    property ShadowOffsetY: shortint read FShadowOffsetY write SetShadowOffsetY;
    property Style: TFontStyles read FStyle write SetStyle;
    property TextAlignment: TBCAlignment read FTextAlignment write SetTextAlignment default bcaCenter;
    property WordBreak: boolean read FWordBreak write SetWordBreak default false;
    property PaddingLeft: integer read FPaddingLeft write SetPaddingLeft default 0;
    property PaddingRight: integer read FPaddingRight write SetPaddingRight default 0;
    property PaddingTop: integer read FPaddingTop write SetPaddingTop default 0;
    property PaddingBottom: integer read FPaddingBottom write SetPaddingBottom default 0;
  end;

  { TBCBackground }

  TBCBackground = class(TBCProperty)
  private
    FColor: TColor;
    FColorOpacity: byte;
    FGradient1: TBCGradient;
    FGradient1EndPercent: single;
    FGradient2: TBCGradient;
    FStyle: TBCBackgroundStyle;
    procedure OnChangeChildProperty({%H-}Sender: TObject; AData: PtrInt);
    procedure SetColor(AValue: TColor);
    procedure SetColorOpacity(AValue: byte);
    procedure SetGradient1(AValue: TBCGradient);
    procedure SetGradient1EndPercent(AValue: single);
    procedure SetGradient2(AValue: TBCGradient);
    procedure SetStyle(AValue: TBCBackgroundStyle);
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Scale(AScale: single);
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property ColorOpacity: byte read FColorOpacity write SetColorOpacity default 255;
    property Gradient1: TBCGradient read FGradient1 write SetGradient1;
    property Gradient2: TBCGradient read FGradient2 write SetGradient2;
    property Gradient1EndPercent: single read FGradient1EndPercent write SetGradient1EndPercent;
    property Style: TBCBackgroundStyle read FStyle write SetStyle;
  end;

  { TBCBorder }

  TBCBorder = class(TBCProperty)
  private
    FColor: TColor;
    FColorOpacity: byte;
    FLightColor: TColor;
    FLightOpacity: byte;
    FLightWidth: integer;
    FStyle: TBCBorderStyle;
    FWidth: integer;
    procedure SetColor(AValue: TColor);
    procedure SetColorOpacity(AValue: byte);
    procedure SetLightColor(AValue: TColor);
    procedure SetLightOpacity(AValue: byte);
    procedure SetLightWidth(AValue: integer);
    procedure SetStyle(AValue: TBCBorderStyle);
    procedure SetWidth(AValue: integer);
  public
    constructor Create(AControl: TControl); override;
    procedure Assign(Source: TPersistent); override;
    procedure Scale(AScale: single);
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property ColorOpacity: byte read FColorOpacity write SetColorOpacity default 255;
    property LightColor: TColor read FLightColor write SetLightColor default clWhite;
    property LightOpacity: byte read FLightOpacity write SetLightOpacity default 255;
    property LightWidth: integer read FLightWidth write SetLightWidth default 0;
    property Style: TBCBorderStyle read FStyle write SetStyle;
    property Width: integer read FWidth write SetWidth default 1;
  end;

  { TBCRounding }

  TBCRounding = class(TBCProperty)
  private
    FRoundOptions: TRoundRectangleOptions;
    FRoundX: byte;
    FRoundY: byte;
    procedure SetRoundOptions(AValue: TRoundRectangleOptions);
    procedure SetRoundX(AValue: byte);
    procedure SetRoundY(AValue: byte);
  public
    constructor Create(AControl: TControl); override;
    procedure Assign(Source: TPersistent); override;
    procedure Scale(AScale: single);
  published
    property RoundX: byte read FRoundX write SetRoundX;
    property RoundY: byte read FRoundY write SetRoundY;
    property RoundOptions: TRoundRectangleOptions
      read FRoundOptions write SetRoundOptions default [];
  end;

  { TBCPixel }

  TBCPixel = class(TBCProperty)
  private
    FPixel: TBGRAPixel;
  public
    { Constructor }
    constructor Create(AControl: TControl); overload; override;
    constructor Create(AControl: TControl; APixel: TBGRAPixel); overload;
    constructor Create(AControl: TControl; AColor: TColor); overload;
    { Assign values to Pixel }
    procedure Assign(Source: TPersistent); overload;  override;
    procedure Assign(Source: TBGRAPixel); overload;
    procedure Assign(Source: TColor; Opacity: byte = 255);overload;
    procedure Assign(Source: string); overload;
    { Read values }
    property Pixel: TBGRAPixel read FPixel write FPixel;
    function Color: TColor;
    function Hex: string;
    { Color functions }
    procedure ApplyLightness(lightness: word);
    procedure ApplyIntensity(lightness: longword);
    procedure ToGrayscale;
  published
    { Streaming }
    property Red: byte read FPixel.red write FPixel.red;
    property Green: byte read FPixel.green write FPixel.green;
    property Blue: byte read FPixel.blue write FPixel.blue;
    property Alpha: byte read FPixel.alpha write FPixel.alpha;
  end;

{const
  DEF_START_COL      = $00EFE6D2;
  DEF_END_COL        = $00C87511;
  DEF_BORD_COL       = $00AB713B;
  DEF_BORD_COL_HOVER = $00D7B697;
  DEF_FONT_COLOR     = $0072412A; }

implementation

uses math;

{ TBCPixel }

constructor TBCPixel.Create(AControl: TControl);
begin
  inherited Create(AControl);
end;

constructor TBCPixel.Create(AControl: TControl; APixel: TBGRAPixel);
begin
  inherited Create(AControl);
  Pixel := APixel;
end;

constructor TBCPixel.Create(AControl: TControl; AColor: TColor);
begin
  inherited Create(AControl);
  Assign(AColor);
end;

procedure TBCPixel.Assign(Source: TPersistent);
begin
  if Source is TBCPixel then
    Pixel := TBCPixel(Source).Pixel
  else
    inherited Assign(Source);
end;

procedure TBCPixel.Assign(Source: TBGRAPixel);
begin
  Pixel := Source;
end;

procedure TBCPixel.Assign(Source: TColor; Opacity: byte);
begin
  Pixel.FromColor(Source, Opacity);
end;

procedure TBCPixel.Assign(Source: string);
begin
  Pixel := StrToBGRA(Source);
end;

function TBCPixel.Color: TColor;
begin
  Result := Pixel;
end;

function TBCPixel.Hex: string;
begin
  Result := Pixel.ToString;
end;

procedure TBCPixel.ApplyLightness(lightness: word);
begin
  Pixel := ApplyLightnessFast(Pixel, lightness);
end;

procedure TBCPixel.ApplyIntensity(lightness: longword);
begin
  Pixel := ApplyIntensityFast(Pixel, lightness);
end;

procedure TBCPixel.ToGrayscale;
begin
  Pixel := BGRAToGrayscale(Pixel);
end;

{ TBCRounding }

procedure TBCRounding.SetRoundOptions(AValue: TRoundRectangleOptions);
begin
  if FRoundOptions = AValue then
    Exit;
  FRoundOptions := AValue;

  Change;
end;

procedure TBCRounding.SetRoundX(AValue: byte);
begin
  if FRoundX = AValue then
    Exit;
  FRoundX := AValue;

  Change;
end;

procedure TBCRounding.SetRoundY(AValue: byte);
begin
  if FRoundY = AValue then
    Exit;
  FRoundY := AValue;

  Change;
end;

constructor TBCRounding.Create(AControl: TControl);
begin
  inherited Create(AControl);

  FRoundX := 1;
  FRoundY := 1;
  FRoundOptions := [];
end;

procedure TBCRounding.Assign(Source: TPersistent);
begin
  if Source is TBCRounding then
  begin
    FRoundX := TBCRounding(Source).FRoundX;
    FRoundY := TBCRounding(Source).FRoundY;
    FRoundOptions := TBCRounding(Source).FRoundOptions;
  end
  else
    inherited Assign(Source);
end;

procedure TBCRounding.Scale(AScale: single);
begin
  RoundX := min(high(RoundX), round(RoundX * AScale));
  RoundY := min(high(RoundY), round(RoundY * AScale));
end;

{ TBCGradient }

procedure TBCGradient.SetColorCorrection(const AValue: boolean);
begin
  if FColorCorrection = AValue then
    exit;
  FColorCorrection := AValue;

  Change;
end;

procedure TBCGradient.SetDrawMode(const AValue: TDrawMode);
begin
  if FDrawMode = AValue then
    exit;
  FDrawMode := AValue;

  Change;
end;

procedure TBCGradient.SetEndColor(const AValue: TColor);
begin
  if FEndColor = AValue then
    exit;
  FEndColor := AValue;

  Change;
end;

procedure TBCGradient.SetEndColorOpacity(const AValue: byte);
begin
  if FEndColorOpacity = AValue then
    exit;
  FEndColorOpacity := AValue;

  Change;
end;

procedure TBCGradient.SetGradientType(const AValue: TGradientType);
begin
  if FGradientType = AValue then
    exit;
  FGradientType := AValue;

  Change;
end;

procedure TBCGradient.SetPoint1XPercent(const AValue: single);
begin
  if FPoint1XPercent = AValue then
    exit;
  FPoint1XPercent := AValue;

  Change;
end;

procedure TBCGradient.SetPoint1YPercent(const AValue: single);
begin
  if FPoint1YPercent = AValue then
    exit;
  FPoint1YPercent := AValue;

  Change;
end;

procedure TBCGradient.SetPoint2XPercent(const AValue: single);
begin
  if FPoint2XPercent = AValue then
    exit;
  FPoint2XPercent := AValue;

  Change;
end;

procedure TBCGradient.SetPoint2YPercent(const AValue: single);
begin
  if FPoint2YPercent = AValue then
    exit;
  FPoint2YPercent := AValue;

  Change;
end;

procedure TBCGradient.SetSinus(const AValue: boolean);
begin
  if FSinus = AValue then
    exit;
  FSinus := AValue;

  Change;
end;

procedure TBCGradient.SetStartColor(const AValue: TColor);
begin
  if FStartColor = AValue then
    exit;
  FStartColor := AValue;

  Change;
end;

procedure TBCGradient.SetStartColorOpacity(const AValue: byte);
begin
  if FStartColorOpacity = AValue then
    exit;
  FStartColorOpacity := AValue;

  Change;
end;

constructor TBCGradient.Create(AControl: TControl);
begin
  FStartColor := clWhite;
  FStartColorOpacity := 255;
  FDrawMode := dmSet;
  FEndColor := clBlack;
  FEndColorOpacity := 255;
  FGradientType := gtLinear;
  FColorCorrection := True;
  FSinus := False;

  FPoint1XPercent := 0;
  FPoint1YPercent := 0;
  FPoint2XPercent := 0;
  FPoint2YPercent := 100;

  inherited Create(AControl);
end;

procedure TBCGradient.Assign(Source: TPersistent);
begin
  if Source is TBCGradient then
  begin
    FStartColor := TBCGradient(Source).FStartColor;
    FStartColorOpacity := TBCGradient(Source).FStartColorOpacity;
    FDrawMode := TBCGradient(Source).FDrawMode;
    FEndColor := TBCGradient(Source).FEndColor;
    FEndColorOpacity := TBCGradient(Source).FEndColorOpacity;
    FColorCorrection := TBCGradient(Source).FColorCorrection;
    FGradientType := TBCGradient(Source).FGradientType;
    FPoint1XPercent := TBCGradient(Source).FPoint1XPercent;
    FPoint1YPercent := TBCGradient(Source).FPoint1YPercent;
    FPoint2XPercent := TBCGradient(Source).FPoint2XPercent;
    FPoint2YPercent := TBCGradient(Source).FPoint2YPercent;
    FSinus := TBCGradient(Source).FSinus;

    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TBCGradient.Scale(AScale: single);
begin
  //nothing
end;

{ TBCFont }

function TBCFont.IsNameStored: boolean;
begin
  Result := DefFontData.Name <> Name;
end;

procedure TBCFont.SetColor(AValue: TColor);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;

  Change;
end;

procedure TBCFont.SetDisabledColor(AValue: TColor);
begin
  if FDisabledColor = AValue then
    Exit;
  FDisabledColor := AValue;

  Change;
end;

procedure TBCFont.SetEndEllipsis(AValue: boolean);
begin
  if FEndEllipsis = AValue then
    Exit;
  FEndEllipsis := AValue;

  Change;
end;

procedure TBCFont.SetFontQuality(AValue: TBGRAFontQuality);
begin
  if FFontQuality = AValue then
    Exit;
  FFontQuality := AValue;

  Change;
end;

procedure TBCFont.SetHeight(AValue: integer);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;

  Change;
end;

procedure TBCFont.SetName(AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
  if FName = '' then
    FName := 'default';
  Change;
end;

procedure TBCFont.SetPaddingBottom(AValue: integer);
begin
  if FPaddingBottom=AValue then Exit;
  FPaddingBottom:=AValue;

  Change;
end;

procedure TBCFont.SetPaddingLeft(AValue: integer);
begin
  if FPaddingLeft=AValue then Exit;
  FPaddingLeft:=AValue;

  Change;
end;

procedure TBCFont.SetPaddingRight(AValue: integer);
begin
  if FPaddingRight=AValue then Exit;
  FPaddingRight:=AValue;

  Change;
end;

procedure TBCFont.SetPaddingTop(AValue: integer);
begin
  if FPaddingTop=AValue then Exit;
  FPaddingTop:=AValue;

  Change;
end;

procedure TBCFont.SetShadow(AValue: boolean);
begin
  if FShadow = AValue then
    Exit;
  FShadow := AValue;

  Change;
end;

procedure TBCFont.SetShadowColor(AValue: TColor);
begin
  if FShadowColor = AValue then
    Exit;
  FShadowColor := AValue;

  Change;
end;

procedure TBCFont.SetShadowColorOpacity(AValue: byte);
begin
  if FShadowColorOpacity = AValue then
    Exit;
  FShadowColorOpacity := AValue;

  Change;
end;

procedure TBCFont.SetShadowOffsetX(AValue: shortint);
begin
  if FShadowOffsetX = AValue then
    Exit;
  FShadowOffsetX := AValue;

  Change;
end;

procedure TBCFont.SetShadowOffsetY(AValue: shortint);
begin
  if FShadowOffsetY = AValue then
    Exit;
  FShadowOffsetY := AValue;

  Change;
end;

procedure TBCFont.SetShadowRadius(AValue: byte);
begin
  if FShadowRadius = AValue then
    Exit;
  FShadowRadius := AValue;

  Change;
end;

procedure TBCFont.SetSingleLine(AValue: boolean);
begin
  if FSingleLine = AValue then
    Exit;
  FSingleLine := AValue;

  if FSingleLine then
    FWordBreak := False;

  Change;
end;

procedure TBCFont.SetStyle(AValue: TFontStyles);
begin
  if FStyle = AValue then
    Exit;
  FStyle := AValue;

  Change;
end;

procedure TBCFont.SetTextAlignment(AValue: TBCAlignment);
begin
  if FTextAlignment = AValue then
    Exit;
  FTextAlignment := AValue;

  Change;
end;

procedure TBCFont.SetWordBreak(AValue: boolean);
begin
  if FWordBreak = AValue then
    Exit;
  FWordBreak := AValue;

  if FWordBreak then
    FSingleLine := False;

  Change;
end;

constructor TBCFont.Create(AControl: TControl);
begin
  inherited Create(AControl);

  // That is temporary. BGRABitmap draw some yellow background when fqSystemClearType.
  // This problem is reported
  {$IFDEF LCLGTK2}
  FFontQuality := fqFineAntialiasing;
  {$ELSE}
  FFontQuality := fqSystemClearType;
  {$ENDIF}
  FShadow := False;
  FShadowColor := clBlack;
  FShadowColorOpacity := 255;
  FShadowRadius := 5;
  FShadowOffsetX := 5;
  FShadowOffsetY := 5;
  FHeight := 0;
  FTextAlignment := bcaCenter;
  FStyle := [];
  FName := DefFontData.Name;
  FColor := clDefault;
  FDisabledColor := clNone;
  FWordBreak := False;
  FSingleLine := True;
  FEndEllipsis := False;
end;

procedure TBCFont.Assign(Source: TPersistent);
begin
  if Source is TBCFont then
  begin
    FColor := TBCFont(Source).FColor;
    FDisabledColor := TBCFont(Source).FDisabledColor;
    FEndEllipsis := TBCFont(Source).FEndEllipsis;
    FFontQuality := TBCFont(Source).FFontQuality;
    FHeight := TBCFont(Source).FHeight;
    FName := TBCFont(Source).FName;
    FSingleLine := TBCFont(Source).FSingleLine;
    FShadow := TBCFont(Source).FShadow;
    FShadowColor := TBCFont(Source).FShadowColor;
    FShadowColorOpacity := TBCFont(Source).FShadowColorOpacity;
    FShadowRadius := TBCFont(Source).FShadowRadius;
    FShadowOffsetX := TBCFont(Source).FShadowOffsetX;
    FShadowOffsetY := TBCFont(Source).FShadowOffsetY;
    FStyle := TBCFont(Source).FStyle;
    FTextAlignment := TBCFont(Source).FTextAlignment;
    FWordBreak := TBCFont(Source).FWordBreak;
    FPaddingLeft:= TBCFont(Source).PaddingLeft;
    FPaddingTop:= TBCFont(Source).PaddingTop;
    FPaddingRight:= TBCFont(Source).PaddingRight;
    FPaddingBottom:= TBCFont(Source).PaddingBottom;

    Change;
  end else
  if Source is TFont then
  begin
    FColor := TFont(Source).Color;
    FHeight := -TFont(Source).Height;
    FName := TFont(Source).Name;
    FStyle:= TFont(Source).Style;

    Change;
  end else
    inherited Assign(Source);
end;

procedure TBCFont.Scale(AScale: single; APreserveDefaultHeight: boolean);
var
  bmp: TBitmap;
begin
  // we need to have an actual height and not the default value
  if (Height = 0) and not APreserveDefaultHeight then
  begin
    bmp := TBitmap.Create;
    bmp.Canvas.Font.Name:= Name;
    bmp.Canvas.Font.Height:= 0;
    bmp.Canvas.Font.Style:= Style;
    Height := -bmp.Canvas.TextHeight('Bgra');
    bmp.Free;
  end;
  Height := round(Height * AScale);
  ShadowRadius:= min(high(ShadowRadius), round(ShadowRadius * AScale));
  ShadowOffsetX:= max(low(ShadowOffsetX), min(high(ShadowOffsetX), round(ShadowOffsetX*AScale)));
  ShadowOffsetY:= max(low(ShadowOffsetY), min(high(ShadowOffsetY), round(ShadowOffsetY*AScale)));
  PaddingLeft:= round(PaddingLeft * AScale);
  PaddingTop:= round(PaddingTop * AScale);
  PaddingRight:= round(PaddingRight * AScale);
  PaddingBottom:= round(PaddingBottom * AScale);
end;

{ TBCBackground }

procedure TBCBackground.SetStyle(AValue: TBCBackgroundStyle);
begin
  if FStyle = AValue then
    Exit;
  FStyle := AValue;

  Change;
end;

constructor TBCBackground.Create(AControl: TControl);
begin
  FStyle := bbsColor;
  FColorOpacity := 255;
  FGradient1 := TBCGradient.Create(AControl);
  FGradient2 := TBCGradient.Create(AControl);
  FGradient1EndPercent := 35;

  FGradient1.OnChange := OnChangeChildProperty;
  FGradient2.OnChange := OnChangeChildProperty;
  inherited Create(AControl);
end;

destructor TBCBackground.Destroy;
begin
  FGradient1.Free;
  FGradient2.Free;
  inherited Destroy;
end;

procedure TBCBackground.Assign(Source: TPersistent);
begin
  if Source is TBCBackground then
  begin
    FColor := TBCBackground(Source).FColor;
    FColorOpacity := TBCBackground(Source).FColorOpacity;
    FGradient1EndPercent := TBCBackground(Source).FGradient1EndPercent;
    FStyle := TBCBackground(Source).FStyle;
    FGradient1.Assign(TBCBackground(Source).FGradient1);
    FGradient2.Assign(TBCBackground(Source).FGradient2);
  end
  else
    inherited Assign(Source);
end;

procedure TBCBackground.Scale(AScale: single);
begin
  FGradient1.Scale(AScale);
  FGradient2.Scale(AScale);
end;

procedure TBCBackground.SetGradient1(AValue: TBCGradient);
begin
  if FGradient1 = AValue then
    Exit;
  FGradient1 := AValue;

  Change;
end;

procedure TBCBackground.OnChangeChildProperty(Sender: TObject; AData: PtrInt);
begin
  Change(AData);
end;

procedure TBCBackground.SetColor(AValue: TColor);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;

  Change;
end;

procedure TBCBackground.SetColorOpacity(AValue: byte);
begin
  if FColorOpacity = AValue then
    Exit;
  FColorOpacity := AValue;

  Change;
end;

procedure TBCBackground.SetGradient1EndPercent(AValue: single);
begin
  if FGradient1EndPercent = AValue then
    Exit;
  FGradient1EndPercent := AValue;

  Change;
end;

procedure TBCBackground.SetGradient2(AValue: TBCGradient);
begin
  if FGradient2 = AValue then
    Exit;
  FGradient2 := AValue;

  Change;
end;

{ TBCBorder }

procedure TBCBorder.SetLightColor(AValue: TColor);
begin
  if FLightColor = AValue then
    Exit;
  FLightColor := AValue;

  Change;
end;

procedure TBCBorder.SetColor(AValue: TColor);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;

  Change;
end;

procedure TBCBorder.SetColorOpacity(AValue: byte);
begin
  if FColorOpacity = AValue then
    Exit;
  FColorOpacity := AValue;

  Change;
end;

procedure TBCBorder.SetLightOpacity(AValue: byte);
begin
  if FLightOpacity = AValue then
    Exit;
  FLightOpacity := AValue;

  Change;
end;

procedure TBCBorder.SetLightWidth(AValue: integer);
begin
  if FLightWidth = AValue then
    Exit;
  FLightWidth := AValue;

  Change;
end;

procedure TBCBorder.SetStyle(AValue: TBCBorderStyle);
begin
  if FStyle = AValue then
    Exit;
  FStyle := AValue;

  Change;
end;

procedure TBCBorder.SetWidth(AValue: integer);
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;

  Change;
end;

constructor TBCBorder.Create(AControl: TControl);
begin
  FColor := clBlack;
  FColorOpacity := 255;
  FLightWidth := 0;
  FLightOpacity := 255;
  FLightColor := clWhite;
  FStyle := bboSolid;
  FWidth := 1;
  inherited Create(AControl);
end;

procedure TBCBorder.Assign(Source: TPersistent);
begin
  if Source is TBCBorder then
  begin
    FColor := TBCBorder(Source).FColor;
    FColorOpacity := TBCBorder(Source).FColorOpacity;
    FLightColor := TBCBorder(Source).FLightColor;
    FLightOpacity := TBCBorder(Source).FLightOpacity;
    FLightWidth := TBCBorder(Source).FLightWidth;
    FStyle := TBCBorder(Source).FStyle;
    FWidth := TBCBorder(Source).FWidth;
  end
  else
    inherited Assign(Source);
end;

procedure TBCBorder.Scale(AScale: single);
begin
  LightWidth:= round(LightWidth * AScale);
  Width := round(Width * AScale);
end;

end.
