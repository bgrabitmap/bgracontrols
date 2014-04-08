{ Common types for BGRA Controls package

  Copyright (C) 2011 Krzysztof Dibowski dibowski at interia.pl

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit BCTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, BGRABitmap, BGRABitmapTypes, Graphics, BCBasectrls;

type
  TBCMouseState  = (msNone, msHover, msClicked);
  TBCAlignment = (bcaLeftTop, bcaLeftCenter, bcaLeftBottom,
    bcaCenterTop, bcaCenter, bcaCenterBottom, bcaRightTop, bcaRightCenter,
    bcaRightBottom);
  TBCBackgroundStyle = (bbsClear, bbsColor, bbsGradient);
  TBCBorderStyle = (bboNone, bboSolid);
  TBCArrowDirection = (badLeft, badRight, badUp, badDown);
  TBGRATextAlign   = (btaLeft, btaCenter, btaRight); // deprecated
  TBGRATextVAlign  = (btvaTop, btvaCenter, btvaBottom); // deprecated
  TBGRARedrawEvent = procedure(Sender: TObject; Bitmap: TBGRABitmap) of object;

type
  { TBCGradient }

  TBCGradient = class(TBCProperty)
  private
    FColorCorrection: boolean;
    FDrawMode:        TDrawMode;
    FGradientType:    TGradientType;
    FEndColor:        TColor;
    FEndColorOpacity: byte;
    FPoint1XPercent:  single;
    FPoint1YPercent:  single;
    FPoint2XPercent:  single;
    FPoint2YPercent:  single;
    FSinus:           boolean;
    FStartColor:      TColor;
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
  published
    property StartColor: TColor Read FStartColor Write SetStartColor;
    property StartColorOpacity: byte Read FStartColorOpacity
      Write SetStartColorOpacity;
    property DrawMode: TDrawMode Read FDrawMode Write SetDrawMode;
    property EndColor: TColor Read FEndColor Write SetEndColor;
    property EndColorOpacity: byte Read FEndColorOpacity
      Write SetEndColorOpacity;
    property ColorCorrection: boolean Read FColorCorrection
      Write SetColorCorrection;
    property GradientType: TGradientType Read FGradientType
      Write SetGradientType;
    property Point1XPercent: single Read FPoint1XPercent Write SetPoint1XPercent;
    property Point1YPercent: single Read FPoint1YPercent Write SetPoint1YPercent;
    property Point2XPercent: single Read FPoint2XPercent Write SetPoint2XPercent;
    property Point2YPercent: single Read FPoint2YPercent Write SetPoint2YPercent;
    property Sinus: boolean Read FSinus Write SetSinus;
  end;

  { TBCFont }

  TBCFont = class(TBCProperty)
  private
    FColor: TColor;
    FEndEllipsis: Boolean;
    FFontQuality: TBGRAFontQuality;
    FHeight: Integer;
    FName: String;
    FShadow: Boolean;
    FShadowColor: TColor;
    FShadowColorOpacity: Byte;
    FShadowOffsetX: ShortInt;
    FShadowOffsetY: ShortInt;
    FShadowRadius: Byte;
    FSingleLine: Boolean;
    FStyle: TFontStyles;
    FTextAlignment: TBCAlignment;
    FWordBreak: Boolean;
    function IsNamStored: Boolean;
    procedure SetColor(AValue: TColor);
    procedure SetEndEllipsis(AValue: Boolean);
    procedure SetFontQuality(AValue: TBGRAFontQuality);
    procedure SetHeight(AValue: Integer);
    procedure SetName(AValue: String);
    procedure SetShadow(AValue: Boolean);
    procedure SetShadowColor(AValue: TColor);
    procedure SetShadowColorOpacity(AValue: Byte);
    procedure SetShadowOffsetX(AValue: ShortInt);
    procedure SetShadowOffsetY(AValue: ShortInt);
    procedure SetShadowRadius(AValue: Byte);
    procedure SetSingleLine(AValue: Boolean);
    procedure SetStyle(AValue: TFontStyles);
    procedure SetTextAlignment(AValue: TBCAlignment);
    procedure SetWordBreak(AValue: Boolean);
  public
    constructor Create(AControl: TControl); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor;
    property EndEllipsis: Boolean read FEndEllipsis write SetEndEllipsis;
    property FontQuality: TBGRAFontQuality
      read FFontQuality write SetFontQuality;
    property Height: Integer read FHeight write SetHeight;
    property Name: String read FName write SetName stored IsNamStored;
    property SingleLine: Boolean read FSingleLine write SetSingleLine;
    property Shadow: Boolean read FShadow write SetShadow;
    property ShadowColor: TColor
      read FShadowColor write SetShadowColor;
    property ShadowColorOpacity: Byte read FShadowColorOpacity write SetShadowColorOpacity;
    property ShadowRadius: Byte
      read FShadowRadius write SetShadowRadius;
    property ShadowOffsetX: ShortInt
      read FShadowOffsetX write SetShadowOffsetX;
    property ShadowOffsetY: ShortInt
      read FShadowOffsetY write SetShadowOffsetY;
    property Style: TFontStyles read FStyle write SetStyle;
    property TextAlignment: TBCAlignment
      read FTextAlignment write SetTextAlignment;
    property WordBreak: Boolean read FWordBreak write SetWordBreak;
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
    procedure OnChangeChildProperty(Sender: TObject; AData: PtrInt);
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
  published
    property Color: TColor read FColor write SetColor;
    property ColorOpacity: byte read FColorOpacity write SetColorOpacity;
    property Gradient1: TBCGradient read FGradient1 write SetGradient1;
    property Gradient2: TBCGradient read FGradient2 write SetGradient2;
    property Gradient1EndPercent: single read FGradient1EndPercent
      write SetGradient1EndPercent;
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
    FWidth: Integer;
    procedure SetColor(AValue: TColor);
    procedure SetColorOpacity(AValue: byte);
    procedure SetLightColor(AValue: TColor);
    procedure SetLightOpacity(AValue: byte);
    procedure SetLightWidth(AValue: integer);
    procedure SetStyle(AValue: TBCBorderStyle);
    procedure SetWidth(AValue: Integer);
  public
    constructor Create(AControl: TControl); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor;
    property ColorOpacity: byte read FColorOpacity
      write SetColorOpacity;
    property LightColor: TColor read FLightColor write SetLightColor;
    property LightOpacity: byte read FLightOpacity write SetLightOpacity;
    property LightWidth: integer read FLightWidth write SetLightWidth;
    property Style: TBCBorderStyle read FStyle write SetStyle;
    property Width: Integer read FWidth write SetWidth;
  end;

  { TBCRounding }

  TBCRounding = class(TBCProperty)
  private
    FRoundOptions: TRoundRectangleOptions;
    FRoundX: Byte;
    FRoundY: Byte;
    procedure SetRoundOptions(AValue: TRoundRectangleOptions);
    procedure SetRoundX(AValue: Byte);
    procedure SetRoundY(AValue: Byte);
  public
    constructor Create(AControl: TControl); override;
    procedure Assign(Source: TPersistent); override;
  published
    property RoundX: Byte read FRoundX write SetRoundX;
    property RoundY: Byte read FRoundY write SetRoundY;
    property RoundOptions: TRoundRectangleOptions
      read FRoundOptions write SetRoundOptions;
  end;


{const
  DEF_START_COL      = $00EFE6D2;
  DEF_END_COL        = $00C87511;
  DEF_BORD_COL       = $00AB713B;
  DEF_BORD_COL_HOVER = $00D7B697;
  DEF_FONT_COLOR     = $0072412A; }

implementation

{ TBCRounding }

procedure TBCRounding.SetRoundOptions(AValue: TRoundRectangleOptions);
begin
  if FRoundOptions = AValue then Exit;
  FRoundOptions := AValue;

  Change;
end;

procedure TBCRounding.SetRoundX(AValue: Byte);
begin
  if FRoundX = AValue then Exit;
  FRoundX := AValue;

  Change;
end;

procedure TBCRounding.SetRoundY(AValue: Byte);
begin
  if FRoundY = AValue then Exit;
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
    FRoundX       := TBCRounding(Source).FRoundX;
    FRoundY       := TBCRounding(Source).FRoundY;
    FRoundOptions := TBCRounding(Source).FRoundOptions;
  end
  else
    inherited Assign(Source);
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

{ TBCFont }

function TBCFont.IsNamStored: Boolean;
begin
  Result := DefFontData.Name <> Name;
end;

procedure TBCFont.SetColor(AValue: TColor);
begin
  if FColor = AValue then Exit;
  FColor := AValue;

  Change;
end;

procedure TBCFont.SetEndEllipsis(AValue: Boolean);
begin
  if FEndEllipsis = AValue then Exit;
  FEndEllipsis := AValue;

  Change;
end;

procedure TBCFont.SetFontQuality(AValue: TBGRAFontQuality);
begin
  if FFontQuality = AValue then Exit;
  FFontQuality := AValue;

  Change;
end;

procedure TBCFont.SetHeight(AValue: Integer);
begin
  if FHeight = AValue then Exit;
  FHeight := AValue;

  Change;
end;

procedure TBCFont.SetName(AValue: String);
begin
  if FName = AValue then Exit;
  FName := AValue;
  if FName='' then
    FName := 'default';
  Change;
end;

procedure TBCFont.SetShadow(AValue: Boolean);
begin
  if FShadow = AValue then Exit;
  FShadow := AValue;

  Change;
end;

procedure TBCFont.SetShadowColor(AValue: TColor);
begin
  if FShadowColor = AValue then Exit;
  FShadowColor := AValue;

  Change;
end;

procedure TBCFont.SetShadowColorOpacity(AValue: Byte);
begin
  if FShadowColorOpacity = AValue then Exit;
  FShadowColorOpacity := AValue;

  Change;
end;

procedure TBCFont.SetShadowOffsetX(AValue: ShortInt);
begin
  if FShadowOffsetX = AValue then Exit;
  FShadowOffsetX := AValue;

  Change;
end;

procedure TBCFont.SetShadowOffsetY(AValue: ShortInt);
begin
  if FShadowOffsetY = AValue then Exit;
  FShadowOffsetY := AValue;

  Change;
end;

procedure TBCFont.SetShadowRadius(AValue: Byte);
begin
  if FShadowRadius = AValue then Exit;
  FShadowRadius := AValue;

  Change;
end;

procedure TBCFont.SetSingleLine(AValue: Boolean);
begin
  if FSingleLine = AValue then Exit;
  FSingleLine := AValue;

  if FSingleLine then
    FWordBreak := False;

  Change;
end;

procedure TBCFont.SetStyle(AValue: TFontStyles);
begin
  if FStyle = AValue then Exit;
  FStyle := AValue;

  Change;
end;

procedure TBCFont.SetTextAlignment(AValue: TBCAlignment);
begin
  if FTextAlignment = AValue then Exit;
  FTextAlignment := AValue;

  Change;
end;

procedure TBCFont.SetWordBreak(AValue: Boolean);
begin
  if FWordBreak = AValue then Exit;
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
  FFontQuality        := fqFineAntialiasing;
  {$ELSE}
  FFontQuality        := fqSystemClearType;
  {$ENDIF}
  FShadow             := False;
  FShadowColor        := clBlack;
  FShadowColorOpacity := 255;
  FShadowRadius       := 5;
  FShadowOffsetX      := 5;
  FShadowOffsetY      := 5;
  FHeight             := 0;
  FTextAlignment      := bcaCenter;
  FStyle              := [];
  FName               := DefFontData.Name;
  FColor              := clDefault;
  FWordBreak          := False;
  FSingleLine         := True;
  FEndEllipsis        := False;
end;

procedure TBCFont.Assign(Source: TPersistent);
begin
  if Source is TBCFont then
  begin
    FColor              := TBCFont(Source).FColor;
    FEndEllipsis        := TBCFont(Source).FEndEllipsis;
    FFontQuality        := TBCFont(Source).FFontQuality;
    FHeight             := TBCFont(Source).FHeight;
    FName               := TBCFont(Source).FName;
    FSingleLine         := TBCFont(Source).FSingleLine;
    FShadow             := TBCFont(Source).FShadow;
    FShadowColor        := TBCFont(Source).FShadowColor;
    FShadowColorOpacity := TBCFont(Source).FShadowColorOpacity;
    FShadowRadius       := TBCFont(Source).FShadowRadius;
    FShadowOffsetX      := TBCFont(Source).FShadowOffsetX;
    FShadowOffsetY      := TBCFont(Source).FShadowOffsetY;
    FStyle              := TBCFont(Source).FStyle;
    FTextAlignment      := TBCFont(Source).FTextAlignment;
    FWordBreak          := TBCFont(Source).FWordBreak;

    Change;
  end
  else
    inherited Assign(Source);
end;

{ TBCBackground }

procedure TBCBackground.SetStyle(AValue: TBCBackgroundStyle);
begin
  if FStyle = AValue then Exit;
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

  FGradient1.OnChange := @OnChangeChildProperty;
  FGradient2.OnChange := @OnChangeChildProperty;
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
    FColor               := TBCBackground(Source).FColor;
    FColorOpacity        := TBCBackground(Source).FColorOpacity;
    FGradient1EndPercent := TBCBackground(Source).FGradient1EndPercent;
    FStyle               := TBCBackground(Source).FStyle;
    FGradient1.Assign(TBCBackground(Source).FGradient1);
    FGradient2.Assign(TBCBackground(Source).FGradient2);
  end
  else
    inherited Assign(Source);
end;

procedure TBCBackground.SetGradient1(AValue: TBCGradient);
begin
  if FGradient1 = AValue then Exit;
  FGradient1 := AValue;

  Change;
end;

procedure TBCBackground.OnChangeChildProperty(Sender: TObject; AData: PtrInt);
begin
  Change(AData);
end;

procedure TBCBackground.SetColor(AValue: TColor);
begin
  if FColor = AValue then Exit;
  FColor := AValue;

  Change;
end;

procedure TBCBackground.SetColorOpacity(AValue: byte);
begin
  if FColorOpacity = AValue then Exit;
  FColorOpacity := AValue;

  Change;
end;

procedure TBCBackground.SetGradient1EndPercent(AValue: single);
begin
  if FGradient1EndPercent = AValue then Exit;
  FGradient1EndPercent := AValue;

  Change;
end;

procedure TBCBackground.SetGradient2(AValue: TBCGradient);
begin
  if FGradient2 = AValue then Exit;
  FGradient2 := AValue;

  Change;
end;

{ TBCBorder }

procedure TBCBorder.SetLightColor(AValue: TColor);
begin
  if FLightColor = AValue then Exit;
  FLightColor := AValue;

  Change;
end;

procedure TBCBorder.SetColor(AValue: TColor);
begin
  if FColor = AValue then Exit;
  FColor := AValue;

  Change;
end;

procedure TBCBorder.SetColorOpacity(AValue: byte);
begin
  if FColorOpacity = AValue then Exit;
  FColorOpacity := AValue;

  Change;
end;

procedure TBCBorder.SetLightOpacity(AValue: byte);
begin
  if FLightOpacity = AValue then Exit;
  FLightOpacity := AValue;

  Change;
end;

procedure TBCBorder.SetLightWidth(AValue: integer);
begin
  if FLightWidth = AValue then Exit;
  FLightWidth := AValue;

  Change;
end;

procedure TBCBorder.SetStyle(AValue: TBCBorderStyle);
begin
  if FStyle = AValue then Exit;
  FStyle := AValue;

  Change;
end;

procedure TBCBorder.SetWidth(AValue: Integer);
begin
  if FWidth = AValue then Exit;
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
    FColor        := TBCBorder(Source).FColor;
    FColorOpacity := TBCBorder(Source).FColorOpacity;
    FLightColor   := TBCBorder(Source).FLightColor;
    FLightOpacity := TBCBorder(Source).FLightOpacity;
    FLightWidth   := TBCBorder(Source).FLightWidth;
    FStyle        := TBCBorder(Source).FStyle;
    FWidth        := TBCBorder(Source).FWidth;
  end
  else
    inherited Assign(Source);
end;

end.
