unit BGRAPascalScript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes;

type
  TBGRAColor = longword;

var
  BitmapArray: array of TBGRABitmap;

{Internal use only}
procedure bgra_Initialization;
procedure bgra_Finalization;

procedure bgra_AddBitmap(id: integer);
function bgra_GetHighestID: integer;

function BGRAColorToBGRAPixel(AColor: TBGRAColor): TBGRAPixel;

function rgb(red, green, blue: byte): TBGRAColor;
function rgba(red, green, blue, alpha: byte): TBGRAColor;
function getBlue(AColor: TBGRAColor): byte;
function getGreen(AColor: TBGRAColor): byte;
function getRed(AColor: TBGRAColor): byte;
function getAlpha(AColor: TBGRAColor): byte;
function setBlue(AColor: TBGRAColor; AValue: byte): TBGRAColor;
function setGreen(AColor: TBGRAColor; AValue: byte): TBGRAColor;
function setRed(AColor: TBGRAColor; AValue: byte): TBGRAColor;
function setAlpha(AColor: TBGRAColor; AValue: byte): TBGRAColor;

{Constructors}
// Note: overloaded procedures not supported, use unique identifiers
procedure bgra_Create(id: integer);
procedure bgra_CreateWithSize(id: integer; AWidth, AHeight: integer);
procedure bgra_CreateFromFile(id: integer; AFilename: string);
procedure bgra_Destroy(id: integer);

procedure bgra_Fill(id: integer; AColor: TBGRAColor);
procedure bgra_SetPixel(id: integer; x, y: integer; AColor: TBGRAColor);
function bgra_GetPixel(id: integer; x, y: integer): TBGRAColor;

{Loading functions}
procedure bgra_SaveToFile(id: integer; const filename: string);

{Filters - direct apply}
procedure bgra_FilterSmartZoom3(id: integer; Option: TMedianOption);
procedure bgra_FilterMedian(id: integer; Option: TMedianOption);
procedure bgra_FilterSmooth(id: integer);
procedure bgra_FilterSharpen(id: integer; Amount: single);
procedure bgra_FilterSharpenRect(id: integer; ABounds: TRect; Amount: single);
procedure bgra_FilterContour(id: integer);
procedure bgra_FilterPixelate(id: integer; pixelSize: integer;
  useResample: boolean; filter: TResampleFilter);
procedure bgra_FilterBlurRadial(id: integer; radius: integer; blurType: TRadialBlurType);
procedure bgra_FilterBlurRadialRect(id: integer; ABounds: TRect;
  radius: integer; blurType: TRadialBlurType);
procedure bgra_FilterBlurMotion(id: integer; distance: integer;
  angle: single; oriented: boolean);
procedure bgra_FilterBlurMotionRect(id: integer; ABounds: TRect;
  distance: integer; angle: single; oriented: boolean);
procedure bgra_FilterCustomBlur(id: integer; mask: integer);
procedure bgra_FilterCustomBlurRect(id: integer; ABounds: TRect; mask: integer);
procedure bgra_FilterEmboss(id: integer; angle: single);
procedure bgra_FilterEmbossRect(id: integer; angle: single; ABounds: TRect);
procedure bgra_FilterEmbossHighlight(id: integer; FillSelection: boolean);
procedure bgra_FilterEmbossHighlightBorder(id: integer; FillSelection: boolean;
  BorderColor: TBGRAColor);
procedure bgra_FilterEmbossHighlightBorderAndOffset(id: integer;
  FillSelection: boolean; BorderColor: TBGRAColor; Offset: TPoint);
procedure bgra_FilterGrayscale(id: integer);
procedure bgra_FilterGrayscaleRect(id: integer; ABounds: TRect);
procedure bgra_FilterNormalize(id: integer; eachChannel: boolean);
procedure bgra_FilterNormalizeRect(id: integer; ABounds: TRect; eachChannel: boolean);
procedure bgra_FilterRotate(id: integer; origin: TPointF; angle: single;
  correctBlur: boolean);
procedure bgra_FilterSphere(id: integer);
procedure bgra_FilterTwirl(id: integer; ACenter: TPoint; ARadius: single;
  ATurn: single; AExponent: single);
procedure bgra_FilterTwirlRect(id: integer; ABounds: TRect; ACenter: TPoint;
  ARadius: single; ATurn: single; AExponent: single);
procedure bgra_FilterCylinder(id: integer);
procedure bgra_FilterPlane(id: integer);

implementation

procedure bgra_Initialization;
begin

end;

procedure bgra_Finalization;
var
  i: integer;
begin
  for i := 0 to High(BitmapArray) do
    FreeAndNil(BitmapArray[i]);
  BitmapArray := nil;
end;

procedure bgra_AddBitmap(id: integer);
begin
  if id + 1 > length(BitmapArray) then
    SetLength(BitmapArray, id + 1);
  FreeAndNil(BitmapArray[id]);
end;

function bgra_GetHighestID: integer;
begin
  Result := High(BitmapArray);
end;

function BGRAColorToBGRAPixel(AColor: TBGRAColor): TBGRAPixel;
begin
  Result := TBGRAPixel(
{$IFDEF ENDIAN_BIG}
    SwapEndian
{$ENDIF}
    (AColor));
end;

function rgb(red, green, blue: byte): TBGRAColor;
begin
  Result := blue + (green shl 8) + (red shl 16) + $ff000000;
end;

function rgba(red, green, blue, alpha: byte): TBGRAColor;
begin
  Result := blue + (green shl 8) + (red shl 16) + (alpha shl 24);
end;

function getBlue(AColor: TBGRAColor): byte;
begin
  Result := AColor and $ff;
end;

function getGreen(AColor: TBGRAColor): byte;
begin
  Result := (AColor shr 8) and $ff;
end;

function getRed(AColor: TBGRAColor): byte;
begin
  Result := (AColor shr 16) and $ff;
end;

function getAlpha(AColor: TBGRAColor): byte;
begin
  Result := AColor shr 24;
end;

function setBlue(AColor: TBGRAColor; AValue: byte): TBGRAColor;
begin
  Result := (AColor and $ffffff00) or AValue;
end;

function setGreen(AColor: TBGRAColor; AValue: byte): TBGRAColor;
begin
  Result := (AColor and $ffff00ff) or (AValue shl 8);
end;

function setRed(AColor: TBGRAColor; AValue: byte): TBGRAColor;
begin
  Result := (AColor and $ff00ffff) or (AValue shl 16);
end;

function setAlpha(AColor: TBGRAColor; AValue: byte): TBGRAColor;
begin
  Result := (AColor and $00ffffff) or (AValue shl 24);
end;

procedure bgra_Create(id: integer);
begin
  bgra_AddBitmap(id);
  BitmapArray[id] := TBGRABitmap.Create;
end;

procedure bgra_CreateWithSize(id: integer; AWidth, AHeight: integer);
begin
  bgra_AddBitmap(id);
  BitmapArray[id] := TBGRABitmap.Create(AWidth, AHeight);
end;

procedure bgra_CreateFromFile(id: integer; AFilename: string);
begin
  bgra_AddBitmap(id);
  BitmapArray[id] := TBGRABitmap.Create(AFilename);
end;

procedure bgra_Destroy(id: integer);
begin
  FreeAndNil(BitmapArray[id]);
end;

procedure bgra_Fill(id: integer; AColor: TBGRAColor);
begin
  if Assigned(BitmapArray[id]) then
    BitmapArray[id].Fill(TBGRAPixel(
{$IFDEF ENDIAN_BIG}
      SwapEndian
{$ENDIF}
      (AColor)));
end;

procedure bgra_SetPixel(id: integer; x, y: integer; AColor: TBGRAColor);
begin
  if Assigned(BitmapArray[id]) then
    BitmapArray[id].SetPixel(x, y, TBGRAPixel(
{$IFDEF ENDIAN_BIG}
      SwapEndian
{$ENDIF}
      (AColor)));
end;

function bgra_GetPixel(id: integer; x, y: integer): TBGRAColor;
begin
  if Assigned(BitmapArray[id]) then
    Result :=
{$IFDEF ENDIAN_BIG}
      SwapEndian
{$ENDIF}
      (TBGRAColor(BitmapArray[id].GetPixel(x, y)))
  else
    Result := 0;
end;

procedure bgra_SaveToFile(id: integer; const filename: string);
begin
  BitmapArray[id].SaveToFile(filename);
end;

procedure bgra_FilterSmartZoom3(id: integer; Option: TMedianOption);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterSmartZoom3(Option) as TBGRABitmap);
end;

procedure bgra_FilterMedian(id: integer; Option: TMedianOption);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterMedian(Option) as TBGRABitmap);
end;

procedure bgra_FilterSmooth(id: integer);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterSmooth as TBGRABitmap);
end;

procedure bgra_FilterSharpen(id: integer; Amount: single);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterSharpen(Amount) as TBGRABitmap);
end;

procedure bgra_FilterSharpenRect(id: integer; ABounds: TRect; Amount: single);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterSharpen(ABounds, Amount) as
    TBGRABitmap);
end;

procedure bgra_FilterContour(id: integer);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterContour as TBGRABitmap);
end;

procedure bgra_FilterPixelate(id: integer; pixelSize: integer;
  useResample: boolean; filter: TResampleFilter);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterPixelate(pixelSize,
    useResample, filter) as TBGRABitmap);
end;

procedure bgra_FilterBlurRadial(id: integer; radius: integer;
  blurType: TRadialBlurType);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterBlurRadial(radius, blurType) as
    TBGRABitmap);
end;

procedure bgra_FilterBlurRadialRect(id: integer; ABounds: TRect;
  radius: integer; blurType: TRadialBlurType);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterBlurRadial(ABounds,
    radius, blurType) as TBGRABitmap);
end;

procedure bgra_FilterBlurMotion(id: integer; distance: integer;
  angle: single; oriented: boolean);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterBlurMotion(distance,
    angle, oriented) as TBGRABitmap);
end;

procedure bgra_FilterBlurMotionRect(id: integer; ABounds: TRect;
  distance: integer; angle: single; oriented: boolean);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterBlurMotion(ABounds,
    distance, angle, oriented) as TBGRABitmap);
end;

procedure bgra_FilterCustomBlur(id: integer; mask: integer);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterCustomBlur(BitmapArray[mask]) as
    TBGRABitmap);
end;

procedure bgra_FilterCustomBlurRect(id: integer; ABounds: TRect; mask: integer);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterCustomBlur(ABounds,
    BitmapArray[mask]) as TBGRABitmap);
end;

procedure bgra_FilterEmboss(id: integer; angle: single);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterEmboss(angle) as TBGRABitmap);
end;

procedure bgra_FilterEmbossRect(id: integer; angle: single; ABounds: TRect);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterEmboss(angle, ABounds) as
    TBGRABitmap);
end;

procedure bgra_FilterEmbossHighlight(id: integer; FillSelection: boolean);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterEmbossHighlight(FillSelection) as
    TBGRABitmap);
end;

procedure bgra_FilterEmbossHighlightBorder(id: integer; FillSelection: boolean;
  BorderColor: TBGRAColor);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterEmbossHighlight(
    FillSelection, BGRAColorToBGRAPixel(BorderColor)) as TBGRABitmap);
end;

procedure bgra_FilterEmbossHighlightBorderAndOffset(id: integer;
  FillSelection: boolean; BorderColor: TBGRAColor; Offset: TPoint);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterEmbossHighlight(
    FillSelection, BGRAColorToBGRAPixel(BorderColor), Offset) as TBGRABitmap);
end;

procedure bgra_FilterGrayscale(id: integer);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterGrayscale as TBGRABitmap);
end;

procedure bgra_FilterGrayscaleRect(id: integer; ABounds: TRect);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterGrayscale(ABounds) as TBGRABitmap);
end;

procedure bgra_FilterNormalize(id: integer; eachChannel: boolean);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterNormalize(eachChannel) as
    TBGRABitmap);
end;

procedure bgra_FilterNormalizeRect(id: integer; ABounds: TRect; eachChannel: boolean);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterNormalize(ABounds, eachChannel) as
    TBGRABitmap);
end;

procedure bgra_FilterRotate(id: integer; origin: TPointF; angle: single;
  correctBlur: boolean);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterRotate(origin,
    angle, correctBlur) as TBGRABitmap);
end;

procedure bgra_FilterSphere(id: integer);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterSphere as TBGRABitmap);
end;

procedure bgra_FilterTwirl(id: integer; ACenter: TPoint; ARadius: single;
  ATurn: single; AExponent: single);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterTwirl(ACenter,
    ARadius, ATurn, AExponent) as TBGRABitmap);
end;

procedure bgra_FilterTwirlRect(id: integer; ABounds: TRect; ACenter: TPoint;
  ARadius: single; ATurn: single; AExponent: single);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterTwirl(ABounds,
    ACenter, ARadius, ATurn, AExponent) as TBGRABitmap);
end;

procedure bgra_FilterCylinder(id: integer);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterCylinder as TBGRABitmap);
end;

procedure bgra_FilterPlane(id: integer);
begin
  BGRAReplace(BitmapArray[id], BitmapArray[id].FilterPlane as TBGRABitmap);
end;

initialization
  bgra_Initialization;

finalization
  bgra_Finalization;

end.
