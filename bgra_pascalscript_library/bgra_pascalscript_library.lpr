library bgra_pascalscript_library;

{$mode objfpc}{$H+}

{$IFDEF WINDOWS}
  {$define stdcall}
{$ENDIF}

uses
  Classes,
  BGRAPascalScript,
  BGRABitmapTypes;

{ String Utility }
  function PWideCharToUTF8(const str: PWideChar): string;
  begin
    result := UTF8Encode(WideString(str));
  end;

{ Library }

  function GetHighestID: integer; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.bgra_GetHighestID;
  end;

  function rgb(red, green, blue: byte): TBGRAColor; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.rgb(red, green, blue);
  end;

  function rgba(red, green, blue, alpha: byte): TBGRAColor; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.rgba(red, green, blue, alpha);
  end;

  function getBlue(AColor: TBGRAColor): byte; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.getBlue(aColor);
  end;

  function getGreen(AColor: TBGRAColor): byte; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.getGreen(AColor);
  end;

  function getRed(AColor: TBGRAColor): byte; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.getRed(AColor);
  end;

  function getAlpha(AColor: TBGRAColor): byte; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.getAlpha(AColor);
  end;

  function setBlue(AColor: TBGRAColor; AValue: byte): TBGRAColor; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.setBlue(AColor, AValue);
  end;

  function setGreen(AColor: TBGRAColor; AValue: byte): TBGRAColor; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.setGreen(AColor, AValue);
  end;

  function setRed(AColor: TBGRAColor; AValue: byte): TBGRAColor; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.setRed(AColor, AValue);
  end;

  function setAlpha(AColor: TBGRAColor; AValue: byte): TBGRAColor; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.setAlpha(AColor, AValue);
  end;

  {Constructors}
  procedure Create(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_Create(id);
  end;

  procedure CreateWithSize(id: integer; AWidth, AHeight: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_CreateWithSize(id, AWidth, AHeight);
  end;

  procedure Fill(id: integer; AColor: TBGRAColor); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_Fill(id, AColor);
  end;

  procedure SetPixel(id: integer; x, y: integer; AColor: TBGRAColor); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_SetPixel(id, x, y, AColor);
  end;

  function GetPixel(id: integer; x, y: integer): TBGRAColor; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.bgra_GetPixel(id, x, y);
  end;

  procedure CreateFromFile(id: integer; AFilename: PWideChar); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_CreateFromFile(id, PWideCharToUTF8(AFilename));
  end;

  procedure Destroy(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_Destroy(id);
  end;

  procedure DestroyAll; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_DestroyAll;
  end;

  procedure SaveToFile(id: integer; filename: PWideChar); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_SaveToFile(id, PWideCharToUTF8(filename));
  end;

  { Filters }

  procedure FilterSmartZoom3(id: integer; Option: TMedianOption); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterSmartZoom3(id, Option);
  end;

  procedure FilterMedian(id: integer; Option: TMedianOption); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterMedian(id, Option);
  end;

  procedure FilterSmooth(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterSmooth(id);
  end;

  procedure FilterSharpen(id: integer; Amount: single); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterSharpen(id, Amount);
  end;

  procedure FilterSharpenRect(id: integer; ABounds: TRect; Amount: single); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterSharpenRect(id, ABounds, Amount);
  end;

  procedure FilterContour(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterContour(id);
  end;

  procedure FilterPixelate(id: integer; pixelSize: integer;
  useResample: boolean; filter: TResampleFilter); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterPixelate(id, pixelSize, useResample, filter);
  end;

  procedure FilterBlurRadial(id: integer; radius: integer; blurType: TRadialBlurType); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterBlurRadial(id, radius, blurType);
  end;

  procedure FilterBlurRadialRect(id: integer; ABounds: TRect;
  radius: integer; blurType: TRadialBlurType); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterBlurRadialRect(id, ABounds, radius, blurType);
  end;

  procedure FilterBlurMotion(id: integer; distance: integer;
    angle: single; oriented: boolean); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterBlurMotion(id, distance, angle, oriented);
  end;

  procedure FilterBlurMotionRect(id: integer; ABounds: TRect;
    distance: integer; angle: single; oriented: boolean); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterBlurMotionRect(id, ABounds, distance, angle, oriented);
  end;

  procedure FilterCustomBlur(id: integer; mask: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterCustomBlur(id, mask);
  end;

  procedure FilterCustomBlurRect(id: integer; ABounds: TRect; mask: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterCustomBlurRect(id, ABounds, mask);
  end;

  procedure FilterEmboss(id: integer; angle: single); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterEmboss(id, angle);
  end;

  procedure FilterEmbossRect(id: integer; angle: single; ABounds: TRect); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterEmbossRect(id, angle, ABounds);
  end;

  procedure FilterEmbossHighlight(id: integer; FillSelection: boolean); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterEmbossHighlight(id, FillSelection);
  end;

  procedure FilterEmbossHighlightBorder(id: integer; FillSelection: boolean;
    BorderColor: TBGRAColor); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterEmbossHighlightBorder(id, FillSelection, BorderColor);
  end;

  procedure FilterEmbossHighlightBorderAndOffset(id: integer;
    FillSelection: boolean; BorderColor: TBGRAColor; Offset: TPoint); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterEmbossHighlightBorderAndOffset(id, FillSelection, BorderColor, Offset);
  end;

  procedure FilterGrayscale(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterGrayscale(id);
  end;

  procedure FilterGrayscaleRect(id: integer; ABounds: TRect); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterGrayscaleRect(id, ABounds);
  end;

  procedure FilterNormalize(id: integer; eachChannel: boolean); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterNormalize(id, eachChannel);
  end;

  procedure FilterNormalizeRect(id: integer; ABounds: TRect; eachChannel: boolean); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterNormalizeRect(id, ABounds, eachChannel);
  end;

  procedure FilterRotate(id: integer; origin: TPointF; angle: single;
    correctBlur: boolean); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterRotate(id, origin, angle, correctBlur);
  end;

  procedure FilterSphere(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterSphere(id);
  end;

  procedure FilterTwirl(id: integer; ACenter: TPoint; ARadius: single;
    ATurn: single; AExponent: single); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterTwirl(id, ACenter, ARadius, ATurn, AExponent);
  end;

  procedure FilterTwirlRect(id: integer; ABounds: TRect; ACenter: TPoint;
    ARadius: single; ATurn: single; AExponent: single); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterTwirlRect(id, ABounds, ACenter, ARadius, ATurn, AExponent);
  end;

  procedure FilterCylinder(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterCylinder(id);
  end;

  procedure FilterPlane(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterPlane(id);
  end;

exports
  GetHighestID name 'gethighestid',
  rgb name 'rgb',
  rgba name 'rgba',
  getBlue name 'getblue',
  getGreen name 'getgreen',
  getRed name 'getred',
  getAlpha name 'getalpha',
  setBlue name 'setblue',
  setGreen name 'setgreen',
  setRed name 'setred',
  setAlpha name 'setalpha',
  Create name 'create',
  CreateWithSize name 'createwithsize',
  Fill name 'fill',
  SetPixel name 'setpixel',
  GetPixel name 'getpixel',
  CreateFromFile name 'createfromfile',
  Destroy name 'destroy',
  DestroyAll name 'destroyall',
  SaveToFile name 'savetofile',
  { Filters }
  FilterSmartZoom3 name 'filtersmartzoom3',
  FilterMedian name 'filtermedian',
  FilterSmooth name 'filtersmooth',
  FilterSharpen name 'filtersharpen',
  FilterSharpenRect name 'filtersharpenrect',
  FilterContour name 'filtercontour',
  FilterPixelate name 'filterpixelate',
  FilterBlurRadial name 'filterblurradial',
  FilterBlurRadialRect name 'filterblurradialrect',
  FilterBlurMotion name 'filterblurmotion',
  FilterBlurMotionRect name 'filterblurmotionrect',
  FilterCustomBlur name 'filtercustomblur',
  FilterCustomBlurRect name 'filtercustomblurrect',
  FilterEmboss name 'filteremboss',
  FilterEmbossRect name 'filterembossrect',
  FilterEmbossHighlight name 'filterembosshighlight',
  FilterEmbossHighlightBorder name 'filterembosshighlightborder',
  FilterEmbossHighlightBorderAndOffset name 'filterembosshighlightborderandoffset',
  FilterGrayscale name 'filtergrayscale',
  FilterGrayscaleRect name 'filtergrayscalerect',
  FilterNormalize name 'filternormalize',
  FilterNormalizeRect name 'filternormalizerect',
  FilterRotate name 'filterrotate',
  FilterSphere name 'filtersphere',
  FilterTwirl name 'filtertwirl',
  FilterTwirlRect name 'filtertwirlrect',
  FilterCylinder name 'filtercylinder',
  FilterPlane name 'filterplane';

begin
end.
