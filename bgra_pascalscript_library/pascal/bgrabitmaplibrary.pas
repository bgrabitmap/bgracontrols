unit BGRABitmapLibrary;

{$mode objfpc}{$H+}

{$IFDEF Windows}
  {$DEFINE stdcall}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  TBGRAColor = longword;
  TMedianOption = (moNone, moLowSmooth, moMediumSmooth, moHighSmooth);
  TResampleFilter = (rfBox, rfLinear, rfHalfCosine, rfCosine, rfBicubic, rfMitchell, rfSpline, rfLanczos2, rfLanczos3, rfLanczos4, rfBestQuality);
  TRadialBlurType = (rbNormal, rbDisk, rbCorona, rbPrecise, rbFast, rbBox);
  TPointF = packed record x, y: single;end;

function GetHighestID(): integer; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'gethighestid';
function rgb(red, green, blue: byte): TBGRAColor; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'rgb';
function rgba(red, green, blue, alpha: byte): TBGRAColor; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'rgba';
function getBlue(AColor: TBGRAColor): byte; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'getblue';
function getGreen(AColor: TBGRAColor): byte; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'getgreen';
function getRed(AColor: TBGRAColor): byte; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'getred';
function getAlpha(AColor: TBGRAColor): byte; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'getalpha';
function setBlue(AColor: TBGRAColor; AValue: byte): TBGRAColor; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'setblue';
function setGreen(AColor: TBGRAColor; AValue: byte): TBGRAColor; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'setgreen';
function setRed(AColor: TBGRAColor; AValue: byte): TBGRAColor; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'setred';
function setAlpha(AColor: TBGRAColor; AValue: byte): TBGRAColor; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'setalpha';
procedure Create(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'create';
procedure CreateWithSize(id: integer; AWidth, AHeight: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'createwithsize';
procedure Fill(id: integer; AColor: TBGRAColor); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'fill';
procedure SetPixel(id: integer; x, y: integer; AColor: TBGRAColor); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'setpixel';
function GetPixel(id: integer; x, y: integer): TBGRAColor; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'getpixel';
procedure CreateFromFile(id: integer; AFilename: PWideChar); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'createfromfile';
procedure Destroy(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'destroy';
procedure DestroyAll; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'destroyall';
procedure SaveToFile(id: integer; filename: PWideChar); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'savetofile';
{ Filters }
procedure FilterSmartZoom3(id: integer; Option: TMedianOption); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filtersmartzoom3';
procedure FilterMedian(id: integer; Option: TMedianOption); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filtermedian';
procedure FilterSmooth(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filtersmooth';
procedure FilterSharpen(id: integer; Amount: single); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filtersharpen';
procedure FilterSharpenRect(id: integer; ABounds: TRect; Amount: single); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filtersharpenrect';
procedure FilterContour(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filtercontour';
procedure FilterPixelate(id: integer; pixelSize: integer;
useResample: boolean; filter: TResampleFilter); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filterpixelate';
procedure FilterBlurRadial(id: integer; radius: integer; blurType: TRadialBlurType); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filterblurradial';
procedure FilterBlurRadialRect(id: integer; ABounds: TRect;
radius: integer; blurType: TRadialBlurType); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filterblurradialrect';
procedure FilterBlurMotion(id: integer; distance: integer;
angle: single; oriented: boolean); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filterblurmotion';
procedure FilterBlurMotionRect(id: integer; ABounds: TRect;
distance: integer; angle: single; oriented: boolean); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filterblurmotionrect';
procedure FilterCustomBlur(id: integer; mask: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filtercustomblur';
procedure FilterCustomBlurRect(id: integer; ABounds: TRect; mask: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filtercustomblurrect';
procedure FilterEmboss(id: integer; angle: single); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filteremboss';
procedure FilterEmbossRect(id: integer; angle: single; ABounds: TRect); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filterembossrect';
procedure FilterEmbossHighlight(id: integer; FillSelection: boolean); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filterembosshighlight';
procedure FilterEmbossHighlightBorder(id: integer; FillSelection: boolean;
BorderColor: TBGRAColor); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filterembosshighlightborder';
procedure FilterEmbossHighlightBorderAndOffset(id: integer;
FillSelection: boolean; BorderColor: TBGRAColor; Offset: TPoint); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filterembosshighlightborderandoffset';
procedure FilterGrayscale(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filtergrayscale';
procedure FilterGrayscaleRect(id: integer; ABounds: TRect); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filtergrayscalerect';
procedure FilterNormalize(id: integer; eachChannel: boolean); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filternormalize';
procedure FilterNormalizeRect(id: integer; ABounds: TRect; eachChannel: boolean); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filternormalizerect';
procedure FilterRotate(id: integer; origin: TPointF; angle: single;
correctBlur: boolean); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filterrotate';
procedure FilterSphere(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filtersphere';
procedure FilterTwirl(id: integer; ACenter: TPoint; ARadius: single;
ATurn: single; AExponent: single); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filtertwirl';
procedure FilterTwirlRect(id: integer; ABounds: TRect; ACenter: TPoint;
ARadius: single; ATurn: single; AExponent: single); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filtertwirlrect';
procedure FilterCylinder(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filtercylinder';
procedure FilterPlane(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filterplane';




implementation

end.

