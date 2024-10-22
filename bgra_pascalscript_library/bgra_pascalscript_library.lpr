{
  Created by BGRA Controls Team
  Circular, lainz (007) and Fred vS.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html

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


library bgra_pascalscript_library;

{$mode objfpc}{$H+}

{.$DEFINE Java}     //// uncomment if you want a Java-compatible library

uses
 {$IF DEFINED(Java)}
  jni,
 {$endif}
  Classes,
  BGRAPascalScript,
  BGRABitmapTypes;

{ String Utility }
  function PWideCharToUTF8(const str: PWideChar): string;
  begin
    result := UTF8Encode(WideString(str));
  end;

{ Library }

  function GetHighestID({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject {$endif}): integer; {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.bgra_GetHighestID;
  end;

  function rgb({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} red, green, blue: byte): TBGRAColor; {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.rgb(red, green, blue);
  end;

  function rgba({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} red, green, blue, alpha: byte): TBGRAColor; {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.rgba(red, green, blue, alpha);
  end;

  function getBlue({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} AColor: TBGRAColor): byte; {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.getBlue(aColor);
  end;

  function getGreen({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} AColor: TBGRAColor): byte; {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.getGreen(AColor);
  end;

  function getRed({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} AColor: TBGRAColor): byte; {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.getRed(AColor);
  end;

  function getAlpha({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} AColor: TBGRAColor): byte; {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.getAlpha(AColor);
  end;

  function setBlue({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} AColor: TBGRAColor; AValue: byte): TBGRAColor; {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.setBlue(AColor, AValue);
  end;

  function setGreen({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} AColor: TBGRAColor; AValue: byte): TBGRAColor; {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.setGreen(AColor, AValue);
  end;

  function setRed({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} AColor: TBGRAColor; AValue: byte): TBGRAColor; {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.setRed(AColor, AValue);
  end;

  function setAlpha({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} AColor: TBGRAColor; AValue: byte): TBGRAColor; {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.setAlpha(AColor, AValue);
  end;

  {Constructors}
  procedure Create({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_Create(id);
  end;

  procedure CreateWithSize({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; AWidth, AHeight: integer); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_CreateWithSize(id, AWidth, AHeight);
  end;

  procedure Fill({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; AColor: TBGRAColor); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_Fill(id, AColor);
  end;

  procedure SetPixel({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; x, y: integer; AColor: TBGRAColor); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_SetPixel(id, x, y, AColor);
  end;

  function GetPixel({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; x, y: integer): TBGRAColor; {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.bgra_GetPixel(id, x, y);
  end;

 {$IF DEFINED(Java)}
procedure CreateFromFile(PEnv: PJNIEnv; Obj: JObject ; id: integer; AFilename: JString); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_CreateFromFile(id, (PEnv^^).GetStringUTFChars(PEnv, AFilename, nil));
      (PEnv^^).ReleaseStringUTFChars(PEnv, AFilename, nil);
  end;
{$else}
procedure CreateFromFile(id: integer; AFilename: PWideChar); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_CreateFromFile(id, PWideCharToUTF8(AFilename));
  end;
{$endif}

  procedure Destroy({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_Destroy(id);
  end;

  procedure DestroyAll({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject{$endif}); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_DestroyAll;
  end;

 {$IF DEFINED(Java)}
procedure SaveToFile(PEnv: PJNIEnv; Obj: JObject ; id: integer; Filename: JString); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
   BGRAPascalScript.bgra_SaveToFile(id, (PEnv^^).GetStringUTFChars(PEnv, Filename, nil));
      (PEnv^^).ReleaseStringUTFChars(PEnv, Filename, nil);
  end;
{$else}
procedure SaveToFile(id: integer; filename: PWideChar); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_SaveToFile(id, PWideCharToUTF8(filename));
  end;
{$endif}

  { Filters }

  procedure FilterSmartZoom3({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; Option: TMedianOption); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterSmartZoom3(id, Option);
  end;

  procedure FilterMedian({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; Option: TMedianOption); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterMedian(id, Option);
  end;

  procedure FilterSmooth({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterSmooth(id);
  end;

  procedure FilterSharpen({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; Amount: single); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterSharpen(id, Amount);
  end;

  procedure FilterSharpenRect({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; ABounds: TRect; Amount: single); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterSharpenRect(id, ABounds, Amount);
  end;

  procedure FilterContour({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterContour(id);
  end;

  procedure FilterPixelate({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; pixelSize: integer;
  useResample: boolean; filter: TResampleFilter); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterPixelate(id, pixelSize, useResample, filter);
  end;

  procedure FilterBlurRadial({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; radius: integer; blurType: TRadialBlurType); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterBlurRadial(id, radius, blurType);
  end;

  procedure FilterBlurRadialRect({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; ABounds: TRect;
  radius: integer; blurType: TRadialBlurType); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterBlurRadialRect(id, ABounds, radius, blurType);
  end;

  procedure FilterBlurMotion({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; distance: integer;
    angle: single; oriented: boolean); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterBlurMotion(id, distance, angle, oriented);
  end;

  procedure FilterBlurMotionRect({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; ABounds: TRect;
    distance: integer; angle: single; oriented: boolean); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterBlurMotionRect(id, ABounds, distance, angle, oriented);
  end;

  procedure FilterCustomBlur({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; mask: integer); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterCustomBlur(id, mask);
  end;

  procedure FilterCustomBlurRect({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; ABounds: TRect; mask: integer); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterCustomBlurRect(id, ABounds, mask);
  end;

  procedure FilterEmboss({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; angle: single); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterEmboss(id, angle);
  end;

  procedure FilterEmbossRect({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; angle: single; ABounds: TRect); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterEmbossRect(id, angle, ABounds);
  end;

  procedure FilterEmbossHighlight({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; FillSelection: boolean); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterEmbossHighlight(id, FillSelection);
  end;

  procedure FilterEmbossHighlightBorder({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; FillSelection: boolean;
    BorderColor: TBGRAColor); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterEmbossHighlightBorder(id, FillSelection, BorderColor);
  end;

  procedure FilterEmbossHighlightBorderAndOffset({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer;
    FillSelection: boolean; BorderColor: TBGRAColor; Offset: TPoint); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterEmbossHighlightBorderAndOffset(id, FillSelection, BorderColor, Offset);
  end;

  procedure FilterGrayscale({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterGrayscale(id);
  end;

  procedure FilterGrayscaleRect({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; ABounds: TRect); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterGrayscaleRect(id, ABounds);
  end;

  procedure FilterNormalize({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; eachChannel: boolean); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterNormalize(id, eachChannel);
  end;

  procedure FilterNormalizeRect({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; ABounds: TRect; eachChannel: boolean); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterNormalizeRect(id, ABounds, eachChannel);
  end;

  procedure FilterRotate({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; origin: TPointF; angle: single;
    correctBlur: boolean); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterRotate(id, origin, angle, correctBlur);
  end;

  procedure FilterSphere({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterSphere(id);
  end;

  procedure FilterTwirl({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; ACenter: TPoint; ARadius: single;
    ATurn: single; AExponent: single); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterTwirl(id, ACenter, ARadius, ATurn, AExponent);
  end;

  procedure FilterTwirlRect({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer; ABounds: TRect; ACenter: TPoint;
    ARadius: single; ATurn: single; AExponent: single); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterTwirlRect(id, ABounds, ACenter, ARadius, ATurn, AExponent);
  end;

  procedure FilterCylinder({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterCylinder(id);
  end;

  procedure FilterPlane({$IF DEFINED(Java)}PEnv: PJNIEnv; Obj: JObject ; {$endif} id: integer); {$IFDEF windows}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterPlane(id);
  end;

exports
{$IF DEFINED(Java)}
  GetHighestID name 'Java_bgra_gethighestid',
  rgb name 'Java_bgra_rgb',
  rgba name 'Java_bgra_rgba',
  getBlue name 'Java_bgra_getblue',
  getGreen name 'Java_bgra_getgreen',
  getRed name 'Java_bgra_getred',
  getAlpha name 'Java_bgra_getalpha',
  setBlue name 'Java_bgra_setblue',
  setGreen name 'Java_bgra_setgreen',
  setRed name 'Java_bgra_setred',
  setAlpha name 'Java_bgra_setalpha',
  Create name 'Java_bgra_create',
  CreateWithSize name 'Java_bgra_createwithsize',
  Fill name 'Java_bgra_fill',
  SetPixel name 'Java_bgra_setpixel',
  GetPixel name 'Java_bgra_getpixel',
  CreateFromFile name 'Java_bgra_createfromfile',
  Destroy name 'Java_bgra_destroy',
  DestroyAll name 'Java_bgra_destroyall',
  SaveToFile name 'Java_bgra_savetofile',
  { Filters }
  FilterSmartZoom3 name 'Java_bgra_filtersmartzoom3',
  FilterMedian name 'Java_bgra_filtermedian',
  FilterSmooth name 'Java_bgra_filtersmooth',
  FilterSharpen name 'Java_bgra_filtersharpen',
  FilterSharpenRect name 'Java_bgra_filtersharpenrect',
  FilterContour name 'Java_bgra_filtercontour',
  FilterPixelate name 'Java_bgra_filterpixelate',
  FilterBlurRadial name 'Java_bgra_filterblurradial',
  FilterBlurRadialRect name 'Java_bgra_filterblurradialrect',
  FilterBlurMotion name 'Java_bgra_filterblurmotion',
  FilterBlurMotionRect name 'Java_bgra_filterblurmotionrect',
  FilterCustomBlur name 'Java_bgra_filtercustomblur',
  FilterCustomBlurRect name 'Java_bgra_filtercustomblurrect',
  FilterEmboss name 'Java_bgra_filteremboss',
  FilterEmbossRect name 'Java_bgra_filterembossrect',
  FilterEmbossHighlight name 'Java_bgra_filterembosshighlight',
  FilterEmbossHighlightBorder name 'Java_bgra_filterembosshighlightborder',
  FilterEmbossHighlightBorderAndOffset name 'Java_bgra_filterembosshighlightborderandoffset',
  FilterGrayscale name 'Java_bgra_filtergrayscale',
  FilterGrayscaleRect name 'Java_bgra_filtergrayscalerect',
  FilterNormalize name 'Java_bgra_filternormalize',
  FilterNormalizeRect name 'Java_bgra_filternormalizerect',
  FilterRotate name 'Java_bgra_filterrotate',
  FilterSphere name 'Java_bgra_filtersphere',
  FilterTwirl name 'filtertwirl',
  FilterTwirlRect name 'Java_bgra_filtertwirlrect',
  FilterCylinder name 'Java_bgra_filtercylinder',
  FilterPlane name 'Java_bgra_filterplane';
{$else}
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
{$endif}

begin
end.
