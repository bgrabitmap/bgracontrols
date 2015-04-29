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
procedure SaveToFile(id: integer; filename: PWideChar); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'savetofile';
procedure FilterSmartZoom3(id: integer; Option: TMedianOption); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filtersmartzoom3';
procedure FilterGrayscale(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF} external 'bgrabitmap' Name 'filtergrayscale';

implementation

end.

