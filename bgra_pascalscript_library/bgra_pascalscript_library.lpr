library bgra_pascalscript_library;

{$mode objfpc}{$H+}

{$IFDEF WINDOWS}
  {$define stdcall}
{$ENDIF}

uses
  Interfaces,
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

  procedure SaveToFile(id: integer; filename: PWideChar); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_SaveToFile(id, PWideCharToUTF8(filename));
  end;

  { Filters }

  procedure FilterSmartZoom3(id: integer; Option: TMedianOption); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterSmartZoom3(id, Option);
  end;

  procedure FilterGrayscale(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_FilterGrayscale(id);
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
  SaveToFile name 'savetofile',
  FilterSmartZoom3 name 'filtersmartzoom3',
  FilterGrayscale name 'filtergrayscale';

begin
end.
