library bgra_pascalscript_library;

{$mode objfpc}{$H+}

{$IFDEF WINDOWS}
  {$define stdcall}
{$ENDIF}

uses
  Interfaces,
  BGRAPascalScript;

{ String Utility }
  function PWideCharToUTF8(const str: PWideChar): string;
  begin
    result := UTF8Encode(WideString(str));
  end;

{ Library }

  function bgra_GetHighestID: integer; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
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
  procedure bgra_Create(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_Create(id);
  end;

  procedure bgra_CreateWithSize(id: integer; AWidth, AHeight: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_CreateWithSize(id, AWidth, AHeight);
  end;

  procedure bgra_Fill(id: integer; AColor: TBGRAColor); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_Fill(id, AColor);
  end;

  procedure bgra_SetPixel(id: integer; x, y: integer; AColor: TBGRAColor); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_SetPixel(id, x, y, AColor);
  end;

  function bgra_GetPixel(id: integer; x, y: integer): TBGRAColor; {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    Result := BGRAPascalScript.bgra_GetPixel(id, x, y);
  end;

  procedure bgra_CreateFromFile(id: integer; AFilename: PWideChar); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_CreateFromFile(id, PWideCharToUTF8(AFilename));
  end;

  procedure bgra_Destroy(id: integer); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_Destroy(id);
  end;

  procedure bgra_SaveToFile(id: integer; filename: PWideChar); {$IFDEF stdcall}stdcall;{$ELSE}cdecl;{$ENDIF}
  begin
    BGRAPascalScript.bgra_SaveToFile(id, PWideCharToUTF8(filename));
  end;

exports
  bgra_GetHighestID name 'bgraGetHighestID',
  rgb,
  rgba,
  getBlue,
  getGreen,
  getRed,
  getAlpha,
  setBlue,
  setGreen,
  setRed,
  setAlpha,
  bgra_Create name 'bgraCreate',
  bgra_CreateWithSize name 'bgraCreateWithSize',
  bgra_Fill name 'bgraFill',
  bgra_SetPixel name 'bgraSetPixel',
  bgra_GetPixel name 'bgraGetPixel',
  bgra_CreateFromFile name 'bgraCreateFromFile',
  bgra_Destroy name 'bgraDestroy',
  bgra_SaveToFile name 'bgraSaveToFile';

begin
end.
