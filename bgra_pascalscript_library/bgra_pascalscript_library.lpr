library bgra_pascalscript_library;

{$mode objfpc}{$H+}

uses
  Interfaces, BGRAPascalScript;

  function bgra_GetHighestID: Integer; stdcall;
  begin
    result := BGRAPascalScript.bgra_GetHighestID;
  end;

  function rgb(red, green, blue: byte): TBGRAColor; stdcall;
  begin
    result := BGRAPascalScript.rgb(red, green, blue);
  end;

  function rgba(red, green, blue, alpha: byte): TBGRAColor; stdcall;
  begin
    Result := BGRAPascalScript.rgba(red, green, blue, alpha);
  end;

  function getBlue(AColor: TBGRAColor): byte; stdcall;
  begin
    Result := BGRAPascalScript.getBlue(aColor);
  end;

  function getGreen(AColor: TBGRAColor): byte; stdcall;
  begin
    Result := BGRAPascalScript.getGreen(AColor);
  end;

  function getRed(AColor: TBGRAColor): byte; stdcall;
  begin
    Result := BGRAPascalScript.getRed(AColor);
  end;

  function getAlpha(AColor: TBGRAColor): byte; stdcall;
  begin
    Result := BGRAPascalScript.getAlpha(AColor);
  end;

  function setBlue(AColor: TBGRAColor; AValue: byte): TBGRAColor; stdcall;
  begin
    Result := BGRAPascalScript.setBlue(AColor, AValue);
  end;

  function setGreen(AColor: TBGRAColor; AValue: byte): TBGRAColor; stdcall;
  begin
    Result := BGRAPascalScript.setGreen(AColor, AValue);
  end;

  function setRed(AColor: TBGRAColor; AValue: byte): TBGRAColor; stdcall;
  begin
    Result := BGRAPascalScript.setRed(AColor, AValue);
  end;

  function setAlpha(AColor: TBGRAColor; AValue: byte): TBGRAColor; stdcall;
  begin
    Result := BGRAPascalScript.setAlpha(AColor, AValue);
  end;

  {Constructors}
  procedure bgra_Create(id: integer); stdcall;
  begin
    BGRAPascalScript.bgra_Create(id);
  end;

  procedure bgra_CreateWithSize(id: integer; AWidth, AHeight: integer); stdcall;
  begin
    BGRAPascalScript.bgra_CreateWithSize(id, AWidth, AHeight);
  end;

  procedure bgra_Fill(id: integer; AColor: TBGRAColor); stdcall;
  begin
    BGRAPascalScript.bgra_Fill(id, AColor);
  end;

  procedure bgra_SetPixel(id: integer; x, y: integer; AColor: TBGRAColor); stdcall;
  begin
    BGRAPascalScript.bgra_SetPixel(id, x, y, AColor);
  end;

  function bgra_GetPixel(id: integer; x, y: integer): TBGRAColor; stdcall;
  begin
    Result := BGRAPascalScript.bgra_GetPixel(id, x, y);
  end;

  procedure bgra_CreateFromFile(id: integer; AFilename: string); stdcall;
  begin
    BGRAPascalScript.bgra_CreateFromFile(id, AFilename);
  end;

  procedure bgra_Destroy(id: integer); stdcall;
  begin
    BGRAPascalScript.bgra_Destroy(id);
  end;

exports
  bgra_GetHighestID,
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
  bgra_Create,
  bgra_CreateWithSize,
  bgra_Fill,
  bgra_SetPixel,
  bgra_GetPixel,
  bgra_CreateFromFile,
  bgra_Destroy;

begin
end.
