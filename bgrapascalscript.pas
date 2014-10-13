unit BGRAPascalScript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes;

type
  TBGRAColor= LongWord;

var
  BitmapArray: array of TBGRABitmap;

{Internal use only}
procedure bgra_Initialization;
procedure bgra_Finalization;
procedure bgra_AddBitmap(id: Integer);

function rgb(red,green,blue: byte): TBGRAColor;
function rgba(red,green,blue,alpha: byte): TBGRAColor;
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
procedure bgra_Create(id: Integer);
procedure bgra_CreateWithSize(id: Integer; AWidth, AHeight: integer);
procedure bgra_Fill(id: Integer; AColor: TBGRAColor);
procedure bgra_SetPixel(id: Integer; x,y: integer; AColor: TBGRAColor);
function bgra_GetPixel(id: Integer; x,y: integer): TBGRAColor;
procedure bgra_CreateFromFile(id: Integer; AFilename: string);
procedure bgra_Destroy(id: Integer);

implementation

procedure bgra_Initialization;
begin
  //
end;

procedure bgra_Finalization;
var
  i: integer;
begin
  for i:= 0 to High(BitmapArray) do
    FreeAndNil(BitmapArray[i]);
  BitmapArray := nil;
end;

procedure bgra_AddBitmap(id: Integer);
begin
  if id + 1 > length(BitmapArray) then
    SetLength(BitmapArray, id + 1);
  FreeAndNil(BitmapArray[id]);
end;

function rgb(red, green, blue: byte): TBGRAColor;
begin
  result := blue + (green shl 8) + (red shl 16) + $ff000000;
end;

function rgba(red, green, blue, alpha: byte): TBGRAColor;
begin
  result := blue + (green shl 8) + (red shl 16) + (alpha shl 24);
end;

function getBlue(AColor: TBGRAColor): byte;
begin
  result := AColor and $ff;
end;

function getGreen(AColor: TBGRAColor): byte;
begin
  result := (AColor shr 8) and $ff;
end;

function getRed(AColor: TBGRAColor): byte;
begin
  result := (AColor shr 16) and $ff;
end;

function getAlpha(AColor: TBGRAColor): byte;
begin
  result := AColor shr 24;
end;

function setBlue(AColor: TBGRAColor; AValue: byte): TBGRAColor;
begin
  result := (AColor and $ffffff00) or AValue;
end;

function setGreen(AColor: TBGRAColor; AValue: byte): TBGRAColor;
begin
  result := (AColor and $ffff00ff) or (AValue shl 8);
end;

function setRed(AColor: TBGRAColor; AValue: byte): TBGRAColor;
begin
  result := (AColor and $ff00ffff) or (AValue shl 16);
end;

function setAlpha(AColor: TBGRAColor; AValue: byte): TBGRAColor;
begin
  result := (AColor and $00ffffff) or (AValue shl 24);
end;

procedure bgra_Create(id: Integer);
begin
  bgra_AddBitmap(id);
  BitmapArray[id] := TBGRABitmap.Create;
end;

procedure bgra_CreateWithSize(id: Integer; AWidth, AHeight: integer);
begin
  bgra_AddBitmap(id);
  BitmapArray[id] := TBGRABitmap.Create(AWidth,AHeight);
end;

procedure bgra_Fill(id: Integer; AColor: TBGRAColor);
begin
  if Assigned(BitmapArray[id]) then
    BitmapArray[id].Fill(TBGRAPixel({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(AColor)));
end;

procedure bgra_SetPixel(id: Integer; x, y: integer; AColor: TBGRAColor);
begin
  if Assigned(BitmapArray[id]) then
    BitmapArray[id].SetPixel(x,y,TBGRAPixel({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(AColor)));
end;

function bgra_GetPixel(id: Integer; x, y: integer): TBGRAColor;
begin
  if Assigned(BitmapArray[id]) then
    result := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(TBGRAColor(BitmapArray[id].GetPixel(x,y)))
  else
    result := 0;
end;

procedure bgra_CreateFromFile(id: Integer; AFilename: string);
begin
  bgra_AddBitmap(id);
  BitmapArray[id] := TBGRABitmap.Create(AFilename);
end;

procedure bgra_Destroy(id: Integer);
begin
  FreeAndNil(BitmapArray[id]);
end;

initialization
  bgra_Initialization;

finalization
  bgra_Finalization;


end.

