unit BGRAPascalScript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes;

var
  BitmapArray: array of TBGRABitmap;

{Internal use only}
procedure bgra_Initialization;
procedure bgra_Finalization;
procedure bgra_AddBitmap(id: Integer);

{Constructors}
// Note: overloaded procedures not supported, use unique identifiers
procedure bgra_Create(id: Integer);
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
  begin
    if BitmapArray[i] <> nil then
      BitmapArray[i].Free;
    BitmapArray[i] := nil;
  end;
  BitmapArray := nil;
end;

procedure bgra_AddBitmap(id: Integer);
begin
  if id + 1 > length(BitmapArray) then
    SetLength(BitmapArray, id + 1);
  if BitmapArray[id] <> nil then
    BitmapArray[id].Free;
end;

procedure bgra_Create(id: Integer);
begin
  bgra_AddBitmap(id);
  BitmapArray[id] := TBGRABitmap.Create;
end;

procedure bgra_CreateFromFile(id: Integer; AFilename: string);
begin
  bgra_AddBitmap(id);
  BitmapArray[id] := TBGRABitmap.Create(AFilename);
end;

procedure bgra_Destroy(id: Integer);
begin
  BitmapArray[id].Free;
end;

initialization
  bgra_Initialization;

finalization
  bgra_Finalization;


end.

