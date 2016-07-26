library invert;

{$mode objfpc}{$H+}

uses
  Classes, strings, BGRABitmap, BGRABitmapTypes;

procedure FilterName(s: pchar); cdecl;
begin
  StrCopy(s, 'Invert');
end;

procedure ApplyFilter(BGRA: TBGRABitmap); cdecl;
var
  i: integer;
  p: PBGRAPixel;
begin
  p := BGRA.Data;

  for i := BGRA.NBPixels - 1 downto 0 do
  begin
    p^.red := not p^.red;
    p^.green := not p^.green;
    p^.blue := not p^.blue;
    p^.alpha := p^.alpha;
    Inc(p);
  end;
end;

exports
  FilterName,
  ApplyFilter;

begin
end.

