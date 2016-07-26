library grayscale;

{$mode objfpc}{$H+}

uses
  Classes, strings, BGRABitmap, BGRABitmapTypes;

procedure FilterName(s: pchar); cdecl;
begin
  StrCopy(s, 'Grayscale');
end;

procedure ApplyFilter(BGRA: TBGRABitmap); cdecl;
begin
  BGRA.InplaceGrayscale();
end;

exports
  FilterName,
  ApplyFilter;

begin
end.

