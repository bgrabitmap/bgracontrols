unit BCBrightAndContrast;

{ Unit contributed by esvignolo }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLType;

function Bright(aColor: TColor; BrightPercent: byte): TColor;
function GetContrastColor(ABGColor: TColor): TColor;

implementation

function Bright(aColor: TColor; BrightPercent: byte): TColor;
var
  r, g, b: byte;
begin
  aColor := ColorToRGB(aColor);
  r := Red(aColor);
  g := Green(aColor);
  b := Blue(aColor);
  {muldiv (255-r, BrightPercent, 100); - color value in percentage,
  By which it is necessary to increase initial color (integer)}
  r := r + muldiv(255 - r, BrightPercent, 100);
  g := g + muldiv(255 - g, BrightPercent, 100);
  b := b + muldiv(255 - b, BrightPercent, 100);
  Result := RGBToColor(r, g, b);
end;

function GetContrastColor(ABGColor: TColor): TColor;
var
  ADouble: double;
  R, G, B: byte;
begin
  if ABGColor <= 0 then
  begin
    Result := clWhite;
    Exit; // *** EXIT RIGHT HERE ***
  end;

  if ABGColor = clWhite then
  begin
    Result := clBlack;
    Exit; // *** EXIT RIGHT HERE ***
  end;

  // Get RGB from Color
  R := Red(ABGColor);
  G := Green(ABGColor);
  B := Blue(ABGColor);

  // Counting the perceptive luminance - human eye favors green color...
  ADouble := 1 - (0.299 * R + 0.587 * G + 0.114 * B) / 255;

  if (ADouble < 0.5) then
    Result := clBlack  // bright colors - black font
  else
    Result := clWhite;  // dark colors - white font
end;

end.

