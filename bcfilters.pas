unit bcfilters;

{
// all pixels //
var
  i: integer;
  p: PBGRAPixel;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels-1 downto 0 do
  begin
    p^.red := ;
    p^.green := ;
    p^.blue := ;
    p^.alpha := ;
    Inc(p);
  end;

// scan line //
var
  x, y: integer;
  p: PBGRAPixel;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      p^.red := ;
      p^.green := ;
      p^.blue := ;
      p^.alpha := ;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Math, BGRABitmap, BGRABitmapTypes;

type
  TBCSimpleFilter = (bcsNone, bcsGameBoyDithering, bcsBlackAndWhiteDithering, bcsInvert,
    bcsGrayScale, bcsGrayScaleA,
    bcsGrayScaleBGRA, bcsGameBoy, bcsNoise,
    bcsNoiseA, bcsNoiseBW, bcsNoiseBWA, bcsTVScanLinesH, bcsTVScanLinesV,
    bcsCheckeredL, bcsCheckeredR, bcsBlackAndWhite, bcsInstagram1,
    bcsInstagram2, bcsInstagram3, bcsInstagram4, bcsInstagram5, bcsInstagram6,
    bcsPhotoNoise, bcsPolaroid, bcsMovement, bcsRBG, bcsGRB, bcsGBR,
    bcsBRG, bcsBGR, bcsRRG, bcsRGR, bcsGRR, bcsRRB, bcsRBR, bcsBRR,
    bcsGGR, bcsGRG, bcsRGG, bcsGGB, bcsGBG, bcsBGG, bcsBBR, bcsBRB,
    bcsRBB, bcsBBG, bcsBGB, bcsGBB, bcsRRR, bcsGGG, bcsBBB);

const
  BCSimpleFilterStr: array [TBCSimpleFilter] of string =
    ('None', 'GameBoyDithering', 'BlackAndWhiteDithering', 'Invert', 'GrayScale',
    'GrayScaleA', 'GrayScaleBGRA', 'GameBoy',
    'Noise', 'NoiseA', 'NoiseBW', 'NoiseBWA', 'TVScanLinesH', 'TVScanLinesV',
    'CheckeredL', 'CheckeredR', 'BlackAndWhite', 'Instagram1', 'Instagram2',
    'Instagram3', 'Instagram4', 'Instagram5', 'Instagram6', 'PhotoNoise',
    'Polaroid', 'Movement', 'RBG', 'GRB', 'GBR', 'BRG', 'BGR', 'RRG',
    'RGR', 'GRR', 'RRB', 'RBR', 'BRR', 'GGR', 'GRG', 'RGG', 'GGB', 'GBG',
    'BGG', 'BBR', 'BRB', 'RBB', 'BBG', 'BGB', 'GBB', 'RRR', 'GGG', 'BBB');

function StrToTBCSimpleFilter(const s: ansistring): TBCSimpleFilter;
procedure BCSimpleFilterStrList(s: TStrings);

procedure FilterRGB(Bitmap: TBGRABitmap; R, G, B: byte);

procedure RBG(Bitmap: TBGRABitmap);
procedure GRB(Bitmap: TBGRABitmap);
procedure GBR(Bitmap: TBGRABitmap);
procedure BRG(Bitmap: TBGRABitmap);
procedure BGR(Bitmap: TBGRABitmap);
procedure RRG(Bitmap: TBGRABitmap);
procedure RGR(Bitmap: TBGRABitmap);
procedure GRR(Bitmap: TBGRABitmap);
procedure RRB(Bitmap: TBGRABitmap);
procedure RBR(Bitmap: TBGRABitmap);
procedure BRR(Bitmap: TBGRABitmap);
procedure GGR(Bitmap: TBGRABitmap);
procedure GRG(Bitmap: TBGRABitmap);
procedure RGG(Bitmap: TBGRABitmap);
procedure GGB(Bitmap: TBGRABitmap);
procedure GBG(Bitmap: TBGRABitmap);
procedure BGG(Bitmap: TBGRABitmap);
procedure BBR(Bitmap: TBGRABitmap);
procedure BRB(Bitmap: TBGRABitmap);
procedure RBB(Bitmap: TBGRABitmap);
procedure BBG(Bitmap: TBGRABitmap);
procedure BGB(Bitmap: TBGRABitmap);
procedure GBB(Bitmap: TBGRABitmap);
procedure RRR(Bitmap: TBGRABitmap);
procedure GGG(Bitmap: TBGRABitmap);
procedure BBB(Bitmap: TBGRABitmap);

{ Invert colors, keep alpha }
procedure Invert(Bitmap: TBGRABitmap);
{ Invert colors, advanced options }
procedure Invert(Bitmap: TBGRABitmap; touchR, touchG, touchB, touchA: boolean);

{ GrayScale, keep alpha }
procedure GrayScale(Bitmap: TBGRABitmap);
{ GrayScale, keep alpha, pallete }
procedure GrayScale(Bitmap: TBGRABitmap; pallete: byte);
{ GrayScale, alpha 255}
procedure GrayScaleA(Bitmap: TBGRABitmap);
{ GrayScale, using BGRAToGrayScale }
procedure GrayScaleBGRA(Bitmap: TBGRABitmap);

{ like GameBoy}
procedure GameBoy(Bitmap: TBGRABitmap);

{ Dithering }
procedure GameBoyDithering(Bitmap: TBGRABitmap);
procedure BlackAndWhiteDithering(Bitmap: TBGRABitmap);

{ Noise random color, keep alpha }
procedure Noise(Bitmap: TBGRABitmap);
{ Noise random color, advanced options }
procedure Noise(Bitmap: TBGRABitmap; touchR, touchG, touchB, touchA: boolean);
{ Noise random color, random alpha }
procedure NoiseA(Bitmap: TBGRABitmap);

{ Noise random color, set max posible values }
procedure NoiseMax(Bitmap: TBGRABitmap; maxR, maxG, maxB, maxA: byte);
{ Noise random color, set max posible values, advanced options }
procedure NoiseMax(Bitmap: TBGRABitmap; maxR, maxG, maxB, maxA: byte;
  touchR, touchG, touchB, touchA: boolean);

{ Noise black and white, keep alpha }
procedure NoiseBW(Bitmap: TBGRABitmap);
{ Noise black and white, random alpha }
procedure NoiseBWA(Bitmap: TBGRABitmap);

{ TV Lines Horizontal }
procedure TVScanLinesH(Bitmap: TBGRABitmap);
{ TV Lines Vertical }
procedure TVScanLinesV(Bitmap: TBGRABitmap);
{ Checkered Left aligned }
procedure CheckeredL(Bitmap: TBGRABitmap);
{ Checkered Right aligned }
procedure CheckeredR(Bitmap: TBGRABitmap);

{ Black and White, middle 128 }
procedure BlackAndWhite(Bitmap: TBGRABitmap);
{ Black and White, custom middle }
procedure BlackAndWhite(Bitmap: TBGRABitmap; middle: byte);

{ Instagram Filters }
// sepia
procedure Instagram1(Bitmap: TBGRABitmap);
// blue-green
procedure Instagram2(Bitmap: TBGRABitmap);
// purple
procedure Instagram3(Bitmap: TBGRABitmap);
// blue 3 channels
procedure Instagram4(Bitmap: TBGRABitmap);
// green 3 channels
procedure Instagram5(Bitmap: TBGRABitmap);
// red 3 channels
procedure Instagram6(Bitmap: TBGRABitmap);
// white rounded border
procedure Polaroid(Bitmap: TBGRABitmap);
// blured bw noise
procedure PhotoNoise(Bitmap: TBGRABitmap);

{ Pixel movement }
procedure Movement(Bitmap: TBGRABitmap; randXmin: NativeInt = -5;
  randXmax: NativeInt = 5; randYmin: NativeInt = -5; randYmax: NativeInt = 5);

procedure Zoomy(Bitmap: TBGRABitmap; xMy, yMy: extended);

{ Filters that only need Bitmap as parameter }
procedure SimpleFilter(Bitmap: TBGRABitmap; Filter: TBCSimpleFilter);

implementation

function StrToTBCSimpleFilter(const s: ansistring): TBCSimpleFilter;
var
  sf: TBCSimpleFilter;
  ls: ansistring;
begin
  Result := sf;
  ls := UTF8LowerCase(s);
  for sf := low(TBCSimpleFilter) to high(TBCSimpleFilter) do
    if ls = UTF8LowerCase(BCSimpleFilterStr[sf]) then
    begin
      Result := sf;
      break;
    end;
end;

procedure BCSimpleFilterStrList(s: TStrings);
var
  sf: TBCSimpleFilter;
begin
  for sf := low(TBCSimpleFilter) to high(TBCSimpleFilter) do
    s.Add(BCSimpleFilterStr[sf]);
end;

procedure Invert(Bitmap: TBGRABitmap);
var
  i: integer;
  p: PBGRAPixel;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    p^.red := not p^.red;
    p^.green := not p^.green;
    p^.blue := not p^.blue;
    //p^.alpha := not p^.alpha;
    Inc(p);
  end;
end;

procedure Invert(Bitmap: TBGRABitmap; touchR, touchG, touchB, touchA: boolean);
var
  i: integer;
  p: PBGRAPixel;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    if touchR then
      p^.red := not p^.red;
    if touchG then
      p^.green := not p^.green;
    if touchB then
      p^.blue := not p^.blue;
    if touchA then
      p^.alpha := not p^.alpha;
    Inc(p);
  end;
end;

procedure GrayScale(Bitmap: TBGRABitmap);
var
  i: integer;
  p: PBGRAPixel;
  c: byte;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    c := (p^.red + p^.green + p^.blue) div 3;
    p^.red := c;
    p^.green := c;
    p^.blue := c;
    //p^.alpha := 255;
    Inc(p);
  end;
end;

procedure GrayScale(Bitmap: TBGRABitmap; pallete: byte);
var
  i, j: integer;
  p: PBGRAPixel;
  c: byte;
  gpallete: array of byte;
begin
  if pallete = 0 then
    pallete := 1
  else if pallete = 255 then
  begin
    GrayScale(Bitmap);
    exit;
  end;

  SetLength(gpallete, pallete);

  for i := 0 to High(gpallete) do
  begin
    gpallete[i] := (255 * i) div 255;
  end;

  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    c := (p^.red + p^.green + p^.blue) div 3;

    for j := 0 to High(gpallete) do
    begin
      if (c >= gpallete[j]) and (c <= gpallete[j + 1]) then
      begin
        c := gpallete[j];
        break;
      end;
    end;

    p^.red := c;
    p^.green := c;
    p^.blue := c;
    //p^.alpha := 255;
    Inc(p);
  end;
end;

procedure GrayScaleA(Bitmap: TBGRABitmap);
var
  i: integer;
  p: PBGRAPixel;
  c: byte;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    c := (p^.red + p^.green + p^.blue) div 3;
    p^.red := c;
    p^.green := c;
    p^.blue := c;
    p^.alpha := 255;
    Inc(p);
  end;
end;

procedure GrayScaleBGRA(Bitmap: TBGRABitmap);
begin
  Bitmap.InplaceGrayscale;
{var
  i: integer;
  p: PBGRAPixel;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    p^ := BGRAToGrayscale(p^);
    Inc(p);
  end;}
end;

procedure GameBoy(Bitmap: TBGRABitmap);
var
  i: integer;
  p: PBGRAPixel;
  c: integer;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    {c := (p^.red + p^.green + p^.blue) div 3;
    case c of
      0..63: p^ := BGRA(0,80,32,255);
      64..127: p^ := BGRA(0,104,24,255);
      128..191: p^ := BGRA(0,176,0,255);
      192..255: p^ := BGRA(112,224,48,255);
    end;}

    c := p^.red + p^.green + p^.blue;

    if c <= 382 then
    begin
      if c <= 191 then
        p^ := BGRA(0, 80, 32, 255)
      else
        p^ := BGRA(0, 104, 24, 255);
    end
    else
    begin
      if c <= 573 then
        p^ := BGRA(0, 176, 0, 255)
      else
        p^ := BGRA(112, 224, 48, 255);
    end;

    Inc(p);
  end;
end;

procedure GameBoyDithering(Bitmap: TBGRABitmap);

  function find_closest_palette_color(cl: TBGRAPixel): TBGRAPixel;
  var
    c: integer;
  begin
        c := cl.red + cl.green + cl.blue;

    if c <= 382 then
    begin
      if c <= 191 then
        result := BGRA(0, 80, 32, 255)
      else
        result := BGRA(0, 104, 24, 255);
    end
    else
    begin
      if c <= 573 then
        result := BGRA(0, 176, 0, 255)
      else
        result := BGRA(112, 224, 48, 255);
    end;
  end;

  function multiply_divide(pixel,sum: TBGRAPixel;mult,divi: integer):TBGRAPixel;
  begin
    result.red := round(pixel.red + sum.red * mult / divi);
    result.green := round(pixel.green + sum.green * mult / divi);
    result.blue := round(pixel.blue + sum.blue * mult / divi);
  end;

var
  x, y: integer;
  oldpixel, newpixel, quant_error: TBGRAPixel;
begin
  for y := 0 to Bitmap.Height do
  begin
    for x := 0 to Bitmap.Width do
    begin
      oldpixel := Bitmap.GetPixel(x,y);
      newpixel := find_closest_palette_color(oldpixel);
      Bitmap.SetPixel(x,y,newpixel);

      quant_error.red := oldpixel.red - newpixel.red;
      quant_error.green := oldpixel.green - newpixel.green;
      quant_error.blue := oldpixel.blue - newpixel.blue;

      Bitmap.SetPixel(x + 1, y,multiply_divide(Bitmap.GetPixel(x + 1, y),quant_error,7,16));
      Bitmap.SetPixel(x - 1, y + 1,multiply_divide(Bitmap.GetPixel(x - 1, y + 1),quant_error,3,16));
      Bitmap.SetPixel(x, y + 1,multiply_divide(Bitmap.GetPixel(x, y + 1),quant_error,5,16));
      Bitmap.SetPixel(x + 1, y + 1,multiply_divide(Bitmap.GetPixel(x + 1, y + 1),quant_error,1,16));
    end;
  end;
end;

procedure BlackAndWhiteDithering(Bitmap: TBGRABitmap);
    function find_closest_palette_color(cl: TBGRAPixel): TBGRAPixel;
    var
      c: integer;
    begin
      c := cl.red + cl.green + cl.blue;

      if c <= 127 then
        result := BGRABlack
      else
        result := BGRAWhite;
    end;

    function multiply_divide(pixel,sum: TBGRAPixel;mult,divi: integer):TBGRAPixel;
    begin
      result.red := round(pixel.red + sum.red * mult / divi);
      result.green := round(pixel.green + sum.green * mult / divi);
      result.blue := round(pixel.blue + sum.blue * mult / divi);
    end;

  var
    x, y: integer;
    oldpixel, newpixel, quant_error: TBGRAPixel;
  begin
    for y := 0 to Bitmap.Height do
    begin
      for x := 0 to Bitmap.Width do
      begin
        oldpixel := Bitmap.GetPixel(x,y);
        newpixel := find_closest_palette_color(oldpixel);
        Bitmap.SetPixel(x,y,newpixel);

        quant_error.red := oldpixel.red - newpixel.red;
        quant_error.green := oldpixel.green - newpixel.green;
        quant_error.blue := oldpixel.blue - newpixel.blue;

        Bitmap.SetPixel(x + 1, y,multiply_divide(Bitmap.GetPixel(x + 1, y),quant_error,7,16));
        Bitmap.SetPixel(x - 1, y + 1,multiply_divide(Bitmap.GetPixel(x - 1, y + 1),quant_error,3,16));
        Bitmap.SetPixel(x, y + 1,multiply_divide(Bitmap.GetPixel(x, y + 1),quant_error,5,16));
        Bitmap.SetPixel(x + 1, y + 1,multiply_divide(Bitmap.GetPixel(x + 1, y + 1),quant_error,1,16));
      end;
    end;
  end;

procedure Noise(Bitmap: TBGRABitmap);
var
  i: integer;
  p: PBGRAPixel;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    p^.red := Random(256);
    p^.green := Random(256);
    p^.blue := Random(256);
    //p^.alpha := Random(256);
    Inc(p);
  end;
end;

procedure Noise(Bitmap: TBGRABitmap; touchR, touchG, touchB, touchA: boolean);
var
  i: integer;
  p: PBGRAPixel;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    if touchR then
      p^.red := Random(256);
    if touchG then
      p^.green := Random(256);
    if touchB then
      p^.blue := Random(256);
    if touchA then
      p^.alpha := Random(256);
    Inc(p);
  end;
end;

procedure NoiseA(Bitmap: TBGRABitmap);
var
  i: integer;
  p: PBGRAPixel;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    p^.red := Random(256);
    p^.green := Random(256);
    p^.blue := Random(256);
    p^.alpha := Random(256);
    Inc(p);
  end;
end;

procedure NoiseBW(Bitmap: TBGRABitmap);
var
  i: integer;
  p: PBGRAPixel;
  c: byte;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    c := Random(2);
    p^.red := c + 255;
    p^.green := c + 255;
    p^.blue := c + 255;
    //p^.alpha := Random(256);
    Inc(p);
  end;
end;

procedure NoiseBWA(Bitmap: TBGRABitmap);
var
  i: integer;
  p: PBGRAPixel;
  c: byte;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    c := Random(2);
    p^.red := c + 255;
    p^.green := c + 255;
    p^.blue := c + 255;
    p^.alpha := Random(256);
    Inc(p);
  end;
end;

procedure TVScanLinesH(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      if Odd(y) then
      begin
        p^.red := 0;
        p^.green := 0;
        p^.blue := 0;
        //p^.alpha := 255;
      end;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure TVScanLinesV(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      if Odd(x) then
      begin
        p^.red := 0;
        p^.green := 0;
        p^.blue := 0;
        //p^.alpha := 255;
      end;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure CheckeredL(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      if Odd(y) and Odd(x) or not Odd(y) and not Odd(x) then
      begin
        p^.red := 0;
        p^.green := 0;
        p^.blue := 0;
        p^.alpha := 255;
      end;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure CheckeredR(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      if not Odd(y) and Odd(x) or Odd(y) and not Odd(x) then
      begin
        p^.red := 0;
        p^.green := 0;
        p^.blue := 0;
        p^.alpha := 255;
      end;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure BlackAndWhite(Bitmap: TBGRABitmap);
var
  i: integer;
  p: PBGRAPixel;
  c: byte;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    c := (p^.red + p^.green + p^.blue) div 3;
    if c >= 128 then
      c := 255
    else
      c := 0;
    p^.red := c;
    p^.green := c;
    p^.blue := c;
    if p^.alpha > 0 then
      p^.alpha := 255;
    Inc(p);
  end;
end;

procedure BlackAndWhite(Bitmap: TBGRABitmap; middle: byte);
var
  i: integer;
  p: PBGRAPixel;
  c: byte;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    c := (p^.red + p^.green + p^.blue) div 3;
    if c >= middle then
      c := 255
    else
      c := 0;
    p^.red := c;
    p^.green := c;
    p^.blue := c;
    if p^.alpha > 0 then
      p^.alpha := 255;
    Inc(p);
  end;
end;

procedure Movement(Bitmap: TBGRABitmap; randXmin: NativeInt = -5;
  randXmax: NativeInt = 5; randYmin: NativeInt = -5; randYmax: NativeInt = 5);
var
  x, y: integer;
  p: PBGRAPixel;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      p^ := Bitmap.GetPixel(x + RandomRange(randXmin, randXmax), y +
        RandomRange(randYmin, randYmax));
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure Zoomy(Bitmap: TBGRABitmap; xMy, yMy: extended);
var
  x, y: integer;
  p: PBGRAPixel;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      p^{.red} := Bitmap.GetPixel(x * xMy, y * yMy);
      {p^.green := 0;
      p^.blue := 0;
      p^.alpha := 255;}
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure SimpleFilter(Bitmap: TBGRABitmap; Filter: TBCSimpleFilter);
begin
  case Filter of
    bcsGameBoyDithering: GameBoyDithering(Bitmap);
    bcsBlackAndWhiteDithering: BlackAndWhiteDithering(Bitmap);
    bcsInvert: Invert(Bitmap);
    bcsGrayScale: GrayScale(Bitmap);
    bcsGrayScaleA: GrayScaleA(Bitmap);
    bcsGrayScaleBGRA: GrayScaleBGRA(Bitmap);
    bcsGameBoy: GameBoy(Bitmap);
    bcsNoise: Noise(Bitmap);
    bcsNoiseA: NoiseA(Bitmap);
    bcsNoiseBW: NoiseBW(Bitmap);
    bcsNoiseBWA: NoiseBWA(Bitmap);
    bcsTVScanLinesH: TVScanLinesH(Bitmap);
    bcsTVScanLinesV: TVScanLinesV(Bitmap);
    bcsCheckeredL: CheckeredL(Bitmap);
    bcsCheckeredR: CheckeredR(Bitmap);
    bcsBlackAndWhite: BlackAndWhite(Bitmap);
    bcsInstagram1: Instagram1(Bitmap);
    bcsInstagram2: Instagram2(Bitmap);
    bcsInstagram3: Instagram3(Bitmap);
    bcsInstagram4: Instagram4(Bitmap);
    bcsInstagram5: Instagram5(Bitmap);
    bcsInstagram6: Instagram6(Bitmap);
    bcsPhotoNoise: PhotoNoise(Bitmap);
    bcsPolaroid: Polaroid(Bitmap);
    bcsMovement: Movement(Bitmap);
    bcsRBG: RBG(Bitmap);
    bcsGRB: GRB(Bitmap);
    bcsGBR: GBR(Bitmap);
    bcsBRG: BRG(Bitmap);
    bcsBGR: BGR(Bitmap);
    bcsRRG: RRG(Bitmap);
    bcsRGR: RGR(Bitmap);
    bcsGRR: GRR(Bitmap);
    bcsRRB: RRB(Bitmap);
    bcsRBR: RBR(Bitmap);
    bcsBRR: BRR(Bitmap);
    bcsGGR: GGR(Bitmap);
    bcsGRG: GRG(Bitmap);
    bcsRGG: RGG(Bitmap);
    bcsGGB: GGB(Bitmap);
    bcsGBG: GBG(Bitmap);
    bcsBGG: BGG(Bitmap);
    bcsBBR: BBR(Bitmap);
    bcsBRB: BRB(Bitmap);
    bcsRBB: RBB(Bitmap);
    bcsBBG: BBG(Bitmap);
    bcsBGB: BGB(Bitmap);
    bcsGBB: GBB(Bitmap);
    bcsRRR: RRR(Bitmap);
    bcsGGG: GGG(Bitmap);
    bcsBBB: BBB(Bitmap);
  end;
end;

procedure NoiseMax(Bitmap: TBGRABitmap; maxR, maxG, maxB, maxA: byte);
var
  i: integer;
  p: PBGRAPixel;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    p^.red := Random(maxR + 1);
    p^.green := Random(maxG + 1);
    p^.blue := Random(maxB + 1);
    p^.alpha := Random(maxA + 1);
    Inc(p);
  end;
end;

procedure NoiseMax(Bitmap: TBGRABitmap; maxR, maxG, maxB, maxA: byte;
  touchR, touchG, touchB, touchA: boolean);
var
  i: integer;
  p: PBGRAPixel;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    if touchR then
      p^.red := Random(maxR + 1);
    if touchG then
      p^.green := Random(maxG + 1);
    if touchB then
      p^.blue := Random(maxB + 1);
    if touchA then
      p^.alpha := Random(maxA + 1);
    Inc(p);
  end;
end;

// 1
procedure Instagram1(Bitmap: TBGRABitmap);
var
  i: integer;
  p: PBGRAPixel;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    p^.red := round(p^.red * 0.75);
    p^.green := round(p^.red * 0.50);
    p^.blue := round(p^.red * 0.25);
    //p^.alpha := ;
    Inc(p);
  end;
end;

// 2
procedure Instagram2(Bitmap: TBGRABitmap);
var
  i: integer;
  p: PBGRAPixel;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    p^.red := round(p^.red * 0.75);
    p^.green := round(p^.green * 0.50);
    p^.blue := round(p^.blue * 0.25);
    //p^.alpha := ;
    Inc(p);
  end;
end;

// 3
procedure Instagram3(Bitmap: TBGRABitmap);
var
  i: integer;
  p: PBGRAPixel;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    p^.red := p^.red;
    p^.green := round(p^.green * 0.50);
    p^.blue := round(p^.blue * 0.50);
    //p^.alpha := ;
    Inc(p);
  end;
end;

// 4
procedure Instagram4(Bitmap: TBGRABitmap);
begin
  BBB(Bitmap);
end;

// 5
procedure Instagram5(Bitmap: TBGRABitmap);
begin
  GGG(Bitmap);
end;

// 6
procedure Instagram6(Bitmap: TBGRABitmap);
begin
  RRR(Bitmap);
end;

procedure Polaroid(Bitmap: TBGRABitmap);
var
  tmp: TBGRABitmap;
begin
  tmp := TBGRABitmap.Create(Bitmap.Width, Bitmap.Height, BGRAWhite);
  tmp.EraseRoundRectAntialias(
    Round(Bitmap.Width * 0.05),
    Round(Bitmap.Height * 0.05),
    Bitmap.Width - Round(Bitmap.Width * 0.05),
    Bitmap.Height - Round(Bitmap.Height * 0.05),
    Round(Bitmap.Width * 0.05),
    Round(Bitmap.Height * 0.05),
    255, []);
  Bitmap.BlendImage(0, 0, tmp, boLinearBlend);
  tmp.Free;
end;

procedure PhotoNoise(Bitmap: TBGRABitmap);
var
  tmp: TBGRABitmap;
begin
  tmp := TBGRABitmap.Create(Bitmap.Width, Bitmap.Height);
  NoiseBWA(tmp);
  BGRAReplace(tmp, tmp.FilterBlurRadial(1, rbFast));
  Bitmap.BlendImageOver(0, 0, tmp, boLinearBlend, 25);
  tmp.Free;
end;

{Change colors}

procedure FilterRGB(Bitmap: TBGRABitmap; R, G, B: byte);
var
  x, y: integer;
  p: PBGRAPixel;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      p^.red := round(p^.red * (R / 100));
      p^.green := round(p^.green * (G / 100));
      p^.blue := round(p^.blue * (B / 100));
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure RBG(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  r, g, b: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      r := p^.red;
      g := p^.green;
      b := p^.blue;
      p^.red := r;
      p^.green := b;
      p^.blue := g;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure GRB(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  r, g, b: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      r := p^.red;
      g := p^.green;
      b := p^.blue;
      p^.red := g;
      p^.green := r;
      p^.blue := b;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure GBR(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  r, g, b: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      r := p^.red;
      g := p^.green;
      b := p^.blue;
      p^.red := g;
      p^.green := b;
      p^.blue := r;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure BRG(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  r, g, b: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      r := p^.red;
      g := p^.green;
      b := p^.blue;
      p^.red := b;
      p^.green := r;
      p^.blue := g;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure BGR(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  r, g, b: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      r := p^.red;
      g := p^.green;
      b := p^.blue;
      p^.red := b;
      p^.green := g;
      p^.blue := r;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure RRG(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  r, g: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      r := p^.red;
      g := p^.green;
      p^.red := r;
      p^.green := r;
      p^.blue := g;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure RGR(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  r, g: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      r := p^.red;
      g := p^.green;
      p^.red := r;
      p^.green := g;
      p^.blue := r;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure GRR(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  r, g: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      r := p^.red;
      g := p^.green;
      p^.red := g;
      p^.green := r;
      p^.blue := r;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure RRB(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  r, b: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      r := p^.red;
      b := p^.blue;
      p^.red := r;
      p^.green := r;
      p^.blue := b;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure RBR(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  r, b: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      r := p^.red;
      b := p^.blue;
      p^.red := r;
      p^.green := b;
      p^.blue := r;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure BRR(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  r, b: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      r := p^.red;
      b := p^.blue;
      p^.red := b;
      p^.green := r;
      p^.blue := r;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure GGR(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  r, g: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      r := p^.red;
      g := p^.green;
      p^.red := g;
      p^.green := g;
      p^.blue := r;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure GRG(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  r, g: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      r := p^.red;
      g := p^.green;
      p^.red := g;
      p^.green := r;
      p^.blue := g;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure RGG(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  r, g: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      r := p^.red;
      g := p^.green;
      p^.red := r;
      p^.green := g;
      p^.blue := g;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure GGB(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  g, b: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      g := p^.green;
      b := p^.blue;
      p^.red := g;
      p^.green := g;
      p^.blue := b;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure GBG(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  g, b: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      g := p^.green;
      b := p^.blue;
      p^.red := g;
      p^.green := b;
      p^.blue := g;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure BGG(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  g, b: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      g := p^.green;
      b := p^.blue;
      p^.red := b;
      p^.green := g;
      p^.blue := g;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure BBR(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  r, b: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      r := p^.red;
      b := p^.blue;
      p^.red := b;
      p^.green := b;
      p^.blue := r;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure BRB(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  r, b: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      r := p^.red;
      b := p^.blue;
      p^.red := b;
      p^.green := r;
      p^.blue := b;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure RBB(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  r, b: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      r := p^.red;
      b := p^.blue;
      p^.red := r;
      p^.green := b;
      p^.blue := b;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure BBG(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  g, b: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      g := p^.green;
      b := p^.blue;
      p^.red := b;
      p^.green := b;
      p^.blue := g;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure BGB(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  g, b: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      g := p^.green;
      b := p^.blue;
      p^.red := b;
      p^.green := g;
      p^.blue := b;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure GBB(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
  g, b: byte;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      g := p^.green;
      b := p^.blue;
      p^.red := g;
      p^.green := b;
      p^.blue := b;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure RRR(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      p^.green := p^.red;
      p^.blue := p^.red;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure GGG(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      p^.red := p^.green;
      p^.blue := p^.green;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

procedure BBB(Bitmap: TBGRABitmap);
var
  x, y: integer;
  p: PBGRAPixel;
begin
  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      p^.red := p^.blue;
      p^.green := p^.blue;
      Inc(p);
    end;
  end;
  Bitmap.InvalidateBitmap;
end;

end.
