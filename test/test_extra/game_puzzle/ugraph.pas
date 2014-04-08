unit ugraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, bgrabitmap, bgrabitmaptypes, Graphics;

const
  FrameDashLength = 4;

function RectUnion(const rect1, Rect2: TRect): TRect;
function RectOfs(const ARect: TRect; ofsX, ofsY: integer): TRect;
function GetShapeBounds(const pts: array of TPointF; Width: single): TRect;
procedure DrawCheckers(bmp: TBGRABitmap);
procedure DrawGrid(bmp: TBGRABitmap; sizex, sizey: single);
function ComputeAngle(dx, dy: single): single;
function GetSelectionCenter(bmp: TBGRABitmap): TPointF;
procedure ComputeSelectionMask(image: TBGRABitmap; destMask: TBGRABitmap);
procedure SubstractMask(image: TBGRABitmap; mask: TBGRABitmap);
procedure NicePoint(bmp: TBGRABitmap; x, y: single); overload;
procedure NicePoint(bmp: TBGRABitmap; ptF: TPointF); overload;
procedure NiceLine(bmp: TBGRABitmap; x1, y1, x2, y2: single);
function ComputeColorCircle(tx, ty: integer; light: word;
  hueCorrection: boolean = True): TBGRABitmap;
function ChangeCanvasSize(bmp: TBGRABitmap; newWidth, newHeight: integer;
  anchor: string; background: TBGRAPixel; repeatImage: boolean;
  flipMode: boolean = False): TBGRABitmap; overload;

procedure RenderCloudsOn(bmp: TBGRABitmap; color: TBGRAPixel);
procedure RenderWaterOn(bmp: TBGRABitmap; waterColor, skyColor: TBGRAPixel);

function CreateMetalFloorTexture(tx: integer): TBGRABitmap;
function CreatePlastikTexture(tx, ty: integer): TBGRABitmap;
function CreateCamouflageTexture(tx, ty: integer): TBGRABitmap;
function CreateSnowPrintTexture(tx, ty: integer): TBGRABitmap;
function CreateRoundStoneTexture(tx, ty: integer): TBGRABitmap;
function CreateStoneTexture(tx, ty: integer): TBGRABitmap;
function CreateWaterTexture(tx, ty: integer): TBGRABitmap;
function CreateMarbleTexture(tx, ty: integer): TBGRABitmap;
function CreateWoodTexture(tx, ty: integer): TBGRABitmap;
function CreateVerticalWoodTexture(tx, ty: integer): TBGRABitmap;

function ClearTypeFilter(Source: TBGRACustomBitmap): TBGRACustomBitmap;
function ClearTypeInverseFilter(Source: TBGRACustomBitmap): TBGRACustomBitmap;

function DoResample(Source: TBGRABitmap; newWidth, newHeight: integer;
  StretchMode: TResampleMode): TBGRABitmap;

implementation

uses Math, Types, LCLProc, BGRAGradients;

function RectUnion(const rect1, Rect2: TRect): TRect;
begin
  if IsRectEmpty(rect1) then
  begin
    if IsRectEmpty(rect2) then
      Result := EmptyRect
    else
      Result := rect2;
  end
  else
  begin
    Result := rect1;
    if not IsRectEmpty(rect2) then
      UnionRect(Result, Result, rect2);
  end;
end;

function RectOfs(const ARect: TRect; ofsX, ofsY: integer): TRect;
begin
  Result := ARect;
  OffsetRect(Result, ofsX, ofsY);
end;

function GetShapeBounds(const pts: array of TPointF; Width: single): TRect;
var
  ix, iy, i: integer;
begin
  Width /= 2;
  Result.Left := high(integer);
  Result.Top := high(integer);
  Result.Right := low(integer);
  Result.Bottom := low(integer);
  for i := 0 to high(pts) do
  begin
    ix := floor(pts[i].x - Width);
    iy := floor(pts[i].y - Width);
    if ix < Result.left then
      Result.left := ix;
    if iy < Result.Top then
      Result.top := iy;
    ix := ceil(pts[i].x + Width) + 2;
    iy := ceil(pts[i].y + Width) + 2;
    if ix > Result.right then
      Result.right := ix;
    if iy > Result.bottom then
      Result.bottom := iy;
  end;
  if (Result.right <= Result.left) or (Result.bottom <= Result.top) then
    Result := EmptyRect;
end;

procedure DrawCheckers(bmp: TBGRABitmap);
const
  tx = 8;
  ty = 8;
var
  xb, yb, x, y: integer;
  oddColor, evenColor: TBGRAPixel;
begin
  oddColor := BGRA(220, 220, 220);
  evenColor := BGRA(255, 255, 255);
  y := 0;
  for yb := 0 to (bmp.Height - 1) div ty do
  begin
    x := 0;
    for xb := 0 to (bmp.Width - 1) div tx do
    begin
      if odd(xb + yb) then
        bmp.FillRect(x, y, x + tx, y + ty, oddColor, dmSet)
      else
        bmp.FillRect(x, y, x + tx, y + ty, evenColor, dmSet);
      Inc(x, tx);
    end;
    Inc(y, ty);
  end;
end;

procedure DrawGrid(bmp: TBGRABitmap; sizex, sizey: single);
var
  xb, yb: integer;
  imgGrid: TBGRABitmap;
  alpha: byte;
begin
  imgGrid := TBGRABitmap.Create(bmp.Width, 1);
  alpha := min(96, round((abs(sizex) + abs(sizey)) * (96 / 16 / 2)));
  imgGrid.DrawLineAntialias(0, 0, imgGrid.Width - 1, 0, BGRA(255, 255, 255, alpha),
    BGRA(0, 0, 0, alpha),
    min(3, max(1, round(sizex / 8))), True);
  for yb := 1 to trunc(bmp.Height / sizey) do
    bmp.PutImage(0, round(yb * sizey), imgGrid, dmFastBlend);
  imgGrid.Free;

  imgGrid := TBGRABitmap.Create(1, bmp.Height);
  imgGrid.DrawLineAntialias(0, 0, 0, imgGrid.Height - 1, BGRA(0, 0, 0, alpha),
    BGRA(255, 255, 255, alpha),
    min(3, max(1, round(sizey / 8))), True);
  for xb := 1 to trunc(bmp.Width / sizex) do
    bmp.PutImage(round(xb * sizex), 0, imgGrid, dmFastBlend);
  imgGrid.Free;
end;

procedure RenderCloudsOn(bmp: TBGRABitmap; color: TBGRAPixel);
const
  minDensity = 180;
  maxDensity = 240;
var
  i, k, x, y: integer;
  fact, radius: single;
  tempBmp: TBGRABitmap;
  ptemp: PBGRAPixel;
begin
  if color.alpha = 0 then
    exit;

  tempBmp := TBGRABitmap.Create(bmp.Width, bmp.Height, BGRABlack);
  fact := (bmp.Width + bmp.Height) / 15;
  for i := 120 downto 20 do
  begin
    for k := 1 to 2 do
    begin
      radius := ((i + random(50)) / 100) * fact;
      x := random(bmp.Width);
      y := random(bmp.Height);
      tempBmp.GradientFill(floor(x - radius), floor(y - radius), ceil(
        x + radius), ceil(y + radius), BGRA(255, 255, 255, 128), BGRAPixelTransparent,
        gtRadial, pointf(x, y), pointf(x + radius + 0.5, y), dmFastBlend, False);
    end;
  end;

  ptemp := tempBmp.Data;
  for i := tempBmp.nbPixels - 1 downto 0 do
  begin
    if ptemp^.red < minDensity then
      ptemp^ := BGRAPixelTransparent
    else
    if ptemp^.red > maxDensity then
      ptemp^ := color
    else
      ptemp^ := BGRA(color.red, color.green, color.blue, color.alpha *
        (ptemp^.red - minDensity) div (maxDensity - minDensity));
    Inc(ptemp);
  end;
  bmp.PutImage(0, 0, tempBmp, dmDrawWithTransparency);
  tempBmp.Free;
end;

procedure RenderWaterOn(bmp: TBGRABitmap; waterColor, skyColor: TBGRAPixel);
var
  Noise, Temp: TBGRABitmap;
  Phong: TPhongShading;
begin
  Noise := CreateCyclicPerlinNoiseMap(bmp.Width, bmp.Height, 1, 1, 1.2);
  Temp := Noise.FilterBlurRadial(1, rbFast) as TBGRABitmap;
  Noise.Free;
  Noise := Temp;
  Noise.ApplyGlobalOpacity(waterColor.alpha);
  waterColor.alpha := 255;

  Phong := TPhongShading.Create;
  Phong.NegativeDiffusionFactor := 0.1;
  Phong.AmbientFactor := 0.7;
  Phong.LightSourceDistanceFactor := 0;
  Phong.LightDestFactor := 0;
  Phong.LightSourceIntensity := 300;
  Phong.LightPosition := Point(-500, -500);
  Phong.LightColor := skyColor;
  Phong.Draw(bmp, Noise, 30, 0, 0, waterColor);
  Noise.Free;
  Phong.Free;
end;

function Interp256(value1, value2, position: integer): integer; inline;
begin
  Result := (value1 * (256 - position) + value2 * position) shr 8;
end;

function Interp256(color1, color2: TBGRAPixel; position: integer): TBGRAPixel; inline;
begin
  Result.red := Interp256(color1.red, color2.red, position);
  Result.green := Interp256(color1.green, color2.green, position);
  Result.blue := Interp256(color1.blue, color2.blue, position);
  Result.alpha := Interp256(color1.alpha, color2.alpha, position);
end;

function CreateWoodTexture(tx, ty: integer): TBGRABitmap;
var
  colorOscillation, globalColorVariation: integer;
  p: PBGRAPixel;
  i: integer;
begin
  Result := CreateCyclicPerlinNoiseMap(tx, ty, 1.5, 1.5, 1, rfBestQuality);
  p := Result.Data;
  for i := 0 to Result.NbPixels - 1 do
  begin
    colorOscillation := round(sqrt((sin(p^.red * Pi / 16) + 1) / 2) * 256);
    globalColorVariation := p^.red;
    p^ := Interp256(Interp256(BGRA(247, 188, 120), BGRA(255, 218, 170),
      colorOscillation), Interp256(BGRA(157, 97, 60), BGRA(202, 145, 112),
      colorOscillation), globalColorVariation);
    Inc(p);
  end;
end;

function CreateVerticalWoodTexture(tx, ty: integer): TBGRABitmap;
var
  globalPos: single;
  colorOscillation, globalColorVariation: integer;
  p: PBGRAPixel;
  i: integer;
  x, nbVertical: integer;
begin
  Result := CreateCyclicPerlinNoiseMap(tx, ty, 1, 1, 1, rfBestQuality);
  p := Result.Data;
  x := 0;
  nbVertical := tx div 128;
  if nbVertical = 0 then
    nbVertical := 1;
  for i := 0 to Result.NbPixels - 1 do
  begin
    globalPos := p^.red * Pi / 32 + nbVertical * x * 2 * Pi / tx * 8;
    colorOscillation := round(sqrt((sin(globalPos) + 1) / 2) * 256);
    globalColorVariation := p^.red; //round(sin(globalPos/8)*128+128);
    p^ := Interp256(Interp256(BGRA(247, 188, 120), BGRA(255, 218, 170),
      colorOscillation), Interp256(BGRA(157, 97, 60), BGRA(202, 145, 112),
      colorOscillation), globalColorVariation);
    Inc(p);
    Inc(x);
    if x = tx then
      x := 0;
  end;
end;

function ClearTypeFilter(Source: TBGRACustomBitmap): TBGRACustomBitmap;
var
  mul3, temp: TBGRACustomBitmap;
  xb, yb: integer;
  pmul3, pdest: PBGRAPixel;
  a: byte;
begin
  Source.ResampleFilter := rfSpline;
  mul3 := Source.Resample(Source.Width * 3 - 2, Source.Height);
  temp := Source.NewBitmap(Source.Width * 3, Source.Height);
  temp.PutImage(1, 0, mul3, dmSet);
  for yb := 0 to temp.Height - 1 do
  begin
    temp.SetPixel(0, yb, temp.GetPixel(1, yb));
    temp.SetPixel(temp.Width - 1, yb, temp.GetPixel(temp.Width - 2, yb));
  end;
  mul3.Free;
  mul3 := temp;
  Result := Source.NewBitmap(Source.Width, Source.Height);
  for yb := 0 to Result.Height - 1 do
  begin
    pmul3 := mul3.ScanLine[yb];
    pdest := Result.ScanLine[yb];
    for xb := Result.Width - 1 downto 0 do
    begin
      a := (pmul3 + 1)^.alpha;
      if a = 0 then
        pdest^ := BGRAPixelTransparent
      else
      begin
        pdest^.alpha := a;
        if pmul3^.alpha = 0 then
          pdest^.red := 128
        else
          pdest^.red := pmul3^.red;
        pdest^.green := (pmul3 + 1)^.green;
        if (pmul3 + 2)^.alpha = 0 then
          pdest^.blue := 128
        else
          pdest^.blue := (pmul3 + 2)^.blue;
      end;
      Inc(pdest);
      Inc(pmul3, 3);
    end;
  end;
  mul3.Free;
end;

function ClearTypeInverseSubFilter(Source: TBGRACustomBitmap): TBGRACustomBitmap;
const
  blueA = 20;
  blueB = 0;
  blueC = 2;
  redA = 20;
  redB = 0;
  redC = 2;

  maxV = 255;

var
  yb, xb: integer;
  psrc, pdest, pgray: PBGRAPixel;
  a, v: integer;
  grayscale, temp: TBGRACustomBitmap;

  function Merge3(c1, c2, c3: TBGRAPixel): TBGRAPixel;
  var
    c123: cardinal;
  begin
    if (c1.alpha = 0) then
      Result := MergeBGRA(c2, c3)
    else
    if (c2.alpha = 0) then
      Result := MergeBGRA(c1, c3)
    else
    if (c3.alpha = 0) then
      Result := MergeBGRA(c1, c2)
    else
    begin
      c123 := c1.alpha + c2.alpha + c3.alpha;
      Result.red := (c1.red * c1.alpha + c2.red * c2.alpha + c3.red *
        c3.alpha + c123 shr 1) div c123;
      Result.green := (c1.green * c1.alpha + c2.green * c2.alpha +
        c3.green * c3.alpha + c123 shr 1) div c123;
      Result.blue := (c1.blue * c1.alpha + c2.blue * c2.alpha +
        c3.blue * c3.alpha + c123 shr 1) div c123;
      Result.alpha := (c123 + 1) div 3;
    end;
  end;

begin
  if Source.Width <= 1 then
  begin
    Result := Source.duplicate;
    exit;
  end;
  grayscale := Source;
  temp := Source.NewBitmap(Source.Width, Source.Height);
  for yb := 0 to Source.Height - 1 do
  begin
    psrc := Source.Scanline[yb];
    pgray := grayscale.ScanLine[yb];
    pdest := temp.Scanline[yb];
    pdest^.red := psrc^.red;
    pdest^.green := psrc^.green;
    pdest^.alpha := psrc^.alpha;
    a := (psrc^.alpha * blueA) - ((psrc + 1)^.alpha * (blueB));
    if a > 0 then
    begin
      v := ((integer(psrc^.blue) * blueA) * psrc^.alpha - integer(
        (psrc + 1)^.blue * blueB) * (psrc + 1)^.alpha) div a;
      if v >= maxV then
        pdest^.blue := 255
      else
      if v > 0 then
        pdest^.blue := v
      else
        pdest^.blue := 0;
    end
    else
      pdest^.blue := psrc^.blue;
    Inc(pdest);
    Inc(psrc);
    Inc(pgray);
    for xb := Source.Width - 3 downto 0 do
    begin
      pdest^.green := psrc^.green;
      pdest^.alpha := psrc^.alpha;

      a := (psrc^.alpha * redA) - ((psrc - 1)^.alpha * (redB));
      if a > 0 then
      begin
        v := ((integer(psrc^.red) * redA) * psrc^.alpha - integer(
          (psrc - 1)^.red * redB + ((pgray - 1)^.green - pgray^.green) * redC) *
          (psrc - 1)^.alpha) div a;
        if v >= maxV then
          pdest^.red := 255
        else
        if v > 0 then
          pdest^.red := v
        else
          pdest^.red := 0;
      end
      else
        pdest^.red := psrc^.red;

      a := (psrc^.alpha * blueA) - ((psrc + 1)^.alpha * (blueB));
      if a > 0 then
      begin
        v := ((integer(psrc^.blue) * blueA) * psrc^.alpha - integer(
          (psrc + 1)^.blue * blueB + ((pgray + 1)^.green - pgray^.green) * blueC) *
          (psrc + 1)^.alpha) div a;
        if v >= maxV then
          pdest^.blue := 255
        else
        if v > 0 then
          pdest^.blue := v
        else
          pdest^.blue := 0;
      end
      else
        pdest^.blue := psrc^.blue;
      Inc(pdest);
      Inc(psrc);
      Inc(pgray);
    end;
    pdest^.green := psrc^.green;
    pdest^.blue := psrc^.blue;
    pdest^.alpha := psrc^.alpha;

    a := (psrc^.alpha * redA) - ((psrc - 1)^.alpha * (redB));
    if a > 0 then
    begin
      v := ((integer(psrc^.red) * redA) * psrc^.alpha - integer(
        (psrc - 1)^.red * redB) * (psrc - 1)^.alpha) div a;
      if v >= maxV then
        pdest^.red := 255
      else
      if v > 0 then
        pdest^.red := v
      else
        pdest^.red := 0;
    end
    else
      pdest^.red := psrc^.red;
  end;

  Result := temp;
end;

function ClearTypeSharpenFilter(Source, diffbmp: TBGRACustomBitmap): TBGRACustomBitmap;
const
  factnum = 3;
  factdenom = 5;
var
  xb, yb, maxx: integer;
  psrc, pdest, pdiff: PBGRAPixel;
  d1, d2: integer;

  function clamp(Value: integer): byte;
  begin
    if Value <= 0 then
      Result := 0
    else if Value >= 255 then
      Result := 255
    else
      Result := Value;
  end;

  function adjustDiff(ref, v1, v2: integer): integer;
  begin
    v1 -= ref;
    v2 -= ref;
    Result := v1 + v2;
  end;

begin
  if diffbmp = nil then
    diffbmp := Source;
  if (Source.Width <= 1) or (diffbmp.Width <> Source.Width) or
    (diffbmp.Height <> Source.Height) then
  begin
    Result := Source.Duplicate();
    exit;
  end;
  Result := Source.NewBitmap(Source.Width, Source.Height);
  for yb := 0 to Result.Height - 1 do
  begin
    psrc := Source.ScanLine[yb];
    pdest := Result.ScanLine[yb];
    pdiff := diffbmp.ScanLine[yb];
    maxx := Result.Width - 1;
    for xb := 0 to maxx do
    begin
      if psrc^.alpha <> 0 then
      begin
        if (xb > 0) and ((psrc - 1)^.alpha <> 0) and (xb < maxx) and
          ((psrc + 1)^.alpha <> 0) then
        begin
          d1 := BGRADiff((pdiff - 1)^, pdiff^);
          d2 := BGRADiff((pdiff + 1)^, pdiff^);
          if (d1 > 20) and (d2 > 20) and (d1 + d2 > 100) then
          begin
            pdest^.red := clamp(psrc^.red -
              (adjustDiff(psrc^.red, (psrc + 1)^.red, (psrc - 1)^.red)) *
              factnum div (2 * factdenom));
            pdest^.green := psrc^.green;
            pdest^.blue := clamp(psrc^.blue -
              (adjustDiff(psrc^.blue, (psrc + 1)^.blue, (psrc - 1)^.blue)) *
              factnum div (2 * factdenom));
            pdest^.alpha := psrc^.alpha;
          end
          else
            pdest^ := psrc^;
        end
        else
        if (xb < maxx) and ((psrc + 1)^.alpha <> 0) then
        begin
          pdest^.red := clamp(psrc^.red - ((psrc + 1)^.red - psrc^.red) *
            factnum div factdenom);
          pdest^.green := psrc^.green;
          pdest^.blue := clamp(psrc^.blue - ((psrc + 1)^.blue - psrc^.blue) *
            factnum div factdenom);
          pdest^.alpha := psrc^.alpha;
        end
        else
        if (xb > 0) and ((psrc - 1)^.alpha <> 0) then
        begin
          pdest^.red := clamp(psrc^.red - ((psrc - 1)^.red - psrc^.red) *
            factnum div factdenom);
          pdest^.green := psrc^.green;
          pdest^.blue := clamp(psrc^.blue - ((psrc - 1)^.blue - psrc^.blue) *
            factnum div factdenom);
          pdest^.alpha := psrc^.alpha;
        end
        else
          pdest^ := psrc^;
      end
      else
        pdest^ := BGRAPixelTransparent;

      Inc(pdest);
      Inc(psrc);
      Inc(pdiff);
    end;
  end;
end;

function ClearTypeRemoveContradiction(Source: TBGRACustomBitmap): TBGRACustomBitmap;
var
  xb, yb: integer;
  dr, db: integer;
  ratio: single;
  psrc, pdest: PBGRAPixel;

begin
  if Source.Width <= 1 then
  begin
    Result := Source.Duplicate();
    exit;
  end;
  Result := Source.NewBitmap(Source.Width, Source.Height);
  for yb := 0 to Result.Height - 1 do
  begin
    psrc := Source.ScanLine[yb];
    pdest := Result.ScanLine[yb];
    pdest^ := psrc^;
    for xb := Result.Width - 2 downto 0 do
    begin
      (pdest +1)^ := (psrc + 1)^;
      if (psrc^.alpha > 10) and ((psrc + 1)^.alpha > 10) then
      begin
        dr := psrc^.red - (psrc + 1)^.red;
        db := psrc^.blue - (psrc + 1)^.blue;
        if ((db < 0) and (dr > 0)) or ((db > 0) and (dr < 0)) then
        begin
          ratio := abs(dr / db);
          if (ratio > 0.2) and (ratio < 5) then
          begin
            dr := (psrc^.red * psrc^.alpha + (psrc + 1)^.red * (psrc + 1)^.alpha) div
              (psrc^.alpha + (psrc + 1)^.alpha);
            db := (psrc^.blue * psrc^.alpha + (psrc + 1)^.blue * (psrc + 1)^.alpha) div
              (psrc^.alpha + (psrc + 1)^.alpha);
            pdest^.red := dr;
            pdest^.blue := db;
            (pdest +1)^.red := dr;
            (pdest +1)^.blue := db;
          end;
        end;
      end;
      Inc(pdest);
      Inc(psrc);
    end;
  end;
end;

function ClearTypeInverseFilter(Source: TBGRACustomBitmap): TBGRACustomBitmap;
var
  mul3, temp: TBGRACustomBitmap;
  xb, yb: integer;
  pmul3, pdest: PBGRAPixel;
  a: byte;
begin
  Source.ResampleFilter := rfSpline;
  mul3 := Source.Resample(Source.Width * 3 - 2, Source.Height);
  temp := Source.NewBitmap(Source.Width * 3, Source.Height);
  temp.PutImage(1, 0, mul3, dmSet);
  for yb := 0 to temp.Height - 1 do
  begin
    temp.SetPixel(0, yb, temp.GetPixel(1, yb));
    temp.SetPixel(temp.Width - 1, yb, temp.GetPixel(temp.Width - 2, yb));
  end;
  mul3.Free;
  mul3 := temp;
  Result := Source.NewBitmap(Source.Width, Source.Height);
  for yb := 0 to Result.Height - 1 do
  begin
    pmul3 := mul3.ScanLine[yb];
    pdest := Result.ScanLine[yb];
    for xb := Result.Width - 1 downto 0 do
    begin
      a := (pmul3 + 1)^.alpha;
      if a = 0 then
        pdest^ := BGRAPixelTransparent
      else
      begin
        pdest^.alpha := a;
        if (pmul3 + 2)^.alpha = 0 then
          pdest^.red := 128
        else
          pdest^.red := (pmul3 + 2)^.red;
        pdest^.green := (pmul3 + 1)^.green;
        if pmul3^.alpha = 0 then
          pdest^.blue := 128
        else
          pdest^.blue := pmul3^.blue;
      end;
      Inc(pdest);
      Inc(pmul3, 3);
    end;
  end;
  mul3.Free;

  temp := ClearTypeRemoveContradiction(Result);
  Result.Free;
  Result := temp;

  temp := Result;
  Result := ClearTypeSharpenFilter(temp, Source);
  temp.Free;

  temp := ClearTypeRemoveContradiction(Result);
  Result.Free;
  Result := temp;
end;

function DoResample(Source: TBGRABitmap; newWidth, newHeight: integer;
  StretchMode: TResampleMode): TBGRABitmap;
begin
  Result := Source.Resample(newWidth, newHeight, StretchMode) as TBGRABitmap;
end;

function CreateMarbleTexture(tx, ty: integer): TBGRABitmap;
var
  colorOscillation: integer;
  p: PBGRAPixel;
  i: integer;
begin
  Result := CreateCyclicPerlinNoiseMap(tx, ty, 0.5, 0.5, 0.8, rfBestQuality);
  p := Result.Data;
  for i := 0 to Result.NbPixels - 1 do
  begin
    colorOscillation := round(sqrt(sqrt((sin(p^.red * Pi / 128 + 0.5) + 1) / 2)) * 256);
    p^ := Interp256(BGRA(161, 117, 105), BGRA(218, 197, 180), colorOscillation);
    Inc(p);
  end;
end;

function CreateWaterTexture(tx, ty: integer): TBGRABitmap;
const
  blurSize = 5;
var
  temp: TBGRABitmap;
  phong: TPhongShading;
begin
  Result := CreateCyclicPerlinNoiseMap(tx, ty, 1, 1, 1.2, rfBestQuality);
  temp := Result.GetPart(rect(-blurSize, -blurSize, tx + blurSize, ty + blurSize)) as
    TBGRABitmap;
  BGRAReplace(temp, temp.FilterBlurRadial(blurSize, rbFast));
  phong := TPhongShading.Create;
  phong.LightSourceDistanceFactor := 0;
  phong.LightDestFactor := 0;
  phong.LightSourceIntensity := 150;
  phong.LightPositionZ := 80;
  phong.LightColor := BGRA(105, 233, 240);
  phong.NegativeDiffusionFactor := 0.3;
  phong.SpecularIndex := 20;
  phong.AmbientFactor := 0.4;
  phong.Draw(Result, temp, 20, -blurSize, -blurSize, BGRA(28, 139, 166));
  phong.Free;
  temp.Free;
end;

function CreateStoneTexture(tx, ty: integer): TBGRABitmap;
var
  temp: TBGRABitmap;
  phong: TPhongShading;
begin
  Result := CreateCyclicPerlinNoiseMap(tx, ty, 1, 1, 0.6);
  temp := Result.GetPart(rect(-2, -2, tx + 2, ty + 2)) as TBGRABitmap;
  phong := TPhongShading.Create;
  phong.LightSourceDistanceFactor := 0;
  phong.LightDestFactor := 0;
  phong.LightSourceIntensity := 100;
  phong.LightPositionZ := 100;
  phong.NegativeDiffusionFactor := 0.3;
  phong.AmbientFactor := 0.5;
  phong.Draw(Result, temp, 30, -2, -2, BGRA(170, 170, 170));
  phong.Free;
  temp.Free;
end;

function CreateRoundStoneTexture(tx, ty: integer): TBGRABitmap;
var
  temp: TBGRABitmap;
  phong: TPhongShading;
begin
  Result := CreateCyclicPerlinNoiseMap(tx, ty, 1, 1, 1.2, rfBestQuality);
  temp := Result.GetPart(rect(-2, -2, tx + 2, ty + 2)) as TBGRABitmap;
  BGRAReplace(temp, temp.FilterBlurRadial(2, rbFast));
  phong := TPhongShading.Create;
  phong.LightSourceDistanceFactor := 0;
  phong.LightDestFactor := 0;
  phong.LightSourceIntensity := 70;
  phong.LightPositionZ := 100;
  phong.NegativeDiffusionFactor := 0;
  phong.SpecularIndex := 10;
  phong.AmbientFactor := 0.5;
  phong.LightColor := BGRA(255, 255, 192);
  phong.Draw(Result, temp, 30, -2, -2, BGRA(170, 170, 170));
  phong.Free;
  temp.Free;
end;

function CreateSnowPrintTexture(tx, ty: integer): TBGRABitmap;
var
  v: single;
  p: PBGRAPixel;
  i: integer;

  temp: TBGRABitmap;
  phong: TPhongShading;
begin
  Result := CreateCyclicPerlinNoiseMap(tx, ty, 1, 1, 1.2, rfBestQuality);

  p := Result.Data;
  for i := 0 to Result.NbPixels - 1 do
  begin
    v := p^.red;
    if v > 80 then
      v := (v - 80) / 10 + 80;
    if v < 50 then
      v := 50 - (50 - v) / 10;
    p^ := MapHeightToBGRA(v / 255, 255);
    Inc(p);
  end;

  temp := Result.GetPart(rect(-2, -2, tx + 2, ty + 2)) as TBGRABitmap;
  phong := TPhongShading.Create;
  phong.LightSourceDistanceFactor := 0;
  phong.LightDestFactor := 0;
  phong.LightSourceIntensity := 100;
  phong.LightPositionZ := 100;
  phong.NegativeDiffusionFactor := 0.3;
  phong.Draw(Result, temp, 30, -2, -2, BGRAWhite);
  phong.Free;
  temp.Free;
end;

function CreateCamouflageTexture(tx, ty: integer): TBGRABitmap;
var
  v: integer;
  p: PBGRAPixel;
  i: integer;

  temp: TBGRABitmap;
begin
  Result := CreateCyclicPerlinNoiseMap(tx, ty, 1, 1, 1, rfBestQuality);

  p := Result.Data;
  for i := 0 to Result.NbPixels - 1 do
  begin
    v := p^.red;
    if v < 64 then
      p^ := BGRA(31, 33, 46)
    else
    if v < 128 then
      p^ := BGRA(89, 71, 57)
    else
    if v < 192 then
      p^ := BGRA(80, 106, 67)
    else
      p^ := BGRA(161, 157, 121);
    Inc(p);
  end;

  temp := Result.getPart(rect(-2, -2, tx + 2, ty + 2)) as TBGRABitmap;
  BGRAReplace(temp, temp.FilterMedian(moMediumSmooth));
  Result.PutImage(-2, -2, temp, dmSet);
  temp.Free;
end;

function CreatePlastikTexture(tx, ty: integer): TBGRABitmap;
const
  blurSize = 2;
var
  temp: TBGRABitmap;
  phong: TPhongShading;
  p: PBGRAPixel;
  i: integer;
  v: byte;
begin
  Result := CreateCyclicPerlinNoiseMap(tx, ty, 1, 1, 1);

  p := Result.Data;
  for i := 0 to Result.NbPixels - 1 do
  begin
    v := p^.red;
    if v < 32 then
      v := v * 2
    else
    if (v > 32) and (v < 224) then
      v := (v - 32) div 2 + 64
    else
    if v >= 224 then
      v := (v - 224) * 2 + (224 - 32) div 2;
    p^ := BGRA(v, v, v);
    Inc(p);
  end;

  temp := Result.GetPart(rect(-blurSize, -blurSize, tx + blurSize, ty + blurSize)) as
    TBGRABitmap;
  BGRAReplace(temp, temp.FilterNormalize(False));
  BGRAReplace(temp, temp.FilterBlurMotion(ty div 6, 90, False));
  BGRAReplace(temp, temp.FilterBlurRadial(blurSize, rbFast));

  phong := TPhongShading.Create;
  phong.LightSourceDistanceFactor := 0;
  phong.LightDestFactor := 0;
  phong.LightSourceIntensity := 300;
  phong.LightPositionZ := 10;
  phong.NegativeDiffusionFactor := 0;
  phong.AmbientFactor := 0.6;
  phong.SpecularIndex := 25;
  phong.SpecularFactor := 10;
  phong.Draw(Result, temp, 10, -blurSize, -blurSize, BGRA(58, 206, 113));
  phong.Free;
  temp.Free;
end;

function CreateMetalFloorTexture(tx: integer): TBGRABitmap;
var
  temp, noise: TBGRABitmap;
  phong: TPhongShading;
  ty: integer;
begin
  ty := tx div 2;
  Result := TBGRABitmap.Create(tx, ty, BGRABlack);
  Result.FillEllipseAntialias(tx * 1.2 / 8, ty / 2, tx / 20, ty / 3,
    BGRA(240, 240, 240));
  Result.FillEllipseAntialias(tx * 2.8 / 8, ty / 2, tx / 20, ty / 3,
    BGRA(240, 240, 240));
  Result.FillEllipseAntialias(tx * 3 / 4, ty * 1.2 / 4, ty / 3, tx /
    20, BGRA(240, 240, 240));
  Result.FillEllipseAntialias(tx * 3 / 4, ty * 2.8 / 4, ty / 3, tx /
    20, BGRA(240, 240, 240));
  BGRAReplace(Result, Result.FilterBlurRadial(1, rbFast));

  noise := CreateCyclicPerlinNoiseMap(tx, ty, 1, 1, 1);
  noise.FillRect(0, 0, tx, ty, BGRA(0, 0, 0, 220), dmLinearBlend);
  Result.BlendImage(0, 0, noise, boAdditive);
  noise.Free;

  temp := Result.GetPart(rect(-2, -2, tx + 2, ty + 2)) as TBGRABitmap;
  phong := TPhongShading.Create;
  phong.LightSourceDistanceFactor := 0;
  phong.LightDestFactor := 0;
  phong.LightSourceIntensity := 100;
  phong.LightPositionZ := 80;
  phong.NegativeDiffusionFactor := 0;
  phong.AmbientFactor := 0.5;
  phong.Draw(Result, temp, 10, -2, -2, BGRA(116, 116, 116));
  phong.Free;
  temp.Free;
end;

function ComputeAngle(dx, dy: single): single;
begin
  if dy = 0 then
  begin
    if dx < 0 then
      Result := 180
    else
      Result := 0;
  end
  else
  if dx = 0 then
  begin
    if dy < 0 then
      Result := -90
    else
      Result := 90;
  end
  else
  begin
    Result := ArcTan(dy / dx) * 180 / Pi;
    if dx < 0 then
      Result += 180;
  end;
end;

function GetSelectionCenter(bmp: TBGRABitmap): TPointF;
var
  xb, yb: integer;
  p: PBGRAPixel;
  xsum, ysum, asum, alpha: single;
begin
  xsum := 0;
  ysum := 0;
  asum := 0;
  for yb := 0 to bmp.Height - 1 do
  begin
    p := bmp.ScanLine[yb];
    for xb := 0 to bmp.Width - 1 do
    begin
      alpha := p^.red / 255;
      Inc(p);
      xsum += xb * alpha;
      ysum += yb * alpha;
      asum += alpha;
    end;
  end;
  if asum = 0 then
    Result := pointF(bmp.Width / 2 - 0.5, bmp.Height / 2 - 0.5)
  else
    Result := pointF(xsum / asum, ysum / asum);
end;

procedure ComputeSelectionMask(image: TBGRABitmap; destMask: TBGRABitmap);
var
  maxx, maxy: integer;
  aimage: byte;
  xb, yb: integer;
  pimage, pmask: PBGRAPixel;
begin
  maxx := min(image.Width, destMask.Width) - 1;
  maxy := min(image.Height, destMask.Height) - 1;
  for yb := 0 to maxy do
  begin
    pimage := image.ScanLine[yb];
    pmask := destMask.ScanLine[yb];
    for xb := 0 to maxx do
    begin
      aimage := pimage^.alpha;
      pmask^ := BGRA(aimage, aimage, aimage, 255);
      if aimage <> 0 then
        pimage^.alpha := 255;
      Inc(pimage);
      Inc(pmask);
    end;
  end;
end;

procedure SubstractMask(image: TBGRABitmap; mask: TBGRABitmap);
var
  maxx, maxy: integer;
  xb, yb: integer;
  pimage, pmask: PBGRAPixel;
  aimage, amask: byte;
begin
  maxx := min(image.Width, Mask.Width) - 1;
  maxy := min(image.Height, Mask.Height) - 1;
  for yb := 0 to maxy do
  begin
    pimage := image.ScanLine[yb];
    pmask := Mask.ScanLine[yb];
    for xb := 0 to maxx do
    begin
      amask := pmask^.red;
      if amask <> 0 then
      begin
        aimage := pimage^.alpha;
        if aimage > amask then
          pimage^.alpha := aimage - amask
        else
          pimage^ := BGRAPixelTransparent;
      end;
      Inc(pimage);
      Inc(pmask);
    end;
  end;
end;

procedure NicePoint(bmp: TBGRABitmap; x, y: single);
begin
  bmp.EllipseAntialias(x, y, 4, 4, BGRA(0, 0, 0, 192), 1);
  bmp.EllipseAntialias(x, y, 3, 3, BGRA(255, 255, 255, 192), 1);
  bmp.EllipseAntialias(x, y, 2, 2, BGRA(0, 0, 0, 192), 1);
end;

procedure NicePoint(bmp: TBGRABitmap; ptF: TPointF);
begin
  NicePoint(bmp, ptF.x, ptF.y);
end;

procedure NiceLine(bmp: TBGRABitmap; x1, y1, x2, y2: single);
begin
  bmp.DrawLineAntialias(round(x1), round(y1), round(x2),
    round(y2), BGRA(0, 0, 0, 192), 3, True);
  bmp.DrawLineAntialias(round(x1), round(y1), round(x2),
    round(y2), BGRA(255, 255, 255, 192), 1, True);
end;

function ComputeColorCircle(tx, ty: integer; light: word;
  hueCorrection: boolean = True): TBGRABitmap;
var
  xb, yb: integer;
  pdest: PBGRAPixel;
  angle, xc, yc: single;
  ec: TExpandedPixel;
  c: TBGRAPixel;
  gray, level: word;
begin
  Result := TBGRABitmap.Create(tx, ty);
  Result.FillEllipseAntialias(tx / 2 - 0.5, ty / 2 - 0.5, tx / 2, ty / 2, BGRABlack);
  xc := tx / 2 - 0.5;
  yc := ty / 2 - 0.5;
  for yb := 0 to ty - 1 do
  begin
    pdest := Result.scanline[yb];
    for xb := 0 to tx - 1 do
    begin
      if pdest^.alpha <> 0 then
      begin
        ec.alpha := $FFFF;
        angle := ComputeAngle(xb - xc, yb - yc);
        if angle < 0 then
          angle += 360;
        if hueCorrection then
          angle := GtoH(round(angle / 360 * 65536) and 65535) / 65536 * 360;
        if angle < 60 then
        begin
          ec.red := $FFFF;
          ec.green := round(angle / 60 * $FFFF);
          ec.blue := $0000;
        end
        else
        if angle < 120 then
        begin
          ec.red := $FFFF - round((angle - 60) / 60 * $FFFF);
          ec.green := $FFFF;
          ec.blue := $0000;
        end
        else
        if angle < 180 then
        begin
          ec.red := $0000;
          ec.green := $FFFF;
          ec.blue := round((angle - 120) / 60 * $FFFF);
        end
        else
        if angle < 240 then
        begin
          ec.red := $0000;
          ec.green := $FFFFF - round((angle - 180) / 60 * $FFFF);
          ec.blue := $FFFF;
        end
        else
        if angle < 300 then
        begin
          ec.red := round((angle - 240) / 60 * $FFFF);
          ec.green := $0000;
          ec.blue := $FFFF;
        end
        else
        begin
          ec.red := $FFFF;
          ec.green := $0000;
          ec.blue := $FFFFF - round((angle - 300) / 60 * $FFFF);
        end;
        gray := min($FFFF, max(0, $FFFF - round(
          (sqrt(sqr((xb - xc) / (tx / 2)) + sqr((yb - yc) / (ty / 2))) *
          1.2 - 0.1) * $FFFF)));
        level := max(max(ec.red, ec.green), ec.blue);
        {$hints off}
        ec.red := (ec.red * ($FFFF - gray) + level * gray) shr 16;
        ec.green := (ec.green * ($FFFF - gray) + level * gray) shr 16;
        ec.blue := (ec.blue * ($FFFF - gray) + level * gray) shr 16;
        {$hints on}
        ec.red := (ec.red * light) shr 16;
        ec.green := (ec.green * light) shr 16;
        ec.blue := (ec.blue * light) shr 16;
        c := GammaCompression(ec);
        c.alpha := pdest^.alpha;
        pdest^ := c;
      end;
      Inc(pdest);
    end;
  end;
end;

function ChangeCanvasSize(bmp: TBGRABitmap; newWidth, newHeight: integer;
  anchor: string; background: TBGRAPixel; repeatImage: boolean;
  flipMode: boolean = False): TBGRABitmap;
var
  origin: TPoint;
  xb, yb: integer;
  dx, dy: integer;
  minx, miny, maxx, maxy: integer;
  flippedImages: array[boolean, boolean] of TBGRABitmap;
begin
  if (newWidth < 1) or (newHeight < 1) then
    raise Exception.Create('Invalid canvas size');
  origin := Point((newWidth - bmp.Width) div 2, (newHeight - bmp.Height) div 2);
  anchor := UTF8LowerCase(anchor);
  if (anchor = 'topleft') or (anchor = 'top') or (anchor = 'topright') then
    origin.Y := 0;
  if (anchor = 'bottomleft') or (anchor = 'bottom') or (anchor = 'bottomright') then
    origin.Y := newHeight - bmp.Height;
  if (anchor = 'topleft') or (anchor = 'left') or (anchor = 'bottomleft') then
    origin.X := 0;
  if (anchor = 'topright') or (anchor = 'right') or (anchor = 'bottomright') then
    origin.X := newWidth - bmp.Width;
  Result := TBGRABitmap.Create(newWidth, newHeight, background);
  dx := bmp.Width;
  dy := bmp.Height;
  if repeatImage then
  begin
    minx := (0 - origin.X - bmp.Width + 1) div bmp.Width;
    miny := (0 - origin.Y - bmp.Width + 1) div bmp.Width;
    maxx := (newWidth - origin.X + bmp.Width - 1) div bmp.Width;
    maxy := (newHeight - origin.Y + bmp.Width - 1) div bmp.Width;
  end
  else
  begin
    minx := 0;
    miny := 0;
    maxx := 0;
    maxy := 0;
  end;
  if flipMode and repeatImage then
  begin
    flippedImages[False, False] := bmp;
    if (minx <> 0) or (miny <> 0) or (maxx <> 0) or (maxy <> 0) then
    begin
      flippedImages[True, False] := bmp.Duplicate as TBGRABitmap;
      flippedImages[True, False].HorizontalFlip;
      flippedImages[True, True] := flippedImages[True, False].Duplicate as TBGRABitmap;
      flippedImages[True, True].VerticalFlip;
      flippedImages[False, True] := bmp.Duplicate as TBGRABitmap;
      flippedImages[False, True].VerticalFlip;
    end
    else
    begin
      flippedImages[True, False] := nil;  //never used
      flippedImages[True, True] := nil;
      flippedImages[False, True] := nil;
    end;
    for xb := minx to maxx do
      for yb := miny to maxy do
        Result.PutImage(origin.x + xb * dx, origin.Y + yb * dy,
          flippedImages[odd(xb), odd(yb)], dmSet);
    flippedImages[True, False].Free;
    flippedImages[True, True].Free;
    flippedImages[False, True].Free;
  end
  else
  begin
    for xb := minx to maxx do
      for yb := miny to maxy do
        Result.PutImage(origin.x + xb * dx, origin.Y + yb * dy, bmp, dmSet);
  end;
end;

function MakeThumbnail(bmp: TBGRABitmap; Width, Height: integer): TBGRABitmap;
var
  resampled: TBGRABitmap;
begin
  Result := TBGRABitmap.Create(Width, Height);
  if (Width <> 0) and (Height <> 0) and (bmp.Width <> 0) and (bmp.Height <> 0) then
  begin
    if bmp.Width / bmp.Height > Width / Height then
      resampled := bmp.Resample(Width,
        max(1, round(bmp.Height * (Width / bmp.Width)))) as TBGRABitmap

    else
      resampled := bmp.Resample(max(1, round(bmp.Width * (Height / bmp.Height))),
        Height) as TBGRABitmap;
    Result.PutImage((Result.Width - resampled.Width) div 2,
      (Result.Height - resampled.Height) div 2, resampled, dmSet);
    resampled.Free;
  end;
end;

initialization

  Randomize;

end.
