program test_color_svg;

{$mode objfpc}{$H+}

uses
  {$ifdef UNIX}cthreads,{$endif}
  Interfaces, Forms, SysUtils, Graphics,
  BGRABitmap, BGRABitmapTypes, BGRACanvas2D, BGRASVG,
  BCColors, BCColorUtils,
  fpcunit, testregistry, consoletestrunner;

var
  Checks, Failures: integer;
  FailedNames: string;

procedure Check(OK: boolean; const Msg: string);
begin
  Inc(Checks);
  if not OK then
  begin
    Inc(Failures);
    FailedNames := FailedNames + LineEnding + Msg;
    WriteLn('FAIL: ', Msg);
  end;
end;

function PixelText(const P: TBGRAPixel): string;
begin
  Result := BGRAToStr(P, nil, 0, False, True);
end;

{ Compare visible output, allowing one alpha step for SVG opacity rounding.
  RGB must match exactly whenever visible. }
function SameRendered(const A, B: TBGRAPixel): boolean;
begin
  Result := (Abs(integer(A.alpha) - B.alpha) <= 1) and
    (((A.alpha = 0) and (B.alpha = 0)) or
     ((A.red = B.red) and (A.green = B.green) and (A.blue = B.blue)));
end;

function Render(const ColorText, Opacity: string; AsStyle: boolean): TBGRAPixel;
var
  SVG: TBGRASVG;
  Bmp: TBGRABitmap;
  Ctx: TBGRACanvas2D;
  Attr: string;
begin
  if AsStyle then
  begin
    Attr := 'style="fill:' + ColorText;
    if Opacity <> '' then Attr += ';fill-opacity:' + Opacity;
    Attr += '"';
  end
  else
  begin
    Attr := 'fill="' + ColorText + '"';
    if Opacity <> '' then Attr += ' fill-opacity="' + Opacity + '"';
  end;
  SVG := TBGRASVG.CreateFromString(
    '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 16 16">' +
    '<rect x="0" y="0" width="16" height="16" ' + Attr + '/></svg>');
  try
    SVG.DefaultDpi := 96;
    Bmp := TBGRABitmap.Create(16, 16, BGRAPixelTransparent);
    try
      Ctx := TBGRACanvas2D.Create(Bmp);
      try
        SVG.Draw(Ctx, 0, 0);
        Result := Bmp.GetPixel(8, 8); // interior: no edge antialiasing
      finally
        Ctx.Free;
      end;
    finally
      Bmp.Free;
    end;
  finally
    SVG.Free;
  end;
end;

{ Check acceptance and pixels from the BC parser, bitmap parser and SVG fill.
  A rejected bitmap color returns transparent; a rejected SVG fill uses black. }
procedure ColorCase(const S: string; const Expected: TBGRAPixel;
  BCExpected, BitmapExpected: boolean; const BitmapPixel: string = '');
var
  V: TBCColorValue;
  P, Direct, Actual, SVGExpected: TBGRAPixel;
  BCOk, Missing, Error: boolean;
  Mode: integer;
begin
  BCOk := TryStrToBC(S, V);
  Check(BCOk = BCExpected, 'BC parser acceptance: ' + S);
  if BCOk then
  begin
    P := V.ToBGRA;
    Check(P.EqualsExactly(Expected), 'BC parser pixel: ' + S);
    WriteLn('CASE ', S, ' | BC=', PixelText(P));
  end
  else WriteLn('CASE ', S, ' | BC=REJECT');

  P := BGRA(17, 34, 51, 68); // sentinel, not an implicit fallback
  TryStrToBGRA(S, P, Missing, Error);
  Check((not Missing and not Error) = BitmapExpected, 'bitmap acceptance: ' + S);
  WriteLn('  TryStrToBGRA missing=', Missing, ' error=', Error, ' pixel=', PixelText(P));
  SVGExpected := Expected;
  if BitmapPixel <> '' then SVGExpected := StrToBGRA(BitmapPixel);
  if BitmapExpected then Check(SameRendered(P, SVGExpected), 'bitmap pixel: ' + S);
  Direct := StrToBGRA(S);
  if not BitmapExpected then SVGExpected := BGRAPixelTransparent;
  Check(SameRendered(Direct, SVGExpected), 'StrToBGRA fallback: ' + S);
  WriteLn('  StrToBGRA=', PixelText(Direct));
  if not BitmapExpected then SVGExpected := BGRABlack;

  for Mode := 0 to 1 do
  begin
    Actual := Render(S, '', Mode = 1);
    WriteLn('  SVG style=', Mode = 1, ' pixel=', PixelText(Actual),
      ' intended-match=', SameRendered(Actual, Expected));
    Check(SameRendered(Actual, SVGExpected), 'SVG baseline: ' + S);
  end;
end;

procedure SeparateOpacityCase(const S: string; const Expected: TBGRAPixel);
var
  Actual: TBGRAPixel;
  Mode: integer;
begin
  for Mode := 0 to 1 do
  begin
    Actual := Render(S, '0.5', Mode = 1);
    WriteLn('SEPARATE ', S, ' opacity=0.5 style=', Mode = 1, ' pixel=', PixelText(Actual));
    Check(SameRendered(Actual, Expected), 'separate opacity: ' + S);
  end;
end;

{ Check that missing comma channels retain fallback values and invalid RGB
  arguments produce an error. }
procedure PartialCases;
var
  Error: boolean;
  Gray: TBGRAPixel;
begin
  Gray := BGRA(128, 128, 128, 128);
  Check(PartialStrToBGRA('rgb(255,?,?,?)', Gray, Error).EqualsExactly(
    BGRA(255, 128, 128, 128)) and not Error, 'partial: missing values replaced');
  Check(PartialStrToBGRA('rgb(255,?,?)', Gray, Error).EqualsExactly(
    BGRA(255, 128, 128, 255)) and not Error, 'partial: implicit opaque alpha');
  Check(PartialStrToBGRA('rgb(255,,0)', Gray, Error).EqualsExactly(
    BGRA(255, 128, 0, 255)) and not Error, 'partial: empty argument is missing');
  Check(PartialStrToBGRA('rgb(255,abc,0)', Gray, Error).EqualsExactly(
    BGRA(255, 0, 0, 255)) and Error, 'partial: invalid channel is 0 and an error');
  PartialStrToBGRA('rgb(255 ? 0)', Gray, Error);
  Check(Error,
    'partial: no missing values in the space syntax');
  Check(StrToBGRA('rgb(255,0,?)', CSSYellow).EqualsExactly(CSSYellow),
    'partial: missing is a failure for StrToBGRA');
end;

procedure ExportCase;
var
  V: TBCColorValue;
  Symbolic, Resolved: string;
  Expected: TBGRAPixel;
  Mode: integer;
begin
  V := TBCColorValue.New(clBtnFace, 128);
  Symbolic := BCToCSS(V);
  Resolved := BCToCSS(V, clBlack, True);
  Expected := V.ToBGRA;
  WriteLn('EXPORT symbolic=', Symbolic, ' resolved=', Resolved);
  ColorCase(Symbolic, Expected, True, False);
  ColorCase(Resolved, Expected, True, True);
  for Mode := 0 to 1 do
    Check(SameRendered(Render(Resolved, '0.5', Mode = 1),
      ColorToBGRA(clBtnFace, 64)), 'embedded alpha multiplied by fill-opacity');
end;

{ All cases; the FPCUnit test below runs them (and reports exceptions). }
procedure RunChecks;
begin
  ColorCase('red', BGRA(255, 0, 0), True, True);
  ColorCase('#FF000080', BGRA(255, 0, 0, 128), True, True);
  ColorCase('rgba(255,0,0,0.5)', BGRA(255, 0, 0, 128), True, True);
  { Space-separated and relative RGB: accepted by BC, rejected by bitmap/SVG. }
  ColorCase('rgb(255 0 0 / 50%)', BGRA(255, 0, 0, 128), True, False);
  ColorCase('rgb (255 0 0)', BGRAPixelTransparent, False, False);
  ColorCase('rgb(1. 0 0)', BGRAPixelTransparent, False, False);
  ColorCase('rgb(1.e2 0 0)', BGRAPixelTransparent, False, False);
  ColorCase('rgb(1e+ 0 0)', BGRAPixelTransparent, False, False);
  ColorCase('rgb(.5 1.5 2.5 / .3)', BGRA(1, 2, 3, 77), True, False);
  ColorCase('rgb(from red r g b / 50%)', BGRA(255, 0, 0, 128), True, False);
  ColorCase('rgb(from rgba(255,0,0,0.5) r g b)', BGRA(255, 0, 0, 128), True, False);
  ColorCase('rgb(from red r g b / alpha)', BGRA(255, 0, 0), True, False);
  ColorCase('rgb(255 0 0 / )', BGRAPixelTransparent, False, False);
  { Comma syntax: explicitly check differences in rounding and acceptance. }
  ColorCase('rgb(50%,0,0)', BGRA(128, 0, 0), True, True, '#7F0000');
  ColorCase('rgba(255,0,0,50%)', BGRA(255, 0, 0, 128), True, False);
  ColorCase('rgb(127.5,0,0)', BGRA(128, 0, 0), True, False);
  ColorCase('rgb(255,0,0,0.502)', BGRA(255, 0, 0, 128), True, True);
  ColorCase('rgb(1,2,3)xyz', BGRA(1, 2, 3), True, True);
  ColorCase('rgb 1,2,3', BGRA(1, 2, 3), False, True);
  ColorCase('rgb(255,0,?)', BGRAPixelTransparent, False, False);
  PartialCases;
  ColorCase('ThreeDFace', ColorToBGRA(clBtnFace), True, False);
  ColorCase('clBtnFace', ColorToBGRA(clBtnFace), True, False);
  ColorCase('Canvas', BGRAPixelTransparent, False, False);
  ColorCase('transparent', BGRAPixelTransparent, True, True);
  ColorCase('#FF000000', BGRA(255, 0, 0, 0), True, True);
  ColorCase('NotAColor', BGRAPixelTransparent, False, False);
  SeparateOpacityCase('#FF0000', BGRA(255, 0, 0, 128));
  SeparateOpacityCase('ThreeDFace', BGRA(0, 0, 0, 128));
  ExportCase;
end;

type
  { FPCUnit wrapper, so that .github/workflows/make.pas runs this program. }
  TColorSVGTest = class(TTestCase)
  published
    procedure TestColorSVG;
  end;

procedure TColorSVGTest.TestColorSVG;
begin
  Checks := 0;
  Failures := 0;
  FailedNames := '';
  RunChecks;
  WriteLn('Checks=', Checks, ' failures=', Failures);
  if Failures <> 0 then
    Fail(IntToStr(Failures) + ' failed checks:' + FailedNames);
end;

begin
  Application.Initialize;
  RegisterTest(TColorSVGTest);
  with TTestRunner.Create(nil) do
  try
    Initialize;
    Run;
  finally
    Free;
  end;
end.
