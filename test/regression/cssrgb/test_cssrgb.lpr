program test_cssrgb;

{$mode objfpc}{$H+}

uses
  {$ifdef UNIX}cthreads, cwstring,{$endif}
  Interfaces, SysUtils, BGRABitmapTypes, BCColors,
  fpcunit, testregistry, consoletestrunner;

var
  Checks, Failures: integer;
  FailedNames: string;

procedure Check(OK: boolean; const Name: string);
begin
  Inc(Checks);
  if not OK then
  begin
    Inc(Failures);
    FailedNames := FailedNames + LineEnding + Name;
    WriteLn('FAIL ', Name);
  end;
end;

function PixelText(const P: TBGRAPixel): string;
begin
  result := BGRAToStr(P, nil, 0, False, True);
end;

{ Absolute syntax and channel values. }
procedure AbsoluteCase(const S: string; const Expected: TBGRAPixel);
var
  base: string;
  color: TBGRAPixel;
  overrideAlpha: boolean;
begin
  if not TryParseCSSRGB(S, base, color, overrideAlpha) then
  begin
    Check(False, S + ' TryParseCSSRGB accepts');
    Exit; // output parameters are undefined on failure
  end;
  Check(True, S + ' TryParseCSSRGB accepts');
  Check(overrideAlpha, S + ' absolute alpha is explicit or defaults to opaque');
  Check(base = '', S + ' has no origin');
  Check(color.EqualsExactly(Expected), S + ' TryParseCSSRGB gives ' +
    PixelText(Expected) + ', not ' + PixelText(color));

end;

{ Relative syntax returns a validated origin without resolving its RGB. }
procedure RelativeCase(const S, ExpectedBase: string; ExpectedOverride: boolean;
  ExpectedAlpha: byte);
var
  base: string;
  color: TBGRAPixel;
  overrideAlpha: boolean;
begin
  if not TryParseCSSRGB(S, base, color, overrideAlpha) then
  begin
    Check(False, S + ' TryParseCSSRGB accepts');
    Exit; // output parameters are undefined on failure
  end;
  Check(True, S + ' TryParseCSSRGB accepts');
  Check(base = ExpectedBase, S + ' origin is ' + ExpectedBase + ', not ' + base);
  Check(overrideAlpha = ExpectedOverride, S + ' alpha override');
  if ExpectedOverride then
    Check(color.alpha = ExpectedAlpha, S + ' alpha ' + IntToStr(ExpectedAlpha));

end;

{ Check rejection of invalid syntax or unsupported origins. }
procedure RejectCase(const S: string);
var
  base: string;
  color: TBGRAPixel;
  overrideAlpha: boolean;
begin
  Check(not TryParseCSSRGB(S, base, color, overrideAlpha), S + ' TryParseCSSRGB rejects');

end;

{ Valid symbolic origins retain their spelling and case. }
procedure SymbolicCase(const S, ExpectedBase: string; ExpectedOverride: boolean;
  ExpectedAlpha: byte);
var
  base: string;
  color: TBGRAPixel;
  overrideAlpha: boolean;
begin
  if not TryParseCSSRGB(S, base, color, overrideAlpha) then
  begin
    Check(False, S + ' symbolic syntax accepted');
    Exit;
  end;
  Check(base = ExpectedBase, S + ' preserves symbolic origin');
  Check(overrideAlpha = ExpectedOverride, S + ' symbolic alpha override');
  if ExpectedOverride then
    Check(color.alpha = ExpectedAlpha, S + ' symbolic alpha value');

end;

function Nested(ALevels: integer): string;
var
  i: integer;
begin
  result := 'red';
  for i := 1 to ALevels do
    result := 'rgb(from ' + result + ' r g b)';
end;

procedure RunChecks;
begin
  // Comma syntax
  AbsoluteCase('rgb(255,0,0)', BGRA(255, 0, 0));
  AbsoluteCase('rgba(255,0,0,0.5)', BGRA(255, 0, 0, 128));
  AbsoluteCase('rgb(255,0,0,0.502)', BGRA(255, 0, 0, 128));
  AbsoluteCase('rgb( 1 , 2 , 3 )', BGRA(1, 2, 3));
  AbsoluteCase('rgba(255,0,0,50%)', BGRA(255, 0, 0, 128));
  AbsoluteCase('rgb(50%,0,0)', BGRA(128, 0, 0));  // mixed percentages and numbers
  AbsoluteCase('rgb(100%,50%,0%)', BGRA(255, 128, 0));
  AbsoluteCase('rgb(127.5,0,0)', BGRA(128, 0, 0));
  AbsoluteCase('rgb(1e2,0,0)', BGRA(100, 0, 0));
  AbsoluteCase('rgb(300,-5,0)', BGRA(255, 0, 0));  // clamped
  AbsoluteCase('rgba(0,0,0,2)', BGRA(0, 0, 0, 255));
  AbsoluteCase('rgba(0,0,0,-1)', BGRA(0, 0, 0, 0));

  // Space syntax
  AbsoluteCase('rgb(255 0 0)', BGRA(255, 0, 0));
  AbsoluteCase('rgb(255 0 0 / 50%)', BGRA(255, 0, 0, 128));
  AbsoluteCase('rgba(255 0 0 / 0.25)', BGRA(255, 0, 0, 64));
  AbsoluteCase('RGB(10 20 30)', BGRA(10, 20, 30));
  AbsoluteCase('rgb(.5 1.5 2.5)', BGRA(1, 2, 3));
  AbsoluteCase('rgb(+.5 +1.5 +2.5 / .3)', BGRA(1, 2, 3, 77));
  AbsoluteCase('rgb(1e+2 1E-1 -1e2)', BGRA(100, 0, 0));
  AbsoluteCase('rgb(10% 30% 50% / 30%)', BGRA(26, 77, 128, 77));
  AbsoluteCase('rgb(1e300 -1e300 0 / 1e300)', BGRA(255, 0, 0));
  AbsoluteCase('  rgb(  10   20 30  )  ', BGRA(10, 20, 30));
  AbsoluteCase('rgb(10'#9'20'#10'30)', BGRA(10, 20, 30));
  AbsoluteCase('rgb(10 20 30/50%)', BGRA(10, 20, 30, 128));

  // Relative syntax: origin returned as text
  RelativeCase('rgb(from red r g b)', 'red', False, 0);
  RelativeCase('rgb(from red r g b / 25%)', 'red', True, 64);
  RelativeCase('rgb(from red r g b / alpha)', 'red', False, 0);
  RelativeCase('rgb(from #0000FF80 r g b)', '#0000FF80', False, 0);
  RelativeCase('rgb(from #0000FF80 r g b / 1)', '#0000FF80', True, 255);
  RelativeCase('rgb(from rgba(1,2,3,0.5) r g b)', 'rgba(1,2,3,0.5)', False, 0);
  RelativeCase('rgb(from rgb(from red r g b / 50%) r g b)',
    'rgb(from red r g b / 50%)', False, 0);
  RelativeCase(Nested(10), Nested(9), False, 0);
  RelativeCase(Nested(32), Nested(31), False, 0);

  // Validated symbolic origins, without resolving their current RGB.
  SymbolicCase('rgb(from ThreeDFace r g b / 50%)', 'ThreeDFace', True, 128);
  SymbolicCase('rgba(from ThreeDFace r g b)', 'ThreeDFace', False, 0);
  SymbolicCase('RGBA(FROM ThreeDFace R G B / ALPHA)', 'ThreeDFace', False, 0);
  SymbolicCase('rgb(from clForm r g b / 50%)', 'clForm', True, 128);
  SymbolicCase('rgb(from default r g b)', 'default', False, 0);
  RejectCase('rgb(from currentColor r g b / 0)');
  RejectCase('rgb(from notacolor r g b)');
  RejectCase('rgb(from ThreeDFacce r g b / 50%)');
  RejectCase('rgb(from Canvas r g b)');
  RejectCase('rgb(from #GG0000 r g b)');
  RejectCase('rgb(from rgb(from notacolor r g b) r g b)');
  RejectCase('rgb(from rgb(1 2) r g b)');

  RelativeCase('rgba(from transparent r g b)', 'transparent', False, 0);
  RelativeCase('rgb(from transparent r g b / 1)', 'transparent', True, 255);
  RelativeCase('rgb(from rgba(1,2,3,.25) r g b / alpha)',
    'rgba(1,2,3,.25)', False, 0);
  AbsoluteCase('rgba(1 2 3)', BGRA(1, 2, 3));
  AbsoluteCase('rgb(1'#12'2'#13'3)', BGRA(1, 2, 3));

  // Accepted comma forms: optional parentheses, suffixes and Pascal numbers.
  AbsoluteCase('rgb(1,2,3)xyz', BGRA(1, 2, 3));
  AbsoluteCase('rgb 1,2,3', BGRA(1, 2, 3));
  AbsoluteCase('rgb (1,2,3)', BGRA(1, 2, 3));
  AbsoluteCase('rgba   (1,2,3,.5) suffix', BGRA(1, 2, 3, 128));
  AbsoluteCase('rgba 1,2,3,0.5', BGRA(1, 2, 3, 128));
  AbsoluteCase('rgb 1,2,3)', BGRA(1, 2, 3));
  AbsoluteCase('rgb($ff,$10,0)', BGRA(255, 16, 0));
  AbsoluteCase('rgb(%11111111,&20,0)', BGRA(255, 16, 0));
  AbsoluteCase('rgb(50.%,0,0)', BGRA(128, 0, 0));
  AbsoluteCase('rgba(1,2,3,1.)', BGRA(1, 2, 3));
  AbsoluteCase('rgba(1,2,3,5.e-1)', BGRA(1, 2, 3, 128));
  AbsoluteCase('  RGB (1,2,3)ignored  ', BGRA(1, 2, 3));
  AbsoluteCase('RGBA 1,2,3,.3', BGRA(1, 2, 3, 77));

  // Invalid
  RejectCase('rgb()');
  RejectCase('rgb(from / r g b)');
  RejectCase('rgb(from red)(blue r g b)');
  RejectCase('rgb(1e9999 0 0)');
  RejectCase('rgb(NaN 0 0)');
  RejectCase('rgb(Inf 0 0)');
  RejectCase('rgb($ff 0 0)');
  RejectCase('rgb(1 2 3 / ?)');
  RejectCase('rgb(from red r g b / none)');
  RejectCase('rgb(from red r g b / calc(.5))');
  RejectCase('');
  RejectCase('rgb (10 20 30)');
  RejectCase('rgb'#9'(10 20 30)');
  RejectCase('rgb(1. 0 0)');
  RejectCase('rgb(1.e2 0 0)');
  RejectCase('rgb(. 0 0)');
  RejectCase('rgb(+ 0 0)');
  RejectCase('rgb(- 0 0)');
  RejectCase('rgb(1e 0 0)');
  RejectCase('rgb(1e+ 0 0)');
  RejectCase('rgb(1e- 0 0)');
  RejectCase('rgb(1..2 0 0)');
  RejectCase('rgb(1e2e3 0 0)');
  RejectCase('rgb(1 2 3 / 1.)');
  RejectCase('rgb(1 2 3 / 1.e2%)');
  RejectCase('rgb(1,2)');
  RejectCase('rgb(1,2,3,4,5)');
  RejectCase('rgb(1 2)');
  RejectCase('rgb(1 2 3 4)');
  RejectCase('rgb(1 2 3 / )');
  RejectCase('rgb(1 2 3 / 50% 1)');
  RejectCase('rgb(1,2,3 / 0.5)');
  RejectCase('rgb(1 , 2 3)');
  RejectCase('rgb(1,2,3');
  RejectCase('rgb((1,2,3)');
  RejectCase('rgbx(1,2,3)');
  RejectCase('rgb(abc,0,0)');
  RejectCase('rgb(none 0 0)');
  RejectCase('rgb(calc(1) 0 0)');
  RejectCase('rgb(1px 0 0)');
  RejectCase('rgb(from red g r b)');
  RejectCase('rgb(from red r g b 50%)');
  RejectCase('rgb(from red r g)');
  RejectCase('rgb(from red r g b / )');
  RejectCase(Nested(33));  // first level beyond the limit
  RejectCase(Nested(34));
  RejectCase(Nested(40));  // nesting limit

  // Partial channels and invalid comma arguments are rejected.
  RejectCase('rgb(255,?,?,?)');
  RejectCase('rgb(255,?,?)');
  RejectCase('rgb(255,,0)');
  RejectCase('rgb(255,abc,0)');
  RejectCase('rgba(1,2,3,x)');
  RejectCase('rgba(?,bad,3,?)');
  RejectCase('rgba(1,2,3,)');
  RejectCase('rgb 255,?,?');
  RejectCase('rgba (255,?,0,?) trailing');
  RejectCase('rgb(255 ? 0)');  // no partial values in the space syntax

end;

type
  { FPCUnit wrapper, so that .github/workflows/make.pas runs this program. }
  TCSSRGBTest = class(TTestCase)
  published
    procedure TestCSSRGB;
  end;

procedure TCSSRGBTest.TestCSSRGB;
begin
  Checks := 0;
  Failures := 0;
  FailedNames := '';
  RunChecks;
  WriteLn(Checks, ' checks, ', Failures, ' failures');
  if Failures <> 0 then
    Fail(IntToStr(Failures) + ' failed checks:' + FailedNames);
end;

begin
  RegisterTest(TCSSRGBTest);
  with TTestRunner.Create(nil) do
  try
    Initialize;
    Run;
  finally
    Free;
  end;
end.
