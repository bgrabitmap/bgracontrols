program test_color_value;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

uses
  Interfaces, Classes, SysUtils, TypInfo, Graphics, LCLType, BGRABitmapTypes, BCColors, BCColorUtils,
  fpcunit, testregistry, consoletestrunner;

type
  TPair = TBCColorValue;

var
  Failures: integer = 0;
  FailedNames: string;

function BCNone: TPair;
begin
  Result := TBCColorValue.New(scNone);
end;

function BCDefault(A: byte = 255): TPair;
begin
  Result := TBCColorValue.New(scDefault, A);
end;

function ColorToBC(C: TColor; A: byte = 255): TPair;
begin
  Result := TBCColorValue.New(C, A);
end;

function BCToColor(const V: TPair): TColor;
begin
  Result := V.ToColor;
end;

function BCSameValue(const V1, V2: TPair): boolean;
begin
  Result := V1 = V2;
end;

function BCSameColor(const V1, V2: TPair): boolean;
begin
  Result := V1.SameColorAs(V2);
end;

function BCToBGRA(const V: TPair; ADefault: TColor = clBlack): TBGRAPixel;
begin
  Result := V.ToBGRA(ADefault);
end;

function SameBytes(const P1, P2: TBGRAPixel): boolean;
begin
  Result := (P1.red = P2.red) and (P1.green = P2.green) and
    (P1.blue = P2.blue) and (P1.alpha = P2.alpha);
end;

procedure Check(ACond: boolean; const AMsg: string);
begin
  if ACond then
    WriteLn('PASS ', AMsg)
  else
  begin
    WriteLn('FAIL ', AMsg);
    Inc(Failures);
    FailedNames := FailedNames + LineEnding + AMsg;
  end;
end;

function Describe(const V: TPair): string;
var
  P: TBGRAPixel;
begin
  P := V.ToBGRA;
  Result := Format('%s %s A=%3d', [GetEnumName(TypeInfo(TBCSymbolicColor),
    Ord(V.Kind)), BGRAToStr(BGRA(P.red, P.green, P.blue), nil, 0, True, True),
    V.Alpha]);
end;

function PixelText(const P: TBGRAPixel): string;
begin
  Result := BGRAToStr(P, nil, 0, False, True);
end;

procedure Show(const S: string);
var
  V: TPair;
begin
  if TryStrToBC(S, V) then
    WriteLn(Format('  %-40s -> %-30s -> %-36s pixel %s',
      [S, Describe(V), BCToStr(V), PixelText(V.ToBGRA(clBtnFace))]))
  else
    WriteLn(Format('  %-40s -> rejected', [S]));
end;

function Parse(const S: string): TPair;
begin
  if not TryStrToBC(S, Result) then
    raise Exception.Create('cannot parse ' + S);
end;

{ The TColor identity survives parsing and text round-trip. }
procedure Nature(const S: string; AColor: TColor);
var
  V, W: TPair;
begin
  if not TryStrToBC(S, V) then
  begin
    Check(False, 'nature ' + S + ': not parsed');
    Exit;
  end;
  Check((BCToColor(V) = AColor) and TryStrToBC(BCToStr(V), W) and BCSameValue(V, W),
    Format('nature %s -> %s -> %s', [S, ColorToString(BCToColor(V)), BCToStr(V)]));
end;

procedure RoundTrip(const S: string);
var
  V, W: TPair;
begin
  V := Parse(S);
  W := Parse(BCToStr(V));
  Check(BCSameValue(V, W), 'round trip ' + S + ' -> ' + BCToStr(V));
end;

{ BGRABitmap reads S as the pixel P. }
function ReadByBGRABitmap(const S: string; const P: TBGRAPixel): boolean;
var
  Q: TBGRAPixel;
  Missing, Error: boolean;
begin
  Q := BGRABlack;
  TryStrToBGRA(S, Q, Missing, Error);
  Result := not (Missing or Error) and
    ((Q = P) or ((Q.alpha = 0) and (P.alpha = 0)));
end;

procedure TestBCParserContract;
var
  V, W: TBCColorValue;
  Kind: TBCSymbolicColor;
  A, Level: integer;
  Text: string;
begin
  for Kind := Low(TBCSymbolicColor) to High(TBCSymbolicColor) do
  begin
    if Kind = scReserved25 then Continue;
    for A := 0 to 255 do
    begin
      V := TBCColorValue.New(Kind, A);
      Text := BCToStr(V);
      if not TryStrToBC(Text, W) or not (V = W) then
        Check(False, 'exact alpha roundtrip: ' + Text + ', alpha=' + IntToStr(A));
    end;
  end;
  { Check rejection of unknown and unsupported relative origins. }
  Check(not TryStrToBC('rgb(from notacolor r g b)', V), 'unknown origin rejected');
  Check(not TryStrToBC('rgb(from ThreeDFacce r g b / 50%)', V),
    'misspelled system origin rejected');
  Check(not TryStrToBC('rgb(from currentColor r g b)', V),
    'context-dependent origin rejected without a context');
  Check(not TryStrToBC('rgb(from Canvas r g b)', V),
    'unsupported modern system role rejected');
  Check(not TryStrToBC('rgb(from rgb(from notacolor r g b) r g b)', V),
    'unknown nested origin rejected');
  Check(not TryStrToBC('rgb (1 2 3)', V), 'modern space syntax rejects whitespace before function parenthesis');
  Check(not TryStrToBC('rgb(1. 0 0)', V), 'trailing decimal point rejected');
  Check(not TryStrToBC('rgb(1.e2 0 0)', V), 'decimal point requires digits');
  Check(not TryStrToBC('rgb(1e+ 0 0)', V), 'exponent requires digits');
  Check(TryStrToBC('rgb(.5 1.5 2.5 / .3)', V) and
    V.ToBGRA.EqualsExactly(BGRA(1, 2, 3, 77)), 'halfway values round toward positive infinity');
  Text := 'ThreeDFace';
  for Level := 1 to 34 do
  begin
    Text := 'rgb(from ' + Text + ' r g b)';
    if Level >= 32 then
      Check(TryStrToBC(Text, V) = (Level = 32),
        'symbolic nesting boundary ' + IntToStr(Level));
  end;
end;

{ All checks; the FPCUnit test below runs them. }
procedure RunChecks;
var
  V, W: TPair;
  A, I: integer;
  S, S2: TBCSymbolicColor;
  C: TColor;
  AllOK: boolean;
  Seen: TStringList;
  K: TBCColor;

begin
  TestBCParserContract;
  WriteLn;
  WriteLn('Text -> value -> text -> pixel now (default = clBtnFace)');
  Show('#FF0000');
  Show('#FF000000');
  Show('#ff00007f');
  Show('#F008');
  Show('#6495ED80');
  Show('cornflowerblue');
  Show('red');
  Show('clRed');
  Show('clSkyBlue');
  Show('grey');
  Show('Cyan');
  Show('rgb(from red r g b / 50%)');
  Show('rgb(255 0 0 / 50%)');
  Show('rgba(255, 0, 0, 0.25)');
  Show('transparent');
  Show('clNone');
  Show('#00000000');
  Show('default');
  Show('clDefault');
  Show('clBtnFace');
  Show('ThreeDFace');
  Show('buttonface');
  Show('clForm');
  Show('clMenuHighlight');
  Show('rgb(from ThreeDFace r g b / 50%)');
  Show('rgb(from clForm r g b / 0.3)');
  Show('rgb(from #FF0000 r g b / 25%)');
  Show('rgb(from default r g b / 50%)');
  Show('rgb(from transparent r g b / 50%)');
  Show('AccentColor');
  Show('Face');
  Show('hsl(0 100% 50%)');
  WriteLn;

  WriteLn('Checks');

  // record layout and construction
  Check(High(TBCSymbolicColor) = scCustomRGB, 'scCustomRGB is the last member');
  V := TBCColorValue.New(scBtnFace, 123);
  Check((V.Kind = scBtnFace) and (V.Alpha = 123) and (BCToColor(V) = clBtnFace) and
    BCSameValue(V, ColorToBC(clBtnFace, 123)), 'New(kind) = New(TColor)');
  V := scBtnFace;
  W := clBtnFace;
  Check(BCSameValue(V, W) and (V.Alpha = 255), 'implicit := from kind and TColor, opaque');
  V := BGRA(1, 2, 3, 4);
  Check((V.Kind = scCustomRGB) and SameBytes(V.ToBGRA, BGRA(1, 2, 3, 4)), 'implicit := from a pixel');
  Check(ColorToBC(TColor($80000040)).Kind = scCustomRGB,
    'unknown symbolic TColor is resolved to RGB (no enum entry)');
  Check(SameBytes(TBCColorValue.New(scCustomRGB, 50).ToBGRA, BGRA(0, 0, 0, 50)),
    'New(scCustomRGB) is black with the opacity');
  Check((BCToColor(BCDefault) = clDefault) and
    (BCToColor(TBCColorValue.New(BGRA(1, 2, 3, 4))) = RGBToColor(1, 2, 3)),
    'value -> TColor');
  V := TBCColorValue.New(scBtnFace, 10);
  V.Alpha := 99;
  Check((V.Kind = scBtnFace) and (V.Alpha = 99), 'Opacity setter keeps the kind');

  // scReserved25: never stored
  V := TBCColorValue.New(scReserved25, 77);
  Check(BCSameValue(V, BCNone) and (V.Kind = scNone), 'New(scReserved25) gives scNone');
  Check(not scReserved25.IsLCL and not scReserved25.TryToColor(C), 'scReserved25 is not an LCL color');
  Check(ColorToBC(TColor(SYS_COLOR_BASE or 25)).Kind = scCustomRGB,
    'SYS_COLOR_BASE or 25 is not looked up as scReserved25');

  // clNone = scNone, always opacity 0
  Check((ColorToBC(clNone, 200).Kind = scNone) and (ColorToBC(clNone, 200).Alpha = 0),
    'clNone -> scNone, opacity 0 whatever was given');
  Check(BCToColor(BCNone) = clNone, 'scNone -> clNone');
  Check(BCSameValue(Parse('transparent'), BCNone) and BCSameValue(Parse('clNone'), BCNone),
    'transparent = clNone = scNone');
  Check((Parse('#00000000').Kind = scCustomRGB) and not BCSameValue(Parse('#00000000'), BCNone) and
    BCSameColor(Parse('#00000000'), BCNone), '#00000000 is a custom value, the same pixel as transparent');
  Check(BCToStr(Parse('#00000000')) = 'rgb(from black r g b / 0%)', '#00000000 is black with alpha 0');
  Check(SameBytes(BCNone.WithAlpha(128).ToBGRA, BGRA(0, 0, 0, 128)) and
    (BCNone.WithAlpha(128).Kind = scCustomRGB), 'scNone with an opacity is black, as in CSS');
  Check(BCSameValue(BCNone.WithAlpha(0), BCNone), 'scNone with opacity 0 stays scNone');
  Check(BCToColor(TBCColorValue.New(BGRA(255, 0, 0, 0))) = clRed, 'a custom alpha 0 keeps its RGB');

  // All LCL system colors, including those without a CSS name.
  AllOK := True;
  for I := 0 to COLOR_FORM do
  begin
    if I = 25 then Continue;
    C := TColor(SYS_COLOR_BASE or I);
    V := ColorToBC(C);
    if not ((BCToColor(V) = C) and V.Kind.IsLCL and (Ord(V.Kind) = I) and
      (Copy(ColorToString(C), 1, 2) = 'cl')) then
    begin
      WriteLn('  system color ', I, ' ', ColorToString(C));
      AllOK := False;
    end;
  end;
  Check(AllOK, '31 system colors convert to and from TColor, each a LCL identifier');
  Check((ColorToBC(clDefault).Kind = scDefault) and scDefault.IsLCL and scNone.IsLCL,
    'clDefault -> scDefault; clNone and clDefault are LCL colors');
  Check(ColorToBC(TColor(SYS_COLOR_BASE or 32)).Kind = scCustomRGB,
    'a system color newer than the enum is resolved to RGB');
  Check(not scCustomRGB.IsLCL and not scCustomRGB.TryToColor(C) and (scCustomRGB.CSSName = ''),
    'scCustomRGB has neither TColor nor CSS name');
  Check(not TryCSSNameToSymbolic('', S), 'empty CSS name rejected');
  Check(scForm.CSSName = '', 'clForm has no CSS name');
  Check(scDefault.CSSName = '', 'scDefault has no CSS name');
  Check((Ord(scBtnFace) = COLOR_BTNFACE) and (Ord(scReserved25) = 25) and
    (Ord(scHotLight) = COLOR_HOTLIGHT) and (Ord(scForm) = COLOR_FORM),
    'enum order follows the COLOR_* indices');

  // Check canonical symbolic names are unique, do not collide with RGB names,
  // and parse back to the same symbolic kind.
  Seen := TStringList.Create;
  try
    Seen.CaseSensitive := False;
    AllOK := True;
    for S := Low(TBCNamedColor) to High(TBCNamedColor) do
    begin
      if S.CSSName = '' then Continue;
      if (Seen.IndexOf(S.CSSName) >= 0) or
        ((S <> scNone) and (CSSColors.IndexOf(S.CSSName) >= 0)) then
      begin
        WriteLn('  duplicate CSS name ', S.CSSName);
        AllOK := False;
      end;
      Seen.Add(S.CSSName);
      if not (TryCSSNameToSymbolic(LowerCase(S.CSSName), S2) and (S2 = S)) then
        AllOK := False;
    end;
    Check(AllOK and (Seen.Count = 26),
      Format('%d unique CSS names (25 LCL, transparent)', [Seen.Count]));
  finally
    Seen.Free;
  end;

  AllOK := True;
  for S := Low(TBCNamedColor) to High(TBCNamedColor) do
    for A in [255, 128, 0] do
    begin
      V := TBCColorValue.New(S, A);
      if not (TryStrToBC(BCToStr(V), W) and BCSameValue(V, W)) then
      begin
        WriteLn('  ', BCToStr(V));
        AllOK := False;
      end;
    end;
  Check(AllOK, 'every named color with alpha 255, 128, 0 round trips through text');

  Check(Parse('ThreeDFace').Kind = scBtnFace, 'ThreeDFace is a system color');
  Check(BCSameValue(Parse('ThreeDFace'), Parse('clBtnFace')), 'ThreeDFace = clBtnFace as value');
  Check(TryStrToBC('ButtonFace', V) and (V.Kind = scBtnFace) and (BCToStr(V) = 'ThreeDFace'),
    'ButtonFace is read as clBtnFace and written as ThreeDFace');
  Check(BCToStr(Parse('threedface')) = 'ThreeDFace', 'names are case-insensitive, written as in the table');
  Check(BCToStr(Parse('clBtnFace')) = 'ThreeDFace', 'clBtnFace is written as ThreeDFace');
  Check(BCToStr(Parse('clHotLight')) = 'clHotLight', 'clHotLight (no exact CSS name) is written clHotLight');
  Check(BCToStr(Parse('clMenuHighlight')) = 'clMenuHighlight', 'clMenuHighlight (no exact CSS name) is written clMenuHighlight');
  Check(not TryStrToBC('LinkText', V), 'LinkText not mapped (no exact LCL equivalent)');
  Check(BCToStr(Parse('clForm')) = 'clForm', 'clForm (no CSS name) is written clForm');
  Check(BCToStr(ColorToBC(clForm, 128)) = 'rgb(from clForm r g b / 50%)', 'clForm with alpha');
  Check(BCToBGRA(Parse('ThreeDFace')) = ColorToBGRA(ColorToRGB(clBtnFace)), 'ThreeDFace resolves to ColorToRGB(clBtnFace)');
  Check(BCToColor(Parse('ThreeDFace')) = clBtnFace, 'ThreeDFace -> TColor clBtnFace (still symbolic)');
  Check(not TryStrToBC('AccentColor', V), 'AccentColor is not mapped to an LCL color');

  // RGB values: names are not stored
  Check(BCSameValue(Parse('red'), Parse('#FF0000')), 'red = #FF0000 as value');
  Check(BCSameValue(Parse('clRed'), Parse('red')), 'clRed = red as value');
  Check(BCSameValue(Parse('aqua'), Parse('cyan')), 'aqua = cyan as value');
  Check(BCSameValue(Parse('grey'), Parse('gray')), 'grey = gray');
  Check(BCSameValue(Parse('DarkSlateGrey'), Parse('darkslategray')), 'DarkSlateGrey = darkslategray');
  Check((Parse('clRed').Kind = scCustomRGB) and
    (BCToColor(Parse('clRed')) = clRed), 'clRed is RGB');
  Check(BCToStr(Parse('#FF0000')) = 'red', 'opaque RGB with a CSS name is written with it');
  Check(BCToStr(Parse('CornflowerBlue')) = 'cornflowerblue', 'named colors are written lowercase, as in CSS');
  Check(BCToStr(Parse('clSkyBlue')) = '#A6CAF0', 'clSkyBlue (no CSS name) -> #A6CAF0');
  Check(BCToStr(Parse('rgb(from red r g b / 50%)')) = 'rgb(from red r g b / 50%)', 'named RGB with alpha keeps the CSS name');
  Check(BCToStr(Parse('#123456')) = '#123456', 'RGB without a name -> #hex');
  Check(BCToStr(Parse('#12345680')) = '#12345680', 'RGB without a name, with alpha -> #RRGGBBAA');
  Check(not BCSameValue(Parse('#FF000000'), Parse('#00000000')), '#FF000000 keeps its RGB (<> #00000000)');
  AllOK := True;
  for I := 0 to CSSColors.Count - 1 do
    if not (TryStrToBC(CSSColors.Name[I], V) and (V.Kind = scCustomRGB) and
      (BCToBGRA(V) = CSSColors[I])) then
    begin
      WriteLn('  mismatch ', CSSColors.Name[I]);
      AllOK := False;
    end;
  Check(AllOK, Format('all %d BGRABitmap CSS names read with the same RGB', [CSSColors.Count]));

  // LCL identifiers
  Check(Parse('clDefault').Kind = scDefault, 'clDefault is scDefault');
  Check(BCToStr(Parse('clNone')) = 'transparent', 'clNone is written transparent');
  Check(BCToStr(Parse('clDefault')) = 'default', 'clDefault is written default');

  // Convert TColor + opacity pairs to values and back to text.
  Check(BCSameValue(ColorToBC(clBtnFace, 128), Parse('rgb(from ThreeDFace r g b / 50%)')), 'old pair clBtnFace + 128 -> system');
  Check(BCToStr(ColorToBC(clBtnFace, 128)) = 'rgb(from ThreeDFace r g b / 50%)', 'old pair -> text');
  Check(BCToStr(ColorToBC(clRed)) = 'red', 'old pair clRed -> red');
  Check(BCToStr(ColorToBC($123456)) = '#563412', 'old pair without a name -> #hex');
  Check(BCToStr(ColorToBC(clDefault, 128)) = 'rgb(from default r g b / 50%)', 'old pair clDefault + 128');
  Check(BCToColor(ColorToBC(clDefault)) = clDefault, 'clDefault -> TColor clDefault');

  // opacity
  V := Parse('rgb(from ThreeDFace r g b / 50%)');
  Check((V.Kind = scBtnFace) and (V.Alpha = 128), 'relative syntax keeps the system color, opacity 128');
  W := V.WithAlpha(64);
  Check((W.Kind = V.Kind) and (W.Alpha = 64), 'changing the opacity keeps the system color');
  Check(BCSameColor(Parse('transparent'), Parse('rgb(from ThreeDFace r g b / 0%)')), 'every alpha 0 is the same pixel');
  Check(BCSameColor(Parse('#FF000000'), Parse('transparent')), '#FF000000 = transparent as pixel');
  Check(not BCSameValue(Parse('#FF000000'), Parse('transparent')), '#FF000000 <> transparent as value');
  Check(BCSameValue(Parse('rgb(from transparent r g b / 50%)'), TBCColorValue.New(BGRA(0, 0, 0, 128))),
    'transparent with an alpha is black, as in CSS');
  AllOK := True;
  for A := 0 to 255 do
  begin
    V := Parse('ThreeDFace').WithAlpha(A);
    W := Parse('#123456').WithAlpha(A);
    if not (BCSameValue(Parse(BCToStr(V)), V) and BCSameValue(Parse(BCToStr(W)), W)) then
    begin
      WriteLn('  alpha ', A, ' -> ', BCToStr(V), ' ', BCToStr(W));
      AllOK := False;
    end;
  end;
  Check(AllOK, 'alpha 0..255 round trip through text (system and RGB)');

  // defaults
  Check(BCToBGRA(BCDefault(128), clNone) = BGRAPixelTransparent,
    'default can resolve to clNone');
  Check(SameBytes(BCToBGRA(BCDefault(128), clDefault), BGRA(0, 0, 0, 128)),
    'unresolved default falls back to black');
  Check(BCToBGRA(Parse('rgb(from default r g b / 50%)'), clWindow) = ColorToBGRA(ColorToRGB(clWindow), 128), 'default resolves to the control default');
  Check(BCSameColor(Parse('rgb(from default r g b / 0%)'), Parse('transparent')), 'default with alpha 0 = transparent');
  Check(not BCSameColor(Parse('default'), Parse('transparent')), 'visible default <> transparent');
  Check(not BCSameColor(Parse('default'), Parse('#000000')), 'visible default <> black');
  Check(BCSameColor(Parse('default'), BCDefault) and not BCSameColor(Parse('default'), BCDefault(128)),
    'two defaults match only with the same opacity');

  // reading channels: the color of now, the value is not changed
  V := TBCColorValue.New(scBtnFace, 128);
  W := V;
  C := ColorToRGB(clBtnFace);
  Check((V.Red = Red(C)) and (V.Green = Green(C)) and (V.Blue = Blue(C)),
    'channels of a system color are its RGB now');
  Check(BCSameValue(V, W) and (V.Kind = scBtnFace) and (V.Alpha = 128),
    'reading a channel does not change the value');
  Check(SameBytes(V.ToBGRA, ColorToBGRA(C, 128)), 'ToBGRA gives the RGB now');
  V := V.WithAlpha(10);
  Check((V.Kind = scBtnFace) and (V.Red = Red(C)) and (V.Alpha = 10),
    'WithAlpha keeps the system color');
  V := BCDefault;
  Check((V.Red = 0) and (V.Green = 0) and (V.Blue = 0), 'scDefault reads black');
  K := TBCColor.Create(clHighlight, 200);
  try
    C := ColorToRGB(clHighlight);
    Check((K.Red = Red(C)) and (K.Blue = Blue(C)) and (K.Kind = scHighlight),
      'TBCColor channels are read without changing the kind');
  finally
    K.Free;
  end;

  // writing channels
  V := TBCColorValue.New(scBtnFace, 128);
  V.Alpha := 40;
  Check((V.Kind = scBtnFace) and (V.Alpha = 40), 'setting Alpha keeps the kind');
  C := ColorToRGB(clBtnFace);
  V.Green := 7;
  Check((V.Kind = scCustomRGB) and (V.Red = Red(C)) and (V.Green = 7) and
    (V.Blue = Blue(C)) and (V.Alpha = 40),
    'setting a channel makes a custom RGB from the resolved color');
  V := TBCColorValue.New(BGRA(1, 2, 3, 4));
  V.Blue := 9;
  Check(SameBytes(V.ToBGRA, BGRA(1, 2, 9, 4)), 'setting a channel of a custom RGB');
  K := TBCColor.Create(clHighlight, 200);
  try
    C := ColorToRGB(clHighlight);
    K.Alpha := 100;
    Check(K.Kind = scHighlight, 'TBCColor: Alpha keeps the kind');
    K.Red := 1;
    Check((K.Kind = scCustomRGB) and (K.Red = 1) and (K.Green = Green(C)) and
      (K.Blue = Blue(C)) and (K.Alpha = 100), 'TBCColor: a channel makes a custom RGB');
  finally
    K.Free;
  end;

  // the nature of the color survives text -> value -> text -> value
  Nature('#FF0000', clRed);
  Nature('rgb(255 0 0)', clRed);
  Nature('red', clRed);
  Nature('clSkyBlue', clSkyBlue);
  Nature('clBtnFace', clBtnFace);
  Nature('ThreeDFace', clBtnFace);
  Nature('clForm', clForm);
  Nature('rgb(from InfoBackground r g b / 50%)', clInfoBk);
  RoundTrip('transparent');
  Check(BCToStr(BCNone) = 'transparent', 'transparent is written transparent');
  Nature('default', clDefault);
  RoundTrip('#12345678');
  RoundTrip('#00000000');
  RoundTrip('rgb(from clInfoBk r g b / 20%)');
  RoundTrip('rgb(from default r g b / 50%)');

  // relative syntax over a function
  Check(BCToStr(Parse('rgb(from rgb(255 0 0) r g b / 50%)')) = 'rgb(from red r g b / 50%)', 'relative syntax over rgb() as base');
  Check(BCToStr(Parse('rgb(from rgb(from ThreeDFace r g b / 10%) r g b / 50%)')) = 'rgb(from ThreeDFace r g b / 50%)', 'relative syntax over a relative base');

  // CSS only
  Check(BCToCSS(Parse('clBtnFace')) = 'ThreeDFace', 'CSS: clBtnFace -> ThreeDFace');
  { Its RGB depends on the theme (and may have a CSS name): compare pixels. }
  Check((Pos('clForm', BCToCSS(Parse('clForm'))) = 0) and
    (Parse(BCToCSS(Parse('clForm'))).Kind = scCustomRGB) and
    BCSameColor(Parse(BCToCSS(Parse('clForm'))), Parse('clForm')),
    'CSS: clForm (no CSS name) -> its RGB: ' + BCToCSS(Parse('clForm')));
  Check(BCToCSS(Parse('clSkyBlue')) = '#A6CAF0', 'CSS: clSkyBlue -> #A6CAF0');
  Check(BCToCSS(Parse('clNone')) = 'transparent', 'CSS: clNone -> transparent');
  Check(BCToCSS(Parse('default'), clWindow) = 'Window', 'CSS: default with clWindow default -> Window');
  Check(BCToCSS(Parse('rgb(from default r g b / 50%)'), clBtnFace) = 'rgb(from ThreeDFace r g b / 50%)', 'CSS: default + alpha -> its default');
  Check(BCToCSS(Parse('default'), clDefault) = 'black', 'CSS: default without a known default -> black');
  Check(BCToCSS(Parse('default'), clNone) = 'transparent', 'CSS: default with clNone default -> transparent');
  AllOK := True;
  for I := 0 to COLOR_FORM do
  begin
    if I = 25 then Continue;
    V := ColorToBC(TColor(SYS_COLOR_BASE or I), 200);
    if (Pos('cl', BCToCSS(V)) > 0) or not (TryStrToBC(BCToCSS(V), W) and BCSameColor(V, W)) then
    begin
      WriteLn('  CSS ', BCToStr(V), ' -> ', BCToCSS(V));
      AllOK := False;
    end;
    if not ReadByBGRABitmap(BCToCSS(V, clWindow, True), BCToBGRA(V, clWindow)) then
    begin
      WriteLn('  not read by BGRABitmap ', BCToStr(V), ' -> ', BCToCSS(V, clWindow, True));
      AllOK := False;
    end;
  end;
  Check(AllOK, 'every system color: CSS text without LCL names, same pixel; resolved text read by BGRABitmap');
  Check(ReadByBGRABitmap(BCToStr(Parse('cornflowerblue')), BGRA($64, $95, $ED)), 'named RGB text is read by BGRABitmap');

  // rejected
  Check(not TryStrToBC('Face', V), 'hex without # rejected (Face is not #FFAACCEE)');
  Check(not TryStrToBC('rgb(NaN 0 0)', V), 'NaN channel rejected');
  Check(not TryStrToBC('rgb(255 0 0 / NaN)', V), 'NaN alpha rejected');
  Check(not TryStrToBC('rgba(255, 0, 0, NaN)', V), 'NaN legacy alpha rejected');
  Check(not TryStrToBC('rgb(Inf 0 0)', V), 'Inf channel rejected');
  Check(not TryStrToBC('rgb(from red r g b / NaN)', V), 'NaN relative alpha rejected');
  Check(not TryStrToBC('rgb(255 0 0 0.5)', V), 'modern syntax without / rejected');
  Check(not TryStrToBC('rgb(255, 0, 0 / 50%)', V), 'mixed syntax rejected');
  Check(not TryStrToBC('rgb(255 0, 0)', V), 'blank inside a legacy value rejected');
  Check(not TryStrToBC('rgb(255,,0,0)', V), 'empty legacy value rejected');
  Check(not TryStrToBC('rgb(from red r g b / 50%', V), 'missing ) rejected');
  Check(not TryStrToBC('rgb(from rgb(255 0 0 r g b)', V), 'unbalanced parentheses rejected');
  Check(not TryStrToBC('rgb(from ThreeDFace calc(r * 0.8) g b)', V), 'calc rejected');
  Check(not TryStrToBC('NotAColor', V), 'unknown name rejected');
  Check(not TryStrToBC('', V), 'empty rejected');

  WriteLn;
  if Failures = 0 then
    WriteLn('ALL PASS')
  else
    WriteLn(Failures, ' FAILURES');
end;

type
  { FPCUnit wrapper, so that .github/workflows/make.pas runs this program. }
  TColorValueTest = class(TTestCase)
  published
    procedure TestColorValue;
  end;

procedure TColorValueTest.TestColorValue;
begin
  Failures := 0;
  FailedNames := '';
  RunChecks;
  if Failures <> 0 then
    Fail(IntToStr(Failures) + ' failed checks:' + FailedNames);
end;

begin
  RegisterTest(TColorValueTest);
  with TTestRunner.Create(nil) do
  try
    Initialize;
    Run;
  finally
    Free;
  end;
end.
