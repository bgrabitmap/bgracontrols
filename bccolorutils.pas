// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Text and CSS forms of BGRAControls colors, and color swatches. }
unit BCColorUtils;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Graphics, Types, BGRABitmap, BGRABitmapTypes, BCColors;

type
  { Distinct types, so their editors do not apply to all strings. }
  TBCColorString = type string;      // '#RRGGBBAA'
  TBCCSSColorString = type string;   // see TryCSSColor

{ 'transparent', 'default', 'red', '#RRGGBB[AA]' (RGB without a CSS name),
  'ThreeDFace', 'clForm' (no CSS name), or 'rgb(from <name> r g b / <alpha>)'
  for any of these names with an alpha below 255. }
function BCToStr(const V: TBCColorValue): string;
{ Valid CSS: LCL names become their RGB now, scDefault becomes ADefault.
  AResolved: always '#hex' or 'transparent'. }
function BCToCSS(const V: TBCColorValue; ADefault: TColor = clBlack;
  AResolved: boolean = False): string;
{ Reads BCToStr, clXxx identifiers, CSS named RGB colors, supported
  historical CSS system names/synonyms and rgb()/rgba(). Additional modern
  CSS roles (Canvas, Field, SelectedItem...) are not mapped to LCL fallbacks.
  RGB functions use BCColors.TryParseCSSRGB, which validates relative origins.
  They are converted recursively here into values, keeping symbolic identity.
  Parenthesis-free historical RGB text is not dispatched
  to that parser. Outputs must not be used on failure. }
function TryStrToBC(const S: string; out V: TBCColorValue): boolean;

{ '#RRGGBB' or '#RRGGBBAA', '#' optional. }
function TryBCColor(const S: string; out P: TBGRAPixel): boolean;
function BCColorToString(const P: TBGRAPixel): string;
procedure DrawBCCheckers(Bmp: TBGRABitmap; const R: TRect);
procedure DrawColor(Canvas: TCanvas; const R: TRect; const P: TBGRAPixel); overload;
{ scDefault with ADefault = clDefault (unknown) is drawn as a hatch. }
procedure DrawColor(Canvas: TCanvas; const R: TRect; const V: TBCColorValue;
  ADefault: TColor = clDefault); overload;

{ CSS text as read by StrToBGRA. FormatCSSColor keeps the style of AStyle. }
function TryCSSColor(const S: string; out P: TBGRAPixel): boolean;
function FormatCSSColor(const P: TBGRAPixel; const AStyle: string): string;

implementation

uses
  LCLType;

const
  KindDefaultName = 'default';

{ Strict hex syntax for symbolic color text: '#' is required and partial
  channels ('?'), accepted by TryStrToBGRA, are not allowed. Like BGRABitmap,
  preserve RGB channels even when alpha is zero. }
function TryHexColor(const S: string; out P: TBGRAPixel): boolean;
var
  I, N: integer;
  Missing, Error: boolean;
begin
  Result := False;
  N := Length(S) - 1;
  if (S = '') or (S[1] <> '#') or not (N in [3, 4, 6, 8]) then Exit;
  for I := 2 to Length(S) do
    if not (S[I] in ['0'..'9', 'a'..'f', 'A'..'F']) then Exit;
  P := BGRABlack;
  TryStrToBGRA(S, P, Missing, Error);
  Result := not (Missing or Error);
end;

{ The CSSColors table stores the 'gray' spelling; accept 'grey' aliases too. }
function TryCSSNamedColor(const AName: string; out P: TBGRAPixel): boolean;
var
  I: integer;
begin
  I := CSSColors.IndexOf(StringReplace(AName, 'grey', 'gray', [rfIgnoreCase]));
  Result := I >= 0;
  if Result then
    P := CSSColors[I];
end;

{ Prefer a whole percentage when it reads back to the same alpha byte;
  otherwise use up to three decimal places. }
function AlphaToStr(A: byte): string;
var
  P: integer;
  Parsed: TBGRAPixel;
  Base, Candidate: string;
  OverrideAlpha: boolean;
  FS: TFormatSettings;
begin
  P := Round(A * 100 / 255);
  Candidate := IntToStr(P) + '%';
  { Use the same BCColors parser as TryRGBFunction to check round-trip accuracy.
    In particular 30% and 70% decode to 77 and 179, not 76 and 178. }
  if TryParseCSSRGB('rgb(0 0 0 / ' + Candidate + ')', Base, Parsed,
    OverrideAlpha) and (Parsed.alpha = A) then
    Exit(Candidate);
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  Result := FormatFloat('0.###', A / 255, FS);
end;

{ BCColors parses RGB syntax and validates the origin. Interpret that origin
  as a value here, preserving a system name rather than resolving its RGB. }
function TryRGBFunction(const S: string; out V: TBCColorValue): boolean;
var
  Base: string;
  P: TBGRAPixel;
  OverrideAlpha: boolean;
begin
  Result := TryParseCSSRGB(S, Base, P, OverrideAlpha);
  if not Result then Exit;
  if Base = '' then
  begin
    V := TBCColorValue.New(P);
    Exit;
  end;
  Result := TryStrToBC(Base, V);
  if Result and OverrideAlpha then
    V := V.WithAlpha(P.alpha);
end;

function NameWithAlpha(const AName: string; AAlpha: byte): string;
begin
  if AAlpha = 255 then
    Result := AName
  else
    Result := 'rgb(from ' + AName + ' r g b / ' + AlphaToStr(AAlpha) + ')';
end;

{ The CSS name of the RGB, whatever the alpha: 'red' or
  'rgb(from red r g b / 50%)'. '#RRGGBB[AA]' when the RGB has no name. }
function RGBToStr(const P: TBGRAPixel): string;
var
  Opaque: TBGRAPixel;
begin
  Opaque := P;
  Opaque.alpha := 255;
  Result := BGRAToStr(Opaque, CSSColors, 0, True, True);
  if (Result = '') or (Result[1] = '#') then
    Exit(BGRAToStr(P, nil, 0, True, True));
  { Keep canonical names lowercase, but preserve uppercase hex digits. }
  Result := NameWithAlpha(LowerCase(Result), P.alpha);
end;

function BCToStr(const V: TBCColorValue): string;
var
  Name: string;
  C: TColor;
begin
  case V.Kind of
    scCustomRGB:
      Exit(RGBToStr(V.ToBGRA));
    scNone:
      Exit('transparent');
    scDefault:
      Exit(NameWithAlpha(KindDefaultName, V.Alpha));
  end;
  Name := V.Kind.CSSName;
  if (Name = '') and V.Kind.TryToColor(C) then
    Name := ColorToString(C);
  Result := NameWithAlpha(Name, V.Alpha);
end;

function BCToCSS(const V: TBCColorValue; ADefault: TColor;
  AResolved: boolean): string;
var
  P: TBGRAPixel;
begin
  if ADefault = clDefault then
    ADefault := clBlack;
  if AResolved or (V.Kind.IsLCL and (V.Kind.CSSName = '') and
    (V.Kind <> scDefault)) then
  begin
    P := V.ToBGRA(ADefault);
    if P.alpha = 0 then
      Exit('transparent');
    if AResolved then
      Exit(BGRAToStr(P, nil, 0, True, True));
    Exit(RGBToStr(P));
  end;
  if V.Kind = scDefault then
    Exit(BCToCSS(TBCColorValue.New(ADefault, V.Alpha), clBlack));
  Result := BCToStr(V);
end;

function TryStrToBC(const S: string; out V: TBCColorValue): boolean;
var
  T: string;
  P: TBGRAPixel;
  C: longint;
  K: TBCSymbolicColor;
begin
  T := Trim(S);
  Result := T <> '';
  if not Result then Exit;

  if SameText(T, KindDefaultName) then
  begin
    V := TBCColorValue.New(scDefault);
    Exit;
  end;
  if TryCSSNameToSymbolic(T, K) then
  begin
    V := TBCColorValue.New(K);
    Exit;
  end;
  if IdentToColor(T, C) then
  begin
    V := TBCColorValue.New(TColor(C));
    Exit;
  end;
  if TryCSSNamedColor(T, P) then
  begin
    V := TBCColorValue.New(P);
    Exit;
  end;
  if Pos('(', T) > 0 then Exit(TryRGBFunction(T, V));

  { '#' required: 'Face' is not #FFAACCEE }
  Result := TryHexColor(T, P);
  if Result then
    V := TBCColorValue.New(P);
end;

function TryBCColor(const S: string; out P: TBGRAPixel): boolean;
var
  T: string;
  I: integer;
begin
  P := BGRABlack;
  Result := False;
  T := Trim(S);
  if (T <> '') and (T[1] = '#') then
    Delete(T, 1, 1);
  if not (Length(T) in [6, 8]) then Exit;

  for I := 1 to Length(T) do
    if not (T[I] in ['0'..'9', 'a'..'f', 'A'..'F']) then Exit;

  Result := TryCSSColor('#' + T, P);
end;

function BCColorToString(const P: TBGRAPixel): string;
begin
  Result := BGRAToStr(P, nil, 0, False, True);
end;

procedure DrawBCCheckers(Bmp: TBGRABitmap; const R: TRect);
begin
  { White first: DrawCheckers starts with its "odd" color. }
  Bmp.DrawCheckers(R, BGRA(192, 192, 192), BGRAWhite, 6, 6);
end;

procedure DrawColor(Canvas: TCanvas; const R: TRect; const P: TBGRAPixel);
var
  Bmp: TBGRABitmap;
  Old: TColor;
begin
  if (R.Right <= R.Left) or (R.Bottom <= R.Top) then Exit;

  if P.alpha = 255 then
  begin
    Old := Canvas.Brush.Color;
    Canvas.Brush.Color := BGRAToColor(P);
    Canvas.FillRect(R);
    Canvas.Brush.Color := Old;
    Exit;
  end;

  Bmp := TBGRABitmap.Create(R.Right - R.Left, R.Bottom - R.Top);
  try
    DrawBCCheckers(Bmp, Rect(0, 0, Bmp.Width, Bmp.Height));
    Bmp.FillRect(0, 0, Bmp.Width, Bmp.Height, P, dmLinearBlend);
    Bmp.Draw(Canvas, R.Left, R.Top, True);
  finally
    Bmp.Free;
  end;
end;

procedure DrawColor(Canvas: TCanvas; const R: TRect; const V: TBCColorValue;
  ADefault: TColor);
var
  Bmp: TBGRABitmap;
  X: integer;
begin
  if (V.Kind <> scDefault) or (ADefault <> clDefault) then
  begin
    DrawColor(Canvas, R, V.ToBGRA(ADefault));
    Exit;
  end;
  if (R.Right <= R.Left) or (R.Bottom <= R.Top) then Exit;

  Bmp :=TBGRABitmap.Create(R.Right - R.Left, R.Bottom - R.Top, BGRAWhite);
  try
    X := -Bmp.Height;
    while X < Bmp.Width do
    begin
      Bmp.DrawLine(X, Bmp.Height, X + Bmp.Height, 0, BGRA(160, 160, 160), True);
      Inc(X, 4);
    end;
    Bmp.Draw(Canvas, R.Left, R.Top, True);
  finally
    Bmp.Free;
  end;
end;

function TryCSSColor(const S: string; out P: TBGRAPixel): boolean;
var
  Missing, Error: boolean;
begin
  P := BGRABlack;
  Result := False;
  if Trim(S) = '' then Exit;

  TryStrToBGRA(Trim(S), P, Missing, Error);
  Result := not (Missing or Error);
end;

function FormatCSSColor(const P: TBGRAPixel; const AStyle: string): string;
var
  T: string;
  FS: TFormatSettings;
begin
  T := LowerCase(Trim(AStyle));
  if Copy(T, 1, 3) = 'rgb' then
  begin
    if P.alpha = 255 then
      Exit(Format('rgb(%d,%d,%d)', [P.red, P.green, P.blue]));

    FS := DefaultFormatSettings;
    FS.DecimalSeparator := '.';
    Exit(Format('rgba(%d,%d,%d,%s)', [P.red, P.green, P.blue,
      FloatToStrF(P.alpha / 255, ffGeneral, 3, 0, FS)]));
  end;
  if (Length(T) = 7) and (T[1] = '#') and (P.alpha = 255) then
    Exit(BGRAToStr(P, nil, 0, True, True));
  Result := BCColorToString(P);
end;

end.
