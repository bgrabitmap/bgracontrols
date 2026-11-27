// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ BGRAControls color values: a named color or a fixed RGBA
  value, with an alpha, and the TPersistent objects that publish one.
  The CSS RGB syntax parser lives here beside the symbolic color table;
  relative origins are validated through BCColorUtils, which also provides
  text conversion and preserves symbolic identity until pixel resolution. }
unit BCColors;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, TypInfo, Graphics, LCLType, BGRABitmapTypes;

type
  TBCSymbolicColor = (
    { LCL system colors: Ord = COLOR_* index.
      Add new LCL system colors at their exact COLOR_* index, before scNone;
      preserve reserved slots. If indices are not contiguous, update SysColor,
      New(TColor) and the LCL subranges instead of relying on enum ordinals. }
    scScrollBar, scBackground, scActiveCaption, scInactiveCaption,
    scMenu, scWindow, scWindowFrame, scMenuText, scWindowText,
    scCaptionText, scActiveBorder, scInactiveBorder, scAppWorkspace,
    scHighlight, scHighlightText, scBtnFace, scBtnShadow, scGrayText,
    scBtnText, scInactiveCaptionText, scBtnHighlight, sc3DDkShadow,
    sc3DLight, scInfoText, scInfoBk,
    scReserved25,   // index 25 is unassigned: never stored
    scHotLight, scGradientActiveCaption, scGradientInactiveCaption,
    scMenuHighlight, scMenuBar, scForm,
    { Insert new LCL system colors here, before scNone, only if their
      COLOR_* indices continue immediately after scForm. Keep every existing
      system-color ordinal unchanged; otherwise add an explicit mapping. }
    { LCL special colors }
    scNone, scDefault,
    { Add CSS symbolic colors here, before scCustomRGB.
      Update name lookup, text export and resolution; do not map a new CSS
      role to an approximate LCL color and lose its identity. }
    { Fixed RGBA, as opposed to a symbolic color. }
    scCustomRGB);   // always last

  TBCLCLSystemColor = Low(TBCSymbolicColor)..Pred(scNone);
  TBCLCLColor       = Low(TBCSymbolicColor)..scDefault;
  TBCNamedColor     = Low(TBCSymbolicColor)..Pred(scCustomRGB);

  { Alpha is always FColor.alpha. The RGB of a named color is never stored:
    every read asks the system (ColorToRGB), so it follows the current theme
    and a repaint is enough after a theme change. Always normalized: scReserved25 is
    never stored, scNone has alpha 0.
    Build it with New: Default(TBCColorValue) is a transparent scScrollBar. }
  TBCColorValue = packed record
  private
    FKind: TBCSymbolicColor;
    FColor: TBGRAPixel;
    function GetChannel(AIndex: Integer): Byte;
    function GetKind: TBCSymbolicColor;
    function GetAlpha: Byte;
    procedure SetAlpha(AValue: Byte);
    procedure SetChannel(AIndex: Integer; AValue: Byte);
  public
    class function New(AKind: TBCSymbolicColor; AAlpha: Byte = 255): TBCColorValue; static; overload;
    class function New(AColor: TColor; AAlpha: Byte = 255): TBCColorValue; static; overload;
    class function New(const APixel: TBGRAPixel): TBCColorValue; static; overload;
    class operator :=(AKind: TBCSymbolicColor): TBCColorValue;
    class operator :=(AColor: TColor): TBCColorValue;
    class operator :=(const APixel: TBGRAPixel): TBCColorValue;

    { Same value: clBtnFace <> its current RGB, clRed = #FF0000. }
    class operator =(const A, B: TBCColorValue): Boolean;
    { Same pixel now; a visible scDefault only matches itself. }
    function SameColorAs(const AOther: TBCColorValue): Boolean;
    { scNone with an alpha > 0 becomes black, as in CSS. }
    function WithAlpha(AAlpha: Byte): TBCColorValue;
    function TryToColor(out AColor: TColor): Boolean;
    function ToColor: TColor;
    { The color of now. ADefault replaces scDefault:
      clNone is transparent, clDefault black. }
    function ToBGRA(ADefault: TColor = clBlack): TBGRAPixel;
    property Kind: TBCSymbolicColor read GetKind;
    { Reading a channel gives the color of now (scDefault as black).
      Setting Alpha keeps the kind; setting a channel turns the color of now
      into scCustomRGB. }
    property Alpha: Byte read GetAlpha write SetAlpha;
    property Red: Byte index 0 read GetChannel write SetChannel;
    property Green: Byte index 1 read GetChannel write SetChannel;
    property Blue: Byte index 2 read GetChannel write SetChannel;
  end;

  TBCSymbolicColorHelper = type helper for TBCSymbolicColor
    function CSSName: string;
    function IsLCL: Boolean;
    function TryToColor(out AColor: TColor): Boolean;
  end;

  TBCGetColor = procedure(out AColor: TColor; out AAlpha: Byte) of object;
  TBCSetColor = procedure(AColor: TColor; AAlpha: Byte) of object;

  { Shared value API, with no published properties or persistence policy.
    Channels behave like TBCColorValue; Kind is read-only. }
  TBCCustomColor = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    function GetChannel(AIndex: Integer): Byte;
    procedure SetChannel(AIndex: Integer; AValue: Byte);
    function GetKind: TBCSymbolicColor;
    function GetAlpha: Byte;
    function GetValue: TBCColorValue;
    procedure SetAlpha(AValue: Byte);
    procedure SetValue(const AValue: TBCColorValue);
  protected
    procedure DoGetValue(out AValue: TBCColorValue); virtual; abstract;
    procedure DoSetValue(const AValue: TBCColorValue); virtual; abstract;
    procedure Changed; virtual;
  public
    procedure Assign(Source: TPersistent); overload; override;
    procedure Assign(const AValue: TBCColorValue); overload;
    procedure Assign(AColor: TColor; AAlpha: Byte = 255); overload;
    procedure Assign(const APixel: TBGRAPixel); overload;
    function Assign(const S: string): Boolean; overload;
    function SameValueAs(const AValue: TBCColorValue): Boolean; overload;
    function SameValueAs(AColor: TColor; AAlpha: Byte): Boolean; overload;
    function SameValueAs(AOther: TBCCustomColor): Boolean; overload;
    function SameColorAs(const AValue: TBCColorValue): Boolean; overload;
    function SameColorAs(AOther: TBCCustomColor): Boolean; overload;
    function ToBGRA(ADefault: TColor = clBlack): TBGRAPixel;
    function ToString: string; override;
    property Value: TBCColorValue read GetValue write SetValue;
    property Red: Byte index 0 read GetChannel write SetChannel;
    property Green: Byte index 1 read GetChannel write SetChannel;
    property Blue: Byte index 2 read GetChannel write SetChannel;
    property Alpha: Byte read GetAlpha write SetAlpha;
    property Kind: TBCSymbolicColor read GetKind;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { Owns the value. LFM stores Color ('#RRGGBBAA') and KindName via
    DefineProperties, not published API, unless equal to the ancestor in an
    inherited form. Unknown kinds load as scDefault. }
  TBCColor = class(TBCCustomColor)
  private
    FValue: TBCColorValue;
    procedure ReadColor(Reader: TReader);
    procedure ReadKindName(Reader: TReader);
    procedure WriteColor(Writer: TWriter);
    procedure WriteKindName(Writer: TWriter);
  protected
    procedure DoGetValue(out AValue: TBCColorValue); override;
    procedure DoSetValue(const AValue: TBCColorValue); override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AColor: TColor = clDefault; AAlpha: Byte = 255); overload;
    constructor Create(const AValue: TBCColorValue); overload;
  end;

  { Forwards to a TColor + Byte pair of the owner, which streams it: declare
    the property "stored False".
    OnChange reports changes made through this adapter only. Direct changes
    to the owner's color/opacity properties are visible when reading the
    adapter, but do not trigger its OnChange event. }
  TBCLinkedColor = class(TBCCustomColor)
  private
    FGetValue: TBCGetColor;
    FSetValue: TBCSetColor;
    function GetColor: TColor;
    procedure SetColor(AValue: TColor);
  protected
    procedure DoGetValue(out AValue: TBCColorValue); override;
    procedure DoSetValue(const AValue: TBCColorValue); override;
  public
    constructor Create(AGetValue: TBCGetColor; ASetValue: TBCSetColor);
    { Legacy pair; Alpha is inherited. Neither property is streamed here. }
    property Color: TColor read GetColor write SetColor;
  end;

type
  TBCSymbolicCSSName = record
    Name: string;
    Kind: TBCSymbolicColor;
  end;

const
  { Supported historical CSS names and synonyms. Names must be unique
    (case-insensitive); Kind may repeat. The FIRST entry for a Kind is its
    canonical output name; subsequent entries are read aliases only.
    This mapping does not guarantee equal browser and LCL pixels.
    Kinds without an entry have no symbolic CSS export (BCToCSS resolves
    system colors without a name to RGB). Do not add approximate modern
    role mappings here: they would silently discard identity on input. }
  BCSymbolicCSSNames: array[0..28] of TBCSymbolicCSSName = (
    (Name: 'Scrollbar';           Kind: scScrollBar),
    (Name: 'Background';          Kind: scBackground),
    (Name: 'ActiveCaption';       Kind: scActiveCaption),
    (Name: 'InactiveCaption';     Kind: scInactiveCaption),
    (Name: 'Menu';                Kind: scMenu),
    (Name: 'Window';              Kind: scWindow),
    (Name: 'WindowFrame';         Kind: scWindowFrame),
    (Name: 'MenuText';            Kind: scMenuText),
    (Name: 'WindowText';          Kind: scWindowText),
    (Name: 'CaptionText';         Kind: scCaptionText),
    (Name: 'ActiveBorder';        Kind: scActiveBorder),
    (Name: 'InactiveBorder';      Kind: scInactiveBorder),
    (Name: 'AppWorkspace';        Kind: scAppWorkspace),
    (Name: 'Highlight';           Kind: scHighlight),
    (Name: 'HighlightText';       Kind: scHighlightText),
    (Name: 'ThreeDFace';          Kind: scBtnFace),
    (Name: 'ButtonFace';          Kind: scBtnFace),
    (Name: 'ButtonShadow';        Kind: scBtnShadow),
    (Name: 'ThreeDShadow';        Kind: scBtnShadow),
    (Name: 'GrayText';            Kind: scGrayText),
    (Name: 'ButtonText';          Kind: scBtnText),
    (Name: 'InactiveCaptionText'; Kind: scInactiveCaptionText),
    (Name: 'ButtonHighlight';     Kind: scBtnHighlight),
    (Name: 'ThreeDHighlight';     Kind: scBtnHighlight),
    (Name: 'ThreeDDarkShadow';    Kind: sc3DDkShadow),
    (Name: 'ThreeDLightShadow';   Kind: sc3DLight),
    (Name: 'InfoText';            Kind: scInfoText),
    (Name: 'InfoBackground';      Kind: scInfoBk),
    (Name: 'transparent';         Kind: scNone));

{ Named color whose CSS name is AName (case-insensitive): the canonical
  names and historical synonyms in BCSymbolicCSSNames.
  Additional modern CSS roles are deliberately not approximated by LCL
  colors: parsing must not silently discard their identity. }
function TryCSSNameToSymbolic(const AName: string; out AKind: TBCSymbolicColor): Boolean;

{* Parses complete rgb()/rgba() values and validates relative origins.
   True means the syntax and origin are supported by BGRAControls.
   Validation does not convert a named system color to its current RGB.
   Supports comma or space separators and rgb(from <color> r g b [/ <alpha>]).
   If ABaseColor is empty, AColor contains the RGBA value and AOverrideAlpha
   is True; omitted absolute alpha defaults to 255. Otherwise ABaseColor is
   the unchanged, validated origin text for the caller to interpret. Only
   AColor.alpha is relevant, and only when AOverrideAlpha is True. Omitted
   relative alpha or / alpha sets AOverrideAlpha=False, preserving origin alpha.
   Numbers and percentages are clamped and rounded to bytes, ties toward +infinity.
   Historical comma syntax remains accepted: spaces before '(', omitted
   parentheses, ignored suffix after ')', Pascal numeric forms and mixed
   numeric/percentage channels. Partial channels are not supported here.
   Channel expressions using calc(), none or relative channels other than
   r g b are not supported. Modern function nesting is limited to 32 levels.
   Outputs must not be used on failure. }
function TryParseCSSRGB(const S: string; out ABaseColor: string;
  out AColor: TBGRAPixel; out AOverrideAlpha: boolean): boolean;

implementation

uses
  BCColorUtils, Math;

{ Private RGB syntax helpers. Origin validation is performed by the public
  TryParseCSSRGB wrapper. }
type
  TCSSRGBStringArray = array of string;

const
  CSSRGBBlanks = [' ', #9, #10, #12, #13];

{ Modern syntax: the outer function must close at the end. }
function CSSRGBBalanced(const S: string): boolean;
var I, Depth: integer;
begin
  Depth := 0;
  for I := 1 to Length(S) do
  begin
    if S[I] = '(' then Inc(Depth)
    else if S[I] = ')' then
    begin
      Dec(Depth);
      if (Depth = 0) and (I <> Length(S)) then Exit(False);
    end;
    if (Depth < 0) or (Depth > 32) then Exit(False);
  end;
  Result := Depth = 0;
end;

function CSSRGBHasTopLevelComma(const S: string): boolean;
var I, Depth: integer;
begin
  Depth := 0;
  for I := 1 to Length(S) do
    if S[I] = '(' then Inc(Depth)
    else if S[I] = ')' then Dec(Depth)
    else if (S[I] = ',') and (Depth = 0) then Exit(True);
  Result := False;
end;

{ Split outside parentheses; a slash is a separate token. }
function CSSRGBSplit(const S: string): TCSSRGBStringArray;
var
  Parts: TCSSRGBStringArray;
  Tok: string;
  I, Depth: integer;

  procedure Push;
  begin
    if Tok = '' then Exit;
    SetLength(Parts, Length(Parts) + 1);
    Parts[High(Parts)] := Tok;
    Tok := '';
  end;

begin
  Parts := nil;
  Tok := '';
  Depth := 0;
  for I := 1 to Length(S) do
  begin
    if S[I] = '(' then Inc(Depth)
    else if S[I] = ')' then Dec(Depth);
    if (Depth = 0) and (S[I] in CSSRGBBlanks) then Push
    else if (Depth = 0) and (S[I] = '/') then
    begin
      Push;
      Tok := '/';
      Push;
    end
    else Tok := Tok + S[I];
  end;
  Push;
  Result := Parts;
end;

{ Shared by CSS numbers and historical Val forms. F must be finite. }
function CSSRGBToByte(F, MaxValue: double): byte;
begin
  { Clamp before scaling so even large finite inputs cannot overflow. }
  if F <= 0 then Result := 0
  else if F >= MaxValue then Result := 255
  else
  begin
    { Store the scaled double before rounding, also on x87 targets where
      intermediate extended precision can otherwise put .3 * 255 below 76.5. }
    F := F * 255 / MaxValue;
    Result := Trunc(F + 0.5);
  end;
end;

{ CSS number/percentage, clamped and quantized to a byte. }
function CSSRGBNumber(const S: string; AIsAlpha: boolean; out AValue: byte): boolean;
var
  T: string;
  F, MaxValue: double;
  Code, I, Start: integer;
  IsPercent, HasDigits: boolean;
begin
  Result := False;
  if S = '' then Exit;
  IsPercent := S[Length(S)] = '%';
  if IsPercent then T := Copy(S, 1, Length(S) - 1) else T := S;
  if T = '' then Exit;
  { CSS number: sign? (digits | digits? '.' digits) exponent? .
    Val alone also accepts Pascal forms such as 1. and 1.e2. }
  I := 1;
  if T[I] in ['+', '-'] then Inc(I);
  Start := I;
  while (I <= Length(T)) and (T[I] in ['0'..'9']) do Inc(I);
  HasDigits := I > Start;
  if (I <= Length(T)) and (T[I] = '.') then
  begin
    Inc(I);
    Start := I;
    while (I <= Length(T)) and (T[I] in ['0'..'9']) do Inc(I);
    if I = Start then Exit;
    HasDigits := True;
  end;
  if not HasDigits then Exit;
  if (I <= Length(T)) and (T[I] in ['e', 'E']) then
  begin
    Inc(I);
    if (I <= Length(T)) and (T[I] in ['+', '-']) then Inc(I);
    Start := I;
    while (I <= Length(T)) and (T[I] in ['0'..'9']) do Inc(I);
    if I = Start then Exit;
  end;
  if I <= Length(T) then Exit;
  Val(T, F, Code); // locale-independent; no thousands separator
  if (Code <> 0) or IsNan(F) or IsInfinite(F) then Exit;
  if IsPercent then MaxValue := 100
  else if AIsAlpha then MaxValue := 1
  else MaxValue := 255;
  AValue := CSSRGBToByte(F, MaxValue);
  Result := True;
end;

{ Parse modern RGB syntax, optionally retaining missing comma channels. }
function CSSRGBParseSyntax(const S: string; AAllowMissing: boolean;
  out ABaseColor: string; var AColor: TBGRAPixel;
  out AOverrideAlpha, AMissing: boolean): boolean;
var
  T, Inner, Name: string;
  Args: TCSSRGBStringArray;
  P, I, Start: integer;
  Comma: boolean;

  function Arg(AIndex: integer; AIsAlpha: boolean; var AValue: byte): boolean;
  begin
    Result := True;
    if Comma and AAllowMissing and ((Args[AIndex] = '') or (Args[AIndex] = '?')) then
      AMissing := True
    else if not CSSRGBNumber(Args[AIndex], AIsAlpha, AValue) then
    begin
      if AIsAlpha then AValue := 255 else AValue := 0;
      Result := False;
    end;
  end;

begin
  Result := False;
  ABaseColor := '';
  AOverrideAlpha := True;
  AMissing := False;
  T := Trim(S);
  P := Pos('(', T);
  if (P = 0) or (T[Length(T)] <> ')') then Exit;
  Name := Copy(T, 1, P - 1);
  if not (SameText(Name, 'rgb') or SameText(Name, 'rgba')) then Exit;
  Inner := Copy(T, P + 1, Length(T) - P - 1);
  { Count the outer function too: at most 32 nested functions. }
  if not CSSRGBBalanced(T) then Exit;
  Comma := CSSRGBHasTopLevelComma(Inner);
  if Comma then
  begin
    Args := nil;
    Start := 1;
    for I := 1 to Length(Inner) + 1 do
      if (I > Length(Inner)) or (Inner[I] = ',') then
      begin
        SetLength(Args, Length(Args) + 1);
        Args[High(Args)] := Trim(Copy(Inner, Start, I - Start));
        Start := I + 1;
      end;
    if not (Length(Args) in [3, 4]) then Exit;
    Result := True;
    if Length(Args) = 4 then
      Result := Arg(3, True, AColor.alpha)
    else
      AColor.alpha := 255;
  end
  else
  begin
    Args := CSSRGBSplit(Inner);
    if (Length(Args) >= 5) and SameText(Args[0], 'from') then
    begin
      if not (SameText(Args[2], 'r') and SameText(Args[3], 'g') and
        SameText(Args[4], 'b')) then Exit;
      { The public wrapper validates the origin; '/' is a separator here. }
      if Args[1] = '/' then Exit;
      ABaseColor := Args[1];
      AOverrideAlpha := False;
      if Length(Args) = 5 then Exit(True);
      if (Length(Args) <> 7) or (Args[5] <> '/') then Exit;
      if SameText(Args[6], 'alpha') then Exit(True);
      AOverrideAlpha := True;
      Exit(CSSRGBNumber(Args[6], True, AColor.alpha));
    end;
    case Length(Args) of
      3: AColor.alpha := 255;
      5: if (Args[3] <> '/') or not CSSRGBNumber(Args[4], True, AColor.alpha) then Exit;
    else
      Exit;
    end;
    Result := True;
  end;
  { every channel is read, even after an error }
  if not Arg(0, False, AColor.red) then Result := False;
  if not Arg(1, False, AColor.green) then Result := False;
  if not Arg(2, False, AColor.blue) then Result := False;
end;

{ Historical comma syntax, including optional parentheses and Val forms. }
function CSSRGBLegacyParse(const S: string; var AColor: TBGRAPixel;
  out AMissing: boolean): boolean;
var
  Args: TCSSRGBStringArray;
  P, Start, I: integer;

  function ReadValue(const Text: string; IsAlpha: boolean; var Value: byte): boolean;
  var
    Number: string;
    Percent: boolean;
    Code, IntValue: integer;
    F, MaxValue: double;
  begin
    Result := True;
    if (Text = '') or (Text = '?') then
    begin
      AMissing := True;
      Exit;
    end;
    if CSSRGBNumber(Text, IsAlpha, Value) then Exit;
    Number := Text;
    Percent := Number[Length(Number)] = '%';
    if Percent then Delete(Number, Length(Number), 1);
    if Percent or IsAlpha then
      Val(Number, F, Code)
    else
    begin
      Val(Number, IntValue, Code);
      if Code = 0 then F := IntValue;
    end;
    Result := Code = 0;
    if Result then Result := not (IsNan(F) or IsInfinite(F));
    if not Result then
    begin
      if IsAlpha then Value := 255 else Value := 0;
      Exit;
    end;
    if Percent then MaxValue := 100
    else if IsAlpha then MaxValue := 1
    else MaxValue := 255;
    Value := CSSRGBToByte(F, MaxValue);
  end;

begin
  Result := False;
  AMissing := False;
  if not ((Copy(S, 1, 4) = 'rgb(') or (Copy(S, 1, 5) = 'rgba(') or
    (Copy(S, 1, 4) = 'rgb ') or (Copy(S, 1, 5) = 'rgba ')) then Exit;
  Args := nil;
  P := Pos('(', S);
  if P = 0 then Start := Pos(' ', S) + 1 else Start := P + 1;
  I := Start;
  while I <= Length(S) do
  begin
    if S[I] in [',', ')'] then
    begin
      SetLength(Args, Length(Args) + 1);
      Args[High(Args)] := Trim(Copy(S, Start, I - Start));
      Start := I + 1;
      if S[I] = ')' then Break;
    end;
    Inc(I);
  end;
  Result := (P = 0) or (I <= Length(S));
  if (I > Length(S)) and (Start <= Length(S)) then
  begin
    SetLength(Args, Length(Args) + 1);
    Args[High(Args)] := Copy(S, Start, Length(S) - Start + 1);
  end;
  if not (Length(Args) in [3, 4]) then Exit(False);
  if not ReadValue(Args[0], False, AColor.red) then Result := False;
  if not ReadValue(Args[1], False, AColor.green) then Result := False;
  if not ReadValue(Args[2], False, AColor.blue) then Result := False;
  if Length(Args) = 4 then
  begin
    if not ReadValue(Args[3], True, AColor.alpha) then Result := False;
  end
  else AColor.alpha := 255;
end;

{ Modern syntax with historical comma-syntax fallback. }
function CSSRGBParse(const S: string; AAllowMissing: boolean;
  out ABaseColor: string; var AColor: TBGRAPixel;
  out AOverrideAlpha, AMissing: boolean): boolean;
var
  InitialColor: TBGRAPixel;
begin
  InitialColor := AColor;
  Result := CSSRGBParseSyntax(S, AAllowMissing, ABaseColor, AColor,
    AOverrideAlpha, AMissing);
  if Result then Exit;
  AColor := InitialColor;
  ABaseColor := '';
  AOverrideAlpha := True;
  Result := CSSRGBLegacyParse(LowerCase(Trim(S)), AColor, AMissing);
  if AMissing and not AAllowMissing then Result := False;
end;

{ Validate nested origins with the private syntax parser, then let TryStrToBC
  recognize the leaf (CSS/LCL names, default, transparent or hex). Do not call
  TryStrToBC on a function here: it calls the public parser again. No ToBGRA
  conversion is needed, so supported system names keep their identity. }
function CSSRGBOriginValid(const S: string): boolean;
var
  Origin, Base: string;
  P: TBGRAPixel;
  V: TBCColorValue;
  OverrideAlpha, Missing: boolean;
begin
  Origin := S;
  while Pos('(', Origin) > 0 do
  begin
    P := BGRABlack;
    if not CSSRGBParse(Origin, False, Base, P, OverrideAlpha, Missing) then
      Exit(False);
    if Base = '' then Exit(True);
    Origin := Base;
  end;
  Result := TryStrToBC(Origin, V);
end;

function TryParseCSSRGB(const S: string; out ABaseColor: string;
  out AColor: TBGRAPixel; out AOverrideAlpha: boolean): boolean;
var
  Missing: boolean;
begin
  AColor := BGRABlack;
  Result := CSSRGBParse(S, False, ABaseColor, AColor, AOverrideAlpha, Missing);
  if Result and (ABaseColor <> '') then
    Result := CSSRGBOriginValid(ABaseColor);
end;

{ Symbolic lookup and color values (not part of the syntax parser). }

function SysColor(AKind: TBCSymbolicColor): TColor;
begin
  Result := TColor(TColorRef(SYS_COLOR_BASE) or TColorRef(Ord(AKind)));
end;

function TryCSSNameToSymbolic(const AName: string; out AKind: TBCSymbolicColor): Boolean;
var
  I: Integer;
begin
  Result := AName <> '';
  if not Result then Exit;
  for I := Low(BCSymbolicCSSNames) to High(BCSymbolicCSSNames) do
    if SameText(BCSymbolicCSSNames[I].Name, AName) then
    begin
      AKind := BCSymbolicCSSNames[I].Kind;
      Exit;
    end;
  Result := False;
end;

{ TBCColorValue }

function TBCColorValue.GetKind: TBCSymbolicColor;
begin
  Result := FKind;
end;

function TBCColorValue.GetAlpha: Byte;
begin
  Result := FColor.alpha;
end;

function TBCColorValue.GetChannel(AIndex: Integer): Byte;
var
  P: TBGRAPixel;
begin
  P := ToBGRA;
  case AIndex of
    0: Result := P.red;
    1: Result := P.green;
  else
    Result := P.blue;
  end;
end;

procedure TBCColorValue.SetChannel(AIndex: Integer; AValue: Byte);
var
  P: TBGRAPixel;
begin
  P := ToBGRA;
  case AIndex of
    0: P.red := AValue;
    1: P.green := AValue;
  else
    P.blue := AValue;
  end;
  Self := New(P);
end;

procedure TBCColorValue.SetAlpha(AValue: Byte);
begin
  Self := WithAlpha(AValue);
end;

class function TBCColorValue.New(AKind: TBCSymbolicColor; AAlpha: Byte): TBCColorValue;
begin
  if AKind = scReserved25 then
    AKind := scNone;

  if AKind = scNone then
    AAlpha := 0;

  Result.FKind := AKind;
  Result.FColor := BGRA(0, 0, 0, AAlpha);
end;

class function TBCColorValue.New(AColor: TColor; AAlpha: Byte): TBCColorValue;
var
  Index: TColorRef;
begin
  if AColor = clNone then
    Exit(New(scNone, AAlpha));

  if AColor = clDefault then
    Exit(New(scDefault, AAlpha));

  if (TColorRef(AColor) and $FF000000) = TColorRef(SYS_COLOR_BASE) then
  begin
    Index := TColorRef(AColor) and $00FFFFFF;
    if (Index <= Ord(High(TBCLCLSystemColor))) and (Index <> Ord(scReserved25)) then
      Exit(New(TBCSymbolicColor(Index), AAlpha));
  end;
  { other colors, newer LCL system colors included: their RGB now }
  Result := New(ColorToBGRA(AColor, AAlpha));
end;

class function TBCColorValue.New(const APixel: TBGRAPixel): TBCColorValue;
begin
  Result.FKind := scCustomRGB;
  Result.FColor := APixel;
end;

class operator TBCColorValue.:=(AKind: TBCSymbolicColor): TBCColorValue;
begin
  Result := New(AKind);
end;

class operator TBCColorValue.:=(AColor: TColor): TBCColorValue;
begin
  Result := New(AColor);
end;

class operator TBCColorValue.:=(const APixel: TBGRAPixel): TBCColorValue;
begin
  Result := New(APixel);
end;

class operator TBCColorValue.=(const A, B: TBCColorValue): Boolean;
begin
  if (A.FKind <> B.FKind) or (A.FColor.alpha <> B.FColor.alpha) then Exit(False);

  Result := (A.FKind <> scCustomRGB) or A.FColor.EqualsExactly(B.FColor);
end;

function TBCColorValue.SameColorAs(const AOther: TBCColorValue): Boolean;
var
  D1, D2: Boolean;
  P1, P2: TBGRAPixel;
begin
  D1 := FKind = scDefault;
  D2 := AOther.FKind = scDefault;
  if D1 and D2 then
    Exit(FColor.alpha = AOther.FColor.alpha);

  P1 := ToBGRA;
  P2 := AOther.ToBGRA;
  if (P1.alpha = 0) and (P2.alpha = 0) then
    Exit(True);

  if D1 or D2 then
    Exit(False);

  Result := P1.EqualsExactly(P2);
end;

function TBCColorValue.WithAlpha(AAlpha: Byte): TBCColorValue;
begin
  if (FKind = scNone) and (AAlpha > 0) then
    Exit(New(scCustomRGB, AAlpha));

  Result := Self;
  if FKind <> scNone then
    Result.FColor.alpha := AAlpha;
end;

function TBCColorValue.TryToColor(out AColor: TColor): Boolean;
begin
  if FKind = scCustomRGB then
  begin
    AColor := BGRAToColor(FColor);
    Result := True;
  end else
    Result := FKind.TryToColor(AColor);
end;

function TBCColorValue.ToColor: TColor;
begin
  if not TryToColor(Result) then
    Result := clNone;
end;

function TBCColorValue.ToBGRA(ADefault: TColor): TBGRAPixel;
begin
  case FKind of
    scCustomRGB: Exit(FColor);
    scNone, scReserved25: Exit(BGRAPixelTransparent);
    scDefault:
    begin
      if ADefault = clNone then Exit(BGRAPixelTransparent);
      if ADefault = clDefault then
        ADefault := clBlack;

      Exit(New(ADefault, FColor.alpha).ToBGRA(clBlack));
    end;
  end;
  { Resolve only here: the stored value keeps its symbolic identity. }
  Result := ColorToBGRA(SysColor(FKind), FColor.alpha);
end;

{ TBCSymbolicColorHelper }

function TBCSymbolicColorHelper.CSSName: string;
var
  I: Integer;
begin
  for I := Low(BCSymbolicCSSNames) to High(BCSymbolicCSSNames) do
    if BCSymbolicCSSNames[I].Kind = Self then
      Exit(BCSymbolicCSSNames[I].Name);

  Result := '';
end;

function TBCSymbolicColorHelper.IsLCL: Boolean;
begin
  Result := (Self <= High(TBCLCLColor)) and (Self <> scReserved25);
end;

function TBCSymbolicColorHelper.TryToColor(out AColor: TColor): Boolean;
begin
  Result := IsLCL;
  if not Result then Exit;
  case Self of
    scNone: AColor := clNone;
    scDefault: AColor := clDefault;
  else
    AColor := SysColor(Self);
  end;
end;

{ TBCCustomColor }

procedure TBCCustomColor.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TBCCustomColor.GetValue: TBCColorValue;
begin
  DoGetValue(Result);
end;

function TBCCustomColor.GetChannel(AIndex: Integer): Byte;
var
  V: TBCColorValue;
begin
  V := GetValue;
  case AIndex of
    0: Result := V.Red;
    1: Result := V.Green;
  else
    Result := V.Blue;
  end;
end;

procedure TBCCustomColor.SetChannel(AIndex: Integer; AValue: Byte);
var
  V: TBCColorValue;
begin
  V := GetValue;
  case AIndex of
    0: V.Red := AValue;
    1: V.Green := AValue;
  else
    V.Blue := AValue;
  end;
  Assign(V);
end;

procedure TBCCustomColor.SetValue(const AValue: TBCColorValue);
begin
  Assign(AValue);
end;

function TBCCustomColor.GetKind: TBCSymbolicColor;
begin
  Result := GetValue.Kind;
end;

function TBCCustomColor.GetAlpha: Byte;
begin
  Result := GetValue.Alpha;
end;

procedure TBCCustomColor.SetAlpha(AValue: Byte);
begin
  Assign(GetValue.WithAlpha(AValue));
end;

procedure TBCColor.ReadKindName(Reader: TReader);
var
  Index: Integer;
begin
  Index := GetEnumValue(TypeInfo(TBCSymbolicColor), Reader.ReadIdent);
  if Index < 0 then
    Assign(TBCColorValue.New(scDefault, Alpha))
  else if (TBCSymbolicColor(Index) <> scCustomRGB) or (Kind <> scCustomRGB) then
    Assign(TBCColorValue.New(TBCSymbolicColor(Index), Alpha));
end;

procedure TBCColor.WriteKindName(Writer: TWriter);
begin
  Writer.WriteIdent(GetEnumName(TypeInfo(TBCSymbolicColor), Ord(GetKind)));
end;

{ The stored TBGRAPixel as '#RRGGBBAA', alpha included. }
procedure TBCColor.ReadColor(Reader: TReader);
var
  S: string;
  P: TBGRAPixel;
  Missing, Error: Boolean;
begin
  S := Reader.ReadString;
  P := BGRABlack;
  TryStrToBGRA(S, P, Missing, Error);
  if Missing or Error then
    raise EReadError.CreateFmt('Invalid color: %s', [S]);
  Assign(TBCColorValue.New(P));
end;

{ The stored pixel: a named color keeps RGB 0 there (resolved only when
  drawn), so the LFM does not depend on the theme it was saved with. }
procedure TBCColor.WriteColor(Writer: TWriter);
begin
  Writer.WriteString(BGRAToStr(FValue.FColor, nil, 0, False, True));
end;

{ Always written, except in an inherited form when equal to the ancestor. }
procedure TBCColor.DefineProperties(Filer: TFiler);
var
  HasData: Boolean;
begin
  inherited DefineProperties(Filer);
  HasData := not (Filer.Ancestor is TBCCustomColor) or
    not SameValueAs(TBCCustomColor(Filer.Ancestor));
  Filer.DefineProperty('Color', @ReadColor, @WriteColor, HasData);
  Filer.DefineProperty('KindName', @ReadKindName, @WriteKindName, HasData);
end;

procedure TBCCustomColor.Assign(Source: TPersistent);
begin
  if Source is TBCCustomColor then
    Assign(TBCCustomColor(Source).Value)
  else
    inherited Assign(Source);
end;

procedure TBCCustomColor.Assign(const AValue: TBCColorValue);
begin
  if GetValue = AValue then Exit;
  DoSetValue(AValue);
  Changed;
end;

procedure TBCCustomColor.Assign(AColor: TColor; AAlpha: Byte);
begin
  Assign(TBCColorValue.New(AColor, AAlpha));
end;

procedure TBCCustomColor.Assign(const APixel: TBGRAPixel);
begin
  Assign(TBCColorValue.New(APixel));
end;

function TBCCustomColor.Assign(const S: string): Boolean;
var
  V: TBCColorValue;
begin
  Result := TryStrToBC(S, V);
  if Result then
    Assign(V);
end;

function TBCCustomColor.SameValueAs(const AValue: TBCColorValue): Boolean;
begin
  Result := GetValue = AValue;
end;

function TBCCustomColor.SameValueAs(AColor: TColor; AAlpha: Byte): Boolean;
begin
  Result := SameValueAs(TBCColorValue.New(AColor, AAlpha));
end;

function TBCCustomColor.SameValueAs(AOther: TBCCustomColor): Boolean;
begin
  Result := SameValueAs(AOther.Value);
end;

function TBCCustomColor.SameColorAs(const AValue: TBCColorValue): Boolean;
begin
  Result := GetValue.SameColorAs(AValue);
end;

function TBCCustomColor.SameColorAs(AOther: TBCCustomColor): Boolean;
begin
  Result := SameColorAs(AOther.Value);
end;

function TBCCustomColor.ToBGRA(ADefault: TColor): TBGRAPixel;
begin
  Result := GetValue.ToBGRA(ADefault);
end;

function TBCCustomColor.ToString: string;
begin
  Result := BCToStr(GetValue);
end;

{ TBCColor }

constructor TBCColor.Create(AColor: TColor; AAlpha: Byte);
begin
  Create(TBCColorValue.New(AColor, AAlpha));
end;

constructor TBCColor.Create(const AValue: TBCColorValue);
begin
  inherited Create;
  FValue := AValue;
end;

procedure TBCColor.DoGetValue(out AValue: TBCColorValue);
begin
  AValue := FValue;
end;

procedure TBCColor.DoSetValue(const AValue: TBCColorValue);
begin
  FValue := AValue;
end;

{ TBCLinkedColor }

constructor TBCLinkedColor.Create(AGetValue: TBCGetColor; ASetValue: TBCSetColor);
begin
  inherited Create;
  if not Assigned(AGetValue) or not Assigned(ASetValue) then
    raise EPropertyError.Create('TBCLinkedColor: both callbacks are required');

  FGetValue := AGetValue;
  FSetValue := ASetValue;
end;

procedure TBCLinkedColor.DoGetValue(out AValue: TBCColorValue);
var
  C: TColor;
  A: Byte;
begin
  FGetValue(C, A);
  AValue := TBCColorValue.New(C, A);
end;

procedure TBCLinkedColor.DoSetValue(const AValue: TBCColorValue);
begin
  FSetValue(AValue.ToColor, AValue.Alpha);
end;

function TBCLinkedColor.GetColor: TColor;
begin
  Result := Value.ToColor;
end;

procedure TBCLinkedColor.SetColor(AValue: TColor);
begin
  Assign(AValue, Alpha);
end;

end.
