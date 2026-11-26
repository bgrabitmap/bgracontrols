// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Object Inspector editors for TBCColorString, TBCCustomColor, TBCPixel and
  CSS color text: a swatch and the RGBA color picker. }

unit BCColorPropertyEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Types, Graphics, PropEdits, BGRABitmapTypes,
  BCColorUtils, BCColors, BCColorPicker, BCTypes;

type

  { Typed text normalized to '#RRGGBBAA'. }
  TBCColorStringPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure SetValue(const NewValue: ansistring); override;
    procedure Edit; override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      AState: TPropEditDrawState); override;
  end;

  { CSS text, kept as typed (empty allowed). }
  TBCCSSColorPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure SetValue(const NewValue: ansistring); override;
    procedure Edit; override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      AState: TPropEditDrawState); override;
  end;

  { Any TBCCustomColor: shown as BCToStr, changed only with the picker. }
  TBCColorPropertyEditor = class(TClassPropertyEditor)
  private
    procedure Apply(const V: TBCColorValue);
    function ColorAt(Index: integer): TBCCustomColor;
  public
    function AllEqual: boolean; override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      AState: TPropEditDrawState); override;
    procedure RevertToInherited; override;
  end;

  { TBCPixel: '#RRGGBBAA', changed only with the picker. }
  TBCPixelPropertyEditor = class(TClassPropertyEditor)
  private
    function PixelAt(Index: integer): TBCPixel;
  public
    function AllEqual: boolean; override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      AState: TPropEditDrawState); override;
    procedure RevertToInherited; override;
  end;

procedure Register;

implementation

uses
  BGRASVGTheme, BCComboBox, BCCheckComboBox, BGRAShape, BGRAKnob;

type
  TBCPixelAccess = class(TBCPixel);

{ Left part of R; R keeps the rest. }
function SwatchRect(var R: TRect): TRect;
begin
  Result := R;
  Result.Right := R.Left + (R.Bottom - R.Top) - 2;
  if Result.Right > R.Right then
    Result.Right := R.Right;

  if Result.Right < R.Left then
    Result.Right := R.Left;
  R.Left := Result.Right;

  Inc(Result.Left);
  Inc(Result.Top);
  Dec(Result.Right);
  Dec(Result.Bottom, 3);
end;

procedure FrameSwatch(ACanvas: TCanvas; const R: TRect);
var
  LPen: TColor;
  LPenStyle: TPenStyle;
  LPenWidth: integer;
  LBrushStyle: TBrushStyle;
begin
  if (R.Right <= R.Left) or (R.Bottom <= R.Top) then Exit;
  LPenWidth := ACanvas.Pen.Width;
  LPen := ACanvas.Pen.Color;
  LPenStyle := ACanvas.Pen.Style;
  LBrushStyle := ACanvas.Brush.Style;
  try
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Width := 1;
    ACanvas.Pen.Color := clWindowText;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(R);
  finally
    ACanvas.Brush.Style := LBrushStyle;
    ACanvas.Pen.Width := LPenWidth;
    ACanvas.Pen.Style := LPenStyle;
    ACanvas.Pen.Color := LPen;
  end;
end;

procedure DrawSwatch(ACanvas: TCanvas; var R: TRect; const P: TBGRAPixel); overload;
var
  S: TRect;
begin
  S := SwatchRect(R);
  DrawColor(ACanvas, S, P);
  FrameSwatch(ACanvas, S);
end;

procedure DrawSwatch(ACanvas: TCanvas; var R: TRect;
  const V: TBCColorValue); overload;
var
  S: TRect;
begin
  S := SwatchRect(R);
  DrawColor(ACanvas, S, V, clDefault);
  FrameSwatch(ACanvas, S);
end;

{ TBCColorStringPropertyEditor }

function TBCColorStringPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paCustomDrawn];
end;

procedure TBCColorStringPropertyEditor.SetValue(const NewValue: ansistring);
var
  P: TBGRAPixel;
begin
  if not TryBCColor(NewValue, P) then
    raise EPropertyError.Create('Expected #RRGGBB or #RRGGBBAA');
  inherited SetValue(BCColorToString(P));
end;

{ Unchanged for every selected component: nothing to set, the form stays
  unmodified. }
procedure TBCColorStringPropertyEditor.Edit;
var
  P: TBGRAPixel;
  S: string;
begin
  if not TryBCColor(GetStrValue, P) then
    P := BGRABlack;

  if not ExecuteBCColorPicker(P) then Exit;

  S := BCColorToString(P);
  if AllEqual and (S = GetStrValue) then Exit;
  SetValue(S);
end;

procedure TBCColorStringPropertyEditor.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; AState: TPropEditDrawState);
var
  R: TRect;
  P: TBGRAPixel;
begin
  R := ARect;
  if AllEqual and TryBCColor(GetStrValue, P) then
    DrawSwatch(ACanvas, R, P);
    
  inherited PropDrawValue(ACanvas, R, AState);
end;

{ TBCCSSColorPropertyEditor }

function TBCCSSColorPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paCustomDrawn];
end;

procedure TBCCSSColorPropertyEditor.SetValue(const NewValue: ansistring);
var
  P: TBGRAPixel;
begin
  if (Trim(NewValue) <> '') and not TryCSSColor(NewValue, P) then
    raise EPropertyError.Create('Expected #RRGGBBAA, rgba(r,g,b,a) or a color name');
  inherited SetValue(Trim(NewValue));
end;

procedure TBCCSSColorPropertyEditor.Edit;
var
  S: string;
begin
  S := GetStrValue;
  { all selected components, not only the first one }
  if ExecuteBCColorPicker(S) and not (AllEqual and (S = GetStrValue)) then
    SetValue(S);
end;

procedure TBCCSSColorPropertyEditor.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; AState: TPropEditDrawState);
var
  R: TRect;
  P: TBGRAPixel;
begin
  R := ARect;
  if AllEqual and TryCSSColor(GetStrValue, P) then
    DrawSwatch(ACanvas, R, P);
  inherited PropDrawValue(ACanvas, R, AState);
end;

{ TBCColorPropertyEditor }

function TBCColorPropertyEditor.ColorAt(Index: integer): TBCCustomColor;
begin
  Result := TBCCustomColor(GetObjectValueAt(Index, TBCCustomColor));
end;

{ Modified only when a selected color really changes. }
procedure TBCColorPropertyEditor.Apply(const V: TBCColorValue);
var
  I: integer;
  C: TBCCustomColor;
  Changed: boolean;
begin
  Changed := False;
  for I := 0 to PropCount - 1 do
  begin
    C := ColorAt(I);
    if (C = nil) or C.SameValueAs(V) then Continue;

    C.Assign(V);
    Changed := True;
  end;
  if Changed then Modified;
end;

function TBCColorPropertyEditor.AllEqual: boolean;
var
  I: integer;
  First, C: TBCCustomColor;
begin
  Result := False;
  First := ColorAt(0);
  if First = nil then Exit;

  for I := 1 to PropCount - 1 do
  begin
    C := ColorAt(I);
    if (C = nil) or (not C.SameValueAs(First)) then Exit;
  end;
  Result := True;
end;

procedure TBCColorPropertyEditor.Edit;
var
  C: TBCCustomColor;
  T: TBCColor;
begin
  C := ColorAt(0);
  if C = nil then Exit;

  T :=TBCColor.Create;
  try
    T.Assign(C);
    if ExecuteBCColorPicker(T) then Apply(T.Value);
  finally
    T.Free;
  end;
end;

function TBCColorPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paCustomDrawn, paReadOnly, paRevertable];
end;

function TBCColorPropertyEditor.GetValue: ansistring;
var
  C: TBCCustomColor;
begin
  C := ColorAt(0);
  if C = nil then
    Result := ''
  else
    Result := C.ToString;
end;

procedure TBCColorPropertyEditor.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; AState: TPropEditDrawState);
var
  R: TRect;
  C: TBCCustomColor;
begin
  R := ARect;
  C := ColorAt(0);
  if (C <> nil) and AllEqual then
    DrawSwatch(ACanvas, R, C.Value);
  inherited PropDrawValue(ACanvas, R, AState);
end;

procedure TBCColorPropertyEditor.RevertToInherited;
var
  I: integer;
  Entry, Ancestor: TInstProp;
  C, Original: TBCCustomColor;
begin
  if PropertyHook = nil then Exit;

  for I := 0 to PropCount - 1 do
  begin
    Entry := Default(TInstProp);
    Entry.Instance := GetComponent(I);
    Entry.PropInfo := GetPropInfo;
    if not PropertyHook.GetAncestorInstance(Entry, Ancestor) then
      Continue;
    C := ColorAt(I);
    Original := Ancestor.GetObject as TBCCustomColor;
    if (C = nil) or (Original = nil) or C.SameValueAs(Original) then Continue;

    C.Assign(Original); // copy: never replace an owned persistent
    Modified(I);
  end;
end;

{ TBCPixelPropertyEditor }

function TBCPixelPropertyEditor.PixelAt(Index: integer): TBCPixel;
begin
  Result := TBCPixel(GetObjectValueAt(Index, TBCPixel));
end;

function TBCPixelPropertyEditor.AllEqual: boolean;
var
  I: integer;
  First, P: TBCPixel;
begin
  Result := False;
  First := PixelAt(0);
  if First = nil then Exit;

  for I := 1 to PropCount - 1 do
  begin
    P := PixelAt(I);
    if (P = nil) or not P.Pixel.EqualsExactly(First.Pixel) then Exit;
  end;
  Result := True;
end;

procedure TBCPixelPropertyEditor.Edit;
var
  I: integer;
  P: TBCPixel;
  V: TBGRAPixel;
  Changed: boolean;
begin
  P := PixelAt(0);
  if P = nil then Exit;

  V := P.Pixel;
  if not ExecuteBCColorPicker(V, True) then Exit;

  Changed := False;
  for I := 0 to PropCount - 1 do
  begin
    P := PixelAt(I);
    { Compare stored RGBA, not appearance: transparent RGB still matters. }
    if (P = nil) or P.Pixel.EqualsExactly(V) then Continue;

    P.Pixel := V;
    TBCPixelAccess(P).Change;
    Changed := True;
  end;
  { Modified only when a selected pixel really changes }
  if Changed then Modified;
end;

function TBCPixelPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paCustomDrawn, paReadOnly, paRevertable];
end;

function TBCPixelPropertyEditor.GetValue: ansistring;
var
  P: TBCPixel;
begin
  P := PixelAt(0);
  if P = nil then
    Result := ''
  else
    Result := BCColorToString(P.Pixel);
end;

procedure TBCPixelPropertyEditor.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; AState: TPropEditDrawState);
var
  R: TRect;
  P: TBCPixel;
begin
  R := ARect;
  P := PixelAt(0);
  if (P <> nil) and AllEqual then
    DrawSwatch(ACanvas, R, P.Pixel);
  inherited PropDrawValue(ACanvas, R, AState);
end;

procedure TBCPixelPropertyEditor.RevertToInherited;
var
  I: integer;
  Entry, Ancestor: TInstProp;
  P, Original: TBCPixel;
begin
  if PropertyHook = nil then
    Exit;
  for I := 0 to PropCount - 1 do
  begin
    Entry := Default(TInstProp);
    Entry.Instance := GetComponent(I);
    Entry.PropInfo := GetPropInfo;
    if not PropertyHook.GetAncestorInstance(Entry, Ancestor) then Continue;

    P := PixelAt(I);
    Original := Ancestor.GetObject as TBCPixel;
    if (P = nil) or (Original = nil) or
      P.Pixel.EqualsExactly(Original.Pixel) then Continue;

    P.Assign(Original);
    TBCPixelAccess(P).Change;
    Modified(I);
  end;
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TBCColorString), nil, '', TBCColorStringPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TBCCustomColor), nil, '', TBCColorPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TBCPixel), nil, '', TBCPixelPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TBCCSSColorString), nil, '', TBCCSSColorPropertyEditor);

  RegisterPropertyEditor(TypeInfo(string), TBGRASVGTheme, 'ColorizeNormal', TBCCSSColorPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TBGRASVGTheme, 'ColorizeHover', TBCCSSColorPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TBGRASVGTheme, 'ColorizeActive', TBCCSSColorPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TBGRASVGTheme, 'ColorizeDisabled', TBCCSSColorPropertyEditor);

  { TColor + opacity pairs edited through their Alpha* property; still
    published so old LFM files load and new ones keep the same format. }
  RegisterPropertyEditor(TypeInfo(TColor), TBCGradient, 'StartColor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Byte), TBCGradient, 'StartColorOpacity', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TBCGradient, 'EndColor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Byte), TBCGradient, 'EndColorOpacity', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TBCFont, 'ShadowColor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Byte), TBCFont, 'ShadowColorOpacity', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TBCBackground, 'Color', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Byte), TBCBackground, 'ColorOpacity', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TBCBorder, 'Color', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Byte), TBCBorder, 'ColorOpacity', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TBCBorder, 'LightColor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Byte), TBCBorder, 'LightOpacity', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TBCComboBox, 'FocusBorderColor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Byte), TBCComboBox, 'FocusBorderOpacity', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TBCCheckComboBox, 'FocusBorderColor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Byte), TBCCheckComboBox, 'FocusBorderOpacity', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TBGRAKnob, 'PositionColor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Byte), TBGRAKnob, 'PositionOpacity', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TBGRAShape, 'FillColor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Byte), TBGRAShape, 'FillOpacity', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TBGRAShape, 'BorderColor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Byte), TBGRAShape, 'BorderOpacity', THiddenPropertyEditor);
end;

end.
