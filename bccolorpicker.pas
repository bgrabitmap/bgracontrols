// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ RGBA color picker for a TBCCustomColor, a TBGRAPixel or a CSS color text.
  Layout and color model follow the LazPaint color chooser (independent
  implementation). No IDE dependencies. }
unit BCColorPicker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, Graphics,
  Spin, LCLType, Types, Math, BGRABitmap, BGRABitmapTypes, BCColorUtils,
  BCColors;

type

  TBCPickerMode = (pmPixel, pmColorValue, pmCSSText);

  TBCPickerEntryKind = (pekCustom, pekHeader, pekColor);

  { List captions may combine aliases; text output uses canonical names. }
  TBCPickerEntry = record
    EntryKind: TBCPickerEntryKind;
    Value: TBCColorValue;
    Note: string;   // drawn grayed on the right (the LCL name of a CSS name)
  end;

  { Layout in bccolorpicker.lfm (AutoSize, anchors): keep the component names,
    the code and the regression tests use them. }

  { TBCColorPickerForm }

  TBCColorPickerForm = class(TForm)
    AlphaBar: TPaintBox;
    ColorWheel: TPaintBox;
    LightBar: TPaintBox;
    LightLabel: TLabel;
    ColorRow: TPanel;
    ChannelPanel: TPanel;
    ColorLabel: TLabel;
    LightPanel: TPanel;
    OpacityLabel: TLabel;
    OpacityPanel: TPanel;
    ColorWheelPanel: TPanel;
    SystemColor: TComboBox;
    RedLabel: TLabel;
    GreenLabel: TLabel;
    BlueLabel: TLabel;
    AlphaLabel: TLabel;
    RedValue: TSpinEdit;
    GreenValue: TSpinEdit;
    BlueValue: TSpinEdit;
    AlphaValue: TSpinEdit;
    ValueLabel: TLabel;
    ColorText: TEdit;
    Status: TLabel;
    PreviewLabel: TLabel;
    Preview: TPaintBox;
    OKButton: TButton;
    CancelButton: TButton;
    procedure Accept(Sender: TObject);
    procedure ChannelChanged(Sender: TObject);
    procedure ColorTextChanged(Sender: TObject);
    procedure ComboDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ComboSelect(Sender: TObject);
    procedure PaintAlpha(Sender: TObject);
    procedure PaintLight(Sender: TObject);
    procedure PaintPreview(Sender: TObject);
    procedure PaintWheel(Sender: TObject);
    procedure SurfaceDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure SurfaceMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure SurfaceUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    FOriginal: TBCColor;
    FValue: TBCColor;
    FMode: TBCPickerMode;
    FCSSStyle: string;           // last valid CSS text: output style
    { current color = FFullColor at linear light FLight (0..65535) }
    FFullColor: TBGRAPixel;
    FLight: word;
    FWheelFull: TBGRABitmap;     // wheel at full light: the colors to pick
    FWheelCache: TBGRABitmap;    // wheel as shown
    FWheelCacheLight: integer;
    FWheelPos: TPoint;
    FWheelPosValid: boolean;
    FUpdating: boolean;
    FEntries: array of TBCPickerEntry;  // parallel to SystemColor.Items
    FFixedNames: TStringList;  // while BuildColorList runs
    FLastIndex: integer;   // to step over the group headers
    procedure AddEntry(const ACaption: string; AKind: TBCPickerEntryKind;
      const AValue: TBCColorValue; const ANote: string = '');
    procedure AddColorName(const S: string);
    procedure BuildColorList;
    procedure AddSystemColors;
    procedure AddFixedColors;
    procedure SetColorValue(const AValue: TBCColorValue;
      UpdateText: boolean = True);
    function EntryMatches(Index: integer): boolean;
    function MatchingEntry: integer;
    procedure ApplyLight;
    function BarWidthOf(Box: TPaintBox): integer;
    function BarInsetOf(Box: TPaintBox): integer;
    function BarPosToY(Box: TPaintBox; AFrac: double): integer;
    function BarYToPos(Box: TPaintBox; Y: integer): double;
    procedure DrawCursor(Box: TPaintBox; Y: integer);
    procedure LocateCursor;
    function ParseText(const S: string; out V: TBCColorValue): boolean;
    function ItemValue(Index: integer): TBCColorValue;
    function Pixel: TBGRAPixel;
    procedure ReadColor;
    function WheelFull: TBGRABitmap;
    procedure SelectAt(Sender: TObject; X, Y: integer);
    procedure Sync(UpdateText: boolean = True);
    function TextOf: string;
    procedure MeasureList;
  public
    constructor CreatePicker(AMode: TBCPickerMode;
      AShowColorList: boolean = False);
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

{ Assigns AColor once, on OK. Alpha alone keeps a named color; the wheel,
  the light bar and the RGB fields make it a custom RGB. }
function ExecuteBCColorPicker(AColor: TBCCustomColor): boolean; overload;

{ RGBA only; AShowColorList shows the list disabled on "Custom color". }
function ExecuteBCColorPicker(var Pixel: TBGRAPixel;
  AShowColorList: boolean = False): boolean; overload;

{ CSS text: keeps the style of the last valid text, and AText itself when the
  color is unchanged. Invalid AText starts from black. }
function ExecuteBCColorPicker(var AText: string;
  AShowColorList: boolean = True): boolean; overload;

implementation

uses
  LCLIntf;

{$R *.lfm}

const
  CursorSize = 6;          // at 96 PPI
  WheelCursorSize = 3;     // at 96 PPI, half side
  MinWheelLight = 10000;   // keeps the hues visible at low light
  CustomColorCaption = 'Custom color';

{ Hue = angle (perceptual, GtoH), gray center, mixed in linear space. }
function CreateWheel(AWidth, AHeight: integer; ALight: word): TBGRABitmap;
var
  X, Y: integer;
  R, CX, CY, DX, DY, D, Cover, Gray, F, A: single;
  Ch: array[0..2] of single;
  E: TExpandedPixel;
  P: TBGRAPixel;
begin
  Result := TBGRABitmap.Create(AWidth, AHeight, BGRAPixelTransparent);
  R := Min(AWidth, AHeight) / 2;
  if R <= 0 then
    Exit;
  CX := (AWidth - 1) / 2;
  CY := (AHeight - 1) / 2;
  E.alpha := $FFFF;
  for Y := 0 to AHeight - 1 do
    for X := 0 to AWidth - 1 do
    begin
      DX := X - CX;
      DY := Y - CY;
      D := Sqrt(DX * DX + DY * DY);
      Cover := EnsureRange(R - D + 0.5, 0, 1);
      if Cover <= 0 then
        Continue;
      A := ArcTan2(DY, DX) / (2 * Pi);
      if A < 0 then
        A := A + 1;
      F :=GtoH(Round(A * 65536) and $FFFF) / 65536 * 6;
      Ch[0] := EnsureRange(Abs(F - 3) - 1, 0, 1);
      Ch[1] := EnsureRange(2 - Abs(F - 2), 0, 1);
      Ch[2] := EnsureRange(2 - Abs(F - 4), 0, 1);
      Gray := EnsureRange(1 - (D / R * 1.2 - 0.1), 0, 1);
      E.red := Round((Ch[0] * (1 - Gray) + Gray) * ALight);
      E.green := Round((Ch[1] * (1 - Gray) + Gray) * ALight);
      E.blue := Round((Ch[2] * (1 - Gray) + Gray) * ALight);
      P := GammaCompression(E);
      P.alpha := Round(Cover * 255);
      Result.SetPixel(X, Y, P);
    end;
end;

procedure DrawFrame(Canvas: TCanvas; const R: TRect);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clGrayText;
  Canvas.Rectangle(R);
  Canvas.Brush.Style := bsSolid;
end;

procedure DrawFramedSwatch(Canvas: TCanvas; const R: TRect;
  const P: TBGRAPixel); overload;
begin
  DrawColor(Canvas, Rect(R.Left + 1, R.Top + 1, R.Right - 1, R.Bottom - 1), P);
  DrawFrame(Canvas, R);
end;

procedure DrawFramedSwatch(Canvas: TCanvas; const R: TRect;
  const V: TBCColorValue); overload;
begin
  DrawColor(Canvas, Rect(R.Left + 1, R.Top + 1, R.Right - 1, R.Bottom - 1), V);
  DrawFrame(Canvas, R);
end;

const
  { between the names of an item: 'ThreeDFace / ButtonFace' }
  NameSeparator = ' / ';

{ TBCColorPickerForm }

constructor TBCColorPickerForm.CreatePicker(AMode: TBCPickerMode;
  AShowColorList: boolean);
var
  I: integer;
begin
  inherited Create(nil);
  { ParentColor passes clDefault, which each widget resolves on its own: on
    GTK2 a panel gets a lighter color than the form, visible around the
    wheel and under the transparent labels. Give the panels the form color. }
  for I := 0 to ComponentCount - 1 do
    if (Components[I] is TPanel) and TPanel(Components[I]).ParentColor then
      TPanel(Components[I]).Color := GetColorResolvingParent;
  FMode := AMode;
  FWheelCacheLight := -1;
  FOriginal := TBCColor.Create;
  FValue := TBCColor.Create;

  if (FMode = pmColorValue) or AShowColorList then
  begin
    SystemColor.Items.Clear;
    AddEntry(CustomColorCaption, pekCustom, TBCColorValue.New(BGRABlack));
    if (FMode = pmColorValue) then
      BuildColorList
    else
      SystemColor.Enabled := False;
  end
  else
  begin
    FreeAndNil(SystemColor);
    FreeAndNil(ColorLabel);
    ColorRow.Visible := False;
  end;

  if (FMode = pmColorValue) then
    ColorText.Hint :=
      'Examples: #FF000080, red, ThreeDFace, rgb(from ThreeDFace r g b / 50%)'
  else
    ColorText.Hint := 'Examples: #FF000080, #FF0000';
  Status.Caption := '';

  { Designed at 96 PPI. With Application.Scaled, TCustomForm.AfterConstruction
    scales the form after this constructor. }
  if not Application.Scaled and (Screen.PixelsPerInch <> DesignTimePPI) then
    AutoAdjustLayout(lapAutoAdjustForDPI, DesignTimePPI, Screen.PixelsPerInch, 0, 0);
end;

{ The list is measured in screen pixels: only once the form has its final
  PPI, otherwise the scaling would apply a second time. }
procedure TBCColorPickerForm.AfterConstruction;
begin
  inherited AfterConstruction;
  MeasureList;
end;

destructor TBCColorPickerForm.Destroy;
begin
  FWheelCache.Free;
  FWheelFull.Free;
  FOriginal.Free;
  FValue.Free;
  inherited Destroy;
end;

{ csOwnerDrawFixed does not measure the font. The list is as wide as the
  form: widen the form until the longest name and its note fit. Before the
  handle, so that the automatic size includes it. }
procedure TBCColorPickerForm.MeasureList;
var
  I, W: integer;
  B: TBitmap;
begin
  if SystemColor = nil then
    Exit;
  { measured with the font the items are drawn with }
  B := TBitmap.Create;
  try
    B.Canvas.Font := SystemColor.Font;
    SystemColor.ItemHeight := Max(Scale96ToForm(22),
      B.Canvas.TextHeight('Wg') + Scale96ToForm(8));
    W := 0;
    for I := 0 to SystemColor.Items.Count - 1 do
      W := Max(W, B.Canvas.TextWidth(SystemColor.Items[I]) +
        B.Canvas.TextWidth(FEntries[I].Note));
  finally
    B.Free;
  end;
  { swatch and gaps (as in ComboDrawItem), a clear space between the two
    texts, the list scroll bar and the borders }
  W := W + Scale96ToForm(3 + 28 + 8 + 40 + 6 + 8) +
    GetSystemMetrics(SM_CXVSCROLL);
  ChannelPanel.Constraints.MinWidth := W;
end;

procedure TBCColorPickerForm.AddEntry(const ACaption: string;
  AKind: TBCPickerEntryKind; const AValue: TBCColorValue; const ANote: string);
var
  N: integer;
begin
  N := Length(FEntries);
  SetLength(FEntries, N + 1);
  FEntries[N].EntryKind := AKind;
  FEntries[N].Value := AValue;
  FEntries[N].Note := ANote;
  SystemColor.Items.Add(ACaption);
end;

{ The LCL identifiers that are not system colors (fixed RGB values, clNone
  and clDefault), collected by BuildColorList. }
procedure TBCColorPickerForm.AddColorName(const S: string);
var
  C: longint;
begin
  if IdentToColor(S, C) and (TColorRef(C) and SYS_COLOR_BASE = 0) then
    FFixedNames.Add(S);
end;

{ Two groups, with aliases sharing one item (ThreeDFace / ButtonFace,
  aqua / cyan). CSS names have the matching LCL name as a note; entries
  without a CSS name use the LCL name as their caption.
  System colors: grouped by symbolic kind; CSS system colors without an
  LCL equivalent are not listed.
  Fixed colors: CSS names grouped by pixel value, with transparent (clNone)
  inserted alphabetically, then the remaining LCL identifiers in the order
  returned by GetColorValues (including clDefault). }
procedure TBCColorPickerForm.BuildColorList;
begin
  AddSystemColors;
  AddFixedColors;
end;

procedure TBCColorPickerForm.AddSystemColors;
var
  K: TBCLCLSystemColor;
  C: TColor;
  I: integer;
  LCLName, CSSName: string;
begin
  AddEntry('System colors', pekHeader, TBCColorValue.New(BGRABlack));
  for K := Low(TBCLCLSystemColor) to High(TBCLCLSystemColor) do
  begin
    if not TBCSymbolicColor(K).TryToColor(C) then Continue; // scReserved25

    CSSName := '';
    LCLName := ColorToString(C);
    for I := Low(BCSymbolicCSSNames) to High(BCSymbolicCSSNames) do
      if BCSymbolicCSSNames[I].Kind = K then
      begin
        if Length(CSSName) > 0 then
          CSSName := CSSName + NameSeparator;

        CSSName := CSSName + BCSymbolicCSSNames[I].Name;
      end;

    if Length(CSSName) > 0 then
      AddEntry(CSSName, pekColor, TBCColorValue.New(K), LCLName)
    else
      AddEntry(LCLName, pekColor, TBCColorValue.New(K));
  end;
end;

procedure TBCColorPickerForm.AddFixedColors;
var
  C: TColor;
  I, J: integer;
  LCLName, CSSName, CSSTransparentName: string;
  CSSTransparentDone: boolean;
begin
  AddEntry('Fixed colors', pekHeader, TBCColorValue.New(BGRABlack));
  FFixedNames := TStringList.Create;
  try
    GetColorValues(@AddColorName);
    J := FFixedNames.IndexOf(ColorToString(clNone));
    if J <> -1 then
      FFixedNames.Delete(J);

    CSSTransparentName := BCToStr(TBCColorValue.New(scNone));
    CSSTransparentDone := False;

    { CSSColors has only the opaque colors, transparent
      (clNone) is put in its alphabetical place. }
    for I := 0 to CSSColors.Count - 1 do
    begin
      if CSSColors.IndexOfColor(CSSColors[I]) < I then Continue;

      CSSName := LowerCase(CSSColors.Name[I]);
      if (not CSSTransparentDone) and (CompareStr(CSSName, CSSTransparentName) > 0) then
      begin
        AddEntry(CSSTransparentName, pekColor,
          TBCColorValue.New(scNone), ColorToString(clNone));
        CSSTransparentDone := True;
      end;

      for J := I + 1 to CSSColors.Count - 1 do
        if CSSColors[J] = CSSColors[I] then
          CSSName := CSSName + NameSeparator + LowerCase(CSSColors.Name[J]);

      LCLName := '';
      for J := FFixedNames.Count - 1 downto 0 do
      begin
        C := StringToColor(FFixedNames[J]);
        if TBCColorValue.New(C).SameColorAs(TBCColorValue.New(CSSColors[I])) then
        begin
          LCLName := FFixedNames[J];
          FFixedNames.Delete(J);
        end;
      end;

      AddEntry(CSSName, pekColor, TBCColorValue.New(CSSColors[I]), LCLName);
    end;

    if (not CSSTransparentDone) then
      AddEntry(CSSTransparentName, pekColor,
        TBCColorValue.New(scNone), ColorToString(clNone));

    for I := 0 to FFixedNames.Count - 1 do
      AddEntry(FFixedNames[I], pekColor,
        TBCColorValue.New(StringToColor(FFixedNames[I])));
  finally
    FreeAndNil(FFixedNames);
  end;
end;

{ #FF0000 matches red, whatever the opacity. }
function TBCColorPickerForm.EntryMatches(Index: integer): boolean;
var
  V: TBCColorValue;
begin
  Result := (Index > 0) and (Index < Length(FEntries)) and
    (FEntries[Index].EntryKind = pekColor);
  if not Result then
    Exit;
  V := FEntries[Index].Value;
  if V.Kind <> scNone then
    V := V.WithAlpha(FValue.Alpha);
  Result := FValue.SameValueAs(V);
end;

{ The item of FValue, whatever the opacity; 0 (Custom color) when none.
  Aliases share an item, so a value has at most one. }
function TBCColorPickerForm.MatchingEntry: integer;
begin
  if SystemColor <> nil then
    for Result := 1 to SystemColor.Items.Count - 1 do
      if EntryMatches(Result) then
        Exit;
  Result := 0;
end;

procedure TBCColorPickerForm.ApplyLight;
var
  P: TBGRAPixel;
begin
  P := SetIntensity(FFullColor, FLight);
  P.alpha := FValue.Alpha;
  FValue.Assign(TBCColorValue.New(P));
  Sync;
end;

function TBCColorPickerForm.ItemValue(Index: integer): TBCColorValue;
begin
  Result := FEntries[Index].Value;
end;

function TBCColorPickerForm.BarWidthOf(Box: TPaintBox): integer;
begin
  Result := Max(1, Box.Width - 2 * Scale96ToForm(CursorSize));
end;

{ Free space above and below the bar, so the arrow is not clipped at the
  ends: the half height of the arrow. }
function TBCColorPickerForm.BarInsetOf(Box: TPaintBox): integer;
begin
  Result := Max(1, (Box.Width - BarWidthOf(Box)) div 2);
end;

{ AFrac = 1 at the top of the bar, 0 at the bottom. }
function TBCColorPickerForm.BarPosToY(Box: TPaintBox; AFrac: double): integer;
var
  M, L: integer;
begin
  M := BarInsetOf(Box);
  L := Max(1, Box.Height - 2 * M);
  Result := M + Round((1 - AFrac) * (L - 1));
end;

function TBCColorPickerForm.BarYToPos(Box: TPaintBox; Y: integer): double;
var
  M, L: integer;
begin
  M := BarInsetOf(Box);
  L := Max(1, Box.Height - 2 * M);
  Result := EnsureRange((M + L - 1 - Y) / Max(1, L - 1), 0, 1);
end;

procedure TBCColorPickerForm.DrawCursor(Box: TPaintBox; Y: integer);
var
  X, BW, CS: integer;
begin
  BW := BarWidthOf(Box);
  CS := BarInsetOf(Box);
  X := BW + 1;
  with Box.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Self.GetColorResolvingParent;
    Pen.Style := psClear;
    FillRect(Rect(BW, 0, Box.Width, Box.Height));
    FillRect(Rect(0, 0, BW, CS));
    FillRect(Rect(0, Box.Height - CS, BW, Box.Height));
    Pen.Style := psSolid;
    Pen.Color := clWindowText;
    Brush.Color := clWindowText;
    Polygon([Point(X, Y), Point(X + CS, Y - CS), Point(X + CS, Y + CS)]);
  end;
end;

function TBCColorPickerForm.ParseText(const S: string;
  out V: TBCColorValue): boolean;
var
  P: TBGRAPixel;
begin
  if (FMode = pmColorValue) then
    Exit(TryStrToBC(S, V));
  V := TBCColorValue.New(BGRABlack);
  if (FMode = pmCSSText) then
  begin
    Result := TryCSSColor(S, P);
    if Result then
      FCSSStyle := S;
  end
  else
    Result := TryBCColor(S, P);
  if Result then
    V := TBCColorValue.New(P);
end;

function TBCColorPickerForm.Pixel: TBGRAPixel;
begin
  Result := FValue.ToBGRA;
end;

{ Black (no hue) becomes white at light 0, as in LazPaint. }
procedure TBCColorPickerForm.ReadColor;
var
  P: TBGRAPixel;
begin
  P := Pixel;
  FLight := GetIntensity(P);
  FFullColor := SetIntensity(P, $FFFF);
  FFullColor.alpha := 255;
  FWheelPosValid := False;
end;

function TBCColorPickerForm.WheelFull: TBGRABitmap;
begin
  if (FWheelFull = nil) or (FWheelFull.Width <> ColorWheel.Width) or
    (FWheelFull.Height <> ColorWheel.Height) then
  begin
    FreeAndNil(FWheelFull);
    FWheelFull := CreateWheel(ColorWheel.Width, ColorWheel.Height, $FFFF);
    FWheelPosValid := False;
  end;
  Result := FWheelFull;
end;

{ Middle of the wheel pixels closest to FFullColor. }
procedure TBCColorPickerForm.LocateCursor;
var
  B: TBGRABitmap;
  P: PBGRAPixel;
  X, Y, Dist, Best, SumX, SumY, N: integer;
begin
  B := WheelFull;
  Best := MaxInt;
  SumX := 0;
  SumY := 0;
  N := 0;
  for Y := 0 to B.Height - 1 do
  begin
    P := B.ScanLine[Y];
    for X := 0 to B.Width - 1 do
    begin
      if P^.alpha <> 0 then
      begin
        Dist := Abs(P^.red - FFullColor.red) +
          Abs(P^.green - FFullColor.green) + Abs(P^.blue - FFullColor.blue);
        if Dist < Best then
        begin
          Best := Dist;
          SumX := 0;
          SumY := 0;
          N := 0;
        end;
        if Dist = Best then
        begin
          Inc(SumX, X);
          Inc(SumY, Y);
          Inc(N);
        end;
      end;
      Inc(P);
    end;
  end;
  if N > 0 then
    FWheelPos := Point((SumX + N div 2) div N, (SumY + N div 2) div N)
  else
    FWheelPos := Point(B.Width div 2, B.Height div 2);
  FWheelPosValid := True;
end;

procedure TBCColorPickerForm.SelectAt(Sender: TObject; X, Y: integer);
var
  S: TPaintBox;
  R, DX, DY, D: double;
  P: TBGRAPixel;
begin
  if not (Sender is TPaintBox) then Exit;

  S := TPaintBox(Sender);
  if S = ColorWheel then
  begin
    R :=Min(S.Width, S.Height) / 2 - 1;
    DX := X - (S.Width - 1) / 2;
    DY := Y - (S.Height - 1) / 2;
    D := Sqrt(DX * DX + DY * DY);
    if D > R then
    begin
      X := Round((S.Width - 1) / 2 + DX * R / D);
      Y := Round((S.Height - 1) / 2 + DY * R / D);
    end;
    P := WheelFull.GetPixel(X, Y);
    if P.alpha = 0 then
      Exit;
    FFullColor := BGRA(P.red, P.green, P.blue);
    FWheelPos := Point(X, Y);
    FWheelPosValid := True;
    ApplyLight;
  end
  else if S = LightBar then
  begin
    FLight := Round(BarYToPos(S, Y) * $FFFF);
    ApplyLight;
  end
  else if S = AlphaBar then
  begin
    FValue.Alpha := Round(BarYToPos(S, Y) * 255);
    Sync;
  end;
end;

{ External values rebuild wheel state. Wheel/light edits bypass this path
  to retain hue and cursor position at low light. }
procedure TBCColorPickerForm.SetColorValue(const AValue: TBCColorValue;
  UpdateText: boolean);
begin
  FValue.Assign(AValue);
  ReadColor;
  Sync(UpdateText);
end;

procedure TBCColorPickerForm.Sync(UpdateText: boolean);
var
  Sel: integer;
  P: TBGRAPixel;
begin
  Sel := 0;
  FUpdating := True;
  try
    if SystemColor <> nil then
    begin
      Sel := MatchingEntry;
      FLastIndex := Sel;
    end;
    if UpdateText then
      ColorText.Text := TextOf;
    P := Pixel;
    RedValue.Value := P.red;
    GreenValue.Value := P.green;
    BlueValue.Value := P.blue;
    AlphaValue.Value := FValue.Alpha;
    ColorText.Font.Color := clDefault;
    Status.Font.Color := clDefault;
    if FValue.Alpha = 0 then
      Status.Caption := 'Transparent'
    else
      Status.Caption := Format('Opacity: %d%%',
        [Round(FValue.Alpha * 100 / 255)]);
    if SystemColor <> nil then
    begin
      SystemColor.ItemIndex := Sel;
      SystemColor.Invalidate;
    end;
  finally
    FUpdating := False;
  end;
  ColorWheel.Invalidate;
  LightBar.Invalidate;
  AlphaBar.Invalidate;
  Preview.Invalidate;
end;

{ Format the value for the current mode. System-color mode uses canonical
  names, not a typed alias or the combined list caption. CSS mode preserves
  the input format style. }
function TBCColorPickerForm.TextOf: string;
begin
  case FMode of
    pmPixel: Result := BCColorToString(Pixel);
    pmColorValue: Result := FValue.ToString;
    pmCSSText: Result := FormatCSSColor(Pixel, FCSSStyle);
  else
    Result := BCColorToString(Pixel);
  end;
end;

procedure TBCColorPickerForm.Accept(Sender: TObject);
var
  V: TBCColorValue;
begin
  if not ParseText(ColorText.Text, V) then
  begin
    MessageDlg(Status.Caption + '.', mtError, [mbOK], 0);
    ColorText.SetFocus;
    Exit;
  end;
  FValue.Assign(V);
  ModalResult := mrOK;
end;

procedure TBCColorPickerForm.ChannelChanged(Sender: TObject);
var
  P: TBGRAPixel;
begin
  if FUpdating then
    Exit;
  if Sender = AlphaValue then
    FValue.Alpha := AlphaValue.Value
  else
  begin
    P :=BGRA(RedValue.Value, GreenValue.Value, BlueValue.Value,
      FValue.Alpha);
    SetColorValue(TBCColorValue.New(P));
    Exit;
  end;
  Sync;
end;

procedure TBCColorPickerForm.ColorTextChanged(Sender: TObject);
var
  V: TBCColorValue;
begin
  if FUpdating then
    Exit;
  if ParseText(ColorText.Text, V) then
  begin
    SetColorValue(V, False); // keep text and caret while typing
  end
  else
  begin
    ColorText.Font.Color := clRed;
    if (FMode = pmCSSText) then
      Status.Caption := 'Enter #RRGGBBAA, rgba(r,g,b,a) or a color name'
    else if (FMode = pmColorValue) then
      Status.Caption :=
        'Enter #RRGGBBAA, a color name or rgb(from <name> r g b / <alpha>)'
    else
      Status.Caption := 'Enter #RRGGBB or #RRGGBBAA';
    Status.Font.Color := clRed;
  end;
end;

procedure TBCColorPickerForm.ComboDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  C: TCanvas;
  S: TRect;
  P: TBGRAPixel;
  M: integer;
begin
  C := SystemColor.Canvas;
  if (odSelected in State) and not (odComboBoxEdit in State) then
  begin
    C.Brush.Color := clHighlight;
    C.Font.Color := clHighlightText;
  end
  else
  begin
    C.Brush.Color := clWindow;
    C.Font.Color := clWindowText;
  end;
  if (Index >= 0) and (FEntries[Index].EntryKind = pekHeader) then
  begin
    C.Brush.Color := clBtnFace;
    C.FillRect(ARect);
    C.Font.Color := clGrayText;
    C.Font.Style := [fsBold];
    C.Brush.Style := bsClear;
    C.TextOut(ARect.Left + Scale96ToForm(6),
      ARect.Top + (ARect.Height - C.TextHeight('Wg')) div 2,
      SystemColor.Items[Index]);
    C.Brush.Style := bsSolid;
    C.Font.Style := [];
    Exit;
  end;
  C.FillRect(ARect);
  if Index < 0 then
    Exit;
  M :=Scale96ToForm(3);
  S := Rect(ARect.Left + M, ARect.Top + M,
    ARect.Left + M + Scale96ToForm(28), ARect.Bottom - M);
  if Index = 0 then
  begin
    P := Pixel;
    P.alpha := 255;
    DrawFramedSwatch(C, S, P);
  end
  else
    DrawFramedSwatch(C, S, ItemValue(Index));
  C.Brush.Style := bsClear;
  C.TextOut(S.Right + Scale96ToForm(8),
    ARect.Top + (ARect.Height - C.TextHeight('Wg')) div 2,
    SystemColor.Items[Index]);
  M := ARect.Right - Scale96ToForm(6) - C.TextWidth(FEntries[Index].Note);
  { the note only where it does not cover the name }
  if (FEntries[Index].Note <> '') and not (odComboBoxEdit in State) and
    (M >= S.Right + Scale96ToForm(8 + 12) +
      C.TextWidth(SystemColor.Items[Index])) then
  begin
    if not (odSelected in State) then
      C.Font.Color := clGrayText;
    C.TextOut(M, ARect.Top + (ARect.Height - C.TextHeight('Wg')) div 2,
      FEntries[Index].Note);
  end;
  C.Brush.Style := bsSolid;
end;

{ Also the OnChange handler. GTK2 sends OnSelect only when the index differs
  from the one it cached, and caches it after OnSelect: stepping over a
  header there leaves the cache on the header, and the next move onto it
  sends OnChange alone. OnChange comes first and the step then updates the
  cache too. }
procedure TBCColorPickerForm.ComboSelect(Sender: TObject);
var
  P: TBGRAPixel;
  I: integer;
begin
  if FUpdating or (SystemColor.ItemIndex < 0) then
    Exit;
  I := SystemColor.ItemIndex;
  { OnChange and OnSelect both come here: the item already shown is done }
  if (I = FLastIndex) and (FEntries[I].EntryKind <> pekHeader) then
    Exit;
  if FEntries[I].EntryKind = pekHeader then
  begin
    { step over the header in the direction of the move }
    if (I > FLastIndex) and (I < SystemColor.Items.Count - 1) then
      Inc(I)
    else
      Dec(I);
    FUpdating := True;
    SystemColor.ItemIndex := I;
    FUpdating := False;
  end;
  if I = 0 then
  begin
    if FValue.Kind = scCustomRGB then
    begin
      FLastIndex := 0;
      Exit;
    end;
    P := Pixel;
    SetColorValue(TBCColorValue.New(P));
  end
  else
    SetColorValue(ItemValue(I));
end;

procedure TBCColorPickerForm.PaintAlpha(Sender: TObject);
var
  H, BW, M: integer;
  P: TBGRAPixel;
  B: TBGRABitmap;
begin
  M := BarInsetOf(AlphaBar);
  H := Max(1, AlphaBar.Height - 2 * M);
  BW := BarWidthOf(AlphaBar);
  P := Pixel;
  B := TBGRABitmap.Create(BW, H);
  try
    DrawBCCheckers(B, Rect(0, 0, BW, H));
    B.GradientFill(0, 0, BW, H, BGRA(P.red, P.green, P.blue, 255),
      BGRA(P.red, P.green, P.blue, 0), gtLinear, PointF(0, 0),
      PointF(0, H - 1), dmLinearBlend, False);
    B.Draw(AlphaBar.Canvas, 0, M, True);
  finally
    B.Free;
  end;
  DrawCursor(AlphaBar, BarPosToY(AlphaBar, FValue.Alpha / 255));
end;

procedure TBCColorPickerForm.PaintLight(Sender: TObject);
var
  H, BW, M: integer;
  B: TBGRABitmap;
begin
  M := BarInsetOf(LightBar);
  H := Max(1, LightBar.Height - 2 * M);
  BW := BarWidthOf(LightBar);
  B := TBGRABitmap.Create(BW, H);
  try
    B.GradientFill(0, 0, BW, H, FFullColor, BGRABlack, gtLinear,
      PointF(0, 0), PointF(0, H - 1), dmSet, True);
    B.Draw(LightBar.Canvas, 0, M, True);
  finally
    B.Free;
  end;
  DrawCursor(LightBar, BarPosToY(LightBar, FLight / $FFFF));
end;

procedure TBCColorPickerForm.PaintPreview(Sender: TObject);
var
  W: integer;
begin
  W := Preview.Width;
  DrawFramedSwatch(Preview.Canvas, Rect(0, 0, W div 2 + 1, Preview.Height),
    FOriginal.Value);
  DrawFramedSwatch(Preview.Canvas, Rect(W div 2, 0, W, Preview.Height),
    FValue.Value);
end;

procedure TBCColorPickerForm.PaintWheel(Sender: TObject);
const
  RingColors: array[0..2] of TColor = (clBlack, clWhite, clBlack);
var
  W, H, L, S, K, N, Ring: integer;
  R: TRect;
begin
  W := ColorWheel.Width;
  H := ColorWheel.Height;
  L :=Max(MinWheelLight, FLight);
  if (FWheelCache = nil) or (FWheelCache.Width <> W) or
    (FWheelCache.Height <> H) or (FWheelCacheLight <> L) then
  begin
    FreeAndNil(FWheelCache);
    FWheelCache := CreateWheel(W, H, L);
    FWheelCacheLight := L;
  end;
  FWheelCache.Draw(ColorWheel.Canvas, 0, 0, False);

  if not FWheelPosValid or (FWheelFull = nil) or (FWheelFull.Width <> W) or
    (FWheelFull.Height <> H) then
    LocateCursor;
  { black, white and black rings, each K pixels thick, around the picked
    color; at 96 PPI the three 1-pixel squares of half side S + 1, S, S - 1 }
  S := Scale96ToForm(WheelCursorSize);
  K := Max(1, Scale96ToForm(1));
  R := Rect(FWheelPos.X - S - K, FWheelPos.Y - S - K,
    FWheelPos.X + S + K + 1, FWheelPos.Y + S + K + 1);
  with ColorWheel.Canvas do
  begin
    Brush.Style := bsSolid;
    for Ring := Low(RingColors) to High(RingColors) do
    begin
      Brush.Color := RingColors[Ring];
      for N := 1 to K do
      begin
        FrameRect(R);
        InflateRect(R, -1, -1);
      end;
    end;
  end;
end;

procedure TBCColorPickerForm.SurfaceDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if (Button <> mbLeft) then Exit;
  if not (Sender is TPaintBox) then Exit;

  SetCaptureControl(TPaintBox(Sender));
  SelectAt(Sender, X, Y);
end;

procedure TBCColorPickerForm.SurfaceMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if (GetCaptureControl = Sender) and (ssLeft in Shift) then
  begin
    SelectAt(Sender, X, Y);
    if Sender is TPaintBox then
      TPaintBox(Sender).Update;
  end;
end;

procedure TBCColorPickerForm.SurfaceUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbLeft) and (GetCaptureControl = Sender) then
    SetCaptureControl(nil);
end;

function RunPicker(var V: TBCColorValue;
  AMode: TBCPickerMode; AShowColorList: boolean = False;
  ACSSStyle: PString = nil): boolean;
var
  F: TBCColorPickerForm;
begin
  F := TBCColorPickerForm.CreatePicker(AMode, AShowColorList);
  try
    if ACSSStyle <> nil then
    begin
      F.FCSSStyle := ACSSStyle^;
      F.ColorText.Hint := 'Examples: #FF000080, rgba(255,0,0,0.5), red';
    end;
    F.FOriginal.Assign(V);
    F.SetColorValue(V);
    if ACSSStyle <> nil then
    begin
      F.FUpdating := True;
      F.ColorText.Text := ACSSStyle^;
      F.FUpdating := False;
    end;
    Result := F.ShowModal = mrOK;
    if Result then
    begin
      V := F.FValue.Value;
      if ACSSStyle <> nil then
        ACSSStyle^ := F.FCSSStyle;
    end;
  finally
    F.Free;
  end;
end;

function ExecuteBCColorPicker(AColor: TBCCustomColor): boolean;
var
  V: TBCColorValue;
begin
  V := AColor.Value;
  Result := RunPicker(V, pmColorValue);
  if Result then
    AColor.Assign(V);
end;

function ExecuteBCColorPicker(var Pixel: TBGRAPixel;
  AShowColorList: boolean): boolean;
var
  V: TBCColorValue;
begin
  V := TBCColorValue.New(Pixel);
  Result := RunPicker(V, pmPixel, AShowColorList);
  if Result then
    Pixel := V.ToBGRA;
end;

function ExecuteBCColorPicker(var AText: string;
  AShowColorList: boolean): boolean;
var
  P, Last, Original: TBGRAPixel;
  V: TBCColorValue;
  Style: string;
  OriginalValid: boolean;
begin
  OriginalValid := TryCSSColor(AText, P);
  Original := P;
  if OriginalValid then
    Style := AText
  else
  begin
    P := BGRABlack;
    Style := '#000000FF';
  end;
  V := TBCColorValue.New(P);
  Result := RunPicker(V, pmCSSText, AShowColorList, @Style);
  if not Result then
    Exit;
  P := V.ToBGRA;
  { Preserve text only for identical stored RGBA, including transparent RGB. }
  if OriginalValid and P.EqualsExactly(Original) then
    Exit;
  if TryCSSColor(Style, Last) and
    Last.EqualsExactly(P) then
    AText := Style
  else
    AText := FormatCSSColor(P, Style);
end;

end.
