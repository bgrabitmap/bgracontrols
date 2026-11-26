program TestColorEditor;

{$mode objfpc}{$H+}

uses Interfaces, Forms, Classes, SysUtils, TypInfo, Graphics, Types, Math,
  ExtCtrls, StdCtrls, Controls, Spin, PropEdits, BGRABitmapTypes,
  BCColorUtils, BCColors, BCColorPicker, BCColorPropertyEditor, BCTypes, BGRASVGTheme, BGRAShape, BCComboBox,
  BCCheckComboBox, BCButton, BGRAKnob, SuperSpinner, SuperSpinnerCommon,
  BCSVGViewer, BGRAGraphicControl, SuperLED
  {$IFDEF LCLGTK2}, Gtk2, Gtk2Def, Gtk2Proc{$ENDIF}
  {$IFDEF LCLQT5}, Qt5, QtWidgets{$ENDIF}
  {$IFDEF LCLQT6}, Qt6, QtWidgets{$ENDIF};

type
  TBCButtonAccess = class(TBCButton)
  public
    property BGRANormal;
  end;

  TAncestorProvider = class
    Ancestor: TPersistent;
    Modifications: integer;
    procedure Modified(Sender: TObject);
    function Find(const Entry: TInstProp; out Original: TInstProp): Boolean;
  end;

  TColorHolder = class(TComponent)
  private
    FColor: TBCColorString;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Color: TBCColorString read FColor write FColor;
  end;

  TBCColorHolder = class(TComponent)
  private
    FFill: TBCColor;
    FChanges: integer;
    procedure FillChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Fill: TBCColor read FFill write FFill;
  end;

  { TBCLinkedColor through callbacks: the pair is written at once. }
  TBCLinkedHolder = class(TComponent)
  private
    FFillColor: TColor;
    FFillOpacity: Byte;
    FFill: TBCLinkedColor;
    FSets: integer;
    procedure GetFillValue(out AColor: TColor; out AAlpha: Byte);
    procedure SetFillValue(AColor: TColor; AAlpha: Byte);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FillColor: TColor read FFillColor write FFillColor;
    property FillOpacity: Byte read FFillOpacity write FFillOpacity;
    property Fill: TBCLinkedColor read FFill write FFill stored False;
  end;

  TBCPixelHolder = class(TComponent)
  private
    FPixel: TBCPixel;
    FChanges: integer;
    procedure PixelChanged(ASender: TObject; AData: PtrInt);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Pixel: TBCPixel read FPixel write FPixel;
  end;

  TCSSHolder = class(TComponent)
  private
    FColor: TBCCSSColorString;
  published
    property Color: TBCCSSColorString read FColor write FColor;
  end;

  TCSSAction = (caNone, caSpinAlpha, caText);

  { Picker in CSS text mode. }
  TCSSDriver = class
    Timer: TTimer;
    Action: TCSSAction;
    Shown, Text, Error: string;
    procedure Tick(Sender: TObject);
  end;

  { Picker opened by the TBCPixel editor: disabled color list on "Custom color". }
  TPixelDriver = class
    Timer: TTimer;
    Text, Error: string;
    procedure Tick(Sender: TObject);
  end;

  TAlphaAction = (aaUnchanged, aaAlphaBottom, aaText, aaList, aaListAlpha,
    aaTextAlpha, aaWheelCenter, aaSpinAlpha, aaSpinRGB, aaInvalidCancel,
    aaListKeys);

  TAlphaDriver = class
    Timer: TTimer;
    Text, Error, ExpectedEntry: string;
    Action: TAlphaAction;
    procedure Tick(Sender: TObject);
  end;

  TPickerDriver = class
    Timer: TTimer;
    Confirm: boolean;
    Error: string;
    procedure Tick(Sender: TObject);
  end;

  TShotDriver = class
    Timer: TTimer;
    FileName, NewText: string;
    procedure Tick(Sender: TObject);
  end;

procedure TAncestorProvider.Modified(Sender: TObject);
begin
  Inc(Modifications);
end;

function TAncestorProvider.Find(const Entry: TInstProp;
  out Original: TInstProp): Boolean;
begin
  Original := Default(TInstProp);
  Original.Instance := Ancestor;
  Original.PropInfo := GetPropInfo(Ancestor, Entry.PropInfo^.Name);
  Result := Original.PropInfo <> nil;
end;

procedure TShotDriver.Tick(Sender: TObject);
var F: TCustomForm; B: TBitmap; E: TComponent;
begin
  F := Screen.ActiveCustomForm;
  if (F = nil) or (F.Caption <> 'RGBA color') then Exit;
  if NewText <> '' then
  begin
    { Apply the text, then capture the updated preview on the next timer tick. }
    E := F.FindComponent('ColorText');
    if E is TEdit then TEdit(E).Text := NewText;
    NewText := '';
    Exit;
  end;
  Timer.Enabled := False;
  B := F.GetFormImage;
  try
    B.SaveToFile(FileName);
  finally
    B.Free;
  end;
  F.ModalResult := mrCancel;
end;

procedure Check(OK: boolean; const Msg: string);
begin
  if not OK then raise Exception.Create(Msg);
end;

function BCToBGRA(AColor: TColor; AAlpha: byte; ADefault: TColor = clBlack): TBGRAPixel;
begin
  Result := TBCColorValue.New(AColor, AAlpha).ToBGRA(ADefault);
end;

function BCSameValue(const V1, V2: TBCColorValue): boolean;
begin
  Result := V1 = V2;
end;

function BCSameColor(const V1, V2: TBCColorValue): boolean;
begin
  Result := V1.SameColorAs(V2);
end;

constructor TColorHolder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := '#000000FF';
end;

procedure TestParser;
var P, Q: TBGRAPixel; A: integer;
begin
  for A := 0 to 255 do
  begin
    P := BGRA(17, 128, 239, A);
    Check(TryBCColor(BCColorToString(P), Q), 'parse roundtrip');
    Check(BCColorToString(Q) = BCColorToString(P), 'RGBA roundtrip');
  end;
  Check(TryBCColor(' #abCDef ', P), 'six digit + whitespace');
  Check(BCColorToString(P) = '#ABCDEFFF', 'six digit opaque');
  Check(TryBCColor('abCDef80', P) and (P.red = $AB) and (P.alpha = $80), 'hex without #');
  Check(not TryBCColor('#abCDef80AA', P) and not TryBCColor('abcde', P), 'hex without # length');
  Check(not TryBCColor('#12345', P), 'short input');
  Check(not TryBCColor('#1234567', P), 'odd input');
  Check(not TryBCColor('#123456GG', P), 'invalid hex');
  Check(not TryBCColor('', P), 'empty input');
  Check(not TryBCColor('clRed', P), 'symbolic not RGBA');
  for A := 0 to 255 do
  begin
    P := BCToBGRA(RGBToColor(17, 128, 239), A);
    Check((P.red = 17) and (P.green = 128) and (P.blue = 239) and
      (P.alpha = A), 'shared renderer preserves RGBA channels');
    Check(BCToBGRA(clNone, A).alpha = 0, 'shared renderer clNone');
    Check(BCToBGRA(clDefault, A) = BGRA(0, 0, 0, A),
      'shared renderer default matches picker');
  end;
  Check(BCToBGRA(clBtnFace, 99) = ColorToBGRA(ColorToRGB(clBtnFace), 99),
    'shared renderer resolves system colors');
  Check(TypeInfo(TBCColorString) <> TypeInfo(string), 'distinct string RTTI');
end;

procedure TestInheritedEditor(AClass: TComponentClass; const PropName: string;
  EditorClass: TPropertyEditorClass);
var A, B, Parent: TComponent; First, Second, Original: TObject;
  Hook: TPropertyEditorHook; Provider: TAncestorProvider; E: TPropertyEditor;
begin
  A := AClass.Create(nil);
  B := AClass.Create(nil);
  Parent := AClass.Create(nil);
  Hook := TPropertyEditorHook.Create(nil);
  Provider := TAncestorProvider.Create;
  E := EditorClass.Create(Hook, 2);
  try
    Provider.Ancestor := Parent;
    Hook.AddHandlerGetAncestorInstProp(@Provider.Find);
    Hook.AddHandlerModified(@Provider.Modified);
    First := GetObjectProp(A, PropName);
    Second := GetObjectProp(B, PropName);
    Original := GetObjectProp(Parent, PropName);
    if Original is TBCCustomColor then
    begin
      TBCCustomColor(Original).Assign(clBtnFace, 83);
      TBCCustomColor(First).Assign(clBlue, 10);
      TBCCustomColor(Second).Assign(clRed, 20);
    end
    else
    begin
      TBCPixel(Original).Assign(BGRA(12, 34, 56, 78));
      TBCPixel(First).Assign(clBlue);
      TBCPixel(Second).Assign(clRed);
    end;
    E.SetPropEntry(0, A, GetPropInfo(A, PropName));
    E.SetPropEntry(1, B, GetPropInfo(B, PropName));
    Check(paRevertable in E.GetAttributes, 'inherited revert available');
    E.RevertToInherited;
    Check(Provider.Modifications = 2, 'revert notifies each changed selection');
    E.RevertToInherited;
    Check(Provider.Modifications = 2, 'unchanged revert does not mark modified');
    if Original is TBCCustomColor then
    begin
      TBCCustomColor(First).Alpha := 1;
      E.RevertToInherited;
      Check(Provider.Modifications = 3,
        'mixed revert only marks the changed selection modified');
    end;
    Check((GetObjectProp(A, PropName) = First) and
      (GetObjectProp(B, PropName) = Second), 'revert keeps owned objects');
    if Original is TBCCustomColor then
    begin
      Check((TBCCustomColor(First).SameValueAs(TBCCustomColor(Original))) and
        (TBCCustomColor(Second).SameValueAs(TBCCustomColor(Original))),
        'alpha revert copies inherited values to multi-selection');
      TBCCustomColor(First).Alpha := 1;
      Check(TBCCustomColor(Original).Alpha = 83, 'ancestor not aliased');
      if A is TBCLinkedHolder then
        Check((TBCLinkedHolder(A).FillOpacity = 1) and
          (First is TBCLinkedColor), 'revert preserves the link');
    end
    else
    begin
      Check((TBCPixel(First).Pixel = TBCPixel(Original).Pixel) and
        (TBCPixel(Second).Pixel = TBCPixel(Original).Pixel),
        'pixel revert copies inherited values to multi-selection');
      Check((TBCPixelHolder(A).FChanges = 1) and
        (TBCPixelHolder(B).FChanges = 1), 'pixel revert notifies owners');

      TBCPixel(Original).Pixel := BGRA(12, 34, 56, 0);
      TBCPixel(First).Pixel := BGRA(255, 0, 0, 0);
      TBCPixel(Second).Pixel := BGRA(0, 0, 255, 0);
      E.RevertToInherited;
      Check((BCColorToString(TBCPixel(First).Pixel) = '#0C223800') and
        (BCColorToString(TBCPixel(Second).Pixel) = '#0C223800'),
        'pixel revert restores RGB even when all values are transparent');
      Check((TBCPixelHolder(A).FChanges = 2) and
        (TBCPixelHolder(B).FChanges = 2), 'transparent pixel revert notifies owners');
    end;
  finally
    E.Free; Hook.Free; Provider.Free;
    Parent.Free; B.Free; A.Free;
  end;
end;

procedure TestStringEditor;
var A, B: TColorHolder; E: TBCColorStringPropertyEditor; Failed: boolean;
begin
  A := TColorHolder.Create(nil);
  B := TColorHolder.Create(nil);
  E := TBCColorStringPropertyEditor.Create(nil, 2);
  try
    E.SetPropEntry(0, A, GetPropInfo(A, 'Color'));
    E.SetPropEntry(1, B, GetPropInfo(B, 'Color'));
    Check([paDialog, paCustomDrawn] <= E.GetAttributes, 'editor attributes');
    B.Color := '#FF000080';
    Check(not E.AllEqual, 'mixed selection');
    E.SetValue('#abcdef');
    Check((A.Color = '#ABCDEFFF') and (B.Color = '#ABCDEFFF'),
      'canonicalization and multi-edit');
    Failed := False;
    try E.SetValue('garbage'); except on EPropertyError do Failed := True; end;
    Check(Failed and (A.Color = '#ABCDEFFF'), 'invalid edit rejected');
  finally
    E.Free; B.Free; A.Free;
  end;
end;

function SaveText(C: TComponent): string;
var B: TMemoryStream; T: TStringStream;
begin
  B := TMemoryStream.Create;
  T := TStringStream.Create('');
  try
    B.WriteComponent(C);
    B.Position := 0;
    ObjectBinaryToText(B, T);
    Result := T.DataString;
  finally
    T.Free; B.Free;
  end;
end;

procedure LoadText(C: TComponent; const S: string);
var B: TMemoryStream; T: TStringStream;
begin
  B := TMemoryStream.Create;
  T := TStringStream.Create(S);
  try
    ObjectTextToBinary(T, B);
    B.Position := 0;
    B.ReadComponent(C);
  finally
    T.Free; B.Free;
  end;
end;

procedure TestStreaming;
var A, B: TColorHolder; S: string;
begin
  RegisterClass(TColorHolder);
  A := TColorHolder.Create(nil);
  B := TColorHolder.Create(nil);
  try
    A.Name := 'Holder';
    A.Color := '#11223344';
    S := SaveText(A);
    Check(Pos('''#11223344''', S) > 0, 'readable LFM value');
    LoadText(B, S);
    Check(B.Color = '#11223344', 'LFM roundtrip');
  finally
    B.Free; A.Free;
  end;
end;

{ White pixels and gray pixels, nothing else. }
function HasHatch(B: TBitmap): boolean;
var X, Y: integer; C: TColor; White, Gray: boolean;
begin
  White := False; Gray := False;
  for Y := 0 to B.Height - 1 do
    for X := 0 to B.Width - 1 do
    begin
      C := ColorToRGB(B.Canvas.Pixels[X, Y]);
      if C = clWhite then White := True
      else if (Red(C) = Green(C)) and (Green(C) = Blue(C)) and (Red(C) < 255) then Gray := True
      else Exit(False);
    end;
  Result := White and Gray;
end;

procedure TestSwatch;
var B: TBitmap;
begin
  B := TBitmap.Create;
  try
    B.SetSize(12, 6);
    DrawColor(B.Canvas, Rect(0,0,12,6), BGRA(255,0,0,0));
    Check(ColorToRGB(B.Canvas.Pixels[0,0]) = ColorToRGB(clWhite), 'transparent white square');
    Check(ColorToRGB(B.Canvas.Pixels[6,0]) = RGBToColor(192,192,192), 'transparent gray square');
    DrawColor(B.Canvas, Rect(0,0,12,6), BGRA(255,0,0,255));
    Check(ColorToRGB(B.Canvas.Pixels[6,0]) = ColorToRGB(clRed), 'opaque swatch');

    { clDefault: the given default color, or a white and gray hatch when it
      is not known (never a guessed color). }
    DrawColor(B.Canvas, Rect(0,0,12,6), TBCColorValue.New(scDefault), clBlue);
    Check(ColorToRGB(B.Canvas.Pixels[6,3]) = ColorToRGB(clBlue), 'clDefault with known default');
    DrawColor(B.Canvas, Rect(0,0,12,6), TBCColorValue.New(scDefault));
    Check(HasHatch(B), 'clDefault with unknown default drawn as a hatch');
    DrawColor(B.Canvas, Rect(0,0,12,6), TBCColorValue.New(clRed));
    Check(ColorToRGB(B.Canvas.Pixels[6,3]) = ColorToRGB(clRed), 'value swatch');
    DrawColor(B.Canvas, Rect(0,0,12,6), TBCColorValue.New(scHighlight));
    Check(ColorToRGB(B.Canvas.Pixels[6,3]) = ColorToRGB(clHighlight), 'system color swatch');
  finally
    B.Free;
  end;
end;

type
  TSpinChangeProbe = class
    Handler: TNotifyEvent;
    Fired: boolean;
    procedure Changed(Sender: TObject);
  end;

procedure TSpinChangeProbe.Changed(Sender: TObject);
begin
  Fired := True;
  if Assigned(Handler) then Handler(Sender);
end;

{ Sets a picker channel field as a user edit does: OnChange runs exactly once.
  Win32 and Qt notify programmatic Value changes, GTK2 does not
  (TGtk2WSCustomFloatSpinEdit.UpdateControl locks OnChange). }
procedure SetSpin(F: TCustomForm; const AName: string; AValue: integer);
var S: TSpinEdit; Probe: TSpinChangeProbe;
begin
  S := TSpinEdit(F.FindComponent(AName));
  Check(S <> nil, 'spin edit ' + AName);
  Probe := TSpinChangeProbe.Create;
  try
    Probe.Handler := S.OnChange;
    S.OnChange := @Probe.Changed;
    try
      S.Value := AValue;
    finally
      S.OnChange := Probe.Handler;
    end;
    if not Probe.Fired and Assigned(Probe.Handler) then
      Probe.Handler(S);
  finally
    Probe.Free;
  end;
end;

{ Selects a list item as the keyboard does: through the native widget, so
  the widgetset decides which events (OnChange, OnSelect) reach the picker.
  GTK2 sends OnSelect only when the index differs from the one it cached. }
procedure UserSelect(L: TComboBox; AIndex: integer);
begin
  {$IF defined(LCLGTK2)}
  gtk_combo_box_set_active(PGtkComboBox(GetWidgetInfo(Pointer(L.Handle))^.CoreWidget),
    AIndex);
  {$ELSEIF defined(LCLQT5) or defined(LCLQT6)}
  QComboBox_setCurrentIndex(QComboBoxH(TQtComboBox(L.Handle).Widget), AIndex);
  {$ELSE}
  L.ItemIndex := AIndex;
  L.OnSelect(L);
  {$ENDIF}
  Application.ProcessMessages;
end;

{ Clicks a picker surface at (X, Y); negative values count from the far edge. }
procedure ClickSurface(F: TCustomForm; const AName: string; X, Y: integer);
var S: TPaintBox;
begin
  S := TPaintBox(F.FindComponent(AName));
  Check(S <> nil, 'surface ' + AName);
  if X < 0 then X := S.Width + X;
  if Y < 0 then Y := S.Height + Y;
  S.OnMouseDown(S, mbLeft, [], X, Y);
  S.OnMouseUp(S, mbLeft, [], X, Y);
end;

procedure TPickerDriver.Tick(Sender: TObject);
var F: TCustomForm; Txt: TEdit; W: TPaintBox; P: TBGRAPixel;
begin
  F := Screen.ActiveCustomForm;
  if (F = nil) or (F.Caption <> 'RGBA color') then Exit;
  Timer.Enabled := False;
  try
    Check(F.FindComponent('SystemColor') = nil, 'no system color combo for pixels');
    Txt := TEdit(F.FindComponent('ColorText'));
    Txt.Text := '#FF000080';
    Check(Txt.Text = '#FF000080', 'typed text not rewritten');
    ClickSurface(F, 'AlphaBar', 0, -1);
    Check(Txt.Text = '#FF000000', 'opacity bar bottom keeps RGB');
    ClickSurface(F, 'LightBar', 0, -1);
    Check(Txt.Text = '#00000000', 'light bar bottom is black');
    ClickSurface(F, 'LightBar', 0, 0);
    Check(Txt.Text = '#FF000000', 'light bar keeps hue and saturation');
    W := TPaintBox(F.FindComponent('ColorWheel'));
    ClickSurface(F, 'ColorWheel', (W.Width - 1) div 2, (W.Height - 1) div 2);
    Check(Txt.Text = '#FFFFFF00', 'wheel center is white');
    ClickSurface(F, 'ColorWheel', W.Width - 1, (W.Height - 1) div 2);
    Check(TryBCColor(Txt.Text, P) and (P.red = 255) and (P.green <= 24) and
      (P.blue <= 24) and (P.alpha = 0), 'wheel right edge is red: ' + Txt.Text);
    ClickSurface(F, 'AlphaBar', 0, 0);
    Check(Copy(Txt.Text, 8, 2) = 'FF', 'opacity bar top is opaque');
    SetSpin(F, 'RedValue', $12);
    SetSpin(F, 'GreenValue', $34);
    SetSpin(F, 'BlueValue', $56);
    Check(Copy(Txt.Text, 1, 7) = '#123456', 'manual RGB entry: ' + Txt.Text);
    SetSpin(F, 'AlphaValue', $78);
    Check(Txt.Text = '#12345678', 'manual alpha entry: ' + Txt.Text);
    Txt.Text := '#9ABCDEF0';
    Check((TSpinEdit(F.FindComponent('RedValue')).Value = $9A) and
      (TSpinEdit(F.FindComponent('GreenValue')).Value = $BC) and
      (TSpinEdit(F.FindComponent('BlueValue')).Value = $DE) and
      (TSpinEdit(F.FindComponent('AlphaValue')).Value = $F0),
      'text updates channel fields');
    Txt.Text := 'clBtnFace';
    Check(Txt.Font.Color = clRed, 'color names rejected for pixels');
    Txt.Text := '#11223300';
  except
    on E: Exception do
    begin
      Error := E.Message;
      F.ModalResult := mrCancel;
      Exit;
    end;
  end;
  if Confirm then TButton(F.FindComponent('OKButton')).Click
  else F.ModalResult := mrCancel;
end;

procedure TestPicker;
var D: TPickerDriver; P: TBGRAPixel;
begin
  D := TPickerDriver.Create;
  D.Timer := TTimer.Create(nil);
  try
    D.Timer.Enabled := False;
    D.Timer.Interval := 100;
    D.Timer.OnTimer := @D.Tick;
    P := BGRA(10,20,30,40);
    D.Confirm := False;
    D.Timer.Enabled := True;
    Check(not ExecuteBCColorPicker(P), 'picker Cancel result');
    Check(D.Error = '', D.Error);
    Check(BCColorToString(P) = '#0A141E28', 'picker Cancel preserves caller');
    D.Confirm := True;
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(P), 'picker OK result: ' + D.Error);
    Check(D.Error = '', D.Error);
    Check(BCColorToString(P) = '#11223300', 'picker commits RGBA');
  finally
    D.Timer.Free; D.Free;
  end;
end;

{ Linked color test component. }

constructor TBCLinkedHolder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFillColor := clBtnFace;
  FFillOpacity := 255;
  FFill := TBCLinkedColor.Create(@GetFillValue, @SetFillValue);
end;

destructor TBCLinkedHolder.Destroy;
begin
  FFill.Free;
  inherited Destroy;
end;

procedure TBCLinkedHolder.GetFillValue(out AColor: TColor; out AAlpha: Byte);
begin
  AColor := FFillColor;
  AAlpha := FFillOpacity;
end;

procedure TBCLinkedHolder.SetFillValue(AColor: TColor; AAlpha: Byte);
begin
  FFillColor := AColor;
  FFillOpacity := AAlpha;
  Inc(FSets);
end;

procedure TestComboFocusBorderRefresh;
var
  C: TBCComboBox;
  B: TBCButtonAccess;
begin
  C := TBCComboBox.Create(nil);
  try
    B := TBCButtonAccess(C.Button);
    B.BGRANormal.NeedRender := False;
    C.FocusBorderAlphaColor.Assign(clRed, 128);
    Check(B.BGRANormal.NeedRender, 'focus border adapter invalidates button cache');
    Check((C.FocusBorderColor = clRed) and (C.FocusBorderOpacity = 128),
      'focus border adapter writes pair');
    B.BGRANormal.NeedRender := False;
    C.FocusBorderColor := clBlue;
    Check(B.BGRANormal.NeedRender, 'legacy focus color invalidates button cache');
    B.BGRANormal.NeedRender := False;
    C.FocusBorderOpacity := 64;
    Check(B.BGRANormal.NeedRender, 'legacy focus opacity invalidates button cache');
    Check(C.FocusBorderAlphaColor.SameValueAs(clBlue, 64),
      'focus adapter reads legacy changes');
    B.BGRANormal.NeedRender := False;
    C.FocusBorderColor := clBlue;
    C.FocusBorderOpacity := 64;
    C.FocusBorderAlphaColor.Assign(clBlue, 64);
    Check(not B.BGRANormal.NeedRender, 'unchanged focus pair keeps button cache');
  finally
    C.Free;
  end;
end;

{ Check combo box focus border adapter: editor registration, legacy pair,
  repaint and LFM. A separate root owns the combo, as a form does, so its
  internal button is not streamed. }
procedure TestCheckComboFocusBorder;
var
  Root, LoadedRoot: TDataModule;
  C, Loaded: TBCCheckComboBox;
  B: TBCButtonAccess;
  Text: string;
begin
  RegisterClass(TBCCheckComboBox);
  Root := TDataModule.CreateNew(nil);
  LoadedRoot := TDataModule.CreateNew(nil);
  try
    Root.Name := 'Root';
    C := TBCCheckComboBox.Create(Root);
    C.Name := 'CheckCombo';
    Check(GetEditorClass(GetPropInfo(C, 'FocusBorderAlphaColor'), C) = TBCColorPropertyEditor,
      'check combo focus adapter editor');
    Check(GetEditorClass(GetPropInfo(C, 'FocusBorderColor'), C) = nil,
      'check combo legacy focus color hidden');
    Check(GetEditorClass(GetPropInfo(C, 'FocusBorderOpacity'), C) = nil,
      'check combo legacy focus opacity hidden');
    Check(C.FocusBorderAlphaColor.SameValueAs(clBlack, 255),
      'check combo focus adapter default');
    B := TBCButtonAccess(C.Button);
    B.BGRANormal.NeedRender := False;
    C.FocusBorderAlphaColor.Assign(clHighlight, 128);
    Check(B.BGRANormal.NeedRender, 'check combo focus adapter invalidates button cache');
    Check((C.FocusBorderColor = clHighlight) and (C.FocusBorderOpacity = 128),
      'check combo focus adapter writes pair');
    B.BGRANormal.NeedRender := False;
    C.FocusBorderColor := clBlue;
    Check(B.BGRANormal.NeedRender, 'check combo legacy focus color invalidates button cache');
    B.BGRANormal.NeedRender := False;
    C.FocusBorderOpacity := 42;
    Check(B.BGRANormal.NeedRender, 'check combo legacy focus opacity invalidates button cache');
    Check(C.FocusBorderAlphaColor.SameValueAs(clBlue, 42),
      'check combo focus adapter reads legacy changes');
    B.BGRANormal.NeedRender := False;
    C.FocusBorderAlphaColor.Assign(clBlue, 42);
    Check(not B.BGRANormal.NeedRender, 'check combo unchanged focus pair keeps button cache');
    Text := SaveText(Root);
    Check(Pos('FocusBorderAlphaColor', Text) = 0, 'check combo focus adapter is not streamed');
    Check(Pos('FocusBorderOpacity = 42', Text) > 0, 'check combo legacy focus opacity is streamed');
    LoadText(LoadedRoot, Text);
    Loaded := LoadedRoot.FindComponent('CheckCombo') as TBCCheckComboBox;
    Check((Loaded <> nil) and Loaded.FocusBorderAlphaColor.SameValueAs(clBlue, 42),
      'check combo focus LFM roundtrip');
  finally
    LoadedRoot.Free;
    Root.Free;
  end;
end;

{ Knob position adapter: editor registration, legacy pair and LFM. }
procedure TestKnobLinkedColor;
var
  K, Loaded: TBGRAKnob;
  Text: string;
begin
  K := TBGRAKnob.Create(nil);
  Loaded := TBGRAKnob.Create(nil);
  try
    K.Name := 'Knob';
    Check(GetEditorClass(GetPropInfo(K, 'PositionAlphaColor'), K) = TBCColorPropertyEditor,
      'knob position adapter editor');
    Check(GetEditorClass(GetPropInfo(K, 'PositionColor'), K) = nil,
      'knob legacy position color hidden');
    Check(GetEditorClass(GetPropInfo(K, 'PositionOpacity'), K) = nil,
      'knob legacy position opacity hidden');
    Check(K.PositionAlphaColor.SameValueAs(clBtnText, 192), 'knob position adapter default');
    K.PositionAlphaColor.Assign(clHighlight, 73);
    Check((K.PositionColor = clHighlight) and (K.PositionOpacity = 73),
      'knob position adapter writes legacy pair');
    K.PositionAlphaColor.Alpha := 42;
    Check(K.PositionColor = clHighlight, 'knob position alpha preserves system color');
    K.PositionColor := clRed;
    K.PositionOpacity := 84;
    Check(K.PositionAlphaColor.SameValueAs(clRed, 84), 'knob position adapter reads legacy pair');
    Text := SaveText(K);
    Check(Pos('PositionAlphaColor', Text) = 0, 'knob position adapter is not streamed');
    Check(Pos('PositionOpacity = 84', Text) > 0, 'knob legacy position opacity is streamed');
    LoadText(Loaded, Text);
    Check(Loaded.PositionAlphaColor.SameValueAs(clRed, 84), 'knob position LFM roundtrip');
  finally
    Loaded.Free;
    K.Free;
  end;
end;

{ Spinner position fill adapter, in the PositionSettings sub-object: editor
  registration, legacy pair, change notification and LFM. }
procedure TestSpinnerLinkedColor;
var
  S, Loaded: TSuperSpinner;
  P: TSSPositionSettings;
  Text: string;
begin
  S := TSuperSpinner.Create(nil);
  Loaded := TSuperSpinner.Create(nil);
  try
    S.Name := 'Spinner';
    P := S.PositionSettings;
    Check(GetEditorClass(GetPropInfo(P, 'FillAlphaColor'), P) = TBCColorPropertyEditor,
      'spinner fill adapter editor');
    Check(GetEditorClass(GetPropInfo(P, 'FillColor'), P) = nil,
      'spinner legacy fill color hidden');
    Check(GetEditorClass(GetPropInfo(P, 'Opacity'), P) = nil,
      'spinner legacy opacity hidden');
    Check(P.FillAlphaColor.SameValueAs(clBlack, 192), 'spinner fill adapter default');
    P.Dirty := False;
    P.FillAlphaColor.Assign(clHighlight, 73);
    Check(P.Dirty, 'spinner fill adapter marks the settings changed');
    Check((P.FillColor = clHighlight) and (P.Opacity = 73),
      'spinner fill adapter writes legacy pair');
    P.Dirty := False;
    P.FillAlphaColor.Assign(clHighlight, 73);
    Check(not P.Dirty, 'spinner unchanged fill keeps the settings clean');
    P.FillColor := clRed;
    P.Opacity := 84;
    Check(P.FillAlphaColor.SameValueAs(clRed, 84), 'spinner fill adapter reads legacy pair');
    Text := SaveText(S);
    Check(Pos('FillAlphaColor', Text) = 0, 'spinner fill adapter is not streamed');
    Check(Pos('PositionSettings.Opacity = 84', Text) > 0, 'spinner legacy opacity is streamed');
    LoadText(Loaded, Text);
    Check(Loaded.PositionSettings.FillAlphaColor.SameValueAs(clRed, 84),
      'spinner fill LFM roundtrip');
  finally
    Loaded.Free;
    S.Free;
  end;
end;

{ SVG viewer background adapter over the inherited Color and ColorOpacity:
  editor registration, legacy pair and LFM. TBGRAGraphicControl, which has
  descendants that ignore ColorOpacity, keeps the separate pair. }
procedure TestSVGViewerLinkedColor;
var
  V, Loaded: TBCSVGViewer;
  G: TBGRAGraphicControl;
  Text: string;
begin
  V := TBCSVGViewer.Create(nil);
  Loaded := TBCSVGViewer.Create(nil);
  G := TBGRAGraphicControl.Create(nil);
  try
    V.Name := 'Viewer';
    Check(GetEditorClass(GetPropInfo(V, 'AlphaColor'), V) = TBCColorPropertyEditor,
      'SVG viewer adapter editor');
    Check(GetEditorClass(GetPropInfo(V, 'Color'), V) = nil,
      'SVG viewer legacy color hidden');
    Check(GetEditorClass(GetPropInfo(V, 'ColorOpacity'), V) = nil,
      'SVG viewer legacy opacity hidden');
    Check(GetEditorClass(GetPropInfo(G, 'Color'), G) <> nil,
      'graphic control color still visible');
    Check(GetEditorClass(GetPropInfo(G, 'ColorOpacity'), G) <> nil,
      'graphic control opacity still visible');
    Check(V.AlphaColor.SameValueAs(V.Color, V.ColorOpacity), 'SVG viewer adapter default');
    V.AlphaColor.Assign(clBtnFace, 73);
    Check((V.Color = clBtnFace) and (V.ColorOpacity = 73),
      'SVG viewer adapter writes legacy pair');
    V.AlphaColor.Alpha := 42;
    Check(V.Color = clBtnFace, 'SVG viewer alpha preserves system color');
    V.Color := clRed;
    V.ColorOpacity := 84;
    Check(V.AlphaColor.SameValueAs(clRed, 84), 'SVG viewer adapter reads legacy pair');
    Text := SaveText(V);
    Check(Pos('AlphaColor', Text) = 0, 'SVG viewer adapter is not streamed');
    Check((Pos('Color = clRed', Text) > 0) and (Pos('ColorOpacity = 84', Text) > 0),
      'SVG viewer legacy pair is streamed');
    LoadText(Loaded, Text);
    Check(Loaded.AlphaColor.SameValueAs(clRed, 84), 'SVG viewer LFM roundtrip');
  finally
    G.Free;
    Loaded.Free;
    V.Free;
  end;
end;

{ LED border adapter: editor registration, legacy pair and LFM. }
procedure TestLEDLinkedColor;
var
  L, Loaded: TSuperLED;
  Text: string;
begin
  L := TSuperLED.Create(nil);
  Loaded := TSuperLED.Create(nil);
  try
    L.Name := 'LED';
    Check(GetEditorClass(GetPropInfo(L, 'BorderAlphaColor'), L) = TBCColorPropertyEditor,
      'LED border adapter editor');
    Check(GetEditorClass(GetPropInfo(L, 'BorderColor'), L) = nil,
      'LED legacy border color hidden');
    Check(GetEditorClass(GetPropInfo(L, 'BorderOpacity'), L) = nil,
      'LED legacy border opacity hidden');
    Check(GetEditorClass(GetPropInfo(L, 'FillOpacity'), L) <> nil,
      'LED fill opacity, shared by the active and inactive colors, stays visible');
    Check(L.BorderAlphaColor.SameValueAs(clGray, 255), 'LED border adapter default');
    L.BorderAlphaColor.Assign(clHighlight, 73);
    Check((L.BorderColor = clHighlight) and (L.BorderOpacity = 73),
      'LED border adapter writes legacy pair');
    L.BorderAlphaColor.Alpha := 42;
    Check(L.BorderColor = clHighlight, 'LED border alpha preserves system color');
    L.BorderColor := clRed;
    L.BorderOpacity := 84;
    Check(L.BorderAlphaColor.SameValueAs(clRed, 84), 'LED border adapter reads legacy pair');
    Text := SaveText(L);
    Check(Pos('BorderAlphaColor', Text) = 0, 'LED border adapter is not streamed');
    Check(Pos('BorderOpacity = 84', Text) > 0, 'LED legacy border opacity is streamed');
    LoadText(Loaded, Text);
    Check(Loaded.BorderAlphaColor.SameValueAs(clRed, 84), 'LED border LFM roundtrip');
  finally
    Loaded.Free;
    L.Free;
  end;
end;

{ Shape adapters: editor registration, legacy pairs, value copies and LFM. }
procedure TestShapeLinkedColors;
var
  S, Loaded: TBGRAShape;
  Text: string;
begin
  S := TBGRAShape.Create(nil);
  Loaded := TBGRAShape.Create(nil);
  try
    S.Name := 'Shape';
    Check(GetEditorClass(GetPropInfo(S, 'FillAlphaColor'), S) = TBCColorPropertyEditor,
      'shape fill adapter editor');
    Check(GetEditorClass(GetPropInfo(S, 'BorderAlphaColor'), S) = TBCColorPropertyEditor,
      'shape border adapter editor');
    { GetEditorClass returns nil for properties registered as hidden. }
    Check(GetEditorClass(GetPropInfo(S, 'FillColor'), S) = nil,
      'shape legacy fill color hidden');
    Check(GetEditorClass(GetPropInfo(S, 'FillOpacity'), S) = nil,
      'shape legacy fill opacity hidden');
    Check(GetEditorClass(GetPropInfo(S, 'BorderColor'), S) = nil,
      'shape legacy border color hidden');
    Check(GetEditorClass(GetPropInfo(S, 'BorderOpacity'), S) = nil,
      'shape legacy border opacity hidden');
    S.FillAlphaColor.Assign(clBtnFace, 73);
    S.BorderAlphaColor.Assign(clHighlight, 121);
    Check((S.FillColor = clBtnFace) and (S.FillOpacity = 73),
      'shape fill adapter writes legacy pair');
    Check((S.BorderColor = clHighlight) and (S.BorderOpacity = 121),
      'shape border adapter writes legacy pair');
    S.FillAlphaColor.Alpha := 42;
    S.BorderAlphaColor.Alpha := 84;
    Check(S.FillColor = clBtnFace, 'shape fill alpha preserves system color');
    Check(S.BorderColor = clHighlight, 'shape border alpha preserves system color');
    Loaded.FillAlphaColor := S.FillAlphaColor;
    Loaded.BorderAlphaColor := S.BorderAlphaColor;
    S.FillAlphaColor.Alpha := 73;
    Check(Loaded.FillAlphaColor.SameValueAs(clBtnFace, 42),
      'shape adapter property assignment copies value, not link');
    Check(Loaded.BorderAlphaColor.SameValueAs(clHighlight, 84),
      'shape border adapter property assignment');
    S.FillColor := clRed;
    S.FillOpacity := 42;
    S.BorderColor := clBlue;
    S.BorderOpacity := 84;
    Check(S.FillAlphaColor.SameValueAs(clRed, 42), 'shape fill adapter reads legacy pair');
    Check(S.BorderAlphaColor.SameValueAs(clBlue, 84), 'shape border adapter reads legacy pair');
    Text := SaveText(S);
    Check((Pos('FillAlphaColor', Text) = 0) and
      (Pos('BorderAlphaColor', Text) = 0), 'shape adapters are not streamed');
    Check((Pos('FillOpacity = 42', Text) > 0) and
      (Pos('BorderOpacity = 84', Text) > 0), 'shape legacy opacity is streamed');
    LoadText(Loaded, Text);
    Check(Loaded.FillAlphaColor.SameValueAs(clRed, 42), 'shape fill LFM roundtrip');
    Check(Loaded.BorderAlphaColor.SameValueAs(clBlue, 84), 'shape border LFM roundtrip');
  finally
    Loaded.Free;
    S.Free;
  end;
end;

{ The two kinds of TBCCustomColor: shared editor, cross Assign, linked
  values streamed only by the owner's original pair. }
procedure TestLinkedColor;
var R, K: TBCLinkedHolder; C: TBCColorHolder;
  Changes: integer; Failed: boolean; S: string; L: TBCLinkedColor;
begin
  R := TBCLinkedHolder.Create(nil);
  K := TBCLinkedHolder.Create(nil);
  C := TBCColorHolder.Create(nil);
  try
    Check(GetEditorClass(GetPropInfo(C, 'Fill'), C) = TBCColorPropertyEditor,
      'editor registered for TBCColor');
    Check(GetEditorClass(GetPropInfo(R, 'Fill'), R) = TBCColorPropertyEditor,
      'editor registered for TBCLinkedColor');

    { Callbacks: one owner call per change, none when unchanged. }
    Check(K.Fill.SameValueAs(clBtnFace, 255), 'callback read');
    K.Fill.Assign(clRed, 40);
    Check((K.FSets = 1) and (K.FillColor = clRed) and (K.FillOpacity = 40),
      'callback writes the pair at once');
    K.Fill.Assign(clRed, 40);
    Check(K.FSets = 1, 'callback not called for unchanged value');
    K.FillOpacity := 7;
    Check(K.Fill.Alpha = 7, 'direct owner write visible');

    { Opacity-only change of a system color. }
    R.Fill.Assign(clBtnFace, 99);
    Check((R.FillColor = clBtnFace) and (R.FillOpacity = 99) and (R.FSets = 1),
      'linked write');

    { Assign across the two kinds. }
    Changes := C.FChanges;
    C.Fill.Assign(R.Fill);
    Check(C.Fill.SameValueAs(clBtnFace, 99) and (C.FChanges = Changes + 1),
      'TBCColor from TBCLinkedColor');
    K.Fill.Assign(C.Fill);
    Check(K.Fill.SameValueAs(R.Fill) and (K.FSets = 2),
      'TBCLinkedColor from TBCColor');

    { Check that Color and Alpha setters update the owner's pair once. }
    Changes := K.FSets;
    K.Fill.Color := clWindow;
    Check((K.Fill.Color = clWindow) and (K.FillColor = clWindow) and
      (K.FillOpacity = 99) and (K.FSets = Changes + 1), 'linked Color preserves alpha');
    K.Fill.Alpha := 76;
    Check((K.FillColor = clWindow) and (K.FillOpacity = 76) and
      (K.FSets = Changes + 2), 'linked Alpha preserves Color');
    K.Fill.Color := clWindow;
    K.Fill.Alpha := 76;
    Check(K.FSets = Changes + 2, 'unchanged legacy pair does not notify');
    K.Fill.Color := clNone;
    Check((K.Fill.Color = clNone) and (K.Fill.Alpha = 0), 'linked clNone normalization');

    { Only the original pair goes to the LFM. }
    R.Name := 'Linked';
    S := SaveText(R);
    Check((Pos('Fill.', S) = 0) and (Pos('FillOpacity = 99', S) > 0),
      'linked value streamed by the owner pair: ' + S);

    Failed := False;
    try
      L := TBCLinkedColor.Create(nil, nil);
      L.Free;
    except on EPropertyError do Failed := True; end;
    Check(Failed, 'callbacks required');
  finally
    C.Free; K.Free; R.Free;
  end;
end;

constructor TBCColorHolder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFill := TBCColor.Create(clRed, 128);
  FFill.OnChange := @FillChanged;
end;

destructor TBCColorHolder.Destroy;
begin
  FFill.Free;
  inherited Destroy;
end;

procedure TBCColorHolder.FillChanged(Sender: TObject);
begin
  Inc(FChanges);
end;

procedure TestSymbolicCSSNames;
var
  I, J, First: integer;
  K: TBCSymbolicColor;
  V: TBCColorValue;
  Canonical: string;
begin
  for I := Low(BCSymbolicCSSNames) to High(BCSymbolicCSSNames) do
  begin
    Check(BCSymbolicCSSNames[I].Name <> '', 'CSS table has no empty names');
    for J := Low(BCSymbolicCSSNames) to I - 1 do
      Check(not SameText(BCSymbolicCSSNames[I].Name, BCSymbolicCSSNames[J].Name),
        'CSS table names are unique');
    Check(TryCSSNameToSymbolic(LowerCase(BCSymbolicCSSNames[I].Name), K) and
      (K = BCSymbolicCSSNames[I].Kind), 'CSS table input maps to its kind');
    First := Low(BCSymbolicCSSNames);
    while BCSymbolicCSSNames[First].Kind <> K do Inc(First);
    Canonical := BCSymbolicCSSNames[First].Name;
    Check(BCToStr(TBCColorValue.New(K)) = Canonical,
      'first CSS name is canonical output');
    Check(TryStrToBC(Canonical, V) and (V.Kind = K), 'canonical CSS roundtrip');
  end;
  Check(not TryCSSNameToSymbolic('', K), 'empty CSS name rejected');
  Check(not TryCSSNameToSymbolic('NotAColor', K), 'unknown CSS name rejected');
  Check(BCToStr(TBCColorValue.New(scForm)) = 'clForm', 'unnamed kind keeps LCL name');
  Check(BCToStr(TBCColorValue.New(scDefault)) = 'default', 'default stays special');
  Check(BCToStr(TBCColorValue.New(scCustomRGB)) = 'black', 'custom RGB stays RGB');
end;

procedure TestColorTextReuse;
var
  I, R, G, B, A: integer;
  V: TBCColorValue;
  P: TBGRAPixel;
  S, Expected: string;
begin
  { All short hex values, including transparent nonblack RGB. }
  for I := 0 to $FFFF do
  begin
    R := ((I shr 12) and 15) * 17;
    G := ((I shr 8) and 15) * 17;
    B := ((I shr 4) and 15) * 17;
    A := (I and 15) * 17;
    Check(TryStrToBC('#' + IntToHex(I, 4), V), 'short RGBA parses');
    Check(V.ToBGRA.EqualsExactly(BGRA(R, G, B, A)), 'short RGBA decoding');
  end;
  for I := 0 to $FFF do
  begin
    Check(TryStrToBC('#' + IntToHex(I, 3), V), 'short RGB parses');
    Check(V.ToBGRA.EqualsExactly(BGRA(((I shr 8) and 15) * 17,
      ((I shr 4) and 15) * 17, (I and 15) * 17)), 'short RGB decoding');
  end;
  Check(not TryStrToBC('#12?4', V), 'partial short hex rejected');
  Check(not TryStrToBC('#12G4', V), 'invalid short hex rejected');
  Check(not TryStrToBC('1234', V), 'short hex requires hash');
  for I := 0 to CSSColors.Count - 1 do
  begin
    P := CSSColors[I];
    Expected := LowerCase(CSSColors.Name[CSSColors.IndexOfColor(P)]);
    S := BCToStr(TBCColorValue.New(P));
    Check(S = Expected, 'RGB output keeps first dictionary name lowercase');
    Check(TryStrToBC(S, V) and V.ToBGRA.EqualsExactly(P), 'named RGB roundtrip');
  end;
  Check(BCToStr(TBCColorValue.New(BGRA($AB, $CD, $EF))) = '#ABCDEF',
    'unnamed opaque RGB keeps uppercase hex');
  Check(BCToStr(TBCColorValue.New(BGRA(255, 0, 0, 0))) = 'rgb(from red r g b / 0%)',
    'transparent named RGB keeps its CSS name and RGB');
end;

procedure TestColorConversions;
var
  K: TBCLCLSystemColor;
  C: TColor;
  V: TBCColorValue;
  P: TBGRAPixel;
  A: integer;
begin
  for K := Low(TBCLCLSystemColor) to High(TBCLCLSystemColor) do
  begin
    if K = scReserved25 then Continue;
    V := TBCColorValue.New(K);
    Check(V.TryToColor(C), 'LCL system color conversion');
    for A := 0 to 255 do
    begin
      V := TBCColorValue.New(C, A);
      Check((V.Kind = K) and (V.ToColor = C), 'system identity preserved');
      Check(BCColorToString(V.ToBGRA) = BCColorToString(ColorToBGRA(C, A)),
        'system pixel uses BGRABitmap conversion');
      Check((V.Kind = K) and (V.Alpha = A), 'resolution does not change value');
    end;
  end;
  for A := 0 to 255 do
  begin
    C := RGBToColor(17, 128, 239);
    V := TBCColorValue.New(C, A);
    Check((V.Kind = scCustomRGB) and (V.ToColor = C), 'RGB TColor roundtrip');
    Check(BCColorToString(V.ToBGRA) = BCColorToString(ColorToBGRA(C, A)),
      'RGB conversion preserves all channels including transparent RGB');
    Check(TryStrToBC('#1180EF' + IntToHex(A, 2), V) and
      TryCSSColor('#1180EF' + IntToHex(A, 2), P), 'both hex parsers accept RGBA');
    Check(BCColorToString(V.ToBGRA) = BCColorToString(P), 'hex parsers agree');
  end;
  Check(not TryStrToBC('#11??EF80', V), 'symbolic parser rejects partial hex');
end;

procedure TestAlphaColor;
var A: TBCColor;

  function FC(C: TColor; Alpha: byte): TBCColorValue;
  begin
    Result := TBCColorValue.New(C, Alpha);
  end;

  procedure RoundTrip(const S, Expected: string; const Expect: TBCColorValue);
  var V, W: TBCColorValue; T: string;
  begin
    Check(TryStrToBC(S, V), 'parse ' + S);
    Check(BCSameValue(V, Expect), 'parsed value of ' + S);
    T := BCToStr(V);
    Check(T = Expected, 'BCToStr of ' + S + ': ' + T);
    Check(TryStrToBC(T, W) and BCSameValue(W, V), 'roundtrip of ' + S);
  end;

  procedure Invalid(const S: string);
  var V: TBCColorValue;
  begin
    Check(not TryStrToBC(S, V), 'reject ' + S);
  end;

begin
  RoundTrip('#11223344', '#11223344', TBCColorValue.New(BGRA($11, $22, $33, $44)));
  RoundTrip(' #abcdef ', '#ABCDEF', TBCColorValue.New(BGRA($AB, $CD, $EF)));
  RoundTrip('#FF000000', 'rgb(from red r g b / 0%)', TBCColorValue.New(BGRA(255, 0, 0, 0)));
  RoundTrip('red', 'red', FC(clRed, 255));
  RoundTrip('clRed', 'red', FC(clRed, 255));
  RoundTrip('rgb(from ThreeDFace r g b / 50%)',
    'rgb(from ThreeDFace r g b / 50%)', TBCColorValue.New(scBtnFace, 128));
  RoundTrip('clBtnFace', 'ThreeDFace', TBCColorValue.New(scBtnFace));
  RoundTrip('ThreeDShadow', 'ButtonShadow', TBCColorValue.New(scBtnShadow));
  RoundTrip('threedhighlight', 'ButtonHighlight', TBCColorValue.New(scBtnHighlight));
  RoundTrip('ButtonFace', 'ThreeDFace', TBCColorValue.New(scBtnFace));
  RoundTrip('rgb(from ThreeDShadow r g b / 50%)',
    'rgb(from ButtonShadow r g b / 50%)', TBCColorValue.New(scBtnShadow, 128));
  { Check rejection of unsupported modern CSS system names. }
  Invalid('Canvas');
  Invalid('CanvasText');
  Invalid('Field');
  Invalid('FieldText');
  Invalid('ButtonBorder');
  Invalid('LinkText');
  Invalid('SelectedItem');
  Invalid('SelectedItemText');
  Invalid('AccentColor');
  Invalid('AccentColorText');
  Invalid('Mark');
  Invalid('MarkText');
  Invalid('ActiveText');
  Invalid('VisitedText');
  Invalid('rgb(from SelectedItem r g b / 50%)');
  Invalid('rgb(from LinkText r g b / 50%)');
  RoundTrip('clNone', 'transparent', TBCColorValue.New(scNone));
  RoundTrip('rgb(from default r g b / 25%)', 'rgb(from default r g b / 25%)',
    TBCColorValue.New(scDefault, 64));
  RoundTrip('rgb(from ThreeDFace r g b / 0%)', 'rgb(from ThreeDFace r g b / 0%)',
    TBCColorValue.New(scBtnFace, 0));
  Invalid('');
  Invalid('#12345');
  Invalid('clNotAColor');
  Invalid('11223344');      { Reject hex without '#'. }
  Invalid('clRed@80');      { Reject the unsupported '@' alpha suffix. }
  Invalid('@80');

  Check(BCToBGRA(clBtnFace, 128) =
    ColorToBGRA(ColorToRGB(clBtnFace), 128), 'system color resolved');
  Check(BCToBGRA(clNone, 200) = BGRAPixelTransparent,
    'clNone transparent');
  Check(BCToBGRA(clDefault, 64) = BGRA(0, 0, 0, 64),
    'clDefault black without a default');
  Check(BCToBGRA(clDefault, 64, clBlue) = BGRA(0, 0, 255, 64),
    'clDefault uses caller default');
  Check(BCToBGRA(clDefault, 64, clBtnFace) =
    ColorToBGRA(ColorToRGB(clBtnFace), 64), 'clDefault resolved to system color');
  Check(BCToBGRA(clDefault, 255, clNone) = BGRAPixelTransparent,
    'clDefault resolved to clNone');
  Check(BCToBGRA(clDefault, 64, clDefault) = BGRA(0, 0, 0, 64),
    'clDefault as default gives black');

  { Stored values. }
  Check(BCSameValue(FC(clBtnFace, 128), FC(clBtnFace, 128)), 'equal values');
  Check(not BCSameValue(FC(clBtnFace, 128), FC(clBtnFace, 127)), 'alpha differs');
  Check(not BCSameValue(FC(clBtnFace, 128), FC(clBtnShadow, 128)), 'color differs');
  Check(BCSameValue(FC(clNone, 1), FC(clNone, 255)), 'clNone: alpha not used');
  Check(not BCSameValue(FC(clBtnFace, 255), FC(ColorToRGB(clBtnFace), 255)),
    'system color differs from its RGB value');

  { Drawn pixel. }
  Check(BCSameColor(FC(clNone, 255), FC(clRed, 0)), 'clNone looks like alpha 0');
  Check(BCSameColor(FC(clRed, 0), FC(clBlue, 0)), 'any alpha 0 looks the same');
  Check(not BCSameColor(FC(clDefault, 255), FC(clBlack, 255)),
    'clDefault is not black');
  Check(BCSameColor(FC(clDefault, 9), FC(clDefault, 9)) and
    not BCSameColor(FC(clDefault, 9), FC(clDefault, 8)), 'clDefault matches clDefault');
  Check(BCSameColor(FC(clDefault, 0), FC(clRed, 0)), 'transparent clDefault');
  Check(BCSameColor(FC(clBtnFace, 99), FC(ColorToRGB(clBtnFace), 99)),
    'system color looks like its RGB value');
  Check(not BCSameColor(FC(clRed, 99), FC(clRed, 98)), 'alpha visible');
  Check(not BCSameColor(FC(clNone, 255), FC(clBlack, 1)),
    'clNone differs from almost transparent black');

  { Same helpers through an object. }
  A := TBCColor.Create(clBtnFace, 128);
  try
    Check(A.ToBGRA = ColorToBGRA(ColorToRGB(clBtnFace), 128), 'ToBGRA');
    Check(A.ToString = 'rgb(from ThreeDFace r g b / 50%)', 'ToString: ' + A.ToString);
    Check(A.SameValueAs(clBtnFace, 128) and not A.SameValueAs(clBtnFace, 127),
      'SameValueAs');
    Check(A.SameColorAs(FC(ColorToRGB(clBtnFace), 128)) and
      not A.SameValueAs(ColorToRGB(clBtnFace), 128), 'SameColorAs');
    Check((A.Kind = scBtnFace) and (A.Alpha = 128),
      'published fields of a system color');
    A.Assign(clNone, 1);
    Check(A.SameValueAs(clNone, 255) and A.SameColorAs(FC(clRed, 0)),
      'clNone: transparent, alpha not used');
    A.Assign(clDefault, 99);
    Check(A.ToBGRA(clBlue) = BGRA(0, 0, 255, 99), 'ToBGRA with default');
  finally
    A.Free;
  end;
end;

procedure TestBCColorAssign;
var C: TBCColor;
begin
  C := TBCColor.Create(clRed, 128);
  try
    C.Assign(clBtnFace);
    Check(C.SameValueAs(clBtnFace, 255), 'Assign(TColor)');
    C.Assign(clBtnFace, 40);
    Check(C.SameValueAs(clBtnFace, 40), 'Assign(TColor, Alpha)');
    C.Assign(BGRA(1, 2, 3, 4));
    Check(C.SameValueAs(RGBToColor(1, 2, 3), 4), 'Assign(TBGRAPixel)');
    Check(C.Assign('rgb(from Window r g b / 50%)') and
      (C.SameValueAs(clWindow, $80)), 'Assign(string)');
    Check(not C.Assign('garbage') and
      (C.SameValueAs(clWindow, $80)), 'invalid Assign(string) unchanged');
    C.Assign(clRed);
    Check(C.SameValueAs(clRed, 255), 'Assign(TColor) is opaque');
    C.Assign(BGRA(0, 0, 255, 7));
    Check(C.SameValueAs(clBlue, 7), 'Assign(TBGRAPixel) keeps alpha');
    C.Assign(clBtnFace, 40);
    C.Red := 0;
    C.Green := 9;
    C.Blue := 0;
    Check(C.SameValueAs(TBCColorValue.New(BGRA(0, 9, 0, 40))) and (C.Kind = scCustomRGB),
      'setting RGB channels keeps opacity and creates a custom color');
    C.Value := TBCColorValue.New(scHighlight, C.Alpha);
    Check((C.Kind = scHighlight) and (C.Alpha = 40), 'Value sets symbolic identity');
    C.Alpha := 76;
    Check(C.SameValueAs(clHighlight, 76), 'Alpha keeps symbolic identity');
    Check((GetPropInfo(C, 'Color') = nil) and (GetPropInfo(C, 'Kind') = nil) and
      (GetPropInfo(C, 'Alpha') = nil) and (GetPropInfo(C, 'Red') = nil) and
      (GetPropInfo(C, 'Green') = nil) and (GetPropInfo(C, 'Blue') = nil),
      'color value API has no published legacy or channel properties');
  finally
    C.Free;
  end;
end;

procedure TestBCColorProperty;
var A, B: TBCColorHolder; S: string;
begin
  RegisterClass(TBCColorHolder);
  A := TBCColorHolder.Create(nil);
  B := TBCColorHolder.Create(nil);
  try
    Check(A.Fill.SameValueAs(clRed, 128), 'constructor value');
    A.Fill.Alpha := 128;
    A.Fill.Red := 255;
    A.Fill.Assign(clRed, 128);
    Check(A.FChanges = 0, 'no OnChange for unchanged values');
    A.Fill.Alpha := 10;
    A.Fill.Value := TBCColorValue.New(clBlue, A.Fill.Alpha);
    A.Fill.Assign(clBtnFace, 20);
    Check(A.FChanges = 3, 'OnChange once per change');
    A.Fill.Assign(clNone, 1);
    A.Fill.Assign(clNone, 2);
    Check((A.FChanges = 4) and (A.Fill.Alpha = 0), 'clNone is transparent, alpha not used');

    { The value must survive a save and reload, also when it equals the
      constructor value of the reloaded object (alpha 255 vs 128). }
    A.Name := 'Holder';
    A.Fill.Assign(clRed, 255);
    S := SaveText(A);
    LoadText(B, S);
    Check(B.Fill.SameValueAs(clRed, 255), 'alpha reloaded: ' + S);
    Check((Pos('Fill.KindName = scCustomRGB', S) > 0) and
      (Pos('Fill.Color = ''#FF0000FF''', S) > 0) and (Pos('Fill.Alpha', S) = 0),
      'custom color written as BGRA Color + KindName: ' + S);

    A.Fill.Assign(clBtnFace, 40);
    S := SaveText(A);
    Check((Pos('Fill.KindName = scBtnFace', S) > 0) and
      (Pos('Fill.Color = ''#00000028''', S) > 0) and (Pos('Fill.Alpha', S) = 0),
      'system color written as stored BGRA Color + KindName: ' + S);
    B.Fill.Assign(BGRA(1, 2, 3, 4));
    LoadText(B, S);
    Check(B.Fill.SameValueAs(clBtnFace, 40), 'system color reloaded: ' + S);

    A.Fill.Assign(BGRA(9, 8, 7, 0));
    S := SaveText(A);
    LoadText(B, S);
    Check(B.Fill.SameValueAs(TBCColorValue.New(BGRA(9, 8, 7, 0))),
      'RGB of a fully transparent custom color reloaded: ' + S);

    { Check that an unknown KindName loads as scDefault. }
    A.Fill.Assign(clBtnFace, 40);
    S := StringReplace(SaveText(A), 'scBtnFace', 'scFutureColor', []);
    LoadText(B, S);
    Check(B.Fill.SameValueAs(clDefault, 40), 'unknown KindName read as default: ' + B.Fill.ToString);

    B.Fill.Assign(A.Fill);
    Check(B.Fill.SameValueAs(A.Fill), 'Assign');
  finally
    B.Free; A.Free;
  end;
end;

{ Inherited form: Fill is written only when it differs from the ancestor. }
procedure TestAncestorStreaming;
var
  A, D: TBCColorHolder;

  function DescendentText: string;
  var B: TMemoryStream; T: TStringStream;
  begin
    B := TMemoryStream.Create;
    T := TStringStream.Create('');
    try
      B.WriteDescendent(D, A);
      B.Position := 0;
      ObjectBinaryToText(B, T);
      Result := T.DataString;
    finally
      T.Free; B.Free;
    end;
  end;

var
  S: string;
begin
  A := TBCColorHolder.Create(nil);
  D := TBCColorHolder.Create(nil);
  try
    A.Fill.Assign(clBtnFace, 40);
    D.Fill.Assign(clBtnFace, 40);
    S := DescendentText;
    Check((Pos('Fill.Color', S) = 0) and (Pos('Fill.KindName', S) = 0),
      'equal to ancestor, not written: ' + S);

    D.Fill.Assign(clBtnFace, 41);
    S := DescendentText;
    Check((Pos('Fill.Color', S) > 0) and (Pos('Fill.KindName', S) > 0),
      'alpha differs from ancestor, written: ' + S);

    D.Fill.Assign(clWindow, 40);
    S := DescendentText;
    Check(Pos('scWindow', S) > 0, 'kind differs from ancestor, written: ' + S);

    LoadText(A, S);
    Check(A.Fill.SameValueAs(D.Fill), 'descendant text reloads: ' + A.Fill.ToString);
  finally
    D.Free; A.Free;
  end;
end;

procedure TestDefinedColorStreaming;
var
  A, B: TBCColorHolder;
  K: TBCSymbolicColor;
  Alpha: integer;
  S: string;
  Failed: boolean;
begin
  A := TBCColorHolder.Create(nil);
  B := TBCColorHolder.Create(nil);
  try
    A.Name := 'Holder';
    for K := Low(TBCSymbolicColor) to High(TBCSymbolicColor) do
    begin
      if K = scReserved25 then Continue;
      for Alpha := 0 to 255 do
      begin
        if K = scCustomRGB then
          A.Fill.Assign(BGRA(9, 8, 7, Alpha))
        else
          A.Fill.Assign(TBCColorValue.New(K, Alpha));
        S := SaveText(A);
        { Loading into transparent scNone must not lose a stored alpha. }
        B.Fill.Assign(clNone);
        LoadText(B, S);
        Check(B.Fill.SameValueAs(A.Fill), 'defined properties roundtrip: ' + S);
      end;
    end;

    { Literal LFM, independent of the writer: BGRA Color, alpha included. }
    LoadText(B, 'object Holder: TBCColorHolder' + LineEnding +
      '  Fill.Color = ''#0708094C''' + LineEnding +
      '  Fill.KindName = scCustomRGB' + LineEnding + 'end');
    Check(B.Fill.Value = TBCColorValue.New(BGRA(7, 8, 9, 76)), 'BGRA Color LFM');
    LoadText(B, 'object Holder: TBCColorHolder' + LineEnding +
      '  Fill.Color = ''#07080900''' + LineEnding +
      '  Fill.KindName = scCustomRGB' + LineEnding + 'end');
    Check(B.Fill.Value = TBCColorValue.New(BGRA(7, 8, 9, 0)),
      'BGRA Color LFM preserves transparent channels');
    LoadText(B, 'object Holder: TBCColorHolder' + LineEnding +
      '  Fill.Color = ''#00000028''' + LineEnding +
      '  Fill.KindName = scBtnFace' + LineEnding + 'end');
    Check(B.Fill.SameValueAs(clBtnFace, 40), 'BGRA Color + symbolic KindName LFM');

    { Only the BGRA text is a Color: no TColor values, no separate Alpha. }
    for Alpha := 0 to 3 do
    begin
      case Alpha of
        0: S := '  Fill.Color = ''notacolor''';
        1: S := '  Fill.Color = clRed';
        2: S := '  Fill.Color = $00070809';
      else
        S := '  Fill.Alpha = 76';
      end;
      Failed := False;
      try
        LoadText(B, 'object Holder: TBCColorHolder' + LineEnding + S +
          LineEnding + 'end');
      except
        on EReadError do Failed := True;
      end;
      Check(Failed, 'non-BGRA color data rejected: ' + S);
    end;
  finally
    B.Free;
    A.Free;
  end;
end;

procedure TestBCColorEditor;
var A, B: TBCColorHolder; E: TBCColorPropertyEditor; D: TAlphaDriver;
begin
  A := TBCColorHolder.Create(nil);
  B := TBCColorHolder.Create(nil);
  E := TBCColorPropertyEditor.Create(nil, 2);
  D := TAlphaDriver.Create;
  D.Timer := TTimer.Create(nil);
  try
    E.SetPropEntry(0, A, GetPropInfo(A, 'Fill'));
    E.SetPropEntry(1, B, GetPropInfo(B, 'Fill'));
    Check([paDialog, paCustomDrawn, paMultiSelect, paReadOnly] <= E.GetAttributes,
      'TBCColor editor attributes');
    Check(not (paSubProperties in E.GetAttributes), 'Color/Alpha not listed');
    Check(E.AllEqual and (E.GetValue = 'rgb(from red r g b / 50%)'), 'editor value: ' + E.GetValue);
    B.Fill.Alpha := 1;
    Check(not E.AllEqual, 'TBCColor mixed selection');

    { Check that typed property text is rejected and the dialog edits the value. }
    D.Timer.Interval := 100;
    D.Timer.OnTimer := @D.Tick;
    D.Action := aaText;
    D.Text := 'rgb(from ThreeDFace r g b / 25%)';
    D.Timer.Enabled := True;
    E.Edit;
    Check(D.Error = '', 'picker from editor: ' + D.Error);
    Check((A.Fill.SameValueAs(clBtnFace, $40)) and
      (B.Fill.SameValueAs(A.Fill)), 'TBCColor multi-edit through the picker');
    Check(E.GetValue = 'rgb(from ThreeDFace r g b / 25%)', 'editor shows color name');
  finally
    D.Timer.Free; D.Free;
    E.Free; B.Free; A.Free;
  end;
end;

constructor TBCPixelHolder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPixel := TBCPixel.Create(nil, BGRA(255, 0, 0, 128));
  FPixel.OnChange := @PixelChanged;
end;

destructor TBCPixelHolder.Destroy;
begin
  FPixel.Free;
  inherited Destroy;
end;

procedure TBCPixelHolder.PixelChanged(ASender: TObject; AData: PtrInt);
begin
  Inc(FChanges);
end;

procedure TPixelDriver.Tick(Sender: TObject);
var F: TCustomForm; L: TComboBox;
begin
  F := Screen.ActiveCustomForm;
  if (F = nil) or (F.Caption <> 'RGBA color') then Exit;
  Timer.Enabled := False;
  try
    L := TComboBox(F.FindComponent('SystemColor'));
    Check(L <> nil, 'TBCPixel picker shows the color list');
    Check(not L.Enabled and (L.Items.Count = 1) and (L.ItemIndex = 0) and
      (L.Items[0] = 'Custom color'), 'color list disabled on Custom color');
    TEdit(F.FindComponent('ColorText')).Text := Text;
    Check(L.ItemIndex = 0, 'typed RGB keeps Custom color');
    Check(TEdit(F.FindComponent('ColorText')).Font.Color <> clRed,
      'typed RGB accepted');
  except
    on E: Exception do
    begin
      Error := E.Message;
      F.ModalResult := mrCancel;
      Exit;
    end;
  end;
  TButton(F.FindComponent('OKButton')).Click;
end;

procedure TCSSDriver.Tick(Sender: TObject);
var F: TCustomForm; L: TComboBox; E: TEdit;
begin
  F := Screen.ActiveCustomForm;
  if (F = nil) or (F.Caption <> 'RGBA color') then Exit;
  Timer.Enabled := False;
  try
    L := TComboBox(F.FindComponent('SystemColor'));
    Check((L <> nil) and not L.Enabled and (L.ItemIndex = 0),
      'CSS picker: color list disabled on Custom color');
    E := TEdit(F.FindComponent('ColorText'));
    Shown := E.Text;
    case Action of
      caNone: ;
      caSpinAlpha: SetSpin(F, 'AlphaValue', 64);
      caText: E.Text := Text;
    end;
    Check(E.Font.Color <> clRed, 'CSS picker accepts the text');
  except
    on Ex: Exception do
    begin
      Error := Ex.Message;
      F.ModalResult := mrCancel;
      Exit;
    end;
  end;
  TButton(F.FindComponent('OKButton')).Click;
end;

procedure TestCSSColor;
var P, Q: TBGRAPixel; I: integer; S: string;
begin
  Check(TryCSSColor('#FF000080', P) and (P = BGRA(255, 0, 0, 128)), 'CSS #RRGGBBAA');
  Check(TryCSSColor(' rgba(255,255,255,0.5) ', P) and (P.alpha in [127, 128]) and
    (P.red = 255), 'CSS rgba()');
  Check(TryCSSColor('red', P) and (P = BGRA(255, 0, 0, 255)), 'CSS color name');
  Check(not TryCSSColor('', P), 'CSS empty rejected');
  Check(not TryCSSColor('nocolor', P), 'CSS invalid rejected');
  for I := 0 to 255 do
  begin
    P := BGRA(10, 20, 30, I);
    S := FormatCSSColor(P, 'rgba(0,0,0,0.5)');
    Check(TryCSSColor(S, Q) and (Q = P), 'CSS rgba roundtrip ' + S);
    S := FormatCSSColor(P, '#000000');
    Check(TryCSSColor(S, Q) and (Q = P), 'CSS hex roundtrip ' + S);
  end;
  Check(FormatCSSColor(BGRA(1, 2, 3), 'rgba(0,0,0,0.5)') = 'rgb(1,2,3)', 'CSS opaque rgb');
  Check(FormatCSSColor(BGRA(1, 2, 3), '#000000') = '#010203', 'CSS 6 digit hex');
  Check(FormatCSSColor(BGRA(1, 2, 3), 'red') = '#010203FF', 'CSS name style');
end;

procedure TestCSSPicker;
var D: TCSSDriver; S: string; P: TBGRAPixel;
begin
  D := TCSSDriver.Create;
  D.Timer := TTimer.Create(nil);
  try
    D.Timer.Interval := 100;
    D.Timer.OnTimer := @D.Tick;

    S := 'rgba(255,255,255,0.5)';
    D.Action := caNone;
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(S) and (D.Error = ''), 'CSS pick unchanged: ' + D.Error);
    Check(D.Shown = 'rgba(255,255,255,0.5)', 'CSS picker shows the text as given');
    Check(S = 'rgba(255,255,255,0.5)', 'CSS unchanged color keeps the text');

    D.Action := caSpinAlpha;
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(S) and (D.Error = ''), 'CSS pick alpha: ' + D.Error);
    Check((Copy(S, 1, 5) = 'rgba(') and TryCSSColor(S, P) and
      (P = BGRA(255, 255, 255, 64)), 'CSS result keeps rgba style: ' + S);

    D.Action := caText;
    D.Text := 'blue';
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(S) and (D.Error = ''), 'CSS typed name: ' + D.Error);
    Check(S = 'blue', 'CSS typed text returned as written: ' + S);

    S := 'rgba(255,0,0,1)';
    D.Action := caText;
    D.Text := '#FF0000FF';
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(S) and (D.Error = ''), 'CSS equivalent text: ' + D.Error);
    Check(S = 'rgba(255,0,0,1)', 'unchanged pixel preserves original CSS');

    S := '#FF000000';
    D.Action := caText;
    D.Text := '#0000FF00';
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(S) and (D.Error = ''), 'CSS transparent RGB edit: ' + D.Error);
    Check(S = '#0000FF00', 'CSS transparent RGB change is saved');

    S := 'rgba(0,0,255,0)';
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(S) and (D.Error = ''), 'CSS transparent equivalent text: ' + D.Error);
    Check(S = 'rgba(0,0,255,0)', 'identical transparent RGBA preserves original CSS');

    S := 'not a color';
    D.Action := caNone;
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(S) and (D.Error = ''), 'CSS invalid start: ' + D.Error);
    Check(TryCSSColor(S, P) and (P = BGRABlack), 'CSS invalid text starts from black');
  finally
    D.Timer.Free; D.Free;
  end;
end;

procedure TestCSSEditor;
var A, B: TCSSHolder; E: TBCCSSColorPropertyEditor; T: TBGRASVGTheme;
  Failed: boolean;
begin
  A := TCSSHolder.Create(nil);
  B := TCSSHolder.Create(nil);
  T := TBGRASVGTheme.Create(nil);
  E := TBCCSSColorPropertyEditor.Create(nil, 2);
  try
    Check(GetEditorClass(GetPropInfo(A, 'Color'), A) = TBCCSSColorPropertyEditor,
      'TBCCSSColorString editor registered');
    Check(GetEditorClass(GetPropInfo(T, 'ColorizeHover'), T) = TBCCSSColorPropertyEditor,
      'TBGRASVGTheme.ColorizeHover uses the CSS editor');
    Check(GetEditorClass(GetPropInfo(T, 'Name'), T) <> TBCCSSColorPropertyEditor,
      'other string properties unchanged');
    E.SetPropEntry(0, A, GetPropInfo(A, 'Color'));
    E.SetPropEntry(1, B, GetPropInfo(B, 'Color'));
    Check([paDialog, paCustomDrawn, paMultiSelect] <= E.GetAttributes,
      'CSS editor attributes');
    Check(not (paReadOnly in E.GetAttributes), 'CSS text can be typed');
    E.SetValue(' rgba(1,2,3,0.5) ');
    Check((A.Color = 'rgba(1,2,3,0.5)') and (B.Color = A.Color), 'CSS text kept as typed');
    E.SetValue('');
    Check(A.Color = '', 'CSS empty allowed');
    Failed := False;
    try
      E.SetValue('nocolor');
    except
      on EPropertyError do Failed := True;
    end;
    Check(Failed and (A.Color = ''), 'CSS invalid text rejected');
  finally
    E.Free; T.Free; B.Free; A.Free;
  end;
end;

procedure TestBCPixelEditor;
var A, B: TBCPixelHolder; E: TBCPixelPropertyEditor; D: TPixelDriver;
begin
  A := TBCPixelHolder.Create(nil);
  B := TBCPixelHolder.Create(nil);
  E := TBCPixelPropertyEditor.Create(nil, 2);
  D := TPixelDriver.Create;
  D.Timer := TTimer.Create(nil);
  BCColorPropertyEditor.Register;
  try
    Check(GetEditorClass(GetPropInfo(A, 'Pixel'), A) = TBCPixelPropertyEditor,
      'TBCPixel editor registered');
    E.SetPropEntry(0, A, GetPropInfo(A, 'Pixel'));
    E.SetPropEntry(1, B, GetPropInfo(B, 'Pixel'));
    Check([paDialog, paCustomDrawn, paMultiSelect, paReadOnly] <= E.GetAttributes,
      'TBCPixel editor attributes');
    Check(not (paSubProperties in E.GetAttributes), 'Red/Green/Blue/Alpha not listed');
    Check(E.AllEqual and (E.GetValue = '#FF000080'), 'TBCPixel editor value');
    B.Pixel.Pixel := BGRA(255, 0, 0, 1);
    Check(not E.AllEqual, 'TBCPixel mixed selection');

    D.Timer.Interval := 100;
    D.Timer.OnTimer := @D.Tick;
    D.Text := '#11223344';
    D.Timer.Enabled := True;
    E.Edit;
    Check(D.Error = '', 'TBCPixel picker: ' + D.Error);
    Check((A.Pixel.Pixel = BGRA($11, $22, $33, $44)) and
      (B.Pixel.Pixel = A.Pixel.Pixel), 'TBCPixel multi-edit through the picker');
    Check((A.FChanges = 1) and (B.FChanges = 1), 'TBCPixel OnChange (redraw)');
    Check(E.GetValue = '#11223344', 'TBCPixel editor shows new value');

    A.Pixel.Pixel := BGRA(255, 0, 0, 0);
    B.Pixel.Pixel := BGRA(0, 255, 0, 0);
    Check(not E.AllEqual, 'transparent pixels with different RGB are mixed values');
    D.Text := '#0000FF00';
    D.Timer.Enabled := True;
    E.Edit;
    Check(D.Error = '', 'TBCPixel transparent RGB picker: ' + D.Error);
    Check((BCColorToString(A.Pixel.Pixel) = '#0000FF00') and
      (BCColorToString(B.Pixel.Pixel) = '#0000FF00'),
      'TBCPixel multi-edit saves transparent RGB');
    Check(E.AllEqual, 'identical transparent RGBA are equal');
    Check((A.FChanges = 2) and (B.FChanges = 2), 'transparent RGB edit notifies owners');
  finally
    D.Timer.Free; D.Free;
    E.Free; B.Free; A.Free;
  end;
end;

{ Moves the list to AIndex as the user does; checks the item shown and that
  the value text agrees with it. }
procedure ListKey(F: TCustomForm; L: TComboBox; AIndex, AExpected: integer);
var T: string;
begin
  UserSelect(L, AIndex);
  T := TEdit(F.FindComponent('ColorText')).Text;
  Check(L.ItemIndex = AExpected, Format('list key to %d shows %d, expected %d (%s)',
    [AIndex, L.ItemIndex, AExpected, L.Items[AExpected]]));
  if AExpected > 0 then
    Check(T = L.Items[AExpected], Format('list key to %d: value %s, item %s',
      [AIndex, T, L.Items[AExpected]]));
end;

procedure TAlphaDriver.Tick(Sender: TObject);
var F: TCustomForm; L: TComboBox; W: TPaintBox; I: integer; P: TBGRAPixel;
begin
  F := Screen.ActiveCustomForm;
  if (F = nil) or (F.Caption <> 'RGBA color') then Exit;
  Timer.Enabled := False;
  try
    L := TComboBox(F.FindComponent('SystemColor'));
    { two groups, by CSS name (aliases in one item), LCL name without one:
      system colors; fixed colors (CSS named colors, the other LCL
      identifiers, transparent for clNone, clDefault) }
    Check((L <> nil) and (L.Items[0] = 'Custom color') and
      (L.Items.IndexOf('System colors') = 1) and
      (L.Items.IndexOf('Fixed colors') > 1) and
      (L.Items.IndexOf('CSS colors') < 0) and (L.Items.IndexOf('LCL colors') < 0) and
      (L.Items.IndexOf('ThreeDFace / ButtonFace') >= 0) and
      (L.Items.IndexOf('ThreeDFace') < 0) and (L.Items.IndexOf('ButtonFace') < 0) and
      (L.Items.IndexOf('clBtnFace') < 0) and (L.Items.IndexOf('clHotLight') >= 0) and
      (L.Items.IndexOf('orange') >= 0) and (L.Items.IndexOf('aqua / cyan') >= 0) and
      (L.Items.IndexOf('aqua') < 0) and (L.Items.IndexOf('cyan') < 0) and
      (L.Items.IndexOf('lime') >= 0) and
      (L.Items.IndexOf('transparent') >= 0) and (L.Items.IndexOf('clNone') < 0) and
      (L.Items.IndexOf('clDefault') >= 0) and
      (L.Items.IndexOf('clRed') < 0) and (L.Items.IndexOf('clLime') < 0) and
      (L.Items.IndexOf('clMoneyGreen') >= 0), 'color combo contents');
    case Action of
      aaInvalidCancel:
        begin
          P := BGRA(TSpinEdit(F.FindComponent('RedValue')).Value,
            TSpinEdit(F.FindComponent('GreenValue')).Value,
            TSpinEdit(F.FindComponent('BlueValue')).Value,
            TSpinEdit(F.FindComponent('AlphaValue')).Value);
          I := L.ItemIndex;
          TEdit(F.FindComponent('ColorText')).Text := Text;
          Check(TEdit(F.FindComponent('ColorText')).Font.Color = clRed,
            'invalid picker input is marked: ' + Text);
          Check((L.ItemIndex = I) and
            (TSpinEdit(F.FindComponent('RedValue')).Value = P.red) and
            (TSpinEdit(F.FindComponent('GreenValue')).Value = P.green) and
            (TSpinEdit(F.FindComponent('BlueValue')).Value = P.blue) and
            (TSpinEdit(F.FindComponent('AlphaValue')).Value = P.alpha),
            'invalid input leaves the last valid selection and channels unchanged');
          TButton(F.FindComponent('CancelButton')).Click;
          Exit;
        end;
      aaAlphaBottom: ClickSurface(F, 'AlphaBar', 0, -1);
      aaText:
        begin
          TEdit(F.FindComponent('ColorText')).Text := Text;
          { Check that valid hex text updates all four channel fields. }
          if TryBCColor(Text, P) then
            Check((TSpinEdit(F.FindComponent('RedValue')).Value = P.red) and
              (TSpinEdit(F.FindComponent('GreenValue')).Value = P.green) and
              (TSpinEdit(F.FindComponent('BlueValue')).Value = P.blue) and
              (TSpinEdit(F.FindComponent('AlphaValue')).Value = P.alpha),
              'typed hex updates the other fields');
        end;
      aaList, aaListAlpha:
        begin
          if Text = 'Window' then { started from an RGB value }
            Check(L.ItemIndex = 0, 'RGB value shows Custom color');
          if Text = 'lime' then { started from RGB 255,0,0 }
            Check(L.Items[L.ItemIndex] = 'red',
              'RGB value equal to a named color shows its CSS name');
          I := L.Items.IndexOf(Text);
          Check(I >= 0, 'combo item ' + Text);
          L.ItemIndex := I;
          L.OnSelect(L);
          { Custom color may show the named color with the same RGB }
          if I > 0 then
            Check(L.ItemIndex = I, 'combo keeps the chosen item ' + Text);
          if Action = aaListAlpha then
          begin
            SetSpin(F, 'AlphaValue', 128);
            Check(L.ItemIndex = I, 'alpha keeps the chosen item ' + Text);
          end;
        end;
      aaTextAlpha:
        begin
          TEdit(F.FindComponent('ColorText')).Text := Text;
          if ExpectedEntry = '' then I := L.Items.IndexOf(Text)
          else I := L.Items.IndexOf(ExpectedEntry);
          Check((I >= 0) and (L.ItemIndex = I), 'typed name selects ' + Text);
          SetSpin(F, 'AlphaValue', 128);
          Check(L.ItemIndex = I, 'alpha keeps the typed item ' + Text);
        end;
      aaWheelCenter:
        begin
          Check(L.ItemIndex > 0, 'system color selected in combo');
          W := TPaintBox(F.FindComponent('ColorWheel'));
          ClickSurface(F, 'ColorWheel', (W.Width - 1) div 2, (W.Height - 1) div 2);
          Check(L.ItemIndex = 0, 'wheel selects Custom color');
        end;
      aaSpinAlpha:
        begin
          SetSpin(F, 'AlphaValue', $40);
          Check(L.ItemIndex = L.Items.IndexOf('ThreeDFace / ButtonFace'),
            'manual alpha keeps the system color selected');
        end;
      aaListKeys:
        begin
          { the group headers are stepped over in the direction of the move,
            and the list always shows the item of the value }
          ListKey(F, L, 2, 2);
          ListKey(F, L, 1, 0);  // up from the first system color
          ListKey(F, L, 1, 2);  // down from Custom color
          ListKey(F, L, 1, 0);  // up again
          I := L.Items.IndexOf('Fixed colors');
          ListKey(F, L, I - 1, I - 1);
          ListKey(F, L, I, I + 1);  // down from the last system color
          ListKey(F, L, I, I - 1);  // up from the first fixed color
          ListKey(F, L, I, I + 1);  // down again
        end;
      aaSpinRGB:
        begin
          SetSpin(F, 'RedValue', $11);
          SetSpin(F, 'GreenValue', $22);
          SetSpin(F, 'BlueValue', $33);
          Check(L.ItemIndex = 0, 'manual RGB selects Custom color');
        end;
    end;
    Text := TEdit(F.FindComponent('ColorText')).Text;
  except
    on E: Exception do
    begin
      Error := E.Message;
      F.ModalResult := mrCancel;
      Exit;
    end;
  end;
  TButton(F.FindComponent('OKButton')).Click;
end;

procedure TestAlphaColorPicker;
var D: TAlphaDriver; V: TBCColor; K: TBCLinkedHolder; P: TBGRAPixel; M: byte;
  A: integer;
  Kind: TBCSymbolicColor; B: boolean;

  { Load stored BGRA and kind, then check the text displayed on opening the picker. }
  procedure LFMPickerCase(const StoredColor, StoredKind, ExpectedText: string);
  var
    Holder: TBCColorHolder;
    Before: TBCColorValue;
    Accepted: boolean;
  begin
    Holder := TBCColorHolder.Create(nil);
    try
      LoadText(Holder, 'object Holder: TBCColorHolder' + LineEnding +
        '  Fill.Color = ' + QuotedStr(StoredColor) + LineEnding +
        '  Fill.KindName = ' + StoredKind + LineEnding + 'end');
      Before := Holder.Fill.Value;
      Check(Holder.Fill.ToString = ExpectedText, 'LFM to color text: ' + ExpectedText);
      D.Action := aaUnchanged;
      D.Error := '';
      D.Timer.Enabled := True;
      Accepted := ExecuteBCColorPicker(Holder.Fill);
      Check(Accepted and (D.Error = ''), 'picker from LFM: ' + D.Error);
      Check(D.Text = ExpectedText, 'picker displays loaded color: ' + D.Text +
        ', expected ' + ExpectedText);
      Check(Holder.Fill.SameValueAs(Before), 'picker OK preserves loaded RGBA and kind');
    finally
      Holder.Free;
    end;
  end;

  procedure InvalidTextCase(const S: string);
  var
    Accepted: boolean;
  begin
    V.Assign(clBtnFace, 76);
    D.Action := aaInvalidCancel;
    D.Text := S;
    D.Error := '';
    D.Timer.Enabled := True;
    Accepted := ExecuteBCColorPicker(V);
    Check(not Accepted and (D.Error = ''), 'invalid input then cancel: ' + D.Error);
    Check(V.SameValueAs(clBtnFace, 76), 'cancel preserves caller identity and alpha');
  end;

  procedure RelativeAliasCase(const S: string);
  var
    Accepted: boolean;
  begin
    V.Assign(clRed, 255);
    D.Action := aaTextAlpha;
    D.Text := S;
    D.Error := '';
    D.ExpectedEntry := 'ButtonHighlight / ThreeDHighlight';
    D.Timer.Enabled := True;
    Accepted := ExecuteBCColorPicker(V);
    Check(Accepted and (D.Error = ''), 'relative alias ' + S + ': ' + D.Error);
    Check(D.Text = 'rgb(from ButtonHighlight r g b / 50%)',
      'alpha gives the canonical name: ' + D.Text);
    Check(V.SameValueAs(clBtnHighlight, 128), 'relative alias preserves identity');
    D.ExpectedEntry := '';
  end;

begin
  D := TAlphaDriver.Create;
  D.Timer := TTimer.Create(nil);
  V := TBCColor.Create;
  try
    D.Timer.Interval := 100;
    D.Timer.OnTimer := @D.Tick;

    LFMPickerCase('#FF0000FF', 'scCustomRGB', 'red');
    LFMPickerCase('#FF000080', 'scCustomRGB', 'rgb(from red r g b / 50%)');
    LFMPickerCase('#FF000000', 'scCustomRGB', 'rgb(from red r g b / 0%)');
    LFMPickerCase('#0708094C', 'scCustomRGB', '#0708094C');
    LFMPickerCase('#00000080', 'scBtnFace', 'rgb(from ThreeDFace r g b / 50%)');
    LFMPickerCase('#0000004C', 'scBtnFace', 'rgb(from ThreeDFace r g b / 0.298)');
    LFMPickerCase('#000000FF', 'scHotLight', 'clHotLight');
    LFMPickerCase('#00000080', 'scDefault', 'rgb(from default r g b / 50%)');
    LFMPickerCase('#00000000', 'scNone', 'transparent');

    { Check unchanged OK preserves symbolic identity and alpha, including
      bytes that cannot round-trip through a whole percentage. }
    D.Action := aaUnchanged;
    for Kind in [scBtnFace, scDefault] do
      for A in [76, 77, 178, 179] do
      begin
        V.Assign(TBCColorValue.New(Kind, A));
        D.Error := '';
        D.Timer.Enabled := True;
        Check(ExecuteBCColorPicker(V) and (D.Error = ''),
          'unchanged symbolic picker: ' + D.Error);
        Check(V.SameValueAs(TBCColorValue.New(Kind, A)),
          'OK without edits preserves alpha ' + IntToStr(A));
      end;

    V.Assign(clBtnFace, 200);
    D.Action := aaSpinAlpha;
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V), 'alpha spin pick accepted: ' + D.Error);
    Check(V.SameValueAs(clBtnFace, $40),
      'picker keeps system identity when editing alpha');

    V.Assign(clBtnFace, 200);
    D.Action := aaAlphaBottom;
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''), 'opacity-only pick: ' + D.Error);
    Check(D.Text = 'rgb(from ThreeDFace r g b / 0%)', 'text shows system color: ' + D.Text);
    Check(V.SameValueAs(clBtnFace, 0), 'system color kept: ' + V.ToString);

    V.Assign(clDefault, 255);
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''), 'clDefault pick: ' + D.Error);
    Check(V.SameValueAs(clDefault, 0), 'clDefault kept: ' + V.ToString);

    V.Assign(clBtnFace, 200);
    D.Action := aaText;
    D.Text := '#11223380';
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''), 'RGB pick: ' + D.Error);
    Check(V.SameValueAs(RGBToColor($11, $22, $33), $80),
      'new RGB replaces system color: ' + V.ToString);

    V.Assign(clRed, 200);
    D.Text := 'rgb(from clBtnShadow r g b / 25%)';
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''), 'typed system color: ' + D.Error);
    Check(V.SameValueAs(clBtnShadow, $40), 'typed system color: ' + V.ToString);

    V.Assign(clNone);
    D.Text := '#00000040';
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''), 'clNone pick: ' + D.Error);
    Check(V.SameValueAs(clBlack, $40), 'clNone becomes RGB: ' + V.ToString);

    V.Assign(RGBToColor($11, $22, $33), $80);
    D.Action := aaList;
    D.Text := 'Window';
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''), 'combo pick: ' + D.Error);
    Check(D.Text = 'Window', 'combo resets opacity: ' + D.Text);
    Check(V.SameValueAs(clWindow, 255), 'combo selects opaque system color');

    V.Assign(RGBToColor(255, 0, 0), $80);
    D.Text := 'lime';
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''), 'named RGB combo pick: ' + D.Error);
    Check(V.SameValueAs(clLime, 255), 'combo selects a named RGB color: ' + V.ToString);
    Check(D.Text = 'lime', 'combo shows the CSS name: ' + D.Text);

    V.Assign(RGBToColor(255, 0, 0), $80);
    D.Text := 'clMoneyGreen';
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''), 'LCL-only combo pick: ' + D.Error);
    Check(V.SameValueAs(clMoneyGreen, 255), 'combo selects an LCL-only color: ' + V.ToString);

    V.Assign(RGBToColor(255, 0, 0), $80);
    D.Text := 'transparent';
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''), 'transparent combo pick: ' + D.Error);
    Check(V.Kind = scNone, 'transparent is clNone: ' + V.ToString);

    { aliases share a combined list item; the text field shows the canonical name }
    V.Assign(clRed, $80);
    D.Text := 'ThreeDFace / ButtonFace';
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''), 'alias combo pick: ' + D.Error);
    Check(D.Text = 'ThreeDFace', 'combo shows the canonical name: ' + D.Text);
    Check(V.SameValueAs(clBtnFace, 255), 'alias is the same system color: ' + V.ToString);
    Check(V.ToString = 'ThreeDFace', 'the value keeps no alias: ' + V.ToString);

    V.Assign(clRed, $80);
    D.Action := aaListAlpha;
    D.Text := 'ThreeDFace / ButtonFace';
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''), 'alias with alpha: ' + D.Error);
    Check(D.Text = 'rgb(from ThreeDFace r g b / 50%)', 'alpha keeps the name: ' + D.Text);
    Check(V.SameValueAs(clBtnFace, 128), 'alias with alpha value: ' + V.ToString);

    V.Assign(clRed, 255);
    D.Action := aaListAlpha;
    D.Text := 'aqua / cyan';
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''), 'CSS combo pick: ' + D.Error);
    Check(D.Text = 'rgb(from aqua r g b / 50%)', 'CSS name with alpha keeps the name: ' + D.Text);
    Check(V.SameValueAs(TBCColorValue.New(BGRA(0, 255, 255, 128))),
      'CSS named color is RGB: ' + V.ToString);

    V.Assign(clRed, 255);
    D.Action := aaList;
    D.Text := 'aqua / cyan';
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''), 'CSS alias pick: ' + D.Error);
    Check(D.Text = 'aqua', 'combo shows the first CSS name: ' + D.Text);

    { a typed alias selects the shared item; alpha gives the canonical name }
    V.Assign(clRed, 255);
    D.Action := aaTextAlpha;
    D.Text := 'ThreeDHighlight';
    D.ExpectedEntry := 'ButtonHighlight / ThreeDHighlight';
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''), 'typed alias: ' + D.Error);
    D.ExpectedEntry := '';
    Check(D.Text = 'rgb(from ButtonHighlight r g b / 50%)', 'typed alias: ' + D.Text);
    Check(V.SameValueAs(clBtnHighlight, 128), 'typed alias value: ' + V.ToString);
    RelativeAliasCase('rgba(from ThreeDHighlight r g b / 25%)');
    RelativeAliasCase('RGB(FROM ThreeDHighlight R G B / 25%)');
    RelativeAliasCase('rgb('#9'from'#10'ThreeDHighlight r g b / 25%)');
    RelativeAliasCase('rgb(from rgba(from ThreeDHighlight r g b) r g b)');

    InvalidTextCase('rgb(from notacolor r g b / 50%)');
    InvalidTextCase('rgba(from ThreeDFacce r g b)');
    InvalidTextCase('rgb(from rgb(from notacolor r g b) r g b)');
    InvalidTextCase('rgb(1 2 3 / )');
    InvalidTextCase('');

    V.Assign(clBtnFace, $80);
    D.Action := aaList;
    D.Text := 'clDefault';
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''), 'clDefault combo pick: ' + D.Error);
    Check(D.Text = 'default', 'clDefault is opaque: ' + D.Text);
    Check(V.SameValueAs(clDefault, 255), 'clDefault resets alpha: ' + V.ToString);

    V.Assign(clBtnFace, $C8);
    D.Text := 'Custom color';
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''), 'custom color pick: ' + D.Error);
    P := ColorToBGRA(ColorToRGB(clBtnFace));
    Check(V.SameValueAs(RGBToColor(P.red, P.green, P.blue), $C8),
      'Custom color turns a system color into its RGB value: ' + V.ToString);

    V.Assign(clDefault, $C8);
    D.Text := 'Custom color';
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''),
      'custom color from clDefault: ' + D.Error);
    Check(V.SameValueAs(clBlack, $C8),
      'Custom color turns clDefault (not known) into black: ' + V.ToString);

    V.Assign(clBtnFace, 200);
    D.Action := aaWheelCenter;
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''), 'wheel pick: ' + D.Error);
    P := ColorToBGRA(ColorToRGB(clBtnFace));
    M := Max(P.red, Max(P.green, P.blue));
    Check(V.SameValueAs(RGBToColor(M, M, M), 200),
      'wheel turns system color into RGB: ' + V.ToString);

    V.Assign(clBtnFace, 200);
    D.Action := aaSpinAlpha;
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''), 'manual alpha: ' + D.Error);
    Check(V.SameValueAs(clBtnFace, $40), 'manual alpha keeps system color: ' + V.ToString);

    V.Assign(clRed);
    D.Action := aaListKeys;
    D.Timer.Enabled := True;
    B := ExecuteBCColorPicker(V);
    Check(B and (D.Error = ''), 'list keys: ' + D.Error);
    Check(V.ToString = 'aliceblue', 'list keys end on the first fixed color: ' +
      V.ToString);

    V.Assign(clBtnFace, 200);
    D.Action := aaSpinRGB;
    D.Timer.Enabled := True;
    Check(ExecuteBCColorPicker(V) and (D.Error = ''), 'manual RGB: ' + D.Error);
    Check(V.SameValueAs(RGBToColor($11, $22, $33), 200),
      'manual RGB replaces system color: ' + V.ToString);

    { An existing TColor + Byte pair, through a TBCLinkedColor: one write. }
    K := TBCLinkedHolder.Create(nil);
    try
      K.FillOpacity := 200;
      D.Action := aaSpinAlpha;
      D.Timer.Enabled := True;
      Check(ExecuteBCColorPicker(K.Fill) and (D.Error = ''), 'linked pick: ' + D.Error);
      Check((K.FillColor = clBtnFace) and (K.FillOpacity = $40) and (K.FSets = 1),
        'linked pair written once, system color kept');
    finally
      K.Free;
    end;
  finally
    V.Free;
    D.Timer.Free; D.Free;
  end;
end;

var DemoPixel: TBGRAPixel; DemoColor: TBCColor; Shot: TShotDriver;
  DemoText: string;
begin
  Application.Initialize;
  if (ParamCount > 1) and (ParamStr(1) = '--picker-shot') then
  begin
    { Saves an image of the picker (system colors mode) to ParamStr(2),
      opened on the color text in ParamStr(3) and changed to ParamStr(4)
      when given. }
    Shot := TShotDriver.Create;
    Shot.FileName := ParamStr(2);
    Shot.Timer := TTimer.Create(nil);
    Shot.Timer.Interval := 500;
    Shot.Timer.OnTimer := @Shot.Tick;
    DemoColor := TBCColor.Create(clBtnFace, 180);
    if ParamCount > 2 then DemoColor.Assign(ParamStr(3));
    if ParamCount > 3 then Shot.NewText := ParamStr(4);
    ExecuteBCColorPicker(DemoColor);
    Shot.Timer.Free; Shot.Free; DemoColor.Free;
    Halt(0);
  end;
  if (ParamCount > 0) and (ParamStr(1) = '--picker-system') then
  begin
    DemoColor := TBCColor.Create(clBtnFace, 180);
    if ExecuteBCColorPicker(DemoColor) then
      WriteLn('Selected: ', DemoColor.ToString)
    else WriteLn('Cancelled');
    DemoColor.Free;
    Halt(0);
  end;
  if (ParamCount > 0) and (ParamStr(1) = '--picker-css') then
  begin
    { CSS text mode, as opened by the TBCCSSColorString editor. }
    DemoText := 'rgba(255,255,255,0.5)';
    if ExecuteBCColorPicker(DemoText) then
      WriteLn('Selected: ', DemoText)
    else WriteLn('Cancelled');
    Halt(0);
  end;
  if (ParamCount > 0) and (ParamStr(1) = '--picker-pixel') then
  begin
    { As opened by the TBCPixel editor: color list disabled on Custom color. }
    DemoPixel := BGRA(60, 140, 220, 180);
    if ExecuteBCColorPicker(DemoPixel, True) then
      WriteLn('Selected: ', BCColorToString(DemoPixel))
    else WriteLn('Cancelled');
    Halt(0);
  end;
  if (ParamCount > 0) and (ParamStr(1) = '--picker') then
  begin
    DemoPixel := BGRA(60, 140, 220, 180);
    if ExecuteBCColorPicker(DemoPixel) then
      WriteLn('Selected: ', BCColorToString(DemoPixel))
    else WriteLn('Cancelled');
    Halt(0);
  end;
  try
    TestParser;
    TestStringEditor;
    TestStreaming;
    TestSwatch;
    TestPicker;
    TestAlphaColor;
    TestColorConversions;
    TestColorTextReuse;
    TestSymbolicCSSNames;
    TestBCColorProperty;
    TestDefinedColorStreaming;
    TestAncestorStreaming;
    TestBCColorAssign;
    TestBCColorEditor;
    TestAlphaColorPicker;
    TestBCPixelEditor;
    TestLinkedColor;  { after TestBCPixelEditor, which registers the editors }
    TestShapeLinkedColors;
    TestComboFocusBorderRefresh;
    TestCheckComboFocusBorder;
    TestKnobLinkedColor;
    TestSpinnerLinkedColor;
    TestSVGViewerLinkedColor;
    TestLEDLinkedColor;
    TestInheritedEditor(TBCColorHolder, 'Fill', TBCColorPropertyEditor);
    TestInheritedEditor(TBCLinkedHolder, 'Fill', TBCColorPropertyEditor);
    TestInheritedEditor(TBCPixelHolder, 'Pixel', TBCPixelPropertyEditor);
    TestCSSColor;
    TestCSSEditor;
    TestCSSPicker;
    WriteLn('PASS: RGBA parsing, TBCColorString editor, LFM streaming, swatch, ',
      'picker OK/Cancel, TBCColor helpers, streaming/editor/picker, TBCLinkedColor, TBCPixel editor, CSS color text');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ', E.Message);
      Halt(1);
    end;
  end;
end.
