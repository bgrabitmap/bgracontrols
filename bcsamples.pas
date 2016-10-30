unit BCSamples;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Graphics,
  StdCtrls, LCLProc, LCLType, LazUTF8,
  BCButton, BCButtonFocus, BCTypes,
  BGRABitmap, BGRABitmapTypes, BGRAGradients, MaterialColors,
  BCBrightAndContrast;

const
  {Accent Colors}
  acMagenta = TColor($009700FF);
  acPurple = TColor($00FF00A2);
  acTeal = TColor($00A9AB00);
  acLime = TColor($0026BF8C);
  acBrown = TColor($000050A0);
  acPink = TColor($00B871E6);
  acOrange = TColor($000996F0);
  acBlue = TColor($00E2A11B);
  acRed = TColor($000014E5);
  acGreen = TColor($00339933);
  {Facebook Colors}
  fbBlue = TColor($00AA785F);
  fbGreen = TColor($004BA567);
  fbGray = TColor($00F8F8F8);
  {Windows 8 Color Scheme - Background}
  clScheme1_Background = TColor($00252525);
  clScheme2_Background = TColor($00252525);
  clScheme3_Background = TColor($00252525);
  clScheme4_Background = TColor($00252525);
  clScheme5_Background = TColor($0000172E);
  clScheme6_Background = TColor($0000004E);
  clScheme7_Background = TColor($0038004E);
  clScheme8_Background = TColor($004E002D);
  clScheme9_Background = TColor($0068001F);
  clScheme10_Background = TColor($004E1E00);
  clScheme11_Background = TColor($00604D00);
  clScheme12_Background = TColor($00004A00);
  clScheme13_Background = TColor($002A9915);
  clScheme14_Background = TColor($00196CE5);
  clScheme15_Background = TColor($001B1BB8);
  clScheme16_Background = TColor($006C1BB8);
  clScheme17_Background = TColor($00B81B69);
  clScheme18_Background = TColor($00B8581B);
  clScheme19_Background = TColor($00E39C56);
  clScheme20_Background = TColor($00AAAA00);
  clScheme21_Background = TColor($001FBA83);
  clScheme22_Background = TColor($00099DD3);
  clScheme23_Background = TColor($00B764E0);
  clScheme24_Background = TColor($00696969);
  clScheme25_Background = TColor($00696969);
  {Windows 8 Color Scheme - Selection}
  clScheme1_Selection = TColor($0000B3F4);
  clScheme2_Selection = TColor($0000BA78);
  clScheme3_Selection = TColor($00EC7326);
  clScheme4_Selection = TColor($003D11AE);
  clScheme5_Selection = TColor($00002F63);
  clScheme6_Selection = TColor($00001EB0);
  clScheme7_Selection = TColor($004F00C1);
  clScheme8_Selection = TColor($00AC0072);
  clScheme9_Selection = TColor($00B41746);
  clScheme10_Selection = TColor($00C16A00);
  clScheme11_Selection = TColor($00878200);
  clScheme12_Selection = TColor($00009919);
  clScheme13_Selection = TColor($003FC100);
  clScheme14_Selection = TColor($001D98FF);
  clScheme15_Selection = TColor($00122EFF);
  clScheme16_Selection = TColor($00771DFF);
  clScheme17_Selection = TColor($00FF40AA);
  clScheme18_Selection = TColor($00FFAE1F);
  clScheme19_Selection = TColor($00FFC556);
  clScheme20_Selection = TColor($00CCD800);
  clScheme21_Selection = TColor($0000D191);
  clScheme22_Selection = TColor($0000B7E1);
  clScheme23_Selection = TColor($00BC76FF);
  clScheme24_Selection = TColor($00A4A400);
  clScheme25_Selection = TColor($00237DFF);

type
  TBCSampleStyle = (ssDefault, ssWindows7, ssWindows7ToolBar, ssOffice2010,
    ssFlashPlayer, ssMacOSXLion, ssWindows8_1, ssWindows8_2, ssWindows8_3,
    ssWindows8_4, ssWindows8_5, ssWindows8_6, ssWindows8_7, ssWindows8_8,
    ssWindows8_9, ssWindows8_10, ssWindows8_11, ssWindows8_12, ssWindows8_13,
    ssWindows8_14, ssWindows8_15, ssWindows8_16, ssWindows8_17, ssWindows8_18,
    ssWindows8_19, ssWindows8_20, ssWindows8_21, ssWindows8_22, ssWindows8_23,
    ssWindows8_24, ssWindows8_25, ssMaterialRed, ssMaterialPink, ssMaterialPurple,
    ssMaterialDeepPurple, ssMaterialIndigo, ssMaterialBlue, ssMaterialLightBlue,
    ssMaterialCyan, ssMaterialTeal, ssMaterialGreen, ssMaterialLightGreen,
    ssMaterialLime, ssMaterialYellow, ssMaterialAmber, ssMaterialOrange,
    ssMaterialDeepOrange, ssMaterialBrown, ssMaterialGrey, ssMaterialBlueGrey);

  TBCSampleDrawing = (sdDefault, sdFlashPlayerBody, sdFlashPlayerButtonPanel,
    sdWindows7Toolbar, sdiOSBar, sdiOSToolBar, sdiOSBackground,
    sdWindows8_1, sdWindows8_2, sdWindows8_3,
    sdWindows8_4, sdWindows8_5, sdWindows8_6, sdWindows8_7, sdWindows8_8,
    sdWindows8_9, sdWindows8_10, sdWindows8_11, sdWindows8_12, sdWindows8_13,
    sdWindows8_14, sdWindows8_15, sdWindows8_16, sdWindows8_17, sdWindows8_18,
    sdWindows8_19, sdWindows8_20, sdWindows8_21, sdWindows8_22, sdWindows8_23,
    sdWindows8_24, sdWindows8_25);

const
  BCSampleStyleStr: array[TBCSampleStyle] of string =
    ('Default', 'Windows 7', 'Windows 7 ToolBar', 'Office 2010',
    'Flash Player', 'Mac OSX Lion', 'Windows 8 Scheme 1', 'Windows 8 Scheme 2',
    'Windows 8 Scheme 3', 'Windows 8 Scheme 4', 'Windows 8 Scheme 5'
    , 'Windows 8 Scheme 6', 'Windows 8 Scheme 7', 'Windows 8 Scheme 8'
    , 'Windows 8 Scheme 9', 'Windows 8 Scheme 10', 'Windows 8 Scheme 11'
    , 'Windows 8 Scheme 12', 'Windows 8 Scheme 13', 'Windows 8 Scheme 14'
    , 'Windows 8 Scheme 15', 'Windows 8 Scheme 16', 'Windows 8 Scheme 17'
    , 'Windows 8 Scheme 18', 'Windows 8 Scheme 19', 'Windows 8 Scheme 20'
    , 'Windows 8 Scheme 21', 'Windows 8 Scheme 22', 'Windows 8 Scheme 23'
    , 'Windows 8 Scheme 24', 'Windows 8 Scheme 25', 'Material Red', 'Material Pink',
    'Material Purple', 'Material Deep Purple', 'Material Indigo', 'Material Blue',
    'Material Light Blue', 'Material Cyan', 'Material Teal', 'Material Green',
    'Material Light Green', 'Material Lime', 'Material Yellow', 'Material Amber',
    'Material Orange', 'Material Deep Orange', 'Material Brown', 'Material Grey',
    'Material Blue Grey');

  BCSampleDrawingStr: array[TBCSampleDrawing] of string =
    ('Default', 'Flash Player Body', 'Flash Player Button Panel',
    'Windows 7 ToolBar', 'iOS Bar', 'iOS ToolBar', 'iOS Background',
    'Windows 8 Scheme 1', 'Windows 8 Scheme 2',
    'Windows 8 Scheme 3', 'Windows 8 Scheme 4', 'Windows 8 Scheme 5'
    , 'Windows 8 Scheme 6', 'Windows 8 Scheme 7', 'Windows 8 Scheme 8'
    , 'Windows 8 Scheme 9', 'Windows 8 Scheme 10', 'Windows 8 Scheme 11'
    , 'Windows 8 Scheme 12', 'Windows 8 Scheme 13', 'Windows 8 Scheme 14'
    , 'Windows 8 Scheme 15', 'Windows 8 Scheme 16', 'Windows 8 Scheme 17'
    , 'Windows 8 Scheme 18', 'Windows 8 Scheme 19', 'Windows 8 Scheme 20'
    , 'Windows 8 Scheme 21', 'Windows 8 Scheme 22', 'Windows 8 Scheme 23'
    , 'Windows 8 Scheme 24', 'Windows 8 Scheme 25');

function StrToTBCSampleStyle(const s: ansistring): TBCSampleStyle;
procedure BCSampleStyleStrList(s: TStrings);

function StrToTBCSampleDrawing(const s: ansistring): TBCSampleDrawing;
procedure BCSampleDrawingStrList(s: TStrings);

procedure StyleButtons(AControl: TControl; AButton: TBCButton);
procedure StyleButtons(AControl: TControl; AButton: TBCButtonFocus);
procedure StyleButtonsSample(AControl: TControl; AStyle: TBCSampleStyle);
procedure StyleButtonsFocusSample(AControl: TControl; AStyle: TBCSampleStyle);

procedure DrawSample(ABitmap: TBGRABitmap; Element: TBCSampleDrawing;
  Align: TAlign = alNone);
function DrawSample(AWidth, AHeight: integer; Element: TBCSampleDrawing;
  Align: TAlign = alNone): TBGRABitmap;
procedure DrawItem(Control: TWinControl; Index: integer; ARect: TRect;
  State: TOwnerDrawState; Style: TBCSampleDrawing);

{ Buttons }
procedure BCButtonWindows7(AButton: TBCButton);
procedure BCButtonWindows7ToolBar(AButton: TBCButton);
procedure BCButtonOffice2010(AButton: TBCButton);
procedure BCButtonFlashPlayer(AButton: TBCButton);
procedure BCButtonMacOSXLion(AButton: TBCButton);
procedure BCButtonWindows8(AButton: TBCButton; cl1, cl2: TColor; rounding: integer = 1);

procedure BCButtonWindows7(AButton: TBCButtonFocus);
procedure BCButtonWindows7ToolBar(AButton: TBCButtonFocus);
procedure BCButtonOffice2010(AButton: TBCButtonFocus);
procedure BCButtonFlashPlayer(AButton: TBCButtonFocus);
procedure BCButtonMacOSXLion(AButton: TBCButtonFocus);
procedure BCButtonWindows8(AButton: TBCButtonFocus; cl1, cl2: TColor;
  rounding: integer = 1);

{ Drawings }
procedure DrawFlashPlayerBody(ABitmap: TBGRABitmap);
procedure DrawFlashPlayerButtonPanel(ABitmap: TBGRABitmap);
procedure DrawWindows7ToolBar(ABitmap: TBGRABitmap; ADir: TAlign;
  Smooth: boolean = False);
procedure DrawiOSBar(ABitmap: TBGRABitmap);
procedure DrawiOSToolBar(ABitmap: TBGRABitmap; Shadow: boolean = True);
procedure DrawiOSBackground(ABitmap: TBGRABitmap);
procedure DrawWindows8Background(ABitmap: TBGRABitmap; {%H-}cl1, cl2: TColor);

implementation

function StrToTBCSampleStyle(const s: ansistring): TBCSampleStyle;
var
  ss: TBCSampleStyle;
  ls: ansistring;
begin
  Result := ssDefault;
  ls := UTF8LowerCase(s);
  for ss := low(TBCSampleStyle) to high(TBCSampleStyle) do
    if ls = UTF8LowerCase(BCSampleStyleStr[ss]) then
    begin
      Result := ss;
      break;
    end;
end;

procedure BCSampleStyleStrList(s: TStrings);
var
  ss: TBCSampleStyle;
begin
  for ss := low(TBCSampleStyle) to high(TBCSampleStyle) do
    s.Add(BCSampleStyleStr[ss]);
end;

procedure StyleButtons(AControl: TControl; AButton: TBCButton);
var
  i: integer;
  WinControl: TWinControl;
begin
  if AControl is TBCButton then
    with AControl as TBCButton do
    begin
      // Those are recomended properties to keep unchanged
      AButton.Glyph := Glyph;
      AButton.GlyphMargin := GlyphMargin;
      AButton.StaticButton := StaticButton;
      AButton.Down := Down;
      AButton.Style := Style;
      Assign(AButton);
    end;
  if AControl is TWinControl then
  begin
    WinControl := TWinControl(AControl);
    if WinControl.ControlCount = 0 then
      exit;
    for i := 0 to WinControl.ControlCount - 1 do
      StyleButtons(WinControl.Controls[i], AButton);
  end;
end;

procedure StyleButtons(AControl: TControl; AButton: TBCButtonFocus);
var
  i: integer;
  WinControl: TWinControl;
begin
  if AControl is TBCButtonFocus then
    with AControl as TBCButtonFocus do
    begin
      // Those are recomended properties to keep unchanged
      AButton.Glyph := Glyph;
      AButton.GlyphMargin := GlyphMargin;
      AButton.StaticButton := StaticButton;
      AButton.Down := Down;
      AButton.Style := Style;
      Assign(AButton);
    end;
  if AControl is TWinControl then
  begin
    WinControl := TWinControl(AControl);
    if WinControl.ControlCount = 0 then
      exit;
    for i := 0 to WinControl.ControlCount - 1 do
      StyleButtons(WinControl.Controls[i], AButton);
  end;
end;

procedure StyleButtonsSample(AControl: TControl; AStyle: TBCSampleStyle);
var
  temp: TBCButton;
begin
  temp := TBCButton.Create(nil);
  case AStyle of
    ssWindows7: BCButtonWindows7(temp);
    ssWindows7ToolBar: BCButtonWindows7ToolBar(temp);
    ssOffice2010: BCButtonOffice2010(temp);
    ssFlashPlayer: BCButtonFlashPlayer(temp);
    ssMacOSXLion: BCButtonMacOSXLion(temp);
    ssWindows8_1: BCButtonWindows8(temp, clScheme1_Background, clScheme1_Selection);
    ssWindows8_2: BCButtonWindows8(temp, clScheme2_Background, clScheme2_Selection);
    ssWindows8_3: BCButtonWindows8(temp, clScheme3_Background, clScheme3_Selection);
    ssWindows8_4: BCButtonWindows8(temp, clScheme4_Background, clScheme4_Selection);
    ssWindows8_5: BCButtonWindows8(temp, clScheme5_Background, clScheme5_Selection);
    ssWindows8_6: BCButtonWindows8(temp, clScheme6_Background, clScheme6_Selection);
    ssWindows8_7: BCButtonWindows8(temp, clScheme7_Background, clScheme7_Selection);
    ssWindows8_8: BCButtonWindows8(temp, clScheme8_Background, clScheme8_Selection);
    ssWindows8_9: BCButtonWindows8(temp, clScheme9_Background, clScheme9_Selection);
    ssWindows8_10: BCButtonWindows8(temp, clScheme10_Background, clScheme10_Selection);
    ssWindows8_11: BCButtonWindows8(temp, clScheme11_Background, clScheme11_Selection);
    ssWindows8_12: BCButtonWindows8(temp, clScheme12_Background, clScheme12_Selection);
    ssWindows8_13: BCButtonWindows8(temp, clScheme13_Background, clScheme13_Selection);
    ssWindows8_14: BCButtonWindows8(temp, clScheme14_Background, clScheme14_Selection);
    ssWindows8_15: BCButtonWindows8(temp, clScheme15_Background, clScheme15_Selection);
    ssWindows8_16: BCButtonWindows8(temp, clScheme16_Background, clScheme16_Selection);
    ssWindows8_17: BCButtonWindows8(temp, clScheme17_Background, clScheme17_Selection);
    ssWindows8_18: BCButtonWindows8(temp, clScheme18_Background, clScheme18_Selection);
    ssWindows8_19: BCButtonWindows8(temp, clScheme19_Background, clScheme19_Selection);
    ssWindows8_20: BCButtonWindows8(temp, clScheme20_Background, clScheme20_Selection);
    ssWindows8_21: BCButtonWindows8(temp, clScheme21_Background, clScheme21_Selection);
    ssWindows8_22: BCButtonWindows8(temp, clScheme22_Background, clScheme22_Selection);
    ssWindows8_23: BCButtonWindows8(temp, clScheme23_Background, clScheme23_Selection);
    ssWindows8_24: BCButtonWindows8(temp, clScheme24_Background, clScheme24_Selection);
    ssWindows8_25: BCButtonWindows8(temp, clScheme25_Background, clScheme25_Selection);
    ssMaterialRed: BCButtonWindows8(temp, MaterialRed.M500, MaterialRed.M300, 5);
    ssMaterialPink: BCButtonWindows8(temp, MaterialPink.M500, MaterialPink.M300, 5);
    ssMaterialPurple: BCButtonWindows8(temp, MaterialPurple.M500,
        MaterialPurple.M300, 5);
    ssMaterialDeepPurple: BCButtonWindows8(temp, MaterialDeepPurple.M500,
        MaterialDeepPurple.M300, 5);
    ssMaterialIndigo: BCButtonWindows8(temp, MaterialIndigo.M500,
        MaterialIndigo.M300, 5);
    ssMaterialBlue: BCButtonWindows8(temp, MaterialBlue.M500, MaterialBlue.M300, 5);
    ssMaterialLightBlue: BCButtonWindows8(temp, MaterialLightBlue.M500,
        MaterialLightBlue.M300, 5);
    ssMaterialCyan: BCButtonWindows8(temp, MaterialCyan.M500, MaterialCyan.M300, 5);
    ssMaterialTeal: BCButtonWindows8(temp, MaterialTeal.M500, MaterialTeal.M300, 5);
    ssMaterialGreen: BCButtonWindows8(temp, MaterialGreen.M500, MaterialGreen.M300, 5);
    ssMaterialLightGreen: BCButtonWindows8(temp, MaterialLightGreen.M500,
        MaterialLightGreen.M300, 5);
    ssMaterialLime: BCButtonWindows8(temp, MaterialLime.M500, MaterialLime.M300, 5);
    ssMaterialYellow: BCButtonWindows8(temp, MaterialYellow.M500,
        MaterialYellow.M300, 5);
    ssMaterialAmber: BCButtonWindows8(temp, MaterialAmber.M500, MaterialAmber.M300, 5);
    ssMaterialOrange: BCButtonWindows8(temp, MaterialOrange.M500,
        MaterialOrange.M300, 5);
    ssMaterialDeepOrange: BCButtonWindows8(temp, MaterialDeepOrange.M500,
        MaterialDeepOrange.M300, 5);
    ssMaterialBrown: BCButtonWindows8(temp, MaterialBrown.M500, MaterialBrown.M300, 5);
    ssMaterialGrey: BCButtonWindows8(temp, MaterialGrey.M500, MaterialGrey.M300, 5);
    ssMaterialBlueGrey: BCButtonWindows8(temp, MaterialBlueGrey.M500,
        MaterialBlueGrey.M300, 5);
  end;
  StyleButtons(AControl, temp);
  temp.Free;
end;

procedure StyleButtonsFocusSample(AControl: TControl; AStyle: TBCSampleStyle);
var
  temp: TBCButtonFocus;
begin
  temp := TBCButtonFocus.Create(nil);
  case AStyle of
    ssWindows7: BCButtonWindows7(temp);
    ssWindows7ToolBar: BCButtonWindows7ToolBar(temp);
    ssOffice2010: BCButtonOffice2010(temp);
    ssFlashPlayer: BCButtonFlashPlayer(temp);
    ssMacOSXLion: BCButtonMacOSXLion(temp);
    ssWindows8_1: BCButtonWindows8(temp, clScheme1_Background, clScheme1_Selection);
    ssWindows8_2: BCButtonWindows8(temp, clScheme2_Background, clScheme2_Selection);
    ssWindows8_3: BCButtonWindows8(temp, clScheme3_Background, clScheme3_Selection);
    ssWindows8_4: BCButtonWindows8(temp, clScheme4_Background, clScheme4_Selection);
    ssWindows8_5: BCButtonWindows8(temp, clScheme5_Background, clScheme5_Selection);
    ssWindows8_6: BCButtonWindows8(temp, clScheme6_Background, clScheme6_Selection);
    ssWindows8_7: BCButtonWindows8(temp, clScheme7_Background, clScheme7_Selection);
    ssWindows8_8: BCButtonWindows8(temp, clScheme8_Background, clScheme8_Selection);
    ssWindows8_9: BCButtonWindows8(temp, clScheme9_Background, clScheme9_Selection);
    ssWindows8_10: BCButtonWindows8(temp, clScheme10_Background, clScheme10_Selection);
    ssWindows8_11: BCButtonWindows8(temp, clScheme11_Background, clScheme11_Selection);
    ssWindows8_12: BCButtonWindows8(temp, clScheme12_Background, clScheme12_Selection);
    ssWindows8_13: BCButtonWindows8(temp, clScheme13_Background, clScheme13_Selection);
    ssWindows8_14: BCButtonWindows8(temp, clScheme14_Background, clScheme14_Selection);
    ssWindows8_15: BCButtonWindows8(temp, clScheme15_Background, clScheme15_Selection);
    ssWindows8_16: BCButtonWindows8(temp, clScheme16_Background, clScheme16_Selection);
    ssWindows8_17: BCButtonWindows8(temp, clScheme17_Background, clScheme17_Selection);
    ssWindows8_18: BCButtonWindows8(temp, clScheme18_Background, clScheme18_Selection);
    ssWindows8_19: BCButtonWindows8(temp, clScheme19_Background, clScheme19_Selection);
    ssWindows8_20: BCButtonWindows8(temp, clScheme20_Background, clScheme20_Selection);
    ssWindows8_21: BCButtonWindows8(temp, clScheme21_Background, clScheme21_Selection);
    ssWindows8_22: BCButtonWindows8(temp, clScheme22_Background, clScheme22_Selection);
    ssWindows8_23: BCButtonWindows8(temp, clScheme23_Background, clScheme23_Selection);
    ssWindows8_24: BCButtonWindows8(temp, clScheme24_Background, clScheme24_Selection);
    ssWindows8_25: BCButtonWindows8(temp, clScheme25_Background, clScheme25_Selection);
    ssMaterialRed: BCButtonWindows8(temp, MaterialRed.M500, MaterialRed.M300, 5);
    ssMaterialPink: BCButtonWindows8(temp, MaterialPink.M500, MaterialPink.M300, 5);
    ssMaterialPurple: BCButtonWindows8(temp, MaterialPurple.M500,
        MaterialPurple.M300, 5);
    ssMaterialDeepPurple: BCButtonWindows8(temp, MaterialDeepPurple.M500,
        MaterialDeepPurple.M300, 5);
    ssMaterialIndigo: BCButtonWindows8(temp, MaterialIndigo.M500,
        MaterialIndigo.M300, 5);
    ssMaterialBlue: BCButtonWindows8(temp, MaterialBlue.M500, MaterialBlue.M300, 5);
    ssMaterialLightBlue: BCButtonWindows8(temp, MaterialLightBlue.M500,
        MaterialLightBlue.M300, 5);
    ssMaterialCyan: BCButtonWindows8(temp, MaterialCyan.M500, MaterialCyan.M300, 5);
    ssMaterialTeal: BCButtonWindows8(temp, MaterialTeal.M500, MaterialTeal.M300, 5);
    ssMaterialGreen: BCButtonWindows8(temp, MaterialGreen.M500, MaterialGreen.M300, 5);
    ssMaterialLightGreen: BCButtonWindows8(temp, MaterialLightGreen.M500,
        MaterialLightGreen.M300, 5);
    ssMaterialLime: BCButtonWindows8(temp, MaterialLime.M500, MaterialLime.M300, 5);
    ssMaterialYellow: BCButtonWindows8(temp, MaterialYellow.M500,
        MaterialYellow.M300, 5);
    ssMaterialAmber: BCButtonWindows8(temp, MaterialAmber.M500, MaterialAmber.M300, 5);
    ssMaterialOrange: BCButtonWindows8(temp, MaterialOrange.M500,
        MaterialOrange.M300, 5);
    ssMaterialDeepOrange: BCButtonWindows8(temp, MaterialDeepOrange.M500,
        MaterialDeepOrange.M300, 5);
    ssMaterialBrown: BCButtonWindows8(temp, MaterialBrown.M500, MaterialBrown.M300, 5);
    ssMaterialGrey: BCButtonWindows8(temp, MaterialGrey.M500, MaterialGrey.M300, 5);
    ssMaterialBlueGrey: BCButtonWindows8(temp, MaterialBlueGrey.M500,
        MaterialBlueGrey.M300, 5);
  end;
  StyleButtons(AControl, temp);
  temp.Free;
end;

procedure DrawItem(Control: TWinControl; Index: integer; ARect: TRect;
  State: TOwnerDrawState; Style: TBCSampleDrawing);
var
  temp: TBGRABitmap;
  str: string;
begin
  if Control is TListBox then
    str := TListBox(Control).Items[Index]
  else if Control is TComboBox then
    str := TComboBox(Control).Items[Index];

  temp := TBGRABitmap.Create(ARect.Right - ARect.Left, ARect.Bottom -
    ARect.Top, Control.Color);

  temp.FontAntialias := True;
  temp.FontHeight := 0;
  temp.FontQuality := fqSystemClearType;

  if odSelected in State then
    DrawSample(temp, Style, alNone);

  temp.TextOut(2, 0, {%H-}str, BGRABlack);

  if Control is TListBox then
    temp.Draw(TListBox(Control).Canvas, ARect, False)
  else if Control is TComboBox then
    temp.Draw(TComboBox(Control).Canvas, ARect, False);

  temp.Free;
end;

procedure BCButtonWindows7(AButton: TBCButton);
begin
  AButton.Rounding.RoundX := 3;
  AButton.Rounding.RoundY := 3;
  AButton.RoundingDropDown.Assign(AButton.Rounding);

  with AButton.StateNormal do
  begin
    Background.Gradient1EndPercent := 50;
    Background.Gradient2.Point1XPercent := 0;
    Background.Gradient2.Point1YPercent := 0;
    Background.Gradient2.Point2YPercent := 100;
    Background.Gradient2.GradientType := gtLinear;
    Border.Color := RGBToColor(112, 112, 112);
    Border.LightWidth := 1;
    Border.LightOpacity := 175;
    Border.Style := bboSolid;
    FontEx.Color := clBlack;
    FontEx.Shadow := False;
    FontEx.Style := [];
  end;

  AButton.StateHover.Assign(AButton.StateNormal);
  AButton.StateClicked.Assign(AButton.StateNormal);

  with AButton.StateNormal do
  begin
    Background.Gradient1.StartColor := RGBToColor(242, 242, 242);
    Background.Gradient1.EndColor := RGBToColor(235, 235, 235);
    Background.Gradient2.StartColor := RGBToColor(221, 221, 221);
    Background.Gradient2.EndColor := RGBToColor(207, 207, 207);
  end;

  with AButton.StateHover do
  begin
    Background.Gradient1.StartColor := RGBToColor(234, 246, 253);
    Background.Gradient1.EndColor := RGBToColor(217, 240, 252);
    Background.Gradient2.StartColor := RGBToColor(190, 230, 253);
    Background.Gradient2.EndColor := RGBToColor(167, 217, 245);
    Border.Color := RGBToColor(60, 127, 177);
  end;

  with AButton.StateClicked do
  begin
    Background.Gradient1.StartColor := RGBToColor(229, 244, 252);
    Background.Gradient1.EndColor := RGBToColor(196, 229, 246);
    Background.Gradient2.StartColor := RGBToColor(152, 209, 239);
    Background.Gradient2.EndColor := RGBToColor(104, 179, 219);
    Background.Gradient1EndPercent := 55;
    Border.Color := RGBToColor(44, 98, 139);
    Border.LightOpacity := 100;
    Border.LightColor := clBlack;
  end;
end;

procedure BCButtonWindows7ToolBar(AButton: TBCButton);
begin
  AButton.Rounding.RoundX := 2;
  AButton.Rounding.RoundY := 2;
  AButton.RoundingDropDown.Assign(AButton.Rounding);

  with AButton.StateNormal do
  begin
    Background.Gradient1EndPercent := 50;
    Background.Gradient2.Point1XPercent := 0;
    Background.Gradient2.Point1YPercent := 0;
    Background.Gradient2.Point2YPercent := 100;
    Background.Gradient2.GradientType := gtLinear;
    Border.Color := RGBToColor(187, 202, 219);
    Border.LightWidth := 1;
    Border.LightOpacity := 200;
    Border.Style := bboSolid;
    FontEx.Color := RGBToColor(30, 57, 91);
    FontEx.Shadow := False;
    FontEx.Style := [];
  end;

  AButton.StateHover.Assign(AButton.StateNormal);
  AButton.StateClicked.Assign(AButton.StateNormal);

  with AButton.StateNormal do
  begin
    Background.Gradient1.StartColor := RGBToColor(245, 250, 255);
    Background.Gradient1.EndColor := RGBToColor(230, 240, 250);
    Background.Gradient2.StartColor := RGBToColor(220, 230, 244);
    Background.Gradient2.EndColor := RGBToColor(221, 233, 247);
  end;

  with AButton.StateHover do
  begin
    Background.Gradient1.StartColor := RGBToColor(248, 251, 254);
    Background.Gradient1.EndColor := RGBToColor(237, 242, 250);
    Background.Gradient2.StartColor := RGBToColor(215, 228, 244);
    Background.Gradient2.EndColor := RGBToColor(193, 210, 232);
  end;

  with AButton.StateClicked do
  begin
    Background.Gradient1.StartColor := RGBToColor(226, 236, 245);
    Background.Gradient1.EndColor := RGBToColor(216, 228, 241);
    Background.Gradient2.StartColor := RGBToColor(207, 219, 236);
    Background.Gradient2.EndColor := RGBToColor(207, 220, 237);
  end;
end;

procedure BCButtonOffice2010(AButton: TBCButton);
begin
  AButton.Rounding.RoundX := 3;
  AButton.Rounding.RoundY := 3;
  AButton.RoundingDropDown.Assign(AButton.Rounding);

  with AButton.StateNormal do
  begin
    Background.Gradient1EndPercent := 50;
    Background.Gradient2.Point1XPercent := 0;
    Background.Gradient2.Point1YPercent := 0;
    Background.Gradient2.Point2YPercent := 100;
    Background.Gradient2.GradientType := gtLinear;
    Border.Color := RGBToColor(207, 208, 210);
    Border.LightWidth := 1;
    Border.LightOpacity := 175;
    Border.Style := bboSolid;
    FontEx.Color := clBlack;
    FontEx.Shadow := False;
    FontEx.Style := [];
  end;

  AButton.StateHover.Assign(AButton.StateNormal);
  AButton.StateClicked.Assign(AButton.StateNormal);

  with AButton.StateNormal do
  begin
    Background.Gradient1.StartColor := RGBToColor(255, 255, 255);
    Background.Gradient1.EndColor := RGBToColor(237, 239, 241);
    Background.Gradient1EndPercent := 100;
  end;

  with AButton.StateHover do
  begin
    Background.Gradient1.StartColor := RGBToColor(254, 241, 189);
    Background.Gradient1.EndColor := RGBToColor(254, 228, 134);
    Background.Gradient2.StartColor := RGBToColor(254, 228, 134);
    Background.Gradient2.EndColor := RGBToColor(254, 248, 196);
    Border.Color := RGBToColor(244, 210, 81);
  end;

  with AButton.StateClicked do
  begin
    Background.Gradient1.StartColor := RGBToColor(255, 229, 117);
    Background.Gradient1.EndColor := RGBToColor(255, 216, 107);
    Background.Gradient2.StartColor := RGBToColor(255, 216, 107);
    Background.Gradient2.EndColor := RGBToColor(255, 239, 129);
    Border.Color := RGBToColor(194, 161, 63);
    Border.LightWidth := 0;
  end;
end;

procedure BCButtonFlashPlayer(AButton: TBCButton);
begin
  AButton.Rounding.RoundX := 1;
  AButton.Rounding.RoundY := 1;
  AButton.RoundingDropDown.Assign(AButton.Rounding);

  with AButton.StateNormal do
  begin
    Background.Gradient1EndPercent := 50;
    Background.Gradient2.Point1XPercent := 0;
    Background.Gradient2.Point1YPercent := 0;
    Background.Gradient2.Point2YPercent := 100;
    Background.Gradient2.GradientType := gtLinear;
    Border.Color := RGBToColor(24, 24, 24);
    Border.LightWidth := 1;
    Border.LightOpacity := 20;
    Border.Style := bboSolid;
    FontEx.Color := clWhite;
    FontEx.Shadow := True;
    FontEx.ShadowRadius := 1;
    FontEx.ShadowOffsetX := 1;
    FontEx.ShadowOffsetY := 1;
    FontEx.Style := [];
  end;

  AButton.StateHover.Assign(AButton.StateNormal);
  AButton.StateClicked.Assign(AButton.StateNormal);

  with AButton.StateNormal do
  begin
    Background.Gradient1.StartColor := RGBToColor(104, 104, 104);
    Background.Gradient1.EndColor := RGBToColor(104, 104, 104);
    Background.Gradient2.StartColor := RGBToColor(103, 103, 103);
    Background.Gradient2.EndColor := RGBToColor(71, 71, 71);
  end;

  with AButton.StateHover do
  begin
    Background.Gradient1.StartColor := RGBToColor(118, 118, 118);
    Background.Gradient1.EndColor := RGBToColor(118, 118, 118);
    Background.Gradient2.StartColor := RGBToColor(117, 117, 117);
    Background.Gradient2.EndColor := RGBToColor(81, 81, 81);
    Border.Color := RGBToColor(24, 24, 24);
  end;

  with AButton.StateClicked do
  begin
    Background.Gradient1.StartColor := RGBToColor(92, 92, 92);
    Background.Gradient1.EndColor := RGBToColor(92, 92, 92);
    Background.Gradient2.StartColor := RGBToColor(91, 91, 91);
    Background.Gradient2.EndColor := RGBToColor(62, 62, 62);
    Background.Gradient1EndPercent := 55;
    Border.Color := RGBToColor(24, 24, 24);
  end;
end;

procedure BCButtonMacOSXLion(AButton: TBCButton);
begin
  AButton.Rounding.RoundX := 3;
  AButton.Rounding.RoundY := 3;
  AButton.RoundingDropDown.Assign(AButton.Rounding);

  with AButton.StateNormal do
  begin
    Background.Gradient1EndPercent := 50;
    Background.Gradient2.Point1XPercent := 0;
    Background.Gradient2.Point1YPercent := 0;
    Background.Gradient2.Point2YPercent := 100;
    Background.Gradient2.GradientType := gtLinear;
    Border.Color := RGBToColor(154, 154, 154);
    Border.LightWidth := 1;
    Border.LightOpacity := 175;
    Border.Style := bboSolid;
    FontEx.Color := clBlack;
    FontEx.Shadow := False;
    FontEx.Style := [];
  end;

  AButton.StateHover.Assign(AButton.StateNormal);
  AButton.StateClicked.Assign(AButton.StateNormal);

  with AButton.StateNormal do
  begin
    Background.Gradient1.StartColor := RGBToColor(255, 255, 255);
    Background.Gradient1.EndColor := RGBToColor(243, 243, 243);
    Background.Gradient2.StartColor := RGBToColor(236, 236, 236);
    Background.Gradient2.EndColor := RGBToColor(235, 235, 235);
  end;

  with AButton.StateHover do
  begin
    Background.Gradient1.StartColor := RGBToColor(204, 229, 252);
    Background.Gradient1.EndColor := RGBToColor(161, 209, 249);
    Background.Gradient2.StartColor := RGBToColor(143, 202, 251);
    Background.Gradient2.EndColor := RGBToColor(207, 245, 253);
    Border.Color := RGBToColor(86, 87, 143);
  end;

  with AButton.StateClicked do
  begin
    Background.Gradient1.StartColor := RGBToColor(144, 195, 241);
    Background.Gradient1.EndColor := RGBToColor(113, 180, 239);
    Background.Gradient2.StartColor := RGBToColor(97, 173, 240);
    Background.Gradient2.EndColor := RGBToColor(147, 206, 241);
    Background.Gradient1EndPercent := 55;
    Border.Color := RGBToColor(86, 87, 143);
    Border.LightWidth := 0;
  end;
end;

procedure BCButtonWindows8(AButton: TBCButton; cl1, cl2: TColor; rounding: integer = 1);
begin
  AButton.Rounding.RoundX := rounding;
  AButton.Rounding.RoundY := rounding;
  AButton.RoundingDropDown.Assign(AButton.Rounding);

  with AButton.StateNormal do
  begin
    Background.Style := bbsColor;
    Background.Color := cl1;
    Border.Style := bboSolid;
    Border.Width := 1;
    Border.Color := cl1;
    Border.LightWidth := 0;
    Border.LightOpacity := 255;
    Border.Style := bboSolid;
    FontEx.Color := GetContrastColor(cl1);
    FontEx.Shadow := False;
    FontEx.Style := [];
  end;

  AButton.StateHover.Assign(AButton.StateNormal);
  AButton.StateClicked.Assign(AButton.StateNormal);

  with AButton.StateHover do
  begin
    Background.Color := cl2;
    Border.Color := cl2;
  end;

  with AButton.StateClicked do
  begin
    Background.Color := cl2;
    Border.Color := cl2;
  end;
end;

procedure BCButtonWindows7(AButton: TBCButtonFocus);
begin
  AButton.Rounding.RoundX := 3;
  AButton.Rounding.RoundY := 3;
  AButton.RoundingDropDown.Assign(AButton.Rounding);

  with AButton.StateNormal do
  begin
    Background.Gradient1EndPercent := 50;
    Background.Gradient2.Point1XPercent := 0;
    Background.Gradient2.Point1YPercent := 0;
    Background.Gradient2.Point2YPercent := 100;
    Background.Gradient2.GradientType := gtLinear;
    Border.Color := RGBToColor(112, 112, 112);
    Border.LightWidth := 1;
    Border.LightOpacity := 175;
    Border.Style := bboSolid;
    FontEx.Color := clBlack;
    FontEx.Shadow := False;
    FontEx.Style := [];
  end;

  AButton.StateHover.Assign(AButton.StateNormal);
  AButton.StateClicked.Assign(AButton.StateNormal);

  with AButton.StateNormal do
  begin
    Background.Gradient1.StartColor := RGBToColor(242, 242, 242);
    Background.Gradient1.EndColor := RGBToColor(235, 235, 235);
    Background.Gradient2.StartColor := RGBToColor(221, 221, 221);
    Background.Gradient2.EndColor := RGBToColor(207, 207, 207);
  end;

  with AButton.StateHover do
  begin
    Background.Gradient1.StartColor := RGBToColor(234, 246, 253);
    Background.Gradient1.EndColor := RGBToColor(217, 240, 252);
    Background.Gradient2.StartColor := RGBToColor(190, 230, 253);
    Background.Gradient2.EndColor := RGBToColor(167, 217, 245);
    Border.Color := RGBToColor(60, 127, 177);
  end;

  with AButton.StateClicked do
  begin
    Background.Gradient1.StartColor := RGBToColor(229, 244, 252);
    Background.Gradient1.EndColor := RGBToColor(196, 229, 246);
    Background.Gradient2.StartColor := RGBToColor(152, 209, 239);
    Background.Gradient2.EndColor := RGBToColor(104, 179, 219);
    Background.Gradient1EndPercent := 55;
    Border.Color := RGBToColor(44, 98, 139);
    Border.LightOpacity := 100;
    Border.LightColor := clBlack;
  end;
end;

procedure BCButtonWindows7ToolBar(AButton: TBCButtonFocus);
begin
  AButton.Rounding.RoundX := 2;
  AButton.Rounding.RoundY := 2;
  AButton.RoundingDropDown.Assign(AButton.Rounding);

  with AButton.StateNormal do
  begin
    Background.Gradient1EndPercent := 50;
    Background.Gradient2.Point1XPercent := 0;
    Background.Gradient2.Point1YPercent := 0;
    Background.Gradient2.Point2YPercent := 100;
    Background.Gradient2.GradientType := gtLinear;
    Border.Color := RGBToColor(187, 202, 219);
    Border.LightWidth := 1;
    Border.LightOpacity := 200;
    Border.Style := bboSolid;
    FontEx.Color := RGBToColor(30, 57, 91);
    FontEx.Shadow := False;
    FontEx.Style := [];
  end;

  AButton.StateHover.Assign(AButton.StateNormal);
  AButton.StateClicked.Assign(AButton.StateNormal);

  with AButton.StateNormal do
  begin
    Background.Gradient1.StartColor := RGBToColor(245, 250, 255);
    Background.Gradient1.EndColor := RGBToColor(230, 240, 250);
    Background.Gradient2.StartColor := RGBToColor(220, 230, 244);
    Background.Gradient2.EndColor := RGBToColor(221, 233, 247);
  end;

  with AButton.StateHover do
  begin
    Background.Gradient1.StartColor := RGBToColor(248, 251, 254);
    Background.Gradient1.EndColor := RGBToColor(237, 242, 250);
    Background.Gradient2.StartColor := RGBToColor(215, 228, 244);
    Background.Gradient2.EndColor := RGBToColor(193, 210, 232);
  end;

  with AButton.StateClicked do
  begin
    Background.Gradient1.StartColor := RGBToColor(226, 236, 245);
    Background.Gradient1.EndColor := RGBToColor(216, 228, 241);
    Background.Gradient2.StartColor := RGBToColor(207, 219, 236);
    Background.Gradient2.EndColor := RGBToColor(207, 220, 237);
  end;
end;

procedure BCButtonOffice2010(AButton: TBCButtonFocus);
begin
  AButton.Rounding.RoundX := 3;
  AButton.Rounding.RoundY := 3;
  AButton.RoundingDropDown.Assign(AButton.Rounding);

  with AButton.StateNormal do
  begin
    Background.Gradient1EndPercent := 50;
    Background.Gradient2.Point1XPercent := 0;
    Background.Gradient2.Point1YPercent := 0;
    Background.Gradient2.Point2YPercent := 100;
    Background.Gradient2.GradientType := gtLinear;
    Border.Color := RGBToColor(207, 208, 210);
    Border.LightWidth := 1;
    Border.LightOpacity := 175;
    Border.Style := bboSolid;
    FontEx.Color := clBlack;
    FontEx.Shadow := False;
    FontEx.Style := [];
  end;

  AButton.StateHover.Assign(AButton.StateNormal);
  AButton.StateClicked.Assign(AButton.StateNormal);

  with AButton.StateNormal do
  begin
    Background.Gradient1.StartColor := RGBToColor(255, 255, 255);
    Background.Gradient1.EndColor := RGBToColor(237, 239, 241);
    Background.Gradient1EndPercent := 100;
  end;

  with AButton.StateHover do
  begin
    Background.Gradient1.StartColor := RGBToColor(254, 241, 189);
    Background.Gradient1.EndColor := RGBToColor(254, 228, 134);
    Background.Gradient2.StartColor := RGBToColor(254, 228, 134);
    Background.Gradient2.EndColor := RGBToColor(254, 248, 196);
    Border.Color := RGBToColor(244, 210, 81);
  end;

  with AButton.StateClicked do
  begin
    Background.Gradient1.StartColor := RGBToColor(255, 229, 117);
    Background.Gradient1.EndColor := RGBToColor(255, 216, 107);
    Background.Gradient2.StartColor := RGBToColor(255, 216, 107);
    Background.Gradient2.EndColor := RGBToColor(255, 239, 129);
    Border.Color := RGBToColor(194, 161, 63);
    Border.LightWidth := 0;
  end;
end;

procedure BCButtonFlashPlayer(AButton: TBCButtonFocus);
begin
  AButton.Rounding.RoundX := 1;
  AButton.Rounding.RoundY := 1;
  AButton.RoundingDropDown.Assign(AButton.Rounding);

  with AButton.StateNormal do
  begin
    Background.Gradient1EndPercent := 50;
    Background.Gradient2.Point1XPercent := 0;
    Background.Gradient2.Point1YPercent := 0;
    Background.Gradient2.Point2YPercent := 100;
    Background.Gradient2.GradientType := gtLinear;
    Border.Color := RGBToColor(24, 24, 24);
    Border.LightWidth := 1;
    Border.LightOpacity := 20;
    Border.Style := bboSolid;
    FontEx.Color := clWhite;
    FontEx.Shadow := True;
    FontEx.ShadowRadius := 1;
    FontEx.ShadowOffsetX := 1;
    FontEx.ShadowOffsetY := 1;
    FontEx.Style := [];
  end;

  AButton.StateHover.Assign(AButton.StateNormal);
  AButton.StateClicked.Assign(AButton.StateNormal);

  with AButton.StateNormal do
  begin
    Background.Gradient1.StartColor := RGBToColor(104, 104, 104);
    Background.Gradient1.EndColor := RGBToColor(104, 104, 104);
    Background.Gradient2.StartColor := RGBToColor(103, 103, 103);
    Background.Gradient2.EndColor := RGBToColor(71, 71, 71);
  end;

  with AButton.StateHover do
  begin
    Background.Gradient1.StartColor := RGBToColor(118, 118, 118);
    Background.Gradient1.EndColor := RGBToColor(118, 118, 118);
    Background.Gradient2.StartColor := RGBToColor(117, 117, 117);
    Background.Gradient2.EndColor := RGBToColor(81, 81, 81);
    Border.Color := RGBToColor(24, 24, 24);
  end;

  with AButton.StateClicked do
  begin
    Background.Gradient1.StartColor := RGBToColor(92, 92, 92);
    Background.Gradient1.EndColor := RGBToColor(92, 92, 92);
    Background.Gradient2.StartColor := RGBToColor(91, 91, 91);
    Background.Gradient2.EndColor := RGBToColor(62, 62, 62);
    Background.Gradient1EndPercent := 55;
    Border.Color := RGBToColor(24, 24, 24);
  end;
end;

procedure BCButtonMacOSXLion(AButton: TBCButtonFocus);
begin
  AButton.Rounding.RoundX := 3;
  AButton.Rounding.RoundY := 3;
  AButton.RoundingDropDown.Assign(AButton.Rounding);

  with AButton.StateNormal do
  begin
    Background.Gradient1EndPercent := 50;
    Background.Gradient2.Point1XPercent := 0;
    Background.Gradient2.Point1YPercent := 0;
    Background.Gradient2.Point2YPercent := 100;
    Background.Gradient2.GradientType := gtLinear;
    Border.Color := RGBToColor(154, 154, 154);
    Border.LightWidth := 1;
    Border.LightOpacity := 175;
    Border.Style := bboSolid;
    FontEx.Color := clBlack;
    FontEx.Shadow := False;
    FontEx.Style := [];
  end;

  AButton.StateHover.Assign(AButton.StateNormal);
  AButton.StateClicked.Assign(AButton.StateNormal);

  with AButton.StateNormal do
  begin
    Background.Gradient1.StartColor := RGBToColor(255, 255, 255);
    Background.Gradient1.EndColor := RGBToColor(243, 243, 243);
    Background.Gradient2.StartColor := RGBToColor(236, 236, 236);
    Background.Gradient2.EndColor := RGBToColor(235, 235, 235);
  end;

  with AButton.StateHover do
  begin
    Background.Gradient1.StartColor := RGBToColor(204, 229, 252);
    Background.Gradient1.EndColor := RGBToColor(161, 209, 249);
    Background.Gradient2.StartColor := RGBToColor(143, 202, 251);
    Background.Gradient2.EndColor := RGBToColor(207, 245, 253);
    Border.Color := RGBToColor(86, 87, 143);
  end;

  with AButton.StateClicked do
  begin
    Background.Gradient1.StartColor := RGBToColor(144, 195, 241);
    Background.Gradient1.EndColor := RGBToColor(113, 180, 239);
    Background.Gradient2.StartColor := RGBToColor(97, 173, 240);
    Background.Gradient2.EndColor := RGBToColor(147, 206, 241);
    Background.Gradient1EndPercent := 55;
    Border.Color := RGBToColor(86, 87, 143);
    Border.LightWidth := 0;
  end;
end;

procedure BCButtonWindows8(AButton: TBCButtonFocus; cl1, cl2: TColor;
  rounding: integer = 1);
begin
  AButton.Rounding.RoundX := rounding;
  AButton.Rounding.RoundY := rounding;
  AButton.RoundingDropDown.Assign(AButton.Rounding);

  with AButton.StateNormal do
  begin
    Background.Style := bbsColor;
    Background.Color := cl1;
    Border.Style := bboSolid;
    Border.Width := 1;
    Border.Color := cl1;
    Border.LightWidth := 0;
    Border.LightOpacity := 255;
    Border.Style := bboSolid;
    FontEx.Color := GetContrastColor(cl1);
    FontEx.Shadow := False;
    FontEx.Style := [];
  end;

  AButton.StateHover.Assign(AButton.StateNormal);
  AButton.StateClicked.Assign(AButton.StateNormal);

  with AButton.StateHover do
  begin
    Background.Color := cl2;
    Border.Color := cl2;
  end;

  with AButton.StateClicked do
  begin
    Background.Color := cl2;
    Border.Color := cl2;
  end;
end;

procedure DrawFlashPlayerBody(ABitmap: TBGRABitmap);
begin
  with ABitmap do
  begin
    GradientFill(0, 0, Width, Height, BGRA(203, 19, 23, 255), BGRA(110, 3, 20, 255),
      gtLinear, PointF(0, 0), PointF(0, Height), dmSet);
    Rectangle(0, 0, Width, Height + 1, BGRA(0, 0, 0, 215), dmDrawWithTransparency);
  end;
end;

procedure DrawFlashPlayerButtonPanel(ABitmap: TBGRABitmap);
begin
  with ABitmap do
  begin
    DrawHorizLine(0, 0, Width, BGRA(30, 30, 30, 255));
    DrawHorizLine(0, Height - 1, Width, BGRA(62, 62, 62, 255));
    Rectangle(0, 1, Width, Height - 1, BGRA(91, 91, 91, 255),
      BGRA(76, 76, 76, 255), dmSet);
  end;
end;

procedure DrawWindows7ToolBar(ABitmap: TBGRABitmap; ADir: TAlign;
  Smooth: boolean = False);
var
  tempBmp: TBGRABitmap;
begin
  if Smooth then
    tempBmp := DoubleGradientAlphaFill(Rect(0, 0, ABitmap.Width, ABitmap.Height),
      BGRA(245, 249, 255, 255), BGRA(222, 232, 245, 255), BGRA(220, 230, 245, 255),
      BGRA(222, 230, 245, 255), gdVertical, gdVertical, gdVertical, 0.50)
  else
    tempBmp := DoubleGradientAlphaFill(Rect(0, 0, ABitmap.Width, ABitmap.Height),
      BGRA(245, 250, 255, 255), BGRA(230, 240, 250, 255), BGRA(220, 230, 244, 255),
      BGRA(221, 233, 247, 255), gdVertical, gdVertical, gdVertical, 0.50);

  ABitmap.BlendImage(0, 0, tempBmp, boLinearBlend);
  tempBmp.Free;

  case ADir of
    alLeft: with ABitmap do
      begin
        if not Smooth then
          Rectangle(0, 0, Width - 2, Height, BGRA(255, 255, 255, 100),
            dmDrawWithTransparency);
        SetVertLine(Width - 1, 0, Height - 1, BGRA(160, 175, 195, 255));
        SetVertLine(Width - 2, 0, Height - 1, BGRA(205, 218, 234, 255));
      end;
    alTop: with ABitmap do
      begin
        if not Smooth then
          Rectangle(0, 0, Width, Height - 2, BGRA(255, 255, 255, 100),
            dmDrawWithTransparency);
        SetHorizLine(0, Height - 1, Width - 1, BGRA(160, 175, 195, 255));
        SetHorizLine(0, Height - 2, Width - 1, BGRA(205, 218, 234, 255));
      end;
    alRight: with ABitmap do
      begin
        if not Smooth then
          Rectangle(2, 0, Width, Height, BGRA(255, 255, 255, 100),
            dmDrawWithTransparency);
        SetVertLine(0, 0, Height, BGRA(160, 175, 195, 255));
        SetVertLine(1, 0, Height, BGRA(205, 218, 234, 255));
      end;
    alBottom: with ABitmap do
      begin
        if not Smooth then
          Rectangle(0, 2, Width, Height, BGRA(255, 255, 255, 100),
            dmDrawWithTransparency);
        SetHorizLine(0, 0, Width - 1, BGRA(160, 175, 195, 255));
        SetHorizLine(0, 1, Width - 1, BGRA(205, 218, 234, 255));
      end;
    alClient, alCustom, alNone: with ABitmap do
      begin
        if not Smooth then
          Rectangle(0, 0, Width, Height, BGRA(255, 255, 255, 100),
            dmDrawWithTransparency);
      end;
  end;
end;

function StrToTBCSampleDrawing(const s: ansistring): TBCSampleDrawing;
var
  ss: TBCSampleDrawing;
  ls: ansistring;
begin
  Result := sdDefault;
  ls := UTF8LowerCase(s);
  for ss := low(TBCSampleDrawing) to high(TBCSampleDrawing) do
    if ls = UTF8LowerCase(BCSampleDrawingStr[ss]) then
    begin
      Result := ss;
      break;
    end;
end;

procedure BCSampleDrawingStrList(s: TStrings);
var
  ss: TBCSampleDrawing;
begin
  for ss := low(TBCSampleDrawing) to high(TBCSampleDrawing) do
    s.Add(BCSampleDrawingStr[ss]);
end;

procedure DrawSample(ABitmap: TBGRABitmap; Element: TBCSampleDrawing;
  Align: TAlign = alNone);
begin
  case Element of
    sdFlashPlayerBody: DrawFlashPlayerBody(ABitmap);
    sdFlashPlayerButtonPanel: DrawFlashPlayerButtonPanel(ABitmap);
    sdWindows7Toolbar: DrawWindows7ToolBar(ABitmap, Align);
    sdiOSBar: DrawiOSBar(ABitmap);
    sdiOSToolBar: DrawiOSToolBar(ABitmap, False);
    sdiOSBackground: DrawiOSBackground(ABitmap);
    sdWindows8_1: DrawWindows8Background(ABitmap, clScheme1_Background,
        clScheme1_Selection);
    sdWindows8_2: DrawWindows8Background(ABitmap, clScheme2_Background,
        clScheme2_Selection);
    sdWindows8_3: DrawWindows8Background(ABitmap, clScheme3_Background,
        clScheme3_Selection);
    sdWindows8_4: DrawWindows8Background(ABitmap, clScheme4_Background,
        clScheme4_Selection);
    sdWindows8_5: DrawWindows8Background(ABitmap, clScheme5_Background,
        clScheme5_Selection);
    sdWindows8_6: DrawWindows8Background(ABitmap, clScheme6_Background,
        clScheme6_Selection);
    sdWindows8_7: DrawWindows8Background(ABitmap, clScheme7_Background,
        clScheme7_Selection);
    sdWindows8_8: DrawWindows8Background(ABitmap, clScheme8_Background,
        clScheme8_Selection);
    sdWindows8_9: DrawWindows8Background(ABitmap, clScheme9_Background,
        clScheme9_Selection);
    sdWindows8_10: DrawWindows8Background(ABitmap, clScheme10_Background,
        clScheme10_Selection);
    sdWindows8_11: DrawWindows8Background(ABitmap, clScheme11_Background,
        clScheme11_Selection);
    sdWindows8_12: DrawWindows8Background(ABitmap, clScheme12_Background,
        clScheme12_Selection);
    sdWindows8_13: DrawWindows8Background(ABitmap, clScheme13_Background,
        clScheme13_Selection);
    sdWindows8_14: DrawWindows8Background(ABitmap, clScheme14_Background,
        clScheme14_Selection);
    sdWindows8_15: DrawWindows8Background(ABitmap, clScheme15_Background,
        clScheme15_Selection);
    sdWindows8_16: DrawWindows8Background(ABitmap, clScheme16_Background,
        clScheme16_Selection);
    sdWindows8_17: DrawWindows8Background(ABitmap, clScheme17_Background,
        clScheme17_Selection);
    sdWindows8_18: DrawWindows8Background(ABitmap, clScheme18_Background,
        clScheme18_Selection);
    sdWindows8_19: DrawWindows8Background(ABitmap, clScheme19_Background,
        clScheme19_Selection);
    sdWindows8_20: DrawWindows8Background(ABitmap, clScheme20_Background,
        clScheme20_Selection);
    sdWindows8_21: DrawWindows8Background(ABitmap, clScheme21_Background,
        clScheme21_Selection);
    sdWindows8_22: DrawWindows8Background(ABitmap, clScheme22_Background,
        clScheme22_Selection);
    sdWindows8_23: DrawWindows8Background(ABitmap, clScheme23_Background,
        clScheme23_Selection);
    sdWindows8_24: DrawWindows8Background(ABitmap, clScheme24_Background,
        clScheme24_Selection);
    sdWindows8_25: DrawWindows8Background(ABitmap, clScheme25_Background,
        clScheme25_Selection);
  end;
end;

function DrawSample(AWidth, AHeight: integer; Element: TBCSampleDrawing;
  Align: TAlign = alNone): TBGRABitmap;
var
  ABitmap: TBGRABitmap;
begin
  ABitmap := TBGRABitmap.Create(AWidth, AHeight);
  DrawSample(ABitmap, Element, Align);
  Result := ABitmap;
end;

procedure DrawiOSBar(ABitmap: TBGRABitmap);
begin
  ABitmap.GradientFill(0, 0, ABitmap.Width, ABitmap.Height - 4,
    BGRA(238, 245, 248, 255), BGRA(196, 204, 208, 255), gtLinear,
    PointF(0, 0), PointF(0, ABitmap.Height - 4), dmSet);
  with ABitmap do
  begin
    // Bottom Bevel
    SetHorizLine(0, Height - 4, Width - 1, BGRA(190, 198, 202, 255));
    SetHorizLine(0, Height - 3, Width - 1, BGRA(201, 209, 213, 255));
    SetHorizLine(0, Height - 2, Width - 1, BGRA(134, 142, 147, 255));
    SetHorizLine(0, Height - 1, Width - 1, BGRA(177, 180, 182, 255));
  end;
end;

procedure DrawiOSToolBar(ABitmap: TBGRABitmap; Shadow: boolean = True);
begin
  if Shadow then
  begin
    DoubleGradientAlphaFill(ABitmap, Rect(0, 3, ABitmap.Width, ABitmap.Height - 6),
      BGRA(172, 185, 201), BGRA(134, 153, 178, 255),
      BGRA(125, 147, 175, 255), BGRA(110, 132, 162, 255),
      gdVertical, gdVertical, gdVertical, 0.5);
    with ABitmap do
    begin
      // Top Bevel
      SetHorizLine(0, 0, Width - 1, BGRA(201, 210, 221, 255));
      SetHorizLine(0, 1, Width - 1, BGRA(173, 184, 200, 255));
      SetHorizLine(0, 2, Width - 1, BGRA(179, 190, 205, 255));
      // Bottom Bevel
      SetHorizLine(0, Height - 6, Width - 1, BGRA(107, 129, 158, 255));
      SetHorizLine(0, Height - 5, Width - 1, BGRA(116, 139, 170, 255));
      SetHorizLine(0, Height - 4, Width - 1, BGRA(48, 54, 62, 255));
      // Bottom Shadow
      SetHorizLine(0, Height - 3, Width - 1, BGRA(0, 0, 0, 75));
      SetHorizLine(0, Height - 2, Width - 1, BGRA(255, 255, 255, 50));
      SetHorizLine(0, Height - 1, Width - 1, BGRA(0, 0, 0, 10));
    end;
  end
  else
  begin
    DoubleGradientAlphaFill(ABitmap, Rect(0, 3, ABitmap.Width, ABitmap.Height - 3),
      BGRA(172, 185, 201), BGRA(134, 153, 178, 255),
      BGRA(125, 147, 175, 255), BGRA(110, 132, 162, 255),
      gdVertical, gdVertical, gdVertical, 0.5);
    with ABitmap do
    begin
      // Top Bevel
      SetHorizLine(0, 0, Width - 1, BGRA(201, 210, 221, 255));
      SetHorizLine(0, 1, Width - 1, BGRA(173, 184, 200, 255));
      SetHorizLine(0, 2, Width - 1, BGRA(179, 190, 205, 255));
      // Bottom Bevel
      SetHorizLine(0, Height - 3, Width - 1, BGRA(107, 129, 158, 255));
      SetHorizLine(0, Height - 2, Width - 1, BGRA(116, 139, 170, 255));
      SetHorizLine(0, Height - 1, Width - 1, BGRA(48, 54, 62, 255));
    end;
  end;
end;

procedure DrawiOSBackground(ABitmap: TBGRABitmap);
var
  temp: TBGRABitmap;
begin
  temp := TBGRABitmap.Create(16, 1, BGRA(197, 204, 211));
  with temp do
  begin
    SetPixel(7, 0, BGRA(203, 210, 217));
    SetPixel(14, 0, BGRA(204, 211, 218));
    BGRAReplace(temp, temp.FilterBlurRadial(1, rbFast));
    BGRAReplace(temp, temp.FilterSharpen);
  end;
  ABitmap.Fill(temp);
  temp.Free;
end;

procedure DrawWindows8Background(ABitmap: TBGRABitmap; cl1, cl2: TColor);
begin
  ABitmap.Fill(cl2);
end;

end.
