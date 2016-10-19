unit BGRACustomDrawn;

{$mode objfpc}{$H+}

interface

uses
  Classes, Types, FPCanvas, Graphics, Controls, Math, LazUTF8, Forms, ExtCtrls,
  BCThemeManager,
  { CustomDrawn }
  CustomDrawnControls, CustomDrawnDrawers, CustomDrawn_Common,
  { BGRABitmap }
  BGRABitmap, BGRABitmapTypes, BGRAGradients;

type

  { TBCDButton }

  TBCDButton = class(TCDButton)
  private
    FBCThemeManager: TBCThemeManager;
    procedure SetFBCThemeManager(AValue: TBCThemeManager);
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFBCThemeManager;
  end;

  { TBCDEdit }

  TBCDEdit = class(TCDEdit)
  private
    FBCThemeManager: TBCThemeManager;
    procedure SetFBCThemeManager(AValue: TBCThemeManager);
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFBCThemeManager;
  end;

  { TBCDStaticText }

  TBCDStaticText = class(TCDStaticText)
  private
    FBCThemeManager: TBCThemeManager;
    procedure SetFBCThemeManager(AValue: TBCThemeManager);
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFBCThemeManager;
  end;

  { TBCDProgressBar }

  TBCDProgressBar = class(TCDProgressBar)
  private
    FBCThemeManager: TBCThemeManager;
    procedure SetFBCThemeManager(AValue: TBCThemeManager);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFBCThemeManager;
  end;

  { TBCDSpinEdit }

  TBCDSpinEdit = class(TCDSpinEdit)
  private
    FBCThemeManager: TBCThemeManager;
    procedure SetFBCThemeManager(AValue: TBCThemeManager);
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFBCThemeManager;
  end;

  { TBCDCheckBox }

  TBCDCheckBox = class(TCDCheckBox)
  private
    FBCThemeManager: TBCThemeManager;
    procedure SetFBCThemeManager(AValue: TBCThemeManager);
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFBCThemeManager;
  end;

  { TBCDRadioButton }

  TBCDRadioButton = class(TCDRadioButton)
  private
    FBCThemeManager: TBCThemeManager;
    procedure SetFBCThemeManager(AValue: TBCThemeManager);
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFBCThemeManager;
  end;

  { TBCDPanel }

  TBCDPanel = class(TPanel)
  private
    FBCThemeManager: TBCThemeManager;
    FDarkTheme: boolean;
    procedure SetFBCThemeManager(AValue: TBCThemeManager);
    procedure SetFDarkTheme(AValue: boolean);
  protected
    procedure Paint; override;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property DarkTheme: boolean read FDarkTheme write SetFDarkTheme default True;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFBCThemeManager;
  end;

  { TBGRADrawer }

  TBGRADrawer = class(TCDDrawerCommon)
  private
    FRandSeed: integer;
  protected
    procedure AssignFont(Bitmap: TBGRABitmap; Font: TFont);
  public
    constructor Create; override;
    { General }
    function GetMeasures(AMeasureID: integer): integer; override;
    procedure DrawTickmark(ADest: TFPCustomCanvas; ADestPos: TPoint;
      AState: TCDControlState); override;
    function DPIAdjustment(const AValue: integer): integer;
    { Button }
    procedure DrawButton(ADest: TFPCustomCanvas; ADestPos: TPoint;
      ASize: TSize; AState: TCDControlState; AStateEx: TCDButtonStateEx); override;
    { Edit }
    procedure DrawEditBackground(ADest: TCanvas; ADestPos: TPoint;
      ASize: TSize; AState: TCDControlState; {%H-}AStateEx: TCDEditStateEx); override;
    procedure DrawEditFrame(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; {%H-}AStateEx: TCDEditStateEx); override;
    procedure DrawCaret(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;
    procedure DrawEdit(ADest: TCanvas; ASize: TSize; AState: TCDControlState;
      AStateEx: TCDEditStateEx); override;
    { Panel }
    procedure DrawPanel(ADest: TCanvas; ASize: TSize; {%H-}AState: TCDControlState;
      {%H-}AStateEx: TCDPanelStateEx); override;
    { Static Text }
    procedure DrawStaticText(ADest: TCanvas; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    { Progress Bar }
    procedure DrawProgressBar(ADest: TCanvas; ASize: TSize;
      {%H-}AState: TCDControlState; AStateEx: TCDProgressBarStateEx); override;
    { CheckBox }
    procedure DrawCheckBoxSquare(ADest: TCanvas; ADestPos: TPoint;
      ASize: TSize; AState: TCDControlState; {%H-}AStateEx: TCDControlStateEx); override;
    procedure DrawCheckBox(ADest: TCanvas; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    { RadioButton }
    procedure DrawRadioButtonCircle(ADest: TCanvas; {%H-}ADestPos: TPoint;
      {%H-}ASize: TSize; AState: TCDControlState; {%H-}AStateEx: TCDControlStateEx); override;
    procedure DrawRadioButton(ADest: TCanvas; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Custom Drawn', [TBCDButton, TBCDEdit,
    TBCDStaticText, TBCDProgressBar, TBCDSpinEdit, TBCDCheckBox,
    TBCDRadioButton, TBCDPanel]);
end;

{ TBCDRadioButton }

procedure TBCDRadioButton.SetFBCThemeManager(AValue: TBCThemeManager);
begin
  if FBCThemeManager = AValue then
    Exit;
  FBCThemeManager := AValue;
end;

{ TBCDCheckBox }

procedure TBCDCheckBox.SetFBCThemeManager(AValue: TBCThemeManager);
begin
  if FBCThemeManager = AValue then
    Exit;
  FBCThemeManager := AValue;
end;

{ TBCDSpinEdit }

procedure TBCDSpinEdit.SetFBCThemeManager(AValue: TBCThemeManager);
begin
  if FBCThemeManager = AValue then
    Exit;
  FBCThemeManager := AValue;
end;

{ TBCDStaticText }

procedure TBCDStaticText.SetFBCThemeManager(AValue: TBCThemeManager);
begin
  if FBCThemeManager = AValue then
    Exit;
  FBCThemeManager := AValue;
end;

{ TBCDEdit }

procedure TBCDEdit.SetFBCThemeManager(AValue: TBCThemeManager);
begin
  if FBCThemeManager = AValue then
    Exit;
  FBCThemeManager := AValue;
end;

{ TBCDButton }

procedure TBCDButton.SetFBCThemeManager(AValue: TBCThemeManager);
begin
  if FBCThemeManager = AValue then
    Exit;
  FBCThemeManager := AValue;
end;

{ TBCDPanel }

procedure TBCDPanel.SetFDarkTheme(AValue: boolean);
begin
  if FDarkTheme = AValue then
    Exit;
  FDarkTheme := AValue;
  Invalidate;
end;

procedure TBCDPanel.SetFBCThemeManager(AValue: TBCThemeManager);
begin
  if FBCThemeManager = AValue then
    Exit;
  FBCThemeManager := AValue;
end;

procedure TBCDPanel.Paint;
begin
  if DarkTheme then
  begin
    if BevelOuter <> bvNone then
    begin
      Canvas.Pen.Color := RGBToColor(40, 40, 40);
      Canvas.Brush.Color := RGBToColor(83, 83, 83);
      Canvas.Rectangle(0, 0, Width, Height);

      Canvas.Pen.Color := RGBToColor(106, 106, 106);
      Canvas.Line(1, 1, Width - 1, 1);
    end
    else
    begin
      Canvas.Pen.Color := RGBToColor(83, 83, 83);
      Canvas.Brush.Color := RGBToColor(83, 83, 83);
      Canvas.Rectangle(0, 0, Width, Height);
    end;
  end
  else
    inherited Paint;
end;

constructor TBCDPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDarkTheme := True;
end;

{ TBCDProgressBar }

procedure TBCDProgressBar.SetFBCThemeManager(AValue: TBCThemeManager);
begin
  if FBCThemeManager = AValue then
    Exit;
  FBCThemeManager := AValue;
end;

constructor TBCDProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := BGRA(102, 163, 226);
end;

{ TBGRADrawer }

procedure TBGRADrawer.AssignFont(Bitmap: TBGRABitmap; Font: TFont);
begin
  Bitmap.FontName := Font.Name;
  Bitmap.FontStyle := Font.Style;
  Bitmap.FontHeight := Font.Height;
  Bitmap.FontQuality := fqSystemClearType;
end;

constructor TBGRADrawer.Create;
begin
  inherited Create;
  randomize;
  FRandSeed := randseed;
end;

function TBGRADrawer.GetMeasures(AMeasureID: integer): integer;
begin
  case AMeasureID of
    TCDEDIT_LEFT_TEXT_SPACING: Result := 6;
    TCDEDIT_RIGHT_TEXT_SPACING: Result := 3;
    TCDEDIT_TOP_TEXT_SPACING: Result := 3;
    TCDEDIT_BOTTOM_TEXT_SPACING: Result := 3;

    TCDCHECKBOX_SQUARE_HALF_HEIGHT: Result :=
        Floor(GetMeasures(TCDCHECKBOX_SQUARE_HEIGHT) / 2);
    TCDCHECKBOX_SQUARE_HEIGHT: Result := DPIAdjustment(13);

    TCDCOMBOBOX_DEFAULT_HEIGHT: Result := 21;

    TCDRADIOBUTTON_CIRCLE_HEIGHT: Result := DPIAdjustment(13);

    TCDSCROLLBAR_BUTTON_WIDTH: Result := 17;
    TCDSCROLLBAR_LEFT_SPACING: Result := 17;
    TCDSCROLLBAR_RIGHT_SPACING: Result := 17;
    TCDSCROLLBAR_LEFT_BUTTON_POS: Result := 0;
    TCDSCROLLBAR_RIGHT_BUTTON_POS: Result := -17;

    TCDTRACKBAR_LEFT_SPACING: Result := 9;
    TCDTRACKBAR_RIGHT_SPACING: Result := 9;
    TCDTRACKBAR_TOP_SPACING: Result := 5;
    TCDTRACKBAR_FRAME_HEIGHT: Result := DPIAdjustment(17);

    TCDLISTVIEW_COLUMN_LEFT_SPACING: Result := 10;
    TCDLISTVIEW_COLUMN_RIGHT_SPACING: Result := 10;
    TCDLISTVIEW_COLUMN_TEXT_LEFT_SPACING: Result := 5;
    TCDLISTVIEW_LINE_TOP_SPACING: Result := 3;
    TCDLISTVIEW_LINE_BOTTOM_SPACING: Result := 3;

    TCDTOOLBAR_ITEM_SPACING: Result := 2;
    TCDTOOLBAR_ITEM_ARROW_WIDTH: Result := 7;
    TCDTOOLBAR_ITEM_BUTTON_DEFAULT_WIDTH: Result := 23;
    TCDTOOLBAR_ITEM_ARROW_RESERVED_WIDTH: Result := 35 - 23;
    TCDTOOLBAR_ITEM_SEPARATOR_DEFAULT_WIDTH: Result := 8;
    TCDTOOLBAR_DEFAULT_HEIGHT: Result := 26;

    TCDCTABCONTROL_CLOSE_TAB_BUTTON_WIDTH: Result := 10;
    TCDCTABCONTROL_CLOSE_TAB_BUTTON_EXTRA_SPACING: Result := 10;
    else
      Result := 0;
  end;
  ;
end;

procedure TBGRADrawer.DrawTickmark(ADest: TFPCustomCanvas; ADestPos: TPoint;
  AState: TCDControlState);
var
  i: integer;
  lSpacing5, lFirstLinesEnd, lSecondLinesEnd: integer;
begin
  if csfPartiallyOn in AState then
    ADest.Pen.FPColor := TColorToFPColor($00AAAAAA)
  else
    ADest.Pen.FPColor := TColorToFPColor($00E5E5E5);
  ADest.Pen.Style := psSolid;

  if Screen.PixelsPerInch <= 125 then
  begin
    // 4 lines going down and to the right
    for i := 0 to 3 do
      ADest.Line(ADestPos.X + 1 + i, ADestPos.Y + 2 + i, ADestPos.X +
        1 + i, ADestPos.Y + 5 + i);
    // Now 5 lines going up and to the right
    for i := 4 to 8 do
      ADest.Line(ADestPos.X + 1 + i, ADestPos.Y + 2 + 6 - i, ADestPos.X +
        1 + i, ADestPos.Y + 5 + 6 - i);
    Exit;
  end;

  lSpacing5 := DPIAdjustment(5);
  lFirstLinesEnd := DPIAdjustment(4) - 1;
  lSecondLinesEnd := DPIAdjustment(9) - 1;

  // 4 lines going down and to the right
  for i := 0 to lFirstLinesEnd do
    ADest.Line(ADestPos.X + 2 + i, ADestPos.Y + 2 + i, ADestPos.X +
      2 + i, ADestPos.Y + lSpacing5 + i);
  // Now 5 lines going up and to the right
  for i := lFirstLinesEnd + 1 to lSecondLinesEnd do
    ADest.Line(ADestPos.X + 2 + i, ADestPos.Y + 2 + lFirstLinesEnd * 2 - i,
      ADestPos.X + 2 + i, ADestPos.Y + 2 + lFirstLinesEnd * 2 + lSpacing5 - i);
end;

function TBGRADrawer.DPIAdjustment(const AValue: integer): integer;
begin
  { Adjustment that works under Windows }
  Result := ScaleY(AValue, 96);
end;

procedure TBGRADrawer.DrawButton(ADest: TFPCustomCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDButtonStateEx);
var
  Bitmap: TBGRABitmap;
  ts: TSize;
begin
  Bitmap := TBGRABitmap.Create(ASize.cx, ASize.cy);

  if csfEnabled in AState then
  begin
    if csfSunken in AState then
    begin
      { Button Down }
      Bitmap.Rectangle(0, 0, ASize.cx, ASize.cy - 1, BGRA(48, 48, 48),
        BGRA(61, 61, 61), dmSet);
      Bitmap.Rectangle(1, 1, ASize.cx - 1, ASize.cy - 2, BGRA(55, 55, 55),
        BGRA(61, 61, 61), dmSet);
      Bitmap.SetHorizLine(0, ASize.cy - 1, ASize.cx - 1, BGRA(115, 115, 115));
    end
    else
    begin
      if csfMouseOver in AState then
      begin
        { Button Hovered }
        Bitmap.GradientFill(0, 0, ASize.cx, ASize.cy, BGRA(132, 132, 132),
          BGRA(109, 109, 109), gtLinear, PointF(0, 0), PointF(0, ASize.cy), dmSet);
        Bitmap.Rectangle(0, 0, ASize.cx, ASize.cy - 1, BGRA(48, 48, 48), dmSet);
        Bitmap.SetHorizLine(1, 1, ASize.cx - 2, BGRA(160, 160, 160));
        Bitmap.SetHorizLine(0, ASize.cy - 1, ASize.cx - 1, BGRA(115, 115, 115));
      end
      else
      begin
        { Button Normal }
        Bitmap.GradientFill(0, 0, ASize.cx, ASize.cy, BGRA(107, 107, 107),
          BGRA(84, 84, 84), gtLinear, PointF(0, 0), PointF(0, ASize.cy), dmSet);
        Bitmap.Rectangle(0, 0, ASize.cx, ASize.cy - 1, BGRA(48, 48, 48), dmSet);
        Bitmap.SetHorizLine(1, 1, ASize.cx - 2, BGRA(130, 130, 130));
        Bitmap.SetHorizLine(0, ASize.cy - 1, ASize.cx - 1, BGRA(115, 115, 115));
        { Button Focused }
        if csfHasFocus in AState then
          Bitmap.Rectangle(1, 2, ASize.cx - 1, ASize.cy - 2, BGRA(80, 111, 172), dmSet);
      end;
    end;
  end
  else
  begin
    { Button Disabled }
    Bitmap.Rectangle(0, 0, ASize.cx, ASize.cy - 1, BGRA(66, 66, 66),
      BGRA(71, 71, 71), dmSet);
    Bitmap.SetHorizLine(0, ASize.cy - 1, ASize.cx - 1, BGRA(94, 94, 94));
  end;

  AssignFont(Bitmap, AStateEx.Font);
  ts := Bitmap.TextSize(AStateEx.Caption);

  if csfEnabled in AState then
  begin
    { Text Enabled }
    Bitmap.TextOut((ASize.cx - ts.cx) div 2, ((ASize.cy - ts.cy) div 2) -
      1, AStateEx.Caption, BGRA(47, 47, 47));
    Bitmap.TextOut((ASize.cx - ts.cx) div 2, (ASize.cy - ts.cy) div 2,
      AStateEx.Caption, BGRA(229, 229, 229));
  end
  else
    { Text Disabled }
    Bitmap.TextOut((ASize.cx - ts.cx) div 2, (ASize.cy - ts.cy) div 2,
      AStateEx.Caption, BGRA(170, 170, 170));

  Bitmap.Draw(TCanvas(ADest), ADestPos.x, ADestPos.y, True);
  Bitmap.Free;
end;

procedure TBGRADrawer.DrawEditBackground(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDEditStateEx);
var
  Bitmap: TBGRABitmap;
begin
  Bitmap := TBGRABitmap.Create(ASize.cx, ASize.cy);

  if csfEnabled in AState then
    if csfHasFocus in AState then
      { Focused }
      Bitmap.Fill(BGRAWhite)
    else
      { Normal }
      Bitmap.Rectangle(1, 1, ASize.cx - 1, ASize.cy - 1, BGRA(41, 41, 41),
        BGRA(58, 58, 58), dmSet)
  else
    { Disabled }
    Bitmap.Rectangle(1, 1, ASize.cx - 1, ASize.cy - 1, BGRA(66, 66, 66),
      BGRA(71, 71, 71), dmSet);

  Bitmap.Draw(TCanvas(ADest), ADestPos.x, ADestPos.y, True);
  Bitmap.Free;
end;

procedure TBGRADrawer.DrawEditFrame(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDEditStateEx);
var
  Bitmap: TBGRABitmap;
begin
  Bitmap := TBGRABitmap.Create(ASize.cx, ASize.cy);

  if csfEnabled in AState then
  begin
    if csfHasFocus in AState then
    begin
      { Focused }
      Bitmap.Rectangle(0, 0, ASize.cx, ASize.cy, BGRA(80, 111, 172), dmSet);
      Bitmap.Rectangle(1, 1, ASize.cx - 1, ASize.cy - 1, BGRA(41, 41, 41), dmSet);
    end
    else
    begin
      { Normal }
      Bitmap.Rectangle(0, 0, ASize.cx, ASize.cy, BGRA(83, 83, 83), dmSet);
      Bitmap.Rectangle(1, 1, ASize.cx - 1, ASize.cy - 1, BGRA(41, 41, 41), dmSet);
      Bitmap.SetHorizLine(1, ASize.cy - 1, ASize.cx - 2, BGRA(105, 105, 105));
    end;
  end
  else
  begin
    { Disabled }
    Bitmap.Rectangle(0, 0, ASize.cx, ASize.cy, BGRA(83, 83, 83), dmSet);
    Bitmap.Rectangle(1, 1, ASize.cx - 1, ASize.cy - 1, BGRA(66, 66, 66), dmSet);
    Bitmap.SetHorizLine(1, ASize.cy - 1, ASize.cx - 2, BGRA(94, 94, 94));
  end;

  Bitmap.Draw(TCanvas(ADest), ADestPos.x, ADestPos.y, False);
  Bitmap.Free;
end;

procedure TBGRADrawer.DrawCaret(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDEditStateEx);
begin
  inherited DrawCaret(ADest, ADestPos, ASize, AState, AStateEx);
end;

procedure TBGRADrawer.DrawEdit(ADest: TCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDEditStateEx);
var
  lVisibleText, lControlText: TCaption;
  lSelLeftPos, lSelLeftPixelPos, lSelLength, lSelRightPos: integer;
  lTextWidth, lLineHeight, lLineTop: integer;
  lControlTextLen: PtrInt;
  lTextLeftSpacing, lTextTopSpacing, lTextBottomSpacing: integer;
  lTextColor: TColor;
  i, lVisibleLinesCount: integer;
begin
  // Background
  DrawEditBackground(ADest, Point(0, 0), ASize, AState, AStateEx);

  // General text configurations which apply to all lines
  // Configure the text color
  if csfEnabled in AState then
    lTextColor := $00E5E5E5
  else
    lTextColor := $00AAAAAA;
  if csfHasFocus in AState then
    lTextColor := clBlack;

  ADest.Brush.Style := bsClear;
  ADest.Font.Assign(AStateEx.Font);
  ADest.Font.Color := lTextColor;
  lTextLeftSpacing := GetMeasures(TCDEDIT_LEFT_TEXT_SPACING);
  //lTextRightSpacing := GetMeasures(TCDEDIT_RIGHT_TEXT_SPACING);
  lTextTopSpacing := GetMeasures(TCDEDIT_TOP_TEXT_SPACING);
  lTextBottomSpacing := GetMeasures(TCDEDIT_BOTTOM_TEXT_SPACING);

  lLineHeight := ADest.TextHeight(cddTestStr) + 2;
  lLineHeight := Min(ASize.cy - lTextBottomSpacing, lLineHeight);

  // Fill this to be used in other parts
  AStateEx.LineHeight := lLineHeight;
  AStateEx.FullyVisibleLinesCount := ASize.cy - lTextTopSpacing - lTextBottomSpacing;
  AStateEx.FullyVisibleLinesCount := AStateEx.FullyVisibleLinesCount div lLineHeight;
  AStateEx.FullyVisibleLinesCount :=
    Min(AStateEx.FullyVisibleLinesCount, AStateEx.Lines.Count);

  // Calculate how many lines to draw
  if AStateEx.Multiline then
    lVisibleLinesCount := AStateEx.FullyVisibleLinesCount + 1
  else
    lVisibleLinesCount := 1;
  lVisibleLinesCount := Min(lVisibleLinesCount, AStateEx.Lines.Count);

  // Now draw each line
  for i := 0 to lVisibleLinesCount - 1 do
  begin
    lControlText := AStateEx.Lines.Strings[AStateEx.VisibleTextStart.Y + i];
    lControlText := VisibleText(lControlText, AStateEx.PasswordChar);
    lControlTextLen := UTF8Length(lControlText);
    lLineTop := lTextTopSpacing + i * lLineHeight;

    // The text
    ADest.Pen.Style := psClear;
    ADest.Brush.Style := bsClear;
    // ToDo: Implement multi-line selection
    if (AStateEx.SelLength = 0) or (AStateEx.SelStart.Y <>
      AStateEx.VisibleTextStart.Y + i) then
    begin
      lVisibleText := UTF8Copy(lControlText, AStateEx.VisibleTextStart.X,
        lControlTextLen);
      ADest.TextOut(lTextLeftSpacing, lLineTop, lVisibleText);
    end
    // Text and Selection
    else
    begin
      lSelLeftPos := AStateEx.SelStart.X;
      if AStateEx.SelLength < 0 then
        lSelLeftPos := lSelLeftPos + AStateEx.SelLength;

      lSelRightPos := AStateEx.SelStart.X;
      if AStateEx.SelLength > 0 then
        lSelRightPos := lSelRightPos + AStateEx.SelLength;

      lSelLength := AStateEx.SelLength;
      if lSelLength < 0 then
        lSelLength := lSelLength * -1;

      // Text left of the selection
      lVisibleText := UTF8Copy(lControlText, AStateEx.VisibleTextStart.X,
        lSelLeftPos - AStateEx.VisibleTextStart.X + 1);
      ADest.TextOut(lTextLeftSpacing, lLineTop, lVisibleText);
      lSelLeftPixelPos := ADest.TextWidth(lVisibleText) + lTextLeftSpacing;

      // The selection background
      lVisibleText := UTF8Copy(lControlText, lSelLeftPos + 1, lSelLength);
      lTextWidth := ADest.TextWidth(lVisibleText);
      ADest.Brush.Color := $00C3C3C3;
      ADest.Brush.Style := bsSolid;
      ADest.Rectangle(Bounds(lSelLeftPixelPos, lLineTop, lTextWidth, lLineHeight));
      ADest.Brush.Style := bsClear;

      // The selection text
      ADest.Font.Color := clWhite;
      ADest.TextOut(lSelLeftPixelPos, lLineTop, lVisibleText);
      lSelLeftPixelPos := lSelLeftPixelPos + lTextWidth;

      // Text right of the selection
      ADest.Brush.Color := clWhite;
      ADest.Font.Color := lTextColor;
      lVisibleText := UTF8Copy(lControlText, lSelLeftPos + lSelLength +
        1, lControlTextLen);
      ADest.TextOut(lSelLeftPixelPos, lLineTop, lVisibleText);
    end;
  end;

  // And the caret
  DrawCaret(ADest, Point(0, 0), ASize, AState, AStateEx);

  // In the end the frame, because it must be on top of everything
  DrawEditFrame(ADest, Point(0, 0), ASize, AState, AStateEx);
end;

procedure TBGRADrawer.DrawPanel(ADest: TCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDPanelStateEx);
var
  Bitmap: TBGRABitmap;
begin
  Bitmap := TBGRABitmap.Create(ASize.cx, ASize.cy);
  Bitmap.Rectangle(0, 0, ASize.cx, ASize.cy, BGRA(40, 40, 40), BGRA(83, 83, 83), dmSet);
  Bitmap.SetHorizLine(1, 1, ASize.cx - 2, BGRA(106, 106, 106));
  Bitmap.Draw(TCanvas(ADest), ASize.cx, ASize.cy, True);
  Bitmap.Free;
end;

procedure TBGRADrawer.DrawStaticText(ADest: TCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDControlStateEx);
var
  lColor: TColor;
begin
  // Background
  lColor := $00535353; //AStateEx.ParentRGBColor;
  ADest.Brush.Color := lColor;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psClear;
  ADest.FillRect(0, 0, ASize.cx, ASize.cy);

  // Now the text
  ADest.Brush.Style := bsClear;
  ADest.Font.Assign(AStateEx.Font);
  if csfEnabled in AState then
    ADest.Font.Color := $00E5E5E5
  else
    ADest.Font.Color := $00AAAAAA;
  ADest.TextOut(0, 0, AStateEx.Caption);
end;

procedure TBGRADrawer.DrawProgressBar(ADest: TCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDProgressBarStateEx);

  function ApplyLightness(c: TBGRAPixel; lightness: word): TBGRAPixel;
  begin
    Result := GammaCompression(SetLightness(GammaExpansion(c), lightness));
  end;

  procedure DrawBar(Bitmap: TBGRABitmap; bounds: TRect);
  var
    lCol: TBGRAPixel;
  begin
    lCol := AStateEx.RGBColor;

    DoubleGradientAlphaFill(Bitmap, bounds,
      ApplyLightness(lCol, 37000), ApplyLightness(lCol, 29000),
      ApplyLightness(lCol, 26000), ApplyLightness(lCol, 18000),
      gdVertical, gdVertical, gdVertical, 0.53);

    InflateRect(bounds, -1, -1);

    DoubleGradientAlphaFill(Bitmap, bounds,
      ApplyLightness(lCol, 28000), ApplyLightness(lCol, 22000),
      ApplyLightness(lCol, 19000), ApplyLightness(lCol, 11000),
      gdVertical, gdVertical, gdVertical, 0.53);
  end;

var
  content: TRect;
  xpos, y, tx, ty: integer;
  grayValue: integer;
  Bitmap: TBGRABitmap;
begin
  Bitmap := TBGRABitmap.Create(ASize.cx, ASize.cy);

  tx := ASize.cx;
  ty := ASize.cy;

  Bitmap.Fill(BGRA(83, 83, 83));
  Bitmap.Rectangle(0, 0, tx, ty, BGRA(255, 255, 255, 6), dmDrawWithTransparency);
  if (tx > 2) and (ty > 2) then
    Bitmap.Rectangle(1, 1, tx - 1, ty - 1, BGRA(29, 29, 29), dmSet);

  if (tx > 4) and (ty > 4) then
  begin
    content := Rect(2, 2, tx - 2, ty - 2);
    randseed := FRandSeed;
    for y := content.Top to content.Bottom - 1 do
    begin
      if y = content.Top then
        grayValue := 33
      else
      if y = content.Top + 1 then
        grayValue := 43
      else
        grayValue := 47 + random(50 - 47 + 1);
      Bitmap.SetHorizLine(content.Left, y, content.Right - 1, BGRA(
        grayValue, grayValue, grayValue));
    end;
    if tx >= 6 then
      Bitmap.DrawVertLine(content.Right - 1, content.Top, content.Bottom - 1,
        BGRA(0, 0, 0, 32));
    xpos := round(AStateEx.PercentPosition * (content.right - content.left)) +
      content.left;
    if xpos > content.left then
    begin
      DrawBar(Bitmap, rect(content.left, content.top, xpos, content.bottom));
      if xpos < content.right then
      begin
        Bitmap.SetPixel(xpos, content.top, BGRA(62, 62, 62));
        Bitmap.SetVertLine(xpos, content.top + 1, content.bottom -
          1, BGRA(40, 40, 40));
      end;
    end;
  end;
  Bitmap.Draw(TCanvas(ADest), 0, 0, True);
  Bitmap.Free;
end;

procedure TBGRADrawer.DrawCheckBoxSquare(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDControlStateEx);
var
  lHalf, lSquareHalf, lSquareHeight: integer;
  Bitmap: TBGRABitmap;
  r: TRect;
begin
  Bitmap := TBGRABitmap.Create(ASize.cx, ASize.cy);
  lHalf := ASize.cy div 2;
  lSquareHalf := GetMeasures(TCDCHECKBOX_SQUARE_HALF_HEIGHT);
  lSquareHeight := GetMeasures(TCDCHECKBOX_SQUARE_HEIGHT);
  r := Bounds(1, lHalf - lSquareHalf, lSquareHeight, lSquareHeight);

  if csfEnabled in AState then
  begin
    if csfSunken in AState then
    begin
      { Down }
      Bitmap.Rectangle(r.Left, r.Top, r.Right, r.Bottom - 1, BGRA(48, 48, 48),
        BGRA(61, 61, 61), dmSet);
      Bitmap.Rectangle(r.Left + 1, r.Top + 1, r.Right - 1, r.Bottom -
        2, BGRA(55, 55, 55),
        BGRA(61, 61, 61), dmSet);
      Bitmap.SetHorizLine(r.Left, r.Bottom - 1, r.Right - 1, BGRA(115, 115, 115));
    end
    else
    begin
      if csfMouseOver in AState then
      begin
        { Hovered }
        Bitmap.GradientFill(r.Left, r.Top, r.Right, r.Bottom, BGRA(132, 132, 132),
          BGRA(109, 109, 109), gtLinear, PointF(0, 0), PointF(0, ASize.cy), dmSet);
        Bitmap.Rectangle(r.Left, r.Top, r.Right, r.Bottom - 1, BGRA(48, 48, 48), dmSet);
        Bitmap.SetHorizLine(r.Left + 1, r.Top + 1, r.Right - 2, BGRA(160, 160, 160));
        Bitmap.SetHorizLine(r.Left, r.Bottom - 1, r.Right - 1, BGRA(115, 115, 115));
      end
      else
      begin
        { Normal }
        Bitmap.GradientFill(r.Left, r.Top, r.Right, r.Bottom, BGRA(107, 107, 107),
          BGRA(84, 84, 84), gtLinear, PointF(0, 0), PointF(0, r.Bottom), dmSet);
        Bitmap.Rectangle(r.Left, r.Top, r.Right, r.Bottom - 1, BGRA(48, 48, 48), dmSet);
        Bitmap.SetHorizLine(r.Left + 1, r.Top + 1, r.Right - 2, BGRA(130, 130, 130));
        Bitmap.SetHorizLine(r.Left, r.Bottom - 1, r.Right - 1, BGRA(115, 115, 115));
      end;
    end;
  end
  else
  begin
    { Disabled }
    Bitmap.Rectangle(r.Left, r.Top, r.Right, r.Bottom - 1, BGRA(66, 66, 66),
      BGRA(71, 71, 71), dmSet);
    Bitmap.SetHorizLine(r.Left, r.Bottom - 1, r.Right - 1, BGRA(94, 94, 94));
  end;

  Bitmap.Draw(TCanvas(ADest), ADestPos.x, ADestPos.y, False);
  Bitmap.Free;
end;

procedure TBGRADrawer.DrawCheckBox(ADest: TCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDControlStateEx);
var
  lColor: TColor;
  lSquareHeight, lValue3: integer;
  lTextHeight, lTextY: integer;
begin
  lSquareHeight := GetMeasures(TCDCHECKBOX_SQUARE_HEIGHT);
  lValue3 := DPIAdjustment(3);

  // Background
  lColor := $00535353; //AStateEx.ParentRGBColor;
  ADest.Brush.Color := lColor;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psClear;
  ADest.FillRect(0, 0, ASize.cx, ASize.cy);

  // The checkbox item itself
  DrawCheckBoxSquare(ADest, Point(0, 0), ASize, AState, AStateEx);

  // The Tickmark
  if (csfOn in AState) or (csfPartiallyOn in AState) then
    DrawTickmark(ADest, Point(lValue3, ASize.cy div 2 -
      GetMeasures(TCDCHECKBOX_SQUARE_HALF_HEIGHT) + lValue3), AState);

  // The text selection
  //if csfHasFocus in AState then
  //  DrawFocusRect(ADest, Point(lSquareHeight+4, 0),
  //    Size(ASize.cx-lSquareHeight-4, ASize.cy));

  // Now the text
  ADest.Brush.Style := bsClear;
  ADest.Pen.Style := psClear;
  ADest.Font.Assign(AStateEx.Font);
  if csfEnabled in AState then
    ADest.Font.Color := $00E5E5E5
  else
    ADest.Font.Color := $00AAAAAA;
  lTextHeight := ADest.TextHeight(cddTestStr);
  // put the text in the center
  if lSquareHeight > lTextHeight then
    lTextY := (lSquareHeight - ADest.TextHeight(cddTestStr)) div 2
  else
    lTextY := 0;
  lTextY := Max(0, lTextY - 1);

  ADest.TextOut(lSquareHeight + 5, lTextY, AStateEx.Caption);
end;

procedure TBGRADrawer.DrawRadioButtonCircle(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDControlStateEx);
var
  lCircleHeight, lCircleMid: integer;
  Bitmap: TBGRABitmap;
  bColor: TBGRAPixel;
begin
  lCircleHeight := GetMeasures(TCDRADIOBUTTON_CIRCLE_HEIGHT);
  lCircleMid := lCircleHeight div 2;

  Bitmap := TBGRABitmap.Create(lCircleHeight, lCircleHeight);

  if csfEnabled in AState then
  begin
    if csfSunken in AState then
      bColor := BGRA(61, 61, 61)
    else
    begin
      if csfMouseOver in AState then
        bColor := BGRA(109, 109, 109)
      else
        bColor := BGRA(84, 84, 84);
    end;
  end
  else
    bColor := BGRA(71, 71, 71);

  if csfOn in AState then
  begin
    if csfEnabled in AState then
      Bitmap.EllipseAntialias(lCircleMid, lCircleMid, lCircleMid - 1,
        lCircleMid - 1, BGRA(48, 48, 48), 1, BGRA(229, 229, 229))
    else
      Bitmap.EllipseAntialias(lCircleMid, lCircleMid, lCircleMid - 1,
        lCircleMid - 1, BGRA(48, 48, 48), 1, BGRA(170, 170, 170));
  end
  else
  begin
    Bitmap.EllipseAntialias(lCircleMid, lCircleMid, lCircleMid - 1,
      lCircleMid - 1, BGRA(48, 48, 48), 1, bColor);
  end;

  Bitmap.Draw(TCanvas(ADest), 0, (ADest.Font.GetTextHeight('a') - lCircleHeight) div
    2, False);
end;

procedure TBGRADrawer.DrawRadioButton(ADest: TCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDControlStateEx);
var
  lColor: TColor;
  lCircleHeight: integer;
  lTextHeight, lTextY: integer;
begin
  lCircleHeight := GetMeasures(TCDRADIOBUTTON_CIRCLE_HEIGHT);

  // Background
  lColor := $00535353; //AStateEx.ParentRGBColor;
  ADest.Brush.Color := lColor;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psClear;
  ADest.FillRect(0, 0, ASize.cx, ASize.cy);

  // The radiobutton circle itself
  DrawRadioButtonCircle(ADest, Point(0, 0), ASize, AState, AStateEx);

  // The text selection
  //if csfHasFocus in AState then
  //  DrawFocusRect(ADest, Point(lCircleHeight+3, 0),
  //    Size(ASize.cx-lCircleHeight-3, ASize.cy));

  // Now the text
  ADest.Brush.Style := bsClear;
  ADest.Font.Assign(AStateEx.Font);
  if csfEnabled in AState then
    ADest.Font.Color := $00E5E5E5
  else
    ADest.Font.Color := $00AAAAAA;
  lTextHeight := ADest.TextHeight(cddTestStr);
  // put the text in the center
  if lCircleHeight > lTextHeight then
    lTextY := (lCircleHeight - ADest.TextHeight(cddTestStr)) div 2
  else
    lTextY := 0;
  lTextY := Max(0, lTextY - 1);
  ADest.TextOut(lCircleHeight + 5, lTextY, AStateEx.Caption);
end;

initialization
  RegisterDrawer(TBGRADrawer.Create, dsCommon);

end.
