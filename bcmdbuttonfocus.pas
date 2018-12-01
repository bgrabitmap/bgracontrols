{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCMDButtonFocus;

{$I bgracontrols.inc}

// Set this to show number of repaint in each MDBUTTON
{ $DEFINE MDBUTTON_DEBUG}

// Set this to animate only a MDBUTTON at a time
{ $DEFINE MDBUTTON_ANIMATEONLYONE}

interface

uses
  Classes, SysUtils, Types, {$IFDEF FPC}LCLType, LResources, LMessages,{$ENDIF}
  Forms, Controls, Graphics, Dialogs,
  {$IFNDEF FPC}Windows, Messages, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCBaseCtrls, BGRABitmap, BGRABitmapTypes, ExtCtrls, Math, BGRABlend, BCMDButton;

type

  { TCustomBCMDButtonFocus }

  TCustomBCMDButtonFocus = class(TBGRACustomCtrl)
  private
    FChecked: boolean;
    FKind: TBCMDButtonKind;
    {$IFDEF INDEBUG}
    FCount: integer;
    {$ENDIF}
    FRounding: integer;
    FTextAutoSize: boolean;
    FTextProportional: boolean;
    FTextProportionalRatio: single;
    FTimer: TTimer;
    FPercent: double;
    FCircleSize: double;
    FCX, FCY: integer;
    FAlphaPercent: double;
    FAlignment: TAlignment;
    FAnimation: boolean;
    FState: TBCMDButtonState;
    FStyleActive: TBCMDButtonStyle;
    FStyleDisabled: TBCMDButtonStyle;
    FStyleHover: TBCMDButtonStyle;
    FStyleNormal: TBCMDButtonStyle;
    FTextLayout: TTextLayout;
    procedure OnChangeStyle(Sender: TObject);
    procedure SetFAlignment(AValue: TAlignment);
    procedure SetFAnimation(AValue: boolean);
    procedure SetFChecked(AValue: boolean);
    procedure SetFKind(AValue: TBCMDButtonKind);
    procedure SetFStyleActive(AValue: TBCMDButtonStyle);
    procedure SetFStyleDisabled(AValue: TBCMDButtonStyle);
    procedure SetFStyleHover(AValue: TBCMDButtonStyle);
    procedure SetFStyleNormal(AValue: TBCMDButtonStyle);
    procedure SetFTextAutoSize(AValue: boolean);
    procedure SetFTextLayout(AValue: TTextLayout);
    procedure SetFTextProportional(AValue: boolean);
    procedure SetFTextProportionalRatio(AValue: single);
  protected
    // START / MDBUTTONFOCUS ONLY
    procedure WMSetFocus(var Message: {$IFDEF FPC}TLMSetFocus{$ELSE}TWMKillFocus{$ENDIF}); message {$IFDEF FPC}LM_SETFOCUS{$ELSE}WM_SETFOCUS{$ENDIF};
    procedure WMKillFocus(var Message: {$IFDEF FPC}TLMKillFocus{$ELSE}TWMKillFocus{$ENDIF}); message {$IFDEF FPC}LM_KILLFOCUS{$ELSE}WM_KILLFOCUS{$ENDIF};
    procedure UpdateFocus(AFocused: boolean);
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
    // END / MDBUTTONFOCUS ONLY
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
    {%H-}WithThemeSpace: boolean); override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure RealSetText(const Value: TCaption); override;
    procedure OnTimer(Sender: TObject);
    procedure OnStartTimer(Sender: TObject);
    procedure OnStopTimer(Sender: TObject);
    function easeInOutQuad(t: double): double;
    function easeOutQuad(t: double): double;
    procedure UncheckOthers;
    class function GetControlClassDefaultSize: TSize;override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SelectAll;
    procedure UnselectAll;
    procedure InvertSelection;
    function GetSelected: TStringList;
  published
    property Animation: boolean read FAnimation write SetFAnimation default False;
    property Alignment: TAlignment read FAlignment write SetFAlignment default taCenter;
    property TextLayout: TTextLayout
      read FTextLayout write SetFTextLayout default tlCenter;
    property StyleNormal: TBCMDButtonStyle read FStyleNormal write SetFStyleNormal;
    property StyleHover: TBCMDButtonStyle read FStyleHover write SetFStyleHover;
    property StyleActive: TBCMDButtonStyle read FStyleActive write SetFStyleActive;
    property StyleDisabled: TBCMDButtonStyle read FStyleDisabled write SetFStyleDisabled;
    property Checked: boolean read FChecked write SetFChecked default False;
    property Kind: TBCMDButtonKind read FKind write SetFKind default mdbkNormal;
    // If text size is used to measure buttons
    // Disable it if you use the buttons in a grid, for example
    property TextAutoSize: boolean read FTextAutoSize write SetFTextAutoSize;
    // Enable it if you want that text size grows with height
    property TextProportional: boolean read FTextProportional write SetFTextProportional;
    // Each character font height proportional to height of control
    // Set it in conjunction with TextProportional, values recommended between 0...1
    property TextProportionalRatio: single read FTextProportionalRatio
      write SetFTextProportionalRatio;
  end;

  TBCMDButtonFocus = class(TCustomBCMDButtonFocus)
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    {$IFDEF FPC} //#
    property OnChangeBounds;
    {$ENDIF}
    //property Cancel;
    property Caption;
    property Color;
    property Constraints;
    //property Default;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBidiMode;
    //property ModalResult;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    //property OnEnter;
    //property OnExit;
    //property OnKeyDown;
    //property OnKeyPress;
    //property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    //property OnUTF8KeyPress;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    //property TabOrder;
    //property TabStop;
    property Visible;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

{$IFDEF MDBUTTON_ANIMATEONLYONE}
var
  MDAnimating: TCustomMDButtonFocus;

{$ENDIF}

{$IFDEF FPC}
procedure Register;
begin
  RegisterComponents('BGRA Button Controls', [TBCMDButtonFocus]);
end;
{$ENDIF}

{ TCustomBCMDButtonFocus }

procedure TCustomBCMDButtonFocus.SetFStyleActive(AValue: TBCMDButtonStyle);
begin
  if FStyleActive = AValue then
    Exit;
  FStyleActive := AValue;
end;

procedure TCustomBCMDButtonFocus.SetFAlignment(AValue: TAlignment);
begin
  if FAlignment = AValue then
    Exit;
  FAlignment := AValue;
  Invalidate;
end;

procedure TCustomBCMDButtonFocus.SetFAnimation(AValue: boolean);
begin
  if FAnimation = AValue then
    Exit;
  FAnimation := AValue;
  Invalidate;
end;

procedure TCustomBCMDButtonFocus.SetFChecked(AValue: boolean);
begin
  if FChecked = AValue then
    Exit;
  FChecked := AValue;
  if FChecked and (FKind in [mdbkToggleGroup, mdbkRadioButton, mdbkTab]) then
    UncheckOthers;
  Invalidate;
end;

procedure TCustomBCMDButtonFocus.SetFKind(AValue: TBCMDButtonKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  Invalidate;
end;

procedure TCustomBCMDButtonFocus.OnChangeStyle(Sender: TObject);
begin
  Invalidate;
end;

procedure TCustomBCMDButtonFocus.SetFStyleDisabled(AValue: TBCMDButtonStyle);
begin
  if FStyleDisabled = AValue then
    Exit;
  FStyleDisabled := AValue;
end;

procedure TCustomBCMDButtonFocus.SetFStyleHover(AValue: TBCMDButtonStyle);
begin
  if FStyleHover = AValue then
    Exit;
  FStyleHover := AValue;
end;

procedure TCustomBCMDButtonFocus.SetFStyleNormal(AValue: TBCMDButtonStyle);
begin
  if FStyleNormal = AValue then
    Exit;
  FStyleNormal := AValue;
end;

procedure TCustomBCMDButtonFocus.SetFTextAutoSize(AValue: boolean);
begin
  if FTextAutoSize = AValue then
    Exit;
  FTextAutoSize := AValue;
end;

procedure TCustomBCMDButtonFocus.SetFTextLayout(AValue: TTextLayout);
begin
  if FTextLayout = AValue then
    Exit;
  FTextLayout := AValue;
  Invalidate;
end;

procedure TCustomBCMDButtonFocus.SetFTextProportional(AValue: boolean);
begin
  if FTextProportional = AValue then
    Exit;
  FTextProportional := AValue;
  Invalidate;
end;

procedure TCustomBCMDButtonFocus.SetFTextProportionalRatio(AValue: single);
begin
  if FTextProportionalRatio = AValue then
    Exit;
  FTextProportionalRatio := AValue;
  Invalidate;
end;

procedure TCustomBCMDButtonFocus.WMSetFocus(var Message:  {$IFDEF FPC}TLMSetFocus{$ELSE}TWMKillFocus{$ENDIF});
begin
  inherited;

  UpdateFocus(True);
end;

procedure TCustomBCMDButtonFocus.WMKillFocus(var Message: {$IFDEF FPC}TLMKillFocus{$ELSE}TWMKillFocus{$ENDIF});
begin
  inherited;

  if Message.FocusedWnd <> Handle then
    UpdateFocus(False);
end;

procedure TCustomBCMDButtonFocus.UpdateFocus(AFocused: boolean);
var
  lForm: TCustomForm;
begin
  lForm := GetParentForm(Self);
  if lForm = nil then
    exit;

{$IFDEF FPC}//#
  if AFocused then
    ActiveDefaultControlChanged(lForm.ActiveControl)
  else
    ActiveDefaultControlChanged(nil);
{$ENDIF}
  Invalidate;
end;

procedure TCustomBCMDButtonFocus.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if (Key = VK_SPACE) or (Key = VK_RETURN) then
    MouseDown(mbLeft, [], Width div 2, Height div 2);
end;

procedure TCustomBCMDButtonFocus.KeyUp(var Key: word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) or (Key = VK_RETURN) then
  begin
    MouseLeave;
    Self.Click;
  end;

  inherited KeyUp(Key, Shift);
end;

procedure TCustomBCMDButtonFocus.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
var
  bmp: TBGRABitmap;
  s: TSize;
begin
  bmp := TBGRABitmap.Create;
  bmp.FontName := Font.Name;
  if FTextProportional then
    bmp.FontHeight := Round(Height * FTextProportionalRatio)
  else
    bmp.FontHeight := 0;
  bmp.FontAntialias := True;
  bmp.FontQuality := fqSystemClearType;
  bmp.FontStyle := Font.Style;
  s := bmp.TextSize(Caption);
  if FTextAutoSize then
  begin
    PreferredWidth := s.Width + 26 {$IFDEF FPC}+ BorderSpacing.InnerBorder{$ENDIF};
    PreferredHeight := s.Height + 10 {$IFDEF FPC}+ BorderSpacing.InnerBorder{$ENDIF};
  end
  else
  begin
    {$IFDEF FPC}//#
    PreferredWidth := BorderSpacing.InnerBorder;
    PreferredHeight := BorderSpacing.InnerBorder;
    {$ENDIF}
  end;
  bmp.Free;
end;

procedure TCustomBCMDButtonFocus.Paint;
var
  bmp: TBGRABitmap;
  iTemp: integer;
  alpha: byte;
  tempState: TBCMDButtonState;
  tempText: string;
  tempRounding: integer;
  tempColor, hoverColor: TBGRAPixel;
begin
  bmp := TBGRABitmap.Create(Width, Height);
  bmp.FontName := Font.Name;
  if FTextProportional then
    bmp.FontHeight := Round(Height * FTextProportionalRatio)
  else
    bmp.FontHeight := 0;
  bmp.FontAntialias := True;
  bmp.FontQuality := fqSystemClearType;
  bmp.FontStyle := Font.Style;
  tempState := FState;

  if Kind = mdbkTab then
    tempRounding := 0
  else
    tempRounding := FRounding;

  if FChecked then
    tempState := mdbsActive
  else
    tempState := FState;

  // START / MDBUTTONFOCUS ONLY
  if Focused and (tempState = mdbsNormal) then
    tempState := mdbsHover;
  // END / MDBUTTONFOCUS ONLY

  tempText := Caption;

  case FKind of
    mdbkCheckBox:
    begin
      if Length(Caption) > 0 then
        tempText := ' ' + Caption;
      if FChecked then
        tempText := BCMDBUTTONBALLOTBOXWITHCHECK + tempText
      else
        tempText := BCMDBUTTONBALLOTBOX + tempText;
    end;
    mdbkRadioButton:
    begin
      if Length(Caption) > 0 then
        tempText := ' ' + Caption;
      if FChecked then
        tempText := BCMDBUTTONRADIOBUTTON + tempText
      else
        tempText := BCMDBUTTONRADIOBUTTONCIRCLE + tempText;
    end;
  end;

  // Enabled
  if Enabled then
  begin
    if not FTimer.Enabled then
    begin
      case tempState of
        mdbsNormal:
        begin
          bmp.RoundRect(0, 0, Width, Height, tempRounding, tempRounding,
            FStyleNormal.Color,
            FStyleNormal.Color);
          {$IFDEF FPC}
          bmp.TextRect(Rect(BorderSpacing.InnerBorder, BorderSpacing.InnerBorder,
            Width - BorderSpacing.InnerBorder, Height - BorderSpacing.InnerBorder),
            tempText, Alignment,
            TextLayout, FStyleNormal.TextColor);
          {$ELSE}
          bmp.TextRect(Rect(0, 0, Width, Height), tempText, Alignment, TextLayout, FStyleNormal.TextColor);
          {$ENDIF}
        end;
        mdbsHover:
        begin
          bmp.RoundRect(0, 0, Width, Height, tempRounding, tempRounding,
            FStyleHover.Color, FStyleHover.Color);
          {$IFDEF FPC}
          bmp.TextRect(Rect(BorderSpacing.InnerBorder, BorderSpacing.InnerBorder,
            Width - BorderSpacing.InnerBorder, Height - BorderSpacing.InnerBorder),
            tempText, Alignment,
            TextLayout, FStyleHover.TextColor);
          {$ELSE}
          bmp.TextRect(Rect(0, 0, Width, Height), tempText, Alignment, TextLayout, FStyleHover.TextColor);
          {$ENDIF}
        end;
        mdbsActive:
        begin
          if not FAnimation then
          begin
            if FKind in [mdbkNormal] then
              bmp.RoundRect(0, 0, Width, Height, tempRounding,
                tempRounding, FStyleActive.Color,
                FStyleActive.Color)
            else
              bmp.RoundRect(0, 0, Width, Height, tempRounding,
                tempRounding, FStyleHover.Color,
                FStyleHover.Color);
          end
          else
            bmp.RoundRect(0, 0, Width, Height, tempRounding, tempRounding,
              FStyleHover.Color,
              FStyleHover.Color);
          {$IFDEF FPC}
          bmp.TextRect(Rect(BorderSpacing.InnerBorder, BorderSpacing.InnerBorder,
            Width - BorderSpacing.InnerBorder, Height - BorderSpacing.InnerBorder),
            tempText, Alignment,
            TextLayout, FStyleActive.TextColor);
          {$ELSE}
          bmp.TextRect(Rect(0, 0, Width, Height), tempText, Alignment, TextLayout, FStyleActive.TextColor);
          {$ENDIF}
        end;
      end;
    end
    else
    begin
      iTemp := round(FCircleSize * easeOutQuad(FPercent));
      alpha := round(easeInOutQuad(FAlphaPercent) * 255);
      case tempState of
        mdbsNormal:
        begin
          bmp.RoundRect(0, 0, Width, Height, tempRounding, tempRounding,
            FStyleNormal.Color,
            FStyleNormal.Color);
          if FPercent < 1 then
            tempColor := FStyleHover.Color
          else
          begin
            tempColor := FStyleNormal.Color;
            hoverColor := ColorToBGRA(FStyleHover.Color, alpha);
            PutPixels(@tempColor, @hoverColor, 1, dmDrawWithTransparency, 255);
          end;
          bmp.FillEllipseAntialias(FCX, FCY, iTemp,
            iTemp, tempColor);
          {$IFDEF FPC}
          bmp.TextRect(Rect(BorderSpacing.InnerBorder, BorderSpacing.InnerBorder,
            Width - BorderSpacing.InnerBorder, Height - BorderSpacing.InnerBorder),
            tempText, Alignment,
            TextLayout, FStyleNormal.TextColor);
          {$ELSE}
          bmp.TextRect(Rect(0, 0, Width, Height), tempText, Alignment, TextLayout, FStyleNormal.TextColor);
          {$ENDIF}
        end;
        mdbsHover, mdbsActive:
        begin
          bmp.RoundRect(0, 0, Width, Height, tempRounding, tempRounding,
            FStyleHover.Color, FStyleHover.Color);
          if FPercent < 1 then
            tempColor := FStyleActive.Color
          else
          begin
            tempColor := FStyleHover.Color;
            hoverColor := ColorToBGRA(FStyleActive.Color, alpha);
            PutPixels(@tempColor, @hoverColor, 1, dmDrawWithTransparency, 255);
          end;
          bmp.FillEllipseAntialias(FCX, FCY, iTemp,
            iTemp, tempColor);
          {$IFDEF FPC}
          bmp.TextRect(Rect(BorderSpacing.InnerBorder, BorderSpacing.InnerBorder,
            Width - BorderSpacing.InnerBorder, Height - BorderSpacing.InnerBorder),
            tempText, Alignment,
            TextLayout, FStyleHover.TextColor);
          {$ELSE}
          bmp.TextRect(Rect(0, 0, Width, Height), tempText, Alignment, TextLayout, FStyleHover.TextColor);
          {$ENDIF}
        end;
      end;
    end;
  end
  // Disabled
  else
  begin
    if FChecked then
    begin
      bmp.RoundRect(0, 0, Width, Height, tempRounding, tempRounding,
        FStyleHover.Color, FStyleHover.Color);
    end
    else
      bmp.RoundRect(0, 0, Width, Height, tempRounding, tempRounding,
        FStyleDisabled.Color, FStyleDisabled.Color);
    {$IFDEF FPC}
    bmp.TextRect(Rect(BorderSpacing.InnerBorder, BorderSpacing.InnerBorder,
      Width - BorderSpacing.InnerBorder, Height - BorderSpacing.InnerBorder),
      tempText, Alignment,
      TextLayout, FStyleDisabled.TextColor);
    {$ELSE}
    bmp.TextRect(Rect(0, 0, Width, Height), tempText, Alignment, TextLayout, FStyleDisabled.TextColor);
    {$ENDIF}
  end;

  // Tab
  if Kind = mdbkTab then
  begin
    if FTimer.Enabled then
    begin
      iTemp := round((bmp.Width div 2) * easeInOutQuad(FPercent));
      bmp.Rectangle((bmp.Width div 2) - iTemp, bmp.Height - 2,
        (bmp.Width div 2) + iTemp, bmp.Height, $00BB513F, dmSet);
    end
    else
    begin
      if FChecked then
        bmp.Rectangle(0, bmp.Height - 2, bmp.Width, bmp.Height, $00BB513F, dmSet);
    end;
  end;

  {$IFDEF MDBUTTON_DEBUG}
  bmp.FontHeight := 10;
  bmp.TextOut(0, 0, FCount.ToString, BGRA(255, 0, 0, 255));
  FCount += 1;
  {$ENDIF}
  bmp.Draw(Canvas, 0, 0, False);
  bmp.Free;
  inherited Paint;
end;

procedure TCustomBCMDButtonFocus.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FState := mdbsActive;
  if FAnimation and BCMDBUTTONANIMATION then
  begin
    FCircleSize := max(round(Width / 1.5) + abs((Width div 2) - X),
      round(Height / 1.5) + abs((Height div 2) - Y));
    FCX := X;
    FCY := Y;
    FTimer.Enabled := False;
    FTimer.Enabled := True;
    {$IFDEF MDBUTTON_ANIMATEONLYONE}
    MDAnimating := Self;
    {$ENDIF}
  end;
  if FKind in [mdbkToggle, mdbkToggleGroup, mdbkCheckBox, mdbkRadioButton, mdbkTab] then
  begin
    FChecked := not FChecked;
    if FKind in [mdbkToggleGroup, mdbkRadioButton, mdbkTab] then
    begin
      FChecked := True;
      UncheckOthers;
    end;
  end;
  Invalidate;
end;

procedure TCustomBCMDButtonFocus.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (x > 0) and (x < Width) and (y > 0) and (y < Height) and (FState = mdbsActive) then
    FState := mdbsHover
  else
    FState := mdbsNormal;
  Invalidate;
end;

procedure TCustomBCMDButtonFocus.MouseEnter;
begin
  inherited MouseEnter;
  FState := mdbsHover;
  Invalidate;
end;

procedure TCustomBCMDButtonFocus.MouseLeave;
begin
  inherited MouseLeave;
  FState := mdbsNormal;
  Invalidate;
end;

procedure TCustomBCMDButtonFocus.RealSetText(const Value: TCaption);
begin
  inherited RealSetText(Value);
  InvalidatePreferredSize;
  Invalidate;
end;

procedure TCustomBCMDButtonFocus.OnTimer(Sender: TObject);
begin
  {$IFDEF MDBUTTON_ANIMATEONLYONE}
  if MDAnimating = Self then
  begin
  {$ENDIF}
    FPercent := FPercent + BCMDBUTTONANIMATIONSPEED;
    if FPercent < 0 then
      FPercent := 0
    else if FPercent > 1 then
      FPercent := 1;

    if FPercent = 1 then
    begin
      FAlphaPercent := FAlphaPercent - BCMDBUTTONANIMATIONSPEED;
      if FAlphaPercent < 0 then
        FAlphaPercent := 0
      else if FAlphaPercent > 1 then
        FAlphaPercent := 1;
    end;
  {$IFDEF MDBUTTON_ANIMATEONLYONE}
  end
  else
    FTimer.Enabled := False;
  {$ENDIF}

  Invalidate;
  if (FPercent >= 1) and (FAlphaPercent <= 0) then
    FTimer.Enabled := False;
end;

procedure TCustomBCMDButtonFocus.OnStartTimer(Sender: TObject);
begin
  FPercent := 0;
  FAlphaPercent := 1;
end;

procedure TCustomBCMDButtonFocus.OnStopTimer(Sender: TObject);
begin

end;

function TCustomBCMDButtonFocus.easeInOutQuad(t: double): double;
begin
  if t < 0.5 then
    Result := 2 * t * t
  else
    Result := -1 + (4 - 2 * t) * t;
end;

function TCustomBCMDButtonFocus.easeOutQuad(t: double): double;
begin
  Result := t * (2 - t);
end;

procedure TCustomBCMDButtonFocus.UncheckOthers;
var
  i: integer;
  control: TWinControl;
begin
  if Parent is TWinControl then
  begin
    control := TWinControl(Parent);
    for i := 0 to control.ControlCount - 1 do
      if (control.Controls[i] <> Self) and (control.Controls[i] is
        TCustomBCMDButtonFocus) then
        if (TCustomBCMDButtonFocus(control.Controls[i]).Kind in
          [mdbkToggleGroup, mdbkRadioButton, mdbkTab]) then
          TCustomBCMDButtonFocus(control.Controls[i]).Checked := False;
  end;
end;

class function TCustomBCMDButtonFocus.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 75;
  Result.CY := 25;
end;

constructor TCustomBCMDButtonFocus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // START / MDBUTTONFOCUS ONLY
  TabStop := True;
  ControlStyle := ControlStyle + [csAcceptsControls];
  DoubleBuffered := True;
  // END / MDBUTTONFOCUS ONLY
  {$IFDEF INDEBUG}
  FCount := 0;
  {$ENDIF}
  // State
  FState := mdbsNormal;
  FChecked := False;
  FKind := mdbkNormal;
  // Text
  FTextAutoSize := True;
  FAlignment := taCenter;
  FTextLayout := tlCenter;
  FTextProportional := False;
  FTextProportionalRatio := 0.5;
  // Style
  FRounding := 6;
  FStyleNormal := TBCMDButtonStyle.Create;
  FStyleNormal.OnChange := OnChangeStyle;
  FStyleHover := TBCMDButtonStyle.Create;
  FStyleHover.OnChange := OnChangeStyle;
  FStyleActive := TBCMDButtonStyle.Create;
  FStyleActive.OnChange := OnChangeStyle;
  FStyleDisabled := TBCMDButtonStyle.Create;
  FStyleDisabled.OnChange := OnChangeStyle;
  // Default Style
  FStyleHover.Color := RGBToColor(220, 220, 220);
  FStyleActive.Color := RGBToColor(198, 198, 198);
  FStyleDisabled.TextColor := RGBToColor(163, 163, 163);
  // Animation
  FAnimation := False;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := BCMDBUTTONTIMERSPEED;
  FTimer.OnTimer := OnTimer;
  {$IFDEF FPC}//#
  FTimer.OnStartTimer := OnStartTimer;
  FTimer.OnStopTimer := OnStopTimer;
  {$ENDIF}

  // Setup default sizes
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

destructor TCustomBCMDButtonFocus.Destroy;
begin
  FTimer.OnTimer := nil;
  {$IFDEF FPC}//#
  FTimer.OnStartTimer := nil;
  FTimer.OnStopTimer := nil;
  {$ENDIF}
  FTimer.Enabled := False;
  FStyleNormal.Free;
  FStyleHover.Free;
  FStyleActive.Free;
  FStyleDisabled.Free;
  inherited Destroy;
end;

procedure TCustomBCMDButtonFocus.SelectAll;
var
  i: integer;
  control: TWinControl;
begin
  if Parent is TWinControl then
  begin
    control := TWinControl(Parent);
    for i := 0 to control.ControlCount - 1 do
      if (control.Controls[i] is TCustomBCMDButtonFocus) then
        if (TCustomBCMDButtonFocus(control.Controls[i]).Kind in
          [mdbkToggle, mdbkCheckBox]) then
          TCustomBCMDButtonFocus(control.Controls[i]).Checked := True;
  end;
end;

procedure TCustomBCMDButtonFocus.UnselectAll;
var
  i: integer;
  control: TWinControl;
begin
  if Parent is TWinControl then
  begin
    control := TWinControl(Parent);
    for i := 0 to control.ControlCount - 1 do
      if (control.Controls[i] is TCustomBCMDButtonFocus) then
        if (TCustomBCMDButtonFocus(control.Controls[i]).Kind in
          [mdbkToggle, mdbkCheckBox]) then
          TCustomBCMDButtonFocus(control.Controls[i]).Checked := False;
  end;
end;

procedure TCustomBCMDButtonFocus.InvertSelection;
var
  i: integer;
  control: TWinControl;
begin
  if Parent is TWinControl then
  begin
    control := TWinControl(Parent);
    for i := 0 to control.ControlCount - 1 do
      if (control.Controls[i] is TCustomBCMDButtonFocus) then
        if (TCustomBCMDButtonFocus(control.Controls[i]).Kind in
          [mdbkToggle, mdbkCheckBox]) then
          TCustomBCMDButtonFocus(control.Controls[i]).Checked :=
            not TCustomBCMDButtonFocus(control.Controls[i]).Checked;
  end;
end;

function TCustomBCMDButtonFocus.GetSelected: TStringList;
var
  i: integer;
  control: TWinControl;
begin
  Result := TStringList.Create;
  if Parent is TWinControl then
  begin
    control := TWinControl(Parent);
    for i := 0 to control.ControlCount - 1 do
      if (control.Controls[i] is TCustomBCMDButtonFocus) then
        if TCustomBCMDButtonFocus(control.Controls[i]).Checked then
          Result.AddObject(TCustomBCMDButtonFocus(control.Controls[i]).Caption,
            TCustomBCMDButtonFocus(control.Controls[i]));
  end;
end;

end.
