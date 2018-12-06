{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCMDButton;

{$I bgracontrols.inc}

// Set this to show number of repaint in each MDBUTTON
{ $DEFINE MDBUTTON_DEBUG}

// Set this to animate only a MDBUTTON at a time
{ $DEFINE MDBUTTON_ANIMATEONLYONE}

interface

uses
  Classes, SysUtils, Types, {$IFDEF FPC}LResources,{$ELSE}BGRAGraphics, GraphType, FPImage,{$ENDIF}
  Forms, Controls, Graphics, Dialogs,
  BCBaseCtrls, BGRABitmap, BGRABitmapTypes, ExtCtrls, Math, BGRABlend;

var
  // Default icons for Check Box
  {BCMDBUTTONBALLOTBOX: string = '☐'; // '✗'
  BCMDBUTTONBALLOTBOXWITHCHECK: string = '☑'; // '✓'

  // Default icons for Radio Button
  BCMDBUTTONRADIOBUTTON: string = '🔘';
  BCMDBUTTONRADIOBUTTONCIRCLE: string = '◯';}

  // Characters that can be used on systems that lack of the previous unicode symbols
  BCMDBUTTONBALLOTBOX: string = '[  ]';
  BCMDBUTTONBALLOTBOXWITHCHECK: string = '[X]';
  BCMDBUTTONRADIOBUTTON: string = '[O]';
  BCMDBUTTONRADIOBUTTONCIRCLE: string = '[  ]';

  // Animation speed
  // Possible values: between 0 and 1
  // 0 is an infinite animation that display nothing (only redraw itself)
  // 1 is the faster animation (like no animation, from 0 to 1 in 1 frame)
  // Recommended values: between 0.01 (slow) and 0.1 (fast), default 0.04
  // Hint: turn on debug to see how much frames are rendered
  BCMDBUTTONANIMATIONSPEED: double = 0.04;

  // Global enable/disable animations
  BCMDBUTTONANIMATION: boolean = True;

const
  // Timer speed: default 15 (a bit more than 60 fps)
  // Other values: 16 (60 fps) 20 (50 fps) 25 (40 fps) 33 (30 fps)
  // Hint: 15 is the smoothest -tested- value on Windows, even if 16 is closer to 60 fps
  //       * values below 15 are not noticeable
  //       * higher values are not smooth
  // Hint: changing this doesn't change the ammount of frames rendered,
  //       only changes the time between frames
  // Hint: if you decrease MDBUTTONTIMERSPEED, increase BCMDBUTTONANIMATIONSPEED
  //       to keep a smooth animation
  BCMDBUTTONTIMERSPEED: integer = 15;

type
  TBCMDButtonState = (mdbsNormal, mdbsHover, mdbsActive);
  TBCMDButtonKind = (mdbkNormal, mdbkToggle, mdbkToggleGroup, mdbkCheckBox,
    mdbkRadioButton, mdbkTab);

  { TBCMDButtonStyle }

  TBCMDButtonStyle = class(TPersistent)
  private
    FColor: TColor;
    FOnChange: TNotifyEvent;
    FTextColor: TColor;
    procedure SetFColor(AValue: TColor);
    procedure SetFTextColor(AValue: TColor);
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
  published
    property Color: TColor read FColor write SetFColor;
    property TextColor: TColor read FTextColor write SetFTextColor;
  end;

  { TCustomBCMDButton }

  TCustomBCMDButton = class(TBGRAGraphicCtrl)
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
    class function GetControlClassDefaultSize: TSize; override;
    function GetRealCaption: string;
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

  TBCMDButton = class(TCustomBCMDButton)
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
  MDAnimating: TCustomMDButton;

{$ENDIF}

{$IFDEF FPC}
procedure Register;
begin
  RegisterComponents('BGRA Button Controls', [TBCMDButton]);
end;
{$ENDIF}

{ TBCMDButtonStyle }

procedure TBCMDButtonStyle.SetFColor(AValue: TColor);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;
  if Assigned(FOnChange) then
    OnChange(Self);
end;

procedure TBCMDButtonStyle.SetFTextColor(AValue: TColor);
begin
  if FTextColor = AValue then
    Exit;
  FTextColor := AValue;
  if Assigned(FOnChange) then
    OnChange(Self);
end;

constructor TBCMDButtonStyle.Create;
begin
  inherited Create;
  FColor := clWhite;
  FTextColor := clBlack;
end;

{ TCustomBCMDButton }

procedure TCustomBCMDButton.SetFStyleActive(AValue: TBCMDButtonStyle);
begin
  if FStyleActive = AValue then
    Exit;
  FStyleActive := AValue;
end;

procedure TCustomBCMDButton.SetFAlignment(AValue: TAlignment);
begin
  if FAlignment = AValue then
    Exit;
  FAlignment := AValue;
  Invalidate;
end;

procedure TCustomBCMDButton.SetFAnimation(AValue: boolean);
begin
  if FAnimation = AValue then
    Exit;
  FAnimation := AValue;
  Invalidate;
end;

procedure TCustomBCMDButton.SetFChecked(AValue: boolean);
begin
  if FChecked = AValue then
    Exit;
  FChecked := AValue;
  if FChecked and (FKind in [mdbkToggleGroup, mdbkRadioButton, mdbkTab]) then
    UncheckOthers;
  Invalidate;
end;

procedure TCustomBCMDButton.SetFKind(AValue: TBCMDButtonKind);
begin
  if FKind = AValue then
    Exit;
  FKind := AValue;
  Invalidate;
end;

procedure TCustomBCMDButton.OnChangeStyle(Sender: TObject);
begin
  Invalidate;
end;

procedure TCustomBCMDButton.SetFStyleDisabled(AValue: TBCMDButtonStyle);
begin
  if FStyleDisabled = AValue then
    Exit;
  FStyleDisabled := AValue;
end;

procedure TCustomBCMDButton.SetFStyleHover(AValue: TBCMDButtonStyle);
begin
  if FStyleHover = AValue then
    Exit;
  FStyleHover := AValue;
end;

procedure TCustomBCMDButton.SetFStyleNormal(AValue: TBCMDButtonStyle);
begin
  if FStyleNormal = AValue then
    Exit;
  FStyleNormal := AValue;
end;

procedure TCustomBCMDButton.SetFTextAutoSize(AValue: boolean);
begin
  if FTextAutoSize = AValue then
    Exit;
  FTextAutoSize := AValue;
end;

procedure TCustomBCMDButton.SetFTextLayout(AValue: TTextLayout);
begin
  if FTextLayout = AValue then
    Exit;
  FTextLayout := AValue;
  Invalidate;
end;

procedure TCustomBCMDButton.SetFTextProportional(AValue: boolean);
begin
  if FTextProportional=AValue then Exit;
  FTextProportional:=AValue;
  Invalidate;
end;

procedure TCustomBCMDButton.SetFTextProportionalRatio(AValue: single);
begin
  if FTextProportionalRatio=AValue then Exit;
  FTextProportionalRatio:=AValue;
  Invalidate;
end;

procedure TCustomBCMDButton.CalculatePreferredSize(
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
  s := bmp.TextSize(GetRealCaption);
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

procedure TCustomBCMDButton.Paint;
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

  tempText := GetRealCaption;

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
          bmp.TextRect(Rect(0, 0,Width, Height),tempText, Alignment, TextLayout, FStyleNormal.TextColor);
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
          bmp.TextRect(Rect(0, 0,Width, Height),tempText, Alignment, TextLayout, FStyleHover.TextColor);
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
          bmp.TextRect(Rect(0, 0,Width, Height),tempText, Alignment, TextLayout, FStyleActive.TextColor);
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
          bmp.TextRect(Rect(0, 0,Width, Height),tempText, Alignment, TextLayout, FStyleNormal.TextColor);
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
          bmp.TextRect(Rect(0, 0,Width, Height),tempText, Alignment, TextLayout, FStyleHover.TextColor);
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
    bmp.TextRect(Rect(0, 0,Width, Height),tempText, Alignment, TextLayout, FStyleDisabled.TextColor);
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

procedure TCustomBCMDButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
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

procedure TCustomBCMDButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (x > 0) and (x < Width) and (y > 0) and (y < Height) and (FState = mdbsActive) then
    FState := mdbsHover
  else
    FState := mdbsNormal;
  Invalidate;
end;

procedure TCustomBCMDButton.MouseEnter;
begin
  inherited MouseEnter;
  FState := mdbsHover;
  Invalidate;
end;

procedure TCustomBCMDButton.MouseLeave;
begin
  inherited MouseLeave;
  FState := mdbsNormal;
  Invalidate;
end;

procedure TCustomBCMDButton.RealSetText(const Value: TCaption);
begin
  inherited RealSetText(Value);
  InvalidatePreferredSize;
  Invalidate;
end;

procedure TCustomBCMDButton.OnTimer(Sender: TObject);
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
      FAlphaPercent := FAlphaPercent -BCMDBUTTONANIMATIONSPEED;
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

procedure TCustomBCMDButton.OnStartTimer(Sender: TObject);
begin
  FPercent := 0;
  FAlphaPercent := 1;
end;

procedure TCustomBCMDButton.OnStopTimer(Sender: TObject);
begin

end;

function TCustomBCMDButton.easeInOutQuad(t: double): double;
begin
  if t < 0.5 then
    Result := 2 * t * t
  else
    Result := -1 + (4 - 2 * t) * t;
end;

function TCustomBCMDButton.easeOutQuad(t: double): double;
begin
  Result := t * (2 - t);
end;

procedure TCustomBCMDButton.UncheckOthers;
var
  i: integer;
  control: TWinControl;
begin
  if Parent is TWinControl then
  begin
    control := TWinControl(Parent);
    for i := 0 to control.ControlCount - 1 do
      if (control.Controls[i] <> Self) and (control.Controls[i] is TCustomBCMDButton) then
        if (TCustomBCMDButton(control.Controls[i]).Kind in
          [mdbkToggleGroup, mdbkRadioButton, mdbkTab]) then
          TCustomBCMDButton(control.Controls[i]).Checked := False;
  end;
end;

class function TCustomBCMDButton.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 75;
  Result.CY := 25;
end;

function TCustomBCMDButton.GetRealCaption: string;
var
  tempText: string;
begin
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
  result := tempText;
end;

constructor TCustomBCMDButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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

destructor TCustomBCMDButton.Destroy;
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

procedure TCustomBCMDButton.SelectAll;
var
  i: integer;
  control: TWinControl;
begin
  if (Parent <> nil) and (Parent is TWinControl) then
  begin
    control := TWinControl(Parent);
    for i := 0 to control.ControlCount - 1 do
      if (control.Controls[i] is TCustomBCMDButton) then
        if (TCustomBCMDButton(control.Controls[i]).Kind in
          [mdbkToggle, mdbkCheckBox]) then
          TCustomBCMDButton(control.Controls[i]).Checked := True;
  end;
end;

procedure TCustomBCMDButton.UnselectAll;
var
  i: integer;
  control: TWinControl;
begin
  if (Parent <> nil) and (Parent is TWinControl) then
  begin
    control := TWinControl(Parent);
    for i := 0 to control.ControlCount - 1 do
      if (control.Controls[i] is TCustomBCMDButton) then
        if (TCustomBCMDButton(control.Controls[i]).Kind in
          [mdbkToggle, mdbkCheckBox]) then
          TCustomBCMDButton(control.Controls[i]).Checked := False;
  end;
end;

procedure TCustomBCMDButton.InvertSelection;
var
  i: integer;
  control: TWinControl;
begin
  if (Parent <> nil) and (Parent is TWinControl) then
  begin
    control := TWinControl(Parent);
    for i := 0 to control.ControlCount - 1 do
      if (control.Controls[i] is TCustomBCMDButton) then
        if (TCustomBCMDButton(control.Controls[i]).Kind in
          [mdbkToggle, mdbkCheckBox]) then
          TCustomBCMDButton(control.Controls[i]).Checked :=
            not TCustomBCMDButton(control.Controls[i]).Checked;
  end;
end;

function TCustomBCMDButton.GetSelected: TStringList;
var
  i: integer;
  control: TWinControl;
begin
  Result := TStringList.Create;
  if (Parent <> nil) and (Parent is TWinControl) then
  begin
    control := TWinControl(Parent);
    for i := 0 to control.ControlCount - 1 do
      if (control.Controls[i] is TCustomBCMDButton) then
        if TCustomBCMDButton(control.Controls[i]).Checked then
          Result.AddObject(TCustomBCMDButton(control.Controls[i]).Caption,
            TCustomBCMDButton(control.Controls[i]));
  end;
end;

end.
