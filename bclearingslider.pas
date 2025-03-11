{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Author: Boban Spasic
 Credits to: hedgehog, circular and lainz from Lazarus forum
 Based on TFluentProgressRing from hedgehog

 2024-11-20 Massimo Magnano Added Draw of Caption and TextLayouts
}

unit BCLeaRingSlider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls, LResources,
  BGRABitmapTypes, BGRABitmap, BGRATextFX, BGRAGradients, BCLeaTheme, BCLeaTypes;

type

  { TBCLeaRingSlider }

  TBCLeaRingSlider = class(TCustomControl)
  private
    FBitmap: TBGRABitmap;
    FTheme: TBCLeaTheme;
    FOnChangeValue: TNotifyEvent;
    FMaxValue: integer;
    FMinValue: integer;
    FOffset: integer;
    FValue: integer;
    FLineColor: TColor;
    FLineBkgColor: TColor;
    FLineWidth: integer;
    FVerticalPos: single;
    FDeltaPos: single;
    FDirection: integer;
    FPrevCurrPosition: single;
    FSettingVerticalPos: boolean;
    FSensitivity: integer;
    FMinAngle: integer;
    FMaxAngle: integer;
    FFontShadowColor: TColor;
    FFontShadowOffsetX: integer;
    FFontShadowOffsetY: integer;
    FFontShadowRadius: integer;
    FDrawText: boolean;
    FDrawPointer: boolean;
    FBkgColor: TColor;
    FStyle: TZStyle;
    FDrawTextPhong: boolean;
    FPointerColor: TColor;
    FPointerSize: integer;
    FAltitude: integer;
    //global intensity of the light
    FLightSourceIntensity: single;
    //minimum distance always added (positive value)
    FLightSourceDistanceTerm: single;
    //how much actual distance is taken into account (usually 0 or 1)
    FLightSourceDistanceFactor: single;
    //how much the location of the lightened pixel is taken into account (usually 0 or 1)
    FLightDestFactor: single;
    //color of the light reflection
    FLightColor: TColor;
    //how much light is reflected (0..1)
    FSpecularFactor: single;
    //how concentrated reflected light is (positive value)
    FSpecularIndex: single;
    //ambiant lighting whereever the point is (0..1)
    FAmbientFactor: single;
    //diffusion, i.e. how much pixels are lightened by light source (0..1)
    FDiffusionFactor: single;
    //how much hidden surface are darkened (0..1)
    FNegativeDiffusionFactor: single;
    //when diffusion saturates, use light color to show it
    FDiffuseSaturation: boolean;
    FLightPositionX: integer;
    FLightPositionY: integer;
    FLightPositionZ: integer;
    rCaptionLayout: TTextLayout;
    rDrawCaption: Boolean;
    rDrawCaptionPhong: Boolean;
    rTextLayout: TTextLayout;

    procedure SetCaptionLayout(AValue: TTextLayout);
    procedure SetDrawCaption(AValue: Boolean);
    procedure SetDrawCaptionPhong(AValue: Boolean);
    procedure SetLineBkgColor(AValue: TColor);
    procedure SetLineColor(AValue: TColor);
    procedure SetMaxValue(AValue: integer);
    procedure SetMinValue(AValue: integer);
    procedure SetTextLayout(AValue: TTextLayout);
    procedure SetValue(AValue: integer);
    procedure SetLineWidth(AValue: integer);
    procedure UpdateVerticalPos(X, Y: integer);
    procedure SetSensitivity(AValue: integer);
    procedure SetMinAngle(AValue: integer);
    procedure SetMaxAngle(AValue: integer);
    procedure SetDrawText(AValue: boolean);
    procedure SetDrawPointer(AValue: boolean);
    procedure SetBkgColor(AValue: TColor);
    procedure SetFFontShadowColor(AValue: TColor);
    procedure SetFFontShadowOffsetX(AValue: integer);
    procedure SetFFontShadowOffsetY(AValue: integer);
    procedure SetFFontShadowRadius(AValue: integer);
    procedure SetStyle(AValue: TZStyle);
    procedure SetDrawTextPhong(AValue: boolean);
    procedure SetPointerColor(AValue: TColor);
    procedure SetPointerSize(AValue: integer);
    procedure SetAltitude(AValue: integer);
    procedure SetTheme(AValue: TBCLeaTheme);

  protected
    procedure SetEnabled(Value: boolean); override;
    procedure SetVisible(Value: boolean); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Redraw;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateTheme;
    procedure ApplyTheme;
    procedure SaveThemeToFile(AFileName: string);
    procedure LoadThemeFromFile(AFileName: string);
    procedure ApplyDefaultTheme;

  published
    property Align;
    property BorderSpacing;
    property Caption;
    property Color;
    property Cursor;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Anchors;
    property Constraints;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnConTextPopup;
    property MinValue: integer read FMinValue write SetMinValue default 0;
    property MaxValue: integer read FMaxValue write SetMaxValue default 100;
    property Value: integer read FValue write SetValue default 0;
    property LineColor: TColor read FLineColor write SetLineColor default TColor($009E5A00);
    property LineBkgColor: TColor read FLineBkgColor write SetLineBkgColor default TColor($00D3D3D3);
    property LineWidth: integer read FLineWidth write SetLineWidth default 8;
    property OnChangeValue: TNotifyEvent read FOnChangeValue write FOnChangeValue;
    //Greater value is less sensitive
    property Sensitivity: integer read FSensitivity write SetSensitivity default 10;
    property MinAngle: integer read FMinAngle write SetMinAngle default 20;
    property MaxAngle: integer read FMaxAngle write SetMaxAngle default 340;
    property FontShadowColor: TColor read FFontShadowColor write SetFFontShadowColor default clBlack;
    property FontShadowOffsetX: integer read FFontShadowOffsetX write SetFFontShadowOffsetX default 2;
    property FontShadowOffsetY: integer read FFontShadowOffsetY write SetFFontShadowOffsetY default 2;
    property FontShadowRadius: integer read FFontSHadowRadius write SetFFontShadowRadius default 4;
    property DrawText: boolean read FDrawText write SetDrawText default True;
    property DrawPointer: boolean read FDrawPointer write SetDrawPointer default False;
    property BackgroundColor: TColor read FBkgColor write SetBkgColor default clBtnFace;
    property Style: TZStyle read FStyle write SetStyle default zsRaised;
    property DrawTextPhong: boolean read FDrawTextPhong write SetDrawTextPhong default False;
    property PointerColor: TColor read FPointerColor write SetPointerColor default TColor($00FF9C15);
    property PointerSize: integer read FPointerSize write SetPointerSize default 2;
    property Altitude: integer read FAltitude write SetAltitude default 2;
    property Theme: TBCLeaTheme read FTheme write SetTheme;
    property TextLayout: TTextLayout read rTextLayout write SetTextLayout default tlCenter;
    property DrawCaption: Boolean read rDrawCaption write SetDrawCaption default False;
    property DrawCaptionPhong: Boolean read rDrawCaptionPhong write SetDrawCaptionPhong default False;
    property CaptionLayout: TTextLayout read rCaptionLayout write SetCaptionLayout default tlBottom;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCLeaRingSlider]);
end;

{ TBCLeaRingSlider }

procedure TBCLeaRingSlider.SetMaxAngle(AValue: integer);
begin
  if FMaxAngle = AValue then
    exit;
  FMaxAngle := AValue;
  if FMaxAngle > 350 then
    FMaxAngle := 350;
  if FMinAngle > FMaxAngle then
    FMaxAngle := FMinAngle;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetMinAngle(AValue: integer);
begin
  if FMinAngle = AValue then
    exit;
  FMinAngle := AValue;
  if FMinAngle < 10 then
    FMinAngle := 10;
  if FMinAngle > FMaxAngle then
    FMinAngle := FMaxAngle;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetSensitivity(AValue: integer);
begin
  if FSensitivity = AValue then
    exit;
  if AValue <> 0 then
    FSensitivity := AValue
  else
    FSensitivity := 10;
end;

procedure TBCLeaRingSlider.SetMaxValue(AValue: integer);
begin
  if FMaxValue = AValue then
    exit;
  FMaxValue := AValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  if FMinValue > FMaxValue then
    FMinValue := FMaxValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetLineBkgColor(AValue: TColor);
begin
  if FLineBkgColor = AValue then
    Exit;
  FLineBkgColor := AValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetCaptionLayout(AValue: TTextLayout);
begin
  if rCaptionLayout=AValue then Exit;
  rCaptionLayout:=AValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetDrawCaption(AValue: Boolean);
begin
  if rDrawCaption=AValue then Exit;
  rDrawCaption:=AValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetDrawCaptionPhong(AValue: Boolean);
begin
  if rDrawCaptionPhong=AValue then Exit;
  rDrawCaptionPhong:=AValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetLineColor(AValue: TColor);
begin
  if FLineColor = AValue then
    Exit;
  FLineColor := AValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetMinValue(AValue: integer);
begin
  if FMinValue = AValue then
    exit;
  FMinValue := AValue;
  if FMinValue <> 0 then FOffset := -FMinValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FMaxValue < FMinValue then
    FMaxValue := FMinValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetTextLayout(AValue: TTextLayout);
begin
  if rTextLayout=AValue then Exit;
  rTextLayout:=AValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetValue(AValue: integer);
begin
  if FValue = AValue then
    exit;
  FValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetLineWidth(AValue: integer);
begin
  if FLineWidth = AValue then exit;
  FLineWidth := AValue;
  if Visible then Redraw;
end;

procedure TBCLeaRingSlider.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  Invalidate;
end;

procedure TBCLeaRingSlider.SetVisible(Value: boolean);
begin
  inherited SetVisible(Value);
  Invalidate;
end;

procedure TBCLeaRingSlider.Paint;
begin
  inherited Paint;
  Redraw;
end;

procedure TBCLeaRingSlider.Resize;
begin
  inherited Resize;
  {$IFDEF LCLgtk2} Invalidate; {$ENDIF}
end;

procedure TBCLeaRingSlider.Redraw;
const
  pi15 = pi * 1.5;
var
  TextBmp: TBGRABitmap;
  TextStr: string;
  EffectiveSize, ScaledPhongSize: integer;
  EffectiveLineWidth: single;
  r: single;
  RMinAngle, RMaxAngle: single;
  RValue: single;
  Blur: TBGRABitmap;
  Mask, Mask2: TBGRABitmap;
  Phong: TPhongShading;
  TextSize: TSize;

  procedure DoDrawArc(a, b: single; c: TColor);
  begin
    FBitmap.Canvas2D.strokeStyle(c);
    FBitmap.Canvas2D.beginPath;
    FBitmap.Canvas2D.arc(0, 0, r, a, b, False);
    FBitmap.Canvas2D.stroke;
  end;

  procedure DoDrawPointer(a: single; c: TColor);
  begin
    FBitmap.Canvas2D.strokeStyle(c);
    FBitmap.Canvas2D.beginPath;
    FBitmap.Canvas2D.arc(0, 0, r, a - (FPointerSize / 100), a + (FPointerSize / 100), False);
    FBitmap.Canvas2D.stroke;
  end;

  procedure DoDrawTick(a, b: single; c: TColor);
  begin
    FBitmap.Canvas2D.strokeStyle(c);
    FBitmap.Canvas2D.lineWidth := 5;
    FBitmap.Canvas2D.beginPath;
    FBitmap.Canvas2D.lineTo(0 - a, 0 - b);
    FBitmap.Canvas2D.lineTo(5 - a, -2 - b);
    FBitmap.Canvas2D.lineTo(5 - a, 2 - b);
    FBitmap.Canvas2D.lineTo(0 - a, 0 - b);
    FBitmap.Canvas2D.stroke;
  end;

begin
  FBitmap.SetSize(Width, Height);
  FBitmap.Fill(FBkgColor);

  if Width < Height then
    EffectiveSize := Width
  else
    EffectiveSize := Height;

  if EffectiveSize < 2 then exit;


  FBitmap.Canvas2D.resetTransform;
  FBitmap.Canvas2D.translate((FBitmap.Width-1)/2, (FBitmap.Height-1)/2);
  FBitmap.Canvas2D.rotate(pi15);

  if FLineWidth = 0 then
    EffectiveLineWidth := EffectiveSize / 12
  else
    EffectiveLineWidth := FLineWidth;
  r := (EffectiveSize - EffectiveLineWidth) / 2;

  FBitmap.Canvas2D.lineWidth := EffectiveLineWidth;

  RMinAngle := (180 + FMinAngle) * pi / 180;
  RMaxAngle := ((180 + FMaxAngle) * pi / 180);
  RValue := ((180 + FMinAngle + ((FMaxAngle - FMinAngle) / FMaxValue * FValue)) * pi / 180);

  FBitmap.Canvas2D.lineCapLCL := pecRound;
  // background line
  if FLineBkgColor <> clNone then
    DoDrawArc(RMinAngle, RMaxAngle, FLineBkgColor);

  if Enabled then
  begin
    if FValue > FMinValue then
    begin
      DoDrawArc(RMinAngle, RValue, FLineColor);
      if FDrawPointer then
        DoDrawPointer(RValue, FPointerColor);
    end;
  end
  else
    DoDrawArc(RMinAngle, RMaxAngle, clGray);

  if FDrawText and FDrawTextPhong then
  begin
    TextStr := IntToStr(FValue);
    TextBmp := TextShadow(FBitmap.Width, FBitmap.Height, TextStr, Font.Height,
      Font.Color, FontShadowColor, FontShadowOffsetX,
      FontShadowOffsetY, FontShadowRadius, Font.Style, Font.Name) as TBGRABitmap;
    TextSize:= TextBmp.TextSize(TextStr);
    TextSize.cy:= TextSize.cy+FontShadowOffsetY; //+2*FontShadowRadius ?

    Case rTextLayout of
    tlTop: FBitmap.PutImage(0, -(HalfUp(((EffectiveSize-TextSize.cy) / 2))-Trunc(EffectiveLineWidth)),
                            TextBmp, dmDrawWithTransparency);
    tlCenter: FBitmap.PutImage(0, 0, TextBmp, dmDrawWithTransparency);
    tlBottom: FBitmap.PutImage(0, +(HalfUp(((EffectiveSize-TextSize.cy) / 2))-Trunc(EffectiveLineWidth)),
                               TextBmp, dmDrawWithTransparency);
    end;

    TextBmp.Free;
  end;

  if rDrawCaption and rDrawCaptionPhong then
  begin
    TextBmp := TextShadow(FBitmap.Width, FBitmap.Height, Caption, Font.Height,
                          Font.Color, FontShadowColor, FontShadowOffsetX,
                          FontShadowOffsetY, FontShadowRadius, Font.Style, Font.Name) as TBGRABitmap;
    TextSize:= TextBmp.TextSize(Caption);
    TextSize.cy:= TextSize.cy+FontShadowOffsetY; //+2*FontShadowRadius ?

    Case rCaptionLayout of
    tlTop: FBitmap.PutImage(0, -(HalfUp(((EffectiveSize-TextSize.cy) / 2))-Trunc(EffectiveLineWidth)),
                            TextBmp, dmDrawWithTransparency);
    tlCenter: FBitmap.PutImage(0, 0, TextBmp, dmDrawWithTransparency);
    tlBottom: FBitmap.PutImage(0, +(HalfUp(((EffectiveSize-TextSize.cy) / 2))-Trunc(EffectiveLineWidth)),
                               TextBmp, dmDrawWithTransparency);
    end;

    TextBmp.Free;
  end;

  if (FStyle = zsRaised) or (FStyle = zsLowered) then
  begin
    ScaledPhongSize := round(EffectiveLineWidth / 2);
    Mask := FBitmap.FilterGrayscale as TBGRABitmap;
    if FStyle = zsRaised then
      Mask.Negative;
    Blur := Mask.FilterBlurRadial(ScaledPhongSize, ScaledPhongSize, rbFast) as TBGRABitmap;
    Blur.FillMask(0, 0, Mask, BGRAPixelTransparent, dmSet);
    Mask.Free;

    Phong := TPhongShading.Create;
    begin
      Phong.AmbientFactor := FAmbientFactor;
      Phong.SpecularIndex := FSpecularIndex;
      Phong.LightDestFactor := FLightDestFactor;
      Phong.LightPosition := Point(FLightPositionX + (FBitmap.Width  - EffectiveSize) div 2,
                                   FLightPositionY + (FBitmap.Height - EffectiveSize) div 2);
      Phong.LightPositionZ := FLightPositionZ;
      Phong.LightSourceIntensity := FLightSourceIntensity;
      Phong.LightSourceDistanceTerm := FLightSourceDistanceTerm;
      Phong.LightSourceDistanceFactor := FLightSourceDistanceFactor;
      Phong.NegativeDiffusionFactor := FNegativeDiffusionFactor;
      Phong.SpecularFactor := FSpecularFactor;
      Phong.DiffusionFactor := FDiffusionFactor;
      Phong.DiffuseSaturation := FDiffuseSaturation;
      Phong.LightColor := FLightColor;
    end;
    Phong.Draw(FBitmap, Blur, FAltitude, 0, 0, FBitmap);
    Phong.Free;
    Blur.Free;

    Mask := TBGRABitmap.Create(FBitmap.Width, FBitmap.Height, BGRABlack);
    Mask.FillEllipseAntialias((FBitmap.Width-1)/2, (FBitmap.Height-1)/2, EffectiveSize div 2, EffectiveSize div 2, BGRAWhite);
    Mask2 := TBGRABitmap.Create(FBitmap.Width, FBitmap.Height, ColorToBGRA(ColorToRGB(FBkgColor)));
    Mask2.PutImage(0, 0, FBitmap, dmSet);
    Mask2.ApplyMask(Mask);
    Mask.Free;
    FBitmap.Fill(FBkgColor);
    FBitmap.PutImage(0, 0, Mask2, dmDrawWithTransparency);
    Mask2.Free;
  end;

  if FDrawText and not FDrawTextPhong then
  begin
    TextStr := IntToStr(FValue);
    TextBmp := TextShadow(FBitmap.Width, FBitmap.Height, TextStr, Font.Height,
      Font.Color, FontShadowColor, FontShadowOffsetX,
      FontShadowOffsetY, FontShadowRadius, Font.Style, Font.Name) as TBGRABitmap;
    TextSize:= TextBmp.TextSize(TextStr);
    TextSize.cy:= TextSize.cy+FontShadowOffsetY; //+2*FontShadowRadius ?

    Case rTextLayout of
    tlTop: FBitmap.PutImage(0, -(HalfUp(((EffectiveSize-TextSize.cy) / 2))-Trunc(EffectiveLineWidth)),
                            TextBmp, dmDrawWithTransparency);
    tlCenter: FBitmap.PutImage(0, 0, TextBmp, dmDrawWithTransparency);
    tlBottom: FBitmap.PutImage(0, +(HalfUp(((EffectiveSize-TextSize.cy) / 2))-Trunc(EffectiveLineWidth)),
                               TextBmp, dmDrawWithTransparency);
    end;

    TextBmp.Free;
  end;

  if rDrawCaption and not(rDrawCaptionPhong) then
  begin
    TextBmp := TextShadow(FBitmap.Width, FBitmap.Height, Caption, Font.Height,
                          Font.Color, FontShadowColor, FontShadowOffsetX,
                          FontShadowOffsetY, FontShadowRadius, Font.Style, Font.Name) as TBGRABitmap;
    TextSize:= TextBmp.TextSize(Caption);
    TextSize.cy:= TextSize.cy+FontShadowOffsetY; //+2*FontShadowRadius ?

    Case rCaptionLayout of
    tlTop: FBitmap.PutImage(0, -(HalfUp(((EffectiveSize-TextSize.cy) / 2))-Trunc(EffectiveLineWidth)),
                            TextBmp, dmDrawWithTransparency);
    tlCenter: FBitmap.PutImage(0, 0, TextBmp, dmDrawWithTransparency);
    tlBottom: FBitmap.PutImage(0, +(HalfUp(((EffectiveSize-TextSize.cy) / 2))-Trunc(EffectiveLineWidth)),
                               TextBmp, dmDrawWithTransparency);
    end;

    TextBmp.Free;
  end;

  FBitmap.Draw(Canvas, 0, 0, True);
end;

constructor TBCLeaRingSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, 100, 100);
  TabStop := True;
  FMaxValue := 100;
  FMinValue := 0;
  FOffset := 0;
  FMinAngle := 20;
  FMaxAngle := 340;
  FValue := 0;
  FDeltaPos := 0;
  FDirection := 0;
  FSensitivity := 10;
  Font.Color := clBlack;
  Font.Height := 20;
  FDrawText := True;
  rTextLayout:= tlCenter;
  FDrawPointer := False;
  rDrawCaption:= False;
  rCaptionLayout:= tlBottom;
  ApplyDefaultTheme;
  FBitmap := TBGRABitmap.Create(Width, Height, FBkgColor);
end;

destructor TBCLeaRingSlider.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TBCLeaRingSlider.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FDeltaPos := ((ClientHeight / FSensitivity) - (Y / FSensitivity)) * (FMaxValue / ClientHeight);
    FDirection := 0;
    FPrevCurrPosition := 0;
    FVerticalPos := FValue;
    FSettingVerticalPos := True;
  end;
end;

procedure TBCLeaRingSlider.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
    FSettingVerticalPos := False;
end;

procedure TBCLeaRingSlider.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FSettingVerticalPos then
    UpdateVerticalPos(X, Y);
end;

procedure TBCLeaRingSlider.UpdateVerticalPos(X, Y: integer);
var
  FPreviousPos: single;
  FCurrPos: single;
  FNewDirection: integer;
begin
  {The whole code here is for beter control of the slider with the mouse movements}
  FPreviousPos := FVerticalPos;
  FCurrPos := ((ClientHeight / FSensitivity) - (Y / FSensitivity)) * (FMaxValue / ClientHeight);

  if FPrevCurrPosition <> 0 then
  begin
    if FCurrPos < FPrevCurrPosition then FNewDirection := -1;
    if FCurrPos > FPrevCurrPosition then FNewDirection := 1;
    if FNewDirection <> FDirection then
    begin
      FDirection := FNewDirection;
      FDeltaPos := ((ClientHeight / FSensitivity) - (Y / FSensitivity)) * (FMaxValue / ClientHeight);
    end;
  end;
  FPrevCurrPosition := FCurrPos;

  FVerticalPos := FVerticalPos - FDeltaPos + FCurrPos;
  if FVerticalPos < FMinValue then FVerticalPos := FMinValue;
  if FVerticalPos > FMaxValue then FVerticalPos := FMaxValue;

  FValue := round(FVerticalPos);
  if FValue < FMinValue then
    FValue := FMinValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;

  Redraw;
  if (FPreviousPos <> FVerticalPos) and Assigned(FOnChangeValue) then
    FOnChangeValue(Self);
end;

procedure TBCLeaRingSlider.SetFFontShadowColor(AValue: TColor);
begin
  if FFontShadowColor = AValue then
    Exit;
  FFontShadowColor := AValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetDrawText(AValue: boolean);
begin
  if FDrawText = AValue then Exit;
  FDrawText := AValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetFFontShadowOffsetX(AValue: integer);
begin
  if FFontShadowOffsetX = AValue then
    Exit;
  FFontShadowOffsetX := AValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetFFontShadowOffsetY(AValue: integer);
begin
  if FFontShadowOffsetY = AValue then
    Exit;
  FFontShadowOffsetY := AValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetFFontShadowRadius(AValue: integer);
begin
  if FFontSHadowRadius = AValue then
    Exit;
  FFontSHadowRadius := AValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetBkgColor(AValue: TColor);
begin
  if FBkgColor = AValue then
    Exit;
  FBkgColor := AValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetDrawPointer(AValue: boolean);
begin
  if FDrawPointer = AValue then
    Exit;
  FDrawPointer := AValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetStyle(AValue: TZStyle);
begin
  if FStyle = AValue then
    Exit;
  FStyle := AValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetDrawTextPhong(AValue: boolean);
begin
  if FDrawTextPhong = AValue then
    Exit;
  FDrawTextPhong := AValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetPointerColor(AValue: TColor);
begin
  if FPointerColor = AValue then
    Exit;
  FPointerColor := AValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetPointerSize(AValue: integer);
begin
  if FPointerSize = AValue then
    Exit;
  FPointerSize := AValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetAltitude(AValue: integer);
begin
  if FAltitude = AValue then
    Exit;
  FAltitude := AValue;
  Invalidate;
end;

procedure TBCLeaRingSlider.SetTheme(AValue: TBCLeaTheme);
begin
  if FTheme = AValue then
    Exit;
  if Assigned(FTheme) then
    FTheme := nil;
  FTheme := AValue;
  ApplyTheme;
end;

procedure TBCLeaRingSlider.UpdateTheme;
begin
  if Assigned(FTheme) then
  begin
    FTheme.RS_LineWidth := FLineWidth;
    FTheme.RS_LineColor := FLineColor;
    FTheme.RS_LineBkgColor := FLineBkgColor;
    FTheme.RS_BkgColor := FBkgColor;
    FTheme.RS_FontShadowColor := FFontShadowColor;
    FTheme.RS_FontShadowOffsetX := FFontShadowOffsetX;
    FTheme.RS_FontShadowOffsetY := FFontShadowOffsetY;
    FTheme.RS_FontShadowRadius := FFontShadowRadius;
    FTheme.RS_PointerSize := FPointerSize;
    FTheme.RS_PointerColor := FPointerColor;
    FTheme.RS_Style := FStyle;
    FTheme.RS_DrawTextPhong := FDrawTextPhong;
    FTheme.RS_Altitude := FAltitude;
  end;
end;

procedure TBCLeaRingSlider.ApplyTheme;
begin
  if Assigned(FTheme) then
  begin
    FLineWidth := FTheme.RS_LineWidth;
    FLineColor := FTheme.RS_LineColor;
    FLineBkgColor := FTheme.RS_LineBkgColor;
    FBkgColor := FTheme.RS_BkgColor;
    FFontShadowColor := FTheme.RS_FontShadowColor;
    FFontShadowOffsetX := FTheme.RS_FontShadowOffsetX;
    FFontShadowOffsetY := FTheme.RS_FontShadowOffsetY;
    FFontShadowRadius := FTheme.RS_FontShadowRadius;
    FPointerSize := FTheme.RS_PointerSize;
    FPointerColor := Ftheme.RS_PointerColor;
    FStyle := FTheme.RS_Style;
    FDrawTextPhong := FTheme.RS_DrawTextPhong;
    FAltitude := FTheme.RS_Altitude;

    FLightSourceIntensity := FTheme.COM_LightSourceIntensity;
    FLightSourceDistanceTerm := FTheme.COM_LightSourceDistanceTerm;
    FLightSourceDistanceFactor := FTheme.COM_LightSourceDistanceFactor;
    FLightDestFactor := FTheme.COM_LightDestFactor;
    FLightColor := FTheme.COM_LightColor;
    FSpecularFactor := FTheme.COM_SpecularFactor;
    FSpecularIndex := FTheme.COM_SpecularIndex;
    FAmbientFactor := FTheme.COM_AmbientFactor;
    FDiffusionFactor := FTheme.COM_DiffusionFactor;
    FNegativeDiffusionFactor := FTheme.COM_NegativeDiffusionFactor;
    FDiffuseSaturation := FTheme.COM_DiffuseSaturation;
    FLightPositionX := FTheme.COM_LightPositionX;
    FLightPositionY := FTheme.COM_LightPositionY;
    FLightPositionZ := FTheme.COM_LightPositionZ;
    Invalidate;
  end
  else
  begin
    ApplyDefaultTheme;
  end;
end;

procedure TBCLeaRingSlider.SaveThemeToFile(AFileName: string);
begin
  if Assigned(FTheme) then
    FTheme.SaveThemeToFile(AFileName);
end;

procedure TBCLeaRingSlider.LoadThemeFromFile(AFileName: string);
begin
  if Assigned(FTheme) then
    FTheme.LoadThemeFromFile(AFileName);
end;

procedure TBCLeaRingSlider.ApplyDefaultTheme;
begin
  FLineWidth := 8;
  FLineColor := TColor($009E5A00);
  FLineBkgColor := TColor($00D3D3D3);
  FBkgColor := clBtnFace;
  FFontShadowColor := clBlack;
  FFontShadowOffsetX := 2;
  FFontShadowOffsetY := 2;
  FFontShadowRadius := 4;
  FPointerSize := 3;
  FPointerColor := TColor($00FF9C15);
  FStyle := zsRaised;
  FDrawTextPhong := False;
  FAltitude := 2;

  FAmbientFactor := 0.3;
  FSpecularIndex := 10;
  FSpecularFactor := 0.6;
  FLightDestFactor := 1;
  FLightPositionX := -100;
  FLightPositionY := -100;
  FLightPositionZ := 100;
  FLightSourceIntensity := 500;
  FLightSourceDistanceTerm := 150;
  FLightSourceDistanceFactor := 1;
  FNegativeDiffusionFactor := 0.1;
  FLightColor := clWhite;
  FDiffuseSaturation := False;
  FDiffusionFactor := 0.9;
end;

end.
