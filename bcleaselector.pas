{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Author: Boban Spasic
 Credits to: hedgehog, circular and lainz from Lazarus forum
 Based on TFluentProgressRing from hedgehog

 2024-11-20 Massimo Magnano Added TextLayout
}

unit BCLeaSelector;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls, LResources,
  BGRABitmapTypes, BGRABitmap, BGRATextFX, BGRAGradients, BCLeaTypes, BCLeaTheme;

type

  { TBCLeaSelector }

  TBCLeaSelector = class(TCustomControl)
  private
    FBitmap: TBGRABitmap;
    FTheme: TBCLeaTheme;
    FOnChangeValue: TNotifyEvent;
    FTicksCount: integer;
    FValue: integer;
    FLineColor: TColor;
    FLineBkgColor: TColor;
    FLineWidth: integer;
    FVerticalPos: single;
    FDeltaPos: single;
    FSettingVerticalPos: boolean;
    FSensitivity: integer;
    FMinAngle: integer;
    FMaxAngle: integer;
    FFontShadowColor: TColor;
    FFontShadowOffsetX: integer;
    FFontShadowOffsetY: integer;
    FFontShadowRadius: integer;
    FDrawText: boolean;
    FDrawTicks: boolean;
    FBkgColor: TColor;
    FItems: TStrings;
    FMinTicksAngle: integer;
    FMaxTicksAngle: integer;
    FPointerSize: integer;
    FStyle: TZStyle;
    FDrawTextPhong: boolean;
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
    rTextLayout: TTextLayout;
    procedure SetLineBkgColor(AValue: TColor);
    procedure SetLineColor(AValue: TColor);
    procedure SetTextLayout(AValue: TTextLayout);
    procedure SetTicksCount(AValue: integer);
    procedure SetValue(AValue: integer);
    procedure SetLineWidth(AValue: integer);
    procedure UpdateVerticalPos(X, Y: integer);
    procedure SetSensitivity(AValue: integer);
    procedure SetMinAngle(AValue: integer);
    procedure SetMaxAngle(AValue: integer);
    procedure SetDrawText(AValue: boolean);
    procedure SetDrawTicks(AValue: boolean);
    procedure SetBkgColor(AValue: TColor);
    procedure SetFFontShadowColor(AValue: TColor);
    procedure SetFFontShadowOffsetX(AValue: integer);
    procedure SetFFontShadowOffsetY(AValue: integer);
    procedure SetFFontShadowRadius(AValue: integer);
    procedure SetItems(const Value: TStrings);
    procedure ItemsChanged(Sender: TObject);
    procedure SetMinTicksAngle(AValue: integer);
    procedure SetMaxTicksAngle(AValue: integer);
    procedure SetPointerSize(AValue: integer);
    procedure SetStyle(AValue: TZStyle);
    procedure SetDrawTextPhong(AValue: boolean);
    procedure SetTheme(AValue: TBCLeaTheme);
    procedure SetAltitude(AValue: integer);
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
    property TicksCount: integer read FTicksCount write SetTicksCount default 3;
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
    property DrawTicks: boolean read FDrawTicks write SetDrawTicks default False;
    property BackgroundColor: TColor read FBkgColor write SetBkgColor default clBtnFace;
    property Items: TStrings read FItems write SetItems;
    property MinTicksAngle: integer read FMinTicksAngle write SetMinTicksAngle default 20;
    property MaxTicksAngle: integer read FMaxTicksAngle write SetMaxTicksAngle default 340;
    property PointerSize: integer read FPointerSize write SetPointerSize default 3;
    property Style: TZStyle read FStyle write SetStyle default zsRaised;
    property DrawTextPhong: boolean read FDrawTextPhong write SetDrawTextPhong default False;
    property Theme: TBCLeaTheme read FTheme write SetTheme;
    property Altitude: integer read FAltitude write SetAltitude default 2;
    property TextLayout: TTextLayout read rTextLayout write SetTextLayout default tlCenter;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCLeaSelector]);
end;

{ TBCLeaSelector }

procedure TBCLeaSelector.SetMaxAngle(AValue: integer);
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

procedure TBCLeaSelector.SetMinAngle(AValue: integer);
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

procedure TBCLeaSelector.SetMaxTicksAngle(AValue: integer);
begin
  if FMaxTicksAngle = AValue then
    exit;
  FMaxTicksAngle := AValue;
  if FMaxTicksAngle > 350 then
    FMaxTicksAngle := 350;
  if FMinTicksAngle > FMaxTicksAngle then
    FMaxTicksAngle := FMinTicksAngle;
  Invalidate;
end;

procedure TBCLeaSelector.SetMinTicksAngle(AValue: integer);
begin
  if FMinTicksAngle = AValue then
    exit;
  FMinTicksAngle := AValue;
  if FMinTicksAngle < 10 then
    FMinTicksAngle := 10;
  if FMinTicksAngle > FMaxTicksAngle then
    FMinTicksAngle := FMaxTicksAngle;
  Invalidate;
end;

procedure TBCLeaSelector.SetSensitivity(AValue: integer);
begin
  if FSensitivity = AValue then
    exit;
  if AValue <> 0 then
    FSensitivity := AValue
  else
    FSensitivity := 10;
end;

procedure TBCLeaSelector.SetLineBkgColor(AValue: TColor);
begin
  if FLineBkgColor = AValue then
    Exit;
  FLineBkgColor := AValue;
  Invalidate;
end;

procedure TBCLeaSelector.SetLineColor(AValue: TColor);
begin
  if FLineColor = AValue then
    Exit;
  FLineColor := AValue;
  Invalidate;
end;

procedure TBCLeaSelector.SetTextLayout(AValue: TTextLayout);
begin
  if rTextLayout=AValue then Exit;
  rTextLayout:=AValue;
  Invalidate;
end;

procedure TBCLeaSelector.SetTicksCount(AValue: integer);
begin
  if FTicksCount = AValue then
    exit;
  if FTicksCount < 2 then
    exit;
  FTicksCount := AValue;
  if FItems.Count < FTicksCount then
  begin
    while FItems.Count <= FTicksCount do
      FItems.Add('Item ' + IntToStr(FItems.Count + 1));
  end;
  Invalidate;
end;

procedure TBCLeaSelector.SetValue(AValue: integer);
begin
  if FValue = AValue then
    exit;
  FValue := AValue;
  if FValue < 1 then
    FValue := 1;
  if FValue > FTicksCount then
    FValue := FTicksCount;
  Invalidate;
end;

procedure TBCLeaSelector.SetPointerSize(AValue: integer);
begin
  if FPointerSize = AValue then
    exit;
  FPointerSize := AValue;
  if FPointerSize < 1 then
    FPointerSize := 1;
  if FPointerSize > 10 then
    FPointerSize := 10;
  Invalidate;
end;

procedure TBCLeaSelector.SetLineWidth(AValue: integer);
begin
  if FLineWidth = AValue then exit;
  FLineWidth := AValue;
  if Visible then Redraw;
end;

procedure TBCLeaSelector.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  Invalidate;
end;

procedure TBCLeaSelector.SetVisible(Value: boolean);
begin
  inherited SetVisible(Value);
  Invalidate;
end;

procedure TBCLeaSelector.Paint;
begin
  inherited Paint;
  Redraw;
end;

procedure TBCLeaSelector.Resize;
begin
  inherited Resize;
  {$IFDEF LCLgtk2} Invalidate; {$ENDIF}
end;

procedure TBCLeaSelector.Redraw;
const
  pi15 = pi * 1.5;
var
  TextBmp: TBGRABitmap;
  TextStr: string;
  EffectiveSize: integer;
  EffectiveLineWidth: single;
  r: single;
  RMinAngle, RMaxAngle, RMinTicksAngle, RMaxTicksAngle, RAngle: single;
  Blur: TBGRABitmap;
  Mask, Mask2: TBGRABitmap;
  Phong: TPhongShading;
  ScaledPhongSize: int64;
  i: integer;
  TextSize: TSize;

  procedure DoDrawArc(a, b: single; c: TColor);
  begin
    FBitmap.Canvas2D.lineCapLCL := pecRound;
    FBitmap.Canvas2D.strokeStyle(c);
    FBitmap.Canvas2D.beginPath;
    FBitmap.Canvas2D.arc(0, 0, r, a, b, False);
    FBitmap.Canvas2D.stroke;
  end;

  procedure DoDrawTicks(a, b: single; c: TColor);
  begin
    FBitmap.Canvas2D.lineCapLCL := pecFlat;
    FBitmap.Canvas2D.strokeStyle(c);
    FBitmap.Canvas2D.beginPath;
    FBitmap.Canvas2D.arc(0, 0, r, a, b, False);
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
  RMaxAngle := ((180 + FMaxAngle) * pi / 180) - RMinAngle;

  RMinTicksAngle := (180 + FMinTicksAngle) * pi / 180;
  RMaxTicksAngle := ((180 + FMaxTicksAngle) * pi / 180) - RMinTicksAngle;

  // background line
  if FLineBkgColor <> clNone then
    DoDrawArc(RMinAngle, (RMaxAngle + RMinAngle), FLineBkgColor);

  if FDrawTicks then
  begin
    for i := 0 to FTicksCount - 1 do
    begin
      RAngle := (RMaxTicksAngle / (FTicksCount - 1)) * (i - ((FTicksCount - 1) / 2));
      DoDrawTicks(RAngle - FPointerSize / 200, RAngle + FPointerSize / 200, clBlack);
    end;
  end;

  RAngle := (RMaxTicksAngle / (FTicksCount - 1)) * (FValue - ((FTicksCount - 1) / 2));
  if Enabled then
  begin
    if FValue >= 0 then
      DoDrawArc(RAngle - FPointerSize / 100, RAngle + FPointerSize / 100, FLineColor);
  end
  else
    DoDrawArc(RAngle - FPointerSize / 100, RAngle + FPointerSize / 100, clGray);

  if FDrawText and FDrawTextPhong then
  begin
    //draw text before we apply phong
    if FItems.Count >= FValue then
      TextStr := FItems[FValue]
    else
      TextStr := 'NaN';
    TextBmp := TextShadow(FBitmap.Width, FBitmap.Height, TextStr, Font.Height,
      Font.Color, FontShadowColor, FontShadowOFfsetX,
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

    //cut out phong-affected area outside the ring and fill with background color
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
    if FItems.Count >= FValue then
      TextStr := FItems[FValue]
    else
      TextStr := 'NaN';
    TextBmp := TextShadow(FBitmap.Width, FBitmap.Height, TextStr, Font.Height,
      Font.Color, FontShadowColor, FontShadowOFfsetX,
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

  FBitmap.Draw(Canvas, 0, 0, True);
end;

constructor TBCLeaSelector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, 100, 100);
  TabStop:=True;
  FTicksCount := 3;
  FMinAngle := 20;
  FMaxAngle := 340;
  FMinTicksAngle := 150;
  FMaxTicksAngle := 210;
  FValue := 0;
  FDeltaPos := 0;
  FSensitivity := 10;
  FDrawText := True;
  rTextLayout:= tlCenter;
  FDrawTicks := False;
  ApplyDefaultTheme;
  FBitmap := TBGRABitmap.Create(Width, Height, FBkgColor);
  FItems := TStringList.Create;
  FItems.Add('Item 1');
  FItems.Add('Item 2');
  FItems.Add('Item 3');
  TStringList(FItems).OnChange := @ItemsChanged;
  Font.Color := clBlack;
  Font.Height := 20;
end;

destructor TBCLeaSelector.Destroy;
begin
  FreeAndNil(FBitmap);
  TStringList(FItems).OnChange := nil;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TBCLeaSelector.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FDeltaPos := ((ClientHeight / FSensitivity) - (Y / FSensitivity)) * ((FTicksCount - 1) / ClientHeight);
    FSettingVerticalPos := True;
    FVerticalPos := FValue;
  end;
end;

procedure TBCLeaSelector.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
    FSettingVerticalPos := False;
end;

procedure TBCLeaSelector.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FSettingVerticalPos then
    UpdateVerticalPos(X, Y);
end;

procedure TBCLeaSelector.UpdateVerticalPos(X, Y: integer);
var
  FPreviousPos: single;
  FCurrPos: single;
begin
  FPreviousPos := FVerticalPos;
  FCurrPos := ((ClientHeight / FSensitivity) - (Y / FSensitivity)) * ((FTicksCount - 1) / ClientHeight);

  FVerticalPos := FVerticalPos - FDeltaPos + FCurrPos;
  if FVerticalPos < 0 then FVerticalPos := 0;
  if FVerticalPos > (FTicksCount - 1) then FVerticalPos := FTicksCount - 1;

  FValue := round(FVerticalPos);
  if FValue < 0 then
    FValue := 0;
  if FValue > (FTicksCount - 1) then
    FValue := FTicksCount - 1;

  Redraw;
  if (FPreviousPos <> FVerticalPos) and Assigned(FOnChangeValue) then
    FOnChangeValue(Self);
end;

procedure TBCLeaSelector.SetFFontShadowColor(AValue: TColor);
begin
  if FFontShadowColor = AValue then
    Exit;
  FFontShadowColor := AValue;
  Invalidate;
end;

procedure TBCLeaSelector.SetDrawText(AValue: boolean);
begin
  if FDrawText = AValue then Exit;
  FDrawText := AValue;
  Invalidate;
end;

procedure TBCLeaSelector.SetFFontShadowOffsetX(AValue: integer);
begin
  if FFontShadowOffsetX = AValue then
    Exit;
  FFontShadowOffsetX := AValue;
  Invalidate;
end;

procedure TBCLeaSelector.SetFFontShadowOffsetY(AValue: integer);
begin
  if FFontShadowOffsetY = AValue then
    Exit;
  FFontShadowOffsetY := AValue;
  Invalidate;
end;

procedure TBCLeaSelector.SetFFontShadowRadius(AValue: integer);
begin
  if FFontSHadowRadius = AValue then
    Exit;
  FFontSHadowRadius := AValue;
  Invalidate;
end;

procedure TBCLeaSelector.SetAltitude(AValue: integer);
begin
  if FAltitude = AValue then
    Exit;
  FAltitude := AValue;
  Invalidate;
end;

procedure TBCLeaSelector.SetBkgColor(AValue: TColor);
begin
  if FBkgColor = AValue then
    Exit;
  FBkgColor := AValue;
  Invalidate;
end;

procedure TBCLeaSelector.SetDrawTicks(AValue: boolean);
begin
  if FDrawTicks = AValue then
    Exit;
  FDrawTicks := AValue;
  Invalidate;
end;

procedure TBCLeaSelector.SetStyle(AValue: TZStyle);
begin
  if FStyle = AValue then
    Exit;
  FStyle := AValue;
  Invalidate;
end;

procedure TBCLeaSelector.SetDrawTextPhong(AValue: boolean);
begin
  if FDrawTextPhong = AValue then
    Exit;
  FDrawTextPhong := AValue;
  Invalidate;
end;

procedure TBCLeaSelector.SetItems(const Value: TStrings);
var
  i: integer;
begin
  FItems.Clear;
  for i := 0 to (FTicksCount - 1) do
  begin
    if i < Value.Count then
      FItems.Add(Value[i])
    else
      FItems.Add(' ');
  end;
  ItemsChanged(self);
end;

procedure TBCLeaSelector.ItemsChanged(Sender: TObject);
begin
  Invalidate;
  if Assigned(FOnChangeValue) then FOnChangeValue(self);
end;

procedure TBCLeaSelector.SetTheme(AValue: TBCLeaTheme);
begin
  if FTheme = AValue then
    Exit;
  if Assigned(FTheme) then
    FTheme := nil;
  FTheme := AValue;
  ApplyTheme;
end;

procedure TBCLeaSelector.UpdateTheme;
begin
  if Assigned(FTheme) then
  begin
    FTheme.SEL_LineWidth := FLineWidth;
    FTheme.SEL_LineColor := FLineColor;
    FTheme.SEL_LineBkgColor := FLineBkgColor;
    FTheme.SEL_BkgColor := FBkgColor;
    FTheme.SEL_FontShadowColor := FFontShadowColor;
    FTheme.SEL_FontShadowOffsetX := FFontShadowOffsetX;
    FTheme.SEL_FontShadowOffsetY := FFontShadowOffsetY;
    FTheme.SEL_FontShadowRadius := FFontShadowRadius;
    FTheme.SEL_PointerSize := FPointerSize;
    FTheme.SEL_Style := FStyle;
    FTheme.SEL_DrawTextPhong := FDrawTextPhong;
    FTheme.SEL_Altitude := FAltitude;
  end;
end;

procedure TBCLeaSelector.ApplyTheme;
begin
  if Assigned(FTheme) then
  begin
    FLineWidth := FTheme.SEL_LineWidth;
    FLineColor := FTheme.SEL_LineColor;
    FLineBkgColor := FTheme.SEL_LineBkgColor;
    FBkgColor := FTheme.SEL_BkgColor;
    FFontShadowColor := FTheme.SEL_FontShadowColor;
    FFontShadowOffsetX := FTheme.SEL_FontShadowOffsetX;
    FFontShadowOffsetY := FTheme.SEL_FontShadowOffsetY;
    FFontShadowRadius := FTheme.SEL_FontShadowRadius;
    FPointerSize := FTheme.SEL_PointerSize;
    FStyle := FTheme.SEL_Style;
    FDrawTextPhong := FTheme.SEL_DrawTextPhong;
    FAltitude := FTheme.SEL_Altitude;

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

procedure TBCLeaSelector.SaveThemeToFile(AFileName: string);
begin
  if Assigned(FTheme) then
    FTheme.SaveThemeToFile(AFileName);
end;

procedure TBCLeaSelector.LoadThemeFromFile(AFileName: string);
begin
  if Assigned(FTheme) then
    FTheme.LoadThemeFromFile(AFileName);
end;

procedure TBCLeaSelector.ApplyDefaultTheme;
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
