{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Author: Boban Spasic
 Credits to: circular from Lazarus forum
}

unit BCLeaQLED;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, LResources, Graphics,
  BGRABitmapTypes, BGRABitmap, BGRAGradients, BCLeaTheme, BCLeaTypes;

type
  TBCLeaQLED = class(TCustomControl)
  private
    FBitmap: TBGRABitmap;
    FTheme: TBCLeaTheme;
    FOnChangeValue: TNotifyEvent;
    FValue: boolean;
    FColorOn: TColor;
    FColorOff: TColor;
    FBkgColor: TColor;
    FStyle: TZStyle;
    FSize: integer;
    FAltitude: integer;
    FClickable: boolean;
    FRounding: integer;
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
    procedure SetValue(AValue: boolean);
    procedure SetAltitude(AValue: integer);
    procedure SetColorOn(AValue: TColor);
    procedure SetColorOff(AValue: TColor);
    procedure SetBkgColor(AValue: TColor);
    procedure SetStyle(AValue: TZStyle);
    procedure SetSize(AValue: integer);
    procedure SetClickable(AValue: boolean);
    procedure SetTheme(AValue: TBCLeaTheme);
    procedure SetRounding(AValue: integer);
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
    property OnContextPopup;
    property Value: boolean read FValue write SetValue default False;
    property ColorOn: TColor read FColorOn write SetColorOn default TColor($00FF9C15);
    property ColorOff: TColor read FColorOff write SetColorOff default TColor($009E5A00);
    property BackgroundColor: TColor read FBkgColor write SetBkgColor default clBtnFace;
    property Size: integer read FSize write SetSize default 30;
    property OnChangeValue: TNotifyEvent read FOnChangeValue write FOnChangeValue;
    property Style: TZStyle read FStyle write SetStyle default zsRaised;
    property Clickable: boolean read FClickable write SetClickable default False;
    property Theme: TBCLeaTheme read FTheme write SetTheme;
    property Altitude: integer read FAltitude write SetAltitude default 2;
    property Rounding: integer read FRounding write SetRounding default 3;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCLeaQLED]);
end;

constructor TBCLeaQLED.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, 50, 50);
  FValue := False;
  ApplyDefaultTheme;
  FBitmap := TBGRABitmap.Create(Width, Height, FBkgColor);
  FClickable := False;
end;

destructor TBCLeaQLED.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TBCLeaQLED.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  Invalidate;
end;

procedure TBCLeaQLED.SetVisible(Value: boolean);
begin
  inherited SetVisible(Value);
  Invalidate;
end;

procedure TBCLeaQLED.Paint;
begin
  inherited Paint;
  Redraw;
end;

procedure TBCLeaQLED.Resize;
begin
  inherited Resize;
  {$IFDEF LCLgtk2} Invalidate; {$ENDIF}
end;

procedure TBCLeaQLED.SetStyle(AValue: TZStyle);
begin
  if FStyle = AValue then
    Exit;
  FStyle := AValue;
  Invalidate;
end;

procedure TBCLeaQLED.SetValue(AValue: boolean);
begin
  if FValue = AValue then
    exit;
  FValue := AValue;
  Invalidate;
end;

procedure TBCLeaQLED.SetSize(AValue: integer);
begin
  if FSize = AValue then
    exit;
  FSize := AValue;
  if FSize < 1 then FSize := 1;
  Invalidate;
end;

procedure TBCLeaQLED.SetColorOn(AValue: TColor);
begin
  if FColorOn = AValue then
    Exit;
  FColorOn := AValue;
  Invalidate;
end;

procedure TBCLeaQLED.SetColorOff(AValue: TColor);
begin
  if FColorOff = AValue then
    Exit;
  FColorOff := AValue;
  Invalidate;
end;

procedure TBCLeaQLED.SetBkgColor(AValue: TColor);
begin
  if FBkgColor = AValue then
    Exit;
  FBkgColor := AValue;
  Invalidate;
end;

procedure TBCLeaQLED.SetAltitude(AValue: integer);
begin
  if FAltitude = AValue then
    Exit;
  FAltitude := AValue;
  Invalidate;
end;

procedure TBCLeaQLED.SetRounding(AValue: integer);
begin
  if FRounding = AValue then
    Exit;
  FRounding := AValue;
  Invalidate;
end;

procedure TBCLeaQLED.SetClickable(AValue: boolean);
begin
  if FClickable = AValue then
    Exit;
  FClickable := AValue;
  Invalidate;
end;

procedure TBCLeaQLED.SetTheme(AValue: TBCLeaTheme);
begin
  if FTheme = AValue then
    Exit;
  if Assigned(FTheme) then
    FTheme := nil;
  FTheme := AValue;
  ApplyTheme;
end;

procedure TBCLeaQLED.UpdateTheme;
begin
  if Assigned(FTheme) then
  begin
    FTheme.QLED_ColorOn := FColorOn;
    FTheme.QLED_ColorOff := FColorOff;
    FTheme.QLED_BkgColor := FBkgColor;
    FTheme.QLED_Style := FStyle;
    FTheme.QLED_Size := FSize;
    FTheme.QLED_Altitude := FAltitude;
    FTheme.QLED_Rounding := FRounding;
  end;
end;

procedure TBCLeaQLED.ApplyTheme;
begin
  if Assigned(FTheme) then
  begin
    FColorOn := FTheme.QLED_ColorOn;
    FColorOff := FTheme.QLED_ColorOff;
    FBkgColor := FTheme.QLED_BkgColor;
    FStyle := FTheme.QLED_Style;
    FSize := FTheme.QLED_Size;
    FAltitude := FTHeme.QLED_Altitude;
    FRounding := FTheme.QLED_Rounding;
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

procedure TBCLeaQLED.SaveThemeToFile(AFileName: string);
begin
  if Assigned(FTheme) then
    FTheme.SaveThemeToFile(AFileName);
end;

procedure TBCLeaQLED.LoadThemeFromFile(AFileName: string);
begin
  if Assigned(FTheme) then
    FTheme.LoadThemeFromFile(AFileName);
end;

procedure TBCLeaQLED.ApplyDefaultTheme;
begin
  FColorOn := TColor($00FF9C15);
  FColorOff := TColor($009E5A00);
  FBkgColor := clBtnFace;
  FStyle := zsRaised;
  FSize := 30;
  FAltitude := 2;
  FRounding := 3;
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

procedure TBCLeaQLED.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if FClickable and (Button = mbLeft) then
  begin
    FValue := not FValue;
    Redraw;
    if Assigned(FOnChangeValue) then
      FOnChangeValue(Self);
  end;
end;

procedure TBCLeaQLED.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TBCLeaQLED.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

procedure TBCLeaQLED.Redraw;
var
  Blur: TBGRABitmap;
  Mask, Mask2: TBGRABitmap;
  Phong: TPhongShading;
  ScaledPhongSize, ScaledBlurSize, ScaledSize: integer;
  img: TBGRABitmap;
  imgSize: integer;
  Margin: integer;
begin
  FBitmap.SetSize(Width, Height);
  FBitmap.Fill(FBkgColor);

  if (Width < 2) or (Height < 2) then Exit;
  ScaledSize := Scale96ToForm(FSize);
  ScaledPhongSize := Scale96ToForm(5);
  ScaledBlurSize := Scale96ToForm(10);
  Margin := ScaledBlurSize;

  imgSize := ScaledSize + 2*Margin;
  img := TBGRABitmap.Create(imgSize, imgSize, ColorToBGRA(ColorToRGB(FBkgColor)));

  if Enabled then
  begin
    if FValue then
      img.FillRoundRectAntialias(Margin, Margin, Margin+ScaledSize, Margin+ScaledSize, FRounding, FRounding, FColorOn)
    else
      img.FillRoundRectAntialias(Margin, Margin, Margin+ScaledSize, Margin+ScaledSize, FRounding, FRounding, FColorOff);
  end else
    img.FillRoundRectAntialias(Margin, Margin, Margin+ScaledSize, Margin+ScaledSize, FRounding, FRounding, clGray);

  if (FStyle = zsRaised) or (FStyle = zsLowered) then
  begin
    Mask := img.FilterGrayscale as TBGRABitmap;
    if (FStyle = zsRaised) then
      Mask.Negative;
    Blur := Mask.FilterBlurRadial(ScaledPhongSize, ScaledPhongSize, rbFast) as TBGRABitmap;
    Blur.FillMask(0, 0, Mask, BGRAPixelTransparent, dmSet);
    Mask.Free;

    Phong := TPhongShading.Create;
    begin
      Phong.AmbientFactor := FAmbientFactor;
      Phong.SpecularIndex := FSpecularIndex;
      Phong.LightDestFactor := FLightDestFactor;
      Phong.LightPosition := Point(FLightPositionX, FLightPositionY);
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
    Phong.Draw(img, Blur, FAltitude, 0, 0, img);
    Phong.Free;
    Blur.Free;

    Mask := TBGRABitmap.Create(imgSize, imgSize, BGRABlack);
    Mask.FillRoundRectAntialias(Margin, Margin, Margin+ScaledSize, Margin+ScaledSize, FRounding, FRounding, BGRAWhite);
    Mask2 := TBGRABitmap.Create(imgSize, imgSize, ColorToBGRA(ColorToRGB(FBkgColor)));
    Mask2.PutImage(0, 0, img, dmSet);
    Mask2.ApplyMask(Mask);
    Mask.Free;
    FBitmap.PutImage((FBitmap.Width - imgSize) div 2, (FBitmap.Height - imgSize) div 2, Mask2, dmDrawWithTransparency);
    Mask2.Free;
  end
  else
  begin
    Mask := TBGRABitmap.Create(imgSize, imgSize, BGRABlack);
    Mask.FillRoundRectAntialias(Margin, Margin, Margin+ScaledSize, Margin+ScaledSize, FRounding, FRounding, BGRAWhite);
    Mask2 := TBGRABitmap.Create(imgSize, imgSize, ColorToBGRA(ColorToRGB(FBkgColor)));
    Mask2.PutImage(0, 0, img, dmSet);
    Mask2.ApplyMask(Mask);
    Mask.Free;
    FBitmap.PutImage((FBitmap.Width-imgSize) div 2, (FBitmap.Height-imgSize) div 2, Mask2, dmDrawWithTransparency);
    Mask2.Free;
  end;
  img.Free;

  if FValue then
  begin
    Mask := TBGRABitmap.Create(imgSize, imgSize);
    Mask.FillRoundRectAntialias(Margin, Margin, Margin+ScaledSize, Margin+ScaledSize, FRounding, FRounding, FColorOn);
    Mask := Mask.FilterBlurRadial(ScaledBlurSize, ScaledBlurSize, rbFast);
    FBitmap.BlendImageOver((FBitmap.Width-imgSize) div 2, (FBitmap.Height-imgSize) div 2, Mask, boGlow);
    Mask.Free;
  end;

  FBitmap.Draw(Canvas, 0, 0, True);
end;

end.
