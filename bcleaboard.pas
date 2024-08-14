unit BCLeaBoard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Controls, Graphics,
  BGRABitmapTypes, BGRABitmap, BGRAGradients, BCLeaTheme, BCLeaTypes;

type
  TBCLeaBoard = class(TCustomControl)
  private
    FBitmap: TBGRABitmap;
    FTheme: TBCLeaTheme;
    FFrameColor: TColor;
    FBoardColor: TColor;
    FBkgColor: TColor;
    FFrameStyle: TZStyle;
    FBoardStyle: TZStyle;
    FAltitude: integer;
    FRounding: integer;
    FFrameHeight: integer;
    FFrameDistance: integer;
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
    procedure SetAltitude(AValue: integer);
    procedure SetFrameColor(AValue: TColor);
    procedure SetBoardColor(AValue: TColor);
    procedure SetBkgColor(AValue: TColor);
    procedure SetFrameStyle(AValue: TZStyle);
    procedure SetBoardStyle(AValue: TZStyle);
    procedure SetTheme(AValue: TBCLeaTheme);
    procedure SetRounding(AValue: integer);
    procedure SetFrameHeight(AValue: integer);
    procedure SetFrameDistance(AValue: integer);
  protected
    procedure SetEnabled(Value: boolean); override;
    procedure SetVisible(Value: boolean); override;
    procedure Paint; override;
    procedure Redraw;
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
    property Cursor;
    property Enabled;
    property Font;
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
    property OnContextPopup;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clBtnFace;
    property BoardColor: TColor read FBoardColor write SetBoardColor default clBtnFace;
    property BackgroundColor: TColor read FBkgColor write SetBkgColor default clBtnFace;
    property FrameStyle: TZStyle read FFrameStyle write SetFrameStyle default zsRaised;
    property BoardStyle: TZStyle read FBoardStyle write SetBoardStyle default zsFlat;
    property Theme: TBCLeaTheme read FTheme write SetTheme;
    property Altitude: integer read FAltitude write SetAltitude default 2;
    property Rounding: integer read FRounding write SetRounding default 10;
    property FrameHeight: integer read FFrameHeight write SetFrameHeight default 10;
    property FrameDistance: integer read FFrameDistance write SetFrameDistance default 3;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCLeaBoard]);
end;

constructor TBCLeaBoard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, 200, 150);
  ControlStyle := [csAcceptsControls, csReplicatable, csClickEvents];
  ApplyDefaultTheme;
  FBitmap := TBGRABitmap.Create(Width, Height, FBkgColor);
end;

destructor TBCLeaBoard.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TBCLeaBoard.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  Invalidate;
end;

procedure TBCLeaBoard.SetVisible(Value: boolean);
begin
  inherited SetVisible(Value);
  Invalidate;
end;

procedure TBCLeaBoard.Paint;
begin
  inherited Paint;
  Redraw;
end;

procedure TBCLeaBoard.SetFrameStyle(AValue: TZStyle);
begin
  if FFrameStyle = AValue then
    Exit;
  FFrameStyle := AValue;
  Invalidate;
end;

procedure TBCLeaBoard.SetBoardStyle(AValue: TZStyle);
begin
  if FBoardStyle = AValue then
    Exit;
  FBoardStyle := AValue;
  Invalidate;
end;

procedure TBCLeaBoard.SetFrameColor(AValue: TColor);
begin
  if FFrameColor = AValue then
    Exit;
  FFrameColor := AValue;
  Invalidate;
end;

procedure TBCLeaBoard.SetBoardColor(AValue: TColor);
begin
  if FBoardColor = AValue then
    Exit;
  FBoardColor := AValue;
  Invalidate;
end;

procedure TBCLeaBoard.SetBkgColor(AValue: TColor);
begin
  if FBkgColor = AValue then
    Exit;
  FBkgColor := AValue;
  Invalidate;
end;

procedure TBCLeaBoard.SetAltitude(AValue: integer);
begin
  if FAltitude = AValue then
    Exit;
  FAltitude := AValue;
  Invalidate;
end;

procedure TBCLeaBoard.SetRounding(AValue: integer);
begin
  if FRounding = AValue then
    Exit;
  FRounding := AValue;
  Invalidate;
end;

procedure TBCLeaBoard.SetTheme(AValue: TBCLeaTheme);
begin
  if FTheme = AValue then
    Exit;
  FTheme := AValue;
  Invalidate;
end;

procedure TBCLeaBoard.SetFrameDistance(AValue: integer);
begin
  if FFrameDistance = AValue then
    Exit;
  FFrameDistance := AValue;
  Invalidate;
end;

procedure TBCLeaBoard.SetFrameHeight(AValue: integer);
begin
  if FFrameHeight = AValue then
    Exit;
  FFrameHeight := AValue;
  Invalidate;
end;

procedure TBCLeaBoard.ApplyDefaultTheme;
begin
  FFrameColor := clBtnFace;
  FBoardColor := clBtnFace;
  FBkgColor := clBtnFace;
  FFrameStyle := zsRaised;
  FBoardStyle := zsFlat;
  FFrameHeight := 10;
  FFrameDistance := 3;
  FAltitude := 2;
  FRounding := 10;
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

procedure TBCLeaBoard.UpdateTheme;
begin
  if Assigned(FTheme) then
  begin
    FTheme.BRD_FrameColor := FFrameColor;
    FTheme.BRD_BoardColor := FBoardColor;
    FTheme.BRD_BkgColor := FBkgColor;
    FTheme.BRD_FrameStyle := FFrameStyle;
    FTheme.BRD_BoardStyle := FBoardStyle;
    FTheme.BRD_FrameHeight := FFrameHeight;
    FTheme.BRD_FrameDistance := FFrameDistance;
    FTheme.BRD_Altitude := FAltitude;
    FTheme.BRD_Rounding := FRounding;
  end;
end;

procedure TBCLeaBoard.ApplyTheme;
begin
  if Assigned(FTheme) then
  begin
    FFrameColor := FTheme.BRD_FrameColor;
    FBoardColor := FTheme.BRD_BoardColor;
    FBkgColor := FTheme.BRD_BkgColor;
    FFrameStyle := FTheme.BRD_FrameStyle;
    FBoardStyle := FTheme.BRD_BoardStyle;
    FFrameHeight := FTheme.BRD_FrameHeight;
    FFrameDistance := FTheme.BRD_FrameDistance;
    FAltitude := FTheme.BRD_Altitude;
    FRounding := FTheme.BRD_Rounding;
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

procedure TBCLeaBoard.SaveThemeToFile(AFileName: string);
begin
  if Assigned(FTheme) then
    FTheme.SaveThemeToFile(AFileName);
end;

procedure TBCLeaBoard.LoadThemeFromFile(AFileName: string);
begin
  if Assigned(FTheme) then
    FTheme.LoadThemeFromFile(AFileName);
end;

procedure TBCLeaBoard.Redraw;
var
  EffectiveSize: integer;
  Blur: TBGRABitmap;
  Mask, TmpBitmap: TBGRABitmap;
  Phong: TPhongShading;
  ScaledPhongSize{, ScaledSize}: integer;

  procedure DoDrawFrame(AFrameColor, ABoardColor: TColor);
  var
    d: integer;
  begin
    d := FFrameDistance;
    FBitmap.FillRoundRectAntialias(d, d, Width - d, Height - d, FRounding, FRounding, AFrameColor);

    d := FFrameDistance + FFrameHeight + FAltitude;
    FBitmap.FillRoundRectAntialias(d, d, Width - d, Height - d, FRounding, FRounding, ABoardColor);

    if (FFrameStyle = zsRaised) or (FFrameStyle = zsLowered) then
    begin
      Mask := FBitmap.FilterGrayscale as TBGRABitmap;
      if (FFrameStyle = zsRaised) then
        Mask.Negative;
      Blur := Mask.FilterBlurRadial(ScaledPhongSize, ScaledPhongSize, rbFast) as TBGRABitmap;
      Blur.FillMask(0, 0, Mask, BGRAPixelTransparent, dmSet);
      Mask.Free;

      Phong := TPhongShading.Create;
      if assigned(FTheme) then
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
      Mask := TBGRABitmap.Create(Width, Height, ColorToBGRA(ColorToRGB(BGRABlack)));
      d := FFrameDistance;
      Mask.FillRoundRectAntialias(d, d, Width - d, Height - d, FRounding, FRounding, BGRAWhite);
      d := FFrameDistance + FFrameHeight + FAltitude;
      FBitmap.FillRoundRectAntialias(d, d, Width - d, Height - d, FRounding, FRounding, BGRABlack);
      TmpBitmap := TBGRABitmap.Create(Width, Height, ColorToBGRA(ColorToRGB(BGRABlack)));
      Phong.Draw(TmpBitmap, Blur, FAltitude, 0, 0, FBitmap);
      Phong.Free;
      Blur.Free;
      TmpBitmap.ApplyMask(Mask);
      FBitmap.PutImage(0,0,TmpBitmap, dmDrawWithTransparency);
      TmpBitmap.Free;
      Mask.Free;
    end;
  end;

  procedure DoDrawBoard(AValue: TColor);
  var
    d: integer;
  begin
    d := FFrameDistance + FFrameHeight + FAltitude;
    FBitmap.FillRoundRectAntialias(d, d, Width - d, Height - d, FRounding, FRounding, AValue);
  end;

begin
  FBitmap.SetSize(Width, Height);
  FBitmap.Fill(FBkgColor);

  if Width < Height then
    EffectiveSize := Width
  else
    EffectiveSize := Height;
  if EffectiveSize < 2 then exit;
  //ScaledSize := Scale96ToForm(FSize);
  ScaledPhongSize := Scale96ToForm(5);

  if Enabled then
  begin
    DoDrawFrame(FFrameColor, FBoardColor);
    if FBoardStyle = zsFlat then
      DoDrawBoard(FBoardColor);
  end
  else
  begin
    DoDrawFrame(clDkGray, clGray);
    if FBoardStyle = zsFlat then
      DoDrawBoard(clGray);
  end;

  FBitmap.Draw(Canvas, 0, 0, True);
end;

end.
