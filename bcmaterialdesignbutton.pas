unit BCMaterialDesignButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Controls, Graphics, ExtCtrls, BGRABitmap, BGRABitmapTypes,
  BGRATextFX;

type

  { TBCMaterialDesignButton }

  TBCMaterialDesignButton = class(TGraphicControl)
  private
    FNormalColor: TColor;
    FNormalColorEffect: TColor;
    FRoundBorders: single;
    FShadowColor: TColor;
    FShadowSize: integer;
    FTextColor: TColor;
    FTextFont: string;
    FTextQuality: TBGRAFontQuality;
    FTextShadowColor: TColor;
    FTextShadowOffsetX: integer;
    FTextShadowOffsetY: integer;
    FTextShadowSize: integer;
    FTextSize: integer;
    FTextStyle: TFontStyles;
    FTimer: TTimer;
    FBGRA: TBGRABitmap;
    FBGRAShadow: TBGRABitmap;
    FMousePos: TPoint;
    FCircleSize: single;
    FCircleAlpha: byte;
    procedure SetFNormalColor(AValue: TColor);
    procedure SetFNormalColorEffect(AValue: TColor);
    procedure SetFRoundBorders(AValue: single);
    procedure SetFShadowColor(AValue: TColor);
    procedure SetFShadowSize(AValue: integer);
    procedure SetFTextColor(AValue: TColor);
    procedure SetFTextFont(AValue: string);
    procedure SetFTextQuality(AValue: TBGRAFontQuality);
    procedure SetFTextShadowColor(AValue: TColor);
    procedure SetFTextShadowOffsetX(AValue: integer);
    procedure SetFTextShadowOffsetY(AValue: integer);
    procedure SetFTextShadowSize(AValue: integer);
    procedure SetFTextSize(AValue: integer);
    procedure SetFTextStyle(AValue: TFontStyles);
  protected
    procedure OnStartTimer(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure TextChanged; override;
    procedure UpdateShadow;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property RoundBorders: single read FRoundBorders write SetFRoundBorders default 5;
    property NormalColor: TColor read FNormalColor write SetFNormalColor default clWhite;
    property NormalColorEffect: TColor read FNormalColorEffect
      write SetFNormalColorEffect default clSilver;
    property ShadowColor: TColor read FShadowColor write SetFShadowColor default clGray;
    property ShadowSize: integer read FShadowSize write SetFShadowSize default 5;
    property TextColor: TColor read FTextColor write SetFTextColor default clBlack;
    property TextSize: integer read FTextSize write SetFTextSize default 16;
    property TextShadowColor: TColor read FTextShadowColor
      write SetFTextShadowColor default clBlack;
    property TextShadowSize: integer read FTextShadowSize
      write SetFTextShadowSize default 2;
    property TextShadowOffsetX: integer read FTextShadowOffsetX
      write SetFTextShadowOffsetX default 0;
    property TextShadowOffsetY: integer read FTextShadowOffsetY
      write SetFTextShadowOffsetY default 0;
    property TextStyle: TFontStyles read FTextStyle write SetFTextStyle default [];
    property TextFont: string read FTextFont write SetFTextFont;
    property TextQuality: TBGRAFontQuality read FTextQuality
      write SetFTextQuality default fqFineAntialiasing;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCMaterialDesignButton]);
end;

{ TBCMaterialDesignButton }

procedure TBCMaterialDesignButton.SetFRoundBorders(AValue: single);
begin
  if FRoundBorders = AValue then
    Exit;
  FRoundBorders := AValue;
  Invalidate;
end;

procedure TBCMaterialDesignButton.SetFShadowColor(AValue: TColor);
begin
  if FShadowColor = AValue then
    Exit;
  FShadowColor := AValue;
  UpdateShadow;
  Invalidate;
end;

procedure TBCMaterialDesignButton.SetFShadowSize(AValue: integer);
begin
  if FShadowSize = AValue then
    Exit;
  FShadowSize := AValue;
  UpdateShadow;
  Invalidate;
end;

procedure TBCMaterialDesignButton.SetFTextColor(AValue: TColor);
begin
  if FTextColor = AValue then
    Exit;
  FTextColor := AValue;
  Invalidate;
end;

procedure TBCMaterialDesignButton.SetFTextFont(AValue: string);
begin
  if FTextFont = AValue then
    Exit;
  FTextFont := AValue;
  Invalidate;
end;

procedure TBCMaterialDesignButton.SetFTextQuality(AValue: TBGRAFontQuality);
begin
  if FTextQuality=AValue then Exit;
  FTextQuality:=AValue;
  Invalidate;
end;

procedure TBCMaterialDesignButton.SetFTextShadowColor(AValue: TColor);
begin
  if FTextShadowColor = AValue then
    Exit;
  FTextShadowColor := AValue;
  UpdateShadow;
  Invalidate;
end;

procedure TBCMaterialDesignButton.SetFTextShadowOffsetX(AValue: integer);
begin
  if FTextShadowOffsetX = AValue then
    Exit;
  FTextShadowOffsetX := AValue;
  Invalidate;
end;

procedure TBCMaterialDesignButton.SetFTextShadowOffsetY(AValue: integer);
begin
  if FTextShadowOffsetY = AValue then
    Exit;
  FTextShadowOffsetY := AValue;
  Invalidate;
end;

procedure TBCMaterialDesignButton.SetFTextShadowSize(AValue: integer);
begin
  if FTextShadowSize = AValue then
    Exit;
  FTextShadowSize := AValue;
  Invalidate;
end;

procedure TBCMaterialDesignButton.SetFTextSize(AValue: integer);
begin
  if FTextSize = AValue then
    Exit;
  FTextSize := AValue;
  Invalidate;
end;

procedure TBCMaterialDesignButton.SetFTextStyle(AValue: TFontStyles);
begin
  if FTextStyle = AValue then
    Exit;
  FTextStyle := AValue;
  Invalidate;
end;

procedure TBCMaterialDesignButton.SetFNormalColor(AValue: TColor);
begin
  if FNormalColor = AValue then
    Exit;
  FNormalColor := AValue;
  Invalidate;
end;

procedure TBCMaterialDesignButton.SetFNormalColorEffect(AValue: TColor);
begin
  if FNormalColorEffect = AValue then
    Exit;
  FNormalColorEffect := AValue;
  Invalidate;
end;

procedure TBCMaterialDesignButton.OnStartTimer(Sender: TObject);
begin
  FCircleAlpha := 255;
  FCircleSize := 5;
end;

procedure TBCMaterialDesignButton.OnTimer(Sender: TObject);
begin
  FCircleSize := FCircleSize + 4;
  FCircleAlpha := FCircleAlpha - 5;
  if FCircleAlpha <= 0 then
    FTimer.Enabled := False;
  Invalidate;
end;

procedure TBCMaterialDesignButton.Paint;
var
  temp: TBGRABitmap;
begin
  if (FBGRA.Width <> Width) or (FBGRA.Height <> Height) then
  begin
    FBGRA.SetSize(Width, Height);
    FBGRAShadow.SetSize(Width, Height);
    UpdateShadow;
  end;

  FBGRA.FillTransparent;
  FBGRA.PutImage(0, 0, FBGRAShadow, dmDrawWithTransparency);

  temp := TBGRABitmap.Create(Width, Height, FNormalColor);
  temp.EllipseAntialias(FMousePos.X, FMousePos.Y, FCircleSize, FCircleSize,
    ColorToBGRA(FNormalColorEffect, FCircleAlpha), 1,
    ColorToBGRA(FNormalColorEffect, FCircleAlpha));
  FBGRA.FillRoundRectAntialias(FShadowSize, 0, Width - FShadowSize, Height - FShadowSize,
    FRoundBorders, FRoundBorders, temp, [rrDefault], False);
  temp.Free;

  if Caption <> '' then
  begin
    temp := TextShadow(Width, Height - FShadowSize, Caption, FTextSize,
      ColorToBGRA(FTextColor), ColorToBGRA(FTextShadowColor),
      FTextShadowOffsetX, FTextShadowOffsetY, FTextShadowSize,
      FTextStyle, FTextFont, True, FTextQuality) as TBGRABitmap;
    FBGRA.PutImage(0, 0, temp, dmDrawWithTransparency);
    temp.Free;
  end;

  FBGRA.Draw(Canvas, 0, 0, False);
end;

procedure TBCMaterialDesignButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FTimer.Enabled := False;
  FMousePos := Point(X, Y);
  FTimer.Enabled := True;
  inherited MouseDown(Button, Shift, X, Y);
end;

class function TBCMaterialDesignButton.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 123;
  Result.CY := 33;
end;

procedure TBCMaterialDesignButton.TextChanged;
begin
  Invalidate;
end;

procedure TBCMaterialDesignButton.UpdateShadow;
begin
  FBGRAShadow.FillTransparent;
  FBGRAShadow.RoundRectAntialias(FShadowSize, FShadowSize, Width - FShadowSize,
    Height - FShadowSize, FRoundBorders, FRoundBorders,
    ColorToBGRA(FShadowColor), 1, ColorToBGRA(FShadowColor), [rrDefault]);
  BGRAReplace(FBGRAShadow, FBGRAShadow.FilterBlurRadial(FShadowSize,
    FShadowSize, rbFast) as TBGRABitmap);
end;

constructor TBCMaterialDesignButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 15;
  FTimer.Enabled := False;
  FTimer.OnStartTimer := @OnStartTimer;
  FTimer.OnTimer := @OnTimer;
  FBGRA := TBGRABitmap.Create(Width, Height);
  FBGRAShadow := TBGRABitmap.Create(Width, Height);
  FRoundBorders := 5;
  FNormalColor := clWhite;
  FNormalColorEffect := clSilver;
  FShadowColor := clGray;
  FShadowSize := 5;
  FTextColor := clBlack;
  FTextSize := 16;
  FTextShadowColor := clBlack;
  FTextShadowSize := 2;
  FTextShadowOffsetX := 0;
  FTextShadowOffsetY := 0;
  FTextStyle := [];
  FTextFont := 'default';
  FTextQuality := fqFineAntialiasing;
end;

destructor TBCMaterialDesignButton.Destroy;
begin
  FTimer.Enabled := False;
  FTimer.OnStartTimer := nil;
  FTimer.OnTimer := nil;
  FreeAndNil(FBGRA);
  FreeAndNil(FBGRAShadow);
  inherited Destroy;
end;

end.
