unit BGRAGraphicControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, BGRABitmap, BCTypes;

type

  { TBGRAGraphicControl }

  TBGRAGraphicControl = class(TGraphicControl)
  private
    { Private declarations }
    FBGRA:         TBGRABitmap;
    FOnRedraw:     TBGRARedrawEvent;
    FBevelInner, FBevelOuter: TPanelBevel;
    FBevelWidth:   TBevelWidth;
    FBorderWidth:  TBorderWidth;
    FAlignment:    TAlignment;
    FColorOpacity: byte;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetBevelInner(const AValue: TPanelBevel);
    procedure SetBevelOuter(const AValue: TPanelBevel);
    procedure SetBevelWidth(const AValue: TBevelWidth);
    procedure SetBorderWidth(const AValue: TBorderWidth);
    procedure SetColorOpacity(const AValue: byte);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure Resize; override;
    procedure BGRASetSize(AWidth, AHeight: integer);
    procedure RedrawBitmapContent; virtual;
    procedure SetColor(Value: TColor); override;
    procedure SetEnabled(Value: boolean); override;
    procedure TextChanged; override;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure RedrawBitmap;
    procedure DiscardBitmap;
    destructor Destroy; override;
  published
    { Published declarations }
    property OnRedraw: TBGRARedrawEvent Read FOnRedraw Write FOnRedraw;
    property Bitmap: TBGRABitmap Read FBGRA;
    property BorderWidth: TBorderWidth Read FBorderWidth Write SetBorderWidth default 0;
    property BevelInner: TPanelBevel Read FBevelInner Write SetBevelInner default bvNone;
    property BevelOuter: TPanelBevel
      Read FBevelOuter Write SetBevelOuter default bvRaised;
    property BevelWidth: TBevelWidth Read FBevelWidth Write SetBevelWidth default 1;
    property Color;
    property ColorOpacity: byte Read FColorOpacity Write SetColorOpacity;
    property Alignment: TAlignment Read FAlignment Write SetAlignment;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property Caption;
  end;

procedure Register;

implementation

uses BGRABitmapTypes, Types;

procedure Register;
begin
  {$I bgragraphiccontrol_icon.lrs}
  RegisterComponents('BGRA Controls', [TBGRAGraphicControl]);
end;

procedure TBGRAGraphicControl.SetAlignment(const Value: TAlignment);
begin
  if FAlignment = Value then
    exit;
  FAlignment := Value;
  DiscardBitmap;
end;

procedure TBGRAGraphicControl.SetBevelInner(const AValue: TPanelBevel);
begin
  if FBevelInner = AValue then
    exit;
  FBevelInner := AValue;
  DiscardBitmap;
end;

procedure TBGRAGraphicControl.SetBevelOuter(const AValue: TPanelBevel);
begin
  if FBevelOuter = AValue then
    exit;
  FBevelOuter := AValue;
  DiscardBitmap;
end;

procedure TBGRAGraphicControl.SetBevelWidth(const AValue: TBevelWidth);
begin
  if FBevelWidth = AValue then
    exit;
  FBevelWidth := AValue;
  DiscardBitmap;
end;

procedure TBGRAGraphicControl.SetBorderWidth(const AValue: TBorderWidth);
begin
  if FBorderWidth = AValue then
    exit;
  FBorderWidth := AValue;
  DiscardBitmap;
end;

procedure TBGRAGraphicControl.SetColorOpacity(const AValue: byte);
begin
  if FColorOpacity = AValue then
    exit;
  FColorOpacity := AValue;
  DiscardBitmap;
end;

procedure TBGRAGraphicControl.Paint;
begin
  BGRASetSize(Width, Height);
  inherited Paint;
  FBGRA.Draw(Canvas, 0, 0, False);
end;

procedure TBGRAGraphicControl.Resize;
begin
  inherited Resize;
  DiscardBitmap;
end;

procedure TBGRAGraphicControl.BGRASetSize(AWidth, AHeight: integer);
begin
  if (FBGRA <> nil) and (AWidth <> FBGRA.Width) and (AHeight <> FBGRA.Height) then
  begin
    FBGRA.SetSize(AWidth, AHeight);
    RedrawBitmapContent;
  end;
end;

procedure TBGRAGraphicControl.RedrawBitmapContent;
var
  ARect: TRect;
  TS: TTextStyle;
begin
  if (FBGRA <> nil) and (FBGRA.NbPixels <> 0) then
  begin
    FBGRA.Fill(ColorToBGRA(ColorToRGB(Color), FColorOpacity));

    ARect := GetClientRect;

    // if BevelOuter is set then draw a frame with BevelWidth
    if (BevelOuter <> bvNone) and (BevelWidth > 0) then
      FBGRA.CanvasBGRA.Frame3d(ARect, BevelWidth, BevelOuter,
        BGRA(255, 255, 255, 200), BGRA(0, 0, 0, 160)); // Note: Frame3D inflates ARect

    InflateRect(ARect, -BorderWidth, -BorderWidth);

    // if BevelInner is set then skip the BorderWidth and draw a frame with BevelWidth
    if (BevelInner <> bvNone) and (BevelWidth > 0) then
      FBGRA.CanvasBGRA.Frame3d(ARect, BevelWidth, BevelInner,
        BGRA(255, 255, 255, 160), BGRA(0, 0, 0, 160)); // Note: Frame3D inflates ARect

    if Caption <> '' then
    begin
      FBGRA.CanvasBGRA.Font.Assign(Canvas.Font);
      TS := Canvas.TextStyle;
      TS.Alignment := Alignment;
      TS.Layout := tlCenter;
      TS.Opaque := False;
      TS.Clipping := False;
      TS.SystemFont := Canvas.Font.IsDefault;

      FBGRA.CanvasBGRA.Font.Color := Color xor $FFFFFF;
      FBGRA.CanvasBGRA.Font.Opacity := 255;

      if not Enabled then
        FBGRA.CanvasBGRA.Font.Style := [fsStrikeOut]
      else
        FBGRA.CanvasBGRA.Font.Style := [];

      FBGRA.CanvasBGRA.TextRect(ARect, ARect.Left, ARect.Top, Caption, TS);
    end;

    if Assigned(FOnRedraw) then
      FOnRedraw(self, FBGRA);
  end;
end;

procedure TBGRAGraphicControl.SetColor(Value: TColor);
begin
  if Value <> Color then
    DiscardBitmap;
  inherited SetColor(Value);
end;

procedure TBGRAGraphicControl.SetEnabled(Value: boolean);
begin
  if Value <> Enabled then
    DiscardBitmap;
  inherited SetEnabled(Value);
end;

procedure TBGRAGraphicControl.TextChanged;
begin
  DiscardBitmap;
end;

constructor TBGRAGraphicControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBGRA := TBGRABitmap.Create;
  FBevelWidth := 1;
  FAlignment := taCenter;
  Color := clWhite;
  FColorOpacity := 128;
  FBevelOuter := bvRaised;
  FBevelInner := bvNone;
end;

procedure TBGRAGraphicControl.RedrawBitmap;
begin
  RedrawBitmapContent;
  Repaint;
end;

procedure TBGRAGraphicControl.DiscardBitmap;
begin
  if (FBGRA <> nil) and (FBGRA.NbPixels <> 0) then
  begin
    FBGRA.SetSize(0, 0);
    Invalidate;
  end;
end;

destructor TBGRAGraphicControl.Destroy;
begin
  FBGRA.Free;
  inherited Destroy;
end;

end.
