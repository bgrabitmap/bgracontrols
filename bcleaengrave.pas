{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Author: Boban Spasic
}

unit BCLeaEngrave;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls, LResources,
  BGRABitmapTypes, BGRABitmap, BGRATextFX, BGRATransform;

type
  TBCLeaEngrave = class(TCustomControl)
  private
    FBitmap: TBGRABitmap;
    FFontShadowColor: TColor;
    FFontShadowOffsetX: integer;
    FFontShadowOffsetY: integer;
    FFontShadowRadius: integer;
    FLayerZoom: integer;
    FLayerOffsetX: integer;
    FLayerOffsetY: integer;
    FBlendOperation: TBlendOperation;
    FInvBlending: boolean;
    FLayerColor: TColor;
    FAngle: integer;
    procedure SetFFontShadowColor(AValue: TColor);
    procedure SetFFontShadowOffsetX(AValue: integer);
    procedure SetFFontShadowOffsetY(AValue: integer);
    procedure SetFFontShadowRadius(AValue: integer);
    procedure SetLayerZoom(AValue: integer);
    procedure SetBlendOperation(AValue: TBlendOperation);
    procedure SetInvBlending(AValue: boolean);
    procedure SetLayerColor(AValue: TColor);
    procedure SetLayerOffsetX(AValue: integer);
    procedure SetLayerOffsetY(AValue: integer);
    procedure SetAngle(AValue: integer);
  protected
    procedure SetVisible(Value: boolean); override;
    procedure Paint; override;
    procedure Redraw;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Align;
    property Caption;
    property Color default clBtnFace;
    property Cursor;
    property Font;
    property ParentShowHint;
    property ShowHint;
    property Anchors;
    property Constraints;
    property Visible;
    property FontShadowColor: TColor read FFontShadowColor write SetFFontShadowColor default clBlack;
    property FontShadowOffsetX: integer read FFontShadowOffsetX write SetFFontShadowOffsetX default 2;
    property FontShadowOffsetY: integer read FFontShadowOffsetY write SetFFontShadowOffsetY default 2;
    property FontShadowRadius: integer read FFontSHadowRadius write SetFFontShadowRadius default 4;
    property BlendOperation: TBlendOperation read FBlendOperation write SetBlendOperation default boSubtractInverse;
    property LayerZoom: integer read FLayerZoom write SetLayerZoom default 102;
    property InverseBlending: boolean read FInvBlending write SetInvBlending default False;
    property LayerColor: TColor read FLayerColor write SetLayerColor default clBlack;
    property LayerOffsetX: integer read FLayerOffsetX write SetLayerOffsetX default 0;
    property LayerOffsetY: integer read FLayerOffsetY write SetLayerOffsetY default 0;
    property Angle: integer read FAngle write SetAngle default 0;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCLeaEngrave]);
end;

constructor TBCLeaEngrave.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, 100, 100);
  Font.Color := clBlack;
  Font.Height := 20;
  FLayerZoom := 102;
  FBlendOperation := boSubtractInverse;
  FFontShadowRadius := 4;
  FFontShadowOffsetX := 2;
  FFontShadowOffsetY := 2;
  FFontShadowColor := clBlack;
  Color := clBtnFace;
  FLayerColor := clBlack;
  FLayerOffsetX := 0;
  FLayerOffsetY := 0;
  FAngle := 0;
  FInvBlending := False;
  FBitmap := TBGRABitmap.Create(Width, Height, Color);
end;

destructor TBCLeaEngrave.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TBCLeaEngrave.SetVisible(Value: boolean);
begin
  inherited SetVisible(Value);
  Invalidate;
end;

procedure TBCLeaEngrave.Paint;
begin
  inherited Paint;
  Redraw;
end;

procedure TBCLeaEngrave.Redraw;
var
  TextBmp, LayerTextBmp: TBGRABitmap;
  Zoom, UnZoom, RotBmp: TBGRABitmap;
  CopyRect: TRect;
  ZFactor: single;
  Rad: single;
  Affine: TBGRAAffineBitmapTransform;
begin
  ZFactor := FLayerZoom / 100;
  FBitmap.SetSize(Width, Height);
  FBitmap.Fill(Color);

  TextBmp := TextShadow(Width, Height, Caption, Font.Height,
    Font.Color, FontShadowColor, FontShadowOFfsetX,
    FontShadowOffsetY, FontShadowRadius, Font.Style, Font.Name) as TBGRABitmap;
  LayerTextBmp := TextShadow(Width, Height, Caption, Font.Height,
    LayerColor, FontShadowColor, FontShadowOFfsetX,
    FontShadowOffsetY, FontShadowRadius, Font.Style, Font.Name) as TBGRABitmap;
  Zoom := LayerTextBmp.Resample(round(Width * ZFactor), round(Height * ZFactor)) as TBGRABitmap;
  CopyRect.Left := (Zoom.Width - Width) div 2 + FLayerOffsetX;
  CopyRect.Right := CopyRect.Left + Width + FLayerOffsetX;
  CopyRect.Top := (Zoom.Height - Height) div 2 + FLayerOffsetY;
  CopyRect.Bottom := CopyRect.Top + Height + FLayerOffsetY;
  UnZoom := Zoom.GetPart(CopyRect) as TBGRABitmap;
  if FInvBlending then
  begin
    FBitmap.PutImage(0, 0, UnZoom, dmDrawWithTransparency);
    FBitmap.BlendImageOver(0, 0, TextBmp, FBlendOperation);
  end
  else
  begin
    FBitmap.PutImage(0, 0, TextBmp, dmDrawWithTransparency);
    FBitmap.BlendImageOver(0, 0, UnZoom, FBlendOperation);
  end;
  if FAngle <> 0 then
  begin
    RotBmp := TBGRABitmap.Create(Width, Height, Color);
    Affine := TBGRAAffineBitmapTransform.Create(FBitmap);
    Affine.Translate(-FBitmap.Width div 2, -FBitmap.Height div 2);
    Affine.RotateDeg(FAngle);
    Affine.Translate(FBitmap.Width div 2, FBitmap.Height div 2);
    RotBmp.Fill(Affine);
    FBitmap.Fill(Color);
    FBitmap.PutImage(0, 0, RotBmp, dmDrawWithTransparency);
    Affine.Free;
    RotBmp.Free;
  end;

  if Visible then
    FBitmap.Draw(Canvas, 0, 0, True);
  TextBmp.Free;
  LayerTextBmp.Free;
  Zoom.Free;
  UnZoom.Free;
end;

procedure TBCLeaEngrave.SetFFontShadowColor(AValue: TColor);
begin
  if FFontShadowColor = AValue then
    Exit;
  FFontShadowColor := AValue;
  Invalidate;
end;

procedure TBCLeaEngrave.SetFFontShadowOffsetX(AValue: integer);
begin
  if FFontShadowOffsetX = AValue then
    Exit;
  FFontShadowOffsetX := AValue;
  Invalidate;
end;

procedure TBCLeaEngrave.SetFFontShadowOffsetY(AValue: integer);
begin
  if FFontShadowOffsetY = AValue then
    Exit;
  FFontShadowOffsetY := AValue;
  Invalidate;
end;

procedure TBCLeaEngrave.SetFFontShadowRadius(AValue: integer);
begin
  if FFontSHadowRadius = AValue then
    Exit;
  FFontSHadowRadius := AValue;
  Invalidate;
end;

procedure TBCLeaEngrave.SetBlendOperation(AValue: TBlendOperation);
begin
  if FBlendOperation = AValue then
    Exit;
  FBlendOperation := AValue;
  Invalidate;
end;

procedure TBCLeaEngrave.SetLayerZoom(AValue: integer);
begin
  if FLayerZoom = AValue then
    Exit;
  FLayerZoom := AValue;
  Invalidate;
end;

procedure TBCLeaEngrave.SetInvBlending(AValue: boolean);
begin
  if FInvBlending = AValue then
    Exit;
  FInvBlending := AValue;
  Invalidate;
end;

procedure TBCLeaEngrave.SetLayerColor(AValue: TColor);
begin
  if FLayerColor = AValue then
    Exit;
  FLayerColor := AValue;
  Invalidate;
end;

procedure TBCLeaEngrave.SetLayerOffsetX(AValue: integer);
begin
  if FLayerOffsetX = AValue then
    Exit;
  FLayerOffsetX := AValue;
  Invalidate;
end;

procedure TBCLeaEngrave.SetLayerOffsetY(AValue: integer);
begin
  if FLayerOffsetY = AValue then
    Exit;
  FLayerOffsetY := AValue;
  Invalidate;
end;

procedure TBCLeaEngrave.SetAngle(AValue: integer);
begin
  if FAngle = AValue then
    Exit;
  FAngle := AValue;
  Invalidate;
end;

end.
