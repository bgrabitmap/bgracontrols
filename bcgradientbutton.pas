// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BCGradientButton;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, BCTypes;

type

  { TBCGradientButton }

  TBCGradientButton = class(TGraphicControl)
  private
    FBorderColor: TBCPixel;
    FBorderSize: integer;
    FColor1: TBCPixel;
    FColor2: TBCPixel;
    FDimColor: TBCPixel;
    FLockHorizontal: boolean;
    FLockVertical: boolean;
    FOnAfterRedraw: TBGRARedrawEvent;
    FOnBeforeRedraw: TBGRARedrawEvent;
    Fx: integer;
    Fy: integer;
    Fdraw: boolean;
    Fupdating: boolean;
    Fdown: boolean;
    procedure ColorInvalidate({%H-}ASender: TObject; {%H-}AData: PtrInt);
    procedure SetBorderColor(AValue: TBCPixel);
    procedure SetBorderSize(AValue: integer);
    procedure SetColor1(AValue: TBCPixel);
    procedure SetColor2(AValue: TBCPixel);
    procedure SetDimColor(AValue: TBCPixel);
    procedure SetLockHorizontal(AValue: boolean);
    procedure SetLockVertical(AValue: boolean);
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property LockHorizontal: boolean read FLockHorizontal
      write SetLockHorizontal default False;
    property LockVertical: boolean
      read FLockVertical write SetLockVertical default False;
    property DimColor: TBCPixel read FDimColor write SetDimColor;
    property Color1: TBCPixel read FColor1 write SetColor1;
    property Color2: TBCPixel read FColor2 write SetColor2;
    property BorderColor: TBCPixel read FBorderColor write SetBorderColor;
    property BorderSize: integer read FBorderSize write SetBorderSize;
    property OnBeforeRedraw: TBGRARedrawEvent read FOnBeforeRedraw write FOnBeforeRedraw;
    property OnAfterRedraw: TBGRARedrawEvent read FOnAfterRedraw write FOnAfterRedraw;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Caption;
    property Enabled;
    property ShowHint;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Button Controls', [TBCGradientButton]);
end;

{ TBCGradientButton }

procedure TBCGradientButton.SetLockHorizontal(AValue: boolean);
begin
  if FLockHorizontal = AValue then
    Exit;
  FLockHorizontal := AValue;
  Invalidate;
end;

procedure TBCGradientButton.SetColor1(AValue: TBCPixel);
begin
  if FColor1 = AValue then
    Exit;
  FColor1 := AValue;
  Invalidate;
end;

procedure TBCGradientButton.SetBorderColor(AValue: TBCPixel);
begin
  if FBorderColor = AValue then
    Exit;
  FBorderColor := AValue;
  Invalidate;
end;

procedure TBCGradientButton.ColorInvalidate(ASender: TObject; AData: PtrInt);
begin
  Invalidate;
end;

procedure TBCGradientButton.SetBorderSize(AValue: integer);
begin
  if FBorderSize = AValue then
    Exit;
  FBorderSize := AValue;
  Invalidate;
end;

procedure TBCGradientButton.SetColor2(AValue: TBCPixel);
begin
  if FColor2 = AValue then
    Exit;
  FColor2 := AValue;
  Invalidate;
end;

procedure TBCGradientButton.SetDimColor(AValue: TBCPixel);
begin
  if FDimColor = AValue then
    Exit;
  FDimColor := AValue;
  Invalidate;
end;

procedure TBCGradientButton.SetLockVertical(AValue: boolean);
begin
  if FLockVertical = AValue then
    Exit;
  FLockVertical := AValue;
  Invalidate;
end;

procedure TBCGradientButton.Paint;
var
  bmp: TBGRABitmap;
  x, y: integer;
begin
  bmp := TBGRABitmap.Create(Width, Height);
  if Assigned(FOnBeforeRedraw) then
    FOnBeforeRedraw(Self, bmp);
  if Fdraw and Enabled then
  begin
    x := Fx;
    y := Fy;
    if FLockHorizontal then
      x := Width div 2;
    if FLockVertical then
      y := Height div 2;
    bmp.GradientFill(0, 0, Width, Height, FColor1.Pixel, FColor2.Pixel, gtRadial,
      PointF(x, y), PointF(x - Width, y), dmDrawWithTransparency);
    bmp.RectangleAntialias(0, 0, Width, Height, FBorderColor.Pixel,
      FBorderSize, BGRAPixelTransparent);
    if Fdown then
      bmp.Rectangle(0, 0, Width, Height, FDimColor.Pixel, FDimColor.Pixel,
        dmDrawWithTransparency);
  end;
  if Assigned(FOnAfterRedraw) then
    FOnAfterRedraw(Self, bmp);
  bmp.Draw(Canvas, 0, 0, False);
  bmp.Free;
end;

procedure TBCGradientButton.Invalidate;
begin
  if Fupdating then
    Exit;
  inherited Invalidate;
end;

procedure TBCGradientButton.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);
  Fx := X;
  Fy := Y;
  Fdraw := True;
  Invalidate;
end;

procedure TBCGradientButton.MouseLeave;
begin
  inherited MouseLeave;
  Fdraw := False;
  Fdown := False;
  Invalidate;
end;

procedure TBCGradientButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  Fdown := True;
  Invalidate;
end;

procedure TBCGradientButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  Fdown := False;
  Invalidate;
end;

constructor TBCGradientButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BeginUpdate;
  FLockHorizontal := False;
  FLockVertical := False;
  FColor1 := TBCPixel.Create(Self, BGRA(255, 255, 255, 100));
  FColor1.OnChange := ColorInvalidate;
  FColor2 := TBCPixel.Create(Self, BGRA(0, 0, 0, 0));
  FColor2.OnChange := ColorInvalidate;
  FBorderColor := TBCPixel.Create(Self, BGRA(255, 255, 255, 100));
  FBorderColor.OnChange := ColorInvalidate;
  FDimColor := TBCPixel.Create(Self, BGRA(0, 0, 0, 100));
  FDimColor.OnChange := ColorInvalidate;
  FBorderSize := 2;
  Fdown := False;
  EndUpdate;
end;

destructor TBCGradientButton.Destroy;
begin
  FColor1.Free;
  FColor2.Free;
  FBorderColor.Free;
  FDimColor.Free;
  inherited Destroy;
end;

procedure TBCGradientButton.BeginUpdate;
begin
  Fupdating := True;
end;

procedure TBCGradientButton.EndUpdate;
begin
  Fupdating := False;
  Invalidate;
end;

end.
