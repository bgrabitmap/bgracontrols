unit ColorSpeedButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, BGRASpeedButton;

type

  { TColorState }

  TColorState = class(TPersistent)
  private
    FOwner: TControl;
    FBorderColor: TColor;
    FBorderWidth: integer;
    FColor: TColor;
    procedure SetFBorderColor(AValue: TColor);
    procedure SetFBorderWidth(AValue: integer);
    procedure SetFColor(AValue: TColor);
  public
    constructor Create(AOwner: TControl);
  published
    property Color: TColor read FColor write SetFColor;
    property BorderColor: TColor read FBorderColor write SetFBorderColor;
    property BorderWidth: integer read FBorderWidth write SetFBorderWidth;
  end;

  { TColorSpeedButton }

  TColorSpeedButton = class(TBGRASpeedButton)
  private
    FPopupMode: boolean;
    FStateActive: TColorState;
    FStateDisabled: TColorState;
    FStateHover: TColorState;
    FStateNormal: TColorState;
    procedure SetFPopupMode(AValue: boolean);
    procedure SetFStateActive(AValue: TColorState);
    procedure SetFStateDisabled(AValue: TColorState);
    procedure SetFStateHover(AValue: TColorState);
    procedure SetFStateNormal(AValue: TColorState);
  protected
    procedure PaintBackground(var PaintRect: TRect); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property PopupMode: boolean read FPopupMode write SetFPopupMode;
    property StateNormal: TColorState read FStateNormal write SetFStateNormal;
    property StateHover: TColorState read FStateHover write SetFStateHover;
    property StateActive: TColorState read FStateActive write SetFStateActive;
    property StateDisabled: TColorState read FStateDisabled write SetFStateDisabled;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TColorSpeedButton]);
end;

{ TColorSpeedButton }

procedure TColorSpeedButton.SetFStateActive(AValue: TColorState);
begin
  if FStateActive = AValue then
    Exit;
  FStateActive := AValue;
  Invalidate;
end;

procedure TColorSpeedButton.SetFPopupMode(AValue: boolean);
begin
  if FPopupMode = AValue then
    Exit;
  FPopupMode := AValue;
end;

procedure TColorSpeedButton.SetFStateDisabled(AValue: TColorState);
begin
  if FStateDisabled = AValue then
    Exit;
  FStateDisabled := AValue;
  Invalidate;
end;

procedure TColorSpeedButton.SetFStateHover(AValue: TColorState);
begin
  if FStateHover = AValue then
    Exit;
  FStateHover := AValue;
  Invalidate;
end;

procedure TColorSpeedButton.SetFStateNormal(AValue: TColorState);
begin
  if FStateNormal = AValue then
    Exit;
  FStateNormal := AValue;
  Invalidate;
end;

procedure TColorSpeedButton.PaintBackground(var PaintRect: TRect);
begin
  case FState of
    bsUp:
    begin
      Canvas.Pen.Color := FStateNormal.BorderColor;
      Canvas.Pen.Width := FStateNormal.BorderWidth;
      Canvas.Brush.Color := FStateNormal.Color;
    end;
    bsDisabled:
    begin
      Canvas.Pen.Color := FStateDisabled.BorderColor;
      Canvas.Pen.Width := FStateDisabled.BorderWidth;
      Canvas.Brush.Color := FStateDisabled.Color;
    end;
    bsDown, bsExclusive:
    begin
      Canvas.Pen.Color := FStateActive.BorderColor;
      Canvas.Pen.Width := FStateActive.BorderWidth;
      Canvas.Brush.Color := FStateActive.Color;
    end;
    bsHot:
    begin
      Canvas.Pen.Color := FStateHover.BorderColor;
      Canvas.Pen.Width := FStateHover.BorderWidth;
      Canvas.Brush.Color := FStateHover.Color;
    end;
  end;
  if Canvas.Pen.Width = 0 then
    Canvas.Pen.Color := Canvas.Brush.Color;
  Canvas.Rectangle(PaintRect);
end;

constructor TColorSpeedButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FStateNormal := TColorState.Create(Self);
  FStateHover := TColorState.Create(Self);
  FStateActive := TColorState.Create(Self);
  FStateDisabled := TColorState.Create(Self);
  { Windows Style }
  FStateNormal.Color := RGBToColor(225, 225, 225);
  FStateNormal.BorderColor := RGBToColor(173, 173, 173);
  FStateHover.Color := RGBToColor(229, 241, 251);
  FStateHover.BorderColor := RGBToColor(0, 120, 215);
  FStateActive.Color := RGBToColor(204, 228, 247);
  FStateActive.BorderColor := RGBToColor(0, 84, 153);
  FStateDisabled.Color := RGBToColor(204, 204, 204);
  FStateDisabled.Color := RGBToColor(191, 191, 191);
end;

destructor TColorSpeedButton.Destroy;
begin
  FStateNormal.Free;
  FStateHover.Free;
  FStateActive.Free;
  FStateDisabled.Free;
  inherited Destroy;
end;

procedure TColorSpeedButton.Click;
var
  p: TPoint;
begin
  if PopupMode then
  begin
    p := Parent.ClientToScreen(Point(Left, Top));
    PopupMenu.PopUp(p.x, p.y+Height);
  end;
  inherited Click;
end;

{ TColorState }

procedure TColorState.SetFBorderColor(AValue: TColor);
begin
  if FBorderColor = AValue then
    Exit;
  FBorderColor := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TColorState.SetFBorderWidth(AValue: integer);
begin
  if FBorderWidth = AValue then
    Exit;
  FBorderWidth := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TColorState.SetFColor(AValue: TColor);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

constructor TColorState.Create(AOwner: TControl);
begin
  inherited Create;
  FOwner := AOwner;
  BorderWidth := 1;
  BorderColor := clBlack;
  Color := clWhite;
end;

end.
