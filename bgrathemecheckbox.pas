// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAThemeCheckBox;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRATheme, Types;

type

  { TBGRAThemeCheckBox }

  TBGRAThemeCheckBox = class(TBGRAThemeControl)
  private
    FChecked: boolean;
    FOnChange: TNotifyEvent;
    FState: TBGRAThemeButtonState;
    procedure SetChecked(AValue: boolean);
  protected
    class function GetControlClassDefaultSize: TSize; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure Click; override;
    procedure SetEnabled(Value: boolean); override;
    procedure TextChanged; override;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Caption;
    property Checked: boolean read FChecked write SetChecked;
    property Font;
    property Enabled;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

implementation

uses BGRABitmapTypes;

procedure Register;
begin
  RegisterComponents('BGRA Themes', [TBGRAThemeCheckBox]);
end;

{ TBGRAThemeCheckBox }

procedure TBGRAThemeCheckBox.SetChecked(AValue: boolean);
begin
  if FChecked = AValue then
    Exit;
  FChecked := AValue;
  Invalidate;
  if Assigned(FOnChange) then FOnChange(Self);
end;

class function TBGRAThemeCheckBox.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 165;
  Result.CY := 19;
end;

procedure TBGRAThemeCheckBox.MouseEnter;
begin
  inherited MouseEnter;
  FState := btbsHover;
  Invalidate;
end;

procedure TBGRAThemeCheckBox.MouseLeave;
begin
  inherited MouseLeave;
  FState := btbsNormal;
  Invalidate;
end;

procedure TBGRAThemeCheckBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FState := btbsActive;
  Invalidate;
end;

procedure TBGRAThemeCheckBox.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if ClientRect.Contains(Point(X, Y)) then
    FState := btbsHover
  else
    FState := btbsNormal;
  if ClientRect.Contains(Point(X, Y)) then
    Checked := not FChecked
  else
    Invalidate;
end;

procedure TBGRAThemeCheckBox.Click;
begin
  inherited Click;
end;

procedure TBGRAThemeCheckBox.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  if Value then
    FState := btbsNormal
  else
    FState := btbsDisabled;
  Invalidate;
end;

procedure TBGRAThemeCheckBox.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
end;

procedure TBGRAThemeCheckBox.Paint;
var
  surface: TBGRAThemeSurface;
begin
  surface := TBGRAThemeSurface.Create(self);
  try
    if Assigned(Theme) then
      Theme.DrawCheckBox(Caption, FState, Focused, Checked, ClientRect, surface)
    else
      BGRADefaultTheme.DrawCheckBox(Caption, FState, Focused, Checked, ClientRect, surface);
  finally
    surface.Free;
  end;
end;

procedure TBGRAThemeCheckBox.Resize;
begin
  Invalidate;
  inherited Resize;
end;

constructor TBGRAThemeCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FState := btbsNormal;

  ControlStyle := ControlStyle + [csParentBackground];

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

end.
