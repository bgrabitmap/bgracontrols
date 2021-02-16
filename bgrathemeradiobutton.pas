// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAThemeRadioButton;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRATheme, Types;

type

  { TBGRAThemeRadioButton }

  TBGRAThemeRadioButton = class(TBGRAThemeControl)
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
    procedure UncheckOthers;
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
  RegisterComponents('BGRA Themes', [TBGRAThemeRadioButton]);
end;

{ TBGRAThemeRadioButton }

procedure TBGRAThemeRadioButton.SetChecked(AValue: boolean);
begin
  if FChecked = AValue then
    Exit;
  FChecked := AValue;
  if FChecked then
    UncheckOthers;
  Invalidate;
  if Assigned(FOnChange) then FOnChange(Self);
end;

class function TBGRAThemeRadioButton.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 165;
  Result.CY := 19;
end;

procedure TBGRAThemeRadioButton.MouseEnter;
begin
  inherited MouseEnter;
  FState := btbsHover;
  Invalidate;
end;

procedure TBGRAThemeRadioButton.MouseLeave;
begin
  inherited MouseLeave;
  FState := btbsNormal;
  Invalidate;
end;

procedure TBGRAThemeRadioButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FState := btbsActive;
  Checked := True;
end;

procedure TBGRAThemeRadioButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if ClientRect.Contains(Point(X, Y)) then
    FState := btbsHover
  else
    FState := btbsNormal;
  Invalidate;
end;

procedure TBGRAThemeRadioButton.Click;
begin
  inherited Click;
end;

procedure TBGRAThemeRadioButton.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  if Value then
    FState := btbsNormal
  else
    FState := btbsDisabled;
  Invalidate;
end;

procedure TBGRAThemeRadioButton.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
end;

procedure TBGRAThemeRadioButton.Paint;
var
  surface: TBGRAThemeSurface;
begin
  surface := TBGRAThemeSurface.Create(self);
  try
    if Assigned(Theme) then
      Theme.DrawRadioButton(Caption, FState, Focused, Checked, ClientRect, surface)
    else
      BGRADefaultTheme.DrawRadioButton(Caption, FState, Focused, Checked, ClientRect, surface);
  finally
    surface.Free;
  end;
end;

procedure TBGRAThemeRadioButton.Resize;
begin
  Invalidate;
  inherited Resize;
end;

procedure TBGRAThemeRadioButton.UncheckOthers;
var
  i: integer;
  control: TWinControl;
begin
  if Parent is TWinControl then
  begin
    control := TWinControl(Parent);
    for i := 0 to control.ControlCount - 1 do
      if (control.Controls[i] <> Self) and (control.Controls[i] is
        TBGRAThemeRadioButton) then
        TBGRAThemeRadioButton(control.Controls[i]).Checked := False;
  end;
end;

constructor TBGRAThemeRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FState := btbsNormal;

  ControlStyle := ControlStyle + [csParentBackground];

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

end.
