// SPDX-License-Identifier: LGPL-3.0-only (modified to allow linking)
unit BGRAThemeButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRATheme, Types;

type

  { TBGRAThemeButton }

  TBGRAThemeButton = class(TBGRAThemeControl)
  private
    FModalResult: TModalResult;
    FState: TBGRAThemeButtonState;
  protected
    class function GetControlClassDefaultSize: TSize; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure DoMouseMove({%H-}x, {%H-}y: integer); virtual;
    procedure Click; override;
    procedure SetEnabled(Value: boolean); override;
    procedure TextChanged; override;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ModalResult: TModalResult
      read FModalResult write FModalResult default mrNone;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Caption;
    property Enabled;
    property Font;
    property OnClick;
  end;

procedure Register;

implementation

uses BGRABitmapTypes;

procedure Register;
begin
  RegisterComponents('BGRA Themes', [TBGRAThemeButton]);
end;

{ TBGRAThemeButton }

class function TBGRAThemeButton.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 125;
  Result.CY := 35;
end;

procedure TBGRAThemeButton.MouseEnter;
var
  NewState: TBGRAThemeButtonState;
begin
  inherited MouseEnter;
  if Enabled then
    NewState := btbsHover
  else
  begin
    FState := btbsNormal;
    NewState := FState;
  end;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TBGRAThemeButton.MouseLeave;
var
  NewState: TBGRAThemeButtonState;
begin
  inherited MouseLeave;
  if Enabled then
    NewState := btbsNormal
  else
  begin
    FState := btbsNormal;
    NewState := FState;
  end;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TBGRAThemeButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  NewState: TBGRAThemeButtonState;
begin
  inherited MouseDown(Button, Shift, X, Y);
  NewState := btbsActive;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
  Invalidate;
end;

procedure TBGRAThemeButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  NewState: TBGRAThemeButtonState;
  p: TPoint;
begin
  inherited MouseUp(Button, Shift, X, Y);
  p := ScreenToClient(Mouse.CursorPos);

  if (p.x >= 0) and (p.x <= Width) and (p.y >= 0) and (p.y <= Height) then
    NewState := btbsHover
  else
    NewState := btbsNormal;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
  Invalidate;
end;

procedure TBGRAThemeButton.DoMouseMove(x, y: integer);
begin
  inherited;
end;

procedure TBGRAThemeButton.Click;
var
  Form: TCustomForm;
begin
  if ModalResult <> mrNone then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.ModalResult := ModalResult;
  end;
  inherited Click;
end;

procedure TBGRAThemeButton.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  if Value then
    FState := btbsNormal
  else
    FState := btbsDisabled;
  Invalidate;
end;

procedure TBGRAThemeButton.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
end;

procedure TBGRAThemeButton.Paint;
var
  surface: TBGRAThemeSurface;
begin
  surface := TBGRAThemeSurface.Create(self);
  try
    if Assigned(Theme) then
      Theme.DrawButton(Caption, FState, Focused, ClientRect, surface)
    else
      BGRADefaultTheme.DrawButton(Caption, FState, Focused, ClientRect, surface);
  finally
    surface.Free;
  end;
end;

procedure TBGRAThemeButton.Resize;
begin
  Invalidate;
  inherited Resize;
end;

constructor TBGRAThemeButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FState := btbsNormal;

  ControlStyle := ControlStyle + [csParentBackground];

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

end.
