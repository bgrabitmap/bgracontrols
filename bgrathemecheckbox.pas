// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAThemeCheckBox;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRATheme, Types, LMessages, LCLType;

type

  { TBGRAThemeCheckBox }

  TBGRAThemeCheckBox = class(TBGRAThemeControl)
  private
    FChecked: boolean;
    FOnChange: TNotifyEvent;
    FState: TBGRAThemeButtonState;
    procedure SetChecked(AValue: boolean);
  protected
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
    procedure WMSetFocus(var Message: {$IFDEF FPC}TLMSetFocus{$ELSE}TWMSetFocus{$ENDIF}); message {$IFDEF FPC}LM_SETFOCUS{$ELSE}WM_SETFOCUS{$ENDIF};
    procedure WMKillFocus(var Message: {$IFDEF FPC}TLMKillFocus{$ELSE}TWMKillFocus{$ENDIF}); message {$IFDEF FPC}LM_KILLFOCUS{$ELSE}WM_KILLFOCUS{$ENDIF};
    procedure UpdateFocus(AFocused: boolean);
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
    property TabStop;
    property TabOrder;
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

procedure TBGRAThemeCheckBox.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if (Key = VK_SPACE) or (Key = VK_RETURN) then
    MouseDown(mbLeft, [], 0, 0);
end;

procedure TBGRAThemeCheckBox.KeyUp(var Key: word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) or (Key = VK_RETURN) then
  begin
    MouseUp(mbLeft, [], 0, 0);
    MouseLeave;
  end;

  inherited KeyUp(Key, Shift);
end;

procedure TBGRAThemeCheckBox.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;

  UpdateFocus(True);
end;

procedure TBGRAThemeCheckBox.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited;

  if Message.FocusedWnd <> Handle then
    UpdateFocus(False);
end;

procedure TBGRAThemeCheckBox.UpdateFocus(AFocused: boolean);
var
  lForm: TCustomForm;
begin
  lForm := GetParentForm(Self);
  if lForm = nil then
    exit;

  {$IFDEF FPC}//#
  if AFocused then
    ActiveDefaultControlChanged(lForm.ActiveControl)
  else
    ActiveDefaultControlChanged(nil);
  {$ENDIF}

  Invalidate;
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

  ControlStyle := ControlStyle + [csParentBackground, csAcceptsControls];

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

end.
