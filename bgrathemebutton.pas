// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAThemeButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRATheme, Types, ExtCtrls, BGRASVGImageList;

type

  { TBGRAThemeButton }

  TBGRAThemeButton = class(TBGRAThemeControl)
  private
    FImageIndex: integer;
    FImageList: TBGRASVGImageList;
    FModalResult: TModalResult;
    FState: TBGRAThemeButtonState;
    FTimerHover: TTimer;
    procedure SetImageIndex(AValue: integer);
    procedure SetImageList(AValue: TBGRASVGImageList);
    procedure SetState(AValue: TBGRAThemeButtonState);
    procedure TimerHoverElapse(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
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
    procedure UpdateHoverState;
    property State: TBGRAThemeButtonState read FState write SetState;
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
    property ImageList: TBGRASVGImageList read FImageList write SetImageList;
    property ImageIndex: integer read FImageIndex write SetImageIndex;
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

procedure TBGRAThemeButton.SetState(AValue: TBGRAThemeButtonState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
  FTimerHover.Enabled := (FState = btbsHover);
  Invalidate;
end;

procedure TBGRAThemeButton.SetImageIndex(AValue: integer);
begin
  if FImageIndex = AValue then
    Exit;
  FImageIndex := AValue;
  Invalidate;
end;

procedure TBGRAThemeButton.SetImageList(AValue: TBGRASVGImageList);
begin
  if FImageList = AValue then
    Exit;
  FImageList := AValue;
  Invalidate;
end;

procedure TBGRAThemeButton.TimerHoverElapse(Sender: TObject);
begin
  UpdateHoverState;
end;

procedure TBGRAThemeButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImageList) then
    FImageList := nil;
end;

class function TBGRAThemeButton.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 125;
  Result.CY := 35;
end;

procedure TBGRAThemeButton.MouseEnter;
begin
  inherited MouseEnter;
  if Enabled then
    State := btbsHover
  else
    State := btbsDisabled;
end;

procedure TBGRAThemeButton.MouseLeave;
begin
  inherited MouseLeave;
  if Enabled then
    State := btbsNormal
  else
    State := btbsDisabled;
end;

procedure TBGRAThemeButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  State := btbsActive;
end;

procedure TBGRAThemeButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  UpdateHoverState;
end;

procedure TBGRAThemeButton.Click;
var
  Form: TCustomForm;
begin
  UpdateHoverState;
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
    State := btbsNormal
  else
    State := btbsDisabled;
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
  Canvas.Font.Assign(Font);
  surface := TBGRAThemeSurface.Create(self);
  try
    if Assigned(Theme) then
      Theme.DrawButton(Caption, FState, Focused, ClientRect, surface, FImageIndex, FImageList)
    else
      BGRADefaultTheme.DrawButton(Caption, FState, Focused, ClientRect, surface, FImageIndex, FImageList);
  finally
    surface.Free;
  end;
end;

procedure TBGRAThemeButton.Resize;
begin
  Invalidate;
  inherited Resize;
end;

procedure TBGRAThemeButton.UpdateHoverState;
var
  p: TPoint;
begin
  p := ScreenToClient(Mouse.CursorPos);
  if (p.x >= 0) and (p.x <= Width) and (p.y >= 0) and (p.y <= Height) then
    State := btbsHover
  else
  if Enabled then
    State := btbsNormal
  else
    State := btbsDisabled;
end;

constructor TBGRAThemeButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FState := btbsNormal;

  ControlStyle := ControlStyle + [csParentBackground];

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  FTimerHover := TTimer.Create(self);
  FTimerHover.Enabled := False;
  FTimerHover.Interval := 100;
  FTimerHover.OnTimer := @TimerHoverElapse;
end;

end.
