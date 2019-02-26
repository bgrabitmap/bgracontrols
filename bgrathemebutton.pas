unit BGRAThemeButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRATheme, Types;

type

  { TBGRAThemeButton }

  TBGRAThemeButton = class(TCustomControl)
  private
    FTheme: TBGRATheme;
    FState: TBGRAThemeButtonState;
    procedure SetFTheme(AValue: TBGRATheme);
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
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption;
    property Font;
    property Enabled;
    property Theme: TBGRATheme read FTheme write SetFTheme;
  end;

procedure Register;

implementation

uses BGRABitmapTypes;

procedure Register;
begin
  RegisterComponents('BGRA Themes', [TBGRAThemeButton]);
end;

{ TBGRAThemeButton }

procedure TBGRAThemeButton.SetFTheme(AValue: TBGRATheme);
begin
  if FTheme = AValue then
    Exit;
  FTheme := AValue;
  Invalidate;
end;

class function TBGRAThemeButton.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 125;
  Result.CY := 35;
end;

procedure TBGRAThemeButton.MouseEnter;
begin
  inherited MouseEnter;
  FState := btbsHover;
  Invalidate;
end;

procedure TBGRAThemeButton.MouseLeave;
begin
  inherited MouseLeave;
  FState := btbsNormal;
  Invalidate;
end;

procedure TBGRAThemeButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FState := btbsActive;
  Invalidate;
end;

procedure TBGRAThemeButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if ClientRect.Contains(Point(X, Y)) then
    FState := btbsHover
  else
    FState := btbsNormal;
  Invalidate;
end;

procedure TBGRAThemeButton.Click;
begin
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
begin
  if Assigned(Theme) then
    Theme.DrawButton(Caption, FState, Focused, ClientRect, Canvas)
  else
    BGRADefaultTheme.DrawButton(Caption, FState, Focused, ClientRect, Canvas);
end;

constructor TBGRAThemeButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FState := btbsNormal;

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

end.
