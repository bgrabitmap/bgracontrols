unit BGRAThemeButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRATheme;

type

  { TBGRAThemeButton }

  TBGRAThemeButton = class(TCustomControl)
  private
    FTheme: TBGRATheme;
    FState: TBGRAThemeButtonState;
    procedure SetFTheme(AValue: TBGRATheme);
  protected
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure Click; override;
    procedure SetEnabled(Value: Boolean); override;
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
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FState := btbsActive;
  Invalidate;
end;

procedure TBGRAThemeButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
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

procedure TBGRAThemeButton.SetEnabled(Value: Boolean);
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
    Theme.DrawButton(Caption, FState, Focused, ClientRect, Canvas);
end;

constructor TBGRAThemeButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FState := btbsNormal;
end;

end.
