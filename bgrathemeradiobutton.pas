unit BGRAThemeRadioButton;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRATheme, Types;

type

  { TBGRAThemeRadioButton }

  TBGRAThemeRadioButton = class(TCustomControl)
  private
    FChecked: boolean;
    FTheme: TBGRATheme;
    FState: TBGRAThemeButtonState;
    procedure SetFChecked(AValue: boolean);
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
    procedure UncheckOthers;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption;
    property Checked: boolean read FChecked write SetFChecked;
    property Font;
    property Enabled;
    property Theme: TBGRATheme read FTheme write SetFTheme;
  end;

procedure Register;

implementation

uses BGRABitmapTypes;

procedure Register;
begin
  RegisterComponents('BGRA Themes', [TBGRAThemeRadioButton]);
end;

{ TBGRAThemeRadioButton }

procedure TBGRAThemeRadioButton.SetFTheme(AValue: TBGRATheme);
begin
  if FTheme = AValue then
    Exit;
  FTheme := AValue;
  Invalidate;
end;

procedure TBGRAThemeRadioButton.SetFChecked(AValue: boolean);
begin
  if FChecked = AValue then
    Exit;
  FChecked := AValue;
  if FChecked then
    UncheckOthers;
  Invalidate;
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
  FChecked := True;
  UncheckOthers;
  Invalidate;
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
begin
  if Assigned(Theme) then
    Theme.DrawRadioButton(Caption, FState, Focused, Checked, ClientRect, Canvas)
  else
    BGRADefaultTheme.DrawRadioButton(Caption, FState, Focused, Checked,
      ClientRect, Canvas);
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

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

end.
