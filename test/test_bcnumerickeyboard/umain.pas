unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, BCNumericKeyboard, BCButton, BCTypes, BCPanel, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCNumericKeyboard1: TBCNumericKeyboard;
    BCPanel1: TBCPanel;
    Button1: TBCButton;
    Button2: TBCButton;
    Edit1: TEdit;
    FloatSpinEdit1: TFloatSpinEdit;
    procedure BCNumericKeyboard1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    NumericSender: TControl;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

{ Button style }
procedure BCButtonWindows8(AButton: TBCButton; cl1, cl2: TColor);
begin
  AButton.Rounding.RoundX := 1;
  AButton.Rounding.RoundY := 1;
  AButton.RoundingDropDown.Assign(AButton.Rounding);

  with AButton.StateNormal do
  begin
    Background.Style := bbsColor;
    Background.Color := cl1;
    Border.Style := bboSolid;
    Border.Width := 1;
    Border.Color := cl1;
    Border.LightWidth := 0;
    Border.LightOpacity := 255;
    Border.Style := bboSolid;
    FontEx.Color := clWhite;
    FontEx.Shadow := False;
    FontEx.Style := [];
  end;

  AButton.StateHover.Assign(AButton.StateNormal);
  AButton.StateClicked.Assign(AButton.StateNormal);

  with AButton.StateHover do
  begin
    Background.Color := cl2;
    Border.Color := cl2;
  end;

  with AButton.StateClicked do
  begin
    Background.Color := cl2;
    Border.Color := cl2;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if (NumericSender <> nil) and (NumericSender.Name = TControl(Sender).Name) and
    (BCNumericKeyboard1.Visible) then
  begin
    BCNumericKeyboard1.Hide();
    // Remove unnecessary comma for button caption
    if Sender is TBCButton or Sender is TEdit then
      if Pos(DefaultFormatSettings.DecimalSeparator, NumericSender.Caption) =
        Length(NumericSender.Caption) then
        NumericSender.Caption :=
          LeftStr(NumericSender.Caption, Length(NumericSender.Caption) - 1);
  end
  else
  begin
    NumericSender := Sender as TControl;
    BCNumericKeyboard1.Value := '';
    BCNumericKeyboard1.Panel.Left := NumericSender.Left;
    BCNumericKeyboard1.Panel.Top := NumericSender.Top + NumericSender.Height;
    BCNumericKeyboard1.Show();
  end;
end;

procedure TForm1.FormClick(Sender: TObject);
begin
  if NumericSender <> nil then
    Button1Click(NumericSender);
end;

procedure TForm1.BCNumericKeyboard1Change(Sender: TObject);
var
  d: double;
begin
  // For buttons
  if NumericSender is TBCButton or NumericSender is TEdit then
  begin
    if BCNumericKeyboard1.Value <> '' then
      NumericSender.Caption :=
        DefaultFormatSettings.CurrencyString + ' ' + BCNumericKeyboard1.Value
    else
      NumericSender.Caption :=
        DefaultFormatSettings.CurrencyString + ' 0' +
        DefaultFormatSettings.DecimalSeparator + '00';
  end;
  // For spin edit
  if NumericSender is TFloatSpinEdit then
  begin
    TryStrToFloat(BCNumericKeyboard1.Value, d);
    TFloatSpinEdit(NumericSender).Value := d;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Assign custom format settings
  DefaultFormatSettings.CurrencyString := '$';
  // DefaultFormatSettings.DecimalSeparator := '.';

  // Assign a style
  BCButtonWindows8(BCNumericKeyBoard1.ButtonStyle, clGray, clSkyBlue);
  // Custom extra size inside the button
  BCNumericKeyBoard1.ButtonStyle.SetSizeVariables(0, 0, 15, 25);
  // Apply the style
  BCNumericKeyboard1.UpdateButtonStyle;

  with BCNumericKeyboard1.Panel do
  begin
    BevelInner := bvNone;
    BevelOuter := bvNone;
    Background.Gradient1.StartColor := clNavy;
    Background.Gradient1.EndColor := clPurple;
    Background.Gradient1.Point1XPercent := 0;
    Background.Gradient1.Point1YPercent := 0;
    Background.Gradient1.Point2XPercent := 0;
    Background.Gradient1.Point2YPercent := 100;
    Background.Gradient1EndPercent := 100;
    Background.Style := bbsGradient;
    // Spacing around
    ChildSizing.TopBottomSpacing := 5;
    ChildSizing.LeftRightSpacing := 5;
    // Spacing between buttons
    ChildSizing.VerticalSpacing := 10;
    ChildSizing.HorizontalSpacing := 10;
  end;
end;

end.
