unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, BCNumericKeyboard, BCButton, BCTypes, BCPanel, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCPanel1: TBCPanel;
    BCRealNumericKeyboard1: TBCRealNumericKeyboard;
    Edit1: TEdit;
    FloatSpinEdit1: TFloatSpinEdit;
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
    (BCRealNumericKeyboard1.Visible) then
  begin
    BCRealNumericKeyboard1.Hide();
  end
  else
  begin
    NumericSender := Sender as TControl;
    BCRealNumericKeyboard1.Panel.Left := NumericSender.Left;
    BCRealNumericKeyboard1.Panel.Top := NumericSender.Top + NumericSender.Height;
    BCRealNumericKeyboard1.Show();
  end;
end;

procedure TForm1.FormClick(Sender: TObject);
begin
  if NumericSender <> nil then
    Button1Click(NumericSender);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Assign custom format settings
  DefaultFormatSettings.CurrencyString := '$';
  // DefaultFormatSettings.DecimalSeparator := '.';

  // Assign a style
  BCButtonWindows8(BCRealNumericKeyboard1.ButtonStyle, clGray, clSkyBlue);
  // Custom extra size inside the button
  BCRealNumericKeyboard1.ButtonStyle.SetSizeVariables(0, 0, 15, 25);
  // Apply the style
  BCRealNumericKeyboard1.UpdateButtonStyle;

  with BCRealNumericKeyboard1.Panel do
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
