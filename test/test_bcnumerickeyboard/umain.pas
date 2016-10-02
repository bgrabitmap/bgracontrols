unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BCNumericKeyboard, BCButton, BCTypes, BCPanel, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCNumericKeyboard1: TBCNumericKeyboard;
    BCPanel1: TBCPanel;
    Button1: TBCButton;
    procedure BCNumericKeyboard1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

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
  if BCNumericKeyboard1.Visible then
    BCNumericKeyboard1.Hide()
  else
  begin
    BCNumericKeyboard1.Panel.Left := 10;
    BCNumericKeyboard1.Panel.Top := 50;
    BCNumericKeyboard1.Show();
  end;
end;

procedure TForm1.FormClick(Sender: TObject);
begin
  if BCNumericKeyboard1.Visible then
    BCNumericKeyboard1.Hide();
end;

procedure TForm1.BCNumericKeyboard1Change(Sender: TObject);
begin
  if BCNumericKeyboard1.Value <> '' then
    Button1.Caption := DefaultFormatSettings.CurrencyString + ' ' +
      BCNumericKeyboard1.Value
  else
    Button1.Caption := DefaultFormatSettings.CurrencyString + ' 0' +
      DefaultFormatSettings.DecimalSeparator + '00';
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

  // Set first time the caption of button
  BCNumericKeyboard1Change(Self);
end;

end.
