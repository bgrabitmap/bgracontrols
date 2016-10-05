unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BCNumericKeyboard, BCButton, BCDefaultThemeManager,
  BCButtonFocus;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCButton1: TBCButton;
    BCButton2: TBCButton;
    BCButton3: TBCButton;
    BCButton4: TBCButton;
    BCButtonFocus1: TBCButtonFocus;
    BCDefaultThemeManager1: TBCDefaultThemeManager;
    BCNumericKeyboard1: TBCNumericKeyboard;
    BCRealNumericKeyboard1: TBCRealNumericKeyboard;
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation
uses
  BCTypes;

{$R *.lfm}

{ TForm1 }

{ For button }
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

{ For button focus }
procedure BCButtonWindows8(AButton: TBCButtonFocus; cl1, cl2: TColor);
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

procedure TForm1.FormShow(Sender: TObject);
begin
  { Set some default theme to theme manager }
  BCButtonWindows8(BCDefaultThemeManager1.Button, clNavy, clGray);
  BCButtonWindows8(BCDefaultThemeManager1.ButtonFocus, clNavy, clGray);

  { Apply to all buttons in this form }
  BCDefaultThemeManager1.Apply();

  BCNumericKeyboard1.Panel.Left:=200;
  BCNumericKeyboard1.Panel.Top:=10;
  BCNumericKeyboard1.Show();

  BCRealNumericKeyboard1.Panel.Left:=200;
  BCRealNumericKeyboard1.Panel.Top:=BCNumericKeyboard1.Panel.Top+BCNumericKeyboard1.Panel.Height+10;
  BCRealNumericKeyboard1.Show();
end;

end.

