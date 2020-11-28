unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BCButtonFocus, BCTypes;

type

  { TaForm }

  TaForm = class(TForm)
    BCButtonFocus1: TBCButtonFocus;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  aForm: TaForm;

implementation
uses
  bcbrightandcontrast;

{$R *.lfm}

{ TaForm }

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
    // This will be automatically set with the GetContrastColor function
    //FontEx.Color := clWhite;
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

procedure TaForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to aForm.ComponentCount - 1 do
  begin
    if aForm.Components[i] is TBCButtonFocus then
    begin
      { Default style, color for normal and pressed }
      BCButtonWindows8(TBCButtonFocus(aForm.Components[i]), clNavy, clGray);
      { Hover color based on normal color }
      TBCButtonFocus(aForm.Components[i]).StateHover.Background.Color :=
        Bright(TBCButtonFocus(aForm.Components[i]).StateNormal.Background.Color, 20);
      { Font color based on normal color }
      TBCButtonFocus(aForm.Components[i]).StateNormal.FontEx.Color :=
        GetContrastColor(TBCButtonFocus(aForm.Components[i]).StateNormal.Background.Color);
      TBCButtonFocus(aForm.Components[i]).StateHover.FontEx.Color :=
        GetContrastColor(TBCButtonFocus(aForm.Components[i]).StateNormal.Background.Color);
      TBCButtonFocus(aForm.Components[i]).StateClicked.FontEx.Color :=
        GetContrastColor(TBCButtonFocus(aForm.Components[i]).StateNormal.Background.Color);
    end;
  end;
end;

end.

