unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  MaterialColors, BCMaterialDesignButton, BGRABitmap, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCMaterialDesignButton1: TBCMaterialDesignButton;
    ComboBox1: TComboBox;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure UpdateButton();
    function GetContrastColor(ABGColor: TColor): TColor;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Fill the list of colors
  ComboBox1.Items.Add('Default');
  MaterialColorsListStr(ComboBox1.Items);
end;

procedure TForm1.UpdateButton;
begin
  if ComboBox1.Caption = 'Default' then
  begin
    BCMaterialDesignButton1.NormalColor := clWhite;
    BCMaterialDesignButton1.NormalColorEffect := clSilver;
    BCMaterialDesignButton1.TextColor := clBlack;
  end
  else
  begin
    BCMaterialDesignButton1.NormalColor :=
      BGRAToColor(MaterialColorsList.KeyData[ComboBox1.Caption].M500);
    BCMaterialDesignButton1.NormalColorEffect :=
      BGRAToColor(MaterialColorsList.KeyData[ComboBox1.Caption].M50);
    BCMaterialDesignButton1.TextColor :=
      GetContrastColor(BCMaterialDesignButton1.NormalColor);
  end;
end;

function TForm1.GetContrastColor(ABGColor: TColor): TColor;
var
  ADouble: double;
  R, G, B: byte;
begin
  if ABGColor <= 0 then
  begin
    Result := clWhite;
    Exit; // *** EXIT RIGHT HERE ***
  end;

  if ABGColor = clWhite then
  begin
    Result := clBlack;
    Exit; // *** EXIT RIGHT HERE ***
  end;

  // Get RGB from Color
  R := Red(ABGColor);
  G := Green(ABGColor);
  B := Blue(ABGColor);

  // Counting the perceptive luminance - human eye favors green color...
  ADouble := 1 - (0.299 * R + 0.587 * G + 0.114 * B) / 255;

  if (ADouble < 0.5) then
    Result := clBlack  // bright colors - black font
  else
    Result := clWhite;  // dark colors - white font
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  UpdateButton();
end;

end.
