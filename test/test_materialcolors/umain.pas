unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  MaterialColors, BCMaterialDesignButton, BGRABitmap, BGRABitmapTypes,
  BCBrightAndContrast;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCMaterialDesignButton1: TBCMaterialDesignButton;
    ComboBox1: TComboBox;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure UpdateButton();
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
      MaterialColorsList.KeyData[ComboBox1.Caption].M500;
    BCMaterialDesignButton1.NormalColorEffect :=
      MaterialColorsList.KeyData[ComboBox1.Caption].M50;
    BCMaterialDesignButton1.TextColor :=
      GetContrastColor(BCMaterialDesignButton1.NormalColor);
  end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  UpdateButton();
end;

end.
