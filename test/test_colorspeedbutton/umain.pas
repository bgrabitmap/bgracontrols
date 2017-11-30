unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, Buttons, ColorSpeedButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    ColorSpeedButton1: TColorSpeedButton;
    ColorSpeedButton2: TColorSpeedButton;
    ColorSpeedButton3: TColorSpeedButton;
    ColorSpeedButton4: TColorSpeedButton;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;
    SpeedButton1: TSpeedButton;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  ColorSpeedButton4.Caption:= StringReplace(ColorSpeedButton4.Caption,' ',LineEnding,[rfReplaceAll]);
  SpeedButton1.Caption:= StringReplace(ColorSpeedButton4.Caption,' ',LineEnding,[rfReplaceAll]);
end;

end.

