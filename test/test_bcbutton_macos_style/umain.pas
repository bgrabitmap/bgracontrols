unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BCButtonFocus, BCButton, BCPanel, BGRASpriteAnimation;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCButton1: TBCButton;
    BCButton2: TBCButton;
    BCButton3: TBCButton;
    BCButton4: TBCButton;
    BCButton5: TBCButton;
    BCButton6: TBCButton;
    BCButton7: TBCButton;
    BCPanel1: TBCPanel;
    BGRASpriteAnimation1: TBGRASpriteAnimation;
    procedure BCButton5Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BCButton5Click(Sender: TObject);
begin
  BCButton5.Down := False;
  BCButton6.Down := False;
  BCButton7.Down := False;
  TBCButton(Sender).Down := True;
end;

end.

