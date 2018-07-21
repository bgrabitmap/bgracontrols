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
    BCPanel1: TBCPanel;
    BGRASpriteAnimation1: TBGRASpriteAnimation;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

