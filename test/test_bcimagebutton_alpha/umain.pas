unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BCImageButton, BGRASpriteAnimation;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCImageButton1: TBCImageButton;
    BCImageButton2: TBCImageButton;
    BGRASpriteAnimation1: TBGRASpriteAnimation;
    procedure BCImageButton1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
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
  BCImageButton1.LoadFromBitmapResource(BCImageButton1.BitmapFile);
  BCImageButton2.LoadFromBitmapResource(BCImageButton2.BitmapFile);
  BGRASpriteAnimation1.LoadFromBitmapResource('TITLE_BATH3');
end;

procedure TForm1.BCImageButton1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin

end;

end.

