unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BGRAFlashProgressBar, BCTrackbarUpdown, BGRABitmap, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCTrackbarUpdown1: TBCTrackbarUpdown;
    BGRAFlashProgressBar1: TBGRAFlashProgressBar;
    BGRAFlashProgressBar2: TBGRAFlashProgressBar;
    procedure BCTrackbarUpdown1Change(Sender: TObject; AByUser: boolean);
    procedure BGRAFlashProgressBar1Redraw(Sender: TObject; Bitmap: TBGRABitmap;
      xpos: integer);
    procedure BGRAFlashProgressBar2Click(Sender: TObject);
    procedure BGRAFlashProgressBar2Redraw(Sender: TObject; Bitmap: TBGRABitmap;
      xpos: integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses BGRATextFX;

{$R *.lfm}

{ TForm1 }

procedure TForm1.BGRAFlashProgressBar2Redraw(Sender: TObject;
  Bitmap: TBGRABitmap; xpos: integer);
begin
  { Draw the progressbar container }
  Bitmap.Rectangle(0, 0, Bitmap.Width, Bitmap.Height, BGRABlack, BGRAWhite, dmSet);
  { Draw the progressbar progress }
  Bitmap.Rectangle(1, 1, xpos + 1, Bitmap.Height - 1, BGRAWhite, BGRABlack, dmSet);
end;

procedure TForm1.BCTrackbarUpdown1Change(Sender: TObject; AByUser: boolean);
begin
  BGRAFlashProgressBar1.Value := BCTrackbarUpdown1.Value;
  BGRAFlashProgressBar2.Value := BCTrackbarUpdown1.Value;
end;

procedure TForm1.BGRAFlashProgressBar1Redraw(Sender: TObject;
  Bitmap: TBGRABitmap; xpos: integer);
var fx: TBGRATextEffect;
begin
  fx:= TBGRATextEffect.Create(IntToStr(BGRAFlashProgressBar1.Value)+'%','Arial',BGRAFlashProgressBar1.Height div 2,True);
  fx.DrawOutline(Bitmap, Bitmap.Width div 2,Bitmap.Height div 4,BGRABlack,taCenter);
  fx.Draw(Bitmap, Bitmap.Width div 2,Bitmap.Height div 4,BGRAWhite,taCenter);
  fx.Free;
end;

procedure TForm1.BGRAFlashProgressBar2Click(Sender: TObject);
begin

end;

end.

