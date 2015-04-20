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

procedure TForm1.BGRAFlashProgressBar2Click(Sender: TObject);
begin

end;

end.

