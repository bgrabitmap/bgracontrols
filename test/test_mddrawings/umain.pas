unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BGRAGraphicControl, BGRABitmap, BCTypes,
  BGRABitmapTypes, Math;

type

  { TForm1 }

  TForm1 = class(TForm)
    g1: TBGRAGraphicControl;
    g2: TBGRAGraphicControl;
    procedure g1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure g2Redraw(Sender: TObject; Bitmap: TBGRABitmap);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.g1Redraw(Sender: TObject; Bitmap: TBGRABitmap
  );
const
  offset = 8;
var
  temp: TBGRABitmap;
begin
  Bitmap.FillTransparent;
  Bitmap.FillEllipseAntialias(g1.Width div 2, (g1.Height) div 2, (g1.Width - 10) div 2, (g1.Height - 10) div 2, BGRA(150, 150, 150));
  temp := TBGRABitmap.Create;
  BGRAReplace(temp, Bitmap.FilterBlurRadial(5, 5, rbFast));
  Bitmap.FillTransparent;
  Bitmap.PutImage(0, 0, temp, dmDrawWithTransparency);
  temp.Free;
  Bitmap.FillEllipseAntialias(g1.Width div 2, (g1.Height - offset) div 2, (g1.Width - offset) div 2, (g1.Height - offset) div 2, BGRAWhite);
  Bitmap.FontHeight := Min(g1.Height div 2, g1.Width div 2);
  Bitmap.TextRect(Rect(0, 0, g1.Width, g1.Height - offset), '🐣', TAlignment.taCenter, TTextLayout.tlCenter, BGRABlack);
end;

procedure TForm1.g2Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  temp: TBGRABitmap;
begin
  Bitmap.FillTransparent;
  Bitmap.FillRoundRectAntialias(10, 10, g2.Width - 10, g2.Height - 10, 8, 8, BGRA(100,100,100));
  temp := TBGRABitmap.Create;
  BGRAReplace(temp, Bitmap.FilterBlurRadial(5, 5, rbFast));
  Bitmap.FillTransparent;
  Bitmap.PutImage(0, 0, temp, dmDrawWithTransparency);
  temp.Free;
  Bitmap.FillRoundRectAntialias(8, 0, g2.Width - 8, g2.Height - 10, 10, 10, BGRAWhite, [], False);
  Bitmap.FontHeight := Min(g2.Height div 2, g2.Width div 2);
  Bitmap.TextRect(Rect(0, 0, g2.Width, g2.Height - 8), '🐣', TAlignment.taCenter, TTextLayout.tlCenter, BGRABlack);
end;

end.

