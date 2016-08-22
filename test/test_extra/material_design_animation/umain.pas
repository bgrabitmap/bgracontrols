unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, ExtCtrls, Dialogs,
  BGRAGraphicControl, BGRABitmap, BGRABitmapTypes,
  BGRATextFX, bcmaterialdesignbutton;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCMaterialDesignButton1: TBCMaterialDesignButton;
    BCMaterialDesignButton2: TBCMaterialDesignButton;
    Button1: TBGRAGraphicControl;
    Timer1: TTimer;
    procedure BCMaterialDesignButton1Click(Sender: TObject);
    procedure Button1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Button1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure Timer1StartTimer(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    MouseP: TPoint;
    CircleX: single;
    CircleAlpha: byte;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  Timer1.Enabled := False;
  MouseP := Point(X, Y);
  Timer1.Enabled := True;
end;

procedure TForm1.BCMaterialDesignButton1Click(Sender: TObject);
begin
  //ShowMessage('Click');
end;

procedure TForm1.Button1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  temp: TBGRABitmap;
begin
  { Shadow }
  Bitmap.Fill(BGRAPixelTransparent);
  Bitmap.RoundRect(5, 5, Button1.Width - 5, Button1.Height - 5, 5, 5,
    BGRA(100, 100, 100), BGRA(100, 100, 100));

  temp := Bitmap.FilterBlurRadial(5, 5, rbFast) as TBGRABitmap;
  Bitmap.Fill(BGRAPixelTransparent);
  Bitmap.PutImage(0, 0, temp, dmDrawWithTransparency);
  temp.Free;

  { Round Rectangle }
  temp := TBGRABitmap.Create(Button1.Width, Button1.Height, BGRAWhite);
  { Circle Effect }
  temp.EllipseAntialias(MouseP.X, MouseP.Y, CircleX, CircleX,
    BGRA(100, 100, 100, CircleAlpha), 1, BGRA(100, 100, 100, CircleAlpha));
  Bitmap.FillRoundRectAntialias(5, 0, Button1.Width - 5, Button1.Height -
    5, 5, 5, temp, [rrDefault], False);
  temp.Free;

  { Text }
  temp := TextShadow(Button1.Width, Button1.Height, 'Material Design',
    20, BGRABlack, BGRABlack, 2, 2, 2) as TBGRABitmap;
  Bitmap.PutImage(0, 0, temp, dmDrawWithTransparency);
  temp.Free;
end;

procedure TForm1.Timer1StartTimer(Sender: TObject);
begin
  CircleAlpha := 255;
  CircleX := 5;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  CircleX := CircleX + 4;
  CircleAlpha := CircleAlpha - 5;
  if CircleAlpha <= 0 then
    Timer1.Enabled := False;
  Button1.DiscardBitmap;
end;

end.
