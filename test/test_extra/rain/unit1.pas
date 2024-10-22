unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRAGraphicControl, BGRABitmap, BCTypes, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAGraphicControl1: TBGRAGraphicControl;
    Timer1: TTimer;
    procedure BGRAGraphicControl1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    bkg: TBGRABitmap;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BGRAGraphicControl1Redraw(Sender: TObject; Bitmap: TBGRABitmap
  );
var
  i: integer;
  bmp: TBGRABitmap;
begin
  //Bitmap.Fill(BGRABlack);
  Bitmap.FillTransparent;

  for i:= 0 to Bitmap.Width -1 do
  begin
    Bitmap.DrawVertLine(i,{Random(Bitmap.Height)}0,Random(Bitmap.Height),BGRA(255,255,255,Random(25)));
    Bitmap.DrawVertLine(i,Random(Bitmap.Height),Random(Bitmap.Height),BGRA(255,255,255,Random(50)));
  end;

  bmp := Bitmap.FilterBlurMotion(10,270,True) as TBGRABitmap;
  BGRAReplace(bmp, bmp.FilterBlurRadial(1,rbFast));
  Bitmap.BlendImageOver(0,0,bkg,boLinearBlend);
  Bitmap.BlendImageOver(0,0,bmp,boLinearBlend);
  bmp.Free;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer1.Enabled := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  bkg := TBGRABitmap.Create('Lighthouse.jpg');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  bkg.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  BGRAGraphicControl1.DiscardBitmap;
end;

end.

