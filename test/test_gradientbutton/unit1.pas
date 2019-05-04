unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BCGradientButton,
  BCButton, BGRABitmap, BCTypes, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCGradientButton1: TBCGradientButton;
    procedure BCGradientButton1BeforeRedraw(Sender: TObject; Bitmap: TBGRABitmap
      );
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    bmp: TBGRABitmap;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  BCGradientButton1.BeginUpdate;
  BCGradientButton1.BorderSize := 4;
  BCGradientButton1.EndUpdate;
  bmp := TBGRABitmap.Create(Application.Location + 'image.png');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  bmp.Free;
end;

procedure TForm1.BCGradientButton1BeforeRedraw(Sender: TObject;
  Bitmap: TBGRABitmap);
begin
  Bitmap.Fill(clNavy);
  Bitmap.StretchPutImageProportionally(Rect(0, 0, Bitmap.Width, Bitmap.Height), TACenter, TLCenter, bmp, dmDrawWithTransparency);
end;

end.

