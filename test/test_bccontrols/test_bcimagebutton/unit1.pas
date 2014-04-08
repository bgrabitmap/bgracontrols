unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Forms, Graphics,
  BCImageButton, BGRABitmap;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCImageButton1: TBCImageButton;
    BCImageButton2: TBCImageButton;
    BCImageButton3: TBCImageButton;
    BCImageButton4: TBCImageButton;
    BCImageButton5: TBCImageButton;
    BCImageButton6: TBCImageButton;
    procedure FormCreate(Sender: TObject);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  BCImageButton1.BitmapOptions.Bitmap := TBGRABitmap.Create('sample_1.png');
  BCImageButton2.BitmapOptions.Bitmap := TBGRABitmap.Create('sample_2.png');
  BCImageButton3.BitmapOptions.Bitmap := TBGRABitmap.Create('sample_3.png');
  BCImageButton4.BitmapOptions.Bitmap := TBGRABitmap.Create('sample_4.png');
  BCImageButton5.BitmapOptions.Bitmap := TBGRABitmap.Create('sample_5.png');
end;

end.

