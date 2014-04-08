unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Forms, Graphics,
  BCImageButton, BGRABitmap,
  BCFilters, BCButton;

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
  BCImageButton1.LoadFromBitmapFile;
  BCImageButton2.LoadFromBitmapFile;
  BCImageButton3.LoadFromBitmapFile;
  BCImageButton4.LoadFromBitmapFile;
  BCImageButton5.LoadFromBitmapFile;
  GrayScale(BCImageButton1.BitmapOptions.Bitmap);
end;

end.

