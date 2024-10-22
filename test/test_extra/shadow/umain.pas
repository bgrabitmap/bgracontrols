unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BGRAVirtualScreen, BGRABitmap, BCTypes, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    blurSize: integer;
    offset: TPoint;
    logo, logoShadow: TBGRABitmap;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure SingleColor(Bitmap: TBGRABitmap; Color: TBGRAPixel);
var
  i: integer;
  p: PBGRAPixel;
begin
  p := Bitmap.Data;

  for i := Bitmap.NBPixels - 1 downto 0 do
  begin
    p^.red := Color.Red;
    p^.green := Color.Green;
    p^.blue := Color.Blue;
    Inc(p);
  end;
end;

function Shadow(Source: TBGRABitmap; Color: TBGRAPixel; Blur: integer): TBGRABitmap;
begin
  Result := TBGRABitmap.Create(Source.Width + (2 * Blur), Source.Height + (2 * Blur));
  Result.PutImage(Blur, Blur, Source, dmDrawWithTransparency);
  SingleColor(Result, Color);
  BGRAReplace(Result, Result.FilterBlurRadial(Blur, rbFast));
end;

{ TForm1 }

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.PutImage(blurSize + offSet.x, blurSize + offset.y, logoShadow,
    dmDrawWithTransparency);
  Bitmap.PutImage(blurSize * 2, blurSize * 2, logo, dmDrawWithTransparency);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  blurSize := 5;
  offSet := Point(5, 5);
  logo := TBGRABitmap.Create('logo.png');
  logoShadow := Shadow(logo, BGRA(0, 200, 200, 255), blurSize);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  logo.Free;
  logoShadow.Free;
end;

end.
