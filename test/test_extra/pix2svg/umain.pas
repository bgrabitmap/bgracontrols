unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BCFilters, BGRABitmap, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    bitmap: TBGRABitmap;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

function Pix2svg(Bitmap: TBGRABitmap; mX, mY: single;
  Ellipse, SkipTransparent: boolean): TStringList;

  function BGRAtoRGBAstr(bgra: TBGRAPixel): string;
  begin
    Result := 'style="fill:rgb(' + IntToStr(bgra.red) + ',' +
      IntToStr(bgra.green) + ',' + IntToStr(bgra.blue) + '); fill-opacity:' +
      FloatToStr(bgra.alpha / 255) + ';';
  end;

var
  x, y: integer;
  p: PBGRAPixel;
begin
  Result := TStringList.Create;
  Result.Add('<?xml version="1.0" encoding="UTF-8" standalone="no"?>');
  Result.Add('<svg '{width="' + IntToStr(bitmap.Width) + '" height="' +
    IntToStr(bitmap.Height) + '" viewPort="0 0 ' + IntToStr(bitmap.Width) +
    ' ' + IntToStr(bitmap.Height) + '" '} +
    'xmlns="http://www.w3.org/2000/svg" version="1.1">');
  Result.Add('  <g>');

  for y := 0 to Bitmap.Height - 1 do
  begin
    p := Bitmap.Scanline[y];
    for x := 0 to Bitmap.Width - 1 do
    begin
      if (SkipTransparent) and (p^.alpha = 0) then
      // nothing
      else
      if Ellipse then
        Result.Add('    <ellipse cx="' + FloatToStr(x * mX) +
          '.5" cy="' + FloatToStr(y * mY) + '.5" rx="' + FloatToStr(0.5 * mX) +
          '" ry="' + FloatToStr(0.5 * mY) + '" ' + BGRAtoRGBAstr(p^) + '"/>')
      else
        Result.Add('    <rect x="' + FloatToStr(x * mX) + '.5" y="' +
          FloatToStr(y * mY) + '.5" width="' + FloatToStr(1 * mX) +
          '" height="' + FloatToStr(1 * mY) + '" ' + BGRAtoRGBAstr(p^) + '"/>');
      Inc(p);
    end;
  end;

  Result.Add('  </g>');
  Result.Add('</svg>');
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  s: TStringList;
begin
  DecimalSeparator := '.';

  bitmap := TBGRABitmap.Create('lazpaint.png');
  s := Pix2svg(bitmap, 1, 1, True, True);
  s.SaveToFile('lazpaint.svg');
  s.Free;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  bitmap.Free;
end;

end.
