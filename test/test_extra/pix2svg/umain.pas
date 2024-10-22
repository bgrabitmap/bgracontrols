unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BCFilters, BGRABitmap, BGRABitmapTypes;

type

  TPix2SVGStyle = (pix2rectangle, pix2ellipse, pix2hexagon);

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

function Pix2svg(Bitmap: TBGRABitmap; mX, mY: single; Style: TPix2SVGStyle;
  SkipTransparent: boolean): TStringList;

  function BGRAtoRGBAstr(bgra: TBGRAPixel): string;
  begin
    Result := 'style="fill:rgb(' + IntToStr(bgra.red) + ',' +
      IntToStr(bgra.green) + ',' + IntToStr(bgra.blue) + '); fill-opacity:' +
      FloatToStr(bgra.alpha / 255) + ';';
  end;

  function hexagonStr(x, y: integer; p: PBGRAPixel): string;
  var
    p1, p2, p3, p4, p5, p6: string;
  begin
    if odd(y) then // impar
    begin
      p1 := FloatToStr(x + 1 / 2) + ' ' + FloatToStr(0.75 * y + 0.5 / 2) + ', ';
      p2 := FloatToStr(x + 2 / 2) + ' ' + FloatToStr(0.75 * y + 1 / 2) + ', ';
      p3 := FloatToStr(x + 2 / 2) + ' ' + FloatToStr(0.75 * y + 2 / 2) + ', ';
      p4 := FloatToStr(x + 1 / 2) + ' ' + FloatToStr(0.75 * y + 2.5 / 2) + ', ';
      p5 := FloatToStr(x + 0 / 2) + ' ' + FloatToStr(0.75 * y + 2 / 2) + ', ';
      p6 := FloatToStr(x + 0 / 2) + ' ' + FloatToStr(0.75 * y + 1 / 2);
    end
    else // par
    begin
      p1 := FloatToStr(0.5 + x + 1 / 2) + ' ' + FloatToStr(0.75 * y + 0.5 / 2) + ', ';
      p2 := FloatToStr(0.5 + x + 2 / 2) + ' ' + FloatToStr(0.75 * y + 1 / 2) + ', ';
      p3 := FloatToStr(0.5 + x + 2 / 2) + ' ' + FloatToStr(0.75 * y + 2 / 2) + ', ';
      p4 := FloatToStr(0.5 + x + 1 / 2) + ' ' + FloatToStr(0.75 * y + 2.5 / 2) + ', ';
      p5 := FloatToStr(0.5 + x + 0 / 2) + ' ' + FloatToStr(0.75 * y + 2 / 2) + ', ';
      p6 := FloatToStr(0.5 + x + 0 / 2) + ' ' + FloatToStr(0.75 * y + 1 / 2);
    end;
    Result := '    <polygon points="' + p1 + p2 + p3 + p4 + p5 +
      p6 + '" ' + BGRAtoRGBAstr(p^) + '" />';
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
        case Style of
          pix2ellipse: Result.Add('    <ellipse cx="' + FloatToStr(x * mX) +
              '.5" cy="' + FloatToStr(y * mY) + '.5" rx="' +
              FloatToStr(0.5 * mX) + '" ry="' + FloatToStr(0.5 * mY) +
              '" ' + BGRAtoRGBAstr(p^) + '"/>');
          pix2rectangle: Result.Add('    <rect x="' + FloatToStr(x * mX) +
              '.5" y="' + FloatToStr(y * mY) + '.5" width="' +
              FloatToStr(1 * mX) + '" height="' + FloatToStr(1 * mY) +
              '" ' + BGRAtoRGBAstr(p^) + '"/>');
          pix2hexagon: Result.Add(hexagonStr(x, y, p));
        end;
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
  s := Pix2svg(bitmap, 1, 1, pix2hexagon, True);
  s.SaveToFile('lazpaint.svg');
  s.Free;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  bitmap.Free;
end;

end.
