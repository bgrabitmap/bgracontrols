unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type
  TBGRAColor = longword;

procedure bgra_CreateFromFile(id: integer; AFilename: string); stdcall;
  external 'bgrabitmap.dll' Name 'bgra_CreateFromFile';
procedure bgra_Destroy(id: integer); stdcall;
  external 'bgrabitmap.dll' Name 'bgra_Destroy';
function bgra_GetPixel(id: integer; x, y: integer): TBGRAColor; stdcall;
  external 'bgrabitmap.dll' Name 'bgra_GetPixel';

type

  { TForm1 }

  TForm1 = class(TForm)
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
var
  pixel: TBGRAColor;
begin
  bgra_CreateFromFile(0, '16.png');
  pixel := bgra_GetPixel(0, 0, 0);
  ShowMessage(IntToStr(pixel));
  bgra_Destroy(0);

  pixel := bgra_GetPixel(0, 0, 0);
  ShowMessage(IntToStr(pixel));
end;

end.
