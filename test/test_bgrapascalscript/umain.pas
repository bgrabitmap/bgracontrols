unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BGRAPascalScript, uPSI_BGRAPascalScript, BGRAVirtualScreen, uPSComponent,
  BGRABitmap, BCTypes,
  BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    PSImport_BGRAPascalScript1: TPSImport_BGRAPascalScript;
    PSScript1: TPSScript;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  PSScript1.Script := Memo1.Lines;
  if PSScript1.Compile then
  begin
    PSScript1.Execute;
    Button2.Enabled := True;
  end;
end;

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  if Button2.Enabled then
    Bitmap.StretchPutImage(Rect(0, 0, 32, 32), BitmapArray[0], dmSet);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  BGRAVirtualScreen1.RedrawBitmap;
end;

end.
