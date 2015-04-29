unit utest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BGRABitmapLibrary;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
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
  BGRABitmapLibrary.Create(0);
  BGRABitmapLibrary.CreateWithSize(1,100,100);
  ShowMessage(IntToStr(BGRABitmapLibrary.GetHighestID()));
  BGRABitmapLibrary.Fill(1, BGRABitmapLibrary.rgb(0, 255, 0));
  BGRABitmapLibrary.FilterSmartZoom3(0, moNone);
  BGRABitmapLibrary.SaveToFile(1, 'test.png');
  BGRABitmapLibrary.Destroy(1);
  BGRABitmapLibrary.Destroy(0);
end;

end.

