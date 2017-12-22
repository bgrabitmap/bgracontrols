unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BCSVGViewer, BGRABitmap, BCTypes,
  BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCSVGViewer1: TBCSVGViewer;
    procedure BCSVGViewer1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure BCSVGViewer1Resize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  //BCSVGViewer1.LoadFromFile('SVG_Logo.svg');
  BCSVGViewer1.LoadFromResource('01 CIRCLE');
end;

procedure TForm1.BCSVGViewer1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin

end;

procedure TForm1.BCSVGViewer1Resize(Sender: TObject);
begin

end;

end.

