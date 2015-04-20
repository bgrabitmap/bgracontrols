unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  BCToolBar, BGRABitmap;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCToolBar1: TBCToolBar;
    ToolButton1: TToolButton;
    procedure BCToolBar1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
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

procedure TForm1.BCToolBar1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  DrawWindows7ToolBar(Bitmap);
end;

end.
