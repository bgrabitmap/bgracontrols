unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  BCMaterialProgressBarMarquee, BCFluentProgressRing;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    TrackBar1: TTrackBar;
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    ring: TBCFluentProgressRing;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  ring:= TBCFluentProgressRing.Create(self);
  ring.Width:= 180;
  ring.Height:= 180;
  ring.Left:= 10;
  ring.top:= 10;
  ring.Value:= 75;
  ring.Parent:= self;

end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  ring.Value:= TrackBar1.Position;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  ring.Indeterminate:= CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Change(Sender: TObject);
begin
  ring.Enabled:= CheckBox2.Checked;
end;

end.

