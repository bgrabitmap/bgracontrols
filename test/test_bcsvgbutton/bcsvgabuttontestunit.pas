unit bcsvgabuttontestunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  BCSVGButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCSVGButton1: TBCSVGButton;
    BCSVGButton2: TBCSVGButton;
    BCSVGButton3: TBCSVGButton;
    BCSVGButton4: TBCSVGButton;
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
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
  form1.DoubleBuffered:=true;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  BCSVGButton1.SetBounds(0,0,Panel2.width div 2, Panel2.height div 2);
  BCSVGButton2.SetBounds(Panel2.width div 2,0,Panel2.width div 2, Panel2.height div 2);
  BCSVGButton3.SetBounds(0,Panel2.height div 2,Panel2.width div 2, Panel2.height div 2);
  BCSVGButton4.SetBounds(Panel2.width div 2,Panel2.height div 2,Panel2.width div 2, Panel2.height div 2);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  BCSVGButton1.Down:=True;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  BCSVGButton1.Down:=False;
end;

end.

