unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BCFluentSlider;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label1: TLabel;
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SliderChangeValue(Sender: TObject);

  public
    Slider: TBCFluentSlider;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Slider:= TBCFluentSlider.Create(self);
  with Slider do
  begin
    Top:= 10;
    Left:= 10;
    Width:= 240;
    OnChangeValue:= @SliderChangeValue;
    Parent:= self;
  end;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  Slider.ShowTicks:= CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Change(Sender: TObject);
begin
  if CheckBox2.Checked then
    Slider.Orientation:= pbVertical
  else
    Slider.Orientation:= pbHorizontal;
end;

procedure TForm1.SliderChangeValue(Sender: TObject);
begin
  Label1.Caption:= 'Value: '+IntToStr(Slider.Value);
end;

end.

