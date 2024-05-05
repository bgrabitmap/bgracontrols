unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BCFluentSlider, BCFluentProgressRing;

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
    procedure DestroyInternalEdit(Data: PtrInt);
    procedure InternalEditEditingDone(Sender: TObject);
    procedure SliderChangeValue(Sender: TObject);
    procedure SliderDblClick(Sender: TObject);

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
    OnDblClick:= @SliderDblClick;
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

procedure TForm1.SliderDblClick(Sender: TObject);
var
  ed: TEdit;
begin
  ed:= TEdit.Create(self);
  ed.AutoSelect:= false;
  if (Slider.Orientation = pbHorizontal) or (Slider.Orientation =pbRightToLeft) then
  begin
    ed.Top:= Slider.BoundsRect.Bottom;
    ed.Left:= Slider.Left + Slider.ThumbPosition.X - Slider.ThumbRadius;
  end
  else
  begin
    ed.Top:= Slider.Top + Slider.ThumbPosition.Y - Slider.ThumbRadius;
    ed.Left:= Slider.BoundsRect.Right;
  end;
  ed.Parent:= self;
  ed.OnEditingDone:= @InternalEditEditingDone;
  ed.OnMouseLeave:= @InternalEditEditingDone;
  ed.SetFocus;
end;

procedure TForm1.DestroyInternalEdit(Data: PtrInt);
var
  ed: TEdit;
begin
  ed:= TEdit(Data);
  ed.Free;
end;

procedure TForm1.InternalEditEditingDone(Sender: TObject);
var
  ed: TEdit;
begin
  if Sender is TEdit then
  begin
    ed:= TEdit(Sender);
    Label1.Caption:= ed.Text;
    ed.OnEditingDone:= nil;
    ed.OnMouseLeave:= nil;
    ed.Visible:= false;
    Application.QueueAsyncCall(@DestroyInternalEdit, PtrInt(ed));
  end;
end;

end.

