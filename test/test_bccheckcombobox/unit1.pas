unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CheckLst, StdCtrls,
  BCCheckComboBox;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCCheckComboBox1: TBCCheckComboBox;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
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

end;

procedure TForm1.Button1Click(Sender: TObject);
var
  s: String = '';
  i: integer;
begin
  for i:=0 to BCCheckComboBox1.Items.Count-1 do
  begin
    if (BCCheckComboBox1.ListBox.Checked[i]) then
      s += BCCheckComBoBox1.Items[i] + LineEnding;
  end;
  if (s = '') then
    ShowMessage('No one is checked')
  else
    ShowMessage('Checked items: ' + LineEnding + s);
end;

end.

