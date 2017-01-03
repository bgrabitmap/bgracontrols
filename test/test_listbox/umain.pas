unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, BCListBox;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCPaperListBox1: TBCPaperListBox;
    BCPaperListBox2: TBCPaperListBox;
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
begin
  BCPaperListBox1.ListBox.MultiSelect := True;
  with BCPaperListBox1.ListBox.Items do
  begin
    Add('One');
    Add('Two');
    Add('Three');
    Add('Four');
    Add('Five');
  end;

  with BCPaperListBox2.ListBox.Items do
  begin
    Add('One');
    Add('Two');
    Add('Three');
    Add('Four');
    Add('Five');
  end;
end;

end.

