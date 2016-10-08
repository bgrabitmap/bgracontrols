unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BCKeyboard, BCDefaultThemeManager;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCDefaultThemeManager1: TBCDefaultThemeManager;
    BCKeyboard1: TBCKeyboard;
    Edit1: TEdit;
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  BCDefaultThemeManager1.Apply();
  BCKeyboard1.Panel.Left:=0;
  BCKeyboard1.Panel.Top:=Edit1.Height;
  BCKeyboard1.Show();
  Form1.Width := BCKeyboard1.Panel.Width;
end;

end.

