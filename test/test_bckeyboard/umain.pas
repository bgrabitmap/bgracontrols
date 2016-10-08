unit umain;

{$mode objfpc}{$H+}

interface

uses
  Forms, BCKeyboard, BCDefaultThemeManager, BGRACustomDrawn;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCDefaultThemeManager1: TBCDefaultThemeManager;
    BCDPanel1: TBCDPanel;
    BCKeyboard1: TBCKeyboard;
    Edit1: TBCDEdit;
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
  BCKeyboard1.PanelsColor := $00535353;
  BCKeyboard1.Panel.Left := 0;
  BCKeyboard1.Panel.Top := Edit1.Height + 2;
  BCKeyboard1.Show();
  Form1.Width := BCKeyboard1.Panel.Width;
end;

end.

