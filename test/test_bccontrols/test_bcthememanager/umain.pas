unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BCNumericKeyboard, BCButton, BCDefaultThemeManager,
  BCButtonFocus;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCButton1: TBCButton;
    BCButton2: TBCButton;
    BCButton3: TBCButton;
    BCButton4: TBCButton;
    BCButtonFocus1: TBCButtonFocus;
    BCButtonFocus2: TBCButtonFocus;
    BCDefaultThemeManager1: TBCDefaultThemeManager;
    BCNumericKeyboard1: TBCNumericKeyboard;
    BCRealNumericKeyboard1: TBCRealNumericKeyboard;
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
  { Apply to all buttons in this form }
  BCDefaultThemeManager1.Apply();

  BCNumericKeyboard1.Panel.Left := 200;
  BCNumericKeyboard1.Panel.Top := 10;
  BCNumericKeyboard1.Show();

  BCRealNumericKeyboard1.Panel.Left := 200;
  BCRealNumericKeyboard1.Panel.Top :=
    BCNumericKeyboard1.Panel.Top + BCNumericKeyboard1.Panel.Height + 10;
  BCRealNumericKeyboard1.Show();
end;

end.
