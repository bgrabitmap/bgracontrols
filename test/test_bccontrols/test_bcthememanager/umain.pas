unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, BCNumericKeyboard, BCButton, BCDefaultThemeManager, BCButtonFocus,
  BCSamples;

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
    ComboBox1: TComboBox;
    Panel1: TPanel;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  BCNumericKeyboard1.Panel.Top := 50;
  BCNumericKeyboard1.Show();

  BCRealNumericKeyboard1.Panel.Left := 200;
  BCRealNumericKeyboard1.Panel.Top :=
    BCNumericKeyboard1.Panel.Top + BCNumericKeyboard1.Panel.Height + 10;
  BCRealNumericKeyboard1.Show();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BCSampleStyleStrList(ComboBox1.Items);
  ComboBox1.ItemIndex := 0;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  BCDefaultThemeManager1.BCStyle := StrToTBCSampleStyle(ComboBox1.Caption);
  BCDefaultThemeManager1.Apply();
end;

end.
