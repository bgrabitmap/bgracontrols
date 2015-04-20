unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BCTrackbarUpdown, BCButton, BCLabel, BCPanel;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCButton1: TBCButton;
    BCLabel1: TBCLabel;
    BCPanel1: TBCPanel;
    BCTrackbarUpdown1: TBCTrackbarUpdown;
    BCTrackbarUpdown2: TBCTrackbarUpdown;
    BCTrackbarUpdown3: TBCTrackbarUpdown;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    procedure BCTrackbarUpdown3Change(Sender: TObject; AByUser: boolean);
    procedure CheckBox1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    changeCount: integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BCTrackbarUpdown3Change(Sender: TObject; AByUser: boolean);
begin
  inc(changeCount);
  label1.Caption := '#'+inttostr(changeCount)+': '+inttostr(TBCTrackbarUpdown(Sender).Value);
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  BCTrackbarUpdown1.Enabled := CheckBox1.Checked;
end;

end.

