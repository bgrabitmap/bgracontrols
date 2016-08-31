unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRACustomDrawn, BCPanel;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    BCDButton1: TBCDButton;
    BCDButton2: TBCDButton;
    BCDEdit1: TBCDEdit;
    BCDEdit2: TBCDEdit;
    BCDProgressBar1: TBCDProgressBar;
    BCDSpinEdit1: TBCDSpinEdit;
    BCDStaticText1: TBCDStaticText;
    BCDStaticText2: TBCDStaticText;
    BCPanel1: TBCPanel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Self.AutoAdjustLayout(lapAutoAdjustForDPI, Self.DesignTimeDPI,
    Screen.PixelsPerInch, Self.Width, ScaleX(Self.Width, Self.DesignTimeDPI));
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  if BCDProgressBar1.Position <> BCDProgressBar1.Max then
    BCDProgressBar1.Position := BCDProgressBar1.Position + 1
  else
    Timer1.Enabled := False;
end;

end.
