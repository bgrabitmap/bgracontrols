unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  BCRadialProgressBar, BCLabel, BGRASpriteAnimation, BCMDButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCRadialProgressBar1: TBCRadialProgressBar;
    BGRASpriteAnimation1: TBGRASpriteAnimation;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    BCMDButton1: TBCMDButton;
    BCMDButton2: TBCMDButton;
    BCMDButton3: TBCMDButton;
    BCMDButton4: TBCMDButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
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
  {$ifdef windows}
  //DoubleBuffered := True;
  {$endif}
end;

end.

