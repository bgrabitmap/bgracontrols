unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BCMDButtonFocus,
  BCMDButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCMDButton1: TBCMDButton;
    BCMDButtonFocus1: TBCMDButtonFocus;
    BCMDButtonFocus10: TBCMDButtonFocus;
    BCMDButtonFocus11: TBCMDButtonFocus;
    BCMDButtonFocus12: TBCMDButtonFocus;
    BCMDButtonFocus2: TBCMDButtonFocus;
    BCMDButtonFocus3: TBCMDButtonFocus;
    BCMDButtonFocus4: TBCMDButtonFocus;
    BCMDButtonFocus5: TBCMDButtonFocus;
    BCMDButtonFocus6: TBCMDButtonFocus;
    BCMDButtonFocus7: TBCMDButtonFocus;
    BCMDButtonFocus8: TBCMDButtonFocus;
    BCMDButtonFocus9: TBCMDButtonFocus;
    procedure FormCreate(Sender: TObject);
    procedure BCMDButton1Click(Sender: TObject);
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
  DoubleBuffered := True;
end;

procedure TForm1.BCMDButton1Click(Sender: TObject);
begin
  BCMDBUTTONANIMATION := BCMDButton1.Checked;
end;

end.

