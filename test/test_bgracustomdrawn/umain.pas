unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BGRACustomDrawn;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    BCDButton1: TBCDButton;
    BCDButton2: TBCDButton;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

end.

