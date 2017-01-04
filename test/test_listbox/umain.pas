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
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

