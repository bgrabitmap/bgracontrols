{ BCComboBox with custom colors, saved in the legacy LFM format
  (TColor properties and TColor + opacity pairs). }
unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, BCComboBox;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCComboBox1: TBCComboBox;
    BCComboBox2: TBCComboBox;
    BCComboBox3: TBCComboBox;
    BCComboBox4: TBCComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.
