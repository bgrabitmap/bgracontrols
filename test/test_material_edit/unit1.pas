unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BCMaterialEdit, BCMaterialFloatSpinEdit, BCMaterialSpinEdit;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCMaterialEdit1: TBCMaterialEdit;
    BCMaterialFloatSpinEdit1: TBCMaterialFloatSpinEdit;
    BCMaterialSpinEdit1: TBCMaterialSpinEdit;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure BCMaterialEdit1Change(Sender: TObject);
    procedure BCMaterialFloatSpinEdit1Change(Sender: TObject);
    procedure BCMaterialSpinEdit1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BCMaterialEdit1Change(Sender: TObject);
begin
  Label1.Caption := BCMaterialEdit1.Edit.Text;
end;

procedure TForm1.BCMaterialFloatSpinEdit1Change(Sender: TObject);
begin
  Label2.Caption := BCMaterialFloatSpinEdit1.Edit.Text;
end;

procedure TForm1.BCMaterialSpinEdit1Change(Sender: TObject);
begin
  Label3.Caption := BCMaterialSpinEdit1.Edit.Text;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Self.Color := clBlack;

  Label1.Font.Color := clWhite;
  Label2.Font.Color := clWhite;
  Label3.Font.Color := clWhite;

  BCMaterialEdit1.Color := clBlack;
  BCMaterialEdit1.Edit.Color := clBlack;
  BCMaterialEdit1.Edit.Font.Color := clWhite;
  BCMaterialEdit1.Font.Color := clWhite;

  BCMaterialFloatSpinEdit1.Color := clBlack;
  BCMaterialFloatSpinEdit1.Edit.Color := clBlack;
  BCMaterialFloatSpinEdit1.Edit.Font.Color := clWhite;
  BCMaterialFloatSpinEdit1.Font.Color := clWhite;

  BCMaterialSpinEdit1.Color := clBlack;
  BCMaterialSpinEdit1.Edit.Color := clBlack;
  BCMaterialSpinEdit1.Edit.Font.Color := clWhite;
  BCMaterialSpinEdit1.Font.Color := clWhite;
end;

end.

