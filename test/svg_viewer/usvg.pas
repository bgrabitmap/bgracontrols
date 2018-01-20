unit usvg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, BCSVGViewer, BCPanel, BCButton, BCTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCButton_Open: TBCButton;
    BCPanel1: TBCPanel;
    BCSVGViewer1: TBCSVGViewer;
    CheckBox_Proportional: TCheckBox;
    ComboBox_HorizAlign: TComboBox;
    ComboBox_VertAlign: TComboBox;
    ComboBox_StrechMode: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    OpenDialog1: TOpenDialog;
    procedure BCButton_OpenClick(Sender: TObject);
    procedure CheckBox_ProportionalChange(Sender: TObject);
    procedure ComboBox_HorizAlignChange(Sender: TObject);
    procedure ComboBox_StrechModeChange(Sender: TObject);
    procedure ComboBox_VertAlignChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BCButton_OpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    try
      BCSVGViewer1.LoadFromFile(OpenDialog1.FileName);
    except
      on ex: Exception do
        ShowMessage(ex.Message);
    end;
    BCSVGViewer1.Invalidate;
  end;
end;

procedure TForm1.CheckBox_ProportionalChange(Sender: TObject);
begin
  BCSVGViewer1.Proportional := CheckBox_Proportional.Checked;
end;

procedure TForm1.ComboBox_HorizAlignChange(Sender: TObject);
begin
  Case ComboBox_HorizAlign.ItemIndex of
  0: BCSVGViewer1.HorizAlign:= taLeftJustify;
  1: BCSVGViewer1.HorizAlign:= taCenter;
  2: BCSVGViewer1.HorizAlign:= taRightJustify;
  end;
end;

procedure TForm1.ComboBox_StrechModeChange(Sender: TObject);
begin
  Case ComboBox_StrechMode.ItemIndex of
  0: BCSVGViewer1.StretchMode:= smNone;
  1: BCSVGViewer1.StretchMode:= smShrink;
  2: BCSVGViewer1.StretchMode:= smStretch;
  end;
end;

procedure TForm1.ComboBox_VertAlignChange(Sender: TObject);
begin
  Case ComboBox_VertAlign.ItemIndex of
  0: BCSVGViewer1.VertAlign:= tlTop;
  1: BCSVGViewer1.VertAlign:= tlCenter;
  2: BCSVGViewer1.VertAlign:= tlBottom;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BCSVGViewer1.StretchMode := smShrink;
  BCSVGViewer1.Proportional:= CheckBox_Proportional.Checked;
  BCSVGViewer1.DrawCheckers := true;
end;

end.

