unit test_bcExpandPanel_unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
  ExtCtrls, StdCtrls, Buttons, Spin, BCExpandPanels, StrUtils;

type

  { TbcExpandPanelForm1 }

  TbcExpandPanelForm1 = class(TForm)
    cbRounded: TCheckBox;
    cbFlat: TCheckBox;
    cbBorders: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    p1: TBCExpandPanel;
    RGlyph: TRadioGroup;
    RCapt: TRadioGroup;
    RColl:      TRadioGroup;
    RButt:      TRadioGroup;
    RGlyphKind: TRadioGroup;
    RStyle: TComboBox;
    SpeedButton1: TSpeedButton;
    edTabWidth: TSpinEdit;
    edButtonSize: TSpinEdit;
    procedure cbFlatClick(Sender: TObject);
    procedure cbRoundedClick(Sender: TObject);
    procedure cbBordersChange(Sender: TObject);
    procedure edButtonSizeChange(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure edTabWidthChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RGlyphClick(Sender: TObject);
    procedure RCaptClick(Sender: TObject);
    procedure RButtClick(Sender: TObject);
    procedure RCollClick(Sender: TObject);
    procedure RGlyphKindClick(Sender: TObject);
    procedure RStyleChange(Sender: TObject);
    procedure RDirectionClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  bcExpandPanelForm1: TbcExpandPanelForm1;

implementation

{ TbcExpandPanelForm1 }


procedure TbcExpandPanelForm1.Edit1Change(Sender: TObject);
begin
   p1.Button.Caption := Edit1.Text;
end;

procedure TbcExpandPanelForm1.cbRoundedClick(Sender: TObject);
begin
  p1.BevelRounded:=cbRounded.Checked;
end;

procedure TbcExpandPanelForm1.cbBordersChange(Sender: TObject);
begin
  p1.BevelOuter:=TBevelcut(cbBorders.ItemIndex);
end;

procedure TbcExpandPanelForm1.edButtonSizeChange(Sender: TObject);
begin
  p1.ButtonSize:=edButtonSize.Value;
end;

procedure TbcExpandPanelForm1.cbFlatClick(Sender: TObject);
begin
  p1.Button.Flat:=cbFlat.Checked;
end;

procedure TbcExpandPanelForm1.edTabWidthChange(Sender: TObject);
begin
  p1.Button.TabWidth:=edTabWidth.Value;
end;

procedure TbcExpandPanelForm1.FormShow(Sender: TObject);
begin
  edButtonSize.Value:=p1.ButtonSize;
  edTabWidth.Value:=p1.Button.TabWidth;
end;

procedure TbcExpandPanelForm1.RGlyphClick(Sender: TObject);
begin
  p1.Button.GlyphLayout:=TGlyphLayout(RGlyph.ItemIndex);
end;

procedure TbcExpandPanelForm1.RCaptClick(Sender: TObject);
begin
     p1.Button.TextLayout:=TTextLayout(RCapt.ItemIndex);
end;


procedure TbcExpandPanelForm1.RButtClick(Sender: TObject);
begin
  p1.ButtonPosition := TAnchorKind(RButt.ItemIndex);
end;

procedure TbcExpandPanelForm1.RCollClick(Sender: TObject);
begin
  p1.CollapseKind := TAnchorKind(RColl.ItemIndex);
end;

procedure TbcExpandPanelForm1.RGlyphKindClick(Sender: TObject);
begin
  p1.Button.GlyphKind:=TGlyphKind(RGlyphKind.ItemIndex);
end;

procedure TbcExpandPanelForm1.RStyleChange(Sender: TObject);
begin
  p1.Button.Style:=TBCBoundButtonStyle(RStyle.ItemIndex);
end;

procedure TbcExpandPanelForm1.RDirectionClick(Sender: TObject);
begin
end;

procedure TbcExpandPanelForm1.SpeedButton1Click(Sender: TObject);
begin
     p1.Button.Caption := DupeString(Edit1.Caption, 10);
end;

initialization
  {$I test_bcexpandpanel_unit1.lrs}

end.
