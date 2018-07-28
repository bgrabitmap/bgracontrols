unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  BCMDButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCMDButton10: TBCMDButton;
    BCMDButton11: TBCMDButton;
    BCMDButton12: TBCMDButton;
    BCMDButton13: TBCMDButton;
    BCMDButton4: TBCMDButton;
    BCMDButton5: TBCMDButton;
    BCMDButton6: TBCMDButton;
    BCMDButton7: TBCMDButton;
    BCMDButton8: TBCMDButton;
    BCMDButton9: TBCMDButton;
    PageControl1: TPageControl;
    Panel2: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure BCMDButton10Click(Sender: TObject);
    procedure BCMDButton11Click(Sender: TObject);
    procedure BCMDButton12Click(Sender: TObject);
    procedure BCMDButton13Click(Sender: TObject);
    procedure BCMDButton1Click(Sender: TObject);
    procedure BCMDButton2Click(Sender: TObject);
    procedure BCMDButton3Click(Sender: TObject);
    procedure BCMDButton4Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BCMDButton1Click(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
end;

procedure TForm1.BCMDButton10Click(Sender: TObject);
begin
  Panel2.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  Panel2.Align := alTop;
end;

procedure TForm1.BCMDButton11Click(Sender: TObject);
begin
  Panel2.ChildSizing.Layout := cclTopToBottomThenLeftToRight;
  Panel2.Align := alRight;
end;

procedure TForm1.BCMDButton12Click(Sender: TObject);
begin
  Panel2.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  Panel2.Align := alBottom;
end;

procedure TForm1.BCMDButton13Click(Sender: TObject);
begin
  BCMDBUTTONANIMATION := BCMDButton13.Checked;
end;

procedure TForm1.BCMDButton2Click(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 1;
end;

procedure TForm1.BCMDButton3Click(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 2;
end;

procedure TForm1.BCMDButton4Click(Sender: TObject);
begin
  Panel2.ChildSizing.Layout := cclTopToBottomThenLeftToRight;
  Panel2.Align := alLeft;
end;

end.

