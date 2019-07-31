unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, BCComboBox,
  BCListBox, LCLType, BCSamples;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCComboBox1: TBCComboBox;
    procedure FormCreate(Sender: TObject);
  private
    procedure OnListBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Adding items
  BCComboBox1.Items.Add('One');
  BCComboBox1.Items.Add('Two');
  BCComboBox1.Items.Add('Three');

  // Selecting items
  BCComboBox1.ListBox.ItemIndex := 0;

  // Style ListBox
  BCComboBox1.ListBox.Style := lbOwnerDrawFixed;
  BCComboBox1.ListBox.OnDrawItem := @OnListBoxDrawItem;
  BCComboBox1.ListBox.Color := clGray;
  BCComboBox1.ListBox.ItemHeight := 2 * Canvas.GetTextHeight('aq');
  BCComboBox1.ListBox.Options := []; // do not draw focus rect

  // Style Button
  StyleButtonsSample(BCComboBox1.Button, TBCSampleStyle.ssFlashPlayer);
end;

procedure TForm1.OnListBoxDrawItem(Control: TWinControl; Index: integer;
  ARect: TRect; State: TOwnerDrawState);
var
  aCanvas: TCanvas;
begin
  aCanvas := TListBox(Control).Canvas;

  // selected item
  if odSelected in State then
    aCanvas.Brush.Color := clBlack
  else
    aCanvas.Brush.Color := clGray;

  aCanvas.Font.Color := clWhite;
  aCanvas.FillRect(ARect);

  // mouse over
  if Index = BCComboBox1.HoverItem then
  begin
    aCanvas.Pen.Color := clRed;
    aCanvas.Rectangle(ARect);
  end;

  // vertically centered text
  aCanvas.TextRect(ARect, 15, ARect.Top +
    (aCanvas.GetTextHeight(TListBox(Control).Items[Index]) div 2),
    TListBox(Control).Items[Index]);
end;

end.
