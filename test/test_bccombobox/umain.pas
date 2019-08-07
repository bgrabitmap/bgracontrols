unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, BCComboBox,
  BCListBox, LCLType, BCSamples, BGRAThemeRadioButton, BGRAColorTheme, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCComboBox1: TBCComboBox;
    BGRAColorTheme1: TBGRAColorTheme;
    Label1: TLabel;
    Label2: TLabel;
    RadioCustom: TBGRAThemeRadioButton;
    RadioFlash: TBGRAThemeRadioButton;
    RadioWin7: TBGRAThemeRadioButton;
    procedure BCComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioButtonChange(Sender: TObject);
  private
    procedure OnBCComboBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  public
    procedure ApplyFlashStyle;
    procedure ApplyWin7Style;
    procedure ApplyCustomStyle;
    procedure UpdateStyle;

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
  BCComboBox1.ItemIndex := 0;

  // Style drop down
  UpdateStyle;
  BCComboBox1.Button.StateNormal.FontEx.Height := 16;
  BCComboBox1.Button.StateHover.FontEx.Height := 16;
  BCComboBox1.Button.StateClicked.FontEx.Height := 16;
end;

procedure TForm1.RadioButtonChange(Sender: TObject);
begin
  UpdateStyle;
end;

procedure TForm1.BCComboBox1Change(Sender: TObject);
begin
  Label1.Caption := 'Changed to '+BCComboBox1.Text;
end;

procedure TForm1.OnBCComboBoxDrawItem(Control: TWinControl; Index: integer;
  ARect: TRect; State: TOwnerDrawState);
var
  aCanvas: TCanvas;
  s: String;
begin
  aCanvas := BCComboBox1.Canvas;
  s := BCComboBox1.Items[Index];

  // selected item
  if odChecked in State then s := '√ '+s;

  if odSelected in State then
    aCanvas.Brush.Color := clBlack
  else
  if odChecked in State then
    aCanvas.Brush.Color := $505050
  else
    aCanvas.Brush.Color := clGray;

  aCanvas.Font.Color := clWhite;
  aCanvas.FillRect(ARect);

  // mouse over
  if odSelected in State then
  begin
    aCanvas.Pen.Style := psSolid;
    aCanvas.Pen.Color := clRed;
    aCanvas.Rectangle(ARect);
  end;

  // vertically centered text
  aCanvas.TextRect(ARect, 15, ARect.Top +
    (ARect.Height - aCanvas.GetTextHeight(s)) div 2, s);
end;

procedure TForm1.ApplyFlashStyle;
begin
  StyleButtonsSample(BCComboBox1.Button, TBCSampleStyle.ssFlashPlayer);
  BCComboBox1.DropDownColor := $606060;
  BCComboBox1.DropDownFontColor := $c0c0c0;
  BCComboBox1.DropDownBorderSize:= 2;
  BCComboBox1.DropDownBorderColor:= $404040;
  BCComboBox1.DropDownHighlight := $FC992E;
  BCComboBox1.DropDownFontHighlight := clWhite;
  BCComboBox1.OnDrawItem := nil;
end;

procedure TForm1.ApplyWin7Style;
begin
  StyleButtonsSample(BCComboBox1.Button, TBCSampleStyle.ssWindows7);
  BCComboBox1.DropDownColor := clWhite;
  BCComboBox1.DropDownFontColor := clBlack;
  BCComboBox1.DropDownBorderSize:= 1;
  BCComboBox1.DropDownBorderColor:= clBlack;
  BCComboBox1.DropDownHighlight := $FC992E;
  BCComboBox1.DropDownFontHighlight := clWhite;
  BCComboBox1.OnDrawItem := nil;
end;

procedure TForm1.ApplyCustomStyle;
begin
  StyleButtonsSample(BCComboBox1.Button, TBCSampleStyle.ssDefault);
  BCComboBox1.DropDownColor := clGray;
  BCComboBox1.DropDownBorderSize:= 3;
  BCComboBox1.DropDownBorderColor:= clGreen;
  BCComboBox1.OnDrawItem := @OnBCComboBoxDrawItem;
  BCComboBox1.ItemHeight := 2*Canvas.GetTextHeight('aq');
end;

procedure TForm1.UpdateStyle;
begin
  if RadioWin7.Checked then ApplyWin7Style
  else if RadioFlash.Checked then ApplyFlashStyle
  else ApplyCustomStyle;
end;

end.
