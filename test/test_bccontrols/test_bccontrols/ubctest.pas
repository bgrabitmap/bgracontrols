unit ubctest;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls,
  StdCtrls, types, LCLType, ExtCtrls,
  BCButton, BCLabel, BCPanel, BGRAShape, BCSamples;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCButton1: TBCButton;
    BCButton2: TBCButton;
    BCButton3: TBCButton;
    BCButton4: TBCButton;
    BCLabel1: TBCLabel;
    BCPanel1: TBCPanel;
    ComboBox1: TComboBox;
    ListBox1: TListBox;
    Shape1: TBGRAShape;
    Shape10: TBGRAShape;
    Shape11: TBGRAShape;
    Shape12: TBGRAShape;
    Shape13: TBGRAShape;
    Shape14: TBGRAShape;
    Shape15: TBGRAShape;
    Shape16: TBGRAShape;
    Shape17: TBGRAShape;
    Shape18: TBGRAShape;
    Shape19: TBGRAShape;
    Shape2: TBGRAShape;
    Shape20: TBGRAShape;
    Shape21: TBGRAShape;
    Shape22: TBGRAShape;
    Shape23: TBGRAShape;
    Shape24: TBGRAShape;
    Shape25: TBGRAShape;
    Shape26: TBGRAShape;
    Shape3: TBGRAShape;
    Shape4: TBGRAShape;
    Shape5: TBGRAShape;
    Shape6: TBGRAShape;
    Shape7: TBGRAShape;
    Shape8: TBGRAShape;
    Shape9: TBGRAShape;
    procedure FormCreate(Sender: TObject);
    procedure ListDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
    procedure Shape23MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  Shape1.FillColor := clScheme1_Background;
  Shape1.BorderColor := clScheme1_Selection;

  Shape2.FillColor := clScheme2_Background;
  Shape2.BorderColor := clScheme2_Selection;

  Shape3.FillColor := clScheme3_Background;
  Shape3.BorderColor := clScheme3_Selection;

  Shape4.FillColor := clScheme4_Background;
  Shape4.BorderColor := clScheme4_Selection;

  Shape5.FillColor := clScheme5_Background;
  Shape5.BorderColor := clScheme5_Selection;

  Shape6.FillColor := clScheme6_Background;
  Shape6.BorderColor := clScheme6_Selection;

  Shape7.FillColor := clScheme7_Background;
  Shape7.BorderColor := clScheme7_Selection;

  Shape8.FillColor := clScheme8_Background;
  Shape8.BorderColor := clScheme8_Selection;

  Shape9.FillColor := clScheme9_Background;
  Shape9.BorderColor := clScheme9_Selection;

  Shape10.FillColor := clScheme10_Background;
  Shape10.BorderColor := clScheme10_Selection;

  Shape11.FillColor := clScheme11_Background;
  Shape11.BorderColor := clScheme11_Selection;

  Shape12.FillColor := clScheme12_Background;
  Shape12.BorderColor := clScheme12_Selection;

  Shape13.FillColor := clScheme13_Background;
  Shape13.BorderColor := clScheme13_Selection;

  Shape14.FillColor := clScheme14_Background;
  Shape14.BorderColor := clScheme14_Selection;

  Shape15.FillColor := clScheme15_Background;
  Shape15.BorderColor := clScheme15_Selection;

  Shape16.FillColor := clScheme16_Background;
  Shape16.BorderColor := clScheme16_Selection;

  Shape17.FillColor := clScheme17_Background;
  Shape17.BorderColor := clScheme17_Selection;

  Shape18.FillColor := clScheme18_Background;
  Shape18.BorderColor := clScheme18_Selection;

  Shape19.FillColor := clScheme19_Background;
  Shape19.BorderColor := clScheme19_Selection;

  Shape20.FillColor := clScheme20_Background;
  Shape20.BorderColor := clScheme20_Selection;

  Shape21.FillColor := clScheme21_Background;
  Shape21.BorderColor := clScheme21_Selection;

  Shape22.FillColor := clScheme22_Background;
  Shape22.BorderColor := clScheme22_Selection;

  Shape23.FillColor := clScheme23_Background;
  Shape23.BorderColor := clScheme23_Selection;

  Shape24.FillColor := clScheme24_Background;
  Shape24.BorderColor := clScheme24_Selection;

  Shape25.FillColor := clScheme25_Background;
  Shape25.BorderColor := clScheme25_Selection;


  BCSampleStyleStrList(ListBox1.Items);
  BCSampleDrawingStrList(ComboBox1.Items);
end;

procedure TForm1.ListDrawItem(Control: TWinControl; Index: integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  DrawItem(Control, Index, ARect, State, StrToTBCSampleDrawing(ComboBox1.Caption));
end;

procedure TForm1.ListBox1SelectionChange(Sender: TObject; User: boolean);
begin
  StyleButtonsSample(Self, StrToTBCSampleStyle(ListBox1.GetSelectedText));
end;

procedure TForm1.Shape23MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TForm1.Shape2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  BCPanel1.Background.Color := TBGRAShape(Sender).FillColor;
  Shape26.FillColor := TBGRAShape(Sender).BorderColor;
end;

end.
