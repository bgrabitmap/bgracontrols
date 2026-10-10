program SpeedButtonImagesDemo;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  Interfaces, Forms, Controls, Graphics, Buttons, ImgList, StdCtrls, Classes,
  BGRASpeedButton, BGRAResizeSpeedButton, ColorSpeedButton, BGRAImageList;
type
  TButtonClass = class of TSpeedButton;
const
  ButtonClasses: array[0..3] of TButtonClass =
    (TSpeedButton, TBGRASpeedButton, TColorSpeedButton, TBGRAResizeSpeedButton);
var
  Form: TForm;
  StockImages: TImageList;
  BGRAImages: TBGRAImageList;
  Icon: TBitmap;
  Button: TSpeedButton;
  LabelControl: TLabel;
  Row, Col, i: Integer;
  Colors: array[0..2] of TColor = (clRed, clLime, clBlue);
procedure AddLabel(const Text: string; Left, Top: Integer);
begin
  LabelControl := TLabel.Create(Form);
  LabelControl.Parent := Form;
  LabelControl.Caption := Text;
  LabelControl.SetBounds(Left, Top, 180, 24);
end;
begin
  RequireDerivedFormResource := False;
  Application.Initialize;
  Application.CreateForm(TForm, Form);
  Form.Caption := 'SpeedButton image lists (#161)';
  Form.SetBounds(80, 80, 820, 330);
  StockImages := TImageList.Create(Form);
  BGRAImages := TBGRAImageList.Create(Form);
  StockImages.Width := 16; StockImages.Height := 16;
  BGRAImages.Width := 16; BGRAImages.Height := 16;
  Icon := TBitmap.Create;
  try
    Icon.SetSize(16, 16);
    for i := 0 to 2 do
    begin
      Icon.Canvas.Brush.Color := clFuchsia;
      Icon.Canvas.FillRect(0, 0, 16, 16);
      Icon.Canvas.Brush.Color := Colors[i];
      Icon.Canvas.Pen.Color := Colors[i];
      Icon.Canvas.Ellipse(2, 2, 14, 14);
      StockImages.AddMasked(Icon, clFuchsia);
      BGRAImages.AddMasked(Icon, clFuchsia);
    end;
    // Use an opaque bitmap for the independent, legacy Glyph example.
    Icon.Canvas.Brush.Color := clBtnFace;
    Icon.Canvas.FillRect(0, 0, 16, 16);
    Icon.Canvas.Brush.Color := clBlue;
    Icon.Canvas.Pen.Color := clBlue;
    Icon.Canvas.Ellipse(2, 2, 14, 14);
    AddLabel('Normal: red; pressed: green; disabled: blue. Last row uses Glyph.', 12, 12);
    for Col := 0 to 3 do
      AddLabel(ButtonClasses[Col].ClassName, 12 + Col * 200, 44);
    for Row := 0 to 2 do
      for Col := 0 to 3 do
      begin
        Button := ButtonClasses[Col].Create(Form);
        Button.Parent := Form;
        Button.SetBounds(12 + Col * 200, 76 + Row * 72, 180, 60);
        if Row < 2 then
        begin
          if Row = 0 then begin Button.Images := StockImages; Button.Caption := 'TImageList'; end
          else begin Button.Images := BGRAImages; Button.Caption := 'TBGRAImageList'; end;
          Button.ImageIndex := 0;
          Button.PressedImageIndex := 1;
          Button.DisabledImageIndex := 2;
          if Row = 1 then Button.Enabled := False;
        end
        else
        begin
          Button.Caption := 'Glyph';
          Button.Glyph.Assign(Icon);
          Button.Glyph.Transparent := False;
        end;
      end;
  finally
    Icon.Free;
  end;
  Application.Run;
end.
