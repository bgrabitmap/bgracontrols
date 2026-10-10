program TestSpeedButtonImages;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  Interfaces, Forms, Controls, Graphics, Buttons, ImgList, Classes, SysUtils,
  Types, BGRASpeedButton, BGRAResizeSpeedButton, ColorSpeedButton, BGRAImageList;
type
  TReferenceButton = class(TSpeedButton)
    procedure Render(Bitmap: TBitmap; State: TButtonState);
  end;
  TTestButton = class(TBGRASpeedButton)
    procedure Render(Bitmap: TBitmap; State: TButtonState);
  end;
  TTestColorButton = class(TColorSpeedButton)
    procedure Render(Bitmap: TBitmap; State: TButtonState);
  end;
  TTestResizeButton = class(TBGRAResizeSpeedButton)
    procedure Render(Bitmap: TBitmap; State: TButtonState);
  end;
procedure TReferenceButton.Render(Bitmap: TBitmap; State: TButtonState);
begin DrawGlyph(Bitmap.Canvas, Rect(4, 4, 60, 28), Point(8, 2), State, True, 0); end;
procedure TTestButton.Render(Bitmap: TBitmap; State: TButtonState);
begin DrawGlyph(Bitmap.Canvas, Rect(4, 4, 60, 28), Point(8, 2), State, True, 0); end;
procedure TTestColorButton.Render(Bitmap: TBitmap; State: TButtonState);
begin DrawGlyph(Bitmap.Canvas, Rect(4, 4, 60, 28), Point(8, 2), State, True, 0); end;
procedure TTestResizeButton.Render(Bitmap: TBitmap; State: TButtonState);
begin DrawGlyph(Bitmap.Canvas, Rect(4, 4, 60, 28), Point(8, 2), State, True, 0); end;
procedure Clear(Bitmap: TBitmap);
begin
  Bitmap.Canvas.Brush.Color := clWhite;
  Bitmap.Canvas.FillRect(0, 0, Bitmap.Width, Bitmap.Height);
end;
procedure Compare(Expected, Actual: TBitmap; const Description: string);
var x, y: Integer;
begin
  for y := 0 to Expected.Height - 1 do
    for x := 0 to Expected.Width - 1 do
      if Expected.Canvas.Pixels[x, y] <> Actual.Canvas.Pixels[x, y] then
        raise Exception.CreateFmt('%s differs at %d,%d', [Description, x, y]);
end;
procedure Check(UseBGRA: Boolean);
var
  Form: TForm;
  Images: TCustomImageList;
  Icon, Expected, Actual: TBitmap;
  Reference: TReferenceButton;
  Button: TTestButton;
  ColorButton: TTestColorButton;
  ResizeButton: TTestResizeButton;
  State: TButtonState;
  i: Integer;
  Colors: array[0..2] of TColor = (clRed, clLime, clBlue);
  Description: string;
begin
  Form := TForm.CreateNew(nil);
  Icon := TBitmap.Create;
  Expected := TBitmap.Create;
  Actual := TBitmap.Create;
  try
    if UseBGRA then Images := TBGRAImageList.Create(Form)
    else Images := TImageList.Create(Form);
    Images.Width := 16;
    Images.Height := 16;
    Icon.SetSize(16, 16);
    for i := 0 to 2 do
    begin
      Icon.Canvas.Brush.Color := Colors[i];
      Icon.Canvas.FillRect(0, 0, 16, 16);
      Images.Add(Icon, nil);
    end;
    Reference := TReferenceButton.Create(Form);
    Button := TTestButton.Create(Form);
    ColorButton := TTestColorButton.Create(Form);
    ResizeButton := TTestResizeButton.Create(Form);
    Reference.Parent := Form;
    Button.Parent := Form;
    ColorButton.Parent := Form;
    ResizeButton.Parent := Form;
    Reference.Images := Images; Reference.ImageIndex := 0;
    Button.Images := Images; Button.ImageIndex := 0;
    ColorButton.Images := Images; ColorButton.ImageIndex := 0;
    ResizeButton.Images := Images; ResizeButton.ImageIndex := 0;
    Reference.PressedImageIndex := 1; Button.PressedImageIndex := 1;
    ColorButton.PressedImageIndex := 1; ResizeButton.PressedImageIndex := 1;
    Reference.DisabledImageIndex := 2; Button.DisabledImageIndex := 2;
    ColorButton.DisabledImageIndex := 2; ResizeButton.DisabledImageIndex := 2;
    Expected.SetSize(64, 32);
    Actual.SetSize(64, 32);
    for State := Low(TButtonState) to High(TButtonState) do
    begin
      Clear(Expected);
      Reference.Render(Expected, State);
      if Expected.Canvas.Pixels[12, 6] = clWhite then
        raise Exception.Create('Reference did not draw its image');
      Description := Images.ClassName + ' state=' + IntToStr(Ord(State));
      Clear(Actual); Button.Render(Actual, State);
      Compare(Expected, Actual, 'TBGRASpeedButton ' + Description);
      Clear(Actual); ColorButton.Render(Actual, State);
      Compare(Expected, Actual, 'TColorSpeedButton ' + Description);
      Clear(Actual); ResizeButton.Render(Actual, State);
      Compare(Expected, Actual, 'TBGRAResizeSpeedButton ' + Description);
    end;
    WriteLn('PASS: ', Images.ClassName, ' / three button classes / all states');
  finally
    Actual.Free;
    Expected.Free;
    Icon.Free;
    Form.Free;
  end;
end;
begin
  Application.Initialize;
  try
    Check(False);
    Check(True);
    WriteLn('PASS');
  except
    on E: Exception do begin WriteLn('FAIL: ', E.Message); Halt(1); end;
  end;
end.
