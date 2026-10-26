program TestSpeedButtonDPI;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  Interfaces, Forms, Controls, Graphics, Buttons, Classes, SysUtils, Types,
  BGRASpeedButton, ColorSpeedButton;
type
  TReferenceButton = class(TSpeedButton)
  public
    function GlyphSize: TSize;
    procedure Render(Bitmap: TBitmap; const Offset: TPoint);
  end;
  TTestButton = class(TBGRASpeedButton)
  public
    procedure Render(Bitmap: TBitmap; const Offset: TPoint);
  end;
  TTestColorButton = class(TColorSpeedButton)
  public
    procedure Render(Bitmap: TBitmap; const Offset: TPoint);
  end;
function TReferenceButton.GlyphSize: TSize;
begin
  Result := GetGlyphSize(True, Rect(0, 0, 80, 80));
end;
procedure TReferenceButton.Render(Bitmap: TBitmap; const Offset: TPoint);
begin
  DrawGlyph(Bitmap.Canvas, Rect(0, 0, Bitmap.Width, Bitmap.Height), Offset,
    bsUp, True, 0);
end;
procedure TTestButton.Render(Bitmap: TBitmap; const Offset: TPoint);
begin
  DrawGlyph(Bitmap.Canvas, Rect(0, 0, Bitmap.Width, Bitmap.Height), Offset,
    bsUp, True, 0);
end;
procedure TTestColorButton.Render(Bitmap: TBitmap; const Offset: TPoint);
begin
  DrawGlyph(Bitmap.Canvas, Rect(0, 0, Bitmap.Width, Bitmap.Height), Offset,
    bsUp, True, 0);
end;
procedure Clear(Bitmap: TBitmap);
begin
  Bitmap.Canvas.Brush.Color := clWhite;
  Bitmap.Canvas.FillRect(0, 0, Bitmap.Width, Bitmap.Height);
end;
function VisibleBounds(Bitmap: TBitmap): TRect;
var
  x, y: Integer;
  Found: Boolean;
begin
  Result := Rect(MaxInt, MaxInt, -1, -1);
  Found := False;
  for y := 0 to Bitmap.Height - 1 do
    for x := 0 to Bitmap.Width - 1 do
      if Bitmap.Canvas.Pixels[x, y] <> clWhite then
      begin
        if x < Result.Left then Result.Left := x;
        if y < Result.Top then Result.Top := y;
        if x > Result.Right then Result.Right := x;
        if y > Result.Bottom then Result.Bottom := y;
        Found := True;
      end;
  if not Found then
    raise Exception.Create('The glyph was not drawn');
end;
procedure CheckBounds(Expected, Actual: TRect; const Description: string);
begin
  if (Abs((Expected.Right - Expected.Left) - (Actual.Right - Actual.Left)) > 2) or
     (Abs((Expected.Bottom - Expected.Top) - (Actual.Bottom - Actual.Top)) > 2) or
     (Abs((Expected.Left + Expected.Right) - (Actual.Left + Actual.Right)) > 2) or
     (Abs((Expected.Top + Expected.Bottom) - (Actual.Top + Actual.Bottom)) > 2) then
    raise Exception.CreateFmt('%s: expected bounds %d,%d..%d,%d, got %d,%d..%d,%d',
      [Description, Expected.Left, Expected.Top, Expected.Right, Expected.Bottom,
       Actual.Left, Actual.Top, Actual.Right, Actual.Bottom]);
end;
procedure Check(APPI: Integer);
const
  CanvasSize = 80;
var
  Form: TForm;
  Reference: TReferenceButton;
  Button: TTestButton;
  ColorButton: TTestColorButton;
  Icon, Expected, Actual: TBitmap;
  GlyphSize: TSize;
  Offset: TPoint;
  ExpectedBounds, ActualBounds: TRect;
  Description: string;
begin
  Form := TForm.CreateNew(nil);
  Icon := TBitmap.Create;
  Expected := TBitmap.Create;
  Actual := TBitmap.Create;
  try
    Icon.SetSize(20, 20);
    Icon.Canvas.Brush.Color := clWhite;
    Icon.Canvas.FillRect(0, 0, 20, 20);
    Icon.Canvas.Brush.Color := clBlack;
    Icon.Canvas.FillRect(5, 5, 15, 15);
    Reference := TReferenceButton.Create(Form);
    Button := TTestButton.Create(Form);
    ColorButton := TTestColorButton.Create(Form);
    Reference.Parent := Form;
    Button.Parent := Form;
    ColorButton.Parent := Form;
    Reference.Glyph.Assign(Icon);
    Button.Glyph.Assign(Icon);
    ColorButton.Glyph.Assign(Icon);
    Reference.Font.PixelsPerInch := APPI;
    Button.Font.PixelsPerInch := APPI;
    ColorButton.Font.PixelsPerInch := APPI;
    GlyphSize := Reference.GlyphSize;
    Offset := Point((CanvasSize - GlyphSize.cx) div 2,
      (CanvasSize - GlyphSize.cy) div 2);
    Expected.SetSize(CanvasSize, CanvasSize);
    Actual.SetSize(CanvasSize, CanvasSize);
    Clear(Expected);
    Reference.Render(Expected, Offset);
    ExpectedBounds := VisibleBounds(Expected);
    Description := Format('%d PPI, glyph %dx%d', [APPI, GlyphSize.cx, GlyphSize.cy]);
    Clear(Actual);
    Button.Render(Actual, Offset);
    ActualBounds := VisibleBounds(Actual);
    CheckBounds(ExpectedBounds, ActualBounds, 'TBGRASpeedButton ' + Description);
    Clear(Actual);
    ColorButton.Render(Actual, Offset);
    ActualBounds := VisibleBounds(Actual);
    CheckBounds(ExpectedBounds, ActualBounds, 'TColorSpeedButton ' + Description);
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
    Check(96);
    Check(120);
    Check(144);
    Check(168);
    Check(192);
    Check(288);
    WriteLn('PASS');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ', E.Message);
      Halt(1);
    end;
  end;
end.
