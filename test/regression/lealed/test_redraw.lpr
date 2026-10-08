program TestLeaLEDRedraw;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  Interfaces, Forms, Controls, Graphics, Classes, SysUtils,
  BCLeaLED, BCLeaTypes, BGRABitmap, BGRABitmapTypes;
type
  TTestLED = class(TBCLeaLED)
  public
    procedure Render;
  end;
procedure TTestLED.Render;
begin
  Redraw;
end;
procedure Require(Value: Boolean; const Message: string);
begin
  if not Value then raise Exception.Create(Message);
end;
var
  Form: TForm;
  LED: TTestLED;
  Bitmap: TBitmap;
  Image: TBGRABitmap;
  Style: TZStyle;
  OnState: Boolean;
  i, x, y: Integer;
  BeforeBytes, AfterBytes: PtrUInt;
  Hash: LongWord;
  Pixel: TBGRAPixel;
  OutputPath, Filename: string;
begin
  try
    Application.Initialize;
    Form := TForm.CreateNew(nil);
    try
      Form.SetBounds(40, 40, 120, 100);
      Form.Caption := 'LED redraw regression';
      LED := TTestLED.Create(Form);
      LED.Parent := Form;
      LED.SetBounds(0, 0, 64, 64);
      LED.BackgroundColor := clWhite;
      Form.Show;
      Application.ProcessMessages;
      OutputPath := ParamStr(1);
      if OutputPath <> '' then ForceDirectories(OutputPath);
      Bitmap := TBitmap.Create;
      try
        Bitmap.SetSize(64, 64);
        for Style := Low(TZStyle) to High(TZStyle) do
          for OnState := False to True do
          begin
            LED.Style := Style;
            LED.Value := OnState;
            LED.Render;
            Bitmap.Canvas.CopyRect(Rect(0, 0, 64, 64), LED.Canvas, Rect(0, 0, 64, 64));
            Image := TBGRABitmap.Create(Bitmap);
            try
              Hash := 2166136261;
              for y := 0 to Image.Height - 1 do
                for x := 0 to Image.Width - 1 do
                begin
                  Pixel := Image.GetPixel(x, y);
                  Hash := LongWord((QWord(Hash xor Pixel.red) * 16777619) and $FFFFFFFF);
                  Hash := LongWord((QWord(Hash xor Pixel.green) * 16777619) and $FFFFFFFF);
                  Hash := LongWord((QWord(Hash xor Pixel.blue) * 16777619) and $FFFFFFFF);
                  Hash := LongWord((QWord(Hash xor Pixel.alpha) * 16777619) and $FFFFFFFF);
                end;
              WriteLn('pixels ', Ord(Style), ' ', Ord(OnState), ': ', IntToHex(Hash, 8));
              if OutputPath <> '' then
              begin
                Filename := IncludeTrailingPathDelimiter(OutputPath) +
                  'style-' + IntToStr(Ord(Style)) + '-on-' + IntToStr(Ord(OnState)) + '.png';
                Image.SaveToFile(Filename);
              end;
            finally Image.Free; end;
          end;
      finally Bitmap.Free; end;
      LED.Style := zsRaised;
      LED.Value := True;
      for i := 1 to 8 do LED.Render;
      BeforeBytes := GetFPCHeapStatus.CurrHeapUsed;
      for i := 1 to 64 do LED.Render;
      AfterBytes := GetFPCHeapStatus.CurrHeapUsed;
      WriteLn('heap growth over 64 lit redraws: ', Int64(AfterBytes) - Int64(BeforeBytes), ' bytes');
      Require(AfterBytes <= BeforeBytes + 4096, 'Lit redraws retain bitmap allocations');
      for i := 1 to 12 do
      begin
        LED.Enabled := Odd(i);
        LED.Value := Odd(i);
        LED.Style := TZStyle(i mod 3);
        LED.Render;
      end;
      LED.SetBounds(0, 0, 1, 1);
      LED.Render;
    finally Form.Free; end;
    WriteLn('PASS: redraw allocation stability, style/value/enabled changes, tiny control');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
