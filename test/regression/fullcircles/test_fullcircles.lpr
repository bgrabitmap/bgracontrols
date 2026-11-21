program test_fullcircles;
{$mode objfpc}{$H+}
uses Interfaces, Forms, Controls, Graphics, ExtCtrls, Classes, SysUtils,
  LCLIntf, SuperLED, BCRadialProgressBar, BCFluentProgressRing;
type
  TTestLED = class(TSuperLED)
    procedure RenderTo(Target: TBitmap);
  end;
  TTestRadial = class(TBCRadialProgressBar)
    procedure RenderTo(Target: TBitmap);
  end;
  TTestRing = class(TBCFluentProgressRing)
    procedure RenderTo(Target: TBitmap);
  end;
  TTestForm = class(TForm)
    Timer: TTimer;
    LED: TTestLED; Radial: TTestRadial; Ring: TTestRing;
    constructor Create(AOwner: TComponent); override;
    procedure RunTests(Sender: TObject);
  end;
var MainForm: TTestForm; Log: TStringList; Failures, Checks: Integer;
procedure TTestLED.RenderTo(Target: TBitmap);
begin
  Canvas.Handle := Target.Canvas.Handle;
  try Paint; finally Canvas.Handle := 0; end;
end;
procedure TTestRadial.RenderTo(Target: TBitmap);
begin
  RenderControl;
  Canvas.Handle := Target.Canvas.Handle;
  try Paint; finally Canvas.Handle := 0; end;
end;
procedure TTestRing.RenderTo(Target: TBitmap);
begin
  DiscardBitmap;
  Canvas.Handle := Target.Canvas.Handle;
  try Paint; finally Canvas.Handle := 0; end;
end;
procedure Check(OK: Boolean; const Msg: string);
begin
  Inc(Checks);
  if OK then Log.Add('PASS '+Msg)
  else begin Inc(Failures); Log.Add('FAIL '+Msg); end;
end;
constructor TTestForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  Caption := 'BGRAControls full-circle regression'; Color := clWhite;
  SetBounds(100,100,400,220);
  LED := TTestLED.Create(Self); LED.Parent := Self; LED.SetBounds(10,10,128,128);
  LED.BorderColor := clBlack; LED.BorderThickness := 3;
  LED.ActiveColor := clLime; LED.InactiveColor := clGray;
  Radial := TTestRadial.Create(Self); Radial.Parent := Self;
  Radial.SetBounds(140,10,128,128); Radial.DrawText := False;
  Radial.Color := RGBToColor(245,245,220);
  Radial.LineColor := clBlue; Radial.LineBkgColor := clSilver; Radial.LineWidth := 8;
  Ring := TTestRing.Create(Self); Ring.Parent := Self; Ring.SetBounds(270,10,128,128);
  Ring.LineColor := clBlue; Ring.LineBkgColor := clSilver; Ring.LineWidth := 10;
  Ring.Indeterminate := False;
  Timer := TTimer.Create(Self); Timer.Interval := 300; Timer.OnTimer := @RunTests;
end;
procedure TTestForm.RunTests(Sender: TObject);
var B: TBitmap; I, X, Y, R, G, Blue, Gray, Painted: Integer;
  C: TColor; CaseName: string;
begin
  Timer.Enabled := False; SetForegroundWindow(Handle); Application.ProcessMessages;
  B := TBitmap.Create;
  try
    B.SetSize(128,128);
    for I := 0 to 9 do
    begin
      B.Canvas.Brush.Color := clWhite; B.Canvas.FillRect(0,0,128,128);
      if I < 3 then
      begin
        CaseName := 'LED '+IntToStr(I);
        LED.Active := I <> 0;
        if I = 2 then LED.Style := slsShaded else LED.Style := slsFlat;
        LED.RenderTo(B);
      end else if I < 6 then
      begin
        CaseName := 'radial '+IntToStr((I-3)*50)+'%';
        Radial.Value := (I-3)*50; Radial.RenderTo(B);
      end else
      begin
        if I = 9 then begin CaseName := 'ring disabled 100%'; Ring.Enabled := False; Ring.Value := 100; end
        else begin CaseName := 'ring '+IntToStr((I-6)*50)+'%'; Ring.Enabled := True; Ring.Value := (I-6)*50; end;
        Ring.RenderTo(B);
      end;
      Blue := 0; Gray := 0; Painted := 0;
      for Y := 0 to 127 do for X := 0 to 127 do
      begin
        C := ColorToRGB(B.Canvas.Pixels[X,Y]); R := C and 255; G := (C shr 8) and 255;
        if C <> ColorToRGB(clWhite) then Inc(Painted);
        if ((C shr 16) and 255 > 120) and (R < 100) and (G < 120) then Inc(Blue);
        if (Abs(R-G) < 4) and (Abs(R-((C shr 16) and 255)) < 4)
          and (R > 40) and (R < 240) then Inc(Gray);
      end;
      Log.Add(Format('%s: painted=%d blue=%d gray=%d',[CaseName,Painted,Blue,Gray]));
      Check(ColorToRGB(B.Canvas.Pixels[0,0]) = ColorToRGB(clWhite),CaseName+' outside clear');
      if I < 3 then
      begin
        Check(Painted > 5000,CaseName+' filled area');
        Check(ColorToRGB(B.Canvas.Pixels[64,64]) <> ColorToRGB(clWhite),CaseName+' filled center');
      end else
      begin
        if I < 6 then Check(ColorToRGB(B.Canvas.Pixels[64,64]) <> ColorToRGB(clWhite),CaseName+' full background')
        else Check(ColorToRGB(B.Canvas.Pixels[64,64]) = ColorToRGB(clWhite),CaseName+' center stays clear');
        if (I = 3) or (I = 6) or (I = 9) then Check(Blue = 0,CaseName+' no blue progress')
        else Check(Blue > 500,CaseName+' visible blue progress');
        if (I = 5) or (I = 8) then Check(Gray < 100,CaseName+' no gray gap at maximum')
        else Check(Gray > 500,CaseName+' visible gray ring');
      end;
      B.SaveToFile(ExtractFilePath(Application.ExeName)+'case-'+IntToStr(I)+'.bmp');
    end;
  except on E: Exception do begin Inc(Failures); Log.Add('EXCEPTION '+E.Message); end; end;
  B.Free;
  Log.Add(Format('Checks: %d; Failures: %d',[Checks,Failures]));
  Log.SaveToFile(ExtractFilePath(Application.ExeName)+'fullcircles-result.txt');
  Application.Terminate;
end;
begin
  Log := TStringList.Create;
  Application.Initialize; Application.CreateForm(TTestForm,MainForm); Application.Run;
  Log.Free;
  if Failures <> 0 then Halt(1);
end.
