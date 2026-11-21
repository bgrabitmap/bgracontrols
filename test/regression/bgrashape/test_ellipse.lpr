program test_ellipse;
{$mode objfpc}{$H+}
uses Interfaces, Forms, Controls, Graphics, ExtCtrls, Classes, SysUtils,
  LCLIntf, BGRAShape;
type
  TTestShape = class(TBGRAShape)
    procedure RenderTo(Target: TBitmap);
  end;
  TTestForm = class(TForm)
    Timer: TTimer;
    Shape: TTestShape;
    procedure RunTests(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
  end;
var MainForm: TTestForm; Log: TStringList; Failures: Integer;
procedure TTestShape.RenderTo(Target: TBitmap);
begin
  Canvas.Handle := Target.Canvas.Handle;
  try Paint; finally Canvas.Handle := 0; end;
end;
procedure Check(OK: Boolean; const Msg: string);
begin
  if OK then Log.Add('PASS '+Msg)
  else begin Inc(Failures); Log.Add('FAIL '+Msg); end;
end;
constructor TTestForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  Caption := 'BGRAControls #297 ellipse regression';
  SetBounds(100,100,340,220);
  Color := clWhite;
  Shape := TTestShape.Create(Self);
  Shape.Parent := Self;
  Shape.SetBounds(20,20,160,100);
  Timer := TTimer.Create(Self);
  Timer.Interval := 300;
  Timer.OnTimer := @RunTests;
end;
procedure TTestForm.RunTests(Sender: TObject);
var Target: TBitmap; CaseIndex, X, Y, Painted: Integer; CaseName: string;
begin
  Timer.Enabled := False;
  SetForegroundWindow(Handle);
  Application.ProcessMessages;
  Target := TBitmap.Create;
  try
    for CaseIndex := 0 to 6 do
    begin
      Shape.ShapeType := stEllipse;
      Shape.SetBounds(20,20,160,100);
      Shape.BorderWidth := 4;
      Shape.BorderColor := clBlack;
      Shape.FillColor := clRed;
      Shape.FillOpacity := 255;
      Shape.UseFillGradient := False;
      Shape.UseBorderGradient := False;
      Shape.UseRatioXY := False;
      case CaseIndex of
        0: begin CaseName := 'polygon control'; Shape.ShapeType := stRegularPolygon; end;
        1: CaseName := 'solid ellipse';
        2: begin CaseName := 'fill gradient'; Shape.UseFillGradient := True;
             Shape.FillGradient.StartColor := clRed; Shape.FillGradient.EndColor := clBlue; end;
        3: begin CaseName := 'border gradient'; Shape.UseBorderGradient := True;
             Shape.BorderGradient.StartColor := clBlack; Shape.BorderGradient.EndColor := clBlue; end;
        4: begin CaseName := 'fixed ratio'; Shape.UseRatioXY := True; Shape.RatioXY := 1; end;
        5: begin CaseName := 'no border'; Shape.BorderWidth := 0; end;
        6: begin CaseName := 'border only'; Shape.FillOpacity := 0; end;
      end;
      Target.SetSize(Shape.Width, Shape.Height);
      Target.Canvas.Brush.Color := clWhite;
      Target.Canvas.FillRect(0,0,Target.Width,Target.Height);
      Shape.RenderTo(Target);
      Painted := 0;
      for Y := 0 to Target.Height-1 do
        for X := 0 to Target.Width-1 do
          if ColorToRGB(Target.Canvas.Pixels[X,Y]) <> ColorToRGB(clWhite) then Inc(Painted);
      if CaseIndex = 6 then
      begin
        Check(Painted > 100, CaseName+' visible outline');
        Check(ColorToRGB(Target.Canvas.Pixels[80,50]) = ColorToRGB(clWhite), CaseName+' transparent center');
      end else
      begin
        Check(Painted > 1000, CaseName+' visible area');
        Check(ColorToRGB(Target.Canvas.Pixels[80,50]) <> ColorToRGB(clWhite), CaseName+' filled center');
      end;
      Check(ColorToRGB(Target.Canvas.Pixels[0,0]) = ColorToRGB(clWhite), CaseName+' outside stays clear');
      if CaseIndex = 4 then
        Check(ColorToRGB(Target.Canvas.Pixels[10,50]) = ColorToRGB(clWhite), CaseName+' horizontal margin');
      Target.SaveToFile(ExtractFilePath(Application.ExeName)+'case-'+IntToStr(CaseIndex)+'.bmp');
    end;
  except on E: Exception do begin Inc(Failures); Log.Add('EXCEPTION '+E.Message); end; end;
  Target.Free;
  Log.Add('Failures: '+IntToStr(Failures));
  Log.SaveToFile(ExtractFilePath(Application.ExeName)+'ellipse-result.txt');
  Application.Terminate;
end;
begin
  Log := TStringList.Create;
  Application.Initialize;
  Application.CreateForm(TTestForm, MainForm);
  Application.Run;
  Log.Free;
  if Failures <> 0 then Halt(1);
end.
