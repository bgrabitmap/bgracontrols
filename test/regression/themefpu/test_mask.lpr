program test_mask;
{$mode objfpc}{$H+}
uses Interfaces, Forms, Controls, Graphics, ExtCtrls, Classes, SysUtils, Math,
  LCLIntf, BGRATheme, BGRAThemeButton, BGRASVGImageList;
const AllExceptions: TFPUExceptionMask = [exInvalidOp,exDenormalized,exZeroDivide,
  exOverflow,exUnderflow,exPrecision];
type
  EProbePaint = class(Exception);
  TProbeTheme = class(TBGRATheme)
  public
    TestActive, RaisePaint, InvalidMath: Boolean;
    procedure DrawButton(Caption: string; State: TBGRAThemeButtonState;
      Focused: Boolean; ARect: TRect; ASurface: TBGRAThemeSurface;
      AImageIndex: Integer = -1; AImageList: TBGRASVGImageList = nil); override;
  end;
  TProbeButton = class(TBGRAThemeButton)
    procedure RenderTo(Target: TBitmap);
  end;
  TTestForm = class(TForm)
  public
    Timer: TTimer;
    Probe: TProbeTheme; Button: TProbeButton;
    constructor Create(AOwner: TComponent); override;
    procedure RunTests(Sender: TObject);
  end;
var MainForm: TTestForm; Log: TStringList; Checks, Failures: Integer;
procedure Check(OK: Boolean; const Msg: string);
begin
  Inc(Checks);
  if OK then Log.Add('PASS '+Msg)
  else begin Inc(Failures); Log.Add('FAIL '+Msg); end;
end;
procedure TProbeTheme.DrawButton(Caption: string; State: TBGRAThemeButtonState;
  Focused: Boolean; ARect: TRect; ASurface: TBGRAThemeSurface;
  AImageIndex: Integer; AImageList: TBGRASVGImageList);
var V: Double;
begin
  if not TestActive then Exit;
  Check(GetExceptionMask = AllExceptions,'paint masks floating exceptions');
  if InvalidMath then
  begin
    V := -1; V := Sqrt(V);
    Check(IsNan(V),'benign invalid math remains masked');
  end;
  if RaisePaint then raise EProbePaint.Create('paint probe');
end;
procedure TProbeButton.RenderTo(Target: TBitmap);
begin
  Canvas.Handle := Target.Canvas.Handle;
  try Paint; finally Canvas.Handle := 0; end;
end;
constructor TTestForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  Caption := 'Theme FPU regression'; SetBounds(100,100,320,140);
  Probe := TProbeTheme.Create(Self);
  Button := TProbeButton.Create(Self); Button.Parent := Self;
  Button.SetBounds(10,10,200,50); Button.Theme := Probe;
  Timer := TTimer.Create(Self); Timer.Interval := 300; Timer.OnTimer := @RunTests;
end;
procedure TTestForm.RunTests(Sender: TObject);
var B: TBitmap; I: Integer; OriginalMask, CallerMask, ObservedMask: TFPUExceptionMask;
  DidRaise: Boolean;
begin
  Timer.Enabled := False; SetForegroundWindow(Handle); Application.ProcessMessages;
  OriginalMask := GetExceptionMask; B := TBitmap.Create;
  try
    B.SetSize(200,50); Probe.TestActive := True;
    for I := 0 to 3 do
    begin
      Probe.RaisePaint := Odd(I); Probe.InvalidMath := I >= 2;
      CallerMask := AllExceptions - [exInvalidOp];
      ClearExceptions(False); SetExceptionMask(CallerMask);
      DidRaise := False;
      try
        try Button.RenderTo(B); except on EProbePaint do DidRaise := True; end;
        ObservedMask := GetExceptionMask;
      finally
        ClearExceptions(False); SetExceptionMask(AllExceptions);
      end;
      Check(DidRaise = Probe.RaisePaint,'ordinary exception propagates as expected '+IntToStr(I));
      Check(ObservedMask = CallerMask,'caller mask restored '+IntToStr(I));
    end;
  except on E: Exception do begin Inc(Failures); Log.Add('EXCEPTION '+E.Message); end; end;
  Probe.TestActive := False;
  ClearExceptions(False); SetExceptionMask(OriginalMask);
  B.Free;
  Log.Add(Format('Checks: %d; Failures: %d',[Checks,Failures]));
  Log.SaveToFile(ExtractFilePath(Application.ExeName)+'mask-result.txt');
  Application.Terminate;
end;
begin
  Log := TStringList.Create;
  Application.Initialize; Application.CreateForm(TTestForm,MainForm); Application.Run;
  Log.Free;
  if Failures <> 0 then Halt(1);
end.
