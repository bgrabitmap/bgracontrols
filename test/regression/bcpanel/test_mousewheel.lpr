program TestBCPanelMouseWheel;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  Interfaces, Forms, Controls, ExtCtrls, Classes, SysUtils, Types, BCPanel;
type
  TControlAccess = class(TControl);
  TWheelEvents = class
    ChildCalls, ParentCalls: Integer;
    ConsumeChild, ConsumeParent: Boolean;
    procedure ChildWheel(Sender: TObject; Shift: TShiftState; Delta: Integer;
      Pos: TPoint; var Handled: Boolean);
    procedure ParentWheel(Sender: TObject; Shift: TShiftState; Delta: Integer;
      Pos: TPoint; var Handled: Boolean);
  end;
procedure TWheelEvents.ChildWheel(Sender: TObject; Shift: TShiftState;
  Delta: Integer; Pos: TPoint; var Handled: Boolean);
begin
  Inc(ChildCalls);
  Handled := ConsumeChild;
end;
procedure TWheelEvents.ParentWheel(Sender: TObject; Shift: TShiftState;
  Delta: Integer; Pos: TPoint; var Handled: Boolean);
begin
  Inc(ParentCalls);
  Handled := ConsumeParent;
end;
{$IF DEFINED(LINUX) AND DEFINED(LCLqt5)}
function XOpenDisplay(Name: PChar): Pointer; cdecl; external 'libX11.so.6';
function XCloseDisplay(Display: Pointer): Integer; cdecl; external 'libX11.so.6';
function XFlush(Display: Pointer): Integer; cdecl; external 'libX11.so.6';
function XDefaultRootWindow(Display: Pointer): NativeUInt; cdecl; external 'libX11.so.6';
function XWarpPointer(Display: Pointer; Source, Destination: NativeUInt;
  SourceX, SourceY: LongInt; SourceWidth, SourceHeight: Cardinal;
  DestinationX, DestinationY: LongInt): Integer; cdecl; external 'libX11.so.6';
function XTestFakeButtonEvent(Display: Pointer; Button: Cardinal;
  Press: LongInt; Delay: NativeUInt): LongInt; cdecl; external 'libXtst.so.6';
procedure ProcessEvents;
var i: Integer;
begin
  for i := 1 to 20 do begin Sleep(20); Application.ProcessMessages; end;
end;
procedure SendWheel(Control: TWinControl);
var Display: Pointer; Position: TPoint;
begin
  Position := Control.ClientToScreen(Point(20, 20));
  Display := XOpenDisplay(nil);
  if Display = nil then raise Exception.Create('An X11 display is required');
  try
    XWarpPointer(Display, 0, XDefaultRootWindow(Display), 0, 0, 0, 0,
      Position.X, Position.Y);
    if XTestFakeButtonEvent(Display, 5, 1, 0) = 0 then
      raise Exception.Create('XTest wheel injection failed');
    XTestFakeButtonEvent(Display, 5, 0, 0);
    XFlush(Display);
    ProcessEvents;
  finally
    XCloseDisplay(Display);
  end;
end;
procedure Check(Standard, ConsumeChild, ConsumeParent, Nested: Boolean);
var
  Form: TForm;
  Flow: TFlowPanel;
  Panel, Target: TWinControl;
  Events: TWheelEvents;
  i, BeforePos, AfterPos: Integer;
begin
  Form := TForm.CreateNew(nil);
  Events := TWheelEvents.Create;
  try
    Form.SetBounds(50, 50, 679, 518);
    Form.AutoScroll := True;
    Flow := TFlowPanel.Create(Form);
    Flow.Parent := Form;
    Flow.Align := alClient;
    Target := nil;
    for i := 0 to 11 do
    begin
      if Standard then Panel := TPanel.Create(Form)
      else Panel := TBCPanel.Create(Form);
      Panel.Parent := Flow;
      Panel.SetBounds(0, 0, 170, 167);
      Panel.BorderSpacing.Around := 5;
      if i = 0 then Target := Panel;
    end;
    if Nested then
    begin
      Panel := TBCPanel.Create(Form);
      Panel.Parent := Target;
      Panel.SetBounds(5, 5, 140, 140);
      Target := Panel;
    end;
    Events.ConsumeChild := ConsumeChild;
    Events.ConsumeParent := ConsumeParent;
    TControlAccess(Target).OnMouseWheel := @Events.ChildWheel;
    TControlAccess(Flow).OnMouseWheel := @Events.ParentWheel;
    Form.Show;
    ProcessEvents;
    BeforePos := Form.VertScrollBar.Position;
    SendWheel(Target);
    AfterPos := Form.VertScrollBar.Position;
    WriteLn(Target.ClassName, ' nested=', Nested, ' consume child/parent=',
      ConsumeChild, '/', ConsumeParent, ' calls child/parent=',
      Events.ChildCalls, '/', Events.ParentCalls, ' scroll=', BeforePos, ' -> ', AfterPos);
    if Events.ChildCalls <> 1 then raise Exception.Create('Child handler must run once');
    if ConsumeChild and (Events.ParentCalls <> 0) then
      raise Exception.Create('Consumed child event reached parent');
    if ConsumeParent and not ConsumeChild and (Events.ParentCalls <> 1) then
      raise Exception.Create('Consuming parent handler must run once');
    if ConsumeChild or ConsumeParent then
    begin
      if AfterPos <> BeforePos then raise Exception.Create('Consumed wheel scrolled');
    end
    else if AfterPos <= BeforePos then raise Exception.Create('Unhandled wheel did not scroll');
  finally
    Form.Free;
    Events.Free;
  end;
end;
{$ENDIF}
begin
  Application.Initialize;
  try
    {$IF DEFINED(LINUX) AND DEFINED(LCLqt5)}
    Check(True, False, False, False);
    Check(False, False, False, False);
    Check(False, True, False, False);
    Check(False, False, True, False);
    Check(False, False, False, True);
    Check(False, False, True, True);
    WriteLn('PASS');
    {$ELSE}
    WriteLn('SKIP: this input regression requires Linux/X11 and Qt5');
    {$ENDIF}
  except
    on E: Exception do begin WriteLn('FAIL: ', E.Message); Halt(1); end;
  end;
end.
