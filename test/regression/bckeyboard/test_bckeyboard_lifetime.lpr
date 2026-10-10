program TestBCKeyboardLifetime;

{$mode objfpc}{$H+}
{$IFDEF LCLgtk2}{$DEFINE PREVENTFOCUS}{$ENDIF}

// Compile with -dPREVENTFOCUS to also exercise the GTK2 focus tracking
// lifecycle on the host widgetset. This does not test GTK2 key delivery.
uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  Interfaces, Forms, Controls, SysUtils, BCKeyboard;

type
  TTestKeyboard = class(TBCKeyboard)
  public
    procedure PendingCall(Data: PtrInt);
    procedure QueuePendingCall;
    {$IFDEF PREVENTFOCUS}
    procedure Remember(AControl: TWinControl);
    procedure RestoreFocus;
    {$ENDIF}
  end;

var
  Calls: Integer;
  Keyboard: TTestKeyboard;
  {$IFDEF PREVENTFOCUS}
  Target: TWinControl;
  {$ENDIF}

procedure TTestKeyboard.PendingCall(Data: PtrInt);
begin
  Inc(Calls);
end;

procedure TTestKeyboard.QueuePendingCall;
begin
  Application.QueueAsyncCall(@PendingCall, 0);
end;

{$IFDEF PREVENTFOCUS}
procedure TTestKeyboard.Remember(AControl: TWinControl);
begin
  ScreenActiveControlChanged(Screen, AControl);
end;

procedure TTestKeyboard.RestoreFocus;
begin
  ReactivateControl(0);
end;
{$ENDIF}

begin
  try
  Application.Initialize;
  Keyboard := TTestKeyboard.Create(nil);
  try
    {$IFDEF PREVENTFOCUS}
    Target := TWinControl.Create(nil);
    try
      Keyboard.Remember(Target);
      if Keyboard.ActiveControl <> Target then
        raise Exception.Create('Active control was not remembered');
      // A detached control cannot receive focus. Restoring it must be harmless.
      Keyboard.RestoreFocus;
    finally
      Target.Free;
    end;
    if Keyboard.ActiveControl <> nil then
      raise Exception.Create('Destroyed active control is still referenced');
    Keyboard.RestoreFocus;
    {$ENDIF}
    // A live keyboard must still be able to receive queued calls.
    Keyboard.QueuePendingCall;
    Application.ProcessMessages;
    if Calls <> 1 then
      raise Exception.Create('Queued callback on live keyboard was lost');
    Calls := 0;
    Keyboard.QueuePendingCall;
  finally
    Keyboard.Free;
  end;
  Application.ProcessMessages;
  if Calls <> 0 then
    raise Exception.Create('Queued callback ran after keyboard destruction');
  {$IFDEF PREVENTFOCUS}
  WriteLn('PASS: queued callbacks, destroyed target, unavailable focus');
  {$ELSE}
  WriteLn('PASS: queued callbacks');
  {$ENDIF}
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ', E.Message);
      Halt(1);
    end;
  end;
end.
