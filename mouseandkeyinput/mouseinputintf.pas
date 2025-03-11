{ MouseInputIntf

  Copyright (C) 2008 Tom Gregorovic

  This source is free software; you can redistribute it and/or modify it under the terms of the
  GNU General Public License as published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
  even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit MouseInputIntf;

{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}
interface

uses
  Classes, SysUtils, {$IFNDEF FPC}Types, windows,{$ENDIF} Controls, Forms;

type
  { TMouseInput }

  TMouseInput = class
  protected
    procedure DoDown(Button: TMouseButton); dynamic; abstract;
    procedure DoMove(ScreenX, ScreenY: Integer); dynamic; abstract;
    procedure DoUp(Button: TMouseButton); dynamic; abstract;
    procedure DoScrollUp; dynamic; abstract;
    procedure DoScrollDown; dynamic; abstract;
  public
    procedure Down(Button: TMouseButton; Shift: TShiftState); overload;
    procedure Down(Button: TMouseButton; Shift: TShiftState; Control: TControl; X, Y: Integer); overload;
    procedure Down(Button: TMouseButton; Shift: TShiftState; ScreenX, ScreenY: Integer); overload;

    procedure Move(Shift: TShiftState; Control: TControl; X, Y: Integer; Duration: Integer = 0); overload;
    procedure MoveBy(Shift: TShiftState; DX, DY: Integer; Duration: Integer = 0); overload;
    procedure Move(Shift: TShiftState; ScreenX, ScreenY: Integer; Duration: Integer); overload;
    procedure Move(Shift: TShiftState; ScreenX, ScreenY: Integer); overload;

    procedure ScrollUp(Shift: TShiftState); overload;
    procedure ScrollUp(Shift: TShiftState; Control: TControl; X, Y: Integer); overload;
    procedure ScrollUp(Shift: TShiftState; ScreenX, ScreenY: Integer); overload;
    procedure ScrollDown(Shift: TShiftState); overload;
    procedure ScrollDown(Shift: TShiftState; Control: TControl; X, Y: Integer); overload;
    procedure ScrollDown(Shift: TShiftState; ScreenX, ScreenY: Integer); overload;

    procedure Up(Button: TMouseButton; Shift: TShiftState); overload;
    procedure Up(Button: TMouseButton; Shift: TShiftState; Control: TControl; X, Y: Integer); overload;
    procedure Up(Button: TMouseButton; Shift: TShiftState; ScreenX, ScreenY: Integer); overload;

    procedure Click(Button: TMouseButton; Shift: TShiftState); overload;
    procedure Click(Button: TMouseButton; Shift: TShiftState; Control: TControl; X, Y: Integer); overload;
    procedure Click(Button: TMouseButton; Shift: TShiftState; ScreenX, ScreenY: Integer); overload;

    procedure DblClick(Button: TMouseButton; Shift: TShiftState); overload;
    procedure DblClick(Button: TMouseButton; Shift: TShiftState; Control: TControl; X, Y: Integer); overload;
    procedure DblClick(Button: TMouseButton; Shift: TShiftState; ScreenX, ScreenY: Integer); overload;
  end;

implementation

uses
  Math, MouseAndKeyInput;

{ TMouseInput }

procedure TMouseInput.Down(Button: TMouseButton; Shift: TShiftState);
begin
  KeyInput.Apply(Shift);
  try
    DoDown(Button);
  finally
    KeyInput.Unapply(Shift);
  end;
  Application.ProcessMessages;
end;

procedure TMouseInput.Down(Button: TMouseButton; Shift: TShiftState;
  Control: TControl; X, Y: Integer);
var
  P: TPoint;
begin
  P := Control.ClientToScreen(Point(X, Y));
  Down(Button, Shift, P.X, P.Y);
end;

procedure TMouseInput.Down(Button: TMouseButton; Shift: TShiftState;
  ScreenX, ScreenY: Integer);
begin
  KeyInput.Apply(Shift);
  try
    DoMove(ScreenX, ScreenY);
    DoDown(Button);
  finally
    KeyInput.Unapply(Shift);
  end;
end;

procedure TMouseInput.Move(Shift: TShiftState; Control: TControl; X, Y: Integer; Duration: Integer = 0);
var
  P: TPoint;
begin
  P := Control.ClientToScreen(Point(X, Y));
  Move(Shift, P.X, P.Y, Duration);
end;

procedure TMouseInput.MoveBy(Shift: TShiftState; DX, DY: Integer; Duration: Integer = 0);
var
  P: TPoint;
begin
  P := Mouse.CursorPos;
  Move(Shift, P.X + DX, P.Y + DY, Duration);
end;

procedure TMouseInput.Move(Shift: TShiftState; ScreenX, ScreenY: Integer; Duration: Integer);
const
  Interval = 20; //ms
var
  TimeStep: Integer;
  X, Y: Integer;
  Start: TPoint;
  S: LongWord;
begin
  Start := Mouse.CursorPos;

  while Duration > 0 do
  begin
    TimeStep := Min(Interval, Duration);

    S := {%H-}{$IFNDEF FPC}Windows.{$ENDIF}GetTickCount;
    while {%H-}{$IFNDEF FPC}Windows.{$ENDIF}GetTickCount - S < TimeStep do Application.ProcessMessages;

    X := Start.X + ((ScreenX - Start.X) * TimeStep) div Duration;
    Y := Start.Y + ((ScreenY - Start.Y) * TimeStep) div Duration;
    Move(Shift, X, Y);

    Duration := Duration - TimeStep;
    Start := Point(X, Y);
  end;

  Move(Shift, ScreenX, ScreenY);
end;

procedure TMouseInput.Move(Shift: TShiftState; ScreenX, ScreenY: Integer);
begin
  KeyInput.Apply(Shift);
  try
    DoMove(ScreenX, ScreenY);
  finally
    KeyInput.Unapply(Shift);
  end;
  Application.ProcessMessages;
end;

procedure TMouseInput.ScrollUp(Shift: TShiftState);
begin
  KeyInput.Apply(Shift);
  try
    DoScrollUp;
  finally
    KeyInput.Unapply(Shift);
  end;
  Application.ProcessMessages;
end;

procedure TMouseInput.ScrollUp(Shift: TShiftState; Control: TControl;
  X, Y: Integer);
var
  P: TPoint;
begin
  P := Control.ClientToScreen(Point(X, Y));
  ScrollUp(Shift, P.X, P.Y);
end;

procedure TMouseInput.ScrollUp(Shift: TShiftState; ScreenX, ScreenY: Integer);
begin
  Move(Shift, ScreenX, ScreenY);
  ScrollUp(Shift);
end;

procedure TMouseInput.ScrollDown(Shift: TShiftState);
begin
  KeyInput.Apply(Shift);
  try
    DoScrollDown;
  finally
    KeyInput.Unapply(Shift);
  end;
  Application.ProcessMessages;
end;

procedure TMouseInput.ScrollDown(Shift: TShiftState; Control: TControl;
  X, Y: Integer);
var
  P: TPoint;
begin
  P := Control.ClientToScreen(Point(X, Y));
  ScrollDown(Shift, P.X, P.Y);
end;

procedure TMouseInput.ScrollDown(Shift: TShiftState; ScreenX, ScreenY: Integer);
begin
  Move(Shift, ScreenX, ScreenY);
  ScrollDown(Shift);
end;

procedure TMouseInput.Up(Button: TMouseButton; Shift: TShiftState);
begin
  KeyInput.Apply(Shift);
  try
    DoUp(Button);
  finally
    KeyInput.Unapply(Shift);
  end;
  Application.ProcessMessages;
end;

procedure TMouseInput.Up(Button: TMouseButton; Shift: TShiftState;
  Control: TControl; X, Y: Integer);
var
  P: TPoint;
begin
  P := Control.ClientToScreen(Point(X, Y));
  Up(Button, Shift, P.X, P.Y);
end;

procedure TMouseInput.Up(Button: TMouseButton; Shift: TShiftState;
  ScreenX, ScreenY: Integer);
begin
  Move(Shift, ScreenX, ScreenY);
  Up(Button, Shift);
end;

procedure TMouseInput.Click(Button: TMouseButton; Shift: TShiftState);
begin
  Down(Button, Shift);
  Up(Button, Shift);
end;

procedure TMouseInput.Click(Button: TMouseButton; Shift: TShiftState;
  Control: TControl; X, Y: Integer);
var
  P: TPoint;
begin
  P := Control.ClientToScreen(Point(X, Y));
  Click(Button, Shift, P.X, P.Y);
end;

procedure TMouseInput.Click(Button: TMouseButton; Shift: TShiftState;
  ScreenX, ScreenY: Integer);
begin
  Move(Shift, ScreenX, ScreenY);
  Click(Button, Shift);
end;

procedure TMouseInput.DblClick(Button: TMouseButton; Shift: TShiftState);
begin
  Click(Button, Shift);
  Click(Button, Shift);
end;

procedure TMouseInput.DblClick(Button: TMouseButton; Shift: TShiftState;
  Control: TControl; X, Y: Integer);
var
  P: TPoint;
begin
  P := Control.ClientToScreen(Point(X, Y));
  DblClick(Button, Shift, P.X, P.Y);
end;

procedure TMouseInput.DblClick(Button: TMouseButton; Shift: TShiftState;
  ScreenX, ScreenY: Integer);
begin
  Move(Shift, ScreenX, ScreenY);
  DblClick(Button, Shift);
end;

end.

