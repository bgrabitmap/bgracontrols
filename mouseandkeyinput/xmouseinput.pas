{ XMouseInput

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
unit XMouseInput;

{$mode objfpc}{$H+}
{$linklib Xtst}

interface

uses
  Classes, SysUtils, Controls, Forms,
  XLib, MouseInputIntf;
  
type

  { TXMouseInput }

  TXMouseInput = class(TMouseInput)
  protected
    procedure DoDown(Button: TMouseButton); override;
    procedure DoMove(ScreenX, ScreenY: Integer); override;
    procedure DoUp(Button: TMouseButton); override;
    procedure DoScrollUp; override;
    procedure DoScrollDown; override;
  end;
  
function InitializeMouseInput: TMouseInput;

function XTestFakeButtonEvent(dpy: PDisplay; button: dword; is_press: Boolean;
  delay: dword): longint; cdecl; external;

function XTestFakeMotionEvent(dpy: PDisplay; screen: longint; x: longint; y: longint;
  delay: dword): longint; cdecl; external;

implementation

function InitializeMouseInput: TMouseInput;
begin
  Result := TXMouseInput.Create;
end;

const
  MouseButtonToXButton: array [TMouseButton] of Integer = (1, 3, 2, 4, 5);

{ TXMouseInput }

procedure TXMouseInput.DoDown(Button: TMouseButton);
var
  Display: PDisplay;
begin
  Display := XOpenDisplay(nil);
  XTestFakeButtonEvent(Display, MouseButtonToXButton[Button], True, 0);
  XFlush(Display);
  XCloseDisplay(Display);
end;

procedure TXMouseInput.DoMove(ScreenX, ScreenY: Integer);
var
  Display: PDisplay;
begin
  Display := XOpenDisplay(nil);
  XTestFakeMotionEvent(Display, 0, ScreenX, ScreenY, 0);
  XFlush(Display);
  XCloseDisplay(Display);
end;

procedure TXMouseInput.DoUp(Button: TMouseButton);
var
  Display: PDisplay;
begin
  Display := XOpenDisplay(nil);
  XTestFakeButtonEvent(Display, MouseButtonToXButton[Button], False, 0);
  XFlush(Display);
  XCloseDisplay(Display);
end;

procedure TXMouseInput.DoScrollUp;
var
  Display: PDisplay;
begin
  Display := XOpenDisplay(nil);
  XTestFakeButtonEvent(Display, 4, True, 0);
  XTestFakeButtonEvent(Display, 4, False, 0);
  XFlush(Display);
  XCloseDisplay(Display);
end;

procedure TXMouseInput.DoScrollDown;
var
  Display: PDisplay;
begin
  Display := XOpenDisplay(nil);
  XTestFakeButtonEvent(Display, 5, True, 0);
  XTestFakeButtonEvent(Display, 5, False, 0);
  XFlush(Display);
  XCloseDisplay(Display);
end;

end.

