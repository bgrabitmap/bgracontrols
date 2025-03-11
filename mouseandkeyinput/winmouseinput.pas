{ WinMouseInput

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
unit WinMouseInput;

{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}

interface

uses
  Classes, SysUtils, Controls, Forms,
  Windows, {$IFDEF FPC}JwaWinUser,{$ENDIF}
  MouseInputIntf;

type

  { TWinMouseInput }

  TWinMouseInput = class(TMouseInput)
  protected
    procedure DoDown(Button: TMouseButton); override;
    procedure DoMove(ScreenX, ScreenY: Integer); override;
    procedure DoUp(Button: TMouseButton); override;
    procedure DoScrollUp; override;
    procedure DoScrollDown; override;
  end;

function InitializeMouseInput: TMouseInput;

implementation

function InitializeMouseInput: TMouseInput;
begin
  Result := TWinMouseInput.Create;
end;

procedure SendMouseInput(Flag: DWORD; MouseData: DWORD = 0); overload;
var
  Input: TInput;
begin
{$IFDEF VER2_6}
  FillChar(Input, SizeOf(Input), 0);
{$ELSE}
  Input := Default(TInput);
{$ENDIF}
  Input.mi.mouseData := MouseData;
  Input.{$IFDEF FPC}type_{$ELSE}Itype{$ENDIF} := INPUT_MOUSE;
  Input.mi.dwFlags := Flag;

  SendInput(1, {$IFDEF FPC}@{$ENDIF}Input, SizeOf(Input));
end;

procedure SendMouseInput(Flag: DWORD; X, Y: Integer); overload;
var
  Input: TInput;
begin
{$IFDEF VER2_6}
  FillChar(Input, SizeOf(Input), 0);
{$ELSE}
  Input := Default(TInput);
{$ENDIF}
  Input.{$IFDEF FPC}type_{$ELSE}Itype{$ENDIF} := INPUT_MOUSE;
  Input.mi.dx := MulDiv(X, 65535, Screen.Width - 1); // screen horizontal coordinates: 0 - 65535
  Input.mi.dy := MulDiv(Y, 65535, Screen.Height - 1); // screen vertical coordinates: 0 - 65535
  Input.mi.dwFlags := Flag or MOUSEEVENTF_ABSOLUTE;

  SendInput(1, {$IFDEF FPC}@{$ENDIF}Input, SizeOf(Input));
end;

{ TWinMouseInput }

procedure TWinMouseInput.DoDown(Button: TMouseButton);
var
  Flag: DWORD;
begin
  case Button of
    mbRight: Flag := MOUSEEVENTF_RIGHTDOWN;
    mbMiddle: Flag := MOUSEEVENTF_MIDDLEDOWN;
  else
    Flag := MOUSEEVENTF_LEFTDOWN;
  end;
  SendMouseInput(Flag);
end;

procedure TWinMouseInput.DoMove(ScreenX, ScreenY: Integer);
begin
  SendMouseInput(MOUSEEVENTF_MOVE, ScreenX, ScreenY);
end;

procedure TWinMouseInput.DoUp(Button: TMouseButton);
var
  Flag: DWORD;
begin
  case Button of
    mbRight: Flag := MOUSEEVENTF_RIGHTUP;
    mbMiddle: Flag := MOUSEEVENTF_MIDDLEUP;
  else
    Flag := MOUSEEVENTF_LEFTUP;
  end;
  SendMouseInput(Flag);
end;

procedure TWinMouseInput.DoScrollUp;
begin
  SendMouseInput(MOUSEEVENTF_WHEEL, WHEEL_DELTA);
end;

procedure TWinMouseInput.DoScrollDown;
begin
  SendMouseInput(MOUSEEVENTF_WHEEL, DWORD(-WHEEL_DELTA));
end;

end.

