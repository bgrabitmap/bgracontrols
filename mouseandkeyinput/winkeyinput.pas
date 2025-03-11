{ WinKeyInput

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
unit WinKeyInput;

{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}
interface

uses
  Classes, SysUtils, Controls, Forms,
  Windows, {$IFDEF FPC}JwaWinUser,{$ENDIF}
  KeyInputIntf;

type

  { TWinKeyInput }

  TWinKeyInput = class(TKeyInput)
  protected
    procedure DoDown(Key: Word); override;
    procedure DoUp(Key: Word); override;
  end;

function InitializeKeyInput: TKeyInput;

implementation

function InitializeKeyInput: TKeyInput;
begin
  Result := TWinKeyInput.Create;
end;

procedure SendKeyInput(Flag: DWORD; Key: Word);
var
  Input: TInput;
begin
  FillChar({%H-}Input, SizeOf(Input), 0);
  Input.{$IFDEF FPC}type_{$ELSE}Itype{$ENDIF} := INPUT_KEYBOARD;
  Input.ki.dwFlags := Flag;
  Input.ki.wVk := Key;

  SendInput(1, {$IFDEF FPC}@{$ENDIF}Input, SizeOf(Input));
end;


{ TWinKeyInput }

procedure TWinKeyInput.DoDown(Key: Word);
begin
  SendKeyInput(0, Key);
end;

procedure TWinKeyInput.DoUp(Key: Word);
begin
  SendKeyInput(KEYEVENTF_KEYUP, Key);
end;

end.

