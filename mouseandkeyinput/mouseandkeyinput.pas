{ MouseAndKeyInput

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
unit MouseAndKeyInput;

interface

uses
  MouseInputIntf,
  KeyInputIntf,
  {$IFDEF FPC}
	  {$IFDEF WINDOWS}
	  WinMouseInput,
	  WinKeyInput,
	  {$ENDIF}
	  {$IFDEF UNIX}
	    {$IFDEF LCLcarbon}
	    CarbonMouseInput,
	    CarbonKeyInput,
	    {$ELSE}
	    XMouseInput,
	    XKeyInput,
	    {$ENDIF}
	  {$ENDIF}
  {$ELSE}
	  WinMouseInput,
	  WinKeyInput,  
  {$ENDIF}
  Classes, SysUtils;

var
  MouseInput: TMouseInput;
  KeyInput: TKeyInput;

implementation



initialization

  // Create platform specific object for mouse input
  MouseInput := InitializeMouseInput;

  // Create platform specific object for key input
  KeyInput := InitializeKeyInput;

finalization

  FreeAndNil(MouseInput);
  FreeAndNil(KeyInput);


end.
