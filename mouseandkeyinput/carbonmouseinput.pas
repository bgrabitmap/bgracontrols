{ CarbonMouseInput

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
unit CarbonMouseInput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms,
  MacOSAll, CarbonProc,
  MouseInputIntf;
  
type

  { TCarbonMouseInput }

  TCarbonMouseInput = class(TMouseInput)
  protected
    procedure DoDown(Button: TMouseButton); override;
    procedure DoMove(ScreenX, ScreenY: Integer); override;
    procedure DoUp(Button: TMouseButton); override;
  end;
  
function InitializeMouseInput: TMouseInput;


implementation

function InitializeMouseInput: TMouseInput;
begin
  Result := TCarbonMouseInput.Create;
end;

const
  MouseButtonToCarbonButton: array [TMouseButton] of Integer =
    (kCGMouseButtonLeft, kCGMouseButtonRight, kCGMouseButtonCenter,kCGMouseButtonLeft,kCGMouseButtonLeft);
  

{ TCarbonMouseInput }

procedure TCarbonMouseInput.DoDown(Button: TMouseButton);
begin
  CGPostMouseEvent(PointToHIPoint(Mouse.CursorPos), 0, 1, 1, MouseButtonToCarbonButton[Button]);
end;

procedure TCarbonMouseInput.DoMove(ScreenX, ScreenY: Integer);
begin
  CGPostMouseEvent(GetHIPoint(ScreenX, ScreenY), 1, 1, 0, 0);
end;

procedure TCarbonMouseInput.DoUp(Button: TMouseButton);
begin
  CGPostMouseEvent(PointToHIPoint(Mouse.CursorPos), 0, 1, 0, MouseButtonToCarbonButton[Button]);
end;

end.

