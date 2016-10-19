{
  Created by Fox. Part of BGRA Controls.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit BGRAResizeSpeedButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Buttons, LResources, Forms,
  Controls, Graphics, Dialogs, BGRASpeedButton, BGRABitmap;

type
  TBGRAResizeSpeedButton = class(TBGRASpeedButton)
  private
    { Private declarations }
    FBGRA: TBGRABitmap;
  protected
    { Protected declarations }
    function DrawGlyph(ACanvas: TCanvas; const AClient: TRect;
      const {%H-}AOffset: TPoint; AState: TButtonState; {%H-}ATransparent: boolean;
      {%H-}BiDiFlags: longint): TRect; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

function TBGRAResizeSpeedButton.DrawGlyph(ACanvas: TCanvas;
  const AClient: TRect; const AOffset: TPoint; AState: TButtonState;
  ATransparent: boolean; BiDiFlags: longint): TRect;

begin
  Result := Rect(0, 0, 0, 0);
  if Glyph = nil then
    Exit;
  Result := AClient;
  if Assigned(Glyph) and not Glyph.Empty then
  begin
    FBGRA.Assign(Glyph);
    BGRAReplace(FBGRA, FBGRA.Resample(Self.Width - 6, Self.Height - 6));
    if (AState = bsDown) or (Down = True) then
      FBGRA.Draw(ACanvas, 4, 4, False)
    else
      FBGRA.Draw(ACanvas, 3, 3, False);
  end;
end;

constructor TBGRAResizeSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBGRA := TBGRABitmap.Create;
end;

destructor TBGRAResizeSpeedButton.Destroy;
begin
  FBGRA.Free;
  inherited Destroy;
end;

procedure Register;
begin
  {$I icons\bgraresizespeedbutton_icon.lrs}
  RegisterComponents('BGRA Controls', [TBGRAResizeSpeedButton]);
end;

end.
