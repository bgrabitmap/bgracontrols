{ This component partialy solve problem with no alpha in lazarus GTK.
  It is using BGRABitmap library for drawing icons.

  Copyright (C) 2011 Krzysztof Dibowski dibowski at interia.pl

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
unit BGRASpeedButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons, BGRABitmap,
  BGRABitmapTypes;

{$IFDEF LCLgtk}
  {$DEFINE BGRA_DRAW}
{$ELSE}
  {$IFDEF LCLgtk2}
    {$DEFINE BGRA_DRAW}
  {$ENDIF}
{$ENDIF}

type

  { TBGRASpeedButton }

  TBGRASpeedButton = class(TSpeedButton)
  private
    { Private declarations }
    {$IFDEF BGRA_DRAW}
    FBGRA: TBGRABitmap;
    {$ENDIF}
  protected
    { Protected declarations }
    {$IFDEF BGRA_DRAW}
    function DrawGlyph(ACanvas: TCanvas; const AClient: TRect;
      const AOffset: TPoint; AState: TButtonState; ATransparent: boolean;
      BiDiFlags: longint): TRect; override;
    {$ENDIF}
  public
    { Public declarations }
    {$IFDEF BGRA_DRAW}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$ENDIF}
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I bgraspeedbutton_icon.lrs}
  RegisterComponents('BGRA Controls', [TBGRASpeedButton]);
end;

{$IFDEF BGRA_DRAW}
{ TBGRASpeedButton }

function TBGRASpeedButton.DrawGlyph(ACanvas: TCanvas; const AClient: TRect;
  const AOffset: TPoint; AState: TButtonState; ATransparent: boolean;
  BiDiFlags: longint): TRect;
begin
  {*** We are using BGRABitmap drawing only ***}
  {Result := inherited DrawGlyph(ACanvas, AClient, AOffset, AState,
    ATransparent, BiDiFlags); }

  if Glyph = nil then
    Exit;
  { It's not good solution assigning glyph on each draw call but FGlyph and SetGlyph is
    in private section }
  FBGRA.Assign(Glyph);

  if Assigned(Glyph) then
  begin
    if (AState = bsDown) or (Down = True) then
      FBGRA.Draw(ACanvas, AOffset.x + 1, AOffset.y + 1, False)
    else
      FBGRA.Draw(ACanvas, AOffset.x, AOffset.y, False);
  end;
end;

constructor TBGRASpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBGRA := TBGRABitmap.Create;
end;

destructor TBGRASpeedButton.Destroy;
begin
  FBGRA.Free;
  inherited Destroy;
end;

{$ENDIF}

end.
