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
unit BGRAImageList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  GraphType, BGRABitmap, BGRABitmapTypes, ImgList;

{$IFDEF LCLgtk}
  {$DEFINE BGRA_DRAW}
{$ELSE}
  {$IFDEF LCLgtk2}
    {$DEFINE BGRA_DRAW}
  {$ENDIF}
{$ENDIF}

type

  { TBGRAImageList }

  TBGRAImageList = class(TImageList)
  private
    { Private declarations }
    {$IFDEF BGRA_DRAW}
    FBGRA: TBGRABitmap;
    FBmp:  TBitmap;
    {$ENDIF}
  protected
    { Protected declarations }
  public
    { Public declarations }
    {$IFDEF BGRA_DRAW}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: integer;
      ADrawingStyle: TDrawingStyle; AImageType: TImageType;
      ADrawEffect: TGraphicsDrawEffect); override;
    {$ENDIF}
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I bgraimagelist_icon.lrs}
  RegisterComponents('BGRA Controls', [TBGRAImageList]);
end;

{$IFDEF BGRA_DRAW}
{ TBGRAImageList }
constructor TBGRAImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBGRA := TBGRABitmap.Create;
  FBmp  := TBitmap.Create;
end;

destructor TBGRAImageList.Destroy;
begin
  FBGRA.Free;
  FBmp.Free;
  inherited Destroy;
end;

{ Problem with no alpha is only on GTK so on Windows we use default drawing }
procedure TBGRAImageList.Draw(ACanvas: TCanvas; AX, AY, AIndex: integer;
  ADrawingStyle: TDrawingStyle; AImageType: TImageType;
  ADrawEffect: TGraphicsDrawEffect);
begin
  //inherited; - We use TBGRABitmap drawing only

  // This is required part from TImageList.Draw
  if (AIndex < 0) or (AIndex >= Count) then
    Exit;
  ReferenceNeeded;

  {*** BGRA Drawing *** }
  case ADrawEffect of
    gdeDisabled:
    begin
      GetBitmap(AIndex, FBmp, gdeNormal);
      FBGRA.Assign(FBmp);
      BGRAReplace(FBGRA, FBGRA.FilterGrayscale);
    end;
    else
    begin
      GetBitmap(AIndex, FBmp, ADrawEffect);
      FBGRA.Assign(FBmp);
    end;
  end;
  if ADrawingStyle in [dsFocus, dsSelected] then
    FBGRA.ApplyGlobalOpacity(128);
  FBGRA.Draw(ACanvas, AX, AY, False);
end;

{$ENDIF}

end.
