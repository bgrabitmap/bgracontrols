{
  Created by BGRA Controls Team
  Dibo, Circular, lainz (007) and contributors.
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

unit BCToolBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  BGRABitmap, BGRABitmapTypes, BGRAGradients, BCTypes;

type

  { TBCToolBar }

  TBCToolBar = class(TToolBar)
  private
    FLimitMemoryUsage: boolean;
    { Private declarations }
    FOnRedraw: TBGRARedrawEvent;
    FBGRA: TBGRABitmap;
    procedure SetLimitMemoryUsage(AValue: boolean);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure CheckMemoryUsage; virtual;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property OnRedraw: TBGRARedrawEvent read FOnRedraw write FOnRedraw;
    property LimitMemoryUsage: boolean read FLimitMemoryUsage write SetLimitMemoryUsage;
  end;

procedure DrawWindows7ToolBar(Bitmap: TBGRABitmap);

procedure Register;

implementation

procedure DrawWindows7ToolBar(Bitmap: TBGRABitmap);
var
  c1, c2, c3, c4: TBGRAPixel;
  ARect, ARect2: TRect;
begin
  ARect := Rect(0, 0, Bitmap.Width, Bitmap.Height);
  // Font: RGBToColor(30,57,91)

  Bitmap.HorizLine(ARect.Left, ARect.Top, ARect.Right-1, BGRA(169, 191, 214), dmSet);
  Bitmap.HorizLine(ARect.Left, ARect.Top + 1, ARect.Right-1, BGRA(250, 252, 253), dmSet);
  Bitmap.HorizLine(ARect.Left, ARect.Top + 2, ARect.Right-1, BGRA(253, 254, 255), dmSet);

  c1 := BGRA(252, 254, 255);
  c2 := BGRA(243, 248, 253);
  c3 := BGRA(238, 243, 250);
  c4 := BGRA(238, 244, 251);
  ARect2 := Rect(ARect.Left, ARect.Top + 3, ARect.Right, ARect.Bottom - 3);
  DoubleGradientAlphaFill(Bitmap, ARect2, c1, c2, c3, c4, gdVertical,
    gdVertical, gdVertical, 0.5);

  c1 := BGRA(249, 252, 255);
  c2 := BGRA(230, 240, 250);
  c3 := BGRA(220, 230, 244);
  c4 := BGRA(221, 233, 247);
  ARect2 := Rect(ARect.Left + 1, ARect.Top + 3, ARect.Right - 1, ARect.Bottom - 3);
  DoubleGradientAlphaFill(Bitmap, ARect2, c1, c2, c3, c4, gdVertical,
    gdVertical, gdVertical, 0.5);

  Bitmap.HorizLine(ARect.Left, ARect.Bottom - 3, ARect.Right-1, BGRA(228, 239, 251), dmSet);
  Bitmap.HorizLine(ARect.Left, ARect.Bottom - 2, ARect.Right-1, BGRA(205, 218, 234), dmSet);
  Bitmap.HorizLine(ARect.Left, ARect.Bottom - 1, ARect.Right-1, BGRA(160, 175, 195), dmSet);
end;

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCToolBar]);
end;

{ TBCToolBar }

constructor TBCToolBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBGRA := TBGRABitmap.Create;
end;

destructor TBCToolBar.Destroy;
begin
  FBGRA.Free;
  inherited Destroy;
end;

procedure TBCToolBar.SetLimitMemoryUsage(AValue: boolean);
begin
  if FLimitMemoryUsage=AValue then Exit;
  FLimitMemoryUsage:=AValue;
  CheckMemoryUsage;
end;

procedure TBCToolBar.Paint;
begin
  if (FBGRA.Width <> Width) or (FBGRA.Height <> Height) then
  begin
    FBGRA.SetSize(Width, Height);
    if Assigned(FOnRedraw) then
      { Draw using event }
      FOnRedraw(self, FBGRA)
    else
      { Draw this default }
      DrawWindows7ToolBar(FBGRA);
  end;
  FBGRA.Draw(Canvas, 0, 0);
  CheckMemoryUsage;
end;

procedure TBCToolBar.CheckMemoryUsage;
begin
  if FLimitMemoryUsage then
  begin
    if FBGRA.NbPixels <> 0 then
      FBGRA.SetSize(0,0);
  end;
end;

end.
