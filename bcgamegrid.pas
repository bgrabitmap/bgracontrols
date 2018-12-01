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

{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCGameGrid;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources, LCLProc,{$ENDIF} Types, Forms, Controls, Graphics, Dialogs,
  {$IFNDEF FPC}BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCBaseCtrls, BGRABitmap, BGRABitmapTypes;

type

  TOnRenderControl = procedure(Sender: TObject; Bitmap: TBGRABitmap;
    r: TRect; n, x, y: integer) of object;
  TOnClickControl = procedure(Sender: TObject; n, x, y: integer) of object;

  { TBCCustomGrid }

  TBCCustomGrid = class(TBCGraphicControl)
  private
    FBGRA: TBGRABitmap;
    FGridWidth: integer;
    FGridHeight: integer;
    FBlockWidth: integer;
    FBlockHeight: integer;
    FOnRenderControl: TOnRenderControl;
    FOnClickControl: TOnClickControl;
  private
    procedure SetFBlockHeight(AValue: integer);
    procedure SetFBlockWidth(AValue: integer);
    procedure SetFGridHeight(AValue: integer);
    procedure SetFGridWidth(AValue: integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Click; override;
    procedure DrawControl; override;
    procedure RenderControl; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RenderAndDrawControl;
    property GridWidth: integer read FGridWidth write SetFGridWidth;
    property GridHeight: integer read FGridHeight write SetFGridHeight;
    property BlockWidth: integer read FBlockWidth write SetFBlockWidth;
    property BlockHeight: integer read FBlockHeight write SetFBlockHeight;
    property OnRenderControl: TOnRenderControl
      read FOnRenderControl write FOnRenderControl;
    property OnClickControl: TOnClickControl read FOnClickControl write FOnClickControl;
  published
    { Published declarations }
  end;

  TBCGameGrid = class(TBCCustomGrid)
  published
    property GridWidth;
    property GridHeight;
    property BlockWidth;
    property BlockHeight;
    // Support 'n, x, y'
    property OnRenderControl;
    property OnClickControl;
    // 'Classic' events, to be changed...
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    // Ok...
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

{$IFDEF FPC}
procedure Register;
begin
  {$IFDEF FPC}
  {$I icons\bcgamegrid_icon.lrs}
  {$ENDIF}
  RegisterComponents('BGRA Controls', [TBCGameGrid]);
end;
{$ENDIF}

{ TBCCustomGrid }

procedure TBCCustomGrid.SetFBlockHeight(AValue: integer);
begin
  if FBlockHeight = AValue then
    Exit;
  if AValue < 1 then
    FBlockHeight := 1
  else
    FBlockHeight := AValue;

  RenderAndDrawControl;
end;

procedure TBCCustomGrid.SetFBlockWidth(AValue: integer);
begin
  if FBlockWidth = AValue then
    Exit;
  if AValue < 1 then
    FBlockWidth := 1
  else
    FBlockWidth := AValue;

  RenderAndDrawControl;
end;

procedure TBCCustomGrid.SetFGridHeight(AValue: integer);
begin
  if FGridHeight = AValue then
    Exit;
  if AValue < 1 then
    FGridHeight := 1
  else
    FGridHeight := AValue;

  RenderAndDrawControl;
end;

procedure TBCCustomGrid.SetFGridWidth(AValue: integer);
begin
  if FGridWidth = AValue then
    Exit;
  if AValue < 1 then
    FGridWidth := 1
  else
    FGridWidth := AValue;

  RenderAndDrawControl;
end;

procedure TBCCustomGrid.Click;
var
  n, x, y: integer;
  r: TRect;
var
  pos: TPoint;
begin
  if (BlockWidth <= 0) or (BlockHeight <= 0) or (GridWidth <= 0) or
    (GridHeight <= 0) then
    Exit;

  pos := ScreenToClient(Mouse.CursorPos);

  n := 0;

  for y := 0 to GridHeight - 1 do
  begin
    for x := 0 to GridWidth - 1 do
    begin
      r.Left := BlockWidth * x;
      r.Top := BlockHeight * y;
      r.Right := r.Left + BlockWidth;
      r.Bottom := r.Top + BlockHeight;

      if (pos.x >= r.Left) and (pos.x <= r.Right) and (pos.y >= r.Top) and
        (pos.y <= r.Bottom) then
      begin
        //DebugLn(['TControl.Click ',DbgSName(Self)]);
        if Assigned(FOnClickControl) and (Action <> nil) and
          (not CompareMethods(TMethod(Action.OnExecute), TMethod(FOnClickControl))) then
          // the OnClick is set and differs from the Action => call the OnClick
          FOnClickControl(Self, n, x, y)
        else if (not (csDesigning in ComponentState)) and (ActionLink <> nil) then
          ActionLink.Execute(Self)
        else if Assigned(FOnClickControl) then
          FOnClickControl(Self, n, x, y);
      end;

      Inc(n);
    end;
  end;
end;

procedure TBCCustomGrid.DrawControl;
begin
  if FBGRA <> nil then
    FBGRA.Draw(Canvas, 0, 0, False);
end;

procedure TBCCustomGrid.RenderControl;
var
  n, x, y: integer;
  r: TRect;
begin
  if (BlockWidth <= 0) or (BlockHeight <= 0) or (GridWidth <= 0) or
    (GridHeight <= 0) then
    Exit;

  if FBGRA <> nil then
    FreeAndNil(FBGRA);

  FBGRA := TBGRABitmap.Create(Width, Height);

  n := 0;

  for y := 0 to GridHeight - 1 do
  begin
    for x := 0 to GridWidth - 1 do
    begin
      r.Left := BlockWidth * x;
      r.Top := BlockHeight * y;
      r.Right := r.Left + BlockWidth;
      r.Bottom := r.Top + BlockHeight;

      FBGRA.Rectangle(r, BGRA(127, 127, 127, 127), BGRA(255, 255, 255, 127),
        dmDrawWithTransparency);

      if Assigned(FOnRenderControl) then
        FOnRenderControl(Self, FBGRA, r, n, x, y);

      Inc(n);
    end;
  end;
end;

procedure TBCCustomGrid.RenderAndDrawControl;
begin
  RenderControl;
  Invalidate;
end;

constructor TBCCustomGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  BlockHeight := 30;
  BlockWidth := 30;
  GridHeight := 5;
  GridWidth := 5;
end;

destructor TBCCustomGrid.Destroy;
begin
  if FBGRA <> nil then
    FreeAndNil(FBGRA);
  inherited Destroy;
end;

end.
