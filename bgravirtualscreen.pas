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
unit BGRAVirtualScreen;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LMessages, LResources, LCLIntf,{$ENDIF}  Types, Forms, BCBaseCtrls, Controls, Graphics, Dialogs,
  {$IFNDEF FPC}Windows, Messages, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  ExtCtrls, BGRABitmap, BCTypes;

type
  { TCustomBGRAVirtualScreen }

  TCustomBGRAVirtualScreen = class(TBGRACustomPanel)
  private
    { Private declarations }
    FBGRA:        TBGRABitmap;
    FOnRedraw:    TBGRARedrawEvent;
    FBevelInner, FBevelOuter: TPanelBevel;
    FBevelWidth:  TBevelWidth;
    FBorderWidth: TBorderWidth;
    FAlignment:   TAlignment;
    function GetVSCaption: string;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetBevelInner(const AValue: TPanelBevel);
    procedure SetBevelOuter(const AValue: TPanelBevel);
    procedure SetBevelWidth(const AValue: TBevelWidth);
    procedure SetBorderWidth(const AValue: TBorderWidth);
    procedure SetVSCaption(AValue: string);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure Resize; override;
    procedure BGRASetSize(AWidth, AHeight: integer);
    procedure RedrawBitmapContent; virtual;
    procedure SetColor(Value: TColor); {$IFDEF FPC}override;{$ENDIF}
    procedure WMEraseBkgnd(var Message: {$IFDEF FPC}TLMEraseBkgnd{$ELSE}TWMEraseBkgnd{$ENDIF}); message {$IFDEF FPC}LM_ERASEBKGND{$ELSE}WM_ERASEBKGND{$ENDIF};
    procedure SetEnabled(Value: boolean); override;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure RedrawBitmap; overload;
    procedure RedrawBitmap(ARect: TRect); overload;
    procedure RedrawBitmap(ARectArray: array of TRect); overload;
    procedure DiscardBitmap;
    destructor Destroy; override;
  public
    property OnRedraw: TBGRARedrawEvent Read FOnRedraw Write FOnRedraw;
    property Bitmap: TBGRABitmap Read FBGRA;
    property BorderWidth: TBorderWidth Read FBorderWidth Write SetBorderWidth default 0;
    property BevelInner: TPanelBevel Read FBevelInner Write SetBevelInner default bvNone;
    property BevelOuter: TPanelBevel Read FBevelOuter Write SetBevelOuter default bvNone;
    property BevelWidth: TBevelWidth Read FBevelWidth Write SetBevelWidth default 1;
    property Alignment: TAlignment Read FAlignment Write SetAlignment;
    property Caption: string read GetVSCaption write SetVSCaption;
  end;

  TBGRAVirtualScreen = class(TCustomBGRAVirtualScreen)
  published
    property OnRedraw;
    property Bitmap;
    // TPanel
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    {$IFDEF FPC} //#
    property OnGetDockCaption;
    {$ENDIF}
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

uses BGRABitmapTypes, math;

{$IFDEF FPC}
procedure Register;
begin
  //{$I icons\bgravirtualscreen_icon.lrs}
  RegisterComponents('BGRA Controls', [TBGRAVirtualScreen]);
end;
{$ENDIF}
{ TCustomBGRAVirtualScreen }

procedure TCustomBGRAVirtualScreen.SetAlignment(const Value: TAlignment);
begin
  if FAlignment = Value then
    exit;
  FAlignment := Value;
  DiscardBitmap;
end;

function TCustomBGRAVirtualScreen.GetVSCaption: string;
begin
  result := inherited Caption;
end;

procedure TCustomBGRAVirtualScreen.SetBevelInner(const AValue: TPanelBevel);
begin
  if FBevelInner = AValue then
    exit;
  FBevelInner := AValue;
  DiscardBitmap;
end;

procedure TCustomBGRAVirtualScreen.SetBevelOuter(const AValue: TPanelBevel);
begin
  if FBevelOuter = AValue then
    exit;
  FBevelOuter := AValue;
  DiscardBitmap;
end;

procedure TCustomBGRAVirtualScreen.SetBevelWidth(const AValue: TBevelWidth);
begin
  if FBevelWidth = AValue then
    exit;
  FBevelWidth := AValue;
  DiscardBitmap;
end;

procedure TCustomBGRAVirtualScreen.SetBorderWidth(const AValue: TBorderWidth);
begin
  if FBorderWidth = AValue then
    exit;
  FBorderWidth := AValue;
  DiscardBitmap;
end;

procedure TCustomBGRAVirtualScreen.SetVSCaption(AValue: string);
begin
  inherited Caption := AValue;
  DiscardBitmap;
end;

procedure TCustomBGRAVirtualScreen.Paint;
begin
  {$IFDEF WINDOWS}
  // to avoid flickering in Windows running without themes (classic style)
  DoubleBuffered := ControlCount <> 0;
  {$ENDIF}
  BGRASetSize(ClientWidth, ClientHeight);
  FBGRA.Draw(Canvas, 0, 0);
end;

procedure TCustomBGRAVirtualScreen.Resize;
begin
  inherited Resize;
  if (FBGRA <> nil) and ((ClientWidth <> FBGRA.Width) or (ClientHeight <> FBGRA.Height)) then
    DiscardBitmap;
end;

procedure TCustomBGRAVirtualScreen.BGRASetSize(AWidth, AHeight: integer);
begin
  if (FBGRA <> nil) and ((AWidth <> FBGRA.Width) or (AHeight <> FBGRA.Height)) then
  begin
    FBGRA.SetSize(AWidth, AHeight);
    RedrawBitmapContent;
  end;
end;

procedure TCustomBGRAVirtualScreen.RedrawBitmapContent;
var
  ARect: TRect;
  TS: TTextStyle;
begin
  if (FBGRA <> nil) and (FBGRA.NbPixels <> 0) then
  begin
    FBGRA.FillRect(FBGRA.ClipRect, ColorToRGB(Color));

    ARect := GetClientRect;

    // if BevelOuter is set then draw a frame with BevelWidth
    if BevelOuter <> bvNone then
      FBGRA.CanvasBGRA.Frame3d(ARect, BevelWidth, BevelOuter,
        BGRA(255, 255, 255, 200), BGRA(0, 0, 0, 160)); // Note: Frame3D inflates ARect

    InflateRect(ARect, -BorderWidth, -BorderWidth);

    // if BevelInner is set then skip the BorderWidth and draw a frame with BevelWidth
    if BevelInner <> bvNone then
      FBGRA.CanvasBGRA.Frame3d(ARect, BevelWidth, BevelInner,
        BGRA(255, 255, 255, 160), BGRA(0, 0, 0, 160)); // Note: Frame3D inflates ARect

    if Caption <> '' then
    begin
      FBGRA.CanvasBGRA.Font.Assign(Canvas.Font);
      {$IFDEF FPC}//#
      TS := Canvas.TextStyle;
      {$ENDIF}
      TS.Alignment := Alignment;
      TS.Layout := tlTop;
      TS.Opaque := False;
      TS.Clipping := False;
      {$IFDEF FPC}//#
      TS.SystemFont := Canvas.Font.IsDefault;
      {$ENDIF}

      FBGRA.CanvasBGRA.Font.Color := Color xor $FFFFFF;

      if not Enabled then
        FBGRA.CanvasBGRA.Font.Style := [fsStrikeOut]
      else
        FBGRA.CanvasBGRA.Font.Style := [];

      FBGRA.CanvasBGRA.TextRect(ARect, ARect.Left, ARect.Top, Caption, TS);
    end;

    if Assigned(FOnRedraw) then
      FOnRedraw(self, FBGRA);
  end;
end;

procedure TCustomBGRAVirtualScreen.SetColor(Value: TColor);
begin
  if Value <> Color then
    DiscardBitmap;

  {$IFDEF FPC}
  inherited SetColor(Value);
  {$ENDIF}
end;

{$hints off}
procedure TCustomBGRAVirtualScreen.WMEraseBkgnd(var Message: {$IFDEF FPC}TLMEraseBkgnd{$ELSE}TWMEraseBkgnd{$ENDIF});
begin
  //do nothing
end;

{$hints on}

procedure TCustomBGRAVirtualScreen.SetEnabled(Value: boolean);
begin
  if Value <> Enabled then
    DiscardBitmap;
  inherited SetEnabled(Value);
end;

constructor TCustomBGRAVirtualScreen.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  inherited BevelOuter := bvNone;
  FBGRA := TBGRABitmap.Create;
  FBevelWidth := 1;
  FAlignment := taLeftJustify;
  Color := clWhite;
end;

procedure TCustomBGRAVirtualScreen.RedrawBitmap;
begin
  RedrawBitmapContent;
  Repaint;
end;

procedure TCustomBGRAVirtualScreen.RedrawBitmap(ARect: TRect);
var
  All: TRect;
begin
  if Assigned(FBGRA) then
  begin
    All := Rect(0,0,FBGRA.Width,FBGRA.Height);
    ARect.Intersect(All);
    if ARect.IsEmpty then exit;
    if ARect.Contains(All) then
    begin
      RedrawBitmap;
    end
    else
    begin
      FBGRA.ClipRect := ARect;
      RedrawBitmapContent;
      FBGRA.NoClip;
      {$IFDEF LINUX}
      FBGRA.DrawPart(ARect, Canvas, ARect.Left,ARect.Top, True);
      {$ELSE}
      InvalidateRect(Handle, @ARect, False);
      Update;
      {$ENDIF}
    end;
  end;
end;

procedure TCustomBGRAVirtualScreen.RedrawBitmap(ARectArray: array of TRect);
const cellShift = 6;
      cellSize = 1 shl cellShift;
var
  grid: array of array of boolean;
  gW,gH, i,gCount: integer;
  gR: TRect;
  gAll: TRect;
  y,x: LongInt;
  expand: boolean;
begin
  if not Assigned(FBGRA) then exit;
  gW := (Width+cellSize-1) shr cellShift;
  gH := (Height+cellSize-1) shr cellShift;
  gAll := rect(0,0,gW,gH);

  //determine which cells of the grid to redraw
  setlength(grid,gH,gW);
  for i := 0 to high(ARectArray) do
  begin
    with ARectArray[i] do
      gR := rect(max(Left,0) shr cellShift, max(Top,0) shr cellShift,
                 (max(Right,0)+cellSize-1) shr cellShift,
                 (max(Bottom,0)+cellSize-1) shr cellShift);
    gR.Intersect(gAll);
    if gR.IsEmpty then Continue;

    for y := gR.Top to gR.Bottom-1 do
      for x := gR.Left to gR.Right-1 do
        grid[y,x] := true;
  end;

  gCount := 0;
  for y := 0 to gH-1 do
    for x := 0 to gW-1 do
      if grid[y,x] then inc(gCount);

  if gCount >= gH*gW div 5 then
  begin
    RedrawBitmap(rect(0,0,Width,Height));
  end else
  for y := 0 to gH-1 do
  begin
    x := 0;
    while x < gW do
    begin
      if grid[y,x] then
      begin
        gR.Left := x;
        grid[y,x] := false;
        inc(x);
        while (x < gW) and grid[y,x] do
        begin
          grid[y,x] := false;
          inc(x);
        end;
        gR.Right := x;
        gR.Top := y;
        gR.Bottom := y+1;
        expand := true;
        while expand and (gR.Bottom < gH) do
        begin
          expand := true;
          for x := gR.Left to gR.Right-1 do
            if not grid[gR.Bottom, x] then
            begin
              expand := false;
              break;
            end;
          if expand then
          begin
            for x := gR.Left to gR.Right-1 do
              grid[gR.Bottom,x] := false;
            inc(gR.Bottom);
          end;
        end;

        RedrawBitmap(rect(gR.Left shl cellShift,gR.Top shl cellShift,gr.Right shl cellShift,gr.Bottom shl cellShift));
      end else
        inc(x);
    end;
  end;
end;

procedure TCustomBGRAVirtualScreen.DiscardBitmap;
begin
  if (FBGRA <> nil) and (FBGRA.NbPixels <> 0) then
  begin
    FBGRA.SetSize(0, 0);
    Invalidate;
  end;
end;

destructor TCustomBGRAVirtualScreen.Destroy;
begin
  FBGRA.Free;
  inherited Destroy;
end;

end.
