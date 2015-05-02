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

unit BGRAVirtualScreen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, LMessages, BGRABitmap, BCTypes;

type
  { TCustomBGRAVirtualScreen }

  TCustomBGRAVirtualScreen = class(TCustomPanel)
  private
    { Private declarations }
    FBGRA:        TBGRABitmap;
    FOnRedraw:    TBGRARedrawEvent;
    FBevelInner, FBevelOuter: TPanelBevel;
    FBevelWidth:  TBevelWidth;
    FBorderWidth: TBorderWidth;
    FAlignment:   TAlignment;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetBevelInner(const AValue: TPanelBevel);
    procedure SetBevelOuter(const AValue: TPanelBevel);
    procedure SetBevelWidth(const AValue: TBevelWidth);
    procedure SetBorderWidth(const AValue: TBorderWidth);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure Resize; override;
    procedure BGRASetSize(AWidth, AHeight: integer);
    procedure RedrawBitmapContent; virtual;
    procedure SetColor(Value: TColor); override;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure SetEnabled(Value: boolean); override;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure RedrawBitmap;
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
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property ChildSizing;
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
    property OnGetDockCaption;
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

procedure Register;

implementation

uses BGRABitmapTypes, Types;

procedure Register;
begin
  {$I icons\bgravirtualscreen_icon.lrs}
  RegisterComponents('BGRA Controls', [TBGRAVirtualScreen]);
end;

{ TCustomBGRAVirtualScreen }

procedure TCustomBGRAVirtualScreen.SetAlignment(const Value: TAlignment);
begin
  if FAlignment = Value then
    exit;
  FAlignment := Value;
  DiscardBitmap;
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

procedure TCustomBGRAVirtualScreen.Paint;
begin
  {$IFDEF WINDOWS}
  // to avoid flickering in Windows running without themes (classic style)
  DoubleBuffered := ControlCount <> 0;
  {$ENDIF}
  BGRASetSize(Width, Height);
  FBGRA.Draw(Canvas, 0, 0);
end;

procedure TCustomBGRAVirtualScreen.Resize;
begin
  inherited Resize;
  if (FBGRA <> nil) and ((Width <> FBGRA.Width) or (Height <> FBGRA.Height)) then
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
    FBGRA.Fill(ColorToRGB(Color));

    ARect := GetClientRect;

    // if BevelOuter is set then draw a frame with BevelWidth
    if (BevelOuter <> bvNone) and (BevelWidth > 0) then
      FBGRA.CanvasBGRA.Frame3d(ARect, BevelWidth, BevelOuter,
        BGRA(255, 255, 255, 200), BGRA(0, 0, 0, 160)); // Note: Frame3D inflates ARect

    InflateRect(ARect, -BorderWidth, -BorderWidth);

    // if BevelInner is set then skip the BorderWidth and draw a frame with BevelWidth
    if (BevelInner <> bvNone) and (BevelWidth > 0) then
      FBGRA.CanvasBGRA.Frame3d(ARect, BevelWidth, BevelInner,
        BGRA(255, 255, 255, 160), BGRA(0, 0, 0, 160)); // Note: Frame3D inflates ARect

    if Caption <> '' then
    begin
      FBGRA.CanvasBGRA.Font.Assign(Canvas.Font);
      TS := Canvas.TextStyle;
      TS.Alignment := Alignment;
      TS.Layout := tlTop;
      TS.Opaque := False;
      TS.Clipping := False;
      TS.SystemFont := Canvas.Font.IsDefault;

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
  inherited SetColor(Value);
end;

{$hints off}
procedure TCustomBGRAVirtualScreen.WMEraseBkgnd(var Message: TLMEraseBkgnd);
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
