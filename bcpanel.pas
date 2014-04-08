{ Equivalent of standard lazarus TPanel but using BGRA Controls framework for render

  Functionality:
  - Customizable background (gradient etc.)
  - Customizable border (frame 3D or normal border, rounding etc)
  - FontEx (shadow etc.)

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
unit BCPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BCBaseCtrls, BGRABitmapTypes, BCTypes, Types;

type
  TOnAfterRenderBCPanel = procedure(Sender: TObject; const ABGRA: TBGRABitmap;
    ARect: TRect) of object;
  TBCPanelBorderStyle = (bpsBorder, bpsFrame3d);

  { TCustomBCPanel }

  TCustomBCPanel = class(TBCStyleCustomControl)
  private
    { Private declarations }
    {$IFDEF DEBUG}
    FRenderCount: Integer;
    {$ENDIF}
    FBackground: TBCBackground;
    FBevelWidth: Integer;
    FBGRA: TBGRABitmapEx;
    FBevelInner, FBevelOuter : TBevelCut;
    FBorder: TBCBorder;
    FBorderBCStyle: TBCPanelBorderStyle;
    FFontEx: TBCFont;
    FOnAfterRenderBCPanel: TOnAfterRenderBCPanel;
    FRounding: TBCRounding;
    procedure SetBackground(AValue: TBCBackground);
    procedure SetBevelInner(AValue: TBevelCut);
    procedure SetBevelOuter(AValue: TBevelCut);
    procedure SetBevelWidth(AValue: Integer);
    procedure SetBorder(AValue: TBCBorder);
    procedure SetBorderBCStyle(AValue: TBCPanelBorderStyle);
    procedure SetFontEx(AValue: TBCFont);
    procedure SetRounding(AValue: TBCRounding);
    procedure Render;
    procedure OnChangeProperty(Sender: TObject; AData: PtrInt);
    procedure OnChangeFont(Sender: TObject; AData: PtrInt);
  protected
    { Protected declarations }
    procedure AdjustClientRect(var aRect: TRect); override;
    class function GetControlClassDefaultSize: TSize; override;
    function GetDefaultDockCaption: String; override;
    procedure SetEnabled(Value: boolean); override;
    procedure TextChanged; override;
  protected
    function GetStyleExtension: String; override;
    {$IFDEF DEBUG}
    function GetDebugText: String; override;
    {$ENDIF}
    procedure DrawControl; override;
    procedure RenderControl; override;
  protected
    property Background: TBCBackground read FBackground write SetBackground;
    property BevelInner: TBevelCut read FBevelInner write SetBevelInner;
    property BevelOuter: TBevelCut read FBevelOuter write SetBevelOuter;
    property BevelWidth: Integer read FBevelWidth write SetBevelWidth;
    property Border: TBCBorder read FBorder write SetBorder;
    property BorderBCStyle: TBCPanelBorderStyle
      read FBorderBCStyle write SetBorderBCStyle default bpsFrame3d;
    property FontEx: TBCFont read FFontEx write SetFontEx;
    property Rounding: TBCRounding read FRounding write SetRounding;
  protected
    { Events }
    property OnAfterRenderBCPanel: TOnAfterRenderBCPanel
      Read FOnAfterRenderBCPanel Write FOnAfterRenderBCPanel;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateControl; override; // Called by EndUpdate
  end;

  { TBCPanel }

  TBCPanel = class(TCustomBCPanel)
  published
    property Align;
    property Anchors;
    property AssignStyle;
    property AutoSize;
    property BorderSpacing;
    property Background;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property Border;
    property BorderBCStyle;
    property Caption;
    property ChildSizing;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FontEx;
    property PopupMenu;
    property Rounding;
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
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnAfterRenderBCPanel;
  end;

procedure Register;

implementation

uses BCTools;

procedure Register;
begin
  {$I bcpanel_icon.lrs}
  RegisterComponents('BGRA Controls', [TBCPanel]);
end;

{ TCustomBCPanel }

procedure TCustomBCPanel.DrawControl;
begin
  inherited DrawControl;
  if FBGRA.NeedRender then
    Render;
  if Assigned (FRounding) then
  begin
    if (FRounding.RoundX<>0) and (FRounding.RoundY<>0) then
      FBGRA.Draw(Self.Canvas, 0, 0, False)
    else
      FBGRA.Draw(Self.Canvas, 0, 0);
  end
  else
    FBGRA.Draw(Self.Canvas, 0, 0);
end;

procedure TCustomBCPanel.RenderControl;
begin
  inherited RenderControl;
  if FBGRA<>nil then
    FBGRA.NeedRender := True;
end;

function TCustomBCPanel.GetStyleExtension: String;
begin
  Result := 'bcpnl';
end;

{$IFDEF DEBUG}
function TCustomBCPanel.GetDebugText: String;
begin
  Result := 'R: '+IntToStr(FRenderCount);
end;
{$ENDIF}

procedure TCustomBCPanel.Render;
var r: TRect;
begin
  if (csCreating in FControlState) or IsUpdating then
    Exit;

  FBGRA.NeedRender := False;

  FBGRA.SetSize(Width, Height);
  FBGRA.Fill(BGRAPixelTransparent);
  RenderBackground(FBGRA.ClipRect, FBackground, TBGRABitmap(FBGRA), FRounding);
  r := FBGRA.ClipRect;

  case FBorderBCStyle of
  bpsBorder:
    begin
      CalculateBorderRect(FBorder,r);
      RenderBorder(r,FBorder, TBGRABitmap(FBGRA), FRounding);
    end;
  bpsFrame3d:
    begin
      // if BevelOuter is set then draw a frame with BevelWidth
      if (FBevelOuter <> bvNone) and (FBevelWidth > 0) then
        FBGRA.CanvasBGRA.Frame3d(r, FBevelWidth, FBevelOuter,
          BGRA(255, 255, 255, 180), BGRA(0, 0, 0, 160)); // Note: Frame3D inflates ARect

      InflateRect(r, -FBevelWidth, -FBevelWidth);

      // if BevelInner is set then skip the BorderWidth and draw a frame with BevelWidth
      if (FBevelInner <> bvNone) and (FBevelWidth > 0) then
        FBGRA.CanvasBGRA.Frame3d(r, FBevelWidth, FBevelInner,
          BGRA(255, 255, 255, 160), BGRA(0, 0, 0, 160)); // Note: Frame3D inflates ARect
    end;
  end;

  if Caption <> '' then
    RenderText(r,FFontEx,Caption,TBGRABitmap(FBGRA));

  if Assigned(FOnAfterRenderBCPanel) then
    FOnAfterRenderBCPanel(Self, FBGRA, r);

  {$IFDEF DEBUG}
  FRenderCount += 1;
  {$ENDIF}
end;

procedure TCustomBCPanel.OnChangeProperty(Sender: TObject; AData: PtrInt);
begin
  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.OnChangeFont(Sender: TObject; AData: PtrInt);
begin
  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.SetRounding(AValue: TBCRounding);
begin
  if FRounding = AValue then Exit;
  FRounding.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.AdjustClientRect(var aRect: TRect);
var BevelSize: Integer;
begin
  inherited AdjustClientRect(aRect);

  BevelSize := BorderWidth;
  if (BevelOuter <> bvNone) then
    inc(BevelSize, BevelWidth);
  if (BevelInner <> bvNone) then
    inc(BevelSize, BevelWidth);

  InflateRect(aRect, -BevelSize, -BevelSize);
end;

class function TCustomBCPanel.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 170;
  Result.CY := 50;
end;

function TCustomBCPanel.GetDefaultDockCaption: String;
begin
  Result := Caption;
end;

procedure TCustomBCPanel.SetBackground(AValue: TBCBackground);
begin
  if FBackground = AValue then Exit;
  FBackground.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.SetBevelInner(AValue: TBevelCut);
begin
  if FBevelInner = AValue then Exit;
  FBevelInner := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.SetBevelOuter(AValue: TBevelCut);
begin
  if FBevelOuter = AValue then Exit;
  FBevelOuter := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.SetBevelWidth(AValue: Integer);
begin
  if FBevelWidth = AValue then Exit;
  FBevelWidth := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.SetBorder(AValue: TBCBorder);
begin
  if FBorder = AValue then Exit;
  FBorder.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.SetBorderBCStyle(AValue: TBCPanelBorderStyle);
begin
  if FBorderBCStyle = AValue then Exit;
  FBorderBCStyle := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.SetFontEx(AValue: TBCFont);
begin
  if FFontEx = AValue then Exit;
  FFontEx.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.TextChanged;
begin
  inherited TextChanged;

  RenderControl;
  Invalidate;
end;

constructor TCustomBCPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  {$IFDEF DEBUG}
  FRenderCount := 0;
  {$ENDIF}
  DisableAutoSizing;
  Include(FControlState, csCreating);
  BeginUpdate;
  try
    ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse,
      csClickEvents, csSetCaption, csDoubleClicks, csReplicatable,
      csNoFocus, csAutoSize0x0]
      - [csOpaque]; // we need the default background
    //Self.DoubleBuffered := True;
    with GetControlClassDefaultSize do
      SetInitialBounds(0, 0, CX, CY);

    FBGRA               := TBGRABitmapEx.Create;
    FBorderBCStyle      := bpsFrame3d;
    FBackground         := TBCBackground.Create(Self);
    FBorder             := TBCBorder.Create(Self);
    FFontEx             := TBCFont.Create(Self);
    FBevelOuter         := bvRaised;
    FBevelInner         := bvNone;
    FBevelWidth         := 1;
    ParentColor         := True;
    UseDockManager      := True;

    FBackground.OnChange := @OnChangeProperty;
    FBorder.OnChange     := @OnChangeProperty;
    FFontEx.OnChange     := @OnChangeFont;

    FBackground.Style   := bbsColor;
    FBackground.Color   := {$ifdef UseCLDefault}clDefault{$else}clBtnFace{$endif};
    FBorder.Style       := bboNone;

    FRounding           := TBCRounding.Create(Self);
    FRounding.OnChange  := @OnChangeProperty;
  finally
    EnableAutoSizing;
    EndUpdate;
    Exclude(FControlState, csCreating);
  end;
end;

destructor TCustomBCPanel.Destroy;
begin
  FBackground.Free;
  FBorder.Free;
  FFontEx.Free;
  FBGRA.Free;
  FRounding.Free;
  inherited Destroy;
end;

procedure TCustomBCPanel.UpdateControl;
begin
  Render;
  inherited UpdateControl; // invalidate
end;

end.
