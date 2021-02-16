// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Created by BGRA Controls Team
  Dibo, Circular, lainz (007) and contributors.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html
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
    FDiscardedRect: TRect;
    FBevelInner, FBevelOuter: TPanelBevel;
    FBevelWidth:  TBevelWidth;
    FBorderWidth: TBorderWidth;
    FAlignment:   TAlignment;
    FBitmapAutoScale: boolean;
    function GetBitmapHeight: integer;
    function GetBitmapScale: double;
    function GetBitmapWidth: integer;
    function GetVSCaption: string;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetBevelInner(const AValue: TPanelBevel);
    procedure SetBevelOuter(const AValue: TPanelBevel);
    procedure SetBevelWidth(const AValue: TBevelWidth);
    procedure SetBitmapAutoScale(AValue: boolean);
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
    procedure DiscardBitmap; overload;
    procedure DiscardBitmap(ARect: TRect); overload;
    destructor Destroy; override;
  public
    property OnRedraw: TBGRARedrawEvent Read FOnRedraw Write FOnRedraw;
    property Bitmap: TBGRABitmap Read FBGRA;
    property BitmapAutoScale: boolean read FBitmapAutoScale write SetBitmapAutoScale default true;
    property BitmapScale: double read GetBitmapScale;
    property BitmapWidth: integer read GetBitmapWidth;
    property BitmapHeight: integer read GetBitmapHeight;
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
    property BitmapAutoScale;
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

uses BGRABitmapTypes, math, LazVersion;

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

function TCustomBGRAVirtualScreen.GetBitmapScale: double;
begin
  {$if laz_fullversion >= 2000000}
  if not FBitmapAutoScale then
    result := GetCanvasScaleFactor
  else
    result := 1;
  {$else}
  result := 1;
  {$endif}
end;

function TCustomBGRAVirtualScreen.GetBitmapHeight: integer;
begin
  result := round(ClientHeight * BitmapScale);
end;

function TCustomBGRAVirtualScreen.GetBitmapWidth: integer;
begin
  result := round(ClientWidth * BitmapScale);
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

procedure TCustomBGRAVirtualScreen.SetBitmapAutoScale(AValue: boolean);
begin
  if FBitmapAutoScale=AValue then Exit;
  DiscardBitmap; //before to get correct invalidate bounds
  FBitmapAutoScale:=AValue;
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
  BGRASetSize(BitmapWidth, BitmapHeight);
  if FBGRA <> nil then
  begin
    if not FDiscardedRect.IsEmpty then
    begin
      FBGRA.ClipRect := FDiscardedRect;
      FDiscardedRect := EmptyRect;
      RedrawBitmapContent;
      FBGRA.NoClip;
    end;
    FBGRA.Draw(Canvas, rect(0, 0, ClientWidth, ClientHeight));
  end;
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
    FDiscardedRect := EmptyRect;
  end;
end;

procedure TCustomBGRAVirtualScreen.RedrawBitmapContent;
var
  ARect: TRect;
  TS: TTextStyle;
  scale: Double;
begin
  if (FBGRA <> nil) and (FBGRA.NbPixels <> 0) then
  begin
    FBGRA.FillRect(FBGRA.ClipRect, ColorToRGB(Color));

    scale := BitmapScale;
    ARect := GetClientRect;
    ARect.Left := round(ARect.Left*scale);
    ARect.Top := round(ARect.Top*scale);
    ARect.Right := round(ARect.Right*scale);
    ARect.Bottom := round(ARect.Bottom*scale);

    // if BevelOuter is set then draw a frame with BevelWidth
    if BevelOuter <> bvNone then
      FBGRA.CanvasBGRA.Frame3d(ARect, round(BevelWidth*scale), BevelOuter,
        BGRA(255, 255, 255, 200), BGRA(0, 0, 0, 160)); // Note: Frame3D inflates ARect

    InflateRect(ARect, -round(BorderWidth*scale), -round(BorderWidth*scale));

    // if BevelInner is set then skip the BorderWidth and draw a frame with BevelWidth
    if BevelInner <> bvNone then
      FBGRA.CanvasBGRA.Frame3d(ARect, round(BevelWidth*scale), BevelInner,
        BGRA(255, 255, 255, 160), BGRA(0, 0, 0, 160)); // Note: Frame3D inflates ARect

    if Caption <> '' then
    begin
      FBGRA.CanvasBGRA.Font.Assign(Canvas.Font);
      FBGRA.CanvasBGRA.Font.Height:= round(FBGRA.CanvasBGRA.Font.Height*scale);
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
  FBitmapAutoScale := true;
  FBevelWidth := 1;
  FAlignment := taLeftJustify;
  FDiscardedRect := EmptyRect;
  Color := clWhite;
end;

procedure TCustomBGRAVirtualScreen.RedrawBitmap;
begin
  RedrawBitmapContent;
  FDiscardedRect := EmptyRect;
  Repaint;
end;

procedure TCustomBGRAVirtualScreen.RedrawBitmap(ARect: TRect);
var
  All, displayRect: TRect;
  scale: Double;
begin
  if Assigned(FBGRA) then
  begin
    All := Rect(0,0,FBGRA.Width,FBGRA.Height);
    ARect.Intersect(All);
    if not FDiscardedRect.IsEmpty then
    begin
      if ARect.IsEmpty then
        ARect := FDiscardedRect
      else
        ARect.Union(FDiscardedRect);
      FDiscardedRect := EmptyRect;
    end;
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
      scale := BitmapScale;
      displayRect := rect(round(ARect.Left/scale), round(ARect.Top/scale),
        round(ARect.Right/scale), round(ARect.Bottom/scale));
      {$IFDEF LINUX}
      FBGRA.DrawPart(ARect, Canvas, displayRect, True);
      {$ELSE}
      InvalidateRect(Handle, @displayRect, False);
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
  gAll: TRect;

  procedure IncludeRect(ARect: TRect);
  var
    gR: TRect;
    y,x: LongInt;
  begin
    with ARect do
      gR := rect(max(Left,0) shr cellShift, max(Top,0) shr cellShift,
                 (max(Right,0)+cellSize-1) shr cellShift,
                 (max(Bottom,0)+cellSize-1) shr cellShift);
    gR.Intersect(gAll);
    if gR.IsEmpty then exit;

    for y := gR.Top to gR.Bottom-1 do
      for x := gR.Left to gR.Right-1 do
        grid[y,x] := true;
  end;

var
  gW,gH, i,gCount: integer;
  gR: TRect;
  y,x: LongInt;
  expand: boolean;


begin
  if not Assigned(FBGRA) then exit;
  gW := (Bitmap.Width+cellSize-1) shr cellShift;
  gH := (Bitmap.Height+cellSize-1) shr cellShift;
  gAll := rect(0,0,gW,gH);

  //determine which cells of the grid to redraw
  setlength(grid,gH,gW);
  for i := 0 to high(ARectArray) do
    IncludeRect(ARectArray[i]);
  if not FDiscardedRect.IsEmpty then
  begin
    IncludeRect(FDiscardedRect);
    FDiscardedRect := EmptyRect;
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
  if FBGRA <> nil then
    DiscardBitmap(rect(0,0,FBGRA.Width,FBGRA.Height));
end;

procedure TCustomBGRAVirtualScreen.DiscardBitmap(ARect: TRect);
var
  scale: Double;
  displayRect: TRect;
begin
  ARect.Intersect(rect(0,0,FBGRA.Width,FBGRA.Height));
  if ARect.IsEmpty then exit;
  if FBGRA <> nil then
  begin
    if FDiscardedRect.IsEmpty then
      FDiscardedRect := ARect
    else
      FDiscardedRect.Union(ARect);
    scale := BitmapScale;
    displayRect := rect(round(ARect.Left/scale), round(ARect.Top/scale),
      round(ARect.Right/scale), round(ARect.Bottom/scale));
    InvalidateRect(self.Handle, @displayRect, false);
  end;
end;

destructor TCustomBGRAVirtualScreen.Destroy;
begin
  FBGRA.Free;
  inherited Destroy;
end;

end.
