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
unit BGRAGraphicControl;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Forms, Controls, Graphics, Dialogs, Types,
  {$IFNDEF FPC}BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCBaseCtrls, ExtCtrls, BGRABitmap, BCTypes;

type

  { TCustomBGRAGraphicControl }

  TCustomBGRAGraphicControl = class(TBGRAGraphicCtrl)
  private
    { Private declarations }
    FOnRedraw:     TBGRARedrawEvent;
    FBevelInner, FBevelOuter: TPanelBevel;
    FBevelWidth:   TBevelWidth;
    FBorderWidth:  TBorderWidth;
    FAlignment:    TAlignment;
    FColorOpacity: byte;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetBevelInner(const AValue: TPanelBevel);
    procedure SetBevelOuter(const AValue: TPanelBevel);
    procedure SetBevelWidth(const AValue: TBevelWidth);
    procedure SetBorderWidth(const AValue: TBorderWidth);
    procedure SetColorOpacity(const AValue: byte);
  protected
    { Protected declarations }
    FBGRA:         TBGRABitmap;
    procedure Paint; override;
    procedure Resize; override;
    procedure BGRASetSize(AWidth, AHeight: integer); virtual;
    procedure RedrawBitmapContent; virtual;
    procedure SetColor(Value: TColor); override;
    procedure SetEnabled(Value: boolean); override;
    procedure TextChanged; override;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure RedrawBitmap;
    procedure DiscardBitmap;
    destructor Destroy; override;
    property OnRedraw: TBGRARedrawEvent Read FOnRedraw Write FOnRedraw;
    property Bitmap: TBGRABitmap Read FBGRA;
    property BorderWidth: TBorderWidth Read FBorderWidth Write SetBorderWidth default 0;
    property BevelInner: TPanelBevel Read FBevelInner Write SetBevelInner default bvNone;
    property BevelOuter: TPanelBevel
      Read FBevelOuter Write SetBevelOuter default bvRaised;
    property BevelWidth: TBevelWidth Read FBevelWidth Write SetBevelWidth default 1;
    property ColorOpacity: byte Read FColorOpacity Write SetColorOpacity;
    property Alignment: TAlignment Read FAlignment Write SetAlignment;
  end;

  TBGRAGraphicControl = class(TCustomBGRAGraphicControl)
  published
    { Published declarations }
    property Align;
    property Anchors;
    property OnRedraw;
    property Bitmap;
    property BorderWidth;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property Color;
    property ColorOpacity;
    property Alignment;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF FPC}
    property OnPaint;
    {$ENDIF}
    property Caption;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

uses BGRABitmapTypes;

{$IFDEF FPC}
procedure Register;
begin
  //{$I icons\bgragraphiccontrol_icon.lrs}
  RegisterComponents('BGRA Controls', [TBGRAGraphicControl]);
end;
{$ENDIF}

procedure TCustomBGRAGraphicControl.SetAlignment(const Value: TAlignment);
begin
  if FAlignment = Value then
    exit;
  FAlignment := Value;
  DiscardBitmap;
end;

procedure TCustomBGRAGraphicControl.SetBevelInner(const AValue: TPanelBevel);
begin
  if FBevelInner = AValue then
    exit;
  FBevelInner := AValue;
  DiscardBitmap;
end;

procedure TCustomBGRAGraphicControl.SetBevelOuter(const AValue: TPanelBevel);
begin
  if FBevelOuter = AValue then
    exit;
  FBevelOuter := AValue;
  DiscardBitmap;
end;

procedure TCustomBGRAGraphicControl.SetBevelWidth(const AValue: TBevelWidth);
begin
  if FBevelWidth = AValue then
    exit;
  FBevelWidth := AValue;
  DiscardBitmap;
end;

procedure TCustomBGRAGraphicControl.SetBorderWidth(const AValue: TBorderWidth);
begin
  if FBorderWidth = AValue then
    exit;
  FBorderWidth := AValue;
  DiscardBitmap;
end;

procedure TCustomBGRAGraphicControl.SetColorOpacity(const AValue: byte);
begin
  if FColorOpacity = AValue then
    exit;
  FColorOpacity := AValue;
  DiscardBitmap;
end;

procedure TCustomBGRAGraphicControl.Paint;
begin
  BGRASetSize(Width, Height);
  inherited Paint;
  FBGRA.Draw(Canvas, 0, 0, False);
end;

procedure TCustomBGRAGraphicControl.Resize;
begin
  inherited Resize;
  DiscardBitmap;
end;

procedure TCustomBGRAGraphicControl.BGRASetSize(AWidth, AHeight: integer);
begin
  if (FBGRA <> nil) and (AWidth <> FBGRA.Width) and (AHeight <> FBGRA.Height) then
  begin
    FBGRA.SetSize(AWidth, AHeight);
    RedrawBitmapContent;
  end;
end;

procedure TCustomBGRAGraphicControl.RedrawBitmapContent;
var
  ARect: TRect;
  TS: TTextStyle;
begin
  if (FBGRA <> nil) and (FBGRA.NbPixels <> 0) then
  begin
    FBGRA.Fill(ColorToBGRA(ColorToRGB(Color), FColorOpacity));

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
      {$IFDEF FPC}//#
      TS := Canvas.TextStyle;
      {$ENDIF}
      TS.Alignment := Alignment;
      TS.Layout := tlCenter;
      TS.Opaque := False;
      TS.Clipping := False;
      {$IFDEF FPC}//#
      TS.SystemFont := Canvas.Font.IsDefault;
      {$ENDIF}

      FBGRA.CanvasBGRA.Font.Color := Color xor $FFFFFF;
      FBGRA.CanvasBGRA.Font.Opacity := 255;

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

procedure TCustomBGRAGraphicControl.SetColor(Value: TColor);
begin
  if Value <> Color then
    DiscardBitmap;

  inherited SetColor(Value);
end;

procedure TCustomBGRAGraphicControl.SetEnabled(Value: boolean);
begin
  if Value <> Enabled then
    DiscardBitmap;
  inherited SetEnabled(Value);
end;

procedure TCustomBGRAGraphicControl.TextChanged;
begin
  DiscardBitmap;
end;

constructor TCustomBGRAGraphicControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  FBGRA := TBGRABitmap.Create;
  FBevelWidth := 1;
  FAlignment := taCenter;
  Color := clWhite;
  FColorOpacity := 128;
  FBevelOuter := bvRaised;
  FBevelInner := bvNone;
end;

procedure TCustomBGRAGraphicControl.RedrawBitmap;
begin
  RedrawBitmapContent;
  Repaint;
end;

procedure TCustomBGRAGraphicControl.DiscardBitmap;
begin
  if (FBGRA <> nil) and (FBGRA.NbPixels <> 0) then
  begin
    FBGRA.SetSize(0, 0);
    Invalidate;
  end;
end;

destructor TCustomBGRAGraphicControl.Destroy;
begin
  FBGRA.Free;
  inherited Destroy;
end;

end.
