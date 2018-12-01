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
unit BCTrackbarUpdown;

{$I bgracontrols.inc}

interface

uses
  {$IFDEF FPC}LCLType, LResources,{$ENDIF}
  Classes, SysUtils, Types, Forms, Controls, Graphics, Dialogs,
  {$IFNDEF FPC}BGRAGraphics, GraphType, FPImage, {$ENDIF}
  ExtCtrls, BGRABitmap, BCBaseCtrls, BCTypes;

type
  TTrackBarUpDownChangeEvent = procedure(Sender: TObject; AByUser: boolean) of object;

  { TCustomBCTrackbarUpdown }

  TCustomBCTrackbarUpdown = class(TBCCustomControl)
  protected
    FHandlingUserInput: boolean;
    FLongTimeInterval,FShortTimeInterval: integer;
    FMinValue,FMaxValue,FIncrement,FValue: integer;
    FBarExponent: single;
    FSelStart,FSelLength: integer;
    FEmptyText: boolean;
    FBarClick,FUpClick,FDownClick: boolean;

    FTimer: TTimer;
    FOnChange: TTrackBarUpDownChangeEvent;
    FBCBorder: TBCBorder;
    FBCRounding: TBCRounding;
    FBCBackground: TBCBackground;
    FBCButtonBackground,FBCButtonDownBackground: TBCBackground;
    FArrowColor: TColor;
    FHasTrackBar: boolean;

    FTextLeft: Integer;
    FBarLeft,FBarTop,FBarWidth,FBarHeight: Integer;
    FUpDownWidth: Integer;
    FUpDownLeft: Integer;
    function GetValue: integer;
    procedure SetArrowColor(AValue: TColor);
    procedure SetHasTrackBar(AValue: boolean);
    procedure SetBarExponent(AValue: single);
    procedure SetBCBackground(AValue: TBCBackground);
    procedure SetBCBorder(AValue: TBCBorder);
    procedure SetBCButtonBackground(AValue: TBCBackground);
    procedure SetBCButtonDownBackground(AValue: TBCBackground);
    procedure SetBCRounding(AValue: TBCRounding);
    procedure OnChangeProperty({%H-}Sender: TObject; {%H-}AData: PtrInt);
    procedure Timer({%H-}Sender: TObject);
    procedure RenderOnBitmap(ABitmap: TBGRABitmap);
    procedure DrawControl; override;
    procedure DoSelectAll;
    function GetText: string; virtual;
    procedure SetText(AValue: string); virtual;
    procedure EnabledChanged; override;
    procedure NotifyChange; virtual;
    procedure SetIncrement(AValue: integer);
    procedure SetMaxValue(AValue: integer);
    procedure SetMinValue(AValue: integer);
    procedure SetValue(AValue: integer);
    function ValueToBarPos(AValue: integer): integer;
    function BarPosToValue(ABarPos: integer): integer;
    procedure MouseDown(Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure UTF8KeyPress(var UTF8Key: {$IFDEF FPC}TUTF8Char{$ELSE}String{$ENDIF}); override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SelectAll;
    function RemoveSelection: boolean; //returns True if there was a selection to be removed
    procedure DelayTimer; //use after the program has been busy updating something according to the value of this component
    procedure SetFocus; override;
    destructor Destroy; override;
    property Border: TBCBorder read FBCBorder write SetBCBorder;
    property Background: TBCBackground read FBCBackground write SetBCBackground;
    property ButtonBackground: TBCBackground read FBCButtonBackground write SetBCButtonBackground;
    property ButtonDownBackground: TBCBackground read FBCButtonDownBackground write SetBCButtonDownBackground;
    property Rounding: TBCRounding read FBCRounding write SetBCRounding;
    property ArrowColor: TColor read FArrowColor write SetArrowColor;
    property HasTrackBar: boolean read FHasTrackBar write SetHasTrackBar;

    property BarExponent: single read FBarExponent write SetBarExponent;
    property Increment: integer read FIncrement write SetIncrement;
    property LongTimeInterval: integer read FLongTimeInterval write FLongTimeInterval;
    property MinValue: integer read FMinValue write SetMinValue;
    property MaxValue: integer read FMaxValue write SetMaxValue;
    property OnChange: TTrackBarUpDownChangeEvent read FOnChange write FOnChange;
    property Text: string read GetText write SetText;
    property Value: integer read GetValue write SetValue;
    property SelStart: integer read FSelStart;
    property SelLength: integer read FSelLength;
    property ShortTimeInterval: integer read FShortTimeInterval write FShortTimeInterval;
  end;

  TBCTrackbarUpdown = class(TCustomBCTrackbarUpdown)
  published
    property BarExponent;
    property Increment;
    property LongTimeInterval;
    property MinValue;
    property MaxValue;
    property OnChange;
    property Value;
    property SelStart;
    property SelLength;
    property ShortTimeInterval;
    property Background;
    property ButtonBackground;
    property ButtonDownBackground;
    property Border;
    property Rounding;
    property Font;
    property HasTrackBar;
    property ArrowColor;

    //inherited
    property Align;
    property Anchors;
    property BorderSpacing;
    property ChildSizing;
    {$IFDEF FPC} //#
    property OnGetDockCaption;
    {$ENDIF}
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
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
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

uses BGRABitmapTypes, Math, BCTools;

{$IFDEF FPC}
procedure Register;
begin
//{$I icons\bctrackbarupdown_icon.lrs}
  RegisterComponents('BGRA Controls', [TBCTrackbarUpdown]);
end;
{$ENDIF}

{ TCustomBCTrackbarUpdown }

function TCustomBCTrackbarUpdown.GetText: string;
begin
  if FEmptyText then result := '' else
    result := IntToStr(FValue);
end;

procedure TCustomBCTrackbarUpdown.SetText(AValue: string);
var errPos,tempValue: integer;
  txt: string;
  prevActualValue: integer;
begin
  if trim(AValue) = '' then
  begin
    if not FEmptyText then
    begin
      FEmptyText:= true;
      Invalidate;
    end;
    exit;
  end;
  prevActualValue:= Value;
  val(AValue,tempValue,errPos);
  if errPos = 0 then
  begin
    if tempValue > FMaxValue then tempValue := FMaxValue;
    if (FValue = tempValue) and not FEmptyText then exit;
    FValue := tempValue;
    FEmptyText:= false;
  end;
  txt := Text;
  if FSelStart > length(txt) then FSelStart := length(txt);
  if FSelStart+FSelLength > length(txt) then FSelLength:= length(txt)-FSelStart;
  Repaint;
  if Value <> prevActualValue then NotifyChange;
end;

procedure TCustomBCTrackbarUpdown.EnabledChanged;
begin
  inherited EnabledChanged;
  Invalidate;
end;

procedure TCustomBCTrackbarUpdown.NotifyChange;
begin
  if Assigned(FOnChange) then FOnChange(self, FHandlingUserInput);
end;

procedure TCustomBCTrackbarUpdown.SetIncrement(AValue: integer);
begin
  if FIncrement=AValue then Exit;
  FIncrement:=AValue;
end;

procedure TCustomBCTrackbarUpdown.SetMaxValue(AValue: integer);
begin
  if AValue < 0 then AValue := 0;
  if FMaxValue=AValue then Exit;
  FMaxValue:=AValue;
  if FMaxValue < FMinValue then FMinValue := FMaxValue;
  if AValue > FMaxValue then FMaxValue:= AValue;
  Invalidate;
end;

procedure TCustomBCTrackbarUpdown.SetMinValue(AValue: integer);
begin
  if AValue < 0 then AValue := 0;
  if FMinValue=AValue then Exit;
  FMinValue:=AValue;
  if FMinValue > FMaxValue then FMaxValue := FMinValue;
  if AValue < FMinValue then FMinValue:= AValue;
  Invalidate;
end;

procedure TCustomBCTrackbarUpdown.SetValue(AValue: integer);
begin
  if AValue < FMinValue then AValue := FMinValue;
  if AValue > FMaxValue then AValue := FMaxValue;
  if FValue=AValue then Exit;
  FValue:=AValue;
  FEmptyText:= false;
  DoSelectAll;
  Invalidate;
  NotifyChange;
end;

procedure TCustomBCTrackbarUpdown.SetArrowColor(AValue: TColor);
begin
  if FArrowColor=AValue then Exit;
  FArrowColor:=AValue;
  Invalidate;
end;

procedure TCustomBCTrackbarUpdown.SetHasTrackBar(AValue: boolean);
begin
  if FHasTrackBar=AValue then Exit;
  FHasTrackBar:=AValue;
  Invalidate;
end;

function TCustomBCTrackbarUpdown.GetValue: integer;
begin
  if FValue < FMinValue then result := FMinValue else
    result := FValue;
end;

procedure TCustomBCTrackbarUpdown.SetBarExponent(AValue: single);
begin
  if AValue <= 0 then exit;
  if FBarExponent=AValue then Exit;
  FBarExponent:=AValue;
  Invalidate;
end;

procedure TCustomBCTrackbarUpdown.SetBCBackground(AValue: TBCBackground);
begin
  if FBCBackground=AValue then Exit;
  FBCBackground.Assign(AValue);
end;

procedure TCustomBCTrackbarUpdown.SetBCBorder(AValue: TBCBorder);
begin
  if FBCBorder=AValue then Exit;
  FBCBorder.Assign(AValue);
end;

procedure TCustomBCTrackbarUpdown.SetBCButtonBackground(AValue: TBCBackground);
begin
  if FBCButtonBackground=AValue then Exit;
  FBCButtonBackground.Assign(AValue);
end;

procedure TCustomBCTrackbarUpdown.SetBCButtonDownBackground(
  AValue: TBCBackground);
begin
  if FBCButtonDownBackground=AValue then Exit;
  FBCButtonDownBackground.Assign(AValue);
end;

procedure TCustomBCTrackbarUpdown.SetBCRounding(AValue: TBCRounding);
begin
  if FBCRounding=AValue then Exit;
  FBCRounding.Assign(AValue);
end;

procedure TCustomBCTrackbarUpdown.OnChangeProperty(Sender: TObject;
  AData: PtrInt);
begin
  RenderControl;
  Invalidate;
end;

procedure TCustomBCTrackbarUpdown.Timer(Sender: TObject);
begin
  FHandlingUserInput:= true;
  if FUpClick then
  begin
    Value := Value + Increment;
  end else
  if FDownClick then
    Value := Value - Increment;
  FHandlingUserInput:= false;
  FTimer.Interval := ShortTimeInterval;
end;

procedure TCustomBCTrackbarUpdown.RenderOnBitmap(ABitmap: TBGRABitmap);
var bordercolor,fgcolor,btntext: TBGRAPixel;
  x,y,ty,barx: integer;
  s: TSize;
  midy: integer;
  midx: single;
  beforeSel,inSel,afterSel: string;
  bounds,fullBounds: TRect;
begin
  fullbounds := rect(0,0,ABitmap.Width,ABitmap.Height);
  bounds := fullBounds;
  CalculateInnerRect(Border, bounds);
  ty := bounds.bottom-bounds.top-2;
  FTextLeft := bounds.left+1+((ty+5) div 10);
  FUpDownWidth := (ty*3+3) div 5;
  FUpDownLeft := bounds.right-FUpDownWidth;

  FBarLeft := bounds.left+1;
  if FHasTrackBar then
  begin
    FBarHeight := (bounds.bottom-bounds.top+3) div 5+1;
    FBarWidth := bounds.right-FUpDownWidth-FBarHeight+1-FBarLeft;
    if (Rounding.RoundX > 1) and (Rounding.RoundY > 1) then
      FBarLeft := FBarLeft +FBarHeight+1;
  end else
  begin
    FBarWidth := 0;
    FBarHeight := 2;
  end;
  FBarTop := bounds.bottom-FBarHeight;

  midy := ABitmap.Height div 2;

  ABitmap.ClipRect := rect(fullbounds.left,fullbounds.top,FUpDownLeft+1,fullbounds.bottom);
  RenderBackgroundAndBorder(fullbounds, Background, ABitmap, Rounding, Border);

  bordercolor := ColorToBGRA(ColorToRGB(Border.Color),Border.ColorOpacity);
  ABitmap.VertLine(FUpDownLeft,bounds.top,bounds.bottom-1,bordercolor,dmDrawWithTransparency);

  if FUpClick then
  begin
    ABitmap.ClipRect := rect(FUpDownLeft+1,fullbounds.top,fullbounds.Right,midy);
    RenderBackgroundAndBorder(fullbounds, ButtonDownBackground, ABitmap, Rounding, Border);
    ABitmap.ClipRect := rect(FUpDownLeft+1,midy,fullbounds.Right,fullbounds.bottom);
    RenderBackgroundAndBorder(fullbounds, ButtonBackground, ABitmap, Rounding, Border);
  end else
  if FDownClick then
  begin
    ABitmap.ClipRect := rect(FUpDownLeft+1,fullbounds.top,fullbounds.Right,midy+1);
    RenderBackgroundAndBorder(fullbounds, ButtonBackground, ABitmap, Rounding, Border);
    ABitmap.ClipRect := rect(FUpDownLeft+1,midy+1,fullbounds.Right,fullbounds.bottom);
    RenderBackgroundAndBorder(fullbounds, ButtonDownBackground, ABitmap, Rounding, Border);
  end else
  begin
    ABitmap.ClipRect := rect(FUpDownLeft+1,fullbounds.top,fullbounds.Right,fullbounds.bottom);
    RenderBackgroundAndBorder(fullbounds, ButtonBackground, ABitmap, Rounding, Border);
  end;
  ABitmap.NoClip;
  ABitmap.HorizLine(FUpDownLeft+1,midy,bounds.right-1,bordercolor,dmDrawWithTransparency);

  ABitmap.FontQuality := fqFineAntialiasing;
  ABitmap.FontName := Font.Name;
  ABitmap.FontStyle := Font.Style;
  ABitmap.FontHeight := ((ty-FBarHeight+1)*8+4) div 9;
  fgcolor := Font.Color;

  x := FTextLeft;
  y := bounds.top+1;
  if Focused then
  begin
    if SelStart = 0 then
    begin
      beforeSel := '';
      inSel := Text;
    end else
    begin
      beforeSel := copy(Text,1,SelStart);
      inSel := copy(Text,SelStart+1,length(Text)-SelStart);
    end;
    if length(inSel)>SelLength then
    begin
      afterSel:= copy(inSel,SelLength+1,length(inSel)-SelLength);
      inSel := copy(inSel,1,SelLength);
    end else
      afterSel := '';
    ABitmap.TextOut(x,y,beforeSel,fgcolor);
    inc(x, ABitmap.TextSize(beforeSel).cx);
    if inSel = '' then ABitmap.SetVertLine(x,y,y+ABitmap.FontFullHeight-1,fgcolor)
    else
    begin
      s := ABitmap.TextSize(inSel);
      ABitmap.FillRect(x,y+1,x+s.cx,y+s.cy,ColorToRGB(clHighlight),dmSet);
      ABitmap.TextOut(x,y,inSel,ColorToRGB(clHighlightText));
      inc(x,s.cx);
    end;
    ABitmap.TextOut(x,y,afterSel,fgcolor);
  end else
  begin
    if Enabled then
      ABitmap.TextOut(x,y,Text,fgcolor)
    else
      ABitmap.TextOut(x,y,Text,BGRA(fgcolor.red,fgcolor.green,fgcolor.blue,fgcolor.alpha div 2));
  end;

  barx := ValueToBarPos(Value);
  if FHasTrackBar then
    ABitmap.FillPolyAntialias([PointF(barx,FBarTop),PointF(barx+FBarHeight,FBarTop+FBarHeight),
      PointF(barx-FBarHeight,FBarTop+FBarHeight)],fgcolor);
  midx := FUpDownLeft+(FUpDownWidth-1)/2;
  btntext := FArrowColor;
  ABitmap.FillPolyAntialias([PointF(FUpDownLeft+2,midy*4/5),PointF(midx,midy/5),PointF(FUpDownLeft+FUpDownWidth-3,midy*4/5)],btntext);
  ABitmap.FillPolyAntialias([PointF(FUpDownLeft+2,midy*6/5),PointF(midx,ABitmap.Height-midy/5),PointF(FUpDownLeft+FUpDownWidth-3,midy*6/5)],btntext);
end;

function TCustomBCTrackbarUpdown.ValueToBarPos(AValue: integer): integer;
var t: single;
begin
  if FMaxValue>FMinValue then
  begin
    t := (AValue-FMinValue)/(FMaxValue-FMinValue);
    if t < 0 then t := 0;
    if t > 1 then t := 1;
    result := FBarLeft+round(power(t,1/FBarExponent)*(FBarWidth-1))
  end
  else
    result := FBarLeft;
end;

function TCustomBCTrackbarUpdown.BarPosToValue(ABarPos: integer): integer;
var t: single;
begin
  if FBarWidth > FBarLeft then
  begin
    t := (ABarPos-FBarLeft)/(FBarWidth-1);
    if t < 0 then t := 0;
    if t > 1 then t := 1;
    result := round(power(t,FBarExponent)*(FMaxValue-FMinValue))+FMinValue
  end
  else
    result := FMinValue;
end;

procedure TCustomBCTrackbarUpdown.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FHandlingUserInput:= true;
    if X >= FUpDownLeft then
    begin
      if Y > Height div 2 then
      begin
        FDownClick:= true;
        Value := Value-Increment;
        Invalidate;
        FTimer.Interval := LongTimeInterval;
        FTimer.Enabled:= true;
      end else
      if Y < Height div 2 then
      begin
        FUpClick:= true;
        Value := Value+Increment;
        Invalidate;
        FTimer.Interval := LongTimeInterval;
        FTimer.Enabled:= true;
      end;
    end else
    if (Y >= Height-FBarHeight-1) and (FBarWidth>1) then
    begin
      FBarClick:= true;
      Value := BarPosToValue(X);
      Repaint;
    end;
    FHandlingUserInput:= false;
  end;
  if not Focused then
  begin
    SetFocus;
    SelectAll;
  end;
end;

procedure TCustomBCTrackbarUpdown.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FBarClick and (FBarWidth>1) then
  begin
    FHandlingUserInput:= true;
    Value := BarPosToValue(X);
    FHandlingUserInput:= false;
  end;
end;

procedure TCustomBCTrackbarUpdown.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    if FBarClick then FBarClick:= false else
    if FUpClick then
    begin
      FUpClick:= false;
      Invalidate;
      FTimer.Enabled:= false;
    end else
    if FDownClick then
    begin
      FDownClick:= false;
      Invalidate;
      FTimer.Enabled:= false;
    end;
  end;
end;

procedure TCustomBCTrackbarUpdown.UTF8KeyPress(var UTF8Key: {$IFDEF FPC}TUTF8Char{$ELSE}String{$ENDIF});
var tempText: string;
begin
  FHandlingUserInput:= true;
  if UTF8Key = #8 then
  begin
    if not RemoveSelection and (SelStart > 0) then
    begin
      tempText := Text;
      Dec(FSelStart);
      Delete(tempText,SelStart+1,1);
      Text := tempText;
      Invalidate;
    end;
    UTF8Key:= #0;
  end else
  if (length(UTF8Key)=1) and (UTF8Key[1] in['0'..'9']) then
  begin
    RemoveSelection;
    tempText := Text;
    Insert(UTF8Key,tempText,SelStart+1);
    Text := tempText;
    if FSelStart < length(Text) then inc(FSelStart);
    Invalidate;
    UTF8Key:= #0;
  end;
  FHandlingUserInput:= false;
end;

procedure TCustomBCTrackbarUpdown.DoEnter;
begin
  inherited DoEnter;
  Invalidate;
end;

procedure TCustomBCTrackbarUpdown.DoExit;
begin
  inherited DoExit;
  FEmptyText:= false;
  if FValue < FMinValue then FValue := FMinValue;
  Invalidate;
end;

procedure TCustomBCTrackbarUpdown.DrawControl;
var bmp: TBGRABitmap;
begin
  bmp := TBGRABitmap.Create(Width,Height);
  RenderOnBitmap(bmp);
  bmp.Draw(Canvas,0,0,False);
  bmp.Free;
end;

constructor TCustomBCTrackbarUpdown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  FMinValue:= 0;
  FMaxValue := 100;
  FValue := 50;
  FIncrement := 1;
  FBarExponent:= 1;
  FTimer := TTimer.Create(self);
  FTimer.Enabled := false;
  FTimer.OnTimer:=Timer;
  FLongTimeInterval:= 400;
  FShortTimeInterval:= 100;
  FHasTrackBar:= true;
  FBCBorder := TBCBorder.Create(self);
  FBCBorder.Color := clWindowText;
  FBCBorder.Width := 1;
  FBCBorder.Style := bboSolid;
  FBCBorder.OnChange := OnChangeProperty;
  FBCRounding := TBCRounding.Create(self);
  FBCRounding.RoundX := 1;
  FBCRounding.RoundY := 1;
  FBCRounding.OnChange := OnChangeProperty;
  FBCBackground := TBCBackground.Create(self);
  FBCBackground.Style := bbsColor;
  FBCBackground.Color := clWindow;
  FBCBackground.OnChange := OnChangeProperty;
  FBCButtonBackground := TBCBackground.Create(self);
  FBCButtonBackground.Style := bbsGradient;
  FBCButtonBackground.Gradient1EndPercent := 50;
  FBCButtonBackground.Gradient1.Point1YPercent := -50;
  FBCButtonBackground.Gradient1.Point2YPercent := 50;
  FBCButtonBackground.Gradient1.StartColor := clBtnShadow;
  FBCButtonBackground.Gradient1.EndColor := clBtnFace;
  FBCButtonBackground.Gradient2.Point1YPercent := 50;
  FBCButtonBackground.Gradient2.Point2YPercent := 150;
  FBCButtonBackground.Gradient2.StartColor := clBtnFace;
  FBCButtonBackground.Gradient2.EndColor := clBtnShadow;
  FBCButtonBackground.OnChange := OnChangeProperty;
  FBCButtonDownBackground := TBCBackground.Create(self);
  FBCButtonDownBackground.Style := bbsColor;
  FBCButtonDownBackground.Color := clBtnShadow;
  FBCButtonDownBackground.OnChange := OnChangeProperty;
  FArrowColor:= clBtnText;
  Font.Color := clWindowText;
  Font.Name := 'Arial';

  DoSelectAll;
  TabStop := true;
end;

procedure TCustomBCTrackbarUpdown.DoSelectAll;
begin
  FSelStart := 0;
  FSelLength := length(Text);
end;

procedure TCustomBCTrackbarUpdown.SelectAll;
begin
  DoSelectAll;
  Invalidate;
end;

function TCustomBCTrackbarUpdown.RemoveSelection: boolean;
var
  tempText: string;
  len:integer;
begin
  if SelLength > 0 then
  begin
    tempText := Text;
    len := FSelLength;
    FSelLength := 0;
    Delete(tempText,SelStart+1,len);
    Text := tempText;
    Invalidate;
    result := true
  end else
    result := false;
end;

procedure TCustomBCTrackbarUpdown.DelayTimer;
begin
  if FTimer.Enabled then
  begin
    FTimer.Enabled:= false;
    FTimer.Enabled:= true;
  end;
end;

procedure TCustomBCTrackbarUpdown.SetFocus;
begin
  try
    inherited SetFocus;
  except
    //in some cases, it is impossible to set the focus
    //but that's not a reason to crash the program
  end;
end;

destructor TCustomBCTrackbarUpdown.Destroy;
begin
  FreeAndNil(FTimer);
  FreeAndNil(FBCBackground);
  FreeAndNil(FBCButtonBackground);
  FreeAndNil(FBCButtonDownBackground);
  FreeAndNil(FBCBorder);
  FreeAndNil(FBCRounding);
  inherited Destroy;
end;

end.
