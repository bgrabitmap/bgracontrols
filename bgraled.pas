
{------------------------------------------------------------------------------

  Forked from Miguel A. Risco Castillo TBGRALED v0.2
  http://ue.accesus.com/uecontrols

  Copyright Miguel A. Risco Castillo and СМ630.
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
  the specific language governing rights and limitations under the License.

------------------------------------------------------------------------------}

{-----------------------------------------------------------------------------
List of features and bugfixes added to TuEKnob v0.6.9
- „Bright“ property renamed to „On“ in order to provide consistency with other boolean elements. „Bright“ property is kept for legacy reasons.
- Background colour for OFF state of the LED is made customizable.
- Added blinking mode. Aftef blinking mode is switched off, the state opf the LED before blinking started is retained.
-----------------------------------------------------------------------------}

unit BGRALED;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLIntf, LCLType, Types, BGRABitmap, BGRABitmapTypes, ExtCtrls;

type

  { TBGRALED }
  TLedType = (ledRound, ledSquare);

  TBGRALED = class(TGraphicControl)
  private
    FColor: TColor;
    FBackgroung: TColor;
    FActive: Boolean;
    FOnChange: TNotifyEvent;
    FChanging:Boolean;
    FOn : Boolean;
    FReflection : Boolean;
    FLedType : TLedType;
    FBlinking: Boolean;
    FBlinkInterval: QWord;
    BlinkTimer: TTimer;
    OnStatus: Boolean;
    procedure DrawLED;
    procedure DrawLedRound(const r: integer; const LColor: TColor; const LBackground: TColor);
    procedure DrawLedSquare(const r: integer; const LColor: TColor; const LBackground: TColor);
    procedure SetActive(AValue:Boolean);
  protected
    class procedure WSRegisterClass; override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure Resize; override;
    procedure SetBackground(AValue: TColor); virtual;
    procedure SetColor(AValue: TColor); virtual; //override; //„override“ allows calling an internal procedure, which should not be called.
    procedure SetOn(Avalue:Boolean); virtual;
    procedure SetReflection(Avalue:Boolean); virtual;
    procedure SetLedType(AValue:TLedType); virtual;
    procedure SetBlinking(AValue: Boolean); virtual;
    procedure BlinkTimerTimer(Sender: TObject); virtual;
    procedure DoChange; virtual;
  public
    Bitmap: TBGRABitmap;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReDraw;
  published
    property Active: boolean read FActive write SetActive;
    property LedType: TLedType read FLedType write SetLedType;
    property On : boolean read FOn write SetOn;
    property Reflection: boolean read FReflection write SetReflection;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Blinking: Boolean read FBlinking write SetBlinking;
    property BlinkInterval: QWord read FBlinkInterval write FBlinkInterval;
    property Color: TColor read FColor write SetColor default clRed;
    property Backgroung: TColor read FBackgroung write SetBackground default clMaroon;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
  //  property ParentColor;   //TODO: What is this for?
    property ParentColor: Boolean default False;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeBounds;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;
function Darker(Color:TColor; Percent:Byte):TBGRAPixel;

implementation

constructor TBGRALED.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable, csCaptureMouse, csClickEvents, csDoubleClicks];
  Bitmap:=TBGRABitmap.Create(0,0);
  with GetControlClassDefaultSize do SetInitialBounds(0, 0, CX, CY);
  FChanging:=False;
  FActive:=true;
  FOn:=true;
  OnStatus:=FOn;
  FReflection:=true;
  FColor:=clRed;
  FBackgroung:=clMaroon;
  FLedType:=ledRound;
  FBlinking:=False;
  FBlinkInterval:=300;
  BlinkTimer:=TTimer.Create(Nil);
  BlinkTimer.Enabled:=False;
  BlinkTimer.Interval:=FBlinkInterval;
  BlinkTimer.OnTimer:=@BlinkTimerTimer;
end;

destructor TBGRALED.Destroy;
begin
  if Assigned(Bitmap) then
  begin
    Bitmap.Free;
    Bitmap := nil;
    BlinkTimer.Free;
  end;
  inherited Destroy;
end;

procedure TBGRALED.ReDraw;
begin
  Paint;
end;

procedure TBGRALED.Paint;

  procedure DrawFrame;
  begin
    with inherited Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Style := psDash;
      MoveTo(0, 0);
      LineTo(Self.Width-1, 0);
      LineTo(Self.Width-1, Self.Height-1);
      LineTo(0, Self.Height-1);
      LineTo(0, 0);
    end;
  end;

begin
  if csDesigning in ComponentState then DrawFrame;
  if assigned(Bitmap) then
  begin
    Bitmap.Draw(inherited Canvas,0,0,false);
  end;
end;

procedure TBGRALED.Loaded;
begin
  inherited Loaded;
end;

procedure TBGRALED.Resize;
begin
  inherited Resize;
  DrawLED;
end;

procedure TBGRALED.SetColor(AValue: TColor);
begin
  if FColor = AValue then exit;
  FColor:=AValue;
  DrawLED;
  inherited SetColor(AValue);
end;

procedure TBGRALED.SetBackground(AValue: TColor);
begin
  if FBackgroung = AValue then exit;
  FBackgroung := AValue;
  DrawLED;
//  inherited SetBackground(AValue);
end;

procedure TBGRALED.SetOn(Avalue: Boolean);
begin
  if FOn = AValue then exit;
  FOn := AValue;
  OnStatus:= AValue;
  DrawLED;
end;

procedure TBGRALED.SetReflection(Avalue: Boolean);
begin
  if FReflection = AValue then exit;
  FReflection := AValue;
  DrawLED;
end;

procedure TBGRALED.SetLedType(AValue: TLedType);
begin
  if FLedType = AValue then exit;
  FLedType := AValue;
  DrawLED;
end;

procedure TBGRALED.SetBlinking (Avalue: Boolean);
begin
  if FBlinking = AValue then exit;
  FBlinking := AValue;
  BlinkTimer.Enabled:=FBlinking;
  OnStatus:=fOn;
  DrawLED;
end;

procedure TBGRALED.DrawLED;
var r:integer;
begin
  if (csLoading in ComponentState) or FChanging then exit;

  FChanging := True;
  Bitmap.SetSize(width,height);
  Bitmap.Fill(BGRAPixelTransparent);

  if Width < Height then r:=Width else r:=Height;
  r:=r div 10;

  Case FLedType of
    ledSquare : DrawLedSquare(r+2, FColor, FBackgroung);
  else
    DrawLedRound(r+3, FColor, FBackgroung)
  end;

  FChanging := False;
  Invalidate;
  DoChange;
end;

procedure TBGRALED.DrawLedRound(const r: integer; const LColor: TColor; const LBackground: TColor);
var
  mask: TBGRABitmap;
  layer: TBGRABitmap;
  drawcol: TColor;
begin
  if OnStatus then drawcol:= LColor else drawcol:= LBackground;
  //Bright
  if FActive and OnStatus then
  begin
    layer:=TBGRABitmap.Create(Width, Height);
    layer.GradientFill(0,0,layer.Width,layer.Height,
                       ColorToBGRA(ColortoRGB(LColor),240),ColorToBGRA(ColortoRGB(drawcol),0),
                       gtRadial,PointF(layer.Width/2,layer.Height/2),PointF(0,layer.Height/2),
                       dmSet);
    Bitmap.PutImage(0,0,layer,dmDrawWithTransparency);
    layer.free;
  end;

  // Solid Led
  if FActive then
  begin
    layer:=TBGRABitmap.Create(Width-2*r, Height-2*r);
    layer.GradientFill(0,0,layer.Width,layer.Height,
                       ColorToBGRA(ColortoRGB(drawcol)),BGRA(0,0,0),
                       gtRadial,PointF(layer.Width/2,layer.Height*8/15),PointF(layer.Width*1.5,layer.Height*1.5),
                       dmSet);
    mask := TBGRABitmap.Create(layer.Width,layer.Height,BGRABlack);
    mask.FillEllipseAntialias((layer.Width-1)/2,(layer.Height-1)/2,layer.Width/2,layer.Height/2,BGRAWhite);
    layer.ApplyMask(mask);
    mask.Free;
    Bitmap.PutImage(r,r,layer,dmDrawWithTransparency);
    layer.free;
  end else Bitmap.FillEllipseAntialias((Width-1)/2,(Height-1)/2,Width/2-r,Height/2-r, Darker(drawcol,80));

  //Reflexion
  if FReflection then
  begin
    layer:=TBGRABitmap.Create((Width-1)-2*r, 5*(Height-2*r) div 8);
    layer.GradientFill(0,0,layer.Width,layer.Height,
                       BGRA(255,255,255,128),BGRA(255,255,255,0),
                       gtLinear,PointF(layer.Width/2,0),PointF(layer.Width/2,layer.Height*6/10),
                       dmSet);
    mask := TBGRABitmap.Create(layer.Width,layer.Height,BGRABlack);
    mask.FillEllipseAntialias(layer.Width/2,layer.Height/2,(layer.Width/2)*(4/5),(layer.Height/2)*(9/10),BGRAWhite);
    layer.ApplyMask(mask);
    mask.Free;
    Bitmap.PutImage(r,r,layer,dmDrawWithTransparency);
    layer.free;
  end;
end;

procedure TBGRALED.DrawLedSquare(const r: integer; const LColor: TColor; const LBackground: Tcolor);
var
  mask: TBGRABitmap;
  layer: TBGRABitmap;
  drawcol: TColor;
begin
  if OnStatus then drawcol:= LColor else drawcol:= LBackground;
  //Bright
  if FActive and OnStatus then
  begin
    layer:=TBGRABitmap.Create(Width, Height);
    layer.GradientFill(0,0,layer.Width,layer.Height,
                ColorToBGRA(ColortoRGB(LColor),255),ColorToBGRA(ColortoRGB(drawcol),0),
                gtRadial,PointF(layer.Width/2,layer.Height/2),PointF(0,3*layer.Height/4),
                dmSet);
    Bitmap.PutImage(0,0,layer,dmDrawWithTransparency);
    layer.free;
  end;

  // Solid Led
  if FActive then
  begin
    layer:=TBGRABitmap.Create(Width-2*r, Height-2*r);
    layer.GradientFill(0,0,layer.Width,layer.Height,
                       ColorToBGRA(ColortoRGB(drawcol)),BGRA(0,0,0),
                       gtRadial,PointF(layer.Width/2,layer.Height/2),PointF(layer.Width*1.5,layer.Height*1.5),
                       dmSet);
    mask := TBGRABitmap.Create(layer.Width,layer.Height,BGRABlack);
    mask.FillRoundRectAntialias(0,0,layer.Width,layer.Height,r,r,BGRAWhite);
    layer.ApplyMask(mask);
    mask.Free;
    Bitmap.PutImage(r,r,layer,dmDrawWithTransparency);
    layer.free;
  end else Bitmap.FillRoundRectAntialias(r,r,Width-r,Height-r,r,r, Darker(LColor,80));

  //Reflexion
  if FReflection then
  begin
    layer:=TBGRABitmap.Create((Width-1)-2*r, 5*(Height-2*r) div 8);
    layer.GradientFill(0,0,layer.Width,layer.Height,
                       BGRA(255,255,255,160),BGRA(255,255,255,0),
                       gtLinear,PointF(layer.Width/2,0),PointF(layer.Width/2,layer.Height*6/10),
                       dmSet);
    mask := TBGRABitmap.Create(layer.Width,layer.Height,BGRABlack);
    mask.FillRoundRectAntialias(layer.Width*(1/20),layer.Height*(1/20),layer.Width*(19/20),layer.Height*(19/20),r,r,BGRAWhite);
    layer.ApplyMask(mask);
    mask.Free;
    Bitmap.PutImage(r,r,layer,dmDrawWithTransparency);
    layer.free;
  end;

end;

procedure TBGRALED.SetActive(AValue: Boolean);
begin
  if AValue <> FActive then
  begin
    FActive := AValue;
    DrawLED;
  end;
end;

class function TBGRALED.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 24;
  Result.CY := 24;
end;

procedure TBGRALED.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

class procedure TBGRALED.WSRegisterClass;
begin
  inherited WSRegisterClass;
end;

function Darker(Color:TColor; Percent:Byte):TBGRAPixel;
begin
  Result:=ColorToBGRA(ColorToRGB(Color));
  With Result do
  begin
    red:=red-muldiv(red,Percent,100);  //Percent% closer to black
    green:=green-muldiv(green,Percent,100);
    blue:=blue-muldiv(blue,Percent,100);
  end;
end;

procedure Register;
begin
  {$I icons\bgraled_icon.lrs}
  RegisterComponents('BGRA Controls', [TBGRALED]);
end;

procedure TBGRALED.BlinkTimerTimer(Sender: TObject);
begin
  //FOn := not FOn;
  OnStatus:= not OnStatus;
  DrawLED;
end;

end.


