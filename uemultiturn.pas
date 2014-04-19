{-----------------------------------------------------------------------------
Miguel A. Risco Castillo TuEMultiTurn v0.2.2
http://ue.accesus.com/uecontrols

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.
-----------------------------------------------------------------------------}

unit uEMultiTurn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLIntf, LCLType, LCLProc, Types,
  BGRABitmap, BGRABitmapTypes, uERotImage;

type

  TuEAngle = Integer;   //3600 = 360.0 deg


  { TCustomuEMultiTurn }

  TCustomuEMultiTurn = class(TCustomuERotImage)
  private
  protected
    FLTicks: integer;
    FLTicksColor: TColor;
    FLTicksSize: integer;
    FSTicks: integer;
    FSTicksColor: TColor;
    FSTicksSize: integer;
    FTicksMargin: integer;
    FShowValues: Boolean;
    FValuesFont: TFont;
    FValuesMargin: integer;
    FOnChange: TNotifyEvent;
    FPosition: Real;
    FTransparent:Boolean;
    FDefKnobRadius: integer;
    procedure SetDefKnobRadius(AValue: integer);
    procedure SetLTicksColor(const AValue: TColor); virtual;
    procedure SetPosition(const AValue: Real); virtual;
    procedure SetLargeTicks(const AValue: integer); virtual;
    procedure SetLTicksSize(const AValue: integer); virtual;
    procedure SetSTicks(const AValue: integer); virtual;
    procedure SetSTicksColor(const AValue: TColor); virtual;
    procedure SetSTicksSize(const AValue: integer); virtual;
    procedure SetTicksMargin(const AValue: integer); virtual;
    procedure SetShowValues(const AValue: Boolean); virtual;
    procedure SetTransparent(const AValue: Boolean); virtual;
    procedure SetValueMargin(const AValue: integer); virtual;
    procedure SetValuesFont(const AValue: TFont); virtual;
    class procedure WSRegisterClass; override;
    procedure CalculatePreferredSize(var PreferredWidth,
                                     PreferredHeight: integer;
                                     WithThemeSpace: Boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure SetColor(AValue: TColor); override;
    procedure FontChanged(Sender: TObject); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DefaultPicture; virtual;
    procedure ForcePosition(const AValue: Real); virtual;
    procedure DrawScales(LBitmap:TBGRABitmap); virtual;
    procedure UpdateScales; virtual;
    procedure DoOnChange; virtual;
    procedure DoOnResize; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function GetCenter: TPoint;
    function PointToAngle(APoint, ACenter: TPoint): TuEAngle;
    function AngleToPos(AnAngle: TuEAngle): Real;
    function PosToAngle(Pos: Real): TuEAngle;
    function Delta(X, Y: Integer): TuEAngle;
    property Position: Real read FPosition write SetPosition;
    property TicksMargin:integer read FTicksMargin write SetTicksMargin;
    property LTicks:integer read FLTicks write SetLargeTicks;
    property LTicksSize:integer read FLTicksSize write SetLTicksSize;
    property LTicksColor:TColor read FLTicksColor write SetLTicksColor;
    property STicks:integer read FSTicks write SetSTicks;
    property STicksSize:integer read FSTicksSize write SetSTicksSize;
    property STicksColor:TColor read FSTicksColor write SetSTicksColor;
    property ShowValues:Boolean read FShowValues write SetShowValues;
    property ValuesMargin:integer read FValuesMargin write SetValueMargin;
    property ValuesFont:TFont read FValuesFont write SetValuesFont;
    property DefKnobRadius:integer read FDefKnobRadius write SetDefKnobRadius;
    property Transparent:Boolean read FTransparent write SetTransparent;
    property OnPaint;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    Background:TBGRABitmap;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


  { TuEMultiTurn }

  TuEMultiTurn = class(TCustomuEMultiTurn)
  private
  published
    procedure DrawScales(LBitmap:TBGRABitmap); override;
    property Picture;
    property Position;
//    property TicksMargin;
//    property LTicks;
//    property LTicksSize;
//    property LTicksColor;
//    property STicks;
//    property STicksSize;
//    property STicksColor;
//    property ShowValues;
//    property ValuesMargin;
//    property ValuesFont;
    property Transparent;
    property DefKnobRadius;
    property OnChange;
    property OnPaint;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
//
    property Align;
    property Anchors;
    property BorderSpacing;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnChangeBounds;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;

//    property Angle;
  end;


procedure Register;

implementation

{ TCustomuEMultiTurn }

procedure TCustomuEMultiTurn.SetPosition(const AValue: Real);
begin
  if FPosition=AValue then exit;
  ForcePosition(AValue);
end;

procedure TCustomuEMultiTurn.ForcePosition(const AValue: Real);
begin
  FPosition:=AValue;
  inherited Angle:=PostoAngle(FPosition)/10;
  invalidate;
  DoOnChange;
end;

procedure TCustomuEMultiTurn.SetShowValues(const AValue: Boolean);
begin
  if FShowValues=AValue then exit;
  FShowValues:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomuEMultiTurn.SetTransparent(const AValue: Boolean);
begin
  if FTransparent=AValue then exit;
  FTransparent:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomuEMultiTurn.SetSTicks(const AValue: integer);
begin
  if FSTicks=AValue then exit;
  FSTicks:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomuEMultiTurn.SetSTicksColor(const AValue: TColor);
begin
  if FSTicksColor=AValue then exit;
  FSTicksColor:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomuEMultiTurn.SetSTicksSize(const AValue: integer);
begin
  if FSTicksSize=AValue then exit;
  FSTicksSize:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomuEMultiTurn.SetTicksMargin(const AValue: integer);
begin
  if FTicksMargin=AValue then exit;
  FTicksMargin:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomuEMultiTurn.SetValueMargin(const AValue: integer);
begin
  if FValuesMargin=AValue then exit;
  FValuesMargin:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomuEMultiTurn.SetValuesFont(const AValue: TFont);
begin
  if FValuesFont.IsEqual(AValue) then exit;
  FValuesFont.Assign(AValue);
end;

class procedure TCustomuEMultiTurn.WSRegisterClass;
begin
  inherited WSRegisterClass;
end;

procedure TCustomuEMultiTurn.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);
end;

class function TCustomuEMultiTurn.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 45;
  Result.CY := 45;
end;

procedure TCustomuEMultiTurn.Paint;
begin
  Background.Draw(inherited Canvas,0,0,false);
  inherited Paint;
end;

procedure TCustomuEMultiTurn.DefaultPicture;
var
  tbmp:TBGRABitmap;
  c:real;
begin
  c:=(FDefKnobRadius-1)/2;
  tbmp :=TBGRABitmap.Create(FDefKnobRadius,FDefKnobRadius,BGRAPixelTransparent);
  tbmp.FillEllipseAntialias(c,c,c,c,BGRABlack);
  tbmp.GradientFill(0,0,FDefKnobRadius,FDefKnobRadius,
                      BGRA(128,128,128,255),BGRA(0,0,0,0),
                      gtRadial,PointF(c,c),PointF(0,c),
                      dmDrawWithTransparency);
  tbmp.DrawLineAntialias(c,FDefKnobRadius-7,c,FDefKnobRadius-5,BGRAWhite,2);
  tbmp.DrawLineAntialias(c,4,c,6,BGRAWhite,2);
  tbmp.DrawLineAntialias(4,c,6,c,BGRAWhite,2);
  tbmp.DrawLineAntialias(FDefKnobRadius-7,c,FDefKnobRadius-5,c,BGRAWhite,2);

  tbmp.DrawLineAntialias(FDefKnobRadius-8,7,FDefKnobRadius-10,9,BGRAWhite,2);
  tbmp.DrawLineAntialias(8,7,10,9,BGRAWhite,2);
  tbmp.DrawLineAntialias(7,FDefKnobRadius-8,9,FDefKnobRadius-10,BGRAWhite,2);
  tbmp.DrawLineAntialias(FDefKnobRadius-10,FDefKnobRadius-10,FDefKnobRadius-8,FDefKnobRadius-8,BGRAWhite,2);
  try
    Picture.Bitmap.Assign(tbmp.Bitmap);
  finally
    tbmp.free;
  end;
end;

procedure TCustomuEMultiTurn.Loaded;
begin
  inherited Loaded;
  ForcePosition(FPosition);
end;

procedure TCustomuEMultiTurn.SetColor(AValue: TColor);
begin
  if Color=AValue then exit;
  inherited SetColor(AValue);
  UpdateScales;
end;

procedure TCustomuEMultiTurn.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  UpdateScales;
  invalidate;
end;

function TCustomuEMultiTurn.AngleToPos(AnAngle: TuEAngle): Real;
// Convert angle AnAngle to a position.
begin
  Result :=  1000*AnAngle/3600;  //FMin + ((FMax - FMin) * (AnAngle - FMinAngle)/(FMaxAngle - FMinAngle));
end;

function TCustomuEMultiTurn.PosToAngle(Pos: Real): TuEAngle;
// Convert position Pos to an angle.
begin
  Result := Round(Pos*3600/1000);  //FMinAngle + Round((FMaxAngle - FMinAngle) * (Pos - FMin) / (FMax - FMin));
end;

procedure TCustomuEMultiTurn.DoOnResize;
begin
  BackGround.SetSize(width,height);
  UpdateScales;
  inherited DoOnResize;
end;

constructor TCustomuEMultiTurn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefKnobRadius:=34;
  FPosition:=0;
  FLTicks:=0;
  FSTicks:=0;
  FShowValues:=false;
  FTransparent:=true;
  FValuesFont:=TFont.Create;
  FValuesFont.Name:='Sans';
  FValuesFont.Orientation:=0;
  FValuesFont.Style:=[];
  FValuesFont.Color:=clBlack;
  FValuesFont.Size:=8;
  FValuesFont.OnChange:=@FontChanged;
  if Picture.Bitmap.Width=0 then DefaultPicture;
  UniqueSize:=true;
  Center:=true;
  ControlStyle := ControlStyle + [csReplicatable, csCaptureMouse, csClickEvents, csDoubleClicks];
  with GetControlClassDefaultSize do
  begin
    SetInitialBounds(0, 0, CX, CY);
    BackGround:=TBGRABitmap.Create(CX,CY);
  end;
end;

destructor TCustomuEMultiTurn.Destroy;
begin
  FreeThenNil(Background);
  FValuesFont.OnChange:=nil;
  FValuesFont.free;
  inherited Destroy;
end;

// Convert a APoint to an angle (relative to ACenter),
// where bottom is 0, left is 900, top is 1800 and so on.

function TCustomuEMultiTurn.PointToAngle(APoint, ACenter: TPoint): TuEAngle;
var
  N: Integer;
begin
  N := APoint.X - ACenter.X;
  if N = 0 then
    if APoint.Y < ACenter.Y then Result := 900 else Result := 2700
  else
  begin
    Result:=Round(ArcTan((ACenter.Y - APoint.Y) / N) * 1800 / PI);
  end;
  if N < 0 then Result := Result + 1800;
  Result := 2700 - Result;
end;

function TCustomuEMultiTurn.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result:=inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  SetPosition(FPosition+WheelDelta/200);
end;

function TCustomuEMultiTurn.Delta(X,Y:Integer):TuEAngle;
Var C,M,D:TuEAngle;
begin
  if Angle>0 then C:=Round(Angle*10) mod 3600 else C:=3600+(Round(Angle*10) mod 3600);
  M:=PointToAngle(Point(X, Y), GetCenter);
  D:=C-M;
  Delta:=0;
  if (D>-1800) or (D<1800) then Delta:=-D;
  if (D>0) and (D>1800) then Delta:=(3600-D);
  if (D<0) and (D<-1800) then Delta:=-(3600+D);
end;

procedure TCustomuEMultiTurn.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  MouseCapture := True;
  SetPosition(FPosition+AngletoPos(Delta(X,Y)));
end;

procedure TCustomuEMultiTurn.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if MouseCapture then SetPosition(FPosition+AngletoPos(Delta(X,Y)));
end;

procedure TCustomuEMultiTurn.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  MouseCapture := False;
end;

procedure TCustomuEMultiTurn.DoOnChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TCustomuEMultiTurn.GetCenter: TPoint;
begin
  with Result do
  begin
    X := Width div 2;
    Y := Height div 2;
  end;
end;

procedure TCustomuEMultiTurn.SetDefKnobRadius(AValue: integer);
begin
  if FDefKnobRadius=AValue then Exit;
  FDefKnobRadius:=AValue;
end;

procedure TCustomuEMultiTurn.SetLTicksColor(const AValue: TColor);
begin
  if FLTicksColor=AValue then exit;
  FLTicksColor:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomuEMultiTurn.SetLargeTicks(const AValue: integer);
begin
  if FLTicks=AValue then exit;
  FLTicks:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomuEMultiTurn.SetLTicksSize(const AValue: integer);
begin
  if FLTicksSize=AValue then exit;
  FLTicksSize:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomuEMultiTurn.DrawScales(LBitmap:TBGRABitmap);
var i,j:integer;
    x1,y1,x2,y2,lpos:real;
    xc,yc,langle:real;
    sn,cn:real;
    lc,sc,vc:TBGRAPixel;
    ts:TSize;
    la:string;
const
  FMax=10;
  FMin=0;

begin
  if (Picture.Bitmap.Width mod 2) = 0 then xc:=LBitmap.Width/2-1 else xc:=LBitmap.Width/2;
  if (Picture.Bitmap.Height mod 2) = 0 then yc:=LBitmap.Height/2-1 else yc:=LBitmap.Height/2;
  lc:=ColorToBGRA(ColorToRGB(FLTicksColor));
  sc:=ColorToBGRA(ColorToRGB(FSTicksColor));
  vc:=ColorToBGRA(ColorToRGB(FValuesFont.Color));
  LBitmap.FontHeight:=abs(FValuesFont.Height);
  LBitmap.FontStyle:=FValuesFont.Style;
  LBitmap.FontName:=FValuesFont.Name;
  LBitmap.FontOrientation:=FValuesFont.Orientation;
  if FLTicks>0 then For i:=0 to FLTicks do
  begin
    lpos:=(i/FLTicks)*(FMax-FMin)+FMin;
    langle:=PosToAngle(lpos)*PI/1800 +PI/2;
    sn:=sin(langle);
    cn:=cos(langle);
    x1:=xc+FTicksMargin*cn;
    y1:=yc+FTicksMargin*sn;
    x2:=xc+(FTicksMargin+FLTicksSize)*cn;
    y2:=yc+(FTicksMargin+FLTicksSize)*sn;
    LBitmap.DrawLineAntialias(x1,y1,x2,y2,lc, 1);
    if FShowValues then
    begin
      x2:=xc+(FTicksMargin+FLTicksSize+FValuesMargin)*cn;
      y2:=yc+(FTicksMargin+FLTicksSize+FValuesMargin)*sn;
      la:=floattostrF(lpos,ffGeneral,4,2);
      ts:=LBitmap.TextSize(la);
      LBitmap.TextOut(trunc(x2+1), trunc(y2-ts.cy/2+1), la, vc, taCenter);
    end;
    if (lpos<Fmax) then For j:=1 to FSTicks do
    begin
      lpos:=(i/FLTicks)*(FMax-FMin)+FMin+j*((FMax-FMin)/FLTicks)/(FSTicks+1);
      langle:=PosToAngle(lpos)*PI/1800 +PI/2;
      sn:=sin(langle);
      cn:=cos(langle);
      x1:=xc+FTicksMargin*cn;
      y1:=yc+FTicksMargin*sn;
      x2:=xc+(FTicksMargin+FSTicksSize)*cn;
      y2:=yc+(FTicksMargin+FSTicksSize)*sn;
      LBitmap.DrawLineAntialias(x1,y1,x2,y2,sc, 1);
    end;
  end;
end;

procedure TCustomuEMultiTurn.UpdateScales;
begin
  if ([csLoading,csDestroying]*ComponentState<>[]) then exit;
  if FTransparent then BackGround.Fill(BGRAPixelTransparent) else BackGround.Fill(ColortoBGRA(ColortoRGB(Color)));
  DrawScales(BackGround);
end;

{ TuEMultiTurn }

procedure TuEMultiTurn.DrawScales(LBitmap: TBGRABitmap);
begin
  inherited DrawScales(LBitmap);
end;

procedure Register;
begin
  {$I icons\ueMultiTurn_icon.lrs}
  RegisterComponents('BGRA Controls', [TuEMultiTurn]);
end;


end.
