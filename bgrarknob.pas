{-----------------------------------------------------------------------------
BGRARKnob v.2014.09.01

Forked from on Miguel A. Risco Castillo TuEKnob v0.6.9
http://ue.accesus.com/uecontrols

Copyright Miguel A. Risco Castillo and СМ630.
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.
-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------
List of features and bugfixes added to TuEKnob v0.6.9
- Added property „Width“ for Lticks.
- „LticksSize“ property renamed to „LTicksLength“. „LticksSize“ property is kept for legacy reasons.
- Added „indicator“ mode.
- Added „value list“ to allow custom string values of the LTicks scale.
-----------------------------------------------------------------------------}


unit BGRARKnob;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLIntf, LCLType, LCLProc, Types, BGRABitmap, BGRABitmapTypes, uERotImage;

type

  TuEAngle = 0..3600; // 0.0 - 360.0 deg


  { TCustomBGRARKnob }

  TCustomBGRARKnob = class(TCustomuERotImage)
  private
  protected
    FMax: Real;
    FMaxAngle: TuEAngle;
    FMin: Real;
    FMinAngle: TuEAngle;
    FOffsetAngle:integer;
    FIndicator: Boolean;
    FLTicks: integer;
    FLTicksColor: TColor;
    FLTicksSize: integer;
    FLTicksWidth: integer;
    FSTicks: integer;
    FSTicksColor: TColor;
    FSTicksSize: integer;
    FTicksMargin: integer;
    FShowValues: Boolean;
    FUseValueList: Boolean;
    FValuesFont: TFont;
    FValueList: TStringList;
    FValuesMargin: integer;
    FOnChange: TNotifyEvent;
    FPosition: Real;
    FTransparent:Boolean;
    FDefKnobRadius:integer;
    procedure SetLTicksColor(const AValue: TColor); virtual;
    procedure SetMax(const AValue: Real); virtual;
    procedure SetMaxAngle(const AValue: TuEAngle); virtual;
    procedure SetMin(const AValue: Real);  virtual;
    procedure SetMinAngle(const AValue: TuEAngle); virtual;
    procedure SetPosition(const AValue: Real); virtual;
    procedure SetLargeTicks(const AValue: integer); virtual;
    procedure SetLTicksSize(const AValue: integer); virtual;
    procedure SetLTicksWidth(const AValue: integer); virtual;
    procedure SetSTicks(const AValue: integer); virtual;
    procedure SetSTicksColor(const AValue: TColor); virtual;
    procedure SetSTicksSize(const AValue: integer); virtual;
    procedure SetTicksMargin(const AValue: integer); virtual;
    procedure SetShowValues(const AValue: Boolean); virtual;
    procedure SetUseValueList (const AValue: Boolean); virtual;
    procedure SetTransparent(const AValue: Boolean); virtual;
    procedure SetValueList (const AValue: TStringList); virtual;
    procedure SetValueMargin(const AValue: integer); virtual;
    procedure SetValuesFont(const AValue: TFont); virtual;
    procedure SetDefKnobRadius(AValue: integer); virtual;
    procedure SetOffsetAngle(AValue: integer); virtual;
    class procedure WSRegisterClass; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
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
    property Position: Real read FPosition write SetPosition;
    property Max: Real read FMax write SetMax;
    property MaxAngle: TuEAngle read FMaxAngle write SetMaxAngle;
    property Min: Real read FMin write SetMin;
    property MinAngle: TuEAngle read FMinAngle write SetMinAngle;
    property OffsetAngle: integer read FOffsetAngle write SetOffsetAngle;
    property TicksMargin:integer read FTicksMargin write SetTicksMargin;
    property Indicator: Boolean read FIndicator write FIndicator;
    property LTicks:integer read FLTicks write SetLargeTicks;
    property LTicksSize:integer read FLTicksSize write SetLTicksSize;
    property LTicksLength:integer read FLTicksSize write SetLTicksSize;
    property LTicksWidth:integer read FLTicksWidth write SetLTicksWidth;
    property LTicksColor:TColor read FLTicksColor write SetLTicksColor;
    property STicks:integer read FSTicks write SetSTicks;
    property STicksSize:integer read FSTicksSize write SetSTicksSize;
    property STicksColor:TColor read FSTicksColor write SetSTicksColor;
    property ShowValues:Boolean read FShowValues write SetShowValues;
    property UseValueList: Boolean read FUseValueList write SetUseValueList;
    property ValuesMargin:integer read FValuesMargin write SetValueMargin;
    property ValuesFont:TFont read FValuesFont write SetValuesFont;
    property ValueList: TStringList read FValueList write SetValueList;
    property Transparent:Boolean read FTransparent write SetTransparent;
    property DefKnobRadius:integer read FDefKnobRadius write SetDefKnobRadius;
    property OnPaint;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    Background:TBGRABitmap;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


  { TBGRARKnob }

  TBGRARKnob = class(TCustomBGRARKnob)
  published
    procedure DrawScales(LBitmap:TBGRABitmap); override;
    property Max;
    property MaxAngle;
    property Min;
    property MinAngle;
    property OffsetAngle;
    property Picture;
    property Position;
    property TicksMargin;
    property Indicator;
    property LTicks;
    property LTicksSize;
    property LTicksLength;
    property LTicksWidth;
    property LTicksColor;
    property STicks;
    property STicksSize;
    property STicksColor;
    property UseValueList;
    property ShowValues;
    property ValueList;
    property ValuesMargin;
    property ValuesFont;
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

  end;


procedure Register;

implementation

{ TCustomBGRARKnob }

procedure TCustomBGRARKnob.SetMax(const AValue: Real);
begin
  if (FMax=AValue) and (AValue<=FMin) then exit;
  FMax:=AValue;
  UpdateScales;
  ForcePosition(FPosition);
end;

procedure TCustomBGRARKnob.SetMaxAngle(const AValue: TuEAngle);
begin
  if FMaxAngle=AValue then exit;
  FMaxAngle:=AValue;
  UpdateScales;
  ForcePosition(FPosition);
end;

procedure TCustomBGRARKnob.SetMin(const AValue: Real);
begin
  if (FMin=AValue) and (AValue>=FMax) then exit;
  FMin:=AValue;
  UpdateScales;
  ForcePosition(FPosition);
end;

procedure TCustomBGRARKnob.SetMinAngle(const AValue: TuEAngle);
begin
  if FMinAngle=AValue then exit;
  FMinAngle:=AValue;
  UpdateScales;
  ForcePosition(FPosition);
end;

procedure TCustomBGRARKnob.SetPosition(const AValue: Real);
begin
  if FPosition=AValue then exit;
  ForcePosition(AValue);
end;

procedure TCustomBGRARKnob.ForcePosition(const AValue: Real);
begin
  if AValue<FMin then FPosition:=FMin
  else if AValue>FMax then FPosition:=FMax
  else FPosition:=AValue;
  inherited Angle:=(PostoAngle(FPosition)+FOffsetAngle)/10;
  invalidate;
  DoOnChange;
end;

procedure TCustomBGRARKnob.SetShowValues(const AValue: Boolean);
begin
  if FShowValues=AValue then exit;
  FShowValues:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomBGRARKnob.SetTransparent(const AValue: Boolean);
begin
  if FTransparent=AValue then exit;
  FTransparent:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomBGRARKnob.SetSTicks(const AValue: integer);
begin
  if FSTicks=AValue then exit;
  FSTicks:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomBGRARKnob.SetSTicksColor(const AValue: TColor);
begin
  if FSTicksColor=AValue then exit;
  FSTicksColor:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomBGRARKnob.SetSTicksSize(const AValue: integer);
begin
  if FSTicksSize=AValue then exit;
  FSTicksSize:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomBGRARKnob.SetTicksMargin(const AValue: integer);
begin
  if FTicksMargin=AValue then exit;
  FTicksMargin:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomBGRARKnob.SetValueMargin(const AValue: integer);
begin
  if FValuesMargin=AValue then exit;
  FValuesMargin:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomBGRARKnob.SetValuesFont(const AValue: TFont);
begin
  if FValuesFont.IsEqual(AValue) then exit;
  FValuesFont.Assign(AValue);
end;

class procedure TCustomBGRARKnob.WSRegisterClass;
begin
  inherited WSRegisterClass;
end;

procedure TCustomBGRARKnob.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);
end;

class function TCustomBGRARKnob.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 90;
  Result.CY := 90;
end;

procedure TCustomBGRARKnob.Paint;
begin
  Background.Draw(inherited Canvas,0,0,false);
  inherited Paint;
end;

procedure TCustomBGRARKnob.DefaultPicture;
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
  tbmp.DrawLineAntialias(c,c+5,c,FDefKnobRadius-5,BGRAWhite,2);
  try
    Picture.Bitmap.Assign(tbmp.Bitmap);
  finally
    tbmp.free;
  end;
end;

procedure TCustomBGRARKnob.Loaded;
begin
  inherited Loaded;
  ForcePosition(FPosition);
end;

procedure TCustomBGRARKnob.SetColor(AValue: TColor);
begin
  if Color=AValue then exit;
  inherited SetColor(AValue);
  UpdateScales;
end;

procedure TCustomBGRARKnob.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  UpdateScales;
end;

function TCustomBGRARKnob.AngleToPos(AnAngle: TuEAngle): Real;
// Convert angle AnAngle to a position.
begin
  Result := FMin + ((FMax - FMin) * (AnAngle - FMinAngle)/(FMaxAngle - FMinAngle));
end;

function TCustomBGRARKnob.PosToAngle(Pos: Real): TuEAngle;
// Convert position Pos to an angle.
begin
  Result := FMinAngle + Round((FMaxAngle - FMinAngle) * (Pos - FMin) / (FMax - FMin));
end;

procedure TCustomBGRARKnob.DoOnResize;
begin
  BackGround.SetSize(width,height);
  UpdateScales;
  inherited DoOnResize;
end;

function TCustomBGRARKnob.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result:=inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if FIndicator= False then SetPosition(FPosition+(FMax-FMin)*WheelDelta/20000);
end;

constructor TCustomBGRARKnob.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIndicator:= False;
  FDefKnobRadius:=34;
  FPosition:=0;
  FMax:=100;
  FMin:=0;
  FMaxAngle:=3300;
  FMinAngle:=300;
  FOffsetAngle:=0;
  FTicksMargin:=20;
  FLTicks:=10;
  FLTicksSize:=8;
  FLTicksWidth:=1;
  FLTicksColor:=clBlack;
  FSTicks:=3;
  FSTicksSize:=5;
  FSTicksColor:=clBlack;
  FShowValues:=true;
  FValuesMargin:=8;
  FValuesFont:=TFont.Create;
  FValuesFont.Name:='Sans';
  FValuesFont.Orientation:=0;
  FValuesFont.Style:=[];
  FValuesFont.Color:=clBlack;
  FValuesFont.Size:=8;
  FValuesFont.OnChange:=@FontChanged;
  FTransparent:=true;
  FValueList:= TStringList.Create;
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

destructor TCustomBGRARKnob.Destroy;
begin
  FreeThenNil(Background);
  FValuesFont.OnChange:=nil;
  FValuesFont.free;
  inherited Destroy;
end;

// Convert a APoint to an angle (relative to ACenter),
// where bottom is 0, left is 900, top is 1800 and so on.

function TCustomBGRARKnob.PointToAngle(APoint, ACenter: TPoint): TuEAngle;
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

procedure TCustomBGRARKnob.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var TmpAngle:Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  MouseCapture := True;
  TmpAngle:=PointToAngle(Point(X, Y), GetCenter)-OffsetAngle;
  if TmpAngle>3600 then TmpAngle:=TmpAngle-3600;
  if TmpAngle<0 then TmpAngle:=TmpAngle+3600;
  if FIndicator= False then SetPosition(AngletoPos(TmpAngle));
end;

procedure TCustomBGRARKnob.MouseMove(Shift: TShiftState; X, Y: Integer);
Var TmpAngle:Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if MouseCapture then
  begin
    TmpAngle:=PointToAngle(Point(X, Y), GetCenter)-OffsetAngle;
    if TmpAngle>3600 then TmpAngle:=TmpAngle-3600;
    if TmpAngle<0 then TmpAngle:=TmpAngle+3600;
    if FIndicator= False then SetPosition(AngletoPos(TmpAngle));
  end;
end;

procedure TCustomBGRARKnob.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  MouseCapture := False;
end;

procedure TCustomBGRARKnob.DoOnChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TCustomBGRARKnob.GetCenter: TPoint;
begin
  with Result do
  begin
    X := Width div 2;
    Y := Height div 2;
  end;
end;

procedure TCustomBGRARKnob.SetDefKnobRadius(AValue: integer);
begin
  if FDefKnobRadius=AValue then Exit;
  FDefKnobRadius:=AValue;
end;

procedure TCustomBGRARKnob.SetOffsetAngle(AValue: integer);
begin
  if FOffsetAngle=AValue then Exit;
  FOffsetAngle:=AValue;
  UpdateScales;
  ForcePosition(FPosition);
end;

procedure TCustomBGRARKnob.SetLTicksColor(const AValue: TColor);
begin
  if FLTicksColor=AValue then exit;
  FLTicksColor:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomBGRARKnob.SetLargeTicks(const AValue: integer);
begin
  if FLTicks=AValue then exit;
  FLTicks:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomBGRARKnob.SetLTicksSize(const AValue: integer);
begin
  if FLTicksSize=AValue then exit;
  FLTicksSize:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomBGRARKnob.SetLTicksWidth(const AValue: integer);
begin
  if FLTicksWidth=AValue then exit;
  FLTicksWidth:=AValue;
  UpdateScales;
  invalidate;
end;

procedure TCustomBGRARKnob.DrawScales(LBitmap:TBGRABitmap);
var i,j:integer;
    x1,y1,x2,y2,lpos:real;
    xc,yc,langle:real;
    sn,cn:real;
    lc,sc,vc:TBGRAPixel;
    ts:TSize;
    la:string;
    ss:boolean;
    LabelX, LabelY: integer;
    LabelOffset: integer;
begin
  LabelOffset:=trunc(FLTicksWidth/2);  //Needed because ticks are rounded. To be revisioned, when support for square ticks is added.
  if (Picture.Bitmap.Width mod 2) = 0 then xc:=LBitmap.Width/2-1 else xc:=LBitmap.Width/2;
  if (Picture.Bitmap.Height mod 2) = 0 then yc:=LBitmap.Height/2-1 else yc:=LBitmap.Height/2;
  lc:=ColorToBGRA(ColorToRGB(FLTicksColor));
  sc:=ColorToBGRA(ColorToRGB(FSTicksColor));
  vc:=ColorToBGRA(ColorToRGB(FValuesFont.Color));
  LBitmap.FontHeight:=abs(FValuesFont.Height);
  LBitmap.FontStyle:=FValuesFont.Style;
  LBitmap.FontName:=FValuesFont.Name;
  LBitmap.FontOrientation:=FValuesFont.Orientation;
  ss:=((FMaxAngle-FMinAngle) mod 3600)=0;
  if FLTicks>0 then For i:=0 to FLTicks do
  begin
    lpos:=(i/FLTicks)*(FMax-FMin)+FMin;
    langle:=(PosToAngle(lpos)+FOffsetAngle)*PI/1800+PI/2;
    sn:=sin(langle);
    cn:=cos(langle);
    x1:=xc+FTicksMargin*cn;
    y1:=yc+FTicksMargin*sn;
    x2:=xc+(FTicksMargin+FLTicksSize)*cn;
    y2:=yc+(FTicksMargin+FLTicksSize)*sn;
    LBitmap.DrawLineAntialias(x1,y1,x2,y2,lc, FLTicksWidth);
    if FShowValues and not(ss and (i=0)) then
    begin
      x2:=xc+(FTicksMargin+FLTicksSize+FValuesMargin)*cn;
      y2:=yc+(FTicksMargin+FLTicksSize+FValuesMargin)*sn;
      if UseValueList and (FValueList.Count> trunc(lpos-Min))
         then la:=FValueList.Strings[trunc(lpos-Min)]
         else la:=floattostrF(lpos,ffGeneral,4,2);
      ts:=LBitmap.TextSize(la);
      if (x2> (xc-LabelOffset)) and (x2<xc+LabelOffset)
         then LabelX:=trunc(x2) else if x2< (xc-LabelOffset) then LabelX:= trunc(x2-LabelOffset) else LabelX:= trunc(x2+LabelOffset+1);
      if y2< yc then LabelY:= trunc(y2-ts.cy/2-4) else LabelY:= trunc(y2-ts.cy/2+4);
      LBitmap.TextOut(LabelX, LabelY, la, vc, taCenter);
    end;
    if (lpos<Fmax) then For j:=1 to FSTicks do
    begin
      lpos:=(i/FLTicks)*(FMax-FMin)+FMin+j*((FMax-FMin)/FLTicks)/(FSTicks+1);
      langle:=(PosToAngle(lpos)+FOffsetAngle)*PI/1800 +PI/2;
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

procedure TCustomBGRARKnob.UpdateScales;
begin
  if ([csLoading,csDestroying]*ComponentState<>[]) then exit;
  //if AutoSizeDelayed then exit;

  if FTransparent then BackGround.Fill(BGRAPixelTransparent) else BackGround.Fill(ColortoBGRA(ColortoRGB(Color)));
  DrawScales(BackGround);
end;

procedure TCustomBGRARKnob.SetValueList (const AValue: TStringList);
begin
  if FValueList.Equals(AValue) then exit;
  if AValue.Count>1 then FValueList.Assign(AValue);
  UpdateScales;
  invalidate;
end;

procedure TCustomBGRARKnob.SetUseValueList (const AValue: Boolean);
begin
  if FUseValueList=AValue then exit;
  FUseValueList:=AValue;
  UpdateScales;
  invalidate;
end;

{ TBGRARKnob }

procedure TBGRARKnob.DrawScales(LBitmap: TBGRABitmap);
begin
  inherited DrawScales(LBitmap);
end;

procedure Register;
begin
  {$I icons\bgrarknob_icon.lrs}
  RegisterComponents('BGRA Controls', [TBGRARKnob]);
end;


end.
