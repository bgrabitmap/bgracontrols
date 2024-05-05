{
 2024 by hedgehog
}

unit BCFluentSlider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls,
  ComCtrls,
  BGRAGraphicControl, BGRABitmapTypes, BCTypes;

type

  TTickPlacement = (tpBottomRight, tpTopLeft);
  TSliderOrientation = (pbHorizontal, pbVertical, pbRightToLeft, pbTopDown);

  { TBCFluentSlider }

  TBCFluentSlider = class(TCustomBGRAGraphicControl)
  private
    FOnChangeValue: TNotifyEvent;
    FShowTicks: boolean;
    FThumbState: TBCMouseState;
    FDeltaThumbPos: TPointF;
    FFullRect, FDrawRect: TRectF;
    FThumbRadius: single;
    FBorderWidth: single;
    FThumbRect: TRectF;
    FCenterDrawRect: TPointF;
    FRealLineWidth: single;
    FRealLineColor: TBGRAPixel;
    FOrientation: TSliderOrientation;
    FStartTickCount: QWord;
    FAnimationPercent: integer;
    FStartPercent: integer;
    FTickFrequency: integer;
    FTickPlacement: TTickPlacement;
    FTimer: TTimer;
    FMaxValue: integer;
    FMinValue: integer;
    FValue: integer;
    FLineColor: TColor;
    FLineBkgColor: TColor;
    FLineWidth: integer;
    procedure CalculateThumbRect;
    function GetThumbPosition: TPoint;
    function GetThumbRadius: integer;
    function GetValueFromMouse(X, Y: single): integer;
    function GetXYFromValue(v: integer): TPointF;
    procedure SetLineBkgColor(AValue: TColor);
    procedure SetLineColor(AValue: TColor);
    procedure SetMaxValue(AValue: integer);
    procedure SetMinValue(AValue: integer);
    procedure SetOrientation(AValue: TSliderOrientation);
    procedure SetShowTicks(AValue: boolean);
    procedure SetTickFrequency(AValue: integer);
    procedure SetTickPlacement(AValue: TTickPlacement);
    procedure SetValue(AValue: integer);
    procedure SetLineWidth(AValue: integer);
    procedure DrawBackLine;
    procedure DrawTicks;
    procedure DrawThumb;
    procedure SetStateAnimation(NewState: TBCMouseState);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure RedrawBitmapContent; override;
    procedure TimerEvent({%H-}Sender: TObject);
    procedure TimerStart({%H-}Sender: TObject);
    procedure DrawBackground; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property ThumbPosition: TPoint read GetThumbPosition;
    property ThumbRadius: integer read GetThumbRadius;
  published
    property MinValue: integer read FMinValue write SetMinValue default 0;
    property MaxValue: integer read FMaxValue write SetMaxValue default 100;
    property Value: integer read FValue write SetValue default 0;
    property LineColor: TColor read FLineColor write SetLineColor default
      TColor($009E5A00);
    property LineBkgColor: TColor read FLineBkgColor write SetLineBkgColor default
      TColor($00808080);
    property LineWidth: integer read FLineWidth write SetLineWidth default 0;
    property Orientation: TSliderOrientation read FOrientation write SetOrientation default pbHorizontal;
    property ShowTicks: boolean read FShowTicks write SetShowTicks default false;
    property OnChangeValue: TNotifyEvent read FOnChangeValue write FOnChangeValue;
    property TickPlacement: TTickPlacement read FTickPlacement write SetTickPlacement default tpBottomRight;
    property TickFrequency: integer read FTickFrequency write SetTickFrequency default 10;
    property OnRedraw;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Enabled;
    property Hint;
    property PopupMenu;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnChangeBounds;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
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
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCFluentSlider]);
end;


const
  ThumbPercentArray: array [TBCMouseState] of integer = (50, 62, 41);

function InflateRectF(const R: TRectF; dxy: single): TRectF;
begin
  Result:= RectF(R.Left-dxy, R.Top-dxy, R.Right+dxy, R.Bottom+ dxy);
end;

function InflateRectF(const R: TRectF; dx, dy: single): TRectF;
begin
  Result:= RectF(R.Left-dx, R.Top-dy, R.Right+dx, R.Bottom+ dy);
end;

function CenterOfRectF(const R: TRectF): TPointF;
begin
  Result.x:= (R.Left + R.Right)/2;
  Result.y:= (R.Top + R.Bottom)/2;
end;

function ContainsInRectF(const R: TRectF; X, Y: integer): boolean;
begin
  Result:=  (X>= R.Left) and (X<= R.Right) and (Y>= R.Top) and (Y<= R.Bottom);
end;

//  TPointF.Offset() not worked for me(
function MovePointF(const P: TPointF; dx, dy: integer): TPointF;
begin
  Result.x:= P.x + dx;
  Result.y:= P.Y + dy;

end;

{ TBCFluentSlider }

procedure TBCFluentSlider.SetMaxValue(AValue: integer);
begin
  if FMaxValue = AValue then
    exit;
  FMaxValue := AValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  if FMinValue > FMaxValue then
    FMinValue := FMaxValue;
  DiscardBitmap;
end;

procedure TBCFluentSlider.SetLineBkgColor(AValue: TColor);
begin
  if FLineBkgColor = AValue then
    Exit;
  FLineBkgColor := AValue;
  DiscardBitmap;
end;

procedure TBCFluentSlider.SetLineColor(AValue: TColor);
begin
  if FLineColor = AValue then
    Exit;
  FLineColor := AValue;
  DiscardBitmap;
end;

procedure TBCFluentSlider.SetMinValue(AValue: integer);
begin
  if FMinValue = AValue then
    exit;
  FMinValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FMaxValue < FMinValue then
    FMaxValue := FMinValue;
  DiscardBitmap;
end;

procedure TBCFluentSlider.SetOrientation(AValue: TSliderOrientation);
var
  k: integer;
begin
  if FOrientation=AValue then Exit;
  k:= ord(FOrientation) + ord(AValue)+1;
  FOrientation:=AValue;
  if odd(k) or (Width=Height) then
    DiscardBitmap
  else
    SetBounds(Left, Top, Height, Width);
end;

procedure TBCFluentSlider.SetShowTicks(AValue: boolean);
begin
  if FShowTicks=AValue then Exit;
  FShowTicks:=AValue;
  DiscardBitmap;
end;

procedure TBCFluentSlider.SetTickFrequency(AValue: integer);
begin
  if FTickFrequency=AValue then Exit;
  FTickFrequency:=AValue;
  if FShowTicks then
    DiscardBitmap;
end;

procedure TBCFluentSlider.SetTickPlacement(AValue: TTickPlacement);
begin
  if FTickPlacement=AValue then Exit;
  FTickPlacement:=AValue;
  if FShowTicks then
    DiscardBitmap;
end;

procedure TBCFluentSlider.SetValue(AValue: integer);
begin
  if FValue = AValue then
    exit;
  FValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  DiscardBitmap;
  if Assigned(FOnChangeValue) then FOnChangeValue(self);
end;

procedure TBCFluentSlider.SetLineWidth(AValue: integer);
begin
  if FLineWidth = AValue then exit;
  FLineWidth := AValue;
  DiscardBitmap;
end;

procedure TBCFluentSlider.DrawBackground;
begin
  //todo:
end;

procedure TBCFluentSlider.DrawBackLine;
var
  p, p1, p2: TPointF;
begin
  p:= CenterOfRectF(FThumbRect);
  p1:= p;
  p2:= p;
  Bitmap.LineCap := pecRound;
  case FOrientation of
    pbHorizontal:
    begin
      p1.x:= FDrawRect.Right-1;
      p2.x:= FDrawRect.Left;
    end;
    pbVertical:
    begin
      p1.y:= FDrawRect.Top;
      p2.y:= FDrawRect.Bottom-1;
    end;
    pbRightToLeft:
    begin
      p1.x:= FDrawRect.Left;
      p2.x:= FDrawRect.Right-1;
    end;
    pbTopDown:
    begin
      p1.y:= FDrawRect.Bottom-1;
      p2.y:= FDrawRect.Top;
    end;
  end;
  if FValue < FMaxValue then
    Bitmap.DrawLineAntialias(p.x, p.y, p1.x, p1.y, FLineBkgColor, FRealLineWidth);
  if FValue > FMinValue then
  begin
    Bitmap.DrawLineAntialias(p2.x, p2.y, p.x, p.y, FRealLineColor, FRealLineWidth);
  end;
end;

procedure TBCFluentSlider.DrawTicks;
var
  v: integer;
  p: TPointF;
  x, y, tickSize, xy1, xy2: single;
  c: TBGRAPixel;
begin
  if (not FShowTicks) or (FTickFrequency<1) then exit;
  c:= FLineBkgColor;
  tickSize:= Scale96ToScreen(40)/10;
  if FTickPlacement = tpBottomRight then
  begin
    xy2:= FThumbRadius;
    xy1:= xy2 - tickSize;
  end
  else
  begin
    xy2:= -FThumbRadius;
    xy1:= xy2 + tickSize;
  end;
  v:= (FMinValue div FTickFrequency)*FTickFrequency;
  if v < FMinValue then
    v+= FTickFrequency;

  while v<= FMaxValue do
  begin
    p:= GetXYFromValue(v);
    if (FOrientation = pbHorizontal) or (FOrientation = pbRightToLeft) then
    begin
      if FBorderWidth<2 then x:= round(p.x) else x:= p.x;
      Bitmap.DrawLineAntialias(x, p.y + xy1, x, p.y+ xy2, c, FBorderWidth);
    end
    else
    begin
      if FBorderWidth<2 then y:= round(p.y) else y:= p.y;
      Bitmap.DrawLineAntialias(x + xy1, y, x+ xy2, y, c, FBorderWidth);
    end;
    v+= FTickFrequency;
  end;
end;

procedure TBCFluentSlider.DrawThumb;
var
  r: single;
  c: TBGRAPixel;
  p: TPointF;
begin
  c:= BGRABlack;
  c.alpha:= 50;
  p:= CenterOfRectF(FThumbRect);
  r:= FThumbRadius-FBorderWidth/2;
  Bitmap.EllipseAntialias(p.X, p.Y, r, r, c, FBorderWidth);
  c:= BGRAWhite;
  c.alpha:= 220;
  r:= FThumbRadius-FBorderWidth;
  Bitmap.FillEllipseAntialias(p.X, p.Y, r, r, c);
  c:= FRealLineColor;
  if FTimer.Enabled then
    r:= FThumbRadius*FAnimationPercent/100
  else
  begin
    r:= FThumbRadius*ThumbPercentArray[FThumbState]/100;
    if FThumbState = msClicked then
      c.alpha:= 220;
  end;
  Bitmap.FillEllipseAntialias(p.X, p.Y, r, r, c);
end;

procedure TBCFluentSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (FThumbState = msClicked) then
  begin
    SetValue(GetValueFromMouse(X+FDeltaThumbPos.x, Y+FDeltaThumbPos.y));
  end
  else if (FThumbState = msHover) then
  begin
    if not ContainsInRectF(FThumbRect, X, Y) then
      SetStateAnimation(msNone);
  end
  else if (FThumbState = msNone) then
  begin
    if ContainsInRectF(FThumbRect, X, Y) then
      SetStateAnimation(msHover);
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TBCFluentSlider.SetStateAnimation(NewState: TBCMouseState);
begin
  if FTimer.Enabled then
  begin
    FStartPercent:= FAnimationPercent;
    FTimer.Enabled:= false;
  end
  else
  begin
    FStartPercent:= ThumbPercentArray[FThumbState];
  end;
  FThumbState:= NewState;
  FTimer.Enabled:= true;
end;

procedure TBCFluentSlider.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if FThumbState = msHover then
    begin
      FDeltaThumbPos:= MovePointF(CenterOfRectF(FThumbRect), -X, -Y);
      SetStateAnimation(msClicked);
    end
    else
    begin
      FThumbState:= msClicked;
      FDeltaThumbPos:= PointF(0, 0);
      SetValue(GetValueFromMouse(X, Y));
    end;
  end;
  inherited;
end;

function TBCFluentSlider.GetValueFromMouse(X, Y: single): integer;

  function GetValPos(k: double): integer; inline;
  begin
    Result:= round(FMinValue +(FMaxValue - FMinValue)*k);
  end;

var
  R: TRectF;
begin
  R:= InflateRectF(FFullRect, - FThumbRadius);
  case FOrientation of
    pbHorizontal:
      begin
        if X< R.Left then Result:= FMinValue
        else if X> R.Right then Result:= FMaxValue
        else Result:= GetValPos((X - R.Left)/(R.Width-1));
      end;
    pbVertical:
      begin
        if Y> R.Bottom then Result:= FMinValue
        else if Y< R.Top then Result:= FMaxValue
        else Result:= GetValPos((R.Bottom - Y)/(R.Height-1));
      end;
    pbRightToLeft:
      begin
        if X< R.Left then Result:= FMaxValue
        else if X> R.Right then Result:= FMinValue
        else Result:= GetValPos((R.Right - X)/(R.Width-1));
      end;
    pbTopDown:
    begin
      if Y> R.Bottom then Result:= FMaxValue
      else if Y< R.Top then Result:= FMinValue
      else Result:= GetValPos((Y - R.Top)/(R.Height-1));
    end;
  end;
end;

procedure TBCFluentSlider.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if (Button = mbLeft) and (FThumbState = msClicked) then
  begin
    if ContainsInRectF(FThumbRect, X, Y) then
      SetStateAnimation(msHover)
    else
      SetStateAnimation(msNone);
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TBCFluentSlider.MouseLeave;
begin
  if FThumbState = msHover then
    SetStateAnimation(msNone);
  inherited MouseLeave;
end;

function TBCFluentSlider.GetXYFromValue(v: integer): TPointF;
var
  k: double;
  R: TRectF;
  p: TPointF;
begin
  R:= InflateRectF(FFullRect, -FThumbRadius);
  k:= (v- FMinValue)/(FMaxValue - FMinValue);
  p:= FCenterDrawRect;
  case FOrientation of
    pbHorizontal:  p.x:=
      R.Left + (R.Width-1)*k;
    pbVertical:    p.y:=
      R.Bottom -1 - (R.Height-1)*k;
    pbRightToLeft: p.x:=
      R.Right -1 - (R.Width-1)*k;
    pbTopDown:     p.y:=
      R.Top + (R.Height-1)*k;
  end;
  Result:= p;
end;

procedure TBCFluentSlider.CalculateThumbRect;
var
  k: double;
  R: TRectF;
  p: TPointF;
begin
   FThumbRadius:= Scale96ToScreen(11);
  if FLineWidth<1 then
    FRealLineWidth:= Scale96ToScreen(40)/10
  else
    FRealLineWidth:= Scale96ToScreen(FLineWidth*10)/10;
  FBorderWidth:= Scale96ToScreen(10)/10;
  FFullRect:= RectF(ClientRect);
  FDrawRect:= InflateRectF(FFullRect, -FRealLineWidth/2);
  R:= InflateRectF(FFullRect, -FThumbRadius);
  FCenterDrawRect := CenterOfRectF(FDrawRect);
  k:= (FValue- FMinValue)/(FMaxValue - FMinValue);
  p:= FCenterDrawRect;
  case FOrientation of
    pbHorizontal:  p.x:=
      R.Left + (R.Width-1)*k;
    pbVertical:    p.y:=
      R.Bottom -1 - (R.Height-1)*k;
    pbRightToLeft: p.x:=
      R.Right -1 - (R.Width-1)*k;
    pbTopDown:     p.y:=
      R.Top + (R.Height-1)*k;
  end;
  FThumbRect:= RectF(
    p.x - FThumbRadius,
    p.y - FThumbRadius,
    p.x + FThumbRadius,
    p.y + FThumbRadius);
end;

function TBCFluentSlider.GetThumbPosition: TPoint;
var
  p: TPointF;
begin
  p:= CenterOfRectF(FThumbRect);
  Result.X:= round(p.x);
  Result.Y:= round(p.y);
end;

function TBCFluentSlider.GetThumbRadius: integer;
begin
  Result:= round(FThumbRadius);
end;

procedure TBCFluentSlider.RedrawBitmapContent;
begin
  CalculateThumbRect;
  DrawBackground;
  FRealLineColor:= FLineColor;
  if not Enabled then
    FRealLineColor:= FRealLineColor.ToGrayscale;
  DrawBackLine;
  DrawTicks;
  DrawThumb;
  if Assigned(OnRedraw) then
    OnRedraw(self, FBGRA);
end;

procedure TBCFluentSlider.TimerEvent(Sender: TObject);
var
  TickCount: QWord;
  FAnimationTime: Int64;
const
  Duration = 130; // ms
begin
  TickCount:= GetTickCount64;
  FAnimationTime:= TickCount - FStartTickCount;
  if FAnimationTime >= Duration then
    FTimer.Enabled:= false
  else
    FAnimationPercent:= FStartPercent+
      round(FAnimationTime*(ThumbPercentArray[FThumbState] - FStartPercent)/Duration);
  DiscardBitmap;
end;

procedure TBCFluentSlider.TimerStart(Sender: TObject);
begin
  FStartTickCount:= GetTickCount64;
end;

constructor TBCFluentSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThumbState:= msNone;
  FOrientation:= pbHorizontal;
  FTickFrequency:= 10;
  FTimer:= TTimer.Create(self);
  FTimer.Interval := 15;
  FTimer.Enabled := false;
  FTimer.OnTimer := @TimerEvent;
  FTimer.OnStartTimer:= @TimerStart;

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, 150, 32);
  FMaxValue := 100;
  FMinValue := 0;
  FValue := 0;
  FLineWidth:=0;
  FLineColor := TColor($009E5A00);
  FLineBkgColor := TColor($00808080);
end;


end.


