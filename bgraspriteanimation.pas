unit BGRASpriteAnimation;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics, LCLIntF, LResources,
  BGRABitmap, BGRABitmapTypes, BCTypes, BGRAAnimatedGif;

type

  TFlipMode     = (flNone, flHorizontal, flVertical, flBoth);
  TRotationMode = (rtNone, rtClockWise, rtCounterClockWise);

  { TBGRASpriteAnimation }

  TBGRASpriteAnimation = class(TGraphicControl)
  private
    { Private declarations }
    FAnimInvert:     boolean;
    FAnimPosition:   cardinal;
    FAnimRepeat:     cardinal;
    FAnimRepeatLap:  cardinal;
    FAnimSpeed:      cardinal;
    FAnimStatic:     boolean;
    FAnimTimer:      TTimer;
    FAutoSize:       boolean;
    FCenter:         boolean;
    FOnLapChanged:   TNotifyEvent;
    FOnLapChanging:  TNotifyEvent;
    FOnPositionChanged: TNotifyEvent;
    FOnPositionChanging: TNotifyEvent;
    FOnRedrawAfter:  TBGRARedrawEvent;
    FOnRedrawBefore: TBGRARedrawEvent;
    FProportional:   boolean;
    FSprite:         TBitmap;
    FSpriteCount:    cardinal;
    FSpriteFillOpacity: byte;
    FSpriteFlipMode: TFlipMode;
    FSpriteKeyColor: TColor;
    FSpriteResampleFilter: TResampleFilter;
    FSpriteResampleMode: TResampleMode;
    FSpriteRotation: TRotationMode;
    FStretch:        boolean;
    FTile:           boolean;
    function DoCalculateDestRect(AWidth, AHeight: integer): TRect;
    function DoCalculatePosition(AValue: integer): integer;
    function DoCalculateSize(AValue: cardinal): cardinal;
    procedure DoAnimTimerOnTimer(Sender: TObject);
    procedure DoSpriteDraw(ABitmap: TBGRABitmap);
    procedure DoSpriteFillOpacity(ABitmap: TBGRABitmap);
    procedure DoSpriteFlip(ABitmap: TBGRABitmap);
    procedure DoSpriteKeyColor(ABitmap: TBGRABitmap);
    procedure DoSpriteResampleFilter(ABitmap: TBGRABitmap);
    procedure SetFAnimInvert(const AValue: boolean);
    procedure SetFAnimPosition(const AValue: cardinal);
    procedure SetFAnimRepeat(const AValue: cardinal);
    procedure SetFAnimRepeatLap(const AValue: cardinal);
    procedure SetFAnimSpeed(const AValue: cardinal);
    procedure SetFAnimStatic(const AValue: boolean);
    procedure SetFAutoSize(const AValue: boolean);
    procedure SetFCenter(const AValue: boolean);
    procedure SetFProportional(const AValue: boolean);
    procedure SetFSprite(const AValue: TBitmap);
    procedure SetFSpriteCount(const AValue: cardinal);
    procedure SetFSpriteFillOpacity(const AValue: byte);
    procedure SetFSpriteFlipMode(const AValue: TFlipMode);
    procedure SetFSpriteKeyColor(const AValue: TColor);
    procedure SetFSpriteResampleFilter(const AValue: TResampleFilter);
    procedure SetFSpriteResampleMode(const AValue: TResampleMode);
    procedure SetFSpriteRotation(const AValue: TRotationMode);
    procedure SetFStretch(const AValue: boolean);
    procedure SetFTile(const AValue: boolean);
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    function AnimatedGifToSprite(Filename: string): TBGRABitmap;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property AnimInvert: boolean Read FAnimInvert Write SetFAnimInvert;
    property AnimPosition: cardinal Read FAnimPosition Write SetFAnimPosition;
    property AnimRepeat: cardinal Read FAnimRepeat Write SetFAnimRepeat;
    property AnimRepeatLap: cardinal Read FAnimRepeatLap Write SetFAnimRepeatLap;
    property AnimSpeed: cardinal Read FAnimSpeed Write SetFAnimSpeed;
    property AnimStatic: boolean Read FAnimStatic Write SetFAnimStatic;
    property AutoSize: boolean Read FAutoSize Write SetFAutoSize; // to be implemented
    property Center: boolean Read FCenter Write SetFCenter;
    property Proportional: boolean Read FProportional Write SetFProportional;
    property Sprite: TBitmap Read FSprite Write SetFSprite;
    property SpriteCount: cardinal Read FSpriteCount Write SetFSpriteCount;
    property SpriteFillOpacity: byte Read FSpriteFillOpacity Write SetFSpriteFillOpacity;
    property SpriteFlipMode: TFlipMode Read FSpriteFlipMode Write SetFSpriteFlipMode;
    property SpriteKeyColor: TColor Read FSpriteKeyColor Write SetFSpriteKeyColor;
    property SpriteResampleFilter: TResampleFilter
      Read FSpriteResampleFilter Write SetFSpriteResampleFilter;
    property SpriteResampleMode: TResampleMode
      Read FSpriteResampleMode Write SetFSpriteResampleMode;
    property SpriteRotation: TRotationMode Read FSpriteRotation Write SetFSpriteRotation;
    property Stretch: boolean Read FStretch Write SetFStretch;
    property Tile: boolean Read FTile Write SetFTile;
  published
    property Align;
    property Anchors;
    property Caption;
    property Enabled;
    property OnClick;
    property OnDblClick;
    property OnLapChanged: TNotifyEvent Read FOnLapChanged Write FOnLapChanged;
    property OnLapChanging: TNotifyEvent Read FOnLapChanging Write FOnLapChanging;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPositionChanged: TNotifyEvent
      Read FOnPositionChanged Write FOnPositionChanged;
    property OnPositionChanging: TNotifyEvent
      Read FOnPositionChanging Write FOnPositionChanging;
    property OnRedrawAfter: TBGRARedrawEvent Read FOnRedrawAfter Write FOnRedrawAfter;
    property OnRedrawBefore: TBGRARedrawEvent Read FOnRedrawBefore Write FOnRedrawBefore;
    property PopupMenu;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I bgraspriteanimation_icon.lrs}
  RegisterComponents('BGRA Controls', [TBGRASpriteAnimation]);
end;

{ TBGRASpriteAnimation }

{ Animation Variables }

procedure TBGRASpriteAnimation.SetFAnimInvert(const AValue: boolean);
begin
  if FAnimInvert = AValue then
    Exit;
  FAnimInvert := AValue;

  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TBGRASpriteAnimation.SetFAnimPosition(const AValue: cardinal);
begin
  if FAnimPosition = AValue then
    Exit;
  if (AValue < 1) or (AValue > FSpriteCount) then
    FAnimPosition := 1
  else
    FAnimPosition := AValue;

  if Assigned(FOnPositionChanged) then
    FOnPositionChanged(Self);

  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TBGRASpriteAnimation.SetFAnimRepeat(const AValue: cardinal);
begin
  if FAnimRepeat = AValue then
    Exit;
  FAnimRepeat := AValue;
end;

procedure TBGRASpriteAnimation.SetFAnimRepeatLap(const AValue: cardinal);
begin
  if (FAnimRepeatLap = AValue) then
    Exit;
  FAnimRepeatLap := AValue;

  if (AValue = FAnimRepeat) and (AValue <> 0) then
  begin
    if csDesigning in ComponentState then
      Exit;
    SetFAnimStatic(True);
  end;

  if Assigned(FOnLapChanged) then
    FOnLapChanged(Self);
end;

procedure TBGRASpriteAnimation.SetFAnimSpeed(const AValue: cardinal);
begin
  if FAnimSpeed = AValue then
    Exit;
  FAnimSpeed := AValue;
  FAnimTimer.Interval := AValue;
end;

procedure TBGRASpriteAnimation.SetFAnimStatic(const AValue: boolean);
begin
  if FAnimStatic = AValue then
    Exit;
  FAnimStatic := AValue;

  if csDesigning in ComponentState then
    Exit;
  FAnimTimer.Enabled := not AValue;
end;

{ Sprite Variables }

procedure TBGRASpriteAnimation.SetFSprite(const AValue: TBitmap);
begin
  if (FSprite = AValue) or (AValue = nil) then
    Exit;

  FSprite := AValue;

  Invalidate;
end;

procedure TBGRASpriteAnimation.SetFSpriteCount(const AValue: cardinal);
begin
  if (FSpriteCount = AValue) or (FSprite = nil) then
    Exit;

  if (AValue < 1) or (AValue > cardinal(FSprite.Width)) then
    FSpriteCount := 1
  else
    FSpriteCount := AValue;

  if AnimPosition > AValue then
    SetFAnimPosition(1);

  Invalidate;
end;

procedure TBGRASpriteAnimation.SetFSpriteFillOpacity(const AValue: byte);
begin
  if FSpriteFillOpacity = AValue then
    Exit;
  FSpriteFillOpacity := AValue;

  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TBGRASpriteAnimation.SetFSpriteFlipMode(const AValue: TFlipMode);
begin
  if FSpriteFlipMode = AValue then
    Exit;
  FSpriteFlipMode := AValue;

  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TBGRASpriteAnimation.SetFSpriteKeyColor(const AValue: TColor);
begin
  if FSpriteKeyColor = AValue then
    Exit;
  FSpriteKeyColor := AValue;

  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TBGRASpriteAnimation.SetFSpriteResampleFilter(const AValue: TResampleFilter);
begin
  if FSpriteResampleFilter = AValue then
    Exit;
  FSpriteResampleFilter := AValue;

  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TBGRASpriteAnimation.SetFSpriteResampleMode(const AValue: TResampleMode);
begin
  if FSpriteResampleMode = AValue then
    Exit;
  FSpriteResampleMode := AValue;

  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TBGRASpriteAnimation.SetFSpriteRotation(const AValue: TRotationMode);
begin
  if FSpriteRotation = AValue then
    Exit;
  FSpriteRotation := AValue;

  if csDesigning in ComponentState then
    Invalidate;
end;

{ General Variables }

procedure TBGRASPriteAnimation.SetFAutoSize(const AValue: boolean);
begin
  if FAutoSize = AValue then
    Exit;
  FAutoSize := AValue;

  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TBGRASpriteAnimation.SetFCenter(const AValue: boolean);
begin
  if FCenter = AValue then
    Exit;
  FCenter := AValue;

  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TBGRASpriteAnimation.SetFProportional(const AValue: boolean);
begin
  if FProportional = AValue then
    Exit;
  FProportional := AValue;

  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TBGRASpriteAnimation.SetFStretch(const AValue: boolean);
begin
  if FStretch = AValue then
    Exit;
  FStretch := AValue;

  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TBGRASpriteAnimation.SetFTile(const AValue: boolean);
begin
  if FTile = AValue then
    Exit;
  FTile := AValue;

  if csDesigning in ComponentState then
    Invalidate;
end;

{ Utils }

function TBGRASpriteAnimation.DoCalculateDestRect(AWidth, AHeight: integer): TRect;
var
  PicWidth: integer;
  PicHeight: integer;
  ImgWidth: integer;
  ImgHeight: integer;
  w: integer;
  h: integer;
begin
  PicWidth  := AWidth;
  PicHeight := AHeight;
  ImgWidth  := ClientWidth;
  ImgHeight := ClientHeight;
  if Stretch or (Proportional and ((PicWidth > ImgWidth) or
    (PicHeight > ImgHeight))) then
  begin
    if Proportional and (PicWidth > 0) and (PicHeight > 0) then
    begin
      w := ImgWidth;
      h := (PicHeight * w) div PicWidth;
      if h > ImgHeight then
      begin
        h := ImgHeight;
        w := (PicWidth * h) div PicHeight;
      end;
      PicWidth  := w;
      PicHeight := h;
    end
    else
    begin
      PicWidth  := ImgWidth;
      PicHeight := ImgHeight;
    end;
  end;

  Result := Rect(0, 0, PicWidth, PicHeight);

  if Center then
    OffsetRect(Result, (ImgWidth - PicWidth) div 2, (ImgHeight - PicHeight) div 2);
end;

function TBGRASpriteAnimation.DoCalculatePosition(AValue: integer): integer;
begin
  if FAnimInvert then
    Result := -AValue * (FSpriteCount - FAnimPosition)
  else
    Result := -AValue * (FAnimPosition - 1);
end;

function TBGRASpriteAnimation.DoCalculateSize(AValue: cardinal): cardinal;
begin
  Result := trunc(AValue div FSpriteCount);
end;

procedure TBGRASpriteAnimation.DoSpriteResampleFilter(ABitmap: TBGRABitmap);
begin
  ABitmap.ResampleFilter := FSpriteResampleFilter;
end;

procedure TBGRASpriteAnimation.DoSpriteFillOpacity(ABitmap: TBGRABitmap);
begin
  if FSpriteFillOpacity <> 255 then
    ABitmap.ApplyGlobalOpacity(FSpriteFillOpacity);
end;

procedure TBGRASpriteAnimation.DoSpriteFlip(ABitmap: TBGRABitmap);
begin
  case FSpriteFlipMode of
    flNone: Exit;
    flHorizontal: ABitmap.HorizontalFlip;
    flVertical: ABitmap.VerticalFlip;
    flBoth:
    begin
      ABitmap.HorizontalFlip;
      ABitmap.VerticalFlip;
    end;
  end;
end;

procedure TBGRASpriteAnimation.DoSpriteKeyColor(ABitmap: TBGRABitmap);
begin
  if FSpriteKeyColor <> clNone then
    ABitmap.ReplaceColor(ColorToBGRA(ColorToRGB(FSpriteKeyColor), 255),
      BGRAPixelTransparent);
end;

{ Main }

procedure TBGRASpriteAnimation.Paint;

  procedure DrawFrame;
  begin
    with inherited Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Style := psDash;
      MoveTo(0, 0);
      LineTo(Self.Width - 1, 0);
      LineTo(Self.Width - 1, Self.Height - 1);
      LineTo(0, Self.Height - 1);
      LineTo(0, 0);
    end;
  end;

var
  TempSprite, TempSpriteBGRA: TBGRABitmap;
  TempSpriteWidth, TempSpriteHeight, TempSpritePosition: integer;
begin
  if csDesigning in ComponentState then
    DrawFrame;

  if FSprite = nil then
    Exit;

  if (Width > 0) and (Height > 0) then
  begin
    TempSpriteWidth  := DoCalculateSize(FSprite.Width);
    TempSpriteHeight := FSprite.Height;
    TempSpritePosition := DoCalculatePosition(TempSpriteWidth);

    TempSpriteBGRA := TBGRABitmap.Create(FSprite);
    TempSprite := TBGRABitmap.Create(TempSpriteWidth, TempSpriteHeight);
    TempSprite.BlendImage(TempSpritePosition, 0, TempSpriteBGRA, boLinearBlend);
    TempSpriteBGRA.Free;

    if Assigned(FOnRedrawBefore) then
      FOnRedrawBefore(Self, TempSprite);

    DoSpriteDraw(TempSprite);
  end;
end;

function TBGRASpriteAnimation.AnimatedGifToSprite(Filename: string): TBGRABitmap;
var
  TempGif: TBGRAAnimatedGif;
  TempBitmap: TBGRABitmap;
  n: integer;
begin
  TempGif := TBGRAAnimatedGif.Create(Filename);
  TempBitmap := TBGRABitmap.Create(TempGif.Width * TempGif.Count, TempGif.Height);

  for n := 0 to TempGif.Count do
  begin
    TempGif.CurrentImage := n;
    TempBitmap.BlendImage(TempGif.Width * n, 0, TempGif.MemBitmap, boLinearBlend);
  end;
  TempGif.Free;

  Result := TempBitmap;
end;

procedure TBGRASpriteAnimation.DoSpriteDraw(ABitmap: TBGRABitmap);
var
  TempRect: TRect;
begin
  DoSpriteResampleFilter(ABitmap);
  DoSpriteKeyColor(ABitmap);
  DoSpriteFillOpacity(ABitmap);
  DoSpriteFlip(ABitmap);

  case FSpriteRotation of
    rtClockWise: BGRAReplace(ABitmap, ABitmap.RotateCW);
    rtCounterClockWise: BGRAReplace(ABitmap, ABitmap.RotateCCW);
  end;

  { TODO -oLainz : If there is no Sprite loaded and you set 'Tile' to true a division by cero error is shown }
  if Tile then
    BGRAReplace(ABitmap, ABitmap.GetPart(rect(0, 0, Width, Height)));

  TempRect := DoCalculateDestRect(ABitmap.Width, ABitmap.Height);

  if Assigned(FOnRedrawAfter) then
    FOnRedrawAfter(Self, ABitmap);

  if Stretch and (FSpriteResampleMode = rmFineResample) then
    BGRAReplace(ABitmap, ABitmap.Resample(Width, Height, FSpriteResampleMode));

  ABitmap.Draw(Canvas, TempRect, False);
  ABitmap.Free;
end;

procedure TBGRASpriteAnimation.DoAnimTimerOnTimer(Sender: TObject);
begin
  Invalidate;

  if Assigned(FOnPositionChanging) then
    FOnPositionChanging(Self);
  SetFAnimPosition(FAnimPosition + 1);

  if FAnimPosition = FSpriteCount then
  begin
    if Assigned(FOnLapChanging) then
      FOnLapChanging(Self);
    SetFAnimRepeatLap(FAnimRepeatLap + 1);
  end;
end;

{ Create / Destroy }

constructor TBGRASpriteAnimation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  FAnimInvert := False;
  FAnimPosition := 1;
  FAnimRepeat := 0;
  FAnimRepeatLap := 0;
  FAnimSpeed := 1000;
  FAnimStatic := False;
  FAnimTimer := TTimer.Create(Self);
  FAnimTimer.Interval := FAnimSpeed;
  FAnimTimer.OnTimer := @DoAnimTimerOnTimer;
  FAutoSize := False;
  FCenter := True;
  FProportional := True;
  FStretch := True;
  FSprite := TBitmap.Create;
  FSpriteCount := 1;
  FSpriteFillOpacity := 255;
  FSpriteFlipMode := flNone;
  FSpriteKeyColor := clNone;
  FSpriteResampleFilter := rfLinear;
  FSpriteResampleMode := rmSimpleStretch;
  FSpriteRotation := rtNone;
  FTile := False;

  if csDesigning in ComponentState then
    FAnimTimer.Enabled := False;
end;

destructor TBGRASpriteAnimation.Destroy;
begin
  FAnimTimer.OnTimer := nil;
  FAnimTimer.Free;
  FSprite.Free;
  inherited Destroy;
end;

end.
