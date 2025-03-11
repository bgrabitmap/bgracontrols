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
- FreeMan35

***************************** END CONTRIBUTOR(S) *****************************}
unit BGRASpriteAnimation;

{$I bgracontrols.inc}

interface

uses
  Types, Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics,
  {$IFDEF FPC}
  LCLIntF, LResources,
  {$ELSE}
  BGRAGraphics, GraphType, FPImage,
  {$ENDIF}
  BCBaseCtrls, BGRABitmap, BGRABitmapTypes, BCTypes, BGRAAnimatedGif;

type
  TBGRASpriteAnimation = class;

  { TSpriteBitmap }

  TSpriteBitmap = class(TBitmap)
  private
    FOwner: TBGRASpriteAnimation;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TBGRASpriteAnimation); overload;
    procedure Assign(Source: TPersistent); override;
  end;

  TFlipMode = (flNone, flHorizontal, flVertical, flBoth);
  TRotationMode = (rtNone, rtClockWise, rtCounterClockWise, rt180);

  { TBGRASpriteAnimation }

  TBGRASpriteAnimation = class(TBGRAGraphicCtrl)
  private
    { Private declarations }
    FAnimInvert: boolean;
    FAnimPosition: cardinal;
    FAnimRepeat: cardinal;
    FAnimRepeatLap: cardinal;
    FAnimSpeed: cardinal;
    FAnimStatic: boolean;
    FAnimTimer: TTimer;
    FCenter: boolean;
    FOnLapChanged: TNotifyEvent;
    FOnLapChanging: TNotifyEvent;
    FOnPositionChanged: TNotifyEvent;
    FOnPositionChanging: TNotifyEvent;
    FOnRedrawAfter: TBGRARedrawEvent;
    FOnRedrawBefore: TBGRARedrawEvent;
    FProportional: boolean;
    FSprite: TBitmap;
    FSpriteCount: cardinal;
    FSpriteFillOpacity: byte;
    FSpriteFlipMode: TFlipMode;
    FSpriteKeyColor: TColor;
    FSpriteResampleFilter: TResampleFilter;
    FSpriteResampleMode: TResampleMode;
    FSpriteRotation: TRotationMode;
    FStretch: boolean;
    FTile: boolean;
    function DoCalculateDestRect(AWidth, AHeight: integer): TRect;
    function DoCalculatePosition(AValue: integer): integer;
    function DoCalculateSize(AValue: cardinal): cardinal;
    procedure DoAnimTimerOnTimer({%H-}Sender: TObject);
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
    procedure SpriteChange(Sender: TObject);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      {%H-}WithThemeSpace: Boolean); override;
  public
    { Public declarations }
    procedure GifImageToSprite(Gif: TBGRAAnimatedGif);
    procedure SpriteToGifImage(Gif: TBGRAAnimatedGif);
    procedure LoadFromResourceName(Instance: THandle; const ResName: string); overload;
    procedure LoadFromBitmapResource(const Resource: string); overload;
    {$IF BGRABitmapVersion > 11030100}
    procedure LoadFromBitmapStream(AStream: TStream);
    {$ENDIF}
    procedure LoadFromBGRABitmap(const BGRA: TBGRABitmap);
    procedure SpriteToAnimatedGif(Filename: string);
    procedure AnimatedGifToSprite(Filename: string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property AnimInvert: boolean read FAnimInvert write SetFAnimInvert;
    property AnimPosition: cardinal read FAnimPosition write SetFAnimPosition;
    property AnimRepeat: cardinal read FAnimRepeat write SetFAnimRepeat;
    property AnimRepeatLap: cardinal read FAnimRepeatLap write SetFAnimRepeatLap;
    property AnimSpeed: cardinal read FAnimSpeed write SetFAnimSpeed;
    property AnimStatic: boolean read FAnimStatic write SetFAnimStatic;
    property Center: boolean read FCenter write SetFCenter;
    property Proportional: boolean read FProportional write SetFProportional;
    property Sprite: TBitmap read FSprite write SetFSprite;
    property SpriteCount: cardinal read FSpriteCount write SetFSpriteCount;
    property SpriteFillOpacity: byte read FSpriteFillOpacity write SetFSpriteFillOpacity;
    property SpriteFlipMode: TFlipMode read FSpriteFlipMode write SetFSpriteFlipMode;
    property SpriteKeyColor: TColor read FSpriteKeyColor write SetFSpriteKeyColor;
    property SpriteResampleFilter: TResampleFilter
      read FSpriteResampleFilter write SetFSpriteResampleFilter;
    property SpriteResampleMode: TResampleMode
      read FSpriteResampleMode write SetFSpriteResampleMode;
    property SpriteRotation: TRotationMode read FSpriteRotation write SetFSpriteRotation;
    property Stretch: boolean read FStretch write SetFStretch;
    property Tile: boolean read FTile write SetFTile;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property Caption;
    property Color;
    property Enabled;
    property OnClick;
    property OnDblClick;
    property OnLapChanged: TNotifyEvent read FOnLapChanged write FOnLapChanged;
    property OnLapChanging: TNotifyEvent read FOnLapChanging write FOnLapChanging;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPositionChanged: TNotifyEvent
      read FOnPositionChanged write FOnPositionChanged;
    property OnPositionChanging: TNotifyEvent
      read FOnPositionChanging write FOnPositionChanging;
    property OnRedrawAfter: TBGRARedrawEvent read FOnRedrawAfter write FOnRedrawAfter;
    property OnRedrawBefore: TBGRARedrawEvent read FOnRedrawBefore write FOnRedrawBefore;
    property PopupMenu;
    property Visible;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

{$IFDEF FPC}
procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBGRASpriteAnimation]);
end;

{ TSpriteBitmap }

procedure TSpriteBitmap.AssignTo(Dest: TPersistent);
begin
  if Dest is TBGRAAnimatedGif then
    FOwner.SpriteToGifImage(TBGRAAnimatedGif(Dest));
  inherited AssignTo(Dest);
end;

constructor TSpriteBitmap.Create(AOwner: TBGRASpriteAnimation);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TSpriteBitmap.Assign(Source: TPersistent);
begin
  if Source is TBGRAAnimatedGif then
    FOwner.GifImageToSprite(TBGRAAnimatedGif(Source))
  else
    inherited Assign(Source);
end;

{$ENDIF}

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

  FSprite.Assign(AValue);
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
  InvalidatePreferredSize;
  AdjustSize;
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
  InvalidatePreferredSize;
  AdjustSize;
end;

{ General Variables }

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

procedure TBGRASpriteAnimation.SpriteChange(Sender: TObject);
begin
  Invalidate;
  InvalidatePreferredSize;
  AdjustSize;
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
  PicWidth := AWidth;
  PicHeight := AHeight;
  ImgWidth := ClientWidth;
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
      PicWidth := w;
      PicHeight := h;
    end
    else
    begin
      PicWidth := ImgWidth;
      PicHeight := ImgHeight;
    end;
  end;

  Result := Rect(0, 0, PicWidth, PicHeight);

  if Center then
    Types.OffsetRect(Result, (ImgWidth - PicWidth) div 2, (ImgHeight - PicHeight) div 2);
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
  Result := AValue div FSpriteCount;
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
      Pen.Style := graphics.psDash;
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
  if (Color <> clNone) and (Color <> clDefault) then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(ClientRect);
  end;

  if csDesigning in ComponentState then
    DrawFrame;

  if FSprite = nil then
    Exit;

  if (Width > 0) and (Height > 0) then
  begin
    TempSpriteWidth := DoCalculateSize(FSprite.Width);
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

procedure TBGRASpriteAnimation.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  if SpriteRotation in [rtClockWise,rtCounterClockWise] then
  begin
    PreferredWidth := Sprite.Height;
    PreferredHeight := Sprite.Width div SpriteCount;
  end else
  begin
    PreferredWidth := Sprite.Width div SpriteCount;
    PreferredHeight := Sprite.Height;
  end;
end;

procedure TBGRASpriteAnimation.GifImageToSprite(Gif: TBGRAAnimatedGif);
{$IF BGRABitmapVersion > 11030100}
var
  TempBitmap: TBGRABitmap;
  n: integer;
begin
  if Gif.Count = 0 then exit;
  TempBitmap := TBGRABitmap.Create(Gif.Width * Gif.Count, Gif.Height);
  try
    for n := 0 to Gif.Count-1 do
    begin
      Gif.CurrentImage := n;
      TempBitmap.PutImage(Gif.Width * n, 0, Gif.MemBitmap, dmSet);
    end;
    TempBitmap.AssignToBitmap(FSprite);
    SpriteCount := Gif.Count;
    AnimSpeed := Gif.TotalAnimationTimeMs div Gif.Count;
  finally
    TempBitmap.Free;
  end;
{$ELSE}
var
  TempBitmap: TBGRABitmap;
  n: integer;
begin
  if Gif.Count = 0 then exit;

  TempBitmap := TBGRABitmap.Create(Gif.Width * Gif.Count, Gif.Height);
  for n := 0 to Gif.Count do
  begin
    Gif.CurrentImage := n;
    TempBitmap.BlendImage(Gif.Width * n, 0, Gif.MemBitmap, boLinearBlend);
  end;

  AnimSpeed := Gif.TotalAnimationTimeMs div Gif.Count;
  FSpriteCount := Gif.Count;
  FSprite.Width := Gif.Width * Gif.Count;
  FSprite.Height := Gif.Height;
  FSprite.Canvas.Brush.Color := SpriteKeyColor;
  FSprite.Canvas.FillRect(Rect(0, 0, FSprite.Width, FSprite.Height));
  FSprite.Canvas.Draw(0, 0, TempBitmap.Bitmap);
  TempBitmap.Free;
{$ENDIF}
end;

procedure TBGRASpriteAnimation.SpriteToGifImage(Gif: TBGRAAnimatedGif);
var
  i: integer;
  TempSpriteWidth: Integer;
  TempSpritePosition: Integer;
  TempSpriteBGRA, TempSprite: TBGRABitmap;
begin
  gif.Clear;
  if AnimRepeat > high(Word) then
    gif.LoopCount := 0
  else
    gif.LoopCount := AnimRepeat;
  TempSpriteBGRA := TBGRABitmap.Create(FSprite);
  TempSpriteWidth := TempSpriteBGRA.Width div FSpriteCount;
  gif.SetSize(TempSpriteWidth, TempSpriteBGRA.Height);
  for i:=0 to FSpriteCount-1 do
  begin
    TempSpritePosition := -TempSpriteWidth * i;
    TempSprite := TBGRABitmap.Create(TempSpriteWidth, TempSpriteBGRA.Height);
    TempSprite.BlendImage(TempSpritePosition, 0, TempSpriteBGRA, boLinearBlend);
    gif.AddFullFrame(TempSprite, FAnimSpeed);
    TempSprite.Free;
  end;
  TempSpriteBGRA.Free;
end;

procedure TBGRASpriteAnimation.LoadFromResourceName(Instance: THandle;
  const ResName: string);
var
  TempGif: TBGRAAnimatedGif;
begin
  TempGif := TBGRAAnimatedGif.Create;
  {$IFDEF FPC}//#
  TempGif.LoadFromResourceName(Instance, ResName);
  {$ENDIF}
  GifImageToSprite(TempGif);

  TempGif.Free;
end;

procedure TBGRASpriteAnimation.LoadFromBitmapResource(const Resource: string);
{$IF BGRABitmapVersion > 11030100}
var
  stream: TStream;
begin
  stream := BGRAResource.GetResourceStream(Resource);
  try
    LoadFromBitmapStream(stream);
  finally
    stream.Free;
  end;
{$ELSE}
var
  tempGif: TBGRAAnimatedGif;
begin
  tempGif := TBGRAAnimatedGif.Create;
  try
    tempGif.LoadFromResource(Resource);
    GifImageToSprite(tempGif);
  finally
    tempGif.Free;
  end;
{$ENDIF}
end;

{$IF BGRABitmapVersion > 11030100}
procedure TBGRASpriteAnimation.LoadFromBitmapStream(AStream: TStream);
var
  tempGif: TBGRAAnimatedGif;
  tempBGRA: TBGRABitmap;
begin
  if DetectFileFormat(AStream) = ifGif then
  begin
    tempGif := TBGRAAnimatedGif.Create;
    try
      tempGif.LoadFromStream(AStream);
      GifImageToSprite(tempGif);
    finally
      tempGif.Free;
    end;
  end else
  begin
    tempBGRA := TBGRABitmap.Create;
    try
      tempBGRA.LoadFromStream(AStream);
      tempBGRA.AssignToBitmap(FSprite);
    finally
      tempBGRA.FRee;
    end;
  end;
end;
{$ENDIF}

procedure TBGRASpriteAnimation.LoadFromBGRABitmap(const BGRA: TBGRABitmap);
begin
  {$IF BGRABitmapVersion > 11030100}
  BGRA.AssignToBitmap(FSprite);
  {$ELSE}
  FSprite.Width := BGRA.Width;
  FSprite.Height := BGRA.Height;
  BGRA.Draw(FSprite.Canvas, 0, 0, False);
  {$ENDIF}
end;

procedure TBGRASpriteAnimation.SpriteToAnimatedGif(Filename: string);
var
  gif : TBGRAAnimatedGif;
begin
  gif := TBGRAAnimatedGif.Create;
  SpriteToGifImage(Gif);
  gif.SaveToFile(Filename);
  gif.Free;
end;

procedure TBGRASpriteAnimation.AnimatedGifToSprite(Filename: string);
var
  TempGif: TBGRAAnimatedGif;
begin
  TempGif := TBGRAAnimatedGif.Create(Filename);
  try
    GifImageToSprite(TempGif);
  finally
    TempGif.Free;
  end;
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
    rt180: ABitmap.RotateUDInplace;
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
  FAnimTimer.OnTimer := DoAnimTimerOnTimer;
  FCenter := True;
  FProportional := True;
  FStretch := True;
  FSprite := TSpriteBitmap.Create(self);
  FSprite.OnChange:=SpriteChange;
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
  FAnimTimer.Enabled := False;
  FAnimTimer.OnTimer := nil;
  FAnimTimer.Free;
  FSprite.Free;
  inherited Destroy;
end;

end.
