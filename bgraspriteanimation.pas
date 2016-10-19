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

unit BGRASpriteAnimation;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics, LCLIntF, LResources,
  BGRABitmap, BGRABitmapTypes, BCTypes, BGRAAnimatedGif;

type

  TFlipMode = (flNone, flHorizontal, flVertical, flBoth);
  TRotationMode = (rtNone, rtClockWise, rtCounterClockWise);

  { TBGRASpriteAnimation }

  TBGRASpriteAnimation = class(TGraphicControl)
  private
    { Private declarations }
    FAnimInvert: boolean;
    FAnimPosition: cardinal;
    FAnimRepeat: cardinal;
    FAnimRepeatLap: cardinal;
    FAnimSpeed: cardinal;
    FAnimStatic: boolean;
    FAnimTimer: TTimer;
    FAutoSize: boolean;
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
    procedure GifImageToSprite(Gif: TBGRAAnimatedGif);//FreeMan35 added
    procedure LoadFromResourceName(Instance: THandle; const ResName: string);
    //FreeMan35 added
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
    property AutoSize: boolean read FAutoSize write SetFAutoSize; // to be implemented
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
    property Caption;
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

procedure Register;

implementation

procedure Register;
begin
  {$I icons\bgraspriteanimation_icon.lrs}
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

procedure TBGRASpriteAnimation.SetFAutoSize(const AValue: boolean);
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

procedure TBGRASpriteAnimation.GifImageToSprite(Gif: TBGRAAnimatedGif);
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
  FSprite.Canvas.FillRect(0, 0, FSprite.Width, FSprite.Height);
  FSprite.Canvas.Draw(0, 0, TempBitmap.Bitmap);
  TempBitmap.Free;
end;

procedure TBGRASpriteAnimation.LoadFromResourceName(Instance: THandle;
  const ResName: string);
var
  TempGif: TBGRAAnimatedGif;
begin
  TempGif := TBGRAAnimatedGif.Create;
  TempGif.LoadFromResourceName(Instance, ResName);

  GifImageToSprite(TempGif);

  TempGif.Free;
end;

procedure TBGRASpriteAnimation.AnimatedGifToSprite(Filename: string);
var
  TempGif: TBGRAAnimatedGif;
begin
  TempGif := TBGRAAnimatedGif.Create(Filename);

  GifImageToSprite(TempGif);

  TempGif.Free;
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
