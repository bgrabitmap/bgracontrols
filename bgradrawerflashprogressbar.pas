unit BGRADrawerFlashProgressBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, {$IFDEF BGRABITMAP_USE_MSEGUI} mclasses, {$ENDIF} SysUtils, Types, BGRABitmap, BGRABitmapTypes, BGRAGraphics, BGRAGradients,
  Math, fptimer;

type
  TBGRAPBarStyle = (pbstNormal, pbstMultiProgress, pbstMarquee);
  TBGRAPBarMarqueeMode = (pbmmToLeft, pbmmToRight);
  TBGRAPBarMarqueeSpeed = (pbmsSlow, pbmsMedium, pbmsFast);

  TBGRAProgressBarRedrawEvent = procedure(Sender: TObject; Bitmap: TBGRABitmap; xpos: integer) of object;

  { TBGRADrawerFlashProgressBar }

  TBGRADrawerFlashProgressBar = class(TPersistent)
  private
    FBackgroundColor: TColor;
    FBackgroundRandomize: boolean;
    FBackgroundRandomizeMaxIntensity: word;
    FBackgroundRandomizeMinIntensity: word;
    FBarColor,
    FBarColorM: TColor;
    FMarqueeMode: TBGRAPBarMarqueeMode;
    FMarqueeSpeed: TBGRAPBarMarqueeSpeed;
    FMarqueeWidth: Word;
    FMaxValue: integer;
    FMinValue: integer;
    FOnChange: TNotifyEvent;
    FRandSeed: integer;
    FStyle: TBGRAPBarStyle;
    FValue,
    FValueM: integer;
    xpos: integer;
    marqueeTimer: TFPTimer;
    marqueeLeft,
    marqueeRight: Integer;

    procedure SetBackgroundRandomize(AValue: boolean);
    procedure SetBackgroundRandomizeMaxIntensity(AValue: word);
    procedure SetBackgroundRandomizeMinIntensity(AValue: word);
    procedure SetBarColor(AValue: TColor);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetBarColorM(AValue: TColor);
    procedure SetMarqueeMode(AValue: TBGRAPBarMarqueeMode);
    procedure SetMarqueeSpeed(AValue: TBGRAPBarMarqueeSpeed);
    procedure SetMarqueeWidth(AValue: Word);
    procedure SetMaxValue(AValue: integer);
    procedure SetMinValue(AValue: integer);
    procedure SetRandSeed(AValue: integer);
    procedure SetStyle(AValue: TBGRAPBarStyle);
    procedure SetValue(AValue: integer);
    procedure SetValueM(AValue: integer);

  protected
    marqueeCurMode: TBGRAPBarMarqueeMode;

    procedure MarqueeOnTimer(Sender: TObject);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Draw(ABitmap: TBGRABitmap);

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property RandSeed: integer read FRandSeed write SetRandSeed;
    property BarColor: TColor read FBarColor write SetBarColor;
    property BarColorM: TColor read FBarColorM write SetBarColorM;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property BackgroundRandomizeMinIntensity: word
      read FBackgroundRandomizeMinIntensity write SetBackgroundRandomizeMinIntensity;
    property BackgroundRandomizeMaxIntensity: word
      read FBackgroundRandomizeMaxIntensity write SetBackgroundRandomizeMaxIntensity;
    property BackgroundRandomize: boolean read FBackgroundRandomize
      write SetBackgroundRandomize;
    property XPosition: integer read xpos;
    property Style: TBGRAPBarStyle read FStyle write SetStyle default pbstNormal;
    property MarqueeWidth: Word read FMarqueeWidth write SetMarqueeWidth default 30;
    property MarqueeSpeed: TBGRAPBarMarqueeSpeed read FMarqueeSpeed write SetMarqueeSpeed default pbmsMedium;
    property MarqueeMode: TBGRAPBarMarqueeMode read FMarqueeMode write SetMarqueeMode default pbmmToRight;

    property MinValue: integer read FMinValue write SetMinValue;
    property MaxValue: integer read FMaxValue write SetMaxValue;
    property Value: integer read FValue write SetValue;
    property ValueM: integer read FValueM write SetValueM;
  end;

implementation

{ TBGRADrawerFlashProgressBar }

procedure TBGRADrawerFlashProgressBar.SetBarColor(AValue: TColor);
begin
  if FBarColor = AValue then
    Exit;
  FBarColor := AValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBGRADrawerFlashProgressBar.SetBackgroundRandomize(AValue: boolean);
begin
  if FBackgroundRandomize = AValue then
    Exit;
  FBackgroundRandomize := AValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBGRADrawerFlashProgressBar.SetBackgroundRandomizeMaxIntensity(AValue: word);
begin
  if FBackgroundRandomizeMaxIntensity = AValue then
    Exit;
  FBackgroundRandomizeMaxIntensity := AValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBGRADrawerFlashProgressBar.SetBackgroundRandomizeMinIntensity(AValue: word);
begin
  if FBackgroundRandomizeMinIntensity = AValue then
    Exit;
  FBackgroundRandomizeMinIntensity := AValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBGRADrawerFlashProgressBar.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor = AValue then
    Exit;
  FBackgroundColor := AValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBGRADrawerFlashProgressBar.SetBarColorM(AValue: TColor);
begin
  if FBarColorM = AValue then
    Exit;
  FBarColorM := AValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBGRADrawerFlashProgressBar.SetMarqueeMode(AValue: TBGRAPBarMarqueeMode);
begin
  if (FMarqueeMode <> AValue) then
  begin
    FMarqueeMode:= AValue;
    marqueeCurMode:= AValue;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TBGRADrawerFlashProgressBar.SetMarqueeSpeed(AValue: TBGRAPBarMarqueeSpeed);
begin
  FMarqueeSpeed:=AValue;
  case FMarqueeSpeed of
  pbmsSlow: marqueeTimer.Interval:= 50;
  pbmsMedium: marqueeTimer.Interval:= 20;
  pbmsFast: marqueeTimer.Interval:= 10;
  end;
end;

procedure TBGRADrawerFlashProgressBar.SetMarqueeWidth(AValue: Word);
begin
  if FMarqueeWidth=AValue then Exit;
  FMarqueeWidth:=AValue;
end;

procedure TBGRADrawerFlashProgressBar.SetMaxValue(AValue: integer);
begin
  if FMaxValue = AValue then
    exit;
  FMaxValue := AValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  if FMinValue > FMaxValue then
    FMinValue := FMaxValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBGRADrawerFlashProgressBar.SetMinValue(AValue: integer);
begin
  if FMinValue = AValue then
    exit;
  FMinValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FMaxValue < FMinValue then
    FMaxValue := FMinValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBGRADrawerFlashProgressBar.SetRandSeed(AValue: integer);
begin
  if FRandSeed = AValue then
    Exit;
  FRandSeed := AValue;
end;

procedure TBGRADrawerFlashProgressBar.SetStyle(AValue: TBGRAPBarStyle);
begin
  if (FStyle <> AValue) then
  begin
    if (AValue = pbstMarquee)
    then begin
           //if not(csDesigning in ComponentState) then
           marqueeLeft:= 0;
           marqueeTimer.Enabled:= True;
         end
    else marqueeTimer.Enabled:= False;

    FStyle:= AValue;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TBGRADrawerFlashProgressBar.SetValue(AValue: integer);
begin
  if FValue = AValue then
    exit;
  FValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBGRADrawerFlashProgressBar.SetValueM(AValue: integer);
begin
  if FValueM = AValue then
    exit;
  FValueM := AValue;
  if FValueM < FMinValue then
    FValueM := FMinValue;
  if FValueM > FValue then
    FValueM := FValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBGRADrawerFlashProgressBar.MarqueeOnTimer(Sender: TObject);
begin
  if (marqueeCurMode = pbmmToRight)
  then inc(marqueeLeft, 2)
  else dec(marqueeRight, 2);

  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TBGRADrawerFlashProgressBar.Create;
begin
  inherited Create;

  FStyle:= pbstNormal;
  FMarqueeWidth:= 30;
  FMarqueeSpeed:= pbmsMedium;
  FMarqueeMode:= pbmmToRight;
  marqueeCurMode:= pbmmToRight;
  marqueeLeft:= 0;
  marqueeRight:= 0;
  marqueeTimer:= TFPTimer.Create(nil);
  marqueeTimer.Enabled:= False;
  marqueeTimer.Interval:= 20;
  marqueeTimer.OnTimer:= @MarqueeOnTimer;
end;

destructor TBGRADrawerFlashProgressBar.Destroy;
begin
  marqueeTimer.Free;
  inherited Destroy;
end;

procedure TBGRADrawerFlashProgressBar.Draw(ABitmap: TBGRABitmap);
var
  content: TRect;
  y, tx, ty,
  marqueeOver: integer;
  bgColor: TBGRAPixel;

  function ApplyLightness(c: TBGRAPixel; lightness: word): TBGRAPixel;
  begin
    Result := GammaCompression(SetLightness(GammaExpansion(c), lightness));
  end;

  procedure DrawBar(bounds: TRect; AColor: TColor);
  var
    lCol: TBGRAPixel;
  begin
    lCol := AColor;

    DoubleGradientAlphaFill(ABitmap, bounds,
      ApplyLightness(lCol, 37000), ApplyLightness(lCol, 29000),
      ApplyLightness(lCol, 26000), ApplyLightness(lCol, 18000),
      gdVertical, gdVertical, gdVertical, 0.53);

    InflateRect(bounds, -1, -1);

    DoubleGradientAlphaFill(ABitmap, bounds,
      ApplyLightness(lCol, 28000), ApplyLightness(lCol, 22000),
      ApplyLightness(lCol, 19000), ApplyLightness(lCol, 11000),
      gdVertical, gdVertical, gdVertical, 0.53);
  end;

begin
  ABitmap.FillTransparent;
  tx := ABitmap.Width;
  ty := ABitmap.Height;

  ABitmap.Rectangle(0, 0, tx, ty, BGRA(255, 255, 255, 6), BackgroundColor, dmSet);
  if (tx > 2) and (ty > 2) then
    ABitmap.Rectangle(1, 1, tx - 1, ty - 1, BGRA(29, 29, 29), dmSet);

  if (tx > 4) and (ty > 4) then
  begin
    content  := Rect(2, 2, tx - 2, ty - 2);
    randseed := FRandSeed;
    if BackgroundRandomize then
    for y := content.Top to content.Bottom - 1 do
    begin
      bgColor := BackgroundColor;
      bgColor.Intensity := RandomRange(BackgroundRandomizeMinIntensity, BackgroundRandomizeMaxIntensity);
      ABitmap.HorizLine(content.Left, y, content.Right - 1, bgColor, dmSet);
    end;
    if tx >= 6 then
      ABitmap.DrawVertLine(content.Right - 1, content.Top, content.Bottom - 1,
        BGRA(0, 0, 0, 32));

    Case FStyle of
      pbstNormal: begin
        if FMaxValue > FMinValue then
        begin
          xpos := round((FValue - FMinValue) / (FMaxValue - FMinValue) *
                        (content.right - content.left)) + content.left;
          if xpos > content.left then
          begin
            DrawBar(rect(content.left, content.top, xpos, content.bottom), FBarColor);
            if xpos < content.right then
            begin
              ABitmap.SetPixel(xpos, content.top, BGRA(62, 62, 62));
              ABitmap.SetVertLine(xpos, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
            end;
          end;
        end;
      end;
      pbstMultiProgress: begin
        if FMaxValue > FMinValue then
        begin
          xpos := round((FValue - FMinValue) / (FMaxValue - FMinValue) *
                        (content.right - content.left)) + content.left;
          if xpos > content.left then
          begin
            DrawBar(rect(content.left, content.top, xpos, content.bottom), FBarColor);
            if xpos < content.right then
            begin
              ABitmap.SetPixel(xpos, content.top, BGRA(62, 62, 62));
              ABitmap.SetVertLine(xpos, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
            end;
          end;

          xpos := round((FValueM - FMinValue) / (FMaxValue - FMinValue) *
                        (content.right - content.left)) + content.left;
          if xpos > content.left then
          begin
            DrawBar(rect(content.left, content.top, xpos, content.bottom), FBarColorM);
            if xpos < content.right then
            begin
              ABitmap.SetPixel(xpos, content.top, BGRA(62, 62, 62));
              ABitmap.SetVertLine(xpos, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
            end;
          end;

        end;
      end;
      pbstMarquee: begin
        if (marqueeCurMode = pbmmToRight)
        then begin
               if (marqueeLeft >= tx-2) then marqueeLeft:= 2;

               marqueeRight:= marqueeLeft+(FMarqueeWidth-1);
               marqueeOver:= 0;

               if (marqueeRight > tx-2) then
               begin
                 marqueeOver:= marqueeRight-tx+4;
                 marqueeRight:= tx-2;
               end;

               DrawBar(rect(marqueeLeft, content.top, marqueeRight, content.bottom), FBarColor);
               ABitmap.SetPixel(marqueeLeft, content.top, BGRA(62, 62, 62));
               ABitmap.SetVertLine(marqueeLeft, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));

               if (marqueeOver = 0)
               then begin
                      ABitmap.SetPixel(marqueeRight, content.top, BGRA(62, 62, 62));
                      ABitmap.SetVertLine(marqueeRight, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
                    end
               else begin
                      DrawBar(rect(2, content.top, marqueeOver, content.bottom), FBarColor);
                      ABitmap.SetPixel(marqueeOver, content.top, BGRA(62, 62, 62));
                      ABitmap.SetVertLine(marqueeOver, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
                    end;
             end
        else begin
               if (marqueeRight < 2) then marqueeRight:= tx-2;

               marqueeLeft:= marqueeRight-(FMarqueeWidth-1);
               marqueeOver:= 0;

               if (marqueeRight < FMarqueeWidth) then
               begin
                 marqueeOver:= FMarqueeWidth-abs(marqueeLeft)+1;
                 marqueeLeft:= tx-2-abs(marqueeLeft);
               end;

               if (marqueeOver = 0)
               then begin
                      DrawBar(rect(marqueeLeft, content.top, marqueeRight, content.bottom), FBarColor);
                      ABitmap.SetPixel(marqueeLeft, content.top, BGRA(62, 62, 62));
                      ABitmap.SetVertLine(marqueeLeft, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
                      ABitmap.SetPixel(marqueeRight, content.top, BGRA(62, 62, 62));
                      ABitmap.SetVertLine(marqueeRight, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
                    end
               else begin
                      DrawBar(rect(2, content.top, marqueeOver, content.bottom), FBarColor);
                      ABitmap.SetPixel(marqueeOver, content.top, BGRA(62, 62, 62));
                      ABitmap.SetVertLine(marqueeOver, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
                      DrawBar(rect(marqueeLeft, content.top, tx-2, content.bottom), FBarColor);
                      ABitmap.SetPixel(marqueeLeft, content.top, BGRA(62, 62, 62));
                      ABitmap.SetVertLine(marqueeLeft, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
                    end;
             end;
      end;
    end;
  end;
end;

end.
