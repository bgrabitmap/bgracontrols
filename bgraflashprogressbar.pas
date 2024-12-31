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

- Massimo Magnano
    2024-12  Added Marquee and MultiProgress Style
             Added Caption, CaptionShowPercent, CaptionShowPercentAlign, CaptionShowPercentDigits;
             Changed Values to Double Type;
             Deleted Unit BGRADrawerFlashProgressBar;
             New Test with all Features
             Added Timer Style
***************************** END CONTRIBUTOR(S) *****************************}
unit BGRAFlashProgressBar;

{$I bgracontrols.inc}

interface

uses
  Classes, {$IFDEF BGRABITMAP_USE_MSEGUI} mclasses, {$ENDIF}
  SysUtils, Types, Forms, Controls, Graphics,
  {$IFDEF FPC} LResources, LMessages,
  {$ELSE} Messages, Windows, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCBaseCtrls, BGRABitmap, BGRABitmapTypes, BGRAGraphics, BGRAGradients,
  Math, fptimer;

type
  TBGRAPBarStyle = (pbstNormal, pbstMultiProgress, pbstMarquee, pbstTimer);
  TBGRAPBarMarqueeDirection = (pbmdToRight, pbmdToLeft);
  TBGRAPBarMarqueeSpeed = (pbmsSlow, pbmsMedium, pbmsFast);

  TBGRAProgressBarRedrawEvent = procedure(Sender: TObject; Bitmap: TBGRABitmap; xpos: integer) of object;

  { TBGRAFlashProgressBar }

  TBGRAFlashProgressBar = class(TBGRAGraphicCtrl)
  private
    FBGRA: TBGRABitmap;
    FCaptionPercentDigits: Integer;
    FCaptionPercentTimerFormat: String;
    FCaptionShowPercent: Boolean;
    FCaptionShowPercentAlign: TAlignment;
    FCaptionShowPercentAlignM: TAlignment;
    FCaptionShowPercentM: Boolean;
    FMarqueeBounce: Word;
    FOnRedraw: TBGRAProgressBarRedrawEvent;
    FBackgroundColor: TColor;
    FBackgroundRandomize: boolean;
    FBackgroundRandomizeMaxIntensity: word;
    FBackgroundRandomizeMinIntensity: word;
    FBarColor,
    FBarColorM: TColor;
    FMarqueeDirection: TBGRAPBarMarqueeDirection;
    FMarqueeSpeed: TBGRAPBarMarqueeSpeed;
    FMarqueeWidth,
    rMarqueeWidth: Word;
    FOnTimerTimer: TNotifyEvent;
    FTimerAutoRestart: Boolean;
    FOnTimerEnd: TNotifyEvent;
    FOnTimerStart: TNotifyEvent;
    FTimerInterval: Cardinal;
    FMaxValue,
    FMinValue,
    FValue,
    FValueM: Double;
    FOnChange: TNotifyEvent;
    FRandSeed: integer;
    FStyle: TBGRAPBarStyle;
    xpos: integer;
    internalTimer: TFPTimer;
    marqueeLeft,
    marqueeRight: Integer;

    procedure SetBackgroundRandomize(AValue: boolean);
    procedure SetBackgroundRandomizeMaxIntensity(AValue: word);
    procedure SetBackgroundRandomizeMinIntensity(AValue: word);
    procedure SetBarColor(AValue: TColor);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetBarColorM(AValue: TColor);
    procedure SetCaptionPercentDigits(AValue: Integer);
    procedure SetCaptionPercentTimerFormat(AValue: String);
    procedure SetCaptionShowPercent(AValue: Boolean);
    procedure SetCaptionShowPercentAlign(AValue: TAlignment);
    procedure SetCaptionShowPercentAlignM(AValue: TAlignment);
    procedure SetCaptionShowPercentM(AValue: Boolean);
    procedure SetMarqueeBounce(AValue: Word);
    procedure SetMarqueeDirection(AValue: TBGRAPBarMarqueeDirection);
    procedure SetMarqueeSpeed(AValue: TBGRAPBarMarqueeSpeed);
    procedure SetMarqueeWidth(AValue: Word);
    procedure SetMaxValue(AValue: Double);
    procedure SetMinValue(AValue: Double);
    procedure SetRandSeed(AValue: integer);
    procedure SetStyle(AValue: TBGRAPBarStyle);
    procedure SetTimerInterval(AValue: Cardinal);
    procedure SetValue(AValue: Double);
    procedure SetValueM(AValue: Double);

  protected
    marqueeCurMode: TBGRAPBarMarqueeDirection;

    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean); override;
    procedure DoOnResize; override;
    procedure WMEraseBkgnd(var Message: {$IFDEF FPC}TLMEraseBkgnd{$ELSE}TWMEraseBkgnd{$ENDIF}); message {$IFDEF FPC}LM_ERASEBKGND{$ELSE}WM_ERASEBKGND{$ENDIF};
    procedure Paint; override;
    procedure Loaded; override;
    procedure TextChanged; override;

    procedure TimerOnTimer(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Streaming }
    {$IFDEF FPC}
    procedure SaveToFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
    {$ENDIF}

    procedure Draw(ABitmap: TBGRABitmap);

    //Timer Methods applies only if Style is pbstTimer
    procedure TimerReStart;
    procedure TimerPlayPause;

    property XPosition: integer read xpos;

  published
    property Align;
    property Anchors;
    property Caption;
    property CaptionShowPercent: Boolean read FCaptionShowPercent write SetCaptionShowPercent default False;
    property CaptionShowPercentAlign: TAlignment read FCaptionShowPercentAlign write SetCaptionShowPercentAlign default taCenter;
    property CaptionShowPercentM: Boolean read FCaptionShowPercentM write SetCaptionShowPercentM default False;
    property CaptionShowPercentAlignM: TAlignment read FCaptionShowPercentAlignM write SetCaptionShowPercentAlignM default taLeftJustify;
    property CaptionPercentDigits: Integer read FCaptionPercentDigits write SetCaptionPercentDigits default 0;
    property CaptionPercentTimerFormat: String read FCaptionPercentTimerFormat write SetCaptionPercentTimerFormat;
    property Font;
    property ParentFont;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property Value: Double read FValue write SetValue;
    property ValueM: Double read FValueM write SetValueM;
    property Color; deprecated 'User BarColor instead';
    property RandSeed: integer read FRandSeed write SetRandSeed;
    property BarColor: TColor read FBarColor write SetBarColor;
    property BarColorM: TColor read FBarColorM write SetBarColorM;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property BackgroundRandomizeMinIntensity: Word read FBackgroundRandomizeMinIntensity write SetBackgroundRandomizeMinIntensity;
    property BackgroundRandomizeMaxIntensity: Word read FBackgroundRandomizeMaxIntensity write SetBackgroundRandomizeMaxIntensity;
    property BackgroundRandomize: Boolean read FBackgroundRandomize write SetBackgroundRandomize;
    property Style: TBGRAPBarStyle read FStyle write SetStyle default pbstNormal;
    property MarqueeWidth: Word read FMarqueeWidth write SetMarqueeWidth default 0;
    property MarqueeSpeed: TBGRAPBarMarqueeSpeed read FMarqueeSpeed write SetMarqueeSpeed default pbmsMedium;
    property MarqueeDirection: TBGRAPBarMarqueeDirection read FMarqueeDirection write SetMarqueeDirection default pbmdToRight;

    { #todo 5 -oMaxM : I'm implementing this in the new year }
    property MarqueeBounce: Word read FMarqueeBounce write SetMarqueeBounce;

    property TimerInterval: Cardinal read FTimerInterval write SetTimerInterval default 100;
    property TimerAutoRestart: Boolean read FTimerAutoRestart write FTimerAutoRestart default True;

    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnRedraw: TBGRAProgressBarRedrawEvent read FOnRedraw write FOnRedraw;
    property OnTimerStart: TNotifyEvent read FOnTimerStart write FOnTimerStart;
    property OnTimerEnd: TNotifyEvent read FOnTimerEnd write FOnTimerEnd;
    property OnTimerTimer: TNotifyEvent read FOnTimerTimer write FOnTimerTimer;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

uses DateUtils, BGRATextFX;

{$IFDEF FPC}
procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBGRAFlashProgressBar]);
end;
{$ENDIF}

{ TBGRAFlashProgressBar }

procedure TBGRAFlashProgressBar.SetBarColor(AValue: TColor);
begin
  if FBarColor = AValue then exit;
  FBarColor := AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetBackgroundRandomize(AValue: boolean);
begin
  if FBackgroundRandomize = AValue then exit;
  FBackgroundRandomize := AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetBackgroundRandomizeMaxIntensity(AValue: word);
begin
  if FBackgroundRandomizeMaxIntensity = AValue then exit;
  FBackgroundRandomizeMaxIntensity := AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetBackgroundRandomizeMinIntensity(AValue: word);
begin
  if FBackgroundRandomizeMinIntensity = AValue then exit;
  FBackgroundRandomizeMinIntensity := AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor = AValue then exit;
  FBackgroundColor := AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetBarColorM(AValue: TColor);
begin
  if FBarColorM = AValue then exit;
  FBarColorM := AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetCaptionPercentDigits(AValue: Integer);
begin
  if FCaptionPercentDigits=AValue then Exit;
  FCaptionPercentDigits:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetCaptionPercentTimerFormat(AValue: String);
begin
  if FCaptionPercentTimerFormat=AValue then Exit;
  FCaptionPercentTimerFormat:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetCaptionShowPercent(AValue: Boolean);
begin
  if FCaptionShowPercent=AValue then Exit;
  FCaptionShowPercent:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetCaptionShowPercentAlign(AValue: TAlignment);
begin
  if FCaptionShowPercentAlign=AValue then Exit;
  FCaptionShowPercentAlign:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetCaptionShowPercentAlignM(AValue: TAlignment);
begin
  if FCaptionShowPercentAlignM=AValue then Exit;
  FCaptionShowPercentAlignM:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetCaptionShowPercentM(AValue: Boolean);
begin
  if FCaptionShowPercentM=AValue then Exit;
  FCaptionShowPercentM:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetMarqueeBounce(AValue: Word);
begin
  if FMarqueeBounce=AValue then Exit;
  FMarqueeBounce:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetMarqueeDirection(AValue: TBGRAPBarMarqueeDirection);
begin
  if (FMarqueeDirection <> AValue) then
  begin
    FMarqueeDirection:= AValue;
    marqueeCurMode:= AValue;

    if Assigned(FOnChange) then FOnChange(Self);
    Invalidate;
  end;
end;

procedure TBGRAFlashProgressBar.SetMarqueeSpeed(AValue: TBGRAPBarMarqueeSpeed);
begin
  FMarqueeSpeed:=AValue;
  case FMarqueeSpeed of
  pbmsSlow: internalTimer.Interval:= 50;
  pbmsMedium: internalTimer.Interval:= 20;
  pbmsFast: internalTimer.Interval:= 10;
  end;
end;

procedure TBGRAFlashProgressBar.SetMarqueeWidth(AValue: Word);
begin
  if FMarqueeWidth=AValue then Exit;
  FMarqueeWidth:= AValue;
  if (FMarqueeWidth = 0)
  then rMarqueeWidth:= Width div 4
  else rMarqueeWidth:= FMarqueeWidth;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetMaxValue(AValue: Double);
begin
  if FMaxValue = AValue then exit;

  FMaxValue := AValue;
  if (FValue > FMaxValue) then FValue := FMaxValue;
  if (FMinValue > FMaxValue) then FMinValue := FMaxValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetMinValue(AValue: Double);
begin
  if FMinValue = AValue then exit;

  FMinValue := AValue;
  if (FValue < FMinValue) then FValue := FMinValue;
  if (FMaxValue < FMinValue) then FMaxValue := FMinValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetRandSeed(AValue: integer);
begin
  if FRandSeed = AValue then exit;
  FRandSeed := AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetStyle(AValue: TBGRAPBarStyle);
begin
  if (FStyle <> AValue) then
  begin
    FStyle:= AValue;

    Case FStyle of
      pbstMarquee: begin
        SetMarqueeSpeed(FMarqueeSpeed);
        marqueeLeft:= 0;

        if not(csLoading in ComponentState) and
           not(csDesigning in ComponentState) then internalTimer.Enabled:= True;
      end;
      pbstTimer: begin
        FValue:= FMaxValue;
        internalTimer.Interval:= FTimerInterval;

        if FTimerAutoRestart and
           not(csLoading in ComponentState) and
           not(csDesigning in ComponentState) then internalTimer.Enabled:= True;
      end;
    else internalTimer.Enabled:= False;
    end;

    if Assigned(FOnChange) then FOnChange(Self);
    Invalidate;
  end;
end;

procedure TBGRAFlashProgressBar.SetTimerInterval(AValue: Cardinal);
begin
  if FTimerInterval=AValue then Exit;
  FTimerInterval:=AValue;

  if (FStyle = pbstTimer) then internalTimer.Interval:= AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetValue(AValue: Double);
begin
  if FValue = AValue then exit;

  FValue := AValue;
  if (FValue < FMinValue) then FValue := FMinValue;
  if (FValue > FMaxValue) then FValue := FMaxValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetValueM(AValue: Double);
begin
  if FValueM = AValue then exit;

  FValueM := AValue;
  if (FValueM < FMinValue) then FValueM := FMinValue;
  if (FValueM > FValue) then FValueM := FValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.TimerOnTimer(Sender: TObject);
begin
  Case FStyle of
    pbstMarquee: begin
      if (marqueeCurMode = pbmdToRight)
      then inc(marqueeLeft, 2)
      else dec(marqueeLeft, 2);
    end;
    pbstTimer: begin
      //FValue:= TTime(, FTimerInterval);
      FValue:= IncMilliSecond(FValue, -internalTimer.Interval);
      if (FValue <= 0)
      then begin
             if Assigned(FOnTimerEnd) then FOnTimerEnd(Self);

             if FTimerAutoRestart then FValue:= FMaxValue;
             internalTimer.Enabled:= FTimerAutoRestart;
           end
      else if Assigned(FOnTimerTimer) then FOnTimerTimer(Self);
    end;
  end;

  Invalidate;
end;

{$hints off}
procedure TBGRAFlashProgressBar.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
begin
  PreferredWidth  := 380;
  PreferredHeight := 33;
end;

procedure TBGRAFlashProgressBar.DoOnResize;
begin
  inherited DoOnResize;

  if (FMarqueeWidth = 0)
  then rMarqueeWidth:= Width div 4
  else rMarqueeWidth:= FMarqueeWidth;
end;

{$hints on}

procedure TBGRAFlashProgressBar.Paint;
begin
  if (ClientWidth <> FBGRA.Width) or (ClientHeight <> FBGRA.Height)
  then FBGRA.SetSize(ClientWidth, ClientHeight);

  Draw(FBGRA);

  if Assigned(OnRedraw) then OnRedraw(Self, FBGRA, {%H-}XPosition);

  FBGRA.Draw(Canvas, 0, 0, False);
end;

procedure TBGRAFlashProgressBar.Loaded;
begin
  inherited Loaded;

  Case FStyle of
    pbstMarquee: if not(csDesigning in ComponentState) then internalTimer.Enabled:= True;
    pbstTimer: begin
                 FValue:= FMaxValue;
                 internalTimer.Interval:= FTimerInterval;

                 if FTimerAutoRestart and not(csDesigning in ComponentState) then internalTimer.Enabled:= True;
               end;
    else internalTimer.Enabled:= False;
  end;
end;

procedure TBGRAFlashProgressBar.TextChanged;
begin
  Invalidate;
end;

{$hints off}
procedure TBGRAFlashProgressBar.WMEraseBkgnd(var Message: {$IFDEF FPC}TLMEraseBkgnd{$ELSE}TWMEraseBkgnd{$ENDIF});
begin
  //do nothing
end;
{$hints on}

constructor TBGRAFlashProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, 33);

  // Bitmap
  FBGRA := TBGRABitmap.Create(Width, Height);
  // Functionality
  FMinValue := 0;
  FMaxValue := 100;
  FValue := 30;
  FValueM := 10;
  // Functionality and Style
  Randomize;
  FRandSeed := RandSeed;
  FCaptionShowPercent:= False;
  FCaptionShowPercentAlign:= taCenter;
  FCaptionShowPercentAlignM:= taLeftJustify;
  FCaptionPercentDigits:= 0;
  Caption:= '';
  // Style
  FStyle:=pbstNormal;
  FBarColor := BGRA(102, 163, 226);
  FBarColorM := BGRA(240, 240, 15);
  FBackgroundColor := BGRA(47,47,47);
  FBackgroundRandomize := True;
  FBackgroundRandomizeMinIntensity := 4000;
  FBackgroundRandomizeMaxIntensity := 5000;

  //Marquee
  FMarqueeWidth:= 0; //AutoWidth
  rMarqueeWidth:= 95; //PreferredWidth div 4
  FMarqueeSpeed:= pbmsMedium;
  FMarqueeDirection:= pbmdToRight;
  marqueeCurMode:= pbmdToRight;
  marqueeLeft:= 0;
  marqueeRight:= 0;

  //Timer
  FTimerInterval:= 100;
  FTimerAutoRestart:= True;
  FCaptionPercentTimerFormat:= 'nn:ss.zzz';

  internalTimer:= TFPTimer.Create(nil);
  internalTimer.Enabled:= False;
  internalTimer.Interval:= 20;
  internalTimer.OnTimer:= TimerOnTimer;
end;

destructor TBGRAFlashProgressBar.Destroy;
begin
  internalTimer.Free;
  FBGRA.Free;

  inherited Destroy;
end;

{$IFDEF FPC}
procedure TBGRAFlashProgressBar.SaveToFile(AFileName: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    WriteComponentAsTextToStream(AStream, Self);
    AStream.SaveToFile(AFileName);
  finally
    AStream.Free;
  end;
end;

procedure TBGRAFlashProgressBar.LoadFromFile(AFileName: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFileName);
    ReadComponentFromTextStream(AStream, TComponent(Self), OnFindClass);
  finally
    AStream.Free;
  end;
end;

procedure TBGRAFlashProgressBar.OnFindClass(Reader: TReader;
  const AClassName: string; var ComponentClass: TComponentClass);
begin
  if CompareText(AClassName, 'TBGRAFlashProgressBar') = 0 then
    ComponentClass := TBGRAFlashProgressBar;
end;
{$ENDIF}

procedure TBGRAFlashProgressBar.Draw(ABitmap: TBGRABitmap);
var
  content: TRect;
  y, tx, ty,
  marqueeOver: integer;
  bgColor: TBGRAPixel;
  pStr: String;
  pValue: Double;

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

  procedure DrawText(ACaption: String; AAlign: TAlignment);
  var
     fx: TBGRATextEffect;

  begin
    try
       if (Font.Size=0)
       then fx:= TBGRATextEffect.Create(ACaption, Font.Name, ABitmap.Height div 2, True)
       else fx:= TBGRATextEffect.Create(ACaption, Font, True);

       Case AAlign of
         taLeftJustify: begin
           fx.DrawOutline(ABitmap, 4, ABitmap.Height div 5, BGRABlack, taLeftJustify);
           fx.Draw(ABitmap, 4, ABitmap.Height div 5, BGRAWhite, taLeftJustify);
         end;
         taRightJustify: begin
           fx.DrawOutline(ABitmap, tx-4, ABitmap.Height div 5, BGRABlack, taRightJustify);
           fx.Draw(ABitmap, tx-4, ABitmap.Height div 5, BGRAWhite, taRightJustify);
         end;
         taCenter: begin
           fx.DrawOutline(ABitmap, ABitmap.Width div 2, ABitmap.Height div 5, BGRABlack, taCenter);
           fx.Draw(ABitmap, ABitmap.Width div 2, ABitmap.Height div 5, BGRAWhite, taCenter);
         end;
       end;

    finally
      fx.Free;
    end;
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

            //Draw Value Text
            pStr:= '';
            if FCaptionShowPercent then
            begin
              pValue:= 100*(FValue - FMinValue)/FMaxValue;
              if (pValue <> 0) then pStr:= FloatToStrF(pValue, ffFixed, 15, FCaptionPercentDigits)+'%'
            end;
            DrawText(Caption+pStr, FCaptionShowPercentAlign);
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

          //Draw Value Text
          pStr:= '';
          if FCaptionShowPercent then
          begin
            pValue:= 100*(FValue - FMinValue)/FMaxValue;
            if (pValue <> 0) then pStr:= FloatToStrF(pValue, ffFixed, 15, FCaptionPercentDigits)+'%'
          end;
          DrawText(Caption+pStr, FCaptionShowPercentAlign);

          //Draw ValueM Text
          pStr:= '';
          if FCaptionShowPercentM then
          begin
            pValue:= 100*(FValueM - FMinValue)/FMaxValue;
            if (pValue <> 0) then pStr:= FloatToStrF(pValue, ffFixed, 15, FCaptionPercentDigits)+'%'
          end;
          DrawText(pStr, FCaptionShowPercentAlignM);
        end;
      end;
      pbstMarquee: begin
        if (marqueeCurMode = pbmdToRight)
        then begin
               //check if the whole bar is out put it back to the beginning
               if (marqueeLeft >= tx-2) then
               begin
                 marqueeLeft:= 2;
               end;

               //Calculate the Right
               marqueeRight:= marqueeLeft+(rMarqueeWidth-1);

               //Check if part of the bar is out calculate the visible piece on the left
               marqueeOver:= 0;
               if (marqueeRight > tx-2) then
               begin
                 marqueeOver:= marqueeRight-(tx-2);
               end;
             end
        else begin
               //check if the whole bar is out put it back to the end
               if (marqueeLeft <= -(rMarqueeWidth+2)) then
               begin
                 marqueeLeft:= tx-2-rMarqueeWidth;
               end;

               //Calculate the Right
               marqueeRight:= marqueeLeft+(rMarqueeWidth-1);

               //check if part of the bar is out then the visible piece on the left is equal to marqueeRight
               marqueeOver:= 0;
               if (marqueeRight < rMarqueeWidth) then
               begin
                 marqueeOver:= marqueeRight;
               end;
             end;

        if (marqueeOver = 0)
        then begin
               //Draw Normal Bar Left-Right
               DrawBar(rect(marqueeLeft, content.top, marqueeRight, content.bottom), FBarColor);
               ABitmap.SetPixel(marqueeLeft, content.top, BGRA(62, 62, 62));
               ABitmap.SetVertLine(marqueeLeft, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
               ABitmap.SetPixel(marqueeRight, content.top, BGRA(62, 62, 62));
               ABitmap.SetVertLine(marqueeRight, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
             end
        else begin
               //Draw visible piece on the Left
               DrawBar(rect(2, content.top, marqueeOver, content.bottom), FBarColor);
               ABitmap.SetPixel(marqueeOver, content.top, BGRA(62, 62, 62));
               ABitmap.SetVertLine(marqueeOver, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
               //Draw visible piece on the Right
               DrawBar(rect(tx-2-(rMarqueeWidth+1-marqueeOver), content.top, tx-2, content.bottom), FBarColor);
               ABitmap.SetPixel(tx-2-(rMarqueeWidth+1-marqueeOver), content.top, BGRA(62, 62, 62));
               ABitmap.SetVertLine(tx-2-(rMarqueeWidth+1-marqueeOver), content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
             end;

      end;
      pbstTimer: begin
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

            //Draw Timer Text
            pStr:= '';
            if FCaptionShowPercent then
            begin
              if (FValue <> 0) then pStr:= FormatDateTime(FCaptionPercentTimerFormat, FValue)
            end;
            DrawText(Caption+pStr, FCaptionShowPercentAlign);
          end;
        end;
      end;
    end;
  end;
end;

procedure TBGRAFlashProgressBar.TimerReStart;
begin
  if (FStyle = pbstTimer) then
  begin
    FValue:= FMaxValue;
    internalTimer.Interval:= FTimerInterval;
    internalTimer.Enabled:= True;
    Invalidate;

    if Assigned(FOnTimerStart) then FOnTimerStart(Self);
  end;
end;

procedure TBGRAFlashProgressBar.TimerPlayPause;
begin
  if (FStyle = pbstTimer) then
  begin
    internalTimer.Enabled:= not(internalTimer.Enabled);
    Invalidate;
  end;
end;

end.
