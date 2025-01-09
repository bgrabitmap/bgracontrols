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
    2025-01  Added Marquee Bounce and Stepit Method,
             TimerPlayPause works also for Marquee (useful for debugging)
             Added Graph Style and ShowDividers, Renamed MultiProgress properties
             Added ShowBarAnimation
***************************** END CONTRIBUTOR(S) *****************************}
unit BGRAFlashProgressBar;

{$I bgracontrols.inc}

interface

//{$define TESTS}

uses
  Classes, {$IFDEF BGRABITMAP_USE_MSEGUI} mclasses, {$ENDIF}
  SysUtils, Types, Forms, Controls, Graphics,
  {$IFDEF FPC} LResources, LMessages,
  {$ELSE} Messages, Windows, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCBaseCtrls, BGRABitmap, BGRABitmapTypes, BGRAGraphics, BGRAGradients,
  Math, fptimer;

type
  TBGRAPBarStyle = (pbstNormal, pbstMultiProgress, pbstMarquee, pbstTimer, pbstGraph);
  TBGRAPBarMarqueeDirection = (pbmdToRight, pbmdToLeft);
  TBGRAPBarMarqueeSpeed = (pbmsSlow, pbmsMedium, pbmsFast);

  TBGRAProgressBarRedrawEvent = procedure(Sender: TObject; Bitmap: TBGRABitmap; xpos: integer) of object;

  { TBGRAFlashProgressBar }

  TGraphValue = record
    XValue, YValue: Double;
  end;
  TGraphValues = array of TGraphValue;

  TBGRAFlashProgressBar = class(TBGRAGraphicCtrl)
  private
    procedure SetBackgroundRandomize(AValue: boolean);
    procedure SetBackgroundRandomizeMaxIntensity(AValue: word);
    procedure SetBackgroundRandomizeMinIntensity(AValue: word);
    procedure SetBarColor(AValue: TColor);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetBarColorSub(AValue: TColor);
    procedure SetCaptionPercentDigits(AValue: Integer);
    procedure SetCaptionPercentTimerFormat(AValue: String);
    procedure SetCaptionShowPercent(AValue: Boolean);
    procedure SetCaptionPercentAlign(AValue: TAlignment);
    procedure SetCaptionPercentSubAlign(AValue: TAlignment);
    procedure SetCaptionShowPercentSub(AValue: Boolean);
    procedure SetGraphShowYLine(AValue: Boolean);
    procedure SetGraphYLineAfter(AValue: String);
    procedure SetGraphYLineCaption(AValue: String);
    procedure SetGraphYLineDigits(AValue: Integer);
    procedure SetShowBarAnimation(AValue: Boolean);
    procedure SetShowDividers(AValue: Boolean);
    procedure SetMarqueeBounce(AValue: Word);
    procedure SetMarqueeDirection(AValue: TBGRAPBarMarqueeDirection);
    procedure SetMarqueeSpeed(AValue: TBGRAPBarMarqueeSpeed);
    procedure SetMarqueeWidth(AValue: Word);
    procedure SetMaxValue(AValue: Double);
    procedure SetMaxYValue(AValue: Double);
    procedure SetMinValue(AValue: Double);
    procedure SetMinYValue(AValue: Double);
    procedure SetRandSeed(AValue: integer);
    procedure SetGraphShowYDividers(AValue: Boolean);
    procedure SetStyle(AValue: TBGRAPBarStyle);
    procedure SetTimerInterval(AValue: Cardinal);
    procedure SetValueSub(AValue: Double);

  protected
    FBGRA: TBGRABitmap;
    FCaptionPercentDigits: Integer;
    FCaptionPercentTimerFormat: String;
    FCaptionShowPercent: Boolean;
    FCaptionPercentAlign: TAlignment;
    FCaptionPercentSubAlign: TAlignment;
    FCaptionShowPercentSub: Boolean;
    FMarqueeBounce: Word;
    FOnRedraw: TBGRAProgressBarRedrawEvent;
    FBackgroundColor: TColor;
    FBackgroundRandomize: boolean;
    FBackgroundRandomizeMaxIntensity: word;
    FBackgroundRandomizeMinIntensity: word;
    FShowDividers,
    FGraphShowYDividers: Boolean;
    FBarColor,
    FBarColorSub: TColor;
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
    FMinYValue,
    FMaxYValue,
    FValue,
    FValueSub: Double;
    FOnChange: TNotifyEvent;
    FRandSeed: integer;
    FStyle: TBGRAPBarStyle;
    FGraphShowYLine: Boolean;
    FGraphYLineAfter: String;
    FGraphYLineCaption: String;
    FGraphYLineDigits: Integer;
    FShowBarAnimation: Boolean;

    xpos,
    xposSub,
    marqueeLeft,
    marqueeRight,
    marqueeCount,
    marqueeBCount,
    barAnimLeft: Integer;
    marqueeWall,
    marqueeBouncing: Boolean;
    marqueeCurMode: TBGRAPBarMarqueeDirection;
    internalTimer: TFPTimer;
    closing: Boolean;
    GraphValues: TGraphValues;  //array of Real Graph Values
    GraphPoints: array of TPointF; //array of Calculated xpos and ypos

    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean); override;
    procedure DoOnResize; override;
    procedure WMEraseBkgnd(var Message: {$IFDEF FPC}TLMEraseBkgnd{$ELSE}TWMEraseBkgnd{$ENDIF}); message {$IFDEF FPC}LM_ERASEBKGND{$ELSE}WM_ERASEBKGND{$ENDIF};
    procedure Paint; override;
    procedure Loaded; override;
    procedure TextChanged; override;

    procedure TimerOnTimer(Sender: TObject);

  public
    {$ifdef TESTS}
    p1, p2:TPointF;
    pT: TGradientType;
    {$endif}

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

    procedure SetValue(AValue: Double); overload;

    //Set Current Value and it's Y Value in Graph Style
    procedure SetValue(AValue, AYValue: Double); overload;

    //Step It, if Style is pbstNormal then Inc/Dec Value,
    //         if pbstMarquee then do next Animation Step (AIncrement is ignored)
    //         if pbstTimer then Value is decremented of 100ms (AIncrement is ignored)
    procedure StepIt(AIncrement: Double);

    //Timer Restart applies only if Style is pbstTimer
    procedure TimerReStart;
    //Timer Play/Pause applies only if Style is pbstMarquee or pbstTimer
    procedure TimerPlayPause;

    property XPosition: integer read xpos;
    property XPositionSub: integer read xposSub;

  published
    property Align;
    property BorderSpacing;
    property Anchors;
    property Caption;
    property CaptionShowPercent: Boolean read FCaptionShowPercent write SetCaptionShowPercent default False;
    property CaptionPercentAlign: TAlignment read FCaptionPercentAlign write SetCaptionPercentAlign default taCenter;
    property CaptionShowPercentSub: Boolean read FCaptionShowPercentSub write SetCaptionShowPercentSub default False;
    property CaptionPercentSubAlign: TAlignment read FCaptionPercentSubAlign write SetCaptionPercentSubAlign default taLeftJustify;
    property CaptionPercentDigits: Integer read FCaptionPercentDigits write SetCaptionPercentDigits default 0;
    property CaptionPercentTimerFormat: String read FCaptionPercentTimerFormat write SetCaptionPercentTimerFormat;
    property Font;
    property ParentFont;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property MinYValue: Double read FMinYValue write SetMinYValue;
    property MaxYValue: Double read FMaxYValue write SetMaxYValue;
    property Value: Double read FValue write SetValue;
    property ValueSub: Double read FValueSub write SetValueSub;
    property Color; deprecated 'User BarColor instead';
    property RandSeed: integer read FRandSeed write SetRandSeed;
    property BarColor: TColor read FBarColor write SetBarColor;
    property BarColorSub: TColor read FBarColorSub write SetBarColorSub;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property BackgroundRandomizeMinIntensity: Word read FBackgroundRandomizeMinIntensity write SetBackgroundRandomizeMinIntensity;
    property BackgroundRandomizeMaxIntensity: Word read FBackgroundRandomizeMaxIntensity write SetBackgroundRandomizeMaxIntensity;
    property BackgroundRandomize: Boolean read FBackgroundRandomize write SetBackgroundRandomize;
    property ShowDividers: Boolean read FShowDividers write SetShowDividers default False;
    property ShowBarAnimation: Boolean read FShowBarAnimation write SetShowBarAnimation default False;
    property Style: TBGRAPBarStyle read FStyle write SetStyle default pbstNormal;
    property MarqueeWidth: Word read FMarqueeWidth write SetMarqueeWidth default 0;
    property MarqueeSpeed: TBGRAPBarMarqueeSpeed read FMarqueeSpeed write SetMarqueeSpeed default pbmsMedium;
    property MarqueeDirection: TBGRAPBarMarqueeDirection read FMarqueeDirection write SetMarqueeDirection default pbmdToRight;
    property MarqueeBounce: Word read FMarqueeBounce write SetMarqueeBounce;

    property TimerInterval: Cardinal read FTimerInterval write SetTimerInterval default 100;
    property TimerAutoRestart: Boolean read FTimerAutoRestart write FTimerAutoRestart default True;

    property GraphShowYDividers: Boolean read FGraphShowYDividers write SetGraphShowYDividers default False;
    property GraphShowYLine: Boolean read FGraphShowYLine write SetGraphShowYLine default False;
    property GraphYLineCaption: String read FGraphYLineCaption write SetGraphYLineCaption;
    property GraphYLineAfter: String read FGraphYLineAfter write SetGraphYLineAfter;
    property GraphYLineDigits: Integer read FGraphYLineDigits write SetGraphYLineDigits default 0;

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

const
  BAR_ANIM_TIMER = 20;
  BAR_ANIM_INC = 4;
  MARQUEE_TIMER_SLOW = 50;
  MARQUEE_TIMER_MED  = 20;
  MARQUEE_TIMER_FAST = 10;
  MARQUEE_INC = 2;

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

procedure TBGRAFlashProgressBar.SetBarColorSub(AValue: TColor);
begin
  if FBarColorSub = AValue then exit;
  FBarColorSub := AValue;

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

procedure TBGRAFlashProgressBar.SetCaptionPercentAlign(AValue: TAlignment);
begin
  if FCaptionPercentAlign=AValue then Exit;
  FCaptionPercentAlign:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetCaptionPercentSubAlign(AValue: TAlignment);
begin
  if FCaptionPercentSubAlign=AValue then Exit;
  FCaptionPercentSubAlign:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetCaptionShowPercentSub(AValue: Boolean);
begin
  if FCaptionShowPercentSub=AValue then Exit;
  FCaptionShowPercentSub:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetGraphShowYLine(AValue: Boolean);
begin
  if FGraphShowYLine=AValue then Exit;
  FGraphShowYLine:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetGraphYLineAfter(AValue: String);
begin
  if FGraphYLineAfter=AValue then Exit;
  FGraphYLineAfter:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetGraphYLineCaption(AValue: String);
begin
  if FGraphYLineCaption=AValue then Exit;
  FGraphYLineCaption:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetGraphYLineDigits(AValue: Integer);
begin
  if FGraphYLineDigits=AValue then Exit;
  FGraphYLineDigits:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetShowBarAnimation(AValue: Boolean);
begin
  if FShowBarAnimation=AValue then Exit;
  FShowBarAnimation:=AValue;

  if (FStyle in [pbstNormal, pbstMultiProgress, pbstGraph]) and
     not(csLoading in ComponentState) and
     not(csDesigning in ComponentState) then
  begin
    barAnimLeft:= 0;
    if FShowBarAnimation then internalTimer.Interval:= BAR_ANIM_TIMER;
    internalTimer.Enabled:= FShowBarAnimation;
  end;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetShowDividers(AValue: Boolean);
begin
  if FShowDividers=AValue then Exit;
  FShowDividers:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetMarqueeBounce(AValue: Word);
begin
  marqueeBCount:= AValue;
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
  pbmsSlow: internalTimer.Interval:= MARQUEE_TIMER_SLOW;
  pbmsMedium: internalTimer.Interval:= MARQUEE_TIMER_MED;
  pbmsFast: internalTimer.Interval:= MARQUEE_TIMER_FAST;
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

procedure TBGRAFlashProgressBar.SetMaxYValue(AValue: Double);
begin
  if FMaxYValue=AValue then Exit;
  FMaxYValue:=AValue;
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

procedure TBGRAFlashProgressBar.SetMinYValue(AValue: Double);
begin
  if FMinYValue=AValue then Exit;
  FMinYValue:=AValue;
end;

procedure TBGRAFlashProgressBar.SetRandSeed(AValue: integer);
begin
  if FRandSeed = AValue then exit;
  FRandSeed := AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetGraphShowYDividers(AValue: Boolean);
begin
  if FGraphShowYDividers=AValue then Exit;
  FGraphShowYDividers:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetStyle(AValue: TBGRAPBarStyle);
begin
  if (FStyle <> AValue) then
  begin
    FStyle:= AValue;

    Case FStyle of
      pbstNormal,
      pbstMultiProgress: begin
        if FShowBarAnimation and
           not(csLoading in ComponentState) and
           not(csDesigning in ComponentState)
        then begin
               barAnimLeft:= 0;
               internalTimer.Interval:= BAR_ANIM_TIMER;
               internalTimer.Enabled:= True;
             end
        else internalTimer.Enabled:= False;
      end;
      pbstMarquee: begin
        SetMarqueeSpeed(FMarqueeSpeed);

        if (FMarqueeDirection = pbmdToRight)
        then marqueeLeft:= 2
        else marqueeLeft:= -FMarqueeWidth;

        if FTimerAutoRestart and
           not(csLoading in ComponentState) and
           not(csDesigning in ComponentState) then internalTimer.Enabled:= True;
      end;
      pbstTimer: begin
        FValue:= FMaxValue;
        internalTimer.Interval:= FTimerInterval;

        if FTimerAutoRestart and
           not(csLoading in ComponentState) and
           not(csDesigning in ComponentState) then internalTimer.Enabled:= True;
      end;
      pbstGraph: begin
        //Save space for the 2 points to close the polygon
        if (Length(GraphPoints) < 2) then SetLength(GraphPoints, 2);

        if FShowBarAnimation and
           not(csLoading in ComponentState) and
           not(csDesigning in ComponentState)
        then begin
               internalTimer.Interval:= BAR_ANIM_TIMER;
               internalTimer.Enabled:= True;
             end
        else internalTimer.Enabled:= False;
      end;
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

procedure TBGRAFlashProgressBar.SetValueSub(AValue: Double);
begin
  if FValueSub = AValue then exit;

  FValueSub := AValue;
  if (FValueSub < FMinValue) then FValueSub := FMinValue;
  if (FValueSub > FValue) then FValueSub := FValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBGRAFlashProgressBar.TimerOnTimer(Sender: TObject);
begin
  try
  if closing then exit;

  Case FStyle of
    pbstNormal,
    pbstMultiProgress,
    pbstGraph: if FShowBarAnimation then begin
        inc(barAnimLeft, BAR_ANIM_INC);

        //Wait 16 times after reached the end
        if (barAnimLeft+18 > xpos) then barAnimLeft:= -16*BAR_ANIM_INC;
    end;
    pbstMarquee: begin
      if (FMarqueeBounce > 0) then
      begin
        if marqueeBouncing then
        begin
          if (marqueeCount = 0) //we've reached the rebound wall
          then begin
                 marqueeCount:= 3; //Set the bounce length (3*2pixels)

                 if (marqueeCurMode = pbmdToRight)
                 then marqueeCurMode:= pbmdToLeft
                 else marqueeCurMode:= pbmdToRight;

                 //decreases the rebound counter only if we are in the real wall
                 if marqueeWall then dec(marqueeBCount);

                 if (marqueeBCount > 0)
                 then marqueeBouncing:= True
                 else begin
                        //Stop Bouncing
                        if marqueeWall then marqueeBCount:= FMarqueeBounce;
                        marqueeBouncing:= False;
                      end;
               end
          else dec(marqueeCount);
        end;
      end;

      //Move the bar 2 pixels
      if (marqueeCurMode = pbmdToRight)
      then inc(marqueeLeft, MARQUEE_INC)
      else dec(marqueeLeft, MARQUEE_INC);
    end;
    pbstTimer: begin
      { #note -oMaxM : If we had to be more precise we should keep the Start time and subtract the current time }
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

  except
    //MaxM: Ignore Exception sometimes it happens when we are closing
  end;
end;

{$hints off}
procedure TBGRAFlashProgressBar.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
begin
  PreferredWidth  := 380;
  PreferredHeight := 34;
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
    pbstNormal,
    pbstMultiProgress,
    pbstGraph: begin
      if FShowBarAnimation then internalTimer.Interval:= BAR_ANIM_TIMER;
      internalTimer.Enabled:= FShowBarAnimation;
    end;
    pbstMarquee: begin
      if (FMarqueeDirection = pbmdToRight)
      then marqueeLeft:= 2
      else marqueeLeft:= -FMarqueeWidth;

      if FTimerAutoRestart and not(csDesigning in ComponentState) then internalTimer.Enabled:= True;
    end;
    pbstTimer: begin
      FValue:= FMaxValue;
      internalTimer.Interval:= FTimerInterval;

      if FTimerAutoRestart and not(csDesigning in ComponentState) then internalTimer.Enabled:= True;
    end;
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
  FValueSub := 10;
  xpos:= 0;
  xposSub:= 0;

  // Functionality and Style
  Randomize;
  FRandSeed := RandSeed;
  FCaptionShowPercent:= False;
  FCaptionPercentAlign:= taCenter;
  FCaptionPercentSubAlign:= taLeftJustify;
  FCaptionPercentDigits:= 0;
  Caption:= '';

  // Style
  FStyle:=pbstNormal;
  FBarColor := BGRA(102, 163, 226);
  FBarColorSub := BGRA(240, 240, 15);
  FBackgroundColor := BGRA(47,47,47);
  FBackgroundRandomize := True;
  FBackgroundRandomizeMinIntensity := 4000;
  FBackgroundRandomizeMaxIntensity := 5000;
  FShowDividers:= False;
  FGraphShowYDividers:= False;
  FShowBarAnimation:= False;
  barAnimLeft:= 0;

  //Marquee
  FMarqueeWidth:= 0; //AutoWidth
  rMarqueeWidth:= 95; //PreferredWidth div 4
  FMarqueeSpeed:= pbmsMedium;
  FMarqueeDirection:= pbmdToRight;
  marqueeCurMode:= pbmdToRight;
  marqueeLeft:= 0;
  marqueeRight:= 0;
  marqueeBouncing:= False;

  //Timer
  FTimerInterval:= 100;
  FTimerAutoRestart:= True;
  FCaptionPercentTimerFormat:= 'nn:ss.zzz';

  //Graph
  FMinYValue := 0;
  FMaxYValue := 100;
  GraphValues:= nil;
  GraphPoints:= nil;
  FGraphShowYDividers:= False;
  FGraphShowYLine:= False;
  FGraphYLineCaption:= '';
  FGraphYLineAfter:= '';
  FGraphYLineDigits:= 0;

  internalTimer:= TFPTimer.Create(Self);
  internalTimer.Enabled:= False;
  internalTimer.Interval:= MARQUEE_TIMER_MED;
  internalTimer.OnTimer:= TimerOnTimer;
  closing:= False;
end;

destructor TBGRAFlashProgressBar.Destroy;
begin
  //Avoid Exception when internalTimer is Enabled
  closing:= True;
  internalTimer.Enabled:=False;
  CheckSynchronize(40);

  internalTimer.Free;
  GraphValues:= nil;
  GraphPoints:= nil;
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

  procedure DrawBarAnimation;
  begin
    {$ifdef TESTS}
      ABitmap.GradientFill(4, content.Top, 4+36, content.Bottom,
                           BGRA(255, 255, 255, 64), BGRA(255, 255, 255, 2), pT,
                           p1, p2,
                           dmLinearBlend);
    {$else}
    if FShowBarAnimation and (barAnimLeft >= 0)
    then ABitmap.GradientFill(barAnimLeft, content.Top, barAnimLeft+36, content.Bottom,
                              BGRA(255, 255, 255, 64), BGRA(255, 255, 255, 2), gtReflected,
                              PointF(barAnimLeft+18, content.Bottom-content.Top/2), PointF(barAnimLeft+36, content.Bottom-content.Top/2),
                              dmLinearBlend);
    {$endif}
  end;

  procedure DrawText(ACaption: String; AAlign: TAlignment);
  var
     fx: TBGRATextEffect;

  begin
    try
       if (Font.Size=0)
       then fx:= TBGRATextEffect.Create(ACaption, Font.Name, ABitmap.Height div 2, True)
       else fx:= TBGRATextEffect.Create(ACaption, Font, True);

       y:= (ABitmap.Height-fx.TextHeight) div 2;

       Case AAlign of
         taLeftJustify: begin
           fx.DrawOutline(ABitmap, 4, y, BGRABlack, taLeftJustify);
           fx.Draw(ABitmap, 4, y, BGRAWhite, taLeftJustify);
         end;
         taRightJustify: begin
           fx.DrawOutline(ABitmap, tx-4, y, BGRABlack, taRightJustify);
           fx.Draw(ABitmap, tx-4, y, BGRAWhite, taRightJustify);
         end;
         taCenter: begin
           fx.DrawOutline(ABitmap, ABitmap.Width div 2, y, BGRABlack, taCenter);
           fx.Draw(ABitmap, ABitmap.Width div 2, y, BGRAWhite, taCenter);
         end;
       end;

    finally
      fx.Free;
    end;
  end;

  procedure DrawDividers(DrawYDiv: Boolean);
  var
    lColD: TBGRAPixel;
    posS: Single;
    i: Integer;

  begin
    lColD:= BGRA(128, 128, 128, 128);
    for i:= 1 to 9 do
    begin
      posS:= content.left+(i*10*(content.right-content.left)/100);
      ABitmap.DrawLineAntialias(posS, 2, posS, content.Bottom-1, lColD, 1, True);
    end;

    if DrawYDiv then
      for i:= 1 to 9 do
      begin
        posS:= content.Bottom-1-(i*10*(content.Bottom-content.Top)/100);
        ABitmap.DrawLineAntialias(2, posS, content.Right-1, posS, lColD, 1, True);
      end;
  end;

  procedure DrawG;
  var
    lCol,
    lColB: TBGRAPixel;
    posS: Single;
    curIndex: Integer;
    fx: TBGRATextEffect;

  begin
    lCol := FBarColor;
    lColB:= ApplyLightness(lCol, 37000);

    posS:= content.left+((FValue-FMinValue)/(FMaxValue-FMinValue)*(content.right-content.left));
    if (posS > content.Right-1) then posS:= content.Right-1;

    //Fixed Points to Close the Path
    GraphPoints[0].x:= posS;
    GraphPoints[0].y:= content.Bottom-1;
    GraphPoints[1].x:= content.Left;
    GraphPoints[1].y:= content.Bottom-1;

    //Draw Value Position
    xpos:= Round(posS);
    ABitmap.RectangleAntialias(content.left, content.Top, xpos, content.Bottom-1, lColB, 1, lColB);

    if FShowDividers then DrawDividers(FGraphShowYDividers);

    //Draw the Graph
    if (Length(GraphPoints) > 2) then
    begin
      ABitmap.DrawPolygonAntialias(GraphPoints, lCol, 1, lCol);

      if FGraphShowYLine then
      begin
        curIndex:= Length(GraphValues)-1;

        //Check if we have at least one Value
        if (curIndex >= 0) then
        begin
          lColB:= BGRA(0, 0, 0, 192);
          pStr:= FGraphYLineCaption+FloatToStrF(GraphValues[curIndex].YValue, ffFixed, 15, FGraphYLineDigits)+FGraphYLineAfter;

          //Get last Value Y Point and draw a horizontal line
          curIndex:= Length(GraphPoints)-1;
          posS:= GraphPoints[curIndex].y;
          ABitmap.DrawLineAntialias(2, posS, tx-4, posS, lColB, 1, True);

          try
             fx:= TBGRATextEffect.Create(pStr, Font.Name, 12, True);

             //Write the text above the line if possible else write below
             if (Round(posS-fx.TextHeight) >= 2) then posS:= posS-fx.TextHeight;

             fx.Draw(ABitmap, tx-6, Round(posS), lColB, taRightJustify);

          finally
            fx.Free;
          end;
        end;
      end;
    end;

    DrawBarAnimation; { #note -oMaxM : Evaluate how it seems }

    //Draw Value Text
    pStr:= '';
    if FCaptionShowPercent then
    begin
      pValue:= 100*(FValue - FMinValue)/FMaxValue;
      if (pValue <> 0) then pStr:= FloatToStrF(pValue, ffFixed, 15, FCaptionPercentDigits)+'%'
    end;
    DrawText(Caption+pStr, FCaptionPercentAlign);
 end;

begin
  try
  ABitmap.FillTransparent;
  tx := ABitmap.Width;
  ty := ABitmap.Height;

  ABitmap.Rectangle(0, 0, tx, ty, BGRA(255, 255, 255, 6), FBackgroundColor, dmSet);
  if (tx > 2) and (ty > 2) then
    ABitmap.Rectangle(1, 1, tx - 1, ty - 1, BGRA(29, 29, 29), dmSet);

  if (tx > 4) and (ty > 4) then
  begin
    content  := Rect(2, 2, tx - 2, ty - 2);
    randseed := FRandSeed;
    if FBackgroundRandomize then
    for y := content.Top to content.Bottom - 1 do
    begin
      bgColor := FBackgroundColor;
      bgColor.Intensity := RandomRange(FBackgroundRandomizeMinIntensity, FBackgroundRandomizeMaxIntensity);
      ABitmap.HorizLine(content.Left, y, content.Right - 1, bgColor, dmSet);
    end;
    if tx >= 6 then
      ABitmap.DrawVertLine(content.Right - 1, content.Top, content.Bottom - 1,
        BGRA(0, 0, 0, 32));

    Case FStyle of
      pbstNormal: begin
        if FMaxValue > FMinValue then
        begin
          //Draw Value Bar
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

            if FShowDividers then DrawDividers(False);

            DrawBarAnimation;

            //Draw Value Text
            pStr:= '';
            if FCaptionShowPercent then
            begin
              pValue:= 100*(FValue - FMinValue)/FMaxValue;
              if (pValue <> 0) then pStr:= FloatToStrF(pValue, ffFixed, 15, FCaptionPercentDigits)+'%'
            end;
            DrawText(Caption+pStr, FCaptionPercentAlign);
          end;
        end
        else if FShowDividers then DrawDividers(False);
      end;
      pbstMultiProgress: begin
        if FMaxValue > FMinValue then
        begin
          //Draw Value Bar
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

          //Draw ValueSub Bar
          xposSub := round((FValueSub - FMinValue) / (FMaxValue - FMinValue) *
                           (content.right - content.left)) + content.left;
          if xposSub > content.left then
          begin
            DrawBar(rect(content.left, content.top, xposSub, content.bottom), FBarColorSub);
            if xposSub < content.right then
            begin
              ABitmap.SetPixel(xposSub, content.top, BGRA(62, 62, 62));
              ABitmap.SetVertLine(xposSub, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
            end;
          end;

          if FShowDividers then DrawDividers(False);

          DrawBarAnimation;

         //Draw Value Text
          pStr:= '';
          if FCaptionShowPercent then
          begin
            pValue:= 100*(FValue - FMinValue)/FMaxValue;
            if (pValue <> 0) then pStr:= FloatToStrF(pValue, ffFixed, 15, FCaptionPercentDigits)+'%'
          end;
          DrawText(Caption+pStr, FCaptionPercentAlign);

          //Draw ValueSub Text
          pStr:= '';
          if FCaptionShowPercentSub then
          begin
            pValue:= 100*(FValueSub - FMinValue)/FMaxValue;
            if (pValue <> 0) then pStr:= FloatToStrF(pValue, ffFixed, 15, FCaptionPercentDigits)+'%'
          end;
          DrawText(pStr, FCaptionPercentSubAlign);
        end
        else if FShowDividers then DrawDividers(False);
      end;
      pbstMarquee: begin
        if (marqueeCurMode = pbmdToRight)
        then begin
               //check if the whole bar is out put it back to the beginning
               if (marqueeLeft >= content.Right)
               then marqueeLeft:= content.Left;

               //Calculate the Right
               marqueeRight:= marqueeLeft+(rMarqueeWidth-1);

               //Check if part of the bar is out calculate the visible piece on the left
               marqueeOver:= 0;
               marqueeWall:= (marqueeRight >= content.Right-1);
               if marqueeWall then
               begin
                 if (FMarqueeBounce > 0)
                 then begin
                        //Put perfectly on the Right edge
                        marqueeRight:= content.Right-1;
                        marqueeLeft:= marqueeRight-(rMarqueeWidth-1);
                        marqueeBouncing:= True;
                      end
                 else marqueeOver:= marqueeRight-(content.Right-1);
               end;
             end
        else begin
               //check if the whole bar is out put it back to the end
               if (marqueeLeft <= -rMarqueeWidth)
               then marqueeLeft:= content.Right-rMarqueeWidth;

               //Calculate the Right
               marqueeRight:= marqueeLeft+(rMarqueeWidth-1);

               //check if part of the bar is out then the visible piece on the left is equal to marqueeRight
               marqueeOver:= 0;
               marqueeWall:= (marqueeRight-1 <= rMarqueeWidth);
               if marqueeWall then
               begin
                 if (FMarqueeBounce > 0)
                 then begin
                        //Put perfectly on the Left edge
                        marqueeLeft:= content.Left;
                        marqueeRight:= marqueeLeft+(rMarqueeWidth-1);
                        marqueeBouncing:= True;
                      end
                 else marqueeOver:= marqueeRight;
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
               DrawBar(rect(content.Left, content.top, marqueeOver, content.bottom), FBarColor);
               ABitmap.SetPixel(marqueeOver, content.top, BGRA(62, 62, 62));
               ABitmap.SetVertLine(marqueeOver, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
               //Draw visible piece on the Right
               DrawBar(rect(content.Right-(rMarqueeWidth+1-marqueeOver), content.top, tx-2, content.bottom), FBarColor);
               ABitmap.SetPixel(content.Right-(rMarqueeWidth+1-marqueeOver), content.top, BGRA(62, 62, 62));
               ABitmap.SetVertLine(content.Right-(rMarqueeWidth+1-marqueeOver), content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
             end;
      end;
      pbstTimer: begin
        if FMaxValue > FMinValue then
        begin
          //Draw Timer Bar
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

            if FShowDividers then DrawDividers(False);

            //Draw Timer Text
            pStr:= '';
            if FCaptionShowPercent then
            begin
              if (FValue <> 0) then pStr:= FormatDateTime(FCaptionPercentTimerFormat, FValue)
            end;
            DrawText(Caption+pStr, FCaptionPercentAlign);
          end;
        end
        else if FShowDividers then DrawDividers(False);
      end;
      pbstGraph: DrawG;
      end;
  end;

  except
    //MaxM: Ignore Exception sometimes it happens when the timer is active and we are closing
  end;
end;

procedure TBGRAFlashProgressBar.SetValue(AValue: Double);
begin
  SetValue(AValue, 0);
end;

procedure TBGRAFlashProgressBar.SetValue(AValue, AYValue: Double);
var
   curIndex: Integer;

begin
  if (FStyle = pbstGraph)
  then begin
         if (AValue >= FMinValue) and (AValue <= FMaxValue) then
         begin
           //Check if Y Value is on the Range
           FValue := AValue;
           if (AYValue < FMinYValue) then AYValue := FMinYValue;
           if (AYValue > FMaxYValue) then AYValue := FMaxYValue;

           if (AValue > FValue)
           then begin
                  //Add a new Value in the array
                  curIndex:= Length(GraphValues);
                  SetLength(GraphValues, curIndex+1);
                  GraphValues[curIndex].XValue:= AValue;
                  GraphValues[curIndex].YValue:= AYValue;

                  //Calculate new Value x/y Position and add in the array
                  curIndex:= Length(GraphPoints);
                  SetLength(GraphPoints, curIndex+1);
                  GraphPoints[curIndex].x:= 2+((AValue-FMinValue) / (FMaxValue-FMinValue))*(Width-4);
                  GraphPoints[curIndex].y:= Height-3-((AYValue-FMinYValue) / (FMaxYValue-FMinYValue))*(Height-4);

                  if (GraphPoints[curIndex].x > Width-4) then GraphPoints[curIndex].x:= Width-4;
                  if (GraphPoints[curIndex].y < 2) then GraphPoints[curIndex].y:= 2;
                end
           else begin
                  //Deletes all values from the array that are no longer visible
                  curIndex:= Length(GraphValues)-1;
                  while (curIndex>=0) and (GraphValues[curIndex].XValue > AValue) do
                  begin
                    SetLength(GraphValues, curIndex);
                    SetLength(GraphPoints, curIndex+2); //there are 2 fixed points at the beginning
                    dec(curIndex);
                  end;

                  //If the last XValue is the same then assign the YValue else add a new Value
                  if (curIndex>=0) and (GraphValues[curIndex].XValue = AValue)
                  then GraphValues[curIndex].YValue:= AYValue
                  else begin
                         curIndex:= Length(GraphValues);
                         SetLength(GraphValues, curIndex+1);
                         GraphValues[curIndex].XValue:= AValue;
                         GraphValues[curIndex].YValue:= AYValue;
                         SetLength(GraphPoints, Length(GraphPoints)+1);
                       end;

                  curIndex:= Length(GraphPoints)-1;
                  GraphPoints[curIndex].x:= 2+((AValue-FMinValue) / (FMaxValue-FMinValue))*(Width-4);
                  GraphPoints[curIndex].y:= Height-3-((AYValue-FMinYValue) / (FMaxYValue-FMinYValue))*(Height-4);

                  if (GraphPoints[curIndex].x > Width-4) then GraphPoints[curIndex].x:= Width-4;
                  if (GraphPoints[curIndex].y < 2) then GraphPoints[curIndex].y:= 2;
                end;

           FValue:= AValue;

           if Assigned(FOnChange) then FOnChange(Self);
           Invalidate;
         end;
       end
  else if (FValue <> AValue) then
       begin
         FValue := AValue;
         if (FValue < FMinValue) then FValue := FMinValue;
         if (FValue > FMaxValue) then FValue := FMaxValue;

         if Assigned(FOnChange) then FOnChange(Self);
         Invalidate;
       end;
end;

procedure TBGRAFlashProgressBar.StepIt(AIncrement: Double);
begin
  Case FStyle of
    pbstMarquee,
    pbstTimer: begin
      internalTimer.Enabled:= False;
      TimerOnTimer(nil);
    end
  else Value:= Value+AIncrement;
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
  if (FStyle in [pbstMarquee, pbstTimer]) then
  begin
    internalTimer.Enabled:= not(internalTimer.Enabled);
    Invalidate;
  end;
end;

end.
