unit BCEffect;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, LCLProc, BGRABitmapTypes;

{-- Fading --}

type
  TFadingMode = (fmSuspended, fmFadeIn, fmFadeOut, fmFadeInCycle, fmFadeOutCycle, fmFadeInOut, fmFadeOutIn);

const
  FadingModeStr: array[TFadingMode] of string = ('Suspended', 'Fade In', 'Fade Out', 'Fade In Cycle','Fade Out Cycle', 'Fade In Out', 'Fade Out In');

function StrToTFadingMode(const s: ansistring): TFadingMode;
procedure FadingModeStrList(s: TStrings);

type

  { TFading }

  TFading = record
  private
    FAlpha: byte;
    FMode: TFadingMode;
    FAlphaStep: byte;
    FDuration: integer;
    FPrevDate: TDateTime;
    FElapsedMsAccumulator: integer;
  public
    procedure SetFAlpha(AValue: byte);
    procedure SetFMode(AValue: TFadingMode);
    procedure SetFAlphaStep(AValue: byte);
    procedure SetFDuration(AValue: integer);
  public
    function Execute(AStepCount: integer= 1): byte; // execute and return new alpha
    function Reset: byte;   // reset and return new alpha
    procedure PutImage(ADestination: TBGRACustomBitmap; AX,AY: integer; ASource: TBGRACustomBitmap);
    procedure FillRect(ADestination: TBGRACustomBitmap; ARect: TRect; AColor: TBGRAPixel);
  public
    property Alpha: byte read FAlpha write SetFAlpha;
    property Mode: TFadingMode read FMode write SetFMode;
    property Step: byte read FAlphaStep write SetFAlphaStep;
    property Duration: integer read FDuration write SetFDuration;
  end;

{-- Fading --}

implementation

{-- Fading --}

function StrToTFadingMode(const s: ansistring): TFadingMode;
var
  fm: TFadingMode;
  ls: ansistring;
begin
  ls := UTF8LowerCase(s);
  for fm := low(TFadingMode) to high(TFadingMode) do
    if ls = UTF8LowerCase(FadingModeStr[fm]) then
    begin
      Result := fm;
      break;
    end;
  Result := fm;
end;

procedure FadingModeStrList(s: TStrings);
var
  fm: TFadingMode;
begin
  for fm := low(TFadingMode) to high(TFadingMode) do
    s.Add(FadingModeStr[fm]);
end;

{ TFading }

procedure TFading.SetFAlpha(AValue: byte);
begin
  if FAlpha = AValue then
    Exit;
  FAlpha := AValue;
end;

procedure TFading.SetFMode(AValue: TFadingMode);
begin
  if FMode = AValue then
    Exit;
  FMode := AValue;
  FPrevDate:= 0;
end;

procedure TFading.SetFAlphaStep(AValue: byte);
begin
  if FAlphaStep = AValue then
    Exit
  else
    FAlphaStep := AValue;
end;

procedure TFading.SetFDuration(AValue: integer);
begin
  FDuration:= AValue;
end;

function TFading.Execute(AStepCount: integer= 1): byte;
var curDate: TDateTime;
  alphaStep: byte;
  timeGrain: integer;
begin
  if FAlphaStep <= 0 then
    alphaStep := 1
  else
    alphaStep := FAlphaStep;

  if FDuration > 0 then
  begin
    curDate := Now;
    if FPrevDate = 0 then
    begin
      FPrevDate := curDate;
      FElapsedMsAccumulator := 0;
      result := FAlpha;
      exit;
    end;

    inc(FElapsedMsAccumulator, round((curDate-FPrevDate)*(24*60*60*1000)) );
    timeGrain := round(FDuration*alphaStep/255);
    if timeGrain <= 0 then timeGrain := 1;
    AStepCount := FElapsedMsAccumulator div timeGrain;
    FElapsedMsAccumulator:= FElapsedMsAccumulator mod timeGrain;
    FPrevDate := curDate;
  end;

  if AStepCount < 0 then AStepCount := 0
  else if AStepCount > 255 then AStepCount := 255;

  case FMode of
    fmFadeIn, fmFadeInOut, fmFadeInCycle:
    begin
      if (FAlpha = 255) and (FMode = fmFadeInCycle) then
        FAlpha := 0
      else
      if FAlpha + alphaStep*AStepCount >= 255 then
      begin
        FAlpha := 255;
        if FMode = fmFadeInOut then
          FMode := fmFadeOutIn
        else if FMode <> fmFadeInCycle then
          FMode := fmSuspended;
      end
      else
        FAlpha += alphaStep*AStepCount;
    end;
    fmFadeOut,fmFadeOutIn, fmFadeOutCycle:
    begin
      if (FAlpha = 0) and (FMode = fmFadeOutCycle) then
        FAlpha := 255
      else
      if FAlpha - alphaStep*AStepCount <= 0 then
      begin
        FAlpha := 0;
        if FMode = fmFadeOutIn then
          FMode := fmFadeInOut
        else if FMode <> fmFadeOutCycle then
          FMode := fmSuspended;
      end
      else
        FAlpha -= alphaStep*AStepCount;
    end;
  end;

  Result := FAlpha;
end;

function TFading.Reset: byte;
begin
  case FMode of
    fmFadeIn, fmFadeInOut:
    begin
      FAlpha := 0;
    end;
    fmFadeOut,fmFadeOutIn:
    begin
      FAlpha := 255;
    end;
  end;
  Result := FAlpha;
  FPrevDate := 0;
end;

procedure TFading.PutImage(ADestination: TBGRACustomBitmap; AX, AY: integer;
  ASource: TBGRACustomBitmap);
begin
  ADestination.PutImage(AX,AY,ASource,dmDrawWithTransparency,Alpha);
end;

procedure TFading.FillRect(ADestination: TBGRACustomBitmap; ARect: TRect;
  AColor: TBGRAPixel);
begin
  ADestination.FillRect(ARect, BGRA(AColor.red,AColor.green,AColor.blue,AColor.alpha*Alpha div 255),dmDrawWithTransparency);
end;

{-- Fading --}

end.
