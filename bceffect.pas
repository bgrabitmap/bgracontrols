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

{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCEffect;

{$I bgracontrols.inc}
{$IFDEF FPC}
{$modeswitch advancedrecords}
{$ENDIF}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LCLProc, LazUTF8, {$ELSE}Types, BGRAGraphics, GraphType, FPImage, {$ENDIF} BGRABitmapTypes;

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
  ls := {$IFDEF FPC}UTF8LowerCase{$ELSE}LowerCase{$ENDIF}(s);
  for fm := low(TFadingMode) to high(TFadingMode) do
    if ls = {$IFDEF FPC}UTF8LowerCase{$ELSE}LowerCase{$ENDIF}(FadingModeStr[fm]) then
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
        FAlpha := FAlpha + (alphaStep*AStepCount);
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
        FAlpha := FAlpha - (alphaStep*AStepCount);
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
