// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Part of BGRA Controls. Made by third party.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html

}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)
- Sandy Ganz | sganz@pacbell.net
  Evolved from DTAnalogCommon, specific for New Gauge Work
  Massive overhaul, fixes and features, begat Super Gauge
  Needed to split off as changes broke compatibility badly

***************************** END CONTRIBUTOR(S) *****************************}
unit SuperGauge;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, Graphics, {$IFDEF FPC}LResources,{$ELSE} BGRAGraphics, {$ENDIF} Forms, Controls, Dialogs, SuperGaugeCommon,
  BGRABitmap, BGRABitmapTypes, BGRAVectorize, BGRAPath, math, bctypes, bctools;

const
  INTERNAL_GAUGE_MIN_VALUE = 0;   // internal lowest value
  INTERNAL_GAUGE_MAX_VALUE = 270; // internal highest value
  VERSIONSTR = '1.02';            // SG version, Should ALWAYS show as a delta when merging!

type

  { TSGCustomSuperGauge }

  TBandsArray = array[0..3] of TSGBandSettings;
  TTextsArray = array[0..2] of TSGTextSettings;
  TMarkersArray = array[0..2] of TSGMarkerSettings;

  TTextsBitmapArray = array[0..2] of TBGRABitmap;

  TSGRangeStateErrorEvent = procedure(Sender: TObject; OutOfRangeValue: single) of object;  // called anytime out of range
  TSGRangeStateOKEvent = procedure(Sender: TObject; RangeValue: single) of object;          // called only when back to in range
  TSGRangeStateChangeEvent = procedure(Sender: TObject; Value: single) of object;           // called when state RangeLed Active changes to True

  TSGCustomSuperGauge = class(TGraphicControl)
  private
    { Private declarations }
    FDirty: boolean;

    FFaceSettings: TSGFaceSettings;
    FFrameSettings: TSGFrameSettings;
    FPointerCapSettings: TSGPointerCapSettings;
    FScaleSettings: TSGScaleSettings;
    FBandsSettings: TBandsArray;
    FTextsSettings: TTextsArray;
    FPointerSettings: TSGPointerSettings;
    FRangeLEDSettings: TSGRangeCheckLEDSettings;
    FMarkersSettings: TMarkersArray;
    FGaugeBitmap: TBGRABitmap;
    FFrameBitmap: TBGRABitmap;
    FFaceBitmap: TBGRABitmap;
    FTextBitmap: TBGRABitmap;
    FScaleBitmap: TBGRABitmap;
    FBandBitmap: TBGRABitmap;
    FTextsBitmaps: TTextsBitmapArray;

    FMultiBitmap: TBGRABitmap;
    FPointerBitmap: TBGRABitmap;
    FMarkerBitmap: TBGRABitmap;
    FPointerCapBitmap: TBGRABitmap;
    FLEDActiveBitmap: TBGRABitmap;
    FLEDInActiveBitmap: TBGRABitmap;

    FMinValue: single;  // the min value mapped to lowest/leftmost angle on the gauge
    FMaxValue: single;  // the max value mapped to highest/rightmost angle on the gauge
    FValue: single;     // this is the VALUE not a position
    FOutOfRange: TSGRangeStateErrorEvent;         // change of state ONLY
    FBackInRange: TSGRangeStateOKEvent;           // change of state ONLY
    FRangeLEDActive: TSGRangeStateChangeEvent;    // change of state ONLY
    FRangeLEDInactive: TSGRangeStateChangeEvent;  // change of state ONLY
    FOutOfRangeState: boolean;
    FRangeLEDStateChanged: boolean;
    FAutoScale: boolean;

    procedure SetBandSettings1(AValue: TSGBandSettings);
    procedure SetBandSettings2(AValue: TSGBandSettings);
    procedure SetBandSettings3(AValue: TSGBandSettings);
    procedure SetBandSettings4(AValue: TSGBandSettings);

    procedure SetTextSettings1(AValue: TSGTextSettings);
    procedure SetTextSettings2(AValue: TSGTextSettings);
    procedure SetTextSettings3(AValue: TSGTextSettings);

    procedure SetMarkerSettings1(AValue: TSGMarkerSettings);
    procedure SetMarkerSettings2(AValue: TSGMarkerSettings);
    procedure SetMarkerSettings3(AValue: TSGMarkerSettings);

    procedure SetFaceSettings(AValue: TSGFaceSettings);
    procedure SetScaleSettings(AValue: TSGScaleSettings);
    procedure SetFrameSettings(AValue: TSGFrameSettings);
    procedure SetPointerSettings(AValue: TSGPointerSettings);
    procedure SetRangeLedSettings(AValue: TSGRangeCheckLEDSettings);
    procedure SetPointerCapSettings(AValue: TSGPointerCapSettings);
    procedure SetMaxValue(AValue: single);
    procedure SetMinValue(AValue: single);
    function GetMaxValue: single;
    function GetMinValue: single;

    procedure SetValue(AValue: single);
    function GetValue: single;
    procedure SetAutoScale(AValue: boolean);
    function CheckOutOfRange(AValue: single): single;

  protected
    { Protected declarations }
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DoChange({%H-}Sender: TObject);
    procedure DoRangeLEDChange({%H-}Sender: TObject);
    procedure DoPictureChange({%H-}Sender: TObject);
    procedure DoChangeFont1({%H-}ASender: TObject; {%H-}AData: PtrInt); // Wrapper for FontEx DoChange
    procedure DoChangeFont2({%H-}ASender: TObject; {%H-}AData: PtrInt); // Wrapper for FontEx DoChange
    procedure DoChangeFont3({%H-}ASender: TObject; {%H-}AData: PtrInt); // Wrapper for FontEx DoChange
    procedure SetAllBandsDirtyState(AValue: boolean);
    procedure SetAllTextsDirtyState(AValue: boolean);
    procedure SetAllMarkersDirtyState(AValue: boolean);
    function IsAnyBandDirty: boolean;
    function IsAnyMarkerDirty: boolean;
    property Dirty: boolean read FDirty write FDirty;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property PointerSettings: TSGPointerSettings read FPointerSettings write SetPointerSettings; // sjg added
    property PointerCapSettings: TSGPointerCapSettings read FPointerCapSettings write SetPointerCapSettings;
    property FaceSettings: TSGFaceSettings read FFaceSettings write SetFaceSettings;
    property FrameSettings: TSGFrameSettings read FFrameSettings write SetFrameSettings;
    property ScaleSettings: TSGScaleSettings read FScaleSettings write SetScaleSettings;
    property BandSettings1: TSGBandSettings read FBandsSettings[0] write SetBandSettings1; // will need an array thing here
    property BandSettings2: TSGBandSettings read FBandsSettings[1] write SetBandSettings2; // will need an array thing here
    property BandSettings3: TSGBandSettings read FBandsSettings[2] write SetBandSettings3; // will need an array thing here
    property BandSettings4: TSGBandSettings read FBandsSettings[3] write SetBandSettings4; // will need an array thing here
    property TextSettings1: TSGTextSettings read FTextsSettings[0] write SetTextSettings1;
    property TextSettings2: TSGTextSettings read FTextsSettings[1] write SetTextSettings2;
    property TextSettings3: TSGTextSettings read FTextsSettings[2] write SetTextSettings3;
    property RangeLedSettings: TSGRangeCheckLEDSettings read FRangeLEDSettings write SetRangeLedSettings;
    property MarkerSettings1: TSGMarkerSettings read FMarkersSettings[0] write SetMarkerSettings1;
    property MarkerSettings2: TSGMarkerSettings read FMarkersSettings[1] write SetMarkerSettings2;
    property MarkerSettings3: TSGMarkerSettings read FMarkersSettings[2] write SetMarkerSettings3;
    property MinValue: single read GetMinValue write SetMinValue default 0.0;
    property MaxValue: single read GetMaxValue write SetMaxValue default 100.0;
    property AutoScale: boolean read FAutoScale write SetAutoScale default False;
    property Value: single read GetValue write SetValue default 0.0;
    property OutOfRange: TSGRangeStateErrorEvent read FOutOfRange write FOutOfRange;
    property BackInRange: TSGRangeStateOKEvent read FBackInRange write FBackInRange;
    property RangeLEDActive: TSGRangeStateChangeEvent read FRangeLEDActive write FRangeLEDActive;
    property RangeLEDInActive: TSGRangeStateChangeEvent read FRangeLEDInactive write FRangeLEDInactive;
    function RemapRange(OldValue: single; OldMin, OldMax, NewMin, NewMax: single): single;
    function GaugeToUser(GaugeValue: single): single;
    function UserToGauge(UserValue: single): single;

    procedure Paint; override;
    procedure DrawFrame;
    procedure DrawFace;
    procedure DrawScale;
    procedure DrawBand(const BandSettings: TSGBandSettings);
    procedure DrawBands;
    procedure DrawMulti;
    procedure DrawText(TextBitmap: TBGRABitmap; const TextSettings: TSGTextSettings);
    procedure DrawLed;
    procedure DrawMarker(MarkerBitmap: TBGRABitmap; const MarkerSettings: TSGMarkerSettings);
    procedure DrawMarkers;
    procedure DrawPointer;
    procedure DrawPointerCap;
    function CheckRangeLED(AValue: single): boolean;
  end;

  { TSuperGauge }

  TSuperGauge = class(TSGCustomSuperGauge)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    property MinValue;
    property MaxValue;
    property FaceSettings;
    property BandSettings1;
    property BandSettings2;
    property BandSettings3;
    property BandSettings4;
    property TextSettings1;
    property TextSettings2;
    property TextSettings3;
    property FrameSettings;
    property ScaleSettings;
    property RangeLedSettings;

    property MarkerSettings1;
    property MarkerSettings2;
    property MarkerSettings3;

    property PointerSettings;
    property PointerCapSettings;
    property AutoScale;
    property Value;
    property OutOfRange;
    property BackInRange;
    property RangeLEDActive;
    property RangeLEDInactive;
    property Color default clNone;

    // Added missing events

    property Anchors;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

  {$IFDEF FPC}procedure Register;{$ENDIF}

implementation
{$IFDEF FPC}
procedure Register;
begin
  RegisterComponents('BGRA Controls', [TSuperGauge]);
end;
{$ENDIF}

{ TSGCustomSuperGauge }

constructor TSGCustomSuperGauge.Create(AOwner: TComponent);
var
    i: integer;
begin
  inherited Create(AOwner);

  Width := 240;
  Height := 240;

  FFaceSettings := TSGFaceSettings.Create;
  FaceSettings.OnChange := DoChange;
  FaceSettings.Picture.OnChange := DoPictureChange; // need to set this so we can catch changes to the picture!

  FFrameSettings := TSGFrameSettings.Create;
  FrameSettings.OnChange := DoChange;

  FScaleSettings := TSGScaleSettings.Create;
  ScaleSettings.OnChange := DoChange;

  for i := low(FBandsSettings) to high(FBandsSettings) do
  begin
    FBandsSettings[i] := TSGBandSettings.Create;
    FBandsSettings[i].OnChange := DoChange;
    FBandsSettings[i].Text := 'Band ' + IntToStr(i + 1);
  end;

  for i := low(FTextsSettings) to high(FTextsSettings) do
  begin
    FTextsSettings[i] := TSGTextSettings.Create;
    FTextsSettings[i].OnChange := DoChange;
    FTextsBitmaps[i] := TBGRABitmap.Create;
  end;

  // Set Text font change events and defaults

  FTextsSettings[0].FontEx.OnChange := DoChangeFont1;
  FTextsSettings[1].FontEx.OnChange := DoChangeFont2;
  FTextsSettings[2].FontEx.OnChange := DoChangeFont3;
  FTextsSettings[0].Text := 'Text1';
  FTextsSettings[1].Text := 'Text2';
  FTextsSettings[2].Text := 'Text3';

  // Pointer Cap

  FPointerCapSettings := TSGPointerCapSettings.Create;
  FPointerCapSettings.OnChange := DoChange;

  // Pointer

  FPointerSettings := TSGPointerSettings.Create;
  FPointerSettings.OnChange := DoChange;
  FPointerSettings.Color := BGRA(255, 127, 63); // orange

  // RangeLED

  FRangeLEDSettings := TSGRangeCheckLEDSettings.Create;
  FRangeLEDSettings.OnChange := DoRangeLEDChange;

  // Markers

  for i := low(FMarkersSettings) to high(FMarkersSettings) do
  begin
    FMarkersSettings[i] := TSGMarkerSettings.Create;
    FMarkersSettings[i].OnChange := DoChange;
  end;

  // make marker each different to save confusion

  FMarkersSettings[0].Color := clLime;
  FMarkersSettings[1].Color := clRed;
  FMarkersSettings[2].Color := clYellow;

  // create needed bitmaps, Don't Forget to FREE!!!

  FFaceBitmap := TBGRABitmap.Create;
  FFrameBitmap := TBGRABitmap.Create;
  FGaugeBitmap := TBGRABitmap.Create;
  FTextBitmap := TBGRABitmap.Create;
  FPointerBitmap := TBGRABitmap.Create;
  FPointerCapBitmap := TBGRABitmap.Create;
  FScaleBitmap := TBGRABitmap.Create;
  FBandBitmap := TBGRABitmap.Create;
  FMultiBitmap := TBGRABitmap.Create;
  FLEDActiveBitmap := TBGRABitmap.Create;
  FLEDInActiveBitmap := TBGRABitmap.Create;
  FMarkerBitmap := TBGRABitmap.Create;

  // initialized (some above)

  FOutOfRangeState := False;
  FRangeLEDStateChanged := False;
  FValue := 0;
  FAutoScale := false;
  FMinValue := 0;
  FMaxValue := 100;
  Color := clNone;
  FDirty := True;   // Always force initial paint/draw on everything!
end;

destructor TSGCustomSuperGauge.Destroy;
var
    i: integer;
begin

  FPointerCapSettings.OnChange := nil;
  FPointerCapSettings.Free;

  FPointerSettings.OnChange := nil;
  FPointerSettings.Free;

  FRangeLEDSettings.OnChange := nil;
  FRangeLEDSettings.Free;

  ScaleSettings.OnChange := nil;
  FScaleSettings.Free;

  for i := low(FTextsSettings) to high(FTextsSettings) do
  begin
   FBandsSettings[i].OnChange := nil;
   FBandsSettings[i].Free;
  end;

  for i := low(FTextsSettings) to high(FTextsSettings) do
  begin
   FTextsSettings[i].OnChange := nil;
   FTextsSettings[i].FontEx.OnChange := nil;
   FTextsSettings[i].Free;
   FTextsBitmaps[i].Free;
  end;

  for i := low(FMarkersSettings) to high(FMarkersSettings) do
  begin
   FMarkersSettings[i].OnChange := nil;
   FMarkersSettings[i].Free;
  end;

  FFaceSettings.OnChange := nil;
  FFaceSettings.Free;

  FFrameSettings.OnChange := nil;
  FFrameSettings.Free;

  // now clean bitmaps, should match what's in creat method

  FLEDActiveBitmap.Free;
  FLEDInactiveBitmap.Free;
  FMarkerBitmap.Free;
  FBandBitmap.Free;
  FScaleBitmap.Free;
  FPointerBitmap.Free;
  FPointerCapBitmap.Free;
  FTextBitmap.Free;
  FFaceBitmap.Free;
  FMultiBitmap.Free;
  FFrameBitmap.Free;
  FGaugeBitmap.Free;

  inherited Destroy;
end;

function TSGCustomSuperGauge.RemapRange(OldValue: single; OldMin, OldMax, NewMin, NewMax: single): single;
begin
  // Generic mapping of ranges. Value is the number to remap, returns number
  // in the new range. Looks for odd div by 0 condition and fixes

  if OldMin = OldMax then
  begin
    // need to return something reasonable

    if OldValue <= OldMin then
      Exit(NewMin)
    else
      Exit(NewMax);
  end;

  Result := (((OldValue - OldMin) * (NewMax - NewMin)) / (OldMax - OldMin)) + NewMin;
end;

function TSGCustomSuperGauge.GaugeToUser(GaugeValue: single): single;
begin
  // Helper to translates internal gauge value to external user value

  Result := RemapRange(GaugeValue, INTERNAL_GAUGE_MIN_VALUE, INTERNAL_GAUGE_MAX_VALUE, FMinValue, FMaxValue);
end;

function TSGCustomSuperGauge.UserToGauge(UserValue: single): single;
begin
  // Helper to translates external user value to internal gauge value

  Result := RemapRange(UserValue, FMinValue, FMaxValue, INTERNAL_GAUGE_MIN_VALUE, INTERNAL_GAUGE_MAX_VALUE);
end;

function TSGCustomSuperGauge.GetValue: single;
begin
  // Scale from internal back to user range

  Result := GaugeToUser(FValue);
end;

procedure TSGCustomSuperGauge.SetValue(AValue: single);
var
    gaugeValue: single;
begin

  // Tricky case here, since we are calling the RangeLED range check
  // here too, if that is in any way dirty we should process the value
  // and not skip. Triggering any change on RangeLEDSettings should call this.

  // Get the user value into gauge value space

  gaugeValue := UserToGauge(AValue);

  // skip if a few conditions exit. This is a bit tricky as the gauge value will reset
  // to min or max values on overload so need to always update if that's the case. Should
  // not affect performance. Similar for LED, if dirty no skip.

  if (FValue = gaugeValue) and (not FRangeLEDSettings.Dirty) and (not FOutOfRangeState) then
    Exit;

  // If out of range conditions are at play the gauge Value (FValue) will never
  // be out of range. This value is passed to the out of range handler for the
  // user to deal with and DO SOMETHING to indicate it.

  FValue := CheckOutOfRange(gaugeValue);

  // If we have a change in the of the LED's Active property we need
  // to call the event handlers too. Also we will check it's values and set
  // if needed. NOTE : that if the range type is set to rtNone, it will always
  // return the state of the RangeLEDSettings.Active, otherwise it will calculate
  // the needed value for a range check as set. FRangeLEDStateChanged is set in
  // IsRangeLEDActive function so should be called before this!

  // MUST NOT CALL .Active as this will cause a recursive call, use the
  // hacked ActiveNoDoChange which will just set the property value with
  // no side effects

  // True if LED Should be On, False if not, AValue is in User space for LED's

  FRangeLEDSettings.ActiveNoDoChange := CheckRangeLED(AValue);

  // We must dirty the Pointer here or no redraw

  PointerSettings.Dirty := True;
  DoChange(self);
end;

function TSGCustomSuperGauge.CheckOutOfRange(AValue: single): Single;
begin
  // These values are in gauge space, so typically never less than 0, or > 270

  Result := AValue; // SAFE so always will return a value

  if AValue < INTERNAL_GAUGE_MIN_VALUE then
  begin
    // Under Range event

    FOutOfRangeState := True;
    if Assigned(FOutOfRange) then
      FOutOfRange(Self, GaugeToUser(AValue));
    Result := INTERNAL_GAUGE_MIN_VALUE;
  end
    else
      if AValue > INTERNAL_GAUGE_MAX_VALUE then
      begin
        // Over Range event

        FOutOfRangeState := True;
        if Assigned(FOutOfRange) then
          FOutOfRange(Self, GaugeToUser(AValue)); // must translate back to user space
        Result := INTERNAL_GAUGE_MAX_VALUE;
      end
      else
        begin
          // If NOT over/under flow then will need to clear
          // that state/flag and reset any indicators if was in a overange state

          if FOutOfRangeState then
          begin
            if Assigned(FBackInRange) then
              FBackInRange(self, GaugeToUser(AValue)); // here to, get into user space

            FOutOfRangeState := False;  // reset so no more calls
          end;
      end;
end;

procedure TSGCustomSuperGauge.SetAutoScale(AValue: boolean);
begin
  if FAutoScale = AValue then
    exit;

  FAutoScale := AValue;
  FScaleSettings.Dirty := True;  // set it, as it will need a repaint

  DoChange(self);
end;

function TSGCustomSuperGauge.GetMaxValue: single;
begin
  Result := FMaxValue;
end;

procedure TSGCustomSuperGauge.SetMaxValue(AValue: single);
var
    currUser: single;
begin
  // Note : MinValue and MaxValue can span negative ranges and be increasing
  //        or decreasing

  // Min and Max out of order, bounce

  if (FMinValue >= AValue) then
    exit;

  // If changing min/max must refresh the value to adjust

  currUser := GaugeToUser(FValue);
  FMaxValue := AValue;  // setting this will change UserToGauge() in SetValue!

  // Recompute

  SetValue(currUser);
end;

function TSGCustomSuperGauge.GetMinValue: single;
begin
  Result := FMinValue;
end;

procedure TSGCustomSuperGauge.SetMinValue(AValue: single);
var
    currUser: single;
begin
  // Note : MinValue and MaxValue can span negative ranges and be increasing
  //        or decreasing

  // Min and Max out of order, bounce

  if (FMaxValue <= AValue) then
    exit;

  // If changing min/max must refresh the value to adjust

  currUser := GaugeToUser(FValue);
  FMinValue := AValue;  // setting this will change UserToGauge() in SetValue!

  // Recompute

  SetValue(currUser);
end;

procedure TSGCustomSuperGauge.SetFaceSettings(AValue: TSGFaceSettings);
begin
  if FFaceSettings = AValue then
    Exit;

  FFaceSettings := AValue;
  FFaceSettings.Dirty := True;  // set it, as it will need a repaint

  DoChange(self);
end;

procedure TSGCustomSuperGauge.SetFrameSettings(AValue: TSGFrameSettings);
begin
  if FFrameSettings = AValue then
    Exit;

  FFrameSettings := AValue;
  FFrameSettings.Dirty := True;  // set it, as it will need a repaint

  DoChange(self);
end;

procedure TSGCustomSuperGauge.SetScaleSettings(AValue: TSGScaleSettings);
begin
  if FScaleSettings = AValue then
    Exit;

  FScaleSettings := AValue;
  FScaleSettings.Dirty := True;

  DoChange(self);
end;

procedure TSGCustomSuperGauge.SetAllBandsDirtyState(AValue: boolean);
var
  i: integer;
begin
  // helper to just set all bands to a specific state!

  for i := low(FBandsSettings) to high(FBandsSettings) do
    FBandsSettings[i].Dirty := AValue;
end;

function TSGCustomSuperGauge.IsAnyBandDirty : boolean;
var
  i: integer;
begin
  // helper to just see if any band has a dirty flag

  for i := low(FBandsSettings) to high(FBandsSettings) do
  begin
    if FBandsSettings[i].Dirty then
      exit(True);
  end;

  result := False;
end;

procedure TSGCustomSuperGauge.SetBandSettings1(AValue: TSGBandSettings);
begin
  if FBandsSettings[0] = AValue then
    Exit;

  FBandsSettings[0] := AValue;
  FBandsSettings[0].Dirty := True;
  DoChange(self);
end;

procedure TSGCustomSuperGauge.SetBandSettings2(AValue: TSGBandSettings);
begin
  if FBandsSettings[1] = AValue then
    Exit;

  FBandsSettings[1] := AValue;
  FBandsSettings[1].Dirty := True;
  DoChange(self);
end;

procedure TSGCustomSuperGauge.SetBandSettings3(AValue: TSGBandSettings);
begin
  if FBandsSettings[2] = AValue then
    Exit;

  FBandsSettings[2] := AValue;
  FBandsSettings[2].Dirty := True;
  DoChange(self);
end;

procedure TSGCustomSuperGauge.SetBandSettings4(AValue: TSGBandSettings);
begin
  if FBandsSettings[3] = AValue then
    Exit;

  FBandsSettings[3] := AValue;
  FBandsSettings[3].Dirty := True;
  DoChange(self);
end;

procedure TSGCustomSuperGauge.SetAllTextsDirtyState(AValue: boolean);
var
  i: integer;
begin
  // helper to just set all texts to a specific state!

  for i := low(FTextsSettings) to high(FTextsSettings) do
    FTextsSettings[i].Dirty := AValue;
end;

procedure TSGCustomSuperGauge.SetTextSettings1(AValue: TSGTextSettings);
begin
  if FTextsSettings[0] = AValue then
    Exit;

  FTextsSettings[0] := AValue;
  FTextsSettings[0].Dirty := True;  // set it, as it will need a repaint

  DoChange(self);
end;

procedure TSGCustomSuperGauge.SetTextSettings2(AValue: TSGTextSettings);
begin
  if FTextsSettings[1] = AValue then
    Exit;

  FTextsSettings[1] := AValue;
  FTextsSettings[1].Dirty := True;

  DoChange(self);
end;

procedure TSGCustomSuperGauge.SetTextSettings3(AValue: TSGTextSettings);
begin
  if FTextsSettings[2] = AValue then
    Exit;

  FTextsSettings[2] := AValue;
  FTextsSettings[2].Dirty := True;

  DoChange(self);
end;

function TSGCustomSuperGauge.IsAnyMarkerDirty: boolean;
var
  i: integer;
begin
  // helper to just see if any band has a dirty flag

  for i := low(FMarkersSettings) to high(FMarkersSettings) do
  begin
    if FMarkersSettings[i].Dirty then
      exit(True);
  end;

  result := False;
end;

procedure TSGCustomSuperGauge.SetAllMarkersDirtyState(AValue: boolean);
var
  i: integer;
begin
  // helper to just set all markers to a specific state!

  for i := low(FMarkersSettings) to high(FMarkersSettings) do
    FMarkersSettings[i].Dirty := AValue;
end;

procedure TSGCustomSuperGauge.SetMarkerSettings1(AValue: TSGMarkerSettings);
begin
  if FMarkersSettings[0] = AValue then
    Exit;

  FMarkersSettings[0] := AValue;
  FMarkersSettings[0].Dirty := True;

  DoChange(self);
end;

procedure TSGCustomSuperGauge.SetMarkerSettings2(AValue: TSGMarkerSettings);
begin
  if FMarkersSettings[1] = AValue then
    Exit;

  FMarkersSettings[1] := AValue;
  FMarkersSettings[1].Dirty := True;

  DoChange(self);
end;

procedure TSGCustomSuperGauge.SetMarkerSettings3(AValue: TSGMarkerSettings);
begin
  if FMarkersSettings[2] = AValue then
    Exit;

  FMarkersSettings[2] := AValue;
  FMarkersSettings[2].Dirty := True;

  DoChange(self);
end;

procedure TSGCustomSuperGauge.SetPointerSettings(AValue: TSGPointerSettings);
begin
  if FPointerSettings = AValue then
    Exit;

  FPointerSettings := AValue;
  FPointerSettings.Dirty := True;

  DoChange(self);
end;

procedure TSGCustomSuperGauge.SetRangeLedSettings(AValue: TSGRangeCheckLEDSettings);
begin
  if FRangeLEDSettings = AValue then
    Exit;

  FRangeLEDSettings := AValue;
  FRangeLEDSettings.Dirty := True;

  DoChange(self);
end;

procedure TSGCustomSuperGauge.SetPointerCapSettings(AValue: TSGPointerCapSettings);
begin
  if FPointerCapSettings = AValue then
    Exit;

  FPointerCapSettings := AValue;
  FPointerCapSettings.Dirty := True;
  DoChange(self);
end;

procedure TSGCustomSuperGauge.DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  FDirty := true; // Called on Resize of component
end;

procedure TSGCustomSuperGauge.DoChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TSGCustomSuperGauge.DoRangeLEDChange(Sender: TObject);
begin
  // This is needed as anytime a RangeLED settings is updated we
  // MAY need to update and call event handlers. update as the RangeLEDSettings.Dirty

  CheckRangeLED(Value); // Tricky may not work!
  DoChange(self);
end;

procedure TSGCustomSuperGauge.DoPictureChange(Sender: TObject);
begin
  // This is similar to DoRangeLEDChange, if we have a picture change this
  // is how we can propagate it up to the gauge to tell if a repaint is needed.

  FaceSettings.Dirty := True;  // trigger a redraw since the image has changed
  DoChange(Sender);
end;


procedure TSGCustomSuperGauge.DoChangeFont1(ASender: TObject; AData: PtrInt);
begin
  // Simlar to the regular dochange but needed a different procedure signature
  // so just a wrapper, TObject is not a gauge so use Self here for DoChange()

  FTextsSettings[0].Dirty := True;
  DoChange(self);
end;

procedure TSGCustomSuperGauge.DoChangeFont2(ASender: TObject; AData: PtrInt);
begin
  // Simlar to the regular dochange but needed a different procedure signature
  // so just a wrapper, TObject is not a gauge so use Self here for DoChange()

  FTextsSettings[1].Dirty := True;
  DoChange(self);
end;

procedure TSGCustomSuperGauge.DoChangeFont3(ASender: TObject; AData: PtrInt);
begin
  // Simlar to the regular dochange but needed a different procedure signature
  // so just a wrapper, TObject is not a gauge so use Self here for DoChange()

  FTextsSettings[2].Dirty := True;
  DoChange(self);
end;

procedure TSGCustomSuperGauge.Paint;
var
  i: integer;
  offsetX, offsetY: integer;
  gaugeCenX, gaugeCenY: integer;
begin
  inherited Paint;

  // IF the component is resized OR moved (this is safer) we
  // need to make sure EVERYTHING redraws. The base class will
  // also do it's own thing to invalidate and redraw it all.

  if FDirty then
  begin
    FFrameSettings.Dirty := True;
    FFaceSettings.Dirty := True;
    FScaleSettings.Dirty := True;
    SetAllBandsDirtyState(True);
    SetAllTextsDirtyState(True);
    FRangeLEDSettings.Dirty := True;
    FPointerCapSettings.Dirty := True;
    FPointerSettings.Dirty := True;
    SetAllMarkersDirtyState(True);
    FDirty := False;  // everything here marked, so can reset
  end;

  // Now start Drawing into the offscreen bitmaps. IF the particular
  // subcomponent is not changed, the DrawXXXX will just leave it as is
  // and not waste cycles to redraw it.

  FGaugeBitmap.SetSize(Width, Height);

  // If the gauge color is clNone then we start with a transparent background,
  // Otherwise we start with the users color.

  if Color = clNone then
    FGaugeBitmap.Fill(BGRA(0, 0, 0, 0))  // fill transparent
  else
    FGaugeBitmap.Fill(ColorToBGRA(Color, 255));  // fill solid color

  gaugeCenX := FGaugeBitmap.Width div 2;
  gaugeCenY := FGaugeBitmap.Height div 2;

  // Face, Frame, Scale and Bands are usually static, so do yet another
  // bitmap for these that will require less Blend Images.

  DrawMulti;
  FGaugeBitmap.BlendImage(0, 0, FMultiBitmap, boLinearBlend);

  // now draw any texts if enabled and dirty

  for i := low(FTextsSettings) to high(FTextsSettings) do
  begin
    if FTextsSettings[i].Enabled then
    begin
      DrawText(FTextsBitmaps[i], FTextsSettings[i]);
      offsetX := FTextsSettings[i].OffsetX + gaugeCenX - FTextsBitmaps[i].Width div 2;
      offsetY := FTextsSettings[i].OffsetY + gaugeCenY - FTextsBitmaps[i].Height div 2;
      FGaugeBitmap.BlendImage(offsetX, offsetY, FTextsBitmaps[i], boLinearBlend);
    end;
  end;

  FGaugeBitmap.BlendImage(offsetX, offsetY, FTextBitmap, boLinearBlend);

  // Draw range LED, small bitmap so center and move

  DrawLed;
  offsetX := FRangeLEDSettings.OffsetX + gaugeCenX - FLEDActiveBitmap.Width div 2;
  offsetY := FRangeLEDSettings.OffsetY + gaugeCenY - FLEDActiveBitmap.height div 2;

  // set up the led, if user sets Active state will keep led on even if
  // the out of range state is set.

  if FRangeLEDSettings.Active then
    FGaugeBitmap.BlendImage(offsetX, offsetY, FLEDActiveBitmap, boLinearBlend)
  else
    FGaugeBitmap.BlendImage(offsetX, offsetY, FLEDInActiveBitmap, boLinearBlend);

  // Draw Markers BEFORE the pointer(s)

  DrawMarkers;
  FGaugeBitmap.BlendImage(0, 0, FMarkerBitmap,boLinearBlend);

  // draw cap over or under the pointer. Note that the pointer is a special
  // case when drawing since it's almost always dirty.

  if PointerCapSettings.CapStyle <> csNone then
    begin
      DrawPointerCap;
      offsetX := gaugeCenX  - FPointerCapBitmap.Width div 2;
      offsetY := gaugeCenY - FPointerCapBitmap.Height div 2;

      if PointerCapSettings.CapPosition = cpOver then
        begin
          DrawPointer;
          FGaugeBitmap.BlendImage(offsetX, offsetY, FPointerCapBitmap, boLinearBlend); // Cap on top
        end
      else
        begin
          FGaugeBitmap.BlendImage(offsetX, offsetY, FPointerCapBitmap, boLinearBlend); // Cap on Bottom
          DrawPointer;
        end;
    end
      else
        DrawPointer;

  // make it all visable to the user!

  FGaugeBitmap.Draw(Canvas, 0, 0, False);
end;

procedure TSGCustomSuperGauge.DrawMulti;
begin
  // The strategy here is that these typically only change infrequently
  // so if so, just draw as a bundle and saves some blendimages calls. Each of the
  // drawXXX still handles it's own dirty flag. The bitmap will be set up
  // as on instantiation so all of the others have their dirty flag set True, so no
  // need to do any initialization. Makes painting much faster even
  // with the individual dirty flags!

  if FFrameSettings.Dirty or FFaceSettings.Dirty or FScaleSettings.Dirty or IsAnyBandDirty then
    begin
      Initializebitmap(FMultiBitmap, Width, Height);

      DrawFrame;
      FMultiBitmap.BlendImage(0, 0, FFrameBitmap, boLinearBlend);

      DrawFace;
      FMultiBitmap.BlendImage(0, 0, FFaceBitmap, boLinearBlend);

      DrawBands; // will handle the enable/disable and draw of each band
      FMultiBitmap.BlendImage(0, 0, FBandBitmap, boLinearBlend);

      DrawScale;
      FMultiBitmap.BlendImage(0, 0, FScaleBitmap, boLinearBlend);
    end;
end;

procedure TSGCustomSuperGauge.DrawFrame;
var
  Origin: TSGOrigin;
  r: integer;
begin
  if not FrameSettings.Dirty then
    Exit;

  FrameSettings.Dirty := False;

  Origin := Initializebitmap(FFrameBitmap, Width, Height);

  // Always fills the space so AutoScale is sorta' always on

  r := round(Origin.Radius * 0.95);

  // Draw Bitmap frame

  FFrameBitmap.FillEllipseAntialias(Origin.CenterPoint.x,
    Origin.CenterPoint.y,
    r, r, FFrameSettings.FrameColor);

  // Draw thin antialiased border to smooth against background

  FFrameBitmap.EllipseAntialias(Origin.CenterPoint.x,
    Origin.CenterPoint.y,
    r, r, FFrameSettings.BorderColor, FFrameSettings.BorderRadius);
end;

procedure TSGCustomSuperGauge.DrawFace;
var
  OriginFace: TSGOrigin;
  r, d: integer;
  xb, yb: integer;
  d2, h: single;
  Center: TPointF;
  v: TPointF;
  p: PBGRAPixel;
  Image: TBGRABitmap;
  Mask: TBGRABitmap;
  Map: TBGRABitmap;

begin
  if not FaceSettings.Dirty then
    Exit;

  FaceSettings.Dirty := False;

  OriginFace := Initializebitmap(FFaceBitmap, Width, Height);

  // Always fills the space so AutoScale is sorta' always on for the face

  r := round(OriginFace.Radius * 0.95) - 5;

  // Fill types : fsNone, fsGradient, fsFlat, fsPhong

  case FFaceSettings.FillStyle of
    fsGradient:
      begin
        FFaceBitmap.FillEllipseLinearColorAntialias(OriginFace.CenterPoint.x,
          OriginFace.CenterPoint.y, r, r, FFaceSettings.OuterColor,
          FFaceSettings.InnerColor);
      end;

    fsFlat:
      begin
        FFaceBitmap.FillEllipseAntialias(OriginFace.CenterPoint.x, OriginFace.CenterPoint.y,
          r, r, FFaceSettings.InnerColor);
      end;

    fsPhong:
      begin
        d := r * 2;
        Center := PointF((d - 1) / 2, (d - 1) / 2);
        Map := TBGRABitmap.Create(d, d);

        for yb := 0 to d - 1 do
        begin
          p := Map.ScanLine[yb];

          for xb := 0 to d - 1 do
          begin
            // compute vector between center and current pixel

            v := PointF(xb, yb) - Center;

            // scale down to unit circle (with 1 pixel margin for soft border)

            v.x := v.x / (r + 1);
            v.y := v.y / (r + 1);

            // compute squared distance with scalar product

            d2 := v {$if FPC_FULLVERSION < 30203}*{$ELSE}**{$ENDIF} v;

            // interpolate as quadratic curve and apply power function

            if d2 > 1 then
              h := 0
            else
              h := power(1 - d2, FFaceSettings.CurveExponent);
            p^ := MapHeightToBGRA(h, 255);
            Inc(p);
          end;
        end;

        // mask image round with and antialiased border

        Mask := TBGRABitmap.Create(d, d, BGRABlack);
        Mask.FillEllipseAntialias(Center.x, Center.y, r, r, BGRAWhite);
        Map.ApplyMask(Mask);
        Mask.Free;

        // now draw

        FFaceSettings.FPhong.Draw(FFaceBitmap, Map, 30,
                OriginFace.CenterPoint.x - r, OriginFace.CenterPoint.y - r,
                FFaceSettings.InnerColor);
        Map.Free;
      end;
  end;

  // see if valid size and enabled, draw if so!

  if ((FaceSettings.Picture.Width > 0) or (FaceSettings.Picture.Height > 0)) and (FFaceSettings.PictureEnabled) then
  begin

    Image := TBGRABitmap.Create(FaceSettings.Picture.Bitmap);
    FFaceBitmap.BlendImage(
                OriginFace.CenterPoint.X + FaceSettings.PictureOffsetX,
                OriginFace.CenterPoint.y + FaceSettings.PictureOffsetY,
                image,
                boLinearBlend);
    Image.Free; // needed!
  end;
end;

procedure TSGCustomSuperGauge.DrawBands;
var
  i: integer;
begin
  // Draw mult bands on the same bitmap. we can do this since
  // we are drawing over the entire fullsized bitmap. Since
  // this is the case, you can draw all of the bands here in one shot
  // and on one bitmap. Init bitmap here!

  // Only change if something dirty
  // nothing dirty, no init, no draw, just bounce!

  if not IsAnyBandDirty then
    exit;

  Initializebitmap(FBandBitmap, Width, Height); // clear it before we draw anything

  for i := low(FBandsSettings) to high(FBandsSettings) do
  begin
    FBandsSettings[i].Dirty := True;  // force draw, if any band is dirty they are are dirty
    DrawBand(FBandsSettings[i]);      // will clear any dirty for specific band
  end;
end;

procedure TSGCustomSuperGauge.DrawBand(const BandSettings : TSGBandSettings);
var
  BandRadius, TextRadius: single;
  TextSize: integer;
  baseAngle, startAngle, endAngle: single;
  cenX, cenY: integer;
  fontRenderer: TBGRAVectorizedFontRenderer;
  TextPath: TBGRAPath;
begin

  // TODO : Maybe be removed since calling here always paints them all
  if not BandSettings.Dirty then
    Exit;

  BandSettings.Dirty := False;

  // Now, if not enabled we can leave if flag reset!

  if not BandSettings.Enabled then
    exit;

  TextSize := BandSettings.TextSize * 15;

  // Origin := Initializebitmap(FBandBitmap, Width, Height); drawbands needs to set this up

  cenX := Width div 2;
  cenY := Height div 2;

  BandRadius := BandSettings.BandRadius - BandSettings.Thickness div 2;    // may need to adjust for band thickness
  TextRadius := BandSettings.TextRadius - BandSettings.TextSize div 2 - BandSettings.Thickness div 2; // offset to center

  // Start = 225 degree is 0 on gague scale (Not the angle), and -45 degree is 100 on scale

  // 270, down (gauge angle 0)180 flat, increase moves towards 0 decrease towards 100
  // 0 is flat line, right most end. Increase goes backwards towards 0, -45 is 100 percent on scale

  baseAngle := 225 * PI / 180;
  startAngle := baseAngle - ((BandSettings.StartValue * 270 / 100) * PI / 180);
  endAngle := baseAngle - ((BandSettings.EndValue * 270 / 100) * PI / 180);

  FBandBitmap.LineCap := pecFlat; // caps should be flat
  FBandBitmap.Arc(
                   cenX, cenY,
                   BandRadius + 0.5, BandRadius + 0.5, // push down a bit
                   // (360-135) 225, -45
                   // 3.92699,-0.785398, // must use start and end angle, internally Point calcs won't work due to arcsin2() limits
                   startAngle, endAngle,
                   BandSettings.BandColor,
                   BandSettings.Thickness,
                   false,
                   BGRA(0,0,0,0) // last param is alpha, so no interior color, inner routings ONLY draw the arc, no fill
  );

  if BandSettings.EnableText then
  begin
    FontRenderer := TBGRAVectorizedFontRenderer.Create;
    FBandBitmap.FontRenderer := fontRenderer;     // assign text vectorial font renderer
    FBandBitmap.FontHeight := round(TextSize * 0.09);
    FBandBitmap.FontQuality := fqFineAntialiasing;
    FBandBitmap.FontName := BandSettings.TextFont;
    FBandBitmap.FontStyle := BandSettings.TextStyle;
    FontRenderer.OutlineColor := BGRABlack;
    FontRenderer.OutlineWidth := TextSize / 600;
    FontRenderer.OutlineVisible := true;
    FBandBitmap.FontVerticalAnchor := fvaBaseline;
    TextPath := TBGRAPath.Create;

    // drawing is backwards on textpath

    TextPath.Arc(cenX, cenY, TextRadius, -startAngle, -endAngle, False);
    FBandBitmap.TextOutCurved(TextPath, BandSettings.Text, BandSettings.TextColor, taCenter, 0);
  end;
end;

procedure TSGCustomSuperGauge.DrawText(TextBitmap: TBGRABitmap; const TextSettings: TSGTextSettings);
var
  TextBoxWidth, TextBoxHeight: integer;
  TextRect: TRect;
begin

  if not TextSettings.Dirty then
    Exit;

  TextSettings.Dirty := False;

  // get the bounding box so we can create a SMALLER bitmap. This will be referenced
  // to the Center of the text and gauge

  CalculateTextSize(TextSettings.Text, TextSettings.FontEx, TextBoxWidth, TextBoxHeight, TextSettings.FontEx.Shadow);
  Initializebitmap(TextBitmap, TextBoxWidth, TextBoxHeight);

  // Set up text bounding box,

  TextRect.Left := 0;
  TextRect.Top := 0;
  TextRect.Height := TextBoxHeight;
  TextRect.Width := TextBoxWidth;

  // Draw into the TextBitmap for later use

  RenderText(TextRect, TextSettings.FontEx, TextSettings.Text, TextBitmap, Enabled);
end;

procedure TSGCustomSuperGauge.DrawScale;
var
  Origin: TSGOrigin;
  i, n, x, y, xt, yt: integer;
  scaleStartValue, scaleBump: integer;
  ScaleRadius, TextRadius: single;
  TextSize: integer;
  pStart, pEnd: TPointF;
  startAngle, endAngle: single;
  innerTicRadius: single;
begin
  // if nothing dirty then skip it, we have a bitmap with
  // the scale already drawn. This is slow so saves a lot of time
  // as scales are slow to draw

  if not ScaleSettings.Dirty then
    Exit;

  ScaleSettings.Dirty := False;  // mark as clean, so next run will not need a rebuild!

  Origin := Initializebitmap(FScaleBitmap, Width, Height);

  // Calc radius for scale and text or set it from the user

  if FAutoScale then
  begin
    ScaleRadius := Round(Origin.Radius * 0.90);
    TextRadius := Round(Origin.Radius * 0.65);
    TextSize := Round(Origin.Radius * 0.15);

    // fix up scaling for small or large gauges

    if (Width < 250) or (Height < 250) then
    begin
      TextSize := 15;
      TextRadius := TextRadius - 10;
    end
    else
      begin
        if (Width > 500) or (Height > 500) then
        begin
          TextSize := TextSize + 5;
          TextRadius := TextRadius + 10;
        end;
      end;
    end
  else
    begin
      ScaleRadius := ScaleSettings.ScaleRadius;
      TextRadius := ScaleSettings.TextRadius;
      TextSize := ScaleSettings.TextSize;
    end;

  // Draw SubTicks

  if ScaleSettings.EnableSubTicks then
  begin
    n := ScaleSettings.MainTickCount * ScaleSettings.SubTickCount;

    for i := 0 to n do
    begin
      // Calculate draw from point

      X := Origin.CenterPoint.x - Round(ScaleRadius * cos((-45 + i * 270 / n) * Pi / 180));
      Y := Origin.CenterPoint.y - Round(ScaleRadius * sin((-45 + i * 270 / n) * Pi / 180));

      // Calculate draw to point

      Xt := Origin.CenterPoint.x - Round((ScaleRadius - ScaleSettings.LengthSubTick) *
        cos((-45 + i * 270 / n) * Pi / 180));
      Yt := Origin.CenterPoint.y - Round((ScaleRadius - ScaleSettings.LengthSubTick) *
        sin((-45 + i * 270 / n) * Pi / 180));

      FScaleBitmap.DrawLineAntialias(x, y, xt, yt, ScaleSettings.TickColor, ScaleSettings.ThicknessSubTick);

      if (ScaleSettings.TickArcStyle = taboth) and (not ScaleSettings.EnableMainTicks) then
        begin
          // need caps on the ends so the gauge doesn't look stupid if both inner and outer
          // tic arcs are visiable

          if (i = 0) or (i = n) then
            begin
              if not ScaleSettings.EnableMainTicks then
                innerTicRadius := ScaleSettings.LengthSubTick
              else
                innerTicRadius := ScaleSettings.LengthMainTick;

              // draw end pieces in the MainTick thickness to match

              Xt := Origin.CenterPoint.x - Round((ScaleRadius - innerTicRadius) *
                cos((-45 + i * 270 / n) * Pi / 180));
              Yt := Origin.CenterPoint.y - Round((ScaleRadius - innerTicRadius) *
                sin((-45 + i * 270 / n) * Pi / 180));

              FScaleBitmap.DrawLineAntialias(x, y, xt, yt, ScaleSettings.TickColor,
                ScaleSettings.ThicknessMainTick);
            end;
        end;
    end;
  end;

  // Draw after the sub-tics

  if ScaleSettings.EnableMainTicks then
  begin
    n := ScaleSettings.MainTickCount;

    for i := 0 to n do
    begin

      // Draw main ticks
      // Calculate draw from point bottom

      x := Origin.CenterPoint.x - Round(ScaleRadius * cos((-45 + i * 270 / n) * Pi / 180));
      y := Origin.CenterPoint.y - Round(ScaleRadius * sin((-45 + i * 270 / n) * Pi / 180));

      // Calculate draw to point top

      xt := Origin.CenterPoint.x - Round((ScaleRadius - ScaleSettings.LengthMainTick) *
        cos((-45 + i * 270 / n) * Pi / 180));
      yt := Origin.CenterPoint.y - Round((ScaleRadius - ScaleSettings.LengthMainTick) *
        sin((-45 + i * 270 / n) * Pi / 180));

      FScaleBitmap.DrawLineAntialias(x, y, xt, yt, ScaleSettings.TickColor, ScaleSettings.ThicknessMainTick);
    end;
  end;

  // Draw text, these are only for the Main Ticks

  if ScaleSettings.EnableScaleText then
    begin

      FScaleBitmap.FontName := ScaleSettings.TextFont;
      FScaleBitmap.FontHeight := TextSize;
      FScaleBitmap.FontQuality := fqFineAntialiasing;
      FScaleBitmap.FontStyle := FScaleSettings.TextStyle;

      n := ScaleSettings.MainTickCount;

      // if draw the scale reversed, do some tricky stuff so we can
      // count up or down. Start is swapped with the actual end value here

      if ScaleSettings.ReverseScale then
      begin
        scaleBump := -1;
        scaleStartValue := n * ScaleSettings.Step + ScaleSettings.Start;
      end
      else
      begin
        scaleBump := 1;
        scaleStartValue := ScaleSettings.Start;
      end;

      // Draw text for main ticks

      for i := 0 to n do
      begin
        xt := Origin.CenterPoint.x - Round(TextRadius * cos((-45 + i * 270 / n) * Pi / 180));
        yt := Origin.CenterPoint.y - Round(TextRadius * sin((-45 + i * 270 / n) * Pi / 180));

        FScaleBitmap.TextOut(xt, yt - (FScaleBitmap.FontHeight / 1.7),
          IntToStr(scaleStartValue + i * ScaleSettings.Step * scaleBump),
          ScaleSettings.TextColor, taCenter);
      end;
    end;

    // draw outer rings/bands

    if (ScaleSettings.TickArcStyle = taOuter) or (ScaleSettings.TickArcStyle = taboth) then
    begin
      // draw arc OUSIDE on the tics, doesn't matter main or sub, all at the top
      // inner of tic

      pStart.x := Origin.CenterPoint.x - Round(ScaleRadius * cos(-45 * Pi / 180));
      pStart.y := Origin.CenterPoint.y - Round(ScaleRadius * sin(-45 * Pi / 180));

      startAngle := arctan2((Origin.CenterPoint.y - pStart.y),(Origin.CenterPoint.x - pStart.x)) + 4.71239; // add 270

      // Calculate draw to point outer

      pEnd.x := Origin.CenterPoint.x - Round((ScaleRadius - ScaleSettings.LengthMainTick) * cos(225 * Pi / 180));
      pEnd.y := Origin.CenterPoint.y - Round((ScaleRadius - ScaleSettings.LengthMainTick) * sin(225 * Pi / 180));

      endAngle :=  -arctan2((pEnd.y - Origin.CenterPoint.y),(pEnd.x - Origin.CenterPoint.x));
      FScaleBitmap.Arc(
                     Origin.CenterPoint.x, Origin.CenterPoint.y,
                     ScaleRadius + 0.5, ScaleRadius + 0.5, // push down a bit
                     startAngle, endAngle,
                     ScaleSettings.TickColor,
                     ScaleSettings.ThicknessMainTick,
                     false,
                     BGRA(0,0,0,0) // last param is alpha, so no interior color, inner routings ONLY draw the arc, no fill
                     );
    end;

    if (ScaleSettings.TickArcStyle = taInner) or (ScaleSettings.TickArcStyle = taBoth) then
    begin
      // Inner will chose main tics (for now) if both main and sub tics on)
      // will need to find out the radius for what selected... or do something
      // like use what ever tic is LONGER (logic here will need a change)

      // draw arc OUSIDE on the tics, doesn't matter main or sub, all at the top

      // inner of tick

      pStart.x := Origin.CenterPoint.x - Round(ScaleRadius * cos(-45 * Pi / 180));
      pStart.y := Origin.CenterPoint.y - Round(ScaleRadius * sin(-45 * Pi / 180));

      startAngle := arctan2((Origin.CenterPoint.y - pStart.y),(Origin.CenterPoint.x - pStart.x)) + 4.71239; // add 270

      // Calculate draw to point outer

      pEnd.x := Origin.CenterPoint.x - Round((ScaleRadius - ScaleSettings.LengthMainTick) * cos(225 * Pi / 180));
      pEnd.y := Origin.CenterPoint.y - Round((ScaleRadius - ScaleSettings.LengthMainTick) * sin(225 * Pi / 180));

      endAngle := -arctan2((pEnd.y - Origin.CenterPoint.y),(pEnd.x - Origin.CenterPoint.x));

      // be nice and if not displaying main tics, use the sub tic length to bottom
      // up against them

      if not ScaleSettings.EnableMainTicks then
        innerTicRadius := ScaleSettings.LengthSubTick
     else
        innerTicRadius := ScaleSettings.LengthMainTick;

      FScaleBitmap.Arc(
                     Origin.CenterPoint.x, Origin.CenterPoint.y,
                     ScaleRadius - 0.5 - innerTicRadius, ScaleRadius - 0.5 - innerTicRadius,
                     startAngle, endAngle,
                     ScaleSettings.TickColor,
                     ScaleSettings.ThicknessMainTick,
                     false,
                     BGRA(0,0,0,0) // last param is alpha, so no interior color, inner routings ONLY draw the arc, no fill
                     );
    end;
end;

procedure TSGCustomSuperGauge.DrawPointer;
var
  Origin: TSGOrigin;
  x, y, x1, y1, extLen: integer;
  commonSubEx: single;
  PointerLength: single;
  startAngle, endAngle: single;
  bandRadius: single;
  vecLen: single;
  A, B, U, V: TPointF;
begin
  // Note : Min and max values are the GAUGE Settings, not the Scales,
  //        the scale display is independant of the value of the gauge to
  //        allow for multiple pointers if later needed

  if not PointerSettings.Dirty then
    Exit;

  Origin.CenterPoint.X:= FGaugeBitmap.Width div 2;
  Origin.CenterPoint.Y:= FGaugeBitmap.Height div 2;

  // radius is smaller of the 2 dimensions

  if Origin.CenterPoint.x < Origin.CenterPoint.y then
    Origin.Radius := Origin.CenterPoint.x
  else
    Origin.Radius := Origin.CenterPoint.Y;

  // Set the pointer length, does not apply to arc

  if FAutoScale then
    begin
      PointerLength := Round(Origin.Radius * 0.85);
    end
    else
      begin
        PointerLength := PointerSettings.Length;
      end;

  // draw the arc style of pointer

  if (PointerSettings.Style = psLine) or  (PointerSettings.Style = psLineExt) then
    begin
      // if we are need to draw the extension behind the cap, we can
      // recalc the ending point to just do one line draw instead of
      // 2 discrete lines from the center. That is easier, but slower
      // If extension len is 0, skip as will show a partial pixel

      FGaugeBitMap.LineCap := pecRound; // caps should be round for line type pointers

      if (PointerSettings.Style = psLineExt) and (PointerSettings.ExtensionLength > 0) then
        begin
          // The extension is always pixels visable from the center or edge of the
          // cap, fix as needed. Makes nice for the user.

          if PointerCapSettings.CapStyle = csNone then
            extLen := PointerSettings.ExtensionLength
          else
            extLen := PointerSettings.ExtensionLength + PointerCapSettings.Radius;

          // compute end point of pointer if an extension

          commonSubEx := (-225 + FValue) * Pi / 180;
          x1 := Origin.CenterPoint.x - Round(extLen * cos(commonSubEx));
          y1 := Origin.CenterPoint.y - Round(extLen * sin(commonSubEx));

        end
          else
            begin
              // no extension or extension length is 0, just draw to center

              x1 := Origin.CenterPoint.x;
              y1 := Origin.CenterPoint.y;
            end;

      // computer start point of pointer

      commonSubEx := (-45 + FValue) * Pi / 180;
      x := Origin.CenterPoint.x - Round(PointerLength * cos(commonSubEx));
      y := Origin.CenterPoint.y - Round(PointerLength * sin(commonSubEx));

      // finally draw it

      FGaugeBitMap.DrawLineAntialias(x, y, x1, y1, PointerSettings.Color, PointerSettings.Thickness)
    end
      else
        if PointerSettings.Style = psTriangle then
          begin
              // Draw a Triangle style pointer

              // Draw from center point out

              commonSubEx := (-45 + FValue) * Pi / 180;
              x := Origin.CenterPoint.x;
              y := Origin.CenterPoint.y;
              A := PointF(x, y);

              // Calculate draw to point top

              x1 := Origin.CenterPoint.x - Round(PointerSettings.Length * cos(commonSubEx));
              y1 := Origin.CenterPoint.y - Round(PointerSettings.Length * sin(commonSubEx));
              B := PointF(x1, y1);

              // set line cap just in case

              FMarkerBitmap.LineCap := pecRound; // Ensure Round Cap

              // This is the vector that runs from outer to inner

              U := B - A;

              // build the perpendicular vector
              // (clockwise in screen coordinates while the opposite would be counter clockwise)

              V := PointF(-U.y, U.x);

              // scale it to set the new segment length

              vecLen := VectLen(V);

              // catch odd case of zero len vector, do nothing

              if vecLen = 0.0 then
                Exit;

              V := V * (PointerSettings.Thickness / vecLen);

              // draw a full triangle pointer

              FGaugeBitMap.FillPolyAntialias([B, A + V, A - V], PointerSettings.Color);
          end
            else
              if PointerSettings.Style = psArc then
                begin
                  // drawn arc pointer, ensure not negative or crash, zero no need to draw

                  if FValue <= 0.0 then
                    Exit;

                   BandRadius := PointerLength - PointerSettings.Thickness div 2;    // adjust for band thickness so end of pointer is top

                   // Start = 225 degree is 0 on gague scale (Not the angle), and -45 degree is 100 on scale
                   // 270, down (gauge angle 0)180 flat, increase moves towards 0 decrease towards 100
                   // 0 is flat line, right most end. Increase goes backwards towards 0, -45 is 100 percent on scale

                   startAngle := 225 * PI / 180;  // start at 0 on the gauge
                   endAngle := startAngle - FValue * PI / 180;

                   FGaugeBitMap.LineCap := pecFlat; // caps should be flat, rounded does not align to scales well
                   FGaugeBitMap.Arc(
                                    Origin.CenterPoint.x, Origin.CenterPoint.y,
                                    BandRadius + 0.5, BandRadius + 0.5, // push down a bit
                                    startAngle, endAngle,
                                    PointerSettings.Color,
                                    PointerSettings.Thickness,
                                    false,
                                    BGRA(0,0,0,0) // last param is alpha, so no interior color, inner routings ONLY draw the arc, no fill
                               );
                end;
end;

procedure TSGCustomSuperGauge.DrawPointerCap;
var
  Origin: TSGOrigin;
  sizeWH : integer;
  pCapEdge : integer;
  tx, ty: integer;
  h: single;
  d2: single;
  v: TPointF;
  p: PBGRAPixel;
  Center: TPointF;
  yb: integer;
  xb: integer;
  mask: TBGRABitmap;
  Map: TBGRABitmap;

begin

  // skip drawing if nothing changed

  if not PointerCapSettings.Dirty then
    Exit;

  PointerCapSettings.Dirty := False;

  // drawing is the size of the cap, not of the entire gauge!

  sizeWH := (PointerCapSettings.Radius + PointerCapSettings.EdgeThickness) * 2 + 2;
  Origin := Initializebitmap(FPointerCapBitmap, SizeWH, SizeWH);
  pCapEdge := PointerCapSettings.Radius + PointerCapSettings.EdgeThickness div 2;

  if PointerCapSettings.CapStyle = csFlat then
    begin
      // Draw the flat cap, but make sure size is similar to the shaded below or will be odd

      FPointerCapBitmap.EllipseAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y,
        pCapEdge,
        pCapEdge,
        PointerCapSettings.EdgeColor,
        PointerCapSettings.EdgeThickness,
        PointerCapSettings.FillColor);
    end
    else
      begin

        tx := PointerCapSettings.Radius * 2; // keeps size consistent with flat cap
        ty := tx;

        if (tx = 0) or (ty = 0) then
          Exit;

        if PointerCapSettings.CapStyle = csPhong then
          begin
            //compute knob height map

            Center := PointF((tx - 1) / 2, (ty - 1) / 2);
            Map := TBGRABitmap.Create(tx, ty);

            for yb := 0 to ty - 1 do
            begin
              p := map.ScanLine[yb];
              for xb := 0 to tx - 1 do
              begin
                //compute vector between center and current pixel

                v := PointF(xb, yb) - Center;

                //scale down to unit circle (with 1 pixel margin for soft border)

                v.x := v.x / (tx / 2 + 1);
                v.y := v.y / (ty / 2 + 1);

                //compute squared distance with scalar product

                d2 := v {$if FPC_FULLVERSION < 30203}*{$ELSE}**{$ENDIF} v;

                //interpolate as quadratic curve and apply power function

                if d2 > 1 then
                  h := 0
                else
                  h := power(1 - d2, PointerCapSettings.CurveExponent);
                p^ := MapHeightToBGRA(h, 255);
                Inc(p);
              end;
            end;

            // mask image round with and antialiased border

            mask := TBGRABitmap.Create(tx, ty, BGRABlack);
            Mask.FillEllipseAntialias(Center.x, Center.y, tx / 2, ty / 2, BGRAWhite);
            map.ApplyMask(mask);
            Mask.Free;

            // now draw

            PointerCapSettings.FPhong.Draw(FPointerCapBitmap, Map, 30,
                    origin.CenterPoint.x - tx div 2, origin.CenterPoint.y - ty div 2,
                    PointerCapSettings.FillColor);
            Map.Free;

            // Draw a flat radius around the cap if set, must be alpha 0 or will not
            // be an outline

            if PointerCapSettings.EdgeThickness > 0 then
              FPointerCapBitmap.EllipseAntialias(origin.CenterPoint.x, origin.CenterPoint.y,
                pCapEdge,
                pCapEdge,
                PointerCapSettings.EdgeColor,
                PointerCapSettings.EdgeThickness, BGRA(0,0,0,0));
          end
        else
          begin
            // Regular shading

            FPointerCapBitmap.FillEllipseLinearColorAntialias(origin.CenterPoint.x, origin.CenterPoint.y,
              pCapEdge,
              pCapEdge,
              PointerCapSettings.FillColor,
              PointerCapSettings.EdgeColor
              );

            // draw edge since the shading is backwards ending on fill color not Edge

            FPointerCapBitmap.EllipseAntialias(origin.CenterPoint.x, origin.CenterPoint.y,
              pCapEdge,
              pCapEdge,
              PointerCapSettings.EdgeColor,
              PointerCapSettings.EdgeThickness, BGRA(0,0,0,0)
              );

          end;
      end;
end;

procedure TSGCustomSuperGauge.DrawLed;
var
  Origin: TSGOrigin;
  sizeWH : integer;
  mask: TBGRABitmap;
begin

  // skip drawing if nothing changed or not drawn

  if not FRangeLEDSettings.Dirty then
    Exit;

  FRangeLEDSettings.Dirty := False;

  // compute the size needed NOT a full gauge bitmap

  sizeWH := FRangeLEDSettings.Size * 2 + 2; // square size at lease LED radius and a bit more
  Origin := Initializebitmap(FLEDActiveBitmap, sizeWH, sizeWH);
  Initializebitmap(FLEDInActiveBitmap, sizeWH, sizeWH);

  // offset must be done later in the Paint proc to
  // keep bitmap small so the center point is the centerpoint of the bitmap
  // The caller MUST move to the correct offset

  // draw both active and inactive so we never need to do either unless props changed
  // need to find/get x, y to place the LED

  if RangeLEDSettings.Shape = lshRound then
    begin
    if FRangeLEDSettings.Style = lsFlat then
      begin
        FLEDActiveBitmap.EllipseAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y,
          FRangeLEDSettings.Size,
          FRangeLEDSettings.Size,
          FRangeLEDSettings.BorderColor,
          1,
          FRangeLEDSettings.ActiveColor);
      end
    else
      if FRangeLEDSettings.Style = lsShaded then
        begin
          // draw shaded, could do better here but good for starts

          FLEDActiveBitmap.FillEllipseLinearColorAntialias(
            Origin.CenterPoint.x,
            Origin.CenterPoint.y,
            FRangeLEDSettings.Size,
            FRangeLEDSettings.Size,
            FRangeLEDSettings.InactiveColor,
            FRangeLEDSettings.ActiveColor);

          // draw border

          FLEDActiveBitmap.EllipseAntialias(
            Origin.CenterPoint.x, Origin.CenterPoint.y,
            FRangeLEDSettings.Size,
            FRangeLEDSettings.Size,
            FRangeLEDSettings.BorderColor,
            1,
            BGRA(0,0,0,0));  // fill transparent
        end;

    // Simple flat round for inactive always

    if RangeLedSettings.Style <> lsNone then
      begin
        FLEDInactiveBitmap.EllipseAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y,
          FRangeLEDSettings.Size,
          FRangeLEDSettings.Size,
          FRangeLEDSettings.BorderColor,
          1,
          FRangeLEDSettings.InActiveColor);
      end;
    end // Round
      else
        if RangeLEDSettings.Shape = lshSquare then
          begin
            // draw a Square LED

            if FRangeLEDSettings.Style = lsFlat then
              begin
                // Flat

                FLEDActiveBitmap.FillRoundRectAntialias(
                    0, 0,
                    FLEDActiveBitmap.Width,
                    FLEDActiveBitmap.Height,
                    Origin.Radius / 2,
                    Origin.Radius / 2,
                    FRangeLEDSettings.ActiveColor);

                // draw border for Flat

                FLEDActiveBitmap.RoundRectAntialias(
                    0,0,
                    FLEDActiveBitmap.Width - 1,
                    FLEDActiveBitmap.Height - 1,
                    Origin.Radius / 2,
                    Origin.Radius / 2,
                    FRangeLEDSettings.BorderColor,
                    1);
              end
            else
              if FRangeLEDSettings.Style = lsShaded then
                begin
                  // draw shaded

                  FLEDActiveBitmap.GradientFill(
                      0, 0,
                      FLEDActiveBitmap.Width,
                      FLEDActiveBitmap.Height,
                      FRangeLEDSettings.ActiveColor,
                      BGRA(0,0,0),
                      gtRadial,
                      PointF(FLEDActiveBitmap.Width / 2, FLEDActiveBitmap.Height / 2),
                      PointF(FLEDActiveBitmap.Width * 1.5,FLEDActiveBitmap.Height * 1.5),
                      dmSet);

                  mask := TBGRABitmap.Create(FLEDActiveBitmap.Width, FLEDActiveBitmap.Height, BGRABlack);
                  mask.FillRoundRectAntialias(
                      0, 0,
                      FLEDActiveBitmap.Width,
                      FLEDActiveBitmap.Height,
                      Origin.Radius / 2,
                      Origin.Radius / 2,
                      BGRAWhite);

                  FLEDActiveBitmap.ApplyMask(mask);
                  mask.Free;

                  // draw border for shaded

                  FLEDActiveBitmap.RoundRectAntialias(
                            0, 0,
                            FLEDActiveBitmap.Width - 1,
                            FLEDActiveBitmap.Height - 1,
                            Origin.Radius / 2,
                            Origin.Radius / 2,
                            FRangeLEDSettings.BorderColor,
                            1);
                end;

            // Simple flat square for inactive always

            if RangeLEDSettings.Style <> lsNone then
              begin
                // Need to draw the filled
                FLEDInactiveBitmap.FillRoundRectAntialias(
                    0, 0,
                    FLEDActiveBitmap.Width,
                    FLEDActiveBitmap.Height,
                    Origin.Radius / 2,
                    Origin.Radius / 2,
                    FRangeLEDSettings.InactiveColor);

                // Now the border
                FLEDInactiveBitmap.RoundRectAntialias(
                          0, 0,
                          FLEDActiveBitmap.Width - 1,
                          FLEDActiveBitmap.Height - 1,
                          Origin.Radius / 2,
                          Origin.Radius / 2,
                          FRangeLEDSettings.BorderColor,
                          1);
              end;
          end // square
        else
          if RangeLEDSettings.Shape = lshTriangle then
            begin
              // draw a triangle and border

              if FRangeLEDSettings.Style = lsFlat then  // TODO : add lsShaded
                begin
                  FLEDActiveBitmap.DrawPolyLineAntialias(
                      [ PointF(FLEDActiveBitmap.Width / 2, 1),
                        PointF(FLEDActiveBitmap.Width - 1, FLEDActiveBitmap.Height - 1),
                        PointF(1, FLEDActiveBitmap.Height - 1),
                        PointF(FLEDActiveBitmap.Width / 2, 1) // close it for border
                      ],
                      FRangeLEDSettings.BorderColor,
                      1,
                      FRangeLEDSettings.ActiveColor);

                end
              else
                if FRangeLEDSettings.Style = lsShaded then
                  begin
                    // draw shaded
                     FLEDActiveBitmap.FillPolyLinearColor(
                          [ PointF(FLEDActiveBitmap.Width / 2, 1),
                          PointF(FLEDActiveBitmap.Width - 1, FLEDActiveBitmap.Height - 1),
                          PointF(1, FLEDActiveBitmap.Height - 1)],
                          [FRangeLEDSettings.InactiveColor,
                          FRangeLEDSettings.ActiveColor,
                          FRangeLEDSettings.ActiveColor]);
                    // draw border
                     FLEDActiveBitmap.DrawPolyLineAntialias(
                         [ PointF(FLEDActiveBitmap.Width / 2, 1),
                           PointF(FLEDActiveBitmap.Width - 1, FLEDActiveBitmap.Height - 1),
                           PointF(1, FLEDActiveBitmap.Height - 1),
                           PointF(FLEDActiveBitmap.Width / 2, 1) // close it for border
                         ],
                         FRangeLEDSettings.BorderColor,
                         1,
                         BGRA(0,0,0,0));
                  end;
                if RangeLEDSettings.Style <> lsNone then
                  begin
                    FLEDInactiveBitmap.DrawPolyLineAntialias(
                        [ PointF(FLEDActiveBitmap.Width / 2, 1),
                          PointF(FLEDActiveBitmap.Width - 1, FLEDActiveBitmap.Height - 1),
                          PointF(1, FLEDActiveBitmap.Height - 1),
                          PointF(FLEDActiveBitmap.Width / 2, 1) // close it for border
                        ],
                        FRangeLEDSettings.BorderColor,
                        1,
                        FRangeLEDSettings.InactiveColor);
                  end;
            end // triangle
          else
            if RangeLEDSettings.Shape = lshDownTriangle then
              begin
                  // draw a downward pointing triangle and border
                  if FRangeLEDSettings.Style = lsFlat then
                    begin
                      FLEDActiveBitmap.DrawPolyLineAntialias(
                        [ PointF(1,1),
                          PointF(FLEDActiveBitmap.Width / 2, FLEDActiveBitmap.Height  - 1),
                          PointF(FLEDActiveBitmap.Width - 1, 1),
                          PointF(1,1)
                        ],
                        FRangeLEDSettings.BorderColor,
                        1,
                        FRangeLEDSettings.ActiveColor);
                      end
                    else
                      if FRangeLEDSettings.Style = lsShaded then
                        begin
                          // draw shaded

                          FLEDActiveBitmap.FillPolyLinearColor(
                             [ PointF(1,1),
                               PointF(FLEDActiveBitmap.Width / 2, FLEDActiveBitmap.Height  - 1),
                               PointF(FLEDActiveBitmap.Width - 1, 1)
                             ],
                             [FRangeLEDSettings.InactiveColor,
                             FRangeLEDSettings.ActiveColor,
                             FRangeLEDSettings.ActiveColor]);

                          // draw border

                          FLEDActiveBitmap.DrawPolyLineAntialias(
                             [ PointF(1,1),
                               PointF(FLEDActiveBitmap.Width / 2, FLEDActiveBitmap.Height  - 1),
                               PointF(FLEDActiveBitmap.Width - 1, 1),
                               PointF(1,1)
                             ],
                               FRangeLEDSettings.BorderColor,
                               1,
                               BGRA(0,0,0,0));
                        end;
                  // Draw Inactive DownTri
                  if RangeLEDSettings.Style <> lsNone then
                  begin
                    FLEDInactiveBitmap.DrawPolyLineAntialias(
                      [ PointF(1,1),
                        PointF(FLEDActiveBitmap.Width / 2, FLEDActiveBitmap.Height  - 1),
                        PointF(FLEDActiveBitmap.Width - 1, 1),
                        PointF(1,1)
                      ],
                      FRangeLEDSettings.BorderColor,
                      1,
                      FRangeLEDSettings.InactiveColor);
                  end;

              end;
end;

procedure TSGCustomSuperGauge.DrawMarkers;
var
  i: integer;
begin
  if not IsAnyMarkerDirty then
    exit;

  Initializebitmap(FMarkerBitmap, Width, Height); // clear it before we draw anything

  // Draws low to high, so if overlapping, last will be visible

  for i := low(FMarkersSettings) to high(FMArkersSettings) do
  begin
    FMarkersSettings[i].Dirty := True; // need to dirty them all
    DrawMarker(FMarkerBitmap, FMarkersSettings[i]);    // will clear any dirty
  end;
end;

procedure TSGCustomSuperGauge.DrawMarker(MarkerBitmap: TBGRABitmap; const MarkerSettings: TSGMarkerSettings);
var
  x1, y1, x2, y2: integer;
  cenX, cenY: integer;
  j, vecLen: single;
  A, B, U, V: TPointF;

begin
  // skip drawing if nothing changed or not drawn

  if not MarkerSettings.Dirty then
    Exit;

  MarkerSettings.Dirty := False;

  if not MarkerSettings.Enabled then
    Exit;

  // Center of bitmap

  cenX := MarkerBitmap.Width div 2;
  cenY := MarkerBitmap.Height div 2;

  j := (180 - 270) / 2;
  x1 := cenX - Round(MarkerSettings.Radius * cos((j + MarkerSettings.Value * 270 / 100) * Pi / 180));
  y1 := cenY - Round(MarkerSettings.Radius * sin((j + MarkerSettings.Value * 270 / 100) * Pi / 180));
  A := PointF(x1,y1);

  // Calculate draw to point top

  x2 := cenX - Round((MarkerSettings.Radius - MarkerSettings.Height) * cos((j + MarkerSettings.Value * 270 / 100) * Pi / 180));
  y2 := cenY - Round((MarkerSettings.Radius - MarkerSettings.Height) * sin((j + MarkerSettings.Value * 270 / 100) * Pi / 180));
  B := PointF(X2, y2);

  // set line cap just in case

  FMarkerBitmap.LineCap := pecRound; // Ensure Round Cap

  // This is the vector that runs from outer to inner

  U := B - A;

  // build the perpendicular vector
  // (clockwise in screen coordinates while the opposite would be counter clockwise)

  V := PointF(-U.y, U.x);

  // scale it to set the new segment length

  vecLen := VectLen(V);

  // catch odd case of zero len vector, do nothing

  if vecLen = 0.0 then
    Exit;

  V := V * (MarkerSettings.Width / vecLen);

  case MarkerSettings.Style of
    msCenter: // triangle centered on the value
      begin
        MarkerBitmap.FillPolyAntialias([B, A + V, A - V], MarkerSettings.Color);
      end;

    msLeft:   // triangle left side only (if looking at it at half way on the gauge)
      begin
        MarkerBitmap.FillPolyAntialias([B, A + V, A], MarkerSettings.Color);
      end;

    msRight:
      begin   // triangle right side only
        MarkerBitmap.FillPolyAntialias([B, A, A - V], MarkerSettings.Color);
      end;
  end;
end;

/////////////////

function TSGCustomSuperGauge.CheckRangeLED(AValue: single): boolean;
begin
  // If a single value is used for both StartRangeValue and
  // EndRangeValue the option for rcBetween makes no sense and is a not valid
  // and will never trigger. Also Manually setting the .Active prop will ONLY
  // work if rcNone is set, otherwise the range checks will prevail as the
  // way the Active state is set and overide the manual setting.
  //
  // Current List
  // TSGRangeCheckType = (rcNone, rcBetween, rcBothInclusive, rcStartInclusive,
  //                      rcEndInclusive, rcBothBetweenOutside,
  //                      rcBothInclusiveOutside, rcGreaterStart, RangeEndValue);
  //
  // NOTE - rcGreaterStart, RangeEndValue ignore RangeEnd and RangeStart respectivly

  if FRangeLEDSettings.RangeType = rcNone then
  begin
    Result := FRangeLEDSettings.Active;   // need to always return the current state here, Will never trigger RangeLED Events
  end
  else
    if FRangeLEDSettings.Rangetype = rcGaugeOutOfRange then     // Special case to ONLY look at the gauge state, ignores the start/end
      Result := FOutOfRangeState                                // Will NOT trigger any events for RangeLED, this is handled elsewhere
    else
      if FRangeLEDSettings.RangeType = rcGreaterStart then
        Result := (AValue > FRangeLEDSettings.RangeStartValue)  // ignore range end, most common case
      else
        if FRangeLEDSettings.RangeType = rcLessEnd then
          Result := (AValue < FRangeLEDSettings.RangeEndValue)  // ignor range start
        else
          if FRangeLEDSettings.RangeType = rcBetween then
            Result := (AValue > FRangeLEDSettings.RangeStartValue) and (AValue < FRangeLEDSettings.RangeEndValue)
          else
            if FRangeLEDSettings.Rangetype = rcBothInclusive then
              Result := (AValue >= FRangeLEDSettings.RangeStartValue) and (AValue <= FRangeLEDSettings.RangeEndValue)
            else
              if FRangeLEDSettings.Rangetype = rcBothBetweenOutside then
                Result := (AValue < FRangeLEDSettings.RangeStartValue) or (AValue > FRangeLEDSettings.RangeEndValue)
              else
                if FRangeLEDSettings.Rangetype = rcStartInclusive then
                  Result := (AValue >= FRangeLEDSettings.RangeStartValue) and (AValue < FRangeLEDSettings.RangeEndValue)
                else
                  if FRangeLEDSettings.Rangetype = rcEndInclusive then
                    Result := (AValue > FRangeLEDSettings.RangeStartValue) and (AValue <= FRangeLEDSettings.RangeEndValue)
                  else
                    if FRangeLEDSettings.Rangetype = rcBothInclusiveOutside then
                      Result := (AValue <= FRangeLEDSettings.RangeStartValue) or (AValue >= FRangeLEDSettings.RangeEndValue);

  // Now set the flag we have changed so others SetValue() can update as needed

  FRangeLEDStateChanged := FRangeLEDStateChanged or (Result <> FRangeLEDSettings.Active);

  // Try the callbacks now, should hit one or the other depending on Active state
  // if they are assigned! Rember some will NEVER casuse a call back, rcNone and
  // rcGaugeOutOfRange

  if FRangeLEDStateChanged and (FRangeLEDSettings.RangeType <> rcNone)
    and (FRangeLEDSettings.RangeType <> rcGaugeOutOfRange) then
  begin
      if Assigned(FRangeLedActive) and Result then
        FRangeLEDActive(Self, AValue)
      else
        if Assigned(FRangeLedActive) and (not Result) then
          FRangeLEDInactive(Self, AValue);

      FRangeLEDStateChanged := False;   // clear the state
  end;

  FRangeLEDSettings.ActiveNoDoChange := Result;
end;

end.
