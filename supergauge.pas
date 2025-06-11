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
{******************************** CHANGE LOG *********************************
v2.00 - Breaking Changes from V1 SuperGauge Sandy Ganz, sganz@pacbell.net

        Autoscale of SG now fully supported and seems to be working well.
        Fixed bug in About box that was displaying the wrong version numbers.
        Minor changes to the rendered gauge due to the autoscale code changes.
        Possible breaking change on Frame properties, better to fix now.
        Cleaned up how the Frames works, now 3 individual concentric, Outer, middle, inner.
        Pointer Thickness is now Width to be more consistant with other props (breaking Change).
        Added additional options for RangeLED to now check just Start or Ending value.
        Fixed Bug in that Marker Values were not scaled, so if the Min/Max was not 0/100 it would not draw correctly.
        Fixed bug in Arc pointer drawing using only the FValue for a range check instead of the passed in parameter.
        Added Second Scale and Pointer options, known as AuxPointer and AuxScale.
        Added missing events for the AuxValue so it can also emit OutOfRange type events.
        Removed unintended exposed property on RangeLED (OK to Remove from .lfm if warned)
        Changed RangeLED type of rcGaugeOutOfRange to rcGaugeOverload and events to
        make it language different then RangeCheckLED.

******************************* END CHANGE LOG *******************************}

unit SuperGauge;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, Graphics, {$IFDEF FPC}LResources,{$ELSE} BGRAGraphics, {$ENDIF} Forms, Controls, Dialogs, SuperGaugeCommon,
  BGRABitmap, BGRABitmapTypes, BGRAVectorize, BGRAPath, math, bctypes, bctools;

const
  VERSIONSTR = '2.02';            // SG version, Should ALWAYS show as a delta when merging!
  INTERNAL_GAUGE_MIN_VALUE = 0;   // internal lowest value
  INTERNAL_GAUGE_MAX_VALUE = 270; // internal highest value
  BASELINE_SIZE = 300;            // For ResolveSizes()


type

  { TSGCustomSuperGauge }

  TBandsArray = array[0..3] of TSGBandSettings;
  TTextsArray = array[0..2] of TSGTextSettings;
  TMarkersArray = array[0..2] of TSGMarkerSettings;
  TTextsBitmapArray = array[0..2] of TBGRABitmap;

  TSGRangeStateErrorEvent = procedure(Sender: TObject; OutOfRangeValue: single) of object;  // called anytime out of range
  TSGRangeStateOKEvent = procedure(Sender: TObject; RangeValue: single) of object;          // called only when back to in range
  TSGRangeStateChangeEvent = procedure(Sender: TObject; Value: single) of object;           // called when state RangeLed Active changes to True

  // TResolvedSizes is a helper for scaling. It's used
  // in most of the DrawXXX procedures to help with autoscale.

  TResolveSizes = Record
    MinRadius: integer;
    MinWH: integer;
    Scale: single;

    // Frame Specific since its already sorta' scaling

    OuterFrameThickness: single;
    MiddleFrameThickness: single;
    InnerFrameThickness: single;

    // keep track of the internal frame radius, helps later

    OuterFrameInsideRadius: single;
    MiddleFrameInsideRadius: single;
    InnerFrameInsideRadius: single;
    FrameThickness: single;

    // Face

    FaceRadiusStart: single;

    // add anything here that might need autosize
    // also initialize these in the constructor
    // and fill in the ResolveSizes()
  end;

  TSGCustomSuperGauge = class(TGraphicControl)
  private
    { Private declarations }
    FDirty: boolean;
    FAutoScale: boolean;
    FResolvedSizes: TResolveSizes;
    FFaceSettings: TSGFaceSettings;
    FFrameSettings: TSGFrameSettings;
    FPointerSettings: TSGPointerSettings;
    FAuxPointerSettings: TSGPointerSettings;
    FPointerCapSettings: TSGPointerCapSettings;
    FScaleSettings: TSGScaleSettings;
    FAuxScaleSettings: TSGScaleSettings;
    FBandsSettings: TBandsArray;
    FTextsSettings: TTextsArray;
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

    FAuxMinValue: single;  // Aux Pointer Values, only pointer no markers, no RangeLED interactions
    FAuxMaxValue: single;
    FAuxValue: single;

    FOverloadTriggered: TSGRangeStateErrorEvent;
    FOverloadRecovered: TSGRangeStateOKEvent;
    FOverloadTriggeredState: boolean;
    FAuxOverloadTriggered: TSGRangeStateErrorEvent;
    FAuxOverloadRecovered: TSGRangeStateOKEvent;
    FAuxOverloadTriggeredState: boolean;

    FRangeLEDActive: TSGRangeStateChangeEvent;
    FRangeLEDInactive: TSGRangeStateChangeEvent;
    FRangeLEDStateChanged: boolean;

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
    procedure SetAuxScaleSettings(AValue: TSGScaleSettings);
    procedure SetFrameSettings(AValue: TSGFrameSettings);
    procedure SetPointerSettings(AValue: TSGPointerSettings);
    procedure SetAuxPointerSettings(AValue: TSGPointerSettings);
    procedure SetRangeLEDSettings(AValue: TSGRangeCheckLEDSettings);
    procedure SetPointerCapSettings(AValue: TSGPointerCapSettings);

    procedure SetMaxValue(AValue: single);
    procedure SetMinValue(AValue: single);
    procedure SetValue(AValue: single);
    function GetValue: single;

    procedure SetAuxMaxValue(AValue: single);
    procedure SetAuxMinValue(AValue: single);
    procedure SetAuxValue(AValue: single);
    function GetAuxValue: single;

    procedure SetAutoScale(AValue: boolean);
    function CheckOutOfRange(AValue: single): single;
    function AuxCheckOutOfRange(AValue: single): single;

  protected
    { Protected declarations }

    class function GetControlClassDefaultSize: TSize; override;
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
    function GetMinSize: integer;
    procedure ResolveSizes;
    property Dirty: boolean read FDirty write FDirty;

  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property PointerSettings: TSGPointerSettings read FPointerSettings write SetPointerSettings;
    property AuxPointerSettings: TSGPointerSettings read FAuxPointerSettings write SetAuxPointerSettings;
    property PointerCapSettings: TSGPointerCapSettings read FPointerCapSettings write SetPointerCapSettings;
    property FaceSettings: TSGFaceSettings read FFaceSettings write SetFaceSettings;
    property FrameSettings: TSGFrameSettings read FFrameSettings write SetFrameSettings;
    property ScaleSettings: TSGScaleSettings read FScaleSettings write SetScaleSettings;
    property AuxScaleSettings: TSGScaleSettings read FAuxScaleSettings write SetAuxScaleSettings;
    property BandSettings1: TSGBandSettings read FBandsSettings[0] write SetBandSettings1;
    property BandSettings2: TSGBandSettings read FBandsSettings[1] write SetBandSettings2;
    property BandSettings3: TSGBandSettings read FBandsSettings[2] write SetBandSettings3;
    property BandSettings4: TSGBandSettings read FBandsSettings[3] write SetBandSettings4;
    property TextSettings1: TSGTextSettings read FTextsSettings[0] write SetTextSettings1;
    property TextSettings2: TSGTextSettings read FTextsSettings[1] write SetTextSettings2;
    property TextSettings3: TSGTextSettings read FTextsSettings[2] write SetTextSettings3;
    property RangeLEDSettings: TSGRangeCheckLEDSettings read FRangeLEDSettings write SetRangeLEDSettings;
    property MarkerSettings1: TSGMarkerSettings read FMarkersSettings[0] write SetMarkerSettings1;
    property MarkerSettings2: TSGMarkerSettings read FMarkersSettings[1] write SetMarkerSettings2;
    property MarkerSettings3: TSGMarkerSettings read FMarkersSettings[2] write SetMarkerSettings3;
    property MinValue: single read FMinValue write SetMinValue default 0.0;
    property MaxValue: single read FMaxValue write SetMaxValue default 100.0;
    property Value: single read GetValue write SetValue default 0.0;
    property AuxMinValue: single read FAuxMinValue write SetAuxMinValue default 0.0;
    property AuxMaxValue: single read FAuxMaxValue write SetAuxMaxValue default 100.0;
    property AuxValue: single read GetAuxValue write SetAuxValue default 0.0;

    property OverloadTriggered: TSGRangeStateErrorEvent read FOverloadTriggered write FOverloadTriggered;
    property OverloadRecovered: TSGRangeStateOKEvent read FOverloadRecovered write FOverloadRecovered;
    property AuxOverloadTriggered: TSGRangeStateErrorEvent read FAuxOverloadTriggered write FAuxOverloadTriggered;
    property AuxOverloadRecovered: TSGRangeStateOKEvent read FAuxOverloadRecovered write FAuxOverloadRecovered;

    property RangeLEDActive: TSGRangeStateChangeEvent read FRangeLEDActive write FRangeLEDActive;
    property RangeLEDInActive: TSGRangeStateChangeEvent read FRangeLEDInactive write FRangeLEDInactive;
    property AutoScale: boolean read FAutoScale write SetAutoScale default False;

    function RemapRange(OldValue: single; OldMin, OldMax, NewMin, NewMax: single): single;
    function GaugeToUser(GaugeValue, MinVal, MaxVal: single): single;
    function UserToGauge(UserValue, MinVal, MaxVal: single): single;
    procedure Paint; override;
    procedure DrawFrame;
    procedure DrawFace;
    procedure DrawScales;
    procedure DrawScale(const Settings: TSGScaleSettings; Scale: single);
    procedure DrawBand(const BandSettings: TSGBandSettings; BandScale: single);
    procedure DrawBands;
    procedure DrawMulti;
    procedure DrawText(TextBitmap: TBGRABitmap; const TextSettings: TSGTextSettings);
    procedure DrawLED;
    procedure DrawMarker(MarkerBitmap: TBGRABitmap; const MarkerSettings: TSGMarkerSettings);
    procedure DrawMarkers;
    procedure DrawPointer(const Settings: TSGPointerSettings; Value: single);
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
    property Value;
    property AuxMinValue;
    property AuxMaxValue;
    property AuxValue;

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
    property AuxScaleSettings;
    property RangeLEDSettings;

    property MarkerSettings1;
    property MarkerSettings2;
    property MarkerSettings3;

    property PointerSettings;
    property AuxPointerSettings;
    property PointerCapSettings;
    property AutoScale;
    property OverloadTriggered;
    property OverloadRecovered;
    property AuxOverloadTriggered;
    property AuxOverloadRecovered;
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

  // remember if form is scaled CX, CY values will be too!
  // this may not do anything!!!

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  FFaceSettings := TSGFaceSettings.Create;
  FaceSettings.OnChange := DoChange;
  FaceSettings.Picture.OnChange := DoPictureChange; // need to set this so we can catch changes to the picture!

  FFrameSettings := TSGFrameSettings.Create;
  FrameSettings.OnChange := DoChange;

  FScaleSettings := TSGScaleSettings.Create;
  FScaleSettings.OnChange := DoChange;

  FAuxScaleSettings := TSGScaleSettings.Create;
  FAuxScaleSettings.OnChange := DoChange;
  FAuxScaleSettings.Enabled := False;

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

  // Pointers

  FPointerSettings := TSGPointerSettings.Create;
  FPointerSettings.OnChange := DoChange;
  FPointerSettings.Color := BGRA(255, 127, 63); // orange
  FPointerSettings.Enabled := True;
  FAuxPointerSettings := TSGPointerSettings.Create;
  FAuxPointerSettings.OnChange := DoChange;
  FAuxPointerSettings.Color := clRed;
  FAuxPointerSettings.Enabled := False;

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

  FOverloadTriggeredState := False;
  FAuxOverloadTriggeredState := False;
  FRangeLEDStateChanged := False;
  FValue := 0;
  FAutoScale := False;
  FMinValue := 0;
  FMaxValue := 100;
  FAuxMinValue := 0;
  FAuxMaxValue := 100;
  Color := clNone;

  // set up baseline values from the defaults, good starting point any-a-ways

  FResolvedSizes.MinRadius := 0;  // can't know MinRadius or MinWH yet, not resolved
  FResolvedSizes.MinWH := 0;

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

  FAuxPointerSettings.OnChange := nil;
  FAuxPointerSettings.Free;

  FRangeLEDSettings.OnChange := nil;
  FRangeLEDSettings.Free;

  ScaleSettings.OnChange := nil;
  FScaleSettings.Free;

  AuxScaleSettings.OnChange := nil;
  FAuxScaleSettings.Free;

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

// These are generally used for the pointer and range check scaling
// Passing in the Main Min/Max or the Aux Min/Max valves to set them.
// Again NOTE this will not affect the visable scale on the gauge, just the
// position of the pointers and range check as the displayed Scale is indpendant of this.

function TSGCustomSuperGauge.GaugeToUser(GaugeValue, MinVal, MaxVal: single): single;
begin
  // Helper to translate internal gauge value to external user value

  Result := RemapRange(GaugeValue, INTERNAL_GAUGE_MIN_VALUE, INTERNAL_GAUGE_MAX_VALUE, MinVal, MaxVal);
end;

function TSGCustomSuperGauge.UserToGauge(UserValue, MinVal, MaxVal: single): single;
begin
  // Helper to translate external user value to internal gauge value

  Result := RemapRange(UserValue, MinVal, MaxVal, INTERNAL_GAUGE_MIN_VALUE, INTERNAL_GAUGE_MAX_VALUE);
end;

function TSGCustomSuperGauge.GetValue: single;
begin
  // Scale from internal back to user range

  Result := GaugeToUser(FValue, FMinValue, FMaxValue);
end;

function TSGCustomSuperGauge.CheckOutOfRange(AValue: single): Single;
begin
  // These values are in gauge space, so typically never less than 0, or > 270

  Result := AValue; // SAFE so always will return a value

  if AValue < INTERNAL_GAUGE_MIN_VALUE then
  begin
    // Under Range event

    FOverloadTriggeredState := True;
    if Assigned(FOverloadTriggered) then
      FOverloadTriggered(Self, GaugeToUser(AValue, FMinValue, FMaxValue));
    Result := INTERNAL_GAUGE_MIN_VALUE;
  end
    else
      if AValue > INTERNAL_GAUGE_MAX_VALUE then
      begin
        // Over Range event

        FOverloadTriggeredState := True;
        if Assigned(FOverloadTriggered) then
          FOverloadTriggered(Self, GaugeToUser(AValue, FMinValue, FMaxValue)); // must translate back to user space
        Result := INTERNAL_GAUGE_MAX_VALUE;
      end
      else
        begin
          // If NOT over/under flow then will need to clear
          // that state/flag and reset any indicators if was in a overange state

          if FOverloadTriggeredState then
          begin
            if Assigned(FOverloadRecovered) then
              FOverloadRecovered(self, GaugeToUser(AValue, FMinValue, FMaxValue)); // here to, get into user space

            FOverloadTriggeredState := False;  // reset so no more calls
          end;
      end;
end;

function TSGCustomSuperGauge.AuxCheckOutOfRange(AValue: single): Single;
begin
  // Same as above, but for the Aux Values
  // These values are in gauge space, so typically never less than 0, or > 270

  Result := AValue; // SAFE so always will return a value

  if AValue < INTERNAL_GAUGE_MIN_VALUE then
  begin
    // Under Range event

    FAuxOverloadTriggeredState := True;
    if Assigned(FAuxOverloadTriggered) then
      FAuxOverloadTriggered(Self, GaugeToUser(AValue, FAuxMinValue, FAuxMaxValue));
    Result := INTERNAL_GAUGE_MIN_VALUE;
  end
    else
      if AValue > INTERNAL_GAUGE_MAX_VALUE then
      begin
        // Over Range event

        FAuxOverloadTriggeredState := True;
        if Assigned(FAuxOverloadTriggered) then
          FAuxOverloadTriggered(Self, GaugeToUser(AValue, FAuxMinValue, FAuxMaxValue)); // must translate back to user space
        Result := INTERNAL_GAUGE_MAX_VALUE;
      end
      else
        begin
          // If NOT over/under flow then will need to clear
          // that state/flag and reset any indicators if was in a overange state

          if FAuxOverloadTriggeredState then
          begin
            if Assigned(FAuxOverloadRecovered) then
              FAuxOverloadRecovered(self, GaugeToUser(AValue, FAuxMinValue, FAuxMaxValue)); // here to, get into user space

            FAuxOverloadTriggeredState := False;  // reset so no more calls
          end;
      end;
end;

// Override the base class which has a rectangular dimension

class function TSGCustomSuperGauge.GetControlClassDefaultSize: TSize;
begin
  // Note the preferred size for the control is 300xy, however in highdpi modes
  // on windows (maybe others) the control is scaled since the by default the forms
  // scale will affect the actual value on creation. So as an example, Windows 11,
  // 4k monitor, 150% scaling (windows settings), will cause the component to be
  // created and rendered with the size of 300x300. So these numbers get scaled
  // UP in this instance. If you run the scaling on Windows 11 at 100%, settings
  // after LCL does it's business is 200x200.

  Result.CX := 200;
  Result.CY := 200;
end;

function TSGCustomSuperGauge.GetMinSize: integer;
begin
  // Take the smallest width or height so we can use for max size gauge

  if ClientWidth < ClientHeight then
    Exit(ClientWidth)
  else
    Exit(ClientHeight);
end;

procedure TSGCustomSuperGauge.ResolveSizes;
var
  r: integer;

begin
  // Calculate anything needed to scale all elements of the Gauge. This is
  // called at the beginning of Paint so put individual scaling directly
  // in the DrawXXX procedures since they are often not redrawn and this
  // can save some computations in that case (Not dirty elements)

  // Drawing sized based on proportions of the DEFAULT component size (Baseline)

  FResolvedSizes.MinWH := GetMinSize;
  FResolvedSizes.MinRadius := FResolvedSizes.MinWH div 2;

  if FAutoScale then
  begin
    FResolvedSizes.Scale := FResolvedSizes.MinWH / BASELINE_SIZE;

    FResolvedSizes.OuterFrameThickness := FFrameSettings.OuterFrameThickness * FResolvedSizes.Scale;
    FResolvedSizes.MiddleFrameThickness := FFrameSettings.MiddleFrameThickness * FResolvedSizes.Scale;
    FResolvedSizes.InnerFrameThickness := FFrameSettings.InnerFrameThickness * FResolvedSizes.Scale;
  end
    else
      begin
        // Easy, not scaling

        FResolvedSizes.OuterFrameThickness := FFrameSettings.OuterFrameThickness;
        FResolvedSizes.MiddleFrameThickness := FFrameSettings.MiddleFrameThickness;
        FResolvedSizes.InnerFrameThickness := FFrameSettings.InnerFrameThickness;
        FResolvedSizes.Scale := 1.0;
      end;

  r := FResolvedSizes.MinRadius - 1; // Fudge factor to help with div 2

  // Outer Frame EndRadius

  FResolvedSizes.OuterFrameInsideRadius := r - FResolvedSizes.OuterFrameThickness / 2;  // Moves in from size of client (min size that is)

  // Middle Frame EndRadus

  FResolvedSizes.MiddleFrameInsideRadius := r - FResolvedSizes.MiddleFrameThickness / 2 - FResolvedSizes.OuterFrameThickness;

  // Innermost Frame End Radius

  FResolvedSizes.InnerFrameInsideRadius := r - FResolvedSizes.InnerFrameThickness / 2 - FResolvedSizes.OuterFrameThickness - FResolvedSizes.MiddleFrameThickness;

  FResolvedSizes.FrameThickness := FResolvedSizes.OuterFrameThickness
      + FResolvedSizes.MiddleFrameThickness + FResolvedSizes.InnerFrameThickness;

  // Get the drawing position for the face

  FResolvedSizes.FaceRadiusStart := FResolvedSizes.InnerFrameInsideRadius - FResolvedSizes.InnerFrameThickness / 2; // compensate for width of last ring
end;

procedure TSGCustomSuperGauge.SetAutoScale(AValue: boolean);
begin
  if FAutoScale = AValue then
    Exit;

  FAutoScale := AValue;
  FDirty := True;  // set it, as it will need a full repaint
  DoChange(self);
end;

procedure TSGCustomSuperGauge.SetMaxValue(AValue: single);
var
  currUser: single;

begin
  // Note : MinValue and MaxValue can span negative ranges and be increasing
  //        or decreasing. Can't really range check here

  // If changing min/max must refresh the value to adjust

  currUser := GaugeToUser(FValue, FMinValue, FMaxValue);
  FMaxValue := AValue;  // setting this will change UserToGauge() in SetValue!

  // Recompute

  SetValue(currUser);
end;

procedure TSGCustomSuperGauge.SetMinValue(AValue: single);
var
  currUser: single;

begin
  // Note : MinValue and MaxValue can span negative ranges and be increasing
  //        or decreasing. Can't really range check here

  // If changing min/max must refresh the value to adjust

  currUser := GaugeToUser(FValue, FMinValue, FMaxValue);
  FMinValue := AValue;  // setting this will change UserToGauge() in SetValue!

  // Recompute

  SetValue(currUser);
end;

procedure TSGCustomSuperGauge.SetValue(AValue: single);
var
    gaugeValue: single;
begin

  // Tricky case here, since we are calling the RangeLED range check
  // here too, if that is in any way dirty we should process the value
  // and not skip. Triggering any change on RangeLEDSettings should call this.

  // Get the user value into gauge value space

  gaugeValue := UserToGauge(AValue, FMinValue, FMaxValue);

  // Skip if a few conditions exit. This is a bit tricky as the gauge value will reset
  // to min or max values on overload so need to always update if that's the case. Should
  // not affect performance. Similar for LED, if dirty no skip.

  if (FValue = gaugeValue) and (not FRangeLEDSettings.Dirty) and (not FOverloadTriggeredState) then
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

procedure TSGCustomSuperGauge.SetAuxMaxValue(AValue: single);
var
  auxCurrUser: single;

begin
  // Note : MinValue and MaxValue can span negative ranges and be increasing
  //        or decreasing. Can't really range check here

  // If changing min/max must refresh the value to adjust

  auxCurrUser := GaugeToUser(FAuxValue, FAuxMinValue, FAuxMaxValue);
  FAuxMaxValue := AValue;  // setting this will change UserToGauge() in SetValue!

  // Recompute

  SetAuxValue(auxCurrUser);
end;

procedure TSGCustomSuperGauge.SetAuxMinValue(AValue: single);
var
  auxCurrUser: single;

begin
  // Note : MinValue and MaxValue can span negative ranges and be increasing
  //        or decreasing. Can't really range check here

  // If changing min/max must refresh the value to adjust

  auxCurrUser := GaugeToUser(FAuxValue, FAuxMinValue, FAuxMaxValue);
  FAuxMinValue := AValue;  // setting this will change UserToGauge() in SetValue!

  // Recompute

  SetAuxValue(auxCurrUser);
end;

function TSGCustomSuperGauge.GetAuxValue: single;
begin
  // Scale from internal back to user range

  Result := GaugeToUser(FAuxValue, FAuxMinValue, FAuxMaxValue);
end;

procedure TSGCustomSuperGauge.SetAuxValue(AValue: single);
var
  auxGaugeValue: single;

begin
  // Get the user value into gauge value space

  auxGaugeValue := UserToGauge(AValue, FAuxMinValue, FAuxMaxValue);

  if (FAuxValue = auxGaugeValue) and (not FAuxOverloadTriggeredState) then
    Exit;

  // Simple overflow here, It will Peg Gauge at MIN or MAX.
  // If out of range conditions are at play the gauge Value (FValue) will never
  // be out of range. This value is passed to the out of range handler for the
  // user to deal with and DO SOMETHING to indicate it.

  FAuxValue := AuxCheckOutOfRange(auxGaugeValue);

  // We must dirty the Pointer here or no redraw

  FAuxPointerSettings.Dirty := True;
  DoChange(self);
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

procedure TSGCustomSuperGauge.SetAuxScaleSettings(AValue: TSGScaleSettings);
begin
  if FAuxScaleSettings = AValue then
    Exit;

  FAuxScaleSettings := AValue;
  FAuxScaleSettings.Dirty := True;

  DoChange(self);
end;

procedure TSGCustomSuperGauge.SetAllBandsDirtyState(AValue: boolean);
var
  i: integer;
begin
  // helper to just set all bands to a specific state

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
  // helper to just set all texts to a specific state

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
  // helper to just set all markers to a specific state

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

procedure TSGCustomSuperGauge.SetAuxPointerSettings(AValue: TSGPointerSettings);
begin
  if FAuxPointerSettings = AValue then
    Exit;

  FAuxPointerSettings := AValue;
  FAuxPointerSettings.Dirty := True;

  DoChange(self);
end;

procedure TSGCustomSuperGauge.SetRangeLEDSettings(AValue: TSGRangeCheckLEDSettings);
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

  CheckRangeLED(Value);
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

  // ResolveSizes calculates scale and a few other sizes as needed. It must
  // be called PRIOR to drawing anything to get scale and related set up.

  ResolveSizes;

  // IF the component is resized OR moved (this is safer) we
  // need to make sure EVERYTHING redraws. The base class will
  // also do it's own thing to invalidate and redraw it all.

  if FDirty then
  begin
    FFrameSettings.Dirty := True;
    FFaceSettings.Dirty := True;
    FScaleSettings.Dirty := True;
    FAuxScaleSettings.Dirty := True;
    SetAllBandsDirtyState(True);
    SetAllTextsDirtyState(True);
    FRangeLEDSettings.Dirty := True;
    FPointerCapSettings.Dirty := True;
    FPointerSettings.Dirty := True;
    FAuxPointerSettings.Dirty := True;
    SetAllMarkersDirtyState(True);
    FDirty := False;  // everything here marked, so can reset
  end;

  // Now start Drawing into the offscreen bitmaps. IF the particular
  // subcomponent is not changed, the DrawXXXX will just leave it as is
  // and not waste cycles to redraw it wi

  FGaugeBitmap.SetSize(Width, Height); // should always be the components full size

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
      offsetX := Round(FResolvedSizes.Scale * FTextsSettings[i].OffsetX) + gaugeCenX - FTextsBitmaps[i].Width div 2;
      offsetY := Round(FResolvedSizes.Scale * FTextsSettings[i].OffsetY) + gaugeCenY - FTextsBitmaps[i].Height div 2;

      FGaugeBitmap.BlendImage(offsetX, offsetY, FTextsBitmaps[i], boLinearBlend);
    end;
  end;

  FGaugeBitmap.BlendImage(offsetX, offsetY, FTextBitmap, boLinearBlend);

  // Draw range LED, small bitmap so center and move to needed position

  DrawLED;
  offsetX := Round(FResolvedSizes.Scale * FRangeLEDSettings.OffsetX + gaugeCenX - FLEDActiveBitmap.Width / 2);
  offsetY := Round(FResolvedSizes.Scale * FRangeLEDSettings.OffsetY + gaugeCenY - FLEDActiveBitmap.height / 2);

  // Set up the LED, if user sets Active state will keep led on even if
  // the out of range state is set.

  if FRangeLEDSettings.Active then
    FGaugeBitmap.BlendImage(offsetX, offsetY, FLEDActiveBitmap, boLinearBlend)
  else
    FGaugeBitmap.BlendImage(offsetX, offsetY, FLEDInActiveBitmap, boLinearBlend);

  // Draw Markers BEFORE the pointer(s)

  DrawMarkers;
  FGaugeBitmap.BlendImage(0, 0, FMarkerBitmap,boLinearBlend);

  // Draw cap over or under the Main pointer. Note that the pointer is a special
  // case when drawing since it's almost always dirty.
  // Always draw aux point always under everything both the regular pointer and cap

  DrawPointer(FAuxPointerSettings, FAuxValue);

  if PointerCapSettings.CapStyle <> csNone then
    begin
      DrawPointerCap;
      offsetX := gaugeCenX  - FPointerCapBitmap.Width div 2;
      offsetY := gaugeCenY - FPointerCapBitmap.Height div 2;

      if PointerCapSettings.CapPosition = cpOver then
        begin
          DrawPointer(FPointerSettings, FValue);
          FGaugeBitmap.BlendImage(offsetX, offsetY, FPointerCapBitmap, boLinearBlend); // Cap on top
        end
      else
        begin
          FGaugeBitmap.BlendImage(offsetX, offsetY, FPointerCapBitmap, boLinearBlend); // Cap on Bottom
          DrawPointer(FPointerSettings, FValue);
        end;
    end
      else
      begin
        DrawPointer(FPointerSettings, FValue);
      end;

  // make it all visable to the user!

  FGaugeBitmap.Draw(Canvas, 0, 0, False);
end;

procedure TSGCustomSuperGauge.DrawMulti;
begin
  // The strategy here is that these typically only change infrequently
  // so just draw as a bundle and saves some blendimages calls. Each of the
  // drawXXX still handles it's own dirty flag. The bitmap will be set up
  // as on instantiation so all of the others have their dirty flag set True, so no
  // need to do any initialization. Makes painting much faster even
  // with the individual dirty flags!

  if FFrameSettings.Dirty or FFaceSettings.Dirty
    or FScaleSettings.Dirty or FAuxScaleSettings.Dirty
    or IsAnyBandDirty then
    begin
      Initializebitmap(FMultiBitmap, Width, Height);

      DrawFrame;
      FMultiBitmap.BlendImage(0, 0, FFrameBitmap, boLinearBlend);

      DrawFace;
      FMultiBitmap.BlendImage(0, 0, FFaceBitmap, boLinearBlend);

      DrawBands; // will handle the enable/disable and draw of each band
      FMultiBitmap.BlendImage(0, 0, FBandBitmap, boLinearBlend);

      DrawScales;
      FMultiBitmap.BlendImage(0, 0, FScaleBitmap, boLinearBlend);
    end;
end;

procedure TSGCustomSuperGauge.DrawFrame;
var
  Origin: TSGOrigin;

begin
  if not FrameSettings.Dirty then
    Exit;

  // Frame is 3 parts, inner, middle, outer, each with
  // a different color, possibly shading if someone wants to add it.
  //
  // The Frame will scale with the component size, so this is a bit
  // different then other parts of the gauge.

  FrameSettings.Dirty := False;

  // Dirty the Face, it's going to also need a redraw if we are here
  // Also implies face is draw AFTER this

  FaceSettings.Dirty := True; // need to dirty the FACE, cascade.

  Origin := Initializebitmap(FFrameBitmap, Width, Height);

  //Outer

  FFrameBitmap.EllipseAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y,
    FResolvedSizes.OuterFrameInsideRadius,
    FResolvedSizes.OuterFrameInsideRadius,
    FFrameSettings.OuterFrameColor, FResolvedSizes.OuterFrameThickness + 0.5);

  // Middle

  FFrameBitmap.EllipseAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y,
    FResolvedSizes.MiddleFrameInsideRadius,
    FResolvedSizes.MiddleFrameInsideRadius,
    FFrameSettings.MiddleFrameColor, FResolvedSizes.MiddleFrameThickness + 0.5);

  // Innermost

  FFrameBitmap.EllipseAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y,
    FResolvedSizes.InnerFrameInsideRadius,
    FResolvedSizes.InnerFrameInsideRadius,
    FFrameSettings.InnerFrameColor, FResolvedSizes.InnerFrameThickness + 0.5);
end;

procedure TSGCustomSuperGauge.DrawFace;
var
  OriginFace: TSGOrigin;
  r: single;
  d: integer;
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

  FFaceSettings.Dirty := False;

  OriginFace := Initializebitmap(FFaceBitmap, Width, Height);
  r := FResolvedSizes.FaceRadiusStart;  // this is the inner size from the last drawn Frame ring

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
        d := Round(r * 2);
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
                Round(OriginFace.CenterPoint.x - r), Round(OriginFace.CenterPoint.y - r),
                FFaceSettings.InnerColor);
        Map.Free;
      end;
  end;

  // see if valid size and enabled, draw if so!

  if ((FaceSettings.Picture.Width > 0) or (FaceSettings.Picture.Height > 0)) and (FFaceSettings.PictureEnabled) then
  begin

    Image := TBGRABitmap.Create(FaceSettings.Picture.Bitmap);

    // Resize the image if we are in AutoScale Mode

    if FAutoScale then
      BGRAReplace(Image, Image.Resample(Round(Image.Width * FResolvedSizes.Scale), Round(Image.Height * FResolvedSizes.Scale), rmFineResample));

    FFaceBitmap.BlendImage(
                OriginFace.CenterPoint.X + Round(FFaceSettings.PictureOffsetX * FResolvedSizes.Scale),
                OriginFace.CenterPoint.y + Round(FFaceSettings.PictureOffsetY * FResolvedSizes.Scale),
                image,
                boLinearBlend);

    Image.Free; // needed!
  end;
end;

procedure TSGCustomSuperGauge.DrawBands;
var
  i: integer;

begin
  // Draw multiple bands on the same bitmap. we can do this since
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
    FBandsSettings[i].Dirty := True;  // force draw, if any band is dirty they are all dirty
    DrawBand(FBandsSettings[i], FResolvedSizes.Scale);      // will clear any dirty for specific band
  end;
end;

procedure TSGCustomSuperGauge.DrawBand(const BandSettings: TSGBandSettings; BandScale: single);
var
  BandRadius, TextRadius: single;
  TextSize: integer;
  baseAngle, startAngle, endAngle: single;
  cenX, cenY: integer;
  fontRenderer: TBGRAVectorizedFontRenderer;
  TextPath: TBGRAPath;

begin

  if not BandSettings.Dirty then
    Exit;

  BandSettings.Dirty := False;

  // Now, if not enabled we can leave if flag reset!

  if not BandSettings.Enabled then
    exit;

  TextSize := Round(BandSettings.TextSize * 15 * BandScale);

  // Needs to be the components sizes here

  cenX := Width div 2;
  cenY := Height div 2;

  BandRadius := Round((BandSettings.BandRadius - BandSettings.BandThickness / 2) * BandScale);    // may need to adjust for band thickness
  TextRadius := Round((BandSettings.TextRadius - BandSettings.BandThickness) * BandScale) ; // offset to center

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
                   BandSettings.BandThickness * BandScale,
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
  SaveFont: TBCFont;

begin

  if not TextSettings.Dirty then
    Exit;

  TextSettings.Dirty := False;

  // Need to save off the current Fonts size, as we may need to update it
  // if the Gauge is scaled. We can't just pass the TextsSettings.FontEX because
  // we need to change the size of the font, and in doing so will cause the
  // Gauge to be dirty and refresh, and refresh, and refresh... you get the idea.

  SaveFont := TBCFont.Create(nil);
  SaveFont.Assign(TextSettings.FontEx); // assign will not copy the OnChange

  // If scaling of other elements (Shadow, Offsets, etc are needed just use
  // the Scale() functionality. For now we only need the Height scaled
  // so can save some cycles and just scale it.
  // SaveFont.Scale(TextScale, True);    // Awesome, takes care of all elements (height, shadows, etc)

  SaveFont.Height := round(SaveFont.Height * FResolvedSizes.Scale); // only using this from the font for now

  // get the bounding box so we can create a SMALLER bitmap. This will be referenced
  // to the Center of the text and gauge

  CalculateTextSize(TextSettings.Text, SaveFont, TextBoxWidth, TextBoxHeight, TextSettings.FontEx.Shadow);
  Initializebitmap(TextBitmap, TextBoxWidth, TextBoxHeight);

  // Set up text bounding box,

  TextRect.Left := 0;
  TextRect.Top := 0;
  TextRect.Height := TextBoxHeight;
  TextRect.Width := TextBoxWidth;

  // Draw into the TextBitmap for later use

  RenderText(TextRect, SaveFont, TextSettings.Text, TextBitmap, Enabled);

  // Clean it up

  SaveFont.Free;
end;

procedure TSGCustomSuperGauge.DrawScales;
begin

  if FScaleSettings.Dirty or FAuxScaleSettings.Dirty then
  begin
    Initializebitmap(FScaleBitmap, Width, Height);
    FScaleSettings.Dirty := True;
    FAuxScaleSettings.Dirty := True;
  end;

  // Draw AuxScale first so it's on the bottom and under the MainScale

  DrawScale(FAuxScaleSettings, FResolvedSizes.Scale);
  DrawScale(FScaleSettings, FResolvedSizes.Scale);

end;

procedure TSGCustomSuperGauge.DrawScale(const Settings: TSGScaleSettings; Scale: single);
var
  cenX, cenY: integer;
  i, n: integer;
  x, y, xt, yt:single;
  pStart, pEnd: TPointF;
  startAngle, endAngle: single;
  innerTicRadius: single;
  scaleStartValue, scaleBump: integer;
  scaleRadius, scaleTextRadius: single;
  scaleTextSize: integer;
  scaleMainTickLength, scaleSubTickLength: single;
  scaleMainTickThickness, scaleSubTickThickness: single;
  scaleInnerTickArcThickness, scaleOuterTickArcThickness: single;

begin

  // if nothing dirty then skip it, we have a bitmap with
  // the scale already drawn. This is slow so saves a lot of time
  // as scales are slow to draw

  if not Settings.Dirty then
    Exit;

  Settings.Dirty := False;  // mark as clean, so next run will not need a rebuild!

  if not Settings.Enabled then
    Exit;


  cenX := Width div 2;
  cenY := Height div 2;

  scaleRadius := Settings.ScaleRadius * FResolvedSizes.Scale;
  scaleTextRadius :=  Settings.TextRadius * FResolvedSizes.Scale;
  scaleTextSize :=  Round(Settings.TextSize * FResolvedSizes.Scale);

  scaleMainTickLength := Settings.MainTickLength * Scale;
  scaleSubTickLength := Settings.SubTickLength * Scale;
  scaleMainTickThickness := Settings.MainTickThickness * Scale;
  scaleSubTickThickness := Settings.SubTickThickness * Scale;
  scaleInnerTickArcThickness := Settings.InnerTickArcThickness * Scale;
  scaleOuterTickArcThickness := Settings.OuterTickArcThickness * Scale;

  // Needs to be flat caps for scale and related

  FScaleBitmap.LineCap := pecFlat;

  // Draw SubTicks

  if Settings.EnableSubTicks then
  begin
    n := Settings.MainTickCount * Settings.SubTickCount;

    for i := 0 to n do
    begin
      // Calculate draw from point

      X := cenX - (scaleRadius * cos((-45 + i * 270 / n) * Pi / 180));
      Y := cenY - (scaleRadius * sin((-45 + i * 270 / n) * Pi / 180));

      // Calculate draw to point

      Xt := cenX - (scaleRadius - scaleSubTickLength) *
        cos((-45 + i * 270 / n) * Pi / 180);
      Yt := cenY - (scaleRadius - scaleSubTickLength) *
        sin((-45 + i * 270 / n) * Pi / 180);

      if scaleSubTickThickness > 0 then
      begin
        if Settings.SubTickUseDots then
        begin
          // Can draw the center at the outer (fixed) or the Inner
          // which is based on the LENGTH of the tick

          FScaleBitmap.FillEllipseAntialias(xt, yt,
            scaleSubTickThickness, scaleSubTickThickness,
            Settings.TickColor)
        end
          else
            FScaleBitmap.DrawLineAntialias(x, y, xt, yt, Settings.TickColor, scaleSubTickThickness);
      end;

      // if dot mode, might not be needed, but likely don't use taboth and dots

      if (Settings.TickArcStyle = taboth) and (not Settings.EnableMainTicks) then
        begin
          // need caps on the ends so the gauge doesn't look stupid if both inner and outer
          // tic arcs are visiable

          if (i = 0) or (i = n) then
            begin
              if not Settings.EnableMainTicks then
                innerTicRadius := scaleSubTickLength
              else
                innerTicRadius := scaleMainTickLength;

              // draw end pieces in the MainTick thickness to match

              x := cenX - (scaleRadius + scaleOuterTickArcThickness / 2) * cos((-45 + i * 270 / n) * Pi / 180);
              y := ceny - (scaleRadius + scaleOuterTickArcThickness / 2) * sin((-45 + i * 270 / n) * Pi / 180);

              Xt := cenX - (scaleRadius - innerTicRadius - scaleOuterTickArcThickness / 2) *
                cos((-45 + i * 270 / n) * Pi / 180);
              Yt := ceny - (scaleRadius - innerTicRadius - scaleOuterTickArcThickness / 2) *
                sin((-45 + i * 270 / n) * Pi / 180);

              FScaleBitmap.DrawLineAntialias(x, y, xt, yt, Settings.TickColor,
                 scaleMainTickThickness);
            end;
        end;
    end;
  end;

  // Draw after the sub-ticks so main ticks are on top

  if Settings.EnableMainTicks then
  begin
    n := Settings.MainTickCount;

    for i := 0 to n do
    begin

      // Draw main ticks
      // Calculate draw from point bottom, to compensate for the width of the first and last
      // Main Ticks we adust the starting point so no gap. This is easier then trying
      // to extend the arc start and end (Have at it if you want)

      x := cenX - (scaleRadius + scaleOuterTickArcThickness / 2) * cos((-45 + i * 270 / n) * Pi / 180);
      y := cenY - (scaleRadius + scaleOuterTickArcThickness / 2) * sin((-45 + i * 270 / n) * Pi / 180);

      // Calculate draw to point top, again trying to compensate for the widths of things for the endpoints
      // if widths of the ticks get too big the edges will bleed out of the TickArc.

      xt := cenX - (scaleRadius - scaleMainTickLength + 1) *
        cos((-45 + i * 270 / n) * Pi / 180);
      yt := cenY - (scaleRadius - scaleMainTickLength + 1) *
        sin((-45 + i * 270 / n) * Pi / 180);

      if scaleMainTickThickness > 0 then
      begin

        if Settings.MainTickUseDots then
        begin
          // Can draw the center at the outer (fixed) or the Inner
          // which is based on the LENGTH of the tick

          FScaleBitmap.FillEllipseAntialias(xt, yt,
              scaleMainTickThickness, scaleMainTickThickness,
              Settings.TickColor)

        end
          else
            FScaleBitmap.DrawLineAntialias(x, y, xt, yt, Settings.TickColor, scaleMainTickThickness);
      end;
    end;
  end;

  // Draw text, these are only for the Main Ticks

  if Settings.EnableScaleText then
    begin

      FScaleBitmap.FontName := Settings.TextFont;
      FScaleBitmap.FontHeight := scaleTextSize;
      FScaleBitmap.FontQuality := fqFineAntialiasing;
      FScaleBitmap.FontStyle := Settings.TextStyle;

      n := Settings.MainTickCount;

      // if draw the scale reversed, do some tricky stuff so we can
      // count up or down. Start is swapped with the actual end value here

      if Settings.ReverseScale then
      begin
        scaleBump := -1;
        scaleStartValue := n * Settings.Step + Settings.Start;
      end
        else
          begin
            scaleBump := 1;
            scaleStartValue := Settings.Start;
          end;

      // Draw text for main ticks

      for i := 0 to n do
      begin
        xt := cenX - scaleTextRadius * cos((-45 + i * 270 / n) * Pi / 180);
        yt := cenY - scaleTextRadius * sin((-45 + i * 270 / n) * Pi / 180);

        FScaleBitmap.TextOut(xt, yt - (FScaleBitmap.FontHeight / 1.7),
          IntToStr(scaleStartValue + i * Settings.Step * scaleBump),
          Settings.TextColor, taCenter);
      end;
    end;

    // draw outer arcs

    if (Settings.TickArcStyle = taOuter) or (Settings.TickArcStyle = taboth) then
    begin
      // draw arc OUSIDE on the tics, doesn't matter main or sub, all at the top
      // inner of tic

      pStart.x := cenX - ScaleRadius * cos(-45 * Pi / 180);
      pStart.y := cenY - ScaleRadius * sin(-45 * Pi / 180);

      // Start angle will compensate for the width of the MAJOR tick

      startAngle := arctan2((cenY - pStart.y),
          (cenX - pStart.x)) + 4.71239; // add 270 0.0174533 1 deg in radians

      // Calculate draw to point outer

      pEnd.x := cenX - (ScaleRadius - scaleMainTickLength) * cos(225 * Pi / 180);
      pEnd.y := cenY - (ScaleRadius - scaleMainTickLength) * sin(225 * Pi / 180);

      // Same for End Angle

      endAngle :=  -arctan2((pEnd.y - cenY),(pEnd.x - cenX));

      FScaleBitmap.Arc(cenX, cenY,
                     ScaleRadius + 0.5, ScaleRadius + 0.5, // push down a bit
                     startAngle, endAngle,
                     Settings.TickArcColor,
                     scaleOuterTickArcthickness,
                     false,
                     BGRA(0,0,0,0) // last param is alpha, so no interior color, inner routings ONLY draw the arc, no fill
                     );
    end;

    if (Settings.TickArcStyle = taInner) or (Settings.TickArcStyle = taBoth) then
    begin
      // Inner will chose main tics (for now) if both main and sub tics on)
      // will need to find out the radius for what selected... or do something
      // like use what ever tic is LONGER (logic here will need a change)

      // draw arc OUSIDE on the tics, doesn't matter main or sub, all at the top

      // inner of tick

      pStart.x := cenX - ScaleRadius * cos(-45 * Pi / 180);
      pStart.y := cenY - ScaleRadius * sin(-45 * Pi / 180);

      startAngle := arctan2((cenY - pStart.y),(cenX - pStart.x)) + 4.71239; // add 270

      // Calculate draw to point outer

      pEnd.x := cenX - (ScaleRadius - scaleMainTickLength) * cos(225 * Pi / 180);
      pEnd.y := cenY - (ScaleRadius - scaleMainTickLength) * sin(225 * Pi / 180);

      endAngle := -arctan2((pEnd.y - cenY),(pEnd.x - cenX));

      // be nice and if not displaying main tics, use the sub tic length to bottom
      // up against them

      if not Settings.EnableMainTicks then
        innerTicRadius := scaleSubTickLength
     else
        innerTicRadius := scaleMainTickLength;

      FScaleBitmap.Arc(
                     cenX, cenY,
                     ScaleRadius - innerTicRadius+2, ScaleRadius - innerTicRadius+2,
                     startAngle, endAngle,
                     Settings.TickArcColor,
                     scaleInnerTickArcThickness,
                     false,
                     BGRA(0,0,0,0) // last param is alpha, so no interior color, inner routings ONLY draw the arc, no fill
                     );
    end;
end;

procedure TSGCustomSuperGauge.DrawPointer(const Settings: TSGPointerSettings; Value: single);
var
  Origin: TSGOrigin;
  x, y, x1, y1: single;
  subEx: single;
  PointerLength, PointerExtensionLength, PointerWidth, HighlightThickness: single;
  PointerCapRadius: single;
  startAngle, endAngle: single;
  bandRadius: single;
  vecLen: single;
  A, B, U, V: TPointF;

begin
    // Pointers are ALWAYS DRAWN since we don't create a specific pointer
    // bitmap to blend (and save) we need to always draw, this is faster
    // then creating another full size bitmap
    //
    // if not Settings.Dirty then
    //   Exit;
    // Settings.Dirty := False;

  if not Settings.Enabled then
    Exit;

  Origin.CenterPoint.X:= FGaugeBitmap.Width div 2;
  Origin.CenterPoint.Y:= FGaugeBitmap.Height div 2;

  // radius is smaller of the 2 dimensions

  Origin.Radius := FResolvedSizes.MinRadius;

  // Set the pointer length, does not apply to arc

  PointerLength := Settings.Length * FResolvedSizes.Scale;
  PointerExtensionLength := Settings.ExtensionLength * FResolvedSizes.Scale;
  PointerWidth := Settings.Width * FResolvedSizes.Scale;
  PointerCapRadius := FPointerCapSettings.Radius * FResolvedSizes.Scale;
  HighlightThickness := Settings.HighlightThickness * FResolvedSizes.Scale;


  // draw the arc style of pointer

  if (Settings.Style = psLine) or (Settings.Style = psLineExt) then
    begin
      // if we are need to draw the extension behind the cap, we can
      // recalc the ending point to just do one line draw instead of
      // 2 discrete lines from the center. That is easier, but slower.
      // If extension len is 0, skip as will show a partial pixel

      FGaugeBitMap.LineCap := pecRound; // caps should be round for line type pointers

      if (Settings.Style = psLineExt) and (PointerExtensionLength > 0) then
        begin
          // The extension is always pixels visable from the center or edge of the
          // cap, fix as needed. Makes nice for the user.

          if PointerCapSettings.CapStyle <> csNone then
            PointerExtensionLength := PointerExtensionLength + PointerCapRadius;

          // compute end point of pointer if an extension

          subEx := (-225 + Value) * Pi / 180;
          x1 := Origin.CenterPoint.x - PointerExtensionLength * cos(subEx);
          y1 := Origin.CenterPoint.y - PointerExtensionLength * sin(subEx);
        end
          else
            begin
              // no extension or extension length is 0, just draw to center

              x1 := Origin.CenterPoint.x;
              y1 := Origin.CenterPoint.y;
            end;

      // computer start point of pointer

      subEx := (-45 + Value) * Pi / 180;
      x := Origin.CenterPoint.x - PointerLength * cos(subEx);
      y := Origin.CenterPoint.y - PointerLength * sin(subEx);

      // finally draw it

      FGaugeBitMap.DrawLineAntialias(x, y, x1, y1, Settings.Color, PointerWidth);

      // Highlight in the case of line pointers is a CENTER line

      if Settings.HighlightLine then
        FGaugeBitMap.DrawLineAntialias(x, y, x1, y1, Settings.HighlightColor, HighlightThickness)

    end
      else
        if Settings.Style = psTriangle then
          begin
              // Draw a Triangle style pointer

              // Draw from center point out

              subEx := (-45 + Value) * Pi / 180;
              x := Origin.CenterPoint.x;
              y := Origin.CenterPoint.y;
              A := PointF(x, y);

              // Calculate draw to point top

              x1 := Origin.CenterPoint.x - PointerLength * cos(subEx);
              y1 := Origin.CenterPoint.y - PointerLength * sin(subEx);
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

              V := V * (PointerWidth / vecLen);

              // draw a full triangle pointer

              FGaugeBitMap.FillPolyAntialias([B, A + V, A - V], Settings.Color);

              // Triangle mode is to draw a highlight AROUND it

              if Settings.HighlightLine then
                FGaugeBitMap.DrawPolygonAntialias([B, A + V, A - V], Settings.HighlightColor, Settings.HighlightThickness);
          end
            else
              if Settings.Style = psArc then
                begin
                  // drawn arc pointer, ensure not negative or crash

                  if Value < 0.0 then
                    Exit;

                  // hack to show a marker for start at 0, may not scale
                  // well but fixes the issue of nothing drawing at 0 values

                  if Value = 0.0 then
                   Value := -1;

                   BandRadius := PointerLength - PointerWidth / 2;    // adjust for band thickness so end of pointer is top

                   // Start = 225 degree is 0 on gague scale (Not the angle), and -45 degree is 100 on scale
                   // 270, down (gauge angle 0)180 flat, increase moves towards 0 decrease towards 100
                   // 0 is flat line, right most end. Increase goes backwards towards 0, -45 is 100 percent on scale

                   startAngle := 225 * PI / 180;  // start at 0 on the gauge
                   endAngle := startAngle - Value * PI / 180;

                   FGaugeBitMap.LineCap := pecFlat; // caps should be flat, rounded does not align to scales well
                   FGaugeBitMap.Arc(
                                    Origin.CenterPoint.x, Origin.CenterPoint.y,
                                    BandRadius, BandRadius,
                                    startAngle, endAngle,
                                    Settings.Color,
                                    PointerWidth,
                                    False,
                                    BGRA(0,0,0,0) // last param is alpha, so no interior color, inner routings ONLY draw the arc, no fill
                               );
                end;
end;

procedure TSGCustomSuperGauge.DrawPointerCap;
var
  Origin: TSGOrigin;
  sizeWH : integer;
  pCapEdge : single;
  PointerCapRadius, PointerCapEdgeWidth: single;
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

  PointerCapRadius := FResolvedSizes.Scale * FPointerCapSettings.Radius;
  PointerCapEdgeWidth := FResolvedSizes.Scale * FPointerCapSettings.EdgeWidth;
  sizeWH := Round((PointerCapRadius + PointerCapEdgeWidth) * 2 + 2);
  Origin := Initializebitmap(FPointerCapBitmap, SizeWH, SizeWH);
  pCapEdge := PointerCapRadius + PointerCapEdgeWidth / 2;

  if PointerCapSettings.CapStyle = csFlat then
  begin
      // Draw the flat cap, but make sure size is similar to the shaded below or will be odd

      FPointerCapBitmap.EllipseAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y,
        pCapEdge,
        pCapEdge,
        PointerCapSettings.EdgeColor,
        PointerCapEdgeWidth,
        PointerCapSettings.FillColor);
  end
    else
      if PointerCapSettings.CapStyle = csShaded then
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
              PointerCapEdgeWidth, BGRA(0,0,0,0)
              );

      end
      else
        if PointerCapSettings.CapStyle = csPhong then
        begin
          // Phong it is

          tx := Round(PointerCapRadius * 2); // keeps size consistent with flat cap
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

              if PointerCapEdgeWidth > 0 then
                FPointerCapBitmap.EllipseAntialias(origin.CenterPoint.x, origin.CenterPoint.y,
                  pCapEdge,
                  pCapEdge,
                  PointerCapSettings.EdgeColor,
                  PointerCapEdgeWidth, BGRA(0,0,0,0));
            end;
      end;
end;

procedure TSGCustomSuperGauge.DrawLED;
var
  Origin: TSGOrigin;
  sizeWH : integer;
  RangeLedSize: single;
  mask: TBGRABitmap;
begin

  // skip drawing if nothing changed or not drawn

  if not FRangeLEDSettings.Dirty then
    Exit;

  FRangeLEDSettings.Dirty := False;

  // compute the size needed NOT a full gauge bitmap

  RangeLEDSize := FResolvedSizes.Scale * FRangeLEDSettings.Size;
  sizeWH := Round(RangeLEDSize * 2 + 2); // square size at lease LED radius and a bit more
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
        FLEDActiveBitmap.EllipseAntialias(
          Origin.CenterPoint.x, Origin.CenterPoint.y,
          RangeLEDSize, RangeLEDSize,
          FRangeLEDSettings.BorderColor,
          1,
          FRangeLEDSettings.ActiveColor);
      end
    else
      if FRangeLEDSettings.Style = lsShaded then
        begin
          // draw shaded, could do better here but good for starts

          FLEDActiveBitmap.FillEllipseLinearColorAntialias(
            Origin.CenterPoint.x, Origin.CenterPoint.y,
            RangeLEDSize, RangeLEDSize,
            FRangeLEDSettings.InactiveColor,
            FRangeLEDSettings.ActiveColor);

          // draw border

          FLEDActiveBitmap.EllipseAntialias(
            Origin.CenterPoint.x, Origin.CenterPoint.y,
            RangeLEDSize, RangeLEDSize,
            FRangeLEDSettings.BorderColor,
            1,
            BGRA(0,0,0,0));  // fill transparent
        end;

    // Simple flat round for inactive always

    if RangeLedSettings.Style <> lsNone then
      begin
        FLEDInactiveBitmap.EllipseAntialias(
          Origin.CenterPoint.x, Origin.CenterPoint.y,
          RangeLEDSize, RangeLEDSize,
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
                    Origin.Radius / 2, Origin.Radius / 2,
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

              if FRangeLEDSettings.Style = lsFlat then
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
                          PointF(FLEDActiveBitmap.Width / 2, FLEDActiveBitmap.Height - 1),
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

  // draws the fill sized bitmap to draw ALL markers onto one bitmap

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
  markerValue: single;
  x1, y1, x2, y2: single;
  cenX, cenY: single;
  vecLen: single;
  subx, cosSubx, sinSubX: single;
  A, B, U, V: TPointF;

begin
  // skip drawing if nothing changed or not drawn

  if not MarkerSettings.Dirty then
    Exit;

  MarkerSettings.Dirty := False;

  if not MarkerSettings.Enabled then
    Exit;

  // Center of bitmap

  cenX := MarkerBitmap.Width / 2;
  cenY := MarkerBitmap.Height / 2;

  // Need to translate the marker value. Since the value coming in is the USER
  // value in terms of Min/Max settings we need to also translate the markers
  // to match

  markerValue := UserToGauge(MarkerSettings.Value, FMinValue, FMaxValue);

  // We need to do a quick range check here for anything set like the pointer
  // but need to do it here since we can catch all markers being drawn. Range
  // check will force min or max gauge value if out of range respectivly

  if markerValue < INTERNAL_GAUGE_MIN_VALUE then
    markerValue := INTERNAL_GAUGE_MIN_VALUE
  else
    if markerValue > INTERNAL_GAUGE_MAX_VALUE then
      markerValue := INTERNAL_GAUGE_MAX_VALUE;

  subx := (-45 + markerValue) * Pi / 180;   // just like the pointer
  cosSubX := cos(subX);
  sinSubX := sin(subX);

  x1 := cenX - (MarkerSettings.Radius * FResolvedSizes.scale) * cosSubX;
  y1 := cenY - (MarkerSettings.Radius * FResolvedSizes.scale) * sinSubX;
  A := PointF(x1,y1);

  // Calculate draw to point top

  x2 := cenX - (MarkerSettings.Radius * FResolvedSizes.scale - MarkerSettings.Height * FResolvedSizes.scale) * cosSubX;
  y2 := cenY - (MarkerSettings.Radius * FResolvedSizes.scale - MarkerSettings.Height * FResolvedSizes.scale) * sinSubX;
  B := PointF(X2, y2);

  // set line cap just in case

  FMarkerBitmap.LineCap := pecRound; // Ensure Round Cap

  // This is the vector that runs from outer to inner

  U := B - A;

  // build the perpendicular vector
  // (clockwise in screen coordinates while the opposite would be counter clockwise)

  V := PointF(-U.y, U.x);
  vecLen := VectLen(V);

  // catch odd case of zero len vector, do nothing

  if vecLen = 0.0 then
    Exit;

  // Set length

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

function TSGCustomSuperGauge.CheckRangeLED(AValue: single): boolean;
begin
  // Manually setting the .Active prop will ONLY
  // work if rcNone is set, otherwise the range checks will prevail as the
  // way the Active state is set and overide the manual setting.
  //
  // Current List
  // TSGRangeCheckType = (rcNone, rcBetween, rcBothInclusive, rcStartInclusive,
  //                      rcEndInclusive, rcBothBetweenOutside,
  //                      rcBothInclusiveOutside, rcGreaterStart, RangeEndValue,
  //                      rcGreaterStartInclusive, rcLessEndInclusive);
  //
  // NOTE - rcGreaterStart, RangeEndValue, rcGreaterStartInclusive, rcLessEndInclusive
  //        ignore RangeEnd and RangeStart as indicated

  if FRangeLEDSettings.RangeType = rcNone then
  begin
    Result := FRangeLEDSettings.Active;   // need to always return the current state here, Will never trigger RangeLED Events
  end
  else
    if FRangeLEDSettings.Rangetype = rcGaugeOverload then       // Special case to ONLY look at the gauge state, ignores the start/end
      Result := FOverloadTriggeredState                                // Will NOT trigger any events for RangeLED, this is handled elsewhere
    else
      if FRangeLEDSettings.RangeType = rcGreaterStart then
        Result := (AValue > FRangeLEDSettings.RangeStartValue)  // ignore range end, common case
      else
        if FRangeLEDSettings.RangeType = rcGreaterStartInclusive then
          Result := (AValue >= FRangeLEDSettings.RangeStartValue)  // ignore range end, common case
        else
          if FRangeLEDSettings.RangeType = rcLessEnd then
            Result := (AValue < FRangeLEDSettings.RangeEndValue)  // ignor range start
          else
            if FRangeLEDSettings.RangeType = rcLessEndInclusive then
              Result := (AValue <= FRangeLEDSettings.RangeEndValue)  // ignor range start
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
  // if they are assigned! Rember some will NEVER cause a call back, rcNone and
  // rcGaugeOverload

  if FRangeLEDStateChanged and (FRangeLEDSettings.RangeType <> rcNone)
    and (FRangeLEDSettings.RangeType <> rcGaugeOverload) then
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
