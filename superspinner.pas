// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  The BGRASpinner is basically a spinner that just spins like an encoder pulse
  wheel. You can set many specific details to render and operate this control.

  Remeber you capture the pulses as the knob spins, it does retain the angular
  position, but typically that is not really used. Events for key operations
  including movement, wrapping, etc.

  In addition specific events for clicking on the center button (if enabled) or
  the spinner area if desired.

  When using these controls it best to have the form scaling set to FALSE, since
  some aspects of the compoent reference SIZE of the client area and scaling
  will update your sizes of Width and Height, good luck.

  Lastly the Resolution of the spinner can be controled, but since angular control
  or trying to set it for all conditions became a problem, the settings are
  from Higest to Lowest. Highest being maximum resolution of the mouse movements
  and Lowest makes it more like an old iPod with larger movements (clicks).
}
{******************************* CONTRIBUTOR(S) ******************************
- Sandy Ganz | sganz@pacbell.net
  02/20/2025 - Begat conversion from BGRASpinner, loads of changes to support
               the way a Spinner works, new events and props. Updated code style
               to be more similar to SuperGauge.
***************************** END CONTRIBUTOR(S) *****************************}

unit SuperSpinner;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Forms, Controls, Graphics,
  {$IFNDEF FPC}BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCBaseCtrls, BGRAGradients, BGRABitmap, BGRABitmapTypes, SuperSpinnerCommon;

const
  VERSIONSTR = '1.00';          // spinner version
  WHEEL_SPEED_FACTOR = 0.005;   // used to calculate mouse wheel speed

  RESOLUTION_HIGHEST = 1;       // used for setting spinners resolution
  RESOLUTION_HIGH = 2;          // Keeps the number of position somewhat hidden
  RESOLUTION_HIGH_MEDIUM = 3;
  RESOLUTION_MEDIUM = 4;
  RESOLUTION_MEDIUM_LOW = 5;
  RESOLUTION_LOW = 10;
  RESOLUTION_LOWEST = 20;

type
  TSSHitType = (shtNone, shtCap, shtKnob); // for sub component hit test
  TSSResolution = (srHighest, srHigh, srHighMedium, srMedium, srMediumLow, srLow, srLowest);

  TSSpinnerPosChangedEvent = procedure(Sender: TObject; Shift: TShiftState; Value: single; MoveDir : TSSDirection) of object;
  TSSpinnerCapClickEvent = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState) of object;
  TSSpinnerKnobClickEvent = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState) of object;
  TSSpinnerWrappedEvent = procedure(Sender: TObject; Shift: TShiftState; OldAngle, NewAngle: single; MoveDir : TSSDirection) of object;
  TSSpinnerCapEnterEvent = procedure(Sender: TObject; Shift: TShiftState; X,Y: Integer) of object;
  TSSpinnerCapLeaveEvent = procedure(Sender: TObject; Shift: TShiftState; X,Y: Integer) of object;
  TSSpinnerKnobEnterEvent = procedure(Sender: TObject; Shift: TShiftState;X,Y: Integer) of object;
  TSSpinnerKnobLeaveEvent = procedure(Sender: TObject; Shift: TShiftState; X,Y: Integer) of object;

  TResolveSizes = Record
    MinRadius: integer;
    MinWH: integer;
    FrameBorderWidth: integer;
    CapRadius: integer;
    CapEdgeThickness: integer;
    PositionRadius: integer;
    PositionMargin: integer;
    PositionCenterMargin: integer;
    PositionLineWidth: integer;
    KnobEdgeThickness: integer;

    // add anything here that might need autosize
    // also initialize these in the constructor
  end;

  { TCustomSuperSpinner }

  TCustomSuperSpinner = class(TBGRAGraphicCtrl)
  private
    { Private declarations }
    FDirty: boolean;

    // Settings

    FAutoScale: boolean;
    FResolvedSizes: TResolveSizes;
    FPositionSettings: TSSPositionSettings;
    FCapSettings: TSSCapSettings;
    FFrameSettings: TSSFrameSettings;
    FKnobSettings: TSSKnobSettings;
    FMouseDownAnglePos: single;
    FMouseDownExistingPos : single;
    FCapMouseDown: boolean;
    FKnobMouseDown: boolean;
    FInCap: boolean;
    FInKnob: boolean;
    FSpinnerBmp: TBGRABitmap;  // Main assembled image
    FFrameBmp: TBGRABitmap;    // Draws just the frame
    FKnobBmp: TBGRABitmap;     // Draws just the knob
    FCapBmp: TBGRABitmap;      // Draws just the cap that sits in the middle of the knob
    FPositionBmp: TBGRABitmap; // Draws just the position (lines, finger hole)
    FAngularPos: single;       // In RADIANS
    FCWSkipCounter: integer;
    FCCWSkipCounter: integer;
    FSpinnerResolution: TSSResolution;
    FSpinnerResolutionCount: integer;
    FSettingAngularPos: boolean;
    FPositionSnap: boolean;
    FOnSpinnerPosChange: TSSpinnerPosChangedEvent;
    FOnCapClick: TSSpinnerCapClickEvent;
    FOnKnobClick: TSSpinnerKnobClickEvent;
    FOnWrapped: TSSpinnerWrappedEvent;
    FOnMouseCapEnter: TSSpinnerCapEnterEvent;
    FOnMouseCapLeave: TSSpinnerCapLeaveEvent;
    FOnMouseKnobEnter: TSSpinnerKnobEnterEvent;
    FOnMouseKnobLeave: TSSpinnerKnobLeaveEvent;
    FLocked: boolean;     // Keeps Mouse from doing most things
    FWheelSpeed: byte;    // 0 : no wheel, 1 slowest, 255 fastest
    FMinRadius: integer;  // Computed minimum dimension for radius of spinner including Margin

    function GetAngle: single;
    function RadPosToDeg(RadPos: single): single;
    function DegPosToAngular(DegPos: single): single;
    procedure SetAngle(AValue: single);
    procedure SetPositionSnap(const AValue: boolean);
    function CalcAngularPos(X, Y: integer) : single;
    procedure UpdateAngularPos(Shift: TShiftState; AngularPos: single);
    function CapHitTest(X, Y: integer): boolean;
    function KnobHitTest(X, Y: integer): boolean;
    function HitTest(X, Y: integer): TSSHitType;
    procedure SetAutoScale(AValue: boolean);
    procedure SetWheelSpeed(AValue: byte);
    procedure SetLocked(AValue: boolean);
    procedure SetPositionSettings(AValue: TSSPositionSettings);
    procedure SetCapSettings(AValue: TSSCapSettings);
    procedure SetFrameSettings(AValue: TSSFrameSettings);
    procedure SetKnobSettings(AValue: TSSKnobSettings);
    procedure SetResolution(const AValue: TSSResolution);

  protected
    { Protected declarations }

    class function GetControlClassDefaultSize: TSize; override;
    procedure DoChange({%H-}Sender: TObject);
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    function GetMinSize: integer;
    procedure ResolveSizes;
    procedure Paint; override;
    procedure DrawFrame;
    procedure DrawKnob;
    procedure DrawCap;
    procedure DrawPosition;

    function DoMouseWheel(Shift: TShiftState; WheelDelta: integer; MousePos: TPoint): boolean; override;
    procedure MouseWheelPos({%H-}Shift: TShiftState; WheelDelta: integer); virtual;

  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  public
    { Streaming }

    {$IFDEF FPC}
    procedure SaveToFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);
    {$ENDIF}
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string; var ComponentClass: TComponentClass);
    procedure Bump(Direction: TSSDirection; Degrees: single);
    procedure Spin(Direction: TSSDirection; Degrees: single; Count: integer; ProcessMessages: Boolean = True);

  published
    { Published declarations }

    property AutoScale: boolean read FAutoScale write SetAutoScale default False;
    property PositionSettings: TSSPositionSettings read FPositionSettings write SetPositionSettings;
    property CapSettings: TSSCapSettings read FCapSettings write SetCapSettings;
    property FrameSettings: TSSFrameSettings read FFrameSettings write SetFrameSettings;
    property KnobSettings: TSSKnobSettings read FKnobSettings write SetKnobSettings;
    property PositionSnap: boolean read FPositionSnap write SetPositionSnap default False;
    property Angle: single read GetAngle write SetAngle nodefault;
    property SpinResolution: TSSResolution read FSpinnerResolution write SetResolution default srHighest;
    property WheelSpeed: byte read FWheelSpeed write SetWheelSpeed default 0;
    property Locked: boolean read FLocked write SetLocked default False; // TODO : Check if we need to cancel mouse movement, etc
    property OnPosChanged: TSSpinnerPosChangedEvent read FOnSpinnerPosChange write FOnSpinnerPosChange;
    property OnCapClick: TSSpinnerCapClickEvent read FOnCapClick write FOnCapClick;
    property OnKnobClick: TSSpinnerKnobClickEvent read FOnKnobClick write FOnKnobClick;
    property OnWrapped: TSSpinnerWrappedEvent read FOnWrapped write FOnWrapped;
    property OnMouseCapEnter: TSSpinnerCapEnterEvent read FOnMouseCapEnter write FOnMouseCapEnter;
    property OnMouseCapLeave: TSSpinnerCapLeaveEvent read FOnMouseCapLeave write FOnMouseCapLeave;
    property OnMouseKnobEnter: TSSpinnerKnobEnterEvent read FOnMouseKnobEnter write FOnMouseKnobEnter;
    property OnMouseKnobLeave: TSSpinnerKnobLeaveEvent read FOnMouseKnobLeave write FOnMouseKnobLeave;
    property OnMouseWheel;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property Visible;
  end;

  { TSuperSpinner }

  TSuperSpinner = class(TCustomSuperSpinner)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }

    property Anchors;
    property Color default clNone;
    property Hint;
    property ShowHint;
  end;

  {$IFDEF FPC}
        procedure Register;
  {$ENDIF}

implementation

uses Math;

{$IFDEF FPC}
procedure Register;
begin
  RegisterComponents('BGRA Controls', [TSuperSpinner]);
end;
{$ENDIF}

{ TCustomSuperSpinner }

constructor TCustomSuperSpinner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // remember if form is scaled CX, CY values will be too!
  // this may not do anything!!!

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  // Position Settings

  FPositionSettings := TSSPositionSettings.Create;
  FPositionSettings.OnChange := DoChange;

  // Spinner Cap Settings

  FCapSettings := TSSCapSettings.Create;
  FCapSettings.OnChange := DoChange;

  // Frame Settings

  FFrameSettings := TSSFrameSettings.Create;
  FFrameSettings.OnChange := DoChange;

  // Knob Settings

  FKnobSettings := TSSKnobSettings.Create;
  FKnobSettings.OnChange := DoChange;

  // Bitmaps

  FFrameBmp := TBGRABitmap.Create;
  FKnobBmp := TBGRABitmap.Create;
  FCapBmp := TBGRABitmap.Create;
  FPositionBmp := TBGRABitmap.Create;
  FSpinnerBmp := TBGRABitmap.Create;

  // General Inits

  FOnSpinnerPosChange := nil;
  FOnCapClick := nil;
  FOnKnobClick := nil;
  FOnWrapped := nil;
  FPositionSnap := False;
  FWheelSpeed := 0;        // 0, no wheel, 1 slowest, 255 fastest
  FLocked := False;
  FMouseDownAnglePos := 0;
  FMouseDownExistingPos := 0;
  FCapMouseDown := False;
  FKnobMouseDown := False;
  FSettingAngularPos := False;
  FInCap := False;
  Color := clNone;
  FCWSkipCounter := 0;
  FCCWSkipCounter := 0;
  FSpinnerResolutionCount := RESOLUTION_HIGHEST;  // how many clicks it takes to make a revolution
  SetAngle(0);      // Does NOT call any events
  FMinRadius := 0;  // Can't know just yet

  // set up baseline values from the defaults, good starting point any-a-ways

  FResolvedSizes.MinRadius := 0;  // can't know MinRadius or MinWH yet, not resolved
  FResolvedSizes.MinWH := 0;
  FResolvedSizes.FrameBorderWidth := FFrameSettings.BorderWidth;
  FResolvedSizes.CapRadius := FCapSettings.Radius;
  FResolvedSizes.CapEdgeThickness := FCapSettings.EdgeThickness;
  FResolvedSizes.PositionRadius := FPositionSettings.Radius;
  FResolvedSizes.PositionMargin := FPositionSettings.Margin;
  FResolvedSizes.PositionCenterMargin := FPositionSettings.CenterMargin;
  FResolvedSizes.PositionLineWidth := FPositionSettings.LineWidth;
  FResolvedSizes.KnobEdgeThickness := FKnobSettings.EdgeThickness;

  FDirty := True;   // Always force initial paint/draw on everything!
end;

destructor TCustomSuperSpinner.Destroy;
begin
  // Free up the bitmaps

  FSpinnerBmp.Free;
  FPositionBmp.Free;
  FFrameBmp.Free;
  FKnobBmp.Free;
  FCapBmp.Free;

  // Handlers (May not be needed, but good idea)

  FOnSpinnerPosChange := nil;
  FOnCapClick := nil;
  FOnKnobClick := nil;
  FOnWrapped := nil;

  // Position Settings

  FPositionSettings.OnChange := nil;
  FPositionSettings.Free;

  // Cap Settings

  FCapSettings.OnChange := nil;
  FCapSettings.Free;

  // Frame Settings

  FFrameSettings.OnChange := nil;
  FFrameSettings.Free;

  // Knob Settings

  FKnobSettings.OnChange := nil;
  FKnobSettings.Free;

  inherited Destroy;
end;

// Override the base class which has a rectangular dimension

class function TCustomSuperSpinner.GetControlClassDefaultSize: TSize;
begin
  // Note the preferred size for the control is 150x150, however in highdpi modes
  // on windows (maybe others) the control is scaled since the by default the forms
  // scale will affect the actual value on creation. So as an example, Windows 11,
  // 4k monitor, 150% scaling (windows settings), will cause the component to be
  // created and rendered with the size of 150x150. So these numbers get scaled
  // UP in this instance. If you run the scaling on Windows 11 at 100%, settings
  // after LCL does it's business is 100x100. This is tricky since some spinner
  // setting are NOT referenced by the size of the component but by pixels. So
  // the Cap for example is in non-scaled pixels, lines for the position is
  // based on component width so kinda' works OK, but not the cap. I remember
  // when pixels were just pixels...

  Result.CX := 100;
  Result.CY := 100;
end;

function TCustomSuperSpinner.GetMinSize: integer;
begin
  // Take the smallest width or height so we can use for max size spinner

  if ClientWidth < ClientHeight then
    Exit(ClientWidth)
  else
    Exit(ClientHeight);
end;

procedure TCustomSuperSpinner.ResolveSizes;
var
  scale: single;

begin
  // Compute the size of the drawing elements of the spinner based
  // on the FMinRadius size. If AutoScale is enabled for the control
  // will calculate the drawing elements needed. If not will return
  // the correct properties so all the testing for the AutoScale
  // option setting is done here.

  // Drawing sized based on proportions of the DEFAULT component values and
  // may not always look right based on settings of cap, position, borders, etc.

  // Get the minimum size for the drawing of the spinner

  // Todo : Not sure if FMinRadius belongs here...

  FResolvedSizes.MinWH := GetMinSize;
  FResolvedSizes.MinRadius := FResolvedSizes.MinWH div 2;
  FMinRadius := FResolvedSizes.MinRadius;
  scale := FResolvedSizes.MinWH / 150.0;

  if FAutoScale then
  begin
    // AutoScale based on 150x150 spinner size. Computes ratios from that to any size
    // Will it always look good? Hard to say, but can use break points on sizes to
    // also help with some edge cases like tiny and large if needed

    FResolvedSizes.FrameBorderWidth := Round(FFrameSettings.BorderWidth * scale);
    FResolvedSizes.CapRadius := Round(FCapSettings.Radius * scale);
    FResolvedSizes.CapEdgeThickness := Round(FCapSettings.EdgeThickness * scale);
    FResolvedSizes.PositionRadius := Round(FPositionSettings.Radius * scale);
    FResolvedSizes.PositionMargin := Round(FPositionSettings.Margin * scale);
    FResolvedSizes.PositionCenterMargin := Round(FPositionSettings.CenterMargin * scale);
    FResolvedSizes.PositionLineWidth := Round(FPositionSettings.LineWidth * scale);
    FResolvedSizes.KnobEdgeThickness := Round(FKnobSettings.EdgeThickness * scale);
  end
    else
      begin
        // Easy, not scaling

        FResolvedSizes.FrameBorderWidth := FFrameSettings.BorderWidth;
        FResolvedSizes.CapRadius := FCapSettings.Radius;
        FResolvedSizes.CapEdgeThickness := FCapSettings.EdgeThickness;
        FResolvedSizes.PositionRadius := FPositionSettings.Radius;
        FResolvedSizes.PositionMargin := FPositionSettings.Margin;
        FResolvedSizes.PositionCenterMargin := FPositionSettings.CenterMargin;
        FResolvedSizes.PositionLineWidth := FPositionSettings.LineWidth;
        FResolvedSizes.KnobEdgeThickness := FKnobSettings.EdgeThickness
      end;
end;

procedure TCustomSuperSpinner.SetAutoScale(AValue: boolean);
begin
  if FAutoScale = AValue then
    Exit;

  FAutoScale := AValue;
  FDirty := True;
  DoChange(self);
end;

procedure TCustomSuperSpinner.SetPositionSettings(AValue: TSSPositionSettings);
begin
  if FPositionSettings = AValue then
    Exit;

  FPositionSettings := AValue;
  FPositionSettings.Dirty := True;
  DoChange(self);
end;

procedure TCustomSuperSpinner.SetCapSettings(AValue: TSSCapSettings);
begin
  if FCapSettings = AValue then
    Exit;

  FCapSettings := AValue;
  FCapSettings.Dirty := True;
  DoChange(self);
end;

procedure TCustomSuperSpinner.SetFrameSettings(AValue: TSSFrameSettings);
begin
  if FFrameSettings = AValue then
    Exit;

  FFrameSettings := AValue;
  FFrameSettings.Dirty := True;
  DoChange(self);
end;

procedure TCustomSuperSpinner.SetKnobSettings(AValue: TSSKnobSettings);
begin
  if FKnobSettings = AValue then
    Exit;

  FKnobSettings := AValue;
  FKnobSettings.Dirty := True;
  DoChange(self);
end;

procedure TCustomSuperSpinner.DoChange(Sender: TObject);
begin
  Invalidate;
end;

// Handler to force redraw when in design mode

procedure TCustomSuperSpinner.DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  FDirty := true; // Called on Resize of component
end;

function TCustomSuperSpinner.RadPosToDeg(RadPos: single): single;
begin
  // helper to convert AnglePos in radians to degrees, wraps as needed

  Result := RadPos * 180 / Pi;

  if Result < 0 then
    Result := Result + 360;

  Result := 270 - Result;  // adjusts for screen coords

  if Result < 0 then
    Result := Result + 360;

  if Result > 360 then
    Result := Result - 360;

end;

function TCustomSuperSpinner.DegPosToAngular(DegPos: single): single;
begin
  // helper to convert Angle in degrees to radians, wraps as needed
  // 3 * pi/2 = 270 degrees, degs to radians = degs * pi/180

  Result := 3 * Pi / 2 - DegPos * Pi / 180;

  if Result > Pi then
    Result := Result - (2 * Pi);

  if Result < -Pi then
    Result := Result + (2 * Pi);
end;

procedure TCustomSuperSpinner.SetWheelSpeed(AValue: byte);
begin
  // Sets the mouse wheel speed

  FWheelSpeed := AValue;
end;

procedure TCustomSuperSpinner.SetLocked(AValue: boolean);
begin
  // If we are locking, this may cause some issues as we are disabling
  // some of the mouse control. So reset back to a clean state if needed

  if AValue = FLocked then
    Exit;

  FLocked := AValue;

  // if we are not locked now we should reset stuff to a clean state.
  // this MIGHT be needed if the lock happens while clicking or moving
  // in the spinner. The user should be starting fresh if this happens
  // (Or so I think)

  if not FLocked then
    begin

      // Reset Skip Counters

      FCWSkipCounter := 0;
      FCCWSkipCounter := 0;

      // If mouse was down in cap or knob reset too, we lose that tracking

      FCapMouseDown := False;
      FKnobMouseDown := False;
      FInCap := False;
      FInKnob := False;

      // Finally stop any mouse tracking

      FSettingAngularPos := False;

    end;
end;

procedure TCustomSuperSpinner.SetAngle(AValue: single);
begin
  // Sets the angle (in Degrees) of the Knobs position. This
  // will NOT call the OnPosChange event, and not affected by
  // the spinners resolution

  if DegPosToAngular(AValue) = FAngularPos then
    Exit;

  FAngularPos := DegPosToAngular(AValue);
  DoChange(self);
end;

function TCustomSuperSpinner.GetAngle: single;
begin
  Result := RadPosToDeg(FAngularPos);
end;

// Sets if the spinner position should snap to the mouse when clicked
// otherwise will allow the mouse to spin the knob without first 'snapping'
// to the mouse down position

procedure TCustomSuperSpinner.SetPositionSnap(const AValue: boolean);
begin
  if FPositionSnap = AValue then
    exit;

  FPositionSnap := AValue;
  DoChange(self);
end;

procedure TCustomSuperSpinner.SetResolution(const AValue: TSSResolution);
begin
  if AValue = FSpinnerResolution then
    Exit;

  FSpinnerResolution := AValue;

  // In general It's best to have it at srHighest. If you want it more like
  // an old iPod spinner try Low or Lowest. These are essentially
  // messing with the number of clicks per revolution, but I decided
  // not to try to calculate an exact value so these are just abstracting
  // that

  case AValue of
    srHighest: FSpinnerResolutionCount := RESOLUTION_HIGHEST;
    srHigh: FSpinnerResolutionCount := RESOLUTION_HIGH;
    srHighMedium: FSpinnerResolutionCount := RESOLUTION_HIGH_MEDIUM;
    srMedium: FSpinnerResolutionCount :=RESOLUTION_MEDIUM;
    srMediumLow: FSpinnerResolutionCount := RESOLUTION_MEDIUM_LOW;
    srLow: FSpinnerResolutionCount := RESOLUTION_LOW;
    srLowest: FSpinnerResolutionCount := RESOLUTION_LOWEST;
  end;
end;

function TCustomSuperSpinner.CalcAngularPos(X, Y: integer) : single;
begin
  // returns -pi to pi based on the XY of the mouse in the client box

  Result := ArcTan2(-1 * (Y - ClientHeight / 2) / ClientHeight, (X - ClientWidth / 2) / ClientWidth);
end;

procedure TCustomSuperSpinner.Bump(Direction: TSSDirection; Degrees: single);
var
  Offset: single;

begin
  if (Degrees < 0) or (Degrees > 359.99999) then
    Exit;

  Offset := GetAngle();

  if Direction = sdCW then
    Offset := Offset + Degrees
  else
    Offset := Offset - Degrees;

  // Force move, since UpdateAngularPos() PRE-Increments the Skip counters we
  // Must be one less or this trick won't work
  //
  // Since we may be forcing a specific degree move here, it can
  // shift the position of the spinner to an off increment angle than
  // the mouse is moving since that angle to bump to is arbitrary.
  // In general Spin and Bump are not great to use for this reason unless needed.

  // Must invalidate both as we don't know the current direction it's moving
  // so one will get reset, the other will trigger, so always works.

  FCWSkipCounter := FSpinnerResolutionCount - 1;
  FCCWSkipCounter := FCWSkipCounter;

  UpdateAngularPos([], DegPosToAngular(Offset));
end;

procedure TCustomSuperSpinner.Spin(Direction: TSSDirection; Degrees: single; Count: integer; ProcessMessages: Boolean = True);
var
  i, processRate: integer;

begin
  // This is something that likely should not be used more so then bump. It is easy to animate
  // a movement to a number of events triggered. This is tricky as you need to
  // call ProcessMessages or the update of the spinner will/could show up just
  // at the finish point since it will just do it fast if no movement will be shown.
  // Some tricky-ness can be done, for example if you want to do a Count of 100 at
  // 1 Degree per, that will be quickly animated, if you want to slow it down
  // you can try 0.1 Degrees per, and 1000 for the Count and only process
  // 1 out of 10 movement events to make it the same, the spinner will go slower
  // as it's rendering at a higher resolution, this is a hack for sure.

  // Degrees will be validated in Bump()

  if (Count < 1) then
    Exit;

  // Super Hack
  //
  // Try to keep fast for fine moves or moves with a lot of steps so looks nice
  // Tries to keep down calls to ProcessMessages, but Still update the display
  // As the Count goes up or the Degree granularity goes up (smaller Degree) the
  // processRate is smaller to have more screen updated UNLESS the count is
  // just too large, and then it slows down a lot. This is all testing
  // on a fast machine, fast video, Low or high res, lower speed CPU or Video
  // would totally impact this code.
  //
  // SUPER HACK

  if (Degrees < 1.0) or (Count < 25) then
    processRate := 2  // process a lot of screen updates
  else
    processRate := 4; // Less

  // If we have a lot of resolution can turn down the process rate a lot

  if (Count / Degrees) > 500 then
    processRate := 16;  // A lot less since movement is very small, not worth a lot of updates

  for i := 0 to Count - 1  do
  begin

    // Bump will call the event handler for movement for each

    Bump(Direction, Degrees);

    // Call ProcessMessages at a slower rate for small Degrees or large Count
    // Not sure if their is a better way to move and update the visuals. This
    // may not be needed IF the PosChanged event handler actually does a lot of
    // stuff, but I think (on Windows) the drawing of the spinners are all
    // coalesced until the message loop is caught up and only the last update
    // to the screen is seen. Application. ProcessMessages an optional call
    // and can let the handler deal with it as needed.

    if (i mod processRate = 0) and ProcessMessages then
      Application.ProcessMessages;
  end;
end;

procedure TCustomSuperSpinner.UpdateAngularPos(Shift: TShiftState; AngularPos: single);
var
  Direction: TSSDirection;
  currAngle, newAngle: single;

begin
  // AngularPos is in Rads, Wrap range if needed (Radians wrap)

  if AngularPos > Pi then
    AngularPos := AngularPos - (2 * Pi);

  if AngularPos < -Pi then
    AngularPos := AngularPos + (2 * Pi);

  // See which direction we are going, check start (Current)
  // is less than the new. This will give us the direction
  // This works EXCEPT at wrap around from 359 to 0 and 0 to 359
  // so either bring in the X, Y and do it sector by sector or
  // hack and say that if in the lower 2 sectors and track around
  // that. The 270 is a big delta, and unlikely, so unless a very large
  // update it works great. Remember that setting the position by
  // Angle does NOT cause the handler to be called ONLY this update method.

  currAngle := GetAngle();  // Degs
  newAngle := RadPosToDeg(AngularPos); // Degs

  // need this for skipping first

  if newAngle - currAngle > 270 then // crossing CCW over 359 to 0
    Direction := sdCCw
  else
      if currAngle - newAngle > 270 then // crossing CW over 0 to 359
        Direction := sdCW
      else
          if currAngle < newAngle then
            Direction := sdCW
          else
            Direction := sdCCW;

  // Must take into account direction changes so we can
  // have fresh counts in the correct direction or it
  // would have an inconsistant value if moving back and forth!

  if Direction = sdCW then
  begin
    Inc(FCWSkipCounter);
    FCCWSkipCounter := 0;
  end
  else
    begin
      Inc(FCCWSkipCounter);
      FCWSkipCounter := 0;
    end;

  // 1 is never skip since we pre-inc the numbers above, 2 is skip every other and so on

  if (FCWSkipCounter = FSpinnerResolutionCount) or (FCCWSkipCounter = FSpinnerResolutionCount) then
  begin

    // We are moving, so can reset BOTH, and set the new position, then update

    FCWSkipCounter := 0;
    FCCWSkipCounter := 0;

    // Need to check wrap here before we update the positions

    if newAngle - currAngle > 270 then // crossing CCW over 359 to 0
    begin
      if Assigned(FOnWrapped) then
        FOnWrapped(Self, Shift, currAngle, newAngle, sdCCW);
    end
      else
        if currAngle - newAngle > 270 then // crossing CW over 0 to 359
        begin
          if Assigned(FOnWrapped) then
            FOnWrapped(Self, Shift, currAngle, newAngle, sdCW);
        end;

    FAngularPos := AngularPos;

    if Assigned(FOnSpinnerPosChange) then
      FOnSpinnerPosChange(Self, Shift, Angle, Direction);

    DoChange(self);
  end;
end;

procedure TCustomSuperSpinner.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  hitIn: TSSHitType;

begin
  inherited MouseDown(Button, Shift, X, Y);

  if FLocked then
    Exit;

  // See if anything clicked on the cap, and then the knob
  // If we do have a Cap hit and it's enabled, then we
  // do not need to check the Knob, as it can't be in it!

  // HEADS UP : If the knob porting is not clicked on, ie, blank client area,
  // the border, the cap (and future stuff) the state of FSettingAngularPos
  // will NOT change. Mouse movement once started does NOT look at any boundries

  hitIn := HitTest(X, Y);

  if hitIn = shtCap then
    FCapMouseDown := True
  else
    if hitIn = shtKnob then
      FKnobMouseDown := True;

  // if user has pressed the left mouse button, then start tracking
  // skip any movement if mouse down in the cap (button enabled)

  if (Button = mbLeft) and (not FCapMouseDown) and (FKnobMouseDown) then
  begin
    FSettingAngularPos := True; // start of dragging the spinner, update the state

    // save the angle of the mouse down, this will later
    // be used to offset to the current position with existing angle
    // to allow the user to grab anywhere on the knob and spin

    FMouseDownAnglePos := CalcAngularPos(X, Y);

    if FPositionSnap then
    begin

      // If we have position snap enabled, when the mouse clicks on it, will spin
      // the spinners angle to it, position to it, but will NOT update anything
      // else or call the handler for movement

      FAngularPos := FMouseDownAnglePos;
      DoChange(self);
    end;

    FMouseDownExistingPos := FAngularPos; // after update always set this
  end;
end;

procedure TCustomSuperSpinner.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  hitIn: TSSHitType;

begin
  inherited MouseUp(Button, Shift, X, Y);

  if FLocked then
    Exit;

  if Button = mbLeft then
    FSettingAngularPos := False;  // Change state to not moving

  // can have different mouse buttons click on the cap, catch then all I guess
  // let the OnClicks sort it out if needed

  // see if we are still in the cap, if so call back as this is a OnClick
  // style event. Always clear the state in anycase as a left mouse up
  // should stop tracking the down events

  hitIn := HitTest(X, Y);

  if FCapMouseDown and (hitIn = shtCap) then
  begin
    if Assigned(FOnCapClick) then
      FOnCapClick(Self, Button, Shift);
  end
    else
      if FKnobMouseDown and (hitIn = shtKnob) then
      begin
        if Assigned(FOnKnobClick) then
          FOnKnobClick(Self, Button, Shift);
      end;

  FCapMouseDown := False; // wipes all potential mouse downs
  FKnobMouseDown := False;
end;

procedure TCustomSuperSpinner.MouseMove(Shift: TShiftState; X, Y: integer);
var
  hitIn: TSSHitType;

begin
  inherited MouseMove(Shift, X, Y);

  if FLocked then
    Exit;

  // being nice, here is a specific event for the cap/knob enter/exit
  // can do some nice stuff with it like highlight when over them

  hitIn := HitTest(X, Y);

  // See what's cooking with the cap first. If cap disabled always False

  if FInCap <> (hitIn = shtCap) then
  begin
    if FInCap then
    begin
      // we are in the cap, then exiting

      FInCap := False;
      if Assigned(FOnMouseCapLeave) then
          FOnMouseCapLeave(Self, Shift, X, Y);
    end
    else
      begin
        // Out of the cap, then entering

        FInCap := True;
        if Assigned(FOnMouseCapEnter) then
            FOnMouseCapEnter(Self, Shift, X, Y);
      end;
  end;

  // now the Knob part

  if FInKnob <> (hitIn = shtKnob) then
  begin
    if FInKnob then
    begin
      // we are in the cap, then exiting

      FInKnob := False;
      if Assigned(FOnMouseKnobLeave) then
          FOnMouseKnobLeave(Self, Shift, X, Y);
    end
    else
      begin
        // Out of the cap, then entering

        FInKnob := True;
        if Assigned(FOnMouseKnobEnter) then
            FOnMouseKnobEnter(Self, Shift, X, Y);
      end;

  end;

  if FSettingAngularPos then
  begin

    // Move the spinner, takes into account the MouseDown values
    // to either snap the wheels angle to the mouse or ignore. This
    // is all done in MouseDown based on the PositionSnap setting.

    FKnobMouseDown := False; // Cancel KnobMouseDown so we don't allow click if moving
    UpdateAngularPos(Shift, FMouseDownExistingPos + CalcAngularPos(X, Y) - FMouseDownAnglePos);
  end;
end;

function TCustomSuperSpinner.CapHitTest(X, Y: integer) : boolean;
begin
  // Easy check, if mouse distance from center of client is
  // within center radius (also at center of client) we can
  // hit test the cap circle

  // see if we need to even do anything, also prevents error if radius is 0

  if FCapSettings.Style = csNone then   // safe-tee
    Exit(False);

  // If the distance of the mouse to center is less than the radius of the cap
  // and the edge we are in the cap, remember dealing with the RADIUS not Diameter
  // The center of the measure is not 0,0 but half the the min size. So if the
  // size of the MinRadius is 75, the line is measured from 75,75 to the Mouse
  // X,Y which is in terms of the client area. Tricky but works. Similar for
  // the Knob. We need to use the client sizes for width and height here to get the center!

Result := Sqrt(((FSpinnerBmp.Width div 2 - X)** 2 + (FSpinnerBmp.Height div 2 - Y)** 2))
      <= (FResolvedSizes.CapRadius + FResolvedSizes.CapEdgeThickness - 1);
end;

function TCustomSuperSpinner.KnobHitTest(X, Y: integer) : boolean;
begin
  // if are using the cap as a button, and it's a hit,
  // get out, we don't count that as a knob hit as
  // it's excluded in this case, so a bit slower to call this first
  // but what can you do unless you want to do more math below...

  if CapHitTest(X, Y) then
   Exit(False);

  // Get the current Radius of the knob, GetMinRadius returns the smaller of
  // width/height of the client and less the frame width.

  // Todo : May just use FMinRadius as it must be calculated if we had
  // a paint event done. So might not need to recompute
  // shortRadius := FMinRadius - FFrameSettings.BorderWidth;


  // Test if the distance from the mouse to the center is less then the short radius
  // we are in the knob. Remeber we tested for cap and if in that we are not here
  // as the radius must be longer then the cap! We need to use the client sizes
  // for width and height here to get the center!

    Result := Sqrt(((FSpinnerBmp.Width div 2 - X)** 2 + (FSpinnerBmp.Height div 2 - Y)** 2))
          <= (FResolvedSizes.MinRadius - FResolvedSizes.FrameBorderWidth);
end;

// This done for future expansion of sub items in a spinner, for now simple

function TCustomSuperSpinner.HitTest(X, Y: integer) : TSSHitType;
begin
  // if are using the cap as a button, and it's a hit all done, only one
  // can be hit at a time

  if CapHitTest(X, Y) then
    Exit(shtCap);

  // now on with the knob

  if KnobHitTest(X, Y) then
    Exit(shtknob);

  Result := shtNone;
end;

procedure TCustomSuperSpinner.Paint;
var
  offsetX, offsetY: integer;

begin
  if (ClientWidth = 0) or (ClientHeight = 0) then
    exit;

  // Generally all objects should draw in the center of the client area, so
  // the spinner is ALWAYS square. IF the initializebitmap routine is used
  // it also has min size for width and height, but you still need to div/2
  // to get the radius, this can save a bit of calcs in loops if it can be used.

  // ResolveSizes calculates a bunch of sizes for the component based on the
  // setting of the AutoScale. It MUST be called prior to paint so all needed
  // sizes and dimensions for drawing the elements are resolved!

  ResolveSizes;

  // IF the component is resized OR moved (this is safer) we
  // need to make sure EVERYTHING redraws. The base class will
  // also do it's own thing to invalidate and redraw it all.

  if FDirty then
  begin
    FPositionSettings.Dirty := True;  // sjg - this is ALWAYS dirty for drawing
    FCapSettings.Dirty := True;
    FFrameSettings.Dirty := True;
    FKnobSettings.Dirty := True;

    FDirty := False;  // everything here marked, so can reset
  end;

  // no cost on SetSize if same sizes!

  FSpinnerBmp.SetSize(Width, Height);

  // If the spinner color is clNone then we start with a transparent background,
  // Otherwise we start with the users color.

  if Color = clNone then
    FSpinnerBmp.Fill(BGRA(0, 0, 0, 0))  // fill transparent
  else
    FSpinnerBmp.Fill(ColorToBGRA(Color, 255));  // fill solid color

  // If the frame changes we must dirty the knob as the frame
  // changes could impact size of the knob

  if FFrameSettings.Dirty then
    FKnobSettings.Dirty := True;

  DrawFrame;
  FSpinnerBmp.BlendImage(0, 0, FFrameBmp, boLinearBlend);

  DrawKnob;
  offsetX := FSpinnerBmp.Width div 2  - FKnobBmp.Width div 2;
  offsetY := FSpinnerBmp.Height div 2 - FKnobBmp.Height div 2;
  FSpinnerBmp.BlendImage(offsetX, offsetY, FKnobBmp, boLinearBlend);

  // Position is most always rendered and drawn. Could optimize
  // by drawing the position and moving around the spinnerbmp
  // by getting the correct position. Left as an exercise for the
  // coder...

  DrawPosition;
  FSpinnerBmp.BlendImage(0, 0, FPositionBmp, boLinearBlend);

  // Draw Cap last as it can be a nice look over lines if needed

  if FCapSettings.Style <> csNone then
  begin
    DrawCap;
    offsetX := FSpinnerBmp.Width div 2  - FCapBmp.Width div 2;
    offsetY := FSpinnerBmp.Height div 2 - FCapBmp.Height div 2;
    FSpinnerBmp.BlendImage(offsetX, offsetY, FCapBmp, boLinearBlend);
  end;

  // draw other stuff as needed here before the canvas draw

  FSpinnerBmp.Draw(Canvas, 0, 0, False);
end;

procedure TCustomSuperSpinner.DrawFrame;
var
  Origin: TSSOrigin;
  r: integer;

begin
  if not FFrameSettings.Dirty then
    Exit;

  FFrameSettings.Dirty := False;

  // Origin has the correct Max size the radius can be!

  Origin := Initializebitmap(FFrameBmp, Width, Height);

  // skip doing anything further if border is 0

  if FResolvedSizes.FrameBorderWidth < 1 then
    Exit;

  // Get the radius of the frame, less border so we can fit

  r := FResolvedSizes.MinRadius - FResolvedSizes.FrameBorderWidth div 2 - 1;

  // Draw thin antialiased border to smooth against background

  FFrameBmp.EllipseAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y,
    r, r,
    FFrameSettings.BorderColor,
    FResolvedSizes.FrameBorderWidth);
end;

procedure TCustomSuperSpinner.DrawKnob;
var
  xy: integer;
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
  if not FKnobSettings.Dirty then
    Exit;

  FKnobSettings.Dirty := False;

  // set the knob size less the radius

  xy := FResolvedSizes.MinWH;

  FKnobBmp.SetSize(xy, xy);

  // Clear bitmap to transparent

  FKnobBmp.Fill(BGRA(0, 0, 0, 0));
  Center := PointF(xy / 2, xy / 2);

  case FKnobSettings.Style of
  ssFlat:
    begin // draw flat knob

      // This will draw it filled with an edge, must remove both
      // the knob's edge and the frames edge thickness to get the right size

        FKnobBmp.EllipseAntialias(Center.x, Center.y,
        FResolvedSizes.MinRadius - FResolvedSizes.FrameBorderWidth - FResolvedSizes.KnobEdgeThickness div 2,
        FResolvedSizes.MinRadius - FResolvedSizes.FrameBorderWidth - FResolvedSizes.KnobEdgeThickness div 2,
          FKnobSettings.EdgeColor,
          FResolvedSizes.KnobEdgeThickness,
          FKnobSettings.FillColor);
    end;

  ssShaded:
    begin   // shaded knob

      FKnobBmp.FillEllipseLinearColorAntialias(Center.x, Center.y,
        FResolvedSizes.MinRadius - FResolvedSizes.FrameBorderWidth,
        FResolvedSizes.MinRadius - FResolvedSizes.FrameBorderWidth,
        FKnobSettings.EdgeColor,
        FKnobSettings.FillColor);

      FKnobBmp.EllipseAntialias(Center.x, Center.y,
        FResolvedSizes.MinRadius - FResolvedSizes.FrameBorderWidth - FResolvedSizes.KnobEdgeThickness div 2,
        FResolvedSizes.MinRadius - FResolvedSizes.FrameBorderWidth - FResolvedSizes.KnobEdgeThickness div 2,
        FKnobSettings.EdgeColor,
        FResolvedSizes.KnobEdgeThickness);
    end;

  ssPhong:
    begin // Phong shaded knob

      // compute spinner height map

      Map := TBGRABitmap.Create(xy, xy);

      for yb := 0 to xy - 1 do
      begin
        p := map.ScanLine[yb];
        for xb := 0 to xy - 1 do
        begin

          // compute vector between center and current pixel

          v := PointF(xb, yb) - Center;

          // scale down to unit circle (with 1 pixel margin for soft border)

          v.x := v.x / (xy / 2 + 1);
          v.y := v.y / (xy / 2 + 1);

          // compute squared distance with scalar product

          d2 := v {$if FPC_FULLVERSION < 30203}*{$ELSE}**{$ENDIF} v;

          // interpolate as quadratic curve and apply power function

          if d2 > 1 then
            h := 0
          else
            h := power(1 - d2, FKnobSettings.CurveExponent);
          p^ := MapHeightToBGRA(h, 255);
          Inc(p);
        end;
      end;

    mask := TBGRABitmap.Create(xy, xy, BGRABlack);

    // Adjust Size for frame AND knob edge. Note this is a FILL so no div 2

    Mask.FillEllipseAntialias(Center.x, Center.y,
     FResolvedSizes.MinRadius - FResolvedSizes.FrameBorderWidth - FResolvedSizes.KnobEdgeThickness +1,
     FResolvedSizes.MinRadius - FResolvedSizes.FrameBorderWidth - FResolvedSizes.KnobEdgeThickness +1,
     BGRAWhite);

      map.ApplyMask(mask);
      Mask.Free;

      KnobSettings.FPhong.Draw(FKnobBmp, Map, 30, 0, 0, FKnobSettings.FillColor);
      Map.Free;

    // Fill the edge now

    FKnobBmp.EllipseAntialias(Center.x, Center.y,
      FResolvedSizes.MinRadius - FResolvedSizes.FrameBorderWidth - FResolvedSizes.KnobEdgeThickness div 2,
      FResolvedSizes.MinRadius - FResolvedSizes.FrameBorderWidth - FResolvedSizes.KnobEdgeThickness div 2,
         FKnobSettings.EdgeColor,
        FResolvedSizes.KnobEdgeThickness);
    end;
  end;
end;

procedure TCustomSuperSpinner.DrawCap;
var
  Origin: TSSOrigin;
  sizeWH : integer;
  pCapEdge : integer;
  xy: integer;
  xyFDiv2: single;
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

  if not FCapSettings.Dirty then
    Exit;

  FCapSettings.Dirty := False;

  // drawing is the size of the cap, not of the entire knob!

  sizeWH := (FResolvedSizes.CapRadius + FResolvedSizes.CapEdgeThickness) * 2 + 2;
  Origin := Initializebitmap(FCapBmp, SizeWH, SizeWH);

  // can skip drawing if nothing to draw, but still needed to init the bmp

  if FCapSettings.Style = csNone then
    Exit;

  pCapEdge := FResolvedSizes.CapRadius + FResolvedSizes.CapEdgeThickness div 2;

  case FCapSettings.Style of

    csFlat:
      begin

        // Draw the flat cap, but make sure size is similar to the shaded below or will be odd

        FCapBmp.EllipseAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y,
          pCapEdge,
          pCapEdge,
          FCapSettings.EdgeColor,
          FResolvedSizes.CapEdgeThickness,
          FCapSettings.FillColor);
      end;

    csShaded:
      begin

        // Regular shading

         FCapBmp.FillEllipseLinearColorAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y,
           pCapEdge,
           pCapEdge,
           FCapSettings.FillColor,
           FCapSettings.EdgeColor);

         // draw edge since the shading is backwards ending on fill color not Edge

         FCapBmp.EllipseAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y,
           pCapEdge,
           pCapEdge,
           FCapSettings.EdgeColor,
           FResolvedSizes.CapEdgeThickness);
      end;

    csPhong:
      begin

        // Phong shaded cap

        // Draw a flat radius around the cap if set, must be alpha 0 or will not
        // be an outline. Draw First, fixes some issues with Phong drawing

        xy := FResolvedSizes.CapRadius * 2 ;
        xyFDiv2 := FResolvedSizes.CapRadius;

        if xy = 0 then
          Exit;

         if FResolvedSizes.CapEdgeThickness > 0 then
           FCapBmp.EllipseAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y,
             pCapEdge - 1,  // suck in a little to make sure we are under it all
             pCapEdge - 1,
             FCapSettings.EdgeColor,
             FResolvedSizes.CapEdgeThickness);

          // compute knob height map

          Center := PointF(xyFDiv2 , xyFDiv2);
          Map := TBGRABitmap.Create(xy, xy);

          for yb := 0 to xy - 1 do
          begin
            p := map.ScanLine[yb];
            for xb := 0 to xy - 1 do
            begin

              // compute vector between center and current pixel

              v := PointF(xb, yb) - Center;

              // scale down to unit circle (with 1 pixel margin for soft border)

              v.x := v.x / (xyFDiv2 + 1);
              v.y := v.y / (xyFDiv2 + 1);

              // compute squared distance with scalar product

              d2 := v {$if FPC_FULLVERSION < 30203}*{$ELSE}**{$ENDIF} v;

              // interpolate as quadratic curve and apply power function

              if d2 > 1 then
                h := 0
              else
                h := power(1 - d2, FCapSettings.CurveExponent);
              p^ := MapHeightToBGRA(h, 255);
              Inc(p);
            end;
          end;

          // mask image round with and antialiased border

          mask := TBGRABitmap.Create(xy, xy, BGRABlack);

          // Shrink the size by one as the antialias gets chopped on the right edge
          // if the image is full size. Looks nicer too.

          Mask.FillEllipseAntialias(Center.x, Center.y, xyFDiv2 - 1, xyFDiv2 - 1 , BGRAWhite);
          map.ApplyMask(mask);
          Mask.Free;

          // now draw it all

          FCapSettings.FPhong.Draw(FCapBmp, Map, 30,
                  Origin.CenterPoint.x - xy div 2, Origin.CenterPoint.y - xy div 2,
                  FCapSettings.FillColor);
          Map.Free;
      end;

    csOutline:
      begin

        // Just an outline

        if FResolvedSizes.CapEdgeThickness > 0 then
          FCapBmp.EllipseAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y,
            pCapEdge,
            pCapEdge,
            FCapSettings.EdgeColor,
            FResolvedSizes.CapEdgeThickness);
      end;
  end;
end;

procedure TCustomSuperSpinner.DrawPosition;
var
  Center, Pos: TPointF;
  PosColor: TBGRAPixel;
  PosLen, x,y,xt,yt: single;
  i, n : integer;

begin
  // Note this is mostly always be dirty, if the knob moves or a setting
  // changes it's dirty so always, no need to currently check dirty flag

  // Do some magic since we can adjust opacity with an additional property
  // This sometimes draws different color in design vs. runtime BGRA issue??

  PosColor := ColorToBGRA(ColorToRGB(FPositionSettings.FillColor), FPositionSettings.Opacity);

  // set up positions for position indicator, use ResolvedSizes!
  // Pos.X and Pos.Y should be both based on the minimum sized dimension

  Center := PointF(ClientWidth / 2, ClientHeight / 2);
  Pos.X := Cos(FAngularPos) * (FResolvedSizes.MinWH / 2);
  Pos.Y := -Sin(FAngularPos) * (FResolvedSizes.MinWH / 2);

  PosLen := VectLen(Pos);
  Pos := Pos * ((PosLen - FResolvedSizes.PositionMargin - FResolvedSizes.PositionRadius) / PosLen);
  Pos := Center + Pos;

  // Size and Clear bitmap to transparent, keep full size bitmap

  FPositionBmp.SetSize(ClientWidth, ClientHeight);
  FPositionBmp.Fill(BGRA(0, 0, 0, 0));

  case PositionSettings.Style of
    psFilledCircle:
      begin
        FPositionBmp.FillEllipseAntialias(Pos.X, Pos.Y,
            FResolvedSizes.PositionRadius, FResolvedSizes.PositionRadius,
            PosColor);
      end;

    psHollowCircle:
      begin
        FPositionBmp.EllipseAntialias(Pos.X, Pos.Y,
            FResolvedSizes.PositionRadius, FResolvedSizes.PositionRadius,
            PosColor, FPositionSettings.LineWidth);
      end;
    psShaded:
      begin
        // Regular shading similar to Cap

         FPositionBmp.FillEllipseLinearColorAntialias(Pos.X, Pos.Y,
         FResolvedSizes.PositionRadius, FResolvedSizes.PositionRadius,
           Poscolor,
           FPositionSettings.EdgeColor);
      end;

    psIndentCircle:
      begin
        // hack to give some indented depth, Doing colors
        // backwards to make it look nicer.

        FPositionBmp.FillEllipseLinearColorAntialias(Pos.X, Pos.Y,
            FResolvedSizes.PositionRadius, FResolvedSizes.PositionRadius,
             PosColor, FKnobSettings.EdgeColor);

        FPositionBmp.EllipseAntialias(Pos.X, Pos.Y,
            FResolvedSizes.PositionRadius, FResolvedSizes.PositionRadius,
             PosColor, 1);
      end;

    psLines:
      begin
        FPositionBmp.LineCap := pecRound; // ensure correct cap mode
        n := FPositionSettings.LineCount;

        // Skip if number of lines is 0

        if n > 0 then
          for i := 0 to n - 1 do
          begin
            // Center Point

            x := Center.x - FResolvedSizes.PositionCenterMargin * cos((i * 360 / n) * Pi / 180 - FAngularPos - PI);
            y := Center.y - FResolvedSizes.PositionCenterMargin * sin((i * 360 / n) * Pi / 180 - FAngularPos - PI);

            // Draw to Outer Point

            xt := Center.x - (FResolvedSizes.MinRadius - FResolvedSizes.PositionMargin) * cos((i * 360 / n) * Pi / 180 - FAngularPos - PI);
            yt := Center.y - (FResolvedSizes.MinRadius - FResolvedSizes.PositionMargin)* sin((i * 360 / n) * Pi / 180 - FAngularPos - PI);

            FPositionBmp.DrawLineAntialias(x, y, xt, yt, PosColor, FResolvedSizes.PositionLineWidth);
          end;
    end;
  end;

  // Draw outer circle border if desired, only for circle types

  if (FPositionSettings.EdgeThickness > 0) and (FPositionSettings.Style <> psLines)
        and (FPositionSettings.Style <> psNone) then
  begin
    FPositionBmp.EllipseAntialias(Pos.X, Pos.Y,
          FResolvedSizes.PositionRadius + FPositionSettings.EdgeThickness div 2,
          FResolvedSizes.PositionRadius + FPositionSettings.EdgeThickness div 2,
          FPositionSettings.EdgeColor, FPositionSettings.EdgeThickness);
  end;
end;

{$IFDEF FPC}
procedure TCustomSuperSpinner.SaveToFile(AFileName: string);
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

procedure TCustomSuperSpinner.LoadFromFile(AFileName: string);
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
{$ENDIF}

procedure TCustomSuperSpinner.OnFindClass(Reader: TReader; const AClassName: string;
  var ComponentClass: TComponentClass);
begin

  if CompareText(AClassName, 'TCustomSuperSpinner') = 0 then
    ComponentClass := TCustomSuperSpinner;
end;

function TCustomSuperSpinner.DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint): boolean;
begin

  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  MouseWheelPos(Shift, WheelDelta);
end;

procedure TCustomSuperSpinner.MouseWheelPos(Shift: TShiftState; WheelDelta: integer);
var
  newValue: single;

begin
  if FLocked then
    Exit;

  // WheelSpeed is a Base Value and a factor to slow or speed up the wheel affect.
  // FWheelSpeed = 0 then no wheel, 1 slowest movement, 255 fastest movement
  // Wheel speed still just does one step no matter what the wheel angle is set to
  // so the WheelSpeed just really adjust the look of how fast the knob spins

  if FWheelSpeed > 0 then
  begin

    // WheelDelta should just catch direction, negative or positive
    // not sure if 0 is ever possible????

    if WheelDelta >= 0 then
      newValue := -1.0
    else
      newValue := 1.0;

    // Must invalidate both as we don't know the current direction it's moving
    // so one will get reset, the other will trigger, so always works.
    // This is used in UpdateAngularPos to help with direction changes

    FCWSkipCounter := FSpinnerResolutionCount - 1;
    FCCWSkipCounter := FCWSkipCounter;

    // Scale the Wheel rate so 1-255 will give good dynamic range of really slow to really fast

    // TIP : To make the mouse movement sorta' match the Resolution you can change
    // the Wheel speed to make it more closely match if resolution is not the highest

    UpdateAngularPos(Shift, FAngularPos + WHEEL_SPEED_FACTOR * newValue * FWheelSpeed);

  end; // wheel speed enabled
end;

end.
