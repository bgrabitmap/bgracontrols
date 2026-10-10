// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Part of BGRA Controls. Made by third party.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html

}
{******************************* CONTRIBUTOR(S) ******************************
- Sandy Ganz | sganz@pacbell.net
  Evolved from BGRAShape, thin wrapper to make a nice looking LED component. Fast
  cached drawing and a bunch of shape, size and drawing options. Special thanks
  to the BGRA team for nice code in the BGRAShape component that was easily
  reused and modified.

  Note On Auto Scale -

  This component by default has Auto Scale OFF. That means that it will not be
  subjected to any LCL Auto Scaling based on DPI or Screen Zoom (as far as I can test).
  Auto Scale will ONLY cause a change in the component at run time on items that are scaled
  by the Paint procedure. This means that toggling the Auto Scale property will
  NOT change the ClientWidth/Height of the component after the initial form is created.

  Again, changing the Auto Scale setting will not change the Component width/height
  after the component is created. It must be set prior to form create for the scale
  of the ClientWidth/Height to be affected. After that, it will not change.

  In the case of the SuperLED the only item that currently will be modified is
  the 'BorderThickness' IF Auto Scale is set to True. The drawing of the border
  will change between 1.0 Scale and what ever the new scale is at run time, but
  again the components ClientWidth and Height will not.

***************************** END CONTRIBUTOR(S) *****************************}
{******************************** CHANGE LOG *********************************
v1.00 - 07-11-2025 Begat sjg by sganz@pacbell.net
v1.01 - 07-27-2025 Minor Code clean, Comments about Auto Scale

******************************* END CHANGE LOG *******************************}

unit SuperLED;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC} LResources, {$ENDIF} Forms, Controls, Graphics, Dialogs,
  {$IFNDEF FPC} Types, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCBaseCtrls, BGRABitmap, BGRAShape, BGRABitmapTypes, BGRAGradientScanner, BCTypes;

const
  VERSIONSTR = '1.01';            // SLED version, Should ALWAYS show as a delta when merging!
  BASELINE_SIZE = 32;             // Default size for the LED
  MAX_SHAPE_SIDES = 6;            // Max sides for a shape, Hexagon is 6
  DARKEN_PERCENT = 50;            // Darkening Default for the Inactive color
  BRIGHTNESS_SCALER = 32767;      // Used to make scale sorta 0-100 percent in Brightness where 32767 is 100%

type
  TSLEDStyle = (slsFlat, slsShaded);
  TSLEDShape = (slshRound, slshSquare, slshTriangle, slshPentagon, slshHexagon);

  { TSuperLED }

  TSuperLED = class(TBGRAGraphicCtrl)
  private
    { Private declarations }

    FActive: boolean;
    FAutoScale: boolean;
    FActiveColor: TColor;
    FInactiveColor: TColor;
    FInactiveBrightness: integer;
    FStyle: TSLEDStyle;
    FShape: TSLEDShape;
    FOnChange: TNotifyEvent;
    FActiveBmp: TBGRABitmap;
    FInactiveBmp: TBGRABitmap;
    FBorderColor: TColor;
    FBorderOpacity: byte;
    FBorderStyle: TPenStyle;
    FBorderThickness: integer;
    FRoundRadius: integer;
    FFillOpacity: byte;
    FBorderGradient: TBCGradient;
    FFillGradient: TBCGradient;
    FAngle: single;
    FScaling: Double;
    FDirty: boolean;

    function ShapeToCount(AShape: TSLEDShape): integer;
    procedure SetActive(AValue: boolean);
    procedure SetActiveColor(AValue: TColor);
    procedure SetAutoScale(AValue: boolean);
    procedure SetInactiveColor(AValue: TColor);
    procedure SetInactiveBrightness(AValue: integer);
    procedure SetBorderColor(AValue: TColor);
    procedure SetBorderOpacity(Avalue: byte);
    procedure SetBorderThickness(AValue: integer);
    procedure SetBorderStyle(AValue: TPenStyle);
    procedure SetStyle(AValue: TSLEDStyle);
    procedure SetShape(AValue: TSLEDShape);
    procedure SetAngle(AValue: single);
    procedure SetRoundRadius(AValue: integer);
    procedure SetFillOpacity(AValue: byte);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure DoChange;

  protected
    { Protected declarations }

    class function GetControlClassDefaultSize: TSize; override;
    procedure Paint; override;
    procedure SetColor(AValue: TColor); override;
    function GetColor: TColor;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DrawLED;
    procedure DrawLEDBmp(LEDBitmap: TBGRABitmap; Active: boolean);
    function CreateGradient(AGradient: TBCGradient; ARect: TRect): TBGRAGradientScanner;
    function Brightness(Color: TColor; Brightness: integer): TBGRAPixel;

  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy;
          const AFromPPI, AToPPI, AOldFormWidth, ANewFormWidth: Integer); override;
  public
    { Streaming }

    {$IFDEF FPC}
    procedure SaveToFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);
    {$ENDIF}
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);

  published
    { Published declarations }

    property ActiveColor: TColor read FActiveColor write SetActiveColor default clRed;
    property InactiveColor: TColor read FInactiveColor write SetInactiveColor default clBlack;
    property InactiveBrightness: integer read FInactiveBrightness write SetInactiveBrightness default DARKEN_PERCENT;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;
    property BorderOpacity: byte read FBorderOpacity write SetBorderOpacity default 255;
    property BorderThickness: integer read FBorderThickness write SetBorderThickness default 1;
    property BorderStyle: TPenStyle Read FBorderStyle Write SetBorderStyle default psSolid;
    property RoundRadius: integer read FRoundRadius write SetRoundRadius default 0;
    property FillOpacity: byte read FFillOpacity write SetFillOpacity default 255;
    property Style: TSLEDStyle read FStyle write SetStyle default slsShaded;
    property Shape: TSLEDShape read FShape write SetShape default slshRound;
    property Angle: single read FAngle write SetAngle default 0;
    property Active: boolean read FActive write SetActive default False;
    property AutoScale: boolean read FAutoScale write SetAutoScale default False;
    property Color: TColor read GetColor write SetColor default clNone; // need to override the ancestor since we need to dirty to update
    property Align;
    property ShowHint;
    property Anchors;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;

    // Debug Only TO allow easy reading of Auto Scale Factor
    // property ScalingFactor: double read FScaling;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

uses BCTools; // possibly get the gradient code into here

{$IFDEF FPC}
procedure Register;
begin
  RegisterComponents('BGRA Controls', [TSuperLED]);
end;
{$ENDIF}

{ TSuperLED }

procedure TSuperLED.AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy;
  const AFromPPI, AToPPI, AOldFormWidth, ANewFormWidth: Integer);
begin
  // If autoscaling then we will let the system mess with the component size
  // otherwise it will just leave it along as the ACTUAL size in the designer
  // as 1:1 with no scaling on anything. By not calling AutoAdjustLayout()
  // Scaling will be 1:1
  //
  // Note - that toggling the AutoScale setting will cause a repaint
  // but NOT a resize of the Components client area

  if FAutoScale then
    inherited AutoAdjustLayout(AMode, AFromPPI, AToPPI, AOldFormWidth, ANewFormWidth);
end;

constructor TSuperLED.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  FDirty := True;
  FActiveBmp := TBGRABitmap.Create;
  FInactiveBmp := TBGRABitmap.Create;
  FActiveColor := clRed;
  FInactiveColor := clBlack;
  FInactiveBrightness := DARKEN_PERCENT; // In percent of brightness, 100 is full on, 0 black
  FShape := slshRound;
  FStyle := slsShaded;
  FBorderColor := clGray;
  FBorderOpacity := 255;
  FBorderThickness := 1;
  FBorderStyle := psSolid;
  FRoundRadius := 0;
  FFillOpacity := 255;
  FAngle := 0;
  FScaling := 1.0;
  FAutoScale := False;
  Color := clNone;

  // Border Gradient

  FBorderGradient := TBCGradient.Create(Self);
  FBorderGradient.Point2XPercent := 100;
  FBorderGradient.StartColor := FBorderColor;
  FBorderGradient.EndColor := Brightness(FActiveColor, FInactiveBrightness);

  // Fill Gradient

  FFillGradient := TBCGradient.Create(Self);
  FFillGradient.StartColor := FActiveColor;
  FFillGradient.EndColor := Brightness(FActiveColor, FInactiveBrightness);
end;

destructor TSuperLED.Destroy;
begin
  FActiveBmp.Free;
  FInactiveBmp.Free;
  FFillGradient.Free;
  FBorderGradient.Free;

  inherited Destroy;
end;

// Override the base class which has a rectangular dimension

class function TSuperLED.GetControlClassDefaultSize: TSize;
begin
  // Set the preferred size of the control. This may be subject to scaling!

  Result.CX := BASELINE_SIZE;
  Result.CY := BASELINE_SIZE;
end;

procedure TSuperLED.DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  FDirty := true; // Called on Resize of component
end;

// Original from BCTools.pas

function TSuperLED.CreateGradient(AGradient: TBCGradient; ARect: TRect): TBGRAGradientScanner;
begin
  Result := TBGRAGradientScanner.Create(
    ColorToBGRA(ColorToRGB(AGradient.StartColor), AGradient.StartColorOpacity),
    ColorToBGRA(ColorToRGB(AGradient.EndColor), AGradient.EndColorOpacity),
    AGradient.GradientType, PointF(ARect.Left + Round(
    ((ARect.Right - ARect.Left) / 100) * AGradient.Point1XPercent),
    ARect.Top + Round(((ARect.Bottom - ARect.Top) / 100) * AGradient.Point1YPercent)),
    PointF(ARect.Left + Round(((ARect.Right - ARect.Left) / 100) *
    AGradient.Point2XPercent), ARect.Top + Round(
    ((ARect.Bottom - ARect.Top) / 100) * AGradient.Point2YPercent)),
    AGradient.ColorCorrection, AGradient.Sinus);
end;

// sets the brightness for a color. Useful for a single
// color setting and dim or bright changes on that.
// Brightness = 0  Black
// Brightness = 100 Origional Color
// Brightness > 100 your mileage may vary
//
// In precent as indicated above

function TSuperLED.Brightness(Color: TColor; Brightness: integer): TBGRAPixel;
begin
  Result := ApplyIntensityFast(ColorToBGRA(ColorToRGB(Color)), Round((Brightness / 100) * BRIGHTNESS_SCALER));
end;

// given a shape type get the number of sides. slshRound is not really
// used but here just in case...

function TSuperLED.ShapeToCount(AShape: TSLEDShape): integer;
begin
  // Only allow a few predefined shapes for the LED, so this helper
  // Translates to what's needed if a polygon is drawn. slshRound
  // is not really used, but left in just because.
  //
  // TSLEDShape = (slshRound = 0, slshSquare = 4, slshTriangle = 3,
  //               slshPentagon = 5, slshHexagon = 6)

  case AShape of
    slshRound: Result := 0;
    slshTriangle: Result := 3;
    slshSquare: Result := 4;
    slshPentagon: Result := 5;
    slshHexagon: Result := 6;
  else
      Result := 0;  // slshRound
  end;
end;

procedure TSuperLED.SetActive(AValue: boolean);
begin
  if FActive = AValue then
    Exit;

  FActive := AValue;
  Invalidate; // don't set the dirty flag since we don't count this as dirty, just a redraw
end;

procedure TSuperLED.SetActiveColor(AValue: TColor);
begin
  if FActiveColor = AValue then
    Exit;

  FActiveColor := AValue;
  DoChange;
end;

// The Set/Get color must be overidden since they are in the ancestor class
// and we need to know they changed since the LED needs to see a Dirty flag
// to repaint efficently the way we pre-paint active and inactive bitmaps

function TSuperLED.GetColor: TColor;
begin
  Result := inherited Color;
end;

procedure TSuperLED.SetColor(AValue: TColor);
begin
  if inherited Color = AValue then
    Exit;

  inherited SetColor(AValue);
  DoChange;
end;

procedure TSuperLED.SetInactiveColor(AValue: TColor);
begin
  if FInactiveColor = AValue then
    Exit;

  FInactiveColor := AValue;
  DoChange;
end;

// allows a 0-100% change in brightness for the INACTIVE state as
// well as used for the gradient transistions

procedure TSuperLED.SetInactiveBrightness(AValue: integer);
begin
  if (FInactiveBrightness = AValue) or (AValue < 0) or (AValue > 100) then
    Exit;

  FInactiveBrightness := AValue;
  DoChange;
end;

procedure TSuperLED.SetBorderColor(AValue: TColor);
begin
  if FBorderColor = AValue then
    Exit;

  FBorderColor := AValue;
  DoChange;
end;

procedure TSuperLED.SetBorderOpacity(Avalue: byte);
begin
  if FBorderOpacity = AValue then
    Exit;

  FBorderOpacity := AValue;
  DoChange;
end;

procedure TSuperLED.SetBorderThickness(Avalue: integer);
begin
  if (FBorderThickness = AValue) or (AValue < 0) then
    Exit;

  FBorderThickness := AValue;
  DoChange;
end;

procedure TSuperLED.SetBorderStyle(AValue: TPenStyle);
begin
  if FBorderStyle = AValue then
    Exit;

  FBorderStyle := AValue;
  DoChange;
end;

procedure TSuperLED.SetStyle(AValue: TSLEDStyle);
begin
  if FStyle = AValue then
    Exit;

  FStyle := AValue;
  DoChange;
end;

procedure TSuperLED.SetShape(AValue: TSLEDShape);
begin
  if FShape = AValue then
    Exit;

  FShape := AValue;
  DoChange;
end;

procedure TSuperLED.SetAngle(AValue: single);
begin
  if FAngle = AValue then
    Exit;

  FAngle := AValue;
  DoChange;
end;

procedure TSuperLED.SetAutoScale(AValue: boolean);
begin
  if FAutoScale = AValue then
    Exit;

  FAutoScale := AValue;
  DoChange;
end;

procedure TSuperLED.SetRoundRadius(AValue: integer);
begin
  if FRoundRadius = AValue then
    Exit;

  FRoundRadius := AValue;
  DoChange;
end;

procedure TSuperLED.SetFillOpacity(AValue: byte);
begin
  if FFillOpacity = AValue then
    Exit;

  FFillOpacity := AValue;
  DoChange;
end;

procedure TSuperLED.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;

  // this will not dirty it

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSuperLED.DoChange;
begin
  FDirty := True;
  Invalidate;   // if we get here a prop must have changed, mark dirty

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSuperLED.Paint;
begin
  inherited Paint;

  // Scaling should only affect visuals that are not based on Width/Height
  // as they change when scaled, so if based ClientWidth/Height, good, but not all are!
  // IF Scaling then this may/will be not 1:1 if the LCL is scaling the form.
  // Also note that set properties for objects like Border thickness will need to
  // be scaled scaled here.

  // Somewhat experimental, seems to work OK

  if FAutoScale then
    FScaling := ScaleDesignToForm(BASELINE_SIZE)/BASELINE_SIZE // just get the ratio from arbitrary dims
  else
    FScaling := 1.0;  // not scaling the component, so no scale of anything else done

  DrawLED;

  if FActive then
  begin
    // draw the Active BMP to the canvas

    FActiveBmp.Draw(Canvas, 0, 0, False);
  end
  else
  begin
    // Draw the Inactive BMP to the canvas

    FInactiveBmp.Draw(Canvas, 0, 0, False);
  end;
end;

procedure TSuperLED.DrawLED;
begin
  // See if we need to redraw the bitmaps, we always do both
  // the Active and Inactive as we need both.

  if Not FDirty then
    Exit;

  FDirty := False;

  // Draw the Active then Inactive

  FActiveBmp.SetSize(Width, Height);
  FInactiveBmp.SetSize(Width, Height);

  // Clear bitmap to transparent or background color.
  // NOTE we must overide the ancestor class to force a dirty
  // flag for design time repaint, see SetColor() code/comments

  if Color = clNone then
  begin
    FActiveBmp.FillTransparent;
    FInactiveBmp.FillTransparent;
  end
  else
  begin
    FActiveBmp.Fill(Color);
    FInactiveBmp.Fill(Color);
  end;

  // The magic happens in DrawLEDBmp!

  DrawLEDBmp(FActiveBmp, True);
  DrawLEDBmp(FInactiveBmp, False);
end;

procedure TSuperLED.DrawLEDBmp(LEDBitmap: TBGRABitmap; Active: boolean);
var
  cx, cy, rx, ry, a: single;
  coords: array[0..MAX_SHAPE_SIDES] of TPointF;
  minCoord, maxCoord: TPointF;
  i: integer;
  borderGrad, fillGrad: TBGRACustomScanner;
  sideCnt: integer;
  flatFillColor : TColor;

begin
  // Basline code from BGRAShape, changed to work a bit better
  // with the simplified LED shapes and borders. Also updated to
  // support different fill color based on the state and Gradient borders

  sideCnt := ShapeToCount(FShape);  // get the number of sides for the users shape
  LEDBitmap.PenStyle := FBorderStyle;

  FFillGradient.EndColor := Brightness(FActiveColor, FInactiveBrightness);
  FBorderGradient.Startcolor := FBorderColor;
  FBorderGradient.EndColor := Brightness(FBorderColor, FInactiveBrightness);

  // set up anything related to the state, mostly color

  if Active then
  begin
    FFillGradient.StartColor := FActiveColor;
    flatFillColor := FActiveColor;
  end
  else
    begin
      FFillGradient.StartColor := FInactiveColor;
      flatFillColor := Brightness(FInactiveColor, FInactiveBrightness); // allow brightness on flat
    end;

  with LEDBitmap.Canvas2D do
  begin
    lineJoin := 'round';

    // if we are shaded we gradient both fill and border
    // if not we draw both flat.

    if FStyle = slsShaded then // use Gradient
    begin
      FBorderGradient.StartColorOpacity := FBorderOpacity; // sjg - Added Opacity to both
      FBorderGradient.EndColorOpacity := FBorderOpacity;
      borderGrad := CreateGradient(FBorderGradient, Classes.rect(0, 0, LEDBitmap.Width, LEDBitmap.Height));
      strokeStyle(borderGrad);
    end
    else
      begin
        borderGrad := nil;
        strokeStyle(ColorToBGRA(ColorToRGB(FBorderColor), FBorderOpacity));
      end;

    lineStyle(LEDBitmap.CustomPenStyle);
    lineWidth := FBorderThickness * FScaling;

    if FStyle = slsShaded then
    begin
      fillGrad := CreateGradient(FFillGradient, Classes.rect(0, 0, LEDBitmap.Width, LEDBitmap.Height));
      fillStyle(fillGrad);
    end
    else
      begin
        fillGrad := nil;
        fillStyle(ColorToBGRA(ColorToRGB(flatFillColor), FFillOpacity));
      end;

    cx := LEDBitmap.Width / 2;
    cy := LEDBitmap.Height / 2;
    rx := (LEDBitmap.Width - FBorderThickness * FScaling) / 2;
    ry := (LEDBitmap.Height - FBorderThickness * FScaling) / 2;

    // Now Draw a circle or polygon

    if FShape = slshRound then
    begin
      // slshRound - circle

      save;
      translate(cx, cy);
      scale(rx, ry);
      beginPath;
      arc(0, 0, 1, 0, 2 * Pi);
      restore;
    end
    else
      begin
        // Polygon drawing all here

        for i := 0 to sideCnt - 1 do
        begin
          a := (i / sideCnt + FAngle / 360) * 2 * Pi;
          coords[i] := PointF(sin(a), -cos(a));
        end;

        minCoord := coords[0];
        maxCoord := coords[0];

        for i := 1 to sideCnt - 1 do
        begin
          if coords[i].x < minCoord.x then
            minCoord.x := coords[i].x;
          if coords[i].y < minCoord.y then
            minCoord.y := coords[i].y;
          if coords[i].x > maxCoord.x then
            maxCoord.x := coords[i].x;
          if coords[i].y > maxCoord.y then
            maxCoord.y := coords[i].y;
        end;

        for i := 0 to sideCnt - 1 do
        begin
          with (coords[i] - minCoord) do
            coords[i] := PointF((x / (maxCoord.x - minCoord.x) - 0.5) *
              2 * rx + cx, (y / (maxCoord.y - minCoord.y) - 0.5) * 2 * ry + cy);
        end;

        beginPath;

        for i := 0 to sideCnt - 1 do
        begin
          lineTo((coords[i] + coords[(i + 1) mod sideCnt]) * (1 / 2));
          arcTo(coords[(i + 1) mod sideCnt], coords[(i + 2) mod sideCnt], FRoundRadius);
        end;

        closePath;
      end;

    fill;
    if FBorderThickness <> 0 then
      stroke;

    fillStyle(BGRAWhite);
    strokeStyle(BGRABlack);

    fillGrad.Free;
    borderGrad.Free;
  end;
end;

{$IFDEF FPC}
procedure TSuperLED.SaveToFile(AFileName: string);
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

procedure TSuperLED.LoadFromFile(AFileName: string);
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

procedure TSuperLED.OnFindClass(Reader: TReader; const AClassName: string;
  var ComponentClass: TComponentClass);
begin
  if CompareText(AClassName, 'TSuperLED') = 0 then
    ComponentClass := TSuperLED;
end;

end.
