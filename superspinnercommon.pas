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
  Evolved from BGRAKnob and SuperGauge, changed style to be more inline with
  SuperGauge settings and related. Mostly support classes
***************************** END CONTRIBUTOR(S) *****************************}
unit SuperSpinnerCommon;

{$I bgracontrols.inc}

interface
uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ELSE}Types, {$ENDIF} Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRAGradients;

type
  TSSPositionStyle = (psNone, psFilledCircle, psHollowCircle, psShaded, psIndentCircle, psLines);
  TSSStyle = (ssFlat, ssShaded, ssPhong);
  TSSCapStyle = (csNone, csFlat, csShaded, csPhong, csOutline);
  TSSDirection = (sdCW, sdCCW);

  { TSSOrigin }

  TSSOrigin = packed record
    CenterPoint: TPoint;
    Radius: integer;
  end;

  { TSSFrameSettings }

  TSSFrameSettings = class(TPersistent)
  private
    FBorderColor: TColor;
    FBorderWidth: integer;
    FOnChange: TNotifyEvent;
    FDirty: boolean;

    procedure SetBorderWidth(AValue: integer);
    procedure SetBorderColor(AValue: TColor);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure DirtyOnChange;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Dirty: boolean read FDirty write FDirty;

  published
    property BorderWidth: integer read FBorderWidth write SetBorderWidth default 5;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;
  end;

  { TSSPositionSettings }

  TSSPositionSettings = class(TPersistent)
  private
    FEdgeColor: TColor;
    FEdgeThickness: integer;
    FFillColor: TColor;
    FStyle:TSSPositionStyle;
    FMargin: integer;
    FCenterMargin: integer;
    FLineWidth: integer;  // total width of position
    FLineCount: integer;  // Number of lines to be draw
    FRadius: integer;
    FOpacity: byte;
    FDirty: boolean;
    FOnChange: TNotifyEvent;

    procedure SetEdgeColor(AValue: TColor);
    procedure SetEdgeThickness(AValue: integer);
    procedure SetColor(AValue: TColor);
    procedure SetStyle(const AValue: TSSPositionStyle);
    procedure SetMargin(const AValue: integer);
    procedure SetCenterMargin(const AValue: integer);
    procedure SetLineWidth(const AValue: integer);
    procedure SetLineCount(const AValue: integer);
    procedure SetRadius(const AValue: integer);
    procedure SetOpacity(const AValue: byte);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure DirtyOnChange;

  protected
  public

    property Dirty: boolean read FDirty write FDirty;
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;

  published
    property FillColor: TColor read FFillColor write SetColor default clBlack;
    property Style: TSSPositionStyle read FStyle write SetStyle default psLines;
    property Margin: integer read FMargin write SetMargin default 15;
    property CenterMargin: integer read FCenterMargin write SetCenterMargin default 20;
    property LineWidth: integer read FLineWidth write SetLineWidth default 4;
    property LineCount: integer read FLineCount write SetLineCount default 10;
    property Radius: integer read FRadius write SetRadius default 20;
    property Opacity: byte  read FOpacity write SetOpacity default 192;
    property EdgeColor: TColor read FEdgeColor write SetEdgeColor default clGray;
    property EdgeThickness: integer read FEdgeThickness write SetEdgeThickness default 2;
  end;

  { TSSCapSettings }

  TSSCapSettings = class(TPersistent)
    private
      FEdgeColor: TColor;
      FEdgeThickness: integer;
      FFillColor: TColor;
      FOnChange: TNotifyEvent;
      FRadius: integer;
      FCurveExponent: single;
      FStyle: TSSCapStyle;
      FDirty: boolean;

      procedure SetEdgeColor(AValue: TColor);
      procedure SetEdgeThickness(AValue: integer);
      procedure SetFillColor(AValue: TColor);
      procedure SetOnChange(AValue: TNotifyEvent);
      procedure SetRadius(AValue: integer);
      procedure SetLightIntensity(const AValue: integer);
      function GetLightIntensity: integer;
      procedure SetCurveExponent(const AValue: single);
      procedure SetStyle(const AValue: TSSCapStyle);
      procedure DirtyOnChange;

    protected

    public
      FPhong: TPhongShading;
      property Dirty: boolean read FDirty write FDirty;
      constructor Create;
      destructor Destroy; override;
      property OnChange: TNotifyEvent read FOnChange write SetOnChange;

    published
      property EdgeColor: TColor read FEdgeColor write SetEdgeColor default clGray;
      property FillColor: TColor read FFillColor write SetFillColor default clWhite;
      property Radius: integer read FRadius write SetRadius default 20;
      property EdgeThickness: integer read FEdgeThickness write SetEdgeThickness default 2;
      property LightIntensity: integer read GetLightIntensity write SetLightIntensity default 300;
      property CurveExponent: single read FCurveExponent write SetCurveExponent default 0.05;
      property Style: TSSCapStyle read FStyle write SetStyle default csPhong;

    end;

    { TSSKnobSettings }

    TSSKnobSettings = class(TPersistent)
      private
        FEdgeColor: TColor;
        FEdgeThickness: integer;
        FFillColor: TColor;
        FOnChange: TNotifyEvent;
        FCurveExponent: single;
        FStyle: TSSStyle;
        FDirty: boolean;

        procedure SetEdgeColor(AValue: TColor);
        procedure SetEdgeThickness(AValue: integer);
        procedure SetFillColor(AValue: TColor);
        procedure SetOnChange(AValue: TNotifyEvent);
        procedure SetLightIntensity(const AValue: integer);
        function GetLightIntensity: integer;
        procedure SetCurveExponent(const AValue: single);
        procedure SetStyle(const AValue: TSSStyle);
        procedure DirtyOnChange;

      protected

      public
        FPhong: TPhongShading;
        property Dirty: boolean read FDirty write FDirty;
        constructor Create;
        destructor Destroy; override;
        property OnChange: TNotifyEvent read FOnChange write SetOnChange;

      published
        property EdgeColor: TColor read FEdgeColor write SetEdgeColor default clMedGray;
        property FillColor: TColor read FFillColor write SetFillColor default clWhite;
        property EdgeThickness: integer read FEdgeThickness write SetEdgeThickness default 2;
        property LightIntensity: integer read GetLightIntensity write SetLightIntensity default 300;
        property CurveExponent: single read FCurveExponent write SetCurveExponent default 0.05;
        property Style: TSSStyle read FStyle write SetStyle default ssPhong;
      end;

  function Initializebitmap(var Bitmap: TBGRABitmap; Width, Height: integer): TSSOrigin;

implementation

// Helper for all bitmap setup

function Initializebitmap(var Bitmap: TBGRABitmap; Width, Height: integer): TSSOrigin;
begin
  Bitmap.SetSize(Width, Height);

  // Clear bitmap to transparent

  BitMap.Fill(BGRA(0, 0, 0, 0));

  // Get origin information

  Result.CenterPoint.x := Width div 2;
  Result.CenterPoint.y := Height div 2;

  // Take the smallest so radius will always fit

  if Result.CenterPoint.x < Result.CenterPoint.y then
    Result.Radius := Result.CenterPoint.x
  else
    Result.Radius := Result.CenterPoint.y;
end;

{ TSSFrameSettings }

constructor TSSFrameSettings.Create;
begin

  FBorderColor := clGray;
  FBorderWidth := 5;
  FDirty := True;
end;

destructor TSSFrameSettings.Destroy;
begin
  inherited Destroy;
end;

procedure TSSFrameSettings.SetBorderWidth(AValue: integer);
begin
  if (FBorderWidth = AValue) or (AValue < 0) then
    Exit;

  FBorderWidth := AValue;
  DirtyOnChange;
end;


procedure TSSFrameSettings.SetBorderColor(AValue: TColor);
begin
  if FBorderColor = AValue then
    Exit;

  FBorderColor := AValue;
  DirtyOnChange;
end;

procedure TSSFrameSettings.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSSFrameSettings.DirtyOnChange;
begin
  FDirty := True;   // if we get here a prop must have changed, mark dirty

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TSSPositionSettings }

constructor TSSPositionSettings.Create;
begin
  FOpacity := 192;
  FStyle := psLines;
  FEdgeColor := clGray;
  FFillColor := clBlack;
  FMargin := 15;
  FCenterMargin := 40;
  FLineWidth := 4;
  FLineCount := 10;
  FRadius := 20;
  FEdgeThickness := 2;
  FDirty := True;
end;

destructor TSSPositionSettings.Destroy;
begin
  inherited Destroy;
end;
procedure TSSPositionSettings.SetStyle(const AValue: TSSPositionStyle);
begin
  if FStyle = AValue then
    Exit;

  FStyle := AValue;
  DirtyOnChange;
end;

procedure TSSPositionSettings.SetOpacity(const AValue: byte);
begin
  if FOpacity = AValue then
    Exit;

  FOpacity := AValue;
  DirtyOnChange;
end;

procedure TSSPositionSettings.SetEdgeColor(AValue: TColor);
begin
  if FEdgeColor = AValue then
    Exit;

  FEdgeColor := AValue;
  DirtyOnChange;
end;

procedure TSSPositionSettings.SetEdgeThickness(AValue: integer);
begin
  if (FEdgeThickness = AValue) or (AValue < 0) then
    Exit;

  FEdgeThickness := AValue;
  DirtyOnChange;
end;

procedure TSSPositionSettings.SetColor(AValue: TColor);
begin
  if FFillColor = AValue then
    Exit;

  FFillColor := AValue;
  DirtyOnChange;
end;

procedure TSSPositionSettings.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;

  // no dirty needed possibly, call directly

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

// Diameter of the the spinner circle, ignored for lines
procedure TSSPositionSettings.SetRadius(const AValue: integer);
begin
  if FRadius = AValue then
    Exit;

  FRadius := AValue;
  DirtyOnChange;
end;

// Line width for hollow circle, and lines types. Ignored for others
procedure TSSPositionSettings.SetLineWidth(const AValue: integer);
begin
  if FLineWidth = AValue then
    Exit;

  FLineWidth := AValue;
  DirtyOnChange;
end;

// Line count, for lines, Ignored for others
procedure TSSPositionSettings.SetLineCount(const AValue: integer);
begin
  if FLineCount = AValue then
    Exit;

  FLineCount := AValue;
  DirtyOnChange;
end;

// Offset from the edge of the knob
procedure TSSPositionSettings.SetMargin(const AValue: integer);
begin
  if FMargin = AValue then
    Exit;

  FMargin := AValue;
  DirtyOnChange;
end;

// Offset from the center of the knob
procedure TSSPositionSettings.SetCenterMargin(const AValue: integer);
begin
  if FCenterMargin = AValue then
    Exit;

  FCenterMargin := AValue;
  DirtyOnChange;
end;

procedure TSSPositionSettings.DirtyOnChange;
begin
  FDirty := True;   // if we get here some props must have changed, mark dirty

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TSSSpinnerCapSettings }

constructor TSSCapSettings.Create;
begin
  // create a phong shader, will need to delete on clean up

  FPhong := TPhongShading.Create;
  FPhong.LightPositionZ := 100;
  FPhong.LightSourceIntensity := 300;
  FPhong.NegativeDiffusionFactor := 0.8;
  FPhong.AmbientFactor := 0.5;
  FPhong.DiffusionFactor := 0.6;
  FCurveExponent := 0.05;
  FStyle := csPhong;
  FEdgeColor := clGray;
  FFillColor := clWhite;
  FRadius := 20;
  FEdgeThickness := 2;

  FDirty := True;
end;

destructor TSSCapSettings.Destroy;
begin
  FPhong.Free;
  inherited Destroy;
end;

procedure TSSCapSettings.SetStyle(const AValue: TSSCapStyle);
begin
  if FStyle = AValue then
    Exit;

  FStyle := AValue;
  DirtyOnChange;
end;

procedure TSSCapSettings.SetLightIntensity(const AValue: integer);
begin
  if AValue = FPhong.LightSourceIntensity then
    Exit;

  FPhong.LightSourceIntensity := AValue;
  DirtyOnChange;
end;

function TSSCapSettings.GetLightIntensity: integer;
begin
  Result := round(FPhong.LightSourceIntensity);
end;

procedure TSSCapSettings.SetCurveExponent(const AValue: single);
begin
  if FCurveExponent = AValue then
    Exit;

  FCurveExponent := AValue;
  DirtyOnChange;
end;

procedure TSSCapSettings.SetEdgeColor(AValue: TColor);
begin
  if FEdgeColor = AValue then
    Exit;

  FEdgeColor := AValue;
  DirtyOnChange;
end;

procedure TSSCapSettings.SetEdgeThickness(AValue: integer);
begin
  if (FEdgeThickness = AValue) or (AValue < 0) then
    Exit;

  FEdgeThickness := AValue;
  DirtyOnChange;
end;

procedure TSSCapSettings.SetFillColor(AValue: TColor);
begin
  if FFillColor = AValue then
    Exit;

  FFillColor := AValue;
  DirtyOnChange;
end;

procedure TSSCapSettings.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;

  // no dirty needed possibly, call directly

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSSCapSettings.SetRadius(AValue: integer);
begin
  if FRadius = AValue then
    Exit;

  FRadius := AValue;
  DirtyOnChange;
end;

procedure TSSCapSettings.DirtyOnChange;
begin
  FDirty := True;   // if we get here some props must have changed, mark dirty

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TSSKnobSettings }

constructor TSSKnobSettings.Create;
begin
  // create a phong shader, will need to delete on clean up

  FPhong := TPhongShading.Create;
  FPhong.LightPositionZ := 100;
  FPhong.LightSourceIntensity := 300;
  FPhong.NegativeDiffusionFactor := 0.8;
  FPhong.AmbientFactor := 0.5;
  FPhong.DiffusionFactor := 0.6;

  FCurveExponent := 0.2;
  FStyle := ssPhong;
  FEdgeColor := clMedGray;
  FFillColor := clWhite;
  FEdgeThickness := 2;
  FDirty := True;
end;

destructor TSSKnobSettings.Destroy;
begin
  FPhong.Free;
  inherited Destroy;
end;

procedure TSSKnobSettings.SetStyle(const AValue: TSSStyle);
begin
  if FStyle = AValue then
    Exit;

  FStyle := AValue;
  DirtyOnChange;
end;

procedure TSSKnobSettings.SetLightIntensity(const AValue: integer);
begin
  if AValue = FPhong.LightSourceIntensity then
    Exit;

  FPhong.LightSourceIntensity := AValue;
  DirtyOnChange;
end;

function TSSKnobSettings.GetLightIntensity: integer;
begin
  Result := round(FPhong.LightSourceIntensity);
end;

procedure TSSKnobSettings.SetCurveExponent(const AValue: single);
begin
  if FCurveExponent = AValue then
    Exit;

  FCurveExponent := AValue;
  DirtyOnChange;
end;

procedure TSSKnobSettings.SetEdgeColor(AValue: TColor);
begin
  if FEdgeColor = AValue then
    Exit;

  FEdgeColor := AValue;
  DirtyOnChange;
end;

procedure TSSKnobSettings.SetEdgeThickness(AValue: integer);
begin
  if (FEdgeThickness = AValue) or (AValue < 0) then
    Exit;

  FEdgeThickness := AValue;
  DirtyOnChange;
end;

procedure TSSKnobSettings.SetFillColor(AValue: TColor);
begin
  if FFillColor = AValue then
    Exit;

  FFillColor := AValue;
  DirtyOnChange;
end;

procedure TSSKnobSettings.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;

  // no dirty needed possibly, call directly

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSSKnobSettings.DirtyOnChange;
begin
  FDirty := True;   // if we get here some props must have changed, mark dirty

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.

