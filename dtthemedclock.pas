unit dtthemedclock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LResources, Forms, Controls, Graphics, Dialogs, DTAnalogCommon,
  BGRABitmap, BGRABitmapTypes;

type

  { TDTCustomThemedClock }

  TDTCustomThemedClock = class(TDTBaseAnalogDevice)
  private
    FClockFace: TBGRABitmap;
    FPointerBitmap: TBGRABitmap;
    FEnabled: boolean;
    FHoursPointerSettings: TDTPointerSettings;
    FMinutesPointerSettings: TDTPointerSettings;
    FPointerCapSettings: TDTPointerCapSettings;
    FPosition: integer;
    FSecondsPointerSettings: TDTPointerSettings;
    FTimer: TTimer;
    procedure SetEnabled(AValue: boolean);
    procedure SetHoursPointerSettings(AValue: TDTPointerSettings);
    procedure SetMinutesPointerSettings(AValue: TDTPointerSettings);
    procedure SetPointerCapSettings(AValue: TDTPointerCapSettings);
    procedure SetPosition(AValue: integer);
    procedure SetSecondsPointerSettings(AValue: TDTPointerSettings);
    { Private declarations }
  protected
    { Protected declarations }
    property SecondsPointerSettings: TDTPointerSettings read FSecondsPointerSettings write SetSecondsPointerSettings;
    property MinutesPointerSettings: TDTPointerSettings read FMinutesPointerSettings write SetMinutesPointerSettings;
    property HoursPointerSettings: TDTPointerSettings read FHoursPointerSettings write SetHoursPointerSettings;
    property PointerCapSettings: TDTPointerCapSettings read FPointerCapSettings write SetPointerCapSettings;
    property Position: integer read FPosition write SetPosition;
    property Enabled: boolean read FEnabled write SetEnabled;
    procedure TimerEvent(Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure DrawClock;
    procedure DrawPointers;
  end;

  TDTThemedClock = class(TDTCustomThemedClock)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Public declarations }
    property SecondsPointerSettings;
    property MinutesPointerSettings;
    property HoursPointerSettings;
    property PointerCapSettings;
    property ScaleSettings;
    property Position;
    property Enabled;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TDTThemedClock]);
end;

{ TDTCustomThemedClock }

procedure TDTCustomThemedClock.SetPointerCapSettings(AValue: TDTPointerCapSettings);
begin
  if FPointerCapSettings = AValue then
    Exit;
  FPointerCapSettings := AValue;

  DoChange(self);
end;

procedure TDTCustomThemedClock.SetHoursPointerSettings(AValue: TDTPointerSettings);
begin
  if FHoursPointerSettings = AValue then
    Exit;
  FHoursPointerSettings := AValue;

  DoChange(self);
end;

procedure TDTCustomThemedClock.SetEnabled(AValue: boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;

  FTimer.Enabled := FEnabled;

  DoChange(self);
end;

procedure TDTCustomThemedClock.SetMinutesPointerSettings(AValue: TDTPointerSettings);
begin
  if FMinutesPointerSettings = AValue then
    Exit;
  FMinutesPointerSettings := AValue;

  DoChange(self);
end;

procedure TDTCustomThemedClock.SetPosition(AValue: integer);
begin
  if FPosition = AValue then
    Exit;
  FPosition := AValue;

  DoChange(self);
end;

procedure TDTCustomThemedClock.SetSecondsPointerSettings(AValue: TDTPointerSettings);
begin
  if FSecondsPointerSettings = AValue then
    Exit;
  FSecondsPointerSettings := AValue;

  DoChange(self);
end;

procedure TDTCustomThemedClock.TimerEvent(Sender: TObject);
begin
  DrawPointers;
  DoChange(self);
end;

constructor TDTCustomThemedClock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSecondsPointerSettings := TDTPointerSettings.Create;
  FSecondsPointerSettings.OnChange := @DoChange;
  FMinutesPointerSettings := TDTPointerSettings.Create;
  FMinutesPointerSettings.OnChange := @DoChange;
  FHoursPointerSettings := TDTPointerSettings.Create;
  FHoursPointerSettings.OnChange := @DoChange;

  FPointerCapSettings := TDTPointerCapSettings.Create;
  FPointerCapSettings.OnChange := @DoChange;

  FClockFace := TBGRABitmap.Create;
  FPointerBitmap := TBGRABitmap.Create;

  FSecondsPointerSettings.Color := BGRAToColor(BGRA(255, 81, 81));
  FSecondsPointerSettings.Length := 80;
  FSecondsPointerSettings.Thickness := 3;

  FMinutesPointerSettings.Color := BGRAToColor(BGRA(199, 199, 173));
  FMinutesPointerSettings.Length := 80;
  FMinutesPointerSettings.Thickness := 3;

  FHoursPointerSettings.Color := BGRAToColor(BGRA(199, 199, 173));
  FHoursPointerSettings.Length := 60;
  FHoursPointerSettings.Thickness := 5;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 1000;
  FTimer.Enabled := FEnabled;
  FTimer.OnTimer := @TimerEvent;

end;

destructor TDTCustomThemedClock.Destroy;
begin
  FSecondsPointerSettings.OnChange:=nil;
  FSecondsPointerSettings.Free;
  FMinutesPointerSettings.OnChange:=nil;
  FMinutesPointerSettings.Free;
  FHoursPointerSettings.OnChange:=nil;
  FHoursPointerSettings.Free;
  FTimer.Enabled:=False;
  FTimer.OnTimer:=nil;
  FPointerCapSettings.OnChange:=nil;
  FPointerCapSettings.Free;

  FClockFace.Free;
  FPointerBitmap.Free;

  inherited Destroy;
end;

procedure TDTCustomThemedClock.Paint;
begin
  inherited Paint;

  DrawClock;
  DrawPointers;

  FGaugeBitmap.BlendImage(0, 0, FClockFace, boLinearBlend);
  FGaugeBitmap.BlendImage(0, 0, FPointerBitmap, boLinearBlend);

  FGaugeBitmap.Draw(Canvas, 0, 0, False);
end;

procedure TDTCustomThemedClock.DrawClock;
var
  Origin: TDTOrigin;
  r, i, x, y, xt, yt: integer;
begin

  Origin := Initializebitmap(FClockFace, Width, Height);

  r := round(Origin.Radius * 0.85);

  // Draw minor tick marks
  if ScaleSettings.EnableSubTicks then
  begin
    for i := 1 to 60 do
    begin
      // Calculate draw from point
      X := Origin.CenterPoint.x + Round(r * sin(6 * i * Pi / 180));
      Y := Origin.CenterPoint.y - Round(r * cos(6 * i * Pi / 180));

      // Calculate draw to point
      xt := Origin.CenterPoint.x + Round((r - ScaleSettings.LengthSubTick) * sin(6 * i * Pi / 180));
      yt := Origin.CenterPoint.y - Round((r - ScaleSettings.LengthSubTick) * cos(6 * i * Pi / 180));

      FClockFace.DrawLineAntialias(x, y, xt, yt, ColorToBGRA(ScaleSettings.TickColor), ScaleSettings.ThicknessSubTick);
    end;
  end;

  // Draw major tick marks
  if ScaleSettings.EnableMainTicks then
  begin
    for i := 1 to 12 do
    begin
      // Calculate draw from point
      X := Origin.CenterPoint.x + Round(r * sin(30 * i * Pi / 180));
      Y := Origin.CenterPoint.y - Round(r * cos(30 * i * Pi / 180));

      // Calculate draw to point
      xt := Origin.CenterPoint.x + Round((r - ScaleSettings.LengthMainTick) * sin(30 * i * Pi / 180));
      yt := Origin.CenterPoint.y - Round((r - ScaleSettings.LengthMainTick) * cos(30 * i * Pi / 180));

      FClockFace.DrawLineAntialias(x, y, xt, yt, ColorToBGRA(ScaleSettings.TickColor), ScaleSettings.ThicknessMainTick);

      if ScaleSettings.EnableScaleText then
      begin
        FClockFace.FontName := ScaleSettings.TextFont;
        FClockFace.FontHeight := ScaleSettings.TextSize;
        FClockFace.FontQuality := fqFineAntialiasing;

        // Draw text for main ticks
        xt := Origin.CenterPoint.x + Round(ScaleSettings.TextRadius * sin(30 * i * Pi / 180));
        yt := Origin.CenterPoint.y - Round(ScaleSettings.TextRadius * cos(30 * i * Pi / 180));

        FClockFace.TextOut(Xt, Yt - (FClockFace.FontHeight / 1.7), IntToStr(i), ColorToBGRA(ScaleSettings.TextColor), taCenter);
      end;

    end;
  end;

end;

procedure TDTCustomThemedClock.DrawPointers;
var
  Origin: TDTOrigin;
  r, i, x, y, xt, yt: integer;
  Xs, Ys, Xm, Ym, Xh, Yh: integer;
  th, tm, ts, tn: word;
begin

  Origin := Initializebitmap(FPointerBitmap, Width, Height);

  r := round(Origin.Radius * 0.85);

  //// Convert current time to integer values
  decodetime(Time, th, tm, ts, tn);

  //{ Set coordinates (length of arm) for seconds }
  Xs := Origin.CenterPoint.x + Round(SecondsPointerSettings.Length * Sin(ts * 6 * Pi / 180));
  Ys := Origin.CenterPoint.y - Round(SecondsPointerSettings.Length * Cos(ts * 6 * Pi / 180));

  //{ Set coordinates (length of arm) for minutes }
  Xm := Origin.CenterPoint.x + Round(MinutesPointerSettings.Length * Sin(tm * 6 * Pi / 180));
  Ym := Origin.CenterPoint.y - Round(MinutesPointerSettings.Length * Cos(tm * 6 * Pi / 180));

  //{ Set coordinates (length of arm) for hours }
  Xh := Origin.CenterPoint.x + Round(HoursPointerSettings.Length * Sin((th * 30 + tm / 2) * Pi / 180));
  Yh := Origin.CenterPoint.y - Round(HoursPointerSettings.Length * Cos((th * 30 + tm / 2) * Pi / 180));

  FPointerBitmap.DrawLineAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y, xs, ys, ColorToBGRA(SecondsPointerSettings.Color), SecondsPointerSettings.Thickness);
  FPointerBitmap.DrawLineAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y, xm, ym, ColorToBGRA(MinutesPointerSettings.Color), MinutesPointerSettings.Thickness);
  FPointerBitmap.DrawLineAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y, xh, yh, ColorToBGRA(HoursPointerSettings.Color), HoursPointerSettings.Thickness);

  // Draw cap over needle
  FPointerBitmap.EllipseAntialias(origin.CenterPoint.x, origin.CenterPoint.y, PointerCapSettings.Radius, PointerCapSettings.Radius, ColorToBGRA(PointerCapSettings.EdgeColor), 2, ColorToBGRA(PointerCapSettings.FillColor));


end;

end.
