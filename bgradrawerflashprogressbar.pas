unit BGRADrawerFlashProgressBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, {$IFDEF BGRABITMAP_USE_MSEGUI} mclasses, {$ENDIF} SysUtils, Types, BGRABitmap, BGRABitmapTypes, BGRAGraphics, BGRAGradients,
  Math;

type

  TBGRAProgressBarRedrawEvent = procedure(Sender: TObject; Bitmap: TBGRABitmap; xpos: integer) of object;

  { TBGRADrawerFlashProgressBar }

  TBGRADrawerFlashProgressBar = class(TPersistent)
  private
    FBackgroundColor: TColor;
    FBackgroundRandomize: boolean;
    FBackgroundRandomizeMaxIntensity: word;
    FBackgroundRandomizeMinIntensity: word;
    FBarColor: TColor;
    FMaxValue: integer;
    FMinValue: integer;
    FOnChange: TNotifyEvent;
    FRandSeed: integer;
    FValue: integer;
    xpos: integer;
    procedure SetBackgroundRandomize(AValue: boolean);
    procedure SetBackgroundRandomizeMaxIntensity(AValue: word);
    procedure SetBackgroundRandomizeMinIntensity(AValue: word);
    procedure SetBarColor(AValue: TColor);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetMaxValue(AValue: integer);
    procedure SetMinValue(AValue: integer);
    procedure SetRandSeed(AValue: integer);
    procedure SetValue(AValue: integer);
  public
    procedure Draw(ABitmap: TBGRABitmap);
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property RandSeed: integer read FRandSeed write SetRandSeed;
    property BarColor: TColor read FBarColor write SetBarColor;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property BackgroundRandomizeMinIntensity: word
      read FBackgroundRandomizeMinIntensity write SetBackgroundRandomizeMinIntensity;
    property BackgroundRandomizeMaxIntensity: word
      read FBackgroundRandomizeMaxIntensity write SetBackgroundRandomizeMaxIntensity;
    property BackgroundRandomize: boolean read FBackgroundRandomize
      write SetBackgroundRandomize;
    property XPosition: integer read xpos;
  public
    property MinValue: integer read FMinValue write SetMinValue;
    property MaxValue: integer read FMaxValue write SetMaxValue;
    property Value: integer read FValue write SetValue;
  end;

implementation

{ TBGRADrawerFlashProgressBar }

procedure TBGRADrawerFlashProgressBar.SetBarColor(AValue: TColor);
begin
  if FBarColor = AValue then
    Exit;
  FBarColor := AValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBGRADrawerFlashProgressBar.SetBackgroundRandomize(AValue: boolean);
begin
  if FBackgroundRandomize = AValue then
    Exit;
  FBackgroundRandomize := AValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBGRADrawerFlashProgressBar.SetBackgroundRandomizeMaxIntensity(AValue: word);
begin
  if FBackgroundRandomizeMaxIntensity = AValue then
    Exit;
  FBackgroundRandomizeMaxIntensity := AValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBGRADrawerFlashProgressBar.SetBackgroundRandomizeMinIntensity(AValue: word);
begin
  if FBackgroundRandomizeMinIntensity = AValue then
    Exit;
  FBackgroundRandomizeMinIntensity := AValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBGRADrawerFlashProgressBar.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor = AValue then
    Exit;
  FBackgroundColor := AValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBGRADrawerFlashProgressBar.SetMaxValue(AValue: integer);
begin
  if FMaxValue = AValue then
    exit;
  FMaxValue := AValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  if FMinValue > FMaxValue then
    FMinValue := FMaxValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBGRADrawerFlashProgressBar.SetMinValue(AValue: integer);
begin
  if FMinValue = AValue then
    exit;
  FMinValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FMaxValue < FMinValue then
    FMaxValue := FMinValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBGRADrawerFlashProgressBar.SetRandSeed(AValue: integer);
begin
  if FRandSeed = AValue then
    Exit;
  FRandSeed := AValue;
end;

procedure TBGRADrawerFlashProgressBar.SetValue(AValue: integer);
begin
  if FValue = AValue then
    exit;
  FValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBGRADrawerFlashProgressBar.Draw(ABitmap: TBGRABitmap);
var
  content: TRect;
  y, tx, ty: integer;
  bgColor: TBGRAPixel;

  function ApplyLightness(c: TBGRAPixel; lightness: word): TBGRAPixel;
  begin
    Result := GammaCompression(SetLightness(GammaExpansion(c), lightness));
  end;

  procedure DrawBar(bounds: TRect);
  var
    lCol: TBGRAPixel;
  begin
    lCol := BarColor;

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
    if FMaxValue > FMinValue then
    begin
      xpos := round((FValue - FMinValue) / (FMaxValue - FMinValue) *
        (content.right - content.left)) + content.left;
      if xpos > content.left then
      begin
        DrawBar(rect(content.left, content.top, xpos, content.bottom));
        if xpos < content.right then
        begin
          ABitmap.SetPixel(xpos, content.top, BGRA(62, 62, 62));
          ABitmap.SetVertLine(xpos, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
        end;
      end;
    end;
  end;
end;

end.
