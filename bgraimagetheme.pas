unit BGRAImageTheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, BGRATheme,
  BGRASliceScaling, BGRABitmap, BGRABitmapTypes;

type

  { TBGRAImageTheme }

  TBGRAImageTheme = class(TBGRATheme)
  private
    FBackgroundColor: TColor;
    FSliceScalingButton: TBGRAMultiSliceScaling;
    procedure SetFBackgroundColor(AValue: TColor);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadResources(AFileName: string);
    procedure DrawButton(Caption: string; State: TBGRAThemeButtonState;
      {%H-}Focused: boolean; ARect: TRect; DestCanvas: TCanvas); override;
  published
    property BackgroundColor: TColor read FBackgroundColor
      write SetFBackgroundColor default clForm;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Themes', [TBGRAImageTheme]);
end;

{ TBGRAImageTheme }

procedure TBGRAImageTheme.SetFBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor = AValue then
    Exit;
  FBackgroundColor := AValue;
end;

constructor TBGRAImageTheme.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BackgroundColor := clForm;
end;

destructor TBGRAImageTheme.Destroy;
begin
  FSliceScalingButton.Free;
  inherited Destroy;
end;

procedure TBGRAImageTheme.LoadResources(AFileName: string);
begin
  FreeAndNil(FSliceScalingButton);
  FSliceScalingButton := TBGRAMultiSliceScaling.Create(AFileName, 'Button');
end;

procedure TBGRAImageTheme.DrawButton(Caption: string; State: TBGRAThemeButtonState;
  Focused: boolean; ARect: TRect; DestCanvas: TCanvas);
var
  Style: TTextStyle;
  ImageIndex: integer;
  bmp: TBGRABitmap;
begin
  case State of
    btbsHover: ImageIndex := 1;
    btbsActive: ImageIndex := 2;
    btbsDisabled: ImageIndex := 3;
  else {btbsNormal}
    ImageIndex := 0;
  end;

  bmp := TBGRABitmap.Create(ARect.Width, ARect.Height, BackgroundColor);

  if Assigned(FSliceScalingButton) then
    FSliceScalingButton.Draw(ImageIndex, bmp, 0, 0, bmp.Width, bmp.Height);

  bmp.Draw(DestCanvas, ARect);
  bmp.Free;

  if Caption <> '' then
  begin
    Style.Alignment := taCenter;
    Style.Layout := tlCenter;
    Style.Wordbreak := True;
    Style.SystemFont := False;
    Style.Clipping := True;
    Style.Opaque := False;
    DestCanvas.TextRect(ARect, 0, 0, Caption, Style);
  end;
end;

end.
