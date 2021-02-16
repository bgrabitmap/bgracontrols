// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAImageTheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, BGRATheme,
  BGRASliceScaling, BGRABitmap, BGRABitmapTypes, BGRASVGImageList;

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
      Focused: boolean; ARect: TRect; ASurface: TBGRAThemeSurface; AImageIndex: Integer = -1; AImageList: TBGRASVGImageList = nil); override;
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

procedure TBGRAImageTheme.DrawButton(Caption: string;
  State: TBGRAThemeButtonState; Focused: boolean; ARect: TRect;
  ASurface: TBGRAThemeSurface; AImageIndex: Integer;
  AImageList: TBGRASVGImageList);
var
  Style: TTextStyle;
  ImageIndex: integer;
begin
  With ASurface do
  begin
    case State of
      btbsHover: ImageIndex := 1;
      btbsActive: ImageIndex := 2;
      btbsDisabled: ImageIndex := 3;
    else {btbsNormal}
      ImageIndex := 0;
    end;

    Bitmap.Fill(BackgroundColor);

    if Assigned(FSliceScalingButton) then
      FSliceScalingButton.Draw(ImageIndex, Bitmap, 0, 0, Bitmap.Width, Bitmap.Height);

    DrawBitmap;

    if Caption <> '' then
    begin
      fillchar(Style, sizeof(Style), 0);
      Style.Alignment := taCenter;
      Style.Layout := tlCenter;
      Style.Wordbreak := True;
      DestCanvas.TextRect(ARect, 0, 0, Caption, Style);
    end;
  end;
end;

end.
