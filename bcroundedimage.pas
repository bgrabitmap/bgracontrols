// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  BCRoundedImage
  by Lainz

  Last modified: 2020-09-06 19:16 GMT-3

  Changelog:
  - 2020-09-06: Initial version supporting circle, rounded rectangle and square.
                Changing the quality of the resample, setting the rounding.
                OnPaintEvent to customize the final drawing.
}
unit BCRoundedImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes;

type
  TBCRoundedImage = class;

  // Event to draw before the image is sent to canvas
  TBCRoundedImagePaintEvent = procedure (const Sender: TBCRoundedImage; const Bitmap: TBGRABitmap) of object;
  // Supported styles are circle, rounded rectangle and square
  TBCRoundedImageStyle = (isCircle, isRoundedRectangle, isSquare);

  // Control that draws an image within a rounded border

  { TBCRoundedImage }

  TBCRoundedImage = class(TGraphicControl)
  private
    FBorderStyle: TRoundRectangleOptions;
    FOnPaintEvent: TBCRoundedImagePaintEvent;
    FPicture: TPicture;
    FQuality: TResampleFilter;
    FStyle: TBCRoundedImageStyle;
    FRounding: single;
    procedure SetBorderStyle(AValue: TRoundRectangleOptions);
    procedure SetPicture(AValue: TPicture);
    procedure SetQuality(AValue: TResampleFilter);
    procedure SetStyle(AValue: TBCRoundedImageStyle);
    procedure SetRounding(AValue: single);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    // The image that's used as background
    property Picture: TPicture read FPicture write SetPicture;
    // The style can be circle, rounded rectangle or square
    property Style: TBCRoundedImageStyle read FStyle write SetStyle;
    // The style of the rounded rectangle
    property BorderStyle: TRoundRectangleOptions read FBorderStyle write SetBorderStyle;
    // Rounding is used when you choose the rounded rectangle style
    property Rounding: single read FRounding write SetRounding;
    // The quality when resizing the image
    property Quality: TResampleFilter read FQuality write SetQuality;
    // You can paint before the bitmap is drawn on canvas
    property OnPaintEvent: TBCRoundedImagePaintEvent read FOnPaintEvent write FOnPaintEvent;
  published
    property Anchors;
    property Align;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnClick;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCRoundedImage]);
end;

procedure TBCRoundedImage.SetPicture(AValue: TPicture);
begin
  if FPicture = AValue then
    Exit;
  FPicture := AValue;
  Invalidate;
end;

procedure TBCRoundedImage.SetBorderStyle(AValue: TRoundRectangleOptions);
begin
  if FBorderStyle=AValue then Exit;
  FBorderStyle:=AValue;
  Invalidate;
end;

procedure TBCRoundedImage.SetQuality(AValue: TResampleFilter);
begin
  if FQuality = AValue then
    Exit;
  FQuality := AValue;
  Invalidate;
end;

procedure TBCRoundedImage.SetStyle(AValue: TBCRoundedImageStyle);
begin
  if FStyle = AValue then
    Exit;
  FStyle := AValue;
  Invalidate;
end;

procedure TBCRoundedImage.SetRounding(AValue: single);
begin
  if FRounding = AValue then
    Exit;
  FRounding := AValue;
  Invalidate;
end;

constructor TBCRoundedImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPicture := TPicture.Create;
  FRounding := 10;
  FQuality := rfBestQuality;
end;

destructor TBCRoundedImage.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

procedure TBCRoundedImage.Paint;
var
  bgra: TBGRABitmap;
  image: TBGRABitmap;
begin
  if (FPicture.Width = 0) or (FPicture.Height = 0) then
    Exit;
  // Picture
  image := TBGRABitmap.Create(FPicture.Bitmap);
  bgra := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
  try
    // Quality
    image.ResampleFilter := FQuality;
    BGRAReplace(image, image.Resample(Width, Height));
    // Style
    case FStyle of
      isCircle: bgra.FillEllipseAntialias(Width div 2, Height div 2,
          Width div 2, Height div 2, image);
      // Rounding, BorderStyle
      isRoundedRectangle: bgra.FillRoundRectAntialias(0, 0, Width,
          Height, FRounding, FRounding, image, FBorderStyle);
      else
        bgra.PutImage(0, 0, image, dmDrawWithTransparency);
    end;
    // OnPaintEvent
    if Assigned(FOnPaintEvent) then
      FOnPaintEvent(Self, bgra);
    bgra.Draw(Canvas, 0, 0, False);
  finally
    bgra.Free;
    image.Free;
  end;
end;

end.
