// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  BCRoundedImage
  by Lainz

  Last modified: 2020-09-06 19:16 GMT-3

  Changelog:
  - 2020-09-06: Initial version supporting circle, rounded rectangle and square.
                Changing the quality of the resample, setting the rounding.
                OnPaintEvent to customize the final drawing.
  - 2025-01: MaxM, Changed class ancestor to TCustomBGRAGraphicControl;
                   Added TBGRABitmap Bitmap draw;
                   Added Stretch, Proportional, Alignments.
}
unit BCRoundedImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRAGraphicControl, BCTypes;

type
  TBCRoundedImage = class;

  // Event to draw before the image is sent to canvas
  //TBCRoundedImagePaintEvent = procedure (const Sender: TBCRoundedImage; const Bitmap: TBGRABitmap) of object;
  TBCRoundedImagePaintEvent = TBGRARedrawEvent;

  // Supported styles are circle, rounded rectangle and square
  TBCRoundedImageStyle = (isCircle, isRoundedRectangle, isSquare);

  // Control that draws an image within a rounded border

  { TBCRoundedImage }

  TBCRoundedImage = class(TCustomBGRAGraphicControl)
  private
    FBorderStyle: TRoundRectangleOptions;
    FOnPaintEvent: TBCRoundedImagePaintEvent;
    FPicture: TPicture;
    FImageBitmap: TBGRABitmap;
    FQuality: TResampleFilter;
    FStyle: TBCRoundedImageStyle;
    FRounding: single;
    FProportional: Boolean;
    FOnChange: TNotifyEvent;
    FAlignment: TAlignment;
    FStretch: Boolean;
    FVerticalAlignment: TTextLayout;

    function GetOnPaintEvent: TBCRoundedImagePaintEvent;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetBitmap(AValue: TBGRABitmap);
    procedure SetBorderStyle(AValue: TRoundRectangleOptions);
    procedure SetOnPaintEvent(AValue: TBCRoundedImagePaintEvent);
    procedure SetPicture(AValue: TPicture);
    procedure SetProportional(AValue: Boolean);
    procedure SetQuality(AValue: TResampleFilter);
    procedure SetStretch(AValue: Boolean);
    procedure SetStyle(AValue: TBCRoundedImageStyle);
    procedure SetRounding(AValue: single);
    procedure SetVerticalAlignment(AValue: TTextLayout);

  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    procedure Draw(ABitmap: TBGRABitmap);

    property Bitmap: TBGRABitmap read FImageBitmap write setBitmap;

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
    // Stretch Proportianally
    property Proportional: Boolean read FProportional write SetProportional;
    property Stretch: Boolean read FStretch write SetStretch default True;

    // Alignments of the Image inside the Control
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property VerticalAlignment: TTextLayout read FVerticalAlignment write SetVerticalAlignment default tlCenter;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    // You can paint before the bitmap is drawn on canvas
    property OnPaintEvent: TBCRoundedImagePaintEvent read GetOnPaintEvent write SetOnPaintEvent; deprecated 'Use OnRedraw instead';

    property Anchors;
    property Align;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnClick;
  end;


{ #todo -oMaxM : we could move it to a common unit and use it in BGRAImageList too }
function CalcProportionalRect(AWidth, AHeight, AImageWidth, AImageHeight: Integer;
                               AHorizAlign: TAlignment; AVertAlign: TTextLayout): TRect;


procedure Register;

implementation

function CalcProportionalRect(AWidth, AHeight, AImageWidth, AImageHeight: Integer; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout): TRect;
var
  rW, rH:Single;
  newWidth,
  newHeight:Integer;

begin
  FillChar(Result, sizeof(Result), 0);
  if (AImageWidth > 0) and (AImageHeight > 0) then
  begin
    rW := AImageWidth / AWidth;
    rH := AImageHeight / AHeight;

    if (rW > rH)
    then begin
           newHeight:= round(AImageHeight / rW);
           newWidth := AWidth;
           end
    else begin
           newWidth := round(AImageWidth / rH);
           newHeight := AHeight;
         end;

    case AHorizAlign of
    taCenter: Result.Left:= (AWidth-newWidth) div 2;
    taRightJustify: Result.Left:= AWidth-newWidth;
    end;
    case AVertAlign of
    tlCenter: Result.Top:= (AHeight-newHeight) div 2;
    tlBottom: Result.Top:= AHeight-newHeight;
    end;

    Result.Right:= Result.Left+newWidth;
    Result.Bottom:= Result.Top+newHeight;
  end;
end;

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCRoundedImage]);
end;

procedure TBCRoundedImage.SetProportional(AValue: Boolean);
begin
  if FProportional=AValue then Exit;
  FProportional:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBCRoundedImage.SetBorderStyle(AValue: TRoundRectangleOptions);
begin
  if FBorderStyle=AValue then Exit;
  FBorderStyle:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

function TBCRoundedImage.GetOnPaintEvent: TBCRoundedImagePaintEvent;
begin
  Result:= OnRedraw;
end;

procedure TBCRoundedImage.SetAlignment(AValue: TAlignment);
begin
  if FAlignment=AValue then Exit;
  FAlignment:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBCRoundedImage.SetBitmap(AValue: TBGRABitmap);
begin
  if (AValue <> FImageBitmap) then
  begin
    // Clear actual image
    FImageBitmap.Free;

    FImageBitmap :=TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);

    if (AValue<>nil) then FImageBitmap.Assign(AValue, True); // Associate the new bitmap

    if Assigned(FOnChange) then FOnChange(Self);
    Invalidate;
  end;
end;

procedure TBCRoundedImage.SetPicture(AValue: TPicture);
begin
  if (AValue <> FPicture) then
  begin
    // Clear actual Picture
    FPicture.Free;

    FPicture :=TPicture.Create;

    if (AValue<>nil) then FPicture.Assign(AValue); // Associate the new Picture

    if Assigned(FOnChange) then FOnChange(Self);
    Invalidate;
  end;
end;

procedure TBCRoundedImage.SetOnPaintEvent(AValue: TBCRoundedImagePaintEvent);
begin
  OnRedraw:= AValue;
end;

procedure TBCRoundedImage.SetQuality(AValue: TResampleFilter);
begin
  if FQuality = AValue then
    Exit;
  FQuality := AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBCRoundedImage.SetStretch(AValue: Boolean);
begin
  if FStretch=AValue then Exit;
  FStretch:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBCRoundedImage.SetStyle(AValue: TBCRoundedImageStyle);
begin
  if FStyle = AValue then
    Exit;
  FStyle := AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBCRoundedImage.SetRounding(AValue: single);
begin
  if FRounding = AValue then
    Exit;
  FRounding := AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TBCRoundedImage.SetVerticalAlignment(AValue: TTextLayout);
begin
  if FVerticalAlignment=AValue then Exit;
  FVerticalAlignment:=AValue;

  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

{$hints off}
procedure TBCRoundedImage.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
begin
  PreferredWidth  := 100;
  PreferredHeight := 100;
end;

constructor TBCRoundedImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAlignment:= taCenter;
  FVerticalAlignment:= tlCenter;
  FStretch:= True;

  // Create the Image Bitmap
  FPicture := TPicture.Create;
  FImageBitmap := TBGRABitmap.Create;

  FRounding := 10;
  FQuality := rfBestQuality;
  FBGRA.FillTransparent;
end;

destructor TBCRoundedImage.Destroy;
begin
  FPicture.Free;
  FImageBitmap.Free;

  inherited Destroy;
end;

procedure TBCRoundedImage.Paint;
begin
  if (ClientWidth <> FBGRA.Width) or (ClientHeight <> FBGRA.Height)
  then FBGRA.SetSize(ClientWidth, ClientHeight);

  Draw(FBGRA);

  if Assigned(OnRedraw) then OnRedraw(Self, FBGRA);

  FBGRA.Draw(Canvas, 0, 0, False);
end;

procedure TBCRoundedImage.Draw(ABitmap: TBGRABitmap);
var
  image,
  imageD: TBGRABitmap;
  imgRect: TRect;

begin
  ABitmap.FillTransparent;

  if ((FPicture.Width = 0) or (FPicture.Height = 0)) and
      FImageBitmap.Empty then exit;

  try
    if FImageBitmap.Empty
    then image := TBGRABitmap.Create(FPicture.Bitmap)
    else image := TBGRABitmap.Create(FImageBitmap.Bitmap);

    imageD:= TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);

    if FProportional
    then imgRect:= CalcProportionalRect(Width, Height, image.Width, image.Height,
                                        FAlignment, FVerticalAlignment)
    else begin
           if FStretch
           then imgRect:= Rect(0,0,Width,Height)
           else begin
                  case FAlignment of
                  taLeftJustify: imgRect.Left:= 0;
                  taCenter: imgRect.Left:= (Width-image.Width) div 2;
                  taRightJustify: imgRect.Left:= Width-image.Width;
                  end;
                  case FVerticalAlignment of
                  tlTop: imgRect.Top:= 0;
                  tlCenter: imgRect.Top:= (Height-image.Height) div 2;
                  tlBottom: imgRect.Top:= Height-image.Height;
                  end;

                  imgRect.Right:= imgRect.Left+image.Width;
                  imgRect.Bottom:= imgRect.Top+image.Height;
                end;
         end;

    if FStretch or FProportional then
    begin
      // Stretch with Quality
      image.ResampleFilter := FQuality;
      BGRAReplace(image, image.Resample(imgRect.Width, imgRect.Height));
    end;

    imageD.PutImage(imgRect.Left, imgRect.Top, image, dmDrawWithTransparency);

    // Style
    case FStyle of
    isCircle: ABitmap.FillEllipseAntialias(Width div 2, Height div 2,
                          (Width div 2)-FRounding, (Height div 2)-FRounding, imageD);
    isRoundedRectangle: ABitmap.FillRoundRectAntialias(0, 0, Width,
                                    Height, FRounding, FRounding, imageD, FBorderStyle);
    else ABitmap.PutImage(0, 0, imageD, dmDrawWithTransparency);
    end;

  finally
    imageD.Free;
    image.Free;
  end;
end;

end.
