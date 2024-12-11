// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ This component partialy solve problem with no alpha in lazarus GTK.
  It is using BGRABitmap library for drawing icons.

  originally written in 2011 by Krzysztof Dibowski dibowski at interia.pl
}
(******************************** CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)   { #note -oMaxM : VCL Compatibility? }

- Massimo Magnano 2024/12
  Added Before/AfterDraw events (don't works with Widgetsets)
  Added UseBGRADraw             ( " )
  Added Proportionally add methods

***************************** END CONTRIBUTOR(S) *****************************)
unit BGRAImageList;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources, {$ENDIF} Controls, Graphics,
  GraphType, BGRABitmap, BGRABitmapTypes, {%H-}ImgList;

{$IFDEF LCLgtk}
  { $DEFINE BGRA_DRAW}
{$ELSE}
  {$IFDEF LCLgtk2}
    { $DEFINE BGRA_DRAW}
  {$ENDIF}
{$ENDIF}

type
  { TBGRAImageListResolution }

  TBGRAImageListResolution = class(TDragImageListResolution)
  public
    procedure BGRADraw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AOverlay: TOverlay;
      ADrawingStyle: TDrawingStyle; AImageType: TImageType;
      ADrawEffect: TGraphicsDrawEffect; ABkColor, ABlendColor: TColor; AStretch: Boolean=False); virtual;

    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: integer; ADrawingStyle: TDrawingStyle; AImageType: TImageType;
      ADrawEffect: TGraphicsDrawEffect); override;

    procedure DrawOverlay(ACanvas: TCanvas; AX, AY, AIndex: Integer; AOverlay: TOverlay; ADrawingStyle:
      TDrawingStyle; AImageType: TImageType; ADrawEffect: TGraphicsDrawEffect); overload;

    procedure StretchDrawOverlay(ACanvas: TCanvas; AIndex: Integer; ARect: TRect; AOverlay: TOverlay;
      AEnabled: Boolean = True); virtual;
  end;

  { TBGRAImageList }
  TBGRAImageList = class;

  //Return True whether the default draw should be called
  TCustomImageListBeforeDraw= function (Sender: TBGRAImageList;
    ACanvas: TCanvas; var ARect: TRect; var AIndex: Integer;
    var ADrawingStyle: TDrawingStyle; var AImageType: TImageType;
    var ADrawOverlay: Boolean; var AOverlay: TOverlay;
    var ADrawEffect: TGraphicsDrawEffect): Boolean of object;

  TCustomImageListAfterDraw= procedure (Sender: TBGRAImageList;
    ACanvas: TCanvas; ARect: TRect; AIndex: Integer;
    ADrawingStyle: TDrawingStyle; AImageType: TImageType;
    ADrawOverlay: Boolean; AOverlay: TOverlay;
    ADrawEffect: TGraphicsDrawEffect) of object;

  TOverlaysArray = array[TOverlay] of Integer;

  TBGRAImageList = class(TImageList)
  private
    rUseBGRADraw: Boolean;
    FBGRA: TBGRABitmap;
    FBmp:  TBitmap;

    procedure SetUseBGRADraw(AValue: Boolean);

  protected
    FOnBeforeDraw: TCustomImageListBeforeDraw;
    FOnAfterDraw: TCustomImageListAfterDraw;

    { #note -oMaxM : we keep our copy of the FOverlays array since it is declared private without any logic,
                     so derived classes cannot use it in any way also because there is no property to read them}
    rOverlays: TOverlaysArray;

    function GetResolution(AImageWidth: Integer): TBGRAImageListResolution;

    function CreateProportionalImage(AImage: TCustomBitmap; AHorizAlign: TAlignment; AVertAlign: TTextLayout): TBitmap;
    function CreateMaskImage(AImage: TCustomBitmap; MaskColor: TColor): TBitmap;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetResolutionClass: TCustomImageListResolutionClass; override;

    procedure Overlay(AIndex: Integer; AOverlay: TOverlay);

    procedure StretchDrawOverlay(ACanvas: TCanvas; AIndex: Integer; ARect: TRect; AOverlay: TOverlay; AEnabled: Boolean = True);

    function AddProportionally(Image: TCustomBitmap; Mask: TCustomBitmap=nil;
                               AHorizAlign: TAlignment=taCenter; AVertAlign: TTextLayout=tlCenter): Integer;
    function AddMaskedProportionally(Image: TBitmap; MaskColor: TColor;
                               AHorizAlign: TAlignment=taCenter; AVertAlign: TTextLayout=tlCenter): Integer;
    procedure InsertProportionally(AIndex: Integer; AImage: TCustomBitmap; AMask: TCustomBitmap=nil;
                               AHorizAlign: TAlignment=taCenter; AVertAlign: TTextLayout=tlCenter);
    procedure InsertMaskedProportionally(Index: Integer; AImage: TCustomBitmap; MaskColor: TColor;
                               AHorizAlign: TAlignment=taCenter; AVertAlign: TTextLayout=tlCenter);
    procedure ReplaceProportionally(AIndex: Integer; AImage: TCustomBitmap; AMask: TCustomBitmap=nil; const AllResolutions: Boolean = True;
                               AHorizAlign: TAlignment=taCenter; AVertAlign: TTextLayout=tlCenter);
    procedure ReplaceMaskedProportionally(Index: Integer; AImage: TCustomBitmap; MaskColor: TColor; const AllResolutions: Boolean = True;
                               AHorizAlign: TAlignment=taCenter; AVertAlign: TTextLayout=tlCenter);

    property Overlays: TOverlaysArray read rOverlays;

  published
    property UseBGRADraw: Boolean read rUseBGRADraw write SetUseBGRADraw;

    { #note -oMaxM : This Events don't works inside Widgetsets that use the imagelist handle
                     but only if you draw directly on the Canvas using ImageList Draw methods }
    property OnBeforeDraw: TCustomImageListBeforeDraw read FOnBeforeDraw write FOnBeforeDraw;
    property OnAfterDraw: TCustomImageListAfterDraw read FOnAfterDraw write FOnAfterDraw;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

const
  EffectMap: array[Boolean] of TGraphicsDrawEffect = (
    gdeDisabled,
    gdeNormal
  );

{$IFDEF FPC}
procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBGRAImageList]);
end;
{$ENDIF}

{ TBGRAImageListResolution }

{ Problem with no alpha is only on GTK so on Windows we use default drawing }
procedure TBGRAImageListResolution.BGRADraw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AOverlay: TOverlay;
                           ADrawingStyle: TDrawingStyle; AImageType: TImageType;
                           ADrawEffect: TGraphicsDrawEffect; ABkColor, ABlendColor: TColor; AStretch: Boolean);
var
   bmpDrawEffect: TGraphicsDrawEffect;
   OverlayI: Integer;

begin
  if (AIndex < 0) or (AIndex >= Count) then Exit;
  ReferenceNeeded;

  with TBGRAImageList(ImageList) do
  begin
    if (FBGRA = nil) then FBGRA:= TBGRABitmap.Create;
    if (FBmp = nil) then FBmp:= TBitmap.Create;

    {*** BGRA Drawing *** }
    if (ADrawEffect = gdeDisabled)
    then bmpDrawEffect:= gdeNormal
    else bmpDrawEffect:= ADrawEffect;

    {$IFDEF FPC}
      GetBitmap(AIndex, FBmp, bmpDrawEffect);
    {$ELSE}
      GetBitmapRaw(AIndex, FBmp, bmpDrawEffect);
    {$ENDIF}
    FBGRA.Assign(FBmp);

    if (AOverlay > 0) then
    begin
      OverlayI := rOverlays[AOverlay];
      if (OverlayI in [0..Count-1]) then
      begin
       {$IFDEF FPC}
         GetBitmap(OverlayI, FBmp, bmpDrawEffect);
       {$ELSE}
         GetBitmapRaw(OverlayI, FBmp, bmpDrawEffect);
       {$ENDIF}
       FBmp.Mask(ImageList.BkColor);

       FBGRA.PutImage(0, 0, FBmp, dmLinearBlend);
      end;
    end;

    if (ADrawEffect = gdeDisabled) then BGRAReplace(FBGRA, FBGRA.FilterGrayscale);

    if (ADrawingStyle in [dsFocus, dsSelected]) then FBGRA.ApplyGlobalOpacity(128);

    if AStretch
    then FBGRA.Draw(ACanvas, ARect, (ABkColor <> clNone))
    else FBGRA.Draw(ACanvas, ARect.Left, ARect.Top, (ABkColor <> clNone));
  end;
end;

procedure TBGRAImageListResolution.Draw(ACanvas: TCanvas; AX, AY, AIndex: integer; ADrawingStyle: TDrawingStyle;
  AImageType: TImageType; ADrawEffect: TGraphicsDrawEffect);
var
   vRect: TRect;
   vIndex: Integer;
   vDrawingStyle: TDrawingStyle;
   vImageType: TImageType;
   vDrawOverlay,
   stdDraw: Boolean;
   vOverlay: TOverlay;
   vDrawEffect: TGraphicsDrawEffect;
   rImageList: TBGRAImageList;

begin
  if (AIndex < 0) or (AIndex >= Count) then Exit;
  ReferenceNeeded;

  rImageList:= TBGRAImageList(ImageList);

  //Copy Parameters to vars
  vRect:= Rect(AX, AY, Width, Height);
  vIndex:= AIndex;
  vDrawingStyle:= ADrawingStyle;
  vImageType:= AImageType;
  vDrawOverlay:= False;
  vOverlay:= 0;
  vDrawEffect:= ADrawEffect;

  stdDraw:= True;
  if Assigned(rImageList.FOnBeforeDraw)
  then stdDraw:= rImageList.FOnBeforeDraw(rImageList, ACanvas, vRect, vIndex, vDrawingStyle, vImageType,
                                          vDrawOverlay, vOverlay, vDrawEffect);

  if stdDraw then
  begin
    if not(vDrawOverlay) then vOverlay:= 0;

    if rImageList.rUseBGRADraw
    then BGRADraw(ACanvas, vRect, vIndex, vOverlay, vDrawingStyle, vImageType, vDrawEffect,
                    rImageList.BkColor, rImageList.BlendColor)
    else begin
           if vDrawOverlay
           then inherited DrawOverlay(ACanvas, vRect.Left, vRect.Top, vIndex, vOverlay, vDrawingStyle, vImageType, vDrawEffect)
           else inherited Draw(ACanvas, vRect.Left, vRect.Top, vIndex, vDrawingStyle, vImageType, vDrawEffect);
         end;
  end;

  if Assigned(rImageList.FOnAfterDraw)
  then rImageList.FOnAfterDraw(rImageList, ACanvas, vRect, vIndex, vDrawingStyle, vImageType,
                               vDrawOverlay, vOverlay, vDrawEffect);
end;

procedure TBGRAImageListResolution.DrawOverlay(ACanvas: TCanvas; AX, AY, AIndex: Integer; AOverlay: TOverlay;
  ADrawingStyle: TDrawingStyle; AImageType: TImageType; ADrawEffect: TGraphicsDrawEffect);
var
   vRect: TRect;
   vIndex: Integer;
   vDrawingStyle: TDrawingStyle;
   vImageType: TImageType;
   vDrawOverlay,
   stdDraw: Boolean;
   vOverlay: TOverlay;
   vDrawEffect: TGraphicsDrawEffect;
   rImageList: TBGRAImageList;

begin
  if (AIndex < 0) or (AIndex >= Count) then Exit;
  ReferenceNeeded;

  rImageList:= TBGRAImageList(ImageList);

  //Copy Parameters to vars
  vRect:= Rect(AX, AY, Width, Height);
  vIndex:= AIndex;
  vDrawingStyle:= ADrawingStyle;
  vImageType:= AImageType;
  vDrawOverlay:= True;
  vOverlay:= AOverlay;
  vDrawEffect:= ADrawEffect;

  stdDraw:= True;
  if Assigned(rImageList.FOnBeforeDraw)
  then stdDraw:= rImageList.FOnBeforeDraw(rImageList, ACanvas, vRect, vIndex, vDrawingStyle, vImageType,
                                          vDrawOverlay, vOverlay, vDrawEffect);

  if stdDraw then
  begin
    if not(vDrawOverlay) then vOverlay:= 0;

    if rImageList.rUseBGRADraw
    then BGRADraw(ACanvas, vRect, vIndex, vOverlay, vDrawingStyle, vImageType, vDrawEffect,
                  rImageList.BkColor, rImageList.BlendColor)
    else begin
           if vDrawOverlay
           then inherited DrawOverlay(ACanvas, vRect.Left, vRect.Top, vIndex, vOverlay, vDrawingStyle, vImageType, vDrawEffect)
           else inherited Draw(ACanvas, vRect.Left, vRect.Top, vIndex, vDrawingStyle, vImageType, vDrawEffect);
         end;
  end;

  if Assigned(TBGRAImageList(ImageList).FOnAfterDraw)
  then TBGRAImageList(ImageList).FOnAfterDraw(TBGRAImageList(ImageList), ACanvas, vRect, vIndex, vDrawingStyle, vImageType,
                               vDrawOverlay, vOverlay, vDrawEffect);
end;

procedure TBGRAImageListResolution.StretchDrawOverlay(ACanvas: TCanvas; AIndex: Integer; ARect: TRect;
  AOverlay: TOverlay; AEnabled: Boolean);
var
   Bmp: TBitmap;
   vRect: TRect;
   OverlayI,
   vIndex: Integer;
   vDrawingStyle: TDrawingStyle;
   vImageType: TImageType;
   vDrawOverlay,
   stdDraw: Boolean;
   vOverlay: TOverlay;
   vDrawEffect: TGraphicsDrawEffect;
   rImageList: TBGRAImageList;

begin
  if ((ARect.Right-ARect.Left)=Width) and ((ARect.Bottom-ARect.Top)=Height) then
    DrawOverlay(ACanvas, ARect.Left, ARect.Top, AIndex, AOverlay, AEnabled)
  else
  begin
      rImageList:= TBGRAImageList(ImageList);

      //Copy Parameters to vars
      vRect:= ARect;
      vIndex:= AIndex;
      vDrawingStyle:= rImageList.DrawingStyle;
      vImageType:= rImageList.ImageType;
      vDrawOverlay:= True;
      vOverlay:= AOverlay;
      vDrawEffect:= EffectMap[AEnabled];

      stdDraw:= True;
      if Assigned(rImageList.FOnBeforeDraw)
      then stdDraw:= rImageList.FOnBeforeDraw(rImageList, ACanvas, vRect, vIndex, vDrawingStyle, vImageType,
                                              vDrawOverlay, vOverlay, vDrawEffect);

      if stdDraw then
      begin
        if not(vDrawOverlay) then vOverlay:= 0;

        if rImageList.rUseBGRADraw
        then BGRADraw(ACanvas, vRect, vIndex, vOverlay, vDrawingStyle, vImageType, vDrawEffect,
                      rImageList.BkColor, rImageList.BlendColor, True)
        else begin
               try
                  Bmp := TBitmap.Create;
                  {$IFDEF FPC}
                    GetBitmap(vIndex, Bmp, vDrawEffect);
                  {$ELSE}
                    GetBitmapRaw(vIndex, Bmp, vDrawEffect);
                  {$ENDIF}
                  ACanvas.StretchDraw(vRect, Bmp);

                  if vDrawOverlay and (vOverlay > 0) then
                  begin
                    OverlayI := rImageList.rOverlays[vOverlay];
                    {$IFDEF FPC}
                      GetBitmap(OverlayI, Bmp, vDrawEffect);
                    {$ELSE}
                      GetBitmapRaw(OverlayI, Bmp, vDrawEffect);
                    {$ENDIF}
                    Bmp.Mask(rImageList.BkColor);
                    ACanvas.StretchDraw(vRect, Bmp);
                  end;

               finally
                 Bmp.Free;
               end;
             end;
      end;

      if Assigned(rImageList.FOnAfterDraw)
      then rImageList.FOnAfterDraw(rImageList, ACanvas, vRect, vIndex, vDrawingStyle, vImageType,
                                   vDrawOverlay, vOverlay, vDrawEffect);
  end;
end;

procedure TBGRAImageList.SetUseBGRADraw(AValue: Boolean);
begin
  if (rUseBGRADraw<>AValue) then
  begin
    rUseBGRADraw:=AValue;
    { #todo 1 -oMaxM : How to Repaint? }
  end;
end;

function TBGRAImageList.GetResolution(AImageWidth: Integer): TBGRAImageListResolution;
begin
  Result := TBGRAImageListResolution(inherited GetResolution(AImageWidth));
end;

function TBGRAImageList.CreateProportionalImage(AImage: TCustomBitmap; AHorizAlign: TAlignment; AVertAlign: TTextLayout): TBitmap;
var
  rW, rH:Single;
  newWidth,
  newHeight:Integer;
  imgRect: TRect;
  Bitmap, BitmapR :TBGRABitmap;

begin
  Result:= nil;
  if (AImage = nil) then exit;

  if (AImage.Width > 0) or (AImage.Height > 0) then
  try
    imgRect.Left:= 0;
    imgRect.Top:= 0;

    rW := AImage.Width / Width;
    rH := AImage.Height / Height;

    if (rW > rH)
    then begin
           newHeight:= round(AImage.Height / rW);
           newWidth := Width;
           end
    else begin
           newWidth := round(AImage.Width / rH);
           newHeight := Height;
         end;

    case AHorizAlign of
    taCenter: imgRect.Left:= (Width-newWidth) div 2;
    taRightJustify: imgRect.Left:= Width-newWidth;
    end;
    case AVertAlign of
    tlCenter: imgRect.Top:= (Height-newHeight) div 2;
    tlBottom: imgRect.Top:= Height-newHeight;
    end;

    imgRect.Right:= imgRect.Left+newWidth;
    imgRect.Bottom:= imgRect.Top+newHeight;

    Result := TBitmap.Create;
    if (BkColor = clNone) then
    begin
      Result.Transparent:= True;
      Result.TransparentColor:= clNone;
    end;
    Result.SetSize(Width, Height);
    Result.Canvas.Brush.Color := BkColor;
    Result.Canvas.FillRect(0, 0, Width, Height);

    Bitmap := TBGRABitmap.Create;
    Bitmap.Assign(AImage);
    BitmapR :=Bitmap.Resample(newWidth, newHeight);
    BitmapR.Draw(Result.Canvas, imgRect, False);

  finally
    Bitmap.Free;
    BitmapR.Free;
  end;
end;

function TBGRAImageList.CreateMaskImage(AImage: TCustomBitmap; MaskColor: TColor): TBitmap;
begin
  Result:= nil;
  if (AImage <> nil) then
  begin
    Result := TBitmap.Create;
    Result.Assign(AImage);
    Result.TransparentColor := MaskColor;
    Result.TransparentMode := tmFixed;
    Result.Transparent := True;
  end;
end;

function TBGRAImageList.GetResolutionClass: TCustomImageListResolutionClass;
begin
  Result := TBGRAImageListResolution;
end;

procedure TBGRAImageList.Overlay(AIndex: Integer; AOverlay: TOverlay);
begin
  TImageList(Self).Overlay(AIndex, AOverlay);
  rOverlays[AOverlay] := AIndex;
end;

procedure TBGRAImageList.StretchDrawOverlay(ACanvas: TCanvas; AIndex: Integer; ARect: TRect; AOverlay: TOverlay;
  AEnabled: Boolean);
begin
  GetResolution(Width).StretchDrawOverlay(ACanvas, AIndex, ARect, AOverlay, AEnabled);
end;

function TBGRAImageList.AddProportionally(Image: TCustomBitmap; Mask: TCustomBitmap; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout): Integer;
begin
  Result := Count;
  InsertProportionally(Result, Image, Mask, AHorizAlign, AVertAlign);
end;

function TBGRAImageList.AddMaskedProportionally(Image: TBitmap; MaskColor: TColor; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout): Integer;
begin
  try
    Result := Count;
    InsertMaskedProportionally(Result, Image, MaskColor, AHorizAlign, AVertAlign);
  except
    Result := -1; // Ignore exceptions, just return -1
  end;
end;

procedure TBGRAImageList.InsertProportionally(AIndex: Integer; AImage: TCustomBitmap; AMask: TCustomBitmap;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout);
var
   Bmp,
   BmpMask: TBitmap;

begin
  try
     Bmp := CreateProportionalImage(AImage, AHorizAlign, AVertAlign);
     BmpMask := CreateProportionalImage(AMask, AHorizAlign, AVertAlign);
     Insert(AIndex, Bmp, BmpMask);

  finally
    BmpMask.Free;
    Bmp.Free;
  end;
end;

procedure TBGRAImageList.InsertMaskedProportionally(Index: Integer; AImage: TCustomBitmap; MaskColor: TColor;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout);
var
   BmpMask: TBitmap;

begin
  try
     BmpMask := CreateMaskImage(AImage, MaskColor);
     InsertProportionally(Index, AImage, BmpMask, AHorizAlign, AVertAlign)

  finally
    BmpMask.Free;
  end;
end;

procedure TBGRAImageList.ReplaceProportionally(AIndex: Integer; AImage: TCustomBitmap; AMask: TCustomBitmap;
  const AllResolutions: Boolean; AHorizAlign: TAlignment; AVertAlign: TTextLayout);
var
   Bmp,
   BmpMask: TBitmap;

begin
  try
     Bmp := CreateProportionalImage(AImage, AHorizAlign, AVertAlign);
     BmpMask := CreateProportionalImage(AMask, AHorizAlign, AVertAlign);
     Replace(AIndex, Bmp, BmpMask, AllResolutions);

  finally
    BmpMask.Free;
    Bmp.Free;
  end;
end;

procedure TBGRAImageList.ReplaceMaskedProportionally(Index: Integer; AImage: TCustomBitmap; MaskColor: TColor;
  const AllResolutions: Boolean; AHorizAlign: TAlignment; AVertAlign: TTextLayout);
var
   BmpMask: TBitmap;

begin
  try
     BmpMask := CreateMaskImage(AImage, MaskColor);
     ReplaceProportionally(Index, AImage, BmpMask, AllResolutions, AHorizAlign, AVertAlign)

  finally
    BmpMask.Free;
  end;
end;

constructor TBGRAImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF BGRA_DRAW}
    rUseBGRADraw:= True;
  {$ENDIF}
end;

destructor TBGRAImageList.Destroy;
begin
  if (FBGRA <> nil) then FBGRA.Free;
  if (FBmp <> nil) then FBmp.Free;

  inherited Destroy;
end;

end.
