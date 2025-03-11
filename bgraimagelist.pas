// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ This component partialy solve problem with no alpha in lazarus GTK.
  It is using BGRABitmap library for drawing icons.

  originally written in 2011 by Krzysztof Dibowski dibowski at interia.pl
}
(******************************** CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)   { #note -oMaxM : VCL Compatibility? }

- Massimo Magnano
 2024/12
   Added Before/AfterDraw events (don't works with Widgetsets)
   Added UseBGRADraw             ( " )
   Added Proportionally add methods
 2025/01
   Added Indexed image reading/writing and Load/SaveFile

***************************** END CONTRIBUTOR(S) *****************************)
unit BGRAImageList;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils,
  {$ifdef FPC}
  LResources, LCLVersion,
  {$endif}
  Controls, Graphics,
  GraphType, BGRABitmap, BGRABitmapTypes, {%H-}ImgList;

{$ifdef LCLgtk or LCLgtk2}
  { $DEFINE BGRA_DRAW}
{$endif}


const
  { #note -oMaxM : redeclared because are not public consts }
  SIG_LAZ1 = #1#0;
  SIG_LAZ2 = 'li';
  SIG_LAZ3 = 'Li';
  SIG_LAZ4 = 'Lz';
  SIG_D3   = 'IL';

  sInvalidIndex = 'Invalid ImageList Index';
  sInvalidFormat ='Invalid Stream Format Signature';

type
  TImageListSignature = array[0..1] of char; { #note -oMaxM : redeclared because is not a public type }

  { TBGRAImageListResolution }

  TBGRAImageListResolution = class(TDragImageListResolution)
  protected
    {$if lcl_fullversion >= 4990000}
    procedure ReadData(AStream: TStream; AIndex: Integer;
                       StartStreamPos: Int64=0; CalcPos: Boolean=True); virtual; overload;
    procedure WriteData(AStream: TStream; AIndex: Integer;
                       StartStreamPos: Int64=0; CalcPos: Boolean=True); virtual; overload;
    {$endif}

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

  {$if lcl_fullversion < 4990000}
  TOverlaysArray = array[TOverlay] of Integer;
  {$endif}

  TBGRAImageList = class(TImageList)
  private
    rUseBGRADraw: Boolean;
    FBGRA: TBGRABitmap;
    FBmp:  TBitmap;

    procedure SetUseBGRADraw(AValue: Boolean);

  protected
    FOnBeforeDraw: TCustomImageListBeforeDraw;
    FOnAfterDraw: TCustomImageListAfterDraw;

    {$if lcl_fullversion < 4990000}
    { #note -oMaxM : we keep our copy of the FOverlays array since it is declared private without any logic,
                     so derived classes cannot use it in any way also because there is no property to read them
                     see merged code freepascal.org/lazarus/lazarus!429
                     }
    rOverlays: TOverlaysArray;
    {$endif}

    function GetResolution(AImageWidth: Integer): TBGRAImageListResolution;

    function CreateEmptyBitmap(AImageWidth, AImageHeight: Integer;
                               AHorizAlign: TAlignment; AVertAlign: TTextLayout;
                               var imgRect: TRect): TBitmap;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetResolutionClass: TCustomImageListResolutionClass; override;

    procedure ReadData(AStream: TStream); override; overload;

    {$if lcl_fullversion >= 4990000}
    //Read/Write AIndex image from Stream without read/write all the images
    procedure ReadData(AStream: TStream; AIndex: Integer;
                       StartStreamPos: Int64=0; CalcPos: Boolean=True); virtual; overload;
    procedure WriteData(AStream: TStream); override; overload;
    procedure WriteData(AStream: TStream; AIndex: Integer;
                       StartStreamPos: Int64=0; CalcPos: Boolean=True); virtual; overload;

    //Read/Write from File
    procedure LoadFromFile(const AFilename: string; AIndex: Integer;
                       StartStreamPos: Int64=0; CalcPos: Boolean=True); overload;
    procedure LoadFromFileUTF8(const AFilenameUTF8: string; AIndex: Integer;
                       StartStreamPos: Int64=0; CalcPos: Boolean=True); overload;
    procedure SaveToFile(const AFilename: string; AIndex: Integer;
                       StartStreamPos: Int64=0; CalcPos: Boolean=True); overload;
    procedure SaveToFileUTF8(const AFilenameUTF8: string; AIndex: Integer;
                       StartStreamPos: Int64=0; CalcPos: Boolean=True); overload;
    {$else}
    procedure Overlay(AIndex: Integer; AOverlay: TOverlay);

    property Overlays: TOverlaysArray read rOverlays;
    {$endif}

    procedure LoadFromFile(const AFilename: string); overload;
    procedure LoadFromFileUTF8(const AFilenameUTF8: string); overload;
    procedure SaveToFile(const AFilename: string); overload;
    procedure SaveToFileUTF8(const AFilenameUTF8: string); overload;

    function CreateProportionalImage(AImage: TCustomBitmap;
                                     AHorizAlign: TAlignment; AVertAlign: TTextLayout): TBitmap; overload;
    function CreateProportionalImage(AImageFileName: String;
                                     AHorizAlign: TAlignment; AVertAlign: TTextLayout): TBitmap; overload;

    function CreateMaskImage(AImage: TCustomBitmap; MaskColor: TColor): TBitmap; overload;
    function CreateMaskImage(AImageFileName: String; MaskColor: TColor): TBitmap; overload;

    function CreateProportionalMaskImage(AImage: TCustomBitmap; MaskColor: TColor;
                                     AHorizAlign: TAlignment; AVertAlign: TTextLayout): TBitmap; overload;
    function CreateProportionalMaskImage(AImageFileName: String; MaskColor: TColor;
                                     AHorizAlign: TAlignment; AVertAlign: TTextLayout): TBitmap; overload;

    procedure StretchDrawOverlay(ACanvas: TCanvas; AIndex: Integer; ARect: TRect; AOverlay: TOverlay; AEnabled: Boolean = True);

    function AddProportionally(Image: TCustomBitmap; Mask: TCustomBitmap=nil;
                               AHorizAlign: TAlignment=taCenter; AVertAlign: TTextLayout=tlCenter): Integer; overload;
    function AddProportionally(AImageFileName: String; AMaskFileName: String='';
                               AHorizAlign: TAlignment=taCenter; AVertAlign: TTextLayout=tlCenter): Integer; overload;

    function AddMaskedProportionally(Image: TCustomBitmap; MaskColor: TColor;
                               AHorizAlign: TAlignment=taCenter; AVertAlign: TTextLayout=tlCenter): Integer; overload;
    function AddMaskedProportionally(AImageFileName: String; MaskColor: TColor;
                               AHorizAlign: TAlignment=taCenter; AVertAlign: TTextLayout=tlCenter): Integer; overload;

    procedure InsertProportionally(AIndex: Integer; AImage: TCustomBitmap; AMask: TCustomBitmap=nil;
                               AHorizAlign: TAlignment=taCenter; AVertAlign: TTextLayout=tlCenter); overload;
    procedure InsertProportionally(AIndex: Integer; AImageFileName: String; AMaskFileName: String='';
                               AHorizAlign: TAlignment=taCenter; AVertAlign: TTextLayout=tlCenter); overload;

    procedure InsertMaskedProportionally(AIndex: Integer; AImage: TCustomBitmap; MaskColor: TColor;
                               AHorizAlign: TAlignment=taCenter; AVertAlign: TTextLayout=tlCenter); overload;
    procedure InsertMaskedProportionally(AIndex: Integer; AImageFileName: String; MaskColor: TColor;
                               AHorizAlign: TAlignment=taCenter; AVertAlign: TTextLayout=tlCenter); overload;

    procedure ReplaceProportionally(AIndex: Integer; AImage: TCustomBitmap; AMask: TCustomBitmap=nil;
                               const AllResolutions: Boolean = True;
                               AHorizAlign: TAlignment=taCenter; AVertAlign: TTextLayout=tlCenter); overload;
    procedure ReplaceProportionally(AIndex: Integer; AImageFileName: String; AMaskFileName: String='';
                               const AllResolutions: Boolean = True;
                               AHorizAlign: TAlignment=taCenter; AVertAlign: TTextLayout=tlCenter); overload;

    procedure ReplaceMaskedProportionally(AIndex: Integer; AImage: TCustomBitmap; MaskColor: TColor;
                               const AllResolutions: Boolean = True;
                               AHorizAlign: TAlignment=taCenter; AVertAlign: TTextLayout=tlCenter); overload;
    procedure ReplaceMaskedProportionally(AIndex: Integer; AImageFileName: String; MaskColor: TColor;
                               const AllResolutions: Boolean = True;
                               AHorizAlign: TAlignment=taCenter; AVertAlign: TTextLayout=tlCenter); overload;


  published
    property UseBGRADraw: Boolean read rUseBGRADraw write SetUseBGRADraw;

    { #note -oMaxM : This Events don't works inside Widgetsets that use the imagelist handle
                     but only if you draw directly on the Canvas using ImageList Draw methods }
    property OnBeforeDraw: TCustomImageListBeforeDraw read FOnBeforeDraw write FOnBeforeDraw;
    property OnAfterDraw: TCustomImageListAfterDraw read FOnAfterDraw write FOnAfterDraw;
  end;

{$ifdef FPC}procedure Register;{$endif}

implementation

uses BGRAUTF8 {$ifdef FPC}, WSImgList{$endif};

const
  EffectMap: array[Boolean] of TGraphicsDrawEffect = (
    gdeDisabled,
    gdeNormal
  );

{$ifdef FPC}
procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBGRAImageList]);
end;
{$endif}

{ TBGRAImageListResolution }

{$if lcl_fullversion >= 4990000}
procedure TBGRAImageListResolution.ReadData(AStream: TStream; AIndex: Integer;
  StartStreamPos: Int64; CalcPos: Boolean);
var
   oStreamPos: Int64;
   Signature: TImageListSignature;
   datPos, sCount: Integer;

begin
  if (AIndex<0) or (AIndex>=Count) then raise EInvalidOperation.Create(SInvalidIndex);

  oStreamPos:= AStream.Position;
  try
     AStream.Position:= StartStreamPos;
     datPos:= AIndex * Width * Height;
     if CalcPos
     then begin
            AStream.Read(Signature, SizeOf(Signature));
            if Signature = SIG_LAZ3
            then begin
                   sCount:=ReadLRSInteger(AStream);
                   if (AIndex>=sCount) then raise EInvalidOperation.Create(SInvalidIndex);

                   AStream.Position:= oStreamPos+SizeOf(Signature)+(3*4)+(datPos*SizeOf(FData[0]));
                   AStream.Read(FData[datPos], Width * Height * SizeOf(FData[0]));
                 end
            else raise Exception.Create(sInvalidFormat+' '+Signature);
          end
     else AStream.Read(FData[datPos], Width * Height * SizeOf(FData[0]));

     if HandleAllocated
     then TWSCustomImageListResolutionClass(WidgetSetClass).Replace(Self, AIndex, @FData[datPos]);

   finally
     AStream.Position:= oStreamPos;
   end;
end;

procedure TBGRAImageListResolution.WriteData(AStream: TStream; AIndex: Integer;
  StartStreamPos: Int64; CalcPos: Boolean);
var
   oStreamPos: Int64;
   Signature: TImageListSignature;
   datPos: Integer;

begin
  if (AIndex<0) or (AIndex>=Count) then raise EInvalidOperation.Create(SInvalidIndex);

  oStreamPos:= AStream.Position;
  try
     AStream.Position:= StartStreamPos;
     datPos:= AIndex * Width * Height;
     if CalcPos
     then begin
            AStream.Read(Signature, SizeOf(Signature));
            if Signature = SIG_LAZ3
            then begin
                   WriteLRSInteger(AStream, Count);
                   AStream.Position:= oStreamPos+SizeOf(Signature)+(3*4)+(datPos*SizeOf(FData[0]));
                   AStream.Write(FData[datPos], Width * Height * SizeOf(FData[0]));
                 end
            else raise Exception.Create(sInvalidFormat+' '+Signature);
          end
     else AStream.Write(FData[datPos], Width * Height * SizeOf(FData[0]));

   finally
     AStream.Position:= oStreamPos;
   end;
end;
{$endif}

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

    {$ifdef FPC}
      GetBitmap(AIndex, FBmp, bmpDrawEffect);
    {$else}
      GetBitmapRaw(AIndex, FBmp, bmpDrawEffect);
    {$endif}
    FBGRA.Assign(FBmp);

    if (AOverlay > 0) then
    begin
      OverlayI := Overlays[AOverlay];
      if (OverlayI in [0..Count-1]) then
      begin
       {$ifdef FPC}
         GetBitmap(OverlayI, FBmp, bmpDrawEffect);
       {$else}
         GetBitmapRaw(OverlayI, FBmp, bmpDrawEffect);
       {$endif}
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
                  {$ifdef FPC}
                    GetBitmap(vIndex, Bmp, vDrawEffect);
                  {$else}
                    GetBitmapRaw(vIndex, Bmp, vDrawEffect);
                  {$endif}
                  ACanvas.StretchDraw(vRect, Bmp);

                  if vDrawOverlay and (vOverlay > 0) then
                  begin
                    OverlayI := rImageList.Overlays[vOverlay];
                    {$ifdef FPC}
                      GetBitmap(OverlayI, Bmp, vDrawEffect);
                    {$else}
                      GetBitmapRaw(OverlayI, Bmp, vDrawEffect);
                    {$endif}
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
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

function TBGRAImageList.GetResolution(AImageWidth: Integer): TBGRAImageListResolution;
begin
  Result := TBGRAImageListResolution(inherited GetResolution(AImageWidth));
end;

function TBGRAImageList.CreateEmptyBitmap(AImageWidth, AImageHeight: Integer; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; var imgRect: TRect): TBitmap;
var
  rW, rH:Single;
  newWidth,
  newHeight:Integer;

begin
  if (AImageWidth > 0) and (AImageHeight > 0) then
  begin
    imgRect.Left:= 0;
    imgRect.Top:= 0;

    rW := AImageWidth / Width;
    rH := AImageHeight / Height;

    if (rW > rH)
    then begin
           newHeight:= round(AImageHeight / rW);
           newWidth := Width;
           end
    else begin
           newWidth := round(AImageWidth / rH);
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
  end;
end;

function TBGRAImageList.CreateProportionalImage(AImage: TCustomBitmap;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout): TBitmap;
var
   imgRect: TRect;
   Bitmap, BitmapR :TBGRABitmap;

begin
  Result:= nil;

  if (AImage <> nil) and (AImage.Width > 0) and (AImage.Height > 0) then
  try
     Result:= CreateEmptyBitmap(AImage.Width, AImage.Height, AHorizAlign, AVertAlign, imgRect);

     //Use our Stretch since TBitmap's one sucks
     Bitmap := TBGRABitmap.Create;
     Bitmap.Assign(AImage);
     BitmapR :=Bitmap.Resample(imgRect.Width, imgRect.Height);
     BitmapR.Draw(Result.Canvas, imgRect, False);

  finally
    Bitmap.Free;
    BitmapR.Free;
  end;
end;

function TBGRAImageList.CreateProportionalImage(AImageFileName: String;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout): TBitmap;
var
   pict: TPicture;

begin
  Result:= nil;

  if FileExists(AImageFileName) then
  try
     pict:= TPicture.Create;
     pict.LoadFromFile(AImageFileName);
     Result:= CreateProportionalImage(pict.Bitmap, AHorizAlign, AVertAlign);

  finally
    pict.Free;
  end;
end;

function TBGRAImageList.CreateMaskImage(AImage: TCustomBitmap; MaskColor: TColor): TBitmap;
begin
  Result:= nil;
  if (AImage <> nil) and (AImage.Width > 0) and (AImage.Height > 0) then
  begin
    Result := TBitmap.Create;
    Result.Assign(AImage);
    Result.TransparentColor := MaskColor;
    Result.TransparentMode := tmFixed;
    Result.Transparent := True;
    Result.Masked:= True;
  end;
end;

function TBGRAImageList.CreateMaskImage(AImageFileName: String; MaskColor: TColor): TBitmap;
var
   //bmpBGRA: TBGRABitmap;
   pict: TPicture;

begin
  Result:= nil;
  if FileExists(AImageFileName) then
  try
    (*bmpBGRA:= TBGRABitmap.Create;
    bmpBGRA.LoadFromFile(AImageFileName);
    Result := bmpBGRA.MakeBitmapCopy(MaskColor, False);
    *)
    pict:= TPicture.Create;
    pict.LoadFromFile(AImageFileName);
    Result:=TBitmap.Create;
    Result.Assign(pict.Bitmap);
    Result.TransparentColor := MaskColor;
    Result.TransparentMode := tmFixed;
    Result.Transparent := True;
    Result.Masked:= True;

  finally
    pict.Free;
    //bmpBGRA.Free;
  end;
end;

function TBGRAImageList.CreateProportionalMaskImage(AImage: TCustomBitmap; MaskColor: TColor;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout): TBitmap;
var
   MaskBmp: TBitmap;

begin
  try
     MaskBmp:= CreateMaskImage(AImage, MaskColor);
     Result:= CreateProportionalImage(MaskBmp, AHorizAlign, AVertAlign);
     Result.TransparentColor:= MaskBmp.TransparentColor;

  finally
    MaskBmp.Free;
  end;
end;

function TBGRAImageList.CreateProportionalMaskImage(AImageFileName: String; MaskColor: TColor;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout): TBitmap;
var
   MaskBmp: TBitmap;

begin
  try
     MaskBmp:= CreateMaskImage(AImageFileName, MaskColor);
     Result:= CreateProportionalImage(MaskBmp, AHorizAlign, AVertAlign);
     Result.TransparentColor:= MaskBmp.TransparentColor;

  finally
    MaskBmp.Free;
  end;
end;

function TBGRAImageList.GetResolutionClass: TCustomImageListResolutionClass;
begin
  Result := TBGRAImageListResolution;
end;

procedure TBGRAImageList.ReadData(AStream: TStream);
begin
 inherited ReadData(AStream);
end;

procedure TBGRAImageList.LoadFromFile(const AFilename: string);
begin
  LoadFromFileUTF8(SysToUtf8(AFilename));
end;

procedure TBGRAImageList.LoadFromFileUTF8(const AFilenameUTF8: string);
var
   stream: TFileStreamUTF8;

begin
  stream := TFileStreamUTF8.Create(AFilenameUTF8, fmOpenRead or fmShareDenyWrite);
  try
     ReadData(stream);
  finally
    stream.Free;
  end;
end;

procedure TBGRAImageList.SaveToFile(const AFilename: string);
begin
  SaveToFileUTF8(SysToUtf8(AFilename));
end;

procedure TBGRAImageList.SaveToFileUTF8(const AFilenameUTF8: string);
var
  stream: TFileStreamUTF8;
begin
   stream := TFileStreamUTF8.Create(AFilenameUTF8, fmCreate);
   try
     WriteData(stream);
   finally
     stream.Free;
   end;
end;

{$if lcl_fullversion>=4990000}
procedure TBGRAImageList.ReadData(AStream: TStream; AIndex: Integer;
  StartStreamPos: Int64; CalcPos: Boolean);
begin
  GetResolution(Width).ReadData(AStream, AIndex, StartStreamPos, CalcPos);
end;

procedure TBGRAImageList.WriteData(AStream: TStream);
begin
  if (csDesigning in ComponentState)
  then inherited WriteData(AStream)
  else GetResolution(Width).WriteData(AStream, False); // don't compress data so we can write the image n without rewriting everything.
end;

procedure TBGRAImageList.WriteData(AStream: TStream; AIndex: Integer;
  StartStreamPos: Int64; CalcPos: Boolean);
begin
  GetResolution(Width).WriteData(AStream, AIndex, StartStreamPos, CalcPos);
end;

procedure TBGRAImageList.LoadFromFile(const AFilename: string; AIndex: Integer;
  StartStreamPos: Int64; CalcPos: Boolean);
begin
  LoadFromFileUTF8(SysToUtf8(AFilename), AIndex, StartStreamPos, CalcPos);
end;

procedure TBGRAImageList.LoadFromFileUTF8(const AFilenameUTF8: string; AIndex: Integer;
  StartStreamPos: Int64; CalcPos: Boolean);
var
   stream: TFileStreamUTF8;

begin
  stream := TFileStreamUTF8.Create(AFilenameUTF8, fmOpenRead or fmShareDenyWrite);
  try
     ReadData(stream, AIndex, StartStreamPos, CalcPos);
  finally
    stream.Free;
  end;
end;

procedure TBGRAImageList.SaveToFile(const AFilename: string; AIndex: Integer;
  StartStreamPos: Int64; CalcPos: Boolean);
begin
  SaveToFileUTF8(SysToUtf8(AFilename), AIndex, StartStreamPos, CalcPos);
end;

procedure TBGRAImageList.SaveToFileUTF8(const AFilenameUTF8: string; AIndex: Integer;
  StartStreamPos: Int64; CalcPos: Boolean);
var
  stream: TFileStreamUTF8;
begin
   stream := TFileStreamUTF8.Create(AFilenameUTF8, fmOpenReadWrite);
   try
     WriteData(stream, AIndex, StartStreamPos, CalcPos);
   finally
     stream.Free;
   end;
end;

{$else}
procedure TBGRAImageList.Overlay(AIndex: Integer; AOverlay: TOverlay);
begin
  TImageList(Self).Overlay(AIndex, AOverlay);
  rOverlays[AOverlay] := AIndex;
end;
{$endif}

procedure TBGRAImageList.StretchDrawOverlay(ACanvas: TCanvas; AIndex: Integer; ARect: TRect; AOverlay: TOverlay;
  AEnabled: Boolean);
begin
  GetResolution(Width).StretchDrawOverlay(ACanvas, AIndex, ARect, AOverlay, AEnabled);
end;

function TBGRAImageList.AddProportionally(Image: TCustomBitmap; Mask: TCustomBitmap; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout): Integer;
begin
  try
     Result:= Count;
     InsertProportionally(Result, Image, Mask, AHorizAlign, AVertAlign);
  except
     Result:= -1;
  end;
end;

function TBGRAImageList.AddProportionally(AImageFileName: String; AMaskFileName: String; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout): Integer;
begin
  try
     Result := Count;
     InsertProportionally(Result, AImageFileName, AMaskFileName, AHorizAlign, AVertAlign);
  except
     Result:= -1;
  end;
end;

function TBGRAImageList.AddMaskedProportionally(Image: TCustomBitmap; MaskColor: TColor; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout): Integer;
begin
  try
     Result := Count;
     InsertMaskedProportionally(Result, Image, MaskColor, AHorizAlign, AVertAlign);
  except
    Result:= -1;
  end;
end;

function TBGRAImageList.AddMaskedProportionally(AImageFileName: String; MaskColor: TColor; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout): Integer;
begin
  try
     Result := Count;
     InsertMaskedProportionally(Result, AImageFileName, MaskColor, AHorizAlign, AVertAlign);
  except
    Result:= -1;
  end;
end;


procedure TBGRAImageList.InsertProportionally(AIndex: Integer; AImage: TCustomBitmap; AMask: TCustomBitmap;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout);
var
   Bmp,
   BmpMask: TBitmap;

begin
  try
     BmpMask := CreateProportionalImage(AMask, AHorizAlign, AVertAlign);
     Bmp := CreateProportionalImage(AImage, AHorizAlign, AVertAlign);
     Insert(AIndex, Bmp, BmpMask);

  finally
    BmpMask.Free;
    Bmp.Free;
  end;
end;

procedure TBGRAImageList.InsertProportionally(AIndex: Integer; AImageFileName: String; AMaskFileName: String;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout);
var
   Bmp,
   BmpMask: TBitmap;

begin
  try
     BmpMask := CreateProportionalImage(AMaskFileName, AHorizAlign, AVertAlign);
     Bmp := CreateProportionalImage(AImageFileName, AHorizAlign, AVertAlign);
     Insert(AIndex, Bmp, BmpMask);

  finally
    BmpMask.Free;
    Bmp.Free;
  end;
end;

procedure TBGRAImageList.InsertMaskedProportionally(AIndex: Integer; AImage: TCustomBitmap; MaskColor: TColor;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout);
var
   BmpMask: TBitmap;

begin
  try
     BmpMask := CreateProportionalMaskImage(AImage, MaskColor, AHorizAlign, AVertAlign);
     InsertMasked(AIndex, BmpMask, MaskColor);

  finally
     BmpMask.Free;
  end;
end;

procedure TBGRAImageList.InsertMaskedProportionally(AIndex: Integer; AImageFileName: String; MaskColor: TColor;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout);
var
   BmpMask: TBitmap;

begin
  try
     BmpMask:= CreateProportionalMaskImage(AImageFileName, MaskColor, AHorizAlign, AVertAlign);
     InsertMasked(AIndex, BmpMask, MaskColor);

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
     BmpMask := CreateProportionalImage(AMask, AHorizAlign, AVertAlign);
     Bmp := CreateProportionalImage(AImage, AHorizAlign, AVertAlign);
     Replace(AIndex, Bmp, BmpMask, AllResolutions);

  finally
    BmpMask.Free;
    Bmp.Free;
  end;
end;

procedure TBGRAImageList.ReplaceProportionally(AIndex: Integer; AImageFileName: String; AMaskFileName: String;
  const AllResolutions: Boolean; AHorizAlign: TAlignment; AVertAlign: TTextLayout);
var
   Bmp,
   BmpMask: TBitmap;

begin
  try
     BmpMask := CreateProportionalImage(AMaskFileName, AHorizAlign, AVertAlign);
     Bmp := CreateProportionalImage(AImageFileName, AHorizAlign, AVertAlign);
     Replace(AIndex, Bmp, BmpMask, AllResolutions);

  finally
    BmpMask.Free;
    Bmp.Free;
  end;
end;

procedure TBGRAImageList.ReplaceMaskedProportionally(AIndex: Integer; AImage: TCustomBitmap; MaskColor: TColor;
  const AllResolutions: Boolean; AHorizAlign: TAlignment; AVertAlign: TTextLayout);
var
   BmpMask: TBitmap;

begin
  try
     BmpMask := CreateProportionalMaskImage(AImage, MaskColor, AHorizAlign, AVertAlign);
     ReplaceMasked(AIndex, BmpMask, MaskColor, AllResolutions);

  finally
    BmpMask.Free;
  end;
end;

procedure TBGRAImageList.ReplaceMaskedProportionally(AIndex: Integer; AImageFileName: String; MaskColor: TColor;
  const AllResolutions: Boolean; AHorizAlign: TAlignment; AVertAlign: TTextLayout);
var
   BmpMask: TBitmap;

begin
  try
     BmpMask := CreateProportionalMaskImage(AImageFileName, MaskColor, AHorizAlign, AVertAlign);
     ReplaceMasked(AIndex, BmpMask, MaskColor, AllResolutions);

  finally
     BmpMask.Free;
  end;
end;

constructor TBGRAImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$ifdef BGRA_DRAW}
    rUseBGRADraw:= True;
  {$endif}
end;

destructor TBGRAImageList.Destroy;
begin
  if (FBGRA <> nil) then FBGRA.Free;
  if (FBmp <> nil) then FBmp.Free;

  inherited Destroy;
end;

end.
