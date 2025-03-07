// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAImageManipulation;

{ ============================================================================
  BGRAImageManipulation Unit

  originally written in 2011 by - Emerson Cavalcanti <emersoncavalcanti at googlesites>

  ============================================================================
  Description:

  TBGRAImageManipulation is a component designed to make simple changes in an
  image while maintaining the aspect ratio of the final image and allow it to
  cut to reduce the unnecessary edges. The selected area is painted with a
  different transparency level for easy viewing of what will be cut.

  ============================================================================
  History:

  2011-05-03 - Emerson Cavalcanti
             - Initial version

  2011-06-01 - Emerson Cavalcanti
             - Fixed aspect ratio when the image has a dimension smaller than
               the size of the component.
             - Fixed memory leak on temporary bitmaps.
             - Fixed unecessary release of bitmap.
             - Inserted Anchor and Align property on component.
             - Implemented 'Keep aspect Ratio' property. Now you can select an
               area without maintaining the aspect ratio.

  2011-06-03 - Emerson Cavalcanti
             - Improved selection when don't use aspect ratio.
             - Improved response when resize component.
             - Fixed memory leak on resample bitmap.

  2011-06-04 - Circular
             - Fixed divide by zero when calculate aspect ratio on
               getImageRect.

  2011-06-07 - Emerson Cavalcanti
             - Improved function of aspect ratio including a variable to
               provide the value directly in the component, instead of using
               the dimensions of the component as the source of this value.
             - Improved exhibition of anchors on selection.
             - Improved mouse cursor.
             - Included function to get the aspect ratio from image size.
             - Included rotate Left and Right functions.

  2013-10-13 - Massimo Magnano
             - Add multi crop areas
             - Add get Bitmap not resampled (original scale)

  2014-08-04 - lainz-007-
             - Included DataType.inc inside the unit

  2021-03-30 - Massimo Magnano
             - Each CropArea has its own AspectRatio, Add Events, Border Color
  2021-04-30 - Massimo Magnano
             - CropArea list Load/Save, bug fixes

  2023-06    - Massimo Magnano
             - the CropArea.Area property is relative to the unscaled image (unused in render/mouse events)
             - added CropArea.ScaledArea property relative to the scaled image (used in render/mouse events)
             - removed the use of DeltaX, DeltaY in render/mouse/etc
             - CropAreas Area and ScaledArea property is updated during the mouse events
             - rewriting of the methods for taking cropped images
      -08    - the CropArea.Area property can be specified in Pixels,Cm,Inch
             - Alt on MouseUp Undo the Crop Area Changes,Optimized mouse events
      -09    - OverAnchor gives precedence to the selected area than Z Order
             - EmptyImage property; CropAreas when Image is Empty; Old Code deleted and optimized
             - XML Use Laz2_XMLCfg in fpc
             - divide by zero in getImageRect on Component Loading
             - EmptyImage size to ClientRect when Width/Height=0; Mouse Events when Image is Empty
             - CropArea Rotate and Flip
             - CropArea Duplicate and SetSize
             - NewCropAreaDefault property (to Cm); ResolutionUnitConvert function; SetEmptyImageSizeToCropAreas
      -10    - Load/Save XML Path Parameters, ContextMenu, UserData in GetAllBitmapCallback, CropArea Icons
  2024-01    - Added CopyProperties to GetBitmap methods
      -06    - Solved Bugs when load/save from xml
      -08    - Removed EmptyImage.Allow, so is always allowed
               CopyPropertiesToArea and Icons in NewCropAreaDefault
               Updated Component icon
  2025-01    - Added Load/Save and their events
      -02    - Deleted recreate of Bitmap and Empty; Optimization and code clean
               Render optimizations and adjustments for Gtk2 support
  ============================================================================
}

{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
{$I bgracontrols.inc}

interface

{$IFDEF FPC}
  {$DEFINE USE_Laz2_XMLCfg}
{$ENDIF}

uses
  Classes, Contnrs, SysUtils,
  {$IFDEF FPC}LCLIntf, LResources, FPImage, {$ENDIF}
  Forms, Controls, Graphics, Dialogs,
  {$IFNDEF FPC}Windows, Messages, BGRAGraphics, GraphType,{$ENDIF}
  {$IFDEF USE_Laz2_XMLCfg}Laz2_XMLCfg,{$ELSE}XMLConf,{$ENDIF}
  BCBaseCtrls, BGRABitmap, BGRABitmapTypes, BGRAGradientScanner;

{$IFNDEF FPC}
const
  crSizeNW      = TCursor(-23);
  crSizeN       = TCursor(-24);
  crSizeNE      = TCursor(-25);
  crSizeW       = TCursor(-26);
  crSizeE       = TCursor(-27);
  crSizeSW      = TCursor(-28);
  crSizeS       = TCursor(-29);
  crSizeSE      = TCursor(-30);
  crUpArrow     = TCursor(-10);
  crHourGlass   = TCursor(-11);
  crDrag        = TCursor(-12);
  crNoDrop      = TCursor(-13);
  crHSplit      = TCursor(-14);
  crVSplit      = TCursor(-15);
  crMultiDrag   = TCursor(-16);
{$ENDIF}

type
  TCoord = packed record
    x1 : LongInt;
    y1 : LongInt;
    x2 : LongInt;
    y2 : LongInt;
  end;

  TRatio = packed record
    Horizontal : LongInt;
    Vertical : LongInt;
  end;

  TCardinalDirection = (NORTH, SOUTH, WEST, EAST);
  TDirection = set of TCardinalDirection;

  TSizeLimits = packed record
    minWidth  : LongInt;
    maxWidth  : LongInt;
    minHeight : LongInt;
    maxHeight : LongInt;
  end;

  TBGRAImageManipulation = class;
  TCropAreaList = class;

  { TCropArea }
  BoolParent = (bFalse=0, bTrue=1, bParent=2);
  TCropAreaIcons = set of (cIcoIndex, cIcoLockSize, cIcoLockMove);

  TCropArea = class(TObject)
  protected
    fOwner   :TBGRAImageManipulation;
    OwnerList:TCropAreaList;
    rScaledArea:TRect;
    rArea    :TRectF;
    rAreaUnit:TResolutionUnit;
    rRatio   :TRatio;
    rAspectX,
    rAspectY,
    rMinHeight,
    rMinWidth : Integer;
    rAspectRatio,
    rName: String;
    rKeepAspectRatio: BoolParent;
    Loading  :Boolean;
    rIcons: TCropAreaIcons;

    procedure CopyAspectFromParent;
    procedure setAspectRatio(AValue: string);
    procedure setKeepAspectRatio(AValue: BoolParent);
    procedure setScaledArea(AValue: TRect);
    function getLeft: Single;
    procedure setLeft(AValue: Single);
    function getTop: Single;
    procedure setTop(AValue: Single);
    function getWidth: Single;
    procedure setWidth(AValue: Single);
    function getHeight: Single;
    procedure setHeight(AValue: Single);
    function getMaxHeight: Single;
    function getMaxWidth: Single;
    function getRealAspectRatio(var ARatio: TRatio):Boolean; //return Real KeepAspect
    function getRealKeepAspectRatio:Boolean;
    function getIndex: Longint;
    function getIsNullSize: Boolean;
    procedure setArea(AValue: TRectF);
    procedure setAreaUnit(AValue: TResolutionUnit);
    procedure setName(AValue: String);
    procedure setIcons(AValue: TCropAreaIcons);

    procedure Render_Invalidate;

    procedure GetImageResolution(var resX, resY:Single; var resUnit:TResolutionUnit);
    procedure CalculateScaledAreaFromArea;
    procedure CalculateAreaFromScaledArea;
    function GetPixelArea(const AValue: TRectF):TRect;

    function CheckScaledOutOfBounds(var AArea:TRect):Boolean;
    function CheckAreaOutOfBounds(var AArea:TRectF):Boolean;

    property ScaledArea :TRect read rScaledArea write setScaledArea;
  public
    Rotate   :Single;
    UserData :Integer;
    BorderColor :TBGRAPixel;

    function getResampledBitmap(ACopyProperties: Boolean=False): TBGRABitmap;
    function getBitmap(ACopyProperties: Boolean=False): TBGRABitmap;

    constructor Create(AOwner: TBGRAImageManipulation; AArea: TRectF;
                       AAreaUnit: TResolutionUnit = ruNone; //Pixels
                       AUserData: Integer = -1); overload;
    constructor Create(AOwner: TBGRAImageManipulation;
                       DuplicateFrom: TCropArea; InsertInList:Boolean); overload;
    destructor Destroy; override;

    //ZOrder
    procedure BringToFront;
    procedure BringToBack;
    procedure BringForward;
    procedure BringBackward;

    //Rotate/Flip
    procedure RotateLeft;
    procedure RotateRight;
    procedure FlipHLeft;
    procedure FlipHRight;
    procedure FlipVUp;
    procedure FlipVDown;

    procedure SetSize(AWidth, AHeight:Single);

    property Area:TRectF read rArea write setArea;
    property AreaUnit:TResolutionUnit read rAreaUnit write setAreaUnit;
    property Top:Single read getTop write setTop;
    property Left:Single read getLeft write setLeft;
    property Width:Single read getWidth write setWidth;
    property Height:Single read getHeight write setHeight;
    property MaxWidth:Single read getMaxWidth;
    property MaxHeight:Single read getMaxHeight;
    property AspectRatio: string read rAspectRatio write setAspectRatio;
    property KeepAspectRatio: BoolParent read rKeepAspectRatio write setKeepAspectRatio default bParent;
    property Index:Longint read getIndex;
    property Name:String read rName write setName;
    property isNullSize: Boolean read getIsNullSize;
    property Icons:TCropAreaIcons read rIcons write setIcons;
  end;

  { TCropAreaList }

  TCropAreaList = class(TObjectList)
  protected
    fOwner   :TBGRAImageManipulation;
    rName    :String;
    rLoading  :Boolean;

    function getCropArea(aIndex: Integer): TCropArea;
    procedure setCropArea(aIndex: Integer; const Value: TCropArea);
    procedure setLoading(AValue: Boolean);

    procedure Notify(Ptr: Pointer; Action: TListNotification); override;

    property Loading :Boolean read rLoading write setLoading;
  public
    constructor Create(AOwner: TBGRAImageManipulation);
    function add(aCropArea: TCropArea): integer;

    procedure Load(const XMLConf: TXMLConfig; XMLPath: String='');
    procedure Save(const XMLConf: TXMLConfig; XMLPath: String='');
    procedure LoadFromStream(Stream: TStream; XMLPath: String='');
    procedure LoadFromFile(const FileName: String; XMLPath: String='');
    procedure SaveToStream(Stream: TStream; XMLPath: String='');
    procedure SaveToFile(const FileName: String; XMLPath: String='');

    //Rotate/Flip
    procedure RotateLeft;
    procedure RotateRight;
    procedure FlipHLeft;
    procedure FlipHRight;
    procedure FlipVUp;
    procedure FlipVDown;

    property items[aIndex: integer] : TCropArea read getCropArea write setCropArea; default;
    property Name:String read rName write rName;
  end;

  TgetAllBitmapsCallback = procedure (Bitmap :TBGRABitmap; CropArea: TCropArea; AUserData:Integer) of object;

  { TBGRAEmptyImage }

  TBGRAEmptyImage = class(TPersistent)
  private
    fOwner: TBGRAImageManipulation;
    rResolutionHeight: Single;
    rResolutionUnit: TResolutionUnit;
    rResolutionWidth: Single;
    rShowBorder: Boolean;

    function getHeight: Integer;
    function getWidth: Integer;
    procedure SetResolutionUnit(AValue: TResolutionUnit);

  public
    property Width:Integer read getWidth;
    property Height:Integer read getHeight;

    constructor Create(AOwner: TBGRAImageManipulation);

  published
    property ResolutionUnit: TResolutionUnit read rResolutionUnit write SetResolutionUnit default ruPixelsPerCentimeter;
    property ResolutionWidth: Single read rResolutionWidth write rResolutionWidth;
    property ResolutionHeight: Single read rResolutionHeight write rResolutionHeight;
    property ShowBorder: Boolean read rShowBorder write rShowBorder default False;
  end;

  { TBGRANewCropAreaDefault }

  TBGRANewCropAreaDefault = class(TPersistent)
  private
    fOwner: TBGRAImageManipulation;
    rAspectRatio: string;
    rIcons: TCropAreaIcons;
    rKeepAspectRatio: BoolParent;
    rResolutionUnit: TResolutionUnit;

  public
    constructor Create(AOwner: TBGRAImageManipulation);

    procedure CopyPropertiesToArea(ANewArea: TCropArea);

  published
    property Icons: TCropAreaIcons read rIcons write rIcons;
    property ResolutionUnit: TResolutionUnit read rResolutionUnit write rResolutionUnit default ruPixelsPerCentimeter;
    property AspectRatio: string read rAspectRatio write rAspectRatio;
    property KeepAspectRatio: BoolParent read rKeepAspectRatio write rKeepAspectRatio default bFalse;
  end;

  { TBGRAImageManipulation }

  TCropAreaEvent = procedure (Sender: TBGRAImageManipulation; CropArea: TCropArea) of object;
  TCropAreaLoadEvent = function (Sender: TBGRAImageManipulation; CropArea: TCropArea;
                                 const XMLConf: TXMLConfig; const Path:String):Integer of object;
  TCropAreaSaveEvent = procedure (Sender: TBGRAImageManipulation; CropArea: TCropArea;
                                 const XMLConf: TXMLConfig; const Path:String) of object;

  TBGRAIMContextPopupEvent = procedure(Sender: TBGRAImageManipulation; CropArea: TCropArea;
                                       AnchorSelected :TDirection; MousePos: TPoint; var Handled: Boolean) of object;

  TBGRAIMBitmapLoadBefore = procedure (Sender: TBGRAImageManipulation; AStream: TStream;
                                 AFormat: TBGRAImageFormat; AHandler: TFPCustomImageReader;
                                 var AOptions: TBGRALoadingOptions) of object;

  TBGRAIMBitmapLoadAfter = procedure (Sender: TBGRAImageManipulation; AStream: TStream;
                                 AFormat: TBGRAImageFormat; AHandler: TFPCustomImageReader;
                                 AOptions: TBGRALoadingOptions) of object;

  TBGRAIMBitmapSaveBefore = procedure (Sender: TBGRAImageManipulation; AStream: TStream;
                                 AFormat: TBGRAImageFormat; AHandler: TFPCustomImageWriter) of object;

  TBGRAIMBitmapSaveAfter = procedure (Sender: TBGRAImageManipulation; AStream: TStream;
                                 AFormat: TBGRAImageFormat; AHandler: TFPCustomImageWriter) of object;

  TBGRAImageManipulation = class(TBGRAGraphicCtrl)
  private
    { Private declarations }
    fAnchorSize:      byte;
    fAnchorSelected:  TDirection;
    fBorderSize:      byte;
    fAspectRatio:     string;
    fAspectX:         integer;
    fAspectY:         integer;
    fKeepAspectRatio: boolean;
    fMinHeight:       integer;
    fMinWidth:        integer;
    fMouseCaught:     boolean;
    fStartPoint,
    fEndPoint:        TPoint;
    fStartArea:       TRect;
    fRatio:      TRatio;
    fSizeLimits: TSizeLimits;
    fImageBitmap, fResampledBitmap, fBackground, fVirtualScreen: TBGRABitmap;
    rNewCropAreaDefault: TBGRANewCropAreaDefault;
    rOnBitmapSaveAfter: TBGRAIMBitmapSaveAfter;
    rOnBitmapSaveBefore: TBGRAIMBitmapSaveBefore;

    function getAnchorSize: byte;
    function getPixelsPerInch: Integer;
    procedure setAnchorSize(const Value: byte);
    function getEmpty: boolean;
    procedure setBitmap(const Value: TBGRABitmap);
    procedure setBorderSize(const Value: byte);
    procedure setAspectRatio(const Value: string);
    procedure setEmptyImage(AValue: TBGRAEmptyImage);
    procedure setKeepAspectRatio(const Value: boolean);
    procedure setMinHeight(const Value: integer);
    procedure setMinWidth(const Value: integer);
    procedure SetOpacity(AValue: Byte);
    procedure setSelectedCropArea(AValue: TCropArea);
  protected
    { Protected declarations }
    rCropAreas :TCropAreaList;
    rNewCropArea,
    rSelectedCropArea :TCropArea;
    rOnCropAreaAdded: TCropAreaEvent;
    rOnCropAreaDeleted: TCropAreaEvent;
    rOnCropAreaChanged: TCropAreaEvent;
    rOnSelectedCropAreaChanged: TCropAreaEvent;
    rOnCropAreaLoad: TCropAreaLoadEvent;
    rOnCropAreaSave: TCropAreaSaveEvent;
    rOnBitmapLoadBefore: TBGRAIMBitmapLoadBefore;
    rOnBitmapLoadAfter: TBGRAIMBitmapLoadAfter;
    rOnContextPopup: TBGRAIMContextPopupEvent;
    rEmptyImage: TBGRAEmptyImage;
    rOpacity: Byte;

    function ApplyDimRestriction(Coords: TCoord; Direction: TDirection; Bounds: TRect; AKeepAspectRatio:Boolean): TCoord;
    function ApplyRatioToAxes(Coords: TCoord; Direction: TDirection; Bounds: TRect;  ACropArea :TCropArea = Nil): TCoord;
    procedure ApplyRatioToArea(ACropArea :TCropArea);
    procedure CalcMaxSelection(ACropArea :TCropArea);
    procedure findSizeLimits;
    function getDirection(const Point1, Point2: TPoint): TDirection;
    function getImageRect(Picture: TBGRABitmap): TRect;
    function getWorkRect: TRect;
    function isOverAnchor(APoint :TPoint; var AnchorSelected :TDirection; var ACursor :TCursor) :TCropArea;
    procedure CreateEmptyImage;
    procedure CreateResampledBitmap;

    class function GetControlClassDefaultSize: TSize; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean); override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure DoOnResize; override;
    procedure RenderBackground;
    procedure Render;
    procedure Render_Invalidate;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    function getAspectRatioFromImage(const Value: TBGRABitmap): string;
    function getResampledBitmap(ACropArea :TCropArea = Nil; ACopyProperties: Boolean=False) : TBGRABitmap;
    function getBitmap(ACropArea :TCropArea = Nil; ACopyProperties: Boolean=False) : TBGRABitmap;

    procedure rotateLeft(ACopyProperties: Boolean=False);
    procedure rotateRight(ACopyProperties: Boolean=False);

    procedure tests;

    //Crop Areas Manipulation functions
    function addCropArea(AArea : TRectF; AAreaUnit: TResolutionUnit = ruNone;
                         AUserData: Integer = -1) :TCropArea;
    function addScaledCropArea(AArea : TRect; AUserData: Integer = -1) :TCropArea;
    procedure delCropArea(ACropArea :TCropArea);
    procedure clearCropAreas;
    procedure getAllResampledBitmaps(ACallBack :TgetAllBitmapsCallback; AUserData:Integer=0; ACopyProperties: Boolean=False);
    procedure getAllBitmaps(ACallBack :TgetAllBitmapsCallback; AUserData:Integer=0; ACopyProperties: Boolean=False);

    procedure SetEmptyImageSizeToCropAreas(ReduceLarger: Boolean=False);
    procedure SetEmptyImageSizeToNull;
    procedure SetEmptyImageSize(AResolutionUnit: TResolutionUnit; AResolutionWidth, AResolutionHeight: Single);

    procedure LoadFromFile(const AFilename: String); overload;
    procedure LoadFromFile(const AFilename: String; AHandler:TFPCustomImageReader; AOptions: TBGRALoadingOptions); overload;
    procedure LoadFromFileUTF8(const AFilenameUTF8: String); overload;
    procedure LoadFromFileUTF8(const AFilenameUTF8: String; AHandler:TFPCustomImageReader; AOptions: TBGRALoadingOptions); overload;
    procedure LoadFromStream(AStream: TStream); overload;
    procedure LoadFromStream(AStream: TStream; AHandler:TFPCustomImageReader; AOptions: TBGRALoadingOptions); overload;

    procedure SaveToFile(const AFilename: String); overload;
    procedure SaveToFile(const AFilename: String; AFormat: TBGRAImageFormat; AHandler:TFPCustomImageWriter=nil); overload;
    procedure SaveToFileUTF8(const AFilenameUTF8: String); overload;
    procedure SaveToFileUTF8(const AFilenameUTF8: String; AFormat: TBGRAImageFormat; AHandler:TFPCustomImageWriter=nil); overload;
    procedure SaveToStream(AStream: TStream; AFormat: TBGRAImageFormat; AHandler:TFPCustomImageWriter=nil); overload;

    property SelectedCropArea :TCropArea read rSelectedCropArea write setSelectedCropArea;
    property CropAreas :TCropAreaList read rCropAreas;
    property PixelsPerInch: Integer read getPixelsPerInch;
  published
    { Published declarations }

    property Align;
    property Anchors;

    property AnchorSize: byte Read getAnchorSize Write setAnchorSize default 5;
    property Bitmap: TBGRABitmap Read fImageBitmap Write setBitmap;
    property BorderSize: byte Read fBorderSize Write setBorderSize default 2;
    property AspectRatio: string Read fAspectRatio Write setAspectRatio;
    property KeepAspectRatio: boolean Read fKeepAspectRatio Write setKeepAspectRatio default True;
    property MinHeight: integer Read fMinHeight Write setMinHeight;
    property MinWidth: integer Read fMinWidth Write setMinWidth;
    property Empty: boolean Read getEmpty;
    property EmptyImage: TBGRAEmptyImage read rEmptyImage write setEmptyImage stored True;
    property NewCropAreaDefault: TBGRANewCropAreaDefault read rNewCropAreaDefault write rNewCropAreaDefault stored True;
    property Opacity: Byte read rOpacity write SetOpacity default 128;

    //Events
    property OnCropAreaAdded:TCropAreaEvent read rOnCropAreaAdded write rOnCropAreaAdded;
    property OnCropAreaDeleted:TCropAreaEvent read rOnCropAreaDeleted write rOnCropAreaDeleted;
    property OnCropAreaChanged:TCropAreaEvent read rOnCropAreaChanged write rOnCropAreaChanged;
    property OnCropAreaLoad:TCropAreaLoadEvent read rOnCropAreaLoad write rOnCropAreaLoad;
    property OnCropAreaSave:TCropAreaSaveEvent read rOnCropAreaSave write rOnCropAreaSave;

             //CropArea Parameter is the Old Selected Area, use SelectedCropArea property for current
    property OnSelectedCropAreaChanged:TCropAreaEvent read rOnSelectedCropAreaChanged write rOnSelectedCropAreaChanged;

    property OnContextPopup: TBGRAIMContextPopupEvent read rOnContextPopup write rOnContextPopup;
(*    property OnStartDrag: TStartDragEvent;
    property OnDragDrop: TDragDropEvent;
    property OnDragOver: TDragOverEvent;
    property OnEndDrag: TEndDragEvent;*)

    property OnBitmapLoadBefore: TBGRAIMBitmapLoadBefore read rOnBitmapLoadBefore write rOnBitmapLoadBefore;
    property OnBitmapLoadAfter: TBGRAIMBitmapLoadAfter read rOnBitmapLoadAfter write rOnBitmapLoadAfter;
    property OnBitmapSaveBefore: TBGRAIMBitmapSaveBefore read rOnBitmapSaveBefore write rOnBitmapSaveBefore;
    property OnBitmapSaveAfter: TBGRAIMBitmapSaveAfter read rOnBitmapSaveAfter write rOnBitmapSaveAfter;
  end;


function RoundUp(AValue:Single):Integer;
function ResolutionUnitConvert(const AValue:Single; fromRes, toRes:TResolutionUnit; predefInchRes:Integer=96):Single;
procedure PixelXResolutionUnitConvert(var resX, resY:Single; fromRes, toRes:TResolutionUnit);

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

uses
  {$ifopt D+}LazLogger,{$endif}
  Math, ExtCtrls, BGRAUTF8, UniversalDrawer, BGRAWritePNG, FPWritePNM;

resourcestring
  SAnchorSizeIsTooLarge =
    'Anchor size is too large. %d is not within the valid range of %d..%d';
  SAnchorSizeIsTooSmall =
    'Anchor size is too small. %d is not within the valid range of %d..%d';
  SAnchorSizeIsNotOdd = 'Anchor size is invalid. %d is not an odd number.';

  SBorderSizeIsTooLarge =
    'Border size is too large. %d is not within the valid range of %d..%d';
  SBorderSizeIsTooSmall =
    'Border size is too small. %d is not within the valid range of %d..%d';

  SAspectRatioIsNotValid = 'Aspect ratio value is invalid. %s contain invalid number.';


{ Calculate the Greatest Common Divisor (GCD) using the algorithm of Euclides }
function getGCD(Nr1, Nr2: longint): longint;
begin
  if Nr2 = 0 then
    Result := Nr1
  else
    Result := getGCD(Nr2, Nr1 mod Nr2);
end;

{ Calculate the Lowest Common Multiple (LCM) using the algorithm of Euclides }
function getLCM(Nr1, Nr2: longint): longint;
begin
    Result := (Nr1 * Nr2) div getGCD(Nr1, Nr2);
end;


procedure CheckAspectRatio(const Value :String; var AspectRatioText :String; var XValue, YValue :Integer);
const
  ValidChars = ['0'..'9', ':'];
var
   Count :Integer;

begin
  if ((pos(':', Value) > 0) and (pos(':', Value) < Length(Value))) then
  begin
    // Check if value is valid
    XValue := 0;
    YValue := 0;
    AspectRatioText := '';
    for Count := 1 to Length(Value) do
    begin
      if (Value[Count] in ValidChars) then
      begin
        if ((Value[Count] = ':') and (Length(AspectRatioText) > 0) and
          (XValue = 0)) then
        begin
          XValue := StrToInt(AspectRatioText);
        end;

        AspectRatioText := AspectRatioText + Value[Count];
      end
      else
      begin
        // Value contain invalid characters
        raise EInvalidArgument.CreateFmt(SAspectRatioIsNotValid, [Value]);
      end;
    end;
    YValue := StrToInt(Copy(AspectRatioText, Pos(':', AspectRatioText) + 1,
      Length(AspectRatioText)));
  end
  else
  begin
    // Value contain invalid characters
    raise EInvalidArgument.CreateFmt(SAspectRatioIsNotValid, [Value]);
  end;
end;

function RoundUp(AValue:Single):Integer;
var
   oRoundMode :TFPURoundingMode;

begin
  oRoundMode :=Math.GetRoundMode;
  //Round to Upper Value
  Math.SetRoundMode(rmUp);
  Result :=Round(AValue);
  Math.SetRoundMode(oRoundMode);
end;

function ResolutionUnitConvert(const AValue:Single; fromRes, toRes:TResolutionUnit; predefInchRes:Integer):Single;
begin
  if (fromRes<>toRes)
  then Case fromRes of
       ruNone: begin
         if toRes=ruPixelsPerInch
         then Result :=AValue/predefInchRes         //in
         else Result :=(AValue/predefInchRes)*2.54; //cm
       end;
       ruPixelsPerInch :begin
         if toRes=ruPixelsPerCentimeter
         then Result :=AValue*2.54           //cm
         else Result :=AValue*predefInchRes; //pixel
       end;
       ruPixelsPerCentimeter :begin
         if toRes=ruPixelsPerInch
         then Result :=AValue/2.54                 //in
         else Result :=(AValue/2.54)*predefInchRes;//cm
       end;
       end
  else Result:=AValue;
end;

procedure PixelXResolutionUnitConvert(var resX, resY: Single; fromRes, toRes: TResolutionUnit);
begin
  //Do Conversion from/to PixelXInch/PixelXCm
  if (toRes <> fromRes) then
  begin
    if (toRes=ruPixelsPerInch)
    then begin
           resX :=resX*2.54;
           resY :=resY*2.54;
         end
    else begin
           resX :=resX/2.54;
           resY :=resY/2.54;
         end
  end;
end;

{ TCropArea }

procedure TCropArea.Render_Invalidate;
begin
  if not(fOwner.rCropAreas.loading) then
  begin
    {$ifopt D+}
     DebugLn('  CropArea Render_Invalidate');
    {$endif}

    fOwner.Render_Invalidate;

    {$ifopt D+}
     DebugLn('  CropArea Render_Invalidate done');
    {$endif}
  end;
end;

procedure TCropArea.GetImageResolution(var resX, resY: Single; var resUnit: TResolutionUnit);
begin
  resX :=fOwner.fImageBitmap.ResolutionX;
  resY :=fOwner.fImageBitmap.ResolutionY;
  resUnit :=fOwner.fImageBitmap.ResolutionUnit;

  if (resX<2) or (resY<2) then      //Some images have 1x1 PixelPerInch ?
  begin
    //No Resolution use predefined Form Values
    resUnit :=rAreaUnit;

    if (rAreaUnit=ruPixelsPerInch)
    then resX :=fOwner.PixelsPerInch
    else resX :=fOwner.PixelsPerInch/2.54;

    resY :=resX;
  end;
end;

function TCropArea.getIsNullSize: Boolean;
begin
  Result := not((abs(rArea.Right - rArea.Left) > 0) and (abs(rArea.Bottom - rArea.Top) > 0));
end;

procedure TCropArea.setName(AValue: String);
begin
  if rName=AValue then Exit;

  rName:=AValue;

  if assigned(fOwner.rOnCropAreaChanged)
  then fOwner.rOnCropAreaChanged(fOwner, Self);
end;

procedure TCropArea.setIcons(AValue: TCropAreaIcons);
begin
  if rIcons=AValue then Exit;
  rIcons:=AValue;
  Render_Invalidate;
end;

function TCropArea.getTop: Single;
begin
  Result :=rArea.Top;
end;

procedure TCropArea.setTop(AValue: Single);
var
   tempArea:TRectF;

begin
  if AValue=rArea.Top then Exit;

  tempArea :=rArea;
  tempArea.Top:=AValue;
  tempArea.Height:=rArea.Height;
  //CheckAreaOutOfBounds(tempArea);
  Area :=tempArea;
end;

function TCropArea.getLeft: Single;
begin
  Result :=rArea.Left;
end;

procedure TCropArea.setLeft(AValue: Single);
var
   tempArea:TRectF;
   tempSArea:TRect;

begin
  if AValue=rArea.Left then Exit;

  tempArea :=rArea;
  tempArea.Left:=AValue;
  tempArea.Width:=rArea.Width;
  //CheckAreaOutOfBounds(tempArea);
  Area :=tempArea;
(*  if CheckScaledOutOfBounds(rScaledArea)
  then begin
         CalculateAreaFromScaledArea;

         if assigned(fOwner.rOnCropAreaChanged)
         then fOwner.rOnCropAreaChanged(fOwner, Self);
       end; *)
end;

function TCropArea.getHeight: Single;
begin
  Result :=rArea.Height;
end;

procedure TCropArea.setHeight(AValue: Single);
var
   tempArea:TRectF;

begin
  if AValue=rArea.Height then Exit;

  tempArea :=rArea;
  tempArea.Height:=AValue;
  //CheckAreaOutOfBounds(tempArea);
  Area :=tempArea;
end;

function TCropArea.getWidth: Single;
begin
  Result :=rArea.Width;
end;

procedure TCropArea.setWidth(AValue: Single);
var
   tempArea:TRectF;

begin
  if AValue=rArea.Width then Exit;

  tempArea :=rArea;
  tempArea.Width:=AValue;
  //CheckAreaOutOfBounds(tempArea);
  Area :=tempArea;
end;

function TCropArea.getMaxHeight: Single;
begin
  if (rAreaUnit=ruNone)
  then Result :=fOwner.fImageBitmap.Height
  else begin
         if (fOwner.fImageBitmap.ResolutionY<2)
         then Result :=fOwner.fImageBitmap.Height     //No Resolution, Some images have 1x1 PixelPerInch ?
         else begin
                Result :=fOwner.fImageBitmap.ResolutionHeight;

                //Do Conversion from/to inch/cm
                if (rAreaUnit <> fOwner.fImageBitmap.ResolutionUnit) then
                begin
                  if (rAreaUnit=ruPixelsPerInch)
                  then Result :=Result/2.54  //Bitmap is in Cm, i'm in Inch
                  else Result :=Result*2.54; //Bitmap is in Inch, i'm in Cm
                end;
              end;
       end;
end;

function TCropArea.getMaxWidth: Single;
begin
  if (rAreaUnit=ruNone)
  then Result :=fOwner.fImageBitmap.Width
  else begin
         if (fOwner.fImageBitmap.ResolutionX<2)
         then Result :=fOwner.fImageBitmap.Width     //No Resolution, Some images have 1x1 PixelPerInch ?
         else begin
                Result :=fOwner.fImageBitmap.ResolutionWidth;

                //Do Conversion from/to inch/cm
                if (rAreaUnit <> fOwner.fImageBitmap.ResolutionUnit) then
                begin
                  if (rAreaUnit=ruPixelsPerInch)
                  then Result :=Result/2.54  //Bitmap is in Cm, i'm in Inch
                  else Result :=Result*2.54; //Bitmap is in Inch, i'm in Cm
                end;
              end;
       end;
end;

function TCropArea.getIndex: Longint;
begin
  Result :=fOwner.CropAreas.IndexOf(Self);
end;

procedure TCropArea.CalculateScaledAreaFromArea;
var
   xRatio, yRatio: Single;
   resX, resY: Single;
   resUnit:TResolutionUnit;

begin
  if not(isNullSize) then
  begin
    // Calculate Scaled Area given Scale and Resolution
    if (fOwner.fImageBitmap.Width=0) or (fOwner.fImageBitmap.Height=0)
    then begin
           xRatio :=1;
           yRatio :=1;
         end
    else begin
           xRatio := fOwner.fResampledBitmap.Width / fOwner.fImageBitmap.Width;
           yRatio := fOwner.fResampledBitmap.Height / fOwner.fImageBitmap.Height;
         end;

    resX :=1;  //if rAreaUnit=ruNone use only Ratio
    resY :=1;

    if (rAreaUnit<>ruNone) then
    begin
      GetImageResolution(resX, resY, resUnit);
      PixelXResolutionUnitConvert(resX, resY, resUnit, rAreaUnit);
    end;

    //MaxM: Use Trunc for Top/Left and Round for Right/Bottom so we
    //      preserve as much data as possible when do the crop
    rScaledArea.Left := Trunc(rArea.Left * resX * xRatio);
    rScaledArea.Top := Trunc(rArea.Top * resY * yRatio);
    rScaledArea.Right := Round(rArea.Right* resX * xRatio);
    rScaledArea.Bottom := Round(rArea.Bottom * resY * yRatio);
  end;
end;

procedure TCropArea.CalculateAreaFromScaledArea;
var
   xRatio, yRatio: Single;
   resX, resY: Single;
   resUnit:TResolutionUnit;

begin
  // Calculate Scaled Area given Scale and Resolution
  if (fOwner.fImageBitmap.Width=0) or (fOwner.fImageBitmap.Height=0)
  then begin
         xRatio :=1;
         yRatio :=1;
       end
  else begin
         xRatio := fOwner.fResampledBitmap.Width / fOwner.fImageBitmap.Width;
         yRatio := fOwner.fResampledBitmap.Height / fOwner.fImageBitmap.Height;
       end;

  resX :=1; //if rAreaUnit=ruNone use only Ratio
  resY :=1;

  if (rAreaUnit<>ruNone) then
  begin
    GetImageResolution(resX, resY, resUnit);
    PixelXResolutionUnitConvert(resX, resY, resUnit, rAreaUnit);
  end;

  rArea.Left := (rScaledArea.Left / resX) / xRatio;
  rArea.Right := (rScaledArea.Right / resX) / xRatio;
  rArea.Top := (rScaledArea.Top / resY) / yRatio;
  rArea.Bottom := (rScaledArea.Bottom / resY) / yRatio;
end;

function TCropArea.GetPixelArea(const AValue: TRectF): TRect;
var
   resX, resY: Single;
   resUnit: TResolutionUnit;

begin
  if (rAreaUnit=ruNone)
  then begin
         Result.Left := Trunc(AValue.Left);
         Result.Right := Trunc(AValue.Right);
         Result.Top := Trunc(AValue.Top);
         Result.Bottom := Trunc(AValue.Bottom);
       end
  else begin
         if (rAreaUnit=ruNone)
         then begin
                resX :=1;
                resY :=1;
              end
         else GetImageResolution(resX, resY, resUnit);

         PixelXResolutionUnitConvert(resX, resY, resUnit, rAreaUnit);

         Result.Left := Trunc(AValue.Left * resX);
         Result.Top := Trunc(AValue.Top * resY);
         Result.Right := Round(AValue.Right* resX);
         Result.Bottom := Round(AValue.Bottom * resY);
    end;
end;

function TCropArea.CheckScaledOutOfBounds(var AArea: TRect): Boolean;
var
   tmpValue: Integer;

begin
  Result :=False;

  if (AArea.Left<0)
  then begin
         tmpValue :=-AArea.Left;
         AArea.Left :=0;
         AArea.Right:=AArea.Right+tmpValue;
         Result :=True;
       end;

  if (AArea.Top<0)
  then begin
         tmpValue :=-AArea.Top;
         AArea.Top :=0;
         AArea.Bottom:=AArea.Bottom+tmpValue;
         Result :=True;
       end;

  if (AArea.Right>fOwner.fResampledBitmap.Width)
  then begin
         tmpValue :=AArea.Right-fOwner.fResampledBitmap.Width;
         AArea.Right :=fOwner.fResampledBitmap.Width;
         AArea.Left:=AArea.Left-tmpValue; //if <0 ? a vicious circle
         Result :=True;
       end;

  if (AArea.Bottom>fOwner.fResampledBitmap.Height)
  then begin
         tmpValue :=AArea.Bottom-fOwner.fResampledBitmap.Height;
         AArea.Bottom :=fOwner.fResampledBitmap.Height;
         AArea.Top:=AArea.Top-tmpValue; //if <0 ? a vicious circle
         Result :=True;
       end;
end;

function TCropArea.CheckAreaOutOfBounds(var AArea: TRectF):Boolean;
var
   tmpValue, resWH: Single;

begin
  Result :=False;
  if (AArea.Left<0)
  then begin
         tmpValue :=-AArea.Left;
         AArea.Left :=0;
         AArea.Right:=AArea.Right+tmpValue;
         Result :=True;
       end;

  if (AArea.Top<0)
  then begin
         tmpValue :=-AArea.Top;
         AArea.Top :=0;
         AArea.Bottom:=AArea.Bottom+tmpValue;
         Result :=True;
       end;

  resWH :=fOwner.fImageBitmap.ResolutionWidth;
  if (AArea.Right>resWH)
  then begin
         tmpValue :=AArea.Right-resWH;
         AArea.Right :=resWH;
         AArea.Left:=AArea.Left-tmpValue; //if <0 ? a vicious circle
         Result :=True;
       end;

  resWH :=fOwner.fImageBitmap.ResolutionHeight;
  if (AArea.Bottom>resWH)
  then begin
         tmpValue :=AArea.Bottom-resWH;
         AArea.Bottom :=resWH;
         AArea.Top:=AArea.Top-tmpValue; //if <0 ? a vicious circle
         Result :=True;
       end;
end;

procedure TCropArea.CopyAspectFromParent;
begin
  rAspectX :=fOwner.fAspectX;
  rAspectY :=fOwner.fAspectY;
  rMinHeight :=fOwner.fMinHeight;
  rMinWidth :=fOwner.fMinWidth;
  rAspectRatio:=fOwner.fAspectRatio;
  rRatio :=fOwner.fRatio;
end;

procedure TCropArea.setAspectRatio(AValue: string);
var
   XValue, YValue: integer;
   AspectRatioText: string;
   fGCD     :integer;

begin
  if (rKeepAspectRatio = bParent)
  then CopyAspectFromParent
  else begin
         if (AValue <> rAspectRatio) then
         begin
            // Check if value contain a valid string
            CheckAspectRatio(AValue, AspectRatioText, XValue, YValue);

            // Set new Aspect Ratio
            rAspectRatio := AspectRatioText;
            rAspectX := XValue;
            rAspectY := YValue;

            // Calculate the ratio
            fGCD := getGCD(rAspectX, rAspectY);

            // Determine the ratio of scale per axle
            with rRatio do
            begin
              Horizontal := rAspectX div fGCD;
              Vertical := rAspectY div fGCD;
            end;

            // Set minimun size
            if ((rRatio.Horizontal < fOwner.fAnchorSize + 10) or
                (rRatio.Vertical < fOwner.fAnchorSize + 10)) then
            begin
              rMinWidth  := rRatio.Horizontal * 10;
              rMinHeight := rRatio.Vertical * 10;
            end
            else
            begin
              rMinWidth  := rRatio.Horizontal;
              rMinHeight := rRatio.Vertical;
            end;

            fOwner.ApplyRatioToArea(Self);

            Render_Invalidate;
         end;
      end;
end;

procedure TCropArea.setKeepAspectRatio(AValue: BoolParent);
begin
  if rKeepAspectRatio=AValue then Exit;

  rKeepAspectRatio :=AValue;

  if (rKeepAspectRatio = bParent) then
  begin
    rAspectRatio :=fOwner.AspectRatio;
    CopyAspectFromParent;
    if (fOwner.KeepAspectRatio)
    then fOwner.ApplyRatioToArea(Self);
  end
  else if (rKeepAspectRatio = bTrue)
       then fOwner.ApplyRatioToArea(Self);


  Render_Invalidate;
end;

procedure TCropArea.setArea(AValue: TRectF);
var
   curKeepAspectRatio :Boolean;
   curRatio :TRatio;
   calcHeight, calcWidth, swapV :Single;

begin
  if (rArea.TopLeft = AValue.TopLeft) and
    (rArea.BottomRight = AValue.BottomRight) then Exit;

  if (AValue.Left > AValue.Right) then
  begin
    swapV :=AValue.Left;
    AValue.Left :=AValue.Right;
    AValue.Right:=swapV;
  end;

  if (AValue.Top > AValue.Bottom) then
  begin
    swapV :=AValue.Top;
    AValue.Top :=AValue.Bottom;
    AValue.Bottom:=swapV;
  end;

  if fOwner.fMouseCaught
  then rArea:=AValue
  else begin
         curKeepAspectRatio :=getRealAspectRatio(curRatio);

         if curKeepAspectRatio
         then begin
                calcWidth :=AValue.Width;
                calcHeight :=AValue.Height;

                //if the Width is Changed recalculate the Height
                if (calcWidth <> rArea.Width)
                then calcHeight :=Trunc(abs(calcWidth) * (curRatio.Vertical / curRatio.Horizontal))
                else begin
                       //if the New Width is the same but the Height is Changed recalculate the New Width
                       if (calcHeight <> rArea.Height)
                       then calcWidth :=Trunc(abs(calcHeight) * (curRatio.Horizontal / curRatio.Vertical));
                     end;

                rArea.Left:=AValue.Left;
                rArea.Top:=AValue.Top;
                rArea.Width:=calcWidth;
                rArea.Height:=calcHeight;
              end
         else rArea:=AValue;     //Free Aspect

         CalculateScaledAreaFromArea;

         Render_Invalidate;
       end;

  if assigned(fOwner.rOnCropAreaChanged)
  then fOwner.rOnCropAreaChanged(fOwner, Self);
end;

procedure TCropArea.setAreaUnit(AValue: TResolutionUnit);
var
   imgResX, imgResY :Single;

begin
  if rAreaUnit=AValue then Exit;

  if not(Loading) and not(isNullSize) then
  begin
    //Get Image Resolution in Pixel/Inchs
    Case fOwner.Bitmap.ResolutionUnit of
    ruPixelsPerInch : begin
      imgResX :=fOwner.Bitmap.ResolutionX;
      imgResY :=fOwner.Bitmap.ResolutionY;
      end;
    ruPixelsPerCentimeter : begin
      imgResX :=fOwner.Bitmap.ResolutionX*2.54;
      imgResY :=fOwner.Bitmap.ResolutionY*2.54;
      end;
    ruNone : begin
      //No Image Resolution, Use predefined Monitor Values
      imgResX :=fOwner.PixelsPerInch;
      imgResY :=fOwner.PixelsPerInch;
      end;
    end;

    //Paranoid test to avoid zero divisions
    if (imgResX=0) then imgResX :=fOwner.PixelsPerInch;
    if (imgResY=0) then imgResY :=fOwner.PixelsPerInch;

    Case rAreaUnit of
    ruPixelsPerInch : begin
      if (AValue=ruNone)
      then begin //From Inchs to Pixels, we need Image Resolution
             //MaxM: Use Trunc for Top/Left and Round for Right/Bottom so we
             //      preserve as much data as possible when do the crop
             rArea.Left:=Trunc(rArea.Left*imgResX);
             rArea.Top:=Trunc(rArea.Top*imgResY);
             rArea.Right:=Round(rArea.Right*imgResX);
             rArea.Bottom:=Round(rArea.Bottom*imgResY);
           end
      else begin //From Inchs to Cm
             rArea.Left:=rArea.Left*2.54;
             rArea.Top:=rArea.Top*2.54;
             rArea.Right:=rArea.Right*2.54;
             rArea.Bottom:=rArea.Bottom*2.54;
           end;
      end;
    ruPixelsPerCentimeter : begin
      if (AValue=ruNone)
      then begin //From Cm to Pixels, first convert to Inchs than use Image Resolution
             rArea.Left:=Trunc((rArea.Left/2.54)*imgResX);
             rArea.Top:=Trunc((rArea.Top/2.54)*imgResY);
             rArea.Right:=Round((rArea.Right/2.54)*imgResX);
             rArea.Bottom:=Round((rArea.Bottom/2.54)*imgResY);
           end
      else begin //From Cm to Inchs
             rArea.Left:=rArea.Left/2.54;
             rArea.Top:=rArea.Top/2.54;
             rArea.Right:=rArea.Right/2.54;
             rArea.Bottom:=rArea.Bottom/2.54;
           end;
      end;
    ruNone : begin
      if (AValue=ruPixelsPerInch)
      then begin //From Pixels to Inchs
             rArea.Left:=rArea.Left/imgResX;
             rArea.Top:=rArea.Top/imgResY;
             rArea.Right:=rArea.Right/imgResX;
             rArea.Bottom:=rArea.Bottom/imgResY;
           end
      else begin
             rArea.Left:=(rArea.Left/2.54)/imgResX;
             rArea.Top:=(rArea.Top/2.54)/imgResY;
             rArea.Right:=(rArea.Right/2.54)/imgResX;
             rArea.Bottom:=(rArea.Bottom/2.54)/imgResY;
           end;
      end;
    end;
  end;

  rAreaUnit:=AValue;

  if assigned(fOwner.rOnCropAreaChanged)
  then fOwner.rOnCropAreaChanged(fOwner, Self);
end;


procedure TCropArea.setScaledArea(AValue: TRect);
var
   curKeepAspectRatio :Boolean;
   curRatio :TRatio;
   calcHeight, calcWidth, swapV :Longint;

begin
  if rScaledArea=AValue then Exit;

  if (AValue.Left > AValue.Right) then
  begin
    swapV :=AValue.Left;
    AValue.Left :=AValue.Right;
    AValue.Right:=swapV;
  end;

  if (AValue.Top > AValue.Bottom) then
  begin
    swapV :=AValue.Top;
    AValue.Top :=AValue.Bottom;
    AValue.Bottom:=swapV;
  end;

  if fOwner.fMouseCaught
  then rScaledArea:=AValue
  else begin
         curKeepAspectRatio :=getRealAspectRatio(curRatio);

         if curKeepAspectRatio
         then begin
                calcWidth :=AValue.Width;
                calcHeight :=AValue.Height;

                //if the Width is Changed recalculate the Height
                if (calcWidth <> rScaledArea.Width)
                then calcHeight :=Trunc(abs(calcWidth) * (curRatio.Vertical / curRatio.Horizontal))
                else begin
                       //if the New Width is the same but the Height is Changed recalculate the New Width
                       if (calcHeight <> rScaledArea.Height)
                       then calcWidth :=Trunc(abs(calcHeight) * (curRatio.Horizontal / curRatio.Vertical));
                     end;

                rScaledArea.Left:=AValue.Left;
                rScaledArea.Top:=AValue.Top;
                rScaledArea.Width:=calcWidth;
                rScaledArea.Height:=calcHeight;
              end
         else rScaledArea:=AValue;     //Free Aspect

         CalculateAreaFromScaledArea;

         Render_Invalidate;
       end;

  if assigned(fOwner.rOnCropAreaChanged)
  then fOwner.rOnCropAreaChanged(fOwner, Self);
end;

function TCropArea.getRealAspectRatio(var ARatio: TRatio): Boolean;
begin
  Case rKeepAspectRatio of
  bParent : begin
              Result :=fOwner.fKeepAspectRatio;
              ARatio :=fOwner.fRatio;
            end;
  bTrue   : begin
              Result :=True;
              ARatio :=Self.rRatio;
            end;
  bFalse  : Result :=False;
  end;
end;

function TCropArea.getRealKeepAspectRatio: Boolean;
begin
  Case rKeepAspectRatio of
  bParent : Result :=fOwner.fKeepAspectRatio;
  bTrue   : Result :=True;
  bFalse  : Result :=False;
  end;
end;

//Get Resampled Bitmap (Scaled to current scale)
function TCropArea.getResampledBitmap(ACopyProperties: Boolean=False): TBGRABitmap;
var
  ResampledBitmap: TBGRACustomBitmap;
  CropBitmap:  TBGRABitmap;

begin
  Result :=nil;
  if not (fOwner.fImageBitmap.Empty) then
  try
     try
        // Create a new bitmap for cropped region in original scale
        CropBitmap := getBitmap(ACopyProperties);

        // Create bitmap to put image on final scale
        Result := TBGRABitmap.Create(rScaledArea.Width, rScaledArea.Height);

        // Resize the cropped image to final scale
        ResampledBitmap := CropBitmap.Resample(rScaledArea.Width, rScaledArea.Height, rmFineResample, ACopyProperties);
        Result.BlendImage(0, 0, ResampledBitmap, boLinearBlend);
     finally
        ResampledBitmap.Free;
        CropBitmap.Free;
     end;
  except
     if (Result<>nil)
     then FreeAndNil(Result);
  end;
end;

//Get Original size Bitmap (not scaled to current scale)
function TCropArea.getBitmap(ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result :=nil;
  if not (fOwner.fImageBitmap.Empty) then
  try
     // Get the cropped image on selected region in original scale
     Result :=fOwner.fImageBitmap.GetPart(GetPixelArea(rArea), ACopyProperties);
  except
     if (Result<>nil)
     then FreeAndNil(Result);
  end;
end;

constructor TCropArea.Create(AOwner: TBGRAImageManipulation; AArea: TRectF;
                             AAreaUnit: TResolutionUnit; AUserData: Integer);
begin
  inherited Create;
  if (AOwner = Nil)
  then raise Exception.Create('TCropArea Owner is Nil');
  OwnerList :=nil;
  fOwner :=AOwner;
  rAreaUnit :=AAreaUnit;
  Area := AArea;
  UserData :=AUserData;
  rAspectX :=3;
  rAspectY :=4;
  rKeepAspectRatio :=bParent;
  Loading:=False;
  CopyAspectFromParent;
end;

constructor TCropArea.Create(AOwner: TBGRAImageManipulation;
                             DuplicateFrom: TCropArea; InsertInList:Boolean);
begin
  if (DuplicateFrom = Nil)
  then raise Exception.Create('TCropArea DuplicateFrom is Nil');

  Create(AOwner, DuplicateFrom.Area, DuplicateFrom.AreaUnit, DuplicateFrom.UserData);

  OwnerList :=nil;
  rAspectX :=DuplicateFrom.rAspectX;
  rAspectY :=DuplicateFrom.rAspectY;
  rKeepAspectRatio :=DuplicateFrom.rKeepAspectRatio;
  Loading:=False;
  if rKeepAspectRatio=bParent
  then CopyAspectFromParent;

  if InsertInList and (DuplicateFrom.OwnerList<>nil)
  then DuplicateFrom.OwnerList.add(Self);
end;

destructor TCropArea.Destroy;
begin
  inherited Destroy;
end;

procedure TCropArea.BringToFront;
begin
  if (OwnerList<>nil) then
  try
    OwnerList.Move(OwnerList.IndexOf(Self), OwnerList.Count-1);
  except
  end;
end;

procedure TCropArea.BringToBack;
begin
  if (OwnerList<>nil) then
  try
    OwnerList.Move(OwnerList.IndexOf(Self), 0);
  except
  end;
end;

procedure TCropArea.BringForward;
var
   curIndex :Integer;

begin
  if (OwnerList<>nil) then
  try
    curIndex :=OwnerList.IndexOf(Self);
    if (curIndex<OwnerList.Count-1)
    then OwnerList.Move(curIndex, curIndex+1);
  except
  end;
end;

procedure TCropArea.BringBackward;
var
   curIndex :Integer;

begin
  if (OwnerList<>nil) then
  try
    curIndex :=OwnerList.IndexOf(Self);
    if (curIndex>0)
    then OwnerList.Move(curIndex, curIndex-1);
  except
  end;
end;

procedure TCropArea.RotateLeft;
var
   newArea :TRect;

begin
  newArea.Right :=rScaledArea.Left;
  newArea.Bottom:=rScaledArea.Bottom;
  newArea.Left:=newArea.Right-rScaledArea.Height;
  newArea.Top:=newArea.Bottom-rScaledArea.Width;
  CheckScaledOutOfBounds(newArea);
  ScaledArea :=newArea;
end;

procedure TCropArea.RotateRight;
var
   newArea :TRect;

begin
  newArea.Left :=rScaledArea.Right;
  newArea.Bottom:=rScaledArea.Bottom;
  newArea.Right:=newArea.Left+rScaledArea.Height;
  newArea.Top:=newArea.Bottom-rScaledArea.Width;
  CheckScaledOutOfBounds(newArea);
  ScaledArea :=newArea;
end;

procedure TCropArea.FlipHLeft;
var
   newArea :TRect;

begin
  newArea.Top:=rScaledArea.Top;
  newArea.Bottom:=rScaledArea.Bottom;
  newArea.Right :=rScaledArea.Left;
  newArea.Left:=newArea.Right-rScaledArea.Width;
  CheckScaledOutOfBounds(newArea);
  ScaledArea :=newArea;
end;

procedure TCropArea.FlipHRight;
var
   newArea :TRect;

begin
  newArea.Top:=rScaledArea.Top;
  newArea.Bottom:=rScaledArea.Bottom;
  newArea.Left :=rScaledArea.Right;
  newArea.Right:=newArea.Left+rScaledArea.Width;
  CheckScaledOutOfBounds(newArea);
  ScaledArea :=newArea;
end;

procedure TCropArea.FlipVUp;
var
   newArea :TRect;

begin
  newArea.Left:=rScaledArea.Left;
  newArea.Right:=rScaledArea.Right;
  newArea.Bottom :=rScaledArea.Top;
  newArea.Top:=newArea.Bottom-rScaledArea.Height;
  CheckScaledOutOfBounds(newArea);
  ScaledArea :=newArea;
end;

procedure TCropArea.FlipVDown;
var
   newArea :TRect;

begin
  newArea.Left:=rScaledArea.Left;
  newArea.Right:=rScaledArea.Right;
  newArea.Top :=rScaledArea.Bottom;
  newArea.Bottom:=newArea.Top+rScaledArea.Height;
  CheckScaledOutOfBounds(newArea);
  ScaledArea :=newArea;
end;

procedure TCropArea.SetSize(AWidth, AHeight: Single);
var
   tempArea:TRectF;

begin
  if (AWidth=rArea.Width) and (AHeight=rArea.Height)
  then exit;

  tempArea :=rArea;
  tempArea.Width:=AWidth;
  tempArea.Height:=AHeight;
  //CheckAreaOutOfBounds(tempArea);
  Area :=tempArea;
end;

{ TCropAreaList }

procedure TCropAreaList.setLoading(AValue: Boolean);
var
   i :Integer;

begin
  for i :=0 to Count-1 do items[i].Loading :=AValue;
  rLoading:=AValue;
end;

function TCropAreaList.getCropArea(aIndex: Integer): TCropArea;
begin
  Result := inherited Items[aIndex] as TCropArea;
end;

procedure TCropAreaList.setCropArea(aIndex: Integer; const Value: TCropArea);
begin
  inherited Items[aIndex] := Value;
end;

procedure TCropAreaList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  Case Action of
  lnAdded: begin
    TCropArea(Ptr).OwnerList :=Self;
    if assigned(fOwner.rOnCropAreaAdded)
    then fOwner.rOnCropAreaAdded(fOwner, Ptr);
  end;
  lnDeleted: begin
    TCropArea(Ptr).OwnerList :=Nil;
    if assigned(fOwner.rOnCropAreaDeleted)
    then fOwner.rOnCropAreaDeleted(fOwner, Ptr);
  end;
  end;

  inherited Notify(Ptr, Action);
end;

constructor TCropAreaList.Create(AOwner: TBGRAImageManipulation);
begin
     inherited Create;

     if (AOwner = Nil)
     then raise Exception.Create('Owner TBGRAImageManipulation is Nil');
     fOwner :=AOwner;
     rName :=Self.ClassName;
     loading :=False;
end;

function TCropAreaList.add(aCropArea: TCropArea): integer;
begin
  Result := inherited Add(aCropArea);
end;

procedure TCropAreaList.Load(const XMLConf: TXMLConfig; XMLPath: String);
var
  i, newCount, newSelected: integer;
  curItemPath, curPath: String;
  newCropArea: TCropArea;
  newArea: TRectF;
  newAreaUnit:TResolutionUnit;

begin
  try
    if (XMLPath='')
    then curPath :=fOwner.Name+'.'+Self.Name+'/'
    else curPath :=XMLPath+'/';

    newCount := XMLConf.GetValue(curPath+'Count', -1);
    if (newCount=-1)
    then raise Exception.Create('XML Path not Found - '+curPath+'Count');

    Clear;
    Loading :=True;

    newSelected := XMLConf.GetValue(curPath+'Selected', -1);
    for i :=0 to newCount-1 do
    begin
      curItemPath :=curPath+'Item' + IntToStr(i)+'/';
      newArea :=RectF(0,0,0,0);

      //Area
      newArea.Left :=StrToFloat(XMLConf.GetValue(curItemPath+'Area/Left', '0'));
      newArea.Top :=StrToFloat(XMLConf.GetValue(curItemPath+'Area/Top', '0'));
      newArea.Width :=StrToFloat(XMLConf.GetValue(curItemPath+'Area/Width', IntToStr(fOwner.MinWidth)));
      newArea.Height :=StrToFloat(XMLConf.GetValue(curItemPath+'Area/Height', IntToStr(fOwner.MinHeight)));

      newAreaUnit :=TResolutionUnit(XMLConf.GetValue(curItemPath+'AreaUnit', 0));
      newCropArea :=TCropArea.Create(Self.fOwner, newArea, newAreaUnit);
      newCropArea.Loading:=True;
      newCropArea.Name :=XMLConf.GetValue(curItemPath+'Name', 'Name '+IntToStr(i));
      newCropArea.KeepAspectRatio :=BoolParent(XMLConf.GetValue(curItemPath+'KeepAspectRatio', Integer(bParent)));
      newCropArea.AspectRatio :=XMLConf.GetValue(curItemPath+'AspectRatio', '3:4');
      newCropArea.Rotate :=StrToFloat(XMLConf.GetValue(curItemPath+'Rotate', '0'));
      newCropArea.UserData :=XMLConf.GetValue(curItemPath+'UserData', -1);

      if assigned(fOwner.rOnCropAreaLoad)
      then newCropArea.UserData :=fOwner.rOnCropAreaLoad(fOwner, newCropArea, XMLConf, curItemPath);
      newCropArea.Loading:=False;

      add(newCropArea);
    end;

    if (newCount>0)
    then begin
           if (newSelected<newCount)
           then fOwner.SelectedCropArea :=items[newSelected]
           else fOwner.SelectedCropArea :=items[0];
         end
    else fOwner.SelectedCropArea :=nil;

   finally
     loading :=False;

     fOwner.Render_Invalidate;
   end;
end;

procedure TCropAreaList.Save(const XMLConf: TXMLConfig; XMLPath: String);
var
  i: integer;
  curItemPath, curPath: String;
  curCropArea: TCropArea;

begin
  if (XMLPath='')
  then curPath :=fOwner.Name+'.'+Self.Name+'/'
  else curPath :=XMLPath+'/';

  XMLConf.DeletePath(curPath);

  XMLConf.SetValue(curPath+'Count', Count);

  if (fOwner.SelectedCropArea<>nil)
  then XMLConf.SetValue(curPath+'Selected', fOwner.SelectedCropArea.Index)
  else XMLConf.SetValue(curPath+'Selected', -1);

  for i :=0 to Count-1 do
  begin
    curItemPath :=curPath+'Item' + IntToStr(i)+'/';
    curCropArea:=Items[i];

    XMLConf.SetValue(curItemPath+'Name', curCropArea.Name);
    XMLConf.SetValue(curItemPath+'KeepAspectRatio', Integer(curCropArea.KeepAspectRatio));
    XMLConf.SetValue(curItemPath+'AspectRatio', curCropArea.AspectRatio);
    XMLConf.SetValue(curItemPath+'Rotate', FloatToStr(curCropArea.Rotate));
    XMLConf.SetValue(curItemPath+'AreaUnit', Integer(curCropArea.AreaUnit));
    XMLConf.SetValue(curItemPath+'UserData', curCropArea.UserData);

    //Area
    XMLConf.SetValue(curItemPath+'Area/Left', FloatToStr(curCropArea.Area.Left));
    XMLConf.SetValue(curItemPath+'Area/Top', FloatToStr(curCropArea.Area.Top));
    XMLConf.SetValue(curItemPath+'Area/Width', FloatToStr(curCropArea.Area.Width));
    XMLConf.SetValue(curItemPath+'Area/Height', FloatToStr(curCropArea.Area.Height));

    if assigned(fOwner.rOnCropAreaSave)
    then fOwner.rOnCropAreaSave(fOwner, curCropArea, XMLConf, curItemPath);
  end;
end;

procedure TCropAreaList.LoadFromStream(Stream: TStream; XMLPath: String);
var
   FXMLConf: TXMLConfig;

begin
  try
    FXMLConf := TXMLConfig.Create(nil);
    {$IFDEF USE_Laz2_XMLCfg}
    FXMLConf.ReadFromStream(Stream);
    {$ELSE}
    FXMLConf.ReadOnly:=True;
    FXMLConf.LoadFromStream(Stream);
    {$ENDIF}
    Load(FXMLConf, XMLPath);
  finally
    FXMLConf.Free;
  end;
end;

procedure TCropAreaList.LoadFromFile(const FileName: String; XMLPath: String);
var
   FXMLConf: TXMLConfig;

begin
  try
    {$IFDEF USE_Laz2_XMLCfg}
    FXMLConf := TXMLConfig.Create(FileName);
    {$ELSE}
    FXMLConf := TXMLConfig.Create(nil);
    FXMLConf.ReadOnly:=True;
    FXMLConf.LoadFromFile(FileName);
    {$ENDIF}
     Load(FXMLConf, XMLPath);
  finally
     FXMLConf.Free;
  end;
end;

procedure TCropAreaList.SaveToStream(Stream: TStream; XMLPath: String);
var
  FXMLConf: TXMLConfig;

begin
  try
    FXMLConf := TXMLConfig.Create(nil);
    Save(FXMLConf, XMLPath);
    {$IFDEF USE_Laz2_XMLCfg}
    FXMLConf.WriteToStream(Stream);
    {$ELSE}
    FXMLConf.SaveToStream(Stream);
    {$ENDIF}
  finally
    FXMLConf.Free;
  end;
end;

procedure TCropAreaList.SaveToFile(const FileName: String; XMLPath: String);
var
  FXMLConf: TXMLConfig;

begin
  try
    {$IFDEF USE_Laz2_XMLCfg}
    FXMLConf := TXMLConfig.Create(FileName);
    Save(FXMLConf, XMLPath);
    FXMLConf.Flush;
    {$ELSE}
    FXMLConf := TXMLConfig.Create(nil);
    Save(FXMLConf, XMLPath);
    FXMLConf.SaveToFile(FileName);
    {$ENDIF}
  finally
    FXMLConf.Free;
  end;
end;

procedure TCropAreaList.RotateLeft;
var
   i :Integer;

begin
  for i:=0 to Count-1 do Items[i].RotateLeft;
end;

procedure TCropAreaList.RotateRight;
var
   i :Integer;

begin
  for i:=0 to Count-1 do Items[i].RotateRight;
end;

procedure TCropAreaList.FlipHLeft;
var
   i :Integer;

begin
  for i:=0 to Count-1 do Items[i].FlipHLeft;
end;

procedure TCropAreaList.FlipHRight;
var
   i :Integer;

begin
  for i:=0 to Count-1 do Items[i].FlipHRight;
end;

procedure TCropAreaList.FlipVUp;
var
   i :Integer;

begin
  for i:=0 to Count-1 do Items[i].FlipVUp;
end;

procedure TCropAreaList.FlipVDown;
var
   i :Integer;

begin
  for i:=0 to Count-1 do Items[i].FlipVDown;
end;

{ TBGRAEmptyImage }

function TBGRAEmptyImage.getHeight: Integer;
var
   wRect: TRect;

begin
  if (rResolutionHeight<=0) or (rResolutionWidth<=0)
  then begin
         //wRect := fOwner.getWorkRect;
         wRect := fOwner.GetClientRect;
         InflateRect(wRect, -fOwner.BorderSize, -fOwner.BorderSize);
         Result := wRect.Bottom-wRect.Top;
       end
  else Case rResolutionUnit of
       ruNone : Result :=Trunc(rResolutionHeight);
       ruPixelsPerInch : Result :=Round(fOwner.PixelsPerInch*rResolutionHeight);
       ruPixelsPerCentimeter : Result :=Round((fOwner.PixelsPerInch/2.54)*rResolutionHeight);
       end;
end;

function TBGRAEmptyImage.getWidth: Integer;
var
   wRect: TRect;

begin
  if (rResolutionWidth<=0) or (rResolutionHeight<=0)
  then begin
         //wRect := fOwner.getWorkRect;
         wRect := fOwner.GetClientRect;
         InflateRect(wRect, -fOwner.BorderSize, -fOwner.BorderSize);
         Result := wRect.Right-wRect.Left;
       end
  else Case rResolutionUnit of
       ruNone : Result :=Trunc(rResolutionWidth);
       ruPixelsPerInch : Result :=Round(fOwner.PixelsPerInch*rResolutionWidth);
       ruPixelsPerCentimeter : Result :=Round((fOwner.PixelsPerInch/2.54)*rResolutionWidth);
       end;
end;

procedure TBGRAEmptyImage.SetResolutionUnit(AValue: TResolutionUnit);
begin
  if (AValue<>rResolutionUnit) then
  begin
    rResolutionWidth :=ResolutionUnitConvert(rResolutionWidth, rResolutionUnit, AValue, fOwner.PixelsPerInch);
    rResolutionHeight :=ResolutionUnitConvert(rResolutionHeight, rResolutionUnit, AValue, fOwner.PixelsPerInch);
    rResolutionUnit :=AValue;
  end;
end;

constructor TBGRAEmptyImage.Create(AOwner: TBGRAImageManipulation);
begin
  inherited Create;
  fOwner :=AOwner;
  rShowBorder :=False;
  rResolutionUnit:=ruPixelsPerCentimeter;
end;

{ TBGRANewCropAreaDefault }

constructor TBGRANewCropAreaDefault.Create(AOwner: TBGRAImageManipulation);
begin
  inherited Create;
  fOwner :=AOwner;
  rKeepAspectRatio:=bFalse;
  rAspectRatio:='3:4';
  rResolutionUnit:=ruPixelsPerCentimeter;
  rIcons:= [];
end;

procedure TBGRANewCropAreaDefault.CopyPropertiesToArea(ANewArea: TCropArea);
begin
  ANewArea.rIcons:= Self.rIcons;
  ANewArea.rAspectRatio:= Self.rAspectRatio;
  ANewArea.KeepAspectRatio:= Self.rKeepAspectRatio;
end;

{ TBGRAImageManipulation }

 { ============================================================================ }
 { =====[ Auxiliary Functions ]================================================ }
 { ============================================================================ }

{ Applies the given size constraint on the coordinates along both axes }
function TBGRAImageManipulation.ApplyDimRestriction(Coords: TCoord;
  Direction: TDirection; Bounds: TRect; AKeepAspectRatio:Boolean): TCoord;
var
  newCoords: TCoord;
  calcWidth, calcHeight: integer;
  recalculateHeight: boolean;
begin
  // Gets coordinates
  newCoords := Coords;
  recalculateHeight := False;

  // Calculated width
  calcWidth  := abs(newCoords.x2 - newCoords.x1);
  calcHeight := abs(newCoords.y2 - newCoords.y1);

  // Checks if the width is smaller than the minimum value
  if (Abs(calcWidth) < MinWidth) and (MinWidth < fImageBitmap.Width) then
  begin
    // Resizes the width based on the minimum value
    calcWidth := MinWidth;

    if (EAST in Direction) then
    begin
      // If the motion is in a positive direction, make sure we're not going out
      // of bounds
      if ((newCoords.x1 + calcWidth) > Bounds.Right) then
      begin
        // Moves the horizontal coordinates
        newCoords.x1 := Bounds.Right - calcWidth;
        newCoords.x2 := Bounds.Right;
      end
      else
      begin
        // Moves the last horizontal coordinate
        newCoords.x2 := newCoords.x1 + calcWidth;
      end;
    end
    else
    begin
      // If the motion is in a negative direction, make sure we're not going out
      // of bounds
      if ((newCoords.x1 - calcWidth) < Bounds.Left) then
      begin
        // Moves the horizontal coordinates
        newCoords.x1 := Bounds.Left + calcWidth;
        newCoords.x2 := Bounds.Left;
      end
      else
      begin
        // Moves the last horizontal coordinate
        newCoords.x2 := newCoords.x1 - calcWidth;
      end;
    end;

    if (AKeepAspectRatio) then
    begin
      // Resizes the height based on the minimum value
      recalculateHeight := True;
    end;
  end;

  // Checks if the height is smaller than the minimum value
  if (((Abs(calcHeight) < MinHeight) and (MinHeight < fImageBitmap.Height)) or
    recalculateHeight) then
  begin
    // Resizes the height based on the minimum value
    calcHeight := MinHeight;

    if (SOUTH in Direction) then
    begin
      // If the motion is in a positive direction, make sure we're not going out
      // of bounds
      if ((newCoords.y1 + calcHeight) > Bounds.Bottom) then
      begin
        // Moves the vertical coordinates
        newCoords.y1 := Bounds.Bottom - calcHeight;
        newCoords.y2 := Bounds.Bottom;
      end
      else
      begin
        // Moves the last horizontal coordinate
        newCoords.y2 := newCoords.y1 + calcHeight;
      end;
    end
    else
    begin
      // If the motion is in a negative direction, make sure we're not going out
      // of bounds
      if ((newCoords.y1 - calcHeight) < Bounds.Top) then
      begin
        // Moves the vertical coordinates
        newCoords.y1 := Bounds.Top + calcHeight;
        newCoords.y2 := Bounds.Top;
      end
      else
      begin
        // Moves the last horizontal coordinate
        newCoords.y2 := newCoords.y1 - calcHeight;
      end;
    end;
  end;

  Result := newCoords;
end;

 { Applies the provided ratio to the coordinates based on direction and bounds  }
 { on both axes.                                                                }
function TBGRAImageManipulation.ApplyRatioToAxes(Coords: TCoord;
  Direction: TDirection; Bounds: TRect; ACropArea :TCropArea = Nil): TCoord;
var
  newCoords: TCoord;
  calcWidth, calcHeight: integer;
  RecalculatesOtherAxis,
  curKeepAspectRatio :Boolean;
  curRatio :TRatio;

begin
  // Gets coordinates
  newCoords := Coords;

  if (ACropArea<>nil)
  then curKeepAspectRatio :=ACropArea.getRealAspectRatio(curRatio)
  else begin
         curKeepAspectRatio :=Self.fKeepAspectRatio;
         curRatio :=Self.fRatio;
       end;

  // Check if movement is only vertical
  if ((fAnchorSelected = [NORTH]) or (fAnchorSelected = [SOUTH])) then
  begin
    // Vertical movement: keep current width
    if (curKeepAspectRatio) then
    begin
      // Calculate height
      calcHeight := newCoords.y2 - newCoords.y1;

      // Make sure we're not going out of bounds
      if (SOUTH in Direction) then
      begin
        if ((newCoords.y1 + calcHeight) > Bounds.Bottom) then
        begin
          calcHeight := Bounds.Bottom - newCoords.y1; // Limite height dimension
          newCoords.y2 := Bounds.Bottom;
        end;
      end
      else
      begin
        if ((newCoords.y1 + calcHeight) < Bounds.Top) then
        begin
          calcHeight := -(newCoords.y1 - Bounds.Top); // Limite height dimension
          newCoords.y2 := Bounds.Top;
        end;
      end;

      // Calculate the new width based on the proportion of height
      calcWidth := Trunc(abs(calcHeight) * (curRatio.Horizontal / curRatio.Vertical));

      // Make sure we're not going out of bounds
      if (fAnchorSelected = [NORTH]) then
      begin
        if ((newCoords.x1 - calcWidth) < Bounds.Left) then
        begin
          calcWidth := newCoords.x1 - Bounds.Left; // Limite width dimension
          newCoords.x2 := Bounds.Left;
          RecalculatesOtherAxis := True;
        end;
      end
      else
      begin
        if ((newCoords.x1 + calcWidth) > Bounds.Right) then
        begin
          calcWidth := Bounds.Right - newCoords.x1; // Limite width dimension
          newCoords.x2 := Bounds.Right;
          RecalculatesOtherAxis := True;
        end;
      end;

      // Apply calculated dimensions of width on height
      if {%H-}(RecalculatesOtherAxis) then
      begin
        if (calcHeight > 0) then
          calcHeight := Trunc(calcWidth * (curRatio.Vertical / curRatio.Horizontal))
        else
          calcHeight := -Trunc(calcWidth * (curRatio.Vertical / curRatio.Horizontal));

        newCoords.y2 := newCoords.y1 + calcHeight;
      end;
    end
    else
    begin
      // Calculate height
      calcHeight := newCoords.y2 - newCoords.y1;

      // Make sure we're not going out of bounds
      if (SOUTH in Direction) then
      begin
        if ((newCoords.y1 + calcHeight) > Bounds.Bottom) then
        begin
          calcHeight := Bounds.Bottom - newCoords.y1; // Limite height dimension
          newCoords.y2 := Bounds.Bottom;
        end;
      end
      else
      begin
        if ((newCoords.y1 + calcHeight) < Bounds.Top) then
        begin
          calcHeight := -(newCoords.y1 - Bounds.Top); // Limite height dimension
          newCoords.y2 := Bounds.Top;
        end;
      end;

      // Calculate width
      if (ACropArea <> Nil)
      then calcWidth := abs(ACropArea.ScaledArea.Right - ACropArea.ScaledArea.Left)
      else calcWidth := 16; //Check
    end;

    if (fAnchorSelected = [NORTH]) then
      newCoords.x2 := newCoords.x1 - calcWidth
    else
      newCoords.x2 := newCoords.x1 + calcWidth;
  end
  else
  // Check if movement is only horizontal
  if ((fAnchorSelected = [EAST]) or (fAnchorSelected = [WEST])) then
  begin
    // Horizontal movement: keep current height
    if (curKeepAspectRatio) then
    begin
      // Calculate width
      calcWidth := newCoords.x2 - newCoords.x1;

      // Make sure we're not going out of bounds
      if (EAST in Direction) then
      begin
        if ((newCoords.x1 + calcWidth) > Bounds.Right) then
        begin
          calcWidth := Bounds.Right - newCoords.x1; // Limite width dimension
          newCoords.x2 := Bounds.Right;
        end;
      end;

      if (WEST in Direction) then
      begin
        if ((newCoords.x1 + calcWidth) < Bounds.Left) then
        begin
          calcWidth := -(newCoords.x1 - Bounds.Left); // Limite width dimension
          newCoords.x2 := Bounds.Left;
        end;
      end;

      // Calculate the new height based on the proportion of width
      calcHeight := Trunc(abs(calcWidth) * (curRatio.Vertical / curRatio.Horizontal));

      // Make sure we're not going out of bounds
      if (fAnchorSelected = [WEST]) then
      begin
        if ((newCoords.y1 - calcHeight) < Bounds.Top) then
        begin
          calcHeight := newCoords.y1 - Bounds.Top; // Limite height dimension
          newCoords.y2 := Bounds.Top;
          RecalculatesOtherAxis := True;
        end;
      end
      else
      begin
        if ((newCoords.y1 + calcHeight) > Bounds.Bottom) then
        begin
          calcHeight := Bounds.Bottom - newCoords.y1; // Limite height dimension
          newCoords.y2 := Bounds.Bottom;
          RecalculatesOtherAxis := True;
        end;
      end;

      // Apply calculated dimensions of height on width
      if (RecalculatesOtherAxis) then
      begin
        if (calcWidth > 0) then
          calcWidth := Trunc(calcHeight * (curRatio.Horizontal / curRatio.Vertical))
        else
          calcWidth := -Trunc(calcHeight * (curRatio.Horizontal / curRatio.Vertical));

        newCoords.x2 := newCoords.x1 + calcWidth;
      end;
    end
    else
    begin
      // Calculate width
      calcWidth := newCoords.x2 - newCoords.x1;

      // Make sure we're not going out of bounds
      if (EAST in Direction) then
      begin
        if ((newCoords.x1 + calcWidth) > Bounds.Right) then
        begin
          calcWidth := Bounds.Right - newCoords.x1; // Limite width dimension
          newCoords.x2 := Bounds.Right;
        end;
      end;

      if (WEST in Direction) then
      begin
        if ((newCoords.x1 + calcWidth) < Bounds.Left) then
        begin
          calcWidth := -(newCoords.x1 - Bounds.Left); // Limite width dimension
          newCoords.x2 := Bounds.Left;
        end;
      end;

      // Calculate height
      if (ACropArea <> Nil)
      then calcHeight := abs(ACropArea.ScaledArea.Bottom - ACropArea.ScaledArea.Top)
      else calcHeight := 16; //Check
    end;

    if (fAnchorSelected = [WEST]) then
      newCoords.y2 := newCoords.y1 - calcHeight
    else
      newCoords.y2 := newCoords.y1 + calcHeight;
  end
  else
  begin
    // Diagonal movement
    if (curKeepAspectRatio) then
    begin
      // Calculate width
      calcWidth := newCoords.x2 - newCoords.x1;

      // Make sure we're not going out of bounds
      if (EAST in Direction) then
      begin
        if ((newCoords.x1 + calcWidth) > Bounds.Right) then
        begin
          calcWidth := Bounds.Right - newCoords.x1; // Limite width dimension
          newCoords.x2 := Bounds.Right;
        end;
      end;

      if (WEST in Direction) then
      begin
        if ((newCoords.x1 + calcWidth) < Bounds.Left) then
        begin
          calcWidth := -(newCoords.x1 - Bounds.Left); // Limite width dimension
          newCoords.x2 := Bounds.Left;
        end;
      end;

      // Calculate the new height based on the proportion of width
      if ((newCoords.y2 - newCoords.y1) > 0) then
        calcHeight := Trunc(abs(calcWidth) * (curRatio.Vertical / curRatio.Horizontal))
      else
        calcHeight := -Trunc(abs(calcWidth) * (curRatio.Vertical / curRatio.Horizontal));

      // Make sure we're not going out of bounds
      if (calcHeight > 0) then
      begin
        if (SOUTH in Direction) then
        begin
          if ((newCoords.y1 + calcHeight) > Bounds.Bottom) then
          begin
            calcHeight := Bounds.Bottom - newCoords.y1; // Limite height dimension
            newCoords.y2 := Bounds.Bottom;
            RecalculatesOtherAxis := True;
          end;
        end
        else
        begin
          if ((newCoords.y1 - calcHeight) < Bounds.Top) then
          begin
            calcHeight := newCoords.y1 - Bounds.Top; // Limite height dimension
            newCoords.y2 := Bounds.Top;
            RecalculatesOtherAxis := True;
          end;
        end;
      end
      else
      begin
        if (SOUTH in Direction) then
        begin
          if ((newCoords.y1 - calcHeight) > Bounds.Bottom) then
          begin
            calcHeight := newCoords.y1 - Bounds.Bottom; // Limite height dimension
            newCoords.y2 := Bounds.Bottom;
            RecalculatesOtherAxis := True;
          end;
        end
        else
        begin
          if ((newCoords.y1 + calcHeight) < Bounds.Top) then
          begin
            calcHeight := Bounds.Top - newCoords.y1; // Limite height dimension
            newCoords.y2 := Bounds.Top;
            RecalculatesOtherAxis := True;
          end;
        end;
      end;

      // Apply calculated dimensions of height on width
      if (RecalculatesOtherAxis) then
      begin
        if (calcWidth > 0) then
          calcWidth := Trunc(abs(calcHeight) * (curRatio.Horizontal / curRatio.Vertical))
        else
          calcWidth := -Trunc(abs(calcHeight) * (curRatio.Horizontal / curRatio.Vertical));

        newCoords.x2 := newCoords.x1 + calcWidth;
      end;
    end
    else
    begin
      // Calculate width
      calcWidth := newCoords.x2 - newCoords.x1;

      // Make sure we're not going out of bounds
      if (EAST in Direction) then
      begin
        if ((newCoords.x1 + calcWidth) > Bounds.Right) then
        begin
          calcWidth := Bounds.Right - newCoords.x1; // Limite width dimension
          newCoords.x2 := Bounds.Right;
        end;
      end;

      if (WEST in Direction) then
      begin
        if ((newCoords.x1 + calcWidth) < Bounds.Left) then
        begin
          calcWidth := -(newCoords.x1 - Bounds.Left); // Limite width dimension
          newCoords.x2 := Bounds.Left;
        end;
      end;

      // Calculate height
      calcHeight := newCoords.y2 - newCoords.y1;

      // Make sure we're not going out of bounds
      if (SOUTH in Direction) then
      begin
        if ((newCoords.y1 + calcHeight) > Bounds.Bottom) then
        begin
          calcHeight := Bounds.Bottom - newCoords.y1; // Limite height dimension
          newCoords.y2 := Bounds.Bottom;
        end;
      end;

      if (NORTH in Direction) then
      begin
        if ((newCoords.y1 + calcHeight) < Bounds.Top) then
        begin
          calcHeight := -(newCoords.y1 - Bounds.Top); // Limite height dimension
          newCoords.y2 := Bounds.Top;
        end;
      end;
    end;

    newCoords.x2 := newCoords.x1 + calcWidth;
    newCoords.y2 := newCoords.y1 + calcHeight;
  end;

  Result := newCoords;
end;

procedure TBGRAImageManipulation.ApplyRatioToArea(ACropArea :TCropArea);
var
   calcWidth, calcHeight :Integer;
   CropAreaRect, Bounds  :TRect;
   curRatio :TRatio;
   curKeepAspectRatio :Boolean;

begin
  if (ACropArea <> Nil) then
  begin
       CropAreaRect :=ACropArea.ScaledArea;
       Bounds := getImageRect(fResampledBitmap);

      // Calculate width
      calcWidth :=CropAreaRect.Right-CropAreaRect.Left;

      // Make sure we're not going out of bounds with Widht
      if ((CropAreaRect.Left+calcWidth)>Bounds.Right) then
      begin
          calcWidth :=Bounds.Right-CropAreaRect.Left; // Limite width dimension
          CropAreaRect.Right :=Bounds.Right;
      end;

      curKeepAspectRatio :=ACropArea.getRealAspectRatio(curRatio);

      if curKeepAspectRatio // Calculate the new height based on the proportion of width
      then calcHeight := Trunc(abs(calcWidth)*(curRatio.Vertical/curRatio.Horizontal));
      //else calcHeight := CropAreaRect.Height; //Raise an Exception ???

      // Make sure we're not going out of bounds with Height
      if ((CropAreaRect.Top+calcHeight) > Bounds.Bottom) then
      begin
           calcHeight :=Bounds.Bottom-CropAreaRect.Top;
           calcWidth :=Trunc(abs(calcHeight)*(curRatio.Horizontal/curRatio.Vertical));
      end;

      CropAreaRect.Right :=CropAreaRect.Left+calcWidth;
      CropAreaRect.Bottom :=CropAreaRect.Top+calcHeight;

      ACropArea.ScaledArea :=CropAreaRect;
  end;
end;

{ Calculate the maximun selection allowed                                      }
procedure TBGRAImageManipulation.CalcMaxSelection(ACropArea :TCropArea);
var
  ImageRect: TRect;
  newCoords: TCoord;
  Direction: TDirection;
  Bounds: TRect;
begin
  if (ACropArea <> Nil) then
  begin
       ImageRect := getImageRect(fImageBitmap);

       // Initiates coord
       with newCoords do
       begin
            x1 := 0;
            y1 := 0;

            x2 := ImageRect.Right - ImageRect.Left;
            y2 := ImageRect.Bottom - ImageRect.Top;
       end;

       // Determine direction
       Direction := getDirection(Point(newCoords.x1, newCoords.y1),
                                 Point(newCoords.x2, newCoords.y2));

       // Determines limite values
       with newCoords do
       begin
            x1 := 0;
            y1 := 0;
            x2 := ImageRect.Right - ImageRect.Left;
            y2 := ImageRect.Bottom - ImageRect.Top;
       end;
       Bounds := getImageRect(fResampledBitmap);

       // Apply the ratio
       newCoords := ApplyRatioToAxes(newCoords, Direction, Bounds);

       // Determines minimum value on both axes
       newCoords := ApplyDimRestriction(newCoords, Direction, Bounds, fKeepAspectRatio);

       ACropArea.ScaledArea := Rect(newCoords.x1, newCoords.y1, newCoords.x2, newCoords.y2);
  end;
end;

{ Calculate the Aspect Ratio for size limits}
procedure TBGRAImageManipulation.findSizeLimits;
var
  WorkRect: TRect;
begin
  // Find the working area of the component
  WorkRect := getWorkRect;

  with fSizeLimits do
  begin
    minWidth  := fAspectX;
    maxWidth  := WorkRect.Right - WorkRect.Left;
    minHeight := fAspectY;
    maxHeight := WorkRect.Bottom - WorkRect.Top;
  end;
end;

{ Get the direction of movement }
function TBGRAImageManipulation.getDirection(const Point1, Point2: TPoint): TDirection;
begin
  Result := [];

  if (Point1.X > Point2.X) then
    Result := Result + [WEST];

  if (Point1.X < Point2.X) then
    Result := Result + [EAST];

  if (Point1.Y > Point2.Y) then
    Result := Result + [NORTH];

  if (Point1.Y < Point2.Y) then
    Result := Result + [SOUTH];
end;

{ Get image rectangle }
function TBGRAImageManipulation.getImageRect(Picture: TBGRABitmap): TRect;
var
  calcWidth, calcHeight, finalWidth, finalHeight, imageWidth, imageHeight: integer;
  WorkRect: TRect;
begin
  // Determine picture size
  imageWidth  := Picture.Width;
  imageHeight := Picture.Height;

  // Determine Work rectangle to final size
  WorkRect := getWorkRect;
  finalWidth := WorkRect.Right - WorkRect.Left;
  finalHeight := WorkRect.Bottom - WorkRect.Top;

  // Recalculate image dimensions
  calcHeight := (finalWidth * imageHeight) div imageWidth;
  calcWidth  := finalWidth;

  if (calcHeight > finalHeight) then
  begin
    calcHeight := finalHeight;
    calcWidth  := (calcHeight * imageWidth) div imageHeight;
  end;

  with Result do
  begin
    Left := 0;
    Top  := 0;
    Right := calcWidth;
    Bottom := calcHeight;
  end;
end;

{ Get work area rectangle }
function TBGRAImageManipulation.getWorkRect: TRect;
begin
  // Get the coordinates of the control
  if (fVirtualScreen <> nil) then
    Result := Rect(0, 0, fVirtualScreen.Width, fVirtualScreen.Height)
  else
    Result := GetClientRect;

  // Remove the non-work areas from our work rectangle
  InflateRect(Result, -fBorderSize, -fBorderSize);
end;

{ Check if mouse is over any anchor }
function TBGRAImageManipulation.isOverAnchor(APoint :TPoint; var AnchorSelected :TDirection; var ACursor :TCursor):TCropArea;
var
   i :Integer;

   function _isOverAnchor(APoint: TPoint; Corner: TPoint): boolean;
   begin
        Result := ((APoint.X >= (Corner.X - AnchorSize)) and
               (APoint.X <= (Corner.X + AnchorSize)) and
               (APoint.Y >= (Corner.Y - AnchorSize)) and
               (APoint.Y <= (Corner.Y + AnchorSize)));
   end;

   function TestArea(rCropArea :TCropArea):TCropArea;
   var
      rCropRect,
      rCropRectI :TRect;

   begin
     Result :=nil;
     rCropRectI :=rCropArea.ScaledArea;
     InflateRect(rCropRectI, AnchorSize, AnchorSize);
     if ({$IFNDEF FPC}BGRAGraphics.{$ENDIF}PtInRect(rCropRectI, APoint)) then
     begin
          rCropRect :=rCropArea.ScaledArea;
          // Verifies that is positioned on an anchor
          // NW
          if (_isOverAnchor(APoint, rCropRect.TopLeft)) then
          begin
               AnchorSelected := [NORTH, WEST];
               ACursor := crSizeNW;
               Result :=rCropArea; exit;
          end;

          // W
          if (_isOverAnchor(APoint, Point(rCropRect.Left, rCropRect.Top +
             (rCropRect.Bottom - rCropRect.Top) div 2))) then
          begin
               AnchorSelected := [WEST];
               ACursor := crSizeWE;
               Result :=rCropArea; exit;
          end;

          // SW
          if (_isOverAnchor(APoint, Point(rCropRect.Left, rCropRect.Bottom))) then
          begin
               AnchorSelected := [SOUTH, WEST];
               ACursor := crSizeSW;
               Result :=rCropArea; exit;
          end;

          // S
          if (_isOverAnchor(APoint, Point(rCropRect.Left +
          ((rCropRect.Right - rCropRect.Left) div 2), rCropRect.Bottom))) then
          begin
               AnchorSelected := [SOUTH];
               ACursor := crSizeNS;
               Result :=rCropArea; exit;
          end;

          // SE
          if (_isOverAnchor(APoint, rCropRect.BottomRight)) then
          begin
               AnchorSelected := [SOUTH, EAST];
               ACursor := crSizeSE;
               Result :=rCropArea; exit;
          end;

          // E
          if (_isOverAnchor(APoint, Point(rCropRect.Right, rCropRect.Top +
             ((rCropRect.Bottom - rCropRect.Top) div 2)))) then
          begin
               AnchorSelected := [EAST];
               ACursor := crSizeWE;
               Result :=rCropArea; exit;
          end;

          // NE
          if (_isOverAnchor(APoint, Point(rCropRect.Right, rCropRect.Top))) then
          begin
               AnchorSelected := [NORTH, EAST];
               ACursor := crSizeNE;
               Result :=rCropArea; exit;
          end;

          // N
          if (_isOverAnchor(APoint, Point(rCropRect.Left +
             ((rCropRect.Right - rCropRect.Left) div 2), rCropRect.Top))) then
          begin
               AnchorSelected := [NORTH];
               ACursor := crSizeNS;
               Result :=rCropArea; exit;
          end;

          // Verifies that is positioned on a cropping area
          if (AnchorSelected = []) then
          begin
               if ((APoint.X >= rCropRect.Left) and (APoint.X <= rCropRect.Right) and
               (APoint.Y >= rCropRect.Top) and (APoint.Y <= rCropRect.Bottom)) then
               begin
                    AnchorSelected := [NORTH, SOUTH, EAST, WEST];
                    ACursor := crSizeAll;
                    Result :=rCropArea; exit;
               end;
          end;
      end;
   end;

begin
     AnchorSelected :=[];
     ACursor :=crDefault;
     Result :=Nil;
     if (SelectedCropArea=nil)
     then for i:=rCropAreas.Count-1 downto 0 do //downto so respect ZOrder
          begin
            Result :=TestArea(rCropAreas[i]);
            if (Result<>nil) then break;
          end
     else begin
            //Gives precedence to the selected area
            Result :=TestArea(SelectedCropArea);
            if (Result=nil) then
            for i:=rCropAreas.Count-1 downto 0 do
            begin
              if (rCropAreas[i]<>SelectedCropArea) then
              begin
                Result :=TestArea(rCropAreas[i]);
                if (Result<>nil) then break;
              end;
            end;
          end;
end;

procedure TBGRAImageManipulation.CreateEmptyImage;
begin
  fImageBitmap.ResolutionUnit :=ruPixelsPerInch;
  fImageBitmap.ResolutionX :=Self.PixelsPerInch;
  fImageBitmap.ResolutionY :=fImageBitmap.ResolutionX;
  fImageBitmap.SetSize(EmptyImage.Width, EmptyImage.Height);
  fImageBitmap.FillTransparent;
end;

procedure TBGRAImageManipulation.CreateResampledBitmap;
var
  DestinationRect: TRect;
  tempBitmap: TBGRACustomBitmap;

begin
  // Get the resampled dimensions to scale image for draw in component
  DestinationRect := getImageRect(fImageBitmap);

  // Recreate resampled bitmap
  fResampledBitmap.SetSize(DestinationRect.Right - DestinationRect.Left,
                           DestinationRect.Bottom - DestinationRect.Top);

  if Self.Empty
  then fResampledBitmap.FillTransparent
  else try
          tempBitmap  := fImageBitmap.Resample(DestinationRect.Right - DestinationRect.Left,
                                               DestinationRect.Bottom - DestinationRect.Top, rmFineResample);
          fResampledBitmap.BlendImage(0, 0, tempBitmap, boLinearBlend);
       finally
         tempBitmap.Free;
       end;
end;

class function TBGRAImageManipulation.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 320;
  Result.CY := 240;
end;

procedure TBGRAImageManipulation.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: boolean);
begin
  PreferredWidth  := 320;
  PreferredHeight := 240;
end;

procedure TBGRAImageManipulation.Loaded;
begin
  {$ifopt D+}
   DebugLn('Loaded '+BoolToStr(csLoading in ComponentState, True));
  {$endif}

  inherited Loaded;

  if Self.Empty then
  begin
    CreateEmptyImage;
    CreateResampledBitmap;
  end;

  // Force Render Struct
//  RenderBackground;
//  Render;

  {$ifopt D+}
   DebugLn('Loaded done');
  {$endif}
end;

 { ============================================================================ }
 { =====[ Component Definition ]=============================================== }
 { ============================================================================ }

constructor TBGRAImageManipulation.Create(AOwner: TComponent);
var
   fGCD     :integer;

begin
  {$ifopt D+}
   DebugLn('Create');
  {$endif}

  inherited Create(AOwner);

  // Set default component values
  with GetControlClassDefaultSize do SetInitialBounds(0, 0, CX, CY);

  // Default property values
  fAnchorSize := 5;
  fAnchorSelected := [];
  fBorderSize := 2;
  fAspectRatio := '3:4';
  fAspectX := 3;
  fAspectY := 4;
  fKeepAspectRatio := True;

  // Default control values
  ControlStyle := ControlStyle + [csReplicatable];
  Cursor := crDefault;

  // Calculate the ratio
  fGCD := getGCD(fAspectX, fAspectY);

  // Determine the ratio of scale per axle
  with fRatio do
  begin
    Horizontal := fAspectX div fGCD;
    Vertical := fAspectY div fGCD;
  end;

  // Find size limits
  findSizeLimits;

  // Create the Image Bitmap
  fImageBitmap := TBGRABitmap.Create;

  // Create the Resampled Bitmap
  fResampledBitmap := TBGRABitmap.Create;

  // Create the Background
  fBackground := TBGRABitmap.Create(Width, Height);

  // Create render surface
  fVirtualScreen := TBGRABitmap.Create(Width, Height);
  rOpacity:= 128;

  rEmptyImage :=TBGRAEmptyImage.Create(Self);
  rNewCropAreaDefault :=TBGRANewCropAreaDefault.Create(Self);

  // Initialize crop area
  rCropAreas :=TCropAreaList.Create(Self);
  rCropAreas.Name:='CropAreas';
  rNewCropArea :=Nil;
  rSelectedCropArea :=Nil;

  fMouseCaught := False;

  {$ifopt D+}
   DebugLn('Create done');
  {$endif}
end;

destructor TBGRAImageManipulation.Destroy;
begin
  fImageBitmap.Free;
  fResampledBitmap.Free;
  fBackground.Free;
  fVirtualScreen.Free;
  rEmptyImage.Free;
  rNewCropAreaDefault.Free;
  rCropAreas.Free;

  inherited Destroy;
end;

procedure TBGRAImageManipulation.Invalidate;
begin
  {$ifopt D+}
   DebugLn('Invalidate');
  {$endif}

  inherited Invalidate;

  {$ifopt D+}
   DebugLn('Invalidate done');
  {$endif}
end;

procedure TBGRAImageManipulation.Paint;
begin
  {$ifopt D+}
   DebugLn('Paint');
  {$endif}

  inherited Paint;

  {$ifopt D+}
   DebugLn('Paint inherited done '+BoolToStr(fVirtualScreen<>nil, True)+' '+BoolToStr(Canvas<>nil, True));
  {$endif}

  fVirtualScreen.Draw(Canvas, 0, 0, True);

  {$ifopt D+}
   DebugLn('Paint done');
  {$endif}
end;

{ This function repaint the background only when necessary to avoid unnecessary
  redraws. Contain a function called DrawCheckers that draws the Background like
  checkers game. Also included was a function that draws 3D effects changed to
  allow color changes. }
procedure TBGRAImageManipulation.RenderBackground;

  procedure DrawCheckers(bmp: TBGRABitmap; ARect: TRect);
  const
    tx = 8;
    ty = 8;
  var
    xb, yb, xdest, ydest, nbx, nby: integer;
    oddColor, evenColor: TBGRAPixel;
  begin
    oddColor := BGRA(220, 220, 220);
    evenColor := BGRA(255, 255, 255);
    bmp.ClipRect := ARect;
    xdest := ARect.Left;
    nbx := ((ARect.Right - ARect.Left) + tx - 1) div tx;
    nby := ((ARect.Bottom - ARect.Top) + ty - 1) div ty;
    for xb := 0 to nbx - 1 do
    begin
      ydest := ARect.Top;
      for yb := 0 to nby - 1 do
      begin
        if odd(xb + yb) then
          bmp.FillRect(xdest, ydest, xdest + tx, ydest + ty, oddColor, dmSet)
        else
          bmp.FillRect(xdest, ydest, xdest + tx, ydest + ty, evenColor, dmSet);
        Inc(ydest, ty);
      end;
      Inc(xdest, tx);
    end;
    bmp.NoClip;
  end;

var
  Border: TRect;
  Grad: TBGRAGradientScanner;
begin
  {$ifopt D+}
   DebugLn('RenderBackground');
  {$endif}

  // Draw the outer bevel
  Border := Rect(0, 0, fVirtualScreen.Width, fVirtualScreen.Height);

  // Draw the rectangle around image
  if (fBorderSize > 2) then
  begin
    // Draw the border gradient
    Grad := TBGRAGradientScanner.Create(BGRA(245, 245, 245),
      BGRA(205, 204, 203), gtLinear, PointF(0, 0), PointF(0, fBackground.Height));
    fBackground.FillRect(0, 0, fBackground.Width, fBorderSize - 2, Grad, dmSet);
    fBackground.FillRect(0, fBorderSize - 2, fBorderSize - 2,
      fBackground.Height - fBorderSize + 2, Grad, dmSet);
    fBackground.FillRect(fBackground.Width - fBorderSize + 2, fBorderSize - 2,
      fBackground.Width, fBackground.Height - fBorderSize + 2,
      Grad, dmSet);
    fBackground.FillRect(0, fBackground.Height - fBorderSize + 2,
      fBackground.Width, fBackground.Height, Grad, dmSet);
    Grad.Free;
    InflateRect(Border, -(fBorderSize - 2), -(fBorderSize - 2));
  end;

  // Draw 3D border
  fBackground.CanvasBGRA.Frame3D(Border, 1, bvLowered,
    clBtnHighlight, cl3DDkShadow);
  fBackground.CanvasBGRA.Frame3D(Border, 1, bvLowered,
    cl3DLight, clBtnShadow);

  DrawCheckers(fBackground, Border);

  {$ifopt D+}
   DebugLn('RenderBackground done');
  {$endif}
end;

{ Resize the component, recalculating the proportions }
procedure TBGRAImageManipulation.DoOnResize;

  function min(const Value: integer; const MinValue: integer): integer;
  begin
    if (Value < MinValue) then
      Result := MinValue
    else
      Result := Value;
  end;

var
  i              :Integer;
  curCropArea    :TCropArea;

begin
  {$ifopt D+}
   DebugLn('DoOnResize '+BoolToStr(fVirtualScreen<>nil, True));
  {$endif}

  if (fVirtualScreen <> nil) then
  begin
    fVirtualScreen.SetSize(min(Self.Width, (fBorderSize * 2 + fAnchorSize + fMinWidth)),
      min(Self.Height, (fBorderSize * 2 + fAnchorSize + fMinHeight)));
    fVirtualScreen.InvalidateBitmap;

    // Resize background
    fBackground.SetSize(fVirtualScreen.Width, fVirtualScreen.Height);

    if Self.Empty then CreateEmptyImage;

    CreateResampledBitmap;

    for i:=0 to rCropAreas.Count-1 do
    begin
      curCropArea :=rCropAreas[i];
      curCropArea.CalculateScaledAreaFromArea;

      if curCropArea.isNullSize then
      begin
        // A Null-size crop selection (delete it or assign max size?)
        //CalcMaxSelection(curCropArea);
      end;
    end;

    // Force Render Struct
    RenderBackground;
    Render;
  end;

  inherited DoOnResize;

  {$ifopt D+}
   DebugLn('DoOnResize done');
  {$endif}
end;

{ Function responsible for rendering the content of the component, including
  the selection border and anchors. The selected area is painted with a
  different transparency level for easy viewing of what will be cut. }
procedure TBGRAImageManipulation.Render;
var
  WorkRect, emptyRect: TRect;
  Mask: TBGRABitmap;
  BorderColor, SelectColor,
  FillColor, IcoColor: TBGRAPixel;
  curCropArea :TCropArea;
  curCropAreaRect :TRect;
  i: Integer;
  curTxt: String;
  TextS: TTextStyle;

begin
  {$ifopt D+}
   DebugLn('Render');
  {$endif}

  // Draw background
  fVirtualScreen.BlendImage(0, 0, fBackground, boLinearBlend);

  // Find the working area of the component
  WorkRect := getWorkRect;

  try
    //Colors
    BorderColor := BGRAWhite;
    SelectColor := BGRA(255, 255, 0, 255);
    FillColor := BGRA(255, 255, 0, rOpacity);

    //Create Mask area
    Mask := TBGRABitmap.Create(WorkRect.Right - WorkRect.Left, WorkRect.Bottom - WorkRect.Top, BGRA(0, 0, 0, rOpacity));

    //Text Style and Font
    TextS.Alignment:=taCenter;
    TextS.SystemFont:=True;
    TextS.Layout:=tlCenter;
    TextS.SingleLine:=True;
    Mask.FontHeight:=10;
    Mask.FontStyle:=[fsBold];

    // Draw image if not empty, else draw empty image borders
    if Self.Empty
    then begin
           if rEmptyImage.ShowBorder then
           begin
             emptyRect :=Rect(0,0,fResampledBitmap.Width-1, fResampledBitmap.Height-1);
             Mask.CanvasBGRA.Frame3d(emptyRect, 1, bvRaised, BGRA(255, 255, 255, 180), BGRA(0, 0, 0, 160));
             //Mask.Rectangle(emptyRect, BorderColor, BGRAPixelTransparent); //wich one?
           end;
         end
    else fVirtualScreen.BlendImage(WorkRect.Left, WorkRect.Top, fResampledBitmap, boLinearBlend);

    // Render the Crop Areas
    for i:=0 to rCropAreas.Count-1 do
    begin
      curCropArea :=rCropAreas[i];
      curCropAreaRect :=curCropArea.ScaledArea;

      if (curCropArea = SelectedCropArea)
      then begin
             BorderColor := BGRA(255, 0, 0, 255);
             IcoColor :=BorderColor;
           end
      else begin
             if (curCropArea = rNewCropArea)
             then BorderColor := BGRA(255, 0, 255, 255)
             else BorderColor := curCropArea.BorderColor;

             IcoColor :=SelectColor;
           end;

      Mask.EraseRectAntialias(curCropAreaRect.Left, curCropAreaRect.Top, curCropAreaRect.Right-1,
                              curCropAreaRect.Bottom-1, 255);

      // Draw a selection box
      with Rect(curCropAreaRect.Left, curCropAreaRect.Top, curCropAreaRect.Right-1, curCropAreaRect.Bottom-1) do
          Mask.DrawPolyLineAntialias([Point(Left, Top), Point(Right, Top), Point(Right, Bottom), Point(Left, Bottom), Point(Left, Top)],
          BorderColor, BGRAPixelTransparent, 1, False);

      //Draw Icons
      { #todo 1 -oMaxM : Draw Other Icons }
      if (cIcoIndex in curCropArea.Icons) then
      begin
        curTxt:= IntToStr(curCropArea.getIndex);

        Mask.EllipseAntialias(curCropAreaRect.Right-12, curCropAreaRect.Top+12, 4, 4, IcoColor, 8);

       (* Shadow?
         Mask.TextRect(Rect(curCropAreaRect.Right-18, curCropAreaRect.Top+2, curCropAreaRect.Right-4, curCropAreaRect.Top+24),
           curCropAreaRect.Right-10, curCropAreaRect.Top+14,
           curTxt, TextS, BGRAWhite); *)
        Mask.TextRect(Rect(curCropAreaRect.Right-18, curCropAreaRect.Top+2, curCropAreaRect.Right-4, curCropAreaRect.Top+24),
           curCropAreaRect.Right-12, curCropAreaRect.Top+12,
           curTxt, TextS, BGRABlack);
      end;

      // Draw anchors
      BorderColor := BGRABlack;

      // NW
      Mask.Rectangle(curCropAreaRect.Left-fAnchorSize, curCropAreaRect.Top-fAnchorSize,
          curCropAreaRect.Left+fAnchorSize+1, curCropAreaRect.Top+fAnchorSize+1,
          BorderColor, FillColor, dmSet);

      // W
      Mask.Rectangle(curCropAreaRect.Left-fAnchorSize,
          (curCropAreaRect.Top+((curCropAreaRect.Bottom - curCropAreaRect.Top) div 2))-fAnchorSize,
          curCropAreaRect.Left+fAnchorSize+1,
          (curCropAreaRect.Top+((curCropAreaRect.Bottom - curCropAreaRect.Top) div 2))+fAnchorSize+1,
          BorderColor, FillColor, dmSet);

      // SW
      Mask.Rectangle(curCropAreaRect.Left-fAnchorSize, curCropAreaRect.Bottom-fAnchorSize-1,
          curCropAreaRect.Left+fAnchorSize+1, curCropAreaRect.Bottom+fAnchorSize,
          BorderColor, FillColor, dmSet);

      // S
      if ((fAnchorSelected = [NORTH]) and (curCropAreaRect.Top < curCropAreaRect.Bottom) and
            (fStartPoint.Y = curCropAreaRect.Top)) or ((fAnchorSelected = [NORTH]) and
            (curCropAreaRect.Top > curCropAreaRect.Bottom) and (fStartPoint.Y = curCropAreaRect.Top)) or
           ((fAnchorSelected = [SOUTH]) and (curCropAreaRect.Top < curCropAreaRect.Bottom) and
            (fStartPoint.Y = curCropAreaRect.Top)) or ((fAnchorSelected = [SOUTH]) and
            (curCropAreaRect.Top > curCropAreaRect.Bottom) and (fStartPoint.Y = curCropAreaRect.Top))
      then Mask.Rectangle((curCropAreaRect.Left+((curCropAreaRect.Right-curCropAreaRect.Left) div 2))-fAnchorSize,
               curCropAreaRect.Bottom-fAnchorSize-1, (curCropAreaRect.Left+((curCropAreaRect.Right - curCropAreaRect.Left) div 2))+fAnchorSize+1,
               curCropAreaRect.Bottom+fAnchorSize,
               BorderColor, SelectColor, dmSet)
      else Mask.Rectangle((curCropAreaRect.Left+((curCropAreaRect.Right-curCropAreaRect.Left) div 2))-fAnchorSize,
               curCropAreaRect.Bottom-fAnchorSize-1, (curCropAreaRect.Left+((curCropAreaRect.Right-curCropAreaRect.Left) div 2))+fAnchorSize+1,
               curCropAreaRect.Bottom+fAnchorSize,
               BorderColor, FillColor, dmSet);

      // SE
      if ((fAnchorSelected = [NORTH, WEST]) and
           ((curCropAreaRect.Left > curCropAreaRect.Right) and (curCropAreaRect.Top > curCropAreaRect.Bottom))) or
           ((fAnchorSelected = [NORTH, WEST]) and
           ((curCropAreaRect.Left < curCropAreaRect.Right) and (curCropAreaRect.Top < curCropAreaRect.Bottom))) or
           ((fAnchorSelected = [NORTH, WEST]) and
           ((curCropAreaRect.Left > curCropAreaRect.Right) and (curCropAreaRect.Top < curCropAreaRect.Bottom))) or
           ((fAnchorSelected = [NORTH, WEST]) and
           ((curCropAreaRect.Left < curCropAreaRect.Right) and (curCropAreaRect.Top > curCropAreaRect.Bottom))) or
           ((fAnchorSelected = [NORTH, EAST]) and
           ((curCropAreaRect.Left < curCropAreaRect.Right) and (curCropAreaRect.Top > curCropAreaRect.Bottom))) or
           ((fAnchorSelected = [NORTH, EAST]) and
           ((curCropAreaRect.Left > curCropAreaRect.Right) and (curCropAreaRect.Top < curCropAreaRect.Bottom))) or
           ((fAnchorSelected = [NORTH, EAST]) and
           ((curCropAreaRect.Left < curCropAreaRect.Right) and (curCropAreaRect.Top < curCropAreaRect.Bottom))) or
           ((fAnchorSelected = [NORTH, EAST]) and
           ((curCropAreaRect.Left > curCropAreaRect.Right) and (curCropAreaRect.Top > curCropAreaRect.Bottom))) or
           ((fAnchorSelected = [SOUTH, EAST]) and
           ((curCropAreaRect.Left > curCropAreaRect.Right) and (curCropAreaRect.Top > curCropAreaRect.Bottom))) or
           ((fAnchorSelected = [SOUTH, EAST]) and
           ((curCropAreaRect.Left < curCropAreaRect.Right) and (curCropAreaRect.Top < curCropAreaRect.Bottom))) or
           ((fAnchorSelected = [SOUTH, EAST]) and
           ((curCropAreaRect.Left > curCropAreaRect.Right) and (curCropAreaRect.Top < curCropAreaRect.Bottom))) or
           ((fAnchorSelected = [SOUTH, EAST]) and
           ((curCropAreaRect.Left < curCropAreaRect.Right) and (curCropAreaRect.Top > curCropAreaRect.Bottom))) or
           ((fAnchorSelected = [SOUTH, WEST]) and
           ((curCropAreaRect.Left > curCropAreaRect.Right) and (curCropAreaRect.Top < curCropAreaRect.Bottom))) or
           ((fAnchorSelected = [SOUTH, WEST]) and
           ((curCropAreaRect.Left < curCropAreaRect.Right) and (curCropAreaRect.Top > curCropAreaRect.Bottom))) or
           ((fAnchorSelected = [SOUTH, WEST]) and
           ((curCropAreaRect.Left > curCropAreaRect.Right) and (curCropAreaRect.Top > curCropAreaRect.Bottom))) or
           ((fAnchorSelected = [SOUTH, WEST]) and
           ((curCropAreaRect.Left < curCropAreaRect.Right) and (curCropAreaRect.Top < curCropAreaRect.Bottom)))
      then Mask.Rectangle(curCropAreaRect.Right-fAnchorSize-1,
               curCropAreaRect.Bottom-fAnchorSize-1, curCropAreaRect.Right+fAnchorSize, curCropAreaRect.Bottom+fAnchorSize,
               BorderColor, SelectColor, dmSet)
      else Mask.Rectangle(curCropAreaRect.Right-fAnchorSize-1,
               curCropAreaRect.Bottom-fAnchorSize-1, curCropAreaRect.Right+fAnchorSize, curCropAreaRect.Bottom+fAnchorSize,
               BorderColor, FillColor, dmSet);

      // E
      if ((fAnchorSelected = [EAST]) and (curCropAreaRect.Left < curCropAreaRect.Right) and
            (fStartPoint.X = curCropAreaRect.Left)) or ((fAnchorSelected = [EAST]) and
            (curCropAreaRect.Left > curCropAreaRect.Right) and (fStartPoint.X = curCropAreaRect.Left)) or
           ((fAnchorSelected = [WEST]) and (curCropAreaRect.Left < curCropAreaRect.Right) and
            (fStartPoint.X = curCropAreaRect.Left)) or ((fAnchorSelected = [WEST]) and
            (curCropAreaRect.Left > curCropAreaRect.Right) and (fStartPoint.X = curCropAreaRect.Left))
      then Mask.Rectangle(curCropAreaRect.Right-fAnchorSize-1,
             (curCropAreaRect.Top+((curCropAreaRect.Bottom - curCropAreaRect.Top) div 2))-fAnchorSize,
              curCropAreaRect.Right+fAnchorSize, (curCropAreaRect.Top+((curCropAreaRect.Bottom-curCropAreaRect.Top) div 2))+fAnchorSize+1,
             BorderColor, SelectColor, dmSet)
      else Mask.Rectangle(curCropAreaRect.Right-fAnchorSize-1, (curCropAreaRect.Top+((curCropAreaRect.Bottom-curCropAreaRect.Top) div 2))-fAnchorSize,
              curCropAreaRect.Right+fAnchorSize, (curCropAreaRect.Top+((curCropAreaRect.Bottom-curCropAreaRect.Top) div 2))+fAnchorSize+1,
              BorderColor, FillColor, dmSet);

      // NE
      Mask.Rectangle(curCropAreaRect.Right-fAnchorSize-1, curCropAreaRect.Top-fAnchorSize,
          curCropAreaRect.Right+fAnchorSize, curCropAreaRect.Top+fAnchorSize+1,
          BorderColor, FillColor, dmSet);

      // N
      Mask.Rectangle((curCropAreaRect.Left+((curCropAreaRect.Right-curCropAreaRect.Left) div 2))-fAnchorSize,
          curCropAreaRect.Top-fAnchorSize, (curCropAreaRect.Left+((curCropAreaRect.Right-curCropAreaRect.Left) div 2))+fAnchorSize+1,
          curCropAreaRect.Top+fAnchorSize+1,
          BorderColor, FillColor, dmSet);
    end;
  finally
    fVirtualScreen.BlendImage(WorkRect.Left, WorkRect.Top, Mask, boLinearBlend);
    Mask.Free;
  end;

  {$ifopt D+}
   DebugLn('Render done');
  {$endif}
end;

procedure TBGRAImageManipulation.Render_Invalidate;
begin
  Render;
  Invalidate;
end;

 { ============================================================================ }
 { =====[ Properties Manipulation ]============================================ }
 { ============================================================================ }

function TBGRAImageManipulation.getAnchorSize: byte;
begin
  Result := fAnchorSize * 2 + 1;
end;

function TBGRAImageManipulation.getPixelsPerInch: Integer;
begin
  if (Owner is TCustomForm)
  then Result :=TCustomForm(Owner).PixelsPerInch
  else Result :=96;
end;

procedure TBGRAImageManipulation.setAnchorSize(const Value: byte);
const
  MinSize = 3;
  MaxSize = 9;
begin
  if (Value <> getAnchorSize) then
  begin
    if (Value < MinSize) then raise ERangeError.CreateFmt(SAnchorSizeIsTooSmall, [Value, MinSize, MaxSize]);
    if (Value > MaxSize) then raise ERangeError.CreateFmt(SAnchorSizeIsTooLarge, [Value, MinSize, MaxSize]);
    if ((Value mod 2) = 0) then raise EInvalidArgument.CreateFmt(SAnchorSizeIsNotOdd, [Value]);

    fAnchorSize:= (Value div 2);
    if not(csLoading in ComponentState) then Render_Invalidate;
  end;
end;

function TBGRAImageManipulation.getEmpty: boolean;
begin
  Result:= fImageBitmap.Empty or (fImageBitmap.Width = 0) or (fImageBitmap.Height = 0);
end;

function TBGRAImageManipulation.getResampledBitmap(ACropArea :TCropArea = Nil; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result := fImageBitmap;
  if not (fImageBitmap.Empty) then
  begin
    if (ACropArea = Nil) then ACropArea:= Self.SelectedCropArea;
    if (ACropArea <> Nil) then Result:= ACropArea.getResampledBitmap(ACopyProperties);
  end;
end;

function TBGRAImageManipulation.getBitmap(ACropArea :TCropArea = Nil; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result := fImageBitmap;
  if not (fImageBitmap.Empty) then
  begin
    if (ACropArea = Nil) then ACropArea:= Self.SelectedCropArea;
    if (ACropArea <> Nil) then Result :=ACropArea.getBitmap(ACopyProperties);
  end;
end;

procedure TBGRAImageManipulation.setBitmap(const Value: TBGRABitmap);
var
  curCropArea: TCropArea;
  i: Integer;

begin
    try
      if (Value = nil) or Value.Empty or (Value.Width = 0) or (Value.Height = 0)
      then CreateEmptyImage
      else fImageBitmap.Assign(Value, True); // Associate the new bitmap

      CreateResampledBitmap;

      for i:=0 to rCropAreas.Count-1 do
      begin
        curCropArea :=rCropAreas[i];
        curCropArea.CalculateScaledAreaFromArea;

        if curCropArea.isNullSize then
        begin
          // A Null-size crop selection (delete it or assign max size?)
          //CalcMaxSelection(curCropArea);
        end;
      end;

    finally
      if not(csLoading in ComponentState) then Render_Invalidate;
    end;
end;

procedure TBGRAImageManipulation.rotateLeft(ACopyProperties: Boolean=False);
var
  TempBitmap: TBGRACustomBitmap;
  curCropArea :TCropArea;
  i               :Integer;

begin
  try
    // Prevent empty image
    if Self.Empty then exit;

    // Rotate bitmap
    TempBitmap := fImageBitmap.RotateCCW(ACopyProperties);
    fImageBitmap.Assign(TempBitmap);

    CreateResampledBitmap;

    { #todo -oMaxM : Rotate the Crop Areas? a bool published property? }
    for i:=0 to rCropAreas.Count-1 do
    begin
      curCropArea :=rCropAreas[i];
      curCropArea.CalculateScaledAreaFromArea;

      if curCropArea.isNullSize then
      begin
        // A Null-size crop selection (delete it or assign max size?)
        //CalcMaxSelection(curCropArea);
      end;
    end;

  finally
    Render_Invalidate;
    TempBitmap.Free;
  end;
end;

procedure TBGRAImageManipulation.rotateRight(ACopyProperties: Boolean=False);
var
  TempBitmap: TBGRACustomBitmap;
  curCropArea :TCropArea;
  i               :Integer;

begin
  try
    // Prevent empty image
    if Self.Empty then exit;

    // Rotate bitmap
    TempBitmap := fImageBitmap.RotateCW(ACopyProperties);
    fImageBitmap.Assign(TempBitmap);

    CreateResampledBitmap;

    { #todo -oMaxM : Rotate the Crop Areas? a bool published property? }
    for i:=0 to rCropAreas.Count-1 do
    begin
      curCropArea :=rCropAreas[i];
      curCropArea.CalculateScaledAreaFromArea;

      if curCropArea.isNullSize then
      begin
        // A Null-size crop selection (delete it or assign max size?)
        //CalcMaxSelection(curCropArea);
      end;
    end;

  finally
    Render_Invalidate;
    TempBitmap.Free;
  end;
end;

procedure TBGRAImageManipulation.tests;
begin
  // Self.AutoSize:=False;
  // Render;
  // Refresh;
end;

function TBGRAImageManipulation.addCropArea(AArea: TRectF; AAreaUnit: TResolutionUnit;
                                            AUserData: Integer): TCropArea;
var
   newCropArea :TCropArea;

begin
  try
     newCropArea :=TCropArea.Create(Self, AArea, AAreaUnit, AUserData);

     newCropArea.BorderColor:= BGRAWhite;
     rNewCropAreaDefault.CopyPropertiesToArea(newCropArea);

     rCropAreas.add(newCropArea);

     if (rSelectedCropArea = nil) then rSelectedCropArea :=newCropArea;

     newCropArea.CalculateScaledAreaFromArea;

     Result :=newCropArea;
  except
     if (newCropArea <> Nil) then newCropArea.Free;

     Result :=Nil;
  end;

  Render_Invalidate;
end;

function TBGRAImageManipulation.addScaledCropArea(AArea: TRect; AUserData: Integer): TCropArea;
begin
  Result :=Self.addCropArea(RectF(0,0,0,0), rNewCropAreaDefault.rResolutionUnit, AUserData);
  Result.ScaledArea :=AArea;

  if (fMouseCaught) then Result.CalculateAreaFromScaledArea;

  Render_Invalidate;
end;

procedure TBGRAImageManipulation.delCropArea(ACropArea: TCropArea);
var
   curIndex, newIndex :Integer;

begin
  if (ACropArea <> Nil) then
  begin
    curIndex :=rCropAreas.IndexOf(ACropArea);

    //determines the new SelectedCropArea
    if (ACropArea = SelectedCropArea) then
    begin
      if (rCropAreas.Count = 1)
      then SelectedCropArea :=nil
      else begin
             newIndex :=curIndex-1;
             if (newIndex < 0)
             then newIndex :=rCropAreas.Count-1;
             SelectedCropArea :=rCropAreas.items[newIndex];
           end;
    end;

    rCropAreas.Delete(curIndex);

    Render_Invalidate;
  end;
end;

procedure TBGRAImageManipulation.clearCropAreas;
begin
  rCropAreas.Clear;
  Render_Invalidate;
end;

procedure TBGRAImageManipulation.getAllResampledBitmaps(ACallBack: TgetAllBitmapsCallback; AUserData:Integer; ACopyProperties: Boolean=False);
var
   i :Integer;
   curBitmap :TBGRABitmap;

begin
  //Get Resampled Bitmap of each CropArea and pass it to CallBack
  for i:=0 to rCropAreas.Count-1 do
  try
     curBitmap :=rCropAreas[i].getResampledBitmap(ACopyProperties);
     ACallBack(curBitmap, rCropAreas[i], AUserData);

  finally
    if (curBitmap<>nil) then curBitmap.Free;
  end;
end;

procedure TBGRAImageManipulation.getAllBitmaps(ACallBack: TgetAllBitmapsCallback; AUserData:Integer; ACopyProperties: Boolean=False);
var
   i :Integer;
   curBitmap :TBGRABitmap;

begin
  //Get Bitmap of each CropArea and pass it to CallBack
  for i:=0 to rCropAreas.Count-1 do
  try
     curBitmap :=rCropAreas[i].getBitmap(ACopyProperties);
     ACallBack(curBitmap, rCropAreas[i], AUserData);

  finally
    if (curBitmap<>nil) then curBitmap.Free;
  end;
end;

procedure TBGRAImageManipulation.SetEmptyImageSizeToCropAreas(ReduceLarger: Boolean);
var
   i :Integer;
   curCropAreaRect :TRectF;
   curCropArea :TCropArea;
   mWidth, mHeight:Single;
   xRatio, yRatio, resX :Single;

begin
  if Self.Empty and (rCropAreas.Count>0) then
  begin
     if ReduceLarger
     then begin
            mWidth:=0;
            mHeight:=0;
          end
     else begin
            mWidth:=EmptyImage.ResolutionWidth;
            mHeight:=EmptyImage.ResolutionHeight;
            if (mWidth=0) or (mHeight=0) then
            begin
              mWidth :=ResolutionUnitConvert(fImageBitmap.Width, ruNone, EmptyImage.ResolutionUnit, Self.PixelsPerInch);
              mHeight :=ResolutionUnitConvert(fImageBitmap.Height, ruNone, EmptyImage.ResolutionUnit, Self.PixelsPerInch);
            end;
          end;

     for i:=0 to rCropAreas.Count-1 do
     begin
       curCropArea :=rCropAreas[i];
       curCropAreaRect :=curCropArea.Area;

       curCropAreaRect.Right :=ResolutionUnitConvert(curCropAreaRect.Right, curCropArea.rAreaUnit,
                                                     EmptyImage.ResolutionUnit, Self.PixelsPerInch);
       curCropAreaRect.Bottom :=ResolutionUnitConvert(curCropAreaRect.Bottom, curCropArea.rAreaUnit,
                                                     EmptyImage.ResolutionUnit, Self.PixelsPerInch);

        if (curCropAreaRect.Right > mWidth)
        then mWidth :=curCropAreaRect.Right;
        if (curCropAreaRect.Bottom > mHeight)
        then mHeight :=curCropAreaRect.Bottom;
     end;

     EmptyImage.ResolutionWidth :=mWidth;
     EmptyImage.ResolutionHeight :=mHeight;
     Resize;
  end;
end;

procedure TBGRAImageManipulation.SetEmptyImageSizeToNull;
begin
  SetEmptyImageSize(ruPixelsPerInch, 0, 0);
end;

procedure TBGRAImageManipulation.SetEmptyImageSize(AResolutionUnit: TResolutionUnit; AResolutionWidth,
  AResolutionHeight: Single);
begin
  EmptyImage.ResolutionUnit:=AResolutionUnit;
  EmptyImage.rResolutionWidth:=AResolutionWidth;
  EmptyImage.rResolutionHeight:=AResolutionHeight;
  Resize;
end;

procedure TBGRAImageManipulation.LoadFromFile(const AFilename: String);
begin
  LoadFromFileUTF8(SysToUtf8(AFilename));
end;

procedure TBGRAImageManipulation.LoadFromFile(const AFilename: String; AHandler: TFPCustomImageReader;
  AOptions: TBGRALoadingOptions);
begin
  LoadFromFileUTF8(SysToUtf8(AFilename), AHandler, AOptions);
end;

procedure TBGRAImageManipulation.LoadFromFileUTF8(const AFilenameUTF8: String);
var
  AStream: TStream;
  AFormat: TBGRAImageFormat;
  AHandler: TFPCustomImageReader;
  AOptions: TBGRALoadingOptions;

begin
  try
     AStream:= TFileStreamUTF8.Create(AFilenameUTF8, fmOpenRead or fmShareDenyWrite);
     AFormat:= DetectFileFormat(AStream, ExtractFileExt(AFilenameUTF8));
     AHandler:= CreateBGRAImageReader(AFormat);
     AOptions:= [loKeepTransparentRGB];

     if Assigned(rOnBitmapLoadBefore) then rOnBitmapLoadBefore(Self, AStream, AFormat, AHandler, AOptions);

     fImageBitmap.LoadFromStream(AStream, AHandler, AOptions);
     setBitmap(fImageBitmap);

     if Assigned(rOnBitmapLoadAfter) then rOnBitmapLoadAfter(Self, AStream, AFormat, AHandler, AOptions);

  finally
    AHandler.Free;
    AStream.Free;
  end;
end;

procedure TBGRAImageManipulation.LoadFromFileUTF8(const AFilenameUTF8: String; AHandler: TFPCustomImageReader;
  AOptions: TBGRALoadingOptions);
var
  AStream: TStream;

begin
  try
     AStream:= TFileStreamUTF8.Create(AFilenameUTF8, fmOpenRead or fmShareDenyWrite);
     LoadFromStream(AStream, AHandler, AOptions);

  finally
    AStream.Free;
  end;
end;

procedure TBGRAImageManipulation.LoadFromStream(AStream: TStream);
var
  AFormat: TBGRAImageFormat;
  AHandler: TFPCustomImageReader;
  AOptions: TBGRALoadingOptions;

begin
  try
    AFormat:= DetectFileFormat(AStream);
    AHandler:= CreateBGRAImageReader(AFormat);
    AOptions:= [loKeepTransparentRGB];
    LoadFromStream(AStream, AHandler, AOptions);

  finally
    AHandler.Free;
  end;
end;

procedure TBGRAImageManipulation.LoadFromStream(AStream: TStream;
                  AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions);
var
  AFormat: TBGRAImageFormat;

begin
  AFormat:= DetectFileFormat(AStream);

  if Assigned(rOnBitmapLoadBefore) then rOnBitmapLoadBefore(Self, AStream, AFormat, AHandler, AOptions);

  fImageBitmap.LoadFromStream(AStream, AHandler, AOptions);
  setBitmap(fImageBitmap);

  if Assigned(rOnBitmapLoadAfter) then rOnBitmapLoadAfter(Self, AStream, AFormat, AHandler, AOptions);
end;

procedure TBGRAImageManipulation.SaveToFile(const AFilename: String);
begin
  SaveToFileUTF8(SysToUtf8(AFilename));
end;

procedure TBGRAImageManipulation.SaveToFile(const AFilename: String; AFormat: TBGRAImageFormat;
  AHandler: TFPCustomImageWriter);
begin
  SaveToFileUTF8(SysToUtf8(AFilename), AFormat, AHandler);
end;

procedure TBGRAImageManipulation.SaveToFileUTF8(const AFilenameUTF8: String);
var
  writer: TFPCustomImageWriter;
  format: TBGRAImageFormat;
  ext: String;

begin
  try
    writer:= TUniversalDrawer.CreateBGRAImageWriter(fImageBitmap, AFilenameUTF8, format);
    SaveToFileUTF8(AFilenameUTF8, format, writer);

  finally
    writer.free;
  end;
end;

procedure TBGRAImageManipulation.SaveToFileUTF8(const AFilenameUTF8: String; AFormat: TBGRAImageFormat;
  AHandler: TFPCustomImageWriter);
var
  AStream: TStream;

begin
  try
     AStream:= TFileStreamUTF8.Create(AFilenameUTF8, fmCreate);
     SaveToStream(AStream, AFormat, AHandler);

  finally
    AStream.Free;
  end;
end;

procedure TBGRAImageManipulation.SaveToStream(AStream: TStream; AFormat: TBGRAImageFormat;
  AHandler: TFPCustomImageWriter);
var
  HandlerNil: Boolean;

begin
  HandlerNil:= (AHandler = nil);
  try
     if HandlerNil then AHandler:= TUniversalDrawer.CreateBGRAImageWriter(fImageBitmap, AFormat);

     if Assigned(rOnBitmapSaveBefore) then rOnBitmapSaveBefore(Self, AStream, AFormat, AHandler);

     TFPCustomImage(fImageBitmap).SaveToStream(AStream, AHandler);

     if Assigned(rOnBitmapSaveAfter) then rOnBitmapSaveAfter(Self, AStream, AFormat, AHandler);

  finally
    if HandlerNil then AHandler.Free;
  end;
end;

procedure TBGRAImageManipulation.setBorderSize(const Value: byte);
const
  MinSize = 2;
  MaxSize = 10;
begin
  if (Value <> fBorderSize) then
  begin
    if (Value < MinSize) then raise ERangeError.CreateFmt(SBorderSizeIsTooSmall, [Value, MinSize, MaxSize]);
    if (Value > MaxSize) then raise ERangeError.CreateFmt(SBorderSizeIsTooLarge, [Value, MinSize, MaxSize]);

    fBorderSize := Value;

    if not(csLoading in ComponentState) then Render_Invalidate;
  end;
end;

procedure TBGRAImageManipulation.setKeepAspectRatio(const Value: boolean);
var
   i :Integer;
   curCropArea :TCropArea;
   imgPresent :Boolean;

begin
  if (Value = fKeepAspectRatio) then Exit;

  fKeepAspectRatio :=Value;

  imgPresent :=not(fImageBitmap.Empty);

  //Change all the Crop Area with KeepAspectRatio=bParent
  for i:=0 to rCropAreas.Count-1 do
  begin
    curCropArea :=rCropAreas[i];

    if (curCropArea<>nil) and (curCropArea.KeepAspectRatio=bParent) then
    begin
      if fKeepAspectRatio
      then curCropArea.CopyAspectFromParent;

      if imgPresent
      then ApplyRatioToArea(curCropArea);
    end;
  end;

  if not(csLoading in ComponentState) then Render_Invalidate;
end;

function TBGRAImageManipulation.getAspectRatioFromImage(const Value: TBGRABitmap): string;
var
  GCD: integer;
begin
  GCD := getGCD(Value.Width, Value.Height);

  Result := IntToStr(Value.Width div GCD) + ':' + IntToStr(Value.Height div GCD);
end;

procedure TBGRAImageManipulation.setAspectRatio(const Value: string);
var
  XValue, YValue: integer;
  AspectRatioText: string;
  i           :Integer;
  fGCD  :integer;
  imgPresent :Boolean;
  curCropArea :TCropArea;

begin
  if (Value <> fAspectRatio) then
  begin
    // Check if value contain a valid string
    CheckAspectRatio(Value, AspectRatioText, XValue, YValue);

    // Set new Aspect Ratio
    fAspectRatio := AspectRatioText;
    fAspectX := XValue;
    fAspectY := YValue;

    // Calculate the ratio
    fGCD := getGCD(fAspectX, fAspectY);

    // Determine the ratio of scale per axle
    with fRatio do
    begin
      Horizontal := fAspectX div fGCD;
      Vertical := fAspectY div fGCD;
    end;

    // Set minimun size
    if ((fRatio.Horizontal < fAnchorSize + 10) or
      (fRatio.Vertical < fAnchorSize + 10)) then
    begin
      fMinWidth  := fRatio.Horizontal * 10;
      fMinHeight := fRatio.Vertical * 10;
    end
    else
    begin
      fMinWidth  := fRatio.Horizontal;
      fMinHeight := fRatio.Vertical;
    end;

    imgPresent :=not(fImageBitmap.Empty);

    //Change all the Crop Area with KeepAspectRatio=bParent
    for i:=0 to rCropAreas.Count-1 do
    begin
      curCropArea :=rCropAreas[i];
      if (curCropArea<>nil) and (curCropArea.KeepAspectRatio=bParent) then
      begin
        if fKeepAspectRatio
        then curCropArea.CopyAspectFromParent;

        if imgPresent
        then ApplyRatioToArea(curCropArea);
      end;
    end;

    if not(csLoading in ComponentState) then Render_Invalidate;
  end;
end;

procedure TBGRAImageManipulation.setEmptyImage(AValue: TBGRAEmptyImage);
begin
  rEmptyImage.Assign(AValue);
end;

procedure TBGRAImageManipulation.setMinHeight(const Value: integer);
begin
  if (Value <> fMinHeight) then
  begin
    if (Value < fSizeLimits.minHeight)
    then fMinHeight := fSizeLimits.minHeight
    else begin
           if (Value > fSizeLimits.maxHeight)
           then fMinHeight := fSizeLimits.maxHeight
           else fMinHeight := Value;
         end;

    if (fKeepAspectRatio) then
    begin
      // Recalculates the width value based on height
      fMinWidth := Trunc(fMinHeight * (fRatio.Horizontal / fRatio.Vertical));
    end;

    if not(csLoading in ComponentState) then Render_Invalidate;
  end;
end;

procedure TBGRAImageManipulation.setMinWidth(const Value: integer);
begin
  if (Value <> fMinWidth) then
  begin
    if (Value < fSizeLimits.minWidth)
    then fMinWidth := fSizeLimits.minWidth
    else begin
           if (Value > fSizeLimits.maxWidth)
           then fMinWidth := fSizeLimits.maxWidth
           else fMinWidth := Value;
         end;

    if (fKeepAspectRatio) then
    begin
      // Recalculates the height value based on width
      fMinHeight := Trunc(fMinWidth * (fRatio.Vertical / fRatio.Horizontal));
    end;

    if not(csLoading in ComponentState) then Render_Invalidate;
  end;
end;

procedure TBGRAImageManipulation.SetOpacity(AValue: Byte);
begin
  if (rOpacity <> AValue) then
  begin
    rOpacity:= AValue;

    if not(csLoading in ComponentState) then Render_Invalidate;
  end;
end;

procedure TBGRAImageManipulation.setSelectedCropArea(AValue: TCropArea);
var
   oldSelected :TCropArea;

begin
  if rSelectedCropArea=AValue then Exit;
  oldSelected :=rSelectedCropArea;
  rSelectedCropArea:=AValue;

  Render_Invalidate;

  if assigned(rOnSelectedCropAreaChanged)
  then rOnSelectedCropAreaChanged(Self, oldSelected);
end;


 { ============================================================================ }
 { =====[ Event Control ]====================================================== }
 { ============================================================================ }

procedure TBGRAImageManipulation.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  WorkRect: TRect;
  ACursor :TCursor;

begin
  // Call the inherited MouseDown() procedure
  inherited MouseDown(Button, Shift, X, Y);

  // Find the working area of the control
  WorkRect := getWorkRect;

  // If over control
  if (((X >= WorkRect.Left) and (X <= WorkRect.Right) and
      (Y >= WorkRect.Top) and (Y <= WorkRect.Bottom)) and
      (Button = mbLeft) and (not (ssDouble in Shift))) then
  begin
    // If this was the left mouse button and nor double click
    fMouseCaught := True;
    fStartPoint  := Point(X - WorkRect.Left, Y - WorkRect.Top);

    //rNewCropArea :=nil;
    SelectedCropArea :=Self.isOverAnchor(fStartPoint, fAnchorSelected, {%H-}ACursor);
    if (SelectedCropArea<>nil)
    then fStartArea :=SelectedCropArea.ScaledArea;

    if (fAnchorSelected = [NORTH, SOUTH, EAST, WEST])
    then begin // Move the cropping area
            fStartPoint :=Point(X - SelectedCropArea.ScaledArea.Left, Y-SelectedCropArea.ScaledArea.Top);
         end
    else begin // Resize the cropping area from cornes
            // Get the coordinate corresponding to the opposite quadrant and
            // set into fStartPoint
            if ((fAnchorSelected = [NORTH]) or (fAnchorSelected = [WEST]) or
                (fAnchorSelected = [NORTH, WEST]))
            then fStartPoint := Point(SelectedCropArea.ScaledArea.Right, SelectedCropArea.ScaledArea.Bottom);

            if (fAnchorSelected = [SOUTH, WEST])
            then fStartPoint := Point(SelectedCropArea.ScaledArea.Right, SelectedCropArea.ScaledArea.Top);

            if ((fAnchorSelected = [SOUTH]) or (fAnchorSelected = [EAST]) or
                (fAnchorSelected = [SOUTH, EAST]))
            then fStartPoint := Point(SelectedCropArea.ScaledArea.Left, SelectedCropArea.ScaledArea.Top);

            if (fAnchorSelected = [NORTH, EAST])
            then fStartPoint := Point(SelectedCropArea.ScaledArea.Left, SelectedCropArea.ScaledArea.Bottom);
         end;
  end;
end;

procedure TBGRAImageManipulation.MouseMove(Shift: TShiftState; X, Y: integer);
var
  needRepaint: boolean;
  WorkRect: TRect;
  newCoords: TCoord;
  Direction: TDirection;
  Bounds: TRect;
  {%H-}overCropArea :TCropArea;
  ACursor      :TCursor;

  procedure newSelection;
  begin
    // Starts a new selection of cropping area
    try
       Cursor := crCross;
       fEndPoint := Point(X - WorkRect.Left, Y - WorkRect.Top);

       // Copy coord
       with newCoords do
       begin
         x1 := fStartPoint.X;
         y1 := fStartPoint.Y;

         x2 := fEndPoint.X;
         y2 := fEndPoint.Y;
       end;

       // Determine direction
       Direction := getDirection(fStartPoint, fEndPoint);

       // Apply the ratio, if necessary
       newCoords := ApplyRatioToAxes(newCoords, Direction, Bounds, rNewCropArea);

       // Determines minimum value on both axes
       // new Area have KeepAspectRatio setted to bParent by default
       newCoords := ApplyDimRestriction(newCoords, Direction, Bounds, fKeepAspectRatio);

       if (rNewCropArea = Nil)
       then begin
              rNewCropArea :=addScaledCropArea(Rect(newCoords.x1, newCoords.y1, newCoords.x2, newCoords.y2));
              SelectedCropArea :=rNewCropArea;
            end
       else rNewCropArea.ScaledArea :=Rect(newCoords.x1, newCoords.y1, newCoords.x2, newCoords.y2);

    finally
      needRepaint := True;
    end;
  end;

  procedure moveCropping;
  begin
    Cursor := crSizeAll;

    // Move the cropping area
    try
       WorkRect :=SelectedCropArea.ScaledArea;
       WorkRect.Left :=fEndPoint.X-fStartPoint.X;    //fStartPoint is Relative to CropArea
       WorkRect.Top :=fEndPoint.Y-fStartPoint.Y;

       //Out of Bounds check
       if (WorkRect.Left<0)
       then WorkRect.Left :=0;

       if (WorkRect.Top<0)
       then WorkRect.Top :=0;

       if (WorkRect.Left+fStartArea.Width>Bounds.Right)
       then WorkRect.Left :=Bounds.Right-fStartArea.Width;

       if (WorkRect.Top+fStartArea.Height>Bounds.Bottom)
       then WorkRect.Top :=Bounds.Bottom-fStartArea.Height;

       WorkRect.Width :=fStartArea.Width;
       WorkRect.Height:=fStartArea.Height;
       SelectedCropArea.ScaledArea :=WorkRect;

    finally
      needRepaint := True;
    end;
  end;

  procedure resizeCropping;
  begin
    // Resize the cropping area
    try
       if ((fAnchorSelected = [EAST]) or (fAnchorSelected = [WEST]))
       then Cursor := crSizeWE
       else if (NORTH in fAnchorSelected)
            then begin
                   if (WEST in fAnchorSelected)
                   then Cursor := crSizeNW
                   else if (EAST in fAnchorSelected)
                        then Cursor := crSizeNE
                        else Cursor := crSizeNS;
                 end
            else begin
                   if (WEST in fAnchorSelected)
                   then Cursor := crSizeSW
                   else if (EAST in fAnchorSelected)
                        then Cursor := crSizeSE
                        else Cursor := crSizeNS;
                 end;

       // Copy coord
       with newCoords do
       begin
         x1 := fStartPoint.X;
         y1 := fStartPoint.Y;

         if (fAnchorSelected = [NORTH]) then
         begin
           x2 := fEndPoint.X - Abs(SelectedCropArea.ScaledArea.Right - SelectedCropArea.ScaledArea.Left) div 2;
           y2 := fEndPoint.Y;
         end
         else
         if (fAnchorSelected = [SOUTH]) then
         begin
           x2 := fEndPoint.X + Abs(SelectedCropArea.ScaledArea.Right - SelectedCropArea.ScaledArea.Left) div 2;
           y2 := fEndPoint.Y;
         end
         else
         if (fAnchorSelected = [EAST]) then
         begin
           x2 := fEndPoint.X;
           y2 := fEndPoint.Y + Abs(SelectedCropArea.ScaledArea.Bottom - SelectedCropArea.ScaledArea.Top) div 2;
         end
         else
         if (fAnchorSelected = [WEST]) then
         begin
           x2 := fEndPoint.X;
           y2 := fEndPoint.Y - Abs(SelectedCropArea.ScaledArea.Bottom - SelectedCropArea.ScaledArea.Top) div 2;
         end
         else
         begin
           x2 := fEndPoint.X;
           y2 := fEndPoint.Y;
         end;
       end;

       // Determine direction
       Direction := getDirection(fStartPoint, fEndPoint);

       // Apply the ratio, if necessary
       newCoords := ApplyRatioToAxes(newCoords, Direction, Bounds, SelectedCropArea);

       // Determines minimum value on both axes
       newCoords := ApplyDimRestriction(newCoords, Direction, Bounds, SelectedCropArea.getRealKeepAspectRatio);

       SelectedCropArea.ScaledArea := Rect(newCoords.x1, newCoords.y1, newCoords.x2, newCoords.y2);
    finally
      needRepaint := True;
    end;
  end;

begin
  {$ifopt D+}
   DebugLn('MouseMove');
  {$endif}

  // Call the inherited MouseMove() procedure
  inherited MouseMove(Shift, X, Y);

  // Set default cursor
  Cursor := crDefault;

  // Find the working area of the component
  WorkRect := GetWorkRect;

  // If the mouse was originally clicked on the control
  if fMouseCaught
  then begin
         // Assume we don't need to repaint the control
         needRepaint := False;

         // Determines limite values
         Bounds := getImageRect(fResampledBitmap);

         // If no anchor selected
         if (fAnchorSelected = [])
         then newSelection
         else begin
                // Get the actual point
                fEndPoint := Point(X - WorkRect.Left, Y - WorkRect.Top);

                // Check what the anchor was dragged
                if (fAnchorSelected = [NORTH, SOUTH, EAST, WEST])
                then moveCropping
                else resizeCropping;
              end;

         // If we need to repaint
         if needRepaint then
         begin
           SelectedCropArea.CalculateAreaFromScaledArea;
           if assigned(rOnCropAreaChanged)
           then rOnCropAreaChanged(Self, SelectedCropArea);

           // Invalidate the control for repainting
           Render;
           Invalidate;//Refresh;
         end;
       end
  else begin
         // If the mouse is just moving over the control, and wasn't originally click in the control
         if ((X >= WorkRect.Left) and (X <= WorkRect.Right) and
             (Y >= WorkRect.Top) and (Y <= WorkRect.Bottom)) then
         begin
           // Mouse is inside the pressable part of the control
           Cursor := crCross;
           fAnchorSelected := [];
           fEndPoint := Point(X - WorkRect.Left, Y - WorkRect.Top);

           // Verifies that is positioned on an anchor
           ACursor := crDefault;
           overCropArea :=Self.isOverAnchor(fEndPoint, fAnchorSelected, ACursor);
           Cursor :=ACursor;
         end;
       end;

  {$ifopt D+}
   DebugLn('MouseMove done');
  {$endif}
end;

procedure TBGRAImageManipulation.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  needRepaint: boolean;
  temp: integer;
  curCropAreaRect :TRect;

begin
  // Call the inherited MouseUp() procedure
  inherited MouseUp(Button, Shift, X, Y);

  // If the mouse was originally clicked over the control
  if (fMouseCaught) then
  begin
    // Show that the mouse is no longer caught
    fMouseCaught := False;

    // Assume we don't need to repaint the control
    needRepaint := False;

    if (rNewCropArea = Nil)
    then begin
           if (ssAlt in Shift)
           then begin
                  SelectedCropArea.ScaledArea :=fStartArea;
                  needRepaint :=True;
                end
         end
    else begin  // Ends a new selection of cropping area
           if (ssAlt in Shift)
           then begin
                  delCropArea(rNewCropArea);
                  rNewCropArea :=Nil;
                  needRepaint :=False;
                end
           else begin
                  SelectedCropArea :=rNewCropArea;
                  rNewCropArea :=Nil;
                  curCropAreaRect :=SelectedCropArea.ScaledArea;

                  if (curCropAreaRect.Left > curCropAreaRect.Right) then
                  begin
                    // Swap left and right coordinates
                    temp := curCropAreaRect.Left;
                    curCropAreaRect.Left := curCropAreaRect.Right;
                    curCropAreaRect.Right := temp;
                  end;

                  if (curCropAreaRect.Top > curCropAreaRect.Bottom) then
                  begin
                    // Swap Top and Bottom coordinates
                    temp := curCropAreaRect.Top;
                    curCropAreaRect.Top := curCropAreaRect.Bottom;
                    curCropAreaRect.Bottom := temp;
                  end;
                  needRepaint :=True;
                end;
         end;

    fAnchorSelected := [];

    // If we need to repaint
    if needRepaint then
    begin
      SelectedCropArea.CalculateAreaFromScaledArea;
      if assigned(rOnCropAreaChanged)
      then rOnCropAreaChanged(Self, SelectedCropArea);

      // Invalidate the control for repainting
      Render;
      Invalidate;//Refresh;
    end;
  end;
 end;

procedure TBGRAImageManipulation.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
var
   xAnchorSelected :TDirection;
   xCursor :TCursor;
   mouseCropArea:TCropArea;

begin
  if Assigned(rOnContextPopup) then
  begin
    mouseCropArea :=Self.isOverAnchor(MousePos, xAnchorSelected, {%H-}xCursor);
    rOnContextPopup(Self, mouseCropArea, xAnchorSelected, MousePos, Handled);
  end;
end;


 { ============================================================================ }
 { =====[ Register Function ]================================================== }
 { ============================================================================ }

{$IFDEF FPC}
procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBGRAImageManipulation]);
end;
{$ENDIF}

end.
