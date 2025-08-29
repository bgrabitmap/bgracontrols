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

  2018-11    - Edivando S. Santos Brasil | mailedivando@gmail.com (Compatibility with Delphi VCL)

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
      -08    - Changed All TResolutionUnit properties/vars to use a subset of TCSSUnit;
               CropAreas use a TPhysicalRect to store coords;
               Use of PhysicalSizeConvert and PhysicalSizeToPixels for conversions;
               Added Rulers;
               Added inherited Mouse Events and new one for Rulers;
               Added EnabledWorkArea to separate the Enabled areas;
  ============================================================================
}

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

  //A Subset of TCSSUnit with only the physical units,
  // use PhysicalToCSSUnit and CSSToPhysicalUnit functions DO NOT CAST to/from TCSSUnit
  TPhysicalUnit = (
    cuPixel,
    cuCentimeter,
    cuMillimeter,
    cuInch,
    cuPica,
    cuPoint,
    cuPercent
  );

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
    rArea    :TPhysicalRect;
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
    procedure setArea(AValue: TPhysicalRect);
    function GetAreaUnit: TPhysicalUnit;
    procedure setAreaUnit(AValue: TPhysicalUnit);
    procedure setName(AValue: String);
    procedure setIcons(AValue: TCropAreaIcons);

    procedure Render_Invalidate;

    procedure CalculateScaledAreaFromArea;
    procedure CalculateAreaFromScaledArea;
    function GetPixelArea(const AValue: TPhysicalRect; scaled: Boolean): TRect;

    function CheckScaledOutOfBounds(var AArea:TRect):Boolean;
    function CheckAreaOutOfBounds(var AArea:TRectF):Boolean;

    property ScaledArea: TRect read rScaledArea write setScaledArea;

  public
    Rotate   :Single;
    UserData :Integer;
    BorderColor :TBGRAPixel;

    function getResampledBitmap(ACopyProperties: Boolean=False): TBGRABitmap;
    function getBitmap(ACopyProperties: Boolean=False): TBGRABitmap;

    constructor Create(AOwner: TBGRAImageManipulation; AArea: TPhysicalRect;
                       AUserData: Integer = -1); overload;
    constructor Create(AOwner: TBGRAImageManipulation; AArea: TRectF;
                       AAreaUnit: TPhysicalUnit = cuPixel;
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

    property Area:TPhysicalRect read rArea write setArea;
    property AreaUnit:TPhysicalUnit read GetAreaUnit write setAreaUnit;
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
    rPhysicalHeight: Single;
    rPhysicalUnit: TPhysicalUnit;
    rPhysicalWidth: Single;
    rShowBorder: Boolean;

    function getHeight: Integer;
    function getWidth: Integer;
    procedure SetPhysicalUnit(AValue: TPhysicalUnit);

  public
    constructor Create(AOwner: TBGRAImageManipulation);

    property Width:Integer read getWidth;
    property Height:Integer read getHeight;

  published
    property PhysicalUnit: TPhysicalUnit read rPhysicalUnit write SetPhysicalUnit default cuCentimeter;
    property PhysicalWidth: Single read rPhysicalWidth write rPhysicalWidth;
    property PhysicalHeight: Single read rPhysicalHeight write rPhysicalHeight;
    property ShowBorder: Boolean read rShowBorder write rShowBorder default False;
  end;

  { TBGRANewCropAreaDefault }

  TBGRANewCropAreaDefault = class(TPersistent)
  private
    rAspectRatio: string;
    rIcons: TCropAreaIcons;
    rKeepAspectRatio: BoolParent;
    rPhysicalUnit: TPhysicalUnit;

  public
    constructor Create;

    procedure CopyPropertiesToArea(ANewArea: TCropArea);

  published
    property Icons: TCropAreaIcons read rIcons write rIcons;
    property PhysicalUnit: TPhysicalUnit read rPhysicalUnit write rPhysicalUnit default cuCentimeter;
    property AspectRatio: string read rAspectRatio write rAspectRatio;
    property KeepAspectRatio: BoolParent read rKeepAspectRatio write rKeepAspectRatio default bFalse;
  end;

  { TBGRAIMRulers }

  TRulersSide = (rsdLeft, rsdTop, rsdRight, rsdBottom);
  TRulersSides = set of TRulersSide;

  TBGRAIMRulers = class(TPersistent)
  protected
    fOwner: TBGRAImageManipulation;
    fBitmap: TBGRABitmap;
    rSides: TRulersSides;
    rPhysicalUnit: TPhysicalUnit;
    rFont: TFont;
    rLineColor,
    rBackgroundColor: TColor;
    rShowPhysicalUnit,
    rStopToImage: Boolean;
    BGRALineColor,
    BGRABackColor: TBGRAPixel;

    procedure SetStopToImage(AValue: Boolean);
    procedure SetShowPhysicalUnit(AValue: Boolean);
    procedure SetLineColor(AValue: TColor);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure SetPhysicalUnit(AValue: TPhysicalUnit);
    procedure SetSides(AValue: TRulersSides);

    procedure Render;
    procedure Render_Invalidate;

  public
    constructor Create(AOwner: TBGRAImageManipulation);
    destructor Destroy; override;

    function OverPos(APos: TPoint): TRulersSides;

  published
    property Sides: TRulersSides read rSides write SetSides default [];
    property PhysicalUnit: TPhysicalUnit read rPhysicalUnit write SetPhysicalUnit default cuCentimeter;
    property Font: TFont read rFont write SetFont;
    property LineColor: TColor read rLineColor write SetLineColor default clBlack;
    property BackgroundColor: TColor read rBackgroundColor write SetBackgroundColor default clWhite;
    property ShowPhysicalUnit: Boolean read rShowPhysicalUnit write SetShowPhysicalUnit default False;
    property StopToImage: Boolean read rStopToImage write SetStopToImage default True;
  end;

  { TBGRAImageManipulation }

  TCropAreaEvent = procedure (Sender: TBGRAImageManipulation; CropArea: TCropArea) of object;
  TCropAreaLoadEvent = function (Sender: TBGRAImageManipulation; CropArea: TCropArea;
                                 const XMLConf: TXMLConfig; const Path:String):Integer of object;
  TCropAreaSaveEvent = procedure (Sender: TBGRAImageManipulation; CropArea: TCropArea;
                                 const XMLConf: TXMLConfig; const Path:String) of object;

  TBGRAIMCropAreaPopupEvent = procedure(Sender: TBGRAImageManipulation; CropArea: TCropArea;
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

  TBGRAIMRulersMouseUp = procedure(Sender: TBGRAImageManipulation; ARulers: TRulersSides;
                                 Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

  TBGRAIMRulersPopupEvent = procedure(Sender: TBGRAImageManipulation; ARulers: TRulersSides;
                                      MousePos: TPoint; var Handled: Boolean) of object;

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
    fImageBitmap,
    fResampledBitmap,
    fBackground,
    fVirtualScreen: TBGRABitmap;
    rEnabledWorkArea: Boolean;
    rNewCropAreaDefault: TBGRANewCropAreaDefault;
    rOnBitmapSaveAfter: TBGRAIMBitmapSaveAfter;
    rOnBitmapSaveBefore: TBGRAIMBitmapSaveBefore;

    function getAnchorSize: byte;
    function GetEnabledWorkArea: Boolean;
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
    rOnCropAreaSelectedChanged: TCropAreaEvent;
    rOnCropAreaLoad: TCropAreaLoadEvent;
    rOnCropAreaSave: TCropAreaSaveEvent;
    rOnCropAreaPopup: TBGRAIMCropAreaPopupEvent;
    rOnBitmapLoadBefore: TBGRAIMBitmapLoadBefore;
    rOnBitmapLoadAfter: TBGRAIMBitmapLoadAfter;
    rOnRulersMouseUp: TBGRAIMRulersMouseUp;
    rOnRulersPopup: TBGRAIMRulersPopupEvent;
    rEmptyImage: TBGRAEmptyImage;
    rRulers: TBGRAIMRulers;
    rOpacity: Byte;
    WorkRect: TRect;
    xRatio, yRatio: Single;

    function ApplyDimRestriction(Coords: TCoord; Direction: TDirection; Bounds: TRect; AKeepAspectRatio:Boolean): TCoord;
    function ApplyRatioToAxes(Coords: TCoord; Direction: TDirection; Bounds: TRect;  ACropArea :TCropArea = Nil): TCoord;
    procedure ApplyRatioToArea(ACropArea :TCropArea);
    procedure CalcMaxSelection(ACropArea :TCropArea);
    function getDirection(const Point1, Point2: TPoint): TDirection;

    function getImageRect(Picture: TBGRABitmap): TRect;
    procedure CalculateWorkRect;
    function isOverAnchor(APoint :TPoint; var AnchorSelected :TDirection; var ACursor :TCursor) :TCropArea;
    procedure CreateEmptyImage;
    procedure CreateResampledBitmap;

    class function GetControlClassDefaultSize: TSize; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean); override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure ResizeVirtualScreen;
    procedure DoOnResize; override;

    procedure RenderBackground;
    procedure Render_All;
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
    function getAspectRatioFromImage(const Value: TBGRABitmap): string;
    function getResampledBitmap(ACropArea :TCropArea = Nil; ACopyProperties: Boolean=False) : TBGRABitmap;
    function getBitmap(ACropArea :TCropArea = Nil; ACopyProperties: Boolean=False) : TBGRABitmap;

    procedure rotateLeft(ACopyProperties: Boolean=False);
    procedure rotateRight(ACopyProperties: Boolean=False);

    //Recreate Bitmap Render useful when use inplace filters directly in Bitmap
    procedure RefreshBitmap;

    procedure tests;

    //Crop Areas Manipulation functions
    function addCropArea(AArea : TRectF; AAreaUnit: TPhysicalUnit = cuPixel;
                         AUserData: Integer = -1) :TCropArea;
    function addScaledCropArea(AArea : TRect; AUserData: Integer = -1) :TCropArea;
    procedure delCropArea(ACropArea :TCropArea);
    procedure clearCropAreas;
    procedure getAllResampledBitmaps(ACallBack :TgetAllBitmapsCallback; AUserData:Integer=0; ACopyProperties: Boolean=False);
    procedure getAllBitmaps(ACallBack :TgetAllBitmapsCallback; AUserData:Integer=0; ACopyProperties: Boolean=False);

    procedure SetEmptyImageSizeToCropAreas(ReduceLarger: Boolean=False);
    procedure SetEmptyImageSizeToNull;
    procedure SetEmptyImageSize(APhysicalUnit: TPhysicalUnit; APhysicalWidth, APhysicalHeight: Single);

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

    property Empty: Boolean read getEmpty;
    property SelectedCropArea :TCropArea read rSelectedCropArea write setSelectedCropArea;
    property CropAreas :TCropAreaList read rCropAreas;
    property PixelsPerInch: Integer read getPixelsPerInch;

  published
    { Published declarations }
    property Align;
    property Anchors;
    property Enabled;
    property EnabledWorkArea: Boolean read GetEnabledWorkArea write rEnabledWorkArea default True;

    property AnchorSize: byte Read getAnchorSize Write setAnchorSize default 5;
    property Bitmap: TBGRABitmap Read fImageBitmap Write setBitmap;
    property BorderSize: byte Read fBorderSize Write setBorderSize default 2;
    property AspectRatio: string Read fAspectRatio Write setAspectRatio;
    property KeepAspectRatio: boolean Read fKeepAspectRatio Write setKeepAspectRatio default True;
    property MinHeight: integer Read fMinHeight Write setMinHeight;
    property MinWidth: integer Read fMinWidth Write setMinWidth;
    property EmptyImage: TBGRAEmptyImage read rEmptyImage write setEmptyImage stored True;
    property NewCropAreaDefault: TBGRANewCropAreaDefault read rNewCropAreaDefault write rNewCropAreaDefault stored True;
    property Opacity: Byte read rOpacity write SetOpacity default 128;
    property Rulers: TBGRAIMRulers read rRulers write rRulers;

    //Events
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnShowHint;

    //Events of CropArea
    property OnCropAreaAdded:TCropAreaEvent read rOnCropAreaAdded write rOnCropAreaAdded;
    property OnCropAreaDeleted:TCropAreaEvent read rOnCropAreaDeleted write rOnCropAreaDeleted;
    property OnCropAreaChanged:TCropAreaEvent read rOnCropAreaChanged write rOnCropAreaChanged;
    property OnCropAreaLoad:TCropAreaLoadEvent read rOnCropAreaLoad write rOnCropAreaLoad;
    property OnCropAreaSave:TCropAreaSaveEvent read rOnCropAreaSave write rOnCropAreaSave;

             //CropArea Parameter is the Old Selected Area, use SelectedCropArea property for current
    property OnCropAreaSelectedChanged:TCropAreaEvent read rOnCropAreaSelectedChanged write rOnCropAreaSelectedChanged;

    property OnCropAreaPopup: TBGRAIMCropAreaPopupEvent read rOnCropAreaPopup write rOnCropAreaPopup;

    //Events of Bitmap
    property OnBitmapLoadBefore: TBGRAIMBitmapLoadBefore read rOnBitmapLoadBefore write rOnBitmapLoadBefore;
    property OnBitmapLoadAfter: TBGRAIMBitmapLoadAfter read rOnBitmapLoadAfter write rOnBitmapLoadAfter;
    property OnBitmapSaveBefore: TBGRAIMBitmapSaveBefore read rOnBitmapSaveBefore write rOnBitmapSaveBefore;
    property OnBitmapSaveAfter: TBGRAIMBitmapSaveAfter read rOnBitmapSaveAfter write rOnBitmapSaveAfter;

    //Events of Rulers
    property OnRulersMouseUp: TBGRAIMRulersMouseUp read rOnRulersMouseUp write rOnRulersMouseUp;
    property OnRulersPopup: TBGRAIMRulersPopupEvent read rOnRulersPopup write rOnRulersPopup;
  end;

const
  PhysicalUnitShortName: array[TPhysicalUnit] of string = ('px','cm','mm','in','pc','pt','%');

function PhysicalToCSSUnit(ASourceUnit: TPhysicalUnit): TCSSUnit;
function CSSToPhysicalUnit(ASourceUnit: TCSSUnit): TPhysicalUnit;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

uses
  Math, ExtCtrls, BGRAUnits, BGRAUTF8, UniversalDrawer, BGRAText, BGRATextFX,
  BGRAWritePNG, FPWritePNM;

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


function PhysicalToCSSUnit(ASourceUnit: TPhysicalUnit): TCSSUnit;
begin
  case ASourceUnit of
  cuPixel: Result:= TCSSUnit.cuPixel;
  cuCentimeter: Result:= TCSSUnit.cuCentimeter;
  cuMillimeter: Result:= TCSSUnit.cuMillimeter;
  cuInch: Result:= TCSSUnit.cuInch;
  cuPica: Result:= TCSSUnit.cuPica;
  cuPoint: Result:= TCSSUnit.cuPoint;
  cuPercent: Result:= TCSSUnit.cuPercent;
  end;
end;

function CSSToPhysicalUnit(ASourceUnit: TCSSUnit): TPhysicalUnit;
begin
  case ASourceUnit of
  TCSSUnit.cuPixel: Result:= cuPixel;
  TCSSUnit.cuCentimeter: Result:= cuCentimeter;
  TCSSUnit.cuMillimeter: Result:= cuMillimeter;
  TCSSUnit.cuInch: Result:= cuInch;
  TCSSUnit.cuPica: Result:= cuPica;
  TCSSUnit.cuPoint: Result:= cuPoint;
  TCSSUnit.cuPercent: Result:= cuPercent;
  end;
end;

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
  if not(fOwner.rCropAreas.loading) then fOwner.Render_Invalidate;
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
   tempArea: TPhysicalRect;

begin
  if AValue=rArea.Top then Exit;

  tempArea :=rArea;
  tempArea.Top:=AValue;
  tempArea.Height:=rArea.Height;
  Area :=tempArea;
end;

function TCropArea.getLeft: Single;
begin
  Result :=rArea.Left;
end;

procedure TCropArea.setLeft(AValue: Single);
var
   tempArea: TPhysicalRect;

begin
  if AValue=rArea.Left then Exit;

  tempArea :=rArea;
  tempArea.Left:=AValue;
  tempArea.Width:=rArea.Width;
  Area :=tempArea;
end;

function TCropArea.getHeight: Single;
begin
  Result :=rArea.Height;
end;

procedure TCropArea.setHeight(AValue: Single);
var
   tempArea: TPhysicalRect;

begin
  if AValue=rArea.Height then Exit;

  tempArea :=rArea;
  tempArea.Height:=AValue;
  Area :=tempArea;
end;

function TCropArea.getWidth: Single;
begin
  Result :=rArea.Width;
end;

procedure TCropArea.setWidth(AValue: Single);
var
   tempArea: TPhysicalRect;

begin
  if AValue=rArea.Width then Exit;

  tempArea :=rArea;
  tempArea.Width:=AValue;
  Area :=tempArea;
end;

function TCropArea.getMaxHeight: Single;
begin
  if (rArea.PhysicalUnit = TCSSUnit.cuPercent)
  then Result:= 100
  else Result:= PhysicalSizeConvert(TCSSUnit.cuPixel, fOwner.fImageBitmap.Height, rArea.PhysicalUnit,
                                      fOwner.fImageBitmap.ResolutionUnit, fOwner.fImageBitmap.ResolutionY);
end;

function TCropArea.getMaxWidth: Single;
begin
  if (rArea.PhysicalUnit = TCSSUnit.cuPercent)
  then Result:= 100
  else Result:= PhysicalSizeConvert(TCSSUnit.cuPixel, fOwner.fImageBitmap.Width, rArea.PhysicalUnit,
                                    fOwner.fImageBitmap.ResolutionUnit, fOwner.fImageBitmap.ResolutionX);
end;

function TCropArea.getIndex: Longint;
begin
  Result :=fOwner.CropAreas.IndexOf(Self);
end;

procedure TCropArea.CalculateScaledAreaFromArea;
begin
  if not(isNullSize) then rScaledArea:= GetPixelArea(rArea, True);
end;

procedure TCropArea.CalculateAreaFromScaledArea;
var
   tmpRect: TPhysicalRect;

begin
  // Calculate Area from Pixels given Scale and Image Infos
  tmpRect.PhysicalUnit:= TCSSUnit.cuPixel;
  tmpRect.TopLeft:= PointF(rScaledArea.TopLeft);
  tmpRect.BottomRight:= PointF(rScaledArea.BottomRight);

  PhysicalSizeConvert(tmpRect, rArea.PhysicalUnit, fOwner.fImageBitmap);

  tmpRect.Top:= tmpRect.Top / fOwner.yRatio;
  tmpRect.Left:= tmpRect.Left / fOwner.xRatio;
  tmpRect.Bottom:= tmpRect.Bottom / fOwner.yRatio;
  tmpRect.Right:= tmpRect.Right / fOwner.xRatio;
  rArea:= tmpRect;
end;

function TCropArea.GetPixelArea(const AValue: TPhysicalRect; scaled: Boolean): TRect;
var
   xRatio, yRatio: Single;
   resRect: TRectF;

begin
  // Calculate Pixels from Area
  xRatio:= 1;
  yRatio:= 1;
  if scaled then
  begin
    xRatio:= fOwner.xRatio;
    yRatio:= fOwner.yRatio;
  end;

  resRect:= PhysicalSizeToPixels(AValue, fOwner.fImageBitmap);

  Result.Left:= Trunc(resRect.Left * xRatio);
  Result.Top:= Trunc(resRect.Top * yRatio);
  Result.Right:= HalfUp(resRect.Right * xRatio);
  Result.Bottom:= HalfUp(resRect.Bottom * yRatio);
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

procedure TCropArea.setArea(AValue: TPhysicalRect);
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

function TCropArea.GetAreaUnit: TPhysicalUnit;
begin
  Result:= CSSToPhysicalUnit(rArea.PhysicalUnit);
end;

procedure TCropArea.setAreaUnit(AValue: TPhysicalUnit);
var
   cssUnit: TCSSUnit;

begin
  cssUnit:= PhysicalToCSSUnit(AValue);
  if (rArea.PhysicalUnit = cssUnit) then exit;

  if not(Loading) and not(isNullSize)
  then PhysicalSizeConvert(rArea, cssUnit, fOwner.fImageBitmap);

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
        CropBitmap := fOwner.fImageBitmap.GetPart(GetPixelArea(rArea, True), ACopyProperties);

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
     Result :=fOwner.fImageBitmap.GetPart(GetPixelArea(rArea, False), ACopyProperties);
  except
     if (Result<>nil)
     then FreeAndNil(Result);
  end;
end;

constructor TCropArea.Create(AOwner: TBGRAImageManipulation; AArea: TPhysicalRect; AUserData: Integer);
begin
  inherited Create;
  if (AOwner = Nil)
  then raise Exception.Create('TCropArea Owner is Nil');
  OwnerList :=nil;
  fOwner :=AOwner;
  rArea :=AArea;
  UserData :=AUserData;
  rAspectX :=3;
  rAspectY :=4;
  rKeepAspectRatio :=bParent;
  Loading:=False;
  CopyAspectFromParent;
end;

constructor TCropArea.Create(AOwner: TBGRAImageManipulation; AArea: TRectF;
                             AAreaUnit: TPhysicalUnit; AUserData: Integer);
begin
  rArea.PhysicalUnit:= PhysicalToCSSUnit(AAreaUnit);
  rArea.TopLeft:= AArea.TopLeft;
  rArea.BottomRight:= AArea.BottomRight;
  Create(AOwner, rArea, AUserData);
end;

constructor TCropArea.Create(AOwner: TBGRAImageManipulation;
                             DuplicateFrom: TCropArea; InsertInList:Boolean);
begin
  if (DuplicateFrom = Nil)
  then raise Exception.Create('TCropArea DuplicateFrom is Nil');

  Create(AOwner, DuplicateFrom.Area, DuplicateFrom.UserData);

  rAspectX :=DuplicateFrom.rAspectX;
  rAspectY :=DuplicateFrom.rAspectY;
  setKeepAspectRatio(DuplicateFrom.rKeepAspectRatio);

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
    Render_Invalidate;

  except
  end;
end;

procedure TCropArea.BringToBack;
begin
  if (OwnerList<>nil) then
  try
    OwnerList.Move(OwnerList.IndexOf(Self), 0);
    Render_Invalidate;

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

    Render_Invalidate;

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

    Render_Invalidate;

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
   tempArea: TPhysicalRect;

begin
  if (AWidth=rArea.Width) and (AHeight=rArea.Height)
  then exit;

  tempArea :=rArea;
  tempArea.Width:=AWidth;
  tempArea.Height:=AHeight;
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
  newAreaUnit:TPhysicalUnit;

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

      newAreaUnit :=TPhysicalUnit(XMLConf.GetValue(curItemPath+'AreaUnit', 0));
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
begin
  if (rPhysicalHeight<=0) or (rPhysicalWidth<=0)
  then Result:= fOwner.WorkRect.Bottom-fOwner.WorkRect.Top
  else Result:= HalfUp(PhysicalSizeToPixels(rPhysicalHeight, fOwner.fImageBitmap.ResolutionUnit, fOwner.fImageBitmap.ResolutionY,
                                            PhysicalToCSSUnit(rPhysicalUnit)));
end;

function TBGRAEmptyImage.getWidth: Integer;
begin
  if (rPhysicalWidth<=0) or (rPhysicalHeight<=0)
  then Result:= fOwner.WorkRect.Right-fOwner.WorkRect.Left
  else Result:= HalfUp(PhysicalSizeToPixels(rPhysicalWidth, fOwner.fImageBitmap.ResolutionUnit, fOwner.fImageBitmap.ResolutionX,
                                            PhysicalToCSSUnit(rPhysicalUnit)));
end;

procedure TBGRAEmptyImage.SetPhysicalUnit(AValue: TPhysicalUnit);
begin
  if (AValue<>rPhysicalUnit) then
  begin
    rPhysicalWidth:= PhysicalSizeConvert(PhysicalToCSSUnit(rPhysicalUnit), rPhysicalWidth,
                                         PhysicalToCSSUnit(AValue),
                                         fOwner.fImageBitmap.ResolutionUnit, fOwner.fImageBitmap.ResolutionX);
    rPhysicalHeight:= PhysicalSizeConvert(PhysicalToCSSUnit(rPhysicalUnit), rPhysicalHeight,
                                          PhysicalToCSSUnit(AValue),
                                          fOwner.fImageBitmap.ResolutionUnit, fOwner.fImageBitmap.ResolutionY);
    rPhysicalUnit:= AValue;
  end;
end;

constructor TBGRAEmptyImage.Create(AOwner: TBGRAImageManipulation);
begin
  inherited Create;

  fOwner :=AOwner;
  rShowBorder :=False;
  rPhysicalUnit:= cuCentimeter;
end;

{ TBGRANewCropAreaDefault }

constructor TBGRANewCropAreaDefault.Create;
begin
  inherited Create;

  rKeepAspectRatio:=bFalse;
  rAspectRatio:='3:4';
  rPhysicalUnit:= cuCentimeter;
  rIcons:= [];
end;

procedure TBGRANewCropAreaDefault.CopyPropertiesToArea(ANewArea: TCropArea);
begin
  ANewArea.rIcons:= Self.rIcons;
  ANewArea.rAspectRatio:= Self.rAspectRatio;
  ANewArea.KeepAspectRatio:= Self.rKeepAspectRatio;
end;

{ TBGRAIMRulers }

procedure TBGRAIMRulers.SetStopToImage(AValue: Boolean);
begin
  if rStopToImage=AValue then Exit;
  rStopToImage:=AValue;
  Render_Invalidate;
end;

procedure TBGRAIMRulers.SetShowPhysicalUnit(AValue: Boolean);
begin
  if rShowPhysicalUnit=AValue then Exit;
  rShowPhysicalUnit:=AValue;
  Render_Invalidate;
end;

procedure TBGRAIMRulers.SetLineColor(AValue: TColor);
begin
  if rLineColor=AValue then Exit;
  rLineColor:=AValue;
  BGRALineColor:=ColorToBGRA(AValue);
  Render_Invalidate;
end;

procedure TBGRAIMRulers.SetBackgroundColor(AValue: TColor);
begin
  if rBackgroundColor=AValue then Exit;
  rBackgroundColor:=AValue;
  BGRABackColor:=ColorToBGRA(AValue);
  Render_Invalidate;
end;

procedure TBGRAIMRulers.SetFont(AValue: TFont);
begin
  if rFont=AValue then Exit;
  rFont:=AValue;
  Render_Invalidate;
end;

procedure TBGRAIMRulers.SetPhysicalUnit(AValue: TPhysicalUnit);
begin
  if rPhysicalUnit=AValue then Exit;
  rPhysicalUnit:=AValue;
  Render_Invalidate;
end;

procedure TBGRAIMRulers.SetSides(AValue: TRulersSides);
begin
  if rSides=AValue then Exit;
  rSides:=AValue;

  with fOwner do
  if not(csLoading in ComponentState) then
  begin
    //Change size of WorkRect
    CalculateWorkRect;

    //Recreate the Resampled Bitmap because sizes and Ratio are Changed
    if Empty then CreateEmptyImage;
    CreateResampledBitmap;

    ResizeVirtualScreen;

    Render_All;
    Invalidate;
  end;
end;

procedure TBGRAIMRulers.Render;
var
   curPixelPos,
   oneUnit: Single;
   curPosI,
   posNum,
   startX, startY,
   endX, endY: Integer;
   cssUnit: TCSSUnit;
   FontColor: TBGRAPixel;

   procedure DrawVerticalText(AText: String; X, Y: integer; AColor: TBGRAPixel;
                              AAlign: TAlignment; AVertAlign: TVerticalAlignment;
                              AHeight:Integer);
   var
      txt: TBGRATextEffect;
      i, yL: Integer;

   begin
     yL:= Y;
     Case AVertAlign of
       taAlignTop:    for i:=1 to Length(AText) do
                      try
                         txt:= TBGRATextEffect.Create(AText[i], rFont.Name, AHeight, True);
                         txt.Draw(fBitmap, X, yL, AColor, AAlign);
                         inc(yL, txt.TextHeight-6);
                         txt.Free;

                      except
                        txt.Free;
                      end;

       taAlignBottom: for i:=Length(AText) downto 1 do
                      try
                         txt:= TBGRATextEffect.Create(AText[i], rFont.Name, AHeight, True);
                         dec(yL, txt.TextHeight-2);
                         txt.Draw(fBitmap, X, yL, AColor, AAlign);
                         txt.Free;

                      except
                        txt.Free;
                      end;
       taVerticalCenter: //MaxM: Complicated, I don't need it now; maybe added in TBGRATextEffect class
     end;
   end;

   procedure DrawHorizontalText(AText: String; X, Y: integer; AColor: TBGRAPixel; AAlign: TAlignment; AHeight:Integer);
   var
      txt: TBGRATextEffect;

   begin
     try
        txt:= TBGRATextEffect.Create(AText, rFont.Name, AHeight, True);
        txt.Draw(fBitmap, X, Y, AColor, AAlign);
        txt.Free;

     except
        txt.Free;
     end;
   end;

   procedure DrawHorizontal(IsTop: Boolean);
   var
      y0, y1, y2, y4, y8, yT: Integer;

   begin
     if IsTop
     then begin
            y0:= 15;   //Base
            y1:= 3;    //Unit
            y2:= 6;    // 1/2
            y4:= 9;    // 1/4
            y8:= 12;   // 1/8
            yT:= -2;   // Txt
            fBitmap.FillRect(0, 0, fBitmap.Width, 16, BGRABackColor);
          end
     else begin
            y0:= fBitmap.Height-16;
            y1:= y0+12;
            y2:= y0+9;
            y4:= y0+6;
            y8:= y0+3;
            yT:= y8+2;
            fBitmap.FillRect(0, y0, fBitmap.Width, fBitmap.Height, BGRABackColor);
          end;

     //Draw X Rule
     with fOwner do
     Case cssUnit of
       TCSSUnit.cuPixel: begin
          posNum:= 0;
          curPosI:= 16;

          curPixelPos:= startX+(curPosI*xRatio);
          while (curPixelPos <= endX) do
          begin
            Case posNum of
              0, 1, 3: fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y8, BGRALineColor, 1);
              2: fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y4, BGRALineColor, 1);
              4: begin
                   fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y1, BGRALineColor, 2);
                   DrawHorizontalText(IntToStr(curPosI), Trunc(curPixelPos-1.5), yT, FontColor,
                                      taRightJustify, 10);
                   posNum:= -1;
                 end;
            end;

            inc(curPosI, 16);
            inc(posNum);

            curPixelPos:= startX+(curPosI*xRatio);
          end;
        end;
        TCSSUnit.cuCentimeter: begin
          oneUnit:= PhysicalSizeToPixels(1/10, fImageBitmap.ResolutionUnit, fImageBitmap.ResolutionX, cssUnit) * xRatio;

          posNum:= 1;
          curPosI:= 1;   //mm

          curPixelPos:= startX+(curPosI*oneUnit);
          while (curPixelPos <= endX) do
          begin
            Case posNum of
              1..4, 6..9: fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y8, BGRALineColor, 1);
              5:  fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y2, BGRALineColor, 1);
              10: begin
                   fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y1, BGRALineColor, 2);
                   DrawHorizontalText(IntToStr(curPosI div 10), Trunc(curPixelPos-1.5), yT, FontColor,
                                      taRightJustify, 12);
                   posNum:= 0;
                 end;
            end;

            inc(curPosI);
            inc(posNum);

            curPixelPos:= startX+(curPosI*oneUnit);
          end;
        end;
        TCSSUnit.cuMillimeter: begin
          oneUnit:= PhysicalSizeToPixels(1, fImageBitmap.ResolutionUnit, fImageBitmap.ResolutionX, cssUnit) * xRatio;

          posNum:= 1;
          curPosI:= 1;   //mm

          curPixelPos:= startX+(curPosI*oneUnit);
          while (curPixelPos <= endX) do
          begin
            Case posNum of
              1..4, 6..9: fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y8, BGRALineColor, 1);
              5:  fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y2, BGRALineColor, 1);
              10: begin
                   fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y1, BGRALineColor, 2);
                   DrawHorizontalText(IntToStr(curPosI), Trunc(curPixelPos-1.5), yT, FontColor,
                                      taRightJustify, 10);
                   posNum:= 0;
                 end;
            end;

            inc(curPosI);
            inc(posNum);

            curPixelPos:= startX+(curPosI*oneUnit);
          end;
        end;
        TCSSUnit.cuInch: begin
          oneUnit:= PhysicalSizeToPixels(1/8, fImageBitmap.ResolutionUnit, fImageBitmap.ResolutionX, cssUnit) * xRatio;

          posNum:= 1;
          curPosI:= 1;   // 1/8 inch

          curPixelPos:= startX+(curPosI*oneUnit);
          while (curPixelPos <= endX) do
          begin
            Case posNum of
              1,3,5,7: fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y8, BGRALineColor, 1);
              2,6: fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y4, BGRALineColor, 1);
              4: fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y2, BGRALineColor, 1);
              8: begin
                   fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y1, BGRALineColor, 2);
                   DrawHorizontalText(IntToStr(curPosI div 8), Trunc(curPixelPos-1.5), yT, FontColor,
                                      taRightJustify, 12);
                   posNum:= 0;
                 end;
            end;

            inc(curPosI);
            inc(posNum);

            curPixelPos:= startX+(curPosI*oneUnit);
          end;
        end;
        TCSSUnit.cuPica: begin
          oneUnit:= PhysicalSizeToPixels(1, fImageBitmap.ResolutionUnit, fImageBitmap.ResolutionX, cssUnit) * xRatio;

          posNum:= 1;
          curPosI:= 1;   // 1 pica

          curPixelPos:= startX+(curPosI*oneUnit);
          while (curPixelPos <= endX) do
          begin
            Case posNum of
              1,2,4,5: fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y8, BGRALineColor, 1);
              3: fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y2, BGRALineColor, 1);
              6: begin
                   fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y1, BGRALineColor, 2);
                   DrawHorizontalText(IntToStr(curPosI), Trunc(curPixelPos-1.5), yT, FontColor,
                                      taRightJustify, 12);
                   posNum:= 0;
                 end;
            end;

            inc(curPosI);
            inc(posNum);

            curPixelPos:= startX+(curPosI*oneUnit);
          end;
        end;
        TCSSUnit.cuPoint: begin
          oneUnit:= PhysicalSizeToPixels(9, fImageBitmap.ResolutionUnit, fImageBitmap.ResolutionX, cssUnit) * xRatio;

          posNum:= 1;
          curPosI:= 1;   // 72/8 Points

          curPixelPos:= startX+(curPosI*oneUnit);
          while (curPixelPos <= endX) do
          begin
            Case posNum of
              1,3,5,7: fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y8, BGRALineColor, 1);
              2,6: fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y4, BGRALineColor, 1);
              4: fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y2, BGRALineColor, 1);
              8: begin
                   fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y1, BGRALineColor, 2);
                   DrawHorizontalText(IntToStr(curPosI * 9), Trunc(curPixelPos-1.5), yT, FontColor,
                                      taRightJustify, 12);
                   posNum:= 0;
                 end;
            end;

            inc(curPosI);
            inc(posNum);

            curPixelPos:= startX+(curPosI*oneUnit);
          end;
        end;
        TCSSUnit.cuPercent: begin
          oneUnit:= fImageBitmap.Width/100 * xRatio;

          posNum:= 1;
          curPosI:= 1;   // 1%

          curPixelPos:= startX+(curPosI*oneUnit);
          while (curPixelPos <= endX) do
          begin
            Case posNum of
              1..4, 6..9: fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y8, BGRALineColor, 1);
              5:  fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y2, BGRALineColor, 1);
              10: begin
                   fBitmap.DrawLineAntialias(curPixelPos, y0, curPixelPos, y1, BGRALineColor, 2);
                   DrawHorizontalText(IntToStr(curPosI), Trunc(curPixelPos-1.5), yT, FontColor,
                                      taRightJustify, 12);
                   posNum:= 0;
                 end;
            end;

            inc(curPosI);
            inc(posNum);

            curPixelPos:= startX+(curPosI*oneUnit);
          end;
        end;
      end;
   end;

   procedure DrawVertical(IsRight: Boolean);
   var
      x0, x1, x2, x4, x8, xT: Integer;
      txtAlign: TAlignment;

   begin
     if IsRight
     then begin
            x0:= fBitmap.Width-16; //Base
            x1:= x0+12;            //Unit
            x2:= x0+9;             // 1/2
            x4:= x0+6;             // 1/4
            x8:= x0+3;             // 1/8
            xT:= fBitmap.Width-1;  // Txt
            txtAlign:= taRightJustify;
            fBitmap.FillRect(x0, 0, fBitmap.Width, fBitmap.Height, BGRABackColor);
          end
     else begin
            x0:= 15;
            x1:= 3;
            x2:= 6;
            x4:= 9;
            x8:= 12;
            xT:= 1;
            txtAlign:= taLeftJustify;
            fBitmap.FillRect(0, 0, 16, fBitmap.Height, BGRABackColor);
          end;

     //Draw Y Rule
     with fOwner do
     Case cssUnit of
       TCSSUnit.cuPixel: begin
          posNum:= 0;
          curPosI:= 16;

          curPixelPos:= startY+(curPosI*yRatio);
          while (curPixelPos <= endY) do
          begin
            Case posNum of
              0, 1, 3: fBitmap.DrawLineAntialias(x0, curPixelPos, x8, curPixelPos, BGRALineColor, 1);
              2: fBitmap.DrawLineAntialias(x0, curPixelPos, x4, curPixelPos, BGRALineColor, 1);
              4: begin
                   fBitmap.DrawLineAntialias(x0, curPixelPos, x1, curPixelPos, BGRALineColor, 2);
                   DrawVerticalText(IntToStr(curPosI), xT, Trunc(curPixelPos-1.5), FontColor,
                                    txtAlign, taAlignBottom, 10);
                   posNum:= -1;
                 end;
            end;

            inc(curPosI, 16);
            inc(posNum);

            curPixelPos:= startY+(curPosI*yRatio);
          end;
        end;
        TCSSUnit.cuCentimeter: begin
          oneUnit:= PhysicalSizeToPixels(1/10, fImageBitmap.ResolutionUnit, fImageBitmap.ResolutionY, cssUnit) * yRatio;

          posNum:= 1;
          curPosI:= 1;   //mm

          curPixelPos:= startY+(curPosI*oneUnit);
          while (curPixelPos <= endY) do
          begin
            curPixelPos:= startY+(curPosI*oneUnit);

            Case posNum of
              1..4, 6..9: fBitmap.DrawLineAntialias(x0, curPixelPos, x8, curPixelPos, BGRALineColor, 1);
              5:  fBitmap.DrawLineAntialias(x0, curPixelPos, x2, curPixelPos, BGRALineColor, 1);
              10: begin
                   fBitmap.DrawLineAntialias(x0, curPixelPos, x1, curPixelPos, BGRALineColor, 2);
                   DrawVerticalText(IntToStr(curPosI div 10), xT, Trunc(curPixelPos-1.5), FontColor,
                                    txtAlign, taAlignBottom, 12);
                   posNum:= 0;
                 end;
            end;

            inc(curPosI);
            inc(posNum);

            curPixelPos:= startY+(curPosI*oneUnit);
          end;
        end;
        TCSSUnit.cuMillimeter: begin
          oneUnit:= PhysicalSizeToPixels(1, fImageBitmap.ResolutionUnit, fImageBitmap.ResolutionY, cssUnit) * yRatio;

          posNum:= 1;
          curPosI:= 1;   //mm

          curPixelPos:= startY+(curPosI*oneUnit);
          while (curPixelPos <= endY) do
          begin
            curPixelPos:= startY+(curPosI*oneUnit);

            Case posNum of
              1..4, 6..9: fBitmap.DrawLineAntialias(x0, curPixelPos, x8, curPixelPos, BGRALineColor, 1);
              5:  fBitmap.DrawLineAntialias(x0, curPixelPos, x2, curPixelPos, BGRALineColor, 1);
              10: begin
                   fBitmap.DrawLineAntialias(x0, curPixelPos, x1, curPixelPos, BGRALineColor, 2);
                   DrawVerticalText(IntToStr(curPosI), xT, Trunc(curPixelPos-1.5), FontColor,
                                    txtAlign, taAlignBottom, 10);
                   posNum:= 0;
                 end;
            end;

            inc(curPosI);
            inc(posNum);

            curPixelPos:= startY+(curPosI*oneUnit);
          end;
        end;
        TCSSUnit.cuInch: begin
          oneUnit:= PhysicalSizeToPixels(1/8, fImageBitmap.ResolutionUnit, fImageBitmap.ResolutionY, cssUnit) * yRatio;

          posNum:= 1;
          curPosI:= 1;   // 1/8 inch

          curPixelPos:= startY+(curPosI*oneUnit);
          while (curPixelPos <= endY) do
          begin
            curPixelPos:= startY+(curPosI*oneUnit);

            Case posNum of
              1,3,5,7: fBitmap.DrawLineAntialias(x0, curPixelPos, x8, curPixelPos, BGRALineColor, 1);
              2,6: fBitmap.DrawLineAntialias(x0, curPixelPos, x4, curPixelPos, BGRALineColor, 1);
              4: fBitmap.DrawLineAntialias(x0, curPixelPos, x2, curPixelPos, BGRALineColor, 1);
              8: begin
                    fBitmap.DrawLineAntialias(x0, curPixelPos, x1, curPixelPos, BGRALineColor, 2);
                    DrawVerticalText(IntToStr(curPosI div 8), xT, Trunc(curPixelPos-1.5), FontColor,
                                              txtAlign, taAlignBottom, 12);
                    posNum:= 0;
                  end;
            end;

            inc(curPosI);
            inc(posNum);

            curPixelPos:= startY+(curPosI*oneUnit);
          end;
        end;
        TCSSUnit.cuPica: begin
          oneUnit:= PhysicalSizeToPixels(1, fImageBitmap.ResolutionUnit, fImageBitmap.ResolutionY, cssUnit) * yRatio;

          posNum:= 1;
          curPosI:= 1;   // 1 pica

          curPixelPos:= startY+(curPosI*oneUnit);
          while (curPixelPos <= endY) do
          begin
            curPixelPos:= startY+(curPosI*oneUnit);

            Case posNum of
              1,2,4,5: fBitmap.DrawLineAntialias(x0, curPixelPos, x8, curPixelPos, BGRALineColor, 1);
              3: fBitmap.DrawLineAntialias(x0, curPixelPos, x2, curPixelPos, BGRALineColor, 1);
              6: begin
                    fBitmap.DrawLineAntialias(x0, curPixelPos, x1, curPixelPos, BGRALineColor, 2);
                    DrawVerticalText(IntToStr(curPosI), xT, Trunc(curPixelPos-1.5), FontColor,
                                              txtAlign, taAlignBottom, 12);
                    posNum:= 0;
                  end;
            end;

            inc(curPosI);
            inc(posNum);

            curPixelPos:= startY+(curPosI*oneUnit);
          end;
        end;
        TCSSUnit.cuPoint: begin
          oneUnit:= PhysicalSizeToPixels(9, fImageBitmap.ResolutionUnit, fImageBitmap.ResolutionY, cssUnit) * yRatio;

          posNum:= 1;
          curPosI:= 1;   // 72/8 Points

          curPixelPos:= startY+(curPosI*oneUnit);
          while (curPixelPos <= endY) do
          begin
            curPixelPos:= startY+(curPosI*oneUnit);

            Case posNum of
              1,3,5,7: fBitmap.DrawLineAntialias(x0, curPixelPos, x8, curPixelPos, BGRALineColor, 1);
              2,6: fBitmap.DrawLineAntialias(x0, curPixelPos, x4, curPixelPos, BGRALineColor, 1);
              4: fBitmap.DrawLineAntialias(x0, curPixelPos, x2, curPixelPos, BGRALineColor, 1);
              8: begin
                    fBitmap.DrawLineAntialias(x0, curPixelPos, x1, curPixelPos, BGRALineColor, 2);
                    DrawVerticalText(IntToStr(curPosI * 9), xT, Trunc(curPixelPos-1.5), FontColor,
                                              txtAlign, taAlignBottom, 12);
                    posNum:= 0;
                  end;
            end;

            inc(curPosI);
            inc(posNum);

            curPixelPos:= startY+(curPosI*oneUnit);
          end;
        end;
        TCSSUnit.cuPercent: begin
          oneUnit:= fImageBitmap.Height/100 * yRatio;

          posNum:= 1;
          curPosI:= 1;   // 1%

          curPixelPos:= startY+(curPosI*oneUnit);
          while (curPixelPos <= endY) do
          begin
            curPixelPos:= startY+(curPosI*oneUnit);

            Case posNum of
              1..4, 6..9: fBitmap.DrawLineAntialias(x0, curPixelPos, x8, curPixelPos, BGRALineColor, 1);
              5:  fBitmap.DrawLineAntialias(x0, curPixelPos, x2, curPixelPos, BGRALineColor, 1);
              10: begin
                   fBitmap.DrawLineAntialias(x0, curPixelPos, x1, curPixelPos, BGRALineColor, 2);
                   DrawVerticalText(IntToStr(curPosI), xT, Trunc(curPixelPos-1.5), FontColor,
                                    txtAlign, taAlignBottom, 10);
                   posNum:= 0;
                 end;
            end;

            inc(curPosI);
            inc(posNum);

            curPixelPos:= startY+(curPosI*oneUnit);
          end;
        end;
      end;
   end;

   procedure DrawPhysicalUnit;
   var
      txt: TBGRATextEffect;
      posX, posY: Integer;
      drawVert: Boolean;
      txtAlign: TAlignment;

   begin
     try
        txt:= TBGRATextEffect.Create(PhysicalUnitShortName[rPhysicalUnit], rFont.Name, 16, True);

        drawVert:= False;
        txtAlign:= taLeftJustify;

        //Set the position according to the visible sides
        if (rsdTop in rSides)
        then posY:= 3-(txt.TextHeight div 2)
        else if (rsdBottom in rSides)
             then posY:= fBitmap.Height-8-(txt.TextHeight div 2)
             else begin
                    drawVert:= True;
                    posY:= 3-(txt.TextHeight div 2);
                  end;

        if (rsdLeft in rSides)
        then posX:= 1
        else if (rsdRight in rSides)
             then begin
                    posX:= fBitmap.Width-1;
                    if drawVert
                    then txtAlign:= taRightJustify
                    else dec(posX, txt.TextWidth);
                  end
             else posX:= 1;

        if drawVert then
        begin
          DrawVerticalText(PhysicalUnitShortName[rPhysicalUnit], posX, posY, FontColor,
                           txtAlign, taAlignTop, 16);
        end
        else txt.Draw(fBitmap, posX, posY, FontColor, taLeftJustify);

        txt.Free;

     except
        txt.Free;
     end;
   end;

begin
  fBitmap.FillTransparent;
  if (rSides = []) then exit;

  with fOwner do
  try
    startX:= 0;
    startY:= 0;
    if (rsdLeft in rSides) then startX:= 16;
    if (rsdTop in rSides) then startY:= 16;

    if rStopToImage
    then begin
           endX:= fResampledBitmap.Width+startX;
           endY:= fResampledBitmap.Height+startY;
         end
    else begin
           endX:= fOwner.WorkRect.Width+startX;
           endY:= fOwner.WorkRect.Height+startY;
         end;

    cssUnit:= PhysicalToCSSUnit(rPhysicalUnit);
    FontColor:= ColorToBGRA(rFont.Color);

    if (rsdLeft in rSides) then DrawVertical(False);
    if (rsdTop in rSides) then DrawHorizontal(True);
    if (rsdRight in rSides) then DrawVertical(True);
    if (rsdBottom in rSides) then DrawHorizontal(False);

    if rShowPhysicalUnit then DrawPhysicalUnit;

  finally
  end;
end;

procedure TBGRAIMRulers.Render_Invalidate;
begin
  with fOwner do
  if not(csLoading in ComponentState) then
  begin
    Self.Render;
    Render_Invalidate;
  end;
end;

constructor TBGRAIMRulers.Create(AOwner: TBGRAImageManipulation);
begin
  inherited Create;

  fOwner:= AOwner;
  fBitmap:= TBGRABitmap.Create;
  rFont:= TFont.Create;
  rFont.Assign(fOwner.Font);

  rSides:= [];
  rPhysicalUnit:= cuCentimeter;
  rBackgroundColor:= clWhite; BGRABackColor:= BGRAWhite;
  rLineColor:= clBlack; BGRALineColor:= BGRABlack;
  rShowPhysicalUnit:= False;
  rStopToImage:= True;
end;

destructor TBGRAIMRulers.Destroy;
begin
  fBitmap.Free;
  rFont.Free;

  inherited Destroy;
end;

function TBGRAIMRulers.OverPos(APos: TPoint): TRulersSides;
var
   iRect: TRect;

begin
  Result:= [];
  if (rSides <> []) then
  begin
    iRect:= Rect(fOwner.fBorderSize, fOwner.fBorderSize, fBitmap.Width-fOwner.fBorderSize, fBitmap.Height-fOwner.fBorderSize);

    if (rsdTop in rSides) and
        PtInRect(APos, Rect(iRect.Left, iRect.Top, iRect.Right, 16))
    then Result:= Result+[rsdTop];

    if (rsdBottom in rSides) and
        PtInRect(APos, Rect(iRect.Left, iRect.Bottom-16, iRect.Right, iRect.Bottom))
    then Result:= Result+[rsdBottom];

    if (rsdLeft in rSides) and
        PtInRect(APos, Rect(iRect.Left, iRect.Top, 16, iRect.Bottom))
    then Result:= Result+[rsdLeft];

    if (rsdRight in rSides) and
        PtInRect(APos, Rect(iRect.Right-16, iRect.Top, iRect.Right, iRect.Bottom))
    then Result:= Result+[rsdRight];
  end;
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
      // If the motion is in a positive direction, make sure we're not going out of bounds
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
      // If the motion is in a negative direction, make sure we're not going out of bounds
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

begin
  // Determine picture size
  imageWidth  := Picture.Width;
  imageHeight := Picture.Height;

  // Determine Work rectangle to final size
  finalWidth := WorkRect.Right - WorkRect.Left;
  finalHeight := WorkRect.Bottom - WorkRect.Top;

  if (imageWidth = 0) or (imageHeight = 0) then exit(WorkRect);

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

{ Calculate work area rectangle }
procedure TBGRAImageManipulation.CalculateWorkRect;
begin
  WorkRect:= GetClientRect;

  //Remove borders
  InflateRect(WorkRect, -fBorderSize, -fBorderSize);

  //Remove Rulers areas
  if (rsdLeft in rRulers.Sides) then inc(WorkRect.Left, 16);
  if (rsdRight in rRulers.Sides) then dec(WorkRect.Right, 16);
  if (rsdTop in rRulers.Sides) then inc(WorkRect.Top, 16);
  if (rsdBottom in rRulers.Sides) then dec(WorkRect.Bottom, 16);

  // Calculate the Aspect Ratio for size limits
  with fSizeLimits do
  begin
    minWidth:= fAspectX;
    maxWidth:= WorkRect.Right - WorkRect.Left;
    minHeight:= fAspectY;
    maxHeight:= WorkRect.Bottom - WorkRect.Top;
  end;
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
     rCropRect :=rCropArea.ScaledArea;
     rCropRectI :=rCropRect;
     rCropRectI.Inflate(AnchorSize, AnchorSize);

     if ({$IFNDEF FPC}BGRAGraphics.{$ENDIF}PtInRect(rCropRectI, APoint)) then
     begin
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
  fResampledBitmap.SetSize(DestinationRect.Right, DestinationRect.Bottom);
  try
     // Recalculate Ratio
     xRatio:= fResampledBitmap.Width / fImageBitmap.Width;
     yRatio:= fResampledBitmap.Height / fImageBitmap.Height;
  except
     xRatio:= 1;
     yRatio:= 1;
  end;

  if Self.Empty
  then fResampledBitmap.FillTransparent
  else try
          tempBitmap:= fImageBitmap.Resample(DestinationRect.Right, DestinationRect.Bottom, rmFineResample);
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
  inherited Loaded;

  CalculateWorkRect;
  if Self.Empty then CreateEmptyImage;
  CreateResampledBitmap;
  //ResizeVirtualScreen is done on Resize
end;

 { ============================================================================ }
 { =====[ Component Definition ]=============================================== }
 { ============================================================================ }

constructor TBGRAImageManipulation.Create(AOwner: TComponent);
var
   fGCD     :integer;

begin
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
  rEnabledWorkArea := True;

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

  // Create Rulers
  rRulers:= TBGRAIMRulers.Create(Self);

  // Create the Image Bitmap
  fImageBitmap := TBGRABitmap.Create;

  // Create the Resampled Bitmap
  fResampledBitmap := TBGRABitmap.Create;
  xRatio:= 1;
  yRatio:= 1;

  // Create the Background
  fBackground := TBGRABitmap.Create;

  // Create render surface
  fVirtualScreen := TBGRABitmap.Create(Width, Height);
  rOpacity:= 128;

  rEmptyImage :=TBGRAEmptyImage.Create(Self);
  rNewCropAreaDefault :=TBGRANewCropAreaDefault.Create;

  // Initialize crop area
  rCropAreas :=TCropAreaList.Create(Self);
  rCropAreas.Name:='CropAreas';
  rNewCropArea :=Nil;
  rSelectedCropArea :=Nil;

  fMouseCaught := False;
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
  rRulers.Free;

  inherited Destroy;
end;

procedure TBGRAImageManipulation.Paint;
begin
  inherited Paint;

  fVirtualScreen.Draw(Canvas, 0, 0, True);
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
  fBackground.FillTransparent;

  // Draw the outer bevel
  Border := Rect(0, 0, fBackground.Width, fBackground.Height);

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

  DrawCheckers(fBackground, WorkRect);
end;

procedure TBGRAImageManipulation.Render_All;
begin
  // Render Bitmaps
  RenderBackground;
  rRulers.Render;
  Render;
end;

{ Resize the render images }
procedure TBGRAImageManipulation.ResizeVirtualScreen;
var
  i              :Integer;
  curCropArea    :TCropArea;

begin
  if (fVirtualScreen <> nil) then
  begin
    fVirtualScreen.SetSize(Max(Self.Width, (fBorderSize * 2 + fAnchorSize + fMinWidth)),
                           Max(Self.Height, (fBorderSize * 2 + fAnchorSize + fMinHeight)));
    fVirtualScreen.InvalidateBitmap;

    // Resize Rulers
    if (rRulers.Sides <> [])
    then rRulers.fBitmap.SetSize(fVirtualScreen.Width-(fBorderSize * 2), fVirtualScreen.Height-(fBorderSize * 2))
    else rRulers.fBitmap.SetSize(0,0);
    rRulers.fBitmap.InvalidateBitmap;

    // Resize background
    fBackground.SetSize(fVirtualScreen.Width, fVirtualScreen.Height);
    fBackground.InvalidateBitmap;

    // Re position of Crop Areas
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
  end;
end;

procedure TBGRAImageManipulation.DoOnResize;
begin
  CalculateWorkRect;
  CreateResampledBitmap;
  ResizeVirtualScreen;

  Render_All;

  inherited DoOnResize;
end;

{ Function responsible for rendering the content of the component, including
  the selection border and anchors. The selected area is painted with a
  different transparency level for easy viewing of what will be cut. }
procedure TBGRAImageManipulation.Render;
var
  emptyRect: TRect;
  Mask: TBGRABitmap;
  BorderColor, SelectColor,
  FillColor, IcoColor: TBGRAPixel;
  curCropArea :TCropArea;
  curCropAreaRect :TRect;
  i: Integer;
  curTxt: String;
  TextS: TTextStyle;
  Grad : TBGRAGradientScanner;

begin
  // Draw background
  fVirtualScreen.BlendImage(0, 0, fBackground, boLinearBlend);

  // Draw Rulers
  fVirtualScreen.BlendImage(fBorderSize, fBorderSize, rRulers.fBitmap, boLinearBlend);

  try
    //Colors
    BorderColor := BGRAWhite;
    SelectColor := BGRA(255, 255, 0, 255);
    FillColor := BGRA(255, 255, 0, rOpacity);

    //Create Mask area
    Mask := TBGRABitmap.Create(WorkRect.Width, WorkRect.Height, BGRA(0, 0, 0, rOpacity));

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

function TBGRAImageManipulation.GetEnabledWorkArea: Boolean;
begin
  Result:= Enabled and rEnabledWorkArea;
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
  Result:= (fImageBitmap.Width = 0) or (fImageBitmap.Height = 0) or fImageBitmap.Empty;
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
  if not(Empty) then
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
      if (Value = nil) or (Value.Width = 0) or (Value.Height = 0) or Value.Empty
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
      if not(csLoading in ComponentState) then
      begin
        rRulers.Render;
        Render_Invalidate;
      end;
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
    rRulers.Render;
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
    rRulers.Render;
    Render_Invalidate;
    TempBitmap.Free;
  end;
end;

procedure TBGRAImageManipulation.RefreshBitmap;
begin
  ResizeVirtualScreen;
  Render_All;
  Invalidate;
end;

procedure TBGRAImageManipulation.tests;
begin
  // Self.AutoSize:=False;
  // Render;
  // Refresh;
end;

function TBGRAImageManipulation.addCropArea(AArea: TRectF; AAreaUnit: TPhysicalUnit;
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
  Result :=Self.addCropArea(RectF(0,0,0,0), rNewCropAreaDefault.rPhysicalUnit, AUserData);
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
   curCropAreaRect :TPhysicalRect;
   curCropArea :TCropArea;
   mWidth, mHeight:Single;

begin
  if (rCropAreas.Count>0) then
  begin
     if ReduceLarger
     then begin
            mWidth:=0;
            mHeight:=0;
          end
     else begin
            mWidth:=EmptyImage.PhysicalWidth;
            mHeight:=EmptyImage.PhysicalHeight;
            if (mWidth=0) or (mHeight=0) then
            begin
              mWidth:= PixelsToPhysicalSize(fImageBitmap.Width, fImageBitmap.ResolutionUnit, fImageBitmap.ResolutionX,
                                            PhysicalToCSSUnit(EmptyImage.PhysicalUnit));
              mHeight:= PixelsToPhysicalSize(fImageBitmap.Height, fImageBitmap.ResolutionUnit, fImageBitmap.ResolutionY,
                                             PhysicalToCSSUnit(EmptyImage.PhysicalUnit));
            end;
          end;

     for i:=0 to rCropAreas.Count-1 do
     begin
       curCropArea :=rCropAreas[i];
       curCropAreaRect :=curCropArea.Area;

       PhysicalSizeConvert(curCropAreaRect, PhysicalToCSSUnit(EmptyImage.PhysicalUnit), fImageBitmap);

        if (curCropAreaRect.Right > mWidth)
        then mWidth :=curCropAreaRect.Right;
        if (curCropAreaRect.Bottom > mHeight)
        then mHeight :=curCropAreaRect.Bottom;
     end;

     SetEmptyImageSize(EmptyImage.PhysicalUnit, mWidth, mHeight);
  end;
end;

procedure TBGRAImageManipulation.SetEmptyImageSizeToNull;
begin
  SetEmptyImageSize(cuCentimeter, 0, 0);
end;

procedure TBGRAImageManipulation.SetEmptyImageSize(APhysicalUnit: TPhysicalUnit; APhysicalWidth, APhysicalHeight: Single);
begin
  EmptyImage.PhysicalUnit:= APhysicalUnit;
  EmptyImage.rPhysicalWidth:= APhysicalWidth;
  EmptyImage.rPhysicalHeight:= APhysicalHeight;

  if Self.Empty then
  begin
    CreateEmptyImage;
    CreateResampledBitmap;
  end;

  Render_Invalidate;
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

    if not(csLoading in ComponentState) then
    begin
      //Change size of WorkRect
      CalculateWorkRect;

      //Recreate the Resampled Bitmap because sizes and Ratio are Changed
      if Self.Empty then CreateEmptyImage;
      CreateResampledBitmap;

      Render_All;
      Invalidate;
    end;
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

  if assigned(rOnCropAreaSelectedChanged)
  then rOnCropAreaSelectedChanged(Self, oldSelected);
end;


 { ============================================================================ }
 { =====[ Event Control ]====================================================== }
 { ============================================================================ }

procedure TBGRAImageManipulation.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  ACursor :TCursor;
  mouseInWorkRect: Boolean;

begin
  mouseInWorkRect:= (X >= WorkRect.Left) and (X <= WorkRect.Right) and
                    (Y >= WorkRect.Top) and (Y <= WorkRect.Bottom);

  if not(rEnabledWorkArea) and mouseInWorkRect then exit;

  // Call the inherited MouseDown() procedure
  inherited MouseDown(Button, Shift, X, Y);

  // If over control
  if  mouseInWorkRect and (Button = mbLeft) and not(ssDouble in Shift) then
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
            then fStartPoint := Point(fStartArea.Right, fStartArea.Bottom);

            if (fAnchorSelected = [SOUTH, WEST])
            then fStartPoint := Point(fStartArea.Right, fStartArea.Top);

            if ((fAnchorSelected = [SOUTH]) or (fAnchorSelected = [EAST]) or
                (fAnchorSelected = [SOUTH, EAST]))
            then fStartPoint := Point(fStartArea.Left, fStartArea.Top);

            if (fAnchorSelected = [NORTH, EAST])
            then fStartPoint := Point(fStartArea.Left, fStartArea.Bottom);
         end;
  end;
end;

procedure TBGRAImageManipulation.MouseMove(Shift: TShiftState; X, Y: integer);
var
  needRepaint,
  mouseInWorkRect: Boolean;
  newCoords: TCoord;
  Direction: TDirection;
  Bounds,
  selAreaRect: TRect;
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
       selAreaRect :=SelectedCropArea.ScaledArea;
       selAreaRect.Left :=fEndPoint.X-fStartPoint.X;    //fStartPoint is Relative to CropArea
       selAreaRect.Top :=fEndPoint.Y-fStartPoint.Y;

       //Out of Bounds check
       if (selAreaRect.Left<0)
       then selAreaRect.Left :=0;

       if (selAreaRect.Top<0)
       then selAreaRect.Top :=0;

       if (selAreaRect.Left+fStartArea.Width>Bounds.Right)
       then selAreaRect.Left :=Bounds.Right-fStartArea.Width;

       if (selAreaRect.Top+fStartArea.Height>Bounds.Bottom)
       then selAreaRect.Top :=Bounds.Bottom-fStartArea.Height;

       selAreaRect.Width :=fStartArea.Width;
       selAreaRect.Height:=fStartArea.Height;
       SelectedCropArea.ScaledArea :=selAreaRect;

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
           x2 := fEndPoint.X - Abs(fStartArea.Right - fStartArea.Left) div 2;
           y2 := fEndPoint.Y;
         end
         else
         if (fAnchorSelected = [SOUTH]) then
         begin
           x2 := fEndPoint.X + Abs(fStartArea.Right - fStartArea.Left) div 2;
           y2 := fEndPoint.Y;
         end
         else
         if (fAnchorSelected = [EAST]) then
         begin
           x2 := fEndPoint.X;
           y2 := fEndPoint.Y + Abs(fStartArea.Bottom - fStartArea.Top) div 2;
         end
         else
         if (fAnchorSelected = [WEST]) then
         begin
           x2 := fEndPoint.X;
           y2 := fEndPoint.Y - Abs(fStartArea.Bottom - fStartArea.Top) div 2;
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
  mouseInWorkRect:= (X >= WorkRect.Left) and (X <= WorkRect.Right) and
                    (Y >= WorkRect.Top) and (Y <= WorkRect.Bottom);

  if not(rEnabledWorkArea) and mouseInWorkRect then exit;

  // Call the inherited MouseMove() procedure
  inherited MouseMove(Shift, X, Y);

  // Set default cursor
  Cursor := crDefault;

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
           Render_Invalidate;
         end;
       end
  else begin
         // If the mouse is just moving over the control, and wasn't originally click in the control
         if mouseInWorkRect then
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
end;

procedure TBGRAImageManipulation.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  needRepaint,
  mouseInWorkRect: Boolean;
  mouseRulers: TRulersSides;

begin
  mouseInWorkRect:= (X >= WorkRect.Left) and (X <= WorkRect.Right) and
                    (Y >= WorkRect.Top) and (Y <= WorkRect.Bottom);

  if not(rEnabledWorkArea) and mouseInWorkRect then exit;

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
      Render_Invalidate;
    end;
  end
  else
  begin
    //Mouse is Outside WorkRect, test wich Rulers is affected
    if Assigned(rOnRulersMouseUp) then
    begin
      mouseRulers:= rRulers.OverPos(Point(X, Y));
      if (mouseRulers <> []) then rOnRulersMouseUp(Self, mouseRulers, Button, Shift, X,Y);
    end;
  end;
end;

procedure TBGRAImageManipulation.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
var
   xAnchorSelected :TDirection;
   xCursor :TCursor;
   mouseCropArea:TCropArea;
   mouseRulers: TRulersSides;
   mouseInWorkRect: Boolean;

begin
  mouseInWorkRect:= (MousePos.X >= WorkRect.Left) and (MousePos.X <= WorkRect.Right) and
                    (MousePos.Y >= WorkRect.Top) and (MousePos.Y <= WorkRect.Bottom);

  if not(rEnabledWorkArea) and mouseInWorkRect then exit;

  Handled:= False;

  if mouseInWorkRect
  then begin
         //Mouse is Inside WorkRect, test wich CropArea/Anchor is affected
         if Assigned(rOnCropAreaPopup) then
         begin
           mouseCropArea:= Self.isOverAnchor(MousePos, xAnchorSelected, {%H-}xCursor);
           if (mouseCropArea <> nil) then
           begin
             rOnCropAreaPopup(Self, mouseCropArea, xAnchorSelected, MousePos, Handled);
             if Handled then exit;
           end;
         end;
       end
  else begin
         //Mouse is Outside WorkRect, test wich Rulers is affected
         if Assigned(rOnRulersPopup) then
         begin
           mouseRulers:= rRulers.OverPos(MousePos);
           if (mouseRulers <> []) then
           begin
             rOnRulersPopup(Self, mouseRulers, MousePos, Handled);
             if Handled then exit;
           end;
         end;
       end;

  inherited DoContextPopup(MousePos, Handled);
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
