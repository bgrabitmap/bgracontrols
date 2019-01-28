{
  Created by BGRA Controls Team
  Dibo, Circular, lainz (007) and contributors.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCImageButton;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  {$IFDEF FPC}LCLType, LResources, LMessages,{$ENDIF} ExtCtrls,
  Types,
  {$IFNDEF FPC}Windows, Messages, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  { BGRAControls }
  BCBaseCtrls, BCEffect,
  { BGRABitmap }
  BGRABitmap, BGRABitmapTypes, BGRASliceScaling;

{off $DEFINE DEBUG}

function CalculateAspectRatioH(W1, H1, W2: integer): integer; //result H2
function CalculateAspectRatioW(W1, H1, H2: integer): integer; //result W2
function CalculateDestRect(ImageW, ImageH, DestW, DestH: integer;
  Stretch, Proportional, Center: boolean): TRect;
procedure AssignFontToBGRA(Source: TFont; Dest: TBGRABitmap);

type
  TBCGraphicButtonState = (gbsNormal, gbsHover, gbsActive, gbsDisabled);

  TOnRenderControl = procedure(Sender: TObject; Bitmap: TBGRABitmap;
    State: TBCGraphicButtonState) of object;

type

  { TBCGraphicButton }

  TBCGraphicButton = class(TBCGraphicControl)
  protected
    FState: TBCGraphicButtonState;
    FModalResult: TModalResult;
  protected
    procedure DoClick; virtual;
    procedure DoMouseDown; virtual;
    procedure DoMouseUp; virtual;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure DoMouseMove({%H-}x, {%H-}y: integer); virtual;
  protected
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
  public
    property ModalResult: TModalResult
      read FModalResult write FModalResult default mrNone;
  end;

  { TBCXButton }
  TBCXButton = class(TBCGraphicButton)
  protected
    FOnRenderControl: TOnRenderControl;
    FBGRANormal, FBGRAHover, FBGRAActive, FBGRADisabled: TBGRABitmap;
  protected
    class function GetControlClassDefaultSize: TSize; override;
    procedure DrawControl; override;
    procedure RenderControl; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnRenderControl: TOnRenderControl
      read FOnRenderControl write FOnRenderControl;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBidiMode;
    property ModalResult;
    {$IFDEF FPC}
    property OnChangeBounds;
    {$ENDIF}
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;

  { TBCSliceScalingOptions }

  TBCCustomSliceScalingOptions = class(TPersistent)
  protected
    FOwner: TControl;
    FBitmap: TBGRABitmap;
    FAutoDetectRepeat, FRepeatTop, FRepeatLeft, FRepeatMiddleHorizontal,
    FRepeatMiddleVertical, FRepeatRight, FRepeatBottom: boolean;
    FMarginTop, FMarginRight, FMarginBottom, FMarginLeft, FNumberOfItems: integer;
    FDirection: TSliceScalingDirection;
    FDrawMode: TDrawMode;
    FResampleMode: TResampleMode;
    FResampleFilter: TResampleFilter;
  private
    procedure SetFBitmap(AValue: TBGRABitmap);
    procedure SetFMarginBottom(AValue: integer);
    procedure SetFMarginLeft(AValue: integer);
    procedure SetFMarginRight(AValue: integer);
    procedure SetFMarginTop(AValue: integer);
    procedure SetFAutoDetectRepeat(AValue: boolean);
    procedure SetFDirection(AValue: TSliceScalingDirection);
    procedure SetFDrawMode(AValue: TDrawMode);
    procedure SetFNumberOfItems(AValue: integer);
    procedure SetFRepeatBottom(AValue: boolean);
    procedure SetFRepeatLeft(AValue: boolean);
    procedure SetFRepeatMiddleHorizontal(AValue: boolean);
    procedure SetFRepeatMiddleVertical(AValue: boolean);
    procedure SetFRepeatRight(AValue: boolean);
    procedure SetFRepeatTop(AValue: boolean);
    procedure SetFResampleFilter(AValue: TResampleFilter);
    procedure SetFResampleMode(AValue: TResampleMode);
  public
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
  published
    property Bitmap: TBGRABitmap read FBitmap write SetFBitmap;
    property AutoDetectRepeat: boolean read FAutoDetectRepeat
      write SetFAutoDetectRepeat default False;
    property RepeatTop: boolean read FRepeatTop write SetFRepeatTop default False;
    property RepeatLeft: boolean read FRepeatLeft write SetFRepeatLeft default False;
    property RepeatMiddleHorizontal: boolean
      read FRepeatMiddleHorizontal write SetFRepeatMiddleHorizontal default False;
    property RepeatMiddleVertical: boolean read FRepeatMiddleVertical
      write SetFRepeatMiddleVertical default False;
    property RepeatRight: boolean read FRepeatRight write SetFRepeatRight default False;
    property RepeatBottom: boolean
      read FRepeatBottom write SetFRepeatBottom default False;
    property MarginTop: integer read FMarginTop write SetFMarginTop default 0;
    property MarginRight: integer read FMarginRight write SetFMarginRight default 0;
    property MarginBottom: integer read FMarginBottom write SetFMarginBottom default 0;
    property MarginLeft: integer read FMarginLeft write SetFMarginLeft default 0;
    property NumberOfItems: integer
      read FNumberOfItems write SetFNumberOfItems default 1;
    property Direction: TSliceScalingDirection read FDirection write SetFDirection;
    property DrawMode: TDrawMode read FDrawMode write SetFDrawMode default
      dmDrawWithTransparency;
    property ResampleMode: TResampleMode read FResampleMode
      write SetFResampleMode default rmFineResample;
    property ResampleFilter: TResampleFilter read FResampleFilter
      write SetFResampleFilter default rfBestQuality;
  end;

  { TBCImageButtonSliceScalingOptions }

  TBCImageButtonSliceScalingOptions = class(TBCCustomSliceScalingOptions)
  private
    procedure SetFCenter(AValue: boolean);
    procedure SetFProportional(AValue: boolean);
    procedure SetFStretch(AValue: boolean);
  protected
    FCenter, FStretch, FProportional: boolean;
  published
    property NumberOfItems: integer read FNumberOfItems default 4;
    property Center: boolean read FCenter write SetFCenter default True;
    property Stretch: boolean read FStretch write SetFStretch default True;
    property Proportional: boolean
      read FProportional write SetFProportional default False;
  public
    constructor Create(AOwner: TControl);
    procedure Assign(Source: TPersistent); override;
  end;

  { TBCCustomImageButton }

  TBCCustomImageButton = class(TBCGraphicButton)
  private
    { Private declarations }
    FAlphaTest: boolean;
    FAlphaTestValue: byte;
    {$IFDEF INDEBUG}
    FDrawCount: integer;
    FRenderCount: integer;
    {$ENDIF}
    FBitmapOptions: TBCImageButtonSliceScalingOptions;
    FBGRAMultiSliceScaling: TBGRAMultiSliceScaling;
    FBGRANormal, FBGRAHover, FBGRAActive, FBGRADisabled: TBGRABitmap;
    FDestRect: TRect;
    FPressed: boolean;
    FTimer: TTimer;
    FFade: TFading;
    FAnimation: boolean;
    FBitmapFile: string;
    FTextVisible: boolean;
    FToggle: boolean;
    FMouse: TPoint;
    procedure SetFAlphaTest(AValue: boolean);
    procedure SetFAlphaTestValue(AValue: byte);
    procedure SetFAnimation(AValue: boolean);
    procedure SetFBitmapFile(AValue: string);
    procedure SetFBitmapOptions(AValue: TBCImageButtonSliceScalingOptions);
    procedure Fade({%H-}Sender: TObject);
    procedure SetFPressed(AValue: boolean);
    procedure SetFTextVisible(AValue: boolean);
    procedure SetFToggle(AValue: boolean);
  protected
    { Protected declarations }
    procedure DrawControl; override;
    procedure RenderControl; override;
    procedure TextChanged; override;
    procedure FontChanged(Sender: TObject); override;
    procedure CMChanged(var {%H-}Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message CM_CHANGED; {$IFDEF FPC}virtual;{$ENDIF}
    {$IFDEF INDEBUG}
    {$IFDEF FPC}
    function GetDebugText: string;
    {$ENDIF}
    {$ENDIF}
    procedure DoMouseDown; override;
    procedure DoMouseUp; override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure DoMouseMove(x, y: integer); override;
    procedure Click; override;
  public
    { Public declarations }
    property AlphaTest: boolean read FAlphaTest write SetFAlphaTest default True;
    property AlphaTestValue: byte
      read FAlphaTestValue write SetFAlphaTestValue default 255;
    property Toggle: boolean read FToggle write SetFToggle default False;
    property Pressed: boolean read FPressed write SetFPressed default False;
    //property State: TBCGraphicButtonState read FState;
    property BitmapOptions: TBCImageButtonSliceScalingOptions
      read FBitmapOptions write SetFBitmapOptions;
    property Animation: boolean read FAnimation write SetFAnimation default True;
    property BitmapFile: string read FBitmapFile write SetFBitmapFile;
    property TextVisible: boolean read FTextVisible write SetFTextVisible default True;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { It loads the 'BitmapFile' }
    procedure LoadFromBitmapResource(const Resource: string; ResourceType: PChar); overload;
    procedure LoadFromBitmapResource(const Resource: string); overload;
    procedure LoadFromBitmapFile;
    procedure Assign(Source: TPersistent); override;
    { Streaming }
    {$IFDEF FPC}
    procedure SaveToFile(AFileName: string); override;
    procedure LoadFromFile(AFileName: string); override;
    procedure AssignFromFile(AFileName: string); override;
    {$ENDIF}
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
  published
    { Published declarations }
  end;

  TBCImageButton = class(TBCCustomImageButton)
  published
    property AlphaTest;
    property AlphaTestValue;
    property Action;
    property Align;
    property Anchors;
    property Animation;
    property AutoSize;
    //property AutoSizeExtraHorizontal;
    //property AutoSizeExtraVertical;
    property BidiMode;
    //property Bitmap;
    property BitmapFile;
    property BitmapOptions;
    property BorderSpacing;
    property Caption;
    //property Checked;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ModalResult;
    {$IFDEF FPC}
    property OnChangeBounds;
    {$ENDIF}
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    //property OnPlaySound;
    //property OnRedraw;
    property OnResize;
    property OnStartDrag;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    //property Shadow;
    property ShowHint;
    //property Sound;
    //property SoundClick;
    //property SoundEnter;
    property TextVisible;
    property Toggle;
    property Pressed;
    property Visible;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

{$IFDEF FPC}procedure Register;
begin
  {$IFDEF FPC}
  {$I icons\bcimagebutton_icon.lrs}
  {$ENDIF}
  RegisterComponents('BGRA Button Controls', [TBCImageButton]);
  //{$I icons\bcxbutton_icon.lrs}
  RegisterComponents('BGRA Button Controls', [TBCXButton]);
end;
{$ENDIF}

function CalculateAspectRatioH(W1, H1, W2: integer): integer;
begin
  Result := Round(H1 / W1 * W2);
end;

function CalculateAspectRatioW(W1, H1, H2: integer): integer;
begin
  Result := Round(W1 / H1 * H2);
end;

function CalculateDestRect(ImageW, ImageH, DestW, DestH: integer;
  Stretch, Proportional, Center: boolean): TRect;
var
  w: integer;
  h: integer;
begin
  // Stretch or Proportional when Image (Width or Height) is bigger than Destination
  if Stretch or (Proportional and ((ImageW > DestW) or (ImageH > DestH))) then
  begin
    // Proportional when Image (Width or Height) is bigger than 0
    if Proportional and (ImageW > 0) and (ImageH > 0) then
    begin
      w := DestW;
      h := CalculateAspectRatioH(ImageW, ImageH, DestW);
      if h > DestH then
      begin
        h := DestH;
        w := CalculateAspectRatioW(ImageW, ImageH, DestH);
      end;
      ImageW := w;
      ImageH := h;
    end
    // Stretch not Proportional or when Image (Width or Height) is 0
    else
    begin
      ImageW := DestW;
      ImageH := DestH;
    end;
  end;

  Result := Rect(0, 0, ImageW, ImageH);

  // Center: Destination (Width or Height) - Image divided by 2
  if Center then
  begin
    Result.Left := Round((DestW - ImageW) div 2);
    Result.Top := Round((DestH - ImageH) div 2);
  end;
end;

procedure AssignFontToBGRA(Source: TFont; Dest: TBGRABitmap);
begin
  Dest.FontAntialias := True;

  Dest.FontName := Source.Name;
  Dest.FontStyle := Source.Style;
  Dest.FontOrientation := Source.Orientation;

  case Source.Quality of
    fqNonAntialiased: Dest.FontQuality := fqSystem;
    fqAntialiased: Dest.FontQuality := fqFineAntialiasing;
    fqProof: Dest.FontQuality := fqFineClearTypeRGB;
    fqDefault, fqDraft, fqCleartype, fqCleartypeNatural: Dest.FontQuality :=
        fqSystemClearType;
  end;

  Dest.FontHeight := -Source.Height;
end;

{ TBCXButton }

class function TBCXButton.GetControlClassDefaultSize: TSize;
begin
  Result := inherited GetControlClassDefaultSize;
end;

procedure TBCXButton.DrawControl;
begin
  if Enabled then
    case FState of
      gbsNormal: FBGRANormal.Draw(Canvas, 0, 0, False);
      gbsHover: FBGRAHover.Draw(Canvas, 0, 0, False);
      gbsActive: FBGRAActive.Draw(Canvas, 0, 0, False);
    end
  else
    FBGRADisabled.Draw(Canvas, 0, 0, False);
end;

procedure TBCXButton.RenderControl;
begin
  { Free cache bitmaps }
  if FBGRANormal <> nil then
    FreeAndNil(FBGRANormal);
  if FBGRAHover <> nil then
    FreeAndNil(FBGRAHover);
  if FBGRAActive <> nil then
    FreeAndNil(FBGRAActive);
  if FBGRADisabled <> nil then
    FreeAndNil(FBGRADisabled);

  { Create cache bitmaps }
  FBGRANormal := TBGRABitmap.Create(Width, Height);
  FBGRAHover := TBGRABitmap.Create(Width, Height);
  FBGRAActive := TBGRABitmap.Create(Width, Height);
  FBGRADisabled := TBGRABitmap.Create(Width, Height);

  if Assigned(FOnRenderControl) then
  begin
    FOnRenderControl(Self, FBGRANormal, gbsNormal);
    FOnRenderControl(Self, FBGRAHover, gbsHover);
    FOnRenderControl(Self, FBGRAActive, gbsActive);
    FOnRenderControl(Self, FBGRADisabled, gbsDisabled);
  end;
end;

constructor TBCXButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

destructor TBCXButton.Destroy;
begin
  if FBGRANormal <> nil then
    FreeAndNil(FBGRANormal);
  if FBGRAHover <> nil then
    FreeAndNil(FBGRAHover);
  if FBGRAActive <> nil then
    FreeAndNil(FBGRAActive);
  if FBGRADisabled <> nil then
    FreeAndNil(FBGRADisabled);
  inherited Destroy;
end;

{ TBCImageButtonSliceScalingOptions }

procedure TBCImageButtonSliceScalingOptions.SetFCenter(AValue: boolean);
begin
  if FCenter = AValue then
    Exit;
  FCenter := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBCImageButtonSliceScalingOptions.SetFProportional(AValue: boolean);
begin
  if FProportional = AValue then
    Exit;
  FProportional := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBCImageButtonSliceScalingOptions.SetFStretch(AValue: boolean);
begin
  if FStretch = AValue then
    Exit;
  FStretch := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

constructor TBCImageButtonSliceScalingOptions.Create(AOwner: TControl);
begin
  inherited Create(AOwner);
  FNumberOfItems := 4;
  FCenter := True;
  FProportional := False;
  FStretch := True;
end;

procedure TBCImageButtonSliceScalingOptions.Assign(Source: TPersistent);
begin
  if Source is TBCImageButtonSliceScalingOptions then
  begin
    FAutoDetectRepeat := TBCImageButtonSliceScalingOptions(Source).AutoDetectRepeat;
    FCenter := TBCImageButtonSliceScalingOptions(Source).Center;
    FRepeatTop := TBCImageButtonSliceScalingOptions(Source).RepeatTop;
    FRepeatLeft := TBCImageButtonSliceScalingOptions(Source).RepeatLeft;
    FRepeatMiddleHorizontal :=
      TBCImageButtonSliceScalingOptions(Source).RepeatMiddleHorizontal;
    FRepeatMiddleVertical := TBCImageButtonSliceScalingOptions(
      Source).RepeatMiddleVertical;
    FRepeatRight := TBCImageButtonSliceScalingOptions(Source).RepeatRight;
    FRepeatBottom := TBCImageButtonSliceScalingOptions(Source).RepeatBottom;
    FMarginTop := TBCImageButtonSliceScalingOptions(Source).MarginTop;
    FMarginRight := TBCImageButtonSliceScalingOptions(Source).MarginRight;
    FMarginBottom := TBCImageButtonSliceScalingOptions(Source).MarginBottom;
    FMarginLeft := TBCImageButtonSliceScalingOptions(Source).MarginLeft;
    FDirection := TBCImageButtonSliceScalingOptions(Source).Direction;
    FDrawMode := TBCImageButtonSliceScalingOptions(Source).DrawMode;
    FResampleMode := TBCImageButtonSliceScalingOptions(Source).ResampleMode;
    FResampleFilter := TBCImageButtonSliceScalingOptions(Source).ResampleFilter;
    FStretch := TBCImageButtonSliceScalingOptions(Source).Stretch;
    FProportional := TBCImageButtonSliceScalingOptions(Source).Proportional;
  end
  else
    inherited Assign(Source);
end;

{ TBCCustomSliceScalingOptions }

procedure TBCCustomSliceScalingOptions.SetFBitmap(AValue: TBGRABitmap);
begin
  if FBitmap = AValue then
    Exit;
  FBitmap := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBCCustomSliceScalingOptions.SetFMarginBottom(AValue: integer);
begin
  if FMarginBottom = AValue then
    Exit;
  FMarginBottom := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBCCustomSliceScalingOptions.SetFMarginLeft(AValue: integer);
begin
  if FMarginLeft = AValue then
    Exit;
  FMarginLeft := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBCCustomSliceScalingOptions.SetFMarginRight(AValue: integer);
begin
  if FMarginRight = AValue then
    Exit;
  FMarginRight := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBCCustomSliceScalingOptions.SetFMarginTop(AValue: integer);
begin
  if FMarginTop = AValue then
    Exit;
  FMarginTop := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBCCustomSliceScalingOptions.SetFAutoDetectRepeat(AValue: boolean);
begin
  if FAutoDetectRepeat = AValue then
    Exit;
  FAutoDetectRepeat := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBCCustomSliceScalingOptions.SetFDirection(AValue: TSliceScalingDirection);
begin
  if FDirection = AValue then
    Exit;
  FDirection := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBCCustomSliceScalingOptions.SetFDrawMode(AValue: TDrawMode);
begin
  if FDrawMode = AValue then
    Exit;
  FDrawMode := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBCCustomSliceScalingOptions.SetFNumberOfItems(AValue: integer);
begin
  if FNumberOfItems = AValue then
    Exit;
  FNumberOfItems := AValue;
end;

procedure TBCCustomSliceScalingOptions.SetFRepeatBottom(AValue: boolean);
begin
  if FRepeatBottom = AValue then
    Exit;
  FRepeatBottom := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBCCustomSliceScalingOptions.SetFRepeatLeft(AValue: boolean);
begin
  if FRepeatLeft = AValue then
    Exit;
  FRepeatLeft := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBCCustomSliceScalingOptions.SetFRepeatMiddleHorizontal(AValue: boolean);
begin
  if FRepeatMiddleHorizontal = AValue then
    Exit;
  FRepeatMiddleHorizontal := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBCCustomSliceScalingOptions.SetFRepeatMiddleVertical(AValue: boolean);
begin
  if FRepeatMiddleVertical = AValue then
    Exit;
  FRepeatMiddleVertical := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBCCustomSliceScalingOptions.SetFRepeatRight(AValue: boolean);
begin
  if FRepeatRight = AValue then
    Exit;
  FRepeatRight := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBCCustomSliceScalingOptions.SetFRepeatTop(AValue: boolean);
begin
  if FRepeatTop = AValue then
    Exit;
  FRepeatTop := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBCCustomSliceScalingOptions.SetFResampleFilter(AValue: TResampleFilter);
begin
  if FResampleFilter = AValue then
    Exit;
  FResampleFilter := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TBCCustomSliceScalingOptions.SetFResampleMode(AValue: TResampleMode);
begin
  if FResampleMode = AValue then
    Exit;
  FResampleMode := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

constructor TBCCustomSliceScalingOptions.Create(AOwner: TControl);
begin
  FOwner := AOwner;
  FBitmap := nil;
  FAutoDetectRepeat := False;
  FRepeatTop := False;
  FRepeatLeft := False;
  FRepeatMiddleHorizontal := False;
  FRepeatMiddleVertical := False;
  FRepeatRight := False;
  FRepeatBottom := False;
  FMarginTop := 0;
  FMarginRight := 0;
  FMarginBottom := 0;
  FMarginLeft := 0;
  FNumberOfItems := 1;
  FDirection := sdVertical;
  FDrawMode := dmDrawWithTransparency;
  FResampleMode := rmFineResample;
  FResampleFilter := rfBestQuality;
  inherited Create;
end;

destructor TBCCustomSliceScalingOptions.Destroy;
begin
  if FBitmap <> nil then
    FreeAndNil(FBitmap);
  inherited Destroy;
end;

{ TBCGraphicButton }

procedure TBCGraphicButton.DoClick;
var
  Form: TCustomForm;
begin
  if ModalResult <> mrNone then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.ModalResult := ModalResult;
  end;
end;

procedure TBCGraphicButton.DoMouseDown;
var
  NewState: TBCGraphicButtonState;
begin
  NewState := gbsActive;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TBCGraphicButton.DoMouseUp;
var
  NewState: TBCGraphicButtonState;
  p: TPoint;
begin
  p := ScreenToClient(Mouse.CursorPos);

  if (p.x >= 0) and (p.x <= Width) and (p.y >= 0) and (p.y <= Height) then
    NewState := gbsHover
  else
    NewState := gbsNormal;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TBCGraphicButton.DoMouseEnter;
var
  NewState: TBCGraphicButtonState;
begin
  if Enabled then
    NewState := gbsHover
  else
  begin
    FState := gbsNormal;
    NewState := FState;
  end;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TBCGraphicButton.DoMouseLeave;
var
  NewState: TBCGraphicButtonState;
begin
  if Enabled then
    NewState := gbsNormal
  else
  begin
    FState := gbsNormal;
    NewState := FState;
  end;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TBCGraphicButton.DoMouseMove(x, y: integer);
begin
  inherited;
end;

procedure TBCGraphicButton.Click;
begin
  DoClick;
  inherited Click;
end;

procedure TBCGraphicButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
    DoMouseDown;
end;

procedure TBCGraphicButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  DoMouseUp;
end;

procedure TBCGraphicButton.MouseEnter;
begin
  inherited MouseEnter;
  DoMouseEnter;
end;

procedure TBCGraphicButton.MouseLeave;
begin
  inherited MouseLeave;
  DoMouseLeave;
end;

procedure TBCGraphicButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  DoMouseMove(X, Y);
end;

{ TBCCustomImageButton }

procedure TBCCustomImageButton.Fade(Sender: TObject);
begin
  if FFade.Mode <> fmSuspended then
    Invalidate;

  if csDesigning in ComponentState then
    Exit;
  FTimer.Enabled := FAnimation;
end;

procedure TBCCustomImageButton.SetFPressed(AValue: boolean);
begin
  if FPressed = AValue then
    Exit;
  FPressed := AValue;

  RenderControl;
end;

procedure TBCCustomImageButton.SetFTextVisible(AValue: boolean);
begin
  if FTextVisible = AValue then
    Exit;
  FTextVisible := AValue;

  RenderControl;
end;

procedure TBCCustomImageButton.SetFToggle(AValue: boolean);
begin
  if FToggle = AValue then
    Exit;
  FToggle := AValue;
end;

procedure TBCCustomImageButton.SetFBitmapOptions(AValue:
  TBCImageButtonSliceScalingOptions);
begin
  if FBitmapOptions = AValue then
    Exit;
  FBitmapOptions := AValue;
end;

procedure TBCCustomImageButton.SetFAlphaTest(AValue: boolean);
begin
  if FAlphaTest = AValue then
      Exit;
    FAlphaTest := AValue;
end;

procedure TBCCustomImageButton.SetFAlphaTestValue(AValue: byte);
begin
  if FAlphaTestValue = AValue then
      Exit;
    FAlphaTestValue := AValue;
end;

procedure TBCCustomImageButton.SetFAnimation(AValue: boolean);
begin
  if FAnimation = AValue then
    Exit;
  FAnimation := AValue;

  if csDesigning in ComponentState then Exit;
    FTimer.Enabled := FAnimation;
end;

procedure TBCCustomImageButton.SetFBitmapFile(AValue: string);
begin
  if FBitmapFile = AValue then
    Exit;
  FBitmapFile := AValue;
end;

procedure TBCCustomImageButton.DrawControl;
var
  temp: TBGRABitmap;
begin
  {$IFNDEF FPC}//# //@  IN DELPHI RenderControl NEDD. IF NO RenderControl BE BLACK AFTER INVALIDATE.
  RenderControl;
  {$ENDIF}

  if Color <> clDefault then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect(0, 0, Width, Height));
  end;

  if Enabled then
  begin
    if (Toggle) then
    begin
      if (Pressed) then
        FBGRAActive.Draw(Canvas, FDestRect.Left, FDestRect.Top, False)
      else
        case FState of
          gbsHover: FBGRAHover.Draw(Canvas, FDestRect.Left,
              FDestRect.Top, False);
          else
            FBGRANormal.Draw(Canvas, FDestRect.Left,
              FDestRect.Top, False);
        end;
    end
    else
    begin
      case FState of
        gbsNormal, gbsHover: FBGRANormal.Draw(Canvas, FDestRect.Left,
            FDestRect.Top, False);
        gbsActive: FBGRAActive.Draw(Canvas, FDestRect.Left, FDestRect.Top, False);
      end;

      temp := TBGRABitmap.Create(Width, Height);
      FFade.Execute;
      FFade.PutImage(temp, 0, 0, FBGRAHover);

      temp.Draw(Canvas, FDestRect.Left, FDestRect.Top, False);
      temp.Free;
    end;
  end
  else
    FBGRADisabled.Draw(Canvas, FDestRect.Left, FDestRect.Top, False);

  {$IFDEF INDEBUG}
  FDrawCount := FDrawCount +1;
  {$ENDIF}

  {$IFDEF INDEBUG}
  Canvas.Brush.Color := clWhite;
  Canvas.TextOut(0, 0, GetDebugText);
  {$ENDIF}
end;

procedure TBCCustomImageButton.RenderControl;

  procedure DrawText(ABitmap: TBGRABitmap);
  begin
    AssignFontToBGRA(Font, ABitmap);
    ABitmap.TextRect(Rect(0, 0, Width, Height), Caption, taCenter, tlCenter,
      Font.Color);
  end;

{$IFDEF INDEBUG}
const
  Debug = True;
{$ELSE}
const
  Debug = False;
{$ENDIF}
var
  i: integer;
begin
  { Free cache bitmaps }
  if FBGRANormal <> nil then
    FreeAndNil(FBGRANormal);
  if FBGRAHover <> nil then
    FreeAndNil(FBGRAHover);
  if FBGRAActive <> nil then
    FreeAndNil(FBGRAActive);
  if FBGRADisabled <> nil then
    FreeAndNil(FBGRADisabled);

  { Create cache bitmaps }
  FBGRANormal := TBGRABitmap.Create(Width, Height);
  FBGRAHover := TBGRABitmap.Create(Width, Height);
  FBGRAActive := TBGRABitmap.Create(Width, Height);
  FBGRADisabled := TBGRABitmap.Create(Width, Height);

  { Free FBGRAMultiSliceScaling }
  if FBGRAMultiSliceScaling <> nil then
    FreeAndNil(FBGRAMultiSliceScaling);

  if (FBitmapOptions.Bitmap <> nil) then
  begin
    { Create FBGRAMultiSliceScaling }
    FBGRAMultiSliceScaling := TBGRAMultiSliceScaling.Create(FBitmapOptions.Bitmap,
      FBitmapOptions.MarginTop, FBitmapOptions.MarginRight,
      FBitmapOptions.MarginBottom, FBitmapOptions.MarginLeft,
      FBitmapOptions.NumberOfItems, FBitmapOptions.Direction);

    { Set FBGRAMultiSliceScaling properties }
    for i := 0 to High(FBGRAMultiSliceScaling.SliceScalingArray) do
    begin
      FBGRAMultiSliceScaling.SliceScalingArray[i].ResampleFilter :=
        FBitmapOptions.ResampleFilter;
      FBGRAMultiSliceScaling.SliceScalingArray[i].ResampleMode :=
        FBitmapOptions.ResampleMode;
      FBGRAMultiSliceScaling.SliceScalingArray[i].DrawMode := FBitmapOptions.DrawMode;
      FBGRAMultiSliceScaling.SliceScalingArray[i].SliceRepeat[srpTop] :=
        FBitmapOptions.RepeatTop;
      FBGRAMultiSliceScaling.SliceScalingArray[i].SliceRepeat[srpBottom] :=
        FBitmapOptions.RepeatBottom;
      FBGRAMultiSliceScaling.SliceScalingArray[i].SliceRepeat[srpLeft] :=
        FBitmapOptions.RepeatLeft;
      FBGRAMultiSliceScaling.SliceScalingArray[i].SliceRepeat[srpRight] :=
        FBitmapOptions.RepeatRight;
      FBGRAMultiSliceScaling.SliceScalingArray[i].SliceRepeat[srpMiddleHorizontal] :=
        FBitmapOptions.RepeatMiddleHorizontal;
      FBGRAMultiSliceScaling.SliceScalingArray[i].SliceRepeat[srpMiddleVertical] :=
        FBitmapOptions.RepeatMiddleVertical;
      if FBitmapOptions.AutoDetectRepeat then
        FBGRAMultiSliceScaling.SliceScalingArray[i].AutodetectRepeat;
    end;

    { Calculate FDestRect }
    FDestRect := CalculateDestRect(
      FBGRAMultiSliceScaling.SliceScalingArray[0].BitmapWidth,
      FBGRAMultiSliceScaling.SliceScalingArray[0].BitmapHeight, Width,
      Height, FBitmapOptions.Stretch, FBitmapOptions.Proportional,
      FBitmapOptions.Center);

    { Draw in cache bitmaps }
    FBGRAMultiSliceScaling.Draw(0, FBGRANormal, 0, 0, FDestRect.Right,
      FDestRect.Bottom, Debug);
    FBGRAMultiSliceScaling.Draw(1, FBGRAHover, 0, 0, FDestRect.Right,
      FDestRect.Bottom, Debug);
    FBGRAMultiSliceScaling.Draw(2, FBGRAActive, 0, 0, FDestRect.Right,
      FDestRect.Bottom, Debug);
    FBGRAMultiSliceScaling.Draw(3, FBGRADisabled, 0, 0, FDestRect.Right,
      FDestRect.Bottom, Debug);

    if TextVisible then
    begin
      { Draw Text }
      DrawText(FBGRANormal);
      DrawText(FBGRAHover);
      DrawText(FBGRAActive);
      DrawText(FBGRADisabled);
    end;
  end
  else
  begin
    { Calculate FDestRect }
    FDestRect := Rect(0, 0, Width, Height);

    { Draw default style in cache bitmaps }
    FBGRANormal.Rectangle(0, 0, Width, Height, BGRA(173, 173, 173), BGRA(225, 225, 225),
      dmSet);
    FBGRAHover.Rectangle(0, 0, Width, Height, BGRA(0, 120, 215), BGRA(229, 241, 251),
      dmSet);
    FBGRAActive.Rectangle(0, 0, Width, Height, BGRA(0, 84, 153), BGRA(204, 228, 247),
      dmSet);
    FBGRADisabled.Rectangle(0, 0, Width, Height, BGRA(191, 191, 191), BGRA(204, 204, 204),
      dmSet);

    if TextVisible then
    begin
      { Draw Text }
      DrawText(FBGRANormal);
      DrawText(FBGRAHover);
      DrawText(FBGRAActive);
      DrawText(FBGRADisabled);
    end;
  end;

  {$IFDEF INDEBUG}
  FRenderCount := FRenderCount +1;
  {$ENDIF}
end;

procedure TBCCustomImageButton.TextChanged;
begin
  InvalidatePreferredSize;
  {$IFDEF FPC}//#
  if Assigned(Parent) and Parent.AutoSize then
    Parent.AdjustSize;
  {$ENDIF}
  AdjustSize;
  RenderControl;
  Invalidate;
end;

procedure TBCCustomImageButton.FontChanged(Sender: TObject);
begin
  inherited;
  RenderControl;
  Invalidate;
end;

procedure TBCCustomImageButton.CMChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
  if csReadingState in ControlState then
    Exit;
  RenderControl;
end;

{$IFDEF INDEBUG}
{$IFDEF FPC}
function TBCCustomImageButton.GetDebugText: string;
begin
  Result := 'Render: ' + IntToStr(FRenderCount) + ' Draw: ' + IntToStr(FDrawCount);
end;
{$ENDIF}
{$ENDIF}

procedure TBCCustomImageButton.DoMouseDown;
begin
  if FAlphaTest and (FBGRANormal.GetPixel(FMouse.X, FMouse.Y).alpha < FAlphaTestValue) then
    Exit;

  FFade.Mode := fmFadeOut;

  if Animation then
    FFade.Step := 60
  else
    FFade.Step := 255;

  inherited DoMouseDown;
end;

procedure TBCCustomImageButton.DoMouseUp;
var
  Ctrl: TControl;
begin
  if FAlphaTest and (FBGRANormal.GetPixel(FMouse.X, FMouse.Y).alpha < FAlphaTestValue) then
    Exit;

  FFade.Mode := fmFadeIn;

  if Animation then
    FFade.Step := 20
  else
    FFade.Step := 255;
  {$IFDEF FPC} //#
  Ctrl := Application.GetControlAtMouse;
  {$ENDIF}
  if Ctrl = Self then
    DoMouseEnter
  else
    DoMouseLeave;

  inherited DoMouseUp;
end;

procedure TBCCustomImageButton.DoMouseEnter;
begin
  FFade.Mode := fmFadeIn;

  if Animation then
    FFade.Step := 15
  else
    FFade.Step := 255;

  inherited DoMouseEnter;
end;

procedure TBCCustomImageButton.DoMouseLeave;
begin
  FFade.Mode := fmFadeOut;

  if Animation then
    FFade.Step := 8
  else
    FFade.Step := 255;

  inherited DoMouseLeave;
end;

procedure TBCCustomImageButton.DoMouseMove(x, y: integer);
begin
  FMouse := Point(X, Y);
  if FAlphaTest then
    if FBGRANormal.GetPixel(X, Y).alpha >= FAlphaTestValue then
      DoMouseEnter
    else
      DoMouseLeave;
end;

procedure TBCCustomImageButton.Click;
begin
  if FAlphaTest and (FBGRANormal.GetPixel(FMouse.X, FMouse.Y).alpha < FAlphaTestValue) then
    Exit;
  inherited Click;
  if (Toggle) then
  begin
    Pressed := not Pressed;
  end;
end;

constructor TBCCustomImageButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF INDEBUG}
  FDrawCount := 0;
  FRenderCount := 0;
  {$ENDIF}
  {$IFDEF FPC}
  DisableAutoSizing;
  Include(FControlState, csCreating);
  {$ELSE} //#

  {$ENDIF}
  BeginUpdate;
  try
    FBitmapOptions := TBCImageButtonSliceScalingOptions.Create(Self);

    with GetControlClassDefaultSize do
      SetInitialBounds(0, 0, CX, CY);
    ControlStyle := ControlStyle + [csAcceptsControls];

//    FBitmapOptions := TBCImageButtonSliceScalingOptions.Create(Self);
    {FBitmapOptions.Bitmap := TBGRABitmap.Create(1,4,BGRAWhite);
    FBitmapOptions.Bitmap.SetPixel(0,0,BGRA(255,0,0,255));
    FBitmapOptions.Bitmap.SetPixel(0,1,BGRA(0,255,0,255));
    FBitmapOptions.Bitmap.SetPixel(0,2,BGRA(0,0,255,255));
    FBitmapOptions.Bitmap.SetPixel(0,3,BGRA(100,100,100,255));}

    FAlphaTest := True;
    FAlphaTestValue := 255;
    FFade.Step := 15;
    FFade.Mode := fmFadeOut;
    FTimer := TTimer.Create(Self);
    FTimer.Interval := 15;
    FTimer.OnTimer := Fade;
    if csDesigning in ComponentState then
      FTimer.Enabled := False;
    FAnimation := True;
    FTextVisible := True;

  finally
    {$IFDEF FPC}
    Exclude(FControlState, csCreating);
    EnableAutoSizing;
    {$ELSE} //#
    {$ENDIF}
    EndUpdate;
  end;
end;

destructor TBCCustomImageButton.Destroy;
begin
  FTimer.Enabled := False;
  FTimer.OnTimer := nil;
  FTimer.Free;
  if FBGRAMultiSliceScaling <> nil then
    FreeAndNil(FBGRAMultiSliceScaling);
  if FBGRANormal <> nil then
    FreeAndNil(FBGRANormal);
  if FBGRAHover <> nil then
    FreeAndNil(FBGRAHover);
  if FBGRAActive <> nil then
    FreeAndNil(FBGRAActive);
  if FBGRADisabled <> nil then
    FreeAndNil(FBGRADisabled);
  FreeAndNil(FBitmapOptions);
  inherited Destroy;
end;

procedure TBCCustomImageButton.LoadFromBitmapResource(const Resource: string;
  ResourceType: PChar);
var
  res: TResourceStream;
begin
  res := TResourceStream.Create(HInstance, Resource, ResourceType);

  if BitmapOptions.Bitmap <> nil then
    BitmapOptions.Bitmap.Free;

  BitmapOptions.Bitmap := TBGRABitmap.Create(res);
  res.Free;
end;

procedure TBCCustomImageButton.LoadFromBitmapResource(const Resource: string);
begin
  LoadFromBitmapResource(Resource, RT_RCDATA);
end;

procedure TBCCustomImageButton.LoadFromBitmapFile;
begin
  if BitmapFile <> '' then
    if BitmapOptions.Bitmap <> nil then
      BitmapOptions.Bitmap.LoadFromFile(BitmapFile)
    else
      BitmapOptions.Bitmap := TBGRABitmap.Create(BitmapFile);
end;

procedure TBCCustomImageButton.Assign(Source: TPersistent);
begin
  if Source is TBCCustomImageButton then
  begin
    FBitmapOptions.Assign(TBCCustomImageButton(Source).BitmapOptions);
    FAnimation := TBCCustomImageButton(Source).Animation;
    FBitmapFile := TBCCustomImageButton(Source).BitmapFile;
    FTextVisible := TBCCustomImageButton(Source).TextVisible;

    if TBCCustomImageButton(Source).BitmapOptions.Bitmap <> nil then
    begin
      if FBitmapOptions.Bitmap <> nil then
        FBitmapOptions.Bitmap.Free;

      FBitmapOptions.Bitmap :=
        TBGRABitmap.Create(TBCCustomImageButton(Source).BitmapOptions.Bitmap.Bitmap);
    end
    else
      LoadFromBitmapFile;

    RenderControl;
    Invalidate;
  end
  else
    inherited Assign(Source);
end;
{$IFDEF FPC}
procedure TBCCustomImageButton.SaveToFile(AFileName: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    WriteComponentAsTextToStream(AStream, Self);
    AStream.SaveToFile(AFileName);
  finally
    AStream.Free;
  end;
end;

procedure TBCCustomImageButton.LoadFromFile(AFileName: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFileName);
    ReadComponentFromTextStream(AStream, TComponent(Self), OnFindClass);
  finally
    AStream.Free;
  end;
end;

procedure TBCCustomImageButton.AssignFromFile(AFileName: string);
var
  AStream: TMemoryStream;
  AButton: TBCImageButton;
begin
  AButton := TBCImageButton.Create(nil);
  AStream := TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFileName);
    ReadComponentFromTextStream(AStream, TComponent(AButton), OnFindClass);
    Assign(AButton);
  finally
    AStream.Free;
    AButton.Free;
  end;
end;
{$ENDIF}

procedure TBCCustomImageButton.OnFindClass(Reader: TReader;
  const AClassName: string; var ComponentClass: TComponentClass);
begin
  if CompareText(AClassName, 'TBCImageButton') = 0 then
    ComponentClass := TBCImageButton;
end;

end.
