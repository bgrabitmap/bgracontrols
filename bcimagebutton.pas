unit BCImageButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, LResources, LMessages, ExtCtrls,
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
  protected
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  public
    property ModalResult: TModalResult
      read FModalResult write FModalResult default mrNone;
  end;

  { TBCXButton }
  TBCXButton = class(TBCGraphicButton)
  private
    FOnRenderControl: TOnRenderControl;
    FBGRANormal, FBGRAHover, FBGRAActive, FBGRADisabled: TBGRABitmap;
  protected
    procedure DrawControl; override;
    procedure RenderControl; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnRenderControl: TOnRenderControl
      read FOnRenderControl write FOnRenderControl;
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
    {$IFDEF DEBUG}
    FDrawCount: integer;
    FRenderCount: integer;
    {$ENDIF}
    FBitmapOptions: TBCImageButtonSliceScalingOptions;
    FBGRAMultiSliceScaling: TBGRAMultiSliceScaling;
    FBGRANormal, FBGRAHover, FBGRAActive, FBGRADisabled: TBGRABitmap;
    FDestRect: TRect;
    FTimer: TTimer;
    FFade: TFading;
    FAnimation: boolean;
    FBitmapFile: string;
    FTextVisible: boolean;
    procedure SetFAnimation(AValue: boolean);
    procedure SetFBitmapFile(AValue: string);
    procedure SetFBitmapOptions(AValue: TBCImageButtonSliceScalingOptions);
    procedure Fade(Sender: TObject);
    procedure SetFTextVisible(AValue: boolean);
  protected
    { Protected declarations }
    procedure DrawControl; override;
    procedure RenderControl; override;
    procedure CMChanged(var Message: TLMessage); message CM_CHANGED; virtual;
    {$IFDEF DEBUG}
    function GetDebugText: string;
    {$ENDIF}
    procedure DoMouseDown; override;
    procedure DoMouseUp; override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
  public
    { Public declarations }
    property BitmapOptions: TBCImageButtonSliceScalingOptions
      read FBitmapOptions write SetFBitmapOptions;
    property Animation: boolean read FAnimation write SetFAnimation default True;
    property BitmapFile: string read FBitmapFile write SetFBitmapFile;
    property TextVisible: boolean read FTextVisible write SetFTextVisible default True;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { It loads the 'BitmapFile' }
    procedure LoadFromBitmapFile;
    procedure Assign(Source: TPersistent); override;
    { Streaming }
    procedure SaveToFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);
    procedure AssignFromFile(AFileName: string);
    procedure OnFindClass(Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
  published
    { Published declarations }
  end;

  TBCImageButton = class(TBCCustomImageButton)
  published
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
    property OnChangeBounds;
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
    //property Toggle;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I icons\bcimagebutton_icon.lrs}
  RegisterComponents('BGRA Controls', [TBCImageButton]);
  RegisterComponents('BGRA Controls', [TBCXButton]);
end;

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

{ TBCCustomImageButton }

procedure TBCCustomImageButton.Fade(Sender: TObject);
begin
  if FFade.Mode <> fmSuspended then
    Invalidate;
end;

procedure TBCCustomImageButton.SetFTextVisible(AValue: boolean);
begin
  if FTextVisible = AValue then
    Exit;
  FTextVisible := AValue;

  RenderControl;
end;

procedure TBCCustomImageButton.SetFBitmapOptions(AValue:
  TBCImageButtonSliceScalingOptions);
begin
  if FBitmapOptions = AValue then
    Exit;
  FBitmapOptions := AValue;
end;

procedure TBCCustomImageButton.SetFAnimation(AValue: boolean);
begin
  if FAnimation = AValue then
    Exit;
  FAnimation := AValue;
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
  if Color <> clDefault then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(0, 0, Width, Height);
  end;

  if Enabled then
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
  end
  else
    FBGRADisabled.Draw(Canvas, FDestRect.Left, FDestRect.Top, False);

  {$IFDEF DEBUG}
  FDrawCount += 1;
  {$ENDIF}

  {$IFDEF DEBUG}
  Canvas.Brush.Color := clWhite;
  Canvas.TextOut(0, 0, GetDebugText);
  {$ENDIF}
end;

procedure TBCCustomImageButton.RenderControl;

  procedure DrawText(ABitmap: TBGRABitmap);
  begin
    AssignFontToBGRA(Font, ABitmap);
    ABitmap.TextRect(Rect(0, 0, Width, Height), Caption, taCenter, tlCenter,
      ColorToBGRA(ColorToRGB(Font.Color)));
  end;

{$IFDEF DEBUG}
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
    FBGRANormal.Rectangle(0, 0, Width, Height, BGRABlack, BGRA(0, 0, 255),
      dmSet);
    FBGRAHover.Rectangle(0, 0, Width, Height, BGRABlack, BGRA(0, 255, 0),
      dmSet);
    FBGRAActive.Rectangle(0, 0, Width, Height, BGRABlack, BGRA(255, 0, 0),
      dmSet);
    FBGRADisabled.Rectangle(0, 0, Width, Height, BGRABlack, BGRA(100, 100, 100),
      dmSet);

    { Draw Text }
    DrawText(FBGRANormal);
    DrawText(FBGRAHover);
    DrawText(FBGRAActive);
    DrawText(FBGRADisabled);
  end;

  {$IFDEF DEBUG}
  FRenderCount += 1;
  {$ENDIF}
end;

procedure TBCCustomImageButton.CMChanged(var Message: TLMessage);
begin
  if csReadingState in ControlState then
    Exit;
  RenderControl;
end;

{$IFDEF DEBUG}
function TBCCustomImageButton.GetDebugText: string;
begin
  Result := 'Render: ' + IntToStr(FRenderCount) + ' Draw: ' + IntToStr(FDrawCount);
end;

{$ENDIF}

procedure TBCCustomImageButton.DoMouseDown;
begin
  FFade.Mode := fmFadeOut;

  if Animation then
    FFade.Step := 60
  else
    FFade.Step := 255;

  inherited DoMouseDown;
end;

procedure TBCCustomImageButton.DoMouseUp;
var
  Ctrl : TControl;
begin
  FFade.Mode := fmFadeIn;

  if Animation then
    FFade.Step := 20
  else
    FFade.Step := 255;

  Ctrl := Application.GetControlAtMouse;
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

constructor TBCCustomImageButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF DEBUG}
  FDrawCount := 0;
  FRenderCount := 0;
  {$ENDIF}
  DisableAutoSizing;
  Include(FControlState, csCreating);
  BeginUpdate;
  try
    with GetControlClassDefaultSize do
      SetInitialBounds(0, 0, CX, CY);
    ControlStyle := ControlStyle + [csAcceptsControls];

    FBitmapOptions := TBCImageButtonSliceScalingOptions.Create(Self);
    {FBitmapOptions.Bitmap := TBGRABitmap.Create(1,4,BGRAWhite);
    FBitmapOptions.Bitmap.SetPixel(0,0,BGRA(255,0,0,255));
    FBitmapOptions.Bitmap.SetPixel(0,1,BGRA(0,255,0,255));
    FBitmapOptions.Bitmap.SetPixel(0,2,BGRA(0,0,255,255));
    FBitmapOptions.Bitmap.SetPixel(0,3,BGRA(100,100,100,255));}

    FFade.Step := 15;
    FFade.Mode := fmFadeOut;
    FTimer := TTimer.Create(Self);
    FTimer.Interval := 15;
    FTimer.OnTimer := @Fade;
    FAnimation := True;
    FTextVisible := True;

  finally
    Exclude(FControlState, csCreating);
    EnableAutoSizing;
    EndUpdate;
  end;
end;

destructor TBCCustomImageButton.Destroy;
begin
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
    ReadComponentFromTextStream(AStream, TComponent(Self), @OnFindClass);
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
    ReadComponentFromTextStream(AStream, TComponent(AButton), @OnFindClass);
    Assign(AButton);
  finally
    AStream.Free;
    AButton.Free;
  end;
end;

procedure TBCCustomImageButton.OnFindClass(Reader: TReader;
  const AClassName: string; var ComponentClass: TComponentClass);
begin
  if CompareText(AClassName, 'TBCImageButton') = 0 then
    ComponentClass := TBCImageButton;
end;

end.
