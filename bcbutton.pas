// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Customizable component which using BGRABitmap for drawing. Control mostly rendered
  using framework.

  Functionality:
  - Gradients
  - Double gradients
  - Rounding
  - Drop down list
  - Glyph
  - States (normal, hover, clicked)
  - Caption with shadow
  - Full alpha and antialias support

  originally written in 2012 by Krzysztof Dibowski dibowski at interia.pl
}

{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}

unit BCButton;

{$I bgracontrols.inc}

interface

uses
  Classes, types, {$IFDEF FPC}LCLType, LResources, {$ENDIF} Controls, Dialogs,
  ActnList, ImgList, Menus, // MORA
  Buttons, Graphics,
  {$IFNDEF FPC}BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BGRABitmap, BGRABitmapTypes, BCThemeManager, BCTypes, Forms, BCBasectrls,
  fpjsonrtti, Typinfo, fpjson;

{off $DEFINE DEBUG}

type
  TBCButtonMemoryUsage = (bmuLow, bmuMedium, bmuHigh);
  TBCButtonState = class;
  TBCButtonStyle = (bbtButton, bbtDropDown);
  TOnAfterRenderBCButton = procedure(Sender: TObject; const ABGRA: TBGRABitmap;
    AState: TBCButtonState; ARect: TRect) of object;
  TBCButtonPropertyData = (pdNone, pdUpdateSize);

  // MORA: DropDown styles
  TBCButtonDropDownStyle = (
    bdsSeparate,     // DropDown is a separate button (default)
    bdsCommon        // DropDown is same as main button
    );
  TBCButtonDropDownPosition = (
    bdpLeft,         // default
    bdpBottom);

  { TBCButtonState }

  TBCButtonState = class(TBCProperty)
  private
    FBackground: TBCBackground;
    FBorder: TBCBorder;
    FFontEx: TBCFont;
    procedure OnChangeFont({%H-}Sender: TObject; {%H-}AData: PtrInt);
    procedure OnChangeChildProperty({%H-}Sender: TObject; AData: PtrInt);
    procedure SetBackground(AValue: TBCBackground);
    procedure SetBorder(AValue: TBCBorder);
    procedure SetFontEx(const AValue: TBCFont);
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Scale(AScale: single; APreserveDefaultFontHeight: boolean = true);
  published
    property Background: TBCBackground read FBackground write SetBackground;
    property Border: TBCBorder read FBorder write SetBorder;
    property FontEx: TBCFont read FFontEx write SetFontEx;
  end;

  { TCustomBCButton }

  TCustomBCButton = class(TBCStyleGraphicControl)
  private
    { Private declarations }
    {$IFDEF INDEBUG}
    FRenderCount: integer;
    {$ENDIF}
    FDropDownArrowSize: integer;
    FDropDownWidth: integer;
    FFlipArrow: boolean;
    FActiveButt: TBCButtonStyle;
    FBGRANormal, FBGRAHover, FBGRAClick: TBGRABitmapEx;
    FCanvasScale: Single;
    FCanvasScaleMode: TBCCanvasScaleMode;
    FGlyphAlignment: TBCAlignment;
    FGlyphOldPlacement: boolean;
    FGlyphScale: single;
    FInnerMargin: single;
    FMemoryUsage: TBCButtonMemoryUsage;
    FPreserveGlyphOnAssign: boolean;
    FRounding: TBCRounding;
    FRoundingDropDown: TBCRounding;
    FStateClicked: TBCButtonState;
    FStateHover: TBCButtonState;
    FStateNormal: TBCButtonState;
    FDown: boolean;
    FGlyph: TBitmap;
    FGlyphMargin: integer;
    FButtonState: TBCMouseState;
    FDownButtonState: TBCMouseState;
    FOnAfterRenderBCButton: TOnAfterRenderBCButton;
    FOnButtonClick: TNotifyEvent;
    FStaticButton: boolean;
    FStyle: TBCButtonStyle;
    FGlobalOpacity: byte;
    FTextApplyGlobalOpacity: boolean;
    AutoSizeExtraY: integer;
    AutoSizeExtraX: integer;
    FLastBorderWidth: integer;
    // MORA
    FClickOffset: boolean;
    FDropDownArrow: boolean;
    FDropDownMenu: TPopupMenu;
    FDropDownMenuVisible: boolean;
    FDropDownClosingTime: TDateTime;
    FDropDownPosition: TBCButtonDropDownPosition;
    FDropDownStyle: TBCButtonDropDownStyle;
    FImageChangeLink: TChangeLink;
    FImageIndex: integer;
    FImages: TCustomImageList;
    FSaveDropDownClosed: TNotifyEvent;
    FShowCaption: boolean;
    procedure AssignDefaultStyle;
    procedure CalculateGlyphSize(out NeededWidth, NeededHeight: integer);
    procedure DropDownClosed(Sender: TObject);
    function GetBGRAClick: TBGRABitmapEx;
    function GetBGRAHover: TBGRABitmapEx;
    function GetBGRANormal: TBGRABitmapEx;
    procedure OnRestoreProperty(Sender: TObject; AObject: TObject;
      Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
    procedure OnStreamProperty(Sender: TObject; AObject: TObject;
      Info: PPropInfo; var Res: TJSONData);
    procedure RenderAll(ANow: boolean = False);
    function GetButtonRect: TRect;
    function GetDropDownWidth(AFull: boolean = True): integer;
    function GetDropDownRect(AFull: boolean = True): TRect;
    procedure SetBCButtonStateClicked(const AValue: TBCButtonState);
    procedure SetBCButtonStateHover(const AValue: TBCButtonState);
    procedure SetBCButtonStateNormal(const AValue: TBCButtonState);
    procedure SetCanvasScaleMode(AValue: TBCCanvasScaleMode);
    procedure SetClickOffset(AValue: boolean);
    procedure SetDown(AValue: boolean);
    procedure SetDropDownArrow(AValue: boolean);
    procedure SetDropDownArrowSize(AValue: integer);
    procedure SetDropDownPosition(AValue: TBCButtonDropDownPosition);
    procedure SetDropDownWidth(AValue: integer);
    procedure SetFlipArrow(AValue: boolean);
    procedure SetGlyph(const AValue: TBitmap);
    procedure SetGlyphAlignment(AValue: TBCAlignment);
    procedure SetGlyphMargin(const AValue: integer);
    procedure SetGlyphOldPlacement(AValue: boolean);
    procedure SetGlyphScale(AValue: single);
    procedure SetImageIndex(AValue: integer);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetInnerMargin(AValue: single);
    procedure SetMemoryUsage(AValue: TBCButtonMemoryUsage);
    procedure SetRounding(AValue: TBCRounding);
    procedure SetRoundingDropDown(AValue: TBCRounding);
    procedure SetShowCaption(AValue: boolean);
    procedure SetStaticButton(const AValue: boolean);
    procedure SetStyle(const AValue: TBCButtonStyle);
    procedure SetGlobalOpacity(const AValue: byte);
    procedure SetTextApplyGlobalOpacity(const AValue: boolean);
    procedure UpdateSize;
    procedure OnChangeGlyph({%H-}Sender: TObject);
    procedure OnChangeState({%H-}Sender: TObject; AData: PtrInt);
    procedure ImageListChange(ASender: TObject);
    function  GetGlyph: TBitmap;
  protected
    { Protected declarations }
    procedure LimitMemoryUsage;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      {%H-}WithThemeSpace: boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter;  override;
    procedure MouseLeave;  override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure SetEnabled(Value: boolean); override;
    procedure TextChanged;  override;
  protected
    // MORA
    procedure ActionChange(Sender: TObject; CheckDefaults: boolean); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure Render(ABGRA: TBGRABitmapEx; AState: TBCButtonState); virtual;
    procedure RenderState(ABGRA: TBGRABitmapEx; AState: TBCButtonState;
      const ARect: TRect; ARounding: TBCRounding); virtual;
    property ClickOffset: boolean read FClickOffset write SetClickOffset default False;
    property DropDownArrow: boolean
      read FDropDownArrow write SetDropDownArrow default False;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
    property DropDownStyle: TBCButtonDropDownStyle
      read FDropDownStyle write FDropDownStyle default bdsSeparate;
    property DropDownPosition: TBCButtonDropDownPosition
      read FDropDownPosition write SetDropDownPosition default bdpLeft;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property ShowCaption: boolean read FShowCaption write SetShowCaption default True;
  protected
    {$IFDEF INDEBUG}
    function GetDebugText: string; override;
    {$ENDIF}
    function GetStyleExtension: string; override;
    procedure DrawControl; override;
    procedure RenderControl; override;
    property BGRANormal: TBGRABitmapEx read GetBGRANormal;
    property BGRAHover: TBGRABitmapEx read GetBGRAHover;
    property BGRAClick: TBGRABitmapEx read GetBGRAClick;
  protected
    property AutoSizeExtraVertical: integer read AutoSizeExtraY;
    property AutoSizeExtraHorizontal: integer read AutoSizeExtraX;
    property CanvasScaleMode: TBCCanvasScaleMode read FCanvasScaleMode write SetCanvasScaleMode default csmAuto;
    property StateNormal: TBCButtonState read FStateNormal write SetBCButtonStateNormal;
    property StateHover: TBCButtonState read FStateHover write SetBCButtonStateHover;
    property StateClicked: TBCButtonState read FStateClicked
      write SetBCButtonStateClicked;
    property Down: boolean read FDown write SetDown default False;
    property DropDownWidth: integer read FDropDownWidth write SetDropDownWidth;
    property DropDownArrowSize: integer read FDropDownArrowSize
      write SetDropDownArrowSize;
    property FlipArrow: boolean read FFlipArrow write SetFlipArrow default False;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property GlyphScale: single read FGlyphScale write SetGlyphScale default 1;
    property GlyphMargin: integer read FGlyphMargin write SetGlyphMargin default 5;
    property GlyphAlignment: TBCAlignment read FGlyphAlignment write SetGlyphAlignment default bcaCenter;
    property GlyphOldPlacement: boolean read FGlyphOldPlacement write SetGlyphOldPlacement default true;
    property Style: TBCButtonStyle read FStyle write SetStyle default bbtButton;
    property StaticButton: boolean
      read FStaticButton write SetStaticButton default False;
    property GlobalOpacity: byte read FGlobalOpacity write SetGlobalOpacity;
    property Rounding: TBCRounding read FRounding write SetRounding;
    property RoundingDropDown: TBCRounding read FRoundingDropDown
      write SetRoundingDropDown;
    property TextApplyGlobalOpacity: boolean
      read FTextApplyGlobalOpacity write SetTextApplyGlobalOpacity;
    property OnAfterRenderBCButton: TOnAfterRenderBCButton
      read FOnAfterRenderBCButton write FOnAfterRenderBCButton;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property MemoryUsage: TBCButtonMemoryUsage read FMemoryUsage write SetMemoryUsage;
    property InnerMargin: single read FInnerMargin write SetInnerMargin;
    property PreserveGlyphOnAssign: boolean read FPreserveGlyphOnAssign write FPreserveGlyphOnAssign default True;
  public
    { Constructor }
    constructor Create(AOwner: TComponent); override;
    { Destructor }
    destructor Destroy; override;
    { Assign the properties from Source to this instance }
    procedure Assign(Source: TPersistent); override;
    { Set dropdown size and autosize extra padding }
    procedure SetSizeVariables(newDropDownWidth, newDropDownArrowSize,
      newAutoSizeExtraVertical, newAutoSizeExtraHorizontal: integer);
    { Called by EndUpdate }
    procedure UpdateControl; override;
    property CanvasScale: single read FCanvasScale;
  public
    procedure ScaleStyle(AScale: single; APreserveDefaultFontHeight: boolean = true);
    {$IFDEF FPC}
    { Save all published settings to file }
    procedure SaveToFile(AFileName: string); override;
    procedure SaveToJSONFile(AFileName: string);
    function SaveToJSON: string;
    { Load and assign all published settings from file }
    procedure LoadFromFile(AFileName: string); override;
    procedure LoadFromJSONFile(AFileName: string);
    procedure LoadFromJSON(AJSON: string);
    { Assign the properties from AFileName to this instance }
    procedure AssignFromFile(AFileName: string); override;
    procedure AssignFromResource(AResourceName: string);
    {$ENDIF}
    { Used by SaveToFile/LoadFromFile }
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
  end;

  TBCButton = class(TCustomBCButton)
  private
    FBCThemeManager: TBCThemeManager;
    procedure SetFBCThemeManager(AValue: TBCThemeManager);
  published
    property Action;
    property Align;
    property Anchors;
    { Click to edit the style. Available when editing only. If you want to stream the style from a file at runtime please use LoadFromFile and SaveToFile methods. }
    property AssignStyle;
    property AutoSize;
    { The style of the button when pressed. }
    property StateClicked;
    { The style of the button when hovered. }
    property StateHover;
    { The default style of the button. }
    property StateNormal;
    property BorderSpacing;
    property CanvasScaleMode;
    property Caption;
    property Color;
    property Constraints;
    { Set to True to change the button to always show a StateClicked style that will not change when button is clicked or hovered. }
    property Down;
    { The width of the dropdown arrow area. }
    property DropDownWidth;
    { The size of the dropdown arrow. }
    property DropDownArrowSize;
    property Enabled;
    { Changes the direction of the arrow. Default: False. }
    property FlipArrow;
    { Set the opacity that will be applied to the whole button. Default: 255. }
    property GlobalOpacity;
    { The glyph icon. }
    property Glyph;
    property GlyphScale;
    property GlyphAlignment;
    property GlyphOldPlacement;
    property PreserveGlyphOnAssign;
    { The margin of the glyph icon. }
    property GlyphMargin;
    property Hint;
    property InnerMargin;
    { Called when the button finish the render. Use it to add your own drawings to the button. }
    property OnAfterRenderBCButton;
    { Called when the button part is clicked, not the dropdown. }
    property OnButtonClick;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property ParentColor;
    property PopupMenu;
    { Change the style of the rounded corners of the button. }
    property Rounding;
    { Change the style of the rounded corners of the dropdown part of the button. }
    property RoundingDropDown;
    { Set to True to change the button to always show a StateNormal style that will not change when button is clicked or hovered. }
    property StaticButton;
    property ShowHint;
    { The style of button that will be used. bbtButton or bbtDropDown. }
    property Style;
    { Apply the global opacity to rendered text. Default: False. }
    property TextApplyGlobalOpacity;
    property Visible;
    { -ToDo: Unused property? }
    property ClickOffset;
    { Show the dropdown arrow. }
    property DropDownArrow;
    { The dropdown menu that will be displayed when the button is pressed. }
    property DropDownMenu;
    { The kind of dropdown that will be used. bdsSeparate will show the dropdown down the dropdown arrow side. bdsCommon will show the dropdown down the whole button. }
    property DropDownStyle;
    { The position of the dropdown arrow. }
    property DropDownPosition;
    { The image list that holds an image to be used with the button ImageIndex property. }
    property Images;
    { The index of the image that will be used for the button as glyph icon if glyph property is not set. }
    property ImageIndex;
    { Show caption or hides it. Default: True. }
    property ShowCaption;
    { Limit memory usage by selecting one of the options. Default: bmuHigh. }
    property MemoryUsage;
    { The unique name of the control in the form. }
    property Name;
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFBCThemeManager;
  end;

  { TBCButtonActionLink }

  TBCButtonActionLink = class(TControlActionLink)
  protected
    procedure AssignClient(AClient: TObject); override;
    procedure SetChecked(Value: boolean); override;
    procedure SetImageIndex(Value: integer); override;
  public
    function IsCheckedLinked: boolean; override;
    function IsImageIndexLinked: boolean; override;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

uses {$IFDEF FPC}LCLIntf, PropEdits, GraphPropEdits, LCLProc, {$ENDIF}Math, BCTools, SysUtils;

const
  DropDownReopenDelay = 0.2/(24*60*60);

{$IFDEF FPC}//#
type
  TBCButtonImageIndexPropertyEditor = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  end;
{$ENDIF}

{ TBCButton }

procedure TBCButton.SetFBCThemeManager(AValue: TBCThemeManager);
begin
  if FBCThemeManager=AValue then Exit;
  FBCThemeManager:=AValue;
end;

{$IFDEF FPC}//#
function TBCButtonImageIndexPropertyEditor.GetImageList: TCustomImageList;
var
  Component: TPersistent;
begin
  Component := GetComponent(0);
  if Component is TCustomBCButton then
    Result := TCustomBCButton(Component).Images
  else
    Result := nil;
end;
{$ENDIF}

{$IFDEF FPC}
procedure Register;
begin
  {$R images\bgracontrols_images.res}
  RegisterComponents('BGRA Button Controls', [TBCButton]);
  RegisterPropertyEditor(TypeInfo(integer), TBCButton,
    'ImageIndex', TBCButtonImageIndexPropertyEditor);
end;
{$ENDIF}

{ TBCButtonActionLink }

procedure TBCButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TCustomBCButton;
end;

procedure TBCButtonActionLink.SetChecked(Value: boolean);
begin
  if IsCheckedLinked then
    TCustomBCButton(FClient).Down := Value;
end;

procedure TBCButtonActionLink.SetImageIndex(Value: integer);
begin
  if IsImageIndexLinked then
    TCustomBCButton(FClient).ImageIndex := Value;
end;

function TBCButtonActionLink.IsCheckedLinked: boolean;
begin
  Result := inherited IsCheckedLinked and (TCustomBCButton(FClient).Down =
    (Action as TCustomAction).Checked);
end;

function TBCButtonActionLink.IsImageIndexLinked: boolean;
begin
  Result := inherited IsImageIndexLinked and
    (TCustomBCButton(FClient).ImageIndex = (Action as TCustomAction).ImageIndex);
end;

{ TBCButtonState }

procedure TBCButtonState.SetFontEx(const AValue: TBCFont);
begin
  if FFontEx = AValue then
    exit;
  FFontEx.Assign(AValue);

  Change;
end;

procedure TBCButtonState.OnChangeFont(Sender: TObject; AData: PtrInt);
begin
  Change(PtrInt(pdUpdateSize));
end;

procedure TBCButtonState.OnChangeChildProperty(Sender: TObject; AData: PtrInt);
begin
  Change(AData);
end;

procedure TBCButtonState.SetBackground(AValue: TBCBackground);
begin
  if FBackground = AValue then
    Exit;
  FBackground.Assign(AValue);

  Change;
end;

procedure TBCButtonState.SetBorder(AValue: TBCBorder);
begin
  if FBorder = AValue then
    Exit;
  FBorder.Assign(AValue);

  Change;
end;

constructor TBCButtonState.Create(AControl: TControl);
begin
  FBackground := TBCBackground.Create(AControl);
  FBorder := TBCBorder.Create(AControl);
  FFontEx := TBCFont.Create(AControl);

  FBackground.OnChange := OnChangeChildProperty;
  FBorder.OnChange := OnChangeChildProperty;
  FFontEx.OnChange := OnChangeFont;

  inherited Create(AControl);
end;

destructor TBCButtonState.Destroy;
begin
  FBackground.Free;
  FBorder.Free;
  FFontEx.Free;
  inherited Destroy;
end;

procedure TBCButtonState.Assign(Source: TPersistent);
begin
  if Source is TBCButtonState then
  begin
    FBackground.Assign(TBCButtonState(Source).FBackground);
    FBorder.Assign(TBCButtonState(Source).FBorder);
    FFontEx.Assign(TBCButtonState(Source).FFontEx);

    Change(PtrInt(pdUpdateSize));
  end
  else
    inherited Assign(Source);
end;

procedure TBCButtonState.Scale(AScale: single; APreserveDefaultFontHeight: boolean);
begin
  FBackground.Scale(AScale);
  FBorder.Scale(AScale);
  FFontEx.Scale(AScale, APreserveDefaultFontHeight);
end;

{ TCustomBCButton }

procedure TCustomBCButton.AssignDefaultStyle;
begin
  FRounding.RoundX := 12;
  FRounding.RoundY := 12;
  // Normal
  with StateNormal do
  begin
    Border.Style := bboNone;
    FontEx.Color := RGBToColor(230, 230, 255);
    FontEx.Style := [fsBold];
    FontEx.Shadow := True;
    FontEx.ShadowOffsetX := 1;
    FontEx.ShadowOffsetY := 1;
    FontEx.ShadowRadius := 2;
    Background.Gradient1EndPercent := 60;
    Background.Style := bbsGradient;
    // Gradient1
    with Background.Gradient1 do
    begin
      EndColor := RGBToColor(64, 64, 128);
      StartColor := RGBToColor(0, 0, 64);
    end;
    // Gradient2
    with Background.Gradient2 do
    begin
      EndColor := RGBToColor(0, 0, 64);
      GradientType := gtRadial;
      Point1XPercent := 50;
      Point1YPercent := 100;
      Point2YPercent := 0;
      StartColor := RGBToColor(64, 64, 128);
    end;
  end;
  // Hover
  with StateHover do
  begin
    Border.Style := bboNone;
    FontEx.Color := RGBToColor(255, 255, 255);
    FontEx.Style := [fsBold];
    FontEx.Shadow := True;
    FontEx.ShadowOffsetX := 1;
    FontEx.ShadowOffsetY := 1;
    FontEx.ShadowRadius := 2;
    Background.Gradient1EndPercent := 100;
    Background.Style := bbsGradient;
    // Gradient1
    with Background.Gradient1 do
    begin
      EndColor := RGBToColor(0, 64, 128);
      GradientType := gtRadial;
      Point1XPercent := 50;
      Point1YPercent := 100;
      Point2YPercent := 0;
      StartColor := RGBToColor(0, 128, 255);
    end;
  end;
  // Clicked
  with StateClicked do
  begin
    Border.Style := bboNone;
    FontEx.Color := RGBToColor(230, 230, 255);
    FontEx.Style := [fsBold];
    FontEx.Shadow := True;
    FontEx.ShadowOffsetX := 1;
    FontEx.ShadowOffsetY := 1;
    FontEx.ShadowRadius := 2;
    Background.Gradient1EndPercent := 100;
    Background.Style := bbsGradient;
    // Gradient1
    with Background.Gradient1 do
    begin
      EndColor := RGBToColor(0, 0, 64);
      GradientType := gtRadial;
      Point1XPercent := 50;
      Point1YPercent := 100;
      Point2YPercent := 0;
      StartColor := RGBToColor(0, 64, 128);
    end;
  end;
end;

procedure TCustomBCButton.CalculateGlyphSize(out NeededWidth, NeededHeight: integer);
begin
  if Assigned(FGlyph) and not FGlyph.Empty then
  begin
    NeededWidth := ceil(FGlyph.Width * FGlyphScale);
    NeededHeight := ceil(FGlyph.Height * FGlyphScale);
  end
  else
  if Assigned(FImages) then
  begin
    NeededWidth := FImages.ResolutionForPPI[FImages.Width, Screen.PixelsPerInch, 1].Width;
    NeededHeight := FImages.ResolutionForPPI[FImages.Width, Screen.PixelsPerInch, 1].Height;
  end
  else
  begin
    NeededHeight := 0;
    NeededWidth := 0;
  end;
end;

procedure TCustomBCButton.RenderAll(ANow: boolean);
begin
  if (csCreating in ControlState) or IsUpdating then
    Exit;

  if ANow then
  begin
    Render(FBGRANormal, FStateNormal);
    Render(FBGRAHover, FStateHover);
    Render(FBGRAClick, FStateClicked);
  end
  else
  begin
    if Assigned(FBGRANormal) then FBGRANormal.NeedRender := True;
    if Assigned(FBGRAHover) then FBGRAHover.NeedRender := True;
    if Assigned(FBGRAClick) then FBGRAClick.NeedRender := True;
  end;
end;

function TCustomBCButton.GetButtonRect: TRect;
begin
  Result := GetClientRect;
  if FStyle = bbtDropDown then
    case FDropDownPosition of
      bdpBottom:
        Dec(Result.Bottom, GetDropDownWidth(False));
      else
        // bdpLeft:
        Dec(Result.Right, GetDropDownWidth(False));
    end;
end;

function TCustomBCButton.GetDropDownWidth(AFull: boolean): integer;
begin
  Result := FDropDownWidth + (ifthen(AFull, 2, 1) * FStateNormal.FBorder.Width);
end;

function TCustomBCButton.GetGlyph: TBitmap;
begin
  Result := FGlyph as TBitmap;
end;

function TCustomBCButton.GetDropDownRect(AFull: boolean): TRect;
begin
  Result := GetClientRect;
  case FDropDownPosition of
    bdpBottom:
      Result.Top := Result.Bottom - GetDropDownWidth(AFull);
    else
      // bdpLeft:
      Result.Left := Result.Right - GetDropDownWidth(AFull);
  end;
end;

procedure TCustomBCButton.Render(ABGRA: TBGRABitmapEx; AState: TBCButtonState);

  procedure GetActualGlyph(out ABitmap: TBitmap; out AScale: single);
  begin
    if Assigned(FGlyph) and not FGlyph.Empty then
    begin
      ABitmap := FGlyph;
      AScale := FCanvasScale * FGlyphScale;
    end else
    if Assigned(FImages) and (FImageIndex > -1) and (FImageIndex < FImages.Count) then
    begin
      ABitmap := TBitmap.Create;
      {$IFDEF FPC}
      FImages.ResolutionForPPI[FImages.Width, Screen.PixelsPerInch, FCanvasScale].GetBitmap(FImageIndex, ABitmap);
      AScale := 1;
      {$ELSE}
      FImages.GetBitmapRaw(FImageIndex, result);
      ABitmap := AScale;
      {$ENDIF}
    end else
    begin
      ABitmap := nil;
      AScale := 1;
    end;
  end;

  procedure RenderGlyph(ARect: TRect; AGlyph: TBitmap);
  begin
    if ARect.IsEmpty or (AGlyph = nil) then exit;
    ABGRA.StretchPutImage(ARect, AGlyph, dmLinearBlend);
  end;

var
  r, r_a, r_g: TRect;
  g: TBitmap;
  actualCaption: TCaption;
  freeScaled: boolean;
  scaledState: TBCButtonState;
  scaledArrowSize, scaledGlyphMargin, scaledInnerMargin: integer;
  scaledRounding, scaledRoundingDropDown: TBCRounding;
  gScale: single;

begin
  if (csCreating in ControlState) or IsUpdating or (ABGRA = nil) then
    Exit;

  if FCanvasScale <> 1 then
  begin
    scaledState := TBCButtonState.Create(nil);
    scaledState.Assign(AState);
    scaledState.Scale(FCanvasScale, false);
    scaledRounding := TBCRounding.Create(nil);
    scaledRounding.Assign(Rounding);
    scaledRounding.Scale(FCanvasScale);
    scaledRoundingDropDown := TBCRounding.Create(nil);
    scaledRoundingDropDown.Assign(RoundingDropDown);
    scaledRoundingDropDown.Scale(FCanvasScale);
    freeScaled := true;
  end
  else
  begin
    scaledState := AState;
    scaledRounding := Rounding;
    scaledRoundingDropDown := RoundingDropDown;
    freeScaled := false;
  end;
  scaledArrowSize := round(DropDownArrowSize * FCanvasScale);
  scaledGlyphMargin := round(GlyphMargin * FCanvasScale);
  scaledInnerMargin := round(InnerMargin * FCanvasScale);

  ABGRA.NeedRender := False;

  { Refreshing size }
  ABGRA.SetSize(round(Width * FCanvasScale), round(Height * FCanvasScale));

  { Clearing previous paint }
  ABGRA.Fill(BGRAPixelTransparent);

  { Basic body }
  r := ScaleRect(GetButtonRect, FCanvasScale);
  RenderState(ABGRA, scaledState, r, scaledRounding);

  if not GlyphOldPlacement then
    r.Inflate(-scaledInnerMargin,-scaledInnerMargin);

  { Calculating rect }
  CalculateBorderRect(scaledState.Border, r);

  if FStyle = bbtDropDown then
  begin
    r_a := ScaleRect(GetDropDownRect, FCanvasScale);
    RenderState(ABGRA, scaledState, r_a, scaledRoundingDropDown);
    CalculateBorderRect(scaledState.Border, r_a);

    // Click offset for arrow
    if FClickOffset and (AState = FStateClicked) then
      r_a.Offset(1,1);

    if FFlipArrow then
      RenderArrow(TBGRABitmap(ABGRA), r_a, scaledArrowSize, badUp,
        scaledState.FontEx.Color)
    else
      RenderArrow(TBGRABitmap(ABGRA), r_a, scaledArrowSize, badDown,
        scaledState.FontEx.Color);
  end;

  // Click offset for text and glyph
  if FClickOffset and (AState = FStateClicked) then
    r.Offset(round(1 * FCanvasScale), round(1 * FCanvasScale));

  // DropDown arrow
  if FDropDownArrow and (FStyle <> bbtDropDown) then
  begin
    r_a := r;
    r_a.Left := r_a.Right - round(FDropDownWidth * FCanvasScale);
    if FFlipArrow then
      RenderArrow(TBGRABitmap(ABGRA), r_a, scaledArrowSize, badUp,
        scaledState.FontEx.Color)
    else
      RenderArrow(TBGRABitmap(ABGRA), r_a, scaledArrowSize, badDown,
        scaledState.FontEx.Color);
    Dec(R.Right, round(FDropDownWidth * FCanvasScale));
  end;

  GetActualGlyph(g, gScale);
  if FShowCaption then actualCaption := self.Caption else actualCaption := '';
  r_g := ComputeGlyphPosition(r, g, GlyphAlignment, scaledGlyphMargin, actualCaption,
    scaledState.FontEx, GlyphOldPlacement, gScale);
  if FTextApplyGlobalOpacity then
  begin
    { Drawing text }
    RenderText(r, scaledState.FontEx, actualCaption, ABGRA, Enabled);
    RenderGlyph(r_g, g);
    { Set global opacity }
    ABGRA.ApplyGlobalOpacity(FGlobalOpacity);
  end
  else
  begin
    { Set global opacity }
    ABGRA.ApplyGlobalOpacity(FGlobalOpacity);
    { Drawing text }
    RenderText(r, scaledState.FontEx, actualCaption, ABGRA, Enabled);
    RenderGlyph(r_g, g);
  end;
  if g <> FGlyph then g.Free;

  { Convert to gray if not enabled }
  if not Enabled then ABGRA.InplaceGrayscale;

  if Assigned(FOnAfterRenderBCButton) then
    FOnAfterRenderBCButton(Self, ABGRA, scaledState, r);

  if freeScaled then
  begin
    FreeAndNil(scaledState);
    FreeAndNil(scaledRounding);
    FreeAndNil(scaledRoundingDropDown);
  end;

  {$IFDEF INDEBUG}
  FRenderCount := FRenderCount +1;
  {$ENDIF}
end;

procedure TCustomBCButton.RenderState(ABGRA: TBGRABitmapEx;
  AState: TBCButtonState; const ARect: TRect; ARounding: TBCRounding);
begin
  RenderBackgroundAndBorder(ARect, AState.FBackground, TBGRABitmap(ABGRA),
    ARounding, AState.FBorder, round(FInnerMargin * FCanvasScale));
end;

procedure TCustomBCButton.OnChangeGlyph(Sender: TObject);
begin
  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.OnChangeState(Sender: TObject; AData: PtrInt);
begin
  RenderControl;
  if (TBCButtonPropertyData(AData) = pdUpdateSize) or
    (FStateNormal.Border.Width <> FLastBorderWidth) then
    UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.ImageListChange(ASender: TObject);
begin
  if ASender = Images then
  begin
    RenderControl;
    Invalidate;
  end;
end;

procedure TCustomBCButton.LimitMemoryUsage;
begin
  {$IFNDEF FPC}//# //@  IN DELPHI NEEDRENDER NEED TO BE TRUE. IF FALSE COMPONENT IN BGRANORMAL BE BLACK AFTER INVALIDATE.
  if Assigned(FBGRANormal) then FBGRANormal.NeedRender := True;
  if Assigned(FBGRAHover) then FBGRAHover.NeedRender := True;
  if Assigned(FBGRAClick) then FBGRAClick.NeedRender := True;
  {$ENDIF}
  if (FMemoryUsage = bmuLow) and Assigned(FBGRANormal) then FBGRANormal.Discard;
  if (FMemoryUsage <> bmuHigh) then
  begin
    if Assigned(FBGRAHover) then FBGRAHover.Discard;
    if Assigned(FBGRAClick) then FBGRAClick.Discard;
  end;
end;

procedure TCustomBCButton.SetBCButtonStateClicked(const AValue: TBCButtonState);
begin
  if FStateClicked = AValue then
    exit;
  FStateClicked.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetBCButtonStateHover(const AValue: TBCButtonState);
begin
  if FStateHover = AValue then
    exit;
  FStateHover.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetBCButtonStateNormal(const AValue: TBCButtonState);
begin
  if FStateNormal = AValue then
    exit;
  FStateNormal.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetCanvasScaleMode(AValue: TBCCanvasScaleMode);
begin
  if FCanvasScaleMode=AValue then Exit;
  FCanvasScaleMode:=AValue;
  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetClickOffset(AValue: boolean);
begin
  if FClickOffset = AValue then
    Exit;
  FClickOffset := AValue;
  RenderControl;
end;

procedure TCustomBCButton.SetDown(AValue: boolean);
begin
  if FDown = AValue then
    exit;
  FDown := AValue;
  if FDown then
    FButtonState := msClicked
  else
    FButtonState := msNone;
  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetDropDownArrow(AValue: boolean);
begin
  if FDropDownArrow = AValue then
    Exit;
  FDropDownArrow := AValue;
  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetDropDownArrowSize(AValue: integer);
begin
  if FDropDownArrowSize = AValue then
    Exit;
  FDropDownArrowSize := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetDropDownPosition(AValue: TBCButtonDropDownPosition);
begin
  if FDropDownPosition = AValue then
    Exit;
  FDropDownPosition := AValue;

  if FStyle <> bbtDropDown then
    Exit;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.SetDropDownWidth(AValue: integer);
begin
  if FDropDownWidth = AValue then
    Exit;
  FDropDownWidth := AValue;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.SetFlipArrow(AValue: boolean);
begin
  if FFlipArrow = AValue then
    Exit;
  FFlipArrow := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetGlyph(const AValue: TBitmap);
begin
  if (FGlyph <> nil) and (FGlyph = AValue) then
    exit;

  FGlyph.Assign(AValue);

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.SetGlyphAlignment(AValue: TBCAlignment);
begin
  if FGlyphAlignment=AValue then Exit;
  FGlyphAlignment:=AValue;
  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.SetGlyphMargin(const AValue: integer);
begin
  if FGlyphMargin = AValue then
    exit;
  FGlyphMargin := AValue;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.SetGlyphOldPlacement(AValue: boolean);
begin
  if FGlyphOldPlacement=AValue then Exit;
  FGlyphOldPlacement:=AValue;
  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.SetGlyphScale(AValue: single);
begin
  if FGlyphScale=AValue then Exit;
  FGlyphScale:=AValue;
  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.SetImageIndex(AValue: integer);
begin
  if FImageIndex = AValue then
    Exit;
  FImageIndex := AValue;
  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetImages(AValue: TCustomImageList);
begin
  if FImages = AValue then
    Exit;
  FImages := AValue;
  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.SetInnerMargin(AValue: single);
begin
  if FInnerMargin=AValue then Exit;
  FInnerMargin:=AValue;
  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.SetMemoryUsage(AValue: TBCButtonMemoryUsage);
begin
  if FMemoryUsage=AValue then Exit;
  FMemoryUsage:=AValue;
  LimitMemoryUsage;
end;

procedure TCustomBCButton.SetRounding(AValue: TBCRounding);
begin
  if FRounding = AValue then
    Exit;
  FRounding.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetRoundingDropDown(AValue: TBCRounding);
begin
  if FRoundingDropDown = AValue then
    Exit;
  FRoundingDropDown.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetShowCaption(AValue: boolean);
begin
  if FShowCaption = AValue then
    Exit;
  FShowCaption := AValue;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.SetStaticButton(const AValue: boolean);
begin
  if FStaticButton = AValue then
    exit;
  FStaticButton := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetStyle(const AValue: TBCButtonStyle);
begin
  if FStyle = AValue then
    exit;
  FStyle := AValue;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.UpdateSize;
begin
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TCustomBCButton.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
var
//  AWidth: integer;
  gh,gw: integer;
  actualCaption: TCaption;
  horizAlign, relHorizAlign: TAlignment;
  vertAlign, relVertAlign: TTextLayout;
  glyphHorzMargin, glyphVertMargin: integer;
  tw, th, availW: integer;
  canvasScale: single;
  scaledFont: TBCFont;
  ownScaledFont: Boolean;
begin
  if (Parent = nil) or (not Parent.HandleAllocated) then
    Exit;

  FLastBorderWidth := FStateNormal.Border.Width;
  CalculateGlyphSize(gw, gh);

  // more precise computation of font with Retina scaling
  canvasScale := GetCanvasScaleFactor;
  if (canvasScale <> 1) and FShowCaption then
  begin
    scaledFont := TBCFont.Create(nil);
    scaledFont.Assign(FStateNormal.FontEx);
    scaledFont.Scale(canvasScale, false);
    ownScaledFont := true;
  end else
  begin
    scaledFont := FStateNormal.FontEx;
    ownScaledFont := false;
    canvasScale := 1;
  end;

  if GlyphOldPlacement then
  begin
    {  if WidthIsAnchored then
        AWidth := Width
      else
        AWidth := 10000;}

    PreferredWidth := 0;
    PreferredHeight := 0;
    if FShowCaption then
    begin
      CalculateTextSize(Caption, scaledFont, PreferredWidth, PreferredHeight);
      PreferredWidth := ceil(PreferredWidth/canvasScale);
      PreferredHeight := ceil(PreferredHeight/canvasScale);
    end;

    // Extra pixels for DropDown
    if Style = bbtDropDown then
      if FDropDownPosition in [bdpBottom] then
        Inc(PreferredHeight, GetDropDownWidth)
      else
        Inc(PreferredWidth, GetDropDownWidth);

    if (Style = bbtButton) and FDropDownArrow then
      Inc(PreferredWidth, FDropDownArrowSize);// GetDropDownWidth);


    //if (FGlyph <> nil) and (not FGlyph.Empty) then
    if (gw > 0) and (gh > 0) then
    begin
      //if Caption = '' then
      if PreferredWidth = 0 then
      begin
        Inc(PreferredWidth, gw{ - AutoSizeExtraY * 2});
        Inc(PreferredHeight, gh);
      end
      else
      begin
        Inc(PreferredWidth, gw + FGlyphMargin);
        if gh > PreferredHeight then
          PreferredHeight := gh;
      end;
    end;

    // Extra pixels for AutoSize
    Inc(PreferredWidth, AutoSizeExtraX);
    Inc(PreferredHeight, AutoSizeExtraY);
  end else
  begin
    if ShowCaption then actualCaption := Caption else actualCaption := '';
    PreferredWidth := round(InnerMargin);
    PreferredHeight := round(InnerMargin);
    case FStyle of
    bbtDropDown:
      case FDropDownPosition of
        bdpBottom: inc(PreferredHeight, GetDropDownWidth(False));
        else{bdpLeft} inc(PreferredWidth, GetDropDownWidth(False));
      end;
    else{bbtButton} if FDropDownArrow then
      inc(PreferredWidth, FDropDownWidth);
    end;
    inc(PreferredWidth, FStateNormal.Border.Width);
    inc(PreferredHeight, FStateNormal.Border.Width);

    if actualCaption='' then
    begin
      inc(PreferredWidth,gw);
      inc(PreferredHeight,gh);
      if gw>0 then inc(PreferredWidth, GlyphMargin*2);
      if gh>0 then inc(PreferredHeight, GlyphMargin*2);
    end else
    begin
      GetGlyphActualLayout(actualCaption, FStateNormal.FontEx, GlyphAlignment, GlyphMargin,
        horizAlign, vertAlign, relHorizAlign, relVertAlign, glyphHorzMargin, glyphVertMargin);
      availW := 65535;
      if (Align in [alTop,alBottom]) and (Parent <> nil) then
        availW := Parent.ClientWidth - PreferredWidth;
      CalculateTextSizeEx(actualCaption, scaledFont, tw, th, availW);
      tw := ceil(tw/canvasScale);
      th := ceil(th/canvasScale);

      if (tw<>0) and FStateNormal.FontEx.WordBreak then inc(tw);
      if vertAlign<>relVertAlign then
      begin
        inc(PreferredWidth,  max(gw+2*GlyphMargin,tw));
        inc(PreferredHeight, GlyphMargin+gh+th);
      end
      else
      begin
        inc(PreferredWidth,  GlyphMargin+gw+tw);
        inc(PreferredHeight, max(gh+2*GlyphMargin,th));
      end;
    end;
  end;
  if ownScaledFont then scaledFont.Free;
end;

class function TCustomBCButton.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 123;
  Result.CY := 33;
end;

procedure TCustomBCButton.Click;
begin
  if (FActiveButt = bbtDropDown) and Assigned(FOnButtonClick) then
  begin
    FOnButtonClick(Self);
    Exit;
  end;
  inherited Click;
end;

procedure TCustomBCButton.DropDownClosed(Sender: TObject);
begin
  if Assigned(FSaveDropDownClosed) then
    FSaveDropDownClosed(Sender);
  {$IFDEF FPC}//#
  if Assigned(FDropDownMenu) then
    FDropDownMenu.OnClose := FSaveDropDownClosed;
  {$ENDIF}

  FDropDownMenuVisible := False;
  FDropDownClosingTime := Now;
end;

function TCustomBCButton.GetBGRAClick: TBGRABitmapEx;
begin
  if FBGRAClick = nil then
  begin
    FBGRAClick := TBGRABitmapEx.Create(round(Width * FCanvasScale),
      round(Height * FCanvasScale), BGRAPixelTransparent);
    FBGRAClick.CustomData := PtrInt(FStateClicked);
  end;
  result := FBGRAClick;
end;

function TCustomBCButton.GetBGRAHover: TBGRABitmapEx;
begin
  if FBGRAHover = nil then
  begin
    FBGRAHover := TBGRABitmapEx.Create(round(Width * FCanvasScale),
      round(Height * FCanvasScale), BGRAPixelTransparent);
    FBGRAHover.CustomData := PtrInt(FStateHover);
  end;
  result := FBGRAHover;
end;

function TCustomBCButton.GetBGRANormal: TBGRABitmapEx;
begin
  if FBGRANormal = nil then
  begin
    FBGRANormal := TBGRABitmapEx.Create(round(Width * FCanvasScale),
      round(Height * FCanvasScale), BGRAPixelTransparent);
    FBGRANormal.CustomData := PtrInt(FStateNormal);
  end;
  result := FBGRANormal;
end;

procedure TCustomBCButton.OnRestoreProperty(Sender: TObject; AObject: TObject;
  Info: PPropInfo; AValue: TJSONData; var Handled: Boolean);
var
  bgracolor: TBGRAPixel;
begin
  Handled := False;
  if (Info^.PropType^.Name = 'TGraphicsColor') then
  begin
    Handled := True;
    bgracolor := StrToBGRA(AValue.AsString);
    SetPropValue(AObject, Info, BGRAToColor(bgracolor));
  end;

  // fix to don't assign null values
  if AValue.JSONType = jtNULL then
    Handled := True;
end;

procedure TCustomBCButton.OnStreamProperty(Sender: TObject; AObject: TObject;
  Info: PPropInfo; var Res: TJSONData);
var
  bgracolor: TBGRAPixel;
begin
  if (Info^.PropType^.Name = 'TGraphicsColor') then
  begin
    bgracolor := ColorToBGRA(TColor(GetPropValue(AObject, Info, False)));
    Res.Free;
    Res := TJSONString.Create('rgb('+IntToStr(bgracolor.red)+','+IntToStr(bgracolor.green)+','+IntToStr(bgracolor.blue)+')');
  end;
end;

procedure TCustomBCButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
 ClientToScreenPoint : TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if csDesigning in ComponentState then
    exit;

  if (Button = mbLeft) and Enabled {and (not (FButtonState = msClicked)) } then
  begin
    case FActiveButt of
      bbtButton:
        if not (FButtonState = msClicked) then
        begin
          FButtonState := msClicked;
          if FDropDownStyle = bdsCommon then
            FDownButtonState := msClicked
          else
            FDownButtonState := msNone;
          Invalidate;
        end;
      bbtDropDown:
        if not (FDownButtonState = msClicked) then
        begin
          if FDropDownStyle = bdsCommon then
            FButtonState := msClicked
          else
            FButtonState := msNone;
          FDownButtonState := msClicked;
          Invalidate;
        end;
    end;
    // Old
    {FButtonState := msClicked;
    Invalidate;}

    // MORA: Show DropDown menu
    if FDropDownMenuVisible or (Now < FDropDownClosingTime+DropDownReopenDelay) then
      FDropDownMenuVisible := False // Prevent redropping
    else
    if ((FActiveButt = bbtDropDown) or (FStyle = bbtButton)) and
      (FDropDownMenu <> nil) and Enabled then
    begin
      ClientToScreenPoint := ClientToScreen(Point(0, Height));
      with ClientToScreenPoint do
      begin
        // normal button
        if FStyle = bbtButton then
        begin
          x := x + Width * integer(FDropDownMenu.Alignment = paRight);
          if FFlipArrow then
            y := y -Height;
        end
        else
          // dropdown button
        begin
          if FDropDownPosition = bdpBottom then
          begin
            x := x + Width * integer(FDropDownMenu.Alignment = paRight);
            if FFlipArrow then
              y := y -(FDropDownWidth + (FStateNormal.FBorder.Width * 2));
          end
          else
          begin
            if FFlipArrow then
              y := y -Height;
            if FDropDownStyle = bdsSeparate then
              x := x + Width - (FDropDownWidth + (FStateNormal.FBorder.Width * 2)) *
                integer(FDropDownMenu.Alignment <> paRight)
            else
              x := x + Width * integer(FDropDownMenu.Alignment = paRight);
          end;
        end;

        FDropDownMenuVisible := True;
        {$IFDEF FPC}//#
        FSaveDropDownClosed := FDropDownMenu.OnClose;
        FDropDownMenu.OnClose := DropDownClosed;
        {$ENDIF}
        FDropDownMenu.PopUp(x, y);
      end;
    end;
  end;
end;

procedure TCustomBCButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
{var
  p: TPoint;}
begin
  inherited MouseUp(Button, Shift, X, Y);
  if csDesigning in ComponentState then
    exit;

  if (Button = mbLeft) and Enabled {and (FButtonState = msClicked)} then
  begin
    case FActiveButt of
      bbtButton:
        if FButtonState = msClicked then
        begin
          FButtonState := msHover;
          if FDropDownStyle = bdsCommon then
            FDownButtonState := msHover
          else
            FDownButtonState := msNone;
          Invalidate;
        end;
      bbtDropDown:
        if FDownButtonState = msClicked then
        begin
          FDownButtonState := msHover;
          if FDropDownStyle = bdsCommon then
            FButtonState := msHover
          else
            FButtonState := msNone;
          Invalidate;
        end;
    end;
    // Old
    {FButtonState := msHover;
    Invalidate;}
  end;

  //if (FActiveButt = bbtDropDown) and (PopupMenu <> nil) and Enabled then
  //begin
  //  if FFlipArrow then
  //    p := ClientToScreen(Point(Width - FDropDownWidth - (FStateNormal.FBorder.Width * 2),
  //      {PopupMenu.Height} -1))
  //  else
  //    p := ClientToScreen(Point(Width - FDropDownWidth - (FStateNormal.FBorder.Width * 2), Height + 1));

  //  PopupMenu.PopUp(p.x, p.y);
  //end;
end;

procedure TCustomBCButton.MouseEnter;
begin
  if csDesigning in ComponentState then
    exit;
  case FActiveButt of
    bbtButton:
    begin
      if FDown then
        FButtonState := msClicked
      else
        FButtonState := msHover;

      if FDropDownStyle = bdsSeparate then
        FDownButtonState := msNone
      else
        FDownButtonState := msHover;
    end;
    bbtDropDown:
    begin
      if FDown then
        FButtonState := msClicked
      else
      if FDropDownStyle = bdsSeparate then
        FButtonState := msNone
      else
        FButtonState := msHover;
      FDownButtonState := msHover;
    end;
  end;
  Invalidate;
  // Old
  {FButtonState := msHover;
  Invalidate;}
  inherited MouseEnter;
end;

procedure TCustomBCButton.MouseLeave;
begin
  if csDesigning in ComponentState then
    exit;
  if FDown then
  begin
    FButtonState := msClicked;
    FActiveButt := bbtButton;
  end
  else
    FButtonState := msNone;
  FDownButtonState := msNone;
  Invalidate;
  inherited MouseLeave;
end;

procedure TCustomBCButton.MouseMove(Shift: TShiftState; X, Y: integer);

  function IsOverDropDown: boolean;
  begin
    with GetButtonRect do
      case FDropDownPosition of
        bdpBottom:
          Result := Y > Bottom;
        else
          Result := X > GetButtonRect.Right;
      end;
  end;

begin
  inherited MouseMove(Shift, X, Y);

  if FStyle = bbtButton then
    FActiveButt := bbtButton
  else
  begin
    // Calling invalidate only when active button changed. Otherwise, we leave
    // this for LCL. This reduce paint call
    if (FActiveButt = bbtButton) and IsOverDropDown then
    begin
      FActiveButt := bbtDropDown;
      if FDropDownStyle <> bdsCommon then // Don't need invalidating
      begin
        FDownButtonState := msHover;
        if FDown then
          FButtonState := msClicked
        else
          FButtonState := msNone;
        Invalidate;
      end;
    end
    else
    if (FActiveButt = bbtDropDown) and not IsOverDropDown then
    begin
      FActiveButt := bbtButton;
      if FDropDownStyle <> bdsCommon then // Don't need invalidating
      begin
        if FDown then
          FButtonState := msClicked
        else
          FButtonState := msHover;
        FDownButtonState := msNone;
        Invalidate;
      end;
    end;
  end;
end;

procedure TCustomBCButton.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.TextChanged;
begin
  inherited TextChanged;
  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.ActionChange(Sender: TObject; CheckDefaults: boolean);
var
  NewAction: TCustomAction;
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
  begin
    NewAction := TCustomAction(Sender);
    if (not CheckDefaults) or (not Down) then
      Down := NewAction.Checked;
    if (not CheckDefaults) or (ImageIndex < 0) then
      ImageIndex := NewAction.ImageIndex;
  end;
end;

function TCustomBCButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TBCButtonActionLink;
end;

procedure TCustomBCButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FImages) and (Operation = opRemove) then
    Images := nil;
end;

procedure TCustomBCButton.UpdateControl;
begin
  RenderControl;
  inherited UpdateControl; // indalidate
end;

procedure TCustomBCButton.ScaleStyle(AScale: single; APreserveDefaultFontHeight: boolean);
begin
  StateNormal.Scale(AScale, APreserveDefaultFontHeight);
  StateHover.Scale(AScale, APreserveDefaultFontHeight);
  StateClicked.Scale(AScale, APreserveDefaultFontHeight);
  Rounding.Scale(AScale);
  RoundingDropDown.Scale(AScale);
  DropDownWidth:= round(DropDownWidth*AScale);
  DropDownArrowSize:= round(DropDownArrowSize*AScale);
  GlyphMargin:= round(GlyphMargin*AScale);
  GlyphScale := GlyphScale*AScale;
  InnerMargin:= round(InnerMargin*AScale);
end;

{$IFDEF FPC}//#
procedure TCustomBCButton.SaveToFile(AFileName: string);
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

procedure TCustomBCButton.SaveToJSONFile(AFileName: string);
begin
  with TStringList.Create do
  begin
    try
      Text := SaveToJSON;
      SaveToFile(AFileName);
    finally
      Free;
    end;
  end;
end;

function TCustomBCButton.SaveToJSON: string;
var
  Streamer: TJSONStreamer;
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    Streamer.OnStreamProperty := OnStreamProperty;
    Result := Streamer.ObjectToJSONString(Self);
  finally
    Streamer.Destroy;
  end;
end;

procedure TCustomBCButton.LoadFromFile(AFileName: string);
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

procedure TCustomBCButton.LoadFromJSONFile(AFileName: string);
var
  sFile: TStringList;
begin
  try
    sFile := TStringList.Create;
    sFile.LoadFromFile(AFileName);
    LoadFromJSON(sFile.Text);
  finally
    sFile.Free;
  end;
end;

procedure TCustomBCButton.LoadFromJSON(AJSON: string);
var
  DeStreamer: TJSONDeStreamer;
  temp: TBCButton;
begin
  temp := TBCButton.Create(nil);
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    DeStreamer.OnRestoreProperty := OnRestoreProperty;
    DeStreamer.JSONToObject(AJSON, temp);
    // Cascading
    Self.BeginUpdate;
    Self.Assign(temp);
    Self.StateNormal.Assign(temp.StateNormal);
    Self.StateHover.Assign(temp.StateNormal);
    Self.StateClicked.Assign(temp.StateNormal);
    // All other properties
    DeStreamer.JSONToObject(AJSON, Self);
    Self.EndUpdate;
  finally
    temp.Free;
    DeStreamer.Destroy;
  end;
end;

procedure TCustomBCButton.AssignFromFile(AFileName: string);
var
  AStream: TMemoryStream;
  AButton: TBCButton;
begin
  AButton := TBCButton.Create(nil);
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

procedure TCustomBCButton.AssignFromResource(AResourceName: string);
var
  AStream : TStream;
  AButton : TBCButton;
begin
  AButton := TBCButton.Create(nil);
  try
    AStream := BGRAResource.GetResourceStream(AResourceName);
    ReadComponentFromTextStream(AStream, TComponent(AButton), OnFindClass);
    Assign(AButton);
  finally
    AStream.Free;
    AButton.Free;
  end;
end;

{$ENDIF}

procedure TCustomBCButton.OnFindClass(Reader: TReader; const AClassName: string;
  var ComponentClass: TComponentClass);
begin
  if CompareText(AClassName, 'TBCButton') = 0 then
    ComponentClass := TBCButton;
end;

{$IFDEF INDEBUG}
function TCustomBCButton.GetDebugText: string;
begin
  Result := 'R: ' + IntToStr(FRenderCount);
end;

{$ENDIF}

procedure TCustomBCButton.DrawControl;
var
  bgra: TBGRABitmapEx;
  r: TRect;
begin
  if (CanvasScaleMode = csmFullResolution) or
    ((CanvasScaleMode = csmAuto) and not Assigned(OnAfterRenderBCButton)) then
    FCanvasScale := GetCanvasScaleFactor
    else FCanvasScale := 1;

  // If style is without dropdown button or state of each button
  // is the same (possible only for msNone) or static button then
  // we can draw whole BGRABitmap
  if (FStyle = bbtButton) or (FButtonState = FDownButtonState) or FStaticButton then
  begin
    // Main button
    if FStaticButton then
      bgra := BGRANormal
    else
    if FDown then
      bgra := BGRAClick
    else
      case FButtonState of
        msNone: bgra := BGRANormal;
        msHover: bgra := BGRAHover;
        msClicked: bgra := BGRAClick;
      end;
    if {%H-}bgra.NeedRender then
      Render(bgra, TBCButtonState(bgra.CustomData));
    bgra.Draw(Self.Canvas, rect(0, 0, Width, Height), False);
  end
  // Otherwise we must draw part of state for each button
  else
  begin
    // The active button must be draw as last because right edge of button and
    // left edge of dropdown are overlapping each other, so we must draw edge
    // for current state of active button
    case FActiveButt of
      bbtButton:
      begin
        // Drop down button
        case FDownButtonState of
          msNone: bgra := BGRANormal;
          msHover: bgra := BGRAHover;
          msClicked: bgra := BGRAClick;
        end;
        if bgra.NeedRender then
          Render(bgra, TBCButtonState(bgra.CustomData));
        r := GetDropDownRect;
        bgra.DrawPart(ScaleRect(r, FCanvasScale), Self.Canvas, r, False);
        // Main button
        if FDown then
          bgra := BGRAClick
        else
          case FButtonState of
            msNone: bgra := BGRANormal;
            msHover: bgra := BGRAHover;
            msClicked: bgra := BGRAClick;
          end;
        if bgra.NeedRender then
          Render(bgra, TBCButtonState(bgra.CustomData));
        r := GetButtonRect;
        bgra.DrawPart(ScaleRect(r, FCanvasScale), Self.Canvas, r, False);
      end;
      bbtDropDown:
      begin
        // Main button
        if FDown then
          bgra := BGRAClick
        else
          case FButtonState of
            msNone: bgra := BGRANormal;
            msHover: bgra := BGRAHover;
            msClicked: bgra := BGRAClick;
          end;
        if bgra.NeedRender then
          Render(bgra, TBCButtonState(bgra.CustomData));
        r := GetButtonRect;
        bgra.DrawPart(ScaleRect(r, FCanvasScale), Self.Canvas, r, False);
        // Drop down button
        case FDownButtonState of
          msNone: bgra := BGRANormal;
          msHover: bgra := BGRAHover;
          msClicked: bgra := BGRAClick;
        end;
        if bgra.NeedRender then
          Render(bgra, TBCButtonState(bgra.CustomData));
        r := GetDropDownRect;
        bgra.DrawPart(ScaleRect(r, FCanvasScale), Self.Canvas, r, False);
      end;
    end;
  end;

  LimitMemoryUsage;
end;

procedure TCustomBCButton.RenderControl;
begin
  inherited RenderControl;
  RenderAll;
end;

procedure TCustomBCButton.SetGlobalOpacity(const AValue: byte);
begin
  if FGlobalOpacity = AValue then
    exit;
  FGlobalOpacity := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetTextApplyGlobalOpacity(const AValue: boolean);
begin
  if FTextApplyGlobalOpacity = AValue then
    exit;
  FTextApplyGlobalOpacity := AValue;

  RenderControl;
  Invalidate;
end;

constructor TCustomBCButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF INDEBUG}
  FRenderCount := 0;
  {$ENDIF}
  FMemoryUsage := bmuHigh;
  {$IFDEF FPC}
  DisableAutoSizing;
  Include(FControlState, csCreating);
  {$ELSE} //#

  {$ENDIF}
  //{$IFDEF WINDOWS}
  // default sizes under different dpi settings
  //SetSizeVariables(ScaleX(8,96), ScaleX(16,96), ScaleY(8,96), ScaleX(24,96));
  //{$ELSE}
  // default sizes
  SetSizeVariables(16, 8, 8, 24);
  //{$ENDIF}
  BeginUpdate;
  try
    with GetControlClassDefaultSize do
      SetInitialBounds(0, 0, CX, CY);
    ControlStyle := ControlStyle + [csAcceptsControls];

    ParentColor := False;
    Color := clNone;
    FCanvasScale:= 1; //will be updated after control is created

    FStateNormal := TBCButtonState.Create(Self);
    FStateHover := TBCButtonState.Create(Self);
    FStateClicked := TBCButtonState.Create(Self);
    FStateNormal.OnChange := OnChangeState;
    FStateHover.OnChange := OnChangeState;
    FStateClicked.OnChange := OnChangeState;

    FRounding := TBCRounding.Create(Self);
    FRounding.OnChange := OnChangeState;

    FRoundingDropDown := TBCRounding.Create(Self);
    FRoundingDropDown.OnChange := OnChangeState;

    FButtonState := msNone;
    FDownButtonState := msNone;
    FFlipArrow := False;
    FGlyph := TBitmap.Create;
    FGlyph.OnChange := OnChangeGlyph;
    FGlyphMargin := 5;
    FGlyphAlignment:= bcaCenter;
    FGlyphOldPlacement:= true;
    FGlyphScale:= 1;
    FStyle := bbtButton;
    FStaticButton := False;
    FActiveButt := bbtButton;
    FGlobalOpacity := 255;
    FTextApplyGlobalOpacity := False;
    //FStates := [];
    FDown := False;

    { Default style }
    AssignDefaultStyle;

    FImageChangeLink := TChangeLink.Create;
    FImageChangeLink.OnChange := ImageListChange;
    FImageIndex := -1;

    FShowCaption := True;
    FPreserveGlyphOnAssign := True;
  finally
    {$IFDEF FPC}
    Exclude(FControlState, csCreating);
    EnableAutoSizing;
    {$ELSE} //#
    {$ENDIF}
    EndUpdate;
  end;
end;

destructor TCustomBCButton.Destroy;
begin
  FImageChangeLink.Free;
  FStateNormal.Free;
  FStateHover.Free;
  FStateClicked.Free;
  FBGRANormal.Free;
  FBGRAHover.Free;
  FBGRAClick.Free;
  FreeAndNil(FGlyph);
  FRounding.Free;
  FRoundingDropDown.Free;
  inherited Destroy;
end;

procedure TCustomBCButton.Assign(Source: TPersistent);
begin
  if Source is TCustomBCButton then
  begin
    if not PreserveGlyphOnAssign then
      Glyph := TCustomBCButton(Source).Glyph;
    FGlyphMargin := TCustomBCButton(Source).FGlyphMargin;
    FStyle := TCustomBCButton(Source).FStyle;
    FFlipArrow := TCustomBCButton(Source).FFlipArrow;
    FStaticButton := TCustomBCButton(Source).FStaticButton;
    FGlobalOpacity := TCustomBCButton(Source).FGlobalOpacity;
    FTextApplyGlobalOpacity := TCustomBCButton(Source).FTextApplyGlobalOpacity;
    FStateNormal.Assign(TCustomBCButton(Source).FStateNormal);
    FStateHover.Assign(TCustomBCButton(Source).FStateHover);
    FStateClicked.Assign(TCustomBCButton(Source).FStateClicked);
    FDropDownArrowSize := TCustomBCButton(Source).FDropDownArrowSize;
    FDropDownWidth := TCustomBCButton(Source).FDropDownWidth;
    AutoSizeExtraX := TCustomBCButton(Source).AutoSizeExtraX;
    AutoSizeExtraY := TCustomBCButton(Source).AutoSizeExtraY;
    FDown := TCustomBCButton(Source).FDown;
    FRounding.Assign(TCustomBCButton(Source).FRounding);
    FRoundingDropDown.Assign(TCustomBCButton(Source).FRoundingDropDown);

    RenderControl;
    Invalidate;
    UpdateSize;
  end
  else
    inherited Assign(Source);
end;

procedure TCustomBCButton.SetSizeVariables(newDropDownWidth,
  newDropDownArrowSize, newAutoSizeExtraVertical, newAutoSizeExtraHorizontal: integer);
begin
  FDropDownArrowSize := newDropDownArrowSize;
  FDropDownWidth := newDropDownWidth;
  AutoSizeExtraY := newAutoSizeExtraVertical;
  AutoSizeExtraX := newAutoSizeExtraHorizontal;

  if csCreating in ControlState then
    Exit;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

function TCustomBCButton.GetStyleExtension: string;
begin
  Result := 'bcbtn';
end;

end.
