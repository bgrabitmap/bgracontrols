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

  Copyright (C) 2012 Krzysztof Dibowski dibowski at interia.pl

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

unit BCButtonFocus;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Controls, Dialogs, BGRABitmap, BGRABitmapTypes,
  ActnList, ImgList, Menus, // MORA
  Buttons, Graphics, LCLType, types, BCTypes, Forms, BCBasectrls,
  BCThemeManager, LMessages;

{off $DEFINE DEBUG}

type
  TBCButtonFocusMemoryUsage = (bmuLowF, bmuMediumF, bmuHighF);
  TBCButtonFocusState = class;
  TBCButtonFocusStyle = (bbtButtonF, bbtDropDownF);
  TOnAfterRenderBCButtonFocus = procedure(Sender: TObject; const ABGRA: TBGRABitmap;
    AState: TBCButtonFocusState; ARect: TRect) of object;
  TBCButtonFocusPropertyData = (pdNoneF, pdUpdateSizeF);

  // MORA: DropDown styles
  TBCButtonFocusDropDownStyle = (
    bdsSeparateF,     // DropDown is a separate button (default)
    bdsCommonF        // DropDown is same as main button
    );
  TBCButtonFocusDropDownPosition = (
    bdpLeftF,         // default
    bdpBottomF);

  { TBCButtonFocusState }

  TBCButtonFocusState = class(TBCProperty)
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
  published
    property Background: TBCBackground read FBackground write SetBackground;
    property Border: TBCBorder read FBorder write SetBorder;
    property FontEx: TBCFont read FFontEx write SetFontEx;
  end;

  { TCustomBCButtonFocus }

  TCustomBCButtonFocus = class(TBCStyleCustomControl)
  private
    { Private declarations }
    {$IFDEF DEBUG}
    FRenderCount: integer;
    {$ENDIF}
    FDropDownArrowSize: integer;
    FDropDownWidth: integer;
    FFlipArrow: boolean;
    FActiveButt: TBCButtonFocusStyle;
    FBGRANormal, FBGRAHover, FBGRAClick: TBGRABitmapEx;
    FInnerMargin: single;
    FMemoryUsage: TBCButtonFocusMemoryUsage;
    FOnPaintButton: TNotifyEvent;
    FPreserveGlyphOnAssign: boolean;
    FRounding: TBCRounding;
    FRoundingDropDown: TBCRounding;
    FStateClicked: TBCButtonFocusState;
    FStateHover: TBCButtonFocusState;
    FStateNormal: TBCButtonFocusState;
    FDown: boolean;
    FGlyph: TBitmap;
    FGlyphMargin: integer;
    FButtonState: TBCMouseState;
    FDownButtonState: TBCMouseState;
    FOnAfterRenderBCButton: TOnAfterRenderBCButtonFocus;
    FOnButtonClick: TNotifyEvent;
    FStaticButton: boolean;
    FStyle: TBCButtonFocusStyle;
    FGlobalOpacity: byte;
    FTextApplyGlobalOpacity: boolean;
    AutoSizeExtraY: integer;
    AutoSizeExtraX: integer;
    // MORA
    FClickOffest: boolean;
    FDropDownArrow: boolean;
    FDropDownMenu: TPopupMenu;
    FDropDownMenuVisible: boolean;
    FDropDownPosition: TBCButtonFocusDropDownPosition;
    FDropDownStyle: TBCButtonFocusDropDownStyle;
    FImageChangeLink: TChangeLink;
    FImageIndex: integer;
    FImages: TCustomImageList;
    FSaveDropDownClosed: TNotifyEvent;
    FShowCaption: boolean;
    procedure AssignDefaultStyle;
    procedure CalculateGlyphSize(var NeededWidth, NeededHeight: integer);
    procedure ConvertToGrayScale(ABGRA: TBGRABitmap);
    procedure DropDownClosed(Sender: TObject);
    procedure RenderAll(ANow: boolean = False);
    function GetButtonRect: TRect;
    function GetDropDownWidth(AFull: boolean = True): integer;
    function GetDropDownRect(AFull: boolean = True): TRect;
    procedure SeTBCButtonStateClicked(const AValue: TBCButtonFocusState);
    procedure SeTBCButtonStateHover(const AValue: TBCButtonFocusState);
    procedure SeTBCButtonStateNormal(const AValue: TBCButtonFocusState);
    procedure SetClickOffset(AValue: boolean);
    procedure SetDown(AValue: boolean);
    procedure SetDropDownArrow(AValue: boolean);
    procedure SetDropDownArrowSize(AValue: integer);
    procedure SetDropDownPosition(AValue: TBCButtonFocusDropDownPosition);
    procedure SetDropDownWidth(AValue: integer);
    procedure SetFlipArrow(AValue: boolean);
    procedure SetGlyph(const AValue: TBitmap);
    procedure SetGlyphMargin(const AValue: integer);
    procedure SetImageIndex(AValue: integer);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetInnerMargin(AValue: single);
    procedure SetMemoryUsage(AValue: TBCButtonFocusMemoryUsage);
    procedure SetRounding(AValue: TBCRounding);
    procedure SetRoundingDropDown(AValue: TBCRounding);
    procedure SetShowCaption(AValue: boolean);
    procedure SetStaticButton(const AValue: boolean);
    procedure SetStyle(const AValue: TBCButtonFocusStyle);
    procedure SetGlobalOpacity(const AValue: byte);
    procedure SetTextApplyGlobalOpacity(const AValue: boolean);
    procedure UpdateSize;
    procedure OnChangeGlyph({%H-}Sender: TObject);
    procedure OnChangeState({%H-}Sender: TObject; AData: PtrInt);
    procedure ImageListChange(ASender: TObject);
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
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure SetEnabled(Value: boolean); override;
    procedure TextChanged; override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
  protected
    // MORA
    procedure ActionChange(Sender: TObject; CheckDefaults: boolean); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure Render(ABGRA: TBGRABitmapEx; AState: TBCButtonFocusState); virtual;
    procedure RenderState(ABGRA: TBGRABitmapEx; AState: TBCButtonFocusState;
      const ARect: TRect; ARounding: TBCRounding); virtual;
    property ClickOffset: boolean read FClickOffest write SetClickOffset default False;
    property DropDownArrow: boolean
      read FDropDownArrow write SetDropDownArrow default False;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
    property DropDownStyle: TBCButtonFocusDropDownStyle
      read FDropDownStyle write FDropDownStyle default bdsSeparateF;
    property DropDownPosition: TBCButtonFocusDropDownPosition
      read FDropDownPosition write SetDropDownPosition default bdpLeftF;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property ShowCaption: boolean read FShowCaption write SetShowCaption default True;
  protected
    {$IFDEF DEBUG}
    function GetDebugText: string; override;
    {$ENDIF}
    function GetStyleExtension: string; override;
    procedure DrawControl; override;
    procedure RenderControl; override;
  protected
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure UpdateFocus(AFocused: boolean);
    property AutoSizeExtraVertical: integer read AutoSizeExtraY;
    property AutoSizeExtraHorizontal: integer read AutoSizeExtraX;
    property StateNormal: TBCButtonFocusState read FStateNormal write SeTBCButtonStateNormal;
    property StateHover: TBCButtonFocusState read FStateHover write SeTBCButtonStateHover;
    property StateClicked: TBCButtonFocusState read FStateClicked
      write SeTBCButtonStateClicked;
    property Down: boolean read FDown write SetDown default False;
    property DropDownWidth: integer read FDropDownWidth write SetDropDownWidth;
    property DropDownArrowSize: integer read FDropDownArrowSize
      write SetDropDownArrowSize;
    property FlipArrow: boolean read FFlipArrow write SetFlipArrow default False;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GlyphMargin: integer read FGlyphMargin write SetGlyphMargin default 5;
    property Style: TBCButtonFocusStyle read FStyle write SetStyle default bbtButtonF;
    property StaticButton: boolean
      read FStaticButton write SetStaticButton default False;
    property GlobalOpacity: byte read FGlobalOpacity write SetGlobalOpacity;
    property Rounding: TBCRounding read FRounding write SetRounding;
    property RoundingDropDown: TBCRounding read FRoundingDropDown
      write SetRoundingDropDown;
    property TextApplyGlobalOpacity: boolean
      read FTextApplyGlobalOpacity write SetTextApplyGlobalOpacity;
    property OnAfterRenderBCButton: TOnAfterRenderBCButtonFocus
      read FOnAfterRenderBCButton write FOnAfterRenderBCButton;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property MemoryUsage: TBCButtonFocusMemoryUsage read FMemoryUsage write SetMemoryUsage;
    property InnerMargin: single read FInnerMargin write SetInnerMargin;
    property OnPaintButton: TNotifyEvent read FOnPaintButton write FOnPaintButton;
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
  public
    { Save all published settings to file }
    procedure SaveToFile(AFileName: string);
    { Load and assign all published settings from file }
    procedure LoadFromFile(AFileName: string);
    { Assign the properties from AFileName to this instance }
    procedure AssignFromFile(AFileName: string);
    { Used by SaveToFile/LoadFromFile }
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
  end;

  TBCButtonFocus = class(TCustomBCButtonFocus)
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
    { The style of button that will be used. bbtButton or bbtDropDownF. }
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
    { Limit memory usage by selecting one of the options. Default: bmuHighF. }
    property MemoryUsage;
    { The unique name of the control in the form. }
    property Name;
    { TabStop }
    property TabStop;
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFBCThemeManager;
    property OnPaintButton;
  end;

  { TBCButtonFocusActionLink }

  TBCButtonFocusActionLink = class(TControlActionLink)
  protected
    procedure AssignClient(AClient: TObject); override;
    procedure SetChecked(Value: boolean); override;
    procedure SetImageIndex(Value: integer); override;
  public
    function IsCheckedLinked: boolean; override;
    function IsImageIndexLinked: boolean; override;
  end;

procedure Register;

implementation

uses LCLIntf, Math, LCLProc, BCTools, SysUtils, PropEdits, GraphPropEdits;

type
  TBCButtonImageIndexPropertyEditor = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  end;

{ TBCButtonFocus }

procedure TBCButtonFocus.SetFBCThemeManager(AValue: TBCThemeManager);
begin
  if FBCThemeManager=AValue then Exit;
  FBCThemeManager:=AValue;
end;

function TBCButtonImageIndexPropertyEditor.GetImageList: TCustomImageList;
var
  Component: TPersistent;
begin
  Component := GetComponent(0);
  if Component is TCustomBCButtonFocus then
    Result := TCustomBCButtonFocus(Component).Images
  else
    Result := nil;
end;

procedure Register;
begin
  {$I icons\bcbuttonfocus_icon.lrs}
  RegisterComponents('BGRA Controls', [TBCButtonFocus]);
  RegisterPropertyEditor(TypeInfo(integer), TBCButtonFocus,
    'ImageIndex', TBCButtonImageIndexPropertyEditor);
end;

{ TBCButtonFocusActionLink }

procedure TBCButtonFocusActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TCustomBCButtonFocus;
end;

procedure TBCButtonFocusActionLink.SetChecked(Value: boolean);
begin
  if IsCheckedLinked then
    TCustomBCButtonFocus(FClient).Down := Value;
end;

procedure TBCButtonFocusActionLink.SetImageIndex(Value: integer);
begin
  if IsImageIndexLinked then
    TCustomBCButtonFocus(FClient).ImageIndex := Value;
end;

function TBCButtonFocusActionLink.IsCheckedLinked: boolean;
begin
  Result := inherited IsCheckedLinked and (TCustomBCButtonFocus(FClient).Down =
    (Action as TCustomAction).Checked);
end;

function TBCButtonFocusActionLink.IsImageIndexLinked: boolean;
begin
  Result := inherited IsImageIndexLinked and
    (TCustomBCButtonFocus(FClient).ImageIndex = (Action as TCustomAction).ImageIndex);
end;

{ TBCButtonFocusState }

procedure TBCButtonFocusState.SetFontEx(const AValue: TBCFont);
begin
  if FFontEx = AValue then
    exit;
  FFontEx.Assign(AValue);

  Change;
end;

procedure TBCButtonFocusState.OnChangeFont(Sender: TObject; AData: PtrInt);
begin
  Change(PtrInt(pdUpdateSizeF));
end;

procedure TBCButtonFocusState.OnChangeChildProperty(Sender: TObject; AData: PtrInt);
begin
  Change(AData);
end;

procedure TBCButtonFocusState.SetBackground(AValue: TBCBackground);
begin
  if FBackground = AValue then
    Exit;
  FBackground.Assign(AValue);

  Change;
end;

procedure TBCButtonFocusState.SetBorder(AValue: TBCBorder);
begin
  if FBorder = AValue then
    Exit;
  FBorder.Assign(AValue);

  Change;
end;

constructor TBCButtonFocusState.Create(AControl: TControl);
begin
  FBackground := TBCBackground.Create(AControl);
  FBorder := TBCBorder.Create(AControl);
  FFontEx := TBCFont.Create(AControl);

  FBackground.OnChange := @OnChangeChildProperty;
  FBorder.OnChange := @OnChangeChildProperty;
  FFontEx.OnChange := @OnChangeFont;

  inherited Create(AControl);
end;

destructor TBCButtonFocusState.Destroy;
begin
  FBackground.Free;
  FBorder.Free;
  FFontEx.Free;
  inherited Destroy;
end;

procedure TBCButtonFocusState.Assign(Source: TPersistent);
begin
  if Source is TBCButtonFocusState then
  begin
    FBackground.Assign(TBCButtonFocusState(Source).FBackground);
    FBorder.Assign(TBCButtonFocusState(Source).FBorder);
    FFontEx.Assign(TBCButtonFocusState(Source).FFontEx);

    Change(PtrInt(pdUpdateSizeF));
  end
  else
    inherited Assign(Source);
end;

{ TCustomBCButtonFocus }

procedure TCustomBCButtonFocus.AssignDefaultStyle;
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

procedure TCustomBCButtonFocus.CalculateGlyphSize(var NeededWidth, NeededHeight: integer);
begin
  if Assigned(FGlyph) and not FGlyph.Empty then
  begin
    NeededWidth := FGlyph.Width;
    NeededHeight := FGlyph.Height;
  end
  else
  if Assigned(FImages) then
  begin
    NeededWidth := FImages.Width;
    NeededHeight := FImages.Height;
  end
  else
  begin
    NeededHeight := 0;
    NeededWidth := 0;
  end;
end;

procedure TCustomBCButtonFocus.ConvertToGrayScale(ABGRA: TBGRABitmap);
var
  bounds: TRect;
  px: PBGRAPixel;
  xb, yb: integer;
begin
  bounds := ABGRA.GetImageBounds;
  if (bounds.Right <= bounds.Left) or (bounds.Bottom <= Bounds.Top) then
    exit;

  for yb := bounds.Top to bounds.bottom - 1 do
  begin
    px := ABGRA.scanline[yb] + bounds.left;
    for xb := bounds.left to bounds.right - 1 do
    begin
      px^ := BGRAToGrayscale(px^);
      Inc(px);
    end;
  end;
  ABGRA.InvalidateBitmap;
end;

procedure TCustomBCButtonFocus.RenderAll(ANow: boolean);
begin
  if (csCreating in FControlState) or IsUpdating or (FBGRANormal = nil) then
    Exit;

  if ANow then
  begin
    Render(FBGRANormal, FStateNormal);
    Render(FBGRAHover, FStateHover);
    Render(FBGRAClick, FStateClicked);
  end
  else
  begin
    FBGRANormal.NeedRender := True;
    FBGRAHover.NeedRender := True;
    FBGRAClick.NeedRender := True;
  end;
end;

function TCustomBCButtonFocus.GetButtonRect: TRect;
begin
  Result := GetClientRect;
  if FStyle = bbtDropDownF then
    case FDropDownPosition of
      bdpBottomF:
        Dec(Result.Bottom, GetDropDownWidth(False));
      else
        // bdpLeft:
        Dec(Result.Right, GetDropDownWidth(False));
    end;
end;

function TCustomBCButtonFocus.GetDropDownWidth(AFull: boolean): integer;
begin
  Result := FDropDownWidth + (ifthen(AFull, 2, 1) * FStateNormal.FBorder.Width);
end;

function TCustomBCButtonFocus.GetDropDownRect(AFull: boolean): TRect;
begin
  Result := GetClientRect;
  case FDropDownPosition of
    bdpBottomF:
      Result.Top := Result.Bottom - GetDropDownWidth(AFull);
    else
      // bdpLeft:
      Result.Left := Result.Right - GetDropDownWidth(AFull);
  end;
end;

procedure TCustomBCButtonFocus.Render(ABGRA: TBGRABitmapEx; AState: TBCButtonFocusState);
var
  r, r_a: TRect;

  { TODO: Create customizable glyph position by creating TBCGlyph type
          and method in BCTools which render it }
  procedure _RenderGlyph;
  var
    w, h, t, l: integer;
    g: TBGRABitmap;
    bitmap: TBitmap;
  begin
    // MORA: getting image to draw
    if Assigned(FGlyph) and not FGlyph.Empty then
      bitmap := FGlyph
    else
    if Assigned(FImages) and (FImageIndex > -1) and (FImageIndex < FImages.Count) then
    begin
      bitmap := TBitmap.Create;
      FImages.GetBitmap(FImageIndex, bitmap);
    end
    else
      bitmap := nil;

    if (bitmap <> nil) and (not bitmap.Empty) then
    begin
      if not FShowCaption then
      begin
        w := 0;
        h := 0;
      end
      else
        CalculateTextSize(Caption, AState.FontEx, w, h);
      l := r.Right - Round(((r.Right - r.Left) + w + bitmap.Width) / 2);
      t := r.Bottom - Round(((r.Bottom - r.Top) + bitmap.Height) / 2);
      g := TBGRABitmap.Create(bitmap);
      ABGRA.BlendImage(l, t, g, boLinearBlend);
      g.Free;
      Inc(r.Left, l + bitmap.Width + FGlyphMargin);
    end;

    if bitmap <> FGlyph then
      bitmap.Free;
  end;

begin
  if (csCreating in FControlState) or IsUpdating then
    Exit;

  ABGRA.NeedRender := False;

  { Refreshing size }
  ABGRA.SetSize(Width, Height);

  { Clearing previous paint }
  ABGRA.Fill(BGRAPixelTransparent);

  { Basic body }
  r := GetButtonRect;
  RenderState(ABGRA, AState, r, FRounding);

  { Calculating rect }
  CalculateBorderRect(AState.Border, r);

  if FStyle = bbtDropDownF then
  begin
    r_a := GetDropDownRect;
    RenderState(ABGRA, AState, r_a, FRoundingDropDown);
    CalculateBorderRect(AState.Border, r_a);

    // Click offset for arrow
    if FClickOffest and (AState = FStateClicked) then
    begin
      Inc(r_a.Left, 2);
      Inc(r_a.Top, 2);
    end;

    if FFlipArrow then
      RenderArrow(TBGRABitmap(ABGRA), r_a, FDropDownArrowSize, badUp,
        AState.FontEx.Color)
    else
      RenderArrow(TBGRABitmap(ABGRA), r_a, FDropDownArrowSize, badDown,
        AState.FontEx.Color);
  end;

  // Click offset for text and glyph
  if FClickOffest and (AState = FStateClicked) then
  begin
    Inc(r.Left, 2);
    Inc(r.Top, 2);
  end;

  // DropDown arrow
  if FDropDownArrow and (FStyle <> bbtDropDownF) then
  begin
    r_a := r;
    r_a.Left := r_a.Right - FDropDownWidth;
    if FFlipArrow then
      RenderArrow(TBGRABitmap(ABGRA), r_a, FDropDownArrowSize, badUp,
        AState.FontEx.Color)
    else
      RenderArrow(TBGRABitmap(ABGRA), r_a, FDropDownArrowSize, badDown,
        AState.FontEx.Color);
    Dec(R.Right, FDropDownWidth);
  end;

  if FTextApplyGlobalOpacity then
  begin
    { Drawing text }
    _RenderGlyph;
    if FShowCaption then
      RenderText(r, AState.FontEx, Self.Caption, TBGRABitmap(ABGRA));

    { Set global opacity }
    ABGRA.ApplyGlobalOpacity(FGlobalOpacity);
  end
  else
  begin
    { Set global opacity }
    ABGRA.ApplyGlobalOpacity(FGlobalOpacity);
    { Drawing text }
    _RenderGlyph;
    if FShowCaption then
      RenderText(r, AState.FontEx, Self.Caption, TBGRABitmap(ABGRA));
  end;

  { Convert to gray if not enabled }
  if not Enabled then
    ConvertToGrayScale(ABGRA);

  if Assigned(FOnAfterRenderBCButton) then
    FOnAfterRenderBCButton(Self, ABGRA, AState, r);

  {$IFDEF DEBUG}
  FRenderCount += 1;
  {$ENDIF}
end;

procedure TCustomBCButtonFocus.RenderState(ABGRA: TBGRABitmapEx;
  AState: TBCButtonFocusState; const ARect: TRect; ARounding: TBCRounding);
begin
  RenderBackgroundAndBorder(ARect, AState.FBackground, TBGRABitmap(ABGRA),
    ARounding, AState.FBorder, FInnerMargin);
end;

procedure TCustomBCButtonFocus.OnChangeGlyph(Sender: TObject);
begin
  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButtonFocus.OnChangeState(Sender: TObject; AData: PtrInt);
begin
  RenderControl;
  if TBCButtonFocusPropertyData(AData) = pdUpdateSizeF then
    UpdateSize;
  Invalidate;
end;

procedure TCustomBCButtonFocus.ImageListChange(ASender: TObject);
begin
  if ASender = Images then
  begin
    RenderControl;
    Invalidate;
  end;
end;

procedure TCustomBCButtonFocus.LimitMemoryUsage;
begin
  if (FMemoryUsage = bmuLowF) and Assigned(FBGRANormal) then FBGRANormal.Discard;
  if (FMemoryUsage <> bmuHighF) then
  begin
    if Assigned(FBGRAHover) then FBGRAHover.Discard;
    if Assigned(FBGRAClick) then FBGRAClick.Discard;
  end;
end;

procedure TCustomBCButtonFocus.SeTBCButtonStateClicked(const AValue: TBCButtonFocusState);
begin
  if FStateClicked = AValue then
    exit;
  FStateClicked.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButtonFocus.SeTBCButtonStateHover(const AValue: TBCButtonFocusState);
begin
  if FStateHover = AValue then
    exit;
  FStateHover.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButtonFocus.SeTBCButtonStateNormal(const AValue: TBCButtonFocusState);
begin
  if FStateNormal = AValue then
    exit;
  FStateNormal.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButtonFocus.SetClickOffset(AValue: boolean);
begin
  if FClickOffest = AValue then
    Exit;
  FClickOffest := AValue;
  RenderControl;
end;

procedure TCustomBCButtonFocus.SetDown(AValue: boolean);
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

procedure TCustomBCButtonFocus.SetDropDownArrow(AValue: boolean);
begin
  if FDropDownArrow = AValue then
    Exit;
  FDropDownArrow := AValue;
  RenderControl;
  Invalidate;
end;

procedure TCustomBCButtonFocus.SetDropDownArrowSize(AValue: integer);
begin
  if FDropDownArrowSize = AValue then
    Exit;
  FDropDownArrowSize := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButtonFocus.SetDropDownPosition(AValue: TBCButtonFocusDropDownPosition);
begin
  if FDropDownPosition = AValue then
    Exit;
  FDropDownPosition := AValue;

  if FStyle <> bbtDropDownF then
    Exit;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButtonFocus.SetDropDownWidth(AValue: integer);
begin
  if FDropDownWidth = AValue then
    Exit;
  FDropDownWidth := AValue;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButtonFocus.SetFlipArrow(AValue: boolean);
begin
  if FFlipArrow = AValue then
    Exit;
  FFlipArrow := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButtonFocus.SetGlyph(const AValue: TBitmap);
begin
  if (FGlyph <> nil) and (FGlyph = AValue) then
    exit;

  FGlyph.Assign(AValue);

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButtonFocus.SetGlyphMargin(const AValue: integer);
begin
  if FGlyphMargin = AValue then
    exit;
  FGlyphMargin := AValue;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButtonFocus.SetImageIndex(AValue: integer);
begin
  if FImageIndex = AValue then
    Exit;
  FImageIndex := AValue;
  RenderControl;
  Invalidate;
end;

procedure TCustomBCButtonFocus.SetImages(AValue: TCustomImageList);
begin
  if FImages = AValue then
    Exit;
  FImages := AValue;
  RenderControl;
  Invalidate;
end;

procedure TCustomBCButtonFocus.SetInnerMargin(AValue: single);
begin
  if FInnerMargin=AValue then Exit;
  FInnerMargin:=AValue;
  RenderControl;
  Invalidate;
end;

procedure TCustomBCButtonFocus.SetMemoryUsage(AValue: TBCButtonFocusMemoryUsage);
begin
  if FMemoryUsage=AValue then Exit;
  FMemoryUsage:=AValue;
  LimitMemoryUsage;
end;

procedure TCustomBCButtonFocus.SetRounding(AValue: TBCRounding);
begin
  if FRounding = AValue then
    Exit;
  FRounding.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButtonFocus.SetRoundingDropDown(AValue: TBCRounding);
begin
  if FRoundingDropDown = AValue then
    Exit;
  FRoundingDropDown.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButtonFocus.SetShowCaption(AValue: boolean);
begin
  if FShowCaption = AValue then
    Exit;
  FShowCaption := AValue;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButtonFocus.SetStaticButton(const AValue: boolean);
begin
  if FStaticButton = AValue then
    exit;
  FStaticButton := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButtonFocus.SetStyle(const AValue: TBCButtonFocusStyle);
begin
  if FStyle = AValue then
    exit;
  FStyle := AValue;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButtonFocus.UpdateSize;
begin
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TCustomBCButtonFocus.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
var
//  AWidth: integer;
  gh: integer = 0;
  gw: integer = 0;
begin
  if (Parent = nil) or (not Parent.HandleAllocated) then
    Exit;
{  if WidthIsAnchored then
    AWidth := Width
  else
    AWidth := 10000;}

  PreferredWidth := 0;
  PreferredHeight := 0;
  if FShowCaption then
    CalculateTextSize(Caption, FStateNormal.FontEx, PreferredWidth, PreferredHeight);

  // Extra pixels for DropDown
  if Style = bbtDropDownF then
    if FDropDownPosition in [bdpBottomF] then
      Inc(PreferredHeight, GetDropDownWidth)
    else
      Inc(PreferredWidth, GetDropDownWidth);

  if (Style = bbtButtonF) and FDropDownArrow then
    Inc(PreferredWidth, FDropDownArrowSize);// GetDropDownWidth);

  CalculateGlyphSize(gw, gh);

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
end;

class function TCustomBCButtonFocus.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 123;
  Result.CY := 33;
end;

procedure TCustomBCButtonFocus.Click;
begin
  if (FActiveButt = bbtDropDownF) and Assigned(FOnButtonClick) then
  begin
    FOnButtonClick(Self);
    Exit;
  end;
  inherited Click;
end;

procedure TCustomBCButtonFocus.DropDownClosed(Sender: TObject);
begin
  if Assigned(FSaveDropDownClosed) then
    FSaveDropDownClosed(Sender);
  if Assigned(FDropDownMenu) then
    FDropDownMenu.OnClose := FSaveDropDownClosed;

  // MORA: DropDownMenu is still visible if mouse is over control
  FDropDownMenuVisible := PtInRect(ClientRect, ScreenToClient(Mouse.CursorPos));
end;

procedure TCustomBCButtonFocus.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if csDesigning in ComponentState then
    exit;

  if CanFocus() then SetFocus();

  if (Button = mbLeft) and Enabled {and (not (FButtonState = msClicked)) } then
  begin
    case FActiveButt of
      bbtButtonF:
        if not (FButtonState = msClicked) then
        begin
          FButtonState := msClicked;
          if FDropDownStyle = bdsCommonF then
            FDownButtonState := msClicked
          else
            FDownButtonState := msNone;
          Invalidate;
        end;
      bbtDropDownF:
        if not (FDownButtonState = msClicked) then
        begin
          if FDropDownStyle = bdsCommonF then
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
    if FDropDownMenuVisible then
      FDropDownMenuVisible := False // Prevent redropping
    else
    if ((FActiveButt = bbtDropDownF) or (FStyle = bbtButtonF)) and
      (FDropDownMenu <> nil) and Enabled then
      with ClientToScreen(Point(0, Height)) do
      begin
        // normal button
        if FStyle = bbtButtonF then
        begin
          x := x + Width * integer(FDropDownMenu.Alignment = paRight);
          if FFlipArrow then
            y -= Height;
        end
        else
          // dropdown button
        begin
          if FDropDownPosition = bdpBottomF then
          begin
            x := x + Width * integer(FDropDownMenu.Alignment = paRight);
            if FFlipArrow then
              y -= (FDropDownWidth + (FStateNormal.FBorder.Width * 2));
          end
          else
          begin
            if FFlipArrow then
              y -= Height;
            if FDropDownStyle = bdsSeparateF then
              x := x + Width - (FDropDownWidth + (FStateNormal.FBorder.Width * 2)) *
                integer(FDropDownMenu.Alignment <> paRight)
            else
              x := x + Width * integer(FDropDownMenu.Alignment = paRight);
          end;
        end;

        FDropDownMenuVisible := True;
        FSaveDropDownClosed := FDropDownMenu.OnClose;
        FDropDownMenu.OnClose := @DropDownClosed;
        FDropDownMenu.PopUp(x, y);
      end;
  end;
end;

procedure TCustomBCButtonFocus.MouseUp(Button: TMouseButton; Shift: TShiftState;
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
      bbtButtonF:
        if FButtonState = msClicked then
        begin
          FButtonState := msHover;
          if FDropDownStyle = bdsCommonF then
            FDownButtonState := msHover
          else
            FDownButtonState := msNone;
          Invalidate;
        end;
      bbtDropDownF:
        if FDownButtonState = msClicked then
        begin
          FDownButtonState := msHover;
          if FDropDownStyle = bdsCommonF then
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

  //if (FActiveButt = bbtDropDownF) and (PopupMenu <> nil) and Enabled then
  //begin
  //  if FFlipArrow then
  //    p := ClientToScreen(Point(Width - FDropDownWidth - (FStateNormal.FBorder.Width * 2),
  //      {PopupMenu.Height} -1))
  //  else
  //    p := ClientToScreen(Point(Width - FDropDownWidth - (FStateNormal.FBorder.Width * 2), Height + 1));

  //  PopupMenu.PopUp(p.x, p.y);
  //end;
end;

procedure TCustomBCButtonFocus.MouseEnter;
begin
  if csDesigning in ComponentState then
    exit;
  case FActiveButt of
    bbtButtonF:
    begin
      if FDown then
        FButtonState := msClicked
      else
        FButtonState := msHover;

      if FDropDownStyle = bdsSeparateF then
        FDownButtonState := msNone
      else
        FDownButtonState := msHover;
    end;
    bbtDropDownF:
    begin
      if FDown then
        FButtonState := msClicked
      else
      if FDropDownStyle = bdsSeparateF then
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

procedure TCustomBCButtonFocus.MouseLeave;
begin
  if csDesigning in ComponentState then
    exit;
  if FDown then
  begin
    FButtonState := msClicked;
    FActiveButt := bbtButtonF;
  end
  else
    FButtonState := msNone;
  FDownButtonState := msNone;
  Invalidate;
  inherited MouseLeave;
end;

procedure TCustomBCButtonFocus.MouseMove(Shift: TShiftState; X, Y: integer);

  function IsOverDropDown: boolean;
  begin
    with GetButtonRect do
      case FDropDownPosition of
        bdpBottomF:
          Result := Y > Bottom;
        else
          Result := X > GetButtonRect.Right;
      end;
  end;

begin
  inherited MouseMove(Shift, X, Y);

  if FStyle = bbtButtonF then
    FActiveButt := bbtButtonF
  else
  begin
    // Calling invalidate only when active button changed. Otherwise, we leave
    // this for LCL. This reduce paint call
    if (FActiveButt = bbtButtonF) and IsOverDropDown then
    begin
      FActiveButt := bbtDropDownF;
      if FDropDownStyle <> bdsCommonF then // Don't need invalidating
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
    if (FActiveButt = bbtDropDownF) and not IsOverDropDown then
    begin
      FActiveButt := bbtButtonF;
      if FDropDownStyle <> bdsCommonF then // Don't need invalidating
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

procedure TCustomBCButtonFocus.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButtonFocus.TextChanged;
begin
  inherited TextChanged;
  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButtonFocus.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if (Key = VK_SPACE) or (Key = VK_RETURN) then
    MouseDown(mbLeft, [], 0, 0);
end;

procedure TCustomBCButtonFocus.KeyUp(var Key: word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) or (Key = VK_RETURN) then
  begin
    MouseLeave;
    Self.Click;
  end;

  inherited KeyUp(Key, Shift);
end;

procedure TCustomBCButtonFocus.ActionChange(Sender: TObject; CheckDefaults: boolean);
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

function TCustomBCButtonFocus.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TBCButtonFocusActionLink;
end;

procedure TCustomBCButtonFocus.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FImages) and (Operation = opRemove) then
    Images := nil;
end;

procedure TCustomBCButtonFocus.UpdateControl;
begin
  RenderControl;
  inherited UpdateControl; // indalidate
end;

procedure TCustomBCButtonFocus.SaveToFile(AFileName: string);
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

procedure TCustomBCButtonFocus.LoadFromFile(AFileName: string);
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

procedure TCustomBCButtonFocus.AssignFromFile(AFileName: string);
var
  AStream: TMemoryStream;
  AButton: TBCButtonFocus;
begin
  AButton := TBCButtonFocus.Create(nil);
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

procedure TCustomBCButtonFocus.OnFindClass(Reader: TReader; const AClassName: string;
  var ComponentClass: TComponentClass);
begin
  if CompareText(AClassName, 'TBCButton') = 0 then
    ComponentClass := TBCButtonFocus;
end;

{$IFDEF DEBUG}
function TCustomBCButton.GetDebugText: string;
begin
  Result := 'R: ' + IntToStr(FRenderCount);
end;

{$ENDIF}

procedure TCustomBCButtonFocus.DrawControl;
var
  bgra: TBGRABitmapEx;
begin

  // If style is without dropdown button or state of each button
  // is the same (possible only for msNone) or static button then
  // we can draw whole BGRABitmap
  if (FStyle = bbtButtonF) or (FButtonState = FDownButtonState) or FStaticButton then
  begin
    // Main button
    if FStaticButton then
      bgra := FBGRANormal
    else
    if FDown then
      bgra := FBGRAClick
    else
      case FButtonState of
        msNone: bgra := FBGRANormal;
        msHover: bgra := FBGRAHover;
        msClicked: bgra := FBGRAClick;
      end;
    if {%H-}bgra.NeedRender then
      Render(bgra, TBCButtonFocusState(bgra.CustomData));
    bgra.Draw(Self.Canvas, 0, 0, False);
  end
  // Otherwise we must draw part of state for each button
  else
  begin
    // The active button must be draw as last because right edge of button and
    // left edge of dropdown are overlapping each other, so we must draw edge
    // for current state of active button
    case FActiveButt of
      bbtButtonF:
      begin
        // Drop down button
        case FDownButtonState of
          msNone: bgra := FBGRANormal;
          msHover: bgra := FBGRAHover;
          msClicked: bgra := FBGRAClick;
        end;
        if bgra.NeedRender then
          Render(bgra, TBCButtonFocusState(bgra.CustomData));
        bgra.DrawPart(GetDropDownRect, Self.Canvas, GetDropDownRect.Left,
          GetDropDownRect.Top, False);
        // Main button
        if FDown then
          bgra := FBGRAClick
        else
          case FButtonState of
            msNone: bgra := FBGRANormal;
            msHover: bgra := FBGRAHover;
            msClicked: bgra := FBGRAClick;
          end;
        if bgra.NeedRender then
          Render(bgra, TBCButtonFocusState(bgra.CustomData));
        bgra.DrawPart(GetButtonRect, Self.Canvas, 0, 0, False);
      end;
      bbtDropDownF:
      begin
        // Main button
        if FDown then
          bgra := FBGRAClick
        else
          case FButtonState of
            msNone: bgra := FBGRANormal;
            msHover: bgra := FBGRAHover;
            msClicked: bgra := FBGRAClick;
          end;
        if bgra.NeedRender then
          Render(bgra, TBCButtonFocusState(bgra.CustomData));
        bgra.DrawPart(GetButtonRect, Self.Canvas, 0, 0, False);
        // Drop down button
        case FDownButtonState of
          msNone: bgra := FBGRANormal;
          msHover: bgra := FBGRAHover;
          msClicked: bgra := FBGRAClick;
        end;
        if bgra.NeedRender then
          Render(bgra, TBCButtonFocusState(bgra.CustomData));
        bgra.DrawPart(GetDropDownRect, Self.Canvas, GetDropDownRect.Left,
          GetDropDownRect.Top, False);
      end;
    end;
  end;

  if Assigned(FOnPaintButton) then
    FOnPaintButton(Self);

  LimitMemoryUsage;
end;

procedure TCustomBCButtonFocus.RenderControl;
begin
  inherited RenderControl;
  RenderAll;
end;

procedure TCustomBCButtonFocus.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;

  UpdateFocus(True);
end;

procedure TCustomBCButtonFocus.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited;

  if Message.FocusedWnd <> Handle then
    UpdateFocus(False);
end;

procedure TCustomBCButtonFocus.UpdateFocus(AFocused: boolean);
var
  lForm: TCustomForm;
begin
  lForm := GetParentForm(Self);
  if lForm = nil then
    exit;

  if AFocused then
    ActiveDefaultControlChanged(lForm.ActiveControl)
  else
    ActiveDefaultControlChanged(nil);

  Invalidate;
end;

procedure TCustomBCButtonFocus.SetGlobalOpacity(const AValue: byte);
begin
  if FGlobalOpacity = AValue then
    exit;
  FGlobalOpacity := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButtonFocus.SetTextApplyGlobalOpacity(const AValue: boolean);
begin
  if FTextApplyGlobalOpacity = AValue then
    exit;
  FTextApplyGlobalOpacity := AValue;

  RenderControl;
  Invalidate;
end;

constructor TCustomBCButtonFocus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF DEBUG}
  FRenderCount := 0;
  {$ENDIF}
  FMemoryUsage := bmuHighF;
  DisableAutoSizing;
  Include(FControlState, csCreating);
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
    TabStop := True;
    ControlStyle := ControlStyle + [csAcceptsControls];
    FBGRANormal := TBGRABitmapEx.Create(Width, Height, BGRAPixelTransparent);
    FBGRAHover := TBGRABitmapEx.Create(Width, Height, BGRAPixelTransparent);
    FBGRAClick := TBGRABitmapEx.Create(Width, Height, BGRAPixelTransparent);

    ParentColor := False;
    Color := clNone;

    FStateNormal := TBCButtonFocusState.Create(Self);
    FStateHover := TBCButtonFocusState.Create(Self);
    FStateClicked := TBCButtonFocusState.Create(Self);
    FStateNormal.OnChange := @OnChangeState;
    FStateHover.OnChange := @OnChangeState;
    FStateClicked.OnChange := @OnChangeState;

    FRounding := TBCRounding.Create(Self);
    FRounding.OnChange := @OnChangeState;

    FRoundingDropDown := TBCRounding.Create(Self);
    FRoundingDropDown.OnChange := @OnChangeState;

    { Connecting bitmaps with states property to easy call and access }
    FBGRANormal.CustomData := PtrInt(FStateNormal);
    FBGRAHover.CustomData := PtrInt(FStateHover);
    FBGRAClick.CustomData := PtrInt(FStateClicked);

    FButtonState := msNone;
    FDownButtonState := msNone;
    FFlipArrow := False;
    FGlyph := TBitmap.Create;
    FGlyph.OnChange := @OnChangeGlyph;
    FGlyphMargin := 5;
    FStyle := bbtButtonF;
    FStaticButton := False;
    FActiveButt := bbtButtonF;
    FGlobalOpacity := 255;
    FTextApplyGlobalOpacity := False;
    //FStates := [];
    FDown := False;

    { Default style }
    AssignDefaultStyle;

    FImageChangeLink := TChangeLink.Create;
    FImageChangeLink.OnChange := @ImageListChange;
    FImageIndex := -1;

    FShowCaption := True;
    FPreserveGlyphOnAssign := True;
  finally
    Exclude(FControlState, csCreating);
    EnableAutoSizing;
    EndUpdate;
  end;
end;

destructor TCustomBCButtonFocus.Destroy;
begin
  FImageChangeLink.Free;
  FStateNormal.Free;
  FStateHover.Free;
  FStateClicked.Free;
  FBGRANormal.Free;
  FBGRAHover.Free;
  FBGRAClick.Free;
  FreeThenNil(FGlyph);
  FRounding.Free;
  FRoundingDropDown.Free;
  inherited Destroy;
end;

procedure TCustomBCButtonFocus.Assign(Source: TPersistent);
begin
  if Source is TCustomBCButtonFocus then
  begin
    if not PreserveGlyphOnAssign then
      Glyph := TCustomBCButtonFocus(Source).Glyph;
    FGlyphMargin := TCustomBCButtonFocus(Source).FGlyphMargin;
    FStyle := TCustomBCButtonFocus(Source).FStyle;
    FFlipArrow := TCustomBCButtonFocus(Source).FFlipArrow;
    FStaticButton := TCustomBCButtonFocus(Source).FStaticButton;
    FGlobalOpacity := TCustomBCButtonFocus(Source).FGlobalOpacity;
    FTextApplyGlobalOpacity := TCustomBCButtonFocus(Source).FTextApplyGlobalOpacity;
    FStateNormal.Assign(TCustomBCButtonFocus(Source).FStateNormal);
    FStateHover.Assign(TCustomBCButtonFocus(Source).FStateHover);
    FStateClicked.Assign(TCustomBCButtonFocus(Source).FStateClicked);
    FDropDownArrowSize := TCustomBCButtonFocus(Source).FDropDownArrowSize;
    FDropDownWidth := TCustomBCButtonFocus(Source).FDropDownWidth;
    AutoSizeExtraX := TCustomBCButtonFocus(Source).AutoSizeExtraX;
    AutoSizeExtraY := TCustomBCButtonFocus(Source).AutoSizeExtraY;
    FDown := TCustomBCButtonFocus(Source).FDown;
    FRounding.Assign(TCustomBCButtonFocus(Source).FRounding);
    FRoundingDropDown.Assign(TCustomBCButtonFocus(Source).FRoundingDropDown);

    RenderControl;
    Invalidate;
    UpdateSize;
  end
  else
    inherited Assign(Source);
end;

procedure TCustomBCButtonFocus.SetSizeVariables(newDropDownWidth,
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

function TCustomBCButtonFocus.GetStyleExtension: string;
begin
  Result := 'bcbtn';
end;

end.
