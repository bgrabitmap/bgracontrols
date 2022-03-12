// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Equivalent of standard lazarus TPanel but using BGRA Controls framework for render

  Functionality:
  - Customizable background (gradient etc.)
  - Customizable border (frame 3D or normal border, rounding etc)
  - FontEx (shadow etc.)

  originally written in 2011 by Krzysztof Dibowski dibowski at interia.pl
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCPanel;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Types, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BCBaseCtrls, BGRABitmapTypes, BCTypes, LCLVersion;

type
  TOnAfterRenderBCPanel = procedure(Sender: TObject; const ABGRA: TBGRABitmap;
    ARect: TRect) of object;
  TBCPanelBorderStyle = (bpsBorder, bpsFrame3d);

  { TCustomBCPanel }

  TCustomBCPanel = class(TBCStyleCustomControl)
  private
    { Private declarations }
    {$IFDEF INDEBUG}
    FRenderCount: Integer;
    {$ENDIF}
    FBackground: TBCBackground;
    FBevelWidth: Integer;
    FBGRA: TBGRABitmapEx;
    FBevelInner, FBevelOuter : TBevelCut;
    FBorder: TBCBorder;
    FBorderBCStyle: TBCPanelBorderStyle;
    FFontEx: TBCFont;
    FOnAfterRenderBCPanel: TOnAfterRenderBCPanel;
    FRounding: TBCRounding;
    procedure SetBackground(AValue: TBCBackground);
    procedure SetBevelInner(AValue: TBevelCut);
    procedure SetBevelOuter(AValue: TBevelCut);
    procedure SetBevelWidth(AValue: Integer);
    procedure SetBorder(AValue: TBCBorder);
    procedure SetBorderBCStyle(AValue: TBCPanelBorderStyle);
    procedure SetFontEx(AValue: TBCFont);
    procedure SetRounding(AValue: TBCRounding);
    procedure Render;
    procedure OnChangeProperty({%H-}Sender: TObject; {%H-}AData: BGRAPtrInt);
    procedure OnChangeFont({%H-}Sender: TObject; {%H-}AData: BGRAPtrInt);
  protected
    { Protected declarations }
    procedure AdjustClientRect(var aRect: TRect); override;
    class function GetControlClassDefaultSize: TSize; override;
    function GetDefaultDockCaption: String; override;
    procedure SetEnabled(Value: boolean); override;
    procedure TextChanged; override;
  protected
    function GetStyleExtension: String; override;
    {$IFDEF INDEBUG}
    function GetDebugText: String; override;
    {$ENDIF}
    procedure DrawControl; override;
    procedure RenderControl; override;
  protected
    {$IF LCL_FULLVERSION >= 2080000}
    procedure SetParentBackground(const AParentBackground: Boolean); override;
    {$ENDIF}
    property Background: TBCBackground read FBackground write SetBackground;
    property BevelInner: TBevelCut read FBevelInner write SetBevelInner;
    property BevelOuter: TBevelCut read FBevelOuter write SetBevelOuter;
    property BevelWidth: Integer read FBevelWidth write SetBevelWidth;
    property Border: TBCBorder read FBorder write SetBorder;
    property BorderBCStyle: TBCPanelBorderStyle
      read FBorderBCStyle write SetBorderBCStyle default bpsFrame3d;
    property FontEx: TBCFont read FFontEx write SetFontEx;
    property Rounding: TBCRounding read FRounding write SetRounding;
  protected
    { Events }
    property OnAfterRenderBCPanel: TOnAfterRenderBCPanel
      Read FOnAfterRenderBCPanel Write FOnAfterRenderBCPanel;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateControl; override; // Called by EndUpdate
  public
    { Streaming }
    {$IFDEF FPC}
    procedure SaveToFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);
    {$ENDIF}
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
  end;

  { TBCPanel }

  TBCPanel = class(TCustomBCPanel)
  published
    property Align;
    property Anchors;
    property AssignStyle;
    property AutoSize;
    property BorderSpacing;
    property ChildSizing;
    {$IFDEF FPC} //#
    property OnGetDockCaption;
    {$ENDIF}
    property Background;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property Border;
    property BorderBCStyle;
    property Caption;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FontEx;
    property ParentBackground;
    property PopupMenu;
    property Rounding;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnAfterRenderBCPanel;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

uses BCTools;

{$IFDEF FPC}
procedure Register;
begin
  //{$I icons\bcpanel_icon.lrs}
  RegisterComponents('BGRA Controls', [TBCPanel]);
end;
{$ENDIF}

{ TCustomBCPanel }

procedure TCustomBCPanel.DrawControl;
begin
  inherited DrawControl;
  if FBGRA.NeedRender then
    Render;
  if Assigned (FRounding) then
  begin
    if (FRounding.RoundX<>0) and (FRounding.RoundY<>0) then
      FBGRA.Draw(Self.Canvas, 0, 0, False)
    else
      FBGRA.Draw(Self.Canvas, 0, 0);
  end
  else
    FBGRA.Draw(Self.Canvas, 0, 0);

  {$IFNDEF FPC}//# //@  IN DELPHI RenderControl NEDD. IF NO RenderControl BE BLACK AFTER INVALIDATE.
  FBGRA.NeedRender := True;
  {$ENDIF}
end;

procedure TCustomBCPanel.RenderControl;
begin
  inherited RenderControl;
  if FBGRA<>nil then
    FBGRA.NeedRender := True;
end;

{$IF LCL_FULLVERSION >= 2080000}
procedure TCustomBCPanel.SetParentBackground(const AParentBackground: Boolean);
begin
  if ParentBackground=AParentBackground then
    Exit;
  if AParentBackground then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  inherited;
end;
{$ENDIF}

function TCustomBCPanel.GetStyleExtension: String;
begin
  Result := 'bcpnl';
end;

{$IFDEF INDEBUG}
function TCustomBCPanel.GetDebugText: String;
begin
  Result := 'R: '+IntToStr(FRenderCount);
end;
{$ENDIF}

procedure TCustomBCPanel.Render;
var r: TRect;
begin
  if (csCreating in ControlState) or IsUpdating then
    Exit;

  FBGRA.NeedRender := False;

  FBGRA.SetSize(Width, Height);
  FBGRA.Fill(BGRAPixelTransparent);
  r := FBGRA.ClipRect;

  case FBorderBCStyle of
  bpsBorder:
    begin
      RenderBackgroundAndBorder(FBGRA.ClipRect, FBackground, TBGRABitmap(FBGRA), FRounding, FBorder);
      CalculateBorderRect(FBorder,r);
    end;
  bpsFrame3d:
    begin
      // if BevelOuter is set then draw a frame with BevelWidth
      if (FBevelOuter <> bvNone) and (FBevelWidth > 0) then
        FBGRA.CanvasBGRA.Frame3d(r, FBevelWidth, FBevelOuter,
          BGRA(255, 255, 255, 180), BGRA(0, 0, 0, 160)); // Note: Frame3D inflates ARect

      // if BevelInner is set then skip the BorderWidth and draw a frame with BevelWidth
      if (FBevelInner <> bvNone) and (FBevelWidth > 0) then
      begin
        InflateRect(r, -FBevelWidth, -FBevelWidth);
        FBGRA.CanvasBGRA.Frame3d(r, FBevelWidth, FBevelInner,
          BGRA(255, 255, 255, 160), BGRA(0, 0, 0, 160)); // Note: Frame3D inflates ARect
      end;
      RenderBackground(r, FBackground, TBGRABitmap(FBGRA), nil, True);
    end;
  else
    RenderBackground(FBGRA.ClipRect, FBackground, TBGRABitmap(FBGRA), FRounding, True);
  end;

  if Caption <> '' then
    RenderText(r,FFontEx,Caption,TBGRABitmap(FBGRA),Enabled);

  if Assigned(FOnAfterRenderBCPanel) then
    FOnAfterRenderBCPanel(Self, FBGRA, r);

  {$IFDEF INDEBUG}
  FRenderCount := FRenderCount + 1;
  {$ENDIF}
end;

procedure TCustomBCPanel.OnChangeProperty(Sender: TObject; AData: BGRAPtrInt);
begin
  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.OnChangeFont(Sender: TObject; AData: BGRAPtrInt);
begin
  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.SetRounding(AValue: TBCRounding);
begin
  if FRounding = AValue then Exit;
  FRounding.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.AdjustClientRect(var aRect: TRect);
var BevelSize: Integer;
begin
  inherited AdjustClientRect(aRect);

  BevelSize := BorderWidth;
  if (BevelOuter <> bvNone) then
    inc(BevelSize, BevelWidth);
  if (BevelInner <> bvNone) then
    inc(BevelSize, BevelWidth);

  InflateRect(aRect, -BevelSize, -BevelSize);
end;

class function TCustomBCPanel.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 170;
  Result.CY := 50;
end;

function TCustomBCPanel.GetDefaultDockCaption: String;
begin
  Result := Caption;
end;

procedure TCustomBCPanel.SetBackground(AValue: TBCBackground);
begin
  if FBackground = AValue then Exit;
  FBackground.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.SetBevelInner(AValue: TBevelCut);
begin
  if FBevelInner = AValue then Exit;
  FBevelInner := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.SetBevelOuter(AValue: TBevelCut);
begin
  if FBevelOuter = AValue then Exit;
  FBevelOuter := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.SetBevelWidth(AValue: Integer);
begin
  if FBevelWidth = AValue then Exit;
  FBevelWidth := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.SetBorder(AValue: TBCBorder);
begin
  if FBorder = AValue then Exit;
  FBorder.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.SetBorderBCStyle(AValue: TBCPanelBorderStyle);
begin
  if FBorderBCStyle = AValue then Exit;
  FBorderBCStyle := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.SetFontEx(AValue: TBCFont);
begin
  if FFontEx = AValue then Exit;
  FFontEx.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCPanel.TextChanged;
begin
  {$IFDEF FPC}
  inherited TextChanged;
  {$ENDIF}

  RenderControl;
  Invalidate;
end;

constructor TCustomBCPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  {$IFDEF INDEBUG}
  FRenderCount := 0;
  {$ENDIF}
  {$IFDEF FPC}
  DisableAutoSizing;
  Include(FControlState, csCreating);
  {$ELSE} //#

  {$ENDIF}

  BeginUpdate;
  try
    ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse,
      csClickEvents, csSetCaption, csDoubleClicks, csReplicatable{$IFDEF FPC},
      csNoFocus, csAutoSize0x0{$ENDIF}]
      + [csOpaque]; // we need the default background
    //Self.DoubleBuffered := True;
    with GetControlClassDefaultSize do
      SetInitialBounds(0, 0, CX, CY);

    FBGRA               := TBGRABitmapEx.Create;
    FBorderBCStyle      := bpsFrame3d;
    FBackground         := TBCBackground.Create(Self);
    FBorder             := TBCBorder.Create(Self);
    FFontEx             := TBCFont.Create(Self);
    FBevelOuter         := bvRaised;
    FBevelInner         := bvNone;
    FBevelWidth         := 1;
    ParentColor         := True;
    UseDockManager      := True;

    FBackground.OnChange := OnChangeProperty;
    FBorder.OnChange     := OnChangeProperty;
    FFontEx.OnChange     := OnChangeFont;

    FBackground.Style   := bbsColor;
    FBackground.Color   := {$ifdef UseCLDefault}clDefault{$else}clBtnFace{$endif};
    FBorder.Style       := bboNone;

    FRounding           := TBCRounding.Create(Self);
    FRounding.OnChange  := OnChangeProperty;
  finally
    {$IFDEF FPC}
    EnableAutoSizing;
    {$ENDIF}
    EndUpdate;
    {$IFDEF FPC}
    Exclude(FControlState, csCreating);
    {$ELSE} //#
    {$ENDIF}
  end;
end;

destructor TCustomBCPanel.Destroy;
begin
  FBackground.Free;
  FBorder.Free;
  FFontEx.Free;
  FBGRA.Free;
  FRounding.Free;
  inherited Destroy;
end;

procedure TCustomBCPanel.UpdateControl;
begin
  Render;
  inherited UpdateControl; // invalidate
end;
{$IFDEF FPC}
procedure TCustomBCPanel.SaveToFile(AFileName: string);
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

procedure TCustomBCPanel.LoadFromFile(AFileName: string);
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
{$ENDIF}

procedure TCustomBCPanel.OnFindClass(Reader: TReader; const AClassName: string;
  var ComponentClass: TComponentClass);
begin
  if CompareText(AClassName, 'TBCPanel') = 0 then
    ComponentClass := TBCPanel;
end;

end.
