{ Base framework classes

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

{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}

unit BCBaseCtrls;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Graphics,
  {$IFNDEF FPC}Windows, Messages, BGRAGraphics, GraphType, FPImage, {$ELSE} LCLType,{$ENDIF}
  BGRABitmap, BGRABitmapTypes;

type

{$IFNDEF FPC}
  TSpacingSize = Integer;
  TControlCellAlign = (
    ccaFill,
    ccaLeftTop,
    ccaRightBottom,
    ccaCenter
    );
  TControlCellAligns = set of TControlCellAlign;

  TControlBorderSpacingDefault = record
    Left: TSpacingSize;
    Top: TSpacingSize;
    Right: TSpacingSize;
    Bottom: TSpacingSize;
    Around: TSpacingSize;
  end;
  PControlBorderSpacingDefault = ^TControlBorderSpacingDefault;


  TBGRAGraphicCtrl = class;
  TBGRACustomCtrl  = class;
  TControlBorderSpacing = class;

  ILCLControl = interface
   ['{97A3D274-C4BD-4095-9B23-8E50D6E0EA24}']
    procedure DoOnResize;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;{%H-}WithThemeSpace: boolean);
    procedure MouseEnter;
    procedure MouseLeave;
    procedure TextChanged;
    procedure FontChanged(Sender: TObject);
    procedure RealSetText(const Value: TCaption);
    procedure Resize;
    procedure SetColor(Value: TColor);
    function GetColor : TColor;
    function ColorIsStored: boolean;
    function RealGetText: TCaption;
    function CreateControlBorderSpacing: TControlBorderSpacing;
    procedure SetBorderSpacing(const AValue: TControlBorderSpacing);
    procedure SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer);
    procedure InvalidatePreferredSize;
    procedure EnableAutoSizing;
    procedure DoBorderSpacingChange(Sender: TObject; InnerSpaceChanged: Boolean);
    function  GetInstance : TObject;
    function IsInnerBorderStored: boolean;
  end;

  TControlBorderSpacing = class(TPersistent)
  private
    FAround: TSpacingSize;
    FBottom: TSpacingSize;
    FCellAlignHorizontal: TControlCellAlign;
    FCellAlignVertical: TControlCellAlign;
    FControl: ILCLControl;
    FInnerBorder: Integer;
    FLeft: TSpacingSize;
    FOnChange: TNotifyEvent;
    FRight: TSpacingSize;
    FTop: TSpacingSize;
    FDefault: PControlBorderSpacingDefault;
    function GetAroundBottom: Integer;
    function GetAroundLeft: Integer;
    function GetAroundRight: Integer;
    function GetAroundTop: Integer;
    function GetControlBottom: Integer;
    function GetControlHeight: Integer;
    function GetControlLeft: Integer;
    function GetControlRight: Integer;
    function GetControlTop: Integer;
    function GetControlWidth: Integer;
    function IsAroundStored: boolean;
    function IsBottomStored: boolean;
    function IsInnerBorderStored: boolean;
    function IsLeftStored: boolean;
    function IsRightStored: boolean;
    function IsTopStored: boolean;
    procedure SetAround(const AValue: TSpacingSize);
    procedure SetBottom(const AValue: TSpacingSize);
    procedure SetCellAlignHorizontal(const AValue: TControlCellAlign);
    procedure SetCellAlignVertical(const AValue: TControlCellAlign);
    procedure SetInnerBorder(const AValue: Integer);
    procedure SetLeft(const AValue: TSpacingSize);
    procedure SetRight(const AValue: TSpacingSize);
    procedure SetSpace(Kind: TAnchorKind; const AValue: integer);
    procedure SetTop(const AValue: TSpacingSize);
  protected
    procedure Change(InnerSpaceChanged: Boolean); virtual;
  public
    constructor Create(OwnerControl: ILCLControl; ADefault: PControlBorderSpacingDefault = nil);
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    function IsEqual(Spacing: TControlBorderSpacing): boolean;
    procedure GetSpaceAround(var SpaceAround: TRect); virtual;
    function GetSideSpace(Kind: TAnchorKind): Integer; // Around+GetSpace
    function GetSpace(Kind: TAnchorKind): Integer; virtual;
    procedure AutoAdjustLayout(const AXProportion, AYProportion: Double);
  public
    property Control: ILCLControl read FControl;
    property Space[Kind: TAnchorKind]: integer read GetSpace write SetSpace;
    property AroundLeft: Integer read GetAroundLeft;
    property AroundTop: Integer read GetAroundTop;
    property AroundRight: Integer read GetAroundRight;
    property AroundBottom: Integer read GetAroundBottom;
    property ControlLeft: Integer read GetControlLeft;
    property ControlTop: Integer read GetControlTop;
    property ControlWidth: Integer read GetControlWidth;
    property ControlHeight: Integer read GetControlHeight;
    property ControlRight: Integer read GetControlRight;
    property ControlBottom: Integer read GetControlBottom;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Left: TSpacingSize read FLeft write SetLeft stored IsLeftStored;
    property Top: TSpacingSize read FTop write SetTop stored IsTopStored;
    property Right: TSpacingSize read FRight write SetRight stored IsRightStored;
    property Bottom: TSpacingSize read FBottom write SetBottom stored IsBottomStored;
    property Around: TSpacingSize read FAround write SetAround stored IsAroundStored;
    property InnerBorder: Integer read FInnerBorder write SetInnerBorder stored IsInnerBorderStored default 0;
    property CellAlignHorizontal: TControlCellAlign read FCellAlignHorizontal write SetCellAlignHorizontal default ccaFill;
    property CellAlignVertical: TControlCellAlign read FCellAlignVertical write SetCellAlignVertical default ccaFill;
  end;

  TChildControlResizeStyle = (
      crsAnchorAligning, // (like Delphi)
      crsScaleChilds, // scale children equally, keep space between children fixed
      crsHomogenousChildResize, // enlarge children equally (i.e. by the same amount of pixel)
      crsHomogenousSpaceResize // enlarge space between children equally
      {$IFDEF EnablecrsSameSize}
      ,crsSameSize  // each child gets the same size (maybe one pixel difference)
      {$ENDIF}
    );

  TControlChildrenLayout = (
      cclNone,
      cclLeftToRightThenTopToBottom, // if BiDiMode <> bdLeftToRight then it becomes RightToLeft
      cclTopToBottomThenLeftToRight
    );

  TControlChildSizing = class(TPersistent)
  private
    FControl: ILCLControl;
    FControlsPerLine: integer;
    FEnlargeHorizontal: TChildControlResizeStyle;
    FEnlargeVertical: TChildControlResizeStyle;
    FHorizontalSpacing: integer;
    FLayout: TControlChildrenLayout;
    FLeftRightSpacing: integer;
    FOnChange: TNotifyEvent;
    FShrinkHorizontal: TChildControlResizeStyle;
    FShrinkVertical: TChildControlResizeStyle;
    FTopBottomSpacing: integer;
    FVerticalSpacing: integer;
    procedure SetControlsPerLine(const AValue: integer);
    procedure SetEnlargeHorizontal(const AValue: TChildControlResizeStyle);
    procedure SetEnlargeVertical(const AValue: TChildControlResizeStyle);
    procedure SetHorizontalSpacing(const AValue: integer);
    procedure SetLayout(const AValue: TControlChildrenLayout);
    procedure SetLeftRightSpacing(const AValue: integer);
    procedure SetShrinkHorizontal(const AValue: TChildControlResizeStyle);
    procedure SetShrinkVertical(const AValue: TChildControlResizeStyle);
    procedure SetTopBottomSpacing(const AValue: integer);
    procedure SetVerticalSpacing(const AValue: integer);
  protected
    procedure Change; virtual;
  public
    constructor Create(OwnerControl: ILCLControl);
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    function IsEqual(Sizing: TControlChildSizing): boolean;
    procedure SetGridSpacing(Spacing: integer);
  public
    property Control: ILCLControl read FControl;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property LeftRightSpacing: integer read FLeftRightSpacing write SetLeftRightSpacing default 0;
    property TopBottomSpacing: integer read FTopBottomSpacing write SetTopBottomSpacing default 0;
    property HorizontalSpacing: integer read FHorizontalSpacing write SetHorizontalSpacing default 0;
    property VerticalSpacing: integer read FVerticalSpacing write SetVerticalSpacing default 0;
    property EnlargeHorizontal: TChildControlResizeStyle read FEnlargeHorizontal
                           write SetEnlargeHorizontal default crsAnchorAligning;
    property EnlargeVertical: TChildControlResizeStyle read FEnlargeVertical
                             write SetEnlargeVertical default crsAnchorAligning;
    property ShrinkHorizontal: TChildControlResizeStyle read FShrinkHorizontal
                            write SetShrinkHorizontal default crsAnchorAligning;
    property ShrinkVertical: TChildControlResizeStyle read FShrinkVertical
                              write SetShrinkVertical default crsAnchorAligning;
    property Layout: TControlChildrenLayout read FLayout write SetLayout default cclNone;
    property ControlsPerLine: integer read FControlsPerLine write SetControlsPerLine default 0;
  end;
{$ENDIF}

{$IFDEF FPC}
  TBGRAGraphicCtrl = class(TGraphicControl)
{$ELSE}
  TBGRAGraphicCtrl = class(TGraphicControl, ILCLControl)
  protected
    FBorderSpacing: TControlBorderSpacing;
    FOnChange: TNotifyEvent;
    FMouseInClient: boolean;
    procedure DoOnResize; virtual;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;{%H-}WithThemeSpace: boolean); virtual;
    procedure MouseEnter;  virtual;
    procedure MouseLeave;  virtual;
    procedure TextChanged; virtual;
    procedure FontChanged(Sender: TObject); virtual;
    procedure RealSetText(const Value: TCaption); virtual;
    procedure Resize; override;
    class function GetControlClassDefaultSize: TSize; virtual;
    procedure SetColor(Value: TColor); virtual;
    function GetColor : TColor;
    procedure CMMouseEnter(var Message :TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message :TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    function ColorIsStored: boolean; virtual;
    function RealGetText: TCaption; virtual;
    function CreateControlBorderSpacing: TControlBorderSpacing; virtual;
    procedure SetBorderSpacing(const AValue: TControlBorderSpacing);
    procedure DoBorderSpacingChange(Sender: TObject; InnerSpaceChanged: Boolean); virtual;
    function  GetInstance : TObject;
    function IsInnerBorderStored: boolean;
  public
    constructor Create(TheOwner: TComponent);override;
    destructor Destroy;override;
    procedure SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer); virtual;
    procedure InvalidatePreferredSize; virtual;
    procedure EnableAutoSizing;
    property Color: TColor read GetColor write SetColor stored ColorIsStored default clWindow;
    property BorderSpacing: TControlBorderSpacing read FBorderSpacing write SetBorderSpacing;
    property Caption;
    property Canvas;
{$ENDIF}
  end;

{$IFDEF FPC}
  TBGRACustomCtrl = class(TCustomControl)
{$ELSE}
  TBGRACustomCtrl = class(TCustomControl, ILCLControl)
  protected
    FBorderSpacing: TControlBorderSpacing;
    FChildSizing: TControlChildSizing;
    FOnChange: TNotifyEvent;
    FMouseInClient: boolean;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;{%H-}WithThemeSpace: boolean); {$IFDEF FPC}override;{$ELSE}virtual;{$ENDIF}
    procedure DoOnResize; virtual;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure TextChanged; virtual;
    procedure FontChanged(Sender: TObject); virtual;
    function GetDefaultDockCaption: String; virtual;
    procedure RealSetText(const Value: TCaption); virtual;
    procedure EnabledChanged; virtual;
    procedure Resize; override;
    class function GetControlClassDefaultSize: TSize; virtual;
    procedure SetColor(Value: TColor); virtual;
    function  GetColor : TColor;
    procedure UTF8KeyPress(var UTF8Key: {$IFDEF FPC}TUTF8Char{$ELSE}String{$ENDIF}); virtual;
    procedure CMMouseEnter(var Message :TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message :TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    function ColorIsStored: boolean; virtual;
    function RealGetText: TCaption; virtual;
    function CreateControlBorderSpacing: TControlBorderSpacing; virtual;
    procedure SetBorderSpacing(const AValue: TControlBorderSpacing);
    procedure DoBorderSpacingChange(Sender: TObject; InnerSpaceChanged: Boolean); virtual;
    function  GetInstance : TObject;
    function IsInnerBorderStored: boolean;
    procedure SetChildSizing(const AValue: TControlChildSizing);
    procedure DoChildSizingChange(Sender: TObject); virtual;
  public
    constructor Create(TheOwner: TComponent);override;
    destructor Destroy;override;
    procedure SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer); virtual;
    procedure InvalidatePreferredSize; virtual;
    procedure EnableAutoSizing;
    property Color: TColor read GetColor write SetColor stored ColorIsStored default clWindow;
    property BorderSpacing: TControlBorderSpacing read FBorderSpacing write SetBorderSpacing;
    property ChildSizing: TControlChildSizing read FChildSizing write SetChildSizing;
    property Caption;
    property Canvas;
{$ENDIF}
  end;


{$IFDEF FPC}
  TBGRACustomPanel = class(TCustomPanel)
{$ELSE}
  TBGRACustomPanel = class(TCustomPanel, ILCLControl)
  protected
    FBorderSpacing: TControlBorderSpacing;
    FChildSizing: TControlChildSizing;
    FOnChange: TNotifyEvent;
    FMouseInClient: boolean;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;{%H-}WithThemeSpace: boolean); {$IFDEF FPC}override;{$ELSE}virtual;{$ENDIF}
    procedure DoOnResize; virtual;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure TextChanged; virtual;
    procedure FontChanged(Sender: TObject); virtual;
    function GetDefaultDockCaption: String; virtual;
    procedure RealSetText(const Value: TCaption); virtual;
    procedure EnabledChanged; virtual;
    procedure Resize; override;
    class function GetControlClassDefaultSize: TSize; virtual;
    procedure SetColor(Value: TColor); virtual;
    function  GetColor : TColor;
    procedure UTF8KeyPress(var UTF8Key: {$IFDEF FPC}TUTF8Char{$ELSE}String{$ENDIF}); virtual;
    procedure CMMouseEnter(var Message :TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message :TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    function ColorIsStored: boolean; virtual;
    function RealGetText: TCaption; virtual;
    function CreateControlBorderSpacing: TControlBorderSpacing; virtual;
    procedure SetBorderSpacing(const AValue: TControlBorderSpacing);
    procedure DoBorderSpacingChange(Sender: TObject; InnerSpaceChanged: Boolean); virtual;
    function  GetInstance : TObject;
    function IsInnerBorderStored: boolean;
    procedure SetChildSizing(const AValue: TControlChildSizing);
    procedure DoChildSizingChange(Sender: TObject); virtual;
  public
    constructor Create(TheOwner: TComponent);override;
    destructor Destroy;override;
    procedure SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer); virtual;
    procedure InvalidatePreferredSize; virtual;
    procedure EnableAutoSizing;
    property Color: TColor read GetColor write SetColor stored ColorIsStored default clWindow;
    property BorderSpacing: TControlBorderSpacing read FBorderSpacing write SetBorderSpacing;
    property ChildSizing: TControlChildSizing read FChildSizing write SetChildSizing;
    property Canvas;
{$ENDIF}
  end;

  TOnBCPropertyChange = procedure(ASender: TObject; AData: PtrInt) of object;

  { TBCProperty
    Base BC Property with OnChange event support
  }

  TBCProperty = class(TPersistent)
  private
    FOnChange: TOnBCPropertyChange;
  protected
    FControl: TControl;
    procedure Change(AData: PtrInt = 0); virtual;
  public
    constructor Create(AControl: TControl); virtual;
  public
    property Control: TControl read FControl;
    property OnChange: TOnBCPropertyChange read FOnChange write FOnChange;
  end;

  { TBGRABitmapEx
    Some BGRABitmap descendant which can store custom data and has NeedRender flag
  }

  TBGRABitmapEx = class(TBGRABitmap)
  private
    FCustomData: PtrInt;
    FNeedRender: Boolean;
  protected
    procedure Init; override;
  public
    property NeedRender: Boolean read FNeedRender write FNeedRender;
    property CustomData: PtrInt read FCustomData write FCustomData;
    procedure Discard;
  end;

  { TBCGraphicControl
    BC graphic control with some basic functionality like begin/end update and
    debug functions
  }

  TBCGraphicControl = class(TBGRAGraphicCtrl)
  private
    {$IFDEF INDEBUG}
    FPaintCount: Integer;
    {$ENDIF}
    FUpdateCount: Integer;
  protected
    procedure DoOnResize; override;
  protected
    {$IFDEF INDEBUG}
    function GetDebugText: String; virtual;
    {$ENDIF}
    procedure Paint; override; // do not override in descendants!
    // All descendants should use DrawControl method instead of Paint.
    // DrawControl is not called between BeginUpdate and EndUpdate
    procedure DrawControl; virtual;
    // This method is called when control should be rendered (when some
    // general action occur which change "body" e.g. resize)
    procedure RenderControl; virtual;
  public
    {$IFDEF FPC}
    { Save all published settings to file }
    procedure SaveToFile(AFileName: string); virtual; abstract;
    { Load and assign all published settings from file }
    procedure LoadFromFile(AFileName: string); virtual; abstract;
    { Assign the properties from AFileName to this instance }
    procedure AssignFromFile(AFileName: string); virtual; abstract;
    { Used by SaveToFile/LoadFromFile }
    {$ENDIF}
    constructor Create(AOwner: TComponent); override;
    // This disable DrawControl method
    procedure BeginUpdate; virtual;
    // This enable DrawControl method
    procedure EndUpdate; virtual;
    // Called on EndUpdate if FUpdateCount is 0
    procedure UpdateControl; virtual;
    // Check if BeginUpdate was called
    function IsUpdating: Boolean;
  end;

  { TBCStyleDummyProperty
    This is only dummy property type for access to style editor from
    object inspector
  }

  TBCStyleDummyProperty = class(TBCProperty)

  end;

  { TBCStyleGraphicControl
    All descendants of this class have support for saving and loading styles and
    access to style editor from object inspector or component context menu
  }

  TBCStyleGraphicControl = class(TBCGraphicControl)
  private
    FAssignStyle: TBCStyleDummyProperty;
  protected
    function GetStyleExtension: String; virtual; abstract;
    // Dummy property for access to style editor dialog
    property AssignStyle: TBCStyleDummyProperty read FAssignStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property StyleExtension: String read GetStyleExtension;
  end;

  { TBCCustomControl
    BC custom control with some basic functionality like begin/end update and
    debug functions
  }

  TBCCustomControl = class(TBGRACustomCtrl)
  private
    {$IFDEF INDEBUG}
    FPaintCount: Integer;
    {$ENDIF}
    FUpdateCount: Integer;
  protected
    procedure DoOnResize; override;
  protected
    {$IFDEF INDEBUG}
    function GetDebugText: String; virtual;
    {$ENDIF}
    procedure Paint; override; // do not override in descendants!
    // All descendants should use DrawControl method instead of Paint.
    // DrawControl is not called between BeginUpdate and EndUpdate
    procedure DrawControl; virtual;
    // This method is called when control should be rendered (when some
    // general action occur which change "body" e.g. resize)
    procedure RenderControl; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    // This disable DrawControl method
    procedure BeginUpdate; virtual;
    // This enable DrawControl method
    procedure EndUpdate; virtual;
    // Called on EndUpdate if FUpdateCount is 0
    procedure UpdateControl; virtual;
    // Check if BeginUpdate was called
    function IsUpdating: Boolean;
  end;

  { TBCStyleCustomControl
    All descendants of this class have support for saving and loading styles and
    access to style editor from object inspector or component context menu
  }

  TBCStyleCustomControl = class(TBCCustomControl)
  private
    FAssignStyle: TBCStyleDummyProperty;
  protected
    function GetStyleExtension: String; virtual; abstract;
    // Dummy property for access to style editor dialog
    property AssignStyle: TBCStyleDummyProperty read FAssignStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property StyleExtension: String read GetStyleExtension;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

//{$IFDEF INDEBUG} uses Graphics; {$ENDIF}

{$IFDEF FPC}
procedure Register;
begin
  RegisterNoIcon([TBCCustomControl]);
end;
{$ENDIF}

{ TBCStyleCustomControl }

constructor TBCStyleCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAssignStyle := TBCStyleDummyProperty.Create(Self);
end;

destructor TBCStyleCustomControl.Destroy;
begin
  FAssignStyle.Free;
  inherited Destroy;
end;

{ TBCStyleGraphicControl }

constructor TBCStyleGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAssignStyle := TBCStyleDummyProperty.Create(Self);
end;

destructor TBCStyleGraphicControl.Destroy;
begin
  FAssignStyle.Free;
  inherited Destroy;
end;

{ TBCCustomControl }

procedure TBCCustomControl.DoOnResize;
begin
  inherited DoOnResize;
  RenderControl;
end;

{$IFDEF INDEBUG}
function TBCCustomControl.GetDebugText: String;
begin
  Result := EmptyStr;
end;
{$ENDIF}

procedure TBCCustomControl.Paint;
begin
  if (csCreating in ControlState) or IsUpdating then
    Exit;

  DrawControl;
  {$IFDEF INDEBUG}
  FPaintCount := FPaintCount +1;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clBlack;
  Canvas.Font.Color := clWhite;
  Canvas.TextOut(1,1,'P: '+IntToStr(FPaintCount)+' '+GetDebugText);
  {$ENDIF}
  inherited Paint;
end;

procedure TBCCustomControl.DrawControl;
begin

end;

procedure TBCCustomControl.RenderControl;
begin

end;

constructor TBCCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF INDEBUG}
  FPaintCount := 0;
  {$ENDIF}
end;

procedure TBCCustomControl.BeginUpdate;
begin
  FUpdateCount := FUpdateCount +1;
end;

procedure TBCCustomControl.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    FUpdateCount := FUpdateCount -1;
    if FUpdateCount=0 then
      UpdateControl;
  end;
end;

procedure TBCCustomControl.UpdateControl;
begin
  Self.Invalidate;
end;

function TBCCustomControl.IsUpdating: Boolean;
begin
  Result := FUpdateCount>0;
end;

{ TBCProperty }

procedure TBCProperty.Change(AData: PtrInt);
begin
  if Assigned(FOnChange) then
    FOnChange(Self,AData);
end;

constructor TBCProperty.Create(AControl: TControl);
begin
  FControl := AControl;

  inherited Create;
end;

{ TBCGraphicControl }

procedure TBCGraphicControl.DoOnResize;
begin
  inherited DoOnResize;
  RenderControl;
end;

{$IFDEF INDEBUG}
function TBCGraphicControl.GetDebugText: String;
begin
  Result := EmptyStr;
end;
{$ENDIF}

procedure TBCGraphicControl.Paint;
begin
  //inherited Paint;
  if (csCreating in ControlState) or IsUpdating then
    Exit;
  DrawControl;
  {$IFDEF INDEBUG}
  FPaintCount := FPaintCount +1;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clBlack;
  Canvas.Font.Color := clWhite;
  Canvas.TextOut(1,1,'P: '+IntToStr(FPaintCount)+' '+GetDebugText);
  {$ENDIF}
end;

procedure TBCGraphicControl.DrawControl;
begin

end;

procedure TBCGraphicControl.RenderControl;
begin

end;

constructor TBCGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF INDEBUG}
  FPaintCount := 0;
  {$ENDIF}
end;

procedure TBCGraphicControl.BeginUpdate;
begin
  FUpdateCount := FUpdateCount +1;
end;

procedure TBCGraphicControl.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    FUpdateCount := FUpdateCount -1;
    if FUpdateCount=0 then
      UpdateControl;
  end;
end;

procedure TBCGraphicControl.UpdateControl;
begin
  Invalidate;
end;

function TBCGraphicControl.IsUpdating: Boolean;
begin
  Result := FUpdateCount>0;
end;

{ TBGRABitmapEx }

procedure TBGRABitmapEx.Init;
begin
  inherited Init;
  FNeedRender := True;
  FCustomData := 0;
end;

procedure TBGRABitmapEx.Discard;
begin
  FNeedRender := true;
  SetSize(0,0);
end;

{$IFNDEF FPC}
constructor TBGRAGraphicCtrl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBorderSpacing := CreateControlBorderSpacing;
  FOnChange := Font.OnChange;
  Font.OnChange := FontChanged;
end;

function TBGRAGraphicCtrl.CreateControlBorderSpacing: TControlBorderSpacing;
begin
  Result := TControlBorderSpacing.Create(Self);
end;

destructor TBGRAGraphicCtrl.Destroy;
begin
  FreeAndNil(FBorderSpacing);
  inherited;
end;

procedure TBGRAGraphicCtrl.DoBorderSpacingChange(Sender: TObject; InnerSpaceChanged: Boolean);
var
  IParent : ILCLControl;
begin
  if Parent <> nil then
    if Supports(Parent, ILCLControl, IParent) then
       IParent.InvalidatePreferredSize;
  AdjustSize;
end;

procedure TBGRAGraphicCtrl.DoOnResize;
begin
  if Assigned(OnResize) then
    OnResize(Self);
end;

procedure TBGRAGraphicCtrl.SetBorderSpacing(const AValue: TControlBorderSpacing);
begin
  if FBorderSpacing=AValue then exit;
  FBorderSpacing.Assign(AValue);
end;

procedure TBGRAGraphicCtrl.SetColor(Value: TColor);
begin
  if inherited Color <> Value then
  begin
    inherited Color := Value;
    ParentColor := False;
    Perform(CM_COLORCHANGED, 0, 0);
    Invalidate;
  end;
end;

function TBGRAGraphicCtrl.GetColor: TColor;
begin
   Result := inherited Color;
end;

procedure TBGRAGraphicCtrl.EnableAutoSizing;
begin
end;

class function TBGRAGraphicCtrl.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 75;
  Result.CY := 50;
end;

function TBGRAGraphicCtrl.GetInstance: TObject;
begin
  result := Self;
end;

procedure TBGRAGraphicCtrl.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;{%H-}WithThemeSpace: boolean);
begin
  PreferredWidth:=0;
  PreferredHeight:=0;
end;

procedure TBGRAGraphicCtrl.MouseEnter;
begin
  if Assigned(OnMouseEnter) then
    OnMouseEnter(Self);
end;

procedure TBGRAGraphicCtrl.MouseLeave;
begin
  if Assigned(OnMouseLeave) then
    OnMouseLeave(Self);
end;

procedure TBGRAGraphicCtrl.TextChanged;
begin
  // overrided;
end;

procedure TBGRAGraphicCtrl.FontChanged(Sender: TObject);
begin
  ParentFont := False;
  DesktopFont := False;
  Invalidate;
  Perform(CM_FONTCHANGED, 0, 0);
  if AutoSize then
  begin
    InvalidatePreferredSize;
    AdjustSize;
  end;
  if Assigned(FOnChange) then
    FOnChange(Self);  
end;

function TBGRAGraphicCtrl.RealGetText: TCaption;
begin
  Result := Caption;
end;

procedure TBGRAGraphicCtrl.RealSetText(const Value: TCaption);
begin
  if RealGetText = Value then Exit;
  Caption := Value;
  Perform(CM_TEXTCHANGED, 0, 0);
end;

procedure TBGRAGraphicCtrl.InvalidatePreferredSize;
begin
//  Invalidate;
end;

function TBGRAGraphicCtrl.IsInnerBorderStored: boolean;
begin
  Result:=BorderSpacing.InnerBorder<>0;
end;

procedure TBGRAGraphicCtrl.Resize;
begin
  inherited;
  Invalidate;
  DoOnResize;
end;

function TBGRAGraphicCtrl.ColorIsStored: boolean;
begin
  Result := not ParentColor;
end;

procedure TBGRAGraphicCtrl.SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
  {if (csLoading in ComponentState)
  or ((Owner<>nil) and (csLoading in Owner.ComponentState)) then
    exit;}
  SetBounds(aLeft,aTop,aWidth,aHeight);
//  DoOnResize;
end;

procedure TBGRAGraphicCtrl.CMTextChanged(var Message: TMessage);
begin
  inherited;
  TextChanged;
end;

procedure TBGRAGraphicCtrl.CMMouseEnter(var Message: TMessage);
begin
  if FMouseInClient then
    Exit;

  FMouseInClient := True;

  // broadcast to parents first
  if Assigned(Parent) then
    Parent.Perform(CM_MOUSEENTER, 0, LParam(Self));

  // if it is not a child message then perform an event
  if (Message.LParam = 0) then
    MouseEnter;
end;

procedure TBGRAGraphicCtrl.CMMouseLeave(var Message: TMessage);
begin
  if not FMouseInClient then
    Exit;

  FMouseInClient := False;

  // broadcast to parents first
  if Assigned(Parent) then
    Parent.Perform(CM_MOUSELEAVE, 0, LParam(Self));

  // if it is not a child message then perform an event
  if (Message.LParam = 0) then
    MouseLeave;
end;

{TBGRACustomCtrl}

constructor TBGRACustomCtrl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBorderSpacing := CreateControlBorderSpacing;
  FChildSizing:=TControlChildSizing.Create(Self);
  FChildSizing.OnChange := DoChildSizingChange;
  FOnChange := Font.OnChange;
  Font.OnChange := FontChanged;
end;

function TBGRACustomCtrl.CreateControlBorderSpacing: TControlBorderSpacing;
begin
  Result := TControlBorderSpacing.Create(Self);
end;

destructor TBGRACustomCtrl.Destroy;
begin
  FreeAndNil(FBorderSpacing);
  FreeAndNil(FChildSizing);
  inherited;
end;

procedure TBGRACustomCtrl.DoBorderSpacingChange(Sender: TObject; InnerSpaceChanged: Boolean);
var
  IParent : ILCLControl;
begin
  if Parent <> nil then
    if Supports(Parent, ILCLControl, IParent) then
       IParent.InvalidatePreferredSize;
  AdjustSize;
end;

procedure TBGRACustomCtrl.DoChildSizingChange(Sender: TObject);
begin
  //debugln('TWinControl.DoChildSizingChange ',DbgSName(Self));
  if ControlCount=0 then exit;
  InvalidatePreferredSize;
  ReAlign;
end;

procedure TBGRACustomCtrl.DoOnResize;
begin
  if Assigned(OnResize) then
    OnResize(Self);
end;

procedure TBGRACustomCtrl.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;{%H-}WithThemeSpace: boolean);
begin
  PreferredWidth:=0;
  PreferredHeight:=0;
end;

procedure TBGRACustomCtrl.MouseEnter;
begin
  if Assigned(OnMouseEnter) then
    OnMouseEnter(Self);
end;

procedure TBGRACustomCtrl.MouseLeave;
begin
  if Assigned(OnMouseLeave) then
    OnMouseLeave(Self);
end;

procedure TBGRACustomCtrl.TextChanged;
begin
  // overrided;
end;

procedure TBGRACustomCtrl.FontChanged(Sender: TObject);
begin
  ParentFont := False;
  DesktopFont := False;
  Invalidate;
  Perform(CM_FONTCHANGED, 0, 0);
  if AutoSize then
  begin
    InvalidatePreferredSize;
    AdjustSize;
  end;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TBGRACustomCtrl.GetDefaultDockCaption: String;
begin
end;

function TBGRACustomCtrl.GetInstance: TObject;
begin
  result := Self;
end;

function TBGRACustomCtrl.RealGetText: TCaption;
begin
  Result := Caption;
end;

procedure TBGRACustomCtrl.RealSetText(const Value: TCaption);
begin
  if RealGetText = Value then Exit;
  Caption := Value;
  Perform(CM_TEXTCHANGED, 0, 0);
end;

procedure TBGRACustomCtrl.EnabledChanged;
begin
end;

procedure TBGRACustomCtrl.InvalidatePreferredSize;
begin
//  Invalidate;
end;

function TBGRACustomCtrl.IsInnerBorderStored: boolean;
begin
  Result:=BorderSpacing.InnerBorder<>0;
end;

procedure TBGRACustomCtrl.Resize;
begin
  inherited;
  Invalidate;
  DoOnResize;
end;

procedure TBGRACustomCtrl.SetBorderSpacing(const AValue: TControlBorderSpacing);
begin
  if FBorderSpacing=AValue then exit;
  FBorderSpacing.Assign(AValue);
end;

procedure TBGRACustomCtrl.SetChildSizing(const AValue: TControlChildSizing);
begin
  if (FChildSizing=AValue) then exit;
  FChildSizing.Assign(AValue);
end;

procedure TBGRACustomCtrl.SetColor(Value: TColor);
begin
  if inherited Color <> Value then
  begin
    inherited Color := Value;
    ParentColor := False;
    Perform(CM_COLORCHANGED, 0, 0);
    Invalidate;
  end;
end;

function TBGRACustomCtrl.GetColor : TColor;
begin
  Result := inherited Color;
end;

procedure TBGRACustomCtrl.UTF8KeyPress(var UTF8Key: {$IFDEF FPC}TUTF8Char{$ELSE}String{$ENDIF});
begin
end;

procedure TBGRACustomCtrl.EnableAutoSizing;
begin
end;

function TBGRACustomCtrl.ColorIsStored: boolean;
begin
  Result := not ParentColor;
end;

procedure TBGRACustomCtrl.SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
  {if (csLoading in ComponentState)
  or ((Owner<>nil) and (csLoading in Owner.ComponentState)) then
    exit;}
  SetBounds(aLeft,aTop,aWidth,aHeight);
//  DoOnResize;
end;

class function TBGRACustomCtrl.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 75;
  Result.CY := 50;
end;

procedure TBGRACustomCtrl.CMTextChanged(var Message: TMessage);
begin
  TextChanged;
end;

procedure TBGRACustomCtrl.CMMouseEnter(var Message: TMessage);
begin
  if FMouseInClient then
    Exit;

  FMouseInClient := True;

  // broadcast to parents first
  if Assigned(Parent) then
    Parent.Perform(CM_MOUSEENTER, 0, LParam(Self));

  // if it is not a child message then perform an event
  if (Message.LParam = 0) then
    MouseEnter;
end;

procedure TBGRACustomCtrl.CMMouseLeave(var Message: TMessage);
begin
  if not FMouseInClient then
    Exit;

  FMouseInClient := False;

  // broadcast to parents first
  if Assigned(Parent) then
    Parent.Perform(CM_MOUSELEAVE, 0, LParam(Self));

  // if it is not a child message then perform an event
  if (Message.LParam = 0) then
    MouseLeave;
end;

{TBGRACustomPanel}

constructor TBGRACustomPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBorderSpacing := CreateControlBorderSpacing;
  FChildSizing:=TControlChildSizing.Create(Self);
  FChildSizing.OnChange := DoChildSizingChange;
  FOnChange := Font.OnChange;
  Font.OnChange := FontChanged;
end;

function TBGRACustomPanel.CreateControlBorderSpacing: TControlBorderSpacing;
begin
  Result := TControlBorderSpacing.Create(Self);
end;

destructor TBGRACustomPanel.Destroy;
begin
  FreeAndNil(FBorderSpacing);
  FreeAndNil(FChildSizing);
  inherited;
end;

procedure TBGRACustomPanel.DoBorderSpacingChange(Sender: TObject; InnerSpaceChanged: Boolean);
var
  IParent : ILCLControl;
begin
  if Parent <> nil then
    if Supports(Parent, ILCLControl, IParent) then
       IParent.InvalidatePreferredSize;
  AdjustSize;
end;

procedure TBGRACustomPanel.DoChildSizingChange(Sender: TObject);
begin
  //debugln('TWinControl.DoChildSizingChange ',DbgSName(Self));
  if ControlCount=0 then exit;
  InvalidatePreferredSize;
  ReAlign;
end;

procedure TBGRACustomPanel.DoOnResize;
begin
  if Assigned(OnResize) then
    OnResize(Self);
end;

procedure TBGRACustomPanel.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;{%H-}WithThemeSpace: boolean);
begin
  PreferredWidth:=0;
  PreferredHeight:=0;
end;

procedure TBGRACustomPanel.MouseEnter;
begin
  if Assigned(OnMouseEnter) then
    OnMouseEnter(Self);
end;

procedure TBGRACustomPanel.MouseLeave;
begin
  if Assigned(OnMouseLeave) then
    OnMouseLeave(Self);
end;

procedure TBGRACustomPanel.TextChanged;
begin
  // overrided;
end;

procedure TBGRACustomPanel.FontChanged(Sender: TObject);
begin
  ParentFont := False;
  DesktopFont := False;
  Invalidate;
  Perform(CM_FONTCHANGED, 0, 0);
  if AutoSize then
  begin
    InvalidatePreferredSize;
    AdjustSize;
  end;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TBGRACustomPanel.GetDefaultDockCaption: String;
begin
end;

function TBGRACustomPanel.GetInstance: TObject;
begin
  result := Self;
end;

function TBGRACustomPanel.RealGetText: TCaption;
begin
  Result := Caption;
end;

procedure TBGRACustomPanel.RealSetText(const Value: TCaption);
begin
  if RealGetText = Value then Exit;
  Caption := Value;
  Perform(CM_TEXTCHANGED, 0, 0);
end;

procedure TBGRACustomPanel.EnabledChanged;
begin
end;

procedure TBGRACustomPanel.InvalidatePreferredSize;
begin
//  Invalidate;
end;

function TBGRACustomPanel.IsInnerBorderStored: boolean;
begin
  Result:=BorderSpacing.InnerBorder<>0;
end;

procedure TBGRACustomPanel.Resize;
begin
  inherited;
  Invalidate;
  DoOnResize;
end;

procedure TBGRACustomPanel.SetBorderSpacing(const AValue: TControlBorderSpacing);
begin
  if FBorderSpacing=AValue then exit;
  FBorderSpacing.Assign(AValue);
end;

procedure TBGRACustomPanel.SetChildSizing(const AValue: TControlChildSizing);
begin
  if (FChildSizing=AValue) then exit;
  FChildSizing.Assign(AValue);
end;

procedure TBGRACustomPanel.SetColor(Value: TColor);
begin
  if inherited Color <> Value then
  begin
    inherited Color := Value;
    ParentColor := False;
    Perform(CM_COLORCHANGED, 0, 0);
    Invalidate;
  end;
end;

function TBGRACustomPanel.GetColor : TColor;
begin
  Result := inherited Color;
end;

procedure TBGRACustomPanel.UTF8KeyPress(var UTF8Key: {$IFDEF FPC}TUTF8Char{$ELSE}String{$ENDIF});
begin
end;

procedure TBGRACustomPanel.EnableAutoSizing;
begin
end;

function TBGRACustomPanel.ColorIsStored: boolean;
begin
  Result := not ParentColor;
end;

procedure TBGRACustomPanel.SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
  {if (csLoading in ComponentState)
  or ((Owner<>nil) and (csLoading in Owner.ComponentState)) then
    exit;}
  SetBounds(aLeft,aTop,aWidth,aHeight);
//  DoOnResize;
end;

class function TBGRACustomPanel.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 75;
  Result.CY := 50;
end;

procedure TBGRACustomPanel.CMTextChanged(var Message: TMessage);
begin
  TextChanged;
end;

procedure TBGRACustomPanel.CMMouseEnter(var Message: TMessage);
begin
  if FMouseInClient then
    Exit;

  FMouseInClient := True;

  // broadcast to parents first
  if Assigned(Parent) then
    Parent.Perform(CM_MOUSEENTER, 0, LParam(Self));

  // if it is not a child message then perform an event
  if (Message.LParam = 0) then
    MouseEnter;
end;

procedure TBGRACustomPanel.CMMouseLeave(var Message: TMessage);
begin
  if not FMouseInClient then
    Exit;

  FMouseInClient := False;

  // broadcast to parents first
  if Assigned(Parent) then
    Parent.Perform(CM_MOUSELEAVE, 0, LParam(Self));

  // if it is not a child message then perform an event
  if (Message.LParam = 0) then
    MouseLeave;
end;

{$ENDIF}

{$IFNDEF FPC}

{ TControlBorderSpacing }

procedure TControlBorderSpacing.SetAround(const AValue: TSpacingSize);
begin
  if FAround=AValue then exit;
  FAround:=AValue;
  Change(false);
end;

function TControlBorderSpacing.IsAroundStored: boolean;
begin
  if FDefault = nil
  then Result := FAround <> 0
  else Result := FAround <> FDefault^.Around;
end;

function TControlBorderSpacing.IsBottomStored: boolean;
begin
  if FDefault = nil
  then Result := FBottom <> 0
  else Result := FBottom <> FDefault^.Bottom;
end;

function TControlBorderSpacing.IsInnerBorderStored: boolean;
begin
  if Control <> nil then
    Result:=Control.IsInnerBorderStored
  else
    Result:=True;
end;

function TControlBorderSpacing.IsLeftStored: boolean;
begin
  if FDefault = nil
  then Result := FLeft <> 0
  else Result := FLeft <> FDefault^.Left;
end;

function TControlBorderSpacing.IsRightStored: boolean;
begin
  if FDefault = nil
  then Result := FRight <> 0
  else Result := FRight <> FDefault^.Right;
end;

function TControlBorderSpacing.IsTopStored: boolean;
begin
  if FDefault = nil
  then Result := FTop <> 0
  else Result := FTop <> FDefault^.Top;
end;

procedure TControlBorderSpacing.SetBottom(const AValue: TSpacingSize);
begin
  if FBottom=AValue then exit;
  FBottom:=AValue;
  Change(false);
end;

procedure TControlBorderSpacing.SetCellAlignHorizontal(
  const AValue: TControlCellAlign);
begin
  if FCellAlignHorizontal=AValue then exit;
  FCellAlignHorizontal:=AValue;
  Change(false);
end;

procedure TControlBorderSpacing.SetCellAlignVertical(
  const AValue: TControlCellAlign);
begin
  if FCellAlignVertical=AValue then exit;
  FCellAlignVertical:=AValue;
  Change(false);
end;

procedure TControlBorderSpacing.SetInnerBorder(const AValue: Integer);
begin
  if FInnerBorder=AValue then exit;
  FInnerBorder:=AValue;
  if Control<>nil then Control.InvalidatePreferredSize;
  Change(true);
end;

procedure TControlBorderSpacing.SetLeft(const AValue: TSpacingSize);
begin
  if FLeft=AValue then exit;
  FLeft:=AValue;
  Change(false);
end;

procedure TControlBorderSpacing.SetRight(const AValue: TSpacingSize);
begin
  if FRight=AValue then exit;
  FRight:=AValue;
  Change(false);
end;

procedure TControlBorderSpacing.SetSpace(Kind: TAnchorKind;
  const AValue: integer);
begin
  case Kind of
  akLeft: Left:=AValue;
  akTop: Top:=AValue;
  akBottom: Bottom:=AValue;
  akRight: Right:=AValue;
  end;
end;

procedure TControlBorderSpacing.SetTop(const AValue: TSpacingSize);
begin
  if FTop=AValue then exit;
  FTop:=AValue;
  Change(false);
end;

constructor TControlBorderSpacing.Create(OwnerControl: ILCLControl; ADefault: PControlBorderSpacingDefault);
begin
  FControl := OwnerControl;
  FDefault := ADefault;
  if ADefault <> nil then
  begin
    FLeft := ADefault^.Left;
    FRight := ADefault^.Right;
    FTop := ADefault^.Top;
    FBottom := ADefault^.Bottom;
    FAround := ADefault^.Around;
  end;
  FCellAlignHorizontal := ccaFill;
  FCellAlignVertical := ccaFill;
  inherited Create;
end;

procedure TControlBorderSpacing.Assign(Source: TPersistent);
var
  SrcSpacing: TControlBorderSpacing;
begin
  if Source is TControlBorderSpacing then begin
    SrcSpacing:=TControlBorderSpacing(Source);
    if IsEqual(SrcSpacing) then exit;

    FAround:=SrcSpacing.Around;
    FBottom:=SrcSpacing.Bottom;
    FLeft:=SrcSpacing.Left;
    FRight:=SrcSpacing.Right;
    FTop:=SrcSpacing.Top;
    FInnerBorder:=SrcSpacing.InnerBorder;
    FCellAlignHorizontal:=SrcSpacing.CellAlignHorizontal;
    FCellAlignVertical:=SrcSpacing.CellAlignVertical;

    Change(false);
  end else
    inherited Assign(Source);
end;

procedure TControlBorderSpacing.AssignTo(Dest: TPersistent);
begin
  Dest.Assign(Self);
end;

procedure TControlBorderSpacing.AutoAdjustLayout(const AXProportion,
  AYProportion: Double);

  procedure Scale(var Value: Integer; const Proportion: Double; var Changed: Boolean);
  begin
    if Value<>0 then
    begin
      Value := Round(Value * Proportion);
      Changed := True;
    end;
  end;
var
  InnerChanged, OuterChanged: Boolean;
begin
  InnerChanged := False;
  OuterChanged := False;

  Scale(FAround, AXProportion, OuterChanged);
  Scale(FInnerBorder, AXProportion, InnerChanged);
  Scale(FLeft, AXProportion, OuterChanged);
  Scale(FTop, AYProportion, OuterChanged);
  Scale(FRight, AXProportion, OuterChanged);
  Scale(FBottom, AYProportion, OuterChanged);

  if OuterChanged or InnerChanged then
  begin
    if Control<>nil then Control.InvalidatePreferredSize;
    Change(InnerChanged);
  end;
end;

function TControlBorderSpacing.IsEqual(Spacing: TControlBorderSpacing
  ): boolean;
begin
  Result:=(FAround=Spacing.Around)
      and (FBottom=Spacing.Bottom)
      and (FLeft=Spacing.Left)
      and (FRight=Spacing.Right)
      and (FTop=Spacing.Top);
end;

procedure TControlBorderSpacing.GetSpaceAround(var SpaceAround: TRect);
begin
  SpaceAround.Left:=Left+Around;
  SpaceAround.Top:=Top+Around;
  SpaceAround.Right:=Right+Around;
  SpaceAround.Bottom:=Bottom+Around;
end;


function TControlBorderSpacing.GetSideSpace(Kind: TAnchorKind): Integer;
begin
  Result:=Around+GetSpace(Kind);
end;

function TControlBorderSpacing.GetSpace(Kind: TAnchorKind): Integer;
begin
  case Kind of
  akLeft: Result:=Left;
  akTop: Result:=Top;
  akRight: Result:=Right;
  akBottom: Result:=Bottom;
  end;
end;

procedure TControlBorderSpacing.Change(InnerSpaceChanged: Boolean);
begin
  if FControl <> nil then
    FControl.DoBorderSpacingChange(Self,InnerSpaceChanged);
  if Assigned(OnChange) then OnChange(Self);
end;

function TControlBorderSpacing.GetAroundBottom: Integer;
begin
  Result := Around+Bottom;
end;

function TControlBorderSpacing.GetAroundLeft: Integer;
begin
  Result := Around+Left;
end;

function TControlBorderSpacing.GetAroundRight: Integer;
begin
  Result := Around+Right;
end;

function TControlBorderSpacing.GetAroundTop: Integer;
begin
  Result := Around+Top;
end;

function TControlBorderSpacing.GetControlBottom: Integer;
begin
  if  (FControl<>nil) and (FControl.GetInstance<>nil) and FControl.GetInstance.InheritsFrom(TControl) then
    Result := TControl(FControl.GetInstance).Top +TControl(FControl.GetInstance).Height +Around+Bottom
  else
    Result := 0;
end;

function TControlBorderSpacing.GetControlHeight: Integer;
begin
  if (FControl<>nil) and (FControl.GetInstance<>nil) and FControl.GetInstance.InheritsFrom(TControl) then
    Result := TControl(FControl.GetInstance).Height+Around*2+Top+Bottom
  else
    Result := 0;
end;

function TControlBorderSpacing.GetControlLeft: Integer;
begin
  if  (FControl<>nil) and (FControl.GetInstance<>nil) and FControl.GetInstance.InheritsFrom(TControl) then
    Result := TControl(FControl.GetInstance).Left-Around-Left
  else
    Result := 0;
end;

function TControlBorderSpacing.GetControlRight: Integer;
begin
  if  (FControl<>nil) and (FControl.GetInstance<>nil) and FControl.GetInstance.InheritsFrom(TControl) then
    Result := TControl(FControl.GetInstance).Left+TControl(FControl.GetInstance).Width+Around+Right
  else
    Result := 0;
end;

function TControlBorderSpacing.GetControlTop: Integer;
begin
  if (FControl<>nil) and (FControl.GetInstance<>nil) and FControl.GetInstance.InheritsFrom(TControl) then
    Result := TControl(FControl.GetInstance).Top-Around-Top
  else
    Result := 0;
end;

function TControlBorderSpacing.GetControlWidth: Integer;
begin
  if (FControl<>nil) and (FControl.GetInstance<>nil) and FControl.GetInstance.InheritsFrom(TControl) then
    Result := TControl(FControl.GetInstance).Width+Around*2+Left+Right
  else
    Result := 0;
end;

{ TControlChildSizing }

procedure TControlChildSizing.SetEnlargeHorizontal(
  const AValue: TChildControlResizeStyle);
begin
  if FEnlargeHorizontal=AValue then exit;
  FEnlargeHorizontal:=AValue;
  Change;
end;

procedure TControlChildSizing.SetControlsPerLine(const AValue: integer);
begin
  if FControlsPerLine=AValue then exit;
  FControlsPerLine:=AValue;
  Change;
end;

procedure TControlChildSizing.SetEnlargeVertical(
  const AValue: TChildControlResizeStyle);
begin
  if FEnlargeVertical=AValue then exit;
  FEnlargeVertical:=AValue;
  Change;
end;

procedure TControlChildSizing.SetHorizontalSpacing(const AValue: integer);
begin
  if FHorizontalSpacing=AValue then exit;
  FHorizontalSpacing:=AValue;
  Change;
end;

procedure TControlChildSizing.SetLayout(const AValue: TControlChildrenLayout);
begin
  if FLayout=AValue then exit;
  FLayout:=AValue;
  //debugln('TControlChildSizing.SetLayout ',DbgSName(Control));
  Change;
end;

procedure TControlChildSizing.SetLeftRightSpacing(const AValue: integer);
begin
  if FLeftRightSpacing=AValue then exit;
  FLeftRightSpacing:=AValue;
  Change;
end;

procedure TControlChildSizing.SetShrinkHorizontal(
  const AValue: TChildControlResizeStyle);
begin
  if FShrinkHorizontal=AValue then exit;
  FShrinkHorizontal:=AValue;
  Change;
end;

procedure TControlChildSizing.SetShrinkVertical(
  const AValue: TChildControlResizeStyle);
begin
  if FShrinkVertical=AValue then exit;
  FShrinkVertical:=AValue;
  Change;
end;

procedure TControlChildSizing.SetTopBottomSpacing(const AValue: integer);
begin
  if FTopBottomSpacing=AValue then exit;
  FTopBottomSpacing:=AValue;
  Change;
end;

procedure TControlChildSizing.SetVerticalSpacing(const AValue: integer);
begin
  if FVerticalSpacing=AValue then exit;
  FVerticalSpacing:=AValue;
  Change;
end;

constructor TControlChildSizing.Create(OwnerControl: ILCLControl);
begin
  inherited Create;
  FControl := OwnerControl;
  FLayout := cclNone;
  FEnlargeHorizontal :=crsAnchorAligning;
  FEnlargeVertical := crsAnchorAligning;
  FShrinkHorizontal := crsAnchorAligning;
  FShrinkVertical := crsAnchorAligning;
end;

procedure TControlChildSizing.Assign(Source: TPersistent);
var
  SrcSizing: TControlChildSizing;
begin
  if Source is TControlChildSizing then begin
    SrcSizing:=TControlChildSizing(Source);
    if IsEqual(SrcSizing) then exit;

    FEnlargeHorizontal:=SrcSizing.EnlargeHorizontal;
    FEnlargeVertical:=SrcSizing.EnlargeVertical;
    FShrinkHorizontal:=SrcSizing.ShrinkHorizontal;
    FShrinkVertical:=SrcSizing.ShrinkVertical;
    FEnlargeHorizontal:=SrcSizing.EnlargeHorizontal;
    FEnlargeVertical:=SrcSizing.EnlargeVertical;
    FShrinkHorizontal:=SrcSizing.ShrinkHorizontal;
    FShrinkVertical:=SrcSizing.ShrinkVertical;
    FControlsPerLine:=SrcSizing.ControlsPerLine;
    FLayout:=SrcSizing.Layout;
    FLeftRightSpacing:=SrcSizing.LeftRightSpacing;
    FTopBottomSpacing:=SrcSizing.TopBottomSpacing;
    FHorizontalSpacing:=SrcSizing.HorizontalSpacing;
    FVerticalSpacing:=SrcSizing.VerticalSpacing;

    Change;
  end else
    inherited Assign(Source);
end;

procedure TControlChildSizing.AssignTo(Dest: TPersistent);
begin
  Dest.Assign(Self);
end;

function TControlChildSizing.IsEqual(Sizing: TControlChildSizing): boolean;
begin
  Result:=(FEnlargeHorizontal=Sizing.EnlargeHorizontal)
      and (FEnlargeVertical=Sizing.EnlargeVertical)
      and (FShrinkHorizontal=Sizing.ShrinkHorizontal)
      and (FShrinkVertical=Sizing.ShrinkVertical)
      and (FEnlargeHorizontal=Sizing.EnlargeHorizontal)
      and (FEnlargeVertical=Sizing.EnlargeVertical)
      and (FShrinkHorizontal=Sizing.ShrinkHorizontal)
      and (FShrinkVertical=Sizing.ShrinkVertical)
      and (FControlsPerLine=Sizing.ControlsPerLine)
      and (FLayout=Sizing.Layout)
      and (FLeftRightSpacing=Sizing.LeftRightSpacing)
      and (FTopBottomSpacing=Sizing.TopBottomSpacing)
      and (FHorizontalSpacing=Sizing.HorizontalSpacing)
      and (FVerticalSpacing=Sizing.VerticalSpacing);
end;

procedure TControlChildSizing.SetGridSpacing(Spacing: integer);
begin
  if (LeftRightSpacing=Spacing)
  and (TopBottomSpacing=Spacing)
  and (HorizontalSpacing=Spacing)
  and (VerticalSpacing=Spacing) then exit;
  fLeftRightSpacing:=Spacing;
  fTopBottomSpacing:=Spacing;
  fHorizontalSpacing:=Spacing;
  fVerticalSpacing:=Spacing;
  Change;
end;

procedure TControlChildSizing.Change;
begin
  if (FControl<>nil) and (FControl.GetInstance<>nil) and FControl.GetInstance.InheritsFrom(TBGRACustomCtrl) then
    TBGRACustomCtrl(FControl.GetInstance).DoChildSizingChange(Self);
  if Assigned(FOnChange) then
   FOnChange(Self);
end;

{$ENDIF}

end.

