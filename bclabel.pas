{ Equivalent of standard lazarus TLabel but using BGRA Controls framework for text
  render.

  Functionality:
  - Customizable background (gradients etc.)
  - Customizable border (rounding etc.)
  - FontEx (shadow, word wrap, etc.)

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
unit BCLabel;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils,{$IFDEF FPC}LResources,{$ENDIF}
  types, Forms, Controls, Graphics, Dialogs,
  BCBasectrls, BGRABitmap, BGRABitmapTypes, BCTypes;

type

  { TCustomBCLabel }

  TCustomBCLabel = class(TBCStyleGraphicControl)
  private
    { Private declarations }
    {$IFDEF INDEBUG}
    FRenderCount: Integer;
    {$ENDIF}
    FBackground: TBCBackground;
    FBGRA: TBGRABitmapEx;
    FBorder: TBCBorder;
    FFontEx: TBCFont;
    FInnerMargin: single;
    FRounding: TBCRounding;
    procedure Render;
    procedure SetInnerMargin(AValue: single);
    procedure SetRounding(AValue: TBCRounding);
    procedure UpdateSize;
    procedure SetBackground(AValue: TBCBackground);
    procedure SetBorder(AValue: TBCBorder);
    procedure SetFontEx(AValue: TBCFont);
    procedure OnChangeProperty(Sender: TObject; {%H-}Data: BGRAPtrInt);
    procedure OnChangeFont({%H-}Sender: TObject; {%H-}AData: BGRAPtrInt);
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      {%H-}WithThemeSpace: boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure TextChanged; override;
  protected
    {$IFDEF INDEBUG}
    function GetDebugText: String; override;
    {$ENDIF}
    procedure DrawControl; override;
    procedure RenderControl; override;
    function GetStyleExtension: String; override;
  protected
    { Protected declarations }
    property AutoSize default True;
    property Background: TBCBackground read FBackground write SetBackground;
    property Border: TBCBorder read FBorder write SetBorder;
    property FontEx: TBCFont read FFontEx write SetFontEx;
    property Rounding: TBCRounding read FRounding write SetRounding;
    property InnerMargin: single read FInnerMargin write SetInnerMargin;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateControl; override; // Called by EndUpdate
  public
    { Streaming }
    {$IFDEF FPC}
    procedure SaveToFile(AFileName: string); override;
    procedure LoadFromFile(AFileName: string); override;
    {$ENDIF}
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
  end;

  { TBCLabel }

  TBCLabel = class(TCustomBCLabel)
  published
    property Action;
    property Align;
    property Anchors;
    property AssignStyle;
    property AutoSize;
    property Background;
    property Border;
    property BorderSpacing;
    property Caption;
    property Cursor;
    property Enabled;
    property FontEx;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property InnerMargin;
    property Left;
    property PopupMenu;
    property Rounding;
    property ShowHint;
    property Tag;
    property Top;
    property Visible;
    property Width;
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
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

uses BCTools;

{$IFDEF FPC}procedure Register;
begin
  //{$I icons\bclabel_icon.lrs}
  RegisterComponents('BGRA Controls',[TBCLabel]);
end;
{$ENDIF}

{ TCustomBCLabel }

procedure TCustomBCLabel.Render;
var r: TRect;
begin
  if (csCreating in ControlState) or IsUpdating then
    Exit;

  FBGRA.NeedRender := False;

  FBGRA.SetSize(Width, Height);
  FBGRA.Fill(BGRAPixelTransparent); // Clear;
  r := FBGRA.ClipRect;
  CalculateBorderRect(FBorder,r);

  RenderBackgroundAndBorder(FBGRA.ClipRect, FBackground, TBGRABitmap(FBGRA), FRounding, FBorder, FInnerMargin);
  RenderText(FBGRA.ClipRect, FFontEx, Caption, TBGRABitmap(FBGRA));

  {$IFDEF INDEBUG}
  FRenderCount := FRenderCount +1;
  {$ENDIF}
  {$IFNDEF FPC}//# //@  IN DELPHI NEEDRENDER NEED TO BE TRUE. IF FALSE COMPONENT IN BGRANORMAL BE BLACK AFTER INVALIDATE.
  FBGRA.NeedRender := True;
  {$ENDIF}
end;

procedure TCustomBCLabel.SetInnerMargin(AValue: single);
begin
  if FInnerMargin=AValue then Exit;
  FInnerMargin:=AValue;
  RenderControl;
  Invalidate;
end;

procedure TCustomBCLabel.SetRounding(AValue: TBCRounding);
begin
  if FRounding = AValue then Exit;
  FRounding.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCLabel.UpdateSize;
begin
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TCustomBCLabel.SetBackground(AValue: TBCBackground);
begin
  FBackground.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCLabel.SetBorder(AValue: TBCBorder);
begin
  FBorder.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCLabel.SetFontEx(AValue: TBCFont);
begin
  FFontEx.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCLabel.OnChangeProperty(Sender: TObject; Data: BGRAPtrInt);
begin
  RenderControl;
  if (Sender = FBorder) and AutoSize then
    UpdateSize;
  Invalidate;
end;

procedure TCustomBCLabel.OnChangeFont(Sender: TObject; AData: BGRAPtrInt);
begin
  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCLabel.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: boolean);
begin
  if (Parent = nil) or (not Parent.HandleAllocated) then
    Exit;

  CalculateTextSize(Caption, FFontEx, PreferredWidth, PreferredHeight);

  if AutoSize and (FBorder.Style<>bboNone) then
  begin
    Inc(PreferredHeight, 2 * FBorder.Width);
    Inc(PreferredWidth, 2 * FBorder.Width);
  end;
end;

class function TCustomBCLabel.GetControlClassDefaultSize: TSize;
begin
  Result.cx := 100;
  Result.cy := 25;
end;

procedure TCustomBCLabel.TextChanged;
begin
  inherited TextChanged;
  RenderControl;
  UpdateSize;
  Invalidate;
end;

{$IFDEF INDEBUG}
function TCustomBCLabel.GetDebugText: String;
begin
  Result := 'R: '+IntToStr(FRenderCount);
end;
{$ENDIF}

procedure TCustomBCLabel.DrawControl;
begin
  inherited DrawControl;
  if FBGRA.NeedRender then
    Render;
  FBGRA.Draw(Self.Canvas,0,0,False);
  {$IFNDEF FPC}//# //@  IN DELPHI RenderControl NEDD. IF NO RenderControl BE BLACK AFTER INVALIDATE.
  FBGRA.NeedRender := True;
  {$ENDIF}
end;

procedure TCustomBCLabel.RenderControl;
begin
  inherited RenderControl;
  if FBGRA<>nil then
    FBGRA.NeedRender := True;
end;

function TCustomBCLabel.GetStyleExtension: String;
begin
  Result := 'bclbl';
end;

procedure TCustomBCLabel.UpdateControl;
begin
  RenderControl;
  inherited UpdateControl; // invalidate
end;
{$IFDEF FPC}
procedure TCustomBCLabel.SaveToFile(AFileName: string);
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

procedure TCustomBCLabel.LoadFromFile(AFileName: string);
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

procedure TCustomBCLabel.OnFindClass(Reader: TReader; const AClassName: string;
  var ComponentClass: TComponentClass);
begin
  if CompareText(AClassName, 'TBCLabel') = 0 then
    ComponentClass := TBCLabel;
end;

constructor TCustomBCLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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
    with GetControlClassDefaultSize do
      SetInitialBounds(0, 0, CX, CY);
    FBGRA := TBGRABitmapEx.Create(Width, Height);
    FBackground         := TBCBackground.Create(Self);
    FBorder             := TBCBorder.Create(Self);
    FFontEx             := TBCFont.Create(Self);
    ParentColor         := True;

    FBackground.OnChange := OnChangeProperty;
    FBorder.OnChange     := OnChangeProperty;
    FFontEx.OnChange     := OnChangeFont;

    FBackground.Style   := bbsClear;
    FBorder.Style       := bboNone;

    FRounding           := TBCRounding.Create(Self);
    FRounding.OnChange  := OnChangeProperty;

    AutoSize := True;
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

destructor TCustomBCLabel.Destroy;
begin
  FBGRA.Free;
  FBackground.Free;
  FBorder.Free;
  FFontEx.Free;
  FRounding.Free;
  inherited Destroy;
end;

end.
