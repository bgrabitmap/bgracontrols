
{------------------------------------------------------------------------------

  Miguel A. Risco Castillo TuERotImage v0.6.3
  http://ue.accesus.com/uecontrols

  using some ideas from:
  TRotateImage v1.54 by Kambiz R. Khojasteh

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
  the specific language governing rights and limitations under the License.

------------------------------------------------------------------------------}

unit uERotImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Types, BGRABitmap, BGRABitmapTypes;

type

  { TuECustomRotImage }

  { TCustomuERotImage }

  TCustomuERotImage = class(TGraphicControl)
  private
    FColor: TColor;
    FPicture: TPicture;
    FOnPictureChanged: TNotifyEvent;
    FStretch: Boolean;
    FCenter: Boolean;
    FTransparent: Boolean;
    FProportional: Boolean;
    FAngle: Extended;
    FUniqueSize: Boolean;
    FMaxSize: Integer;
    FChanging: Boolean;
    FOnRotation: TNotifyEvent;
    FBeforeRotation: TNotifyEvent;
    function GetCanvas: TCanvas;
    procedure RotateImage(AValue: Extended);
    procedure ForceRotate(AValue: Extended);
    procedure SetCenter(const AValue: Boolean);
    procedure SetPicture(const AValue: TPicture);
    procedure SetStretch(const AValue: Boolean);
    procedure SetProportional(const AValue: Boolean);
    procedure SetTransparent(const AValue: Boolean);
    procedure SetAngle(const Value: Extended);
    procedure SetUniqueSize(Value: Boolean);
  protected
    class procedure WSRegisterClass; override;
    procedure PictureChanged(Sender : TObject); virtual;
    procedure CalculatePreferredSize(var PreferredWidth,
                                     PreferredHeight: integer;
                                     WithThemeSpace: Boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure Resize; override;
    procedure SetColor(AValue: TColor); override;
    procedure DoRotation; virtual;
    procedure DoBeforeRotation; virtual;
    function DestRect: TRect; virtual;
    property Canvas: TCanvas read GetCanvas;
    property MaxSize: Integer read FMaxSize;
    property Angle: Extended read FAngle write SetAngle;
    property BorderSpacing;
    property Center: Boolean read FCenter write SetCenter default False;
    property Color: tcolor read FColor write SetColor default clDefault;
    property Picture: TPicture read FPicture write SetPicture;
    property Proportional: Boolean read FProportional write setProportional default False;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property UniqueSize: Boolean read FUniqueSize write SetUniqueSize default False;
    property OnPictureChanged: TNotifyEvent read FOnPictureChanged write FOnPictureChanged;
    property OnRotation: TNotifyEvent read FOnRotation write FOnRotation;
    property BeforeRotation: TNotifyEvent read FBeforeRotation write FBeforeRotation;
  public
    Bitmap: TBGRABitmap;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReDraw;
  end;


  { TuECustomRotImage }

  TuERotImage = class(TCustomuERotImage)
  published
    property Align;
    property Anchors;
    property Angle;
    property AutoSize;
    property BorderSpacing;
    property Center;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property MaxSize;
    property ParentColor;
    property ParentShowHint;
    property Picture;
    property PopupMenu;
    property Proportional;
    property ShowHint;
    property Stretch;
    property Transparent;
    property UniqueSize;
    property Visible;
    property OnChangeBounds;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnPictureChanged;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnRotation;
    property BeforeRotation;
    property OnStartDock;
    property OnStartDrag;
  end;


procedure Register;

implementation

//uses Math;

constructor TCustomuERotImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable, csCaptureMouse, csClickEvents, csDoubleClicks];
  AutoSize := False;
  FCenter := False;
  FProportional := False;
  FStretch := False;
  FTransparent := True;
  FUniqueSize := False;
  with GetControlClassDefaultSize do
  begin
    SetInitialBounds(0, 0, CX, CY);
    FPicture := TPicture.Create;
    FPicture.Bitmap.SetSize(0,0);
    FPicture.OnChange := @PictureChanged;
    Bitmap:=TBGRABitmap.Create(CX,CY);
  end;
end;

destructor TCustomuERotImage.Destroy;
begin
  if Assigned(Bitmap) then
  begin
    Bitmap.Free;
    Bitmap := nil;
  end;
  FPicture.OnChange := nil;
  FPicture.Graphic := nil;
  FPicture.Free;
  inherited Destroy;
end;

procedure TCustomuERotImage.ReDraw;
begin
  ForceRotate(FAngle);
  Paint;
end;

procedure TCustomuERotImage.Paint;

  procedure DrawFrame;
  begin
    with inherited Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Style := psDash;
      MoveTo(0, 0);
      LineTo(Self.Width-1, 0);
      LineTo(Self.Width-1, Self.Height-1);
      LineTo(0, Self.Height-1);
      LineTo(0, 0);
    end;
  end;

var R:TRect;

begin
  if csDesigning in ComponentState then DrawFrame;
  if assigned(Bitmap) then
  begin
    R:=DestRect;
    Bitmap.Draw(Canvas,R,false);
  end;
end;

procedure TCustomuERotImage.Loaded;
begin
  inherited Loaded;
  PictureChanged(Self);
end;


procedure TCustomuERotImage.Resize;
begin
  inherited Resize;
  RotateImage(FAngle);
end;

function TCustomuERotImage.GetCanvas: TCanvas;
begin
  Result := inherited Canvas;
end;

procedure TCustomuERotImage.SetCenter(const AValue: Boolean);
begin
  if FCenter = AValue then exit;
  FCenter := AValue;
  invalidate;
end;

procedure TCustomuERotImage.SetPicture(const AValue: TPicture);
begin
  if FPicture=AValue then exit;
  FPicture.Assign(AValue);
end;

procedure TCustomuERotImage.SetStretch(const AValue: Boolean);
begin
  if FStretch = AValue then exit;
  FStretch := AValue;
  invalidate;
end;

procedure TCustomuERotImage.SetProportional(const AValue: Boolean);
begin
  if FProportional = AValue then exit;
  FProportional := AValue;
  invalidate;
end;

procedure TCustomuERotImage.SetTransparent(const AValue: Boolean);
begin
  if FTransparent = AValue then exit;
  FTransparent := AValue;
  ForceRotate(FAngle);
end;

procedure TCustomuERotImage.SetColor(AValue: TColor);
begin
  if FColor = AValue then exit;
  FColor := AValue;
  ForceRotate(FAngle);
  inherited SetColor(AValue);
end;

procedure TCustomuERotImage.ForceRotate(AValue: Extended);
var xc,yc:real;
  w,h:integer;
  rad,s,c:Extended;
  tbmp:TBGRABitmap;
  fillc:TBGRAPixel;
begin
  if (csLoading in ComponentState) or FChanging then exit;
  FChanging := True;
  rad:=AValue*PI/180;
  s:=abs(sin(rad));
  c:=abs(cos(rad));
  If FTransparent then Fillc:=BGRAPixelTransparent else Fillc:=ColortoBGRA(ColortoRGB(color));
  if FPicture.Bitmap.Width>0 then
  begin
    xc:=(FPicture.Bitmap.width-1)/2;
    yc:=(FPicture.Bitmap.height-1)/2;
    w:= round(FPicture.Bitmap.width*c + FPicture.Bitmap.height*s);
    h:= round(FPicture.Bitmap.width*s + FPicture.Bitmap.height*c);
    if UniqueSize then tbmp:=TBGRABitmap.Create(MaxSize,MaxSize,Fillc)
    else tbmp:=TBGRABitmap.Create(w,h,Fillc);
    Bitmap.SetSize(1,1);
    Bitmap.Fill(Fillc);
    Bitmap.Assign(FPicture.Bitmap);
  end else
  begin
    xc:=(width-1)/2;
    yc:=(height-1)/2;
    tbmp:=TBGRABitmap.Create(width,height,Fillc);
    Bitmap.SetSize(width,height);
    Bitmap.Fill(Fillc);
  end;
  w:=Round((tbmp.Width)/2-1);
  h:=Round((tbmp.Height)/2-1);
  DoBeforeRotation;
  tbmp.PutImageAngle(w,h,Bitmap,AValue,xc,yc);
  Bitmap.Assign(tbmp);
  tbmp.free;
  try
    if AutoSize and (MaxSize <> 0) then
    begin
      InvalidatePreferredSize;
      AdjustSize;
      if UniqueSize then SetBounds(Left, Top, MaxSize, MaxSize)
      else SetBounds(Left, Top, Bitmap.Width, Bitmap.Height);
    end;
  finally
    FChanging := False;
  end;
  Invalidate;
  DoRotation;
end;

procedure TCustomuERotImage.RotateImage(AValue:Extended);
begin
  if AValue <> FAngle then ForceRotate(FAngle);
end;

procedure TCustomuERotImage.SetAngle(const Value: Extended);
begin
  if Value <> FAngle then
  begin
    FAngle := Value;
    ForceRotate(FAngle);
  end;
end;

procedure TCustomuERotImage.SetUniqueSize(Value: Boolean);
begin
  if Value <> UniqueSize then
  begin
    FUniqueSize := Value;
    ForceRotate(FAngle);
  end;
end;

procedure TCustomuERotImage.PictureChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
  begin
    if FPicture.Bitmap.width>0 then
    begin
      if (FPicture.Graphic.MimeType='') and (csDesigning in ComponentState) then
      begin
        showmessage('Bug: unsupported Picture format at design time');
        FPicture.Bitmap.SetSize(0,0);
        FMaxSize := 0;
      end else FMaxSize := Round(Sqrt(Sqr(FPicture.Graphic.Width) + Sqr(FPicture.Graphic.Height)));
    end else FMaxSize := Round(Sqrt(Sqr(Width) + Sqr(Height)));
    ForceRotate(FAngle);
    if Assigned(OnPictureChanged) then OnPictureChanged(Self);
  end;
end;

procedure TCustomuERotImage.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth := Bitmap.Width;
  PreferredHeight := Bitmap.Height;
end;

class function TCustomuERotImage.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 90;
  Result.CY := 90;
end;

function TCustomuERotImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if not (csDesigning in ComponentState) or (MaxSize <> 0) then
  begin
    if Align in [alNone, alLeft, alRight] then
      if UniqueSize then
        NewWidth := MaxSize
      else
        NewWidth := Bitmap.Width;
    if Align in [alNone, alTop, alBottom] then
      if UniqueSize then
        NewHeight := MaxSize
      else
        NewHeight := Bitmap.Height;
  end;
end;

procedure TCustomuERotImage.DoRotation;
begin
  if Assigned(FOnRotation) then FOnRotation(Self);
end;

procedure TCustomuERotImage.DoBeforeRotation;
begin
  if Assigned(FBeforeRotation) then FBeforeRotation(Self);
end;

class procedure TCustomuERotImage.WSRegisterClass;
begin
  inherited WSRegisterClass;
end;

function TCustomuERotImage.DestRect: TRect;
var
  PicWidth: Integer;
  PicHeight: Integer;
  ImgWidth: Integer;
  ImgHeight: Integer;
  w: Integer;
  h: Integer;
begin
  if not Assigned(Bitmap) then exit;
  PicWidth := Bitmap.Width;
  PicHeight := Bitmap.Height;
  ImgWidth := ClientWidth;
  ImgHeight := ClientHeight;
  if Stretch or (Proportional and ((PicWidth > ImgWidth) or (PicHeight > ImgHeight))) then
  begin
    if Proportional and (PicWidth > 0) and (PicHeight > 0) then
    begin
      w:=ImgWidth;
      h:=(PicHeight*w) div PicWidth;
      if h>ImgHeight then
      begin
        h:=ImgHeight;
        w:=(PicWidth*h) div PicHeight;
      end;
      PicWidth:=w;
      PicHeight:=h;
    end
    else begin
      PicWidth := ImgWidth;
      PicHeight := ImgHeight;
    end;
  end;
  Result:=Rect(0,0,PicWidth,PicHeight);
  if Center then
{$IFDEF WINDOWS}
    OffsetRect(Result,(ImgWidth-PicWidth) div 2,(ImgHeight-PicHeight) div 2);
{$ELSE}
    OffsetRect(Result,-(ImgWidth-PicWidth) div 2,-(ImgHeight-PicHeight) div 2);
{$ENDIF}
end;

procedure Register;
begin
  {$I icons\uerotimage_icon.lrs}
  RegisterComponents('BGRA Controls',[TuERotImage]);
end;


end.

