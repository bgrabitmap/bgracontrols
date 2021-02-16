// SPDX-License-Identifier: LGPL-3.0-linking-exception
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCSVGViewer;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BGRAGraphicControl,
  {$IFDEF FPC}LResources, LCLType, {$ENDIF}
  {$IFNDEF FPC}Types, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BGRABitmap, BGRABitmapTypes, BGRASVG, BGRAUnits, BCTypes;

type

  { TBCSVGViewer }

  TBCSVGViewer = class(TCustomBGRAGraphicControl)
  private
    FDrawCheckers: boolean;
    FHorizAlign: TAlignment;
    FProportional: boolean;
    FStretchMode: TBCStretchMode;
    FDestDPI: single;
    FUseSVGAlignment: boolean;
    FVertAlign: TTextLayout;
    Fx: single;
    Fy: single;
    function GetSVGString: string;
    procedure SetDrawCheckers(AValue: boolean);
    procedure SetFDestDPI(AValue: single);
    procedure SetSVGString(AValue: string);
    procedure SetFx(AValue: single);
    procedure SetFy(AValue: single);
    procedure SetHorizAlign(AValue: TAlignment);
    procedure SetProportional(AValue: boolean);
    procedure SetStretchMode(AValue: TBCStretchMode);
    procedure SetUseSVGAlignment(AValue: boolean);
    procedure SetVertAlign(AValue: TTextLayout);
  protected
    FSVG: TBGRASVG;
    procedure RedrawBitmapContent; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: string);
    procedure LoadFromResource(Resource: string);
    function GetSVGRectF: TRectF;
    function GetSVGContainerRectF: TRectF;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property OnRedraw;
    property Bitmap;
    property BorderSpacing;
    property Constraints;
    property SVG: TBGRASVG read FSVG;
    property SVGString: string read GetSVGString write SetSVGString;
    property DestDPI: single read FDestDPI write SetFDestDPI {$IFDEF FPC} default
      96{$ENDIF};
    property x: single read Fx write SetFx {$IFDEF FPC} default 0{$ENDIF};
    property y: single read Fy write SetFy {$IFDEF FPC} default 0{$ENDIF};
    property HorizAlign: TAlignment read FHorizAlign write SetHorizAlign default
      taCenter;
    property VertAlign: TTextLayout read FVertAlign write SetVertAlign default tlCenter;
    property StretchMode: TBCStretchMode
      read FStretchMode write SetStretchMode default smStretch;
    property Proportional: boolean read FProportional write SetProportional default True;
    property DrawCheckers: boolean
      read FDrawCheckers write SetDrawCheckers default False;
    property UseSVGAlignment: boolean read FUseSVGAlignment write SetUseSVGAlignment default False;
    property Color;
    property ColorOpacity;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF FPC}
    property OnPaint;
    {$ENDIF}
    property OnResize;
    property Caption;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

uses BGRAVectorize, math;

{$IFDEF FPC}
procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCSVGViewer]);
end;

{$ENDIF}

{ TBCSVGViewer }

procedure TBCSVGViewer.SetFDestDPI(AValue: single);
begin
  if FDestDPI = AValue then
    Exit;
  FDestDPI := AValue;
  DiscardBitmap;
end;

procedure TBCSVGViewer.SetSVGString(AValue: string);
begin
  FSVG.ASUTF8String := AValue;
  DiscardBitmap;
end;

procedure TBCSVGViewer.SetDrawCheckers(AValue: boolean);
begin
  if FDrawCheckers = AValue then
    Exit;
  FDrawCheckers := AValue;
  DiscardBitmap;
end;

function TBCSVGViewer.GetSVGString: string;
begin
  Result := FSVG.AsUTF8String;
end;

procedure TBCSVGViewer.SetFx(AValue: single);
begin
  if Fx = AValue then
    Exit;
  Fx := AValue;
  DiscardBitmap;
end;

procedure TBCSVGViewer.SetFy(AValue: single);
begin
  if Fy = AValue then
    Exit;
  Fy := AValue;
  DiscardBitmap;
end;

procedure TBCSVGViewer.SetHorizAlign(AValue: TAlignment);
begin
  if FHorizAlign = AValue then
    Exit;
  FHorizAlign := AValue;
  DiscardBitmap;
end;

procedure TBCSVGViewer.SetProportional(AValue: boolean);
begin
  if FProportional = AValue then
    Exit;
  FProportional := AValue;
  DiscardBitmap;
end;

procedure TBCSVGViewer.SetStretchMode(AValue: TBCStretchMode);
begin
  if FStretchMode = AValue then
    Exit;
  FStretchMode := AValue;
  DiscardBitmap;
end;

procedure TBCSVGViewer.SetUseSVGAlignment(AValue: boolean);
begin
  if FUseSVGAlignment=AValue then Exit;
  FUseSVGAlignment:=AValue;
  DiscardBitmap;
end;

procedure TBCSVGViewer.SetVertAlign(AValue: TTextLayout);
begin
  if FVertAlign = AValue then
    Exit;
  FVertAlign := AValue;
  DiscardBitmap;
end;

procedure TBCSVGViewer.RedrawBitmapContent;
var
  r: TRectF;
  checkersSize: integer;
begin
  if (FBGRA <> nil) and (FBGRA.NbPixels <> 0) then
  begin
    r := GetSVGRectF;
    FBGRA.Fill(ColorToBGRA(ColorToRGB(Color), ColorOpacity));
    if FDrawCheckers then
    begin
      checkersSize := round(8 * DestDPI / 96 * BitmapScale);
      with GetSVGContainerRectF do
        FBGRA.DrawCheckers(rect(floor(Left), floor(Top),
          ceil(right), ceil(Bottom)), CSSWhite, CSSSilver,
          checkersSize, checkersSize);
    end;
    FBGRA.Canvas2D.FontRenderer := TBGRAVectorizedFontRenderer.Create;
    FSVG.StretchDraw(FBGRA.Canvas2D, r, UseSVGAlignment);
    if Assigned(OnRedraw) then
      OnRedraw(self, FBGRA);
  end;
end;

constructor TBCSVGViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSVG := TBGRASVG.Create(100, 100, TCSSUnit.cuPercent);
  FDestDPI := 96;
  Fx := 0;
  Fy := 0;
  FStretchMode := smStretch;
  FHorizAlign := taCenter;
  FVertAlign := tlCenter;
  FProportional := True;
  FBitmapAutoScale := False;
  FUseSVGAlignment:= false;
end;

destructor TBCSVGViewer.Destroy;
begin
  FSVG.Free;
  inherited Destroy;
end;

procedure TBCSVGViewer.LoadFromFile(AFileName: string);
begin
  FSVG.LoadFromFile(AFileName);
  DiscardBitmap;
end;

procedure TBCSVGViewer.LoadFromResource(Resource: string);
begin
  FSVG.LoadFromResource(Resource);
  DiscardBitmap;
end;

function TBCSVGViewer.GetSVGRectF: TRectF;
var
  vbSize: TPointF;
  w, h, dpi: single;
  containerRect: TRectF;

  function NoStretch(AX, AY: single): TRectF;
  begin
    case HorizAlign of
      taCenter: Result.Left := (w - vbSize.x) / 2;
      taRightJustify: Result.Left := w - AX - vbSize.x;
      else
        {taLeftJustify} Result.Left := AX;
    end;
    case VertAlign of
      tlCenter: Result.Top := (h - vbSize.y) / 2;
      tlBottom: Result.Top := h - AY - vbSize.y;
      else
        {tlTop} Result.Top := AY;
    end;
    Result.Right := Result.Left + vbSize.x;
    Result.Bottom := Result.Top + vbSize.y;
  end;

begin
  if FSVG = nil then exit(EmptyRectF);

  containerRect := GetSVGContainerRectF;
  w := containerRect.Width;
  h := containerRect.Height;
  dpi := DestDPI * BitmapScale;

  FSVG.Units.ContainerWidth := FloatWithCSSUnit(w * FSVG.Units.DpiX / dpi, cuPixel);
  FSVG.Units.ContainerHeight := FloatWithCSSUnit(h * FSVG.Units.DpiY / dpi, cuPixel);

  if UseSVGAlignment then
    exit(FSVG.GetStretchRectF(containerRect.Left, containerRect.Top, containerRect.Width, containerRect.Height));

  vbSize := FSVG.ViewSizeInUnit[cuPixel];
  vbSize.x := vbSize.x * (dpi / FSVG.Units.DpiX);
  vbSize.y := vbSize.y * (dpi / FSVG.Units.DpiY);
  if ((StretchMode = smShrink) and ((vbSize.x > w + 0.1) or (vbSize.y > h + 0.1))) or
    (StretchMode in[smStretch, smCover]) then
  begin
    if Proportional then
      Result := FSVG.GetStretchRectF(HorizAlign, VertAlign, 0, 0, w, h, StretchMode = smCover)
    else
    if StretchMode = smShrink then
    begin
      NoStretch(0, 0);
      if vbSize.x > w then
      begin
        Result.Left := 0;
        Result.Right := w;
      end;
      if vbSize.y > h then
      begin
        Result.Top := 0;
        Result.Bottom := h;
      end;
    end
    else
      Result := RectF(0, 0, w, h);
  end
  else
    result := NoStretch(x, y);

  result.Offset(containerRect.Left, containerRect.Top);
end;

function TBCSVGViewer.GetSVGContainerRectF: TRectF;
var
  w, h: Integer;
  dpi, ratioX, ratioY, ratio: single;
begin
  w := BitmapWidth;
  h := BitmapHeight;
  dpi := DestDPI * BitmapScale;
  Result := RectF(0, 0, w, h);

  if (FSVG = nil) or not UseSVGAlignment then exit;

  FSVG.Units.ContainerWidth := FloatWithCSSUnit(w * FSVG.Units.DpiX / dpi, cuPixel);
  FSVG.Units.ContainerHeight := FloatWithCSSUnit(h * FSVG.Units.DpiY / dpi, cuPixel);

  if (FSVG = nil) or (FSVG.WidthAsPixel = 0) or
    (FSVG.HeightAsPixel = 0) or (BitmapWidth = 0)
    or (BitmapHeight = 0) then exit(EmptyRectF);

  ratioX := BitmapWidth / FSVG.WidthAsPixel;
  ratioY := BitmapHeight / FSVG.HeightAsPixel;
  case StretchMode of
    smStretch: ratio := min(ratioX, ratioY);
    smShrink: ratio := min(1, min(ratioX, ratioY));
    smCover: ratio := max(ratioX, ratioY);
  else
    ratio := 1;
  end;
  result := RectWithSizeF(0, 0, FSVG.WidthAsPixel * ratio,
              FSVG.HeightAsPixel * ratio);
  result.Offset((BitmapWidth - result.Width) / 2,
    (BitmapHeight - result.Height) / 2);
end;

end.
