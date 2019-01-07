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
    FVertAlign: TTextLayout;
    Fx: single;
    Fy: single;
    procedure SetDrawCheckers(AValue: boolean);
    procedure SetFDestDPI(AValue: single);
    procedure SetFx(AValue: single);
    procedure SetFy(AValue: single);
    procedure SetHorizAlign(AValue: TAlignment);
    procedure SetProportional(AValue: boolean);
    procedure SetStretchMode(AValue: TBCStretchMode);
    procedure SetVertAlign(AValue: TTextLayout);
  protected
    FSVG: TBGRASVG;
    procedure BGRASetSize(AWidth, AHeight: integer); override;
    procedure RedrawBitmapContent; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: string);
    procedure LoadFromResource(Resource: String);
    function GetSVGRectF: TRectF;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property OnRedraw;
    property Bitmap;
    property BorderSpacing;
    property Constraints;
    property SVG: TBGRASVG read FSVG;
    property DestDPI: single read FDestDPI write SetFDestDPI {$IFDEF FPC}default 96{$ENDIF};
    property x: single read Fx write SetFx {$IFDEF FPC}default 0{$ENDIF};
    property y: single read Fy write SetFy {$IFDEF FPC}default 0{$ENDIF};
    property HorizAlign: TAlignment read FHorizAlign write SetHorizAlign default taCenter;
    property VertAlign: TTextLayout read FVertAlign write SetVertAlign default tlCenter;
    property StretchMode: TBCStretchMode read FStretchMode write SetStretchMode default smStretch;
    property Proportional: boolean read FProportional write SetProportional default true;
    property DrawCheckers: boolean read FDrawCheckers write SetDrawCheckers default false;
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

uses BGRAVectorize;

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

procedure TBCSVGViewer.SetDrawCheckers(AValue: boolean);
begin
  if FDrawCheckers=AValue then Exit;
  FDrawCheckers:=AValue;
  DiscardBitmap;
end;

procedure TBCSVGViewer.SetFx(AValue: single);
begin
  if Fx=AValue then Exit;
  Fx:=AValue;
  DiscardBitmap;
end;

procedure TBCSVGViewer.SetFy(AValue: single);
begin
  if Fy=AValue then Exit;
  Fy:=AValue;
  DiscardBitmap;
end;

procedure TBCSVGViewer.SetHorizAlign(AValue: TAlignment);
begin
  if FHorizAlign=AValue then Exit;
  FHorizAlign:=AValue;
  DiscardBitmap;
end;

procedure TBCSVGViewer.SetProportional(AValue: boolean);
begin
  if FProportional=AValue then Exit;
  FProportional:=AValue;
  DiscardBitmap;
end;

procedure TBCSVGViewer.SetStretchMode(AValue: TBCStretchMode);
begin
  if FStretchMode=AValue then Exit;
  FStretchMode:=AValue;
  DiscardBitmap;
end;

procedure TBCSVGViewer.SetVertAlign(AValue: TTextLayout);
begin
  if FVertAlign=AValue then Exit;
  FVertAlign:=AValue;
  DiscardBitmap;
end;

procedure TBCSVGViewer.BGRASetSize(AWidth, AHeight: integer);
begin
  inherited BGRASetSize(AWidth, AHeight);
end;

procedure TBCSVGViewer.RedrawBitmapContent;
var
  r: TRectF;
begin
  if (FBGRA <> nil) and (FBGRA.NbPixels <> 0) then
  begin
    r := GetSVGRectF;
    if FDrawCheckers then
    begin
      FBGRA.DrawCheckers(rect(0,0,FBGRA.Width,FBGRA.Height), CSSWhite, CSSSilver);
      FBGRA.RectangleAntialias(r.Left,r.Top,r.Right,r.Bottom,BGRA(255,0,0,160),1);
    end else
      FBGRA.Fill(ColorToBGRA(ColorToRGB(Color), ColorOpacity));
    FBGRA.Canvas2D.FontRenderer := TBGRAVectorizedFontRenderer.Create;
    FSVG.StretchDraw(FBGRA.Canvas2D, r);
    if Assigned(OnRedraw) then
      OnRedraw(self, FBGRA);
  end;
end;

constructor TBCSVGViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSVG := TBGRASVG.Create(Width, Height, TCSSUnit.cuPixel);
  FDestDPI := 96;
  Fx := 0;
  Fy := 0;
  FStretchMode:= smStretch;
  FHorizAlign:= taCenter;
  FVertAlign:= tlCenter;
  FProportional := true;
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

procedure TBCSVGViewer.LoadFromResource(Resource: String);
var
  res: TResourceStream;
begin
  res := TResourceStream.Create(HInstance, Resource, RT_RCDATA);
  FSVG.LoadFromStream(res);
  res.Free;
  DiscardBitmap;
end;

function TBCSVGViewer.GetSVGRectF: TRectF;
var
  vbSize: TPointF;

  procedure NoStretch(AX,AY: single);
  begin
    case HorizAlign of
      taCenter: result.Left := (Width-vbSize.x)/2;
      taRightJustify: result.Left := Width-AX-vbSize.x;
      else {taLeftJustify} result.Left := AX;
    end;
    case VertAlign of
      tlCenter: result.Top := (Height-vbSize.y)/2;
      tlBottom: result.Top := Height-AY-vbSize.y;
      else {tlTop} result.Top := AY;
    end;
    result.Right := result.Left+vbSize.x;
    result.Bottom := result.Top+vbSize.y;
  end;

begin
  result := RectF(0,0,0,0);
  if FSVG = nil then exit;

  FSVG.Units.ContainerWidth := FloatWithCSSUnit(Width*FSVG.Units.DpiX/DestDPI,cuPixel);
  FSVG.Units.ContainerHeight := FloatWithCSSUnit(Height*FSVG.Units.DpiY/DestDPI,cuPixel);
  vbSize := FSVG.ViewSizeInUnit[cuPixel];
  vbSize.x := vbSize.x * (DestDPI/FSVG.Units.DpiX);
  vbSize.y := vbSize.y * (DestDPI/FSVG.Units.DpiY);
  if ((StretchMode = smShrink) and ((vbSize.x > Width+0.1) or (vbSize.y > Height+0.1))) or
     (StretchMode = smStretch) then
  begin
    if Proportional then
      result := FSVG.GetStretchRectF(HorizAlign, VertAlign, 0,0,Width,Height)
    else
    if StretchMode = smShrink then
    begin
      NoStretch(0,0);
      if vbSize.x > Width then
      begin
        result.Left := 0;
        result.Right := Width;
      end;
      if vbSize.y > Height then
      begin
        result.Top := 0;
        result.Bottom := Height;
      end;
    end else
      result := RectF(0,0,Width,Height);
  end else
  begin
    NoStretch(x,y);
  end;
end;

end.
