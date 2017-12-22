unit BCSVGViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, BGRAGraphicControl,
  BGRABitmap, BGRABitmapTypes, BGRASVG, BGRAUnits;

type

  { TBCSVGViewer }

  TBCSVGViewer = class(TCustomBGRAGraphicControl)
  private
    FSVG: TBGRASVG;
    FDestDPI: single;
    Fx: single;
    Fy: single;
    procedure SetFDestDPI(AValue: single);
    procedure SetFx(AValue: single);
    procedure SetFy(AValue: single);
  protected
    procedure BGRASetSize(AWidth, AHeight: integer); override;
    procedure RedrawBitmapContent; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: string);
  published
    { Published declarations }
    property Align;
    property Anchors;
    property OnRedraw;
    property Bitmap;
    property SVG: TBGRASVG read FSVG;
    property DestDPI: single read FDestDPI write SetFDestDPI default 96;
    property x: single read Fx write SetFx default 0;
    property y: single read Fy write SetFy default 0;
    property Color;
    property ColorOpacity;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property Caption;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCSVGViewer]);
end;

{ TBCSVGViewer }

procedure TBCSVGViewer.SetFDestDPI(AValue: single);
begin
  if FDestDPI = AValue then
    Exit;
  FDestDPI := AValue;
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

procedure TBCSVGViewer.BGRASetSize(AWidth, AHeight: integer);
begin
  if (FSVG <> nil) and (AWidth <> FSVG.Width.Value) and
    (AHeight <> FSVG.Height.Value) then
  begin
    FSVG.Width := FloatWithCSSUnit(AWidth, TCSSUnit.cuPixel);
    FSVG.Height := FloatWithCSSUnit(AHeight, TCSSUnit.cuPixel);
  end;
  inherited BGRASetSize(AWidth, AHeight);
end;

procedure TBCSVGViewer.RedrawBitmapContent;
begin
  if (FBGRA <> nil) and (FBGRA.NbPixels <> 0) then
  begin
    FBGRA.Fill(ColorToBGRA(ColorToRGB(Color), ColorOpacity));
    FSVG.Draw(FBGRA.Canvas2D, x, y, DestDPI);
    if Assigned(OnRedraw) then
      OnRedraw(self, FBGRA);
  end;
end;

constructor TBCSVGViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSVG := TBGRASVG.Create;//(Width, Height, TCSSUnit.cuPixel);
  FDestDPI := 96;
  Fx := 0;
  Fy := 0;
end;

destructor TBCSVGViewer.Destroy;
begin
  FSVG.Free;
  inherited Destroy;
end;

procedure TBCSVGViewer.LoadFromFile(AFileName: string);
begin
  FSVG.LoadFromFile(AFileName);
end;

end.
