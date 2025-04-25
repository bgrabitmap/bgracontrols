// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ATShapeLine is a component which paints line (directions: left-right, up-down, diagonals), with or without arrows at both sides. Line width is option. Line color and arrow color are options. It is Lazarus port of Delphi component TLine (renamed since TLine id is busy with TAChart).

Original author: Gon Perez-Jimenez (Spain, 2002)
Ported to Lazarus by: Alexey Torgashin (Russia)

- I redone get/set of canvas.pen and canvas.brush: do it only inside Paint, before it was all accross the code, in getters, setters, etc. This gives crashes of IDE on changing props in Linux.
- I added any linewidth for any direction with arrow1=true and arrow2=true.
- I converted demo to Laz using ide converter.
- Icon added to component-pallette to 'Misc'.

For BGRAControls by: Lainz

- Using BGRABitmap antialiased drawing (2020-09-09)

2025 - Massimo Magnano
         Fixed gtk draw outside area (Use Width/Height instead of Canvas.Width/Height)
         Added Color Property; Comments in English

Lazarus: 1.6+}

unit atshapelinebgra;

interface

{$mode delphi}

uses
  Graphics, SysUtils, Classes, Controls;

type
  TShapeLineDirection = (drLeftRight, drUpDown, drTopLeftBottomRight, drTopRightBottomLeft);

  { TShapeLineBGRA }

  TShapeLineBGRA = class(TGraphicControl)
  private
    { Private declarations }
    FLineDir: TShapeLineDirection;
    FArrow1: Boolean;
    FArrow2: Boolean;
    FArrowFactor: Integer;
    FLineWidth: integer;
    FLineColor: TColor;
    FArrowColor: TColor;
    FLineStyle: TPenStyle;

    procedure SetArrowColor(AValue: TColor);
    procedure SetLineColor(AValue: TColor);
    procedure SetLineDir(AValue: TShapeLineDirection);
    procedure SetArrow1(Value: Boolean);
    procedure SetArrow2(Value: Boolean);
    procedure SetArrowFactor(Value: integer);
    procedure SetLineWidth(AValue: Integer);
    procedure SetLineStyle(aLineStyle: TPenStyle);
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Color;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Align;
    property Anchors;
    property BorderSpacing;
    property ParentShowHint;
    property Hint;
    property ShowHint;
    property Visible;
    property PopupMenu;
    property Direction: TShapeLineDirection read FLineDir write SetLineDir default drLeftRight;
    property LineColor: TColor read FLineColor write SetLineColor;
    property ArrowColor: TColor read FArrowColor write SetArrowColor;
    property LineWidth: Integer read FLineWidth write SetLineWidth;
    property LineStyle: TPenStyle read FLineStyle write SetLineStyle default psSolid;
    property Arrow1: Boolean read FArrow1 write SetArrow1 default False;
    property Arrow2: Boolean read FArrow2 write SetArrow2 default False;
    property ArrowFactor: Integer read FArrowFactor write SetArrowFactor default 8;

    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEndDock;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnClick;
    property OnDblClick;
  end;

  procedure Register;

implementation

uses Math, BGRABitmap, BGRABitmapTypes;

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TShapeLineBGRA]);
end;

{ TShapeLineBGRA }

constructor TShapeLineBGRA.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  Width:=110;
  Height:=30;
  FArrow1:=false;
  FArrow2:=false;
  FArrowFactor:=8;
  FArrowColor:=clBlack;
  FLineColor:=clBlack;
  FLineWidth:=1;
  FLineStyle:=psSolid;
  FLineDir:=drLeftRight;
end;

destructor TShapeLineBGRA.Destroy;
begin
  inherited Destroy;
end;

procedure TShapeLineBGRA.SetArrowFactor(Value: integer);
begin
  if Value <> FArrowFactor then begin
     FArrowFactor := Value;
     Invalidate; 
  end;
end;

procedure TShapeLineBGRA.SetArrow1(Value: Boolean);
begin
  if Value <> FArrow1 then begin
     FArrow1 := Value;
     Invalidate;
  end;
end;

procedure TShapeLineBGRA.SetArrow2(Value: Boolean);
begin
  if Value <> FArrow2 then begin
     FArrow2 := Value;
     Invalidate;
  end;
end;

procedure TShapeLineBGRA.SetLineWidth(AValue: Integer);
begin
  if AValue <> FLineWidth then
  begin
    FLineWidth := AValue;
    Invalidate;
  end;
end;

procedure TShapeLineBGRA.SetLineStyle(aLineStyle: TPenStyle);
begin
  if aLineStyle <> FLineStyle then
  begin
    FLineStyle := aLineStyle;
    Invalidate;
  end;
end;

procedure TShapeLineBGRA.SetLineColor(AValue: TColor);
begin
  if AValue <> FLineColor then
  begin
    FLineColor := AValue;
    Invalidate;
  end;
end;

procedure TShapeLineBGRA.SetArrowColor(AValue: TColor);
begin
  if AValue <> FArrowColor then
  begin
    FArrowColor := AValue;
    Invalidate;
  end;
end;

procedure TShapeLineBGRA.SetLineDir(AValue: TShapeLineDirection);
begin
  if AValue <> FLineDir then
  begin
    FLineDir := AValue;
    Invalidate;
  end;
end;

procedure TShapeLineBGRA.Paint;
var
  start: Integer;
  p1,p2,p3: TPoint;
  H0,W0,H,W: Integer;
  Alfa: double;
  bgra: TBGRABitmap;
begin
  inherited;

  try
  if (Color=Parent.Color) or (Color=clNone)
  then bgra := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent)
  else bgra := TBGRABitmap.Create(Width, Height, ColorToBGRA(Color));

  bgra.CanvasBGRA.Pen.Color:= FLineColor;
  bgra.CanvasBGRA.Brush.Color:=FArrowColor;
  bgra.CanvasBGRA.Pen.Width:=FLineWidth;
  bgra.CanvasBGRA.Pen.Style:=FLineStyle;

  case FLineDir of
    drLeftRight:
      begin
        start := (Height -1) div 2;
        bgra.CanvasBGRA.Pen.Width:= FLineWidth;
        bgra.CanvasBGRA.MoveTo(IfThen(FArrow1, FArrowFactor), start);
        bgra.CanvasBGRA.LineTo(Width-IfThen(FArrow2, FArrowFactor), Start);
        bgra.CanvasBGRA.Pen.Width:= 1;

        if FArrow1 then begin
          //Left Arrow
          p1:=Point(0,start);
          p2:=Point(FArrowFactor,Start-FArrowFactor);
          p3:=Point(FArrowFactor,Start+FArrowFactor);
          bgra.CanvasBGRA.Polygon([p1,p2,p3]);
        end;

        if FArrow2 then begin
          //Right Arrow
          p1:=Point(Width-1, Start);
          p2:=Point(Width-(FArrowFactor+1),Start-FArrowFactor);
          p3:=Point(Width-(FArrowFactor+1),Start+FArrowFactor);
          bgra.CanvasBGRA.Polygon([p1,p2,p3]);
        end;
      end;

    drUpDown:
      begin
        start := (Width -1) div 2;
        bgra.CanvasBGRA.Pen.Width:= FLineWidth;
        bgra.CanvasBGRA.MoveTo(start, IfThen(FArrow1, FArrowFactor));
        bgra.CanvasBGRA.LineTo(start, Height-IfThen(FArrow2, FArrowFactor));
        bgra.CanvasBGRA.Pen.Width:= 1;

        if FArrow1 then begin
          //Up Arrow
          p1:=Point(start,0);
          p2:=Point(Start-FArrowFactor,FArrowFactor);
          p3:=Point(Start+FArrowFactor,FArrowFactor);
          bgra.CanvasBGRA.Polygon([p1,p2,p3]);
        end;

        if FArrow2 then begin
          //Down Arrow
          p1:=Point(start,Height-1);
          p2:=Point(Start-FArrowFactor,Height-(FArrowFactor+1));
          p3:=Point(Start+FArrowFactor,Height-(FArrowFactor+1));
          bgra.CanvasBGRA.Polygon([p1,p2,p3]);
        end;
      end;

    drTopLeftBottomRight:
      begin
        Alfa:= arctan2(Height, Width);
        bgra.CanvasBGRA.Pen.Width:= FLineWidth;
        bgra.CanvasBGRA.MoveTo(
          IfThen(FArrow1, Trunc(FArrowFactor*cos(Alfa))),
          IfThen(FArrow1, Trunc(FArrowFactor*sin(Alfa)))
          );
        bgra.CanvasBGRA.LineTo(
          Width-IfThen(FArrow2, Trunc(FArrowFactor*cos(Alfa))),
          Height-IfThen(FArrow2, Trunc(FArrowFactor*sin(Alfa)))
          );
        bgra.CanvasBGRA.Pen.Width:= 1;

        if FArrow1 and(Width>0)then begin
          //Up Arrow
          H0:=Round((FArrowFactor+1)*Sin(Alfa));
          W0:=Round((FArrowFactor+1)*Cos(Alfa));

          p1:=Point(0,0);
          W:=Round(W0+(FArrowFactor*Cos((Pi/2)-Alfa)));
          H:=Round(H0-(FArrowFactor*Sin((Pi/2)-Alfa)));

          if H<0 then H:=0;
          if W<0 then W:=0;

          p2:=Point(W,H);

          W:=Round(W0-(FArrowFactor*Cos((Pi/2)-Alfa)));
          H:=Round(H0+(FArrowFactor*Sin((Pi/2)-Alfa)));

          if H<0 then H:=0;
          if W<0 then W:=0;

          p3:=Point(W,H);

          bgra.CanvasBGRA.Polygon([p1,p2,p3]);
        end;


        if FArrow2 and(Width>0)then begin
          //Down Arrow
          H0:=Round((FArrowFactor+1)*Sin(Alfa));
          W0:=Round((FArrowFactor+1)*Cos(Alfa));

          p1:=Point(Width-1, Height-1);

          W:=Round(W0-(FArrowFactor*Cos((Pi/2)-Alfa)));
          H:=Round(H0+(FArrowFactor*Sin((Pi/2)-Alfa)));

          W:=Width-W-1;
          H:=Height-H-1;

          if H>=Height then H:=Height-1;
          if W>=Width then W:=Width-1;

          p2:=Point(W,H);

          W:=Round(W0+(FArrowFactor*Cos((Pi/2)-Alfa)));
          H:=Round(H0-(FArrowFactor*Sin((Pi/2)-Alfa)));

          W:=Width-W-1;
          H:=Height-H-1;

          if H>=Height then H:=Height-1;
          if W>=Width then W:=Width-1;

          p3:=Point(W,H);

          bgra.CanvasBGRA.Polygon([p1,p2,p3]);
        end;

      end;

    drTopRightBottomLeft:
      begin
        Alfa:= arctan2(Height, Width);
        bgra.CanvasBGRA.Pen.Width:= FLineWidth;
        bgra.CanvasBGRA.MoveTo(
          Width-IfThen(FArrow1, Trunc(FArrowFactor*cos(Alfa))),
          IfThen(FArrow1, Trunc(FArrowFactor*sin(Alfa)))
          );
        bgra.CanvasBGRA.LineTo(
          IfThen(FArrow2, Trunc(FArrowFactor*cos(Alfa))),
          Height-IfThen(FArrow2, Trunc(FArrowFactor*sin(Alfa)))
          );
        bgra.CanvasBGRA.Pen.Width:= 1;

        if FArrow1 and(Width>0)then begin
          H0:=Round((FArrowFactor+1)*Sin(Alfa));
          W0:=Round((FArrowFactor+1)*Cos(Alfa));

          p1:=Point(Width-1,0);

          W:=Round(W0+(FArrowFactor*Cos((Pi/2)-Alfa)));
          H:=Round(H0-(FArrowFactor*Sin((Pi/2)-Alfa)));

          W:=Width-W-1;

          if H<0 then H:=0;
          if W>=Width then W:=Width-1;

          p2:=Point(W,H);

          W:=Round(W0-(FArrowFactor*Cos((Pi/2)-Alfa)));
          H:=Round(H0+(FArrowFactor*Sin((Pi/2)-Alfa)));

          W:=Width-W-1;

          if H<0 then H:=0;
          if W>=Width then W:=Width-1;

          p3:=Point(W,H);

          bgra.CanvasBGRA.Polygon([p1,p2,p3]);
        end;

        if FArrow2 and(Width>0)then begin
          H0:=Round((FArrowFactor+1)*Sin(Alfa));
          W0:=Round((FArrowFactor+1)*Cos(Alfa));

          p1:=Point(0, Height-1);

          W:=Round(W0-(FArrowFactor*Cos((Pi/2)-Alfa)));
          H:=Round(H0+(FArrowFactor*Sin((Pi/2)-Alfa)));

          H:=Height-H-1;

          if H>=Height then H:=Height-1;
          if W<0 then W:=0;

          p2:=Point(W,H);

          W:=Round(W0+(FArrowFactor*Cos((Pi/2)-Alfa)));
          H:=Round(H0-(FArrowFactor*Sin((Pi/2)-Alfa)));

          H:=Height-H-1;

          if H>=Height then H:=Height-1;
          if W<0 then W:=0;

          p3:=Point(W,H);

          bgra.CanvasBGRA.Polygon([p1,p2,p3]);
        end;
      end;
  end;

  bgra.Draw(Canvas, 0, 0, False);

  finally
    bgra.Free;
  end;
end;

end.
