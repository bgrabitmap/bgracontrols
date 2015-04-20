unit BCToolBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  BGRABitmap, BGRABitmapTypes, BGRAGradients, BCTypes;

type

  { TBCToolBar }

  TBCToolBar = class(TToolBar)
  private
    { Private declarations }
    FOnRedraw: TBGRARedrawEvent;
    FBGRA: TBGRABitmap;
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property OnRedraw: TBGRARedrawEvent Read FOnRedraw Write FOnRedraw;
  end;

  procedure DrawWindows7ToolBar(Bitmap: TBGRABitmap);

procedure Register;

implementation

procedure DrawWindows7ToolBar(Bitmap: TBGRABitmap);
var
  c1, c2, c3, c4: TBGRAPixel;
  ARect, ARect2: TRect;
begin
  ARect := Rect(0, 0, Bitmap.Width, Bitmap.Height);
  // Font: RGBToColor(30,57,91)
  Bitmap.Canvas.Pen.Color := RGBToColor(169, 191, 214);
  Bitmap.Canvas.Line(ARect.Left, ARect.Top, ARect.Right, ARect.Top);

  Bitmap.Canvas.Pen.Color := RGBToColor(250, 252, 253);
  Bitmap.Canvas.Line(ARect.Left, ARect.Top + 1, ARect.Right, ARect.Top + 1);

  Bitmap.Canvas.Pen.Color := RGBToColor(253, 254, 255);
  Bitmap.Canvas.Line(ARect.Left, ARect.Top + 2, ARect.Right, ARect.Top + 2);

  c1 := BGRA(252, 254, 255);
  c2 := BGRA(243, 248, 253);
  c3 := BGRA(238, 243, 250);
  c4 := BGRA(238, 244, 251);
  ARect2 := Rect(ARect.Left, ARect.Top + 3, ARect.Right, ARect.Bottom - 3);
  DoubleGradientAlphaFill(Bitmap, ARect2, c1, c2, c3, c4, gdVertical,
    gdVertical, gdVertical, 0.5);

  c1 := BGRA(249, 252, 255);
  c2 := BGRA(230, 240, 250);
  c3 := BGRA(220, 230, 244);
  c4 := BGRA(221, 233, 247);
  ARect2 := Rect(ARect.Left + 1, ARect.Top + 3, ARect.Right - 1, ARect.Bottom - 3);
  DoubleGradientAlphaFill(Bitmap, ARect2, c1, c2, c3, c4, gdVertical,
    gdVertical, gdVertical, 0.5);

  Bitmap.Canvas.Pen.Color := RGBToColor(228, 239, 251);
  Bitmap.Canvas.Line(ARect.Left, ARect.Bottom - 3, ARect.Right, ARect.Bottom - 3);

  Bitmap.Canvas.Pen.Color := RGBToColor(205, 218, 234);
  Bitmap.Canvas.Line(ARect.Left, ARect.Bottom - 2, ARect.Right, ARect.Bottom - 2);

  Bitmap.Canvas.Pen.Color := RGBToColor(160, 175, 195);
  Bitmap.Canvas.Line(ARect.Left, ARect.Bottom - 1, ARect.Right, ARect.Bottom - 1);
end;

procedure Register;
begin
  RegisterComponents('BGRA Controls',[TBCToolBar]);
end;

{ TBCToolBar }

constructor TBCToolBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBGRA := TBGRABitmap.Create;
end;

destructor TBCToolBar.Destroy;
begin
  FBGRA.Free;
  inherited Destroy;
end;

procedure TBCToolBar.Paint;
begin
  if (FBGRA.Width <> Width) or (FBGRA.Height <> Height) then
    FBGRA.SetSize(Width, Height);
  if Assigned(FOnRedraw) then
    FOnRedraw(self, FBGRA);
  FBGRA.Draw(Canvas, 0, 0);
end;

end.
