unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes;

const
  wind = -0.5; //1 means 45 degrees rain
  rainDensity = 2; //strictly positive

type

  { TForm1 }

  TForm1 = class(TForm)
    vsRain: TBGRAVirtualScreen;
    Timer1: TTimer;
    TrackBar1: TTrackBar;
    procedure RainRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    bkg,stretchedBkg: TBGRABitmap;
    prevTime: TDateTime;
    prevTimeDefined: boolean;
    rainData: array of record
        x,ystart,yend: single;
        rainWidth, rainSpeed: single;
        grad: TBGRACustomGradient;
        active: boolean;
        inactiveTime: double;
      end;
    procedure ClearRainData;
    procedure PrepareRainArray(nbRain: integer; ScaleX: single);
    function PrepareRainDrop(i: integer; rainSizeX, rainSizeY: single): single;
    procedure NeedRainArray(w, h, rainProba: integer; rainSizeX, rainSizeY: single);
    procedure RainElapse(elapsed: double; rainProba, w, h: integer);
    procedure RenderRain(Bitmap: TBGRABitmap);
  end;

var
  Form1: TForm1;

implementation

uses BGRAGradientScanner, Math;

{$R *.lfm}

{ TForm1 }

procedure TForm1.RainRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  elapsed: double;
  ratio: single;
  x,y,w,h: integer;
begin
  if not prevTimeDefined then
  begin
    elapsed := 0;
  end else
  begin
    elapsed := (Now-prevTime)*86400*10;
    if elapsed < 0 then elapsed := 0;
  end;
  prevTime := now;
  prevTimeDefined := true;

  if Assigned(stretchedBkg) and
    ((stretchedBkg.Width <> Bitmap.Width) or (stretchedBkg.Height <> Bitmap.Height)) then
      FreeAndNil(stretchedBkg);
  if not Assigned(stretchedBkg) then
  begin
    ratio := max(Bitmap.Width/bkg.Width,Bitmap.Height/bkg.Height);
    stretchedBkg := TBGRABitmap.Create(Bitmap.Width,Bitmap.Height,BGRABlack);
    w := round(bkg.Width*ratio);
    h := round(bkg.Height*ratio);
    x := (Bitmap.Width-w) div 2;
    y := (Bitmap.Height-h) div 2;
    stretchedBkg.StretchPutImage(rect(x,y,x+w,y+h),bkg,dmDrawWithTransparency);
  end;

  RainElapse(elapsed,TrackBar1.Position,Bitmap.Width,Bitmap.Height);

  Bitmap.PutImage(0,0,stretchedBkg,dmSet);
  RenderRain(Bitmap);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer1.Enabled := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  bkg := TBGRABitmap.Create('Lighthouse.jpg');
  randomize;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  bkg.Free;
  FreeAndNil(stretchedBkg);
  ClearRainData;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:= false;
  vsRain.RedrawBitmap;
  Timer1.Enabled:= true;
end;

procedure TForm1.ClearRainData;
var i: integer;
begin
  for i := 0 to high(rainData) do
    rainData[i].grad.Free;
  rainData := nil;
end;

procedure TForm1.RenderRain(Bitmap: TBGRABitmap);
var
  i,h2: Integer;
  scan: TBGRAGradientScanner;
begin
  h2 := Bitmap.Height div 2;
  for i:= 0 to high(rainData) do
  with rainData[i] do
  if active then
  begin
    scan := TBGRAGradientScanner.Create(grad, gtLinear, PointF(0,ystart),PointF(0,yend));
    Bitmap.DrawLineAntialias(x+(ystart-h2)*wind,ystart,x+(yend-h2)*wind,yend,scan,rainWidth,true);
    scan.Free;
  end;
end;

//returns raindrop height
function TForm1.PrepareRainDrop(i: integer; rainSizeX,rainSizeY: single): single;
var dist: single;
begin
  with rainData[i] do
  begin
    dist := (random(100)+10)/10;
    rainSpeed := 1/dist;
    rainWidth := rainSizeX/dist;
    if rainWidth < 1 then rainWidth := 1;
    result := rainSizeY/dist*(random(50)+75)/100;
  end;
end;

procedure TForm1.NeedRainArray(w, h, rainProba: integer; rainSizeX,rainSizeY: single);
var
  nbRain: Integer;
  i: Integer;
begin
  nbRain := (w+round(abs(wind)*h)) *rainDensity;
  if length(rainData)<> nbRain then
  begin
    PrepareRainArray(nbRain,1/rainDensity);
    for i := 0 to high(rainData) do
    with rainData[i] do
    begin
      x -= abs(wind)*h/2;
      if random(1000) < rainProba then
      begin
        active := true;
        ystart := Random(h*2)-h/2;
        yend := ystart + PrepareRainDrop(i, rainSizeX,rainSizeY);
      end;
    end;
  end;
end;

procedure TForm1.RainElapse(elapsed: double; rainProba,w,h: integer);
var
  i: integer;
  rainSizeY,rainSizeX: single;
  delta: single;
begin
  rainSizeY := 2+h*TrackBar1.Position/1000;
  rainSizeX := 7*TrackBar1.Position/1000;
  if rainSizeX < 4 then rainSizeX := 4;

  NeedRainArray(w,h, rainProba, rainSizeX,rainSizeY);
  for i := 0 to high(rainData) do
  with rainData[i] do
  if active then
  begin
    delta := h*rainSpeed*elapsed;
    ystart += delta;
    yend += delta;
    if ystart >= h then
    begin
      if random(1000) < rainProba then
      begin
        yend := -(ystart-h);
        ystart := yend - PrepareRainDrop(i, rainSizeX,rainSizeY);
      end else
      begin
        active := false;
        inactiveTime:= 0;
      end;
    end;
  end else
  begin
    inactiveTime+= elapsed;
    if inactiveTime > 0.5 then
    begin
      inactiveTime -= 0.5;
      if random(1000) < rainProba then
      begin
        active := true;
        ystart := -random(h)/2;
        yend := ystart + PrepareRainDrop(i, rainSizeX,rainSizeY);
      end;
    end;
  end;
end;

procedure TForm1.PrepareRainArray(nbRain: integer; ScaleX: single);
var
  i: Integer;
begin
  ClearRainData;
  setlength(rainData, nbRain);
  for i := 0 to high(rainData) do
  with rainData[i] do
  begin
    x := i*scaleX;
    grad := TBGRAMultiGradient.Create([BGRAPixelTransparent, BGRA(255,255,255,random(20)+50), BGRAPixelTransparent],[0,0.9,1],True);
    active:= false;
    inactiveTime := 0;
  end;
end;

end.

