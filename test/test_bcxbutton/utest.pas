unit utest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BCImageButton, BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes, BCTypes,
  BCFilters, BGRATextFX, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCXButton1: TBCXButton;
    BCXButton2: TBCXButton;
    BCXButton3: TBCXButton;
    BCXButton4: TBCXButton;
    BCXButton5: TBCXButton;
    BCXButton6: TBCXButton;
    BCXButton7: TBCXButton;
    vs1: TBGRAVirtualScreen;
    procedure BCXButton1RenderControl(Sender: TObject; Bitmap: TBGRABitmap;
      State: TBCGraphicButtonState);
    procedure BCXButton3RenderControl(Sender: TObject; Bitmap: TBGRABitmap;
      State: TBCGraphicButtonState);
    procedure vs1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Image: TBGRABitmap;
  end;

var
  Form1: TForm1;

implementation

uses
  Themes, LCLType;

{$R *.lfm}

{ Button }

procedure DrawBasicXButton(Sender: TObject; Bitmap: TBGRABitmap;
  State: TBCGraphicButtonState);
begin
  case State of
    gbsNormal: Bitmap.Fill(BGRA(255, 0, 0, 100));
    gbsHover: Bitmap.Fill(BGRA(0, 255, 0, 100));
    gbsActive: Bitmap.Fill(BGRA(0, 0, 255, 100));
    gbsDisabled: Bitmap.Fill(BGRA(0, 0, 0, 100));
  end;
end;

procedure DrawGlassXButton(Sender: TObject; Bitmap, Background: TBGRABitmap;
  State: TBCGraphicButtonState);
var
  tmp: TBGRABitmap;
  r: TRect;
begin
  { Rect to copy backround }
  with TBCXButton(Sender) do
    r := Rect(Left, Top, Left + Width, Top + Height);
  { Copy background }
  tmp := Background.GetPart(r) as TBGRABitmap;
  { Apply blur }
  BGRAReplace(tmp, tmp.FilterBlurRadial(25, rbFast));
  { Blend }
  Bitmap.BlendImageOver(0, 0, tmp, boLinearBlend);
  tmp.Free;
  { Rect to draw borders }
  r := Rect(0, 0, Bitmap.Width, Bitmap.Height);
  { Draw borders }
  case State of
    gbsNormal:
    begin
      Bitmap.Rectangle(r, BGRA(0, 0, 0, 100), BGRA(255, 255, 255, 10),
        dmDrawWithTransparency);
      r := Rect(r.Left + 1, r.Top + 1, r.Right - 1, r.Bottom - 1);
      Bitmap.Rectangle(r, BGRA(255, 255, 255, 100), BGRAPixelTransparent,
        dmDrawWithTransparency);
    end;
    gbsHover:
    begin
      Bitmap.Rectangle(r, BGRA(0, 0, 0, 100), BGRA(255, 255, 255, 50),
        dmDrawWithTransparency);
      r := Rect(r.Left + 1, r.Top + 1, r.Right - 1, r.Bottom - 1);
      Bitmap.Rectangle(r, BGRA(255, 255, 255, 100), BGRAPixelTransparent,
        dmDrawWithTransparency);
    end;
    gbsActive:
    begin
      Bitmap.Rectangle(r, BGRA(0, 0, 0, 100), BGRA(100, 100, 100, 50),
        dmDrawWithTransparency);
      r := Rect(r.Left + 1, r.Top + 1, r.Right - 1, r.Bottom - 1);
      Bitmap.Rectangle(r, BGRA(255, 255, 255, 50), BGRAPixelTransparent,
        dmDrawWithTransparency);
    end;
    gbsDisabled:
    begin
      Bitmap.Rectangle(r, BGRA(0, 0, 0, 100), BGRA(100, 100, 100, 50),
        dmDrawWithTransparency);
      r := Rect(r.Left + 1, r.Top + 1, r.Right - 1, r.Bottom - 1);
      Bitmap.Rectangle(r, BGRA(100, 100, 100, 100), BGRAPixelTransparent,
        dmDrawWithTransparency);
    end;
  end;
end;

procedure DrawXButtonText(Sender: TObject; Bitmap: TBGRABitmap; State: TBCGraphicButtonState; Text: String);
var
  tmp: TBGRABitmap;
  op: byte;
begin
  if State = gbsDisabled then
    op := 100
  else
    op := 255;

  tmp := TextShadow(Bitmap.Width, Bitmap.Height, Text, 18, BGRA(255, 255, 255, op),
  BGRABlack, 1, 1, 2) as TBGRABitmap;

  Bitmap.BlendImageOver(0, 0, tmp, boLinearBlend);
  tmp.Free;
end;

{ TForm1 }

procedure TForm1.BCXButton1RenderControl(Sender: TObject; Bitmap: TBGRABitmap;
  State: TBCGraphicButtonState);
var
  str: string;
begin
  { Style }
  DrawGlassXButton(Sender, Bitmap, Image, State);
  //DrawBasicXButton(Sender, Bitmap, State);

  { Strings }
  str := TControl(Sender).Caption;

  { Text }
  DrawXButtonText(Sender, Bitmap, State, str);
end;

function SaveAlphaRect(ABitmap: TBGRABitmap; ARect: TRect): Pointer;
var width,height,count,y: Integer;
  pAlphaData: PByte;
  pSrc: PBGRAPixel;
begin
  IntersectRect(ARect,ARect,Classes.Rect(0,0,ABitmap.Width,ABitmap.Height));
  width := ARect.Right-ARect.Left;
  height := ARect.Bottom-ARect.Top;
  if (width <= 0) or (height <= 0) then result := nil;
  getmem(result, sizeof(longint)*2 + sizeof(byte)*width*height);
  PLongint(result)^ := width;
  (PLongint(result)+1)^ := height;
  pAlphaData := pbyte(plongint(result)+2);
  for y := ARect.Top to ARect.Bottom-1 do
  begin
    pSrc := ABitmap.ScanLine[y]+ARect.Left;
    count := width;
    while count > 0 do
    begin
      pAlphaData^ := pSrc^.alpha;
      inc(pAlphaData);
      inc(pSrc);
      dec(count);
    end;
  end;
end;

procedure RestoreAlphaRectAndFree(ABitmap: TBGRABitmap; AX,AY: integer; ASavedAlphaRect: Pointer);
var width,height,count,y: Integer;
  pAlphaData: PByte;
  pSrc: PBGRAPixel;
begin
  if ASavedAlphaRect = nil then exit;
  if AX < 0 then AX := 0;
  if AY < 0 then AY := 0;
  width := PLongint(ASavedAlphaRect)^;
  height := (PLongint(ASavedAlphaRect)+1)^;
  pAlphaData := pbyte(plongint(ASavedAlphaRect)+2);
  for y := AY to AY+height-1 do
  begin
    pSrc := ABitmap.ScanLine[y]+AX;
    count := width;
    while count > 0 do
    begin
      pSrc^.alpha := pAlphaData^;
      inc(pAlphaData);
      inc(pSrc);
      dec(count);
    end;
  end;
  freemem(ASavedAlphaRect);
end;

procedure TForm1.BCXButton3RenderControl(Sender: TObject; Bitmap: TBGRABitmap;
  State: TBCGraphicButtonState);
var
  Details: TThemedElementDetails;
  PaintRect: TRect;
  AlphaRect: Pointer;
begin
  PaintRect := Rect(0, 0, Bitmap.Width, Bitmap.Height);

  case State of
    gbsNormal: Details := ThemeServices.GetElementDetails(tbPushButtonNormal);
    gbsHover: Details := ThemeServices.GetElementDetails(tbPushButtonHot);
    gbsActive: Details := ThemeServices.GetElementDetails(tbPushButtonPressed);
    gbsDisabled: Details := ThemeServices.GetElementDetails(tbPushButtonDisabled);
  end;

  ThemeServices.DrawElement(Bitmap.Canvas.Handle, Details, PaintRect, nil);
  Bitmap.Canvas.Changed;

  PaintRect := ThemeServices.ContentRect(Bitmap.Canvas.Handle, Details, PaintRect);
  AlphaRect := SaveAlphaRect(Bitmap, PaintRect);
  ThemeServices.DrawText(Bitmap.Canvas, Details, TControl(Sender).Caption, PaintRect,
    DT_CENTER or DT_VCENTER or DT_SINGLELINE, 0);
  RestoreAlphaRectAndFree(Bitmap, PaintRect.Left,PaintRect.Top, AlphaRect);
end;

{ Image }

procedure TForm1.vs1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.BlendImageOver(0, 0, Image, boLinearBlend);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BCXButton2.Enabled := False;
  BCXButton4.Enabled := False;
  Image := TBGRABitmap.Create(Application.Location + 'Desert.jpg');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Image.Free;
end;

end.
