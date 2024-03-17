unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BGRAVirtualScreen,
  BCLabel, BGRABitmap, BCTypes, BCPanel, BCButton, BGRABitmapTypes, Types,
  BCFilters;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCButton2: TBCButton;
    BCButton3: TBCButton;
    BCButton4: TBCButton;
    BCButton5: TBCButton;
    BCButton6: TBCButton;
    BCButton7: TBCButton;
    BCLabel1: TBCLabel;
    BCLabel3: TBCLabel;
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    BGRAVirtualScreen2: TBGRAVirtualScreen;
    BGRAVirtualScreen3: TBGRAVirtualScreen;
    BGRAVirtualScreen4: TBGRAVirtualScreen;
    procedure BGRAVirtualScreen2Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    background: TBGRABitmap;
    backgroundnoise: TBGRABitmap;
    backgroundblur: TBGRABitmap;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BGRAVirtualScreen2Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  i: integer;
  vs: TBGRAVirtualScreen;
  shadow: TBGRABitmap;
begin
  Bitmap.Fill(BGRABlack);
  if TControl(Sender).Tag = 1 then
  begin
    Bitmap.StretchPutImageProportionally(Rect(-TControl(Sender).Left,-TControl(Sender).Top,Width-TControl(Sender).Left,Height-TControl(Sender).Top), taCenter, tlCenter, backgroundblur, dmSet, 255, True);
    Bitmap.RoundRectAntialias(0, 0, Bitmap.Width-1, Bitmap.Height-1, 5, 5, BGRA(255, 255, 255, 30), 1, BGRA(255, 255, 255, 10));
    Bitmap.PutImage(0, 0, backgroundnoise, dmDrawWithTransparency, 2);
  end
  else
  begin
    shadow := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
    Bitmap.StretchPutImageProportionally(Rect(0,0,Width,Height), taCenter, tlCenter,  background, dmSet, 255, True);
    for i:=0 to TWinControl(Sender).ControlCount-1 do
    begin
      if (TWinControl(Sender).Controls[i] is TBGRAVirtualScreen) then
      begin
        vs := (TWinControl(Sender).Controls[i] as TBGRAVirtualScreen);
        shadow.FillRect(vs.Left, vs.Top, vs.Left+vs.Width, vs.Top+vs.Height, BGRABlack, dmSet);
      end;
    end;
    BGRAReplace(shadow, shadow.FilterBlurRadial(8, 8, rbBox));
    Bitmap.PutImage(0, 0, shadow, dmDrawWithTransparency);
    shadow.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  background := TBGRABitmap.Create('background.jpg');
  //BGRAReplace(background, background.Resample(Width, Height, rmFineResample));
  backgroundblur := background.FilterBlurRadial(20, 20, rbBox);
  backgroundnoise := TBGRABitmap.Create(Screen.Width, Screen.Height, BGRABlack);
  NoiseBW(backgroundnoise);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  background.Free;
  backgroundblur.Free;
  backgroundnoise.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
var
  i: integer;
  vs: TBGRAVirtualScreen;
begin
  for i:=0 to BGRAVIrtualScreen1.ControlCount-1 do
  begin
    if (BGRAVIrtualScreen1.Controls[i] is TBGRAVirtualScreen) then
    begin
      vs := (BGRAVIrtualScreen1.Controls[i] as TBGRAVirtualScreen);
      vs.DiscardBitmap;
    end;
  end;
end;


end.

