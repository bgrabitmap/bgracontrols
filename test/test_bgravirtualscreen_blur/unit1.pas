unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BGRAVirtualScreen,
  BCLabel, BGRABitmap, BCTypes, BCPanel, BCButton, BGRABitmapTypes, Types;

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
  blur: TBGRABitmap;
begin
  Bitmap.Fill(BGRABlack);
  if TControl(Sender).Tag = 1 then
  begin
    Bitmap.StretchPutImageProportionally(Rect(-TControl(Sender).Left,-TControl(Sender).Top,Width-TControl(Sender).Left,Height-TControl(Sender).Top), taCenter, tlCenter, backgroundblur, dmSet);
    Bitmap.Rectangle(0, 0, Bitmap.Width, Bitmap.Height, BGRA(255, 255, 255, 100), BGRA(255, 255, 255, 10), dmDrawWithTransparency);
  end
  else
  begin
    Bitmap.StretchPutImageProportionally(Rect(0,0,Width,Height), taCenter, tlCenter,  background, dmSet);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  background := TBGRABitmap.Create('background.jpg');
  //BGRAReplace(background, background.Resample(Width, Height, rmFineResample));
  backgroundblur := background.FilterBlurRadial(10, 10, rbBox);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  background.Free;
  backgroundblur.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
var
  i: integer;
  vs: TBGRAVirtualScreen;
begin
  for i:=0 to Self.ControlCount-1 do
  begin
    if (Self.Controls[i] is TBGRAVirtualScreen) then
    begin
      vs := (Self.Controls[i] as TBGRAVirtualScreen);
      vs.DiscardBitmap;
    end;
  end;
end;


end.

