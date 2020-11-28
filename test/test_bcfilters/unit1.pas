unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Spin, ExtCtrls, BCFilters, BGRAVirtualScreen, BGRABitmap,
  BGRABitmapTypes, BCTypes, BGRATextFX, Math, BCEffect;

type

  { TForm1 }

  TForm1 = class(TForm)
    fs1: TFloatSpinEdit;
    fs2: TFloatSpinEdit;
    ListBox1: TListBox;
    ListBox2: TListBox;
    ListBox3: TListBox;
    ListBox4: TListBox;
    Timer1: TTimer;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    vs1: TBGRAVirtualScreen;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure fs1Change(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure vs1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
  private
    { private declarations }
  public
    { public declarations }
    Fade: TFading;
    Image: TBGRABitmap;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Image := TBGRABitmap.Create(Application.Location + 'Penguins.jpg');

  BCSimpleFilterStrList(ListBox1.Items);
  BCSimpleFilterStrList(ListBox2.Items);
  BCSimpleFilterStrList(ListBox3.Items);
  BCSimpleFilterStrList(ListBox4.Items);
  ListBox1.Selected[20] := True;
  ListBox2.Selected[21] := True;
  ListBox3.Selected[5] := True;
  ListBox4.Selected[0] := True;

  Fade.Mode := fmFadeOut;
  Fade.Step := 15;
  Fade.Reset;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  Timer1.Enabled := False;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Image.Free;
end;

procedure TForm1.fs1Change(Sender: TObject);
begin
  vs1.RedrawBitmap;
end;

procedure TForm1.ListBox1SelectionChange(Sender: TObject; User: boolean);
begin
  vs1.RedrawBitmap;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  //fs1.Value := fs1.Value + 0.0002;
  fs2.Value := fs2.Value + 0.0002;
  if (fs2.Value = 1) {and (fs1.Value = 1)} then
    Timer1.Enabled:=False;
end;

procedure TForm1.vs1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  tmp: TBGRABitmap;
begin
  Bitmap.BlendImageOver(0, 0, Image, boTransparent, TrackBar4.Position);

  tmp := TextShadow(vs1.Width, vs1.Height, 'Penguin TELEPORT project', 20, BGRA(255,255,255,200), BGRABlack, 1, 1, 2) as TBGRABitmap;
  Bitmap.BlendImage(RandomRange(0,1), RandomRange(vs1.Height div 2 - 40,vs1.Height div 2 - 38), tmp, boLinearBlend);
  tmp.Free;

  { Apply filter 1 }
  SimpleFilter(Bitmap, StrToTBCSimpleFilter(ListBox1.GetSelectedText));
  { Apply filter 2 }
  SimpleFilter(Bitmap, StrToTBCSimpleFilter(ListBox2.GetSelectedText));
  { Apply filter 3 }
  SimpleFilter(Bitmap, StrToTBCSimpleFilter(ListBox3.GetSelectedText));
  { Apply filter 4 }
  SimpleFilter(Bitmap, StrToTBCSimpleFilter(ListBox4.GetSelectedText));

  // Fade Out the black rectangle
  Bitmap.FillRect(0, 0, vs1.Width, vs1.Height, BGRA(0, 0, 0, Fade.Execute), dmFastBlend);

  FilterRGB(Bitmap, TrackBar1.Position, TrackBar2.Position, TrackBar3.Position);

  { Weird thing }
  Zoomy(Bitmap,fs1.Value,fs2.Value);
end;

end.

