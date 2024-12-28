unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls,
  BGRAFlashProgressBar, BCTrackbarUpdown, BGRASpeedButton, BGRABitmap, BGRABitmapTypes,
  BGRADrawerFlashProgressBar;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCTrackbarUpdown1: TBCTrackbarUpdown;
    BGRASpeedButton3: TBGRASpeedButton;
    edMarqueeWidth: TBCTrackbarUpdown;
    BGRAFlashProgressBar1: TBGRAFlashProgressBar;
    BGRAFlashProgressBar2: TBGRAFlashProgressBar;
    BGRAMaxMProgress: TBGRAFlashProgressBar;
    BGRASpeedButton1: TBGRASpeedButton;
    BGRASpeedButton2: TBGRASpeedButton;
    edMultiPValue: TBCTrackbarUpdown;
    edMultiPValueM: TBCTrackbarUpdown;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    rgMarqueeSpeed: TRadioGroup;
    procedure BCTrackbarUpdown1Change(Sender: TObject; AByUser: boolean);
    procedure BGRAFlashProgressBar1Redraw(Sender: TObject; Bitmap: TBGRABitmap;
      xpos: integer);
    procedure BGRAFlashProgressBar2Click(Sender: TObject);
    procedure BGRAFlashProgressBar2Redraw(Sender: TObject; Bitmap: TBGRABitmap;
      xpos: integer);
    procedure BGRASpeedButton1Click(Sender: TObject);
    procedure BGRASpeedButton2Click(Sender: TObject);
    procedure BGRASpeedButton3Click(Sender: TObject);
    procedure edMultiPValueMChange(Sender: TObject; AByUser: boolean);
    procedure edMultiPValueChange(Sender: TObject; AByUser: boolean);
    procedure edMarqueeWidthChange(Sender: TObject; AByUser: boolean);
    procedure rgMarqueeSpeedClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses BGRATextFX;

{$R *.lfm}

{ TForm1 }

procedure TForm1.BGRAFlashProgressBar2Redraw(Sender: TObject;
  Bitmap: TBGRABitmap; xpos: integer);
begin
  { Draw the progressbar container }
  Bitmap.Rectangle(0, 0, Bitmap.Width, Bitmap.Height, BGRABlack, BGRAWhite, dmSet);
  { Draw the progressbar progress }
  Bitmap.Rectangle(1, 1, xpos + 1, Bitmap.Height - 1, BGRAWhite, BGRABlack, dmSet);
end;

procedure TForm1.BGRASpeedButton1Click(Sender: TObject);
begin
  BGRAMaxMProgress.Style:= pbstMarquee;
end;

procedure TForm1.BGRASpeedButton2Click(Sender: TObject);
begin
  if (BGRAMaxMProgress.MarqueeMode = pbmmToRight)
  then BGRAMaxMProgress.MarqueeMode:= pbmmToLeft
  else BGRAMaxMProgress.MarqueeMode:= pbmmToRight;
end;

procedure TForm1.BGRASpeedButton3Click(Sender: TObject);
begin
  BGRAMaxMProgress.Style:= pbstMultiProgress;
end;

procedure TForm1.edMultiPValueMChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser then
  begin
    BGRAMaxMProgress.ValueM:= edMultiPValueM.Value;
    edMultiPValueM.Value:= BGRAMaxMProgress.ValueM;
  end;
end;

procedure TForm1.edMultiPValueChange(Sender: TObject; AByUser: boolean);
begin
  BGRAMaxMProgress.Value:= edMultiPValue.Value;
end;

procedure TForm1.edMarqueeWidthChange(Sender: TObject; AByUser: boolean);
begin
  BGRAMaxMProgress.MarqueeWidth:= edMarqueeWidth.Value;
end;

procedure TForm1.rgMarqueeSpeedClick(Sender: TObject);
begin
  BGRAMaxMProgress.MarqueeSpeed:= TBGRAPBarMarqueeSpeed(rgMarqueeSpeed.ItemIndex);
end;

procedure TForm1.BCTrackbarUpdown1Change(Sender: TObject; AByUser: boolean);
begin
  BGRAFlashProgressBar1.Value := BCTrackbarUpdown1.Value;
  BGRAFlashProgressBar2.Value := BCTrackbarUpdown1.Value;
end;

procedure TForm1.BGRAFlashProgressBar1Redraw(Sender: TObject;
  Bitmap: TBGRABitmap; xpos: integer);
var fx: TBGRATextEffect;
begin
  fx:= TBGRATextEffect.Create(IntToStr(BGRAFlashProgressBar1.Value)+'%','Arial',BGRAFlashProgressBar1.Height div 2,True);
  fx.DrawOutline(Bitmap, Bitmap.Width div 2,Bitmap.Height div 4,BGRABlack,taCenter);
  fx.Draw(Bitmap, Bitmap.Width div 2,Bitmap.Height div 4,BGRAWhite,taCenter);
  fx.Free;
end;

procedure TForm1.BGRAFlashProgressBar2Click(Sender: TObject);
begin

end;

end.

