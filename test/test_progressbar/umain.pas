unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls, Spin,
  BGRAFlashProgressBar, BCTrackbarUpdown, BGRASpeedButton, BGRABitmap, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRASpeedButton3: TBGRASpeedButton;
    BGRASpeedButton4: TBGRASpeedButton;
    cbCaptionPercentM: TCheckBox;
    cbMarqueeWidth: TCheckBox;
    cbCaptionPercent: TCheckBox;
    edCaption: TEdit;
    edMarqueeWidth: TBCTrackbarUpdown;
    BGRAMaxMProgress: TBGRAFlashProgressBar;
    BGRASpeedButton1: TBGRASpeedButton;
    edMultiPValueM: TFloatSpinEdit;
    edValue: TFloatSpinEdit;
    edMin: TFloatSpinEdit;
    edMax: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    rgCaptionAlignM: TRadioGroup;
    rgMarqueeSpeed: TRadioGroup;
    edCaptionDigits: TSpinEdit;
    rgCaptionAlign: TRadioGroup;
    rgMarqueeDirection: TRadioGroup;
    procedure BCTrackbarUpdown1Change(Sender: TObject; AByUser: boolean);
    procedure BGRASpeedButton1Click(Sender: TObject);
    procedure BGRASpeedButton3Click(Sender: TObject);
    procedure BGRASpeedButton4Click(Sender: TObject);
    procedure cbCaptionPercentMChange(Sender: TObject);
    procedure cbMarqueeWidthChange(Sender: TObject);
    procedure cbCaptionPercentChange(Sender: TObject);
    procedure edCaptionDigitsChange(Sender: TObject);
    procedure edMaxChange(Sender: TObject);
    procedure edMinChange(Sender: TObject);
    procedure edMultiPValueMChange(Sender: TObject; AByUser: boolean);
    procedure edValueChange(Sender: TObject; AByUser: boolean);
    procedure edMarqueeWidthChange(Sender: TObject; AByUser: boolean);
    procedure rgCaptionAlignClick(Sender: TObject);
    procedure rgCaptionAlignMClick(Sender: TObject);
    procedure rgMarqueeDirectionClick(Sender: TObject);
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

procedure TForm1.BGRASpeedButton1Click(Sender: TObject);
begin
  BGRAMaxMProgress.Style:= pbstMarquee;
end;

procedure TForm1.BGRASpeedButton3Click(Sender: TObject);
begin
  BGRAMaxMProgress.Style:= pbstMultiProgress;
end;

procedure TForm1.BGRASpeedButton4Click(Sender: TObject);
begin
  BGRAMaxMProgress.Style:= pbstNormal;
end;

procedure TForm1.cbCaptionPercentMChange(Sender: TObject);
begin
  BGRAMaxMProgress.CaptionShowPercentM:= cbCaptionPercentM.Checked;
end;

procedure TForm1.cbMarqueeWidthChange(Sender: TObject);
begin
  if cbMarqueeWidth.checked
  then BGRAMaxMProgress.MarqueeWidth:= 0
  else BGRAMaxMProgress.MarqueeWidth:= edMarqueeWidth.Value;

  edMarqueeWidth.Enabled:= not(cbMarqueeWidth.checked);
end;

procedure TForm1.cbCaptionPercentChange(Sender: TObject);
begin
  BGRAMaxMProgress.CaptionShowPercent:= cbCaptionPercent.Checked;
end;

procedure TForm1.edCaptionDigitsChange(Sender: TObject);
begin
  BGRAMaxMProgress.CaptionPercentDigits:= edCaptionDigits.Value;
end;

procedure TForm1.edMaxChange(Sender: TObject);
begin
  BGRAMaxMProgress.MaxValue:=edMax.Value;
end;

procedure TForm1.edMinChange(Sender: TObject);
begin
  BGRAMaxMProgress.MinValue:=edMin.Value;
end;

procedure TForm1.edMultiPValueMChange(Sender: TObject; AByUser: boolean);
begin
  BGRAMaxMProgress.ValueM:= edMultiPValueM.Value;
  edMultiPValueM.Value:= BGRAMaxMProgress.ValueM;
end;

procedure TForm1.edValueChange(Sender: TObject; AByUser: boolean);
begin
  BGRAMaxMProgress.Value:= edValue.Value;
end;

procedure TForm1.edMarqueeWidthChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser then BGRAMaxMProgress.MarqueeWidth:= edMarqueeWidth.Value;
end;

procedure TForm1.rgCaptionAlignClick(Sender: TObject);
begin
  BGRAMaxMProgress.CaptionShowPercentAlign:= TAlignment(rgCaptionAlign.ItemIndex);
end;

procedure TForm1.rgCaptionAlignMClick(Sender: TObject);
begin
  BGRAMaxMProgress.CaptionShowPercentAlignM:= TAlignment(rgCaptionAlignM.ItemIndex);
end;

procedure TForm1.rgMarqueeDirectionClick(Sender: TObject);
begin
  BGRAMaxMProgress.MarqueeDirection:= TBGRAPBarMarqueeDirection(rgMarqueeDirection.ItemIndex);
end;

procedure TForm1.rgMarqueeSpeedClick(Sender: TObject);
begin
  BGRAMaxMProgress.MarqueeSpeed:= TBGRAPBarMarqueeSpeed(rgMarqueeSpeed.ItemIndex);
end;

procedure TForm1.BCTrackbarUpdown1Change(Sender: TObject; AByUser: boolean);
begin
  BGRAMaxMProgress.Value := edValue.Value;
end;

end.

