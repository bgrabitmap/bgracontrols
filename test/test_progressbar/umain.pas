unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls, Spin, EditBtn,
  ColorBox, BGRAFlashProgressBar, BCTrackbarUpdown, BGRASpeedButton, ColorSpeedButton, BGRABitmap, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    btBackgroundColor: TColorSpeedButton;
    btFontColor: TColorSpeedButton;
    btBarColorM: TColorSpeedButton;
    btGraphAddValue: TBGRASpeedButton;
    btGraphTest: TBGRASpeedButton;
    btTimerPlayPause: TBGRASpeedButton;
    btTimerPlayPause1: TBGRASpeedButton;
    btTimerPlayPause2: TBGRASpeedButton;
    btTimerStart: TBGRASpeedButton;
    cbCaptionPercent: TCheckBox;
    cbBackgroundRandom: TCheckBox;
    cbCaptionPercent1: TCheckBox;
    cbCaptionPercentM: TCheckBox;
    cbShowDividers: TCheckBox;
    cbShowDividersY: TCheckBox;
    cbShowYLine: TCheckBox;
    cbTimerAutoStart: TCheckBox;
    cbTimerAutoStart1: TCheckBox;
    cbRandG: TCheckBox;
    cbShowBarAnimation: TCheckBox;
    ColorDialog1: TColorDialog;
    btBarColor: TColorSpeedButton;
    edCaption: TEdit;
    edMax: TFloatSpinEdit;
    edMin: TFloatSpinEdit;
    edValueSub1: TFloatSpinEdit;
    edValue2: TFloatSpinEdit;
    edYLineCaption: TEdit;
    edYLineAfter: TEdit;
    edYLineDigits: TSpinEdit;
    edCaptionTimerFormat: TEdit;
    BGRAMaxMProgress: TBGRAFlashProgressBar;
    edGraphValue: TFloatSpinEdit;
    edGraphValueY: TFloatSpinEdit;
    edMarqueeBounce: TSpinEdit;
    edMarqueeWidth: TBCTrackbarUpdown;
    edMax2: TFloatSpinEdit;
    edMaxY: TFloatSpinEdit;
    edMin2: TFloatSpinEdit;
    edMinY: TFloatSpinEdit;
    edValueSub: TFloatSpinEdit;
    edValue: TFloatSpinEdit;
    edValue1: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label10: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label24: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbCount: TLabel;
    PageControl1: TPageControl;
    edCaptionDigits: TSpinEdit;
    rgCaptionAlign: TRadioGroup;
    rgCaptionAlignM: TRadioGroup;
    rgMarqueeDirection: TRadioGroup;
    rgMarqueeSpeed: TRadioGroup;
    rgMarqueeWidthType: TRadioGroup;
    TabNormal: TTabSheet;
    TabMarquee: TTabSheet;
    TabMultiProgress: TTabSheet;
    TabTimer: TTabSheet;
    TabGraph: TTabSheet;
    TimeEdit1: TTimeEdit;
    procedure BCTrackbarUpdown1Change(Sender: TObject; AByUser: boolean);
    procedure BGRAMaxMProgressTimerEnd(Sender: TObject);
    procedure btBarColorClick(Sender: TObject);
    procedure btGraphAddValueClick(Sender: TObject);
    procedure btGraphTestClick(Sender: TObject);
    procedure btTimerPlayPause2Click(Sender: TObject);
    procedure btTimerPlayPauseClick(Sender: TObject);
    procedure btTimerStartClick(Sender: TObject);
    procedure cbBackgroundRandomChange(Sender: TObject);
    procedure cbCaptionPercentMChange(Sender: TObject);
    procedure cbMarqueeWidthChange(Sender: TObject);
    procedure cbCaptionPercentChange(Sender: TObject);
    procedure cbShowBarAnimationChange(Sender: TObject);
    procedure cbShowDividersChange(Sender: TObject);
    procedure cbShowDividersYChange(Sender: TObject);
    procedure cbShowYLineChange(Sender: TObject);
    procedure cbTimerAutoStartChange(Sender: TObject);
    procedure edCaptionChange(Sender: TObject);
    procedure edCaptionDigitsChange(Sender: TObject);
    procedure edMarqueeBounceChange(Sender: TObject);
    procedure edMaxChange(Sender: TObject);
    procedure edMaxYChange(Sender: TObject);
    procedure edMinChange(Sender: TObject);
    procedure edMinYChange(Sender: TObject);
    procedure edValueSubChange(Sender: TObject; AByUser: boolean);
    procedure edCaptionTimerFormatChange(Sender: TObject);
    procedure edValueChange(Sender: TObject; AByUser: boolean);
    procedure edMarqueeWidthChange(Sender: TObject; AByUser: boolean);
    procedure edYLineAfterChange(Sender: TObject);
    procedure edYLineCaptionChange(Sender: TObject);
    procedure edYLineDigitsChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure rgCaptionAlignClick(Sender: TObject);
    procedure rgCaptionAlignMClick(Sender: TObject);
    procedure rgMarqueeDirectionClick(Sender: TObject);
    procedure rgMarqueeWidthTypeClick(Sender: TObject);
    procedure rgMarqueeSpeedClick(Sender: TObject);
  private
    { private declarations }
    aCount: Integer;
    Closing: Boolean;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Math;

{$R *.lfm}

{ TForm1 }

procedure TForm1.btTimerPlayPause2Click(Sender: TObject);
begin
  BGRAMaxMProgress.StepIt(0);
end;

procedure TForm1.btTimerPlayPauseClick(Sender: TObject);
begin
  BGRAMaxMProgress.TimerPlayPause;
end;

procedure TForm1.btTimerStartClick(Sender: TObject);
begin
  BGRAMaxMProgress.TimerReStart;
end;

procedure TForm1.cbBackgroundRandomChange(Sender: TObject);
begin
  BGRAMaxMProgress.BackgroundRandomize:= cbBackgroundRandom.Checked;
end;

procedure TForm1.cbCaptionPercentMChange(Sender: TObject);
begin
  BGRAMaxMProgress.CaptionShowPercentSub:= cbCaptionPercentM.Checked;
end;

procedure TForm1.cbMarqueeWidthChange(Sender: TObject);
begin
  BGRAMaxMProgress.MarqueeWidth:= edMarqueeWidth.Value;
end;

procedure TForm1.cbCaptionPercentChange(Sender: TObject);
begin
  if Sender = cbCaptionPercent then
    cbCaptionPercent1.Checked := cbCaptionPercent.Checked
  else
    cbCaptionPercent.Checked := cbCaptionPercent1.Checked;
  BGRAMaxMProgress.CaptionShowPercent:= cbCaptionPercent.Checked;
end;

procedure TForm1.cbShowBarAnimationChange(Sender: TObject);
begin
  BGRAMaxMProgress.ShowBarAnimation:= cbShowBarAnimation.Checked;
end;

procedure TForm1.cbShowDividersChange(Sender: TObject);
begin
  BGRAMaxMProgress.ShowDividers:= cbShowDividers.Checked;
end;

procedure TForm1.cbShowDividersYChange(Sender: TObject);
begin
  BGRAMaxMProgress.GraphShowYDividers:= cbShowDividersY.Checked;
  if BGRAMaxMProgress.GraphShowYDividers then
  begin
    BGRAMaxMProgress.ShowDividers:= True;
    cbShowDividers.Checked:= True;
  end;
end;

procedure TForm1.cbShowYLineChange(Sender: TObject);
begin
  BGRAMaxMProgress.GraphShowYLine:= cbShowYLine.Checked;
  if BGRAMaxMProgress.GraphShowYLine then
  begin
    BGRAMaxMProgress.BackgroundColor:= clWhite;
    BGRAMaxMProgress.BackgroundRandomize:= False;
    cbBackgroundRandom.Checked:= False;
    btBackgroundColor.StateNormal.Color:= clWhite;
  end;
end;

procedure TForm1.cbTimerAutoStartChange(Sender: TObject);
begin
  BGRAMaxMProgress.TimerAutoRestart:= TCheckBox(Sender).Checked;
end;

procedure TForm1.edCaptionChange(Sender: TObject);
begin
  BGRAMaxMProgress.Caption:= edCaption.Text;
end;

procedure TForm1.edCaptionDigitsChange(Sender: TObject);
begin
  BGRAMaxMProgress.CaptionPercentDigits:= edCaptionDigits.Value;
end;

procedure TForm1.edMarqueeBounceChange(Sender: TObject);
begin
  BGRAMaxMProgress.MarqueeBounce:= edMarqueeBounce.Value;
end;

procedure TForm1.edMaxChange(Sender: TObject);
begin
  BGRAMaxMProgress.MaxValue:= TFloatSpinEdit(Sender).Value;
  edMax.Value:= BGRAMaxMProgress.MaxValue;
  edMax2.Value:= BGRAMaxMProgress.MaxValue;
  edValue.MaxValue:= BGRAMaxMProgress.MaxValue;
  edValue1.MaxValue:= BGRAMaxMProgress.MaxValue;
  edGraphValue.MaxValue:= BGRAMaxMProgress.MaxValue;
end;

procedure TForm1.edMaxYChange(Sender: TObject);
begin
  BGRAMaxMProgress.MaxYValue:= edMaxY.Value;
  edGraphValueY.MaxValue:= BGRAMaxMProgress.MaxYValue;
end;

procedure TForm1.edMinChange(Sender: TObject);
begin
  BGRAMaxMProgress.MinValue:= TFloatSpinEdit(Sender).Value;
  edMin.Value:= BGRAMaxMProgress.MinValue;
  edMin2.Value:= BGRAMaxMProgress.MinValue;
  edValue.MinValue:= BGRAMaxMProgress.MinValue;
  edValue1.MinValue:= BGRAMaxMProgress.MinValue;
  edGraphValue.MinValue:= BGRAMaxMProgress.MinValue;
end;

procedure TForm1.edMinYChange(Sender: TObject);
begin
  BGRAMaxMProgress.MinYValue:= edMinY.Value;
  edGraphValueY.MinValue:= BGRAMaxMProgress.MinYValue;
end;

procedure TForm1.edValueSubChange(Sender: TObject; AByUser: boolean);
begin
  BGRAMaxMProgress.ValueSub:= TFloatSpinEdit(Sender).Value;
  edValueSub.Value:= BGRAMaxMProgress.ValueSub;
  edValueSub1.Value:= BGRAMaxMProgress.ValueSub;
end;

procedure TForm1.edCaptionTimerFormatChange(Sender: TObject);
begin
  BGRAMaxMProgress.CaptionPercentTimerFormat:= edCaptionTimerFormat.Text;
end;

procedure TForm1.edValueChange(Sender: TObject; AByUser: boolean);
begin
  BGRAMaxMProgress.Value:= TFloatSpinEdit(Sender).Value;
  edValue.Value:= BGRAMaxMProgress.Value;
  edValue1.Value:= BGRAMaxMProgress.Value;
  edValue2.Value:= BGRAMaxMProgress.Value;
end;

procedure TForm1.edMarqueeWidthChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser then BGRAMaxMProgress.MarqueeWidth:= edMarqueeWidth.Value;
end;

procedure TForm1.edYLineAfterChange(Sender: TObject);
begin
  BGRAMaxMProgress.GraphYLineAfter:= edYLineAfter.Text;
end;

procedure TForm1.edYLineCaptionChange(Sender: TObject);
begin
  BGRAMaxMProgress.GraphYLineCaption:= edYLineCaption.Text;
end;

procedure TForm1.edYLineDigitsChange(Sender: TObject);
begin
  BGRAMaxMProgress.GraphYLineDigits:= edYLineDigits.Value;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Closing:= True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Closing:= False;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  PageControl1.ActivePage:= TabNormal;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  //Update Controls


  if (PageControl1.ActivePage.Tag = 4)
  then BGRAMaxMProgress.Height:= 100 //Graph
  else BGRAMaxMProgress.Height:= 34;

  if (PageControl1.ActivePage.Tag = 3) then
  begin
    //Timer
    aCount:= 0;
    BGRAMaxMProgress.MaxValue:= TimeEdit1.Time;
    BGRAMaxMProgress.TimerAutoRestart:= cbTimerAutoStart.Checked;
    BGRAMaxMProgress.CaptionShowPercent:= cbCaptionPercent1.Checked;
    BGRAMaxMProgress.CaptionPercentTimerFormat:= edCaptionTimerFormat.Text;
  end;

  BGRAMaxMProgress.Style:= TBGRAPBarStyle(PageControl1.ActivePage.Tag);
end;

procedure TForm1.rgCaptionAlignClick(Sender: TObject);
begin
  BGRAMaxMProgress.CaptionPercentAlign:= TAlignment(rgCaptionAlign.ItemIndex);
end;

procedure TForm1.rgCaptionAlignMClick(Sender: TObject);
begin
  BGRAMaxMProgress.CaptionPercentSubAlign:= TAlignment(rgCaptionAlignM.ItemIndex);
end;

procedure TForm1.rgMarqueeDirectionClick(Sender: TObject);
begin
  BGRAMaxMProgress.MarqueeDirection:= TBGRAPBarMarqueeDirection(rgMarqueeDirection.ItemIndex);
end;

procedure TForm1.rgMarqueeWidthTypeClick(Sender: TObject);
begin
  BGRAMaxMProgress.MarqueeWidthType:= TBGRAPBarMarqueeWidthType(rgMarqueeWidthType.ItemIndex);
//  edMarqueeWidth.Enabled:= (BGRAMaxMProgress.MarqueeWidthType = pbmwFixed);
end;

procedure TForm1.rgMarqueeSpeedClick(Sender: TObject);
begin
  BGRAMaxMProgress.MarqueeSpeed:= TBGRAPBarMarqueeSpeed(rgMarqueeSpeed.ItemIndex);
end;

procedure TForm1.BCTrackbarUpdown1Change(Sender: TObject; AByUser: boolean);
begin
  BGRAMaxMProgress.Value := edValue.Value;
end;

procedure TForm1.BGRAMaxMProgressTimerEnd(Sender: TObject);
begin
  inc(aCount);
  lbCount.Caption:= IntToStr(aCount);
end;

procedure TForm1.btBarColorClick(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    if Sender=btBarColor then BGRAMaxMProgress.BarColor:=ColorDialog1.Color
    else
    if Sender=btBarColorM then BGRAMaxMProgress.BarColorSub:=ColorDialog1.Color
    else
    if Sender=btBackgroundColor then BGRAMaxMProgress.BackgroundColor:=ColorDialog1.Color
    else
    if Sender=btFontColor then BGRAMaxMProgress.Font.Color:=ColorDialog1.Color;

    TColorSpeedButton(Sender).StateNormal.Color:=ColorDialog1.Color;
  end;
end;

procedure TForm1.btGraphAddValueClick(Sender: TObject);
begin
  BGRAMaxMProgress.SetValue(edGraphValue.Value, edGraphValueY.Value);
end;

procedure TForm1.btGraphTestClick(Sender: TObject);
var
   i,
   iStep,
   YVal: Double;

begin
  BGRAMaxMProgress.Style:= pbstGraph;
  BGRAMaxMProgress.Value:= 0;
  iStep:= (BGRAMaxMProgress.MaxValue-BGRAMaxMProgress.MinValue) / 100;
  YVal:= 50;
  Randomize;
  i:= BGRAMaxMProgress.MinValue;
  while (i < BGRAMaxMProgress.MaxValue) do
  begin
    i:= i+iStep;

    if cbRandG.Checked
    then YVal:= YVal+RandG(BGRAMaxMProgress.MinYValue, 10)
    else YVal:= Random * (BGRAMaxMProgress.MaxYValue - BGRAMaxMProgress.MinYValue) + BGRAMaxMProgress.MinYValue;

    BGRAMaxMProgress.SetValue(i, YVal);
    Application.ProcessMessages;
    Sleep(150);
  end;
end;

end.

