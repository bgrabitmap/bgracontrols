unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, BGRAGraphicControl, BGRABitmap, BGRABitmapTypes, BCFilters, Spin, BCEffect;

type
  { TForm1 }

  TForm1 = class(TForm)
    bgImage: TBGRAGraphicControl;
    cbFadingMode: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    seFadingStep: TSpinEdit;
    seTimerInterval: TSpinEdit;
    seDuration: TSpinEdit;
    Timer1: TTimer;
    procedure bgImageRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure cbFadingModeChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure seDurationChange(Sender: TObject);
    procedure seFadingStepChange(Sender: TObject);
    procedure seTimerIntervalChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Fade: TFading;
    {$ifdef Windows}
    FadeForm: TFading;
    {$endif}
    Image: TBGRABitmap;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.bgImageRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  // set black background
  Bitmap.Fill(BGRABlack);
  // draw image with fading alpha
  Bitmap.BlendImageOver(0, 0, Image, boLinearBlend, Fade.Alpha);
end;

procedure TForm1.cbFadingModeChange(Sender: TObject);
begin
  // set fading mode
  Fade.Mode := StrToTFadingMode(cbFadingMode.Caption);
  // reset
  Fade.Reset;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  {$ifdef Windows}
  if (FadeForm.Alpha > 0) then
  begin
    FadeForm.Mode := fmFadeOut;
    CanClose := False;
  end
  else
    CanClose := True;
  {$endif}
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // add items to combo box
  FadingModeStrList(cbFadingMode.Items);
  // load step from spin edit
  Fade.Step := seFadingStep.Value;
  // load interval from spin edit
  Timer1.Interval := seTimerInterval.Value;

  // create test image
  Image := TBGRABitmap.Create(bgImage.Width, bgImage.Height);
  NoiseBWA(Image);

  // fade form Windows
  {$ifdef Windows}
  Self.DoubleBuffered := True;
  Self.AlphaBlend := True;
  Self.AlphaBlendValue := 0;
  FadeForm.Mode := fmFadeIn;
  FadeForm.Duration := 300;
  {$endif}
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // free test image
  Image.Free;
end;

procedure TForm1.seDurationChange(Sender: TObject);
begin
  Fade.Duration := seDuration.Value;
end;

procedure TForm1.seFadingStepChange(Sender: TObject);
begin
  // set fading step
  Fade.Step := seFadingStep.Value;
end;

procedure TForm1.seTimerIntervalChange(Sender: TObject);
begin
  // change timer interval
  Timer1.Interval := seTimerInterval.Value;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:= false; //avoid freezing application

  { calculate fading  // display alpha }
  Label3.Caption := concat('FadingAlpha: ', IntToStr(Fade.Execute));

  // update bitmap
  bgImage.RedrawBitmap;

  {$ifdef Windows}
  if FadeForm.Mode <> fmSuspended then
    Self.AlphaBlendValue := FadeForm.Execute;

  if (FadeForm.Mode = fmSuspended) and (FadeForm.Alpha = 0) then
    Close;
  {$endif}
  Timer1.Enabled:= true;
end;

end.
