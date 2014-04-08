unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, Spin, StdCtrls, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, TACustomSource, TAGraph, TASeries, TASources,
  TAAnimatedSource,
  BGRABitmap, BGRABitmapTypes, BGRASliceScaling, types;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnStartStop: TButton;
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Image1: TImage;
    lblSkipped: TLabel;
    ListChartSource1: TListChartSource;
    Panel1: TPanel;
    rgMethod: TRadioGroup;
    seTime: TSpinEdit;
    procedure btnStartStopClick(Sender: TObject);
    procedure Chart1BarSeries1BeforeDrawBar(ASender: TBarSeries;
      ACanvas: TCanvas; const ARect: TRect; APointIndex, AStackIndex: Integer;
      var ADoDefaultDrawing: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rgMethodClick(Sender: TObject);
    procedure seTimeChange(Sender: TObject);
  private
    FAnimatedSource: TCustomAnimatedChartSource;
    procedure OnGetItem(
      ASource: TCustomAnimatedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure OnStop(ASource: TCustomAnimatedChartSource);
  public
    sliceScaling: TBGRASliceScaling;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Math;

{ TForm1 }

procedure TForm1.btnStartStopClick(Sender: TObject);
begin
  if FAnimatedSource.IsAnimating then
    FAnimatedSource.Stop
  else
    FAnimatedSource.Start;
end;

procedure TForm1.Chart1BarSeries1BeforeDrawBar(ASender: TBarSeries;
  ACanvas: TCanvas; const ARect: TRect; APointIndex, AStackIndex: Integer;
  var ADoDefaultDrawing: Boolean);
var
  temp: TBGRABitmap;
begin
  temp := TBGRABitmap.Create(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
  sliceScaling.Draw(temp, 0, 0, temp.Width, temp.Height, False);
  temp.Draw(ACanvas, ARect.Left, ARect.Top, False);
  temp.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FAnimatedSource := TCustomAnimatedChartSource.Create(Self);
  FAnimatedSource.Origin := ListChartSource1;
  FAnimatedSource.AnimationInterval := 30;
  FAnimatedSource.OnGetItem := @OnGetItem;
  FAnimatedSource.OnStop := @OnStop;
  seTimeChange(nil);
  Chart1BarSeries1.Source := FAnimatedSource;
  FAnimatedSource.Start;

  sliceScaling := TBGRASliceScaling.Create(Image1.Picture.Bitmap, 70, 0, 35, 0);
  //sliceScaling.ResampleMode := rmSimpleStretch;
  sliceScaling.AutodetectRepeat;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  sliceScaling.Free;
end;

procedure TForm1.OnGetItem(
  ASource: TCustomAnimatedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
begin
  case rgMethod.ItemIndex of
  0: AItem.Y *= ASource.Progress;
  1:
    if ASource.Count * ASource.Progress < AIndex then
      AItem.Y := 0;
  2:
    case Sign(Trunc(ASource.Count * ASource.Progress) - AIndex) of
      0: AItem.Y *= Frac(ASource.Count * ASource.Progress);
      -1: AItem.Y := 0;
    end;
  end;
end;

procedure TForm1.OnStop(ASource: TCustomAnimatedChartSource);
begin
  lblSkipped.Caption := Format('Skipped frames: %d', [ASource.SkippedFramesCount]);
end;

procedure TForm1.rgMethodClick(Sender: TObject);
begin
  FAnimatedSource.Start;
end;

procedure TForm1.seTimeChange(Sender: TObject);
begin
  FAnimatedSource.AnimationTime := seTime.Value;
end;

end.

