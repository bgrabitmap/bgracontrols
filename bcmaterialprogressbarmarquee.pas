unit BCMaterialProgressBarMarquee;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, BGRAGraphicControl,
  ExtCtrls, BGRABitmap, BGRABitmapTypes;

type

  { TBCMaterialProgressBarMarquee }

  TBCMaterialProgressBarMarquee = class(TBGRAGraphicControl)
  private
    FBarColor: TColor;
    progressbasr_cx, progressbar_cw: integer;
    progressbar_x, progressbar_w: integer;
    progressbar_increase: boolean;
    FTimer: TTimer;
    procedure SetBarColor(AValue: TColor);
    procedure TimerOnTimer(Sender: TObject);
  protected
    procedure SetEnabled(Value: Boolean); override;
    procedure SetVisible(Value: Boolean); override;
  public
    procedure DiscardBitmap;
    procedure RedrawBitmapContent; override;
    constructor Create(AOwner: TComponent); override;
  published
    property BarColor: TColor read FBarColor write SetBarColor;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCMaterialProgressBarMarquee]);
end;

{ TBCMaterialProgressBarMarquee }

procedure TBCMaterialProgressBarMarquee.TimerOnTimer(Sender: TObject);
begin
  if progressbar_increase then
  begin
    progressbar_w := progressbar_w + progressbar_cw;
    if (progressbar_w >= Width - 5) then
    begin
      progressbar_increase := False;
    end;
  end
  else
  begin
    progressbar_w := progressbar_w - progressbar_cw;
    if (progressbar_w <= progressbar_cw) then
    begin
      progressbar_increase := True;
    end;
  end;
  progressbar_x := progressbar_x + progressbasr_cx;
  if (progressbar_x >= Width) then
    progressbar_x := -progressbar_w;
  DiscardBitmap;
end;

procedure TBCMaterialProgressBarMarquee.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  FTimer.Enabled := Value and Visible;
  DiscardBitmap;
end;

procedure TBCMaterialProgressBarMarquee.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  FTimer.Enabled := Enabled and Value;
  DiscardBitmap;
end;

procedure TBCMaterialProgressBarMarquee.SetBarColor(AValue: TColor);
begin
  if FBarColor = AValue then
    Exit;
  FBarColor := AValue;
  DiscardBitmap;
end;

procedure TBCMaterialProgressBarMarquee.DiscardBitmap;
begin
  inherited DiscardBitmap;
  progressbar_cw := Width div 50;
  progressbasr_cx := progressbar_cw * 2;
end;

procedure TBCMaterialProgressBarMarquee.RedrawBitmapContent;
begin
  if FTimer.Enabled then
  begin
  Bitmap.Fill(Color);
  Bitmap.Rectangle(Rect(progressbar_x, 0, progressbar_x + progressbar_w, Bitmap.Height),
    BarColor, BarColor);
  end
  else
  begin
    Bitmap.Fill(BarColor);
  end;
end;

constructor TBCMaterialProgressBarMarquee.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  progressbar_w := Width;
  progressbar_x := -progressbar_w;
  progressbar_increase := False;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 15;
  FTimer.OnTimer := TimerOnTimer;
  FTimer.Enabled := True;
  Color := clWhite;
  BarColor := $00E2A366;
end;

end.
