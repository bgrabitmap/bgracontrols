unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls,

  // DT Controls
  dtthemedgauge, dtthemedclock, DTAnalogClock, DTAnalogGauge;

type

  { TForm1 }

  TForm1 = class(TForm)
    DTAnalogClock1: TDTAnalogClock;
    DTAnalogGauge1: TDTAnalogGauge;
    DTClock1: TDTThemedClock;
    DTThemedClock1: TDTThemedClock;
    DTThemedGauge1: TDTThemedGauge;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  DTThemedGauge1.Position := TrackBar1.Position;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  DTAnalogGauge1.Position := TrackBar2.Position;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin

end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

end.

