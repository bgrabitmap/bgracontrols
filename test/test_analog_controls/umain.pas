unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls,

  // DT Controls
  dtthemedgauge, dtthemedclock, DTAnalogClock;

type

  { TForm1 }

  TForm1 = class(TForm)
    DTAnalogClock1: TDTAnalogClock;
    DTClock1: TDTThemedClock;
    DTThemedClock1: TDTThemedClock;
    DTThemedGauge1: TDTThemedGauge;
    TrackBar1: TTrackBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
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

