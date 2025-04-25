unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  superspinner, superspinnercommon;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRASpinner1: TBGRASpinner;
    BGRASpinner2: TBGRASpinner;
    BitBtn1: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure BGRASpinner1CapClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState);
    procedure BGRASpinner1KnobClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState);
    procedure BGRASpinner1PosChanged(Sender: TObject; Shift: TShiftState;
      Value: single; MoveDir: TBGRASpinnerDirection);
    procedure BGRASpinner2PosChanged(Sender: TObject; Shift: TShiftState;
      Value: single; MoveDir: TBGRASpinnerDirection);
    procedure BGRASpinner2SpinnerDebug(Sender: TObject; DbgStr: string);
    procedure BGRASpinner2Wrapped(Sender: TObject; Shift: TShiftState;
      OldAngle, NewAngle: single; MoveDir: TBGRASpinnerDirection);
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCount: integer;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BGRASpinner1CapClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState);
begin
  if Button = mbLeft then
    begin

      if BGRASpinner1.CapSettings.FillColor = clWhite then
        BGRASpinner1.CapSettings.FillColor := clRed
      else
        BGRASpinner1.CapSettings.FillColor := clWhite;

      BGRASPinner1.Bump(sdCCW, 1.0);
    end;

if Button = mbRight then
  begin

   if BGRASpinner1.CapSettings.FillColor = clWhite then
     BGRASpinner1.CapSettings.FillColor := clGreen
   else
     BGRASpinner1.CapSettings.FillColor := clWhite;

   BGRASPinner1.Bump(sdCW, 1.0);
  end;

end;

procedure TForm1.BGRASpinner1KnobClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState);
begin
  if Button = mbLeft then
    begin
      if BGRASpinner1.KnobSettings.FillColor = clWhite then
        BGRASpinner1.KnobSettings.FillColor := clYellow
      else
        BGRASpinner1.KnobSettings.FillColor := clWhite;
    end;

  if Button = mbRight then
  begin

    if BGRASpinner1.KnobSettings.FillColor = clWhite then
      BGRASpinner1.KnobSettings.FillColor := clBlue
    else
      BGRASpinner1.KnobSettings.FillColor := clWhite;
  end;
end;

procedure TForm1.BGRASpinner1PosChanged(Sender: TObject; Shift: TShiftState;
  Value: single; MoveDir: TBGRASpinnerDirection);
var
  Direction: integer;
  DirectionStr: string;

begin
  if MoveDir = sdCW then
    begin
      Direction := 1;
      DirectionStr := 'sdCW';
    end
  else
  begin
    Direction := -1;
    DirectionStr := 'sdCCW';
  end;

  if ssShift in Shift then
  begin
    Label1.Caption := 'Shift Down';
    Direction := Direction * 10;
    BGRASpinner1.WheelSpeed := 50;
  end
  else
    begin
      Label1.Caption := '';
      BGRASpinner1.WheelSpeed := 10;
    end;

  Label2.Caption :=FloatToStr(value);
  Label3.Caption := DirectionStr;
  FCount := FCount + Direction;
  Label5.Caption := IntToStr(FCount);
  BGRASpinner2.Angle:=BgraSpinner1.Angle;
  Label6.Caption := FloatToStr(BGRASpinner2.Angle);
end;

procedure TForm1.BGRASpinner2PosChanged(Sender: TObject; Shift: TShiftState;
  Value: single; MoveDir: TBGRASpinnerDirection);
var
  Direction: integer;
  DirectionStr: string;

begin
  if MoveDir = sdCW then
    begin
      Direction := 1;
      DirectionStr := 'sdCW';
    end
  else
  begin
    Direction := -1;
    DirectionStr := 'sdCCW';
  end;

  Label6.Caption:=FloatToStr(Value);
  Label3.Caption := DirectionStr;
  FCount := FCount+ Direction;
  Label5.Caption := IntToStr(FCount);
end;

procedure TForm1.BGRASpinner2SpinnerDebug(Sender: TObject; DbgStr: string);
begin
  Label1.Caption := DbgStr;
end;

procedure TForm1.BGRASpinner2Wrapped(Sender: TObject; Shift: TShiftState;
  OldAngle, NewAngle: single; MoveDir: TBGRASpinnerDirection);
var
  DirectionStr: string;

begin

  // make a method to decode

  if MoveDir = sdCW then
    begin
      DirectionStr := 'sdCW';
    end
  else
  begin
    DirectionStr := 'sdCCW';
  end;

  Label7.Caption := 'Wrap@ Old: ' + FloatToStr(OldAngle) + ' New: ' + FloatToStr(NewAngle) + ' Dir: ' + DirectionStr;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  i, j : integer;
begin
    for j := 0 to 10 do
  begin
      for i := 0 to 100 do
      begin
        BGRASpinner1.Angle := i;
        Application.ProcessMessages;
      end;
  end;
  beep;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  BGRASpinner2.Bump(sdCCW, 1.0);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  BGRASpinner2.Bump(sdCW, 1.0);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  BGRASpinner2.Spin(sdCW, 0.1, 72);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  BGRASpinner2.Spin(sdCW, 1.0, 71);
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  BGRASpinner1.Locked := not BGRASpinner1.Locked;
  if(BGRASpinner1.Locked) then
    Button6.Caption := 'Locked'
  else
    Button6.Caption := 'Not Locked';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCount := 0;
end;

end.

