unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, superspinner,
  supergauge;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SuperSpinner1: TSuperSpinner;
    procedure FormCreate(Sender: TObject);
    procedure SuperSpinner1CapClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState);
    procedure SuperSpinner1Click(Sender: TObject);
    procedure SuperSpinner1DblClick(Sender: TObject);
    procedure SuperSpinner1KnobClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState);
    procedure SuperSpinner1MouseCapEnter(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure SuperSpinner1MouseCapLeave(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure SuperSpinner1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SuperSpinner1MouseEnter(Sender: TObject);
    procedure SuperSpinner1MouseKnobEnter(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure SuperSpinner1MouseKnobLeave(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure SuperSpinner1MouseLeave(Sender: TObject);
    procedure SuperSpinner1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  //SS1.ParentFont := False;
end;

procedure TForm1.SuperSpinner1CapClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState);
begin
  Label1.Caption := 'Mouse CapClick';
end;

procedure TForm1.SuperSpinner1Click(Sender: TObject);
begin
    Label2.Caption := 'Click';
end;

procedure TForm1.SuperSpinner1DblClick(Sender: TObject);
begin
    Label2.Caption := 'DBL Click';
end;

procedure TForm1.SuperSpinner1KnobClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState);
begin
  Label1.Caption := 'Mouse KnobClick';
end;

procedure TForm1.SuperSpinner1MouseCapEnter(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Label2.Caption := 'Mouse Cap Enter';
end;

procedure TForm1.SuperSpinner1MouseCapLeave(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Label2.Caption := 'Mouse Cap Leave';
end;

procedure TForm1.SuperSpinner1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    Label2.Caption := 'Mouse Down';
end;

procedure TForm1.SuperSpinner1MouseEnter(Sender: TObject);
begin
  Label2.Caption := 'IN';
end;

procedure TForm1.SuperSpinner1MouseKnobEnter(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Label1.Caption := 'Mouse Knob Enter';
end;

procedure TForm1.SuperSpinner1MouseKnobLeave(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Label1.Caption := 'Mouse Knob Leave';
end;

procedure TForm1.SuperSpinner1MouseLeave(Sender: TObject);
begin
  Label2.Caption := 'OUT';
end;

procedure TForm1.SuperSpinner1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Label2.Caption := 'Mouse Up';
end;

end.

