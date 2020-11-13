unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  BCMDButton;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    BCMDButton1: TBCMDButton;
    BCMDButton10: TBCMDButton;
    BCMDButton11: TBCMDButton;
    BCMDButton15: TBCMDButton;
    BCMDButton16: TBCMDButton;
    BCMDButton17: TBCMDButton;
    BCMDButton2: TBCMDButton;
    BCMDButton27: TBCMDButton;
    BCMDButton28: TBCMDButton;
    BCMDButton29: TBCMDButton;
    BCMDButton30: TBCMDButton;
    BCMDButton31: TBCMDButton;
    BCMDButton32: TBCMDButton;
    BCMDButton5: TBCMDButton;
    mdGetRadio: TBCMDButton;
    mdSelect: TBCMDButton;
    mdUnselect: TBCMDButton;
    mdInvert: TBCMDButton;
    BCMDButton33: TBCMDButton;
    BCMDButton34: TBCMDButton;
    BCMDButton35: TBCMDButton;
    mdAnimations: TBCMDButton;
    mdGet: TBCMDButton;
    BCMDButton9: TBCMDButton;
    Panel1: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel2: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure MDButton1Click(Sender: TObject);
    procedure mdSelectClick(Sender: TObject);
    procedure mdUnselectClick(Sender: TObject);
    procedure mdInvertClick(Sender: TObject);
    procedure mdAnimationsClick(Sender: TObject);
    procedure mdGetRadioClick(Sender: TObject);
    procedure mdGetClick(Sender: TObject);
  private
    procedure EnableAnimations(Control: TControl);
    procedure DoubleBuffering(Control: TControl);
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.mdSelectClick(Sender: TObject);
begin
  BCMDButton33.SelectAll;
end;

procedure TfrmMain.MDButton1Click(Sender: TObject);
begin
  ShowMessage('Hello World');
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  {$ifdef windows}
  DoubleBuffering(Self);
  {$endif}
end;

procedure TfrmMain.mdUnselectClick(Sender: TObject);
begin
  BCMDButton33.UnselectAll;
end;

procedure TfrmMain.mdInvertClick(Sender: TObject);
begin
  BCMDButton33.InvertSelection;
end;

procedure TfrmMain.mdAnimationsClick(Sender: TObject);
begin
  EnableAnimations(Self);
  // Refresh controls
  Invalidate;
end;

procedure TfrmMain.mdGetRadioClick(Sender: TObject);
begin
  with BCMDButton27.GetSelected do
  begin
    ShowMessage(Text);
    Free;
  end;
end;

procedure TfrmMain.mdGetClick(Sender: TObject);
begin
  with BCMDButton33.GetSelected do
  begin
    ShowMessage(Text);
    Free;
  end;
end;

procedure TfrmMain.EnableAnimations(Control: TControl);
var
  i: integer;
  wincontrol: TWinControl;
begin
  if Control is TBCMDButton then
    TBCMDButton(Control).Animation := mdAnimations.Checked;
  if Control is TWinControl then
  begin
    wincontrol := TWinControl(Control);
    if wincontrol.ControlCount > 0 then
      for i := 0 to wincontrol.ControlCount - 1 do
        EnableAnimations(wincontrol.Controls[i]);
  end;
end;

procedure TfrmMain.DoubleBuffering(Control: TControl);
var
  i: integer;
  wincontrol: TWinControl;
begin
  if Control is TWinControl then
  begin
    wincontrol := TWinControl(Control);
    wincontrol.DoubleBuffered := True;
    if wincontrol.ControlCount > 0 then
      for i := 0 to wincontrol.ControlCount - 1 do
        DoubleBuffering(wincontrol.Controls[i]);
  end;
end;

end.

