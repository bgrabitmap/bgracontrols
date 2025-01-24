unit bcmaterialedit_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DividerBevel, BCMaterialEdit, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCMaterialEdit1: TBCMaterialEdit;
    Button1: TButton;
    AnchorsCheckGroup: TCheckGroup;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    ColorButton1: TColorButton;
    ColorDialog1: TColorDialog;
    DividerBevel1: TDividerBevel;
    EventsCheckGroup: TCheckGroup;
    Label1: TMemo;
    Panel1: TPanel;
    procedure BCMaterialEdit1Change(Sender: TObject);
    procedure BCMaterialEdit1ChangeBounds(Sender: TObject);
    procedure BCMaterialEdit1Click(Sender: TObject);
    procedure BCMaterialEdit1DbClick(Sender: TObject);
    procedure BCMaterialEdit1EditingDone(Sender: TObject);
    procedure BCMaterialEdit1Enter(Sender: TObject);
    procedure BCMaterialEdit1Exit(Sender: TObject);
    procedure BCMaterialEdit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BCMaterialEdit1KeyPress(Sender: TObject; var Key: char);
    procedure BCMaterialEdit1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BCMaterialEdit1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BCMaterialEdit1MouseEnter(Sender: TObject);
    procedure BCMaterialEdit1MouseLeave(Sender: TObject);
    procedure BCMaterialEdit1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BCMaterialEdit1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BCMaterialEdit1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure BCMaterialEdit1MouseWheelDown(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure BCMaterialEdit1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure BCMaterialEdit1Resize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure AnchorsCheckGroupItemClick(Sender: TObject; Index: integer);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ColorButton1ColorChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BCMaterialEdit1Change(Sender: TObject);
begin
  if EventsCheckGroup.Checked[0] then Label1.Append('OnChange');
end;

procedure TForm1.BCMaterialEdit1ChangeBounds(Sender: TObject);
begin
  if EventsCheckGroup.Checked[1] then Label1.Append('OnChangeBounds');
end;

procedure TForm1.BCMaterialEdit1Click(Sender: TObject);
begin
  if EventsCheckGroup.Checked[2] then Label1.Append('OnClick');
end;

procedure TForm1.BCMaterialEdit1DbClick(Sender: TObject);
begin
  if EventsCheckGroup.Checked[3] then Label1.Append('OnDbClick');
end;

procedure TForm1.BCMaterialEdit1EditingDone(Sender: TObject);
begin
  if EventsCheckGroup.Checked[4] then Label1.Append('OnEditingDone');
end;

procedure TForm1.BCMaterialEdit1Enter(Sender: TObject);
begin
  if EventsCheckGroup.Checked[5] then Label1.Append('OnEnter');
end;

procedure TForm1.BCMaterialEdit1Exit(Sender: TObject);
begin
  if EventsCheckGroup.Checked[6] then Label1.Append('OnExit');
end;

procedure TForm1.BCMaterialEdit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if EventsCheckGroup.Checked[7] then Label1.Append('OnKeyDown');
end;

procedure TForm1.BCMaterialEdit1KeyPress(Sender: TObject; var Key: char);
begin
  if EventsCheckGroup.Checked[8] then Label1.Append('OnKeyPress');
end;

procedure TForm1.BCMaterialEdit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if EventsCheckGroup.Checked[9] then Label1.Append('OnKeyUp');
end;

procedure TForm1.BCMaterialEdit1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if EventsCheckGroup.Checked[10] then Label1.Append('OnMouseDown');
end;

procedure TForm1.BCMaterialEdit1MouseEnter(Sender: TObject);
begin
  if EventsCheckGroup.Checked[11] then Label1.Append('OnMouseEnter');
end;

procedure TForm1.BCMaterialEdit1MouseLeave(Sender: TObject);
begin
  if EventsCheckGroup.Checked[12] then Label1.Append('OnMouseLeave');
end;

procedure TForm1.BCMaterialEdit1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if EventsCheckGroup.Checked[13] then Label1.Append('OnMouseMove');
end;

procedure TForm1.BCMaterialEdit1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if EventsCheckGroup.Checked[14] then Label1.Append('OnMouseUp');
end;

procedure TForm1.BCMaterialEdit1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if EventsCheckGroup.Checked[15] then Label1.Append('OnMouseWheel');
end;

procedure TForm1.BCMaterialEdit1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if EventsCheckGroup.Checked[16] then Label1.Append('OnMouseWheelDown');
end;

procedure TForm1.BCMaterialEdit1MouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if EventsCheckGroup.Checked[17] then Label1.Append('OnMouseWheelUp');
end;

procedure TForm1.BCMaterialEdit1Resize(Sender: TObject);
begin
  if EventsCheckGroup.Checked[18] then Label1.Append('OnResize');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Panel1.Color = clBlack then
  begin
    Panel1.Color := clWhite;
    BCMaterialEdit1.Color := clWhite;
    BCMaterialEdit1.Font.Color := clBlack;
    BCMaterialEdit1.Edit.Color := clWhite;
    BCMaterialEdit1.Edit.Font.Color := clBlack;
  end else
  begin
    Panel1.Color := clBlack;
    BCMaterialEdit1.Color := clBlack;
    {$IFDEF LCLGTK2}
    BCMaterialEdit1.Font.Color := clBlack;
    BCMaterialEdit1.Edit.Color := clWhite;
    BCMaterialEdit1.Edit.Font.Color := clBlack;
    {$ELSE}
    BCMaterialEdit1.Font.Color := clWhite;
    BCMaterialEdit1.Edit.Color := clBlack;
    BCMaterialEdit1.Edit.Font.Color := clWhite;
    {$ENDIF}
  end;
end;

procedure TForm1.AnchorsCheckGroupItemClick(Sender: TObject; Index: integer);
var
  NewAnchors: TAnchors;
begin
  NewAnchors := [];
  if AnchorsCheckGroup.Checked[0] then Include(NewAnchors, akTop);
  if AnchorsCheckGroup.Checked[1] then Include(NewAnchors, akLeft);
  if AnchorsCheckGroup.Checked[2] then Include(NewAnchors, akRight);
  if AnchorsCheckGroup.Checked[3] then Include(NewAnchors, akBottom);
  BCMaterialEdit1.Anchors := NewAnchors;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Label1.Clear;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if Assigned(Sender) then
    BCMaterialEdit1.AutoSize := not BCMaterialEdit1.AutoSize;

  case BCMaterialEdit1.AutoSize of
    True:  Button3.Caption := 'AutoSize is TRUE';
    False: Button3.Caption := 'AutoSize is FALSE';
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if Assigned(Sender) then
    BCMaterialEdit1.Enabled := not BCMaterialEdit1.Enabled;

  case BCMaterialEdit1.Enabled of
    True:  Button4.Caption := 'Enabled is TRUE';
    False: Button4.Caption := 'Enabled is FALSE';
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  if Assigned(Sender) then
    BCMaterialEdit1.Visible := not BCMaterialEdit1.Visible;

  case BCMaterialEdit1.Visible of
    True:  Button5.Caption := 'Visible is TRUE';
    False: Button5.Caption := 'Visible is FALSE';
  end;
end;

procedure TForm1.ColorButton1ColorChanged(Sender: TObject);
begin
  BCMaterialEdit1.Color := ColorDialog1.Color;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Button3Click(nil);
  Button4Click(nil);
  Button5Click(nil);
end;

end.

