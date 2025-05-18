// SPDX-License-Identifier: LGPL-3.0-linking-exception
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCKeyboard;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LCLType, LResources, LMessages,{$ENDIF}Forms, Controls, Graphics, Dialogs,
  {$IFNDEF FPC}Types, Windows, Messages, BGRAGraphics, GraphType, FPImage, BCBaseCtrls,{$ENDIF}
  BCThemeManager, BCButton, BCPanel, MouseAndKeyInput;

type

  { TBCKeyboard }

  TBCKeyboard = class(TComponent)
  private
    FBCThemeManager: TBCThemeManager;
    FButton: TBCButton;
    FOnUserChange: TNotifyEvent;
    FPanel, FRow1, FRow2, FRow3, FRow4: TBCPanel;
    FPanelsColor: TColor;
    F_q, F_w, F_e, F_r, F_t, F_y, F_u, F_i, F_o, F_p, F_a, F_s, F_d,
    F_f, F_g, F_h, F_j, F_k, F_l, F_z, F_x, F_c, F_v, F_b, F_n, F_m,
    F_shift, F_space, F_back: TBCButton;
    FVisible: boolean;
    procedure SetFButton(AValue: TBCButton);
    procedure SetFPanel(AValue: TBCPanel);
    procedure SetFPanelsColor(AValue: TColor);
    procedure SetFThemeManager(AValue: TBCThemeManager);
  protected
    procedure PressVirtKey(p: PtrInt);
    procedure PressShiftVirtKey(p: PtrInt);
    procedure OnButtonClick(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer); virtual;
    { When value is changed by the user }
    property OnUserChange: TNotifyEvent read FOnUserChange write FOnUserChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Show in a custom form or panel
    procedure Show(AControl: TWinControl); overload;
    // Try to Show in the form where this component is placed
    procedure Show(); overload;
    // Hide the component
    procedure Hide();
    // Update buttons style
    procedure UpdateButtonStyle;
  public
    { The real panel that's used as container for all the numeric buttons }
    property Panel: TBCPanel read FPanel write SetFPanel;
    { The color of all the panels involved in the control }
    property PanelsColor: TColor read FPanelsColor write SetFPanelsColor;
    { A fake button that's used as style base for all the numeric buttons }
    property ButtonStyle: TBCButton read FButton write SetFButton;
    { If it's visible or not }
    property Visible: boolean read FVisible;
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFThemeManager;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

{$IFDEF FPC}procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCKeyboard]);
end;
{$ENDIF}

{ TBCKeyboard }

procedure TBCKeyboard.SetFThemeManager(AValue: TBCThemeManager);
begin
  if FBCThemeManager = AValue then
    Exit;
  FBCThemeManager := AValue;
end;

procedure TBCKeyboard.PressVirtKey(p: PtrInt);
begin
  KeyInput.Down(p);
  KeyInput.Up(p);
end;

procedure TBCKeyboard.PressShiftVirtKey(p: PtrInt);
begin
  KeyInput.Down(VK_SHIFT);
  KeyInput.Down(p);
  KeyInput.Up(p);
  KeyInput.Up(VK_SHIFT);
end;

procedure TBCKeyboard.OnButtonClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  btn: TBCButton;
  str: string;
begin
  btn := TBCButton(Sender);
  str := btn.Caption;

  if str = F_shift.Caption then
  begin
    F_shift.Down := not F_shift.Down;
    if not F_shift.Down then
    begin
      F_q.Caption := LowerCase(F_q.Caption);
      F_w.Caption := LowerCase(F_w.Caption);
      F_e.Caption := LowerCase(F_e.Caption);
      F_r.Caption := LowerCase(F_r.Caption);
      F_t.Caption := LowerCase(F_t.Caption);
      F_y.Caption := LowerCase(F_y.Caption);
      F_u.Caption := LowerCase(F_u.Caption);
      F_i.Caption := LowerCase(F_i.Caption);
      F_o.Caption := LowerCase(F_o.Caption);
      F_p.Caption := LowerCase(F_p.Caption);
      F_a.Caption := LowerCase(F_a.Caption);
      F_s.Caption := LowerCase(F_s.Caption);
      F_d.Caption := LowerCase(F_d.Caption);
      F_f.Caption := LowerCase(F_f.Caption);
      F_g.Caption := LowerCase(F_g.Caption);
      F_h.Caption := LowerCase(F_h.Caption);
      F_j.Caption := LowerCase(F_j.Caption);
      F_k.Caption := LowerCase(F_k.Caption);
      F_l.Caption := LowerCase(F_l.Caption);
      F_z.Caption := LowerCase(F_z.Caption);
      F_x.Caption := LowerCase(F_x.Caption);
      F_c.Caption := LowerCase(F_c.Caption);
      F_v.Caption := LowerCase(F_v.Caption);
      F_b.Caption := LowerCase(F_b.Caption);
      F_n.Caption := LowerCase(F_n.Caption);
      F_m.Caption := LowerCase(F_m.Caption);
    end
    else
    begin
      F_q.Caption := UpperCase(F_q.Caption);
      F_w.Caption := UpperCase(F_w.Caption);
      F_e.Caption := UpperCase(F_e.Caption);
      F_r.Caption := UpperCase(F_r.Caption);
      F_t.Caption := UpperCase(F_t.Caption);
      F_y.Caption := UpperCase(F_y.Caption);
      F_u.Caption := UpperCase(F_u.Caption);
      F_i.Caption := UpperCase(F_i.Caption);
      F_o.Caption := UpperCase(F_o.Caption);
      F_p.Caption := UpperCase(F_p.Caption);
      F_a.Caption := UpperCase(F_a.Caption);
      F_s.Caption := UpperCase(F_s.Caption);
      F_d.Caption := UpperCase(F_d.Caption);
      F_f.Caption := UpperCase(F_f.Caption);
      F_g.Caption := UpperCase(F_g.Caption);
      F_h.Caption := UpperCase(F_h.Caption);
      F_j.Caption := UpperCase(F_j.Caption);
      F_k.Caption := UpperCase(F_k.Caption);
      F_l.Caption := UpperCase(F_l.Caption);
      F_z.Caption := UpperCase(F_z.Caption);
      F_x.Caption := UpperCase(F_x.Caption);
      F_c.Caption := UpperCase(F_c.Caption);
      F_v.Caption := UpperCase(F_v.Caption);
      F_b.Caption := UpperCase(F_b.Caption);
      F_n.Caption := UpperCase(F_n.Caption);
      F_m.Caption := UpperCase(F_m.Caption);
    end;
  end
  else if str = F_back.Caption then
  begin
    {$IFDEF FPC}
    Application.QueueAsyncCall(PressVirtKey, VK_BACK);
    {$ELSE}
    SendKey(VK_BACK);
    {$ENDIF}
  end
  else
  begin
    if str = F_space.Caption then
      str := ' ';
    if F_shift.Down then
      {$IFDEF FPC}
      Application.QueueAsyncCall(PressShiftVirtKey, Ord(UpperCase(str)[1]))
      {$ELSE}
      SendKey(Ord(UpperCase(str)[1]), Shift)
      {$ENDIF}
    else
      {$IFDEF FPC}
      Application.QueueAsyncCall(PressVirtKey, Ord(UpperCase(str)[1]));
      {$ELSE}
      SendKey(Ord(UpperCase(str)[1]))
      {$ENDIF}
  end;

  if Assigned(FOnUserChange) then
    FOnUserChange(Self);
end;

constructor TBCKeyboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FVisible := False;

  FButton := TBCButton.Create(Self);

  FPanel := TBCPanel.Create(Self);
  FPanel.AutoSize := True;
  FPanel.ChildSizing.ControlsPerLine := 1;
  FPanel.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  FPanel.Caption := 'Panel1';
  FPanel.BorderBCStyle := bpsBorder;

  { qwertyuiop }

  FRow1 := TBCPanel.Create(FPanel);
  FRow1.AutoSize := True;
  FRow1.Caption := '';
  FRow1.BorderBCStyle := bpsBorder;
  FRow1.ChildSizing.ControlsPerLine := 10;
  FRow1.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  FRow1.Parent := FPanel;

  F_q := TBCButton.Create(FRow1);
  F_q.Caption := 'Q';
  F_q.Parent := FRow1;
  F_q.OnMouseDown := OnButtonClick;

  F_w := TBCButton.Create(FRow1);
  F_w.Caption := 'W';
  F_w.Parent := FRow1;
  F_w.OnMouseDown := OnButtonClick;

  F_e := TBCButton.Create(FRow1);
  F_e.Caption := 'E';
  F_e.Parent := FRow1;
  F_e.OnMouseDown := OnButtonClick;

  F_r := TBCButton.Create(FRow1);
  F_r.Caption := 'R';
  F_r.Parent := FRow1;
  F_r.OnMouseDown := OnButtonClick;

  F_t := TBCButton.Create(FRow1);
  F_t.Caption := 'T';
  F_t.Parent := FRow1;
  F_t.OnMouseDown := OnButtonClick;

  F_y := TBCButton.Create(FRow1);
  F_y.Caption := 'Y';
  F_y.Parent := FRow1;
  F_y.OnMouseDown := OnButtonClick;

  F_u := TBCButton.Create(FRow1);
  F_u.Caption := 'U';
  F_u.Parent := FRow1;
  F_u.OnMouseDown := OnButtonClick;

  F_i := TBCButton.Create(FRow1);
  F_i.Caption := 'I';
  F_i.Parent := FRow1;
  F_i.OnMouseDown := OnButtonClick;

  F_o := TBCButton.Create(FRow1);
  F_o.Caption := 'O';
  F_o.Parent := FRow1;
  F_o.OnMouseDown := OnButtonClick;

  F_p := TBCButton.Create(FRow1);
  F_p.Caption := 'P';
  F_p.Parent := FRow1;
  F_p.OnMouseDown := OnButtonClick;


  { asdfghjkl }

  FRow2 := TBCPanel.Create(FPanel);
  FRow2.AutoSize := True;
  FRow2.Caption := '';
  FRow2.BorderBCStyle := bpsBorder;
  FRow2.ChildSizing.ControlsPerLine := 9;
  FRow2.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  FRow2.Parent := FPanel;

  F_a := TBCButton.Create(FRow2);
  F_a.Caption := 'A';
  F_a.Parent := FRow2;
  F_a.OnMouseDown := OnButtonClick;

  F_s := TBCButton.Create(FRow2);
  F_s.Caption := 'S';
  F_s.Parent := FRow2;
  F_s.OnMouseDown := OnButtonClick;

  F_d := TBCButton.Create(FRow2);
  F_d.Caption := 'D';
  F_d.Parent := FRow2;
  F_d.OnMouseDown := OnButtonClick;

  F_f := TBCButton.Create(FRow2);
  F_f.Caption := 'F';
  F_f.Parent := FRow2;
  F_f.OnMouseDown := OnButtonClick;

  F_g := TBCButton.Create(FRow2);
  F_g.Caption := 'G';
  F_g.Parent := FRow2;
  F_g.OnMouseDown := OnButtonClick;

  F_h := TBCButton.Create(FRow2);
  F_h.Caption := 'H';
  F_h.Parent := FRow2;
  F_h.OnMouseDown := OnButtonClick;

  F_j := TBCButton.Create(FRow2);
  F_j.Caption := 'J';
  F_j.Parent := FRow2;
  F_j.OnMouseDown := OnButtonClick;

  F_k := TBCButton.Create(FRow2);
  F_k.Caption := 'K';
  F_k.Parent := FRow2;
  F_k.OnMouseDown := OnButtonClick;

  F_l := TBCButton.Create(FRow2);
  F_l.Caption := 'L';
  F_l.Parent := FRow2;
  F_l.OnMouseDown := OnButtonClick;

  { zxcvbnm }

  FRow3 := TBCPanel.Create(FPanel);
  FRow3.AutoSize := True;
  FRow3.Caption := '';
  FRow3.BorderBCStyle := bpsBorder;
  FRow3.ChildSizing.ControlsPerLine := 9;
  FRow3.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  FRow3.Parent := FPanel;

  F_shift := TBCButton.Create(FRow3);
  F_shift.Caption := '^';
  F_shift.Parent := FRow3;
  F_shift.OnMouseDown := OnButtonClick;
  F_shift.Down := True;

  F_z := TBCButton.Create(FRow3);
  F_z.Caption := 'Z';
  F_z.Parent := FRow3;
  F_z.OnMouseDown := OnButtonClick;

  F_x := TBCButton.Create(FRow3);
  F_x.Caption := 'X';
  F_x.Parent := FRow3;
  F_x.OnMouseDown := OnButtonClick;

  F_c := TBCButton.Create(FRow3);
  F_c.Caption := 'C';
  F_c.Parent := FRow3;
  F_c.OnMouseDown := OnButtonClick;

  F_v := TBCButton.Create(FRow3);
  F_v.Caption := 'V';
  F_v.Parent := FRow3;
  F_v.OnMouseDown := OnButtonClick;

  F_b := TBCButton.Create(FRow3);
  F_b.Caption := 'B';
  F_b.Parent := FRow3;
  F_b.OnMouseDown := OnButtonClick;

  F_n := TBCButton.Create(FRow3);
  F_n.Caption := 'N';
  F_n.Parent := FRow3;
  F_n.OnMouseDown := OnButtonClick;

  F_m := TBCButton.Create(FRow3);
  F_m.Caption := 'M';
  F_m.Parent := FRow3;
  F_m.OnMouseDown := OnButtonClick;

  F_back := TBCButton.Create(FRow3);
  F_back.Caption := '<-';
  F_back.Parent := FRow3;
  F_back.OnMouseDown := OnButtonClick;

  { shift space back }

  FRow4 := TBCPanel.Create(FPanel);
  FRow4.AutoSize := True;
  FRow4.Caption := '';
  FRow4.BorderBCStyle := bpsBorder;
  FRow4.ChildSizing.ControlsPerLine := 1;
  FRow4.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  FRow4.Parent := FPanel;

  F_space := TBCButton.Create(FRow4);
  F_space.Caption := '____________________';
  F_space.Parent := FRow4;
  F_space.OnMouseDown := OnButtonClick;
end;

destructor TBCKeyboard.Destroy;
begin
  { Everything inside the panel will be freed }
  FPanel.Free;
  inherited Destroy;
end;

procedure TBCKeyboard.Show(AControl: TWinControl);
begin
  FPanel.Parent := AControl;
  FVisible := True;
end;

procedure TBCKeyboard.Show;
begin
  if Self.Owner is TWinControl then
    Show(Self.Owner as TWinControl)
  else
    raise Exception.Create('The parent is not TWinControl descendant.');
end;

procedure TBCKeyboard.Hide;
begin
  FPanel.Parent := nil;
  FVisible := False;
end;

procedure TBCKeyboard.UpdateButtonStyle;
var
  shift_down: boolean;
begin
  F_q.Assign(FButton);
  F_w.Assign(FButton);
  F_e.Assign(FButton);
  F_r.Assign(FButton);
  F_t.Assign(FButton);
  F_y.Assign(FButton);
  F_u.Assign(FButton);
  F_i.Assign(FButton);
  F_o.Assign(FButton);
  F_p.Assign(FButton);
  F_a.Assign(FButton);
  F_s.Assign(FButton);
  F_d.Assign(FButton);
  F_f.Assign(FButton);
  F_g.Assign(FButton);
  F_h.Assign(FButton);
  F_j.Assign(FButton);
  F_k.Assign(FButton);
  F_l.Assign(FButton);
  F_z.Assign(FButton);
  F_x.Assign(FButton);
  F_c.Assign(FButton);
  F_v.Assign(FButton);
  F_b.Assign(FButton);
  F_n.Assign(FButton);
  F_m.Assign(FButton);

  shift_down := F_shift.Down;
  F_shift.Assign(FButton);
  F_shift.Down := shift_down;

  F_back.Assign(FButton);

  F_space.Assign(FButton);
end;

procedure TBCKeyboard.SetFButton(AValue: TBCButton);
begin
  if FButton = AValue then
    Exit;
  FButton := AValue;
end;

procedure TBCKeyboard.SetFPanel(AValue: TBCPanel);
begin
  if FPanel = AValue then
    Exit;
  FPanel := AValue;
end;

procedure TBCKeyboard.SetFPanelsColor(AValue: TColor);
begin
  if FPanelsColor = AValue then
    Exit;
  FPanelsColor := AValue;
  FPanel.Background.Color := AValue;
  FRow1.Background.Color := AValue;
  FRow2.Background.Color := AValue;
  FRow3.Background.Color := AValue;
  FRow4.Background.Color := AValue;
end;

end.
