// SPDX-License-Identifier: LGPL-3.0-linking-exception
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCNumericKeyboard;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LCLType, LResources, LMessages,{$ENDIF}
  Forms, Controls, Graphics, Dialogs, MouseAndKeyInput,
  {$IFNDEF FPC}Types, Windows, BGRAGraphics, GraphType, FPImage, BCBaseCtrls, {$ENDIF}
  BCPanel, BCButton, BCThemeManager;

type

  { TBCCustomNumericKeyboard }

  TBCCustomNumericKeyboard = class(TComponent)
  private
    FBCThemeManager: TBCThemeManager;
    procedure SetFThemeManager(AValue: TBCThemeManager);
  protected
    FOnChange: TNotifyEvent;
    FOnUserChange: TNotifyEvent;
    FPanel: TBCPanel;
    FButton: TBCButton;
    FBtn0, FBtn1, FBtn2, FBtn3, FBtn4, FBtn5, FBtn6, FBtn7, FBtn8,
    FBtn9, FBtnDot, FBtnClr: TBCButton;
    FValue: string;
    FVisible: boolean;
    procedure SetFButton(AValue: TBCButton);
    procedure SetFPanel(AValue: TBCPanel);
    procedure SetFValue(AValue: string);
  protected
    procedure OnButtonClick(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer); virtual;
  protected
    { The input value }
    property Value: string read FValue write SetFValue;
    { When value is changed by code or by the user }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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
    { A fake button that's used as style base for all the numeric buttons }
    property ButtonStyle: TBCButton read FButton write SetFButton;
    { If it's visible or not }
    property Visible: boolean read FVisible;
  published
    property ThemeManager: TBCThemeManager read FBCThemeManager write SetFThemeManager;
  end;

  TBCNumericKeyboard = class(TBCCustomNumericKeyboard)
  published
    property Value;
    property OnChange;
    property OnUserChange;
    property ThemeManager;
  end;

  { TBCRealNumericKeyboard }

  TBCRealNumericKeyboard = class(TBCCustomNumericKeyboard)
  protected
    procedure OnButtonClick(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer); override;
    procedure PressVirtKey(p: PtrInt);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OnUserChange;
    property ThemeManager;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

{$IFDEF FPC}
procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCNumericKeyboard]);
  RegisterComponents('BGRA Controls', [TBCRealNumericKeyboard]);
end;
{$ENDIF}

{ TBCRealNumericKeyboard }

procedure TBCRealNumericKeyboard.OnButtonClick(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
const
{$IFDEF LINUX}
  vk_DotNumPad = 110;
{$ELSE}
  vk_DotNumPad = 190;
{$ENDIF}
var
  btn: TBCButton;
  num: string;
begin
  btn := TBCButton(Sender);
  num := btn.Caption;

  if num = FBtnClr.Caption then
  begin
    {$IFDEF FPC}
    Application.QueueAsyncCall(PressVirtKey, VK_BACK);
    {$ELSE}
    SendKey(VK_BACK);
    {$ENDIF}
  end
  else if num = FBtnDot.Caption then
  begin
    {$IFDEF FPC}
    Application.QueueAsyncCall(PressVirtKey, vk_DotNumPad);
    {$ELSE}
    SendKey(vk_DotNumPad);
    {$ENDIF}
  end
  else
  begin
    {$IFDEF FPC}
    Application.QueueAsyncCall(PressVirtKey, Ord(TBCButton(Sender).Caption[1]));
    {$ELSE}
    SendKey(Ord(TBCButton(Sender).Caption[1]));
    {$ENDIF}
  end;

  if Assigned(FOnUserChange) then
    FOnUserChange(Self);
end;

procedure TBCRealNumericKeyboard.PressVirtKey(p: PtrInt);
begin
  KeyInput.Down(p);
  KeyInput.Up(p);
end;

constructor TBCRealNumericKeyboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBtnClr.Caption := '<-';
end;

{ TBCCustomNumericKeyboard }

procedure TBCCustomNumericKeyboard.SetFPanel(AValue: TBCPanel);
begin
  if FPanel = AValue then
    Exit;
  FPanel := AValue;
end;

procedure TBCCustomNumericKeyboard.SetFValue(AValue: string);
begin
  if FValue = AValue then
    Exit;
  FValue := AValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCCustomNumericKeyboard.OnButtonClick(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  btn: TBCButton;
  num: string;
begin
  btn := TBCButton(Sender);
  num := btn.Caption;

  if num = FBtnClr.Caption then
  begin
    Value := '';
  end
  else if num = FBtnDot.Caption then
  begin
    if Length(Value) = 0 then
      Value := '0' + num;
    if Pos(num, Value) = 0 then
      Value := Value + num;
  end
  else
  begin
    Value := Value + num;
  end;

  if Assigned(FOnUserChange) then
    FOnUserChange(Self);
end;

procedure TBCCustomNumericKeyboard.SetFThemeManager(AValue: TBCThemeManager);
begin
  if FBCThemeManager = AValue then
    Exit;
  FBCThemeManager := AValue;
end;

procedure TBCCustomNumericKeyboard.SetFButton(AValue: TBCButton);
begin
  if FButton = AValue then
    Exit;
  FButton := AValue;
end;

constructor TBCCustomNumericKeyboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FVisible := False;

  FButton := TBCButton.Create(Self);

  FPanel := TBCPanel.Create(Self);
  FPanel.AutoSize := True;
  FPanel.ChildSizing.ControlsPerLine := 3;
  FPanel.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  FPanel.Caption := '';
  FPanel.BorderBCStyle := bpsBorder;

  FBtn7 := TBCButton.Create(FPanel);
  FBtn7.Parent := FPanel;
  FBtn7.Caption := '7';
  FBtn7.OnMouseDown := OnButtonClick;

  FBtn8 := TBCButton.Create(FPanel);
  FBtn8.Parent := FPanel;
  FBtn8.Caption := '8';
  FBtn8.OnMouseDown := OnButtonClick;

  FBtn9 := TBCButton.Create(FPanel);
  FBtn9.Caption := '9';
  FBtn9.Parent := FPanel;
  FBtn9.OnMouseDown := OnButtonClick;

  FBtn4 := TBCButton.Create(FPanel);
  FBtn4.Parent := FPanel;
  FBtn4.Caption := '4';
  FBtn4.OnMouseDown := OnButtonClick;

  FBtn5 := TBCButton.Create(FPanel);
  FBtn5.Parent := FPanel;
  FBtn5.Caption := '5';
  FBtn5.OnMouseDown := OnButtonClick;

  FBtn6 := TBCButton.Create(FPanel);
  FBtn6.Parent := FPanel;
  FBtn6.Caption := '6';
  FBtn6.OnMouseDown := OnButtonClick;

  FBtn1 := TBCButton.Create(FPanel);
  FBtn1.Parent := FPanel;
  FBtn1.Caption := '1';
  FBtn1.OnMouseDown := OnButtonClick;

  FBtn2 := TBCButton.Create(FPanel);
  FBtn2.Parent := FPanel;
  FBtn2.Caption := '2';
  FBtn2.OnMouseDown := OnButtonClick;

  FBtn3 := TBCButton.Create(FPanel);
  FBtn3.Parent := FPanel;
  FBtn3.Caption := '3';
  FBtn3.OnMouseDown := OnButtonClick;

  FBtn0 := TBCButton.Create(FPanel);
  FBtn0.Parent := FPanel;
  FBtn0.Caption := '0';
  FBtn0.OnMouseDown := OnButtonClick;

  FBtnDot := TBCButton.Create(FPanel);
  FBtnDot.Parent := FPanel;
  FBtnDot.Caption := {$IFDEF FPC}DefaultFormatSettings{$ELSE}FormatSettings{$ENDIF}.DecimalSeparator;
  FBtnDot.OnMouseDown := OnButtonClick;

  FBtnClr := TBCButton.Create(FPanel);
  FBtnClr.Parent := FPanel;
  FBtnClr.Caption := 'C';
  FBtnClr.OnMouseDown := OnButtonClick;
end;

destructor TBCCustomNumericKeyboard.Destroy;
begin
  { Everything inside the panel will be freed }
  FPanel.Free;
  inherited Destroy;
end;

procedure TBCCustomNumericKeyboard.Show(AControl: TWinControl);
begin
  FPanel.Parent := AControl;
  FVisible := True;
end;

procedure TBCCustomNumericKeyboard.Show;
begin
  if Self.Owner is TWinControl then
    Show(Self.Owner as TWinControl)
  else
    raise Exception.Create('The parent is not TWinControl descendant.');
end;

procedure TBCCustomNumericKeyboard.Hide;
begin
  FPanel.Parent := nil;
  FVisible := False;
end;

procedure TBCCustomNumericKeyboard.UpdateButtonStyle;
begin
  FBtn0.Assign(FButton);
  FBtn1.Assign(FButton);
  FBtn2.Assign(FButton);
  FBtn3.Assign(FButton);
  FBtn4.Assign(FButton);
  FBtn5.Assign(FButton);
  FBtn6.Assign(FButton);
  FBtn7.Assign(FButton);
  FBtn8.Assign(FButton);
  FBtn9.Assign(FButton);
  FBtnDot.Assign(FButton);
  FBtnClr.Assign(FButton);
end;

end.
