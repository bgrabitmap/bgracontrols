unit BCNumericKeyboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BCPanel, BCButton;

type

  { TBCNumericKeyboard }

  TBCNumericKeyboard = class(TComponent)
  private
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
    procedure OnButtonClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Show in a custom form or panel
    procedure Show(AControl: TWinControl);
    // Try to Show in the form where this component is placed
    procedure Show();
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
    { The input value }
    property Value: string read FValue write SetFValue;
    { When value is changed by code or by the user }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    { When value is changed by the user }
    property OnUserChange: TNotifyEvent read FOnUserChange write FOnUserChange;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCNumericKeyboard]);
end;

{ TBCNumericKeyboard }

procedure TBCNumericKeyboard.SetFPanel(AValue: TBCPanel);
begin
  if FPanel = AValue then
    Exit;
  FPanel := AValue;
end;

procedure TBCNumericKeyboard.SetFValue(AValue: string);
begin
  if FValue = AValue then
    Exit;
  FValue := AValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCNumericKeyboard.OnButtonClick(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  btn: TBCButton;
  num: string;
begin
  btn := TBCButton(Sender);
  num := btn.Caption;

  if num = 'C' then
  begin
    Value := '';
  end
  else if num = DefaultFormatSettings.DecimalSeparator then
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

procedure TBCNumericKeyboard.SetFButton(AValue: TBCButton);
begin
  if FButton = AValue then
    Exit;
  FButton := AValue;
end;

constructor TBCNumericKeyboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FVisible := False;

  FButton := TBCButton.Create(Self);

  FPanel := TBCPanel.Create(Self);
  FPanel.AutoSize := True;
  FPanel.ChildSizing.ControlsPerLine := 3;
  FPanel.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  FPanel.Caption := '';

  FBtn7 := TBCButton.Create(FPanel);
  FBtn7.Parent := FPanel;
  FBtn7.Caption := '7';
  FBtn7.OnMouseDown := @OnButtonClick;

  FBtn8 := TBCButton.Create(FPanel);
  FBtn8.Parent := FPanel;
  FBtn8.Caption := '8';
  FBtn8.OnMouseDown := @OnButtonClick;

  FBtn9 := TBCButton.Create(FPanel);
  FBtn9.Caption := '9';
  FBtn9.Parent := FPanel;
  FBtn9.OnMouseDown := @OnButtonClick;

  FBtn4 := TBCButton.Create(FPanel);
  FBtn4.Parent := FPanel;
  FBtn4.Caption := '4';
  FBtn4.OnMouseDown := @OnButtonClick;

  FBtn5 := TBCButton.Create(FPanel);
  FBtn5.Parent := FPanel;
  FBtn5.Caption := '5';
  FBtn5.OnMouseDown := @OnButtonClick;

  FBtn6 := TBCButton.Create(FPanel);
  FBtn6.Parent := FPanel;
  FBtn6.Caption := '6';
  FBtn6.OnMouseDown := @OnButtonClick;

  FBtn1 := TBCButton.Create(FPanel);
  FBtn1.Parent := FPanel;
  FBtn1.Caption := '1';
  FBtn1.OnMouseDown := @OnButtonClick;

  FBtn2 := TBCButton.Create(FPanel);
  FBtn2.Parent := FPanel;
  FBtn2.Caption := '2';
  FBtn2.OnMouseDown := @OnButtonClick;

  FBtn3 := TBCButton.Create(FPanel);
  FBtn3.Parent := FPanel;
  FBtn3.Caption := '3';
  FBtn3.OnMouseDown := @OnButtonClick;

  FBtn0 := TBCButton.Create(FPanel);
  FBtn0.Parent := FPanel;
  FBtn0.Caption := '0';
  FBtn0.OnMouseDown := @OnButtonClick;

  FBtnDot := TBCButton.Create(FPanel);
  FBtnDot.Parent := FPanel;
  FBtnDot.Caption := DefaultFormatSettings.DecimalSeparator;
  FBtnDot.OnMouseDown := @OnButtonClick;

  FBtnClr := TBCButton.Create(FPanel);
  FBtnClr.Parent := FPanel;
  FBtnClr.Caption := 'C';
  FBtnClr.OnMouseDown := @OnButtonClick;
end;

destructor TBCNumericKeyboard.Destroy;
begin
  { Everything inside the panel will be freed }
  FPanel.Free;
  inherited Destroy;
end;

procedure TBCNumericKeyboard.Show(AControl: TWinControl);
begin
  FPanel.Parent := AControl;
  FVisible := True;
end;

procedure TBCNumericKeyboard.Show;
begin
  if Self.Owner is TWinControl then
    Show(Self.Owner as TWinControl)
  else
    raise Exception.Create('The parent is not TWinControl descendant.');
end;

procedure TBCNumericKeyboard.Hide;
begin
  FPanel.Parent := nil;
  FVisible := False;
end;

procedure TBCNumericKeyboard.UpdateButtonStyle;
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
