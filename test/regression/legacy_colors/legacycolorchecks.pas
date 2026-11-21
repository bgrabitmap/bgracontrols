unit LegacyColorChecks;

{$mode objfpc}{$H+}

interface

uses umain;

procedure RunLegacyColorChecks(AForm: TForm1);

implementation

uses
  Forms, Controls, Classes, SysUtils, Graphics, StdCtrls, ExtCtrls,
  BCColors, BCColorUtils, BCColorPicker, BCComboBox, BCButton;

type
  TPickerCheck = class
  private
    FTimer: TTimer;
    FExpected, FError: string;
    FChecks, FPairs: integer;
    procedure Check(OK: boolean; const MessageText: string);
    procedure Tick(Sender: TObject);
    procedure CheckPair(Color: TBCLinkedColor; OldColor: TColor; OldAlpha: byte);
    procedure CheckState(State: TBCButtonState);
    procedure CheckCombo(Combo: TBCComboBox);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run(AForm: TForm1);
  end;

constructor TPickerCheck.Create;
begin
  inherited Create;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 20;
  FTimer.OnTimer := @Tick;
end;

destructor TPickerCheck.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

procedure TPickerCheck.Check(OK: boolean; const MessageText: string);
begin
  Inc(FChecks);
  if not OK then raise Exception.Create(MessageText);
end;

{ Read the text shown by the modal picker, then confirm without editing. }
procedure TPickerCheck.Tick(Sender: TObject);
var
  Form: TCustomForm;
  Actual: string;
begin
  Form := Screen.ActiveCustomForm;
  if not (Form is TBCColorPickerForm) then Exit;
  FTimer.Enabled := False;
  try
    Actual := TEdit(Form.FindComponent('ColorText')).Text;
    Check(Actual = FExpected, 'picker expected ' + FExpected + ', got ' + Actual);
    TButton(Form.FindComponent('OKButton')).Click;
  except
    on E: Exception do
    begin
      FError := E.Message;
      Form.ModalResult := mrCancel;
    end;
  end;
end;

{ Compare each linked value with its loaded legacy pair and check text round-trip. }
procedure TPickerCheck.CheckPair(Color: TBCLinkedColor; OldColor: TColor; OldAlpha: byte);
var
  Value, Parsed: TBCColorValue;
  Accepted: boolean;
begin
  Inc(FPairs);
  Value := TBCColorValue.New(OldColor, OldAlpha);
  Check(Color.SameValueAs(Value), 'linked value differs from loaded legacy pair');
  FExpected := BCToStr(Value);
  Check(TryStrToBC(FExpected, Parsed) and (Parsed = Value), 'text roundtrip: ' + FExpected);
  FError := '';
  FTimer.Enabled := True;
  Accepted := ExecuteBCColorPicker(Color);
  Check(Accepted and (FError = ''), 'picker failed: ' + FError);
  Check(Color.SameValueAs(Value), 'picker changed loaded color or alpha');
end;

procedure TPickerCheck.CheckState(State: TBCButtonState);
begin
  with State do
  begin
    CheckPair(Background.AlphaColor, Background.Color, Background.ColorOpacity);
    CheckPair(Border.AlphaColor, Border.Color, Border.ColorOpacity);
    CheckPair(Border.LightAlphaColor, Border.LightColor, Border.LightOpacity);
    CheckPair(FontEx.ShadowAlphaColor, FontEx.ShadowColor, FontEx.ShadowColorOpacity);
    with Background.Gradient1 do
    begin
      CheckPair(StartAlphaColor, StartColor, StartColorOpacity);
      CheckPair(EndAlphaColor, EndColor, EndColorOpacity);
    end;
    with Background.Gradient2 do
    begin
      CheckPair(StartAlphaColor, StartColor, StartColorOpacity);
      CheckPair(EndAlphaColor, EndColor, EndColorOpacity);
    end;
  end;
end;

procedure TPickerCheck.CheckCombo(Combo: TBCComboBox);
begin
  Check((Combo.Items.Count = 3) and (Combo.Items[0] = 'Red') and
    (Combo.Items[1] = 'Green') and (Combo.Items[2] = 'Blue'), 'loaded combo items');
  CheckPair(Combo.FocusBorderAlphaColor, Combo.FocusBorderColor, Combo.FocusBorderOpacity);
  CheckState(Combo.StateNormal);
  CheckState(Combo.StateHover);
  CheckState(Combo.StateClicked);
end;

procedure TPickerCheck.Run(AForm: TForm1);
begin
  Check(AForm.BCComboBox1.FocusBorderColor = 3368601, 'literal focus color');
  Check(AForm.BCComboBox1.FocusBorderOpacity = 200, 'literal focus opacity');
  Check(AForm.BCComboBox3.StateNormal.Background.Color = clBtnFace, 'literal system color');
  Check(AForm.BCComboBox4.StateClicked.Background.ColorOpacity = 76, 'literal alpha 76');
  Check(AForm.BCComboBox4.StateClicked.Border.ColorOpacity = 178, 'literal alpha 178');
  CheckCombo(AForm.BCComboBox1);
  CheckCombo(AForm.BCComboBox2);
  CheckCombo(AForm.BCComboBox3);
  CheckCombo(AForm.BCComboBox4);
  WriteLn('PASS: ', FChecks, ' checks; ', FPairs,
    ' loaded legacy pairs opened and confirmed in picker');
end;

procedure RunLegacyColorChecks(AForm: TForm1);
var
  Checker: TPickerCheck;
begin
  Checker := TPickerCheck.Create;
  try
    Checker.Run(AForm);
  finally
    Checker.Free;
  end;
end;

end.
