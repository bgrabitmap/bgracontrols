unit BCDefaultThemeManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BCButton, BCButtonFocus, BCNumericKeyboard, BCThemeManager;

type

  { TBCDefaultThemeManager }

  TBCDefaultThemeManager = class(TBCThemeManager)
  private
    FButton: TBCButton;
    FButtonFocus: TBCButtonFocus;
    procedure SetFButton(AValue: TBCButton);
    procedure SetFButtonFocus(AValue: TBCButtonFocus);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Apply(AControl: TWinControl);
    procedure Apply();
  published
    property Button: TBCButton read FButton write SetFButton;
    property ButtonFocus: TBCButtonFocus read FButtonFocus write SetFButtonFocus;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCDefaultThemeManager]);
end;

{ TBCDefaultThemeManager }

procedure TBCDefaultThemeManager.SetFButton(AValue: TBCButton);
begin
  if FButton = AValue then
    Exit;
  FButton := AValue;
end;

procedure TBCDefaultThemeManager.SetFButtonFocus(AValue: TBCButtonFocus);
begin
  if FButtonFocus = AValue then
    Exit;
  FButtonFocus := AValue;
end;

constructor TBCDefaultThemeManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FButton := TBCButton.Create(Self);
  //FButtonFocus := TBCButtonFocus.Create(Self);
end;

destructor TBCDefaultThemeManager.Destroy;
begin
  inherited Destroy;
end;

procedure TBCDefaultThemeManager.Apply(AControl: TWinControl);
var
  i: integer;
begin
  { Controls }
  for i := 0 to AControl.ControlCount - 1 do
  begin
    { BCButton }
    if (AControl.Controls[i] is TBCButton) then
      with TBCButton(AControl.Controls[i]) do
        if (Assigned(ThemeManager)) and
          (TBCDefaultThemeManager(ThemeManager).Name = Self.Name) and
          (FButton.Name <> AControl.Controls[i].Name) then
          Assign(FButton);
    { BCButtonFocus }
    if (AControl.Controls[i] is TBCButtonFocus) then
      with TBCButtonFocus(AControl.Controls[i]) do
        if (Assigned(ThemeManager)) and
          (TBCDefaultThemeManager(ThemeManager).Name = Self.Name) and
          (FButtonFocus.Name <> AControl.Controls[i].Name) then
          Assign(FButtonFocus);
  end;
  { Components }
  for i := 0 to AControl.ComponentCount - 1 do
  begin
    { BCNumericKeyboard }
    if (AControl.Components[i] is TBCNumericKeyboard) then
      with TBCNumericKeyboard(AControl.Components[i]) do
        if (Assigned(ThemeManager)) and
          (TBCDefaultThemeManager(ThemeManager).Name = Self.Name) and
          (FButton.Name <> TBCNumericKeyboard(AControl.Components[i]).ButtonStyle.Name) then
        begin
          ButtonStyle.Assign(FButton);
          UpdateButtonStyle;
        end;
    { BCRealNumericKeyboard }
    if (AControl.Components[i] is TBCRealNumericKeyboard) then
      with TBCRealNumericKeyboard(AControl.Components[i]) do
        if (Assigned(ThemeManager)) and
          (TBCDefaultThemeManager(ThemeManager).Name = Self.Name) and
          (FButton.Name <> TBCRealNumericKeyboard(AControl.Components[i]).ButtonStyle.Name) then
        begin
          ButtonStyle.Assign(FButton);
          UpdateButtonStyle;
        end;
  end;
end;

procedure TBCDefaultThemeManager.Apply;
begin
  if Self.Owner is TWinControl then
    Apply(Self.Owner as TWinControl)
  else
    raise Exception.Create('The parent is not TWinControl descendant.');
end;

end.
