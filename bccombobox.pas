unit BCComboBox;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, BCButton,
  StdCtrls;

type

  { TBCComboBox }

  TBCComboBox = class(TCustomControl)
  private
    FButton: TBCButton;
    FForm: TForm;
    FListBox: TListBox;
    procedure ButtonClick(Sender: TObject);
    function GetItems: TStrings;
    procedure ListBoxClick(Sender: TObject);
    procedure ListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure SetItems(AValue: TStrings);
  protected

  public
    constructor Create(AOwner: TComponent); override;
  published
    property Button: TBCButton read FButton write FButton;
    property ListBox: TListBox read FListBox write FListBox;
    property Form: TForm read FForm write FForm;
    property Items: TStrings read GetItems write SetItems;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCComboBox]);
end;

{ TBCComboBox }

procedure TBCComboBox.ButtonClick(Sender: TObject);
var
  p: TPoint;
begin
  p := ControlToScreen(Point(FButton.Left, FButton.Top + FButton.Height));
  FForm.Left := p.X;
  FForm.Top := p.Y;
  FForm.Visible := not FForm.Visible;
  if FForm.Visible and FListBox.CanSetFocus then
    FListBox.SetFocus;
  if FForm.Visible then
    FForm.Constraints.MinWidth := FButton.Width;
end;

function TBCComboBox.GetItems: TStrings;
begin
  Result := FListBox.Items;
end;

procedure TBCComboBox.ListBoxClick(Sender: TObject);
begin
  FForm.Visible := False;
  FButton.Caption := FListBox.Items[FListBox.ItemIndex];
end;

procedure TBCComboBox.ListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  FButton.Caption := FListBox.Items[FListBox.ItemIndex];
end;

procedure TBCComboBox.SetItems(AValue: TStrings);
begin
  Items := AValue;
end;

constructor TBCComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TBCButton.Create(Self);
  FButton.Align := alClient;
  FButton.Parent := Self;
  FButton.OnClick := ButtonClick;
  FButton.DropDownArrow := True;

  FForm := TForm.Create(Self);
  FForm.Visible := False;
  FForm.FormStyle := fsStayOnTop;
  FForm.BorderStyle := bsNone;
  FForm.AutoSize := True;

  FListBox := TListBox.Create(FForm);
  FListBox.Align := alClient;
  FListBox.Parent := FForm;
  FListBox.BorderStyle := bsNone;
  FListBox.OnClick := ListBoxClick;
  FListBox.OnSelectionChange := ListBoxSelectionChange;
end;

end.
