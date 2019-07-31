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
    FHoverItem: integer;
    FListBox: TListBox;
    procedure ButtonClick(Sender: TObject);
    function GetItemIndex: integer;
    function GetItems: TStrings;
    procedure ListBoxClick(Sender: TObject);
    procedure ListBoxMouseLeave(Sender: TObject);
    procedure ListBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure SetItemIndex(AValue: integer);
    procedure SetItems(AValue: TStrings);
  protected
    procedure UpdateCaption;
  public
    constructor Create(AOwner: TComponent); override;
    property HoverItem: integer read FHoverItem;
    property Button: TBCButton read FButton write FButton;
    property ListBox: TListBox read FListBox write FListBox;
    property Items: TStrings read GetItems write SetItems;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
  published

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
  if FForm=nil then
  begin
    FForm := TForm.Create(Self);
    FForm.Visible := False;
    FForm.ShowInTaskBar:= stNever;
    FForm.FormStyle := fsStayOnTop;
    FForm.BorderStyle := bsNone;
    FForm.AutoSize := True;
    p := ControlToScreen(Point(FButton.Left, FButton.Top + FButton.Height));
    FForm.Left := p.X;
    FForm.Top := p.Y;
    FListBox.Parent := FForm;
  end;

  if FForm.Visible then
    FForm.Visible := false
  else
  begin
    FForm.Constraints.MinWidth := FButton.Width;
    FForm.Visible := True;
    if FListBox.CanSetFocus then
      FListBox.SetFocus;
  end;
end;

function TBCComboBox.GetItemIndex: integer;
begin
  result := FListBox.ItemIndex;
end;

function TBCComboBox.GetItems: TStrings;
begin
  Result := FListBox.Items;
end;

procedure TBCComboBox.ListBoxClick(Sender: TObject);
begin
  FForm.Visible := false;
end;

procedure TBCComboBox.ListBoxMouseLeave(Sender: TObject);
begin
  FHoverItem := -1;
  FListBox.Repaint;
end;

procedure TBCComboBox.ListBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  TempItem: integer;
begin
  TempItem := FListBox.ItemAtPos(Point(x, y), True);

  if TempItem <> FHoverItem then
  begin
    FHoverItem := TempItem;
    FListBox.Repaint;
  end;
end;

procedure TBCComboBox.ListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  UpdateCaption;
end;

procedure TBCComboBox.SetItemIndex(AValue: integer);
begin
  FListBox.ItemIndex := AValue;
end;

procedure TBCComboBox.SetItems(AValue: TStrings);
begin
  Items := AValue;
end;

procedure TBCComboBox.UpdateCaption;
begin
  if FListBox.ItemIndex<>-1 then
    FButton.Caption := FListBox.Items[FListBox.ItemIndex]
  else
    FButton.Caption := '';
end;

constructor TBCComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TBCButton.Create(Self);
  FButton.Align := alClient;
  FButton.Parent := Self;
  FButton.OnClick := ButtonClick;
  FButton.DropDownArrow := True;

  FListBox := TListBox.Create(FForm);
  FListBox.Align := alClient;
  FListBox.Parent := FForm;
  FListBox.BorderStyle := bsNone;
  FListBox.OnClick := ListBoxClick;
  FListBox.OnSelectionChange := ListBoxSelectionChange;
  FListBox.OnMouseLeave:=ListBoxMouseLeave;
  FListBox.OnMouseMove:=ListBoxMouseMove;
  FHoverItem := -1;
end;

end.
