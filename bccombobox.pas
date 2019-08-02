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
    FDropDownBorderSize: integer;
    FDropDownCount: integer;
    FDropDownFontColor: TColor;
    FDropDownFontHighlight: TColor;
    FDropDownHighlight: TColor;
    FForm: TForm;
    FFormHideDate: TDateTime;
    FHoverItem: integer;
    FItemHeight: integer;
    FListBox: TListBox;
    FDropDownBorderColor: TColor;
    FOnDrawItem: TDrawItemEvent;
    FOnChange: TNotifyEvent;
    FDrawingDropDown: boolean;
    procedure ButtonClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    function GetComboCanvas: TCanvas;
    function GetItemText: string;
    function GetDropDownBorderStyle: TBorderStyle;
    function GetDropDownColor: TColor;
    function GetItemIndex: integer;
    function GetItems: TStrings;
    procedure ListBoxMouseUp({%H-}Sender: TObject; {%H-}Button: TMouseButton;
                          {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure ListBoxMouseLeave(Sender: TObject);
    procedure ListBoxMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure ListBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure ListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure SetDropDownBorderStyle(AValue: TBorderStyle);
    procedure SetDropDownColor(AValue: TColor);
    procedure SetItemIndex(AValue: integer);
    procedure SetItems(AValue: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
    property HoverItem: integer read FHoverItem;
    property Button: TBCButton read FButton write FButton;
    property ListBox: TListBox read FListBox write FListBox;
    property Text: string read GetItemText;
  published
    property Canvas: TCanvas read GetComboCanvas;
    property Items: TStrings read GetItems write SetItems;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property ItemHeight: integer read FItemHeight write FItemHeight default 0;
    property DropDownBorderColor: TColor read FDropDownBorderColor write FDropDownBorderColor default clWindowText;
    property DropDownBorderSize: integer read FDropDownBorderSize write FDropDownBorderSize default 1;
    property DropDownColor: TColor read GetDropDownColor write SetDropDownColor default clWindow;
    property DropDownFontColor: TColor read FDropDownFontColor write FDropDownFontColor default clWindowText;
    property DropDownCount: integer read FDropDownCount write FDropDownCount default 8;
    property DropDownHighlight: TColor read FDropDownHighlight write FDropDownHighlight default clHighlight;
    property DropDownFontHighlight: TColor read FDropDownFontHighlight write FDropDownFontHighlight default clHighlightText;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

implementation

uses math, LCLType;

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCComboBox]);
end;

{ TBCComboBox }

procedure TBCComboBox.ButtonClick(Sender: TObject);
const MinDelayReopen = 500/(1000*60*60*24);
var
  p: TPoint;
  h: Integer;
begin
  if FForm=nil then
  begin
    FForm := TForm.Create(Self);
    FForm.Visible := False;
    FForm.ShowInTaskBar:= stNever;
    FForm.FormStyle := fsStayOnTop;
    FForm.BorderStyle := bsNone;
    FForm.OnDeactivate:= FormDeactivate;
    FForm.OnHide:=FormHide;
    FListBox.Parent := FForm;
  end;

  if FForm.Visible then
    FForm.Visible := false
  else
  if Now > FFormHideDate+MinDelayReopen then
  begin
    p := ControlToScreen(Point(FButton.Left, FButton.Top + FButton.Height));
    FForm.Left := p.X;
    FForm.Top := p.Y;
    FForm.Color := FDropDownBorderColor;
    FListBox.Font.Name := Button.StateNormal.FontEx.Name;
    FListBox.Font.Style := Button.StateNormal.FontEx.Style;
    FListBox.Font.Height := Button.StateNormal.FontEx.Height;
    FListBox.Canvas.Font.Assign(FListBox.Font);
    if Assigned(FOnDrawItem) and (FItemHeight <> 0) then
      h := FItemHeight else h := FListBox.Canvas.GetTextHeight('Hg');
    FListBox.ItemHeight := h;
    FForm.ClientWidth := FButton.Width;
    FForm.ClientHeight := (h+6)*min(Items.Count, FDropDownCount) + 2*FDropDownBorderSize;
    FListBox.SetBounds(FDropDownBorderSize,FDropDownBorderSize,
      FForm.ClientWidth-2*FDropDownBorderSize,
      FForm.ClientHeight-2*FDropDownBorderSize);
    FForm.Visible := True;
    if FListBox.CanSetFocus then
      FListBox.SetFocus;
  end;
end;

procedure TBCComboBox.FormDeactivate(Sender: TObject);
begin
  FForm.Visible:= false;
end;

procedure TBCComboBox.FormHide(Sender: TObject);
begin
  FFormHideDate := Now;
end;

function TBCComboBox.GetComboCanvas: TCanvas;
begin
  if FDrawingDropDown then
    result := ListBox.Canvas
  else
    result := inherited Canvas;
end;

function TBCComboBox.GetItemText: string;
begin
  if FListBox.ItemIndex<>-1 then
    result := FListBox.Items[FListBox.ItemIndex]
  else
    result := '';
end;

function TBCComboBox.GetDropDownBorderStyle: TBorderStyle;
begin
  result := FListBox.BorderStyle;
end;

function TBCComboBox.GetDropDownColor: TColor;
begin
  result := FListBox.Color;
end;

function TBCComboBox.GetItemIndex: integer;
begin
  result := FListBox.ItemIndex;
end;

function TBCComboBox.GetItems: TStrings;
begin
  Result := FListBox.Items;
end;

procedure TBCComboBox.ListBoxMouseUp(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer);
begin
  FForm.Visible := false;
  FFormHideDate := 0;
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
    if (FHoverItem<>-1) and ([ssLeft,ssRight]*Shift <> []) then
      FListBox.ItemIndex := FHoverItem;
    FListBox.Repaint;
  end;
end;

procedure TBCComboBox.ListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  Button.Caption := GetItemText;
  if User and Assigned(FOnChange) then FOnChange(self);
end;

procedure TBCComboBox.ListBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  aCanvas: TCanvas;
begin
  if Assigned(FOnDrawItem) then
  begin
    FDrawingDropDown := true;
    Exclude(State, odSelected);
    if Index = HoverItem then Include(State, odSelected);
    if Index = ItemIndex then Include(State, odChecked);
    try
      FOnDrawItem(Control, Index, ARect, State);
    finally
      FDrawingDropDown := false;
    end;
    exit;
  end;

  aCanvas := TListBox(Control).Canvas;
  if Index = HoverItem then
  begin
    aCanvas.Brush.Color := DropDownHighlight;
    aCanvas.Font.Color := DropDownFontHighlight;
  end
  else
  begin
    aCanvas.Brush.Color := DropDownColor;
    aCanvas.Font.Color := DropDownFontColor;
  end;
  aCanvas.Pen.Style := psClear;
  aCanvas.FillRect(ARect);
  aCanvas.TextRect(ARect, ARect.Left+4, ARect.Top +
    (ARect.Height - aCanvas.GetTextHeight(Items[Index])) div 2,
    Items[Index]);
end;

procedure TBCComboBox.SetDropDownBorderStyle(AValue: TBorderStyle);
begin
  FListBox.BorderStyle:= AValue;
end;

procedure TBCComboBox.SetDropDownColor(AValue: TColor);
begin
  FListBox.Color := AValue;
end;

procedure TBCComboBox.SetItemIndex(AValue: integer);
begin
  FListBox.ItemIndex := AValue;
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

  FListBox := TListBox.Create(self);
  FListBox.Anchors := [akTop, akLeft, akRight, akBottom];
  FListBox.Parent := nil;
  FListBox.BorderStyle:= bsNone;
  FListBox.OnSelectionChange := ListBoxSelectionChange;
  FListBox.OnMouseLeave:=ListBoxMouseLeave;
  FListBox.OnMouseMove:=ListBoxMouseMove;
  FListBox.OnMouseUp:= ListBoxMouseUp;
  FListBox.Style := lbOwnerDrawFixed;
  FListBox.OnDrawItem:= ListBoxDrawItem;
  FListBox.Options := []; // do not draw focus rect
  FHoverItem := -1;

  FDropDownBorderSize := 1;
  DropDownColor:= clWindow;
  FDropDownCount := 8;
  FDropDownFontColor:= clWindowText;
  FDropDownHighlight:= clHighlight;
  FDropDownFontHighlight:= clHighlightText;
end;

procedure TBCComboBox.Clear;
begin
  Items.Clear;
end;

end.
