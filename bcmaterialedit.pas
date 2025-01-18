unit BCMaterialEdit;

{$I bgracontrols.inc}

interface

uses
  Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics, {$IFDEF FPC} LCLType,
  LResources, {$ENDIF} Menus, StdCtrls, SysUtils;

type

  { TBCMaterialEdit }

  TBCMaterialEdit = class(TCustomControl)
  private
    FAccentColor: TColor;
    FDisabledColor: TColor;
    FEdit: TEdit;
    FLabel: TBoundLabel;
    FFocused: boolean;
    function IsNeededAdjustHeight: boolean;

    function GetOnEditChange: TNotifyEvent;
    function GetOnEditClick: TNotifyEvent;
    function GetOnEditContextPopup: TContextPopupEvent;
    function GetOnEditDblClick: TNotifyEvent;
    function GetOnEditDragDrop: TDragDropEvent;
    function GetOnEditDragOver: TDragOverEvent;
    function GetOnEditEditingDone: TNotifyEvent;
    function GetOnEditEndDrag: TEndDragEvent;
    function GetOnEditEnter: TNotifyEvent;
    function GetOnEditExit: TNotifyEvent;
    function GetOnEditKeyDown: TKeyEvent;
    function GetOnEditKeyPress: TKeyPressEvent;
    function GetOnEditKeyUp: TKeyEvent;
    function GetOnEditMouseDown: TMouseEvent;
    function GetOnEditMouseEnter: TNotifyEvent;
    function GetOnEditMouseLeave: TNotifyEvent;
    function GetOnEditMouseMove: TMouseMoveEvent;
    function GetOnEditMouseUp: TMouseEvent;
    function GetOnEditMouseWheel: TMouseWheelEvent;
    function GetOnEditMouseWheelDown: TMouseWheelUpDownEvent;
    function GetOnEditMouseWheelUp: TMouseWheelUpDownEvent;
    function GetOnEditStartDrag: TStartDragEvent;
    function GetOnEditUTF8KeyPress: TUTF8KeyPressEvent;

    procedure SetOnEditChange(AValue: TNotifyEvent);
    procedure SetOnEditClick(AValue: TNotifyEvent);
    procedure SetOnEditContextPopup(AValue: TContextPopupEvent);
    procedure SetOnEditDblClick(AValue: TNotifyEvent);
    procedure SetOnEditDragDrop(AValue: TDragDropEvent);
    procedure SetOnEditDragOver(AValue: TDragOverEvent);
    procedure SetOnEditEditingDone(AValue: TNotifyEvent);
    procedure SetOnEditEndDrag(AValue: TEndDragEvent);
    procedure SetOnEditEnter(AValue: TNotifyEvent);
    procedure SetOnEditExit(AValue: TNotifyEvent);
    procedure SetOnEditKeyDown(AValue: TKeyEvent);
    procedure SetOnEditKeyPress(AValue: TKeyPressEvent);
    procedure SetOnEditKeyUp(AValue: TKeyEvent);
    procedure SetOnEditMouseDown(AValue: TMouseEvent);
    procedure SetOnEditMouseEnter(AValue: TNotifyEvent);
    procedure SetOnEditMouseLeave(AValue: TNotifyEvent);
    procedure SetOnEditMouseMove(AValue: TMouseMoveEvent);
    procedure SetOnEditMouseUp(AValue: TMouseEvent);
    procedure SetOnEditMouseWheel(AValue: TMouseWheelEvent);
    procedure SetOnEditMouseWheelDown(AValue: TMouseWheelUpDownEvent);
    procedure SetOnEditMouseWheelUp(AValue: TMouseWheelUpDownEvent);
    procedure SetOnEditStartDrag(AValue: TStartDragEvent);
    procedure SetOnEditUTF8KeyPress(AValue: TUTF8KeyPressEvent);
  protected
    function GetEditAlignment: TAlignment;
    function GetEditAutoSize: Boolean;
    function GetEditAutoSelect: Boolean;
    function GetEditCharCase: TEditCharCase;
    function GetEditCursor: TCursor;
    function GetEditDoubleBuffered: Boolean;
    function GetEditDragCursor: TCursor;
    function GetEditDragMode: TDragMode;
    function GetEditEchoMode: TEchoMode;
    function GetEditEnabled: boolean;
    function GetEditHideSelection: Boolean;
    function GetEditHint: TTranslateString;
    function GetEditMaxLength: Integer;
    function GetEditNumbersOnly: Boolean;
    function GetEditPasswordChar: Char;
    function GetEditParentColor: Boolean;
    function GetEditPopupMenu: TPopupMenu;
    function GetEditReadOnly: Boolean;
    function GetEditShowHint: Boolean;
    function GetEditTag: PtrInt;
    function GetEditTabStop: Boolean;
    function GetEditText: TCaption;
    function GetEditTextHint: TTranslateString;
    function GetLabelCaption: TCaption;
    function GetLabelSpacing: Integer;

    procedure SetAnchors(const AValue: TAnchors); override;
    procedure SetEditAlignment(const AValue: TAlignment);
    procedure SetEditAutoSize(AValue: Boolean);
    procedure SetEditAutoSelect(AValue: Boolean);
    procedure SetEditCharCase(AValue: TEditCharCase);
    procedure SetEditCursor(AValue: TCursor);
    procedure SetEditDoubleBuffered(AValue: Boolean);
    procedure SetEditDragCursor(AValue: TCursor);
    procedure SetEditDragMode(AValue: TDragMode);
    procedure SetEditEchoMode(AValue: TEchoMode);
    procedure SetEditEnabled(AValue: boolean);
    procedure SetEditHideSelection(AValue: Boolean);
    procedure SetEditHint(const AValue: TTranslateString);
    procedure SetEditMaxLength(AValue: Integer);
    procedure SetEditNumbersOnly(AValue: Boolean);
    procedure SetEditParentColor(AValue: Boolean);
    procedure SetEditPasswordChar(AValue: Char);
    procedure SetEditPopupMenu(AValue: TPopupmenu);
    procedure SetEditReadOnly(AValue: Boolean);
    procedure SetEditShowHint(AValue: Boolean);
    procedure SetEditTag(AValue: PtrInt);
    procedure SetEditTabStop(AValue: Boolean);
    procedure SetEditText(const AValue: TCaption);
    procedure SetEditTextHint(const Avalue: TTranslateString);
    procedure SetColor(AValue: TColor); override;
    procedure SetLabelCaption(const AValue: TCaption);
    procedure SetLabelSpacing(AValue: Integer);
    procedure SetName(const AValue: TComponentName); override;

    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoOnResize; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Alignment: TAlignment read GetEditAlignment write SetEditAlignment default taLeftJustify;
    property AccentColor: TColor read FAccentColor write FAccentColor;
    property Anchors;
    property AutoSelect: Boolean read GetEditAutoSelect write SetEditAutoSelect default True;
    property AutoSize: Boolean read GetEditAutoSize write SetEditAutoSize;
    property BiDiMode;
    property BorderSpacing;
    property Caption: TCaption read GetLabelCaption write SetLabelCaption;
    property CharCase: TEditCharCase read GetEditCharCase write SetEditCharCase default ecNormal;
    property Color;
    property Constraints;
    property Cursor: TCursor read GetEditCursor write SetEditCursor default crDefault;
    property DisabledColor: TColor read FDisabledColor write FDisabledColor;
    property DoubleBuffered: Boolean read GetEditDoubleBuffered write SetEditDoubleBuffered;
    property DragCursor: TCursor read GetEditDragCursor write SetEditDragCursor default crDrag;
    property DragMode: TDragMode read GetEditDragMode write SetEditDragMode default dmManual;
    property Font;
    property EchoMode: TEchoMode read GetEditEchoMode write SetEditEchoMode default emNormal;
    property Edit: TEdit read FEdit;
    property EditLabel: TBoundLabel read FLabel;
    property Enabled: boolean read GetEditEnabled write SetEditEnabled;
    property HideSelection: Boolean read GetEditHideSelection write SetEditHideSelection default True;
    property Hint: TTranslateString read GetEditHint write SetEditHint;
    property LabelSpacing: Integer read GetLabelSpacing write SetLabelSpacing default 0;
    property MaxLength: Integer read GetEditMaxLength write SetEditMaxLength default 0;
    property NumbersOnly: Boolean read GetEditNumbersOnly write SetEditNumbersOnly default False;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont default False;
    property PasswordChar: Char read GetEditPasswordChar write SetEditPasswordChar default #0;
    property PopupMenu: TPopupmenu read GetEditPopupMenu write SetEditPopupMenu;
    property ReadOnly: Boolean read GetEditReadOnly write SetEditReadOnly default False;
    property ShowHint: Boolean read GetEditShowHint write SetEditShowHint default False;
    property Tag: PtrInt read GetEditTag write SetEditTag default 0;
    property TabOrder;
    property TabStop: boolean read GetEditTabStop write SetEditTabStop default True;
    property Text: TCaption read GetEditText write SetEditText;
    property TextHint: TTranslateString read GetEditTextHint write SetEditTextHint;
    property Visible;

    property OnChange: TNotifyEvent read GetOnEditChange write SetOnEditChange;
    property OnChangeBounds;
    property OnClick: TNotifyEvent read GetOnEditClick write SetOnEditClick;
    property OnContextPopup: TContextPopupEvent read GetOnEditContextPopup write SetOnEditContextPopup;
    property OnDbClick: TNotifyEvent read GetOnEditDblClick write SetOnEditDblClick;
    property OnDragDrop: TDragDropEvent read GetOnEditDragDrop write SetOnEditDragDrop;
    property OnDragOver: TDragOverEvent read GetOnEditDragOver write SetOnEditDragOver;
    property OnEditingDone: TNotifyEvent read GetOnEditEditingDone write SetOnEditEditingDone;
    property OnEndDrag: TEndDragEvent read GetOnEditEndDrag write SetOnEditEndDrag;
    property OnEnter: TNotifyEvent read GetOnEditEnter write SetOnEditEnter;
    property OnExit: TNotifyEvent read GetOnEditExit write SetOnEditExit;
    property OnKeyDown: TKeyEvent read GetOnEditKeyDown write SetOnEditKeyDown;
    property OnKeyPress: TKeyPressEvent read GetOnEditKeyPress write SetOnEditKeyPress;
    property OnKeyUp: TKeyEvent read GetOnEditKeyUp write SetOnEditKeyUp;
    property OnMouseDown: TMouseEvent read GetOnEditMouseDown write SetOnEditMouseDown;
    property OnMouseEnter: TNotifyEvent read GetOnEditMouseEnter write SetOnEditMouseEnter;
    property OnMouseLeave: TNotifyEvent read GetOnEditMouseLeave write SetOnEditMouseLeave;
    property OnMouseMove: TMouseMoveEvent read GetOnEditMouseMove write SetOnEditMouseMove;
    property OnMouseUp: TMouseEvent read GetOnEditMouseUp write SetOnEditMouseUp;
    property OnMouseWheel: TMouseWheelEvent read GetOnEditMouseWheel write SetOnEditMouseWheel;
    property OnMouseWheelDown: TMouseWheelUpDownEvent read GetOnEditMouseWheelDown write SetOnEditMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read GetOnEditMouseWheelUp write SetOnEditMouseWheelUp;
    property OnResize;
    property OnStartDrag: TStartDragEvent read GetOnEditStartDrag write SetOnEditStartDrag;
    property OnUTF8KeyPress: TUTF8KeyPressEvent read GetOnEditUTF8KeyPress write SetOnEditUTF8KeyPress;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$IFDEF FPC}
    {$I icons\bcmaterialedit_icon.lrs}
  {$ENDIF}
  RegisterComponents('BGRA Controls', [TBCMaterialEdit]);
end;

{ TBCMaterialEdit }

function TBCMaterialEdit.GetEditAlignment: TAlignment;
begin
  result := FEdit.Alignment;
end;

function TBCMaterialEdit.GetEditAutoSize: Boolean;
begin
  result := FEdit.AutoSize;
end;

function TBCMaterialEdit.GetEditAutoSelect: Boolean;
begin
  result := FEdit.AutoSelect;
end;

function TBCMaterialEdit.GetLabelCaption: TCaption;
begin
  result := FLabel.Caption
end;

function TBCMaterialEdit.GetEditCharCase: TEditCharCase;
begin
  result := FEdit.CharCase;
end;

function TBCMaterialEdit.GetEditCursor: TCursor;
begin
  result := FEdit.Cursor;
end;

function TBCMaterialEdit.GetEditDoubleBuffered: Boolean;
begin
  result := FEdit.DoubleBuffered;
end;

function TBCMaterialEdit.GetEditDragCursor: TCursor;
begin
  result := FEdit.DragCursor;
end;

function TBCMaterialEdit.GetEditDragMode: TDragMode;
begin
  result := FEdit.DragMode;
end;

function TBCMaterialEdit.GetEditEchoMode: TEchoMode;
begin
  result := FEdit.EchoMode;
end;

function TBCMaterialEdit.GetEditEnabled: boolean;
begin
  result := FEdit.Enabled;
end;

function TBCMaterialEdit.GetEditHideSelection: Boolean;
begin
  result := FEdit.HideSelection;
end;

function TBCMaterialEdit.GetEditHint: TTranslateString;
begin
  result := FEdit.Hint;
end;

function TBCMaterialEdit.GetEditMaxLength: Integer;
begin
  result := FEdit.MaxLength;
end;

function TBCMaterialEdit.GetEditNumbersOnly: Boolean;
begin
  result := FEdit.NumbersOnly;
end;

function TBCMaterialEdit.GetEditPasswordChar: Char;
begin
  result := FEdit.PasswordChar;
end;

function TBCMaterialEdit.GetEditParentColor: Boolean;
begin
  Result := Self.ParentColor;
end;

function TBCMaterialEdit.GetEditPopupMenu: TPopupMenu;
begin
  if (csDestroying in ComponentState) then Exit(nil);

  result := FEdit.PopupMenu;
end;

function TBCMaterialEdit.GetEditReadOnly: Boolean;
begin
  result := FEdit.ReadOnly;
end;

function TBCMaterialEdit.GetEditShowHint: Boolean;
begin
  result := FEdit.ShowHint;
end;

function TBCMaterialEdit.GetEditTag: PtrInt;
begin
  result := FEdit.Tag;
end;

function TBCMaterialEdit.GetEditTabStop: Boolean;
begin
  result := FEdit.TabStop;
end;

function TBCMaterialEdit.GetEditText: TCaption;
begin
  result := FEdit.Text;
end;

function TBCMaterialEdit.GetEditTextHint: TCaption;
begin
  result := FEdit.TextHint;
end;

function TBCMaterialEdit.GetLabelSpacing: Integer;
begin
  result := FLabel.BorderSpacing.Bottom;
end;

function TBCMaterialEdit.GetOnEditChange: TNotifyEvent;
begin
  result := FEdit.OnChange;
end;

function TBCMaterialEdit.GetOnEditClick: TNotifyEvent;
begin
  result := FEdit.OnClick;
end;

function TBCMaterialEdit.GetOnEditContextPopup: TContextPopupEvent;
begin
  result := FEdit.OnContextPopup;
end;

function TBCMaterialEdit.GetOnEditDblClick: TNotifyEvent;
begin
  result := FEdit.OnDblClick;
end;

function TBCMaterialEdit.GetOnEditDragDrop: TDragDropEvent;
begin
  result := FEdit.OnDragDrop;
end;

function TBCMaterialEdit.GetOnEditDragOver: TDragOverEvent;
begin
  result := FEdit.OnDragOver;
end;

function TBCMaterialEdit.GetOnEditEditingDone: TNotifyEvent;
begin
  result := FEdit.OnEditingDone;
end;

function TBCMaterialEdit.GetOnEditEndDrag: TEndDragEvent;
begin
  result := FEdit.OnEndDrag;
end;

function TBCMaterialEdit.GetOnEditEnter: TNotifyEvent;
begin
  result := FEdit.OnEnter;
end;

function TBCMaterialEdit.GetOnEditExit: TNotifyEvent;
begin
  result := FEdit.OnExit;
end;

function TBCMaterialEdit.GetOnEditKeyDown: TKeyEvent;
begin
  result := FEdit.OnKeyDown;
end;

function TBCMaterialEdit.GetOnEditKeyPress: TKeyPressEvent;
begin
  result := FEdit.OnKeyPress;
end;

function TBCMaterialEdit.GetOnEditKeyUp: TKeyEvent;
begin
  result := FEdit.OnKeyUp;
end;

function TBCMaterialEdit.GetOnEditMouseDown: TMouseEvent;
begin
  result := FEdit.OnMouseDown;
end;

function TBCMaterialEdit.GetOnEditMouseEnter: TNotifyEvent;
begin
  result := FEdit.OnMouseEnter;
end;

function TBCMaterialEdit.GetOnEditMouseLeave: TNotifyEvent;
begin
  result := FEdit.OnMouseLeave;
end;

function TBCMaterialEdit.GetOnEditMouseMove: TMouseMoveEvent;
begin
  result := FEdit.OnMouseMove;
end;

function TBCMaterialEdit.GetOnEditMouseUp: TMouseEvent;
begin
  result := FEdit.OnMouseUp;
end;

function TBCMaterialEdit.GetOnEditMouseWheel: TMouseWheelEvent;
begin
  result := FEdit.OnMouseWheel;
end;

function TBCMaterialEdit.GetOnEditMouseWheelDown: TMouseWheelUpDownEvent;
begin
  result := FEdit.OnMouseWheelDown;
end;

function TBCMaterialEdit.GetOnEditMouseWheelUp: TMouseWheelUpDownEvent;
begin
  result := FEdit.OnMouseWheelUp;
end;

function TBCMaterialEdit.GetOnEditStartDrag: TStartDragEvent;
begin
  result := FEdit.OnStartDrag;
end;

function TBCMaterialEdit.GetOnEditUTF8KeyPress: TUTF8KeyPressEvent;
begin
  result := FEdit.OnUTF8KeyPress;
end;

procedure TBCMaterialEdit.SetOnEditChange(AValue: TNotifyEvent);
begin
  FEdit.OnChange := AValue;
end;

procedure TBCMaterialEdit.SetOnEditClick(AValue: TNotifyEvent);
begin
  FEdit.OnClick := AValue;
end;

procedure TBCMaterialEdit.SetOnEditContextPopup(AValue: TContextPopupEvent);
begin
  FEdit.OnContextPopup := AValue;
end;

procedure TBCMaterialEdit.SetOnEditDblClick(AValue: TNotifyEvent);
begin
  FEdit.OnDblClick := AValue;
end;

procedure TBCMaterialEdit.SetOnEditDragDrop(AValue: TDragDropEvent);
begin
  FEdit.OnDragDrop := AValue;
end;

procedure TBCMaterialEdit.SetOnEditDragOver(AValue: TDragOverEvent);
begin
  FEdit.OnDragOver := AValue;
end;

procedure TBCMaterialEdit.SetOnEditEditingDone(AValue: TNotifyEvent);
begin
  FEdit.OnEditingDone := AValue;
end;

procedure TBCMaterialEdit.SetOnEditEndDrag(AValue: TEndDragEvent);
begin
  FEdit.OnEndDrag := AValue;
end;

procedure TBCMaterialEdit.SetOnEditEnter(AValue: TNotifyEvent);
begin
  FEdit.OnEnter := AValue;
end;

procedure TBCMaterialEdit.SetOnEditExit(AValue: TNotifyEvent);
begin
  FEdit.OnExit := AValue;
end;

procedure TBCMaterialEdit.SetOnEditKeyDown(AValue: TKeyEvent);
begin
  FEdit.OnKeyDown := AValue;
end;

procedure TBCMaterialEdit.SetOnEditKeyPress(AValue: TKeyPressEvent);
begin
  FEdit.OnKeyPress := AValue;
end;

procedure TBCMaterialEdit.SetOnEditKeyUp(AValue: TKeyEvent);
begin
  FEdit.OnKeyUp := AValue;
end;

procedure TBCMaterialEdit.SetOnEditMouseDown(AValue: TMouseEvent);
begin
  FEdit.OnMouseDown := AValue;
end;

procedure TBCMaterialEdit.SetOnEditMouseEnter(AValue: TNotifyEvent);
begin
  FEdit.OnMouseEnter := AValue;
end;

procedure TBCMaterialEdit.SetOnEditMouseLeave(AValue: TNotifyEvent);
begin
  FEdit.OnMouseLeave := AValue;
end;

procedure TBCMaterialEdit.SetOnEditMouseMove(AValue: TMouseMoveEvent);
begin
  FEdit.OnMouseMove := AValue;
end;

procedure TBCMaterialEdit.SetOnEditMouseUp(AValue: TMouseEvent);
begin
  FEdit.OnMouseUp := AValue;
end;

procedure TBCMaterialEdit.SetOnEditMouseWheel(AValue: TMouseWheelEvent);
begin
  FEdit.OnMouseWheel := AValue;
end;

procedure TBCMaterialEdit.SetOnEditMouseWheelDown(AValue: TMouseWheelUpDownEvent);
begin
  FEdit.OnMouseWheelDown := AValue;
end;

procedure TBCMaterialEdit.SetOnEditMouseWheelUp(AValue: TMouseWheelUpDownEvent);
begin
  FEdit.OnMouseWheelUp := AValue;
end;

procedure TBCMaterialEdit.SetOnEditStartDrag(AValue: TStartDragEvent);
begin
  FEdit.OnStartDrag := AValue;
end;

procedure TBCMaterialEdit.SetOnEditUTF8KeyPress(AValue: TUTF8KeyPressEvent);
begin
  FEdit.OnUTF8KeyPress := AValue;
end;

function TBCMaterialEdit.IsNeededAdjustHeight: boolean;
begin
  if (Self.Align in [alLeft, alRight, alClient]) then Exit(False);
  if (akTop in Self.Anchors) and (akBottom in Self.Anchors) then Exit(False);
  result := FEdit.AutoSize;
end;

procedure TBCMaterialEdit.SetAnchors(const AValue: TAnchors);
begin
  if Self.Anchors <> AValue then
  begin
    inherited SetAnchors(AValue);
    Self.DoOnResize;
  end;
end;

procedure TBCMaterialEdit.SetEditAlignment(const AValue: TAlignment);
begin
  FEdit.Alignment := AValue;
end;

procedure TBCMaterialEdit.SetEditAutoSize(AValue: Boolean);
begin
  if FEdit.AutoSize <> AValue then
  begin
    FEdit.AutoSize := AValue;
    Self.DoOnResize;
  end;
end;

procedure TBCMaterialEdit.SetEditAutoSelect(AValue: Boolean);
begin
  FEdit.AutoSelect := AValue;
end;

procedure TBCMaterialEdit.SetEditCharCase(AValue: TEditCharCase);
begin
  FEdit.CharCase := AValue;
end;

procedure TBCMaterialEdit.SetEditCursor(AValue: TCursor);
begin
  FEdit.Cursor := AValue;
end;

procedure TBCMaterialEdit.SetEditDoubleBuffered(AValue: Boolean);
begin
  FEdit.DoubleBuffered := AValue;
end;

procedure TBCMaterialEdit.SetEditDragCursor(AValue: TCursor);
begin
  FEdit.DragCursor := AValue;
end;

procedure TBCMaterialEdit.SetEditDragMode(AValue: TDragMode);
begin
  FEdit.DragMode := AValue;
end;

procedure TBCMaterialEdit.SetEditEchoMode(AValue: TEchoMode);
begin
  FEdit.EchoMode := AValue;
end;

procedure TBCMaterialEdit.SetEditEnabled(AValue: boolean);
begin
  FLabel.Enabled := AValue;
  FEdit.Enabled := AValue;
  if FEdit.Enabled then
    Color := Self.Color
  else
    Color := FDisabledColor;
end;

procedure TBCMaterialEdit.SetEditHideSelection(AValue: Boolean);
begin
  FEdit.HideSelection := AValue;
end;

procedure TBCMaterialEdit.SetEditHint(const AValue: TTranslateString);
begin
  FEdit.Hint := AValue;
end;

procedure TBCMaterialEdit.SetEditMaxLength(AValue: Integer);
begin
  FEdit.MaxLength := AValue;
end;

procedure TBCMaterialEdit.SetEditNumbersOnly(AValue: Boolean);
begin
  FEdit.NumbersOnly := AValue;
end;

procedure TBCMaterialEdit.SetEditParentColor(AValue: Boolean);
begin
  FEdit.ParentColor  := AValue;
  FLabel.ParentColor := AValue;
end;

procedure TBCMaterialEdit.SetEditPasswordChar(AValue: Char);
begin
  FEdit.PasswordChar := AValue;
end;

procedure TBCMaterialEdit.SetColor(AValue: TColor);
begin
  inherited SetColor(AValue);
  if HandleAllocated and (not (csDesigning in ComponentState)) then
  begin
    FEdit.ParentColor := Self.ParentColor;
    FLabel.ParentColor := Self.ParentColor;

    if not Self.ParentColor then
    begin
      FEdit.Color := AValue;
      FLabel.Color := AValue;
    end;
    Invalidate;
  end;
end;

procedure TBCMaterialEdit.SetEditTabStop(AValue: Boolean);
begin
  FEdit.TabStop := AValue;
end;

procedure TBCMaterialEdit.SetEditPopupMenu(AValue: TPopupmenu);
begin
  FEdit.PopupMenu := AValue;
end;

procedure TBCMaterialEdit.SetEditReadOnly(AValue: Boolean);
begin
  FEdit.ReadOnly := AValue;
end;

procedure TBCMaterialEdit.SetEditShowHint(AValue: Boolean);
begin
  FEdit.ShowHint := AValue;
end;

procedure TBCMaterialEdit.SetEditTag(AValue: PtrInt);
begin
  FEdit.Tag := AValue;
end;

procedure TBCMaterialEdit.SetEditTextHint(const Avalue: TTranslateString);
begin
  FEdit.TextHint := AValue;
end;

procedure TBCMaterialEdit.SetEditText(const AValue: TCaption);
begin
  FEdit.Text := AValue;
end;

procedure TBCMaterialEdit.SetLabelCaption(const AValue: TCaption);
begin
  FLabel.Caption := AValue;
end;

procedure TBCMaterialEdit.SetLabelSpacing(AValue: Integer);
begin
  if FLabel.BorderSpacing.Bottom <> AValue then
  begin
    FLabel.BorderSpacing.Bottom := AValue;
    Self.DoOnResize;
  end;
end;

procedure TBCMaterialEdit.SetName(const AValue: TComponentName);
begin
  if (csDesigning in ComponentState) then
  begin
    if (FLabel.Caption = '') or (AnsiSameText(FLabel.Caption, AValue)) then
      FLabel.Caption := 'Label';

    if (FLabel.Name = '') or (AnsiSameText(FLabel.Name, AValue)) then
      FLabel.Name := AValue + 'SubLabel';

    if (FEdit.Text = '') or (AnsiSameText(FEdit.Text, AValue)) then
      FEdit.Text := AValue;

    if (FEdit.Name = '') or (AnsiSameText(FEdit.Name, AValue)) then
      FEdit.Name := AValue + 'SubEdit';
  end;
  inherited SetName(AValue);
end;

procedure TBCMaterialEdit.DoEnter;
begin
  inherited DoEnter;
  FFocused := True;
  Invalidate;
  FLabel.Font.Color := AccentColor;
end;

procedure TBCMaterialEdit.DoExit;
begin
  FFocused := False;
  Invalidate;
  FLabel.Font.Color := DisabledColor;
end;

procedure TBCMaterialEdit.DoOnResize;
var
  AutoSizedHeight: longint;
begin
  if IsNeededAdjustHeight then
  begin
    FEdit.Align := alBottom;
    AutoSizedHeight :=
      FLabel.Height +
      FLabel.BorderSpacing.Around +
      FLabel.BorderSpacing.Bottom +
      FLabel.BorderSpacing.Top +
      FEdit.Height +
      FEdit.BorderSpacing.Around +
      FEdit.BorderSpacing.Bottom +
      FEdit.BorderSpacing.Top;

    if Self.Height <> AutoSizedHeight then
      Self.Height := AutoSizedHeight;
  end else
  begin
    FEdit.Align := alClient;
  end;
  inherited DoOnResize;
end;

procedure TBCMaterialEdit.Paint;
var
  LeftPos, RightPos: integer;
begin
  inherited Paint;
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Color;
  Canvas.Rectangle(0, 0, Width, Height);

  if Assigned(Parent) and (Parent.Color = Color) then
  begin
    LeftPos := FEdit.Left;
    RightPos := FEdit.Left + FEdit.Width;
  end else
  begin
    LeftPos := 0;
    RightPos := Width;
  end;

  if (FFocused) and (FEdit.Enabled) then
  begin
    Canvas.Pen.Color := AccentColor;
    Canvas.Line(LeftPos, Height - 2, RightPos, Height - 2);
    Canvas.Line(LeftPos, Height - 1, RightPos, Height - 1);
  end else
  begin
    Canvas.Pen.Color := DisabledColor;
    Canvas.Line(LeftPos, Height - 1, RightPos, Height - 1);
  end;
end;

constructor TBCMaterialEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.AccentColor := clHighlight;
  Self.BorderStyle := bsNone;
  Self.Color := clWindow;
  Self.DisabledColor := $00B8AFA8;
  Self.ParentColor := False;
  FLabel := TBoundLabel.Create(Self);
  FLabel.Align := alTop;
  FLabel.AutoSize := True;
  FLabel.BorderSpacing.Around := 0;
  FLabel.BorderSpacing.Bottom := 2;
  FLabel.BorderSpacing.Left := 4;
  FLabel.BorderSpacing.Right := 4;
  FLabel.BorderSpacing.Top := 4;
  FLabel.Font.Color := $00B8AFA8;
  FLabel.Font.Style := [fsBold];
  FLabel.Parent := Self;
  FLabel.ParentFont := False;
  FLabel.ParentBiDiMode := True;
  FLabel.SetSubComponent(True);
  FEdit := TEdit.Create(Self);
  FEdit.Align := alBottom;
  FEdit.AutoSelect := True;
  FEdit.AutoSize := True;
  FEdit.BorderSpacing.Around := 0;
  FEdit.BorderSpacing.Bottom := 4;
  FEdit.BorderSpacing.Left := 4;
  FEdit.BorderSpacing.Right := 4;
  FEdit.BorderSpacing.Top := 0;
  FEdit.BorderStyle := bsNone;
  FEdit.Color := Color;
  FEdit.Font.Color := clBlack;
  FEdit.Parent := Self;
  FEdit.ParentFont := True;
  FEdit.ParentBiDiMode := True;
  FEdit.TabStop := True;
  FEdit.SetSubComponent(True);
end;

end.
