unit BCMaterialEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics, {$IFDEF FPC} LCLType,
  LResources, {$ENDIF} Menus, StdCtrls, SysUtils;

type

  { TBCMaterialEditBase }

  generic TBCMaterialEditBase<T> = class(TCustomPanel)
  private
    FAccentColor: TColor;
    FDisabledColor: TColor;
    FLabel: TBoundLabel;
    FFocused: boolean;
    function IsNeededAdjustSize: boolean;

    function GetOnEditChange: TNotifyEvent;
    function GetOnEditClick: TNotifyEvent;
    function GetOnEditEditingDone: TNotifyEvent;
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
    function GetOnEditUTF8KeyPress: TUTF8KeyPressEvent;

    procedure SetOnEditChange(AValue: TNotifyEvent);
    procedure SetOnEditClick(AValue: TNotifyEvent);
    procedure SetOnEditEditingDone(AValue: TNotifyEvent);
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
    procedure SetOnEditUTF8KeyPress(AValue: TUTF8KeyPressEvent);
  protected
    FEdit: T;

    function GetEditAlignment: TAlignment;
    function GetEditAutoSize: Boolean;
    function GetEditAutoSelect: Boolean;
    function GetEditCharCase: TEditCharCase;
    function GetEditCursor: TCursor;
    function GetEditDoubleBuffered: Boolean;
    function GetEditEchoMode: TEchoMode;
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
    procedure SetColor(AValue: TColor); override;
    procedure SetEditAlignment(const AValue: TAlignment);
    procedure SetEditAutoSize(AValue: Boolean);
    procedure SetEditAutoSelect(AValue: Boolean);
    procedure SetEditCharCase(AValue: TEditCharCase);
    procedure SetEditCursor(AValue: TCursor);
    procedure SetEditDoubleBuffered(AValue: Boolean);
    procedure SetEditEchoMode(AValue: TEchoMode);
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
    property AutoSize: Boolean read GetEditAutoSize write SetEditAutoSize default True;
    property BiDiMode;
    property BorderSpacing;
    property Caption: TCaption read GetLabelCaption write SetLabelCaption;
    property CharCase: TEditCharCase read GetEditCharCase write SetEditCharCase default ecNormal;
    property Color;
    property Constraints;
    property Cursor: TCursor read GetEditCursor write SetEditCursor default crDefault;
    property DisabledColor: TColor read FDisabledColor write FDisabledColor;
    property DoubleBuffered: Boolean read GetEditDoubleBuffered write SetEditDoubleBuffered;
    property EchoMode: TEchoMode read GetEditEchoMode write SetEditEchoMode default emNormal;
    property EditLabel: TBoundLabel read FLabel;
    property Enabled;
    property Font;
    property HideSelection: Boolean read GetEditHideSelection write SetEditHideSelection default True;
    property Hint: TTranslateString read GetEditHint write SetEditHint;
    property LabelSpacing: Integer read GetLabelSpacing write SetLabelSpacing default 4;
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
    property OnEditingDone: TNotifyEvent read GetOnEditEditingDone write SetOnEditEditingDone;
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
    property OnUTF8KeyPress: TUTF8KeyPressEvent read GetOnEditUTF8KeyPress write SetOnEditUTF8KeyPress;
  end;

  { TBCMaterialEdit }

  TBCMaterialEdit = class(specialize TBCMaterialEditBase<TEdit>)
  private
    function GetEditDragCursor: TCursor;
    function GetEditDragMode: TDragMode;

    function GetOnEditContextPopup: TContextPopupEvent;
    function GetOnEditDblClick: TNotifyEvent;
    function GetOnEditDragDrop: TDragDropEvent;
    function GetOnEditDragOver: TDragOverEvent;
    function GetOnEditEndDrag: TEndDragEvent;
    function GetOnEditStartDrag: TStartDragEvent;

    procedure SetEditDragCursor(AValue: TCursor);
    procedure SetEditDragMode(AValue: TDragMode);

    procedure SetOnEditContextPopup(AValue: TContextPopupEvent);
    procedure SetOnEditDblClick(AValue: TNotifyEvent);
    procedure SetOnEditDragDrop(AValue: TDragDropEvent);
    procedure SetOnEditDragOver(AValue: TDragOverEvent);
    procedure SetOnEditEndDrag(AValue: TEndDragEvent);
    procedure SetOnEditStartDrag(AValue: TStartDragEvent);
  published
    property Align;
    property Alignment;
    property AccentColor;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property Caption;
    property CharCase;
    property Color;
    property Constraints;
    property Cursor;
    property DisabledColor;
    property DoubleBuffered;
    property DragCursor: TCursor read GetEditDragCursor write SetEditDragCursor default crDrag;
    property DragMode: TDragMode read GetEditDragMode write SetEditDragMode default dmManual;
    property Font;
    property EchoMode;
    property Edit: TEdit read FEdit;
    property EditLabel;
    property Enabled;
    property HideSelection;
    property Hint;
    property LabelSpacing;
    property MaxLength;
    property NumbersOnly;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property Tag;
    property TabOrder;
    property TabStop;
    property Text;
    property TextHint;
    property Visible;

    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDbClick: TNotifyEvent read GetOnEditDblClick write SetOnEditDblClick;
    property OnDragDrop: TDragDropEvent read GetOnEditDragDrop write SetOnEditDragDrop;
    property OnDragOver: TDragOverEvent read GetOnEditDragOver write SetOnEditDragOver;
    property OnEditingDone;
    property OnEndDrag: TEndDragEvent read GetOnEditEndDrag write SetOnEditEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag: TStartDragEvent read GetOnEditStartDrag write SetOnEditStartDrag;
    property OnUTF8KeyPress;
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

{ TBCMaterialEditBase }

function TBCMaterialEditBase.GetEditAlignment: TAlignment;
begin
  result := FEdit.Alignment;
end;

function TBCMaterialEditBase.GetEditAutoSize: Boolean;
begin
  result := FEdit.AutoSize;
end;

function TBCMaterialEditBase.GetEditAutoSelect: Boolean;
begin
  result := FEdit.AutoSelect;
end;

function TBCMaterialEditBase.GetEditCharCase: TEditCharCase;
begin
  result := FEdit.CharCase;
end;

function TBCMaterialEditBase.GetEditCursor: TCursor;
begin
  result := FEdit.Cursor;
end;

function TBCMaterialEditBase.GetEditDoubleBuffered: Boolean;
begin
  result := FEdit.DoubleBuffered;
end;

function TBCMaterialEditBase.GetEditEchoMode: TEchoMode;
begin
  result := FEdit.EchoMode;
end;

function TBCMaterialEditBase.GetEditHideSelection: Boolean;
begin
  result := FEdit.HideSelection;
end;

function TBCMaterialEditBase.GetEditHint: TTranslateString;
begin
  result := FEdit.Hint;
end;

function TBCMaterialEditBase.GetEditMaxLength: Integer;
begin
  result := FEdit.MaxLength;
end;

function TBCMaterialEditBase.GetEditNumbersOnly: Boolean;
begin
  result := FEdit.NumbersOnly;
end;

function TBCMaterialEditBase.GetEditPasswordChar: Char;
begin
  result := FEdit.PasswordChar;
end;

function TBCMaterialEditBase.GetEditParentColor: Boolean;
begin
  Result := Self.ParentColor;
end;

function TBCMaterialEditBase.GetEditPopupMenu: TPopupMenu;
begin
  if (csDestroying in ComponentState) then Exit(nil);

  result := FEdit.PopupMenu;
end;

function TBCMaterialEditBase.GetEditReadOnly: Boolean;
begin
  result := FEdit.ReadOnly;
end;

function TBCMaterialEditBase.GetEditShowHint: Boolean;
begin
  result := FEdit.ShowHint;
end;

function TBCMaterialEditBase.GetEditTag: PtrInt;
begin
  result := FEdit.Tag;
end;

function TBCMaterialEditBase.GetEditTabStop: Boolean;
begin
  result := FEdit.TabStop;
end;

function TBCMaterialEditBase.GetEditText: TCaption;
begin
  result := FEdit.Text;
end;

function TBCMaterialEditBase.GetEditTextHint: TCaption;
begin
  result := FEdit.TextHint;
end;

function TBCMaterialEditBase.GetLabelCaption: TCaption;
begin
  result := FLabel.Caption
end;

function TBCMaterialEditBase.GetLabelSpacing: Integer;
begin
  result := FLabel.BorderSpacing.Bottom;
end;

function TBCMaterialEditBase.GetOnEditChange: TNotifyEvent;
begin
  result := FEdit.OnChange;
end;

function TBCMaterialEditBase.GetOnEditClick: TNotifyEvent;
begin
  result := FEdit.OnClick;
end;

function TBCMaterialEditBase.GetOnEditEditingDone: TNotifyEvent;
begin
  result := FEdit.OnEditingDone;
end;

function TBCMaterialEditBase.GetOnEditEnter: TNotifyEvent;
begin
  result := FEdit.OnEnter;
end;

function TBCMaterialEditBase.GetOnEditExit: TNotifyEvent;
begin
  result := FEdit.OnExit;
end;

function TBCMaterialEditBase.GetOnEditKeyDown: TKeyEvent;
begin
  result := FEdit.OnKeyDown;
end;

function TBCMaterialEditBase.GetOnEditKeyPress: TKeyPressEvent;
begin
  result := FEdit.OnKeyPress;
end;

function TBCMaterialEditBase.GetOnEditKeyUp: TKeyEvent;
begin
  result := FEdit.OnKeyUp;
end;

function TBCMaterialEditBase.GetOnEditMouseDown: TMouseEvent;
begin
  result := FEdit.OnMouseDown;
end;

function TBCMaterialEditBase.GetOnEditMouseEnter: TNotifyEvent;
begin
  result := FEdit.OnMouseEnter;
end;

function TBCMaterialEditBase.GetOnEditMouseLeave: TNotifyEvent;
begin
  result := FEdit.OnMouseLeave;
end;

function TBCMaterialEditBase.GetOnEditMouseMove: TMouseMoveEvent;
begin
  result := FEdit.OnMouseMove;
end;

function TBCMaterialEditBase.GetOnEditMouseUp: TMouseEvent;
begin
  result := FEdit.OnMouseUp;
end;

function TBCMaterialEditBase.GetOnEditMouseWheel: TMouseWheelEvent;
begin
  result := FEdit.OnMouseWheel;
end;

function TBCMaterialEditBase.GetOnEditMouseWheelDown: TMouseWheelUpDownEvent;
begin
  result := FEdit.OnMouseWheelDown;
end;

function TBCMaterialEditBase.GetOnEditMouseWheelUp: TMouseWheelUpDownEvent;
begin
  result := FEdit.OnMouseWheelUp;
end;

function TBCMaterialEditBase.GetOnEditUTF8KeyPress: TUTF8KeyPressEvent;
begin
  result := FEdit.OnUTF8KeyPress;
end;

procedure TBCMaterialEditBase.SetOnEditChange(AValue: TNotifyEvent);
begin
  FEdit.OnChange := AValue;
end;

procedure TBCMaterialEditBase.SetOnEditClick(AValue: TNotifyEvent);
begin
  FEdit.OnClick := AValue;
end;

procedure TBCMaterialEditBase.SetOnEditEditingDone(AValue: TNotifyEvent);
begin
  FEdit.OnEditingDone := AValue;
end;

procedure TBCMaterialEditBase.SetOnEditEnter(AValue: TNotifyEvent);
begin
  FEdit.OnEnter := AValue;
end;

procedure TBCMaterialEditBase.SetOnEditExit(AValue: TNotifyEvent);
begin
  FEdit.OnExit := AValue;
end;

procedure TBCMaterialEditBase.SetOnEditKeyDown(AValue: TKeyEvent);
begin
  FEdit.OnKeyDown := AValue;
end;

procedure TBCMaterialEditBase.SetOnEditKeyPress(AValue: TKeyPressEvent);
begin
  FEdit.OnKeyPress := AValue;
end;

procedure TBCMaterialEditBase.SetOnEditKeyUp(AValue: TKeyEvent);
begin
  FEdit.OnKeyUp := AValue;
end;

procedure TBCMaterialEditBase.SetOnEditMouseDown(AValue: TMouseEvent);
begin
  FEdit.OnMouseDown := AValue;
end;

procedure TBCMaterialEditBase.SetOnEditMouseEnter(AValue: TNotifyEvent);
begin
  FEdit.OnMouseEnter := AValue;
end;

procedure TBCMaterialEditBase.SetOnEditMouseLeave(AValue: TNotifyEvent);
begin
  FEdit.OnMouseLeave := AValue;
end;

procedure TBCMaterialEditBase.SetOnEditMouseMove(AValue: TMouseMoveEvent);
begin
  FEdit.OnMouseMove := AValue;
end;

procedure TBCMaterialEditBase.SetOnEditMouseUp(AValue: TMouseEvent);
begin
  FEdit.OnMouseUp := AValue;
end;

procedure TBCMaterialEditBase.SetOnEditMouseWheel(AValue: TMouseWheelEvent);
begin
  FEdit.OnMouseWheel := AValue;
end;

procedure TBCMaterialEditBase.SetOnEditMouseWheelDown(AValue: TMouseWheelUpDownEvent);
begin
  FEdit.OnMouseWheelDown := AValue;
end;

procedure TBCMaterialEditBase.SetOnEditMouseWheelUp(AValue: TMouseWheelUpDownEvent);
begin
  FEdit.OnMouseWheelUp := AValue;
end;

procedure TBCMaterialEditBase.SetOnEditUTF8KeyPress(AValue: TUTF8KeyPressEvent);
begin
  FEdit.OnUTF8KeyPress := AValue;
end;

function TBCMaterialEditBase.IsNeededAdjustSize: boolean;
begin
  if (Self.Align in [alLeft, alRight, alClient]) then Exit(False);
  if (akTop in Self.Anchors) and (akBottom in Self.Anchors) then Exit(False);
  result := FEdit.AutoSize;
end;

procedure TBCMaterialEditBase.SetAnchors(const AValue: TAnchors);
begin
  if (Self.Anchors = AValue) then Exit;
  inherited SetAnchors(AValue);

  if not (csLoading in ComponentState) then Self.DoOnResize;
end;

procedure TBCMaterialEditBase.SetColor(AValue: TColor);
begin
  inherited SetColor(AValue);
  FEdit.Color := AValue;
end;

procedure TBCMaterialEditBase.SetEditAlignment(const AValue: TAlignment);
begin
  FEdit.Alignment := AValue;
end;

procedure TBCMaterialEditBase.SetEditAutoSize(AValue: Boolean);
begin
  if (FEdit.AutoSize = AValue) then Exit;
  FEdit.AutoSize := AValue;

  if not (csLoading in ComponentState) then Self.DoOnResize;
end;

procedure TBCMaterialEditBase.SetEditAutoSelect(AValue: Boolean);
begin
  FEdit.AutoSelect := AValue;
end;

procedure TBCMaterialEditBase.SetEditCharCase(AValue: TEditCharCase);
begin
  FEdit.CharCase := AValue;
end;

procedure TBCMaterialEditBase.SetEditCursor(AValue: TCursor);
begin
  FEdit.Cursor := AValue;
end;

procedure TBCMaterialEditBase.SetEditDoubleBuffered(AValue: Boolean);
begin
  FEdit.DoubleBuffered := AValue;
end;

procedure TBCMaterialEditBase.SetEditEchoMode(AValue: TEchoMode);
begin
  FEdit.EchoMode := AValue;
end;

procedure TBCMaterialEditBase.SetEditHideSelection(AValue: Boolean);
begin
  FEdit.HideSelection := AValue;
end;

procedure TBCMaterialEditBase.SetEditHint(const AValue: TTranslateString);
begin
  FEdit.Hint := AValue;
end;

procedure TBCMaterialEditBase.SetEditMaxLength(AValue: Integer);
begin
  FEdit.MaxLength := AValue;
end;

procedure TBCMaterialEditBase.SetEditNumbersOnly(AValue: Boolean);
begin
  FEdit.NumbersOnly := AValue;
end;

procedure TBCMaterialEditBase.SetEditParentColor(AValue: Boolean);
begin
  FEdit.ParentColor  := AValue;
  FLabel.ParentColor := AValue;
end;

procedure TBCMaterialEditBase.SetEditPasswordChar(AValue: Char);
begin
  FEdit.PasswordChar := AValue;
end;

procedure TBCMaterialEditBase.SetEditTabStop(AValue: Boolean);
begin
  FEdit.TabStop := AValue;
end;

procedure TBCMaterialEditBase.SetEditPopupMenu(AValue: TPopupmenu);
begin
  FEdit.PopupMenu := AValue;
end;

procedure TBCMaterialEditBase.SetEditReadOnly(AValue: Boolean);
begin
  FEdit.ReadOnly := AValue;
end;

procedure TBCMaterialEditBase.SetEditShowHint(AValue: Boolean);
begin
  FEdit.ShowHint := AValue;
end;

procedure TBCMaterialEditBase.SetEditTag(AValue: PtrInt);
begin
  FEdit.Tag := AValue;
end;

procedure TBCMaterialEditBase.SetEditTextHint(const Avalue: TTranslateString);
begin
  FEdit.TextHint := AValue;
end;

procedure TBCMaterialEditBase.SetEditText(const AValue: TCaption);
begin
  FEdit.Text := AValue;
end;

procedure TBCMaterialEditBase.SetLabelCaption(const AValue: TCaption);
begin
  FLabel.Caption := AValue;
end;

procedure TBCMaterialEditBase.SetLabelSpacing(AValue: Integer);
begin
  if (FLabel.BorderSpacing.Bottom = AValue) then Exit;
  FLabel.BorderSpacing.Bottom := AValue;

  if not (csLoading in ComponentState) then Self.DoOnResize;
end;

procedure TBCMaterialEditBase.SetName(const AValue: TComponentName);
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

procedure TBCMaterialEditBase.DoEnter;
begin
  inherited DoEnter;
  FFocused := True;
  Invalidate;
end;

procedure TBCMaterialEditBase.DoExit;
begin
  FFocused := False;
  Invalidate;
  inherited DoExit;
end;

procedure TBCMaterialEditBase.DoOnResize;
var
  AutoSizedHeight: longint;
begin
  if IsNeededAdjustSize then
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

procedure TBCMaterialEditBase.Paint;
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

  if (FFocused) and (Self.Enabled) then
  begin
    Canvas.Pen.Color := AccentColor;
    Canvas.Line(LeftPos, Height - 2, RightPos, Height - 2);
    Canvas.Line(LeftPos, Height - 1, RightPos, Height - 1);
    FLabel.Font.Color := AccentColor;
  end else
  begin
    Canvas.Pen.Color := DisabledColor;
    Canvas.Line(LeftPos, Height - 1, RightPos, Height - 1);
    FLabel.Font.Color := DisabledColor;
  end;
end;

constructor TBCMaterialEditBase.Create(AOwner: TComponent);
begin
  FEdit := T.Create(Self);
  FLabel := TBoundLabel.Create(Self);
  inherited Create(AOwner);
  Self.AccentColor := clHighlight;
  Self.BorderStyle := bsNone;
  Self.Color := clWindow;
  Self.DisabledColor := $00B8AFA8;
  Self.ParentColor := False;

  FLabel.Align := alTop;
  FLabel.AutoSize := True;
  FLabel.BorderSpacing.Around := 0;
  FLabel.BorderSpacing.Bottom := 4;
  FLabel.BorderSpacing.Left := 4;
  FLabel.BorderSpacing.Right := 4;
  FLabel.BorderSpacing.Top := 4;
  FLabel.Font.Color := $00B8AFA8;
  FLabel.Font.Style := [fsBold];
  FLabel.Parent := Self;
  FLabel.ParentFont := False;
  FLabel.ParentBiDiMode := True;
  FLabel.SetSubComponent(True);

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

{ TBCMaterialEdit }

function TBCMaterialEdit.GetEditDragCursor: TCursor;
begin
  result := FEdit.DragCursor;
end;

function TBCMaterialEdit.GetEditDragMode: TDragMode;
begin
  result := FEdit.DragMode;
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

function TBCMaterialEdit.GetOnEditEndDrag: TEndDragEvent;
begin
  result := FEdit.OnEndDrag;
end;

function TBCMaterialEdit.GetOnEditStartDrag: TStartDragEvent;
begin
  result := FEdit.OnStartDrag;
end;

procedure TBCMaterialEdit.SetEditDragCursor(AValue: TCursor);
begin
  FEdit.DragCursor := AValue;
end;

procedure TBCMaterialEdit.SetEditDragMode(AValue: TDragMode);
begin
  FEdit.DragMode := AValue;
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

procedure TBCMaterialEdit.SetOnEditEndDrag(AValue: TEndDragEvent);
begin
  FEdit.OnEndDrag := AValue;
end;

procedure TBCMaterialEdit.SetOnEditStartDrag(AValue: TStartDragEvent);
begin
  FEdit.OnStartDrag := AValue;
end;

end.
