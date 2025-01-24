unit BCMaterialSpinEdit;

{$mode objfpc}{$H+}

interface

uses
  BCMaterialEdit, Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics,
  {$IFDEF FPC} LCLType, LResources, {$ENDIF} Menus, Spin, StdCtrls, SysUtils;

type

  { TBCMaterialSpinEdit }

  TBCMaterialSpinEdit = class(specialize TBCMaterialEditBase<TSpinEdit>)
  private
    function GetEditEditorEnabled: boolean;
    function GetEditIncrement: double;
    function GetEditMinValue: double;
    function GetEditMaxValue: double;
    function GetEditValue: double;

    procedure SetEditEditorEnabled(AValue: boolean);
    procedure SetEditIncrement(AValue: double);
    procedure SetEditMinValue(AValue: double);
    procedure SetEditMaxValue(AValue: double);
    procedure SetEditValue(AValue: double);

    function GetOnEditMouseWheelHorz: TMouseWheelEvent;
    function GetOnEditMouseWheelLeft: TMouseWheelUpDownEvent;
    function GetOnEditMouseWheelRight: TMouseWheelUpDownEvent;

    procedure SetOnEditMouseWheelHorz(AValue: TMouseWheelEvent);
    procedure SetOnEditMouseWheelLeft(AValue: TMouseWheelUpDownEvent);
    procedure SetOnEditMouseWheelRight(AValue: TMouseWheelUpDownEvent);
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
  //property BiDiMode;
    property BorderSpacing;
    property Caption;
  //property CharCase;
    property Color;
    property Constraints;
    property Cursor;
    property DisabledColor;
  //property DoubleBuffered;
  //property EchoMode;
    property Edit: TSpinEdit read FEdit;
    property EditorEnabled: boolean read GetEditEditorEnabled write SetEditEditorEnabled default True;
    property EditLabel;
    property Enabled;
    property Font;
    property Height;
  //property HideSelection;
    property Hint;
    property Increment: double read GetEditIncrement write SetEditIncrement;
    property Left;
    property MinValue: double read GetEditMinValue write SetEditMinValue;
  //property MaxLength;
    property MaxValue: double read GetEditMaxValue write SetEditMaxValue;
    property LabelSpacing;
    property Name;
  //property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
  //property Text;
  //property TextHint;
    property Top;
    property Value: double read GetEditValue write SetEditValue;
    property Visible;
    property Width;

    property OnChange;
    property OnChangeBounds;
    property OnClick;
    //property OnContextPopup;
    //property OnDragDrop;
    //property OnDragOver;
    property OnEditingDone;
    //property OnEndDrag;
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
    property OnMouseWheelHorz: TMouseWheelEvent read GetOnEditMouseWheelHorz write SetOnEditMouseWheelHorz;
    property OnMouseWheelLeft: TMouseWheelUpDownEvent read GetOnEditMouseWheelLeft write SetOnEditMouseWheelLeft;
    property OnMouseWheelRight: TMouseWheelUpDownEvent read GetOnEditMouseWheelRight write SetOnEditMouseWheelRight;
    property OnResize;
    //property OnStartDrag;
    property OnUTF8KeyPress;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$IFDEF FPC}
    {$I icons\bcmaterialspinedit_icon.lrs}
  {$ENDIF}
  RegisterComponents('BGRA Controls', [TBCMaterialSpinEdit]);
end;

function TBCMaterialSpinEdit.GetEditEditorEnabled: boolean;
begin
  result := FEdit.EditorEnabled;
end;

function TBCMaterialSpinEdit.GetEditIncrement: double;
begin
  result := FEdit.Increment;
end;

function TBCMaterialSpinEdit.GetEditMinValue: double;
begin
  result := FEdit.MinValue;
end;

function TBCMaterialSpinEdit.GetEditMaxValue: double;
begin
  result := FEdit.MaxValue;
end;

function TBCMaterialSpinEdit.GetEditValue: double;
begin
  result := FEdit.Value;
end;

procedure TBCMaterialSpinEdit.SetEditEditorEnabled(AValue: boolean);
begin
  FEdit.EditorEnabled := AValue;
end;

procedure TBCMaterialSpinEdit.SetEditIncrement(AValue: double);
begin
  FEdit.Increment := AValue;
end;

procedure TBCMaterialSpinEdit.SetEditMinValue(AValue: double);
begin
  FEdit.MinValue := AValue;
end;

procedure TBCMaterialSpinEdit.SetEditMaxValue(AValue: double);
begin
  FEdit.MaxValue := AValue;
end;

procedure TBCMaterialSpinEdit.SetEditValue(AValue: double);
begin
  FEdit.Value := AValue;
end;

function TBCMaterialSpinEdit.GetOnEditMouseWheelHorz: TMouseWheelEvent;
begin
  result := FEdit.OnMouseWheelHorz;
end;

function TBCMaterialSpinEdit.GetOnEditMouseWheelLeft: TMouseWheelUpDownEvent;
begin
  result := FEdit.OnMouseWheelLeft;
end;

function TBCMaterialSpinEdit.GetOnEditMouseWheelRight: TMouseWheelUpDownEvent;
begin
  result := FEdit.OnMouseWheelRight;
end;

procedure TBCMaterialSpinEdit.SetOnEditMouseWheelHorz(AValue: TMouseWheelEvent);
begin
  FEdit.OnMouseWheelHorz := AValue;
end;

procedure TBCMaterialSpinEdit. SetOnEditMouseWheelLeft(AValue: TMouseWheelUpDownEvent);
begin
  FEdit.OnMouseWheelLeft := AValue;
end;

procedure TBCMaterialSpinEdit.SetOnEditMouseWheelRight(AValue: TMouseWheelUpDownEvent);
begin
  FEdit.OnMouseWheelRight := AValue;
end;

end.
