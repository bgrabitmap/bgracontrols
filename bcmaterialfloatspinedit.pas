unit BCMaterialFloatSpinEdit;

{$mode objfpc}{$H+}

interface

uses
  BCMaterialEdit, Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics,
  {$IFDEF FPC} LCLType, LResources, {$ENDIF} Menus, Spin, StdCtrls, SysUtils;

type

  { TBCMaterialFloatSpinEdit }

  TBCMaterialFloatSpinEdit = class(specialize TBCMaterialEditBase<TFloatSpinEdit>)
  private
    function GetEditDecimalPlaces: integer;
    function GetEditEditorEnabled: boolean;
    function GetEditIncrement: double;
    function GetEditMinValue: double;
    function GetEditMaxValue: double;
    function GetEditValue: double;
    function GetEditValueEmpty: boolean;

    procedure SetEditDecimalPlaces(AValue: integer);
    procedure SetEditEditorEnabled(AValue: boolean);
    procedure SetEditIncrement(AValue: double);
    procedure SetEditMinValue(AValue: double);
    procedure SetEditMaxValue(AValue: double);
    procedure SetEditValue(AValue: double);
    procedure SetEditValueEmpty(AValue: boolean);

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
    property DecimalPlaces: integer read GetEditDecimalPlaces write SetEditDecimalPlaces;
    property DisabledColor;
  //property DoubleBuffered;
  //property EchoMode;
    property Edit: TFloatSpinEdit read FEdit;
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
  //property Text;
  //property TextHint;
    property Tag;
    property Top;
    property Value: double read GetEditValue write SetEditValue;
    property ValueEmpty: boolean read GetEditValueEmpty write SetEditValueEmpty default False;
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
    {$I icons\bcmaterialfloatspinedit_icon.lrs}
  {$ENDIF}
  RegisterComponents('BGRA Controls', [TBCMaterialFloatSpinEdit]);
end;

function TBCMaterialFloatSpinEdit.GetEditDecimalPlaces: integer;
begin
  result := FEdit.DecimalPlaces;
end;

function TBCMaterialFloatSpinEdit.GetEditEditorEnabled: boolean;
begin
  result := FEdit.EditorEnabled;
end;

function TBCMaterialFloatSpinEdit.GetEditIncrement: double;
begin
  result := FEdit.Increment;
end;

function TBCMaterialFloatSpinEdit.GetEditMinValue: double;
begin
  result := FEdit.MinValue;
end;

function TBCMaterialFloatSpinEdit.GetEditMaxValue: double;
begin
  result := FEdit.MaxValue;
end;

function TBCMaterialFloatSpinEdit.GetEditValue: double;
begin
  result := FEdit.Value;
end;

function TBCMaterialFloatSpinEdit.GetEditValueEmpty: boolean;
begin
  result := FEdit.ValueEmpty;
end;

procedure TBCMaterialFloatSpinEdit.SetEditDecimalPlaces(AValue: integer);
begin
  FEdit.DecimalPlaces := AValue;
end;

procedure TBCMaterialFloatSpinEdit.SetEditEditorEnabled(AValue: boolean);
begin
  FEdit.EditorEnabled := AValue;
end;

procedure TBCMaterialFloatSpinEdit.SetEditIncrement(AValue: double);
begin
  FEdit.Increment := AValue;
end;

procedure TBCMaterialFloatSpinEdit.SetEditMinValue(AValue: double);
begin
  FEdit.MinValue := AValue;
end;

procedure TBCMaterialFloatSpinEdit.SetEditMaxValue(AValue: double);
begin
  FEdit.MaxValue := AValue;
end;

procedure TBCMaterialFloatSpinEdit.SetEditValue(AValue: double);
begin
  FEdit.Value := AValue;
end;

procedure TBCMaterialFloatSpinEdit.SetEditValueEmpty(AValue: boolean);
begin
  FEdit.ValueEmpty := AValue;
end;

function TBCMaterialFloatSpinEdit.GetOnEditMouseWheelHorz: TMouseWheelEvent;
begin
  result := FEdit.OnMouseWheelHorz;
end;

function TBCMaterialFloatSpinEdit.GetOnEditMouseWheelLeft: TMouseWheelUpDownEvent;
begin
  result := FEdit.OnMouseWheelLeft;
end;

function TBCMaterialFloatSpinEdit.GetOnEditMouseWheelRight: TMouseWheelUpDownEvent;
begin
  result := FEdit.OnMouseWheelRight;
end;

procedure TBCMaterialFloatSpinEdit.SetOnEditMouseWheelHorz(AValue: TMouseWheelEvent);
begin
  FEdit.OnMouseWheelHorz := AValue;
end;

procedure TBCMaterialFloatSpinEdit. SetOnEditMouseWheelLeft(AValue: TMouseWheelUpDownEvent);
begin
  FEdit.OnMouseWheelLeft := AValue;
end;

procedure TBCMaterialFloatSpinEdit.SetOnEditMouseWheelRight(AValue: TMouseWheelUpDownEvent);
begin
  FEdit.OnMouseWheelRight := AValue;
end;

end.
