unit BCMaterialFloatSpinEdit;

{$mode objfpc}{$H+}

interface

uses
  BCMaterialEdit, Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics,
  {$IFDEF FPC} LCLType, LResources, {$ENDIF} Menus, Spin, StdCtrls, SysUtils;

type

  { TBCMaterialFloatSpinEdit }

  TBCMaterialFloatSpinEdit = class(specialize TBCMaterialEditBase<TFloatSpinEdit>)
  public
    property AutoSelect;
  published
    property Align;
    property Alignment;
    property AccentColor;
    property Anchors;
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
    property DragCursor;
    property DragMode;
    property Font;
    property EchoMode;
    property Edit: TFloatSpinEdit read FEdit;
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
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
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
    property OnStartDrag;
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

end.
