
{**********************************************************************
 Register the BCLeaLCDDisplay editor
***********************************************************************}

unit BCLeaLCDDisplay_EditorRegister;

interface

uses
  Classes, LResources, BCLeaLCDDisplay, BCLeaLCDDisplay_Editor;

procedure Register;

implementation

uses
  PropEdits, ComponentEditors;

//==========================================================
procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TBCLeaCharDefs), TBCLeaLCDDisplay, 'CharDefs', TBCLeaLCDDisplayCharDefsPropertyEditor);
  RegisterComponentEditor(TBCLeaLCDDisplay, TBCLeaLCDDisplayComponentEditor);
end;

end.
