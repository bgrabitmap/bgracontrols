unit BCLeaLCDDisplay_Editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, ComponentEditors,
  BCLeaLCDDisplay;

type
  TBCLeaLCDDisplayCharDefsPropertyEditor = class(TPersistentPropertyEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: integer); override;
    function GetAttributes: TPropertyAttributes; override;
    function GetVerb(Index: integer): string; override;
    function GetVerbCount: integer; override;
    function BCLeaLCDDisplay: TBCLeaLCDDisplay;
  end;

  TBCLeaLCDDisplayComponentEditor = class(TComponentEditor)
  private
    procedure EditLines;
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
    function GetVerbCount: integer; override;
    function BCLeaLCDDisplay: TBCLeaLCDDisplay;
  end;

procedure EditCharDefs(ABCLeaLCDDisplay: TBCLeaLCDDisplay);

implementation

uses
  Controls, StdCtrls, Dialogs, ButtonPanel, Forms,
  BCLeaLCDDisplay_EditorForm;

{ Opens the char def editor. }
procedure EditCharDefs(ABCLeaLCDDisplay: TBCLeaLCDDisplay);
var
  F: TBCLeaLCDCharDefsEditor;
begin
  F := TBCLeaLCDCharDefsEditor.Create(nil);
  try
    F.Position := poScreenCenter;
    F.BCLeaLCDDisplay := TBCLeaLCDDisplay(ABCLeaLCDDisplay);
    F.ShowModal;  // Cancel has been handled by the editor form.
  finally
    F.Free;
  end;
end;

{ Loads the char defs of the specified BCLeaLCDDisplay from an xml file. }
procedure LoadCharDefsFromFile(ABCLeaLCDDisplay: TBCLeaLCDDisplay);
var
  dlg: TOpenDialog;
begin
  dlg := TOpenDialog.Create(nil);
  try
    dlg.FileName := '';
    dlg.Filter := 'XML files (*.xml)|*.xml';
    if dlg.Execute then
    begin
      ABCLeaLCDDisplay.CharDefs.LoadFromFile(dlg.FileName);
      ABCLeaLCDDisplay.Invalidate;
    end;
  finally
    dlg.Free;
  end;
end;

{ Saves the chardefs of the specified BCLeaLCDDisplay to an xml file. }
procedure SaveCharDefsToFile(ABCLeaLCDDisplay: TBCLeaLCDDisplay);
var
  dlg: TOpenDialog;
begin
  dlg := TSaveDialog.Create(nil);
  try
    dlg.FileName := '';
    dlg.Filter := 'XML files (*.xml)|*.xml';
    if dlg.Execute then
      ABCLeaLCDDisplay.CharDefs.SaveToFile(dlg.FileName);
  finally
    dlg.Free;
  end;
end;


{ TBCLeaLCDDisplayCharDefsPropertyEditor }

{ Opens the chardefs editor. }
procedure TBCLeaLCDDisplayCharDefsPropertyEditor.Edit;
begin
  EditCharDefs(BCLeaLCDDisplay);
end;

{ Executes the routines assigned to the CharDefs context menu }
procedure TBCLeaLCDDisplayCharDefsPropertyEditor.ExecuteVerb(Index: integer);
begin
  case Index of
    0: Edit;
    1: LoadCharDefsFromFile(BCLeaLCDDisplay);
    2: SaveCharDefsToFile(BCLeaLCDDisplay);
  end;
end;

{ The property editor should open the CharDefs editor. }
function TBCLeaLCDDisplayCharDefsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{ Determines how many items will be added to the CharDefs context menu. }
function TBCLeaLCDDisplayCharDefsPropertyEditor.GetVerbCount: integer;
begin
  Result := 3;
end;

{ Determines the menu item text for CharDefs context menu. }
function TBCLeaLCDDisplayCharDefsPropertyEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := 'Edit...';
    1: Result := 'Load from file...';
    2: Result := 'Save to file...';
  end;
end;

function TBCLeaLCDDisplayCharDefsPropertyEditor.BCLeaLCDDisplay: TBCLeaLCDDisplay;
begin
  Result := TBCLeaLCDDisplay(GetComponent(0));
end;


{ TBCLeaLCDDisplayComponentEditor }

procedure TBCLeaLCDDisplayComponentEditor.Edit;
begin
  ExecuteVerb(0);
end;

procedure TBCLeaLCDDisplayComponentEditor.EditLines;
var
  F: TForm;
  Memo: TMemo;
begin
  F := TForm.CreateNew(nil);
  try
    F.Caption := 'Edit BCLeaLCDDisplay text';
    F.Position := poScreenCenter;
    F.Width := 300;
    F.Height := 200;
    Memo := TMemo.Create(F);
    with Memo do
    begin
      Align := alClient;
      BorderSpacing.Around := 8;
      Parent := F;
      Lines.Assign(BCLeaLCDDisplay.Lines);
    end;
    with TButtonPanel.Create(F) do
    begin
      ShowButtons := [pbOK, pbCancel];
      Parent := F;
    end;
    if F.ShowModal = mrOk then
    begin
      BCLeaLCDDisplay.Lines.Assign(Memo.Lines);
      BCLeaLCDDisplay.Invalidate;
    end;
  finally
    F.Free;
  end;
end;

procedure TBCLeaLCDDisplayComponentEditor.ExecuteVerb(Index: integer);
begin
  case Index of
    0: EditLines;
    1: EditCharDefs(BCLeaLCDDisplay);
    2: LoadCharDefsFromFile(BCLeaLCDDisplay);
    3: SaveCharDefsToFile(BCLeaLCDDisplay);
  end;
end;

{ Determines how many items will be added to the BCLeaLCDDisplay context menu. }
function TBCLeaLCDDisplayComponentEditor.GetVerbCount: integer;
begin
  Result := 4;
end;

{ Determines the menu item text for BCLeaLCDDisplay context menu. }
function TBCLeaLCDDisplayComponentEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := 'Lines text...';
    1: Result := 'Edit character defs...';
    2: Result := 'Load character defs from file...';
    3: Result := 'Save character defs to file...';
  end;
end;

function TBCLeaLCDDisplayComponentEditor.BCLeaLCDDisplay: TBCLeaLCDDisplay;
begin
  Result := TBCLeaLCDDisplay(GetComponent);
end;

end.
