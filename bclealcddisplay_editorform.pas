unit BCLeaLCDDisplay_EditorForm;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ExtCtrls, Buttons, BCLeaLCDDisplay;

type

  { TBCLeaLCDCharDefsEditor }

  TBCLeaLCDCharDefsEditor = class(TForm)
    btAdd: TBitBtn;
    btReplace: TBitBtn;
    btDelete: TBitBtn;
    btOK: TBitBtn;
    btCancel: TBitBtn;
    cbCharSelector: TComboBox;
    dgDotMatrix: TDrawGrid;
    ImageList1: TImageList;
    Label1: TLabel;
    pnButtons: TPanel;
    pnOKCancel: TPanel;
    procedure btAddClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btReplaceClick(Sender: TObject);
    procedure cbCharSelectorChange(Sender: TObject);
    procedure dgDotMatrixKeyDown(Sender: TObject; var Key: word;
    {%H-}Shift: TShiftState);
    procedure dgDotMatrixMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
    {%H-}Shift: TShiftState; X, Y: integer);
    procedure dgDotMatrixMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure dgDotMatrixPrepareCanvas({%H-}Sender: TObject; aCol, aRow: integer;
    {%H-}aState: TGridDrawState);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDestroy(Sender: TObject);
  private
    FBCLeaLCDDisplay: TBCLeaLCDDisplay;
    FModified: boolean;
    FSavedCharDefs: TBCLeaCharDefs;
    FSelectedChar: string;
    FTmpDotRows: TDotRows;
    FOldRow, FOldCol: integer;
    procedure SetBCLeaLCDDisplay(AValue: TBCLeaLCDDisplay);
    procedure PopulateCharSelector;
    procedure SaveCharDefs;
    procedure ClearEditorGrid;
    procedure SetupEditorGrid;
    function GetDotMatrix: TDotRows;

    function DotSet(ACol, ARow: integer): boolean;
    procedure SetDot(ACol, ARow: integer; AValue: boolean);
    procedure ToggleDot(ACol, ARow: integer);
  public
    property BCLeaLCDDisplay: TBCLeaLCDDisplay read FBCLeaLCDDisplay write SetBCLeaLCDDisplay;

  end;

var
  BCLeaLCDCharDefsEditor: TBCLeaLCDCharDefsEditor;

implementation

{$R *.lfm}

uses
  Math, ButtonPanel;

function CharInputBox(const ACaption, APrompt, ADefault: string): string;
var
  F: TForm;
  ed: TEdit;
  lbl: TLabel;
  bp: TButtonPanel;
begin
  F := TForm.CreateNew(nil);
  try
    F.Caption := ACaption;
    F.BorderStyle := bsDialog;
    lbl := TLabel.Create(F);
    lbl.AnchorSideTop.Control := F;
    lbl.AnchorSideLeft.Control := F;
    lbl.BorderSpacing.Top := F.Scale96ToFont(12);
    lbl.BorderSpacing.Bottom := F.Scale96ToFont(2);
    lbl.BorderSpacing.Left := F.Scale96ToFont(12);
    lbl.BorderSpacing.Right := F.Scale96ToFont(12);
    lbl.WordWrap := True;
    lbl.AutoSize := True;
    lbl.Caption := APrompt;
    lbl.Parent := F;
    lbl.AdjustSize;
    ed := TEdit.Create(F);
    ed.AnchorSideTop.Control := lbl;
    ed.AnchorSideTop.Side := asrBottom;
    ed.AnchorSideLeft.Control := F;
    ed.AnchorSideRight.Control := F;
    ed.AnchorSideRight.Side := asrRight;
    ed.Anchors := [akLeft, akTop, akRight];
    ed.BorderSpacing.Left := F.Scale96ToFont(12);
    ed.BorderSpacing.Right := F.Scale96ToFont(12);
    ed.BorderSpacing.Bottom := F.Scale96ToFont(18);
    ed.MaxLength := 1;
    ed.Text := ADefault;
    ed.Parent := F;
    ed.AdjustSize;
    bp := TButtonPanel.Create(F);
    bp.ShowButtons := [pbOK, pbCancel];
    bp.Parent := F;
    bp.AdjustSize;
    F.Constraints.MinHeight := ed.Top + ed.Height + ed.BorderSpacing.Bottom + bp.Height + 2 * bp.BorderSpacing.Around;
    F.AutoSize := True;
    F.Position := poScreenCenter;
    if F.ShowModal = mrOk then
      Result := ed.Text
    else
      Result := ADefault;
  finally
    F.Free;
  end;
end;

procedure TBCLeaLCDCharDefsEditor.PopulateCharSelector;
var
  i: integer;
begin
  cbCharSelector.DropdownCount := 24;
  cbCharSelector.Items.BeginUpdate;
  try
    cbCharSelector.Clear;
    for i := 0 to FBCLeaLCDDisplay.CharDefs.Count - 1 do
      cbCharSelector.Items.Add(FBCLeaLCDDisplay.CharDefs.CharByIndex[i]);
  finally
    cbCharSelector.Items.EndUpdate;
  end;
end;

procedure TBCLeaLCDCharDefsEditor.btDeleteClick(Sender: TObject);
begin
  if FSelectedChar <> '' then
  begin
    FBCLeaLCDDisplay.CharDefs.Delete(FSelectedChar);
    FBCLeaLCDDisplay.Invalidate;
  end;
  PopulateCharSelector;
end;

procedure TBCLeaLCDCharDefsEditor.btAddClick(Sender: TObject);
var
  newChar: string;
begin
  newChar := CharInputBox('Dot matrix for...', 'Character', '');
  if newChar = '' then
    exit;

  // Check whether the new character already has a dot matrix.
  if FBCLeaLCDDisplay.CharDefs.Find(newChar) then
  begin
    MessageDlg(Format('Character "%s" already exists and cannot be added.', [newChar]),
      mtError, [mbOK], 0);
    exit;
  end;

  // Add new character and its dot matrix to the BCLeaLCDDisplay...
  FSelectedChar := newChar;
  FBCLeaLCDDisplay.CharDefs.Add(FSelectedChar, GetDotMatrix);
  FBCLeaLCDDisplay.Invalidate;
  // ... and update the editor form
  PopulateCharSelector;
  cbCharSelector.ItemIndex := cbCharSelector.Items.IndexOf(FSelectedChar);
  FModified := False;
end;

{ Replaces the dotmatrix of the currently loaded character by the dotmatrix in
  the editor. }
procedure TBCLeaLCDCharDefsEditor.btReplaceClick(Sender: TObject);
begin
  if FSelectedChar <> '' then
  begin
    FBCLeaLCDDisplay.CharDefs.DotRows[FSelectedChar] := GetDotMatrix;
    FBCLeaLCDDisplay.Invalidate;
    FModified := False;
  end;
end;

procedure TBCLeaLCDCharDefsEditor.cbCharSelectorChange(Sender: TObject);
begin
  FSelectedChar := cbCharSelector.Text;
  if FSelectedChar <> '' then
    FTmpDotRows := FBCLeaLCDDisplay.CharDefs.DotRows[FSelectedChar]
  else
    ClearEditorGrid;
  dgDotMatrix.Invalidate;
  FModified := False;
end;

procedure TBCLeaLCDCharDefsEditor.dgDotMatrixKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  r, c: integer;
begin
  r := dgDotMatrix.Row;
  c := dgDotMatrix.Col;
  if Key = 32 then
  begin
    ToggleDot(c, r);
    dgDotMatrix.InvalidateCell(c, r);
    FModified := True;
  end;
end;

procedure TBCLeaLCDCharDefsEditor.dgDotMatrixMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  r, c: integer;
begin
  dgDotMatrix.MouseToCell(X, Y, c, r);
  ToggleDot(c, r);
  dgDotMatrix.InvalidateCell(c, r);
  FOldRow := r;
  FOldCol := c;
  FModified := True;
end;

procedure TBCLeaLCDCharDefsEditor.dgDotMatrixMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  r, c: integer;
begin
  if Shift = [ssLeft] then
  begin
    dgDotMatrix.MouseToCell(X, Y, c, r);
    if (c <> FOldCol) or (r <> FOldRow) then
    begin
      ToggleDot(c, r);
      dgDotMatrix.InvalidateCell(c, r);
      FOldRow := r;
      FOldCol := c;
      FModified := True;
    end;
  end;
end;

procedure TBCLeaLCDCharDefsEditor.dgDotMatrixPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
begin
  if DotSet(ACol, ARow) then
    dgDotMatrix.Canvas.Brush.Color := clBlack
  else
    dgDotMatrix.Canvas.Brush.Color := clWhite;
end;

procedure TBCLeaLCDCharDefsEditor.FormActivate(Sender: TObject);
var
  w: integer;
begin
  w := Max(btOK.Width, btCancel.Width);
  btOK.Constraints.MinWidth := w;
  btCancel.Constraints.MinWidth := w;
end;

procedure TBCLeaLCDCharDefsEditor.FormCloseQuery(Sender: TObject; var CanClose: boolean);
const
  mrReplace = -20;
  mrAddAs = -21;
var
  res: integer;
begin
  if FModified and (ModalResult = mrOk) then
  begin
    res := QuestionDlg('Confirmation', 'Current dotmatrix of "' + FSelectedChar + '" has not yet been applied. What do you want to do?',
      mtConfirmation, [mrReplace, 'Replace', mrAddAs, 'Add as...', 'isDefault', mrCancel, 'Cancel'], 0);
    case res of
      mrReplace:
        btReplaceClick(nil);
      mrAddAs:
        btAddClick(nil);
      mrCancel:
        ;
    end;
    CanClose := (res <> mrCancel) and (not FModified);
  end;

  if CanClose and (ModalResult <> mrOk) then
  begin
    FBCLeaLCDDisplay.CharDefs.Assign(FSavedCharDefs);
    FBCLeaLCDDisplay.Invalidate;
  end;
end;

procedure TBCLeaLCDCharDefsEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSavedCharDefs);
end;

function TBCLeaLCDCharDefsEditor.GetDotMatrix: TDotRows;
begin
  Result := FTmpDotRows;
end;

{ Tests whether the dot corresponding to the specified grid column and row is
  set. Note that the low-bit is at the right! }
function TBCLeaLCDCharDefsEditor.DotSet(ACol, ARow: integer): boolean;
var
  c: integer;
begin
  c := dgDotMatrix.ColCount - 1 - ACol;
  Result := FTmpDotRows[ARow] and (1 shl c) <> 0;  // avoid integer helper to keep usability with old fpc versions
end;

{ Sets the dot in the specified grid column and row if AValue is true, 
  or clears it if AValue is false. 
  Note that the low-bit is at the right of the grid! }
procedure TBCLeaLCDCharDefsEditor.SetDot(ACol, ARow: integer; AValue: boolean);
var
  c: integer;
  lDotRows: TDotRows;
begin
  c := dgDotMatrix.ColCount - 1 - ACol;
  lDotRows := CopyDotRows(FTmpDotRows);
  if AValue then
    lDotRows[ARow] := lDotRows[ARow] or (1 shl c)
  else
    lDotRows[ARow] := lDotRows[ARow] and not (1 shl c);  // avoid integer helper to keep usability with old fpc version
  FTmpDotRows := CopyDotRows(lDotRows);
end;

{ Toggles the dot in the specified grid column/row }
procedure TBCLeaLCDCharDefsEditor.ToggleDot(ACol, ARow: integer);
begin
  SetDot(ACol, ARow, not DotSet(ACol, ARow));
end;

{ Save the char defs so that they can be restored if the form is not closed by OK. }
procedure TBCLeaLCDCharDefsEditor.SaveCharDefs;
begin
  FSavedCharDefs.Free;
  FSavedCharDefs := TBCLeaCharDefs.Create(nil);
  FSavedCharDefs.Assign(FBCLeaLCDDisplay.CharDefs);
end;

procedure TBCLeaLCDCharDefsEditor.SetBCLeaLCDDisplay(AValue: TBCLeaLCDDisplay);
begin
  FBCLeaLCDDisplay := AValue;
  SetLength(FTmpDotRows, FBCLeaLCDDisplay.DotRowCount);
  SaveCharDefs;
  PopulateCharSelector;
  SetupEditorGrid;
end;

procedure TBCLeaLCDCharDefsEditor.ClearEditorGrid;
var
  i: integer;
begin
  for i := 0 to High(FTmpDotRows) do
    FTmpDotRows[i] := 0;
end;

{ Reads the size of the dot matrix from FBCLeaLCDDisplay and use it to define the
  number of rows and columns in the editor grid. }
procedure TBCLeaLCDCharDefsEditor.SetupEditorGrid;
begin
  dgDotMatrix.RowCount := FBCLeaLCDDisplay.DotRowCount;
  ClearEditorGrid;
  dgDotmatrix.ClientWidth := dgDotMatrix.ColCount * dgDotMatrix.DefaultColWidth;
  dgDotMatrix.ClientHeight := dgDotMatrix.RowCount * dgDotMatrix.DefaultRowHeight;
  dgDotMatrix.Constraints.MinWidth := dgDotMatrix.Width;
  dgDotMatrix.Constraints.MinHeight := dgDotMatrix.Height;
end;

end.
