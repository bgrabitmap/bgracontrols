unit bgrasvgimagelistform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, BCSVGViewer,
  BGRASVGImageList, ComponentEditors, Types, Math, LCLType;

type

  { TfrmBGRASVGImageListEditor }

  TfrmBGRASVGImageListEditor = class(TForm)
    BCSVGViewerPreview: TBCSVGViewer;
    btnAdd: TButton;
    btnRemove: TButton;
    btnUp: TButton;
    btnDown: TButton;
    btnReplace: TButton;
    ListBox1: TListBox;
    OpenDialog1: TOpenDialog;
    procedure btnAddClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
  private
    FControl: TControl;
    procedure UpdateListBox;
    procedure UpdateButtons;
  public
    constructor {%H-}Create(AControl: TControl);
  end;

  { TBGRASVGImageListEditor }

  TBGRASVGImageListEditor = class(TComponentEditor)
  protected
    procedure DoShowEditor;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb({%H-}Index: Integer): String; override;
    function  GetVerbCount: Integer; override;
  end;

var
  frmBGRASVGImageListEditor: TfrmBGRASVGImageListEditor;

implementation

{$R *.lfm}

{ TBGRASVGImageListEditor }

procedure TBGRASVGImageListEditor.DoShowEditor;
var
  f: TfrmBGRASVGImageListEditor;
begin
  f := TfrmBGRASVGImageListEditor.Create(TControl(Component));
  try
    f.ShowModal;
    Modified;
  finally
    f.Free;
  end;
end;

procedure TBGRASVGImageListEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: DoShowEditor;
  end;
end;

function TBGRASVGImageListEditor.GetVerb(Index: Integer): String;
begin
  Result := 'Assign style';
end;

function TBGRASVGImageListEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TfrmBGRASVGImageListEditor }

procedure TfrmBGRASVGImageListEditor.btnAddClick(Sender: TObject);
var
  s: TStringList;
begin
  if OpenDialog1.Execute then
  begin
    s := TStringList.Create;
    try
      s.LoadFromFile(OpenDialog1.FileName);
      TBGRASVGImageList(FControl).Add(s.Text);
    finally
      s.Free;
      UpdateListBox;
    end;
  end;
end;

procedure TfrmBGRASVGImageListEditor.btnDownClick(Sender: TObject);
begin
  TBGRASVGImageList(FControl).Exchange(ListBox1.ItemIndex, ListBox1.ItemIndex+1);
  UpdateListBox;
end;

procedure TfrmBGRASVGImageListEditor.btnRemoveClick(Sender: TObject);
begin
  TBGRASVGImageList(FControl).Remove(ListBox1.ItemIndex);
  UpdateListBox;
end;

procedure TfrmBGRASVGImageListEditor.btnReplaceClick(Sender: TObject);
var
  s: TStringList;
begin
  if OpenDialog1.Execute then
  begin
    s := TStringList.Create;
    try
      s.LoadFromFile(OpenDialog1.FileName);
      TBGRASVGImageList(FControl).Replace(ListBox1.ItemIndex, s.Text);
    finally
      s.Free;
      UpdateListBox;
    end;
  end;
end;

procedure TfrmBGRASVGImageListEditor.btnUpClick(Sender: TObject);
begin
  TBGRASVGImageList(FControl).Exchange(ListBox1.ItemIndex, ListBox1.ItemIndex-1);
  UpdateListBox;
end;

procedure TfrmBGRASVGImageListEditor.ListBox1DrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  ListBox1.Canvas.Brush.Color := clWhite;
  if (odSelected in State) then
    ListBox1.Canvas.Brush.Color := clHighlight;
  ListBox1.Canvas.FillRect(ARect);
  ListBox1.Canvas.TextOut(Max(TBGRASVGImageList(FControl).Width, 16) + 5, ARect.Top, Index.ToString);
  if (Index <> -1) then
    TBGRASVGImageList(FControl).Draw(Index, ListBox1.Canvas, ARect.Left, ARect.Top, Max(TBGRASVGImageList(FControl).Width, 16), Max(TBGRASVGImageList(FControl).Height, 16));
end;

procedure TfrmBGRASVGImageListEditor.ListBox1SelectionChange(Sender: TObject;
  User: boolean);
begin
  UpdateButtons;
  if ListBox1.ItemIndex <> -1 then
    BCSVGViewerPreview.SVGString := TBGRASVGImageList(FControl).Get(ListBox1.ItemIndex);
end;

procedure TfrmBGRASVGImageListEditor.UpdateListBox;
var
  i: integer;
begin
  ListBox1.Clear;
  for i:=0 to TBGRASVGImageList(FControl).Count-1 do
    ListBox1.Items.Add('Image' + i.ToString);
  if ListBox1.Count > 0 then
    ListBox1.ItemIndex := 0;
  UpdateButtons;
end;

procedure TfrmBGRASVGImageListEditor.UpdateButtons;
begin
  btnUp.Enabled := (ListBox1.Count > 1) and (ListBox1.ItemIndex > 0);
  btnDown.Enabled := (ListBox1.Count > 1) and (ListBox1.ItemIndex < ListBox1.Count-1);
  btnRemove.Enabled := (ListBox1.Count > 0) and (ListBox1.ItemIndex <> -1);
  btnReplace.Enabled := (ListBox1.Count > 0) and (ListBox1.ItemIndex <> -1);
end;

constructor TfrmBGRASVGImageListEditor.Create(AControl: TControl);
begin
  inherited Create(Application);

  FControl  := AControl;
  ListBox1.ItemHeight := Max(TBGRASVGImageList(FControl).Height, 16);
  UpdateListBox;
end;

initialization
  RegisterComponentEditor(TBGRASVGImageList, TBGRASVGImageListEditor);

end.

