unit bgrasvgimagelistform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, BCSVGViewer,
  BGRASVGImageList, ComponentEditors, Types, Math, LCLType, ComCtrls;

type

  { TfrmBGRASVGImageListEditor }

  TfrmBGRASVGImageListEditor = class(TForm)
    BCSVGViewerPreview: TBCSVGViewer;
    btnAdd: TButton;
    btnRemove: TButton;
    btnUp: TButton;
    btnDown: TButton;
    btnReplace: TButton;
    CheckBox_UseSVGAlignment: TCheckBox;
    ImageList1: TImageList;
    ListBox1: TListBox;
    OpenDialog1: TOpenDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton_AlignTop: TToolButton;
    ToolButton_AlignLeft: TToolButton;
    ToolButton_AlignCenter: TToolButton;
    ToolButton_AlignRight: TToolButton;
    ToolButton_AlignVCenter: TToolButton;
    ToolButton_AlignBottom: TToolButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure CheckBox_UseSVGAlignmentChange(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
    procedure ToolButton_AlignBottomClick(Sender: TObject);
    procedure ToolButton_AlignCenterClick(Sender: TObject);
    procedure ToolButton_AlignLeftClick(Sender: TObject);
    procedure ToolButton_AlignRightClick(Sender: TObject);
    procedure ToolButton_AlignTopClick(Sender: TObject);
    procedure ToolButton_AlignVCenterClick(Sender: TObject);
  private
    FComponent: TComponent;
    function GetImageList: TBGRASVGImageList;
    procedure UpdateListBox;
    procedure UpdateButtons;
    procedure UpdateToolButtonsAlign;
  public
    constructor {%H-}Create(AComponent: TComponent);
    property ImageList: TBGRASVGImageList read GetImageList;
  end;

  { TBGRASVGImageListEditor }

  TBGRASVGImageListEditor = class(TComponentEditor)
  protected
    procedure DoShowEditor;
  public
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb({%H-}Index: integer): string; override;
    function GetVerbCount: integer; override;
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
  f := TfrmBGRASVGImageListEditor.Create(Component);
  try
    f.ShowModal;
    Modified;
  finally
    f.Free;
  end;
end;

procedure TBGRASVGImageListEditor.ExecuteVerb(Index: integer);
begin
  case Index of
    0: DoShowEditor;
  end;
end;

function TBGRASVGImageListEditor.GetVerb(Index: integer): string;
begin
  Result := 'Assign style';
end;

function TBGRASVGImageListEditor.GetVerbCount: integer;
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
      TBGRASVGImageList(FComponent).Add(s.Text);
    finally
      s.Free;
      UpdateListBox;
      ListBox1.ItemIndex := ListBox1.Count - 1;
    end;
  end;
end;

procedure TfrmBGRASVGImageListEditor.btnDownClick(Sender: TObject);
begin
  TBGRASVGImageList(FComponent).Exchange(ListBox1.ItemIndex, ListBox1.ItemIndex + 1);
  UpdateListBox;
  ListBox1.ItemIndex := ListBox1.ItemIndex + 1;
end;

procedure TfrmBGRASVGImageListEditor.btnRemoveClick(Sender: TObject);
begin
  TBGRASVGImageList(FComponent).Remove(ListBox1.ItemIndex);
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
      TBGRASVGImageList(FComponent).Replace(ListBox1.ItemIndex, s.Text);
    finally
      s.Free;
      UpdateListBox;
    end;
  end;
end;

procedure TfrmBGRASVGImageListEditor.btnUpClick(Sender: TObject);
begin
  TBGRASVGImageList(FComponent).Exchange(ListBox1.ItemIndex, ListBox1.ItemIndex - 1);
  UpdateListBox;
  ListBox1.ItemIndex := ListBox1.ItemIndex - 1;
end;

procedure TfrmBGRASVGImageListEditor.CheckBox_UseSVGAlignmentChange(
  Sender: TObject);
begin
  ImageList.UseSVGAlignment:= CheckBox_UseSVGAlignment.Checked;
  BCSVGViewerPreview.UseSVGAlignment:= ImageList.UseSVGAlignment;
  ListBox1.Invalidate;
  UpdateToolButtonsAlign;
end;

procedure TfrmBGRASVGImageListEditor.ListBox1DrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
begin
  ListBox1.Canvas.Brush.Color := clWhite;
  if (odSelected in State) then
    ListBox1.Canvas.Brush.Color := clHighlight;
  ListBox1.Canvas.FillRect(ARect);
  ListBox1.Canvas.TextOut(ARect.Height + ScaleX(4, 96),
                          ARect.Top, Index.ToString);
  if (Index <> -1) then
    TBGRASVGImageList(FComponent).Draw(Index, ListBox1, ListBox1.Canvas,
      ARect.Left, ARect.Top, ARect.Height, ARect.Height);
end;

procedure TfrmBGRASVGImageListEditor.ListBox1SelectionChange(Sender: TObject;
  User: boolean);
begin
  UpdateButtons;
  if ListBox1.ItemIndex <> -1 then
    BCSVGViewerPreview.SVGString :=
      TBGRASVGImageList(FComponent).SVGString[ListBox1.ItemIndex];
end;

procedure TfrmBGRASVGImageListEditor.ToolButton_AlignBottomClick(Sender: TObject
  );
begin
  ImageList.VerticalAlignment:= tlBottom;
  BCSVGViewerPreview.VertAlign:= ImageList.VerticalAlignment;
  UpdateToolButtonsAlign;
  ListBox1.Invalidate;
end;

procedure TfrmBGRASVGImageListEditor.ToolButton_AlignCenterClick(Sender: TObject
  );
begin
  ImageList.HorizontalAlignment:= taCenter;
  BCSVGViewerPreview.HorizAlign:= ImageList.HorizontalAlignment;
  UpdateToolButtonsAlign;
  ListBox1.Invalidate;
end;

procedure TfrmBGRASVGImageListEditor.ToolButton_AlignLeftClick(Sender: TObject);
begin
  ImageList.HorizontalAlignment:= taLeftJustify;
  BCSVGViewerPreview.HorizAlign:= ImageList.HorizontalAlignment;
  UpdateToolButtonsAlign;
  ListBox1.Invalidate;
end;

procedure TfrmBGRASVGImageListEditor.ToolButton_AlignRightClick(Sender: TObject
  );
begin
  ImageList.HorizontalAlignment:= taRightJustify;
  BCSVGViewerPreview.HorizAlign:= ImageList.HorizontalAlignment;
  UpdateToolButtonsAlign;
  ListBox1.Invalidate;
end;

procedure TfrmBGRASVGImageListEditor.ToolButton_AlignTopClick(Sender: TObject);
begin
  ImageList.VerticalAlignment:= tlTop;
  BCSVGViewerPreview.VertAlign:= ImageList.VerticalAlignment;
  UpdateToolButtonsAlign;
  ListBox1.Invalidate;
end;

procedure TfrmBGRASVGImageListEditor.ToolButton_AlignVCenterClick(
  Sender: TObject);
begin
  ImageList.VerticalAlignment:= tlCenter;
  BCSVGViewerPreview.VertAlign:= ImageList.VerticalAlignment;
  UpdateToolButtonsAlign;
  ListBox1.Invalidate;
end;

procedure TfrmBGRASVGImageListEditor.UpdateListBox;
var
  i: integer;
  index: integer;
begin
  index := ListBox1.ItemIndex;
  ListBox1.Clear;
  for i := 0 to TBGRASVGImageList(FComponent).Count - 1 do
    ListBox1.Items.Add('Image' + i.ToString);
  if ListBox1.Count > 0 then
    ListBox1.ItemIndex := index;
  UpdateButtons;
end;

function TfrmBGRASVGImageListEditor.GetImageList: TBGRASVGImageList;
begin
  result := TBGRASVGImageList(FComponent);
end;

procedure TfrmBGRASVGImageListEditor.UpdateButtons;
begin
  btnUp.Enabled := (ListBox1.Count > 1) and (ListBox1.ItemIndex > 0);
  btnDown.Enabled := (ListBox1.Count > 1) and (ListBox1.ItemIndex < ListBox1.Count - 1);
  btnRemove.Enabled := (ListBox1.Count > 0) and (ListBox1.ItemIndex <> -1);
  btnReplace.Enabled := (ListBox1.Count > 0) and (ListBox1.ItemIndex <> -1);
end;

procedure TfrmBGRASVGImageListEditor.UpdateToolButtonsAlign;
begin
  ToolButton_AlignLeft.Down := (ImageList.HorizontalAlignment = taLeftJustify);
  ToolButton_AlignCenter.Down := (ImageList.HorizontalAlignment = taCenter);
  ToolButton_AlignRight.Down := (ImageList.HorizontalAlignment = taRightJustify);
  ToolButton_AlignTop.Down := (ImageList.VerticalAlignment = tlTop);
  ToolButton_AlignVCenter.Down := (ImageList.VerticalAlignment = tlCenter);
  ToolButton_AlignBottom.Down := (ImageList.VerticalAlignment = tlBottom);
  ToolBar1.Enabled:= not ImageList.UseSVGAlignment;
end;

constructor TfrmBGRASVGImageListEditor.Create(AComponent: TComponent);
begin
  inherited Create(Application);

  FComponent := AComponent;
  ListBox1.ItemHeight := Max(ImageList.Height,
                             Max(16, ListBox1.Canvas.TextHeight('0')));
  UpdateListBox;
  CheckBox_UseSVGAlignment.Checked := ImageList.UseSVGAlignment;
  BCSVGViewerPreview.UseSVGAlignment:= ImageList.UseSVGAlignment;
  BCSVGViewerPreview.HorizAlign:= ImageList.HorizontalAlignment;
  BCSVGViewerPreview.VertAlign:= ImageList.VerticalAlignment;
  UpdateToolButtonsAlign;
end;

initialization
  RegisterComponentEditor(TBGRASVGImageList, TBGRASVGImageListEditor);

end.
