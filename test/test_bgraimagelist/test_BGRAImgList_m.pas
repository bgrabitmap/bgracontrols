unit test_BGRAImgList_m;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls, StdCtrls, Grids, ColorBox, CheckLst,
  ExtCtrls, GraphType, ImgList, ExtDlgs, BGRAImageList;

type

  { TForm1 }

  TForm1 = class(TForm)
    btAddThumb: TButton;
    btAddThumbCol: TButton;
    ColorBox1: TColorBox;
    imgListThumbs: TBGRAImageList;
    btStretchDraw: TButton;
    cbIndexDraw: TCheckBox;
    cbOverlay: TCheckBox;
    cbBGRADraw: TCheckBox;
    DrawGrid1: TDrawGrid;
    ImageList1: TBGRAImageList;
    lvCaptured: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    OpenPictDialog: TOpenPictureDialog;
    Panel1: TPanel;
    rgHorizontal: TRadioGroup;
    rgVertical: TRadioGroup;
    procedure btAddThumbClick(Sender: TObject);
    procedure btStretchDrawClick(Sender: TObject);
    procedure cbBGRADrawChange(Sender: TObject);
    procedure cbIndexDrawChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImageList1AfterDraw(Sender: TBGRAImageList; ACanvas: TCanvas; ARect: TRect; AIndex: Integer;
      ADrawingStyle: TDrawingStyle; AImageType: TImageType; ADrawOverlay: Boolean; AOverlay: TOverlay;
      ADrawEffect: TGraphicsDrawEffect);
    function ImageList1BeforeDraw(Sender: TBGRAImageList; ACanvas: TCanvas; var ARect: TRect; var AIndex: Integer;
      var ADrawingStyle: TDrawingStyle; var AImageType: TImageType; var ADrawOverlay: Boolean; var AOverlay: TOverlay;
      var ADrawEffect: TGraphicsDrawEffect): Boolean;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btStretchDrawClick(Sender: TObject);
begin
  ImageList1.StretchDrawOverlay(Panel1.Canvas, 0, Rect(16, 16, 128, 128), 0, True) //AOverlay is changed in Event
end;

procedure TForm1.btAddThumbClick(Sender: TObject);
var
   newItem: TListItem;
   newImgI: Integer;

begin
  if OpenPictDialog.Execute then
  try
    if (Sender=btAddThumbCol)
    then newImgI:= imgListThumbs.AddMaskedProportionally(OpenPictDialog.FileName, ColorBox1.Selected,
                                              TAlignment(rgHorizontal.ItemIndex),
                                              TTextLayout(rgVertical.ItemIndex))
    else newImgI:= imgListThumbs.AddProportionally(OpenPictDialog.FileName, '',
                                              TAlignment(rgHorizontal.ItemIndex),
                                              TTextLayout(rgVertical.ItemIndex));
    newItem:= lvCaptured.Items.Add;
    newItem.Caption:= ExtractFileName(OpenPictDialog.FileName);
    newItem.ImageIndex:= newImgI;

  finally
  end;
end;

procedure TForm1.cbBGRADrawChange(Sender: TObject);
begin
  ImageList1.UseBGRADraw:= cbBGRADraw.Checked;
  DrawGrid1.Invalidate;
end;

procedure TForm1.cbIndexDrawChange(Sender: TObject);
begin
  DrawGrid1.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ImageList1.Overlay(3, 1);
  ImageList1.Overlay(4, 2);
  ImageList1.Overlay(5, 3);
  ImageList1.Overlay(6, 4);
end;

procedure TForm1.ImageList1AfterDraw(Sender: TBGRAImageList; ACanvas: TCanvas; ARect: TRect; AIndex: Integer;
  ADrawingStyle: TDrawingStyle; AImageType: TImageType; ADrawOverlay: Boolean; AOverlay: TOverlay;
  ADrawEffect: TGraphicsDrawEffect);
begin
  if cbIndexDraw.Checked then
  begin
    ACanvas.SaveHandleState;
    ACanvas.Brush.Color:= clSkyBlue;
    ACanvas.Brush.Style:= bsSolid;
    ACanvas.Font.Size:=8;
    ACanvas.TextOut(ARect.Left+ARect.Right-8, ARect.Top+ARect.Bottom-10, IntToStr(AIndex));
    ACanvas.RestoreHandleState;
  end;
end;

function TForm1.ImageList1BeforeDraw(Sender: TBGRAImageList; ACanvas: TCanvas; var ARect: TRect; var AIndex: Integer;
  var ADrawingStyle: TDrawingStyle; var AImageType: TImageType; var ADrawOverlay: Boolean; var AOverlay: TOverlay;
  var ADrawEffect: TGraphicsDrawEffect): Boolean;
begin
  ARect.Left:=ARect.Left-8;
  ADrawEffect:=gdeHighlighted;

  if cbOverlay.Checked then
  begin
    ADrawOverlay:= True;
    AOverlay:=AIndex+1;
  end;
  Result:= True;
end;

end.

