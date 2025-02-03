unit test_BGRAImgList_m;

{$ifdef FPC}
  {$mode objfpc}
{$endif}

{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls, StdCtrls, Grids, ColorBox,
  ExtCtrls, GraphType, ImgList, ExtDlgs, BGRAImageList
  {$ifdef FPC}, LCLVersion{$endif};

type

  { TForm1 }

  TForm1 = class(TForm)
    btAddThumb: TButton;
    btAddThumbCol: TButton;
    btChangeThumb: TButton;
    btChangeThumbCol: TButton;
    btReadData: TButton;
    btReadSel: TButton;
    btStretchDraw: TButton;
    btWriteData: TButton;
    btWriteSel: TButton;
    Button1: TButton;
    cbBGRADraw: TCheckBox;
    cbIndexDraw: TCheckBox;
    cbOverlay: TCheckBox;
    ColorBox1: TColorBox;
    DrawGrid1: TDrawGrid;
    Image1: TImage;
    ImageList1: TBGRAImageList;
    ImageList2: TImageList;
    imgListThumbs: TBGRAImageList;
    lvCaptured: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenPictDialog: TOpenPictureDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    rgHorizontal: TRadioGroup;
    rgVertical: TRadioGroup;
    SaveDialog1: TSaveDialog;
    tabDraw: TTabSheet;
    tabReadWrite: TTabSheet;
    procedure btAddThumbClick(Sender: TObject);
    procedure btChangeThumbClick(Sender: TObject);
    procedure btReadDataClick(Sender: TObject);
    procedure btReadSelClick(Sender: TObject);
    procedure btStretchDrawClick(Sender: TObject);
    procedure btWriteDataClick(Sender: TObject);
    procedure btWriteSelClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
     procedure LoadImgList(AFileName: String);

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

procedure TForm1.Button1Click(Sender: TObject);
var
   newItem: TListItem;
   newImgI: Integer;
   pict: TPicture;
   AResult,
   AResult2: TBitmap;
   c:Boolean;

begin
  if OpenPictDialog.Execute then
  try
    pict:= TPicture.Create;
    pict.LoadFromFile(OpenPictDialog.FileName);

    AResult:=TBitmap.Create;
    AResult.Assign(pict.Bitmap);
    AResult.TransparentColor := ColorBox1.Selected;
    AResult.TransparentMode := tmFixed;
    AResult.Transparent := True;
    AResult.Masked:= True;
    //AResult.Mask(ColorBox1.Selected);

    AResult2:= imgListThumbs.CreateProportionalImage(AResult, taCenter, tlCenter);
    c:= AResult2.Masked;
    c:= AResult2.Transparent;
    AResult2.TransparentColor:=AResult.TransparentColor;

    Image1.Picture.Assign(AResult2);

    newImgI:= imgListThumbs.AddMasked(AResult2, ColorBox1.Selected);
    newItem:= lvCaptured.Items.Add;
    newItem.Caption:= ExtractFileName(OpenPictDialog.FileName);
    newItem.ImageIndex:= newImgI;

  finally
    AResult2.Free;
    AResult.Free;
    pict.Free;
  end;
end;

procedure TForm1.btAddThumbClick(Sender: TObject);
var
   newItem: TListItem;
   newImgI: Integer;
   newBmp: TBitmap;

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

    newBmp:= TBitmap.Create;
    imgListThumbs.GetBitmap(newImgI, newBmp);
    Image1.Picture.Assign(newBmp);

  finally
    newBmp.Free;
  end;
end;

procedure TForm1.btChangeThumbClick(Sender: TObject);
var
   newItem: TListItem;
   oID: Integer;
   newBmp: TBitmap;

begin
  if (lvCaptured.Selected<>nil) and OpenPictDialog.Execute then
  try
    oID:= lvCaptured.Selected.ImageIndex;

    if (Sender=btChangeThumbCol)
    then imgListThumbs.ReplaceMaskedProportionally(oID, OpenPictDialog.FileName, ColorBox1.Selected, True,
                                              TAlignment(rgHorizontal.ItemIndex),
                                              TTextLayout(rgVertical.ItemIndex))
    else imgListThumbs.ReplaceProportionally(oID, OpenPictDialog.FileName, '', True,
                                              TAlignment(rgHorizontal.ItemIndex),
                                              TTextLayout(rgVertical.ItemIndex));

    //lvCaptured.Selected.Caption:= ExtractFileName(OpenPictDialog.FileName);
    lvCaptured.Selected.ImageIndex:=-1;
    lvCaptured.Selected.ImageIndex:=oID;

    newBmp:= TBitmap.Create;
    imgListThumbs.GetBitmap(oID, newBmp);
    Image1.Picture.Assign(newBmp);

  finally
    newBmp.Free;
  end;
end;

procedure TForm1.btReadDataClick(Sender: TObject);
var
   i:Integer;
   oCap: String;
   oId: Integer;

begin
  if OpenDialog1.Execute then
  begin
    lvCaptured.Clear;
    LoadImgList(OpenDialog1.FileName);
(*    for i:=0 to lvCaptured.Items.Count-1 do
    begin
      oID:= lvCaptured.Items[i].ImageIndex;
      lvCaptured.Items[i].ImageIndex:= -1;
      lvCaptured.Items[i].ImageIndex:= oID;
    end; *)
  end;
end;

procedure TForm1.btReadSelClick(Sender: TObject);
var
   oCap: String;
   oID: Integer;

begin
  {$if lcl_fullversion>=4990000}
  if (lvCaptured.Selected<>nil) and OpenDialog1.Execute then
  begin
    oID:=lvCaptured.Selected.ImageIndex;
    imgListThumbs.LoadFromFile(OpenDialog1.FileName, oID);

    //Is the only way to Update the Image?
    //oCap:=lvCaptured.Selected.Caption;
    //lvCaptured.Selected.Caption:='';
    //lvCaptured.Selected.Caption:=oCap;
    lvCaptured.Selected.ImageIndex:=-1;
    lvCaptured.Selected.ImageIndex:=oID;
  end;
  {$endif}
end;

procedure TForm1.btWriteDataClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    imgListThumbs.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TForm1.btWriteSelClick(Sender: TObject);
begin
  {$if lcl_fullversion>=4990000}
  if (lvCaptured.Selected<>nil) and SaveDialog1.Execute then
  begin
    imgListThumbs.SaveToFile(SaveDialog1.FileName, lvCaptured.Selected.ImageIndex);
  end;
  {$endif}
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
  //LoadImgList('c:\tmp\imgList_o.img');
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

procedure TForm1.LoadImgList(AFileName: String);
var
   newItem: TListItem;
   i, k, t: Integer;

begin
//    lvCaptured.BeginUpdate;

    imgListThumbs.LoadFromFile(AFileName);

    if (lvCaptured.Items.Count<imgListThumbs.Count) then
    begin
      k:= lvCaptured.Items.Count;
      t:= imgListThumbs.Count;

      for i:=k to t-1 do
      begin
        newItem:= lvCaptured.Items.Add;
        newItem.Caption:= ExtractFileName(OpenDialog1.FileName+' ('+IntToStr(i)+')');
        newItem.ImageIndex:= i;
      end;
    end;

//    lvCaptured.EndUpdate;
//    lvCaptured.Invalidate;
end;


end.

