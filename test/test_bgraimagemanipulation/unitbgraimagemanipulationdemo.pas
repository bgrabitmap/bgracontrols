unit UnitBGRAImageManipulationDemo;

{ ============================================================================
  BGRAImageManipulation Demon Unit

  originally written in 2011 by - Emerson Cavalcanti

  This program is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  ============================================================================
  Description:

  TFormBGRAImageManipulationDemo is a sample form to use the component
  TBGRAImageManipulation.

  ============================================================================
  History:

  2011-05-06 - Emerson Cavalcanti
             - Initial version

  2011-06-01 - Emerson Cavalcanti
             - Relayout of form.
             - Add control to toggle the option 'Keep Aspect Ratio' in
               component

  2011-06-18 - Emerson Cavalcanti
             - Relayout of form for expand component on resize.
             - Add control to rotate image

  2013-10-13 - Massimo Magnano
             - Add multi crop demo

  ============================================================================
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtDlgs, ComCtrls, ExtCtrls, Menus, BGRAImageManipulation,
  BGRABitmap, BGRABitmapTypes, BCPanel, BCButton, BGRASpeedButton, BCLabel,
  BCTrackbarUpdown{, BGRATrackBar};

type

  { TFormBGRAImageManipulationDemo }

  TFormBGRAImageManipulationDemo = class(TForm)
    Background:        TBCPanel;
    BCLabel1: TBCLabel;
    BCLabel2: TBCLabel;
    BCLabel3: TBCLabel;
    BCLabel4: TBCLabel;
    BCLabel5: TBCLabel;
    BCLabel6: TBCLabel;
    BCPanelCropAreaLoad: TBCPanel;
    BCPanelCropArea: TBCPanel;
    BCPanelCropAreas: TBCPanel;
    btApplyAspectRatio: TSpeedButton;
    btBox_Add: TBGRASpeedButton;
    btBox_Del: TBGRASpeedButton;
    btnLoadCropList: TBCButton;
    btnSaveCropList: TBCButton;
    btnSavePictureAll: TBCButton;
    btnShape:          TBCButton;
    btnOpenPicture:    TBCButton;
    BGRAImageManipulation: TBGRAImageManipulation;
    btnGetAspectRatioFromImage: TBCButton;
    btnSavePicture:    TBCButton;
    btnSetAspectRatio: TBCButton;
    btnRotateLeft:     TBCButton;
    btnRotateRight:    TBCButton;
    cbBoxList: TComboBox;
    chkFullSize: TCheckBox;
    edAspectPersonal: TEdit;
    edAspectRatio:     TEdit;
    edHeight: TBCTrackbarUpdown;
    edLeft: TBCTrackbarUpdown;
    edName: TEdit;
    edTop: TBCTrackbarUpdown;
    edUnit_Type: TComboBox;
    edWidth: TBCTrackbarUpdown;
    KeepAspectRatio:   TCheckBox;
    lbAspectRatio:     TLabel;
    lbOptions:         TLabel;
    lbCompression:     TLabel;
    lbOptions1: TLabel;
    OpenCropList: TOpenDialog;
    OpenPictureDialog: TOpenPictureDialog;
    rgAspect: TRadioGroup;
    SaveCropList: TSaveDialog;
    SavePictureDialog: TSavePictureDialog;
    RateCompression:   TTrackBar;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SpeedButton1: TSpeedButton;
    procedure btnGetAspectRatioFromImageClick(Sender: TObject);
    procedure btnLoadCropListClick(Sender: TObject);
    procedure btnOpenPictureClick(Sender: TObject);
    procedure btnRotateLeftClick(Sender: TObject);
    procedure btnRotateRightClick(Sender: TObject);
    procedure btnSaveCropListClick(Sender: TObject);
    procedure btnSavePictureAllClick(Sender: TObject);
    procedure btnSavePictureClick(Sender: TObject);
    procedure btnSetAspectRatioClick(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure KeepAspectRatioClick(Sender: TObject);

    procedure btBox_AddClick(Sender: TObject);
    procedure btBox_DelClick(Sender: TObject);
    procedure cbBoxListChange(Sender: TObject);
    procedure edHeightChange(Sender: TObject; AByUser: boolean);
    procedure edLeftChange(Sender: TObject; AByUser: boolean);
    procedure edTopChange(Sender: TObject; AByUser: boolean);
    procedure edWidthChange(Sender: TObject; AByUser: boolean);
    procedure FormCreate(Sender: TObject);
    procedure rgAspectSelectionChanged(Sender: TObject);
    procedure btApplyAspectRatioClick(Sender: TObject);

    procedure AddedCrop(AOwner: TBGRAImageManipulation; CropArea: TCropArea);
    procedure DeletedCrop(AOwner: TBGRAImageManipulation; CropArea: TCropArea);
    procedure ChangedCrop(AOwner: TBGRAImageManipulation; CropArea: TCropArea);
    procedure SelectedChangedCrop(AOwner: TBGRAImageManipulation; CropArea: TCropArea);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    lastNewBoxNum :Word;
    changingAspect, closing:Boolean;

    procedure FillBoxUI(ABox :TCropArea);
    procedure SaveCallBack(Bitmap :TBGRABitmap; CropArea: TCropArea);
  public
    { public declarations }
  end;

var
  FormBGRAImageManipulationDemo: TFormBGRAImageManipulationDemo;

implementation

{$R *.lfm}

{ TFormBGRAImageManipulationDemo }

procedure TFormBGRAImageManipulationDemo.btnOpenPictureClick(Sender: TObject);
var
  Bitmap: TBGRABitmap;
begin
  // To put a new image in the component, you will simply need execute open
  // picture dialog to locate an image...
  if OpenPictureDialog.Execute then
  begin
    // ...and create a new TBGRABitmap and load to it
    Bitmap := TBGRABitmap.Create;
    Bitmap.LoadFromFile(OpenPictureDialog.FileName);
    // Finally, associate the image into component
    BGRAImageManipulation.Bitmap := Bitmap;
    Bitmap.Free;
    edLeft.MaxValue:=BGRAImageManipulation.Bitmap.Width;
    edTop.MaxValue:=BGRAImageManipulation.Bitmap.Height;
    edWidth.MaxValue:=BGRAImageManipulation.Bitmap.Width;
    edHeight.MaxValue:=BGRAImageManipulation.Bitmap.Height;
    BGRAImageManipulation.addCropArea(Rect(100,100,220,260));
  end;
end;

procedure TFormBGRAImageManipulationDemo.btnRotateLeftClick(Sender: TObject);
begin
  BGRAImageManipulation.rotateLeft;
end;

procedure TFormBGRAImageManipulationDemo.btnRotateRightClick(Sender: TObject);
begin
  BGRAImageManipulation.rotateRight;
end;

procedure TFormBGRAImageManipulationDemo.btnGetAspectRatioFromImageClick(
  Sender: TObject);
begin
  if not (BGRAImageManipulation.Empty) then
  begin
    edAspectRatio.Text := BGRAImageManipulation.getAspectRatioFromImage(
      BGRAImageManipulation.Bitmap);
  end;
end;

procedure TFormBGRAImageManipulationDemo.btnLoadCropListClick(Sender: TObject);
begin
  try
     if OpenCropList.Execute
     then BGRAImageManipulation.CropAreas.LoadFromFile(OpenCropList.FileName);
  except
  end;
end;

procedure TFormBGRAImageManipulationDemo.btnSaveCropListClick(Sender: TObject);
begin
  try
     if SaveCropList.Execute
     then BGRAImageManipulation.CropAreas.SaveToFile(SaveCropList.FileName);
  except
  end;
end;

procedure TFormBGRAImageManipulationDemo.btnSavePictureClick(Sender: TObject);
var
  JpegImage: TJpegImage;
begin
  // This example save image compress in JPEG format

  // Execute our Save Picture Dialog
  SavePictureDialog.Filter := 'JPEG Image File (*.jpg, *.jpeg)';
  if SavePictureDialog.Execute then
  begin
    try
      // Compress
      JpegImage := TJpegImage.Create;
      if (chkFullSize.Checked)
      then JpegImage.Assign(BGRAImageManipulation.getBitmap)
      else JpegImage.Assign(BGRAImageManipulation.getResampledBitmap);
      JpegImage.CompressionQuality := RateCompression.Position;

      // And save to file
      JpegImage.SaveToFile(SavePictureDialog.FileName);
    finally
      JpegImage.Free;
    end;
  end;
end;

procedure TFormBGRAImageManipulationDemo.SaveCallBack(Bitmap :TBGRABitmap; CropArea: TCropArea);
var
  JpegImage: TJpegImage;
begin
    try
      // Compress
      JpegImage := TJpegImage.Create;
      JpegImage.Assign(Bitmap);
      JpegImage.CompressionQuality := RateCompression.Position;

      // And save to file
      JpegImage.SaveToFile(SelectDirectoryDialog1.FileName+DirectorySeparator+CropArea.Name+'.jpg');
    finally
      JpegImage.Free;
    end;
end;

procedure TFormBGRAImageManipulationDemo.btnSavePictureAllClick(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
  begin
    if (chkFullSize.Checked)
    then Self.BGRAImageManipulation.getAllBitmaps(@SaveCallBack)
    else Self.BGRAImageManipulation.getAllResampledBitmaps(@SaveCallBack);
  end;
end;


procedure TFormBGRAImageManipulationDemo.btnSetAspectRatioClick(Sender: TObject);
begin
  try
    BGRAImageManipulation.AspectRatio := edAspectRatio.Text;
  except
    on E: Exception do
    begin
      ShowMessage('This aspect ratio is invalid');
    end;
  end;
end;

procedure TFormBGRAImageManipulationDemo.edNameChange(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=TCropArea(cbBoxList.Items.Objects[cbBoxList.ItemIndex]);
  CropArea.Name :=edName.Text;
end;

procedure TFormBGRAImageManipulationDemo.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
     closing :=True;
end;

procedure TFormBGRAImageManipulationDemo.KeepAspectRatioClick(Sender: TObject);
begin
  BGRAImageManipulation.KeepAspectRatio := KeepAspectRatio.Checked;
  edAspectRatio.Enabled := KeepAspectRatio.Checked;
  btnSetAspectRatio.Enabled := KeepAspectRatio.Checked;
end;

procedure TFormBGRAImageManipulationDemo.btBox_AddClick(Sender: TObject);
var
   newCropArea :TCropArea;

begin
  newCropArea :=BGRAImageManipulation.addCropArea(Rect(50, 50, 100, 100), 0 (*, Integer(newBox)*));
  newCropArea.BorderColor :=VGALime;
end;

procedure TFormBGRAImageManipulationDemo.btBox_DelClick(Sender: TObject);
var
   CropArea :TCropArea;
   curIndex :Integer;

begin
  curIndex :=cbBoxList.ItemIndex;
  CropArea :=TCropArea(cbBoxList.Items.Objects[curIndex]);
  BGRAImageManipulation.delCropArea(CropArea);
  cbBoxList.Items.Delete(curIndex);
(*  if (BGRAImageManipulation.SelectedCropArea <> nil)
  then cbBoxList.ItemIndex:=BGRAImageManipulation.SelectedCropArea.Index
  else cbBoxList.ItemIndex:=-1;
  FillBoxUI(BGRAImageManipulation.SelectedCropArea);*)
end;

procedure TFormBGRAImageManipulationDemo.cbBoxListChange(Sender: TObject);
begin
   BGRAImageManipulation.SelectedCropArea :=TCropArea(cbBoxList.Items.Objects[cbBoxList.ItemIndex]);
end;

procedure TFormBGRAImageManipulationDemo.edHeightChange(Sender: TObject; AByUser: boolean);
var
   CropArea :TCropArea;

begin
  if AByUser then
  begin
    CropArea :=TCropArea(cbBoxList.Items.Objects[cbBoxList.ItemIndex]);
    CropArea.Height:=edHeight.Value;
  end;
end;

procedure TFormBGRAImageManipulationDemo.edLeftChange(Sender: TObject; AByUser: boolean);
var
   CropArea :TCropArea;

begin
  if AByUser then
  begin
    CropArea :=TCropArea(cbBoxList.Items.Objects[cbBoxList.ItemIndex]);
    CropArea.Left :=edLeft.Value;
  end;
end;

procedure TFormBGRAImageManipulationDemo.edTopChange(Sender: TObject; AByUser: boolean);
var
   CropArea :TCropArea;

begin
  if AByUser then
  begin
    CropArea :=TCropArea(cbBoxList.Items.Objects[cbBoxList.ItemIndex]);
    CropArea.Top :=edTop.Value;
  end;
end;

procedure TFormBGRAImageManipulationDemo.edWidthChange(Sender: TObject; AByUser: boolean);
var
   CropArea :TCropArea;

begin
  if AByUser then
  begin
    CropArea :=TCropArea(cbBoxList.Items.Objects[cbBoxList.ItemIndex]);
    CropArea.Width:=edWidth.Value;
  end;
end;

procedure TFormBGRAImageManipulationDemo.FormCreate(Sender: TObject);
var
   i :Integer;

begin
   closing :=False;
   changingAspect :=False;
   lastNewBoxNum :=0;
   TStringList(cbBoxList.Items).OwnsObjects:=False;
end;

procedure TFormBGRAImageManipulationDemo.rgAspectSelectionChanged(Sender: TObject);
begin
  if changingAspect then Exit;

  Case rgAspect.ItemIndex of
  0 : BGRAImageManipulation.SelectedCropArea.KeepAspectRatio:=bParent;
  1 : BGRAImageManipulation.SelectedCropArea.KeepAspectRatio:=bFalse;
  2 : begin
           BGRAImageManipulation.SelectedCropArea.KeepAspectRatio:=bTrue;
           BGRAImageManipulation.SelectedCropArea.AspectRatio:=edAspectPersonal.Text;
       end;
  end;
end;

procedure TFormBGRAImageManipulationDemo.btApplyAspectRatioClick(Sender: TObject);
begin
   if BGRAImageManipulation.SelectedCropArea.KeepAspectRatio=bTrue
   then BGRAImageManipulation.SelectedCropArea.AspectRatio:=edAspectPersonal.Text
   else begin
             BGRAImageManipulation.SelectedCropArea.KeepAspectRatio:=bTrue;
             BGRAImageManipulation.SelectedCropArea.AspectRatio:=edAspectPersonal.Text;
             changingAspect :=True;
             rgAspect.ItemIndex :=2;
             changingAspect :=False;
        end;
end;

procedure TFormBGRAImageManipulationDemo.AddedCrop(AOwner: TBGRAImageManipulation; CropArea: TCropArea);
var
  curIndex :Integer;

begin
   curIndex :=BGRAImageManipulation.CropAreas.IndexOf(CropArea);

   if (CropArea.Name='')
   then CropArea.Name:='Name '+IntToStr(curIndex);

   cbBoxList.AddItem(CropArea.Name, CropArea);
   cbBoxList.ItemIndex:=cbBoxList.Items.IndexOfObject(CropArea);
   FillBoxUI(CropArea);
end;

procedure TFormBGRAImageManipulationDemo.DeletedCrop(AOwner: TBGRAImageManipulation; CropArea: TCropArea);
var
   delIndex :Integer;
begin
  try
    if not(closing) then
    begin
         delIndex :=cbBoxList.Items.IndexOfObject(CropArea);
         if (delIndex<>-1)
         then cbBoxList.Items.Delete(delIndex);
    end;
  except
  end;
  //MessageDlg('Deleting Crop Area', 'Deleting '+CropArea.Name, mtInformation, [mbOk], 0);
end;

procedure TFormBGRAImageManipulationDemo.ChangedCrop(AOwner: TBGRAImageManipulation; CropArea: TCropArea);
begin
  if (cbBoxList.Items.Objects[cbBoxList.ItemIndex] = CropArea) then
  begin
    FillBoxUI(CropArea);

    //if the Name is Changed change in the comboListbox
    if (CropArea.Name<>cbBoxList.Items.Strings[cbBoxList.ItemIndex])
    then cbBoxList.Items.Strings[cbBoxList.ItemIndex] :=CropArea.Name;
  end;
end;

procedure TFormBGRAImageManipulationDemo.SelectedChangedCrop(AOwner: TBGRAImageManipulation; CropArea: TCropArea);
var
   newIndex :Integer;
begin
   if (BGRAImageManipulation.SelectedCropArea <> nil)
   then newIndex :=cbBoxList.Items.IndexOfObject(BGRAImageManipulation.SelectedCropArea) //BGRAImageManipulation.SelectedCropArea.Index;
   else newIndex :=-1;

   cbBoxList.ItemIndex:=newIndex;
   FillBoxUI(BGRAImageManipulation.SelectedCropArea);
end;

procedure TFormBGRAImageManipulationDemo.SpeedButton1Click(Sender: TObject);
begin
  BGRAImageManipulation.tests;
end;

procedure TFormBGRAImageManipulationDemo.FillBoxUI(ABox: TCropArea);
begin
   if (ABox<>nil)
   then begin
           BCPanelCropArea.Enabled :=True;
           edName.Text :=ABox.Name;
           edLeft.Value :=ABox.Left;
           edTop.Value :=ABox.Top;
           edWidth.Value :=ABox.Width;
           edHeight.Value :=ABox.Height;

           //Aspect Ratio
           changingAspect :=True;
           Case ABox.KeepAspectRatio of
           bParent :rgAspect.ItemIndex:=0;
           bFalse  :rgAspect.ItemIndex:=1;
           bTrue   :rgAspect.ItemIndex:=2;
           end;
           edAspectPersonal.Text:=ABox.AspectRatio;
           changingAspect:=False;

           //edUnit_Type.ItemIndex :=Integer(ABox^.UnitType);
        end
   else BCPanelCropArea.Enabled :=False;
end;


end.
