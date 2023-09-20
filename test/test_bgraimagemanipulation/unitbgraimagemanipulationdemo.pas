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
  2023-08    - Resolution, Save in various formats, Z Order

  ============================================================================
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtDlgs, ComCtrls, ExtCtrls, Menus, Spin,
  {$IFDEF FPC} FPImage,{$ENDIF} BGRAImageManipulation,
  BGRABitmap, BGRABitmapTypes, BCPanel, BCButton, BGRASpeedButton, BCLabel, Laz2_XMLCfg;

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
    BCLabel7: TBCLabel;
    BCPanelCropAreaLoad: TBCPanel;
    BCPanelCropArea: TBCPanel;
    BCPanelCropAreas: TBCPanel;
    btApplyAspectRatio: TSpeedButton;
    btBox_Add: TBGRASpeedButton;
    btBox_Del: TBGRASpeedButton;
    btCFlipHLeft: TSpeedButton;
    btCFlipHRight: TSpeedButton;
    btCFlipVUp: TSpeedButton;
    btCFlipVDown: TSpeedButton;
    btnEmptyImage: TBCButton;
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
    btCRotateRight: TSpeedButton;
    btCRotateLeft: TSpeedButton;
    cbBoxList: TComboBox;
    chkFullSize: TCheckBox;
    cbSaveFormat: TComboBox;
    edAspectPersonal: TEdit;
    edAspectRatio:     TEdit;
    edHeight: TFloatSpinEdit;
    edLeft: TFloatSpinEdit;
    edName: TEdit;
    edTop: TFloatSpinEdit;
    edUnit_Type: TComboBox;
    edWidth: TFloatSpinEdit;
    KeepAspectRatio:   TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbResolution: TLabel;
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
    btZFront: TSpeedButton;
    btZBack: TSpeedButton;
    btZDown: TSpeedButton;
    btZUp: TSpeedButton;
    procedure btCFlipHLeftClick(Sender: TObject);
    procedure btCFlipHRightClick(Sender: TObject);
    procedure btCFlipVDownClick(Sender: TObject);
    procedure btCFlipVUpClick(Sender: TObject);
    procedure btCRotateLeftClick(Sender: TObject);
    procedure btCRotateRightClick(Sender: TObject);
    procedure btnEmptyImageClick(Sender: TObject);
    procedure btnGetAspectRatioFromImageClick(Sender: TObject);
    procedure btnLoadCropListClick(Sender: TObject);
    procedure btnOpenPictureClick(Sender: TObject);
    procedure btnRotateLeftClick(Sender: TObject);
    procedure btnRotateRightClick(Sender: TObject);
    procedure btnSaveCropListClick(Sender: TObject);
    procedure btnSavePictureAllClick(Sender: TObject);
    procedure btnSavePictureClick(Sender: TObject);
    procedure btnSetAspectRatioClick(Sender: TObject);
    procedure btZBackClick(Sender: TObject);
    procedure btZDownClick(Sender: TObject);
    procedure btZFrontClick(Sender: TObject);
    procedure btZUpClick(Sender: TObject);
    function CropAreaLoad(AOwner: TBGRAImageManipulation; CropArea: TCropArea; const XMLConf: TXMLConfig;
      const Path: String): Integer;
    procedure CropAreaSave(AOwner: TBGRAImageManipulation; CropArea: TCropArea; const XMLConf: TXMLConfig;
      const Path: String);
    procedure edNameChange(Sender: TObject);
    procedure edUnit_TypeChange(Sender: TObject);
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
    changingAspect, closing,
    inFillBoxUI :Boolean;

    function GetCurrentCropArea: TCropArea;
    procedure FillBoxUI(ABox :TCropArea);
    procedure SaveCallBack(Bitmap :TBGRABitmap; CropArea: TCropArea);
    procedure UpdateBoxList;
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
  test:Integer;

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

    lbResolution.Caption:='Resolution : '+#13#10+'  '+
          FloatToStr(BGRAImageManipulation.Bitmap.ResolutionX)+' x '+
          FloatToStr(BGRAImageManipulation.Bitmap.ResolutionY)+' '+edUnit_Type.Items[Integer(BGRAImageManipulation.Bitmap.ResolutionUnit)]+#13#10+
          '  '+FloatToStr(BGRAImageManipulation.Bitmap.ResolutionWidth)+' x '+FloatToStr(BGRAImageManipulation.Bitmap.ResolutionHeight)+#13#10+
          '  pixels '+IntToStr(BGRAImageManipulation.Bitmap.Width)+' x '+IntToStr(BGRAImageManipulation.Bitmap.Height);

    if (BGRAImageManipulation.SelectedCropArea=nil)
    then begin
           edUnit_Type.ItemIndex:=Integer(BGRAImageManipulation.Bitmap.ResolutionUnit);
           edLeft.MaxValue:=BGRAImageManipulation.Bitmap.ResolutionWidth;
           edTop.MaxValue:=BGRAImageManipulation.Bitmap.ResolutionHeight;
           edWidth.MaxValue:=BGRAImageManipulation.Bitmap.ResolutionWidth;
           edHeight.MaxValue:=BGRAImageManipulation.Bitmap.ResolutionHeight;
         end
    else FillBoxUI(BGRAImageManipulation.SelectedCropArea);
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

procedure TFormBGRAImageManipulationDemo.btnEmptyImageClick(Sender: TObject);
var
   emptyImg :TBGRABitmap;

begin
  try
     emptyImg :=TBGRABitmap.Create(0, 0);
     BGRAImageManipulation.Bitmap :=emptyImg;
  finally
     emptyImg.Free;
  end;
end;

procedure TFormBGRAImageManipulationDemo.btCRotateLeftClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil
  then CropArea.RotateLeft;
end;

procedure TFormBGRAImageManipulationDemo.btCFlipVDownClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil
  then CropArea.FlipVDown;
end;

procedure TFormBGRAImageManipulationDemo.btCFlipHLeftClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil
  then CropArea.FlipHLeft;
end;

procedure TFormBGRAImageManipulationDemo.btCFlipHRightClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil
  then CropArea.FlipHRight;
end;

procedure TFormBGRAImageManipulationDemo.btCFlipVUpClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil
  then CropArea.FlipVUp;
end;

procedure TFormBGRAImageManipulationDemo.btCRotateRightClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil
  then CropArea.RotateRight;
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
  curBitmap :TBGRABitmap;

begin
  if SavePictureDialog.Execute then
  begin
    try
      if (chkFullSize.Checked)
      then curBitmap :=BGRAImageManipulation.getBitmap
      else curBitmap :=BGRAImageManipulation.getResampledBitmap;

      curBitmap.SaveToFile(SavePictureDialog.FileName);
    finally
      curBitmap.Free;
    end;
  end;
end;

procedure TFormBGRAImageManipulationDemo.SaveCallBack(Bitmap :TBGRABitmap; CropArea: TCropArea);
var
  ext:String;
  i:Integer;

begin
   ext:=ImageHandlers.Extensions[cbSaveFormat.Items[cbSaveFormat.ItemIndex]];
   i :=Pos(';', ext);
   if (i>0) then ext :=Copy(ext, 1, i-1);
   Bitmap.SaveToFile(SelectDirectoryDialog1.FileName+DirectorySeparator+CropArea.Name+'.'+ext);
end;

procedure TFormBGRAImageManipulationDemo.UpdateBoxList;
var
   i :Integer;
   CropArea,
   SelArea:TCropArea;

begin
  cbBoxList.OnChange:=nil;

  //SelArea :=BGRAImageManipulation.SelectedCropArea;
  cbBoxList.Clear;
  for i:=0 to BGRAImageManipulation.CropAreas.Count-1 do
  begin
    CropArea :=BGRAImageManipulation.CropAreas.items[i];
    cbBoxList.AddItem(CropArea.Name, CropArea);
  end;
  //BGRAImageManipulation.SelectedCropArea :=SelArea;
  cbBoxList.ItemIndex:=cbBoxList.Items.IndexOfObject(BGRAImageManipulation.SelectedCropArea);

  cbBoxList.OnChange:=@cbBoxListChange;
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

procedure TFormBGRAImageManipulationDemo.btZBackClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil
  then begin
         CropArea.BringToBack;
         UpdateBoxList;
       end;
end;

procedure TFormBGRAImageManipulationDemo.btZDownClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil
  then begin
         CropArea.BringBackward;
         UpdateBoxList;
       end;
end;

procedure TFormBGRAImageManipulationDemo.btZFrontClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil
  then begin
         CropArea.BringToFront;
         UpdateBoxList;
       end;
end;

procedure TFormBGRAImageManipulationDemo.btZUpClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil
  then begin
         CropArea.BringForward;
         UpdateBoxList;
       end;
end;

function TFormBGRAImageManipulationDemo.CropAreaLoad(AOwner: TBGRAImageManipulation; CropArea: TCropArea;
  const XMLConf: TXMLConfig; const Path: String): Integer;
begin
  //
end;

procedure TFormBGRAImageManipulationDemo.CropAreaSave(AOwner: TBGRAImageManipulation; CropArea: TCropArea;
  const XMLConf: TXMLConfig; const Path: String);
begin
  //
end;

procedure TFormBGRAImageManipulationDemo.edNameChange(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil
  then CropArea.Name :=edName.Text;
end;

procedure TFormBGRAImageManipulationDemo.edUnit_TypeChange(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil then
  begin
    CropArea.AreaUnit:=TResolutionUnit(edUnit_Type.ItemIndex);
    FillBoxUI(CropArea);
  end;
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
  if edUnit_Type.ItemIndex=0
  then newCropArea :=BGRAImageManipulation.addCropArea(Rect(50, 50, 100, 100))
  else newCropArea :=BGRAImageManipulation.addCropArea(Rect(1, 1, 2, 2), TResolutionUnit(edUnit_Type.ItemIndex));

  newCropArea.BorderColor :=VGALime;
end;

procedure TFormBGRAImageManipulationDemo.btBox_DelClick(Sender: TObject);
var
   CropArea :TCropArea;
   curIndex :Integer;

begin
  curIndex :=cbBoxList.ItemIndex;
  if (curIndex>-1) then
  begin
    CropArea :=TCropArea(cbBoxList.Items.Objects[curIndex]);
    BGRAImageManipulation.delCropArea(CropArea);
    cbBoxList.ItemIndex:=cbBoxList.Items.IndexOfObject(BGRAImageManipulation.SelectedCropArea);
  end;
  FillBoxUI(BGRAImageManipulation.SelectedCropArea);
end;

procedure TFormBGRAImageManipulationDemo.cbBoxListChange(Sender: TObject);
begin
   BGRAImageManipulation.SelectedCropArea :=TCropArea(cbBoxList.Items.Objects[cbBoxList.ItemIndex]);
end;

procedure TFormBGRAImageManipulationDemo.edHeightChange(Sender: TObject; AByUser: boolean);
var
   CropArea :TCropArea;

begin
  if inFillBoxUI then exit;

  CropArea :=GetCurrentCropArea;
  if (CropArea<>nil)
  then CropArea.Height:=edHeight.Value;
end;

procedure TFormBGRAImageManipulationDemo.edLeftChange(Sender: TObject; AByUser: boolean);
var
   CropArea :TCropArea;

begin
  if inFillBoxUI then exit;

  CropArea :=GetCurrentCropArea;
  if (CropArea<>nil)
  then CropArea.Left :=edLeft.Value;
end;

procedure TFormBGRAImageManipulationDemo.edTopChange(Sender: TObject; AByUser: boolean);
var
   CropArea :TCropArea;

begin
  if inFillBoxUI then exit;

  CropArea :=GetCurrentCropArea;
  if (CropArea<>nil)
  then CropArea.Top :=edTop.Value;
end;

procedure TFormBGRAImageManipulationDemo.edWidthChange(Sender: TObject; AByUser: boolean);
var
   CropArea :TCropArea;

begin
  if inFillBoxUI then exit;

  CropArea :=GetCurrentCropArea;
  if (CropArea<>nil)
  then CropArea.Width:=edWidth.Value;
end;

procedure TFormBGRAImageManipulationDemo.FormCreate(Sender: TObject);
var
   i,j :Integer;
   t,e:String;

begin
   closing :=False;
   changingAspect :=False;
   inFillBoxUI :=False;
   lastNewBoxNum :=0;
   TStringList(cbBoxList.Items).OwnsObjects:=False;
   j:=0;
   for i :=0 to ImageHandlers.Count-1 do
   begin
     t :=ImageHandlers.TypeNames[i];
     e :=ImageHandlers.Extensions[t];
     if (ImageHandlers.ImageWriter[t]<>nil) then
     begin
       cbSaveFormat.Items.Add(t);
       if (Pos('jpg', e)>0) then j:=i;
     end;
   end;
   cbSaveFormat.ItemIndex:=j-1;
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
   CropArea.AreaUnit:=BGRAImageManipulation.Bitmap.ResolutionUnit;
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
         BCPanelCropArea.Enabled:=(cbBoxList.Items.Count>0);
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

function TFormBGRAImageManipulationDemo.GetCurrentCropArea: TCropArea;
begin
   if (cbBoxList.ItemIndex<0)
   then Result :=nil
   else Result :=TCropArea(cbBoxList.Items.Objects[cbBoxList.ItemIndex]);
end;

procedure TFormBGRAImageManipulationDemo.FillBoxUI(ABox: TCropArea);
begin
   if (ABox<>nil)
   then begin
           inFillBoxUI :=True;
           BCPanelCropArea.Enabled :=True;
           edName.Text :=ABox.Name;
           edUnit_Type.ItemIndex :=Integer(ABox.AreaUnit);

           if (ABox.AreaUnit=ruNone)
           then begin
                  edLeft.DecimalPlaces:=0;
                  edTop.DecimalPlaces:=0;
                  edWidth.DecimalPlaces:=0;
                  edHeight.DecimalPlaces:=0;
                end
           else begin
                  edLeft.DecimalPlaces:=3;
                  edTop.DecimalPlaces:=3;
                  edWidth.DecimalPlaces:=3;
                  edHeight.DecimalPlaces:=3;
                end;
           edLeft.MaxValue:=ABox.MaxWidth;
           edTop.MaxValue:=ABox.MaxHeight;
           edWidth.MaxValue:=edLeft.MaxValue;
           edHeight.MaxValue:=edTop.MaxValue;

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
           inFillBoxUI :=False;
        end
   else BCPanelCropArea.Enabled :=False;
end;


end.
