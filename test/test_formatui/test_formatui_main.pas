// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Created by BGRA Controls Team

         (c) 2025 Massimo Magnano

  Test Save File Format Settings Form
}

unit test_formatui_main;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, EditBtn, Buttons,
  FpImage,
  BGRABitmapTypes,
  BCPanel, BCRoundedImage, BCButton, BGRADialogs;

type
  { TSaveFile_Settings }

  TSaveFile_Settings = class(TForm)
    BCLabel10: TLabel;
    BCLabel9: TLabel;
    btLoad: TBCButton;
    btSave: TBCButton;
    edFileName: TEdit;
    lbExt: TLabel;
    lbDetails: TLabel;
    openPictBGRA: TBGRAOpenPictureDialog;
    panelRead: TBCPanel;
    bcImage: TBCRoundedImage;
    panelWrite: TBCPanel;
    cbSaveFormat: TComboBox;
    dirDestination: TDirectoryEdit;
    Label6: TLabel;
    procedure btLoadClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure cbSaveFormatChange(Sender: TObject);
    procedure dirDestinationChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);

  private
    SaveFormat: TBGRAImageFormat;
    SaveWriter: TFPCustomImageWriter;
    SavePath: String;
    panelFormatUI: TBCPanel;

    procedure AdjustFormatPanel;

  public

  end;

var
  SaveFile_Settings: TSaveFile_Settings;

implementation

{$R *.lfm}

uses Math, BGRAFormatUI, BGRAReadLzp, BGRAWriteLzp, BGRAPaintNet;

{ TSaveFile_Settings }

procedure TSaveFile_Settings.AdjustFormatPanel;
begin
  lbExt.Caption:= '.'+SuggestImageExtension(SaveFormat);

  if (panelFormatUI <> nil)
  then begin
         panelFormatUI.Top:= 150; panelFormatUI.Left:= 10;
         Width:= Max(780, panelFormatUI.Width+130);
         Height:= Max(450, panelFormatUI.Height+150);

         panelFormatUI.Parent:= panelWrite;
         panelFormatUI.Visible:= True;
       end
  else begin
         Width:= 780;
         Height:= 450;
       end;
end;

procedure TSaveFile_Settings.cbSaveFormatChange(Sender: TObject);
begin
  SaveFormat:= TBGRAImageFormat(PTRUInt(cbSaveFormat.Items.Objects[cbSaveFormat.ItemIndex]));
  SaveWriter.Free;
  SaveWriter:= CreateBGRAImageWriter(SaveFormat, True);

  if (panelFormatUI <> nil) then panelFormatUI.Visible:= False;

  TBGRAFormatUIContainer.GetUI(SaveFormat, SaveWriter, panelFormatUI);
  AdjustFormatPanel;
end;

procedure TSaveFile_Settings.btLoadClick(Sender: TObject);
begin
  try
     if openPictBGRA.Execute then
     begin
       bcImage.Bitmap.LoadFromFile(openPictBGRA.FileName); //'c:\tmp\Acquisitions Book 1.03.01, Byzantine.jpg'
       bcImage.Invalidate;
       lbDetails.Caption:= 'image: '+IntToStr(bcImage.Bitmap.Width)+' x '+IntToStr(bcImage.Bitmap.Height);
     end;

  finally
  end;
end;

procedure TSaveFile_Settings.btSaveClick(Sender: TObject);
var
   ext: String;

begin
  if not(bcImage.Bitmap.Empty) then
  begin
    if (BGRAFormatUIContainer <> nil) and
       (panelFormatUI <> nil)
    then BGRAFormatUIContainer.SetWriterProperties(SaveWriter);

    ext:= '.'+SuggestImageExtension(SaveFormat);
    bcImage.Bitmap.SaveToFile(SavePath+DirectorySeparator+edFileName.Text+ext,
                              SaveWriter);

    MessageDlg('Saved as '#13#10+edFileName.Text+ext+#13#10'Class='+SaveWriter.ClassName, mtInformation, [mbOk], 0);
  end;
end;

procedure TSaveFile_Settings.dirDestinationChange(Sender: TObject);
begin
  SavePath :=dirDestination.Directory;
end;

procedure TSaveFile_Settings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (SaveWriter<>nil) then SaveWriter.Free;
  if (BGRAFormatUIContainer <> nil) then BGRAFormatUIContainer.Free;
end;

procedure TSaveFile_Settings.FormShow(Sender: TObject);
begin
  try
     panelFormatUI:= nil;

     dirDestination.Directory:= ExtractFileDir(ParamStr(0));

     //Select JPeg as Current Format
     if (TBGRAFormatUIContainer.BuildSaveFormats(cbSaveFormat, ifJpeg) > 0) then
     begin
       SaveFormat:= ifJpeg;
       SaveWriter:= CreateBGRAImageWriter(SaveFormat, True);
       TBGRAFormatUIContainer.GetUI(SaveFormat, SaveWriter, panelFormatUI);
     end
     else raise Exception.Create('No Writers Registered...');

  finally
    AdjustFormatPanel;
  end;
end;

initialization
  RegisterPaintNetFormat;

end.

