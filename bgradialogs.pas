// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Additional dialogs to take advantage of our controls

  2025-01 Massimo Magnano
}
unit BGRADialogs;

{$mode objfpc}{$H+}

{$ifdef WINDOWS}
  //{$define Show_PreviewControl}  //THIS IS JUST FOR TESTING, It is not recommended for now under Windows
{$endif}

interface

uses
  {$ifdef Show_PreviewControl}
  Windows, Graphics,
  {$endif}
  Classes, SysUtils, ExtDlgs, Controls, StdCtrls, ExtCtrls,
  BGRABitmapTypes, BCRoundedImage;

resourcestring
  rsSelectAPreviewFile = 'Select the File to preview';

type

  { TBGRAOpenPictureDialog }

  TBGRAOpenPictureDialog = class(TPreviewFileDialog)
   private
    FDefaultFilter: string;
    FImageCtrl: TBCRoundedImage;
    FPicturePanel: TPanel;
    FPictureDetails: TLabel;
    FPreviewFilename: string;

  protected
    {$ifdef Show_PreviewControl}
    DialogWnd,
    pParentWnd, pBrotherWnd : HWnd;
    {$endif}

    class procedure WSRegisterClass; override;
    function  IsFilterStored: Boolean; virtual;
    procedure InitPreviewControl; override;
    procedure ClearPreview; virtual;
    procedure UpdatePreview; virtual;

    {$ifdef Show_PreviewControl}
    procedure GetDialogWnd;
    procedure ResizePreviewControl;
    {$endif}

    property ImageCtrl: TBCRoundedImage read FImageCtrl;
    property PicturePanel: TPanel read FPicturePanel;
    property PictureDetails: TLabel read FPictureDetails;

  public
    constructor Create(TheOwner: TComponent); override;
    procedure DoClose; override;
    procedure DoSelectionChange; override;
    procedure DoShow; override;
    function GetFilterExt: String;
    property DefaultFilter: string read FDefaultFilter;
  published
    property Filter stored IsFilterStored;
  end;

  { TSavePictureDialog }

  TBGRASavePictureDialog = class(TBGRAOpenPictureDialog)
  protected
    class procedure WSRegisterClass; override;
    function DefaultTitle: string; override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;


//Functions to Get Filters String useful in Dialogs
function GetBGRAFormatFilter(AFormat: TBGRAImageFormat): String;
procedure BuildBGRAFilterStrings(AUseReaders: Boolean; var Descriptions, Filters: String);
function BuildBGRAImageReaderFilter: String;
function BuildBGRAImageWriterFilter: String;

procedure Register;

implementation

uses
  WSExtDlgs, Masks, FileUtil, LazFileUtils, LCLStrConsts, LCLType;

function GetBGRAFormatFilter(AFormat: TBGRAImageFormat): String;
begin
  Result := StringReplace('*.' + BGRAImageFormat[AFormat].Extensions, ';', ';*.', [rfReplaceAll]);
end;

procedure BuildBGRAFilterStrings(AUseReaders: Boolean; var Descriptions, Filters: String);
var
  iFormat: TBGRAImageFormat;
  Filter: String;
  addExt: Boolean;

begin
  Descriptions := '';
  Filters := '';

  for iFormat:=Low(TBGRAImageFormat) to High(TBGRAImageFormat) do
  begin
    if AUseReaders
    then addExt:= (iFormat<>ifUnknown) and (DefaultBGRAImageReader[iFormat] <> nil)
    else addExt:= (iFormat<>ifUnknown) and (DefaultBGRAImageWriter[iFormat] <> nil);

    if addExt then
    begin
      if (iFormat>ifJpeg) then
      begin
        Descriptions := Descriptions + '|';
        Filters := Filters + ';';
      end;

      Filter := GetBGRAFormatFilter(iFormat);
      FmtStr(Descriptions, '%s%s (%s)|%s',
            [Descriptions, BGRAImageFormat[iFormat].TypeName, Filter, Filter]);
      FmtStr(Filters, '%s%s', [Filters, Filter]);
    end;
  end;

  FmtStr(Descriptions, '%s (%s)|%1:s|%s', [rsGraphic, Filters, Descriptions]);
end;


function BuildBGRAImageReaderFilter: String;
var
  Filters: string;

begin
  Result := '';
  BuildBGRAFilterStrings(True, Result, Filters);
end;

function BuildBGRAImageWriterFilter: String;
var
  Filters: string;

begin
  Result := '';
  BuildBGRAFilterStrings(False, Result, Filters);
end;

{ TBGRAOpenPictureDialog }

class procedure TBGRAOpenPictureDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterOpenPictureDialog;
end;

function TBGRAOpenPictureDialog.IsFilterStored: Boolean;
begin
  Result := (Filter<>FDefaultFilter);
end;

procedure TBGRAOpenPictureDialog.DoClose;
begin
  inherited DoClose;
//  PreviewFileControl.ParentWindow:=0;
end;

procedure TBGRAOpenPictureDialog.DoSelectionChange;
begin
  UpdatePreview;
  inherited DoSelectionChange;
end;

procedure TBGRAOpenPictureDialog.DoShow;
begin
  ClearPreview;
  inherited DoShow;
end;

procedure TBGRAOpenPictureDialog.InitPreviewControl;
begin
  inherited InitPreviewControl;

  PreviewFileControl.Width:=300;
  PreviewFileControl.Height:=300;
  FPicturePanel.Parent:=PreviewFileControl;
  FPicturePanel.Align:=alClient;
  { #note -oMaxM : We create it here because the LCL assumes there is a groupbox
                   with only an image inside and crashes if it find it before this point }
  FPictureDetails:=TLabel.Create(Self);
    with FPictureDetails do begin
      Name:='FPictureDetails';
      Parent:= FPicturePanel;
      Top:=PreviewFileControl.Height-20;
      Height:=20;
      Width:=PreviewFileControl.Width;
      Align:=alBottom;
      Caption:='';
    end;

  FImageCtrl.Align:=alClient;
end;

procedure TBGRAOpenPictureDialog.ClearPreview;
begin
  FPicturePanel.VerticalAlignment:=taVerticalCenter;
  FPicturePanel.Caption:= rsSelectAPreviewFile;
  FImageCtrl.Bitmap:=nil;
  FImageCtrl.Visible:= False;
  FPictureDetails.Caption:='';
end;

procedure TBGRAOpenPictureDialog.UpdatePreview;
var
  CurFilename: String;
  FileIsValid: boolean;

begin
  {$ifdef Show_PreviewControl}
  if (DialogWnd = 0) then GetDialogWnd;
  ResizePreviewControl;
  {$endif}

  FPicturePanel.Caption:= '';
  FPictureDetails.Caption:='';

  CurFilename := FileName;
  if CurFilename = FPreviewFilename then exit;

  FPreviewFilename := CurFilename;
  FileIsValid := FileExistsUTF8(FPreviewFilename)
                 and (not DirPathExists(FPreviewFilename))
                 and FileIsReadable(FPreviewFilename);
  if FileIsValid then
    try
      FImageCtrl.Bitmap.LoadFromFile(FPreviewFilename);
      FImageCtrl.Visible:= True;
      FImageCtrl.Invalidate; { #todo -oMaxM : an event in TBGRBitmap might be useful }

      FPictureDetails.Caption:= Format('%d x %d x %d dpi', [FImageCtrl.Bitmap.Width, FImageCtrl.Bitmap.Height, Trunc(FImageCtrl.Bitmap.ResolutionX)]);
    except
      FileIsValid := False;
    end;
  if not FileIsValid then ClearPreview;
end;

{$ifdef Show_PreviewControl}
procedure TBGRAOpenPictureDialog.GetDialogWnd;
var
  pHandle: HWND;
  thID, prID, appID:DWord;

begin
  pBrotherWnd:= 0;
  pParentWnd:= 0;

  //LCL doesn't pass us the Dialog Handle, so we have to look for it the old fashioned way
  appID:= GetProcessId;
  repeat
    DialogWnd:= FindWindowEx(0, DialogWnd, PChar('#32770'), nil);
    thID:= GetWindowThreadProcessId(DialogWnd, prID);
  until (DialogWnd=0) or (prID = appID);

  //Get Parent and Brother Control
  //  this depends on the OS and needs to be tested as much as possible (for now it works with Windows 10)
  if (DialogWnd<>0) then
  begin
    pHandle:= FindWindowEx(DialogWnd, 0, PChar('DUIViewWndClassName'), nil);
    if (pHandle<>0) then  //Windows 10
    begin
      pParentWnd:= FindWindowEx(pHandle, 0, PChar('DirectUIHWND'), nil);
      if (pParentWnd<>0) then
      begin
        repeat
          pBrotherWnd:= FindWindowEx(pParentWnd, pBrotherWnd, PChar('CtrlNotifySink'), nil);
          pHandle:= FindWindowEx(pBrotherWnd, 0, PChar('SHELLDLL_DefView'), nil);
        until (pBrotherWnd=0) or (pHandle<>0);

        if (pBrotherWnd<>0) and (pHandle<>0) then PreviewFileControl.ParentWindow:=pParentWnd;
      end;
    end;
  end;
end;

procedure TBGRAOpenPictureDialog.ResizePreviewControl;
var
  rectParent, rectBrother: TRect;

begin
  if (DialogWnd<>0) and (pParentWnd<>0) and (pBrotherWnd<>0) then
  begin
    if GetClientRect(pParentWnd, rectParent) and GetWindowRect(pBrotherWnd, rectBrother) then
    begin
      ScreenToClient(pParentWnd, rectBrother.TopLeft);
      ScreenToClient(pParentWnd, rectBrother.BottomRight);
      PreviewFileControl.SetBounds(rectBrother.Left+4+rectBrother.Width, rectBrother.Top+4,
                                   rectParent.Right-rectBrother.Right-8,
                                   rectParent.Bottom-rectBrother.Top-8);
    end;
  end;
end;
{$endif}

constructor TBGRAOpenPictureDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDefaultFilter := BuildBGRAImageReaderFilter+'|'+
                    Format(rsAllFiles,[GetAllFilesMask, GetAllFilesMask,'']);
  Filter:=FDefaultFilter;

  {$ifdef Show_PreviewControl}
  DialogWnd:= 0;
  pBrotherWnd:= 0;
  pParentWnd:= 0;
  {$endif}

  FPicturePanel:=TPanel.Create(Self);
  with FPicturePanel do begin
    Name:='FPicturePanel';
    BorderStyle:=bsNone;
    BevelOuter:=bvNone;
    VerticalAlignment:=taVerticalCenter;
  end;

  FImageCtrl:=TBCRoundedImage.Create(Self);
  with FImageCtrl do begin
    Name:='FImageCtrl';
    Parent:=FPicturePanel;
    Style:=isSquare;
    Proportional:=true;
  end;
end;

function TBGRAOpenPictureDialog.GetFilterExt: String;
var
  ParsedFilter: TParseStringList;
begin
  Result := '';

  ParsedFilter := TParseStringList.Create(Filter, '|');
  try
    if (FilterIndex > 0) and (FilterIndex * 2 <= ParsedFilter.Count) then
    begin
      Result := AnsiLowerCase(ParsedFilter[FilterIndex * 2 - 1]);
      // remove *.*
      if (Result <> '') and (Result[1] = '*') then Delete(Result, 1, 1);
      if (Result <> '') and (Result[1] = '.') then Delete(Result, 1, 1);
      if (Result <> '') and (Result[1] = '*') then Delete(Result, 1, 1);
      // remove all after ;
      if Pos(';', Result) > 0 then Delete(Result, Pos(';', Result), MaxInt);
    end;

    if Result = '' then Result := DefaultExt;
  finally
    ParsedFilter.Free;
  end;
end;

{ TSavePictureDialog }

class procedure TBGRASavePictureDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterSavePictureDialog;
end;

function TBGRASavePictureDialog.DefaultTitle: string;
begin
  Result := rsfdFileSaveAs;
end;

constructor TBGRASavePictureDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDefaultFilter := BuildBGRAImageWriterFilter+'|'+
                    Format(rsAllFiles,[GetAllFilesMask, GetAllFilesMask,'']);
  Filter:=FDefaultFilter;

  fCompStyle:=csSaveFileDialog;
end;

procedure Register;
begin
  RegisterComponents('BGRA Dialogs',[TBGRAOpenPictureDialog, TBGRASavePictureDialog]);
end;


end.

