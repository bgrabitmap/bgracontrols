{
  Created by BGRA Controls Team
  Dibo, Circular, lainz (007) and contributors.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}


unit customdrawn_windows7;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, types, Math, Zipper, FPReadPNG, FileUtil, INIFiles, LCLProc,
  { Custom Drawn }
  customdrawn_common, customdrawndrawers, FPCanvas,
  { BGRABitmap }
  bgrabitmap, bgrabitmaptypes, bgraslicescaling,
  { BGRAControls}
  BCFilters;

procedure AssignFontToBGRA(Source: TFont; Dest: TBGRABitmap);

const
  WIN7_3DDKSHADOW_COLOR = $00696969;
  WIN7_3DLIGHT_COLOR = $00E3E3E3;
  WIN7_ACTIVEBORDER_COLOR = $00B4B4B4;
  WIN7_ACTIVECAPTION_COLOR = $00D1B499;
  WIN7_APPWORKSPACE_COLOR = $00ABABAB;
  WIN7_BACKGROUND_COLOR = clBlack;
  WIN7_BTNFACE_COLOR = $00F0F0F0;
  WIN7_BTNHIGHLIGHT_COLOR = clWhite;
  WIN7_BTNSHADOW_COLOR = $00A0A0A0;
  WIN7_BTNTEXT_COLOR = clBlack;
  WIN7_CAPTIONTEXT_COLOR = clBlack;
  WIN7_FORM_COLOR = $00F0F0F0;
  WIN7_GRADIENTACTIVECAPTION_COLOR = $00EAD1B9;
  WIN7_GRADIENTINACTIVECAPTION_COLOR = $00F2E4D7;
  WIN7_GRAYTEXT_COLOR = $006D6D6D;
  WIN7_HIGHLIGHTTEXT_COLOR = $00FF9933;
  WIN7_HIGHLIGHT_COLOR = $00FF9933;
  WIN7_HOTLIGHT_COLOR = $00CC6600;
  WIN7_INACTIVEBORDER_COLOR = $00FCF7F4;
  WIN7_INACTIVECAPTIONTEXT_COLOR = $00544E43;
  WIN7_INACTIVECAPTION_COLOR = $00DBCDBF;
  WIN7_INFOBK_COLOR = $00E1FFFF;
  WIN7_INFOTEXT_COLOR = clBlack;
  WIN7_MENUBAR_COLOR = $00F0F0F0;
  WIN7_MENUHIGHLIGHT_COLOR = $00FF9933;
  WIN7_MENUTEXT_COLOR = clBlack;
  WIN7_MENU_COLOR = $00F0F0F0;
  WIN7_SCROLLBAR_COLOR = $00C8C8C8;
  WIN7_WINDOWFRAME_COLOR = $00646464;
  WIN7_WINDOWTEXT_COLOR = clBlack;
  WIN7_WINDOW_COLOR = clWhite;

type

  { TBGRAZip }

  TBGRAZip = class
  private
    Bmp: TBGRABitmap;
  private
    procedure DoCreateOutZipStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
    procedure DoDoneOutZipStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
  public
    function Extract(ZipName, FileName: string): TBGRABitmap;
  end;

  { TMemoryStreamZip }

  TMemoryStreamZip = class
  private
    Stm: TMemoryStream;
  private
    procedure DoCreateOutZipStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
    procedure DoDoneOutZipStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
  public
    function Extract(ZipName, FileName: string): TMemoryStream;
  end;

{ Use this to extract PNG files and load into BGRABitmap }
function ExtractZipPNGtoBGRA(ZipName, FileName: string): TBGRABitmap;
{ Use this to extract any file and load into MemoryStream }
function ExtractZipFiletoMemoryStream(ZipName, FileName: string): TMemoryStream;

type
  { TBitmapTheme }

  TBitmapTheme = class
  private
    // general
    FButton: TBGRAMultiSliceScaling;
    FCheckBox: TBGRAMultiSliceScaling;
    FRadioButton: TBGRAMultiSliceScaling;
    FProgressBarHorizontalBackground: TBGRAMultiSliceScaling;
    FProgressBarVerticalBackground: TBGRAMultiSliceScaling;
    FProgressBarHorizontalFill: TBGRAMultiSliceScaling;
    FProgressBarVerticalFill: TBGRAMultiSliceScaling;
    // extra
    FArrow: TBGRAMultiSliceScaling;
    FArrowLeft: TBGRAMultiSliceScaling;
    FArrowRight: TBGRAMultiSliceScaling;
    FCloseButton: TBGRAMultiSliceScaling;
    // settings
    FFolder: string;
    FTickmark: boolean;
    FDPI: integer;
    FDebug: boolean;
    FResourcesLoaded: boolean;
    function GetArrowLeftSkin: TBGRAMultiSliceScaling;
    function GetArrowRightSkin: TBGRAMultiSliceScaling;
    function GetArrowSkin: TBGRAMultiSliceScaling;
    function GetButtonSkin: TBGRAMultiSliceScaling;
    function GetCheckBoxSkin: TBGRAMultiSliceScaling;
    function GetCloseButtonSkin: TBGRAMultiSliceScaling;
    function GetProgressBarHorizontalBackgroundSkin: TBGRAMultiSliceScaling;
    function GetProgressBarHorizontalFillSkin: TBGRAMultiSliceScaling;
    function GetProgressBarVerticalBackgroundSkin: TBGRAMultiSliceScaling;
    function GetProgressBarVerticalFillSkin: TBGRAMultiSliceScaling;
    function GetRadioButtonSkin: TBGRAMultiSliceScaling;
    procedure SetFDebug(AValue: boolean);
    procedure SetFDPI(AValue: integer);
    procedure SetFFolder(AValue: string);
    procedure SetFTickmark(AValue: boolean);
  protected
    procedure LoadBitmapResources;
    procedure FreeBitmapResources;
  public
    constructor Create(Folder: string);
    destructor Destroy; override;
  public
    // general
    property Button: TBGRAMultiSliceScaling read GetButtonSkin;
    property CheckBox: TBGRAMultiSliceScaling read GetCheckBoxSkin;
    property RadioButton: TBGRAMultiSliceScaling read GetRadioButtonSkin;
    property ProgressBarHorizontalBackground: TBGRAMultiSliceScaling
      read GetProgressBarHorizontalBackgroundSkin;
    property ProgressBarVerticalBackground: TBGRAMultiSliceScaling
      read GetProgressBarVerticalBackgroundSkin;
    property ProgressBarHorizontalFill: TBGRAMultiSliceScaling
      read GetProgressBarHorizontalFillSkin;
    property ProgressBarVerticalFill: TBGRAMultiSliceScaling
      read GetProgressBarVerticalFillSkin;
    // extra
    property Arrow: TBGRAMultiSliceScaling read GetArrowSkin;
    property ArrowLeft: TBGRAMultiSliceScaling read GetArrowLeftSkin;
    property ArrowRight: TBGRAMultiSliceScaling read GetArrowRightSkin;
    property CloseButton: TBGRAMultiSliceScaling read GetCloseButtonSkin;
    // settings
    property Folder: string read FFolder write SetFFolder;
    property Tickmark: boolean read FTickmark write SetFTickmark;
    property DPI: integer read FDPI write SetFDPI;
    property Debug: boolean read FDebug write SetFDebug;
  end;

  { TCDWin7 }

  TCDWin7 = class(TCDDrawerCommon)
    procedure LoadFallbackPaletteColors; override;
    // General
    function GetMeasures(AMeasureID: integer): integer; override;
    function GetMeasuresEx(ADest: TCanvas; AMeasureID: integer;
      AState: TCDControlState; AStateEx: TCDControlStateEx): integer; override;
    function GetColor(AColorID: integer): TColor; override;
    function GetClientArea(ADest: TCanvas; ASize: TSize; AControlId: TCDControlID;
      AState: TCDControlState; AStateEx: TCDControlStateEx): TRect; override;
    function DPIAdjustment(const AValue: integer): integer;
    // General drawing routines
    procedure DrawFocusRect(ADest: {TCanvas}TFPCustomCanvas; ADestPos: TPoint;
      ASize: TSize); override;
    // TCDButton
    procedure DrawButton(ADest: TFPCustomCanvas; ADestPos: TPoint;
      ASize: TSize; AState: TCDControlState; AStateEx: TCDButtonStateEx); override;
    // TCDCheckBox
    procedure DrawCheckBoxSquare(ADest: TCanvas; ADestPos: TPoint;
      ASize: TSize; AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    procedure DrawCheckBox(ADest: TCanvas; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    // TCDRadioButton
    procedure DrawRadioButtonCircle(ADest: TCanvas; ADestPos: TPoint;
      ASize: TSize; AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    procedure DrawRadioButton(ADest: TCanvas; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    // TCDStaticText
    procedure DrawStaticText(ADest: TCanvas; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    // TCDProgressBar
    procedure DrawProgressBar(ADest: TCanvas; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDProgressBarStateEx); override;
    // Extra buttons drawing routines
    procedure DrawButtonWithArrow(ADest: TCanvas; ADestPos: TPoint;
      ASize: TSize; AState: TCDControlState); override;
    // TCDScrollBar
    procedure DrawScrollBar(ADest: TCanvas; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDPositionedCStateEx); override;
    procedure DrawSlider(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState); override;
  end;

  { TCDWin7Extra }

  TCDWin7Extra = class(TCDWin7)
    // TCDButton
    procedure DrawButton(ADest: TFPCustomCanvas; ADestPos: TPoint;
      ASize: TSize; AState: TCDControlState; AStateEx: TCDButtonStateEx); override;
  end;

var
  win7: TBitmapTheme;

implementation

uses SysUtils;

{ TMemoryStreamZip }

procedure TMemoryStreamZip.DoCreateOutZipStream(Sender: TObject;
  var AStream: TStream; AItem: TFullZipFileEntry);
begin
  AStream := TMemoryStream.Create;
end;

procedure TMemoryStreamZip.DoDoneOutZipStream(Sender: TObject;
  var AStream: TStream; AItem: TFullZipFileEntry);
begin
  AStream.Position := 0;

  Stm := TMemoryStream.Create;

  Stm.LoadFromStream(AStream);

  Astream.Free;
end;

function TMemoryStreamZip.Extract(ZipName, FileName: string): TMemoryStream;
var
  ZipFile: TUnZipper;
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Add(FileName);
  ZipFile := TUnZipper.Create;
  try
    ZipFile.FileName := ZipName;
    ZipFile.OnCreateStream := @DoCreateOutZipStream;
    ZipFile.OnDoneStream := @DoDoneOutZipStream;
    ZipFile.UnZipFiles(sl);
  finally
    ZipFile.Free;
    sl.Free;
  end;

  Result := Stm;
end;

{ TBGRAZip }

procedure TBGRAZip.DoCreateOutZipStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  AStream := TMemoryStream.Create;
end;

procedure TBGRAZip.DoDoneOutZipStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
var
  reader: TFPReaderPNG;
begin
  AStream.Position := 0;

  if bmp <> nil then
    bmp.Free;

  reader := TFPReaderPNG.Create;
  bmp := TBGRABitmap.Create;
  bmp.LoadFromStream(AStream, reader);
  reader.Free;

  Astream.Free;
end;

function TBGRAZip.Extract(ZipName, FileName: string): TBGRABitmap;
var
  ZipFile: TUnZipper;
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Add(FileName);
  ZipFile := TUnZipper.Create;
  try
    ZipFile.FileName := ZipName;
    ZipFile.OnCreateStream := @DoCreateOutZipStream;
    ZipFile.OnDoneStream := @DoDoneOutZipStream;
    ZipFile.UnZipFiles(sl);
  finally
    ZipFile.Free;
    sl.Free;
  end;

  Result := Bmp;
end;

{ TBitmapTheme }

procedure TBitmapTheme.SetFDebug(AValue: boolean);
begin
  if FDebug = AValue then
    Exit;
  FDebug := AValue;
end;

function TBitmapTheme.GetArrowLeftSkin: TBGRAMultiSliceScaling;
begin
  LoadBitmapResources;
  Result := FArrowLeft;
end;

function TBitmapTheme.GetArrowRightSkin: TBGRAMultiSliceScaling;
begin
  LoadBitmapResources;
  Result := FArrowRight;
end;

function TBitmapTheme.GetArrowSkin: TBGRAMultiSliceScaling;
begin
  LoadBitmapResources;
  Result := FArrow;
end;

function TBitmapTheme.GetButtonSkin: TBGRAMultiSliceScaling;
begin
  LoadBitmapResources;
  Result := FButton;
end;

function TBitmapTheme.GetCheckBoxSkin: TBGRAMultiSliceScaling;
begin
  LoadBitmapResources;
  Result := FCheckBox;
end;

function TBitmapTheme.GetCloseButtonSkin: TBGRAMultiSliceScaling;
begin
  LoadBitmapResources;
  Result := FCloseButton;
end;

function TBitmapTheme.GetProgressBarHorizontalBackgroundSkin: TBGRAMultiSliceScaling;
begin
  LoadBitmapResources;
  Result := FProgressBarHorizontalBackground;
end;

function TBitmapTheme.GetProgressBarHorizontalFillSkin: TBGRAMultiSliceScaling;
begin
  LoadBitmapResources;
  Result := FProgressBarHorizontalFill;
end;

function TBitmapTheme.GetProgressBarVerticalBackgroundSkin: TBGRAMultiSliceScaling;
begin
  LoadBitmapResources;
  Result := FProgressBarVerticalBackground;
end;

function TBitmapTheme.GetProgressBarVerticalFillSkin: TBGRAMultiSliceScaling;
begin
  LoadBitmapResources;
  Result := FProgressBarVerticalFill;
end;

function TBitmapTheme.GetRadioButtonSkin: TBGRAMultiSliceScaling;
begin
  LoadBitmapResources;
  Result := FRadioButton;
end;

procedure TBitmapTheme.SetFDPI(AValue: integer);
begin
  if FDPI = AValue then
    Exit;
  FDPI := AValue;
  FreeBitmapResources;
end;

procedure TBitmapTheme.SetFFolder(AValue: string);
begin
  if FFolder = AValue then
    Exit;
  FFolder := AValue;
  FreeBitmapResources;
end;

procedure TBitmapTheme.SetFTickmark(AValue: boolean);
begin
  if FTickmark = AValue then
    Exit;
  FTickmark := AValue;
end;

procedure TBitmapTheme.LoadBitmapResources;

{ Use this to extract PNG files and load into BGRABitmap }
//function ExtractZipPNGtoBGRA(ZipName, FileName: string): TBGRABitmap;
{ Use this to extract any file and load into MemoryStream }
//function ExtractZipFiletoMemoryStream(ZipName, FileName: string): TMemoryStream;

  function CreateFromZip(ZipName, Section: string): TBGRAMultiSliceScaling;
  var
    Direction: TSliceScalingDirection;
    bmp: TBGRABitmap;
    ini: TIniFile;
    i: integer;
    defaultRepeat: string;
    strIni: TMemoryStream;
  begin
    strIni := ExtractZipFileToMemoryStream(ZipName, 'skin.ini');
    ini := TIniFile.Create(strIni);

    bmp := ExtractZipPNGtoBGRA(ZipName, ini.ReadString(Section, 'Bitmap', ''));

    if ini.ReadBool(Section, 'HorizontalDirection', False) then
      Direction := sdHorizontal
    else
      Direction := sdVertical;

    Result := TBGRAMultiSliceScaling.Create(bmp,
      ini.ReadInteger(Section, 'MarginTop', 0), ini.ReadInteger(Section,
      'MarginRight', 0), ini.ReadInteger(Section, 'MarginBottom', 0),
      ini.ReadInteger(Section, 'MarginLeft', 0), ini.ReadInteger(Section,
      'NumberOfItems', 1), Direction, True);

    defaultRepeat := ini.ReadString(Section, 'Repeat', 'Auto');
    for i := 0 to High(Result.SliceScalingArray) do
      Result.SliceScalingArray[i].SliceRepeatAsString :=
        ini.ReadString(Section, 'Repeat' + IntToStr(i + 1), defaultRepeat);

    ini.Free;
    strIni.Free;
  end;

var
  dpi_str: string;
begin
  debugln('Loading Resources.');

  if FResourcesLoaded then
    exit;

  if (FDPI > 96) and (FDPI <= 120) then
    dpi_str := '120'
  else if (FDPI > 120) then
    dpi_str := '144'
  else
    dpi_str := '';

  FreeBitmapResources;

  if FileExistsUTF8(FFolder) then
  begin
    debugln('Loading From File.');
    // general
    debugln('Loading Button.');
    FButton := CreateFromZip(FFolder, 'Button');
    debugln('Loading CheckBox.');
    FCheckBox := CreateFromZip(FFolder, 'CheckBox' + dpi_str);
    debugln('Loading RadioButton.');
    FRadioButton := CreateFromZip(FFolder, 'RadioButton' + dpi_str);
    debugln('Loading ProgressBar.');
    FProgressBarHorizontalBackground := CreateFromZip(FFolder, 'ProgressBar');
    debugln('Loading ProgressBarV.');
    FProgressBarVerticalBackground := CreateFromZip(FFolder, 'ProgressBarV');
    debugln('Loading ProgressBarFill.');
    FProgressBarHorizontalFill := CreateFromZip(FFolder, 'ProgressBarFill');
    debugln('Loading ProgressBarFillV.');
    FProgressBarVerticalFill := CreateFromZip(FFolder, 'ProgressBarFillV');
    // extra
    debugln('Loading Arrow.');
    FArrow := CreateFromZip(FFolder, 'Arrow' + dpi_str);
    debugln('Loading ArrowLeft.');
    FArrowLeft := CreateFromZip(FFolder, 'ArrowLeft' + dpi_str);
    debugln('Loading ArrowRight.');
    FArrowRight := CreateFromZip(FFolder, 'ArrowRight' + dpi_str);
    debugln('Loading CloseButton.');
    FCloseButton := CreateFromZip(FFolder, 'CloseButton' + dpi_str);
    debugln('Resources Loaded.');
    FResourcesLoaded := True;
  end
  else if DirectoryExistsUTF8(FFolder) then
  begin
    debugln('Loading From Folder.');
    // general
    FButton := TBGRAMultiSliceScaling.Create(FFolder + 'skin.ini', 'Button');
    FCheckBox := TBGRAMultiSliceScaling.Create(FFolder + 'skin.ini',
      'CheckBox' + dpi_str);
    FRadioButton := TBGRAMultiSliceScaling.Create(FFolder + 'skin.ini',
      'RadioButton' + dpi_str);
    FProgressBarHorizontalBackground :=
      TBGRAMultiSliceScaling.Create(FFolder + 'skin.ini', 'ProgressBar');
    FProgressBarVerticalBackground :=
      TBGRAMultiSliceScaling.Create(FFolder + 'skin.ini', 'ProgressBarV');
    FProgressBarHorizontalFill :=
      TBGRAMultiSliceScaling.Create(FFolder + 'skin.ini', 'ProgressBarFill');
    FProgressBarVerticalFill :=
      TBGRAMultiSliceScaling.Create(FFolder + 'skin.ini', 'ProgressBarFillV');
    // extra
    FArrow := TBGRAMultiSliceScaling.Create(FFolder + 'skin.ini', 'Arrow' + dpi_str);
    FArrowLeft := TBGRAMultiSliceScaling.Create(FFolder + 'skin.ini',
      'ArrowLeft' + dpi_str);
    FArrowRight := TBGRAMultiSliceScaling.Create(FFolder + 'skin.ini',
      'ArrowRight' + dpi_str);
    FCloseButton := TBGRAMultiSliceScaling.Create(FFolder + 'skin.ini',
      'CloseButton' + dpi_str);
    FResourcesLoaded := True;
  end;
end;

procedure TBitmapTheme.FreeBitmapResources;
begin
  if not FResourcesLoaded then
    exit;

  // general
  if FButton <> nil then
    FreeAndNil(FButton);
  if FCheckBox <> nil then
    FreeAndNil(FCheckBox);
  if FRadioButton <> nil then
    FreeAndNil(FRadioButton);
  if FProgressBarHorizontalBackground <> nil then
    FreeAndNil(FProgressBarHorizontalBackground);
  if FProgressBarVerticalBackground <> nil then
    FreeAndNil(FProgressBarVerticalBackground);
  if FProgressBarHorizontalFill <> nil then
    FreeAndNil(FProgressBarHorizontalFill);
  if FProgressBarVerticalFill <> nil then
    FreeAndNil(FProgressBarVerticalFill);
  // extra
  if FArrow <> nil then
    FreeAndNil(FArrow);
  if FArrowLeft <> nil then
    FreeAndNil(FArrowLeft);
  if FArrowRight <> nil then
    FreeAndNil(FArrowRight);
  if FCloseButton <> nil then
    FreeAndNil(FCloseButton);
  FResourcesLoaded := False;
end;

constructor TBitmapTheme.Create(Folder: string);
begin
  FDPI := 96;
  FFolder := Folder;
  FResourcesLoaded := False;
  inherited Create;
end;

destructor TBitmapTheme.Destroy;
begin
  FreeBitmapResources;
  inherited Destroy;
end;

{ TCDWin7Extra }

procedure TCDWin7Extra.DrawButton(ADest: TFPCustomCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDButtonStateEx);
var
  { number of bitmap used }
  number: integer;
  { bgrabitmap }
  FBGRA: TBGRABitmap;
begin
  FBGRA := TBGRABitmap.Create(ASize.cx, ASize.cy, AStateEx.ParentRGBColor);
  AssignFontToBGRA(AStateEx.Font, FBGRA);

  if csfEnabled in AState then
  begin
    number := 0;
    if csfHasFocus in AState then
      number := 4;
    if csfMouseOver in AState then
      number := 1;
    if csfSunken in AState then
      number := 2;
  end
  else
    number := 3;

  if (number = 4) then
    if (AStateEx.Caption = 'arrowleft') or (AStateEx.Caption = 'arrowright') or
      (AStateEx.Caption = 'closebutton') then
      number := 0;

  if AStateEx.Caption = 'arrowleft' then
    win7.ArrowLeft.Draw(number, FBGRA, 0, 0, ASize.cx, ASize.cy, win7.Debug)
  else if AStateEx.Caption = 'arrowright' then
    win7.ArrowRight.Draw(number, FBGRA, 0, 0, ASize.cx, ASize.cy, win7.Debug)
  else if AStateEx.Caption = 'arrow' then
    win7.Arrow.Draw(number, FBGRA, 0, 0, ASize.cx, ASize.cy, win7.Debug)
  else if AStateEx.Caption = 'closebutton' then
    win7.CloseButton.Draw(number, FBGRA, 0, 0, ASize.cx, ASize.cy, win7.Debug);

  { Draw and Free }
  FBGRA.Draw(TCanvas(ADest), 0, 0, True);
  FBGRA.Free;
end;

{ TCDWin7 }

procedure TCDWin7.LoadFallbackPaletteColors;
begin
  with Palette do
  begin
    ActiveBorder := WIN7_ACTIVEBORDER_COLOR;
    ActiveCaption := WIN7_ACTIVECAPTION_COLOR;
    AppWorkspace := WIN7_APPWORKSPACE_COLOR;
    Background := WIN7_BACKGROUND_COLOR;
    BtnFace := WIN7_BTNFACE_COLOR;
    BtnHighlight := WIN7_BTNHIGHLIGHT_COLOR;
    BtnShadow := WIN7_BTNSHADOW_COLOR;
    BtnText := WIN7_BTNTEXT_COLOR;
    CaptionText := WIN7_CAPTIONTEXT_COLOR;
    color3DDkShadow := WIN7_3DDKSHADOW_COLOR;
    color3DLight := WIN7_3DLIGHT_COLOR;
    Form := WIN7_FORM_COLOR;
    GradientActiveCaption := WIN7_GRADIENTACTIVECAPTION_COLOR;
    GradientInactiveCaption := WIN7_GRADIENTINACTIVECAPTION_COLOR;
    GrayText := WIN7_GRAYTEXT_COLOR;
    Highlight := WIN7_HIGHLIGHT_COLOR;
    HighlightText := WIN7_HIGHLIGHTTEXT_COLOR;
    HotLight := WIN7_HOTLIGHT_COLOR;
    InactiveBorder := WIN7_INACTIVEBORDER_COLOR;
    InactiveCaption := WIN7_INACTIVECAPTION_COLOR;
    InactiveCaptionText := WIN7_INACTIVECAPTIONTEXT_COLOR;
    InfoBk := WIN7_INFOBK_COLOR;
    InfoText := WIN7_INFOTEXT_COLOR;
    Menu := WIN7_MENU_COLOR;
    MenuBar := WIN7_MENUBAR_COLOR;
    MenuHighlight := WIN7_MENUHIGHLIGHT_COLOR;
    MenuText := WIN7_MENUTEXT_COLOR;
    ScrollBar := WIN7_SCROLLBAR_COLOR;
    Window := WIN7_WINDOW_COLOR;
    WindowFrame := WIN7_WINDOWFRAME_COLOR;
    WindowText := WIN7_WINDOWTEXT_COLOR;
  end;
end;

function TCDWin7.GetMeasures(AMeasureID: integer): integer;
begin
  Result := inherited GetMeasures(AMeasureID);
end;

function TCDWin7.GetMeasuresEx(ADest: TCanvas; AMeasureID: integer;
  AState: TCDControlState; AStateEx: TCDControlStateEx): integer;
begin
  Result := inherited GetMeasuresEx(ADest, AMeasureID, AState, AStateEx);
end;

function TCDWin7.GetColor(AColorID: integer): TColor;
begin
  case AColorId of
    TCDEDIT_BACKGROUND_COLOR: Result := WIN7_WINDOW_COLOR;
    TCDEDIT_TEXT_COLOR: Result := WIN7_WINDOWTEXT_COLOR;
    TCDEDIT_SELECTED_BACKGROUND_COLOR: Result := WIN7_HIGHLIGHTTEXT_COLOR;
    TCDEDIT_SELECTED_TEXT_COLOR: Result := WIN7_WINDOW_COLOR;
    TCDBUTTON_DEFAULT_COLOR: Result := WIN7_BTNFACE_COLOR;
    else
      Result := clBlack;
  end;
end;

function TCDWin7.GetClientArea(ADest: TCanvas; ASize: TSize;
  AControlId: TCDControlID; AState: TCDControlState;
  AStateEx: TCDControlStateEx): TRect;
begin
  Result := inherited GetClientArea(ADest, ASize, AControlId, AState, AStateEx);
end;

function TCDWin7.DPIAdjustment(const AValue: integer): integer;
begin
  {if Screen.PixelsPerInch <= 96 then Result := AValue
  else Result := Round(AValue * Screen.PixelsPerInch / 96);}
  Result := Round(AValue * win7.DPI / 96);
end;

procedure TCDWin7.DrawFocusRect(ADest: {TCanvas}TFPCustomCanvas;
  ADestPos: TPoint; ASize: TSize);
var
  FBGRA: TBGRABitmap;
begin
  FBGRA := TBGRABitmap.Create(ASize.CX, ASize.CY);
  CheckeredR(FBGRA);
  FBGRA.AlphaFillRect(2, 2, FBGRA.Width - 2, FBGRA.Height - 2, 0);
  FBGRA.Draw(TCanvas(ADest), ADestPos.X, ADestPos.Y, False);
  FBGRA.Free;
end;

procedure TCDWin7.DrawButton(ADest: TFPCustomCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDButtonStateEx);
var
  Str: string;
  lGlyphLeftSpacing: integer = 0;
  lTextOutPos: TPoint;
  lGlyphCaptionHeight: integer;
  { number of bitmap used }
  number: integer;
  { bgrabitmap }
  FBGRA: TBGRABitmap;
  FBGRAGlyph: TBGRABitmap;
begin
  FBGRA := TBGRABitmap.Create(ASize.cx, ASize.cy, AStateEx.ParentRGBColor);
  AssignFontToBGRA(AStateEx.Font, FBGRA);

  if csfEnabled in AState then
  begin
    number := 0;
    if csfHasFocus in AState then
      number := 4;
    if csfMouseOver in AState then
      number := 1;
    if csfSunken in AState then
      number := 2;
  end
  else
    number := 3;

  win7.button.Draw(number, FBGRA, 0, 0, ASize.cx, ASize.cy, win7.debug);

  { Position calculations }
  Str := AStateEx.Caption;
  lGlyphCaptionHeight := Max(FBGRA.TextSize(Str).cy, AStateEx.Glyph.Height);
  lTextOutPos.X := (ASize.cx - FBGRA.TextSize(Str).cx - AStateEx.Glyph.Width) div 2;
  lTextOutPos.Y := (ASize.cy - lGlyphCaptionHeight) div 2;
  lTextOutPos.X := Max(lTextOutPos.X, 5);
  lTextOutPos.Y := Max(lTextOutPos.Y, 5);

  { Glyph }
  if not AStateEx.Glyph.Empty then
  begin
    FBGRAGlyph := TBGRABitmap.Create(AStateEx.Glyph);
    if csfEnabled in AState then
    else
      GrayScaleBGRA(FBGRAGlyph);
    FBGRA.BlendImage(lTextOutPos.X, lTextOutPos.Y, FBGRAGlyph, boLinearBlend);
    lGlyphLeftSpacing := FBGRAGlyph.Width + 5;
    FBGRAGlyph.Free;
  end;

  { Text }
  lTextOutPos.X := lTextOutPos.X + lGlyphLeftSpacing;
  lTextOutPos.Y := (ASize.cy - FBGRA.TextSize(Str).cy) div 2;

  if csfEnabled in AState then
    FBGRA.TextOut(lTextOutPos.X, lTextOutPos.Y, AStateEx.Caption, Palette.WindowText)
  else
  begin
    FBGRA.TextOut(lTextOutPos.X + 1, lTextOutPos.Y + 1, AStateEx.Caption,
      Palette.Window);
    FBGRA.TextOut(lTextOutPos.X, lTextOutPos.Y, AStateEx.Caption, Palette.GrayText);
  end;

  { Draw and Free }
  FBGRA.Draw(TCanvas(ADest), 0, 0, True);
  FBGRA.Free;
end;

procedure TCDWin7.DrawCheckBoxSquare(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDControlStateEx);
begin
  inherited DrawCheckBoxSquare(ADest, ADestPos, ASize, AState, AStateEx);
end;

procedure TCDWin7.DrawCheckBox(ADest: TCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDControlStateEx);
var
  lSquareHeight: integer;
  number: integer;
  FBGRA: TBGRABitmap;
begin
  FBGRA := TBGRABitmap.Create(ASize.cx, ASize.cy, AStateEx.ParentRGBColor);
  AssignFontToBGRA(AStateEx.Font, FBGRA);

  lSquareHeight := win7.CheckBox.SliceScalingArray[0].BitmapHeight;

  number := 0;

  if csfOn in AState then
    if win7.tickmark then
      number := 16
    else
      number := 4;

  // for the xp theme
  if (csfOn in AState) and (win7.folder = 'luna' + pathdelim) then
    number := 4;

  if csfPartiallyOn in AState then
    number := 8;

  if csfEnabled in AState then
  begin
    if csfMouseOver in AState then
    begin
      Inc(number, +1);
      if csfSunken in AState then
        Inc(number, +1);
    end
    else
    if csfSunken in AState then
      Inc(number, +2);
  end
  else
    Inc(number, +3);

  win7.checkbox.Draw(number, FBGRA, 0, 0, lSquareHeight, lSquareHeight, win7.debug);

  if csfEnabled in AState then
    FBGRA.TextOut(lSquareHeight + 5, 0, AStateEx.Caption, Palette.WindowText)
  else
  begin
    FBGRA.TextOut(lSquareHeight + 6, 1, AStateEx.Caption, Palette.Window);
    FBGRA.TextOut(lSquareHeight + 5, 0, AStateEx.Caption, Palette.GrayText);
  end;

  { Draw and Free }
  FBGRA.Draw(ADest, 0, 0);
  FBGRA.Free;

  // The text selection
  if (csfHasFocus in AState) {and using the keyboard} then
    DrawFocusRect(ADest, Point(lSquareHeight + 4, 0),
      Size(ASize.cx - lSquareHeight - 4, ASize.cy));
end;

procedure TCDWin7.DrawRadioButtonCircle(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDControlStateEx);
begin
  inherited DrawRadioButtonCircle(ADest, ADestPos, ASize, AState, AStateEx);
end;

procedure TCDWin7.DrawRadioButton(ADest: TCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDControlStateEx);
var
  lCircleHeight: integer;
  FBGRA: TBGRABitmap;
  number: integer;
begin
  FBGRA := TBGRABitmap.Create(ASize.cx, ASize.cy, AStateEx.ParentRGBColor);
  AssignFontToBGRA(AStateEx.Font, FBGRA);

  lCircleHeight := win7.RadioButton.SliceScalingArray[0].BitmapHeight;

  number := 0;

  if csfOn in AState then
    number := 4;

  if csfEnabled in AState then
  begin
    if csfMouseOver in AState then
    begin
      Inc(number, +1);
      if csfSunken in AState then
        Inc(number, +1);
    end
    else
    if csfSunken in AState then
      Inc(number, +2);
  end
  else
    Inc(number, +3);

  win7.radiobutton.Draw(number, FBGRA, 0, 0, lCircleHeight, lCircleHeight, win7.debug);

  if csfEnabled in AState then
    FBGRA.TextOut(lCircleHeight + 5, 0, AStateEx.Caption, Palette.WindowText)
  else
  begin
    FBGRA.TextOut(lCircleHeight + 6, 1, AStateEx.Caption, Palette.Window);
    FBGRA.TextOut(lCircleHeight + 5, 0, AStateEx.Caption, Palette.GrayText);
  end;

  { Draw and Free }
  FBGRA.Draw(ADest, 0, 0);
  FBGRA.Free;

  // The text selection
  if (csfHasFocus in AState) {and using the keyboard} then
    DrawFocusRect(ADest, Point(lCircleHeight + 3, 0),
      Size(ASize.cx - lCircleHeight - 3, ASize.cy));
end;

procedure TCDWin7.DrawStaticText(ADest: TCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDControlStateEx);
var
  FBGRA: TBGRABitmap;
begin
  FBGRA := TBGRABitmap.Create(ASize.cx, ASize.cy, AStateEx.ParentRGBColor);
  AssignFontToBGRA(AStateEx.Font, FBGRA);

  if csfEnabled in AState then
    FBGRA.TextOut(0, 0, AStateEx.Caption, Palette.WindowText)
  else
  begin
    FBGRA.TextOut(1, 1, AStateEx.Caption, Palette.Window);
    FBGRA.TextOut(0, 0, AStateEx.Caption, Palette.GrayText);
  end;

  { Draw and Free }
  FBGRA.Draw(ADest, 0, 0);
  FBGRA.Free;
end;

procedure TCDWin7.DrawProgressBar(ADest: TCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDProgressBarStateEx);
var
  lProgWidth: integer;
  lPoint: TPoint;
  FBGRA: TBGRABitmap;
begin
  FBGRA := TBGRABitmap.Create(ASize.cx, ASize.cy, AStateEx.ParentRGBColor);
  AssignFontToBGRA(AStateEx.Font, FBGRA);

  lPoint := Point(0, 0);

  if (csfHorizontal in AState) or (csfRightToLeft in AState) then
  begin
    win7.ProgressBarHorizontalBackground.Draw(0, FBGRA, 0, 0, ASize.cx,
      ASize.cy, win7.debug);

    lProgWidth := Round(ASize.cx * AStateEx.PercentPosition);

    if csfRightToLeft in AState then
      lPoint.x := ASize.cx - lProgWidth;

    win7.ProgressBarHorizontalFill.Draw(0, FBGRA, lPoint.x, lPoint.y, lProgWidth,
      ASize.cy, win7.Debug);
  end;

  if (csfVertical in AState) or (csfTopDown in AState) then
  begin
    win7.ProgressBarVerticalBackground.Draw(0, FBGRA, 0, 0, ASize.cx,
      ASize.cy, win7.debug);

    lProgWidth := Round(ASize.cy * AStateEx.PercentPosition);

    if csfTopDown in AState then
      lPoint.y := ASize.cy - lProgWidth;

    win7.ProgressBarVerticalFill.Draw(0, FBGRA, lPoint.x, lPoint.y,
      ASize.cx, lProgWidth, win7.Debug);
  end;

  { Draw and Free }
  FBGRA.Draw(ADest, 0, 0);
  FBGRA.Free;
end;

procedure TCDWin7.DrawButtonWithArrow(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState);
var
  number: integer;
  { bgrabitmap }
  FBGRA: TBGRABitmap;
begin
  FBGRA := TBGRABitmap.Create(ASize.cx, ASize.cy);

  //if csfEnabled in AState then
  //begin
  number := 0;
  if csfHasFocus in AState then
    number := 4;
  if csfMouseOver in AState then
    number := 1;
  if csfSunken in AState then
    number := 2;
  //end
  //else
  //  number := 3;

  win7.button.Draw(number, FBGRA, 0, 0, ASize.cx, ASize.cy, win7.debug);

  { Draw and Free }
  FBGRA.Draw(ADest, ADestPos.X, ADestPos.Y, False);
  FBGRA.Free;

  // Now the arrow
  DrawArrow(ADest, Point(ADestPos.X + ASize.CY div 4, ADestPos.Y + ASize.CY * 3 div 8),
    AState, ASize.CY div 2);
end;

procedure TCDWin7.DrawScrollBar(ADest: TCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDPositionedCStateEx);
var
  lPos: TPoint;
  lSize: TSize;
  lArrowState: TCDControlState;
  { number of bitmap used }
  number: integer;
  { bgrabitmap }
  FBGRA: TBGRABitmap;
  FBGRAOut: TBGRABitmap;
begin
  FBGRAOut := TBGRABitmap.Create(ASize.cx, ASize.cy, AStateEx.ParentRGBColor);
  AssignFontToBGRA(AStateEx.Font, FBGRAOut);

  // START BACKGROUND //
  FBGRA := TBGRABitmap.Create(ASize.cx, ASize.cy, AStateEx.ParentRGBColor);
  AssignFontToBGRA(AStateEx.Font, FBGRA);

  if csfEnabled in AState then
  begin
    number := 0;
    //if csfHasFocus in AState then
    //  number := 4;
    //if csfMouseOver in AState then
    //  number := 1;
    if csfSunken in AState then
      number := 2;
  end
  else
    number := 3;

  win7.button.Draw(number, FBGRA, 0, 0, ASize.cx, ASize.cy, win7.debug);

  { Draw and Free }
  FBGRAOut.BlendImage(0, 0, FBGRA, boLinearBlend);
  FBGRA.Free;
  // END BACKGROUND //

  // START L/T BUTTON //
  lPos := Point(0, 0);

  if csfHorizontal in AState then
    lSize := Size(GetMeasures(TCDSCROLLBAR_BUTTON_WIDTH), ASize.CY)
  else
    lSize := Size(ASize.CX, GetMeasures(TCDSCROLLBAR_BUTTON_WIDTH));

  if csfEnabled in AState then
  begin
    number := 0;
    //if csfHasFocus in AState then
    //  number := 4;
    //if csfMouseOver in AState then
    //  number := 1;
    if csfSunken in AState then
      number := 2;
  end
  else
    number := 3;

  if csfLeftArrow in AState then
  begin
    lArrowState := [csfSunken];
    number := 2;
  end
  else
    lArrowState := [];

  FBGRA := TBGRABitmap.Create(lSize.cx, lSize.cy);
  AssignFontToBGRA(AStateEx.Font, FBGRA);

  win7.button.Draw(number, FBGRA, 0, 0, lSize.cx, lSize.cy, win7.debug);

  { Draw and Free }
  FBGRAOut.BlendImage(lPos.x, lPos.y, FBGRA, boLinearBlend);
  FBGRA.Free;

  if csfHorizontal in AState then
    DrawArrow(FBGRAOut.Canvas, Point(lPos.X + 5, lPos.Y + 5),
      [csfLeftArrow] + lArrowState)
  else
    DrawArrow(FBGRAOut.Canvas, Point(lPos.X + 5, lPos.Y + 5),
      [csfUpArrow] + lArrowState);
  // END L/T BUTTON //

  // START R/B BUTTON //
  if csfHorizontal in AState then
    lPos.X := lPos.X + ASize.CX - GetMeasures(TCDSCROLLBAR_BUTTON_WIDTH)
  else
    lPos.Y := lPos.Y + ASize.CY - GetMeasures(TCDSCROLLBAR_BUTTON_WIDTH);

  if csfEnabled in AState then
  begin
    number := 0;
    //if csfHasFocus in AState then
    //  number := 4;
    //if csfMouseOver in AState then
    //  number := 1;
    if csfSunken in AState then
      number := 2;
  end
  else
    number := 3;

  if csfRightArrow in AState then
  begin
    lArrowState := [csfSunken];
    number := 2;
  end
  else
    lArrowState := [];

  FBGRA := TBGRABitmap.Create(lSize.cx, lSize.cy);
  AssignFontToBGRA(AStateEx.Font, FBGRA);

  win7.button.Draw(number, FBGRA, 0, 0, lSize.cx, lSize.cy, win7.debug);

  { Draw and Free }
  FBGRAOut.BlendImage(lPos.x, lPos.y, FBGRA, boLinearBlend);
  FBGRA.Free;

  if csfHorizontal in AState then
    DrawArrow(FBGRAOut.Canvas, Point(lPos.X + 5, lPos.Y + 5),
      [csfRightArrow] + lArrowState)
  else
    DrawArrow(FBGRAOut.Canvas, Point(lPos.X + 5, lPos.Y + 5),
      [csfDownArrow] + lArrowState);
  // END R/B BUTTON //

  // START SLIDER //
  lPos := Point(0, 0);
  if csfHorizontal in AState then
  begin
    if AStateEx.FloatPageSize > 0 then
      lSize.cx := Round(AStateEx.FloatPageSize *
        (ASize.cx - GetMeasures(TCDSCROLLBAR_BUTTON_WIDTH) * 2));
    if lSize.cx < 5 then
      lSize.cx := 5;

    lPos.X := Round(GetMeasures(TCDSCROLLBAR_BUTTON_WIDTH) +
      AStateEx.FloatPos * (ASize.cx - GetMeasures(TCDSCROLLBAR_BUTTON_WIDTH) *
      2 - lSize.cx));
  end
  else
  begin
    if AStateEx.FloatPageSize > 0 then
      lSize.cy := Round(AStateEx.FloatPageSize *
        (ASize.cy - GetMeasures(TCDSCROLLBAR_BUTTON_WIDTH) * 2));

    if lSize.cy < 5 then
      lSize.cy := 5;

    lPos.Y := Round(GetMeasures(TCDSCROLLBAR_BUTTON_WIDTH) +
      AStateEx.FloatPos * (ASize.cy - GetMeasures(TCDSCROLLBAR_BUTTON_WIDTH) *
      2 - lSize.cy));
  end;

  if csfEnabled in AState then
  begin
    number := 0;
    //if csfHasFocus in AState then
    //  number := 4;
    //if csfMouseOver in AState then
    //  number := 1;
    if csfSunken in AState then
      number := 2;
  end
  else
    number := 3;

  FBGRA := TBGRABitmap.Create(lSize.cx, lSize.cy);
  AssignFontToBGRA(AStateEx.Font, FBGRA);

  win7.button.Draw(number, FBGRA, 0, 0, lSize.cx, lSize.cy, win7.debug);

  { Draw and Free }
  FBGRAOut.BlendImage(lPos.x, lPos.y, FBGRA, boLinearBlend);
  FBGRA.Free;
  // END SLIDER //

  FBGRAOut.Draw(ADest, 0, 0);
  FBGRAOut.Free;
end;

procedure TCDWin7.DrawSlider(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState);
var
  { number of bitmap used }
  number: integer;
  { bgrabitmap }
  FBGRA: TBGRABitmap;
begin
  if csfHorizontal in AState then
    FBGRA := TBGRABitmap.Create(ASize.cx, ASize.cy)
  else
    FBGRA := TBGRABitmap.Create(ASize.cy, ASize.cx);

  if csfEnabled in AState then
  begin
    number := 0;
    if csfHasFocus in AState then
      number := 4;
    //if csfMouseOver in AState then
    //  number := 1;
    //if csfSunken in AState then
    //  number := 2;
  end
  else
    number := 3;

  win7.button.Draw(number, FBGRA, 0, 0, FBGRA.Width, FBGRA.Height, win7.debug);

  { Draw and Free }
  if csfHorizontal in AState then
    FBGRA.Draw(TCanvas(ADest), ADestPos.x, ADestPos.y, False)
  else
    FBGRA.Draw(TCanvas(ADest), ADestPos.y, ADestPos.x, False);
  FBGRA.Free;
end;

procedure AssignFontToBGRA(Source: TFont; Dest: TBGRABitmap);
begin
  Dest.FontAntialias := True;

  Dest.FontName := Source.Name;
  Dest.FontStyle := Source.Style;
  Dest.FontOrientation := Source.Orientation;

  case Source.Quality of
    fqNonAntialiased: Dest.FontQuality := fqSystem;
    fqAntialiased: Dest.FontQuality := fqFineAntialiasing;
    fqProof: Dest.FontQuality := fqFineClearTypeRGB;
    fqDefault, fqDraft, fqCleartype, fqCleartypeNatural: Dest.FontQuality :=
        fqSystemClearType;
  end;

  Dest.FontHeight := -Source.Height;
end;

function ExtractZipPNGtoBGRA(ZipName, FileName: string): TBGRABitmap;
var
  z: TBGRAZip;
begin
  z := TBGRAZip.Create;
  Result := z.Extract(ZipName, FileName);
  z.Free;
end;

function ExtractZipFiletoMemoryStream(ZipName, FileName: string): TMemoryStream;
var
  z: TMemoryStreamZip;
begin
  z := TMemoryStreamZip.Create;
  Result := z.Extract(ZipName, FileName);
  z.Free;
end;

initialization
  win7 := TBitmapTheme.Create('aero.zip');
  RegisterDrawer(TCDWin7.Create, dsWindows7);
  RegisterDrawer(TCDWin7Extra.Create, dsExtra1);

finalization
  win7.Free;

end.
