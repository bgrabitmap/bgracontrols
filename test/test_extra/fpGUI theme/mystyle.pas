unit mystyle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_main, fpg_base,
  BGRABitmap, BGRABitmapTypes, BGRASliceScaling, Zipper, FPReadPNG;

const
  WIN10_3DDKSHADOW_COLOR = $00696969;
  WIN10_3DLIGHT_COLOR = $00E3E3E3;
  WIN10_ACTIVEBORDER_COLOR = $00B4B4B4;
  WIN10_ACTIVECAPTION_COLOR = $00D1B499;
  WIN10_APPWORKSPACE_COLOR = $00ABABAB;
  WIN10_BACKGROUND_COLOR = clBlack;
  WIN10_BTNFACE_COLOR = $00F0F0F0;
  WIN10_BTNHIGHLIGHT_COLOR = clWhite;
  WIN10_BTNSHADOW_COLOR = $00A0A0A0;
  WIN10_BTNTEXT_COLOR = clBlack;
  WIN10_CAPTIONTEXT_COLOR = clBlack;
  WIN10_FORM_COLOR = $00F0F0F0;
  WIN10_GRADIENTACTIVECAPTION_COLOR = $00EAD1B9;
  WIN10_GRADIENTINACTIVECAPTION_COLOR = $00F2E4D7;
  WIN10_GRAYTEXT_COLOR = $006D6D6D;
  WIN10_HIGHLIGHTTEXT_COLOR = $00FF9933;
  WIN10_HIGHLIGHT_COLOR = $00FF9933;
  WIN10_HOTLIGHT_COLOR = $00CC6600;
  WIN10_INACTIVEBORDER_COLOR = $00FCF7F4;
  WIN10_INACTIVECAPTIONTEXT_COLOR = $00544E43;
  WIN10_INACTIVECAPTION_COLOR = $00DBCDBF;
  WIN10_INFOBK_COLOR = $00E1FFFF;
  WIN10_INFOTEXT_COLOR = clBlack;
  WIN10_MENUBAR_COLOR = $00F0F0F0;
  WIN10_MENUHIGHLIGHT_COLOR = $00FF9933;
  WIN10_MENUTEXT_COLOR = clBlack;
  WIN10_MENU_COLOR = $00F0F0F0;
  WIN10_SCROLLBAR_COLOR = $00C8C8C8;
  WIN10_WINDOWFRAME_COLOR = $00646464;
  WIN10_WINDOWTEXT_COLOR = clBlack;
  WIN10_WINDOW_COLOR = clWhite;

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

  { TMyStyle }

  TMyStyle = class(TfpgStyle)
  private
    FTheme: TBitmapTheme;
  public
    constructor Create; override;
    destructor Destroy; override;
    { Button }
    procedure DrawButtonFace(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; AFlags: TfpgButtonFlags); override;
  end;

  function ExtractZipPNGtoBGRA(ZipName, FileName: string): TBGRABitmap;
  function ExtractZipFiletoMemoryStream(ZipName, FileName: string): TMemoryStream;

implementation

uses
  fpg_stylemanager, LazFileUtils, FileUtil, INIFiles;

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

{ TMyStyle }

constructor TMyStyle.Create;
begin
  inherited Create;
  FTheme := TBitmapTheme.Create('aerow8.zip');
  fpgSetNamedColor(clWindowBackground, WIN10_FORM_COLOR);
end;

destructor TMyStyle.Destroy;
begin
  FTheme.Free;
  inherited Destroy;
end;

procedure TMyStyle.DrawButtonFace(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord;
  AFlags: TfpgButtonFlags);
var
  bmp: TBGRABitmap;
begin
  bmp := TBGRABitmap.Create(w, h, WIN10_FORM_COLOR);
  FTheme.Button.Draw(0, bmp, 0, 0, bmp.Width, bmp.Height, FTheme.Debug);
  bmp.Draw(ACanvas, Rect(x, y, x+w, y+h));
  bmp.Free;
end;

initialization
  fpgStyleManager.RegisterClass('Demo Style', TMyStyle);

end.

