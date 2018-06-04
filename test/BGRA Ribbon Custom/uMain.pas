unit uMain;

{$mode objfpc}{$H+}

interface

uses
  BGRABitmap, BGRABitmapTypes, Classes, SysUtils, FileUtil, Forms, Controls,
  Graphics, StdCtrls, ExtCtrls, Menus, Dialogs, ActnList, BGRAVirtualScreen,
  BCButton, BCPanel, BGRAImageList, BGRAResizeSpeedButton, LCLType, LCLIntf,
  {$ifdef Windows}
  Windows,
  {$endif}
  XMLConf, SynMemo, SynEdit;

type
  TBorderPos = (bpTopLeft, bpTopRight, bpBottomLeft,
                bpBottomRight, bpTop, bpBottom,bpLeft,bpRight,bpnone);

  { TfrmMain }

  TfrmMain = class(TForm)
    BCButton1: TBCButton;
    BCButton13: TBCButton;
    BCButton14: TBCButton;
    BCButton15: TBCButton;
    BCButton6: TBCButton;
    BCButton8: TBCButton;
    BCPanel1: TBCPanel;
    BCPanel2: TBCPanel;
    btnClipboard: TBCButton;
    btnClose: TBCButton;
    btnColors: TBCButton;
    btnCopy: TBCButton;
    btnCut: TBCButton;
    btnExit: TBCButton;
    btnFileMenu: TBCButton;
    btnFileMenuClose: TBGRAResizeSpeedButton;
    btnFileNew: TBCButton;
    btnFileOpen: TBCButton;
    btnFileSave: TBCButton;
    btnHelp: TBCButton;
    btnHide: TBCButton;
    btnMaximize: TBCButton;
    btnMinimize: TBCButton;
    btnNew: TBCButton;
    btnOpen: TBCButton;
    btnPaste: TBCButton;
    btnPreview: TBCButton;
    btnPrint: TBCButton;
    btnSave: TBCButton;
    btnTabEdit: TBCButton;
    btnTabHome: TBCButton;
    btnTabSettings: TBCButton;
    btnTool1: TBCButton;
    btnTool2: TBCButton;
    btnTool3: TBCButton;
    FileMenu: TBCPanel;
    ImageList16: TBGRAImageList;
    ImageList16White: TBGRAImageList;
    ImageList32: TBGRAImageList;
    ImageListColors: TBGRAImageList;
    imgResize: TImage;
    Label2: TLabel;
    lblTitle: TBCButton;
    mnuColorChange: TMenuItem;
    mnuColorGreen: TMenuItem;
    mnuColorLiteBlue: TMenuItem;
    mnuColorOrange: TMenuItem;
    mnuColorPetrol: TMenuItem;
    mnuColorPurple: TMenuItem;
    mnuColorRed: TMenuItem;
    pnlEdit2: TBCPanel;
    pnlEditClipboard: TBCPanel;
    pnlHomeCommand: TBCPanel;
    pnlHomeExit: TBCPanel;
    pnlSettings2: TBCPanel;
    pnlSettingsUI: TBCPanel;
    popColors: TPopupMenu;
    Splitter1: TSplitter;
    SynMemo1: TSynMemo;
    vsBody: TBGRAVirtualScreen;
    vsBorder: TBGRAVirtualScreen;
    vsForm: TBGRAVirtualScreen;
    vsTab: TBGRAVirtualScreen;
    vsTitle: TBGRAVirtualScreen;
    XMLConfigMain: TXMLConfig;
    procedure btnTool1Click(Sender: TObject);
    procedure btnTool2Click(Sender: TObject);
    procedure btnTool3Click(Sender: TObject);
    procedure SynMemo1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnHideClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnColorsMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure btnCutClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure btnPreviewClick(Sender: TObject);
    procedure btnClipboardClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnMaximizeClick(Sender: TObject);
    procedure btnMinimizeClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnOpenAfterRenderBCButton(Sender: TObject;
      const ABGRA: TBGRABitmap; AState: TBCButtonState; ARect: TRect);
    procedure btnFileMenuCloseMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure btnFileMenuMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure btnHelpMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure btnOpenClick(Sender: TObject);
    procedure btnTabHomeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure imgResizeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure imgResizeMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure Memo1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure mnuColorChangeClick(Sender: TObject);
    procedure vsBodyRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure vsBorderResize(Sender: TObject);
    procedure vsFormRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure vsTabMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure vsTabRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure vsTitleDblClick(Sender: TObject);
    procedure vsTitleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure vsTitleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure vsTitleRedraw(Sender: TObject; Bitmap: TBGRABitmap);
  private
    { private declarations }
    FormStyleModifyDone: boolean;
    //    procedure CreateParams(VAR Params: TCreateParams); override;
    //    procedure WMNCHitTest(VAR Msg: TWMNcHitTest); message WM_NCHITTEST;
    procedure ColorChange(Sender: TObject);
    procedure MoveAnimation(moveobj: TComponent; leftfrom: integer;
      leftto: integer; topfrom: integer; topto: integer);
  public
    { public declarations }
    isFocused: Boolean;
    procedure AppActivate(Sender: TObject);
    procedure AppDeactivate(Sender: TObject);
    constructor Create(TheOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;
  TabColor: string;
  MausPos: TPoint;
  xx, yy: integer;
  d: boolean = False;

  PtSize: TPoint;
  DoResize: boolean;

implementation

{$R *.lfm}

uses
  uRibbon; {, uResize;}

{ TfrmMain }

{procedure TfrmMain.CreateParams(VAR Params: TCreateParams);
begin
  Inherited CreateParams(Params);
  WITH Params DO
    Style := (Style OR WS_POPUP) AND (NOT WS_DLGFRAME);
    {or... Style := Style + WS_POPUP - WS_DLGFRAME; which is the
     equivalent to the above statement}
 end;

procedure TfrmMain.WMNCHitTest(var msg: TWMNCHitTest);
begin
  inherited;
  if  (msg.Result = htClient) then
    msg.Result := htCaption;
end;
}

{ Dropshadow Function places a shadow behind a Form with Borderstyle=bsNone; }
{http://www.delphipraxis.net/32245-dropshadow.html
Am bestem im Konstruktor aufzurufen
****Windows OS only!****

function DropShadow(const Handle: THandle): boolean;

  function IsXP: boolean;
  begin
    Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and
      ((Win32MajorVersion > 5) or ((Win32MajorVersion = 5) and
      (Win32MinorVersion >= 1)));
  end;

const
  SPI_SETDROPSHADOW = $1025;
  CS_DROPSHADOW = $00020000;
begin
  Result := IsXP and SystemParametersInfo(SPI_SETDROPSHADOW, 0, Pointer(True), 0) and
    (SetClassLong(Handle, GCL_STYLE, GetClassLong(Handle, GCL_STYLE) or
    CS_DROPSHADOW) <> 0);
end;
}

{ For whatever reason the following doesnt work? Form is still painted w/o shadow!
Function Dropshadow() works though!

procedure TfrmMain.CreateParams(var Params: TCreateParams);
const
  SPI_SETDROPSHADOW = $1025;
  CS_DROPSHADOW = $00020000;
begin
  inherited;
  if CheckWin32Version(5, 1) then
  begin
    SystemParametersInfo(SPI_SETDROPSHADOW, 0, Pointer(True), 0);
    Params.WindowClass.style := Params.WindowClass.style or CS_DROPSHADOW;
  end;
end;
}

constructor TfrmMain.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Application.OnDeactivate:=@AppDeactivate;
  Application.OnActivate:=@AppActivate;
end;

procedure TfrmMain.AppDeactivate(Sender: TObject);
begin
  isFocused := FALSE;
  vsBorder.Color:=$00838383;
  lblTitle.GlobalOpacity:=125;
  btnMinimize.GlobalOpacity:=125;
  btnMaximize.GlobalOpacity:=125;
  btnClose.GlobalOpacity:=125;
  btnTool1.GlobalOpacity:=125;
  btnTool2.GlobalOpacity:=125;
  btnTool3.GlobalOpacity:=125;
end;

procedure TfrmMain.AppActivate(Sender: TObject);
begin
  isFocused := TRUE;
  vsBorder.Color:=uRibbon.MainColor;
  lblTitle.GlobalOpacity:=255;
  btnMinimize.GlobalOpacity:=255;
  btnMaximize.GlobalOpacity:=255;
  btnClose.GlobalOpacity:=255;
  btnTool1.GlobalOpacity:=255;
  btnTool2.GlobalOpacity:=255;
  btnTool3.GlobalOpacity:=255;
end;


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  { Features presently implemented as of 06.04.2016
    - Moveable and Sizeable Form without Border and Caption
    - Extended drag area
    - Form has shadow (CS_DROPSHADOW)
    - Max- / Minimizing of Form
    - Doubleclick on Title maximizes / restores Form
    - RightClick on Title shows Systemmenu
    - Form indicates whether it has focus or not
    - Quick Access Toolbuttons
    - XML storage of some properties (Form size, position and colors)
    - Custom painting of images and caption on Buttons (Windows ok, but not sure it works on Linux?)
    - Slide-in and slide-out effect of Menu (Windows ok, needs finetuning on Linux, Z-Order?)
  }
  { TODO : Presently cant find a working way to use the sizeable system border }
  { DONE : Rightclick on vsTitle to show SystemMenu }
  { DONE : Replace Icon and Caption by customizable Quick Access Toolbuttons on vsTitle }
  { DONE : Line break in custom drawn text on Buttons }
  { TODO : Radio Button, Drop Down Example on vsBody }
  { TODO : Implementation of File Menu }
  { TODO : Offer classical menus as an option to user }
  { TODO : All color definitions to be shifted to uRibbon.pas }
  { TODO : Linux - Slide effect of Menu needs finetuning }
  { TODO : Linux - Custom painting on Buttons needs to be verified }

  { Create the XML config component to store some settings }
  XMLConfigMain := TXMLConfig.Create(nil);
  XMLConfigMain.Filename := ChangeFileExt(Application.ExeName, '.xml');

  { Set BorderStyle from bsSingle to bsNone to get a Form w/o Title Bar }
  BorderStyle := bsNone;

  { Place a shadow behind the Form }
  {$ifdef Windows}
  //  DropShadow(Handle); // Better to use SetClasslong() as follows (Windows OS only):
  {Handle = all Forms, frmMain.Handle = only frmMain Form
  SetClassLong(Handle, GCL_STYLE, GetClassLong(Handle, GCL_STYLE) or CS_DROPSHADOW);
  }
  SetClassLong(frmMain.Handle, GCL_STYLE, GetClassLong(frmMain.Handle, GCL_STYLE) or CS_DROPSHADOW);
  { Following can be used on Windows < 8
  // Soner: Füge die nächsten 3 Zeilen hinzu ------------------------------------
  // vergiß uses LCLIntf; nicht.
    SetWindowLong(Handle,GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or WS_THICKFRAME);
    ClientHeight:=Height;
    Refresh;
  }
  {$else}
  // Something to be done for OS <> Windows?
  {$endif}

  { Set Doublebuffered property to reduce flicker. }
  Doublebuffered := True;
  //  vsBody.DoubleBuffered:=True;
  //  vsTab.DoubleBuffered:=True;
  //  vsForm.DoubleBuffered:=True;
  //  vsTitle.DoubleBuffered:=True;
  //  vsBorder.DoubleBuffered:=True;

  { Read and set the XML config values }
  Position := poDesigned;
  Top := StrToInt(XMLConfigMain.GetValue('FormTop', '250'));
  Left := StrToInt(XMLConfigMain.GetValue('FormLeft', '550'));
  Height := StrToInt(XMLConfigMain.GetValue('FormHeight', '350'));
  Width := StrToInt(XMLConfigMain.GetValue('FormWidth', '600'));

  { Set Min Height and Min Width of Form to prevent vanishing of Form }
  Constraints.MinHeight := 200;
  Constraints.MinWidth := 250;

  { Set Colors }

  { Colors of Body Buttons are the same for all styles }
  uRibbon.BtnFntColor := $00696969;      // Button Font Color
  uRibbon.BtnNormalColor := $00F1F1F1;   // Button Normal
  uRibbon.BtnHoverColor := $00cdcdcd;    // Button Hover
  uRibbon.BtnClickedColor := $00b0b0b0;  // Button Clicked

  TabColor := XMLConfigMain.GetValue('Color', 'blue');

  { Call ColorChange() to set colors }
  ColorChange(self);

  StyleRibbonBody(pnlHomeCommand);
  StyleRibbonBody(pnlHomeExit);
  StyleRibbonBody(pnlSettingsUI);
  StyleRibbonBody(pnlEditClipboard);

  { to be done, let the user decide which UI he likes to use
  // Set UI to Ribbon, otherwise use Classical Menus
  if StrToInt(XMLConfigMain.GetValue('UI', ''))=1 then
   SetUserInterface(false)
  else
   SetUserInterface(true);
  }

  { Showing Form.Caption in Title bar }
  lblTitle.Caption := frmMain.Caption;

  btnOpen.ShowCaption := False;
  btnNew.ShowCaption := False;
  btnExit.ShowCaption := False;
  btnClipboard.ShowCaption := False;
  btnColors.ShowCaption := False;
  btnOpen.Images := nil;
  btnNew.Images := nil;
  btnExit.Images := nil;
  btnClipboard.Images := nil;
  btnColors.Images := nil;
end;

procedure TfrmMain.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMaximized then
  begin
    with Screen.WorkAreaRect do
      SetBounds(Left, Top, Right - Left, Bottom - Top);
  end;
end;

procedure TfrmMain.imgResizeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  PtSize.y := Mouse.CursorPos.y - Top - Height;
  PtSize.x := Mouse.CursorPos.x - Left - Width;
end;

procedure TfrmMain.imgResizeMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if ssLeft in shift then
  begin
    Width := Mouse.CursorPos.x - Left - PtSize.x;
    Height := Mouse.CursorPos.y - Top - PtSize.y;
  end;
end;

procedure TfrmMain.Memo1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  {Hide Menu if not fixed}
  if vsBody.Tag = 1 then
    vsBody.Hide;
end;

procedure TfrmMain.SynMemo1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  {Hide Menu if not fixed}
  if vsBody.Tag = 1 then
    vsBody.Hide;
end;

procedure TfrmMain.ColorChange(Sender: TObject);
begin
  case TabColor of
    'blue':
    begin
      //      TabColor := 'blue';
      uRibbon.MainColor := $009A572A;
      uRibbon.TabFntColor := $009A572A;
      uRibbon.TabNormalColor := $009A572A;
      uRibbon.TabHoverColor := $00b66d3e;
      uRibbon.TabClickedColor := $00F1F1F1;
      uRibbon.ToolNormalColor := $009A572A;
      uRibbon.ToolHoverColor := $00b66d3e;
      uRibbon.ToolClickedColor := $00784012;
    end;
    'green':
    begin
      //      TabColor := 'green';
      uRibbon.MainColor := $00477422;
      uRibbon.TabFntColor := $00477422;
      uRibbon.TabNormalColor := $00477422;
      uRibbon.TabHoverColor := $00679443;
      uRibbon.TabClickedColor := $00F1F1F1;
      uRibbon.ToolNormalColor := $00477422;
      uRibbon.ToolHoverColor := $00679443;
      uRibbon.ToolClickedColor := $00395d1b;
    end;
    'red':
    begin
      //      TabColor := 'red';
      uRibbon.MainColor := $003a37a4;
      uRibbon.TabFntColor := $003a37a4;
      uRibbon.TabNormalColor := $003a37a4;
      uRibbon.TabHoverColor := $005956c7;
      uRibbon.TabClickedColor := $00F1F1F1;
      uRibbon.ToolNormalColor := $003a37a4;
      uRibbon.ToolHoverColor := $005956c7;
      uRibbon.ToolClickedColor := $002e2c83;
    end;
    'orange':
    begin
      //      TabColor := 'orange';
      uRibbon.MainColor := $002a47b7;
      uRibbon.TabFntColor := $002a47b7;
      uRibbon.TabNormalColor := $002a47b7;
      uRibbon.TabHoverColor := $003959dc;
      uRibbon.TabClickedColor := $00F1F1F1;
      uRibbon.ToolNormalColor := $002a47b7;
      uRibbon.ToolHoverColor := $003959dc;
      uRibbon.ToolClickedColor := $00223992;
    end;
    'liteblue':
    begin
      //      TabColor := 'lightblue';
      uRibbon.MainColor := $00c77301;
      uRibbon.TabFntColor := $00c77301;
      uRibbon.TabNormalColor := $00c77301;
      uRibbon.TabHoverColor := $00d48a2a;
      uRibbon.TabClickedColor := $00F1F1F1;
      uRibbon.ToolNormalColor := $00c77301;
      uRibbon.ToolHoverColor := $00d48a2a;
      uRibbon.ToolClickedColor := $009f5c01;
    end;
    'petrol':
    begin
      //      TabColor := 'petrol';
      uRibbon.MainColor := $00728200;
      uRibbon.TabFntColor := $00728200;
      uRibbon.TabNormalColor := $00728200;
      uRibbon.TabHoverColor := $00839900;
      uRibbon.TabClickedColor := $00F1F1F1;
      uRibbon.ToolNormalColor := $00728200;
      uRibbon.ToolHoverColor := $00839900;
      uRibbon.ToolClickedColor := $005f6b1a;
    end;
    'purple':
    begin
      //      TabColor := 'purple';
      uRibbon.MainColor := $007b3980;
      uRibbon.TabFntColor := $007b3980;
      uRibbon.TabNormalColor := $007b3980;
      uRibbon.TabHoverColor := $009e56a3;
      uRibbon.TabClickedColor := $00F1F1F1;
      uRibbon.ToolNormalColor := $007b3980;
      uRibbon.ToolHoverColor := $009e56a3;
      uRibbon.ToolClickedColor := $00622e66;
    end;
  end;
  RibbonMenu(btnFileMenu);

  RibbonTab(btnTabHome);
  RibbonTab(btnTabEdit);
  RibbonTab(btnTabSettings);

  RibbonTool(btnHelp);

  {Setting colors of Title Bar, Title Buttons and Tool Buttons}
  vsTitle.Color := MainColor;
  btnClose.StateNormal.Background.Color := uRibbon.ToolNormalColor;
  btnMaximize.StateNormal.Background.Color := uRibbon.ToolNormalColor;
  btnMaximize.StateHover.Background.Color := uRibbon.ToolHoverColor;
  btnMaximize.StateClicked.Background.Color := uRibbon.ToolClickedColor;
  btnMinimize.StateNormal.Background.Color := uRibbon.ToolNormalColor;
  btnMinimize.StateHover.Background.Color := uRibbon.ToolHoverColor;
  btnMinimize.StateClicked.Background.Color := uRibbon.ToolClickedColor;
  btnTool1.StateNormal.Background.Color := uRibbon.ToolNormalColor;
  btnTool1.StateHover.Background.Color := uRibbon.ToolHoverColor;
  btnTool1.StateClicked.Background.Color := uRibbon.ToolClickedColor;
  btnTool2.StateNormal.Background.Color := uRibbon.ToolNormalColor;
  btnTool2.StateHover.Background.Color := uRibbon.ToolHoverColor;
  btnTool2.StateClicked.Background.Color := uRibbon.ToolClickedColor;
  btnTool3.StateNormal.Background.Color := uRibbon.ToolNormalColor;
  btnTool3.StateHover.Background.Color := uRibbon.ToolHoverColor;
  btnTool3.StateClicked.Background.Color := uRibbon.ToolClickedColor;

  vsBorder.Color:=MainColor;

  {Redraw all Objects}
  vsTab.RedrawBitmap;
  vsForm.RedrawBitmap;
  vsBody.RedrawBitmap;
  btnFileMenu.Repaint;

{ TODO : Applying color settings to file menu
//    FileMenu.Background.Color:=uRibbon.MainColor;
//    Splitter1.Color:=uRibbon.MainColor;
}

end;

procedure TfrmMain.mnuColorChangeClick(Sender: TObject);
var
  mnu: TMenuItem;
begin
  if (Sender is TMenuItem) then
  begin
    mnu := (Sender as TMenuItem);
    case mnu.Name of
      'mnuColorChange': TabColor := 'blue';
      'mnuColorGreen': TabColor := 'green';
      'mnuColorRed': TabColor := 'red';
      'mnuColorOrange': TabColor := 'orange';
      'mnuColorLiteBlue': TabColor := 'liteblue';
      'mnuColorPetrol': TabColor := 'petrol';
      'mnuColorPurple': TabColor := 'purple';
    end;
    ColorChange(self);
  end;

end;

{ Animation of objects (c) Lazplanet
http://lazplanet.blogspot.de/2013/05/animate-move-of-object.html
Presently not used}
procedure TfrmMain.MoveAnimation(moveobj: TComponent; leftfrom: integer;
  leftto: integer; topfrom: integer; topto: integer);
var
  i: integer;
  step: integer = 5;
  moveareax, moveareay: integer;
begin
  i := 1;
  moveareax := leftto - leftfrom;
  moveareay := topto - topfrom;
  while i <= 100 do
  begin
    TButton(moveobj).Left := round(leftfrom + (moveareax * i / 100));
    TButton(moveobj).Top := round(topfrom + (moveareay * i / 100));
    Repaint;
    Sleep(1);
    // we exit the while...do loop when our work is done
    if i >= 100 then
      Exit;
    Inc(i, step);
    if (100 - i) < step then
      i := 100;
  end;
end;

procedure TfrmMain.btnOpenAfterRenderBCButton(Sender: TObject;
  const ABGRA: TBGRABitmap; AState: TBCButtonState; ARect: TRect);
var
  myText: string;
  myRect: TRect;
  tw: integer;
  c: TBGRAPixel;
begin
{ Custom painting of Caption and Image needed, as standard routines misalign the
  position of Caption and Image }

  ABGRA.FontName := 'Segoe UI';
  ABGRA.FontHeight := 14;

  myText := TBCButton(Sender).Caption;

  { Defining myRect with Bottom -12px }
  myRect.Bottom := ARect.Bottom - 12;
  myRect.Left := ARect.Left;
  myRect.Right := ARect.Right;
  myRect.Top := ARect.Left;

  { Determining the width/height of Caption. We need only width }
  tw := ABGRA.TextSize(mytext).cx;
  c := uRibbon.BtnFntColor;

  if tw > TBCButton(Sender).Width then
  begin
    { Caption doesnt fit in one line, reduce Fontsize }
    ABGRA.FontHeight := 13;
    ABGRA.TextRect(ARect, myText, taCenter, tlBottom, c);
  end
  else
  begin
    { Caption fits in one line }
    ABGRA.TextRect(myRect, myText, taCenter, tlBottom, c);
  end;

  { Painting of Image on Button }
  ImageList32.Draw(ABGRA.Canvas, (TBCButton(Sender).Width - 32) div 2, 4,
    TBCButton(Sender).ImageIndex);
end;

procedure TfrmMain.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.btnMaximizeClick(Sender: TObject);
begin
  if WindowState = wsNormal then
  begin
    WindowState := wsMaximized;
    { Set Form size to fit on screen and not to hide the taskbar}
    with Screen.WorkAreaRect do
      SetBounds(Left, Top, Right - Left, Bottom - Top);
  end
  else
    WindowState := wsNormal;
end;

procedure TfrmMain.btnMinimizeClick(Sender: TObject);
begin
{ TODO : LINUX Application.Minimize;
Doesnt seem to work with Linux? Borderless forms apparently cannot be minimized
by design. Only way would be to use SW_HIDE, but then APP isnt shown on launch
pad. Another possibility would be a complete redesign of the application by
using TrayIcon, or use some normal form as mainform and keep it minimized all
the time (so it's visible in tray) }
  {$ifdef Windows}
  Application.Minimize;
  {$else}
  // Something to be done for OS <> Windows?
  if MessageDlg('Warning', 'Hide application? It wont be shown on launch pad.',
    mtWarning, [mbOK, mbCancel], 0) = mrOK then
    ShowWindow(frmMain.Handle, SW_HIDE);
  {$endif}
//  Application.ProcessMessages;
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.btnHideClick(Sender: TObject);
var
  i: integer = 66; //  vsBody.Top := 66 // needs to be replaced by variable
  step: integer = 10;
begin
  if vsBody.Tag = 0 then  //0 = Visible and Docked
  begin
    vsBody.Align := alNone;
    vsBody.Width := vsBorder.Width-2;
    ;
    vsBody.Anchors := [akTop, akLeft, akRight];
    vsForm.Width := frmMain.Width;
    { Animation / Slide in of vsBody }
    while i >= 0 do
    begin
      vsbody.Top := i;
      Repaint;
      // We spare some time...
      Sleep(1);
      Dec(i, step);
    end;
    vsBody.Hide;
    vsBody.Tag := 1;  // undocked
    btnHide.ImageIndex := 1;
    btnHide.Hint := 'Fix Menu';
  end
  else
  begin
    vsForm.Align := alNone;
    vsBody.Align := alTop;
    vsForm.Align := alClient;
    vsBody.Tag := 0;
    btnHide.ImageIndex := 0;
    btnHide.Hint := 'Hide Menu';
  end;
end;

{//  A more advanced way to resize the Form.
//  Unit uResize is needed as well.
procedure TfrmMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);

  var LeftEdge,RightEdge,TopEdge,BottomEdge: Boolean;
      BorderPosition: TBorderPos;
      BW: Integer;
      fRect: TRect;
      fPos: TPoint;
  begin
    BW := 20;
    LeftEdge := False;
    RightEdge := False;
    TopEdge := False;
    BottomEdge := False;
    //Check if mouse cursor is on left or right edge
    if X < BW then
    begin
      LeftEdge := True;
    end
    else if X > Self.Width-BW then
    begin
      RightEdge := True;
    end;

    //Check if mouse cursor is on top or bottom edge
    if Y < BorderWidth then
    begin
      TopEdge := True;
    end
    else if Y > Self.Height-BW then
    begin
      BottomEdge := True;
    end;

    //Get border position by using if..else if..else clause in order for if block
    //to break as soon as one of the conditions is met
    //First check for corners
    if TopEdge and LeftEdge then BorderPosition := bpTopLeft
    else if TopEdge and RightEdge then BorderPosition := bpTopRight
    else if BottomEdge and LeftEdge then BorderPosition := bpBottomLeft
    else if BottomEdge and RightEdge then BorderPosition := bpBottomRight
    //and only then check for individual edges
    else if TopEdge then BorderPosition := bpTop
    else if BottomEdge then BorderPosition := bpBottom
    else if LeftEdge then BorderPosition := bpLeft
    else if RightEdge then BorderPosition := bpRight
    //mouse cursor is not on any border edge
    else BorderPosition := bpNone;

//    fPos := Point(X, Y);

   //Finally you can use case statement to execute needed code based on which
    //border edge is your mouse cursor positioned
    case BorderPosition of
      bpNone: Cursor:=crDefault;//Label1.Caption := 'Not on border';
      bpLeft:
      begin
        Cursor:=crSizeWE; //.Caption := 'Left border';
        if ssLeft in shift then
          ResizeForm.SetMouseDown( self, 5 );
      end;
      bpRight:
      begin
        Cursor := crSizeWE;//Label1.Caption := 'Right border';
        if ssLeft in Shift then
          ResizeForm.SetMouseDown( self, 4 );
      end;

      bpTop: Cursor:=crSizeNS;//Label1.Caption := 'Top border';
      bpBottom:
      begin
        Cursor:=crSizeNS;//Label1.Caption := 'Bottom border';
        if ssLeft in Shift then
          ResizeForm.SetMouseDown( self, 8 );
      end;
//      bpTopLeft: Label1.Caption := 'Top Left corner';
//      bpTopRight: Label1.Caption := 'Top Right corner';
      bpBottomLeft:
      begin
        Cursor:=crSizeNESW;//Label1.Caption := 'Bottom Left corner';
        if ssLeft in Shift then
          ResizeForm.SetMouseDown( self, 13 );
      end;
      bpBottomRight:
      begin
        Cursor:=crSizeNWSE;//Label1.Caption := 'Bottom Right corner';
        if ssLeft in Shift then
          ResizeForm.SetMouseDown( self, 12 );
      end;
    end;


end;
}


procedure TfrmMain.btnColorsMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
{ TODO : Mousewheel to change Colors }
end;

procedure TfrmMain.btnFileMenuMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  Label2.Caption := 'File Menu clicked';
{  TODO : Implemantation of File Menu
  FileMenu.Show;
  MoveAnimation(FileMenu, -FileMenu.Width, vsTitle.Height, 1, 0);
  // MoveAnimation(FileMenu, -FileMenu.Width, 0, 1, 0);
}
end;

procedure TfrmMain.btnFileMenuCloseMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FileMenu.Hide;
end;

procedure TfrmMain.btnTabHomeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  tab: TBCButton;
  i: integer = 1;
  step: integer = 5;
begin
  if (Sender is TBCButton) then
  begin
    tab := (Sender as TBCButton);
    frmMain.DisableAutoSizing; //reduce flicker
    case tab.Name of
      'btnTabHome':
      begin
        btnTabHome.Down := True;
        btnTabEdit.Down := False;
        btnTabSettings.Down := False;
        pnlHomeCommand.Show;
        pnlHomeExit.Show;
        pnlEditClipboard.Hide;
        pnlEdit2.Hide;
        pnlSettingsUI.Hide;
        pnlSettings2.Hide;
      end;
      'btnTabEdit':
      begin
        btnTabHome.Down := False;
        btnTabEdit.Down := True;
        btnTabSettings.Down := False;
        pnlHomeCommand.Hide;
        pnlHomeExit.Hide;
        pnlSettingsUI.Hide;
        pnlSettings2.Hide;
        pnlEditClipboard.Show;
        pnlEdit2.Show;
      end;
      'btnTabSettings':
      begin
        btnTabHome.Down := False;
        btnTabEdit.Down := False;
        btnTabSettings.Down := True;
        pnlHomeCommand.Hide;
        pnlHomeExit.Hide;
        pnlEditClipboard.Hide;
        pnlEdit2.Hide;
        pnlSettingsUI.Show;
        pnlSettings2.Show;
      end;
    end;
    frmMain.EnableAutoSizing; //reduce flicker

    if not (vsBody.Visible) then
    begin
      vsBody.Show;
      vsForm.Top := vsBody.Top;
      { Animation / Slide-out of vsBody }
      while i <= 66 do
      begin    //66 = vsBody.Top // needs to be replaced by variable
        vsbody.Top := i;
        Repaint;
        // We spare some time...
        Sleep(1);
        Inc(i, step);
      end;
      vsBody.Anchors := [akTop, akLeft, akRight];
    end;
  end;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  {Setting Form style w/o Caption}
  if not FormStyleModifyDone then
  begin
    {$ifdef Windows}
    SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and
      not WS_CAPTION);
    {$else}
    // Something to be done for OS <> Windows?
    {$endif}
    FormStyleModifyDone := True;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  { Set config values, store and free up the XMLConfig }
  XMLConfigMain.SetDeleteValue('FormTop', IntToStr(frmMain.Top), '');
  XMLConfigMain.SetDeleteValue('FormLeft', IntToStr(frmMain.Left), '');
  XMLConfigMain.SetDeleteValue('FormHeight', IntToStr(frmMain.Height), '');
  XMLConfigMain.SetDeleteValue('FormWidth', IntToStr(frmMain.Width), '');
  XMLConfigMain.SetDeleteValue('Color', TabColor, 'blue');
{ TODO: Classical Menu
  if ActClassicalUI.Checked then
   XMLConfigMain.SetDeleteValue('UI', '1', '')
  else
   XMLConfigMain.SetDeleteValue('UI', '0', '');
  //  XMLConfigMain.SetDeleteValue('FontName', string(grdVoyage.Font.Name), '');
  //  XMLConfigMain.SetDeleteValue('FontSize', IntToStr(grdVoyage.Font.Size), '');
  //  XMLConfigMain.SetDeleteValue('FontColor', grdVoyage.Font.Color, '');
}
  XMLConfigMain.Flush;
  XMLConfigMain.Free;

  CloseAction := caFree;
end;

procedure TfrmMain.vsBorderResize(Sender: TObject);
begin
  {Reducing flicker when Form is resized}
  Repaint;
end;

procedure TfrmMain.vsBodyRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  DrawBodyGradient(Bitmap);
end;

procedure TfrmMain.vsFormRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  DrawFormGradient(Bitmap)
end;

procedure TfrmMain.vsTitleRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  DrawTitleGradient(Bitmap);
end;

procedure TfrmMain.vsTabRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  DrawTabGradient(Bitmap);
end;

procedure TfrmMain.vsTabMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  {Hide Menu if not fixed}
  if vsBody.Tag = 1 then
    vsBody.Hide;
end;

procedure TfrmMain.vsTitleDblClick(Sender: TObject);
begin
  {$ifdef Windows}
  { Helper - Sleep 180ms, otherwise its not possible to distinguish
  between Single- and Doubleclick event! }
  Sleep(180);
  {$else}
  // Something to be done for OS <> Windows?
  {$endif}
  if WindowState = wsNormal then
  begin
    WindowState := wsMaximized;
    { Set Form size to fit on screen and not to hide the taskbar}
    with Screen.WorkAreaRect do
      SetBounds(Left, Top, Right - Left, Bottom - Top);
  end
  else
    WindowState := wsNormal;
end;

procedure TfrmMain.vsTitleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
const
  { Undocument message ID }
  WM_POPUPSYSTEMMENU = $313;
var
  vs: TBGRAVirtualScreen;
begin
  {$ifdef Windows}
  {
    How to popup the windows system menu?
    Maybe you can use Keybd_event to eumlate ALT+SPACE
    Maybe you can use a TPopupmenu.
    But they always have some problem.
    The method below is a perfect solution!
    BTW: if your form has borderstyle = bsNone, Please do it like this:
    Set forms style = bsSingle; and use the code below to set form boder:
    SetWindowLong(Handle, GWL_STYLE,GetWindowLong(Handle, GWL_STYLE)
    and (not WS_CAPTION) or WS_DLGFRAME or WS_OVERLAPPED);
  }
  {
  if (Sender is TBGRAVirtualScreen) then
  begin
    vs := (Sender as TBGRAVirtualScreen);
    if vs.Name = 'vsTitle' then // Show SystemMenu only on vsTitle
    begin
}    if ssRIGHT in Shift then
      begin
        SendMessage(Handle, WM_POPUPSYSTEMMENU, 0,
         MakeLong(Mouse.CursorPos.X, Mouse.CursorPos.Y));
        exit;
      end;
{    end;
  end;
}  {$else}
  // Something to be done for OS <> Windows?
  {$endif}

  {Hide Menu if not fixed}
  if vsBody.Tag = 1 then
    vsBody.Hide;
  GetCursorPos(MausPos);
  xx := mauspos.x - frmMain.Left;
  yy := mauspos.y - frmMain.Top;
end;

procedure TfrmMain.vsTitleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if ssLeft in Shift then
  begin
    GetCursorPos(MausPos);
    frmMain.Left := mauspos.x - xx;
    frmMain.Top := mauspos.y - yy;
  end;
end;

procedure TfrmMain.btnHelpMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  { User action }
  MessageDlg('About',
    'BGRA Ribbon' + Chr(13) + Chr(10) +
    'Made with Lazarus' + Chr(13) + Chr(10) +
    'Icons from www.icons8.com'+ Chr(13) + Chr(10) +
    'SVG from www.clker.com/clipart-floral-circles.html'+ Chr(13) + Chr(10) +
    '(c) 2016, coasting',
    mtInformation, [mbOK], 0);
end;

{ User actions }

procedure TfrmMain.btnNewClick(Sender: TObject);
begin
 { User action }
  Label2.Caption := TComponent(Sender).Name;
end;

procedure TfrmMain.btnClipboardClick(Sender: TObject);
begin
  { User action }
  Label2.Caption := TComponent(Sender).Name;
end;

procedure TfrmMain.btnPrintClick(Sender: TObject);
begin
  { User action }
  Label2.Caption := TComponent(Sender).Name;
end;

procedure TfrmMain.btnCopyClick(Sender: TObject);
begin
  { User action }
  Label2.Caption := TComponent(Sender).Name;
end;

procedure TfrmMain.btnPasteClick(Sender: TObject);
begin
  { User action }
  Label2.Caption := TComponent(Sender).Name;
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
  { User action }
  Label2.Caption := TComponent(Sender).Name;
end;

procedure TfrmMain.btnTool1Click(Sender: TObject);
begin
  { User action }
  Label2.Caption := TComponent(Sender).Name;
end;

procedure TfrmMain.btnTool2Click(Sender: TObject);
begin
  { User action }
  Label2.Caption := TComponent(Sender).Name;
end;

procedure TfrmMain.btnTool3Click(Sender: TObject);
begin
  { User action }
  Label2.Caption := TComponent(Sender).Name;
end;

procedure TfrmMain.btnCutClick(Sender: TObject);
begin
  { User action }
  Label2.Caption := TComponent(Sender).Name;
end;

procedure TfrmMain.btnPreviewClick(Sender: TObject);
begin
  { User action }
  Label2.Caption := TComponent(Sender).Name;
end;


procedure TfrmMain.btnOpenClick(Sender: TObject);
begin
  { User action }
  Label2.Caption := TComponent(Sender).Name;
end;


end.
