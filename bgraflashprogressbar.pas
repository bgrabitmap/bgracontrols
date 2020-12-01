// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Created by BGRA Controls Team
  Dibo, Circular, lainz (007) and contributors.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BGRAFlashProgressBar;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources, LMessages,{$ENDIF} Forms, Controls, Graphics,
  {$IFNDEF FPC}Messages, Windows, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCBaseCtrls, Dialogs, BGRABitmap, BGRADrawerFlashProgressBar;

type
  { TBGRAFlashProgressBar }

  TBGRAFlashProgressBar = class(TBGRAGraphicCtrl)
  private
    FBGRA: TBGRABitmap;
    FDrawer: TBGRADrawerFlashProgressBar;
    FOnRedraw: TBGRAProgressBarRedrawEvent;
    function GetBackgroundColor: TColor;
    function GetBackgroundRandomize: boolean;
    function GetBackgroundRandomizeMaxIntensity: word;
    function GetBackgroundRandomizeMinIntensity: word;
    function GetBarColor: TColor;
    function GetMaxValue: integer;
    function GetMinValue: integer;
    function GetValue: integer;
    procedure OnChangeDrawer(Sender: TObject);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetBackgroundRandomize(AValue: boolean);
    procedure SetBackgroundRandomizeMaxIntensity(AValue: word);
    procedure SetBackgroundRandomizeMinIntensity(AValue: word);
    procedure SetBarColor(AValue: TColor);
    procedure SetMaxValue(const AValue: integer);
    procedure SetMinValue(const AValue: integer);
    procedure SetValue(const AValue: integer);
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: boolean); override;
    procedure WMEraseBkgnd(var Message: {$IFDEF FPC}TLMEraseBkgnd{$ELSE}TWMEraseBkgnd{$ENDIF}); message {$IFDEF FPC}LM_ERASEBKGND{$ELSE}WM_ERASEBKGND{$ENDIF};
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    { Streaming }
    {$IFDEF FPC}
    procedure SaveToFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
    {$ENDIF}
  published
    property Align;
    property Anchors;
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property MinValue: integer Read GetMinValue Write SetMinValue;
    property MaxValue: integer Read GetMaxValue Write SetMaxValue;
    property Value: integer Read GetValue Write SetValue;
    property Color; deprecated 'User BarColor instead';
    property BarColor: TColor read GetBarColor write SetBarColor;
    property BackgroundColor: TColor read GetBackgroundColor write SetBackgroundColor;
    property BackgroundRandomizeMinIntensity: word read GetBackgroundRandomizeMinIntensity write SetBackgroundRandomizeMinIntensity;
    property BackgroundRandomizeMaxIntensity: word read GetBackgroundRandomizeMaxIntensity write SetBackgroundRandomizeMaxIntensity;
    property BackgroundRandomize: boolean read GetBackgroundRandomize write SetBackgroundRandomize;
    property OnRedraw: TBGRAProgressBarRedrawEvent read FOnredraw write FOnRedraw;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

uses BGRABitmapTypes;

{$IFDEF FPC}
procedure Register;
begin
  //{$I icons\bgraflashprogressbar_icon.lrs}
  RegisterComponents('BGRA Controls', [TBGRAFlashProgressBar]);
end;
{$ENDIF}

procedure TBGRAFlashProgressBar.SetMinValue(const AValue: integer);
begin
  FDrawer.MinValue := AValue;
end;

procedure TBGRAFlashProgressBar.SetValue(const AValue: integer);
begin
  FDrawer.Value := AValue;
end;

{$hints off}
procedure TBGRAFlashProgressBar.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
begin
  PreferredWidth  := 379;
  PreferredHeight := 33;
end;

{$hints on}

procedure TBGRAFlashProgressBar.Paint;
begin
  if (ClientWidth <> FBGRA.Width) or (ClientHeight <> FBGRA.Height) then
    FBGRA.SetSize(ClientWidth, ClientHeight);
  FDrawer.Draw(FBGRA);
  if Assigned(OnRedraw) then
    OnRedraw(Self, FBGRA, {%H-}FDrawer.XPosition);
  FBGRA.Draw(Canvas, 0, 0, False);
end;

{$hints off}
procedure TBGRAFlashProgressBar.WMEraseBkgnd(var Message: {$IFDEF FPC}TLMEraseBkgnd{$ELSE}TWMEraseBkgnd{$ENDIF});
begin
  //do nothing
end;
{$hints on}

constructor TBGRAFlashProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, 33);

  // Bitmap and Drawer
  FBGRA := TBGRABitmap.Create(Width, Height);
  FDrawer := TBGRADrawerFlashProgressBar.Create;
  FDrawer.OnChange := OnChangeDrawer;
  // Functionality
  MinValue := 0;
  MaxValue := 100;
  Value := 30;
  // Functionality and Style
  Randomize;
  FDrawer.RandSeed := RandSeed;
  // Style
  BarColor := BGRA(102, 163, 226);
  BackgroundColor := BGRA(47,47,47);
  BackgroundRandomize := True;
  BackgroundRandomizeMinIntensity := 4000;
  BackgroundRandomizeMaxIntensity := 5000;
end;

destructor TBGRAFlashProgressBar.Destroy;
begin
  FreeAndNil(FBGRA);
  FDrawer.Free;
  inherited Destroy;
end;
{$IFDEF FPC}
procedure TBGRAFlashProgressBar.SaveToFile(AFileName: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    WriteComponentAsTextToStream(AStream, Self);
    AStream.SaveToFile(AFileName);
  finally
    AStream.Free;
  end;
end;

procedure TBGRAFlashProgressBar.LoadFromFile(AFileName: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFileName);
    ReadComponentFromTextStream(AStream, TComponent(Self), OnFindClass);
  finally
    AStream.Free;
  end;
end;

procedure TBGRAFlashProgressBar.OnFindClass(Reader: TReader;
  const AClassName: string; var ComponentClass: TComponentClass);
begin
  if CompareText(AClassName, 'TBGRAFlashProgressBar') = 0 then
    ComponentClass := TBGRAFlashProgressBar;
end;
{$ENDIF}

procedure TBGRAFlashProgressBar.SetMaxValue(const AValue: integer);
begin
  FDrawer.MaxValue := AValue;
end;

procedure TBGRAFlashProgressBar.OnChangeDrawer(Sender: TObject);
begin
  Invalidate;
end;

function TBGRAFlashProgressBar.GetBackgroundColor: TColor;
begin
  Result := FDrawer.BackgroundColor;
end;

function TBGRAFlashProgressBar.GetBackgroundRandomize: boolean;
begin
  Result := FDrawer.BackgroundRandomize;
end;

function TBGRAFlashProgressBar.GetBackgroundRandomizeMaxIntensity: word;
begin
  Result := FDrawer.BackgroundRandomizeMaxIntensity;
end;

function TBGRAFlashProgressBar.GetBackgroundRandomizeMinIntensity: word;
begin
  Result := FDrawer.BackgroundRandomizeMinIntensity;
end;

function TBGRAFlashProgressBar.GetBarColor: TColor;
begin
  Result := FDrawer.BarColor;
end;

function TBGRAFlashProgressBar.GetMaxValue: integer;
begin
  Result := FDrawer.MaxValue;
end;

function TBGRAFlashProgressBar.GetMinValue: integer;
begin
  Result := FDrawer.MinValue;
end;

function TBGRAFlashProgressBar.GetValue: integer;
begin
  Result := FDrawer.Value;
end;

procedure TBGRAFlashProgressBar.SetBackgroundColor(AValue: TColor);
begin
  FDrawer.BackgroundColor := AValue;
end;

procedure TBGRAFlashProgressBar.SetBackgroundRandomize(AValue: boolean);
begin
  FDrawer.BackgroundRandomize := AValue;
end;

procedure TBGRAFlashProgressBar.SetBackgroundRandomizeMaxIntensity(AValue: word
  );
begin
  FDrawer.BackgroundRandomizeMaxIntensity := AValue;
end;

procedure TBGRAFlashProgressBar.SetBackgroundRandomizeMinIntensity(AValue: word
  );
begin
  FDrawer.BackgroundRandomizeMinIntensity := AValue;
end;

procedure TBGRAFlashProgressBar.SetBarColor(AValue: TColor);
begin
  FDrawer.BarColor := AValue;
end;

end.
