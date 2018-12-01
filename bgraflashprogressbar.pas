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

{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BGRAFlashProgressBar;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources, LMessages,{$ENDIF} Types, Forms, Controls, Graphics,
  {$IFNDEF FPC}Messages, Windows, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCBaseCtrls, Dialogs, BGRABitmap, Math;

type

  TBGRAProgressBarRedrawEvent = procedure(Sender: TObject; Bitmap: TBGRABitmap; xpos: integer) of object;

  { TBGRAFlashProgressBar }

  TBGRAFlashProgressBar = class(TBGRAGraphicCtrl)
  private
    FBackgroundRandomize: boolean;
    FBackgroundRandomizeMaxIntensity: word;
    FBackgroundRandomizeMinIntensity: word;
    FColor: TColor;
    FMaxValue: integer;
    FMinValue: integer;
    FValue:    integer;
    FBmp:      TBGRABitmap;
    FRandSeed: integer;
    FOnRedraw: TBGRAProgressBarRedrawEvent;
    procedure SetFBackgroundRandomize(AValue: boolean);
    procedure SetFBackgroundRandomizeMaxIntensity(AValue: word);
    procedure SetFBackgroundRandomizeMinIntensity(AValue: word);
    procedure SetFColor(AValue: TColor);
    procedure SetMaxValue(const AValue: integer);
    procedure SetMinValue(const AValue: integer);
    procedure SetValue(const AValue: integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: boolean); override;
    procedure Paint; override;
    procedure WMEraseBkgnd(var Message: {$IFDEF FPC}TLMEraseBkgnd{$ELSE}TWMEraseBkgnd{$ENDIF}); message {$IFDEF FPC}LM_ERASEBKGND{$ELSE}WM_ERASEBKGND{$ENDIF};
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    { Streaming }
    {$IFDEF FPC}
    procedure SaveToFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);
    {$ENDIF}
    procedure OnFindClass({%H-}Reader: TReader; const AClassName: string;
      var ComponentClass: TComponentClass);
  published
    { Published declarations }
    property Align;
    property Anchors;
    property MinValue: integer Read FMinValue Write SetMinValue;
    property MaxValue: integer Read FMaxValue Write SetMaxValue;
    property Value: integer Read FValue Write SetValue;
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property OnRedraw: TBGRAProgressBarRedrawEvent read FOnredraw write FOnRedraw;
    property Color;
    property BackgroundColor: TColor read FColor write SetFColor;
    property BackgroundRandomizeMinIntensity: word read FBackgroundRandomizeMinIntensity write SetFBackgroundRandomizeMinIntensity;
    property BackgroundRandomizeMaxIntensity: word read FBackgroundRandomizeMaxIntensity write SetFBackgroundRandomizeMaxIntensity;
    property BackgroundRandomize: boolean read FBackgroundRandomize write SetFBackgroundRandomize;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

uses BGRABitmapTypes, BGRAGradients;

{$IFDEF FPC}
procedure Register;
begin
  //{$I icons\bgraflashprogressbar_icon.lrs}
  RegisterComponents('BGRA Controls', [TBGRAFlashProgressBar]);
end;
{$ENDIF}

{ TBGRAFlashProgressBar }

procedure TBGRAFlashProgressBar.SetMinValue(const AValue: integer);
begin
  if FMinValue = AValue then
    exit;
  FMinValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FMaxValue < FMinValue then
    FMaxValue := FMinValue;
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetValue(const AValue: integer);
begin
  if FValue = AValue then
    exit;
  FValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  Invalidate;
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
var
  content: TRect;
  xpos, y, tx, ty: integer;
  bgColor: TBGRAPixel;

  function ApplyLightness(c: TBGRAPixel; lightness: word): TBGRAPixel;
  begin
    Result := GammaCompression(SetLightness(GammaExpansion(c), lightness));
  end;

  procedure DrawBar(bounds: TRect);
  var
    lCol: TBGRAPixel;
  begin
    lCol := Color;

    DoubleGradientAlphaFill(FBmp, bounds,
      ApplyLightness(lCol, 37000), ApplyLightness(lCol, 29000),
      ApplyLightness(lCol, 26000), ApplyLightness(lCol, 18000),
      gdVertical, gdVertical, gdVertical, 0.53);

    InflateRect(bounds, -1, -1);

    DoubleGradientAlphaFill(FBmp, bounds,
      ApplyLightness(lCol, 28000), ApplyLightness(lCol, 22000),
      ApplyLightness(lCol, 19000), ApplyLightness(lCol, 11000),
      gdVertical, gdVertical, gdVertical, 0.53);
  end;

begin
  tx := ClientWidth;
  ty := ClientHeight;
  if Assigned(FBmp) and ((FBmp.Width <> tx) or (FBmp.Height <> ty)) then
    FreeAndNil(FBmp);

  if not Assigned(FBmp) then
    FBmp := TBGRABitmap.Create(tx, ty)
  else
    FBmp.FillTransparent;

  FBmp.Rectangle(0, 0, tx, ty, BGRA(255, 255, 255, 6), BackgroundColor, dmSet);
  if (tx > 2) and (ty > 2) then
    FBmp.Rectangle(1, 1, tx - 1, ty - 1, BGRA(29, 29, 29), dmSet);

  if (tx > 4) and (ty > 4) then
  begin
    content  := Rect(2, 2, tx - 2, ty - 2);
    randseed := FRandSeed;
    if BackgroundRandomize then
    for y := content.Top to content.Bottom - 1 do
    begin
      bgColor := BackgroundColor;
      bgColor.Intensity := RandomRange(BackgroundRandomizeMinIntensity, BackgroundRandomizeMaxIntensity);
      FBmp.HorizLine(content.Left, y, content.Right - 1, bgColor, dmSet);
    end;
    if tx >= 6 then
      FBmp.DrawVertLine(content.Right - 1, content.Top, content.Bottom - 1,
        BGRA(0, 0, 0, 32));
    if FMaxValue > FMinValue then
    begin
      xpos := round((FValue - FMinValue) / (FMaxValue - FMinValue) *
        (content.right - content.left)) + content.left;
      if xpos > content.left then
      begin
        DrawBar(rect(content.left, content.top, xpos, content.bottom));
        if xpos < content.right then
        begin
          FBmp.SetPixel(xpos, content.top, BGRA(62, 62, 62));
          FBmp.SetVertLine(xpos, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
        end;
      end;
    end;
  end;
  if Assigned(OnRedraw) then
    OnRedraw(Self, FBmp, {%H-}xpos);
  FBmp.Draw(Canvas, 0, 0, False);
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

  FMinValue := 0;
  FMaxValue := 100;
  FValue := 30;
  FBmp := nil;
  randomize;
  FRandSeed := randseed;
  Color := BGRA(102, 163, 226);
  BackgroundColor := BGRA(47,47,47);
  BackgroundRandomize := True;
  BackgroundRandomizeMinIntensity := 4000;
  BackgroundRandomizeMaxIntensity := 5000;
end;

destructor TBGRAFlashProgressBar.Destroy;
begin
  FreeAndNil(FBmp);
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
{$ENDIF}

procedure TBGRAFlashProgressBar.OnFindClass(Reader: TReader;
  const AClassName: string; var ComponentClass: TComponentClass);
begin
  if CompareText(AClassName, 'TBGRAFlashProgressBar') = 0 then
    ComponentClass := TBGRAFlashProgressBar;
end;

procedure TBGRAFlashProgressBar.SetMaxValue(const AValue: integer);
begin
  if FMaxValue = AValue then
    exit;
  FMaxValue := AValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  if FMinValue > FMaxValue then
    FMinValue := FMaxValue;
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetFColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetFBackgroundRandomize(AValue: boolean);
begin
  if FBackgroundRandomize=AValue then Exit;
  FBackgroundRandomize:=AValue;
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetFBackgroundRandomizeMaxIntensity(AValue: word
  );
begin
  if FBackgroundRandomizeMaxIntensity=AValue then Exit;
  FBackgroundRandomizeMaxIntensity:=AValue;
  Invalidate;
end;

procedure TBGRAFlashProgressBar.SetFBackgroundRandomizeMinIntensity(AValue: word
  );
begin
  if FBackgroundRandomizeMinIntensity=AValue then Exit;
  FBackgroundRandomizeMinIntensity:=AValue;
  Invalidate;
end;

end.
