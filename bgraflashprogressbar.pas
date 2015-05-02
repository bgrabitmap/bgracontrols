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

unit BGRAFlashProgressBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LMessages, Forms, Controls, Graphics,
  Dialogs, BGRABitmap;

type

  TBGRAProgressBarRedrawEvent = procedure(Sender: TObject; Bitmap: TBGRABitmap; xpos: integer) of object;

  { TBGRAFlashProgressBar }

  TBGRAFlashProgressBar = class(TGraphicControl)
  private
    FMaxValue: integer;
    FMinValue: integer;
    FValue:    integer;
    FBmp:      TBGRABitmap;
    FRandSeed: integer;
    FOnRedraw: TBGRAProgressBarRedrawEvent;
    procedure SetMaxValue(const AValue: integer);
    procedure SetMinValue(const AValue: integer);
    procedure SetValue(const AValue: integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: boolean); override;
    procedure Paint; override;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    { Streaming }
    procedure SaveToFile(AFileName: string);
    procedure LoadFromFile(AFileName: string);
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
  end;

procedure Register;

implementation

uses BGRABitmapTypes, BGRAGradients, Types;

procedure Register;
begin
  {$I icons\bgraflashprogressbar_icon.lrs}
  RegisterComponents('BGRA Controls', [TBGRAFlashProgressBar]);
end;

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
  grayValue: integer;

  function ApplyLightness(c: TBGRAPixel; lightness: word): TBGRAPixel;
  begin
    Result := GammaCompression(SetLightness(GammaExpansion(c), lightness));
  end;

  procedure DrawBar(bounds: TRect);
  var
    lCol: TBGRAPixel;
  begin
    lCol := ColorToBGRA(ColorToRGB(Color));

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

  FBmp.Rectangle(0, 0, tx, ty, BGRA(255, 255, 255, 6), dmSet);
  if (tx > 2) and (ty > 2) then
    FBmp.Rectangle(1, 1, tx - 1, ty - 1, BGRA(29, 29, 29), dmSet);

  if (tx > 4) and (ty > 4) then
  begin
    content  := Rect(2, 2, tx - 2, ty - 2);
    randseed := FRandSeed;
    for y := content.Top to content.Bottom - 1 do
    begin
      if y = content.Top then
        grayValue := 33
      else
      if y = content.Top + 1 then
        grayValue := 43
      else
        grayValue := 47 + random(50 - 47 + 1);
      FBmp.SetHorizLine(content.Left, y, content.Right - 1, BGRA(
        grayValue, grayValue, grayValue));
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
    OnRedraw(Self, FBmp, xpos);
  FBmp.Draw(Canvas, 0, 0, False);
end;

{$hints off}
procedure TBGRAFlashProgressBar.WMEraseBkgnd(var Message: TLMEraseBkgnd);
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
  Color := BGRAToColor(BGRA(102, 163, 226));
end;

destructor TBGRAFlashProgressBar.Destroy;
begin
  FreeAndNil(FBmp);
  inherited Destroy;
end;

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
    ReadComponentFromTextStream(AStream, TComponent(Self), @OnFindClass);
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

end.
