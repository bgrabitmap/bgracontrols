{
  Part of BGRA Controls. Made by third party.
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
unit DTAnalogCommon;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ELSE}Types, {$ENDIF} Forms, Controls, Graphics, Dialogs,
  BCBaseCtrls, BGRABitmap, BGRABitmapTypes;

type
  TDTFillStyle = (fsnone, fsGradient{, fsTexture});

  TDTNeedleStyle = (nsLine, nsTriangle{, nsLineExt, nsTriangleExt});

  { TDTOrigin }

  TDTOrigin = packed record
    CenterPoint: TPoint;
    Radius: integer;
  end;

  { TDTPointerCapSettings }

  TDTPointerCapSettings = class(TPersistent)
  private
    FEdgeColor: TColor;
    FEdgeThickness: integer;
    FFillColor: TColor;
    FOnChange: TNotifyEvent;
    FRadius: integer;
    procedure SetEdgeColor(AValue: TColor);
    procedure SetEdgeThickness(AValue: integer);
    procedure SetFillColor(AValue: TColor);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetRadius(AValue: integer);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  published
    property EdgeColor: TColor read FEdgeColor write SetEdgeColor;
    property FillColor: TColor read FFillColor write SetFillColor;
    property Radius: integer read FRadius write SetRadius;
    property EdgeThickness: integer read FEdgeThickness write SetEdgeThickness;
  end;

  { TDTPointerSettings }

  TDTPointerSettings = class(TPersistent)
  private
    FColor: TColor;
    FLength: integer;
    FOnChange: TNotifyEvent;
    FThickness: integer;
    procedure SetColor(AValue: TColor);
    procedure SetLength(AValue: integer);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetThickness(AValue: integer);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  published
    property Color: TColor read FColor write SetColor;
    property Length: integer read FLength write SetLength;
    property Thickness: integer read FThickness write SetThickness;
  end;

  { TDTNeedleSettings }

  TDTNeedleSettings = class(TPersistent)
  private
    FCapColor: TColor;
    FCapEdgeColor: TColor;
    FCapRadius: integer;
    FNeedleColor: TColor;
    FNeedleLength: integer;
    FNeedleStyle: TDTNeedleStyle;
    FOnChange: TNotifyEvent;
    procedure SetCapColor(AValue: TColor);
    procedure SetCapEdgeColor(AValue: TColor);
    procedure SetCapRadius(AValue: integer);
    procedure SetNeedleColor(AValue: TColor);
    procedure SetNeedleLength(AValue: integer);
    procedure SetNeedleStyle(AValue: TDTNeedleStyle);
    procedure SetOnChange(AValue: TNotifyEvent);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property NeedleStyle: TDTNeedleStyle read FNeedleStyle write SetNeedleStyle;
  published
    property NeedleColor: TColor read FNeedleColor write SetNeedleColor;
    property NeedleLength: integer read FNeedleLength write SetNeedleLength;
    property CapRadius: integer read FCapRadius write SetCapRadius;
    property CapColor: TColor read FCapColor write SetCapColor;
    property CapEdgeColor: TColor read FCapEdgeColor write SetCapEdgeColor;
  end;

  { TDTScaleSettings }

  TDTScaleSettings = class(TPersistent)
  private
    FEnableScaleText: boolean;
    FMaximum: integer;
    FTesting: boolean;
    FTextFont: string;
    FTextRadius: integer;
    FTextSize: integer;
    FTickColor: TColor;
    FEnableMainTicks: boolean;
    FEnableRangeIndicator: boolean;
    FEnableSubTicks: boolean;
    FLengthMainTick: integer;
    FLengthSubTick: integer;
    FMainTickCount: integer;
    FMinimum: integer;
    FOnChange: TNotifyEvent;
    FSubTickCount: integer;
    FTextColor: TColor;
    FThicknessMainTick: integer;
    FThicknessSubTick: integer;
    FAngle: integer;
    procedure SetEnableScaleText(AValue: boolean);
    procedure SetFAngle(AValue: integer);
    procedure SetMaximum(AValue: integer);
    procedure SetTesting(AValue: boolean);
    procedure SetTextFont(AValue: string);
    procedure SetTextRadius(AValue: integer);
    procedure SetTextSize(AValue: integer);
    procedure SetTickColor(AValue: TColor);
    procedure SetEnableMainTicks(AValue: boolean);
    procedure SetEnableRangeIndicator(AValue: boolean);
    procedure SetEnableSubTicks(AValue: boolean);
    procedure SetLengthMainTick(AValue: integer);
    procedure SetLengthSubTick(AValue: integer);
    procedure SetMainTickCount(AValue: integer);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetSubTickCount(AValue: integer);
    procedure SetTextColor(AValue: TColor);
    procedure SetThicknessMainTick(AValue: integer);
    procedure SetThicknessSubTick(AValue: integer);
  protected
    property Testing: boolean read FTesting write SetTesting;
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  published
    property TickColor: TColor read FTickColor write SetTickColor;
    property TextColor: TColor read FTextColor write SetTextColor;
    property TextSize: integer read FTextSize write SetTextSize;
    property TextFont: string read FTextFont write SetTextFont;
    property EnableMainTicks: boolean read FEnableMainTicks write SetEnableMainTicks;
    property EnableSubTicks: boolean read FEnableSubTicks write SetEnableSubTicks;
    property EnableScaleText: boolean read FEnableScaleText write SetEnableScaleText;
    property Maximum: integer read FMaximum write SetMaximum;
    property MainTickCount: integer read FMainTickCount write SetMainTickCount;
    property SubTickCount: integer read FSubTickCount write SetSubTickCount;
    property LengthMainTick: integer read FLengthMainTick write SetLengthMainTick;
    property LengthSubTick: integer read FLengthSubTick write SetLengthSubTick;
    property ThicknessMainTick: integer read FThicknessMainTick write SetThicknessMainTick;
    property ThicknessSubTick: integer read FThicknessSubTick write SetThicknessSubTick;
    property TextRadius: integer read FTextRadius write SetTextRadius;
    property Angle: integer read FAngle write SetFAngle;
    property EnableRangeIndicator: boolean read FEnableRangeIndicator write SetEnableRangeIndicator;
    //property RangeMinValue: integer read FRangeMinValue write SetRangeMinValue;
    //property RangeMidValue: integer read FRangeMidValue write SetRangeMidValue;
    //property RangeMaxValue: integer read FRangeMaxValue write SetRangeMaxValue;
    //property RangeMinColor: TColor read FRangeMinColor write SetRangeMinColor;
    //property RangeMidColor: TColor read FRangeMidColor write SetRangeMidColor;
    //property RangeMaxColor: TColor read FRangeMaxColor write SetRangeMaxColor;
  end;

  { TDTFaceSettings }

  TDTFaceSettings = class(TPersistent)
  private
    FColorEnd: TColor;
    FColorFrame: TColor;
    FColorStart: TColor;
    FFillStyle: TDTFillStyle;
    FOnChange: TNotifyEvent;
    procedure SetColorEnd(AValue: TColor);
    procedure SetColorFrame(AValue: TColor);
    procedure SetColorStart(AValue: TColor);
    procedure SetFillStyle(AValue: TDTFillStyle);
    procedure SetOnChange(AValue: TNotifyEvent);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  published
    property FillStyle: TDTFillStyle read FFillStyle write SetFillStyle;
    property ColorFrame: TColor read FColorFrame write SetColorFrame;
    property ColorStart: TColor read FColorStart write SetColorStart;
    property ColorEnd: TColor read FColorEnd write SetColorEnd;
  end;

  { TDTBaseAnalogDevice }

  TDTBaseAnalogDevice = class(TBGRAGraphicCtrl)
  private
    FFaceSettings: TDTFaceSettings;
    FScaleSettings: TDTScaleSettings;
    procedure SetFaceSettings(AValue: TDTFaceSettings);
    procedure SetScaleSettings(AValue: TDTScaleSettings);
  protected
    procedure DoChange({%H-}Sender: TObject);
  public
    fGaugeBitmap: TBGRABitmap;
    FFrameBitmap: TBGRABitmap;
    FFaceBitmap: TBGRABitmap;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FaceSettings: TDTFaceSettings read FFaceSettings write SetFaceSettings;
    property ScaleSettings: TDTScaleSettings read FScaleSettings write SetScaleSettings;
    procedure Paint; override;
    procedure DrawGauge;
    procedure DrawFrame;
    procedure DrawFace;
  end;

function Initializebitmap(var Bitmap: TBGRABitmap; Width, Height: integer): TDTOrigin;

implementation

function Initializebitmap(var Bitmap: TBGRABitmap; Width, Height: integer): TDTOrigin;
begin

  Bitmap.SetSize(Width, Height);

  // Clear bitmap to transparent
  BitMap.Fill(BGRA(0, 0, 0, 0));

  // Get origin information
  Result.CenterPoint.x := Width div 2;
  Result.CenterPoint.y := Height div 2;

  Result.Radius := Result.CenterPoint.y;

  if Result.CenterPoint.x > Result.CenterPoint.y then
    Result.Radius := Result.CenterPoint.y;

  if Result.CenterPoint.x < Result.CenterPoint.y then
    Result.Radius := Result.CenterPoint.x;

end;

{ TDTPointerCapSettings }

procedure TDTPointerCapSettings.SetEdgeColor(AValue: TColor);
begin
  if FEdgeColor = AValue then
    Exit;
  FEdgeColor := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTPointerCapSettings.SetEdgeThickness(AValue: integer);
begin
  if FEdgeThickness = AValue then
    Exit;
  FEdgeThickness := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTPointerCapSettings.SetFillColor(AValue: TColor);
begin
  if FFillColor = AValue then
    Exit;
  FFillColor := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTPointerCapSettings.SetOnChange(AValue: TNotifyEvent);
begin
//#  if FOnChange = AValue then
//#    Exit;
  FOnChange := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTPointerCapSettings.SetRadius(AValue: integer);
begin
  if FRadius = AValue then
    Exit;
  FRadius := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TDTPointerCapSettings.Create;
begin
  FEdgeColor := $00CCDCDC;
  FFillColor := $003F3F3F;
  FRadius := 10;
  FEdgeThickness := 1;
end;

destructor TDTPointerCapSettings.Destroy;
begin
  inherited Destroy;
end;

{ TDTPointerSettings }

procedure TDTPointerSettings.SetColor(AValue: TColor);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTPointerSettings.SetLength(AValue: integer);
begin
  if FLength = AValue then
    Exit;
  FLength := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTPointerSettings.SetOnChange(AValue: TNotifyEvent);
begin
//#  if FOnChange = AValue then
//#    Exit;
  FOnChange := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTPointerSettings.SetThickness(AValue: integer);
begin
  if FThickness = AValue then
    Exit;
  FThickness := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TDTPointerSettings.Create;
begin
  FColor := BGRA(199, 199, 173);
  FLength := 100;
  FThickness := 3;
end;

destructor TDTPointerSettings.Destroy;
begin
  inherited Destroy;
end;

{ TDTBaseAnalogDevice }

procedure TDTBaseAnalogDevice.SetFaceSettings(AValue: TDTFaceSettings);
begin
  if FFaceSettings = AValue then
    Exit;
  FFaceSettings := AValue;

  DoChange(self);
end;

procedure TDTBaseAnalogDevice.SetScaleSettings(AValue: TDTScaleSettings);
begin
  if FScaleSettings = AValue then
    Exit;
  FScaleSettings := AValue;

  DoChange(self);
end;

procedure TDTBaseAnalogDevice.DoChange(Sender: TObject);
begin
  Invalidate;
end;

constructor TDTBaseAnalogDevice.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 240;
  Height := 240;

  FScaleSettings := TDTScaleSettings.Create;
  ScaleSettings.OnChange := DoChange;

  ScaleSettings.TextRadius := round((Width * 0.6) / 2);

  FFaceSettings := TDTFaceSettings.Create;
  FaceSettings.OnChange := DoChange;

  FGaugeBitmap := TBGRABitmap.Create;
  FFaceBitmap := TBGRABitmap.Create;
  FFrameBitmap := TBGRABitmap.Create;

end;

destructor TDTBaseAnalogDevice.Destroy;
begin
  FScaleSettings.Free;
  FFaceSettings.Free;

  fGaugeBitmap.Free;
  FFaceBitmap.Free;
  FFrameBitmap.Free;
  inherited Destroy;
end;

procedure TDTBaseAnalogDevice.Paint;
begin
  inherited Paint;
  FGaugeBitmap.Fill(BGRA(0, 0, 0, 0));

  fGaugeBitmap.SetSize(Width, Height);

  DrawGauge;

  FGaugeBitmap.BlendImage(0, 0, FFrameBitmap, boLinearBlend);
  FGaugeBitmap.BlendImage(0, 0, FFaceBitmap, boLinearBlend);

  FGaugeBitmap.Draw(Canvas, 0, 0, False);

end;

procedure TDTBaseAnalogDevice.DrawGauge;
begin
  DrawFrame;
  DrawFace;
end;

procedure TDTBaseAnalogDevice.DrawFrame;
var
  Origin: TDTOrigin;
  r: integer;
begin

  Origin := Initializebitmap(FFrameBitmap, Width, Height);

  r := round(Origin.Radius * 0.95);

  // Draw Bitmap frame
  FFrameBitmap.FillEllipseAntialias(Origin.CenterPoint.x,
    Origin.CenterPoint.y,
    r, r, FFaceSettings.ColorFrame);

  // Draw thin antialiased border to smooth against background
  FFrameBitmap.EllipseAntialias(Origin.CenterPoint.x,
    Origin.CenterPoint.y,
    r, r, ColorToBGRA(clBlack, 120), 1);

end;

procedure TDTBaseAnalogDevice.DrawFace;
var
  Origin: TDTOrigin;
  r: integer;
begin

  Origin := Initializebitmap(FFaceBitmap, Width, Height);

  r := round(Origin.Radius * 0.95) - 5;

  case FFaceSettings.FillStyle of
    fsGradient:
      FFaceBitmap.FillEllipseLinearColorAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y, r, r, ColorToBGRA(FFaceSettings.ColorStart), ColorToBGRA(FFaceSettings.ColorEnd));
    fsnone:
      FFaceBitmap.FillEllipseAntialias(Origin.CenterPoint.x, Origin.CenterPoint.y, r, r, FFaceSettings.ColorStart);
  end;

end;

{ TDTNeedleSettings }

procedure TDTNeedleSettings.SetCapColor(AValue: TColor);
begin
  if FCapColor = AValue then
    Exit;
  FCapColor := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTNeedleSettings.SetCapEdgeColor(AValue: TColor);
begin
  if FCapEdgeColor = AValue then
    Exit;
  FCapEdgeColor := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTNeedleSettings.SetCapRadius(AValue: integer);
begin
  if FCapRadius = AValue then
    Exit;
  FCapRadius := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTNeedleSettings.SetNeedleColor(AValue: TColor);
begin
  if FNeedleColor = AValue then
    Exit;
  FNeedleColor := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTNeedleSettings.SetNeedleLength(AValue: integer);
begin
  if FNeedleLength = AValue then
    Exit;
  FNeedleLength := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTNeedleSettings.SetNeedleStyle(AValue: TDTNeedleStyle);
begin
  if FNeedleStyle = AValue then
    Exit;
  FNeedleStyle := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTNeedleSettings.SetOnChange(AValue: TNotifyEvent);
begin
//#  if FOnChange = AValue then
//#    Exit;
  FOnChange := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TDTNeedleSettings.Create;
begin
  FCapColor := BGRA(63, 63, 63);
  FCapEdgeColor := BGRA(220, 220, 204);
  FCapRadius := 10;

  FNeedleStyle := nsLine;
  FNeedleColor := BGRA(255, 81, 81);
  FNeedleLength := 100;
end;

destructor TDTNeedleSettings.Destroy;
begin
  inherited Destroy;
end;

{ TDTScaleSettings }

constructor TDTScaleSettings.Create;
begin
  FTickColor := bgra(223, 196, 125);
  FTextColor := bgra(140, 208, 211);
  FTextFont := 'Calibri';
  FTextSize := 15;
  FEnableMainTicks := True;
  FEnableSubTicks := True;
  FEnableScaleText := True;
  FMainTickCount := 10;
  FSubTickCount := 5;
  FMaximum := 100;
  FMinimum := 0;
  FLengthMainTick := 15;
  FLengthSubTick := 8;
  FThicknessMainTick := 3;
  FThicknessSubTick := 1;
  FAngle := 300;
  FEnableRangeIndicator := True;
end;

destructor TDTScaleSettings.Destroy;
begin
  inherited Destroy;
end;

procedure TDTScaleSettings.SetTextFont(AValue: string);
begin
  if FTextFont = AValue then
    Exit;
  FTextFont := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTScaleSettings.SetEnableScaleText(AValue: boolean);
begin
  if FEnableScaleText = AValue then
    Exit;
  FEnableScaleText := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTScaleSettings.SetFAngle(AValue: integer);
begin
  if FAngle=AValue then Exit;
  FAngle:=AValue;
end;

procedure TDTScaleSettings.SetMaximum(AValue: integer);
begin
  if FMaximum = AValue then
    Exit;
  FMaximum := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTScaleSettings.SetTesting(AValue: boolean);
begin
  if FTesting = AValue then
    Exit;
  FTesting := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTScaleSettings.SetTextRadius(AValue: integer);
begin
  if FTextRadius = AValue then
    Exit;
  FTextRadius := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTScaleSettings.SetTextSize(AValue: integer);
begin
  if FTextSize = AValue then
    Exit;
  FTextSize := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTScaleSettings.SetTickColor(AValue: TColor);
begin
  if FTickColor = AValue then
    Exit;
  FTickColor := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTScaleSettings.SetEnableMainTicks(AValue: boolean);
begin
  if FEnableMainTicks = AValue then
    Exit;
  FEnableMainTicks := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTScaleSettings.SetEnableRangeIndicator(AValue: boolean);
begin
  if FEnableRangeIndicator = AValue then
    Exit;
  FEnableRangeIndicator := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTScaleSettings.SetEnableSubTicks(AValue: boolean);
begin
  if FEnableSubTicks = AValue then
    Exit;
  FEnableSubTicks := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTScaleSettings.SetLengthMainTick(AValue: integer);
begin
  if FLengthMainTick = AValue then
    Exit;
  FLengthMainTick := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTScaleSettings.SetLengthSubTick(AValue: integer);
begin
  if FLengthSubTick = AValue then
    Exit;
  FLengthSubTick := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTScaleSettings.SetMainTickCount(AValue: integer);
begin
  if FMainTickCount = AValue then
    Exit;
  FMainTickCount := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTScaleSettings.SetOnChange(AValue: TNotifyEvent);
begin
//#  if FOnChange = AValue then
//#    Exit;
  FOnChange := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTScaleSettings.SetSubTickCount(AValue: integer);
begin
  if FSubTickCount = AValue then
    Exit;
  FSubTickCount := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTScaleSettings.SetTextColor(AValue: TColor);
begin
  if FTextColor = AValue then
    Exit;
  FTextColor := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTScaleSettings.SetThicknessMainTick(AValue: integer);
begin
  if FThicknessMainTick = AValue then
    Exit;
  FThicknessMainTick := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTScaleSettings.SetThicknessSubTick(AValue: integer);
begin
  if FThicknessSubTick = AValue then
    Exit;
  FThicknessSubTick := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TDTFaceSettings }

constructor TDTFaceSettings.Create;
begin
  FColorFrame := BGRA(35, 35, 35);
  FColorStart := BGRA(63, 63, 63);
  FColorEnd := BGRA(93, 93, 93);
  FFillStyle := fsGradient;
end;

destructor TDTFaceSettings.Destroy;
begin
  inherited Destroy;
end;

procedure TDTFaceSettings.SetColorEnd(AValue: TColor);
begin
  if FColorEnd = AValue then
    Exit;
  FColorEnd := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTFaceSettings.SetColorFrame(AValue: TColor);
begin
  if FColorFrame = AValue then
    Exit;
  FColorFrame := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTFaceSettings.SetColorStart(AValue: TColor);
begin
  if FColorStart = AValue then
    Exit;
  FColorStart := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTFaceSettings.SetFillStyle(AValue: TDTFillStyle);
begin
  if FFillStyle = AValue then
    Exit;
  FFillStyle := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDTFaceSettings.SetOnChange(AValue: TNotifyEvent);
begin
//#  if FOnChange = AValue then
//3    Exit;
  FOnChange := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TDTBackGround }

end.
