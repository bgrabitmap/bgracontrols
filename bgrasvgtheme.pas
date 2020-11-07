unit BGRASVGTheme;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRATheme, BGRABitmap, BGRABitmapTypes, BGRASVG, BGRASVGType;

type

  { TBGRASVGTheme }

  TBGRASVGTheme = class(TBGRATheme)
  private
    FButtonActive: string;
    FButtonHover: string;
    FButtonNormal: string;
    FButtonSliceScalingBottom: integer;
    FButtonSliceScalingLeft: integer;
    FButtonSliceScalingRight: integer;
    FButtonSliceScalingTop: integer;
    FCheckBoxChecked: string;
    FCheckBoxUnchecked: string;
    FColorizeActive: TBGRAPixel;
    FColorizeDisabled: TBGRAPixel;
    FColorizeHover: TBGRAPixel;
    FColorizeNormal: TBGRAPixel;
    FRadioButtonChecked: string;
    FRadioButtonUnchecked: string;
    procedure SetButtonActive(AValue: string);
    procedure SetButtonHover(AValue: string);
    procedure SetButtonNormal(AValue: string);
    procedure SetButtonSliceScalingBottom(AValue: integer);
    procedure SetButtonSliceScalingLeft(AValue: integer);
    procedure SetButtonSliceScalingRight(AValue: integer);
    procedure SetButtonSliceScalingTop(AValue: integer);
    procedure SetCheckBoxChecked(AValue: string);
    procedure SetCheckBoxUnchecked(AValue: string);
    procedure SetColorizeActive(AValue: TBGRAPixel);
    procedure SetColorizeDisabled(AValue: TBGRAPixel);
    procedure SetColorizeHover(AValue: TBGRAPixel);
    procedure SetColorizeNormal(AValue: TBGRAPixel);
    procedure SetRadioButtonChecked(AValue: string);
    procedure SetRadioButtonUnchecked(AValue: string);

  protected
    procedure CheckEmptyResourceException(const aResource: String);
    procedure SliceScalingDraw(const Source: TBGRASVG;
      const marginLeft, marginTop, marginRight, marginBottom: integer;
      const Dest: TBGRABitmap; DestDPI: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure DrawButton(Caption: string; State: TBGRAThemeButtonState;
      Focused: boolean; ARect: TRect; ASurface: TBGRAThemeSurface); override;
    procedure DrawRadioButton(Caption: string; State: TBGRAThemeButtonState;
    {%H-}Focused: boolean; Checked: boolean; ARect: TRect;
      ASurface: TBGRAThemeSurface); override;
    procedure DrawCheckBox(Caption: string; State: TBGRAThemeButtonState;
    {%H-}Focused: boolean; Checked: boolean; ARect: TRect;
      ASurface: TBGRAThemeSurface); override;
  public
    property ColorizeNormal: TBGRAPixel read FColorizeNormal write SetColorizeNormal;
    property ColorizeHover: TBGRAPixel read FColorizeHover write SetColorizeHover;
    property ColorizeActive: TBGRAPixel read FColorizeActive write SetColorizeActive;
    property ColorizeDisabled: TBGRAPixel read FColorizeDisabled
      write SetColorizeDisabled;
  published
    property CheckBoxUnchecked: string read FCheckBoxUnchecked
      write SetCheckBoxUnchecked;
    property CheckBoxChecked: string read FCheckBoxChecked write SetCheckBoxChecked;
    property RadioButtonUnchecked: string read FRadioButtonUnchecked
      write SetRadioButtonUnchecked;
    property RadioButtonChecked: string read FRadioButtonChecked
      write SetRadioButtonChecked;
    property ButtonNormal: string read FButtonNormal write SetButtonNormal;
    property ButtonHover: string read FButtonHover write SetButtonHover;
    property ButtonActive: string read FButtonActive write SetButtonActive;
    property ButtonSliceScalingLeft: integer
      read FButtonSliceScalingLeft write SetButtonSliceScalingLeft;
    property ButtonSliceScalingTop: integer
      read FButtonSliceScalingTop write SetButtonSliceScalingTop;
    property ButtonSliceScalingRight: integer
      read FButtonSliceScalingRight write SetButtonSliceScalingRight;
    property ButtonSliceScalingBottom: integer
      read FButtonSliceScalingBottom write SetButtonSliceScalingBottom;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Themes', [TBGRASVGTheme]);
end;

{ TBGRASVGTheme }

procedure TBGRASVGTheme.SetCheckBoxUnchecked(AValue: string);
begin
  if FCheckBoxUnchecked = AValue then
    Exit;
  CheckEmptyResourceException(AValue);
  FCheckBoxUnchecked := AValue;
end;

procedure TBGRASVGTheme.SetColorizeActive(AValue: TBGRAPixel);
begin
  if FColorizeActive = AValue then
    Exit;
  FColorizeActive := AValue;
end;

procedure TBGRASVGTheme.SetColorizeDisabled(AValue: TBGRAPixel);
begin
  if FColorizeDisabled = AValue then
    Exit;
  FColorizeDisabled := AValue;
end;

procedure TBGRASVGTheme.SetColorizeHover(AValue: TBGRAPixel);
begin
  if FColorizeHover = AValue then
    Exit;
  FColorizeHover := AValue;
end;

procedure TBGRASVGTheme.SetColorizeNormal(AValue: TBGRAPixel);
begin
  if FColorizeNormal = AValue then
    Exit;
  FColorizeNormal := AValue;
end;

procedure TBGRASVGTheme.SetRadioButtonChecked(AValue: string);
begin
  if FRadioButtonChecked = AValue then
    Exit;
  CheckEmptyResourceException(AValue);
  FRadioButtonChecked := AValue;
end;

procedure TBGRASVGTheme.SetRadioButtonUnchecked(AValue: string);
begin
  if FRadioButtonUnchecked = AValue then
    Exit;
  CheckEmptyResourceException(AValue);
  FRadioButtonUnchecked := AValue;
end;

procedure TBGRASVGTheme.CheckEmptyResourceException(const aResource: String);
begin
  if (aResource.IsEmpty) then
    raise Exception.Create('Resource must not be empty.');
end;

procedure TBGRASVGTheme.SliceScalingDraw(const Source: TBGRASVG;
  const marginLeft, marginTop, marginRight, marginBottom: integer;
  const Dest: TBGRABitmap; DestDPI: integer);
var
  svgBox: TSVGViewBox;
  svgTopLeft, svgBottomRight: TPointF;
  sourcePosX, sourcePosY: array[1..4] of single;
  destPosX, destPosY: array[1..4] of integer;
  y, x: integer;

  procedure DrawPart(sourceRect: TRectF; destRect: TRect);
  var
    zoom: TPointF;
  begin
    if sourceRect.IsEmpty or destRect.IsEmpty then
      exit;
    dest.ClipRect := destRect;
    zoom := PointF(destRect.Width / sourceRect.Width, destRect.Height /
      sourceRect.Height);
    Source.Draw(dest.Canvas2D, -sourceRect.Left * zoom.x + destRect.Left,
      -sourceRect.Top * zoom.y + destRect.Top, Source.DefaultDpi * zoom);
  end;

begin
  svgBox := Source.ViewBoxInUnit[cuPixel];
  svgTopLeft := svgBox.min;
  svgBottomRight := svgBox.min + svgBox.size;

  sourcePosX[1] := svgTopLeft.x;
  sourcePosX[2] := svgTopLeft.x + marginLeft;
  sourcePosX[3] := svgBottomRight.x - marginRight;
  sourcePosX[4] := svgBottomRight.x;
  sourcePosY[1] := svgTopLeft.y;
  sourcePosY[2] := svgTopLeft.y + marginTop;
  sourcePosY[3] := svgBottomRight.y - marginBottom;
  sourcePosY[4] := svgBottomRight.y;
  if sourcePosX[2] > sourcePosX[3] then
  begin
    sourcePosX[2] := (sourcePosX[1] + sourcePosX[4]) / 2;
    sourcePosX[3] := sourcePosX[2];
  end;
  if sourcePosY[2] > sourcePosY[3] then
  begin
    sourcePosY[2] := (sourcePosY[1] + sourcePosY[4]) / 2;
    sourcePosY[3] := sourcePosY[2];
  end;

  destPosX[1] := 0;
  destPosX[2] := round(marginLeft * DestDPI / 96);
  destPosX[3] := dest.Width - round(marginRight * DestDPI / 96);
  destPosX[4] := dest.Width;
  destPosY[1] := 0;
  destPosY[2] := round(marginTop * DestDPI / 96);
  destPosY[3] := dest.Height - round(marginBottom * DestDPI / 96);
  destPosY[4] := dest.Height;
  if destPosX[2] > destPosX[3] then
  begin
    destPosX[2] := round((destPosX[1] + destPosX[4]) / 2);
    destPosX[3] := destPosX[2];
  end;
  if destPosY[2] > destPosY[3] then
  begin
    destPosY[2] := round((destPosY[1] + destPosY[4]) / 2);
    destPosY[3] := destPosY[2];
  end;

  for y := 1 to 3 do
    for x := 1 to 3 do
      DrawPart(RectF(sourcePosX[x], sourcePosY[y], sourcePosX[x + 1], sourcePosY[y + 1]),
        Rect(destPosX[x], destPosY[y], destPosX[x + 1], destPosY[y + 1]));
  Dest.NoClip;
end;

constructor TBGRASVGTheme.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // https://material.io/resources/icons/
  FCheckBoxUnchecked :=
    '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M19 5v14H5V5h14m0-2H5c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h14c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2z"/></svg>';
  FCheckBoxChecked :=
    '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M19 3H5c-1.11 0-2 .9-2 2v14c0 1.1.89 2 2 2h14c1.11 0 2-.9 2-2V5c0-1.1-.89-2-2-2zm-9 14l-5-5 1.41-1.41L10 14.17l7.59-7.59L19 8l-9 9z"/></svg>';
  FRadioButtonUnchecked :=
    '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.42 0-8-3.58-8-8s3.58-8 8-8 8 3.58 8 8-3.58 8-8 8z"/></svg>';
  FRadioButtonChecked :=
    '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M12 7c-2.76 0-5 2.24-5 5s2.24 5 5 5 5-2.24 5-5-2.24-5-5-5zm0-5C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.42 0-8-3.58-8-8s3.58-8 8-8 8 3.58 8 8-3.58 8-8 8z"/></svg>';
  // Button
  FButtonNormal :=
    '<?xml version="1.0" encoding="UTF-8" standalone="no"?><svg   xmlns:dc="http://purl.org/dc/elements/1.1/"   xmlns:cc="http://creativecommons.org/ns#"   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"   xmlns:svg="http://www.w3.org/2000/svg"   xmlns="http://www.w3.org/2000/svg"   xmlns:xlink="http://www.w3.org/1999/xlink"   xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"   width="32"   height="32"   viewBox="0 0 8.4666665 8.4666669"   version="1.1"   id="svg8"   inkscape:version="1.0.1 (3bc2e813f5, 2020-09-07)"   sodipodi:docname="button.svg">  <style     id="style833"></style>  <defs     id="defs2">    <linearGradient       inkscape:collect="always"       id="linearGradient858">      <stop         style="stop-color:#87cdde;stop-opacity:1"         offset="0"         id="stop854" />      <stop         style="stop-color:#ffffff;stop-opacity:1"         offset="1"         id="stop856" />    </linearGradient>    <linearGradient       inkscape:collect="always"       xlink:href="#linearGradient858"       id="linearGradient860"       x1="4.2333326"       y1="5.4568157"       x2="4.2333336"       y2="2.6458333"       gradientUnits="userSpaceOnUse" />  </defs>  <sodipodi:namedview     id="base"     pagecolor="#ffffff"     bordercolor="#666666"     borderopacity="1.0"     inkscape:pageopacity="0.0"     inkscape:pageshadow="2"     inkscape:zoom="1"     inkscape:cx="-159.25918"     inkscape:cy="-122.37605"     inkscape:document-units="px"     inkscape:current-layer="layer1"     inkscape:document-rotation="0"     showgrid="true"     units="px"     inkscape:window-width="1920"     inkscape:window-height="1017"     inkscape:window-x="-8"     inkscape:window-y="-8"     inkscape:window-maximized="1">    <inkscape:grid       type="xygrid"       id="grid837" />  </sodipodi:namedview>  <metadata     id="metadata5">    <rdf:RDF>      <cc:Work         rdf:about="">        <dc:format>image/svg+xml</dc:format>        <dc:type           rdf:resource="http://purl.org/dc/dcmitype/StillImage" />        <dc:title />      </cc:Work>    </rdf:RDF>  </metadata>  <g     inkscape:label="Capa 1"     inkscape:groupmode="layer"     id="layer1">    <path       vectorEffect="non-scaling-stroke"       id="rect835"       style="fill:#000000;stroke:#002255;stroke-width:1;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1"       d="m 2.7400001,0.50000001 h 2.9866666 c 1.2409599,0 2.24,0.99903999 2.24,2.24000009 v 2.9866668 c 0,1.2409598 -0.9990401,2.24 -2.24,2.24 H 2.7400001 C 1.4990401,7.9666669 0.5,6.9676267 0.5,5.7266669 V 2.7400001 C 0.5,1.49904 1.4990401,0.50000001 2.7400001,0.50000001 Z" />    <path       vectorEffect="non-scaling-stroke"       id="rect835-8"       style="fill:url(#linearGradient860);fill-opacity:1;stroke:none;stroke-width:0.999996;stroke-linecap:butt;stroke-miterlimit:4;stroke-dasharray:none;paint-order:markers stroke fill"       d="m 2.6458334,0.52916666 h 3.175 c 1.1726329,0 2.1166662,0.94403314 2.1166662,2.11666644 v 3.1750002 c 0,1.1726334 -0.9440333,2.1166666 -2.1166662,2.1166666 h -3.175 c -1.1726332,0 -2.11666628,-0.9440332 -2.11666628,-2.1166666 V 2.6458331 c 0,-1.1726333 0.94403308,-2.11666644 2.11666628,-2.11666644 z" />  </g></svg>';
  FButtonHover := FButtonNormal;
  FButtonActive := FButtonNormal;
  FButtonSliceScalingLeft := 10;
  FButtonSliceScalingTop := 10;
  FButtonSliceScalingRight := 10;
  FButtonSliceScalingBottom := 10;
  // Colorize
  FColorizeNormal := BGRAPixelTransparent;
  FColorizeHover := ColorToBGRA(clWhite, 100);
  FColorizeActive := ColorToBGRA(clBlack, 100);
  FColorizeDisabled := ColorToBGRA(clGray, 200);
end;

destructor TBGRASVGTheme.Destroy;
begin
  inherited Destroy;
end;

procedure TBGRASVGTheme.DrawButton(Caption: string; State: TBGRAThemeButtonState;
  Focused: boolean; ARect: TRect; ASurface: TBGRAThemeSurface);
var
  Style: TTextStyle;
  svg: TBGRASVG;
  color: TBGRAPixel;
begin
  with ASurface do
  begin
    case State of
      btbsNormal: svg := TBGRASVG.CreateFromString(FButtonNormal);
      btbsHover: svg := TBGRASVG.CreateFromString(FButtonHover);
      btbsActive: svg := TBGRASVG.CreateFromString(FButtonActive);
      btbsDisabled: svg := TBGRASVG.CreateFromString(FButtonNormal);
    end;
    SliceScalingDraw(svg{%H-}, FButtonSliceScalingLeft, FButtonSliceScalingTop,
      FButtonSliceScalingRight, FButtonSliceScalingBottom, ASurface.Bitmap,
      Screen.PixelsPerInch);
    svg.Free;
    case State of
      btbsNormal: color := FColorizeNormal;
      btbsHover: color := FColorizeHover;
      btbsActive: color := FColorizeActive;
      btbsDisabled: color := FColorizeDisabled;
    end;
    BitmapColorOverlay(color{%H-});
    DrawBitmap;

    if Focused then
    begin
      DestCanvas.Pen.Color := clBlack;
      DestCanvas.Rectangle(ARect);
    end;

    if Caption <> '' then
    begin
      Style.Alignment := taCenter;
      Style.Layout := tlCenter;
      Style.Wordbreak := True;
      Style.SystemFont := False;
      Style.Clipping := True;
      Style.Opaque := False;
      DestCanvas.TextRect(ARect, 0, 0, Caption, Style);
    end;
  end;
end;

procedure TBGRASVGTheme.DrawRadioButton(Caption: string;
  State: TBGRAThemeButtonState; Focused: boolean; Checked: boolean;
  ARect: TRect; ASurface: TBGRAThemeSurface);
var
  Style: TTextStyle;
  svg: TBGRASVG;
  color: TBGRAPixel;
begin
  with ASurface do
  begin
    BitmapRect := RectWithSize(ARect.Left, ARect.Top, ARect.Height, ARect.Height);
    if Checked then
      svg := TBGRASVG.CreateFromString(FRadioButtonChecked)
    else
      svg := TBGRASVG.CreateFromString(FRadioButtonUnchecked);
    svg.StretchDraw(Bitmap.Canvas2D, 0, 0, Bitmap.Width, Bitmap.Height);
    svg.Free;
    case State of
      btbsNormal: color := FColorizeNormal;
      btbsHover: color := FColorizeHover;
      btbsActive: color := FColorizeActive;
      btbsDisabled: color := FColorizeDisabled;
    end;
    BitmapColorOverlay(color{%H-});
    DrawBitmap;

    if Caption <> '' then
    begin
      Style.Alignment := taLeftJustify;
      Style.Layout := tlCenter;
      Style.Wordbreak := True;
      Style.SystemFont := False;
      Style.Clipping := True;
      Style.Opaque := False;
      DestCanvas.TextRect(Rect(Arect.Height, 0, ARect.Right, ARect.Bottom),
        ARect.Height, 0, Caption, Style);
    end;
  end;
end;

procedure TBGRASVGTheme.SetCheckBoxChecked(AValue: string);
begin
  if FCheckBoxChecked = AValue then
    Exit;
  CheckEmptyResourceException(AValue);
  FCheckBoxChecked := AValue;
end;

procedure TBGRASVGTheme.SetButtonActive(AValue: string);
begin
  if FButtonActive = AValue then
    Exit;
  CheckEmptyResourceException(AValue);
  FButtonActive := AValue;
end;

procedure TBGRASVGTheme.SetButtonHover(AValue: string);
begin
  if FButtonHover = AValue then
    Exit;
  CheckEmptyResourceException(AValue);
  FButtonHover := AValue;
end;

procedure TBGRASVGTheme.SetButtonNormal(AValue: string);
begin
  if FButtonNormal = AValue then
    Exit;
  CheckEmptyResourceException(AValue);
  FButtonNormal := AValue;
end;

procedure TBGRASVGTheme.SetButtonSliceScalingBottom(AValue: integer);
begin
  if FButtonSliceScalingBottom = AValue then
    Exit;
  FButtonSliceScalingBottom := AValue;
end;

procedure TBGRASVGTheme.SetButtonSliceScalingLeft(AValue: integer);
begin
  if FButtonSliceScalingLeft = AValue then
    Exit;
  FButtonSliceScalingLeft := AValue;
end;

procedure TBGRASVGTheme.SetButtonSliceScalingRight(AValue: integer);
begin
  if FButtonSliceScalingRight = AValue then
    Exit;
  FButtonSliceScalingRight := AValue;
end;

procedure TBGRASVGTheme.SetButtonSliceScalingTop(AValue: integer);
begin
  if FButtonSliceScalingTop = AValue then
    Exit;
  FButtonSliceScalingTop := AValue;
end;

procedure TBGRASVGTheme.DrawCheckBox(Caption: string; State: TBGRAThemeButtonState;
  Focused: boolean; Checked: boolean; ARect: TRect; ASurface: TBGRAThemeSurface);
var
  Style: TTextStyle;
  svg: TBGRASVG;
  color: TBGRAPixel;
begin
  with ASurface do
  begin
    BitmapRect := RectWithSize(ARect.Left, ARect.Top, ARect.Height, ARect.Height);
    if Checked then
      svg := TBGRASVG.CreateFromString(FCheckBoxChecked)
    else
      svg := TBGRASVG.CreateFromString(FCheckBoxUnchecked);
    svg.StretchDraw(Bitmap.Canvas2D, 0, 0, Bitmap.Width, Bitmap.Height);
    svg.Free;
    case State of
      btbsNormal: color := FColorizeNormal;
      btbsHover: color := FColorizeHover;
      btbsActive: color := FColorizeActive;
      btbsDisabled: color := FColorizeDisabled;
    end;
    BitmapColorOverlay(color{%H-});
    DrawBitmap;

    if Caption <> '' then
    begin
      Style.Alignment := taLeftJustify;
      Style.Layout := tlCenter;
      Style.Wordbreak := True;
      Style.SystemFont := False;
      Style.Clipping := True;
      Style.Opaque := False;
      DestCanvas.TextRect(Rect(Arect.Height, 0, ARect.Right, ARect.Bottom),
        ARect.Height, 0, Caption, Style);
    end;
  end;
end;

end.
