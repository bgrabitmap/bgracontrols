unit bgragame;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, LCLType, BGRABitmap, BGRABitmapTypes,
  SysUtils, Forms, Controls;

const
  L1 = VK_LEFT;
  R1 = VK_RIGHT;
  U1 = VK_UP;
  D1 = VK_DOWN;
  L2 = VK_A;
  R2 = VK_D;
  U2 = VK_W;
  D2 = VK_S;

type
  TGameKey = (gkLeft, gkUp, gkRight, gkDown);

  PGameMap = ^TGameMap;

  TGameMap = record
    mapGraphics: string;
    mapProperty: string;
    mapSolve: string;
    mapPrevPos: string;
    mapKeyChar: char;
    mapSolidChar: char;
    mapWidth: integer;
    mapHeight: integer;
    blockWidth: integer;
    blockHeight: integer;
    background: TBGRABitmap;
    ballSphereMap: TBGRABitmap;
    ballPosition,ballSpeed: TPointF;
    ballPositionDefined: boolean;
    mouseDown: boolean;
    mousePos: TPointF;
    mapKeysDown: set of TGameKey;
  end;

procedure DrawMap(var map: TGameMap; bitmap: TBGRABitmap; redrawAll: boolean);
function ClickMapCell(var map: TGameMap; pos: TPoint): boolean;
procedure MouseDownMap(var map: TGameMap; pos: TPoint);
procedure MouseUpMap(var map: TGameMap; pos: TPoint);
function GetGameKey(var Key: word; invertLR, invertUD: boolean): TGameKey;
procedure KeyDownMap(var map: TGameMap; var Key: word);
procedure KeyUpMap(var map: TGameMap; var Key: word);
function KeyPressMapCell(var map: TGameMap; Key: TGameKey): boolean;
function KeyPressMapCell(var map: TGameMap; var Key: word): boolean;
function IsMapSolved(var map: TGameMap): boolean;

procedure ToggleFullScreen(Form: TForm; ARect: TRect);
procedure CenterControl(Control: TControl);
function CalculateAspectRatioH(const W1, H1, W2: integer): integer; //result H2
function CalculateAspectRatioW(const W1, H1, H2: integer): integer; //result W2
function CalculateDestRect(ImageW, ImageH, DestW, DestH: integer;
  Stretch, Proportional, Center: boolean): TRect;
procedure HighDPI(FromDPI: integer);
procedure ScaleDPI(Control: TControl; FromDPI: integer);
procedure ScaleAspectRatio(Control: TControl; OriginalParentW, OriginalParentH: integer);
procedure ScaleAspectRatio(Control: TControl; DestW, DestH: integer;
  Stretch, Proportional, Center: boolean);

implementation

uses BGRAGradients;

procedure DrawMap(var map: TGameMap; bitmap: TBGRABitmap; minx,miny,maxx,maxy: integer; fullbackground: boolean); forward;
procedure DrawBall(var map: TGameMap; bitmap: TBGRABitmap); forward;
procedure InvalidateMap(var map: TGameMap; ARect: TRect);
var minx,miny,maxx,maxy,xb,yb: integer;
begin
  minx := ARect.Left div map.blockWidth;
  maxx := (ARect.Right div map.blockWidth) +1;
  miny := ARect.top div map.blockHeight;
  maxy := (ARect.bottom div map.blockHeight) +1;
  if miny < 0 then miny :=0;
  if minx < 0 then minx := 0;
  if maxx > map.mapWidth-1 then maxx := map.mapWidth-1;
  if maxy > map.mapHeight-1 then maxy := map.mapHeight-1;
  for yb := miny to maxy do
    for xb := minx to maxx do
      map.mapPrevPos[xb+yb*map.mapWidth+1] := '1';
end;

const acceleration = 0.01;
      maxSpeed = 0.1;

procedure MoveBall(var map: TGameMap);
var speed: single;
  ix,iy,nix,niy: integer;

  dir: TPointF;
  dirlen: single;
begin
  if map.ballPositionDefined then
  begin
    if map.mouseDown then
    begin
      dir := map.mousePos-map.ballPosition;
      dirlen := sqrt(dir*dir);
      if dirlen > acceleration then dir *= acceleration/dirlen;
      map.ballSpeed += dir;
    end else
    begin
      if gkLeft in map.mapKeysDown then map.ballSpeed.x -= acceleration;
      if gkRight in map.mapKeysDown then map.ballSpeed.x += acceleration;
      if gkUp in map.mapKeysDown then map.ballSpeed.y -= acceleration;
      if gkDown in map.mapKeysDown then map.ballSpeed.y += acceleration;
    end;

    speed := sqrt(map.ballSpeed*map.ballSpeed);
    if speed > maxSpeed then map.ballSpeed *= maxSpeed/speed;
    ix := round(map.ballPosition.x);
    iy := round(map.ballPosition.y);
    map.ballPosition += map.ballSpeed;
    nix := round(map.ballPosition.x);
    niy := round(map.ballPosition.y);
    if nix > ix then
    begin
      if not KeyPressMapCell(map, gkRight) then
      begin
        map.ballPosition.X := ix+0.499;
        map.ballSpeed.X := -abs(map.ballSpeed.X);
      end;
    end;
    if nix < ix then
    begin
      if not KeyPressMapCell(map, gkLeft) then
      begin
        map.ballPosition.X := ix-0.499;
        map.ballSpeed.X := abs(map.ballSpeed.X);
      end;
    end;
    if niy > iy then
    begin
      if not KeyPressMapCell(map, gkDown) then
      begin
        map.ballPosition.y := iy+0.499;
        map.ballSpeed.y := -abs(map.ballSpeed.y);
      end;
    end;
    if niy < iy then
    begin
      if not KeyPressMapCell(map, gkUp) then
      begin
        map.ballPosition.y := iy-0.499;
        map.ballSpeed.y := abs(map.ballSpeed.y);
      end;
    end;
  end;
end;

procedure DrawMap(var map: TGameMap; bitmap: TBGRABitmap; redrawAll: boolean);
begin
  MoveBall(map);
  if redrawAll then
    DrawMap(map,bitmap,0,0,map.mapWidth-1,map.mapHeight-1,true)
  else
    DrawMap(map,bitmap,0,0,map.mapWidth-1,map.mapHeight-1,false);
  DrawBall(map, bitmap);
end;

procedure DrawBall(var map: TGameMap; bitmap: TBGRABitmap);
var phong: TPhongShading;
  radius,xi,yi: integer;
  r: TRect;
begin
  if map.ballPositionDefined then
  begin
    phong := TPhongShading.Create;
    phong.LightPosition := point(bitmap.Width div 2, bitmap.Height div 2);
    radius := map.blockWidth;
    if map.blockHeight < radius then radius := map.blockHeight;
    radius := round(radius*0.3);
    xi := round((map.ballPosition.X+0.5)*map.blockWidth);
    yi := round((map.ballPosition.Y+0.5)*map.blockHeight);
    r := rect(xi-radius,yi-radius,xi+radius+1,yi+radius+1);
    if (map.ballSphereMap <> nil) and ((map.ballSphereMap.Width <> r.right-r.left) or (map.ballSphereMap.Height <> r.bottom-r.top)) then
      FreeAndNil(map.ballSphereMap);
    if map.ballSphereMap = nil then map.ballSphereMap := CreateSpherePreciseMap(r.right-r.left,r.bottom-r.top);
    phong.Draw(bitmap,map.ballSphereMap,radius,r.left,r.top,CSSLightGray);
    phong.Free;
    InvalidateMap(map, rect(xi-radius,yi-radius,xi+radius+1,yi+radius+1));
  end;
end;

procedure DrawMap(var map: TGameMap; bitmap: TBGRABitmap; minx,miny,maxx,maxy: integer; fullbackground: boolean);
var
  n, nx, ny: integer;
  r: TRect;
  colorOscillation: single;
  oldClip: TRect;
  shouldRedraw: boolean;

begin
  colorOscillation:= sin(frac(Now)*24*60*60 * 2*Pi)*0.5+0.5;

  if minx < 0 then minx := 0;
  if miny < 0 then miny := 0;
  if maxx > map.mapWidth-1 then maxx := map.mapWidth-1;
  if maxy > map.mapHeight-1 then maxy := map.mapHeight-1;

  oldClip := bitmap.ClipRect;
  bitmap.ClipRect := rect(map.blockWidth * minx, map.blockHeight * miny,
    map.blockWidth * (maxx+1), map.blockHeight * (maxy+1));
  if fullbackground then bitmap.PutImage(0, 0, map.background, dmSet);

  if map.mapPrevPos = '' then
  begin
    setlength(map.mapPrevPos, map.mapWidth*map.mapHeight);
    for n := 1 to length(map.mapPrevPos) do
      map.mapPrevPos[n] := '0';
  end;

  r.Top := map.blockHeight * miny;
  r.Bottom := r.Top + map.blockHeight;
  for ny := miny to maxy do
  begin
    r.Left := map.blockWidth * minx;
    r.Right := r.Left + map.blockWidth;
    n := ny*map.mapWidth+minx+1;
    for nx := minx to maxx do
    begin
      shouldRedraw:= (map.mapProperty[n] = map.mapKeyChar) or (map.mapGraphics[n] = 'X');
      if fullbackground or shouldRedraw or (map.mapPrevPos[n] = '1') then
      begin
        if shouldRedraw then map.mapPrevPos[n] := '1' else map.mapPrevPos[n] := '0';
        // begin to draw here
        if not fullbackground then bitmap.PutImagePart(r.left, r.top, map.background, r, dmSet);

        if map.mapProperty[n] = map.mapKeyChar then
        begin
          bitmap.Rectangle(r, BGRAPixelTransparent, BGRA(0, 255, 0, round(200*colorOscillation)),
            dmDrawWithTransparency);
          if not map.ballPositionDefined then
          begin
            map.ballPosition := pointF(nx,ny);
            map.ballPositionDefined := true;
          end;
        end
        else if map.mapProperty[n] = map.mapSolidChar then
          bitmap.Rectangle(r, BGRAPixelTransparent, BGRA(0, 0, 0, 100),
            dmDrawWithTransparency)
        else
          bitmap.Rectangle(r, BGRAPixelTransparent, BGRA(255, 255, 255, 200),
            dmDrawWithTransparency);

        if map.blockWidth > map.blockHeight then
          bitmap.FontHeight := map.blockHeight
        else
          bitmap.FontHeight := map.blockWidth;

        if map.mapGraphics[n] = 'X' then
          bitmap.Rectangle(r, BGRAPixelTransparent, BGRA(255, 0, 0, round(200*colorOscillation)),
            dmDrawWithTransparency);
        // end to draw here
      end;
      Inc(n);
      inc(r.Left,map.blockWidth);
      inc(r.right,map.blockWidth);
    end;
    inc(r.top,map.blockHeight);
    inc(r.bottom,map.blockHeight);
  end;
  bitmap.ClipRect := oldClip;
end;

function ClickMapCell(var map: TGameMap; pos: TPoint): boolean;
var
  n, nx, ny: integer;
  r: TRect;
begin
  Result := False;
  n := 1;
  for ny := 0 to map.mapHeight - 1 do
  begin
    for nx := 0 to map.mapWidth - 1 do
    begin
      r.Left := map.blockWidth * nx;
      r.Top := map.blockHeight * ny;
      r.Right := r.Left + map.blockWidth;
      r.Bottom := r.Top + map.blockHeight;

      if (pos.x >= r.Left) and (pos.x <= r.Right) and (pos.y >= r.Top) and
        (pos.y <= r.Bottom) then
      begin
        // begin here

        // mapKeyChar is n
        if (map.mapProperty[n] = map.mapKeyChar) then
          exit;

        // mapSolidChar is n
        if (map.mapProperty[n] = map.mapSolidChar) then
          exit;

        // mapKeyChar is on the left, move to n
        if (nx <> 0) then
          if (map.mapProperty[n - 1] = map.mapKeyChar) then
          begin
            map.mapProperty[n - 1] := map.mapProperty[n];
            map.mapProperty[n] := map.mapKeyChar;
          end;

        // mapKeyChar is on the right, move to n
        if (nx <> map.mapWidth - 1) then
          if (map.mapProperty[n + 1] = map.mapKeyChar) then
          begin
            map.mapProperty[n + 1] := map.mapProperty[n];
            map.mapProperty[n] := map.mapKeyChar;
          end;

        // mapKeyChar is on top, move to n
        if (map.mapProperty[n - map.mapWidth] = map.mapKeyChar) then
        begin
          map.mapProperty[n - map.mapWidth] := map.mapProperty[n];
          map.mapProperty[n] := map.mapKeyChar;
        end;

        // mapKeyChar is on bottom, move to n
        if (map.mapProperty[n + map.mapWidth] = map.mapKeyChar) then
        begin
          map.mapProperty[n + map.mapWidth] := map.mapProperty[n];
          map.mapProperty[n] := map.mapKeyChar;
        end;

        // end here
        Result := True;
        exit;
      end;

      Inc(n);
    end;
  end;
end;

procedure MouseDownMap(var map: TGameMap; pos: TPoint);
begin
  map.mouseDown := true;
  map.mousePos := pointf(pos.x/map.blockWidth-0.5,pos.y/map.blockHeight-0.5);
end;

procedure MouseUpMap(var map: TGameMap; pos: TPoint);
begin
  map.mouseDown := false;
  map.mousePos := pointf(pos.x/map.blockWidth-0.5,pos.y/map.blockHeight-0.5);
end;

function GetGameKey(var Key: word; invertLR, invertUD: boolean): TGameKey;
begin
  if (Key = L1) or (Key = L2) then
    Result := gkLeft;
  if (Key = R1) or (Key = R2) then
    Result := gkRight;
  if (Key = U1) or (Key = U2) then
    Result := gkUp;
  if (Key = D1) or (Key = D2) then
    Result := gkDown;

  if invertLR then
    case Result of
      gkLeft: Result := gkRight;
      gkRight: Result := gkLeft;
    end;

  if invertUD then
    case Result of
      gkUp: Result := gkDown;
      gkDown: Result := gkUp;
    end;
end;

procedure KeyDownMap(var map: TGameMap; var Key: word);
begin
  map.mapKeysDown := map.mapKeysDown + [GetGameKey(Key,false,false)];
end;

procedure KeyUpMap(var map: TGameMap; var Key: word);
begin
  map.mapKeysDown := map.mapKeysDown - [GetGameKey(Key,false,false)];
end;

function KeyPressMapCell(var map: TGameMap; Key: TGameKey): boolean;
var
  n, nx, ny: integer;
begin
  Result := False;
  n := 1;
  for ny := 0 to map.mapHeight - 1 do
  begin
    for nx := 0 to map.mapWidth - 1 do
    begin
      if map.mapProperty[n] = map.mapKeyChar then
      begin

        // begin here

        case Key of
          gkLeft:
            if (nx <> 0) and (map.mapProperty[n - 1] <> map.mapSolidChar) then
            begin
              map.mapProperty[n] := map.mapProperty[n - 1];
              map.mapProperty[n - 1] := map.mapKeyChar;
              Result := True;
            end;

          gkRight:
            if (nx <> map.mapWidth - 1) and (map.mapProperty[n + 1] <>
              map.mapSolidChar) then
            begin
              map.mapProperty[n] := map.mapProperty[n + 1];
              map.mapProperty[n + 1] := map.mapKeyChar;
              Result := True;
            end;

          gkUp:
            if (ny <> 0) and (map.mapProperty[n - map.mapWidth] <> map.mapSolidChar) then
            begin
              map.mapProperty[n] := map.mapProperty[n - map.mapWidth];
              map.mapProperty[n - map.mapWidth] := map.mapKeyChar;
              Result := True;
            end;

          gkDown:
            if (ny <> map.mapHeight - 1) and
              (map.mapProperty[n + map.mapHeight] <> map.mapSolidChar) then
            begin
              map.mapProperty[n] := map.mapProperty[n + map.mapWidth];
              map.mapProperty[n + map.mapWidth] := map.mapKeyChar;
              Result := True;
            end;
        end;

        // end here

        if Result then
          exit;

      end;
      Inc(n);
    end;
  end;
end;

function KeyPressMapCell(var map: TGameMap; var Key: word): boolean;
begin
  Result := KeyPressMapCell(map, GetGameKey(Key, False, False));
  if result then Key := 0;
end;

function IsMapSolved(var map: TGameMap): boolean;
begin
  Result := map.mapProperty = map.mapSolve;
end;

procedure ToggleFullScreen(Form: TForm; ARect: TRect);
begin
  Form.SetBounds(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  if Form.BorderStyle <> bsNone then
  begin
    Form.BorderStyle := bsNone;
    Form.WindowState := wsMaximized;
  end
  else
  begin
    Form.BorderStyle := bsSizeable;
    Form.WindowState := wsMaximized;
  end;
end;

procedure CenterControl(Control: TControl);
begin
  if not Control.HasParent then
    Exit;
  Control.SetBounds(
    Round((Control.Parent.Width - Control.Width) div 2),
    Round((Control.Parent.Height - Control.Height) div 2),
    Control.Width, Control.Height);
end;

function CalculateAspectRatioH(const W1, H1, W2: integer): integer;
begin
  Result := Round(H1 / W1 * W2);
end;

function CalculateAspectRatioW(const W1, H1, H2: integer): integer;
begin
  Result := Round(W1 / H1 * H2);
end;

function CalculateDestRect(ImageW, ImageH, DestW, DestH: integer;
  Stretch, Proportional, Center: boolean): TRect;
var
  w: integer;
  h: integer;
begin
  // Stretch or Proportional when Image (Width or Height) is bigger than Destination
  if Stretch or (Proportional and ((ImageW > DestW) or (ImageH > DestH))) then
  begin
    // Proportional when Image (Width or Height) is bigger than 0
    if Proportional and (ImageW > 0) and (ImageH > 0) then
    begin
      w := DestW;
      h := CalculateAspectRatioH(ImageW, ImageH, DestW);
      if h > DestH then
      begin
        h := DestH;
        w := CalculateAspectRatioW(ImageW, ImageH, DestH);
      end;
      ImageW := w;
      ImageH := h;
    end
    // Stretch not Proportional or when Image (Width or Height) is 0
    else
    begin
      ImageW := DestW;
      ImageH := DestH;
    end;
  end;

  Result := Rect(0, 0, ImageW, ImageH);

  // Center: Destination (Width or Height) - Image divided by 2
  if Center then
  begin
    Result.Left := Round((DestW - ImageW) div 2);
    Result.Top := Round((DestH - ImageH) div 2);
  end;
end;

procedure HighDPI(FromDPI: integer);
var
  i: integer;
begin
  if Screen.PixelsPerInch = FromDPI then
    exit;

  for i := 0 to Screen.FormCount - 1 do
    ScaleDPI(Screen.Forms[i], FromDPI);
end;

procedure ScaleDPI(Control: TControl; FromDPI: integer);
var
  i: integer;
  WinControl: TWinControl;
begin
  if Screen.PixelsPerInch = FromDPI then
    exit;

  with Control do
  begin
    Left := ScaleX(Left, FromDPI);
    Top := ScaleY(Top, FromDPI);
    Width := ScaleX(Width, FromDPI);
    Height := ScaleY(Height, FromDPI);
  end;

  if Control is TWinControl then
  begin
    WinControl := TWinControl(Control);
    if WinControl.ControlCount = 0 then
      exit;

    with WinControl.ChildSizing do
    begin
      HorizontalSpacing := ScaleX(HorizontalSpacing, FromDPI);
      LeftRightSpacing := ScaleX(LeftRightSpacing, FromDPI);
      TopBottomSpacing := ScaleY(TopBottomSpacing, FromDPI);
      VerticalSpacing := ScaleY(VerticalSpacing, FromDPI);
    end;

    for i := 0 to WinControl.ControlCount - 1 do
      ScaleDPI(WinControl.Controls[i], FromDPI);
  end;
end;

procedure ScaleAspectRatio(Control: TControl; OriginalParentW, OriginalParentH: integer);
var
  l, t, w, h: integer;
begin
  l := MulDiv(Control.Left, Control.Parent.Width, OriginalParentW);
  t := MulDiv(Control.Top, Control.Parent.Height, OriginalParentH);
  w := MulDiv(Control.Width, Control.Parent.Width, OriginalParentW);
  h := MulDiv(Control.Height, Control.Parent.Height, OriginalParentH);
  Control.SetBounds(l, t, w, h);
end;

procedure ScaleAspectRatio(Control: TControl; DestW, DestH: integer;
  Stretch, Proportional, Center: boolean);
var
  i: integer;
  r: TRect;
  WinControl: TWinControl;
  w, h: integer;
begin
  if Control is TWinControl then
  begin
    WinControl := TWinControl(Control);
    w := WinControl.Width;
    h := WinControl.Height;

    r := CalculateDestRect(WinControl.Width, WinControl.Height, DestW,
      DestH, Stretch, Proportional, Center);
    WinControl.SetBounds(r.Left, r.Top, r.Right, r.Bottom);

    if WinControl.ControlCount = 0 then
      exit;

    for i := 0 to WinControl.ControlCount - 1 do
      ScaleAspectRatio(WinControl.Controls[i], w, h);
  end;
end;

end.
