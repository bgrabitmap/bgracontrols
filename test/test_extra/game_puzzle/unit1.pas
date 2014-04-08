{ Puzzle! is a puzzle game using BGRABitmap

  Copyright (C) 2012 lainz http://sourceforge.net/users/lainz

  Thanks to circular and eny from the Lazarus forum for their contributions.

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes, ugame, LCLType,
  ExtCtrls{$IFDEF WINDOWS},
  mmsystem{$ENDIF};

const
  // controls
  L1 = VK_LEFT;
  R1 = VK_RIGHT;
  U1 = VK_UP;
  D1 = VK_DOWN;
  L2 = VK_A;
  R2 = VK_D;
  U2 = VK_W;
  D2 = VK_S;

type
  TArrayOfString = array of string;

  TGameKey = (gkLeft, gkUp, gkRight, gkDown);

  PGameMap = ^TGameMap;

  TGameMap = record
    map, solve: string;
    mapW: integer;
    mapH: integer;
    blockW: integer;
    blockH: integer;
    background: TBGRABitmap;
  end;

var
  Game: array [1..12] of TGameMap;
  GameOver: TGameMap = (map: 'Game-Over'; solve: 'GameOver-'; mapW: 3;
  mapH: 3; blockW: 85; blockH: 85; background: nil);

procedure DrawMap(var map: TGameMap; bitmap: TBGRABitmap; texture: TBGRABitmap);
function ClickMap(var map: TGameMap; pos: TPoint): boolean;
function KeyPressMap(var map: TGameMap; var Key: word): boolean;
function IsMapSolved(var map: TGameMap): boolean;

function CalcularPosibles(str: string; showDebug: boolean = False): TarrayOfString;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Timer1: TTimer;
    procedure BGRAVirtualScreen1Click(Sender: TObject);
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Size: TRect;
    map: PGameMap;
    current: integer;
    texture: TBGRABitmap;
    procedure OpenMap(num: integer);
    procedure CloseMap;
    procedure ScaleBlocks;
    procedure GenerateTexture;
    procedure PlayBlockSound;
  end;

var
  Form1: TForm1;

implementation

uses BGRAGradients, ugraph;

{ DRAWMAP }

procedure DrawMap(var map: TGameMap; bitmap: TBGRABitmap; texture: TBGRABitmap);
var
  r: TRect;
  n, n1, n2: integer;
  phong: TPhongShading;
  tile3D, empty3D: TBGRABitmap;
  h: integer;
begin
  if (bitmap = nil) or (texture = nil) then
    exit;
  phong := TPhongShading.Create;

  if map.background = nil then
  begin
    map.background := TBGRABitmap.Create(map.BlockW * map.mapW, map.BlockH * map.mapH);
    empty3D := TBGRABitmap.Create(texture.Width, texture.Height, BGRABlack);
    for n1 := 1 to (map.background.Width + texture.Width - 1) div texture.Width do
      for n2 := 1 to (map.background.Height + texture.Height - 1) div texture.Height do
        phong.Draw(map.background, empty3D, 0, (n1 - 1) * texture.Width,
          (n2 - 1) * texture.Height, texture);
    empty3D.Free;
  end;
  h := (map.blockW + map.blockH) div 16;
  tile3D := CreateRectanglePreciseMap(map.BlockW, map.BlockH, h, []);

  bitmap.PutImage(0, 0, map.background, dmDrawWithTransparency);
  n1 := 0;
  n2 := 0;
  for n := 1 to length(map.map) do
  begin
    r.Left := map.blockW * n1;
    r.Top := map.blockH * n2;
    r.Right := r.Left + map.blockW;
    r.Bottom := r.Top + map.blockH;

    // begin to draw here //

    if map.map[n] <> '0' then
    begin
      phong.Draw(bitmap, tile3D, h, r.left, r.top, texture);

      if map.blockH > map.blockW then
        bitmap.FontHeight := round(map.blockW * 0.75)
      else
        bitmap.FontHeight := round(map.blockH * 0.75);

      bitmap.TextRect(Rect(r.Left + 10, r.Top + 10, r.Right, r.Bottom),
        map.map[n], taCenter, tlCenter, BGRA(0, 0, 0, 175));
      bitmap.TextRect(r, map.map[n], taCenter, tlCenter, BGRABlack);
    end;
   {
    if map.map[n] = '0' then
    begin
      bitmap.FontHeight := map.blockH div 4;
      if (n1 <> 0) then
        bitmap.TextRect(r, '►', taLeftJustify, tlCenter, BGRA(192, 192, 192));

      if (n1 <> map.mapW - 1) then
        bitmap.TextRect(r, '◄', taRightJustify, tlCenter, BGRA(192, 192, 192));

      if (n2 <> 0) then
        bitmap.TextRect(r, '▼', taCenter, tlTop, BGRA(192, 192, 192));

      if (n2 <> map.mapH - 1) then
        bitmap.TextRect(r, '▲', taCenter, tlBottom, BGRA(192, 192, 192));
    end;
    }

    // end to draw here //

    if n1 = map.mapW - 1 then
    begin
      n1 := -1;
      Inc(n2);
    end;

    Inc(n1);
  end;

  tile3D.Free;
  phong.Free;
end;

function ClickMap(var map: TGameMap; pos: TPoint): boolean;
var
  n, n1, n2: integer;
  r: TRect;
begin
  Result := False;
  n1 := 0;
  n2 := 0;

  for n := 1 to length(map.map) do
  begin
    r.Left := map.blockW * n1;
    r.Top := map.blockH * n2;
    r.Right := r.Left + map.blockW;
    r.Bottom := r.Top + map.blockH;

    { SCALING }
    r.Left += Form1.BGRAVirtualScreen1.Left;
    r.Top += Form1.BGRAVirtualScreen1.Top;
    r.Right += Form1.BGRAVirtualScreen1.Left;
    r.Bottom += Form1.BGRAVirtualScreen1.Top;

    if (pos.x >= r.Left) and (pos.x <= r.Right) and (pos.y >= r.Top) and
      (pos.y <= r.Bottom) then
    begin
      // vacio
      if map.map[n] = '0' then
        exit;

      // el vacio esta a la izquierda
      if (n1 <> 0) then
        if map.map[n - 1] = '0' then
        begin
          map.map[n - 1] := map.map[n];
          map.map[n] := '0';
        end;

      // el vacio esta a la derecha
      if (n1 <> map.mapW - 1) then
        if map.map[n + 1] = '0' then
        begin
          map.map[n + 1] := map.map[n];
          map.map[n] := '0';
        end;

      // el vacio esta arriba
      if map.map[n - map.mapW] = '0' then
      begin
        map.map[n - map.mapW] := map.map[n];
        map.map[n] := '0';
      end;

      // el vacio esta abajo
      if map.map[n + map.mapW] = '0' then
      begin
        map.map[n + map.mapW] := map.map[n];
        map.map[n] := '0';
      end;

      Result := True;
      exit;
    end;

    if n1 = map.mapW - 1 then
    begin
      n1 := -1;
      Inc(n2);
    end;

    Inc(n1);
  end;
end;

function KeyPressMap(var map: TGameMap; var Key: word): boolean;

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

var
  n, n1, n2: integer;
begin
  n1 := 0;
  n2 := 0;
  Result := False;

  for n := 1 to length(map.map) do
  begin
    if map.map[n] = '0' then
    begin

      case GetGameKey(Key, True, True) of
        gkLeft:
          // el de la izquierda
          if (n1 <> 0) then
          begin
            map.map[n] := map.map[n - 1];
            map.map[n - 1] := '0';
            Result := True;
            Key := 0;
          end;

        gkRight:
          // el de la derecha
          if (n1 <> map.mapW - 1) then
          begin
            map.map[n] := map.map[n + 1];
            map.map[n + 1] := '0';
            Result := True;
            Key := 0;
          end;

        gkUp:
          // el de arriba
          if (n2 <> 0) then
          begin
            map.map[n] := map.map[n - map.mapW];
            map.map[n - map.mapW] := '0';
            Result := True;
            Key := 0;
          end;

        gkDown:
          // el de abajo
          if (n2 <> map.mapH - 1) then
          begin
            map.map[n] := map.map[n + map.mapW];
            map.map[n + map.mapW] := '0';
            Result := True;
            Key := 0;
          end;

      end;

      if Result then
        exit;
    end;

    if n1 = map.mapW - 1 then
    begin
      n1 := -1;
      Inc(n2);
    end;

    Inc(n1);
  end;
end;

function IsMapSolved(var map: TGameMap): boolean;
begin
  Result := map.map = map.solve;
end;

function CalcularPosibles(str: string; showDebug: boolean = False): TarrayOfString;

  function Factorial(number: integer): integer;
  var
    i: integer;
  begin
    Result := number;
    for i := number - 1 downto 1 do
    begin
      Result := Result * i;
    end;
  end;

  function MoverIzq(str: string; pos: integer): string;
  var
    s1, s2: char;
  begin
    Result := str;
    s1 := Result[pos];
    s2 := Result[pos - 1];
    Result[pos] := s2;
    Result[pos - 1] := s1;
  end;

  function MoverIni(str: string; pos: integer): string;
  var
    s1, s2: char;
  begin
    Result := str;
    s1 := Result[pos];
    s2 := Result[1];
    Result[pos] := s2;
    Result[1] := s1;
  end;

var
  nLargo, // numero de char en la string
  nFactorial, // numero de combinaciones
  nFactorialDivLargo, // primer char veces repetido
  nFactorialm1DivLargom1, // segundo char veces repetido
  nPosibles: integer; // numero de combinaciones jugables
  n: integer;

  rstr: string;
begin
  // Los comentarios de los numeros son para str:=1230;

  nLargo := length(str); // 4

  if nLargo <= 1 then
    exit;

  nFactorial := Factorial(nLargo); // 24
  //nFactorialDivLargo := nFactorial div nLargo; // 6
  //nFactorialm1DivLargom1 := Factorial(nLargo - 1) div (nLargo - 1); // 2

  nPosibles := nFactorial div 2; // 12

  SetLength(Result, nPosibles);

  //0 to 11
  for n := 0 to nPosibles - 1 do
  begin
    // create a function here ;)

    // start with '1'
    if n = 0 then
      Result[n] := str // 1230 (no clic)
    else
    if n = 1 then // 1203 (clic 3)
      Result[n] := MoverIzq(Result[n - 1], nLargo); // 1203
    if n = 2 then // 1032 (clic 2)
    begin
      Result[n] := MoverIzq(Result[n - 1], nLargo - 1); // 1023
      Result[n] := MoverIzq(Result[n], nLargo); // 1032
    end;

    // start with '2' (the 2 positioned char)
    if n = 3 then // 2310 (clic 3 2 1 3)
    begin
      Result[n] := MoverIni(Result[0], 2); // 2130
      Result[n] := MoverIzq(Result[n], nLargo - 1); // 2310
    end;
    if n = 4 then // 2301
    begin
      Result[n] := MoverIzq(Result[n - 1], nLargo); // 2301
    end;
    if n = 5 then // 2013
    begin
      Result[n] := MoverIzq(Result[n - 1], nLargo - 1); // 2031
      Result[n] := MoverIzq(Result[n], nLargo); // 2013
    end;

    // start with '3' (the 3 positioned char)
    if n = 6 then // 3120
    begin
      Result[n] := MoverIni(Result[0], 3);
      Result[n] := MoverIzq(Result[n], nLargo - 1); // 3120
    end;
    if n = 7 then // 3102
    begin
      Result[n] := MoverIzq(Result[n - 1], nLargo); // 3102
    end;
    if n = 8 then // 3021
    begin
      Result[n] := MoverIzq(Result[n - 1], nLargo - 1);
      Result[n] := MoverIzq(Result[n], nLargo); // 3021
    end;

    // start with '0' (the 4 positioned char)
    if n = 9 then // 0321
    begin
      Result[n] := MoverIni(Result[0], 4);
      Result[n] := MoverIzq(Result[n], nLargo - 1); // 0321
    end;
    if n = 10 then // 0213
    begin
      Result[n] := MoverIzq(Result[n - 1], nLargo - 1);
      Result[n] := MoverIzq(Result[n], nLargo); // 0213
    end;
    if n = 11 then // 0132
    begin
      Result[n] := MoverIzq(Result[n - 1], nLargo - 1);
      Result[n] := MoverIzq(Result[n], nLargo); // 0232
    end;
  end;

  // Debug
  if showDebug then
  begin
    rstr := '';
    for n := 0 to nPosibles - 1 do
    begin
      rstr := concat(rstr, LineEnding, Result[n]);
    end;

    ShowMessage(rstr);

    ShowMessage(
      concat('numero de char en la string: ', IntToStr(nLargo), LineEnding,
      'numero de combinaciones: ', IntToStr(nFactorial), LineEnding,
      //'primer char veces repetido: ', IntToStr(nFactorialDivLargo),
      //LineEnding, 'segundo char veces repetido: ', IntToStr(nFactorialm1DivLargom1), LineEnding,
      'numero de combinaciones jugables: ', IntToStr(nPosibles))
      );
  end;
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  DrawMap(map^, bitmap, texture);
end;

{ MOUSE }

procedure TForm1.BGRAVirtualScreen1Click(Sender: TObject);
var
  pos: TPoint;
begin
  pos := ScreenToClient(Mouse.CursorPos);
  if ClickMap(map^, pos) then
  begin
    PlayBlockSound;
    BGRAVirtualScreen1.DiscardBitmap;
  end;
end;

{ CREATE }

procedure TForm1.FormCreate(Sender: TObject);
var
  a: TArrayOfString;
  i: integer;
begin
  a := CalcularPosibles('1230'{, True});

  // 0 is 1230 the solve
  // 1 to 11
  for i := 1 to length(a) - 1 do
  begin
    Game[i].map := a[i];
    Game[i].solve := a[0];
    Game[i].mapW := length(a[0]) div 2;
    Game[i].mapH := length(a[0]) div 2;
    Game[i].blockW := 128;
    Game[i].blockH := 128;
  end;

  Game[length(Game)] := GameOver;

  { SCALING }
  // FullScreen with current screen resolution
  //Size := Rect(0, 0, Screen.Width, Screen.Height);



  Size := Rect(0, 0, 640, 480);

  SetBounds(Size.Left, Size.Top, Size.Right, Size.Bottom);

  ScaleAspectRatio(BGRAVirtualScreen1, Size.Right, Size.Bottom, True, True, True);

  ToggleFullScreen(Self, Size);

  ScaleBlocks;

  { OPEN }
  randomize; // to randomize the skin

  OpenMap(1);
  current := 1;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CloseMap;
end;

{ KEYBOARD }

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
    Application.Terminate;

  if (Key = VK_F11) then
    ToggleFullScreen(Self, Size);

  if KeyPressMap(map^, Key) then
  begin
    PlayBlockSound;
    BGRAVirtualScreen1.DiscardBitmap;
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  CenterControl(BGRAVirtualScreen1);
end;

{ TIMER }

procedure TForm1.Timer1Timer(Sender: TObject);

{$IFDEF WINDOWS}
  procedure JoyDown(const pKey: word);
  var
    Key: word;
  begin
    Key := pKey;
    FormKeyDown(nil, Key, []);
  end;

{$ENDIF}

var
  nextGame: boolean;
  {$IFDEF WINDOWS}
  myJoy: TJoyInfoEx;
  myJoyCaps: TJoyCaps;
  ErrorResultC, ErrorResultP: MMRESULT;
  {$ENDIF}
begin
  if map <> nil then
  begin
    Caption := 'Level ' + IntToStr(current);

    nextGame := IsMapSolved(map^);
    if (current = length(Game)) and nextGame then
    begin
      Timer1.Enabled := False;
      //ShowMessage('Game Win!');
      exit;
    end;

    if nextGame then
    begin
      Timer1.Enabled := False;
      OpenMap(current + 1);
      //ShowMessage('Next: Level ' + IntToStr(current));
      BGRAVirtualScreen1.DiscardBitmap;
      Timer1.Enabled := True;
    end;

    {$IFDEF WINDOWS}
    ErrorResultC := joyGetDevCaps(joystickid1, @myJoyCaps, sizeof(myJoyCaps));
    ErrorResultP := joyGetPosEx(joystickid1, @MyJoy);
    if (ErrorResultC = JOYERR_NOERROR) and (ErrorResultP = JOYERR_NOERROR) then
    begin
      if (myJoy.dwPOV = JOY_POVFORWARD) or (myJoy.wYpos = myJoyCaps.wYmin) then
        JoyDown(U1)
      else if (myJoy.dwPOV = JOY_POVBACKWARD) or (myJoy.wYpos = myJoyCaps.wYmax) then
        JoyDown(D1)
      else if (myJoy.dwPOV = JOY_POVLEFT) or (myJoy.wXpos = myJoyCaps.wXmin) then
        JoyDown(L1)
      else if (myJoy.dwPOV = JOY_POVRIGHT) or (myJoy.wXpos = myJoyCaps.wXmax) then
        JoyDown(R1);
    end;
   {$ENDIF}

  end;
end;

procedure TForm1.OpenMap(num: integer);
begin
  CloseMap;
  if (num < low(Game)) or (num > high(Game)) then
    halt;
  current := num;
  map := @Game[current];
  GenerateTexture;
end;

procedure TForm1.CloseMap;
begin
  if map <> nil then
    FreeAndNil(map^.background);
  FreeAndNil(texture);
  map := nil;
end;

procedure TForm1.ScaleBlocks;
var
  i: integer;
begin
  for i := low(game) to high(game) do
  begin
    Game[i].blockW := BGRAVirtualScreen1.Width div Game[i].mapW;
    Game[i].blockH := BGRAVirtualScreen1.Height div Game[i].mapH;
  end;
end;

procedure TForm1.GenerateTexture;
begin
  if texture <> nil then
    raise Exception.Create('Texture not freed');
  case random(9) of
    0: texture := CreatePlastikTexture(BGRAVirtualScreen1.Width,
        BGRAVirtualScreen1.Height);
    1: texture := CreateCamouflageTexture(BGRAVirtualScreen1.Width,
        BGRAVirtualScreen1.Height);
    2: texture := CreateSnowPrintTexture(BGRAVirtualScreen1.Width,
        BGRAVirtualScreen1.Height);
    3: texture := CreateRoundStoneTexture(BGRAVirtualScreen1.Width,
        BGRAVirtualScreen1.Height);
    4: texture := CreateStoneTexture(BGRAVirtualScreen1.Width,
        BGRAVirtualScreen1.Height);
    5: texture := CreateWaterTexture(BGRAVirtualScreen1.Width,
        BGRAVirtualScreen1.Height);
    6: texture := CreateMarbleTexture(BGRAVirtualScreen1.Width,
        BGRAVirtualScreen1.Height);
    7: texture := CreateWoodTexture(BGRAVirtualScreen1.Width, BGRAVirtualScreen1.Height);
    8: texture := CreateVerticalWoodTexture(BGRAVirtualScreen1.Width,
        BGRAVirtualScreen1.Height);
  end;
end;

procedure TForm1.PlayBlockSound;
begin
  {$IFDEF WINDOWS}
  PlaySound(PChar('move.wav'), 0, SND_ASYNC);
  {$ENDIF}
end;

end.
