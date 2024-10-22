unit utestbgragame;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, LCLType, ExtCtrls, BGRAVirtualScreen, BGRABitmap,
  BGRABitmapTypes, Dialogs, bgragame;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Timer2: TTimer;
    procedure BGRAVirtualScreen1MouseDown(Sender: TObject;
      Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: integer);
    procedure BGRAVirtualScreen1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure BGRAVirtualScreen1MouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; {%H-}Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { private declarations }
    procedure UpdateVirtualScreen;
  public
    { public declarations }
    level1: TGameMap;
    size: TRect;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  DrawMap(level1, bitmap, True);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Size := Rect(0, 0, 640, 480);
  ToggleFullScreen(Self, Size);

  level1.mapWidth := 20;
  level1.mapHeight := 20;
  level1.mapGraphics :=
    '000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000X000000000000000000000000000000000000000000000000000000000000000000000000000000';
  level1.mapProperty :=
    '2222222222222222222220002020220200022022212022000020020000022020202022002202202220200000200202020002200202200020000220222200220022002020200220200020202220020022202022002002020002022200200000200022000220020200220020002022202220202002202000222000000020200020220222220222002020200002202020200200220220222200002020220000002220202020020222220002202020022000020002022000220000020002000222222222222222222222';
  level1.mapSolve :=
    '2222222222222222222220002020220200022022202022000020020000022020202022002202202220200000200202020002200202200020000220222200220022002020200220200020202220020022202022002002020002022200200000200022000220020200220020002022202220202002202000222000000020200020220222220222002020200002202020200200220220222200002020220000002221202020020222220002202020022000020002022000220000020002000222222222222222222222';
  level1.mapKeyChar := '1'; // Player
  level1.mapSolidChar := '2'; // Wall
  level1.ballPositionDefined := false;
  //see FormResize

  Timer2.Interval:= 16;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  level1.background.Free;
  level1.ballSphereMap.free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
    Application.Terminate;

  if (Key = VK_F11) then
    ToggleFullScreen(Self, Size);

  KeyDownMap(level1, Key);
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  KeyUpMap(level1, Key);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Size := Rect(0, 0, Width, Height);
  ScaleAspectRatio(BGRAVirtualScreen1, Size.Right, Size.Bottom, True, True, True);

  // this has scaling
  level1.blockWidth := BGRAVirtualScreen1.Width div level1.mapWidth;
  level1.blockHeight := BGRAVirtualScreen1.Height div level1.mapHeight;

  // this has scaling
  level1.background.Free;
  level1.background := TBGRABitmap.Create('background.jpg');
  BGRAReplace(level1.background, level1.background.Resample(BGRAVirtualScreen1.Width, BGRAVirtualScreen1.Height));
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  Timer2.Enabled:= false;
  UpdateVirtualScreen;
  if IsMapSolved(level1) then
  begin
    Timer2.Enabled := False;
    ShowMessage('Win!');
    Application.Terminate;
  end;
  Timer2.Enabled:= true;
end;

procedure TForm1.UpdateVirtualScreen;
begin
  if Assigned(BGRAVirtualScreen1.Bitmap) then
  begin
    DrawMap(level1, BGRAVirtualScreen1.bitmap, false);
    BGRAVirtualScreen1.Invalidate;
  end;
end;

procedure TForm1.BGRAVirtualScreen1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
    MouseDownMap(level1, Point(x, y));
end;

procedure TForm1.BGRAVirtualScreen1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  if ssLeft in shift then
    MouseDownMap(level1, Point(x, y));
end;

procedure TForm1.BGRAVirtualScreen1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    MouseUpMap(level1, Point(x, y));
end;

end.
