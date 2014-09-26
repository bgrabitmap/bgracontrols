unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes, BCTileMap;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    cbTileID: TComboBox;
    vsMap: TBGRAVirtualScreen;
    procedure vsMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure vsMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure vsMapMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure vsMapRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTileMap: TMapIni;
    FMouseIsDown: boolean;
    FLastPainted: NativeInt;
    procedure SetFLastPainted(AValue: NativeInt);
    procedure SetFMouseIsDown(AValue: boolean);
    procedure SetFTileMap(AValue: TMapIni);
    { private declarations }
  public
    procedure PaintTile;
  public
    property TileMap: TMapIni read FTileMap write SetFTileMap;
    property MouseIsDown: boolean read FMouseIsDown write SetFMouseIsDown default False;
    property LastPainted: NativeInt read FLastPainted write SetFLastPainted default -1;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: NativeInt;
begin
  TileMap := TMapIni.Create('map.ini');

  for i := 0 to TileMap.TileSet.TileCount - 1 do
  begin
    cbTileID.Items.Add(IntToStr(i));
  end;

  cbTileID.Caption := cbTileID.Items[0];
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  TileMap.Free;
end;

procedure TfrmMain.SetFTileMap(AValue: TMapIni);
begin
  if FTileMap = AValue then
    Exit;
  FTileMap := AValue;
end;

procedure TfrmMain.PaintTile;
var
  pos: TPoint;
  x, y, n: NativeInt;
  r: TRect;
begin
  pos := ScreenToClient(Mouse.CursorPos);
  n := 0;
  for y := 0 to TileMap.Map.Height - 1 do
  begin
    for x := 0 to TileMap.Map.Width - 1 do
    begin
      r.Left := x * TileMap.Map.TileWidth;
      r.Top := y * TileMap.Map.TileHeight;
      r.Right := r.Left + TileMap.Map.TileWidth;
      r.Bottom := r.Top + TileMap.Map.TileHeight;
      if ((pos.x >= r.Left) and (pos.x < r.Right)) and
        ((pos.y >= r.Top) and (pos.y < r.Bottom)) and (LastPainted <> n) then
      begin
        TileMap.Layers[0].Data[n] := cbTileID.Caption;
        vsMap.DiscardBitmap;
        LastPainted := n;
        break;
      end;
      Inc(n);
    end;
  end;
end;

procedure TfrmMain.SetFMouseIsDown(AValue: boolean);
begin
  if FMouseIsDown = AValue then
    Exit;
  FMouseIsDown := AValue;
end;

procedure TfrmMain.SetFLastPainted(AValue: NativeInt);
begin
  if FLastPainted = AValue then
    Exit;
  FLastPainted := AValue;
end;

procedure TfrmMain.vsMapRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  TileMap.DrawMap(Bitmap);
end;

procedure TfrmMain.vsMapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    MouseIsDown := True;
    PaintTile;
  end;
end;

procedure TfrmMain.vsMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if MouseIsDown then
    PaintTile;
end;

procedure TfrmMain.vsMapMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    MouseIsDown := False;
    LastPainted := -1;
  end;
end;

end.
