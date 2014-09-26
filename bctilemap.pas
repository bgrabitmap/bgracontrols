unit BCTileMap;

{$mode objfpc}{$H+}
{ $modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, LCLProc, IniFiles, BGRABitmap, BGRABitmapTypes;

type
  TMapOrientation = (moOrthogonal, moIsometric, moStaggered);

  { TTerrainType }

  TTerrainType = record
    TopLeft, TopRight, BottomLeft, BottomRight: NativeInt;
    //class operator = (aLeft, aRight: TTerrainType): Boolean;
  end;

const
  MapOrientationStr: array [TMapOrientation] of string =
    ('orthogonal', 'isometric', 'staggered');

function StrToTMapOrientation(const s: string): TMapOrientation;
function TerrainType(TopLeft, TopRight, BottomLeft, BottomRight: NativeInt):
  TTerrainType;
function StrToTTerrainType(const s: string): TTerrainType;
function TTerrainTypeToStr(const t: TTerrainType): string;

type
  { TMap }

  TMap = class
  private
    FVersion: NativeInt;
    FOrientation: TMapOrientation;
    FWidth: NativeInt;
    FHeight: NativeInt;
    FTileWidth: NativeInt;
    FTileHeight: NativeInt;
    FLayerCount: NativeInt;
    FBackgroundColor: TBGRAPixel;
    procedure SetFBackgroundColor(AValue: TBGRAPixel);
    procedure SetFHeight(AValue: NativeInt);
    procedure SetFLayerCount(AValue: NativeInt);
    procedure SetFOrientation(AValue: TMapOrientation);
    procedure SetFTileHeight(AValue: NativeInt);
    procedure SetFVersion(AValue: NativeInt);
    procedure SetFWidth(AValue: NativeInt);
  public
    constructor Create;
    constructor Create(MemIniFile: TMemIniFile);
    destructor Destroy; override;
  public
    procedure LoadFromINIFile(MemIniFile: TMemIniFile);
    procedure SaveToINIFile(MemIniFile: TMemIniFile);
  public
    property BackgroundColor: TBGRAPixel read FBackgroundColor
      write SetFBackgroundColor;
  published
    property Version: NativeInt read FVersion write SetFVersion;
    property Orientation: TMapOrientation read FOrientation write SetFOrientation;
    property Width: NativeInt read FWidth write SetFWidth;
    property Height: NativeInt read FHeight write SetFHeight;
    property TileWidth: NativeInt read FTileWidth write SetFTileHeight;
    property TileHeight: NativeInt read FTileHeight write SetFTileHeight;
    property LayerCount: NativeInt read FLayerCount write SetFLayerCount;
  end;

  { TTileSet }

  TTileSet = class
  private
    FFirstGID: NativeInt;
    FSource: string;
    FName: string;
    FTileWidth: NativeInt;
    FTileHeight: NativeInt;
    FTileCount: NativeInt;
    FSpacing: NativeInt;
    FMargin: NativeInt;
    procedure SetFirstGID(AValue: NativeInt);
    procedure SetFMargin(AValue: NativeInt);
    procedure SetFName(AValue: string);
    procedure SetFSource(AValue: string);
    procedure SetFSpacing(AValue: NativeInt);
    procedure SetFTileCount(AValue: NativeInt);
    procedure SetFTileHeight(AValue: NativeInt);
    procedure SetFTileWidth(AValue: NativeInt);
  public
    constructor Create;
    constructor Create(MemIniFile: TMemIniFile);
    destructor Destroy; override;
  public
    procedure LoadFromINIFile(MemIniFile: TMemIniFile);
    procedure SaveToINIFile(MemIniFile: TMemIniFile);
  published
    property FirstGID: NativeInt read FFirstGID write SetFirstGID;
    property Source: string read FSource write SetFSource;
    property Name: string read FName write SetFName;
    property TileWidth: NativeInt read FTileWidth write SetFTileWidth;
    property TileHeight: NativeInt read FTileHeight write SetFTileHeight;
    property TileCount: NativeInt read FTileCount write SetFTileCount;
    property Spacing: NativeInt read FSpacing write SetFSpacing;
    property Margin: NativeInt read FMargin write SetFMargin;
  end;

  { TTileOffset }

  TTileOffset = class
  private
    Fx: NativeInt;
    Fy: NativeInt;
    procedure SetFx(AValue: NativeInt);
    procedure SetFy(AValue: NativeInt);
  public
    constructor Create;
    constructor Create(MemIniFile: TMemIniFile);
    destructor Destroy; override;
  public
    procedure LoadFromINIFile(MemIniFile: TMemIniFile);
    procedure SaveToINIFile(MemIniFile: TMemIniFile);
  published
    property x: NativeInt read Fx write SetFx;
    property y: NativeInt read Fy write SetFy;
  end;

  { TImageSource }

  TImageSource = class
  private
    FBitmap: TBGRABitmap;
    FSource: string;
    FTrans: TBGRAPixel;
    FWidth: NativeInt;
    FHeight: NativeInt;
    procedure SetFBitmap(AValue: TBGRABitmap);
    procedure SetFHeight(AValue: NativeInt);
    procedure SetFSource(AValue: string);
    procedure SetFTrans(AValue: TBGRAPixel);
    procedure SetFWidth(AValue: NativeInt);
  public
    constructor Create;
    constructor Create(MemIniFile: TMemIniFile);
    destructor Destroy; override;
  public
    procedure LoadFromINIFile(MemIniFile: TMemIniFile);
    procedure SaveToINIFile(MemIniFile: TMemIniFile);
  public
    property Bitmap: TBGRABitmap read FBitmap write SetFBitmap;
    property Trans: TBGRAPixel read FTrans write SetFTrans;
  published
    property Source: string read FSource write SetFSource;
    property Width: NativeInt read FWidth write SetFWidth;
    property Height: NativeInt read FHeight write SetFHeight;
  end;

  { TTerrain }

  TTerrain = class
  private
    FName: string;
    FTile: NativeInt;
    procedure SetFName(AValue: string);
    procedure SetFTile(AValue: NativeInt);
  public
    constructor Create;
    constructor Create(MemIniFile: TMemIniFile; Index: NativeInt);
    procedure SaveToINIFile(MemIniFile: TMemIniFile; Index: NativeInt);
    destructor Destroy; override;
  public
    procedure LoadFromINIFile(MemIniFile: TMemIniFile; Index: NativeInt);
  published
    property Name: string read FName write SetFName;
    property Tile: NativeInt read FTile write SetFTile;
  end;

  TTerrainTypes = array of TTerrain;

  { TTile }

  TTile = class
  private
    FID: NativeInt;
    FTerrain: TTerrainType;
    procedure SetFID(AValue: NativeInt);
    procedure SetFTerrain(AValue: TTerrainType);
  public
    constructor Create;
    constructor Create(MemIniFile: TMemIniFile; Index: NativeInt);
    destructor Destroy; override;
  public
    procedure LoadFromINIFile(MemIniFile: TMemIniFile; Index: NativeInt);
    procedure SaveToINIFile(MemIniFile: TMemIniFile; Index: NativeInt);
  public
    property Terrain: TTerrainType read FTerrain write SetFTerrain;
  published
    property ID: NativeInt read FID write SetFID;
  end;

  TTiles = array of TTile;

  { TLayer }

  TLayer = class
  private
    FName: string;
    FOpacity: real;
    FVisible: boolean;
    FData: TStringList;
    procedure SetFData(AValue: TStringList);
    procedure SetFName(AValue: string);
    procedure SetFOpacity(AValue: real);
    procedure SetFVisible(AValue: boolean);
  public
    constructor Create;
    constructor Create(MemIniFile: TMemIniFile; Index: NativeInt);
    destructor Destroy; override;
  public
    procedure LoadFromINIFile(MemIniFile: TMemIniFile; Index: NativeInt);
    procedure SaveToINIFile(MemIniFile: TMemIniFile; Index: NativeInt);
  published
    property Name: string read FName write SetFName;
    property Opacity: real read FOpacity write SetFOpacity;
    property Visible: boolean read FVisible write SetFVisible;
    property Data: TStringList read FData write SetFData;
  end;

  TLayers = array of TLayer;

  { TMapIni }

  TMapIni = class
  private
    FMap: TMap;
    FTileSet: TTileSet;
    FTileOffset: TTileOffset;
    FImageSource: TImageSource;
    FTiles: TTiles;
    FLayers: TLayers;
    FRects: array of TRect;
    procedure SetFImageSource(AValue: TImageSource);
    procedure SetFMap(AValue: TMap);
    procedure SetFTileOffset(AValue: TTileOffset);
    procedure SetFTileSet(AValue: TTileSet);
    procedure InitFRects;
  public
    constructor Create;
    constructor Create(FileName: string);
    destructor Destroy; override;
  public
    procedure LoadFromINIFile(FileName: string);
    procedure SaveToINIFile(FileName: string);
  public
    procedure DrawMap(Bitmap: TBGRABitmap);
  published
    property Map: TMap read FMap write SetFMap;
    property TileSet: TTileSet read FTileSet write SetFTileSet;
    property TileOffset: TTileOffset read FTileOffset write SetFTileOffset;
    property Tiles: TTiles read FTiles write FTiles;
    property Layers: TLayers read FLayers write FLayers;
    property Image: TImageSource read FImageSource write SetFImageSource;
  end;

implementation

function StrToTMapOrientation(const s: string): TMapOrientation;
var
  mo: TMapOrientation;
  ls: string;
begin
  ls := UTF8LowerCase(s);
  for mo := low(TMapOrientation) to high(TMapOrientation) do
    if ls = MapOrientationStr[mo] then
    begin
      Result := mo;
      break;
    end;
  Result := moOrthogonal;
end;

function TerrainType(TopLeft, TopRight, BottomLeft, BottomRight: NativeInt): TTerrainType;
begin
  Result.TopLeft := TopLeft;
  Result.TopRight := TopRight;
  Result.BottomLeft := BottomLeft;
  Result.BottomRight := BottomRight;
end;

function StrToTTerrainType(const s: string): TTerrainType;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.CommaText := s;
  {$ifdef cpu64}
  Result.TopLeft := StrToInt64(sl[0]);
  Result.TopRight := StrToInt64(sl[1]);
  Result.BottomLeft := StrToInt64(sl[2]);
  Result.BottomRight := StrToInt64(sl[3]);
  {$else}
  Result.TopLeft := StrToInt(sl[0]);
  Result.TopRight := StrToInt(sl[1]);
  Result.BottomLeft := StrToInt(sl[2]);
  Result.BottomRight := StrToInt(sl[3]);
  {$endif}
  sl.Free;
end;

function TTerrainTypeToStr(const t: TTerrainType): string;
begin
  Result := IntToStr(t.TopLeft) + ',' + IntToStr(t.TopRight) + ',' +
    IntToStr(t.BottomLeft) + ',' + IntToStr(t.BottomRight);
end;

{ TMapIni }

procedure TMapIni.SetFMap(AValue: TMap);
begin
  if FMap = AValue then
    Exit;
  FMap := AValue;
end;

procedure TMapIni.SetFTileOffset(AValue: TTileOffset);
begin
  if FTileOffset = AValue then
    Exit;
  FTileOffset := AValue;
end;

procedure TMapIni.SetFImageSource(AValue: TImageSource);
begin
  if FImageSource = AValue then
    Exit;
  FImageSource := AValue;
end;

procedure TMapIni.SetFTileSet(AValue: TTileSet);
begin
  if FTileSet = AValue then
    Exit;
  FTileSet := AValue;
end;

procedure TMapIni.InitFRects;
var
  x, y, n, tw, th: NativeInt;
begin
  SetLength(FRects, FTileSet.TileCount);
  th := FTileSet.TileHeight + FTileSet.Margin;
  tw := FTileSet.TileWidth + FTileSet.Margin;
  n := 0;
  for y := 0 to ((Image.Height - TileSet.Margin) div tw) - 1 do
  begin
    for x := 0 to ((Image.Width - TileSet.Margin) div th) - 1 do
    begin
      FRects[n] := Rect(x * tw + TileSet.Margin, y * th + TileSet.Margin,
        x * tw + tw, y * th + th);
      Inc(n);
    end;
  end;
end;

constructor TMapIni.Create;
begin
  inherited Create;
  FMap := TMap.Create;
  FTileSet := TTileSet.Create;
  FTileOffset := TTileOffset.Create;
  FImageSource := TImageSource.Create;
end;

constructor TMapIni.Create(FileName: string);
begin
  Create;
  LoadFromINIFile(FileName);
  InitFRects;
end;

destructor TMapIni.Destroy;
var
  i: NativeInt;
begin
  if FMap <> nil then
    FMap.Free;
  if FTileSet <> nil then
    FTileSet.Free;
  if FTileOffset <> nil then
    FTileOffset.Free;
  if FImageSource <> nil then
    FImageSource.Free;

  for i := 0 to High(FTiles) do
    if FTiles[i] <> nil then
      FTiles[i].Free;

  for i := 0 to High(FLayers) do
    if FLayers[i] <> nil then
      FLayers[i].Free;

  inherited Destroy;
end;

procedure TMapIni.LoadFromINIFile(FileName: string);
var
  ini: TMemIniFile;
  i: NativeInt;
begin
  ini := TMemIniFile.Create(FileName);

  FMap.LoadFromINIFile(ini);
  FTileSet.LoadFromINIFile(ini);
  FTileOffset.LoadFromINIFile(ini);
  FImageSource.LoadFromINIFile(ini);

  if FTileSet.TileCount <> 0 then
  begin
    SetLength(FTiles, FTileSet.TileCount);
    for i := 0 to FTileSet.TileCount - 1 do
      FTiles[i] := TTile.Create(ini, i);
  end;

  if FMap.LayerCount <> 0 then
  begin
    SetLength(FLayers, FMap.LayerCount);
    for i := 0 to FMap.LayerCount - 1 do
      FLayers[i] := TLayer.Create(ini, i);
  end;

  ini.Free;
end;

procedure TMapIni.SaveToINIFile(FileName: string);
var
  ini: TMemIniFile;
  i: NativeInt;
begin
  ini := TMemIniFile.Create(FileName);

  FMap.SaveToINIFile(ini);
  FTileSet.SaveToINIFile(ini);
  FTileOffset.SaveToINIFile(ini);
  FImageSource.SaveToINIFile(ini);

  for i := 0 to High(FTiles) do
    FTiles[i].SaveToINIFile(ini, i);

  for i := 0 to High(FLayers) do
    FLayers[i].SaveToINIFile(ini, i);

  ini.UpdateFile;
  ini.Free;
end;

procedure TMapIni.DrawMap(Bitmap: TBGRABitmap);
var
  x, y, z, n, id: NativeInt;
  bmp: TBGRABitmap;
  opacity: byte;
begin
  Bitmap.Fill(FMap.BackgroundColor);

  for z := 0 to High(FLayers) do
  begin
    if FLayers[z].Visible then
    begin
      opacity := round(255 * FLayers[z].Opacity);
      n := 0;
      for y := 0 to FMap.Height - 1 do
      begin
        for x := 0 to FMap.Width - 1 do
        begin
          {$ifdef cpu64}
          id := StrToInt64(FLayers[z].Data[n]);
          {$else}
          id := StrToInt(FLayers[z].Data[n]);
          {$endif}
          if id <> -1 then
          begin
            bmp := TBGRABitmap(FImageSource.Bitmap.GetPart(FRects[id]));
            Bitmap.BlendImageOver(x * FMap.TileWidth, y * FMap.TileHeight, bmp,
              boTransparent, opacity);
            bmp.Free;
          end; // id pass
          Inc(n);
        end; // x
      end; // y
    end; // layers[z] visible
  end; // layers
end;

{ TLayer }

procedure TLayer.SetFName(AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
end;

procedure TLayer.SetFData(AValue: TStringList);
begin
  if FData = AValue then
    Exit;
  FData := AValue;
end;

procedure TLayer.SetFOpacity(AValue: real);
begin
  if FOpacity = AValue then
    Exit;
  FOpacity := AValue;
end;

procedure TLayer.SetFVisible(AValue: boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
end;

constructor TLayer.Create;
begin
  inherited Create;
  FData := TStringList.Create;
end;

constructor TLayer.Create(MemIniFile: TMemIniFile; Index: NativeInt);
begin
  Create;
  LoadFromINIFile(MemIniFile, Index);
end;

destructor TLayer.Destroy;
begin
  if FData <> nil then
    FData.Free;
  inherited Destroy;
end;

procedure TLayer.LoadFromINIFile(MemIniFile: TMemIniFile; Index: NativeInt);
begin
  FName := MemIniFile.ReadString('Layer' + IntToStr(Index), 'Name', '');
  FOpacity := MemIniFile.ReadFloat('Layer' + IntToStr(Index), 'Opacity', 1);
  FVisible := MemIniFile.ReadBool('Layer' + IntToStr(Index), 'Visible', True);
  FData.CommaText := MemIniFile.ReadString('Layer' + IntToStr(Index), 'Data', '');
end;

procedure TLayer.SaveToINIFile(MemIniFile: TMemIniFile; Index: NativeInt);
begin
  MemIniFile.WriteString('Layer' + IntToStr(Index), 'Name', FName);
  MemIniFile.WriteFloat('Layer' + IntToStr(Index), 'Opacity', FOpacity);
  MemIniFile.WriteBool('Layer' + IntToStr(Index), 'Visible', FVisible);
  MemIniFile.WriteString('Layer' + IntToStr(Index), 'Data', FData.CommaText);
end;

{ TTerrainType }

{class operator TTerrainType.=(aLeft, aRight: TTerrainType): Boolean;
begin
  if (aLeft.TopLeft <> aRight.TopLeft) or
  (aLeft.TopRight <> aRight.TopRight) or
  (aLeft.BottomLeft <> aRight.BottomLeft) or
  (aLeft.BottomRight <> aRight.BottomRight) then
    result := False
  else
    result := True;
end;}

{ TTile }

procedure TTile.SetFID(AValue: NativeInt);
begin
  if FID = AValue then
    Exit;
  FID := AValue;
end;

procedure TTile.SetFTerrain(AValue: TTerrainType);
begin
  {if FTerrain = AValue then
    Exit;}
  FTerrain := AValue;
end;

constructor TTile.Create;
begin
  inherited Create;
end;

constructor TTile.Create(MemIniFile: TMemIniFile; Index: NativeInt);
begin
  inherited Create;
  LoadFromINIFile(MemIniFile, Index);
end;

destructor TTile.Destroy;
begin
  inherited Destroy;
end;

procedure TTile.LoadFromINIFile(MemIniFile: TMemIniFile; Index: NativeInt);
begin
  FTerrain := StrToTTerrainType(MemIniFile.ReadString('Tile' +
    IntToStr(Index), 'Terrain', '0,0,0,0'));
  {$ifdef cpu64}
  FID := MemIniFile.ReadInt64('Tile' + IntToStr(Index), 'ID', Index);
  {$else}
  FID := MemIniFile.ReadInteger('Tile' + IntToStr(Index), 'ID', Index);
  {$endif}
end;

procedure TTile.SaveToINIFile(MemIniFile: TMemIniFile; Index: NativeInt);
begin
  MemIniFile.WriteString('Tile' + IntToStr(Index), 'Terrain',
    TTerrainTypeToStr(FTerrain));
  {$ifdef cpu64}
  MemIniFile.WriteInt64('Tile' + IntToStr(Index), 'ID', Index);//FID);
  {$else}
  MemIniFile.WriteInteger('Tile' + IntToStr(Index), 'ID', Index);//FID);
  {$endif}
end;

{ TTerrain }

procedure TTerrain.SetFName(AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
end;

procedure TTerrain.SetFTile(AValue: NativeInt);
begin
  if FTile = AValue then
    Exit;
  FTile := AValue;
end;

constructor TTerrain.Create;
begin
  inherited Create;
end;

constructor TTerrain.Create(MemIniFile: TMemIniFile; Index: NativeInt);
begin
  inherited Create;
  LoadFromINIFile(MemIniFile, Index);
end;

procedure TTerrain.SaveToINIFile(MemIniFile: TMemIniFile; Index: NativeInt);
begin
  MemIniFile.WriteString('Terrain' + IntToStr(Index), 'Name', FName);
  {$ifdef cpu64}
  MemIniFile.WriteInt64('Terrain' + IntToStr(Index), 'Tile', FTile);
  {$else}
  MemIniFile.WriteInteger('Terrain' + IntToStr(Index), 'Tile', FTile);
  {$endif}
end;

destructor TTerrain.Destroy;
begin
  inherited Destroy;
end;

procedure TTerrain.LoadFromINIFile(MemIniFile: TMemIniFile; Index: NativeInt);
begin
  FName := MemIniFile.ReadString('Terrain' + IntToStr(Index), 'Name', '');
  {$ifdef cpu64}
  FTile := MemIniFile.ReadInt64('Terrain' + IntToStr(Index), 'Tile', 0);
  {$else}
  FTile := MemIniFile.ReadInteger('Terrain' + IntToStr(Index), 'Tile', 0);
  {$endif}
end;

{ TImageSource }

procedure TImageSource.SetFHeight(AValue: NativeInt);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;
end;

procedure TImageSource.SetFBitmap(AValue: TBGRABitmap);
begin
  if FBitmap = AValue then
    Exit;
  FBitmap := AValue;
end;

procedure TImageSource.SetFSource(AValue: string);
begin
  if FSource = AValue then
    Exit;
  FSource := AValue;
end;

procedure TImageSource.SetFTrans(AValue: TBGRAPixel);
begin
  if FTrans = AValue then
    Exit;
  FTrans := AValue;
end;

procedure TImageSource.SetFWidth(AValue: NativeInt);
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;
end;

constructor TImageSource.Create;
begin
  inherited Create;
  FBitmap := TBGRABitmap.Create;
end;

constructor TImageSource.Create(MemIniFile: TMemIniFile);
begin
  inherited Create;
  LoadFromINIFile(MemIniFile);
end;

destructor TImageSource.Destroy;
begin
  if FBitmap <> nil then
    FBitmap.Free;
  inherited Destroy;
end;

procedure TImageSource.LoadFromINIFile(MemIniFile: TMemIniFile);
begin
  FSource := MemIniFile.ReadString('Image', 'Source', '');
  FTrans := StrToBGRA(MemIniFile.ReadString('Image', 'Trans', 'rgba(255,0,255,255)'),
    BGRA(255, 0, 255, 255));
  FBitmap.LoadFromFile(FSource);
  {$ifdef cpu64}
  FWidth := MemIniFile.ReadInt64('Image', 'Width', 0);
  FHeight := MemIniFile.ReadInt64('Image', 'Height', 0);
  {$else}
  FWidth := MemIniFile.ReadInteger('Image', 'Width', 0);
  FHeight := MemIniFile.ReadInteger('Image', 'Height', 0);
  {$endif}
end;

procedure TImageSource.SaveToINIFile(MemIniFile: TMemIniFile);
begin
  MemIniFile.WriteString('Image', 'Source', FSource);
  MemIniFile.WriteString('Image', 'Trans', BGRAToStr(FTrans));
  {$ifdef cpu64}
  MemIniFile.WriteInt64('Image', 'Width', FWidth);
  MemIniFile.WriteInt64('Image', 'Height', FHeight);
  {$else}
  MemIniFile.WriteInteger('Image', 'Width', FWidth);
  MemIniFile.WriteInteger('Image', 'Height', FHeight);
  {$endif}
end;

{ TTileOffset }

procedure TTileOffset.SetFx(AValue: NativeInt);
begin
  if Fx = AValue then
    Exit;
  Fx := AValue;
end;

procedure TTileOffset.SetFy(AValue: NativeInt);
begin
  if Fy = AValue then
    Exit;
  Fy := AValue;
end;

constructor TTileOffset.Create;
begin
  inherited Create;
end;

constructor TTileOffset.Create(MemIniFile: TMemIniFile);
begin
  inherited Create;
  LoadFromINIFile(MemIniFile);
end;

destructor TTileOffset.Destroy;
begin
  inherited Destroy;
end;

procedure TTileOffset.LoadFromINIFile(MemIniFile: TMemIniFile);
begin
  {$ifdef cpu64}
  Fx := MemIniFile.ReadInt64('TileOffset', 'x', 0);
  Fy := MemIniFile.ReadInt64('TileOffset', 'y', 0);
  {$else}
  Fx := MemIniFile.ReadInteger('TileOffset', 'x', 0);
  Fy := MemIniFile.ReadInteger('TileOffset', 'y', 0);
  {$endif}
end;

procedure TTileOffset.SaveToINIFile(MemIniFile: TMemIniFile);
begin
  {$ifdef cpu64}
  MemIniFile.WriteInt64('TileOffset', 'x', Fx);
  MemIniFile.WriteInt64('TileOffset', 'y', Fy);
  {$else}
  MemIniFile.WriteInteger('TileOffset', 'x', Fx);
  MemIniFile.WriteInteger('TileOffset', 'y', Fy);
  {$endif}
end;

{ TTileSet }

procedure TTileSet.SetFName(AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
end;

procedure TTileSet.SetFSource(AValue: string);
begin
  if FSource = AValue then
    Exit;
  FSource := AValue;
end;

procedure TTileSet.SetFMargin(AValue: NativeInt);
begin
  if FMargin = AValue then
    Exit;
  FMargin := AValue;
end;

procedure TTileSet.SetFirstGID(AValue: NativeInt);
begin
  if FFirstGID = AValue then
    Exit;
  FFirstGID := AValue;
end;

procedure TTileSet.SetFSpacing(AValue: NativeInt);
begin
  if FSpacing = AValue then
    Exit;
  FSpacing := AValue;
end;

procedure TTileSet.SetFTileCount(AValue: NativeInt);
begin
  if FTileCount = AValue then
    Exit;
  FTileCount := AValue;
end;

procedure TTileSet.SetFTileHeight(AValue: NativeInt);
begin
  if FTileHeight = AValue then
    Exit;
  FTileHeight := AValue;
end;

procedure TTileSet.SetFTileWidth(AValue: NativeInt);
begin
  if FTileWidth = AValue then
    Exit;
  FTileWidth := AValue;
end;

constructor TTileSet.Create;
begin
  inherited Create;
end;

constructor TTileSet.Create(MemIniFile: TMemIniFile);
begin
  inherited Create;
  LoadFromINIFile(MemIniFile);
end;

destructor TTileSet.Destroy;
begin
  inherited Destroy;
end;

procedure TTileSet.LoadFromINIFile(MemIniFile: TMemIniFile);
begin
  FSource := MemIniFile.ReadString('TileSet', 'Source', '');
  FName := MemIniFile.ReadString('TileSet', 'Name', '');
  {$ifdef cpu64}
  FFirstGID := MemIniFile.ReadInt64('TileSet', 'FirstGID', 0);
  FTileWidth := MemIniFile.ReadInt64('TileSet', 'TileWidth', 0);
  FTileHeight := MemIniFile.ReadInt64('TileSet', 'TileHeight', 0);
  FSpacing := MemIniFile.ReadInt64('TileSet', 'Spacing', 0);
  FMargin := MemIniFile.ReadInt64('TileSet', 'Margin', 0);
  FTileCount := MemIniFile.ReadInt64('TileSet', 'TileCount', 0);
  {$else}
  FFirstGID := MemIniFile.ReadInteger('TileSet', 'FirstGID', 0);
  FTileWidth := MemIniFile.ReadInteger('TileSet', 'TileWidth', 0);
  FTileHeight := MemIniFile.ReadInteger('TileSet', 'TileHeight', 0);
  FSpacing := MemIniFile.ReadInteger('TileSet', 'Spacing', 0);
  FMargin := MemIniFile.ReadInteger('TileSet', 'Margin', 0);
  FTileCount := MemIniFile.ReadInteger('TileSet', 'TileCount', 0);
  {$endif}
end;

procedure TTileSet.SaveToINIFile(MemIniFile: TMemIniFile);
begin
  MemIniFile.WriteString('TileSet', 'Source', FSource);
  MemIniFile.WriteString('TileSet', 'Name', FName);
  {$ifdef cpu64}
  MemIniFile.WriteInt64('TileSet', 'FirstGID', FFirstGID);
  MemIniFile.WriteInt64('TileSet', 'TileWidth', FTileWidth);
  MemIniFile.WriteInt64('TileSet', 'TileHeight', FTileHeight);
  MemIniFile.WriteInt64('TileSet', 'Spacing', FSpacing);
  MemIniFile.WriteInt64('TileSet', 'Margin', FMargin);
  MemIniFile.WriteInt64('TileSet', 'TileCount', FTileCount);
  {$else}
  MemIniFile.WriteInteger('TileSet', 'FirstGID', FFirstGID);
  MemIniFile.WriteInteger('TileSet', 'TileWidth', FTileWidth);
  MemIniFile.WriteInteger('TileSet', 'TileHeight', FTileHeight);
  MemIniFile.WriteInteger('TileSet', 'Spacing', FSpacing);
  MemIniFile.WriteInteger('TileSet', 'Margin', FMargin);
  MemIniFile.WriteInteger('TileSet', 'TileCount', FTileCount);
  {$endif}
end;

{ TMap }

procedure TMap.SetFBackgroundColor(AValue: TBGRAPixel);
begin
  if FBackgroundColor = AValue then
    Exit;
  FBackgroundColor := AValue;
end;

procedure TMap.SetFHeight(AValue: NativeInt);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;
end;

procedure TMap.SetFLayerCount(AValue: NativeInt);
begin
  if FLayerCount = AValue then
    Exit;
  FLayerCount := AValue;
end;

procedure TMap.SetFOrientation(AValue: TMapOrientation);
begin
  if FOrientation = AValue then
    Exit;
  FOrientation := AValue;
end;

procedure TMap.SetFTileHeight(AValue: NativeInt);
begin
  if FTileWidth = AValue then
    Exit;
  FTileWidth := AValue;
end;

procedure TMap.SetFVersion(AValue: NativeInt);
begin
  if FVersion = AValue then
    Exit;
  FVersion := AValue;
end;

procedure TMap.SetFWidth(AValue: NativeInt);
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;
end;

constructor TMap.Create;
begin
  inherited Create;
end;

constructor TMap.Create(MemIniFile: TMemIniFile);
begin
  inherited Create;
  LoadFromINIFile(MemIniFile);
end;

destructor TMap.Destroy;
begin
  inherited Destroy;
end;

procedure TMap.LoadFromINIFile(MemIniFile: TMemIniFile);
begin
  FOrientation := StrToTMapOrientation(MemIniFile.ReadString('Map',
    'Orientation', 'orthogonal'));
  FBackgroundColor := StrToBGRA(
    MemIniFile.ReadString('Map', 'BackgroundColor', 'rgba(0,0,0,255)'), BGRABlack);
  {$ifdef cpu64}
  FVersion := MemIniFile.ReadInt64('Map', 'Version', 1);
  FWidth := MemIniFile.ReadInt64('Map', 'Width', 0);
  FHeight := MemIniFile.ReadInt64('Map', 'Height', 0);
  FTileWidth := MemIniFile.ReadInt64('Map', 'TileWidth', 0);
  FTileHeight := MemIniFile.ReadInt64('Map', 'TileHeight', 0);
  FLayerCount := MemIniFile.ReadInt64('Map', 'LayerCount', 0);
  {$else}
  FVersion := MemIniFile.ReadInteger('Map', 'Version', 1);
  FWidth := MemIniFile.ReadInteger('Map', 'Width', 0);
  FHeight := MemIniFile.ReadInteger('Map', 'Height', 0);
  FTileWidth := MemIniFile.ReadInteger('Map', 'TileWidth', 0);
  FTileHeight := MemIniFile.ReadInteger('Map', 'TileHeight', 0);
  FLayerCount := MemIniFile.ReadInteger('Map', 'LayerCount', 0);
  {$endif}
end;

procedure TMap.SaveToINIFile(MemIniFile: TMemIniFile);
begin
  MemIniFile.WriteString('Map', 'Orientation', MapOrientationStr[FOrientation]);
  MemIniFile.WriteString('Map', 'BackgroundColor', BGRAToStr(FBackgroundColor));
  {$ifdef cpu64}
  MemIniFile.WriteInt64('Map', 'Version', FVersion);
  MemIniFile.WriteInt64('Map', 'Width', FWidth);
  MemIniFile.WriteInt64('Map', 'Height', FHeight);
  MemIniFile.WriteInt64('Map', 'TileWidth', FTileWidth);
  MemIniFile.WriteInt64('Map', 'TileHeight', FTileHeight);
  MemIniFile.WriteInt64('Map', 'LayerCount', FLayerCount);
  {$else}
  MemIniFile.WriteInteger('Map', 'Version', FVersion);
  MemIniFile.WriteInteger('Map', 'Width', FWidth);
  MemIniFile.WriteInteger('Map', 'Height', FHeight);
  MemIniFile.WriteInteger('Map', 'TileWidth', FTileWidth);
  MemIniFile.WriteInteger('Map', 'TileHeight', FTileHeight);
  MemIniFile.WriteInteger('Map', 'LayerCount', FLayerCount);
  {$endif}
end;

initialization
  DecimalSeparator := '.';

end.
