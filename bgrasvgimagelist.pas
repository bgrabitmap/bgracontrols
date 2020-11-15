unit BGRASVGImageList;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, FGL,
  XMLConf, BGRABitmap, BGRABitmapTypes, BGRASVG;

type

  TListOfTStringList = TFPGObjectList<TStringList>;

  { TBGRASVGImageList }

  TBGRASVGImageList = class(TComponent)
  private
    FHeight: integer;
    FItems: TListOfTStringList;
    FWidth: integer;
    procedure ReadData(Stream: TStream);
    procedure SetHeight(AValue: integer);
    procedure SetWidth(AValue: integer);
    procedure WriteData(Stream: TStream);
  protected
    procedure Load(const XMLConf: TXMLConfig);
    procedure Save(const XMLConf: TXMLConfig);
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(ASVG: string): integer;
    procedure Remove(AIndex: integer);
    procedure Exchange(AIndex1, AIndex2: integer);
    procedure Replace(AIndex: integer; ASVG: string);
    function Count: integer;
    // Get SVG string
    function Get(AIndex: integer): string; overload;
    // Get bgrabitmap with custom width and height
    function Get(AIndex: integer; AWidth, AHeight: integer;
      UseSVGAspectRatio: boolean = True): TBGRABitmap; overload;
    // Get bitmap with custom width and height
    function GetBitmap(AIndex: integer; AWidth, AHeight: integer;
      UseSVGAspectRatio: boolean = True): TBitmap; overload;
    // Draw image with svgimagelist width and height
    procedure Draw(AIndex: integer; ACanvas: TCanvas; ALeft, ATop: integer;
      UseSVGAspectRatio: boolean = True); overload;
    // Draw image with custom width and height
    procedure Draw(AIndex: integer; ACanvas: TCanvas;
      ALeft, ATop, AWidth, AHeight: integer; UseSVGAspectRatio: boolean = True); overload;
    // Generate bitmaps for an image list
    procedure PopulateImageList(const AImageList: TImageList; ASizes: array of integer);
  published
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Themes', [TBGRASVGImageList]);
end;

{ TBGRASVGImageList }

procedure TBGRASVGImageList.ReadData(Stream: TStream);
var
  FXMLConf: TXMLConfig;
begin
  FXMLConf := TXMLConfig.Create(Self);
  try
    Stream.Position := 0;
    FXMLConf.LoadFromStream(Stream);
    Load(FXMLConf);
  finally
    FXMLConf.Free;
  end;
end;

procedure TBGRASVGImageList.SetHeight(AValue: integer);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;
end;

procedure TBGRASVGImageList.SetWidth(AValue: integer);
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;
end;

procedure TBGRASVGImageList.WriteData(Stream: TStream);
var
  FXMLConf: TXMLConfig;
begin
  FXMLConf := TXMLConfig.Create(Self);
  try
    Save(FXMLConf);
    FXMLConf.SaveToStream(Stream);
    FXMLConf.Flush;
  finally
    FXMLConf.Free;
  end;
end;

procedure TBGRASVGImageList.Load(const XMLConf: TXMLConfig);
var
  i, j, index: integer;
begin
  try
    FItems.Clear;
    j := XMLConf.GetValue('Count', 0);
    for i := 0 to j - 1 do
    begin
      index := FItems.Add(TStringList.Create);
      FItems[index].Text := XMLConf.GetValue('Item' + i.ToString + '/SVG', '');
    end;
  finally
  end;
end;

procedure TBGRASVGImageList.Save(const XMLConf: TXMLConfig);
var
  i: integer;
begin
  try
    XMLConf.SetValue('Count', FItems.Count);
    for i := 0 to FItems.Count - 1 do
      XMLConf.SetValue('Item' + i.ToString + '/SVG', FItems[i].Text);
  finally
  end;
end;

procedure TBGRASVGImageList.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Items', ReadData, WriteData, True);
end;

constructor TBGRASVGImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TListOfTStringList.Create(True);
  FWidth := 16;
  FHeight := 16;
end;

destructor TBGRASVGImageList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TBGRASVGImageList.Add(ASVG: string): integer;
var
  list: TStringList;
begin
  list := TStringList.Create;
  list.Text := ASVG;
  Result := FItems.Add(list);
end;

procedure TBGRASVGImageList.Remove(AIndex: integer);
begin
  FItems.Remove(FItems[AIndex]);
end;

procedure TBGRASVGImageList.Exchange(AIndex1, AIndex2: integer);
begin
  FItems.Exchange(AIndex1, AIndex2);
end;

function TBGRASVGImageList.Get(AIndex: integer): string;
begin
  Result := FItems[AIndex].Text;
end;

procedure TBGRASVGImageList.Replace(AIndex: integer; ASVG: string);
begin
  FItems[AIndex].Text := ASVG;
end;

function TBGRASVGImageList.Count: integer;
begin
  Result := FItems.Count;
end;

function TBGRASVGImageList.Get(AIndex: integer; AWidth, AHeight: integer;
  UseSVGAspectRatio: boolean): TBGRABitmap;
var
  bmp: TBGRABitmap;
  svg: TBGRASVG;
begin
  bmp := TBGRABitmap.Create(AWidth, AHeight);
  svg := TBGRASVG.CreateFromString(FItems[AIndex].Text);
  try
    svg.StretchDraw(bmp.Canvas2D, 0, 0, AWidth, AHeight, UseSVGAspectRatio);
  finally
    svg.Free;
  end;
  Result := bmp;
end;

function TBGRASVGImageList.GetBitmap(AIndex: integer; AWidth, AHeight: integer;
  UseSVGAspectRatio: boolean): TBitmap;
var
  bmp: TBGRABitmap;
  ms: TMemoryStream;
begin
  bmp := Get(AIndex, AWidth, AHeight, UseSVGAspectRatio);
  ms := TMemoryStream.Create;
  bmp.Bitmap.SaveToStream(ms);
  bmp.Free;
  Result := TBitmap.Create;
  ms.Position := 0;
  Result.LoadFromStream(ms);
  ms.Free;
end;

procedure TBGRASVGImageList.Draw(AIndex: integer; ACanvas: TCanvas;
  ALeft, ATop: integer; UseSVGAspectRatio: boolean);
begin
  Draw(AIndex, ACanvas, ALeft, ATop, FWidth, FHeight, UseSVGAspectRatio);
end;

procedure TBGRASVGImageList.Draw(AIndex: integer; ACanvas: TCanvas;
  ALeft, ATop, AWidth, AHeight: integer; UseSVGAspectRatio: boolean);
var
  bmp: TBGRABitmap;
  svg: TBGRASVG;
begin
  if (AWidth = 0) or (AHeight = 0) then
    Exit;
  bmp := TBGRABitmap.Create(AWidth, AHeight);
  svg := TBGRASVG.CreateFromString(FItems[AIndex].Text);
  try
    svg.StretchDraw(bmp.Canvas2D, 0, 0, AWidth, AHeight, UseSVGAspectRatio);
    bmp.Draw(ACanvas, ALeft, ATop, False);
  finally
    bmp.Free;
    svg.Free;
  end;
end;

procedure TBGRASVGImageList.PopulateImageList(const AImageList: TImageList;
  ASizes: array of integer);
var
  i, j: integer;
  arr: array of TCustomBitmap;
begin
  AImageList.Width := ASizes[0];
  AImageList.Height := ASizes[0];
  AImageList.Scaled := True;
  AImageList.RegisterResolutions(ASizes);
  SetLength(arr, Length(ASizes));
  for j := 0 to Count - 1 do
  begin
    for i := 0 to Length(ASizes) - 1 do
      Arr[i] := GetBitmap(j, ASizes[i], ASizes[i], True);
    AImageList.AddMultipleResolutions(arr);
    for i := 0 to Length(ASizes) - 1 do
      TBitmap(Arr[i]).Free;
  end;
end;

end.
