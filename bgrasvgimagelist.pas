unit BGRASVGImageList;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, FGL,
  XMLConf;

type

  TListOfTStringList = TFPGObjectList<TStringList>;

  { TBGRASVGImageList }

  TBGRASVGImageList = class(TComponent)
  private
    FItems: TListOfTStringList;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  protected
    procedure Load(const XMLConf: TXMLConfig);
    procedure Save(const XMLConf: TXMLConfig);
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(ASVG: String): Integer;
    procedure Remove(AIndex: Integer);
    procedure Exchange(AIndex1, AIndex2: Integer);
    function Get(AIndex: Integer): String;
    procedure Replace(AIndex: Integer; ASVG: String);
  published

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Themes',[TBGRASVGImageList]);
end;

{ TBGRASVGImageList }

procedure TBGRASVGImageList.ReadData(Stream: TStream);
var
  FXMLConf: TXMLConfig;
begin
  FXMLConf := TXMLConfig.Create(Self);
  try
    FXMLConf.RootName := 'BGRASVGImageList';
    Stream.Position := 0;
    FXMLConf.LoadFromStream(Stream);
    Load(FXMLConf);
  finally
    FXMLConf.Free;
  end;
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
    XMLConf.RootName := 'BGRASVGImageList';
    j := XMLConf.GetValue('Count', 0);
    for i:=0 to j-1 do
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
    XMLConf.RootName := 'BGRASVGImageList';
    XMLConf.SetValue('Count', FItems.Count);
    for i:=0 to FItems.Count-1 do
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
end;

destructor TBGRASVGImageList.Destroy;
begin
  inherited Destroy;
end;

function TBGRASVGImageList.Add(ASVG: String): Integer;
var
  list: TStringList;
begin
  list := TStringList.Create;
  list.Text := ASVG;
  Result := FItems.Add(list);
end;

procedure TBGRASVGImageList.Remove(AIndex: Integer);
begin
  FItems.Remove(FItems[AIndex]);
end;

procedure TBGRASVGImageList.Exchange(AIndex1, AIndex2: Integer);
begin
  FItems.Exchange(AIndex1, AIndex2);
end;

function TBGRASVGImageList.Get(AIndex: Integer): String;
begin
  Result := FItems[AIndex].Text;
end;

procedure TBGRASVGImageList.Replace(AIndex: Integer; ASVG: String);
begin
  FItems[AIndex].Text := ASVG;
end;

end.
