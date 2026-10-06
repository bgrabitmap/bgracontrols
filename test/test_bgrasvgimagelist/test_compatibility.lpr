program TestSVGImageListCompatibility;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  Interfaces, Forms, Controls, Classes, SysUtils, Graphics, ImgList,
  BGRASVGImageList, BGRABitmap, BGRABitmapTypes;
const
  RedSVG = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16"><rect width="16" height="16" fill="red"/></svg>';
  BlueSVG = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16"><rect width="16" height="16" fill="blue"/></svg>';
type
  TInitializeThreads = class(TThread)
  protected
    procedure Execute; override;
  end;
  TCountingSource = class(TBGRASVGImageList)
  public
    RasterCount: Integer;
    procedure Rasterize; override;
  end;
procedure TInitializeThreads.Execute;
begin
end;
procedure TCountingSource.Rasterize;
begin
  Inc(RasterCount);
  inherited Rasterize;
end;
procedure Require(Condition: Boolean; const Message: string);
begin
  if not Condition then raise Exception.Create(Message);
end;
procedure CheckColor(Source: TBGRASVGImageList; Index: Integer; Blue: Boolean);
var
  Bitmap: TBGRABitmap;
  Size: Integer;
  Pixel: TBGRAPixel;
begin
  for Size := 16 to 17 do
  begin
    Bitmap := Source.GetBGRABitmap(Index, Size, Size);
    try
      Pixel := Bitmap.GetPixel(Size div 2, Size div 2);
      Require(Pixel.alpha = 255, 'Unexpected alpha');
      if Blue then
        Require((Pixel.blue = 255) and (Pixel.red = 0), 'Expected blue SVG')
      else
        Require((Pixel.red = 255) and (Pixel.blue = 0), 'Expected red SVG');
    finally Bitmap.Free; end;
  end;
end;
procedure TestRendering;
var
  Source: TCountingSource;
  Target: TImageList;
  Bitmap: TBitmap;
  Surface: TBGRABitmap;
  Raised: Boolean;
begin
  Source := TCountingSource.Create(nil);
  Target := TImageList.Create(nil);
  try
    Source.Add(RedSVG);
    Source.Add(BlueSVG);
    Require(Source.TargetRasterImageList = nil, 'Unexpected raster target');
    CheckColor(Source, 0, False);
    CheckColor(Source, 1, True);
    Source.Exchange(0, 1);
    CheckColor(Source, 0, True);
    Source.Remove(1);
    Source.Replace(0, RedSVG);
    CheckColor(Source, 0, False);
    Require(Source.Count = 1, 'Legacy Count changed');
    Bitmap := Source.GetBitmap(0, 24, 24);
    try Require(Bitmap.Width = 24, 'TBitmap conversion failed');
    finally Bitmap.Free; end;
    Surface := TBGRABitmap.Create(32, 32);
    try
      Source.Draw(0, Surface, RectF(0, 0, 32, 32));
      Require(Surface.GetPixel(16, 16).red = 255, 'Direct drawing failed');
    finally Surface.Free; end;
    // Existing two-argument signature and explicit rasterization without a target.
    Source.PopulateImageList(Target, [16, 24, 32]);
    Require((Target.Count = 1) and (Target.ResolutionCount = 3), 'Manual population failed');
    Require(Source.TargetRasterImageList = nil, 'Manual population changed target');
    Raised := False;
    try
      Surface := Source.GetBGRABitmap(-1, 16, 16);
      Surface.Free;
    except on E: ERangeError do Raised := True; end;
    Require(Raised, 'Invalid index not rejected');
    Source.Add('<svg><');
    Raised := False;
    try
      Surface := Source.GetBGRABitmap(1, 16, 16);
      Surface.Free;
    except on E: Exception do Raised := True; end;
    Require(Raised, 'Malformed SVG not rejected');
    Source.Remove(1);
    CheckColor(Source, 0, False);
    CheckSynchronize;
    Require(Source.RasterCount = 0, 'Automatic rasterization without a target');
  finally
    Source.Free;
    Target.Free;
  end;
  WriteLn('PASS: cached drawing, replacement, ordering, old API, invalid input');
end;
procedure TestRasterTarget;
var
  Source: TCountingSource;
  Target: TImageList;
begin
  Source := TCountingSource.Create(nil);
  Target := TImageList.Create(nil);
  try
    Source.TargetRasterImageList := Target;
    Source.Add(RedSVG);
    Source.Add(BlueSVG);
    CheckSynchronize;
    Require(Target.Count = 2, 'Automatic population failed');
    Require(Source.RasterCount = 1, Format('Rasterization was not coalesced: %d calls', [Source.RasterCount]));
    Source.Width := 32;
    Source.Height := 32;
    Source.ReferenceDPI := 192;
    CheckSynchronize;
    Require((Source.Width = 32) and (Target.Width = 32) and (Target.Count = 2), 'Resize lost entries or changed base dimensions');
    FreeAndNil(Target);
    Require(Source.TargetRasterImageList = nil, 'Destroyed target still referenced');
    Source.Replace(0, BlueSVG);
    CheckSynchronize;
    CheckColor(Source, 0, True);
  finally
    Source.Free;
    Target.Free;
  end;
  // A source destroyed before its queued rasterization must leave no callback.
  Target := TImageList.Create(nil);
  Source := TCountingSource.Create(nil);
  Source.TargetRasterImageList := Target;
  Source.Add(RedSVG);
  Source.Free;
  CheckSynchronize;
  Target.Free;
  Source := TCountingSource.Create(nil);
  Target := TImageList.Create(nil);
  Source.TargetRasterImageList := Target;
  Source.Add(RedSVG);
  Target.Free;
  CheckSynchronize;
  Require(Source.RasterCount = 0, 'Destroyed target left pending rasterization');
  Source.Free;
  WriteLn('PASS: optional raster target, resize, coalescing, destruction');
end;
procedure TestLegacyForm(const Filename: string);
var
  Root: TComponent;
  Text, Binary: TMemoryStream;
  Source: TBGRASVGImageList;
  Target: TImageList;
begin
  RegisterClasses([TForm, TImageList, TBGRASVGImageList]);
  Root := TForm.CreateNew(nil);
  Text := TMemoryStream.Create;
  Binary := TMemoryStream.Create;
  try
    Text.LoadFromFile(Filename);
    ObjectTextToBinary(Text, Binary);
    Binary.Position := 0;
    Binary.ReadComponent(Root);
    Source := TBGRASVGImageList(Root.FindComponent('SVGImages'));
    Target := TImageList(Root.FindComponent('RasterImages'));
    Require(Assigned(Source) and Assigned(Target), 'Legacy components missing');
    Require(Source.TargetRasterImageList = Target, 'Legacy target reference lost');
    Require(Source.Count = 1, 'Legacy Items payload not loaded');
    CheckColor(Source, 0, False);
    CheckSynchronize;
    Require(Target.Count = 1, 'Loaded component was not rasterized');
    // New serialization must remain readable with the same class and properties.
    Binary.Clear;
    Binary.WriteComponent(Root);
  finally
    Binary.Free;
    Text.Free;
    Root.Free;
  end;
  CheckSynchronize;
  WriteLn('PASS: unmodified legacy LFM and Items data');
end;
var
  InitializeThreads: TInitializeThreads;
begin
  try
    Application.Initialize;
    // FPC 3.2.2 executes ForceQueue immediately until a thread has been created.
    InitializeThreads := TInitializeThreads.Create(False);
    try InitializeThreads.WaitFor;
    finally InitializeThreads.Free; end;
    TestRendering;
    TestRasterTarget;
    TestLegacyForm(ParamStr(1));
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
