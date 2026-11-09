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
procedure TestOverrideColor;
const
  PartialSVG = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16"><rect width="8" height="16" fill="red" opacity="0.5"/></svg>';
var
  Source: TCountingSource;
  Target: TImageList;
  Original, Tinted, Surface: TBGRABitmap;
  SavedSVG: string;
  Raster: TBitmap;
  x, y, PreviousCount: Integer;
  Pixel, Expected: TBGRAPixel;
begin
  Source := TCountingSource.Create(nil);
  Target := TImageList.Create(nil);
  try
    Require(Source.OverrideColor = clNone, 'Unexpected default override');
    Source.Add(PartialSVG);
    SavedSVG := Source.SVGString[0];
    Original := Source.GetBGRABitmap(0, 16, 16);
    try
      Source.OverrideColor := clBlue;
      Tinted := Source.GetBGRABitmap(0, 16, 16);
      try
        for y := 0 to 15 do
          for x := 0 to 15 do
          begin
            Pixel := Tinted.GetPixel(x, y);
            Require(Pixel.alpha = Original.GetPixel(x, y).alpha, 'Tint changed alpha');
            if Pixel.alpha <> 0 then
              Require((Pixel.blue = 255) and (Pixel.red = 0), 'Tint not applied');
          end;
      finally Tinted.Free; end;
    finally Original.Free; end;
    Surface := TBGRABitmap.Create(32, 32, BGRA(0, 255, 0));
    try
      Source.Draw(0, Surface, RectF(8, 8, 24, 24));
      Pixel := Surface.GetPixel(2, 2);
      Require((Pixel.green = 255) and (Pixel.blue = 0), 'Background was tinted');
      Require(Surface.GetPixel(10, 16).blue > 0, 'Direct drawing ignored tint');
    finally Surface.Free; end;
    Surface := TBGRABitmap.Create(64, 64, BGRA(0, 255, 0));
    try
      Surface.Canvas2D.translate(32, 32);
      Source.Draw(0, Surface, RectF(0, 0, 16, 16));
      Require(Surface.GetPixel(34, 40).blue > 0, 'Canvas transform ignored');
      Pixel := Surface.GetPixel(10, 16);
      Require((Pixel.green = 255) and (Pixel.blue = 0), 'Drawn outside transform');
      Pixel := Surface.GetPixel(44, 40);
      Require((Pixel.green = 255) and (Pixel.blue = 0), 'Transparent area tinted');
    finally Surface.Free; end;
    Surface := TBGRABitmap.Create(64, 64, BGRA(0, 255, 0));
    try
      Surface.Canvas2D.translate(16, 16);
      Surface.Canvas2D.beginPath;
      Surface.Canvas2D.rect(0, 0, 4, 16);
      Surface.Canvas2D.clip;
      Surface.Canvas2D.globalAlpha := 0.5;
      Source.Draw(0, Surface, RectF(0, 0, 16, 16));
      Pixel := Surface.GetPixel(18, 24);
      Require(Pixel.blue > 0, 'Clipping removed visible tinted pixels');
      Pixel := Surface.GetPixel(22, 24);
      Require((Pixel.green = 255) and (Pixel.blue = 0),
        'Tinted drawing ignored Canvas2D clipping');
      Require(Abs(Surface.Canvas2D.globalAlpha - 0.5) < 0.01,
        'Tinted drawing changed canvas alpha');
      // Drawing again without the override must use the original transform/clip.
      Source.OverrideColor := clNone;
      Source.Draw(0, Surface, RectF(0, 0, 16, 16));
      Require(Surface.GetPixel(18, 24).red > 0, 'Canvas transform not restored');
      Require(Surface.GetPixel(22, 24).green = 255, 'Canvas clip not restored');
      Source.OverrideColor := clBlue;
    finally Surface.Free; end;
    Tinted := Source.GetBGRABitmap(0, 16, 16);
    try
      Pixel := Tinted.GetPixel(12, 8);
      Require((Pixel.alpha = 0) and (Pixel.blue = 0), 'Transparent pixel recolored');
    finally Tinted.Free; end;
    Source.TargetRasterImageList := Target;
    CheckSynchronize;
    PreviousCount := Source.RasterCount;
    Source.OverrideColor := clBtnText;
    Source.Refresh;
    CheckSynchronize;
    Require(Source.RasterCount = PreviousCount + 1, 'Color change not coalesced');
    Tinted := Source.GetBGRABitmap(0, 16, 16);
    try
      Expected := ColorToBGRA(ColorToRGB(clBtnText));
      Pixel := Tinted.GetPixel(2, 8);
      Require((Pixel.red = Expected.red) and (Pixel.green = Expected.green) and
        (Pixel.blue = Expected.blue), 'System color not resolved');
    finally Tinted.Free; end;
    PreviousCount := Source.RasterCount;
    Source.Refresh;
    CheckSynchronize;
    Require(Source.RasterCount = PreviousCount + 1, 'Theme refresh not rasterized');
    // Use an opaque fixture so native bitmap alpha handling cannot affect RGB.
    Source.Replace(0, RedSVG);
    Source.OverrideColor := clBlue;
    CheckSynchronize;
    Raster := TBitmap.Create;
    try
      Target.GetBitmap(0, Raster);
      Require(ColorToRGB(Raster.Canvas.Pixels[8, 8]) = ColorToRGB(clBlue),
        'Raster target did not receive override color');
      Source.OverrideColor := clNone;
      CheckSynchronize;
      Target.GetBitmap(0, Raster);
      Require(ColorToRGB(Raster.Canvas.Pixels[8, 8]) = ColorToRGB(clRed),
        'Raster target did not restore original color');
    finally Raster.Free; end;
    Source.Replace(0, SavedSVG);
    Source.OverrideColor := clNone;
    Tinted := Source.GetBGRABitmap(0, 16, 16);
    try Require(Tinted.GetPixel(2, 8).red = 255, 'Original color not restored');
    finally Tinted.Free; end;
    Require(Source.SVGString[0] = SavedSVG, 'SVG source was modified');
  finally
    Source.Free;
    Target.Free;
  end;
  WriteLn('PASS: color override, alpha, background, system colors, refresh');
end;
procedure TestOverrideEquivalence;
const
  SVGs: array[0..1] of string = (
    '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16"><rect width="8" height="16" fill="red" opacity="0.5"/></svg>',
    '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16"><rect width="16" height="16" fill="none" stroke="red" stroke-width="8"/></svg>');
var
  Source, Loaded: TBGRASVGImageList;
  Original, Tinted: TBGRABitmap;
  Stream: TMemoryStream;
  i, x, y: Integer;
  a, b: TBGRAPixel;
begin
  Source := TBGRASVGImageList.Create(nil);
  try
    Source.Add(SVGs[0]);
    for i := Low(SVGs) to High(SVGs) do
    begin
      Source.Replace(0, SVGs[i]);
      Original := TBGRABitmap.Create(48, 48, BGRA(0, 255, 0));
      Tinted := TBGRABitmap.Create(48, 48, BGRA(0, 255, 0));
      try
        Original.Canvas2D.linearBlend := false;
        Tinted.Canvas2D.linearBlend := false;
        Source.OverrideColor := clNone;
        Source.Draw(0, Original, RectF(16, 16, 32, 32));
        Source.OverrideColor := clRed;
        Source.Draw(0, Tinted, RectF(16, 16, 32, 32));
        Require(not Tinted.Canvas2D.linearBlend, 'Canvas blending not restored');
        for y := 0 to 47 do
          for x := 0 to 47 do
          begin
            a := Original.GetPixel(x, y);
            b := Tinted.GetPixel(x, y);
            Require((a.red = b.red) and (a.green = b.green) and
              (a.blue = b.blue) and (a.alpha = b.alpha),
              Format('Override changed rendering: case %d at %d,%d', [i, x, y]));
          end;
      finally
        Tinted.Free;
        Original.Free;
      end;
    end;
    Stream := TMemoryStream.Create;
    Loaded := TBGRASVGImageList.Create(nil);
    try
      Source.OverrideColor := clBtnText;
      Stream.WriteComponent(Source);
      Stream.Position := 0;
      Stream.ReadComponent(Loaded);
      Require(Loaded.OverrideColor = clBtnText, 'Override lost during streaming');
      Require(Loaded.SVGString[0] = Source.SVGString[0], 'Streaming changed SVG');
    finally
      Loaded.Free;
      Stream.Free;
    end;
  finally Source.Free; end;
  WriteLn('PASS: override rendering equivalence and streaming');
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
    TestOverrideColor;
    TestOverrideEquivalence;
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
