program TestSVGRasterImageList;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  Interfaces, Forms, Controls, Buttons, Classes, SysUtils, Graphics,
  BGRASVGImageList;
const
  RedSVG = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16"><rect width="16" height="16" fill="red"/></svg>';
  BlueSVG = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16"><rect width="16" height="16" fill="blue"/></svg>';
type
  TInitializeThreads = class(TThread)
  protected
    procedure Execute; override;
  end;
procedure TInitializeThreads.Execute;
begin
end;
procedure Require(Value: Boolean; const Message: string);
begin
  if not Value then raise Exception.Create(Message);
end;
procedure CheckColor(List: TImageList; Index: Integer; Color: TColor);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    List.GetBitmap(Index, Bitmap);
    Require((Bitmap.Width = List.Width) and (Bitmap.Height = List.Height), 'Native bitmap dimensions changed');
    Require(Bitmap.Canvas.Pixels[Bitmap.Width div 2, Bitmap.Height div 2] = Color, 'Native bitmap color changed');
  finally Bitmap.Free; end;
end;
procedure TestEntries;
var
  List: TBGRASVGRasterImageList;
  Native: TImageList;
  Button: TBitBtn;
  Raised: Boolean;
begin
  List := TBGRASVGRasterImageList.Create(nil);
  Button := TBitBtn.Create(nil);
  try
    Native := List;
    Require(List.AddSVG(RedSVG) = 0, 'First SVG index changed');
    Require(List.AddSVG(BlueSVG) = 1, 'Second SVG index changed');
    Require((List.SVGCount = 2) and (Native.Count = 2), 'Source and raster counts differ');
    Button.Images := List;
    Button.ImageIndex := 0;
    Require(Button.Images = List, 'Cannot assign to standard Images property');
    CheckColor(Native, 0, clRed);
    List.ExchangeSVG(0, 1);
    CheckColor(Native, 0, clBlue);
    List.RemoveSVG(1);
    List.ReplaceSVG(0, RedSVG);
    CheckColor(Native, 0, clRed);
    List.Width := 32;
    List.Height := 24;
    Require((Native.Width = 32) and (Native.Height = 24) and (Native.Count = 1), 'Typed resize lost SVGs');
    Native.Width := 24;
    Native.Height := 24;
    CheckSynchronize;
    Require((List.Width = 24) and (List.Height = 24) and (Native.Count = 1), 'Base-class resize lost SVGs');
    Require(Native.WidthForPPI[0, 192] = 48, 'Native DPI scaling failed');
    List.RegisterResolutions([72, 48, 72]);
    Require(List.ResolutionCount = 5, 'Custom resolutions or deduplication failed');
    List.Scaled := False;
    List.ReplaceSVG(0, BlueSVG);
    Require(not List.Scaled, 'Rasterization overwrote Scaled');
    Native.Clear;
    Require((List.SVGCount = 0) and (Native.Count = 0), 'Native Clear left SVG sources');
    List.Width := 1;
    List.Height := 1;
    List.AddSVG(RedSVG);
    Require(List.ResolutionCount = 2, 'Small dimensions produced duplicate resolutions');
    List.ClearSVG;
    Raised := False;
    try List.AddSVG('<svg><');
    except on E: Exception do Raised := True; end;
    Require(Raised, 'Malformed SVG not rejected');
    List.ClearSVG;
    List.AddSVG(BlueSVG);
    Require(Native.Count = 1, 'Cannot recover after invalid SVG');
  finally
    Button.Free;
    List.Free;
  end;
  CheckSynchronize;
  WriteLn('PASS: standard Images, SVG operations, dimensions, DPI, clearing, invalid input');
end;
procedure TestStreaming;
var
  Root, CopyRoot: TForm;
  List, CopyList: TBGRASVGRasterImageList;
  Stream: TMemoryStream;
begin
  RegisterClasses([TForm, TBGRASVGRasterImageList]);
  Root := TForm.CreateNew(nil);
  CopyRoot := TForm.CreateNew(nil);
  Stream := TMemoryStream.Create;
  try
    Root.Name := 'SVGRoot';
    List := TBGRASVGRasterImageList.Create(Root);
    List.Name := 'SVGImages';
    List.Width := 24;
    List.Height := 24;
    List.AddSVG(RedSVG);
    List.RegisterResolutions([72]);
    Stream.WriteComponent(Root);
    Stream.Position := 0;
    Stream.ReadComponent(CopyRoot);
    CheckSynchronize;
    CopyList := TBGRASVGRasterImageList(CopyRoot.FindComponent('SVGImages'));
    Require(Assigned(CopyList), 'New component not streamed');
    Require((CopyList.SVGCount = 1) and (CopyList.Count = 1), 'SVGItems not restored');
    Require((CopyList.Width = 24) and (CopyList.Height = 24), 'Dimensions not restored');
    Require(CopyList.ResolutionCount = 5, 'Custom resolutions not restored');
    CheckColor(CopyList, 0, clRed);
    CopyList.ReplaceSVG(0, BlueSVG);
    CheckColor(CopyList, 0, clBlue);
  finally
    Stream.Free;
    CopyRoot.Free;
    Root.Free;
  end;
  CheckSynchronize;
  WriteLn('PASS: component streaming, SVGItems, resolutions, editing after load');
end;
var
  Thread: TInitializeThreads;
begin
  try
    Application.Initialize;
    Thread := TInitializeThreads.Create(False);
    try Thread.WaitFor;
    finally Thread.Free; end;
    TestEntries;
    TestStreaming;
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
