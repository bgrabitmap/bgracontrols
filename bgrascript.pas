unit BGRAScript;

{$mode objfpc}{$H+}
{ $define debug}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, Dialogs;

{Template}
procedure SynCompletionList(itemlist: TStrings);
{Scripting}
function ScriptCommand(command: string; var bitmap: TBGRABitmap;
  var variables: TStringList; var line: integer): boolean;
function ScriptCommandList(commandlist: TStrings; var bitmap: TBGRABitmap): boolean;

{Tools}
function StrToDrawMode(mode: string): TDrawMode;

implementation

procedure SynCompletionList(itemlist: TStrings);
begin
  with itemlist do
  begin
    {Assign key values}
    Add('let key "value"');
    {Goto line}
    Add('goto 10');
    {Messages}
    Add('print "Message"');
    Add('input "Title","Message","Default value",result');
    {Read Values}
    Add('GetWidth width');
    Add('GetHeight height');
    {TFPCustomImage override}
    Add('SetSize 320,240');
    {Loading functions}
    Add('SaveToFile "file.png"');
    {Loading functions}
    Add('SetHorizLine 0,0,100,"rgba(0,0,0,1)"');
    Add('XorHorizLine 0,0,100,"rgba(0,0,0,1)"');
    Add('DrawHorizLine 0,0,100,"rgba(0,0,0,1)"');
    Add('FastBlendHorizLine 0,0,100,"rgba(0,0,0,1)"');
    Add('AlphaHorizLine 0,0,100,"rgba(0,0,0,1)"');
    Add('SetVertLine 0,0,100,"rgba(0,0,0,1)"');
    Add('XorVertLine 0,0,100,"rgba(0,0,0,1)"');
    Add('DrawVertLine 0,0,100,"rgba(0,0,0,1)"');
    Add('FastBlendVertLine 0,0,100,"rgba(0,0,0,1)"');
    Add('AlphaVertLine 0,0,100,"rgba(0,0,0,1)"');
    Add('DrawHorizLinediff 0,0,100,"rgba(0,0,0,1)","rgba(255,255,255,1)",128');
    //--
    Add('FillTransparent');
    Add('Rectangle 0,0,100,100,"rgba(0,0,0,1)","rgba(255,255,255,1)","dmDrawWithTransparency"');
    Add('RectangleAntiAlias "0,5","0,5","99,5","99,5","rgba(0,0,0,1)","1,5","rgba(255,255,255,1)"');
    {BGRA bitmap functions}
    Add('RotateCW');
    Add('RotateCCW');
    Add('Negative');
    Add('NegativeRect 0,0,100,100');
    Add('LinearNegative');
    Add('LinearNegativeRect 0,0,100,100');
    Add('InplaceGrayscale');
    Add('InplaceGrayscaleRect 0,0,100,100');
    Add('SwapRedBlue');
    Add('GrayscaleToAlpha');
    Add('AlphaToGrayscale');
    Add('ApplyGlobalOpacity 128');
    Add('ConvertToLinearRGB');
    Add('ConvertFromLinearRGB');
    Add('DrawCheckers 0,0,100,100,"rgba(100,100,100,255)","rgba(0,0,0,0)"');
    {Custom functions}
    Add('VerticalFlip 0,0,100,100');
    Add('HorizontalFlip 0,0,100,100');
    Add('BlendBitmap 0,0,"file.png","boTransparent"');
    Add('BlendBitmapOver 0,0,"file.png","boTransparent",255,"False"');
    Add('ApplyBitmapMask "file.png",0,0,100,100,0,0');
    {Filters}
    Add('FilterFastBlur 5,"False"');
    Add('FilterSmooth "False"');
    Add('FilterSharpen 5,"False"');
    Add('FilterContour');
    Add('FilterEmboss "1,5"');
    Add('FilterNormalize "True"');
    Add('FilterSphere "True"');
    Add('FilterCylinder "True"');
    Add('FilterPlane "True"');
  end;
end;

function ScriptCommand(command: string; var bitmap: TBGRABitmap;
  var variables: TStringList; var line: integer): boolean;

  function ParamCheck(passed, mustbe: integer): boolean;
  begin
    Result := True;
    if passed <> mustbe then
      Result := False;

  {$ifdef debug}
    if not Result then
    begin
      writeln('>> Wrong number of parameters: ' + IntToStr(passed));
      writeln('>> Must be: ' + IntToStr(mustbe));
    end;
  {$endif}
  end;

  function ParamCheckAtLeast(passed, mustbe: integer): boolean;
  begin
    Result := True;
    if passed < mustbe then
      Result := False;

  {$ifdef debug}
    if not Result then
    begin
      writeln('>> Wrong number of parameters: ' + IntToStr(passed));
      writeln('>> At least must be: ' + IntToStr(mustbe));
    end;
  {$endif}
  end;

var
  list: TStringList;
  passed: integer;
  tmpbmp1: TBGRABitmap;
  i: integer;
  a: string;
begin
  { $ifdef debug}
  //writeln('---Script-Command---');
  { $endif}

  Result := True;
  list := TStringList.Create;
  list.CommaText := command;
  passed := list.Count;

  {Replace values in variable names}
  for i := 0 to list.Count - 1 do
    if variables.Values[list[i]] <> '' then
      list[i] := variables.Values[list[i]];

  case LowerCase(list[0]) of
    {Assign key values}
    'let':
    begin
      Result := ParamCheck(passed, 3);
      if Result then
        variables.Add(list[1] + '=' + list[2]);
    end;

    {Messages}
    'input':
    begin
      Result := ParamCheck(passed, 5);
      if Result then
      begin
        a := InputBox(list[1],list[2],list[3]);
        variables.Add(list[4] + '=' + a);
      end;
    end;

    'print':
    begin
      Result := ParamCheckAtLeast(passed, 2);
      if Result then
      begin
        a := '';
        for i:=1 to passed -1 do
         a := a + list[i];
        ShowMessage(a);
      end;
    end;

    {GoTo}
    'goto':
    begin
      Result := ParamCheck(passed,2);
      if Result then
      begin
        line := StrToInt(list[1]) - 2;
        if line < 0 then
          line := -1;
      end;
    end;

    {Read values}
    'getwidth':
    begin
      Result := ParamCheck(passed, 2);
      if Result then
        variables.Add(list[1] + '=' + IntToStr(bitmap.Width));
    end;

    'getheight':
    begin
      Result := ParamCheck(passed, 2);
      if Result then
        variables.Add(list[1] + '=' + IntToStr(bitmap.Height));
    end;

    {TFPCustomImage override}
    'setsize':
    begin
      Result := ParamCheck(passed, 3);
      if Result then
        bitmap.SetSize(StrToInt(list[1]), StrToInt(list[2]));
    end;

    {Loading functions}
    'savetofile':
    begin
      Result := ParamCheck(passed, 2);
      if Result then
        bitmap.SaveToFile(list[1]);
    end;

    {Pixel functions}

    {Loading functions}
    {* Horiz *}
    'sethorizline':
    begin
      Result := ParamCheck(passed, 5);
      if Result then
        bitmap.SetHorizLine(StrToInt(list[1]), StrToInt(list[2]),
          StrToInt(list[3]), StrToBGRA(list[4]));
    end;
    'xorhorizline':
    begin
      Result := ParamCheck(passed, 5);
      if Result then
        bitmap.XorHorizLine(StrToInt(list[1]), StrToInt(list[2]),
          StrToInt(list[3]), StrToBGRA(list[4]));
    end;
    'drawhorizline':
    begin
      Result := ParamCheck(passed, 5);
      if Result then
        bitmap.DrawHorizLine(StrToInt(list[1]), StrToInt(list[2]),
          StrToInt(list[3]), StrToBGRA(list[4]));
    end;
    'fastblendhorizline':
    begin
      Result := ParamCheck(passed, 5);
      if Result then
        bitmap.FastBlendHorizLine(StrToInt(list[1]), StrToInt(list[2]),
          StrToInt(list[3]), StrToBGRA(list[4]));
    end;
    'alphahorizline':
    begin
      Result := ParamCheck(passed, 5);
      if Result then
        bitmap.AlphaHorizLine(StrToInt(list[1]), StrToInt(list[2]),
          StrToInt(list[3]), StrToInt(list[4]));
    end;
    {* Vert *}
    'setvertline':
    begin
      Result := ParamCheck(passed, 5);
      if Result then
        bitmap.SetVertLine(StrToInt(list[1]), StrToInt(list[2]),
          StrToInt(list[3]), StrToBGRA(list[4]));
    end;
    'xorvertline':
    begin
      Result := ParamCheck(passed, 5);
      if Result then
        bitmap.XorVertLine(StrToInt(list[1]), StrToInt(list[2]),
          StrToInt(list[3]), StrToBGRA(list[4]));
    end;
    'drawvertline':
    begin
      Result := ParamCheck(passed, 5);
      if Result then
        bitmap.DrawVertLine(StrToInt(list[1]), StrToInt(list[2]),
          StrToInt(list[3]), StrToBGRA(list[4]));
    end;
    'fastblendvertline':
    begin
      Result := ParamCheck(passed, 5);
      if Result then
        bitmap.FastBlendVertLine(StrToInt(list[1]), StrToInt(list[2]),
          StrToInt(list[3]), StrToBGRA(list[4]));
    end;
    'alphavertline':
    begin
      Result := ParamCheck(passed, 5);
      if Result then
        bitmap.AlphaVertLine(StrToInt(list[1]), StrToInt(list[2]),
          StrToInt(list[3]), StrToInt(list[4]));
    end;
    {* Misc *}
    'drawhorizlinediff':
    begin
      Result := ParamCheck(passed, 7);
      if Result then
        bitmap.DrawHorizLineDiff(StrToInt(list[1]), StrToInt(list[2]),
          StrToInt(list[3]), StrToBGRA(list[4]), StrToBGRA(list[5]), StrToInt(list[6]));
    end;

    //---
    'filltransparent':
    begin
      Result := ParamCheck(passed, 1);
      if Result then
        bitmap.FillTransparent;
    end;

    'rectangle':
    begin
      Result := ParamCheck(passed, 8);
      if Result then
        bitmap.Rectangle(StrToInt(list[1]), StrToInt(list[2]), StrToInt(
          list[3]), StrToInt(list[4]), StrToBGRA(list[5]), StrToBGRA(list[6]),
          StrToDrawMode(list[7]));
    end;

    'rectangleantialias':
    begin
      Result := ParamCheck(passed, 8);
      if Result then
        bitmap.RectangleAntialias(StrToFloat(list[1]), StrToFloat(list[2]),
          StrToFloat(list[3]), StrToFloat(list[4]), StrToBGRA(list[5]),
          StrToFloat(list[6]), StrToBGRA(list[7]));
    end;

    {BGRA bitmap functions}
    'verticalflip':
    begin
      Result := ParamCheck(passed, 5);
      if Result then
        bitmap.VerticalFlip(Rect(StrToInt(list[1]), StrToInt(list[2]),
          StrToInt(list[3]), StrToInt(list[4])));
    end;
    'horizontalflip':
    begin
      Result := ParamCheck(passed, 5);
      if Result then
        bitmap.HorizontalFlip(Rect(StrToInt(list[1]), StrToInt(list[2]),
          StrToInt(list[3]), StrToInt(list[4])));
    end;
    'rotatecw':
    begin
      Result := ParamCheck(passed, 1);
      if Result then
        try
          tmpbmp1 := bitmap.RotateCW as TBGRABitmap;
          bitmap.FillTransparent;
          bitmap.BlendImage(0, 0, tmpbmp1, boLinearBlend);
        finally
          tmpbmp1.Free;
        end;
    end;
    'rotateccw':
    begin
      Result := ParamCheck(passed, 1);
      if Result then
        try
          tmpbmp1 := bitmap.RotateCCW as TBGRABitmap;
          bitmap.FillTransparent;
          bitmap.BlendImage(0, 0, tmpbmp1, boLinearBlend);
        finally
          tmpbmp1.Free;
        end;
    end;
    'negative':
    begin
      Result := ParamCheck(passed, 1);
      if Result then
        bitmap.Negative;
    end;
    'negativerect':
    begin
      Result := ParamCheck(passed, 5);
      if Result then
        bitmap.NegativeRect(Rect(StrToInt(list[1]), StrToInt(list[2]),
          StrToInt(list[3]), StrToInt(list[4])));
    end;
    'linearnegative':
    begin
      Result := ParamCheck(passed, 1);
      if Result then
        bitmap.LinearNegative;
    end;
    'linearnegativerect':
    begin
      Result := ParamCheck(passed, 5);
      if Result then
        bitmap.LinearNegativeRect(Rect(StrToInt(list[1]), StrToInt(list[2]),
          StrToInt(list[3]), StrToInt(list[4])));
    end;
    'inplacegrayscale':
    begin
      Result := ParamCheck(passed, 1);
      if Result then
        bitmap.InplaceGrayscale;
    end;
    'inplacegrayscalerect':
    begin
      Result := ParamCheck(passed, 5);
      if Result then
        bitmap.InplaceGrayscale(Rect(StrToInt(list[1]), StrToInt(list[2]),
          StrToInt(list[3]), StrToInt(list[4])));
    end;
    'swapredblue':
    begin
      Result := ParamCheck(passed, 1);
      if Result then
        bitmap.SwapRedBlue;
    end;
    'grayscaletoalpha':
    begin
      Result := ParamCheck(passed, 1);
      if Result then
        bitmap.GrayscaleToAlpha;
    end;
    'alphatograyscale':
    begin
      Result := ParamCheck(passed, 1);
      if Result then
        bitmap.AlphaToGrayscale;
    end;
    'applyglobalopacity':
    begin
      Result := ParamCheck(passed, 2);
      if Result then
        bitmap.ApplyGlobalOpacity(StrToInt(list[1]));
    end;
    'converttolinearrgb':
    begin
      Result := ParamCheck(passed, 1);
      if Result then
        bitmap.ConvertToLinearRGB;
    end;
    'convertfromlinearrgb':
    begin
      Result := ParamCheck(passed, 1);
      if Result then
        bitmap.ConvertFromLinearRGB;
    end;
    'drawcheckers':
    begin
      Result := ParamCheck(passed, 7);
      if Result then
        bitmap.DrawCheckers(Rect(StrToInt(list[1]), StrToInt(list[2]),
          StrToInt(list[3]), StrToInt(list[4])), StrToBGRA(list[5]), StrToBGRA(list[6]));
    end;

    {Filters}

    {Custom Functions}
    'blendbitmap':
    begin
      Result := ParamCheck(passed, 5);
      if Result then
        try
          tmpbmp1 := TBGRABitmap.Create(list[3]);
          bitmap.BlendImage(StrToInt(list[1]), StrToInt(list[2]), tmpbmp1,
            StrToBlendOperation(list[4]));
        finally
          tmpbmp1.Free;
        end;
    end;
    'blendbitmapover':
    begin
      Result := ParamCheck(passed, 7);
      if Result then
        try
          tmpbmp1 := TBGRABitmap.Create(list[3]);
          bitmap.BlendImageOver(StrToInt(list[1]), StrToInt(list[2]),
            tmpbmp1, StrToBlendOperation(list[4]), StrToInt(list[5]),
            StrToBool(list[6]));
        finally
          tmpbmp1.Free;
        end;
    end;
    'applybitmapmask':
    begin
      Result := ParamCheck(passed, 8);
      if Result then
        try
          tmpbmp1 := TBGRABitmap.Create(list[1]);
          bitmap.ApplyMask(tmpbmp1, Rect(StrToInt(list[2]), StrToInt(
            list[3]), StrToInt(list[4]), StrToInt(list[5])), Point(
            StrToInt(list[6]), StrToInt(list[7])));
        finally
          tmpbmp1.Free;
        end;
    end;
    'filterfastblur':
    begin
      Result := ParamCheck(passed, 3);
      if Result then
      begin
        tmpbmp1 := bitmap.FilterBlurRadial(StrToInt(list[1]), rbFast) as TBGRABitmap;
        if StrToBool(list[2]) then
          bitmap.FillTransparent;
        bitmap.BlendImage(0, 0, tmpbmp1, boLinearBlend);
        tmpbmp1.Free;
      end;
    end;
    'filtersmooth':
    begin
      Result := ParamCheck(passed, 2);
      if Result then
      begin
        tmpbmp1 := bitmap.FilterSmooth as TBGRABitmap;
        if StrToBool(list[1]) then
          bitmap.FillTransparent;
        bitmap.BlendImage(0, 0, tmpbmp1, boLinearBlend);
        tmpbmp1.Free;
      end;
    end;
    'filtersharpen':
    begin
      Result := ParamCheck(passed, 3);
      if Result then
      begin
        tmpbmp1 := bitmap.FilterSharpen(StrToInt(list[1])) as TBGRABitmap;
        if StrToBool(list[2]) then
          bitmap.FillTransparent;
        bitmap.BlendImage(0, 0, tmpbmp1, boLinearBlend);
        tmpbmp1.Free;
      end;
    end;
    'filtercontour':
    begin
      Result := ParamCheck(passed, 1);
      if Result then
      begin
        tmpbmp1 := bitmap.FilterContour as TBGRABitmap;
        bitmap.BlendImage(0, 0, tmpbmp1, boLinearBlend);
        tmpbmp1.Free;
      end;
    end;
    'filteremboss':
    begin
      Result := ParamCheck(passed, 2);
      if Result then
      begin
        tmpbmp1 := bitmap.FilterEmboss(StrToFloat(list[1])) as TBGRABitmap;
        bitmap.BlendImage(0, 0, tmpbmp1, boLinearBlend);
        tmpbmp1.Free;
      end;
    end;
    'filternormalize':
    begin
      Result := ParamCheck(passed, 2);
      if Result then
      begin
        tmpbmp1 := bitmap.FilterNormalize(StrToBool(list[1])) as TBGRABitmap;
        bitmap.FillTransparent;
        bitmap.BlendImage(0, 0, tmpbmp1, boLinearBlend);
        tmpbmp1.Free;
      end;
    end;
    'filtersphere':
    begin
      Result := ParamCheck(passed, 2);
      if Result then
      begin
        tmpbmp1 := bitmap.FilterSphere as TBGRABitmap;
        if StrToBool(list[1]) then
          bitmap.FillTransparent;
        bitmap.BlendImage(0, 0, tmpbmp1, boLinearBlend);
        tmpbmp1.Free;
      end;
    end;
    'filtercylinder':
    begin
      Result := ParamCheck(passed, 2);
      if Result then
      begin
        tmpbmp1 := bitmap.FilterCylinder as TBGRABitmap;
        if StrToBool(list[1]) then
          bitmap.FillTransparent;
        bitmap.BlendImage(0, 0, tmpbmp1, boLinearBlend);
        tmpbmp1.Free;
      end;
    end;
    'filterplane':
    begin
      Result := ParamCheck(passed, 2);
      if Result then
      begin
        tmpbmp1 := bitmap.FilterPlane as TBGRABitmap;
        if StrToBool(list[1]) then
          bitmap.FillTransparent;
        bitmap.BlendImage(0, 0, tmpbmp1, boLinearBlend);
        tmpbmp1.Free;
      end;
    end;

    '//':
    begin
      // comment
    end;

    '{':
    begin
      { comment }
    end;

    else
    begin
      {$ifdef debug}
      writeln('>> Command "' + list[0] + '" not found.');
      {$endif}
      Result := False;
    end;
  end;

  {$ifdef debug}
  if not Result then
    writeln('>> ERROR');
  for i := 0 to list.Count - 1 do
    writeln(' ' + list[i]);
  writeln('____________________');
  {$endif}

  list.Free;
end;

function ScriptCommandList(commandlist: TStrings; var bitmap: TBGRABitmap): boolean;
var
  line: integer;
  variables: TStringList;
begin
  {$ifdef debug}
  //writeln('----SCRIPT--LIST----');
  writeln(' Executing ' + IntToStr(commandlist.Count) + ' lines...');
  writeln('____________________');
  {$endif}

  variables := TStringList.Create;

  {Result := True;
  for i := 0 to commandlist.Count - 1 do
    if commandlist[i] <> '' then
      ScriptCommand(commandlist[i], bitmap, variables);
  }
  Result := True;
  line := 0;
  repeat
    if commandlist[line] <> '' then
      ScriptCommand(commandlist[line], bitmap, variables, line);
    Inc(line);
  until line > commandList.Count;

  variables.Free;

  {$ifdef debug}
  //writeln('----SCRIPT--LIST----');
  writeln(' END');
  writeln('____________________');
  {$endif}
end;

function StrToDrawMode(mode: string): TDrawMode;
begin
  case LowerCase(mode) of
    'dmset': Result := dmSet;
    'dmsetexcepttransparent': Result := dmSetExceptTransparent;
    'dmlinearblend': Result := dmLinearBlend;
    'dmdrawwithtransparency': Result := dmDrawWithTransparency;
    'dmxor': Result := dmXor;
    else
      Result := dmDrawWithTransparency;
  end;
end;

end.
