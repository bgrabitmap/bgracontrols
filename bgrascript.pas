unit BGRAScript;

{$mode objfpc}{$H+}
{$define debug}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes;

{Template}
procedure SynCompletionList(itemlist: TStrings);
{Scripting}
function ScriptCommand(command: string; bitmap: TBGRABitmap): boolean;
function ScriptCommandList(commandlist: TStrings; bitmap: TBGRABitmap): boolean;

implementation

procedure SynCompletionList(itemlist: TStrings);
begin
  with itemlist do
  begin
    {TFPCustomImage override}
    Add('SetSize,320,240');
    {Loading functions}
    Add('SaveToFile,"file.png"');
    {Loading functions}
    Add('SetHorizLine,0,0,100,"rgba(0,0,0,255)"');
    Add('XorHorizLine,0,0,100,"rgba(0,0,0,255)"');
    Add('DrawHorizLine,0,0,100,"rgba(0,0,0,255)"');
    Add('FastBlendHorizLine,0,0,100,"rgba(0,0,0,255)"');
    Add('AlphaHorizLine,0,0,100,"rgba(0,0,0,255)"');
    Add('SetVertLine,0,0,100,"rgba(0,0,0,255)"');
    Add('XorVertLine,0,0,100,"rgba(0,0,0,255)"');
    Add('DrawVertLine,0,0,100,"rgba(0,0,0,255)"');
    Add('FastBlendVertLine,0,0,100,"rgba(0,0,0,255)"');
    Add('AlphaVertLine,0,0,100,"rgba(0,0,0,255)"');
    Add('DrawHorizLinediff,0,0,100,"rgba(0,0,0,255)","rgba(255,255,255,255)",128');
    //--
    Add('FillTransparent');
    Add('Rectangle,0,0,100,100,"rgba(0,0,0,255)","rgba(255,255,255,255)"');
    Add('RectangleAntiAlias,"0,5","0,5","99,5","99,5","rgba(0,0,0,255)","1,5","rgba(255,255,255,255)"');
  end;
end;

function ScriptCommand(command: string; bitmap: TBGRABitmap): boolean;

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

var
  list: TStringList;
  passed: integer;
  {$ifdef debug}
  i: integer;
  {$endif}
begin
  {$ifdef debug}
  writeln('---Script Command---');
  {$endif}

  Result := True;
  list := TStringList.Create;
  list.CommaText := command;
  passed := list.Count;

  case LowerCase(list[0]) of

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
      Result := ParamCheck(passed, 7);
      if Result then
        bitmap.Rectangle(StrToInt(list[1]), StrToInt(list[2]), StrToInt(
          list[3]), StrToInt(list[4]), StrToBGRA(list[5]), StrToBGRA(list[6]),
          dmDrawWithTransparency);
    end;

    'rectangleantialias':
    begin
      Result := ParamCheck(passed, 8);
      if Result then
        bitmap.RectangleAntialias(StrToFloat(list[1]), StrToFloat(list[2]),
          StrToFloat(list[3]), StrToFloat(list[4]), StrToBGRA(list[5]),
          StrToFloat(list[6]), StrToBGRA(list[7]));
    end;

    '//':
    begin
      //,comment
    end;

    '{':
    begin
      {,comment}
    end;

    else
    begin
      Result := False;
    end;
  end;

  {$ifdef debug}
  if not Result then
    writeln('>> ERROR');
  for i := 0 to list.Count - 1 do
    writeln(' ' + list[i]);
  writeln('--------------------');
  {$endif}
end;

function ScriptCommandList(commandlist: TStrings; bitmap: TBGRABitmap): boolean;
var
  i: integer;
begin
  {$ifdef debug}
  writeln('----Script  List----');
  writeln(' Executing ' + IntToStr(commandlist.Count) + ' lines...');
  writeln('--------------------');
  {$endif}

  Result := True;
  for i := 0 to commandlist.Count - 1 do
    if commandlist[i] <> '' then
      ScriptCommand(commandlist[i], bitmap);

  {$ifdef debug}
  writeln('----Script  List----');
  writeln(' END');
  writeln('--------------------');
  {$endif}
end;

end.

