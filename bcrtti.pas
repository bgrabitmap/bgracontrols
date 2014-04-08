{ Useful tools for RTTI. Functions are used expecialy for save/load styles.
  Styles has construction similar to INI files:

  [Header]
  Author=Krzysztof Dibowski
  Description=My test style
  ControlClass=TBCButton

  [Properties]
  State.Border.Width=2
  .....

  But instead of IniFiles unit, we have own functions for read and write styles.

  ------------------------------------------------------------------------------
  Copyright (C) 2012 Krzysztof Dibowski dibowski at interia.pl

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit BCRTTI;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type
  PBCStyleHeader = ^TBCStyleHeader;
  TBCStyleHeader = record
    Author: String;
    ControlClass: String;
    Description: String;
  end;

// Function return data of specified section (header, properties, etc).
// This is smart function, because it doesn't read whole file but read file
// line by line and return only needed section. So it should fastest for reading
// header info instead of TIniFile object which read, parse and index all file.
function GetSectionData(const AFileName, ASectionName: String): TStrings;
// Methods which read header from list or file and parse it into pascal record
procedure GetStyleHeader(const AFileName: String; AOutHeader: PBCStyleHeader);
// Function check if specified name is on ignored list
function IsPropIgnored(const AName: String): Boolean;
// Method load style saved by SaveStyle method
procedure LoadStyle(AControl: TObject; const AFileName: String; ALogs: TStrings = nil);
// Method save all (which are not on ignored list or readonly) public propertys to
// the output string list. This method have support for property
// tree (Propert1.Subpropert1.Color = 543467). Values are represented as "human readable"
// (e.g. Align = alClient). Header info is save too.
procedure SaveStyle(AControl: TObject; const AAuthor, ADescription: String;
  ATargetList: TStrings);

implementation

uses typinfo, variants, sysutils, strutils;

const
  tIGNORED_PROPS: array[0..5] of string =
    ('name','caption','left','top','height','width');
  sSECTION_HEADER_NAME = 'HEADER';
  sSECTION_PROP_NAME   = 'PROPERTIES';
  sSECTION_HEADER = '['+sSECTION_HEADER_NAME+']';
  sSECTION_PROP   = '['+sSECTION_PROP_NAME+']';

function IsPropIgnored(const AName: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(tIGNORED_PROPS) to High(tIGNORED_PROPS) do
    if SameText(tIGNORED_PROPS[i],Trim(AName)) then
      Exit(True);
end;

procedure LoadStyle(AControl: TObject; const AFileName: String;
  ALogs: TStrings = nil);
var
  i, iDot: Integer;
  sPath, sVal: String;
  obj: TObject;
  sl: TStrings;
const
  sLOG_NO_PROP   = 'Can not find property "%s"';
  sLOG_SET_ERR   = 'Can not set value "%s" to property "%s"';
  sLOG_READ_ONLY = 'Property "%s" is read-only';

  procedure _AddLog(const AText: String);
  begin
    if ALogs<>nil then
      ALogs.Add(AText);
  end;

  function _ValidateProp(AObj: TObject; const APropName: String): Boolean;
  begin
    Result := True;
    // If can't find property
    if not IsPublishedProp(AObj,APropName) then
    begin
      _AddLog(Format(sLOG_NO_PROP,[APropName]));
      Exit(False);
    end;
    // If read-only property
    if (GetPropInfo(AObj,APropName)^.SetProc=nil) then
    begin
      _AddLog(Format(sLOG_READ_ONLY,[APropName]));
      Exit(False);
    end;
  end;

begin
  if not FileExists(AFileName) then
    Exit;

  if ALogs<>nil then
    ALogs.Clear;

  sl := GetSectionData(AFileName, sSECTION_PROP_NAME);
  try
    for i:=0 to Pred(sl.Count) do
    begin
      // Full path with hierarchy tree
      sPath := Trim(sl.Names[i]);
      // "Human readable" value
      sVal := Trim(sl.ValueFromIndex[i]);
      iDot := Pos('.', sPath);
      // If simple property then write it value
      if iDot=0 then
      begin
        if not _ValidateProp(AControl,sPath) then
          Continue;
        // Writting property value
        try
          SetPropValue(AControl,sPath,sVal)
        except
          _AddLog(Format(sLOG_SET_ERR,[sVal, sPath]));
        end
      end
      else
      begin
        //... else we must go down in hierarchy tree to the last
        // object and then write value to property
        obj := AControl;
        while iDot>0 do
        begin
          if not _ValidateProp(obj,Copy(sPath,1,iDot-1)) then
          begin
            obj := nil;
            Break;
          end;

          obj := GetObjectProp(obj,Copy(sPath,1,iDot-1));

          Delete(sPath,1,iDot);
          iDot := Pos('.', sPath);
        end;

        // If no dots, then this word is property name
        if (obj<>nil) and (sPath<>'') and _ValidateProp(obj,sPath) then
        begin
          try
            SetPropValue(obj,sPath,sVal)
          except
            _AddLog(Format(sLOG_SET_ERR,[sVal, sPath]));
          end
        end;
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure SaveStyle(AControl: TObject; const AAuthor, ADescription: String;
  ATargetList: TStrings);

  procedure _SaveProp(AObj: TObject; APath: String = '');
  var
    iCount, i: Integer;
    lst: TPropList;
    s: String;
  begin
    if AObj=nil then Exit;

    iCount := GetPropList(PTypeInfo(AObj.ClassInfo), tkProperties, @lst);
    for i := 0 to Pred(iCount) do
      { Notice:
        - IsPublishedProp return true for ALL public properties, not only
          for properties in Published section. For saving styles, we don't need
          all public properties, but only published (visible in object inspector).
          I don't know if this is a bug, I leave it. Maybe it will start
          working in future ;)
        - Second argument check if property should be ignored (but only from root tree),
          because we can't save basic properties of control like Name, Top, Left etc.
        - SetProc<>nil mean "not read only"
      }
      if IsPublishedProp(AObj,lst[i]^.Name) and
         ((AControl<>AObj) or (not IsPropIgnored(lst[i]^.Name))) and
         (lst[i]^.SetProc<>nil)
      then
      begin
        // Building property tree
        if APath=''
        then s := lst[i]^.Name
        else s := APath+'.'+lst[i]^.Name;

        // If property has subproperty, then we start recurrence to
        // build hierarchy tree.
        if (lst[i]^.PropType^.Kind = tkClass) then
          _SaveProp(GetObjectProp(AObj,lst[i]),s)
        else
        begin
          // We are in bottom node, so we can save final property with value
          s := s + ' = ' + String(GetPropValue(AObj,lst[i]^.Name,True));
          ATargetList.Add(s);
        end;
      end;
  end;
begin
  if ATargetList=nil then
    Exit;
  ATargetList.Clear;

  ATargetList.Add(sSECTION_HEADER);
  ATargetList.Add('Author='+AAuthor);
  ATargetList.Add('Description='+ADescription);
  ATargetList.Add('ControlClass='+AControl.ClassName);
  ATargetList.Add('');
  ATargetList.Add(sSECTION_PROP);
  _SaveProp(AControl);
end;

function GetSectionData(const AFileName, ASectionName: String): TStrings;
var
  f: TextFile;
  s: String;
  sl: TStringList;
  bReading: Boolean;
begin
  Result := TStringList.Create;
  Result.Clear;

  if (not FileExists(AFileName)) or (ASectionName='') then
    Exit;

  AssignFile(f,AFileName);
  try
    Reset(f);
    bReading := False;
    while not EOF(f) do
    begin
      ReadLn(f,s);
      s := Trim(s);
      if s='' then
        Continue;

      // If current line is section tag
      if s[1]='[' then
      begin
        // If we currently reading section then we read it all and we must
        // break because another section occur
        if bReading then
        begin
          bReading := False;
          Break;
        end
        else
        // Otherwise if this is section we are looking for, then set flag
        // to "start reading"
        if SameText(ASectionName,TrimSet(s,['[',']'])) then
          bReading := True;
      end else
      // Read section line
      if bReading then
        Result.Add(s);
    end;
  finally
    CloseFile(f);
  end;
end;

procedure GetStyleHeader(const AFileName: String; AOutHeader: PBCStyleHeader);
var sl: TStrings;
begin
  if (AOutHeader=nil) or (not FileExists(AFileName)) then
    Exit;

  sl := GetSectionData(AFileName,sSECTION_HEADER_NAME);
  try
    // Header info (with format Author=Foo) should be at the top of file
    with AOutHeader^ do
    begin
      Author       := sl.Values['Author'];
      Description  := sl.Values['Description'];
      ControlClass := sl.Values['ControlClass'];
    end;
  finally
    sl.Free;
  end;
end;

end.

