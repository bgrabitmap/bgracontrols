// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit bgracontrolsinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  BGRAControlsVersion = 9000107;

  function BGRAControlsVersionStr: string;

implementation

function BGRAControlsVersionStr: string;
var numbers: TStringList;
  i,remaining: cardinal;
begin
  numbers := TStringList.Create;
  remaining := BGRAControlsVersion;
  for i := 1 to 4 do
  begin
    numbers.Insert(0, IntToStr(remaining mod 100));
    remaining := remaining div 100;
  end;
  while (numbers.Count > 1) and (numbers[numbers.Count-1]='0') do
    numbers.Delete(numbers.Count-1);
  numbers.Delimiter:= '.';
  result := numbers.DelimitedText;
  numbers.Free;
end;

end.

