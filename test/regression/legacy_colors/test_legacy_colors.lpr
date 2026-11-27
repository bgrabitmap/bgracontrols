program test_legacy_colors;

{$mode objfpc}{$H+}

uses
  {$ifdef UNIX}cthreads,{$endif}
  Interfaces, Forms, SysUtils, umain, LegacyColorChecks;

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  try
    Application.CreateForm(TForm1, Form1);
    if (ParamCount = 0) or (ParamStr(1) <> '--demo') then
    begin
      try
        RunLegacyColorChecks(Form1);
      finally
        Form1.Free;
      end;
    end
    else
      Application.Run;
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ', E.Message);
      Halt(1);
    end;
  end;
end.
