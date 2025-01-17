#!/usr/bin/env instantfpc

program Make;
{$mode objfpc}{$H+}
{$unitpath /usr/lib64/lazarus/components/lazutils}
uses
  Classes,
  SysUtils,
  StrUtils,
  FileUtil,
  Zipper,
  fphttpclient,
  RegExpr,
  openssl,
  opensslsockets,
  Process;

const
  Target: string = 'test';
  Dependencies: array of string = ();

type
  TLog = (audit, info, error);
  Output = record
    Success: boolean;
    Output: string;
  end;

  procedure OutLog(Knd: TLog; Msg: string);
  begin
    case Knd of
        error: Writeln(stderr, #27'[31m', Msg, #27'[0m');
        info:  Writeln(stderr, #27'[32m', Msg, #27'[0m');
        audit: Writeln(stderr, #27'[33m', Msg, #27'[0m');
    end;
  end;

  function CheckModules: Output;
  begin
    if FileExists('.gitmodules') then
      if RunCommand('git', ['submodule', 'update', '--init', '--recursive',
        '--force', '--remote'], Result.Output) then
        OutLog(info, Result.Output);
  end;

  function AddPackage(Path: string): Output;
  begin
    with TRegExpr.Create do
    begin
      Expression :=
        {$IFDEF MSWINDOWS}
          '(cocoa|x11|_template)'
        {$ELSE}
          '(cocoa|gdi|_template)'
        {$ENDIF}
      ;
      if not Exec(Path) and RunCommand('lazbuild', ['--add-package-link', Path],
        Result.Output) then
        OutLog(audit, 'added ' + Path);
      Free;
    end;
  end;

  function BuildProject(Path: string): Output;
  var
    Line: string;
  begin
    OutLog(audit, 'build from ' + Path);
    try
      Result.Success := RunCommand('lazbuild', ['--build-all', '--recursive',
        '--no-write-project', Path], Result.Output);
      if Result.Success then
        for Line in SplitString(Result.Output, LineEnding) do
        begin
          if ContainsStr(Line, 'Linking') then
          begin
            Result.Output := SplitString(Line, ' ')[2];
            OutLog(info, ' to ' + Result.Output);
            break;
          end;
        end
      else
      begin
        ExitCode += 1;
        for Line in SplitString(Result.Output, LineEnding) do
          with TRegExpr.Create do
          begin
            Expression := '(Fatal|Error):';
            if Exec(Line) then
              OutLog(error, #10 + Line);
            Free;
          end;
      end;
    except
      on E: Exception do
        OutLog(error, E.ClassName + #13#10 + E.Message);
    end;
  end;

  function RunTest(Path: string): Output;
  var
    Temp: string;
  begin
    Result := BuildProject(Path);
    Temp:= Result.Output;
    if Result.Success then
        try
          if not RunCommand(Temp, ['--all', '--format=plain', '--progress'], Result.Output) then
          begin
            ExitCode += 1;
            OutLog(error, Result.Output);
          end;
        except
          on E: Exception do
            OutLog(error, E.ClassName + #13#10 + E.Message);
        end;
  end;

  function InstallOPM(Path: string): string;
  var
    OutFile, Uri: string;
    Zip: TStream;
  begin
    Result :=
      {$IFDEF MSWINDOWS}
      GetEnvironmentVariable('APPDATA') + '\.lazarus\onlinepackagemanager\packages\'
      {$ELSE}
      GetEnvironmentVariable('HOME') + '/.lazarus/onlinepackagemanager/packages/'
      {$ENDIF}
      + Path;
    OutFile := GetTempFileName;
    Uri := 'https://packages.lazarus-ide.org/' + Path + '.zip';
    if not DirectoryExists(Result) then
    begin
      Zip := TFileStream.Create(OutFile, fmCreate or fmOpenWrite);
      with TFPHttpClient.Create(nil) do
      begin
        try
          AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
          AllowRedirect := True;
          Get(Uri, Zip);
          OutLog(audit, 'Download from ' + Uri + ' to ' + OutFile);
        finally
          Free;
          Zip.Free;
        end;
      end;
      CreateDir(Result);
      with TUnZipper.Create do
      begin
        try
          FileName := OutFile;
          OutputPath := Result;
          Examine;
          UnZipAllFiles;
          OutLog(audit, 'Unzip from ' + OutFile + ' to ' + Result);
        finally
          Free;
        end;
      end;
      DeleteFile(OutFile);
    end;
  end;

  procedure BuildAll;
  var
    Each: string;
    List: TStringList;
  begin
    CheckModules;
    InitSSLInterface;
    List := FindAllFiles(GetCurrentDir, '*.lpk', True);
    try
      for Each in Dependencies do
        List.AddStrings(FindAllFiles(InstallOPM(Each), '*.lpk', True));
      for Each in List do
        AddPackage(Each);
      List := FindAllFiles(Target, '*.lpi', True);
      for Each in List do
        if not ContainsStr(Each, 'zengl') then
          if ContainsStr(ReadFileToString(ReplaceStr(Each, '.lpi', '.lpr')),
            'consoletestrunner') then
            RunTest(Each)
          else
            BuildProject(Each);
    finally
      List.Free;
    end;
  end;

begin
  if ParamCount <> 0 then
    case ParamStr(1) of
      'build': BuildAll;
      else OutLog(audit, 'Nothing!');
    end;
  if ExitCode <> 0 then
    OutLog(error, #10 + 'Errors: ' + IntToStr(ExitCode))
  else
    OutLog(info, #10 + 'Errors: ' + IntToStr(ExitCode));
end.
