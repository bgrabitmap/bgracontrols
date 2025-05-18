//https://castle-engine.io/modern_pascal

program Make;
{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  StrUtils,
  FileUtil,
  Zipper,
  fphttpclient,
  RegExpr,
  openssl,
  LazUTF8,
  opensslsockets,
  eventlog,
  Process;

  function OutLog(const Knd: TEventType; const Msg: string): string;
  begin
    case Knd of
      etError: Result := #27'[91m%s'#27'[0m';
      etInfo:  Result := #27'[32m%s'#27'[0m';
      etDebug: Result := #27'[33m%s'#27'[0m';
    end;
    Writeln(stderr, UTF8ToConsole(Result.Format([Msg])));
  end;

  function AddPackage(const Path: string): string;
  begin
    if RunCommand('lazbuild', ['--add-package-link', Path], Result, [poStderrToOutPut]) then
      OutLog(etDebug, 'Add package:'#9 + Path)
    else
    begin
      ExitCode += 1;
      OutLog(etError, Result);
    end;
  end;

  function SelectString(const Input, Reg: string): string;
  var
    Line: string;
  begin
    Result := EmptyStr;
    with TRegExpr.Create do
    begin
      Expression := Reg;
      for Line in Input.Split(LineEnding) do
        if Exec(Line) then
		begin
		  if Result <> EmptyStr then
		    Result += LineEnding;
          Result += Line;
		end;
      Free;
    end;
  end;

  function RunTest(const Path: String): string;
  begin
    OutLog(etDebug, #9'run:'#9 + Path);
    if RunCommand(Path, ['--all', '--format=plain'], Result, [poStderrToOutPut]) then
      OutLog(etInfo, #9'success!')
    else
    begin
      ExitCode += 1;
      OutLog(etError, Result);
    end;
  end;

  function AddDDL(const Path: String): string;
  const
    LibPath: string = '/usr/lib/';
  var
    List: array of string;
    Last: integer;
  begin
    OutLog(etDebug, #9'add:'#9 + Path);
    List := Path.Split(DirectorySeparator);
    Last := High(List);
    if not FileExists(LibPath + List[Last]) then
      if RunCommand('sudo', ['bash', '-c', 'cp %s %s; ldconfig --verbose'.Format([Path, LibPath])], Result, [poStderrToOutPut]) then
        OutLog(etInfo, #9'success!')
      else
      begin
        ExitCode += 1;
        OutLog(etError, Result);
      end;
  end;

  function BuildProject(const Path: string): string;
  var
    Text: string;
  begin
    OutLog(etDebug, 'Build from:'#9 + Path);
    if RunCommand('lazbuild',
      ['--build-all', '--recursive', '--no-write-project', Path], Result, [poStderrToOutPut]) then
    begin
      Result := SelectString(Result, 'Linking').Split(' ')[2].Replace(LineEnding, EmptyStr);
      OutLog(etInfo, #9'to:'#9 + Result);
      Text := ReadFileToString(Path.Replace('.lpi', '.lpr'));
      if Text.Contains('program') and Text.Contains('consoletestrunner') then
        RunTest(Result)
      else if Text.Contains('library') and Text.Contains('exports') then
        AddDDL(Result)
    end
    else
    begin
      ExitCode += 1;
      OutLog(etError, SelectString(Result, '(Fatal|Error|/ld(\.[a-z]+)?):'));
    end;
  end;

  function DownloadFile(const Uri: string): string;
  var
    OutFile: TStream;
  begin
    InitSSLInterface;
    Result := GetTempFileName;
    OutFile := TFileStream.Create(Result, fmCreate or fmOpenWrite);
    with TFPHttpClient.Create(nil) do
    begin
      try
        AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
        AllowRedirect := True;
        Get(Uri, OutFile);
        OutLog(etDebug, 'Download from %s to %s'.Format([Uri, Result]));
      finally
        Free;
        OutFile.Free;
      end;
    end;
  end;

  procedure UnZip(const ZipFile, ZipPath: string);
  begin
    with TUnZipper.Create do
    begin
      try
        FileName := ZipFile;
        OutputPath := ZipPath;
        Examine;
        UnZipAllFiles;
        OutLog(etDebug, 'Unzip from'#9 + ZipFile + #9'to'#9 + ZipPath);
        DeleteFile(ZipFile);
      finally
        Free;
      end;
    end;
  end;

  function InstallOPM(const Path: string): string;
  begin
    Result :=
      {$IFDEF MSWINDOWS}
      GetEnvironmentVariable('APPDATA') + '\.lazarus\onlinepackagemanager\packages\'
      {$ELSE}
      GetEnvironmentVariable('HOME') + '/.lazarus/onlinepackagemanager/packages/'
      {$ENDIF}
      + Path;
    if not DirectoryExists(Result) then
    begin
      if ForceDirectories(Result) then
        UnZip(DownloadFile('https://packages.lazarus-ide.org/%s.zip'.Format([Path])), Result);
    end;
  end;
  
  procedure RetrieveSubmodules;
  var CommandOutput: string;
  begin
    if FileExists('.gitmodules') then
    if RunCommand('git', ['submodule', 'update', '--init',
      '--force', '--remote'], CommandOutput, [poStderrToOutPut]) then
      OutLog(etInfo, CommandOutput)
    else
    begin
      ExitCode += 1;
      OutLog(etError, CommandOutput);
    end;
  end;

  function BuildAll(const Target: string; const Dependencies: array of string): string;
  var
    List: TStringList;
    DT: TDateTime;
  begin
    DT := Time;
	// GitHub already retrieves submodules
    List := FindAllFiles(GetCurrentDir, '*.lpk');
    try
      for Result in Dependencies do
        List.AddStrings(FindAllFiles(InstallOPM(Result), '*.lpk'));
      for Result in List do
        AddPackage(Result);
      List := FindAllFiles(Target, '*.lpi');
      List.Sort;
      for Result in List do
        if not Result.Contains('backup') and not Result.Contains('/use/') then
          BuildProject(Result);
    finally
      List.Free;
    end;
    if not RunCommand('delp', ['-r', GetCurrentDir], Result, [poStderrToOutPut]) then
      OutLog(etError, Result);
    OutLog(etDebug, 'Duration:'#9 + FormatDateTime('hh:nn:ss', Time - DT));
  end;

begin
  try
    BuildAll('.', ['UEControls']);
	OutLog(etDebug,     '------------');
    case ExitCode of
      0: OutLog(etInfo, 'No Errors 😊');
      else
        OutLog(etError, 'Errors:'#9 + ExitCode.ToString);
    end;
	OutLog(etDebug,     '------------');
  except
    on E: Exception do
      Writeln(E.ClassName, #9, E.Message);
  end;
end.
