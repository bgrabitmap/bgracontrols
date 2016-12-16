{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit bgrapascalscriptcomponent;

interface

uses
  uPSI_BGRAPascalScript, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uPSI_BGRAPascalScript', @uPSI_BGRAPascalScript.Register);
end;

initialization
  RegisterPackage('bgrapascalscriptcomponent', @Register);
end.
