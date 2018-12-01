{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit bgrapascalscriptcomponent;

interface

uses
  uPSI_BGRAPascalScript, LazarusPackageIntf;

implementation

{$IFDEF FPC}procedure Register;{$ENDIF}
begin
  RegisterUnit('uPSI_BGRAPascalScript', @uPSI_BGRAPascalScript.Register);
end;

initialization
  RegisterPackage('bgrapascalscriptcomponent', @Register);
end.
