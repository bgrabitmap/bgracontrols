program ProjectBGRAImageManipulationDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UnitBGRAImageManipulationDemo, bgracontrols;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormBGRAImageManipulationDemo, FormBGRAImageManipulationDemo);
  Application.Run;
end.

