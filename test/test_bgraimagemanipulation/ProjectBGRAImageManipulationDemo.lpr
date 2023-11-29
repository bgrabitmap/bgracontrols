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
  Application.Scaled:=True;
  Application.Title:='';
  Application.Initialize;
  Application.CreateForm(TFormBGRAImageManipulationDemo, FormBGRAImageManipulationDemo);
  Application.Run;
end.

