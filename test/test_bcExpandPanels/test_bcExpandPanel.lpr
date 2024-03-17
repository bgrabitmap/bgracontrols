program test_bcExpandPanel;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  test_bcExpandPanel_unit1,
  LResources { you can add units after this };

begin
  Application.Initialize;
  Application.CreateForm(TbcExpandPanelForm1, bcExpandPanelForm1);
  Application.Run;
end.
