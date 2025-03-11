program test_bcnumerickeyboard;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umain, BCNumericKeyboard
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='BC Numeric Keyboard';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

