program test_dismissal;
{$mode objfpc}{$H+}
uses Interfaces, Forms, Controls, StdCtrls, ExtCtrls, Classes, SysUtils,
  LMessages, BCComboBox;
var f:TForm; combo:TBCComboBox; edit:TEdit; button:TButton; i:Integer; log:TStringList;
procedure Check(ok:Boolean; const msg:string);
begin if not ok then raise Exception.Create(msg); log.Add('PASS '+msg); end;
procedure OpenPanel;
begin
 Sleep(550);
 combo.Button.OnClick(combo.Button);
 Check(combo.ListBox.Parent.Visible,'panel opens');
 combo.ListBox.SetFocus;
 Check(combo.ListBox.Focused,'list owns focus');
end;
procedure RunCloseTimer;
var j:Integer;
begin
 for j:=0 to combo.ComponentCount-1 do
 if (combo.Components[j] is TTimer) and Assigned(TTimer(combo.Components[j]).OnTimer) then
 TTimer(combo.Components[j]).OnTimer(combo.Components[j]);
end;
begin
 log:=TStringList.Create;
 try
 Application.Initialize;
 f:=TForm.CreateNew(nil);
 try
 f.SetBounds(100,100,600,300);
 combo:=TBCComboBox.Create(f); combo.Parent:=f; combo.SetBounds(20,20,200,30);
 combo.DropDownOnSameForm:=True;
 for i:=1 to 12 do combo.Items.Add('Item '+IntToStr(i));
 edit:=TEdit.Create(f); edit.Parent:=f; edit.SetBounds(250,20,200,30);
 button:=TButton.Create(f); button.Parent:=f; button.SetBounds(250,70,200,30);
 f.Show; Application.ProcessMessages;
 OpenPanel;
 // A late exit notification must not dismiss a list that regained focus.
 combo.ListBox.OnExit(combo.ListBox); RunCloseTimer;
 Check(combo.ListBox.Parent.Visible,'stale exit does not dismiss focused list');
 // A real focus transfer still dismisses the same-form dropdown.
 edit.SetFocus; combo.ListBox.OnExit(combo.ListBox); RunCloseTimer;
 Check(not combo.ListBox.Parent.Visible,'real focus loss dismisses panel');
 {$IFDEF LCLCocoa}
 OpenPanel;
 Application.NotifyUserInputHandler(combo.ListBox,LM_LBUTTONDOWN);
 Check(combo.ListBox.Parent.Visible,'inside-list mouse down stays open');
 Application.NotifyUserInputHandler(combo.Button,LM_LBUTTONDOWN);
 Check(combo.ListBox.Parent.Visible,'own button is left to normal toggle handling');
 Application.NotifyUserInputHandler(button,LM_LBUTTONDOWN);
 Check(not combo.ListBox.Parent.Visible,'outside button dismisses without focus transfer');
 OpenPanel;
 Application.NotifyUserInputHandler(f,LM_RBUTTONDOWN);
 Check(not combo.ListBox.Parent.Visible,'outside background dismisses panel');
 combo.Free;
 Application.NotifyUserInputHandler(button,LM_LBUTTONDOWN);
 log.Add('PASS handler unregistered on destruction');
 {$ENDIF}
 finally f.Free; end;
 log.SaveToFile('dismissal-result.txt');
 except on e:Exception do begin log.Add('FAIL '+e.Message); log.SaveToFile('dismissal-result.txt'); Halt(1); end; end;
 log.Free;
end.
