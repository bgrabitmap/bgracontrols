program test_highlight;
{$mode objfpc}{$H+}
uses Interfaces, Forms, Controls, StdCtrls, ExtCtrls, Classes, SysUtils,
  Graphics, LCLType, LCLIntf, BCComboBox;
type
 TTestForm = class(TForm)
 public
   Combo: TBCComboBox;
   Timer: TTimer;
   Seen: TOwnerDrawState;
   constructor Create(AOwner:TComponent); override;
   procedure Run(Sender:TObject);
   procedure Draw(Control:TWinControl; Index:Integer; R:TRect; State:TOwnerDrawState);
 end;
var Form:TTestForm; Results:TStringList;
procedure Check(OK:Boolean; const Msg:string);
begin
 if not OK then raise Exception.Create(Msg);
 Results.Add('PASS '+Msg);
end;
constructor TTestForm.Create(AOwner:TComponent);
var i:Integer;
begin
 inherited CreateNew(AOwner);
 SetBounds(100,100,600,300);
 Caption:='BCComboBox #290 - keyboard highlight';
 Combo:=TBCComboBox.Create(Self); Combo.Parent:=Self;
 Combo.SetBounds(20,20,220,30);
 for i:=0 to 19 do Combo.Items.Add('Item '+IntToStr(i));
 Combo.ItemIndex:=0;
 Combo.DropDownOnSameForm:=not FindCmdLineSwitch('separate-popup');
 Timer:=TTimer.Create(Self); Timer.Enabled:=False;
 Timer.Interval:=700; Timer.OnTimer:=@Run;
end;
procedure TTestForm.Draw(Control:TWinControl; Index:Integer; R:TRect; State:TOwnerDrawState);
begin Seen:=State; end;
procedure TTestForm.Run(Sender:TObject);
var Mode, Row:Integer; Key:Word; R:TRect; L:TListBox;
 procedure Paint(Index:Integer);
 begin
   L.OnDrawItem(L,Index,L.ItemRect(Index),[odSelected]);
 end;
begin
 Timer.Enabled:=False;
 SetForegroundWindow(Handle); Application.ProcessMessages;
 if FindCmdLineSwitch('manual') then Exit;
 try
 for Mode:=0 to 1 do begin
   Combo.DropDownOnSameForm:=Mode=0;
   Combo.ItemIndex:=0;
   Combo.Button.OnClick(Combo.Button); Application.ProcessMessages;
   L:=Combo.ListBox;
   Check(L.Parent.Visible,'dropdown visible, mode '+IntToStr(Mode));
   L.TopIndex:=0;
   Application.ProcessMessages;
   R:=L.ItemRect(0);
   L.OnMouseMove(L,[],R.Left+2,(R.Top+R.Bottom) div 2);
   Check(Combo.HoverItem=0,'mouse hovers first row');
   // Exercise keyboard routing and then the selection change normally made by
   // the native widgetset. This tests rendering independently of OS key delivery.
   Key:=VK_DOWN; L.OnKeyDown(L,Key,[]); L.ItemIndex:=2;
   Combo.OnDrawItem:=@Draw;
   Paint(2); Check(odSelected in Seen,'keyboard row highlighted in custom draw');
   Check(odChecked in Seen,'custom draw retains selected-value marker');
   Paint(0); Check(not (odSelected in Seen),'stale hover no longer highlighted');
   Check(not (odChecked in Seen),'unselected row has no selected-value marker');
   Combo.OnDrawItem:=nil;
   Paint(2); Check(L.Canvas.Brush.Color=Combo.DropDownHighlight,'default keyboard highlight color');
   Paint(0); Check(L.Canvas.Brush.Color=Combo.DropDownColor,'default stale-hover background');
   // Moving within the same formerly hovered row must restore mouse highlighting.
   L.OnMouseMove(L,[],R.Left+3,(R.Top+R.Bottom) div 2);
   Combo.OnDrawItem:=@Draw;
   Paint(0); Check(odSelected in Seen,'mouse takes over even within same row');
   Paint(2); Check(not (odSelected in Seen),'mouse takeover clears keyboard highlight');
   Check(odChecked in Seen,'mouse takeover preserves selected-value marker');
   Combo.OnDrawItem:=nil;
   Combo.Button.OnClick(Combo.Button); Application.ProcessMessages;
   // Allow the component cooldown without blocking Cocoa events.
   for Row:=1 to 60 do begin Application.ProcessMessages; Sleep(10); end;
 end;
 except on E:Exception do begin ExitCode:=1; Results.Add('FAIL '+E.Message); end; end;
 Results.SaveToFile(ExtractFilePath(Application.ExeName)+'highlight-result.txt');
 Application.Terminate;
end;
begin
 Results:=TStringList.Create;
 Application.Initialize;
 Application.CreateForm(TTestForm,Form);
 Form.Timer.Enabled:=True;
 Application.Run;
 Form.Free;
 Results.Free;
end.
