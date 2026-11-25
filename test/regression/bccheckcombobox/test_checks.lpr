program test_checks;
{$mode objfpc}{$H+}
uses Interfaces, Forms, Controls, Graphics, ExtCtrls, StdCtrls, Classes, SysUtils,
  CheckLst, LCLIntf, LCLType, BCCheckComboBox;
type
  TCompatCombo = class(TBCCheckComboBox)
    procedure BuildPopup;
    procedure ReleasePopup;
    procedure TrackPointer(const P: TPoint);
    function CheckPixels(AChecked: Boolean; ABackground: TColor): Integer;
  end;
  TChecklistAccess = class(TCheckListBox)
    procedure SendSpace;
    procedure SendKey(K: Word);
  end;
  TTestForm = class(TForm)
  public
    Timer: TTimer; Changes: Integer;
    constructor Create(AOwner: TComponent); override;
    procedure RunTests(Sender: TObject);
    procedure Changed(Sender: TObject);
  end;
var MainForm: TTestForm; Log: TStringList; Checks, Failures: Integer;
procedure Check(OK: Boolean; const Msg: string);
begin
  Inc(Checks);
  if OK then Log.Add('PASS '+Msg)
  else begin Inc(Failures); Log.Add('FAIL '+Msg); end;
end;
procedure TCompatCombo.BuildPopup;
begin CreateForm; end;
procedure TCompatCombo.ReleasePopup;
begin FreeForm; end;
procedure TCompatCombo.TrackPointer(const P: TPoint);
begin TrackListPointer(P); end;
function TCompatCombo.CheckPixels(AChecked: Boolean; ABackground: TColor): Integer;
var B: TBitmap; L: TCheckListBox; X,Y: Integer; Pixel, Wanted: TColor;
begin
  B := TBitmap.Create; L := ListBox;
  try
    B.SetSize(220,32); Checked[0] := AChecked;
    L.Canvas.Handle := B.Canvas.Handle;
    try L.OnDrawItem(L,0,Rect(0,0,220,32),[]);
    finally L.Canvas.Handle := 0; end;
    Wanted := ColorToRGB(ABackground); Result := 0;
    for Y := 0 to 31 do for X := 0 to 30 do
    begin
      Pixel := ColorToRGB(B.Canvas.Pixels[X,Y]);
      if (Abs((Pixel and 255)-(Wanted and 255))>40)
        or (Abs(((Pixel shr 8) and 255)-((Wanted shr 8) and 255))>40)
        or (Abs(((Pixel shr 16) and 255)-((Wanted shr 16) and 255))>40) then Inc(Result);
    end;
  finally B.Free; end;
end;
procedure TChecklistAccess.SendSpace;
var K: Word;
begin K := VK_SPACE; KeyDown(K,[]); end;
procedure TChecklistAccess.SendKey(K: Word);
begin KeyDown(K,[]); end;
constructor TTestForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  Caption := 'BCCheckComboBox checked-state regression'; SetBounds(100,100,440,180);
  Timer := TTimer.Create(Self); Timer.Interval := 300; Timer.OnTimer := @RunTests;
end;
procedure TTestForm.Changed(Sender: TObject);
begin Inc(Changes); end;
procedure TTestForm.RunTests(Sender: TObject);
var C, D: TCompatCombo; L: TCheckListBox; Phase, FrameIndex, Scheme, Hover, EmptyPixels, CheckedPixels: Integer;
  Marker: TObject; RowRect: TRect; Background: TColor;
  procedure Init;
  begin C.Clear; C.Items.Add('A'); C.Items.Add('B'); C.Checked[1] := True; end;
begin
  Timer.Enabled := False; SetForegroundWindow(Handle); Application.ProcessMessages;
  Marker := TObject.Create;
  try
    for Phase := 0 to 1 do
    begin
      Log.Add('PHASE '+IntToStr(Phase)+' (native handle forced: '+BoolToStr(Phase=1,True)+')');
      C := TCompatCombo.Create(Self); C.Parent := Self; C.SetBounds(10,10,200,30);
      D := TCompatCombo.Create(Self); D.Parent := Self; D.SetBounds(220,10,200,30);
      try
        C.OnChange := @Changed;
        if Phase = 1 then C.ListBox.HandleNeeded;
        Init; C.Items.Insert(0,'X');
        Check(not C.Checked[1],'insertion leaves A unchecked');
        Check(C.Checked[2],'insertion keeps B checked');
        C.Items.Delete(0);
        Check(C.Checked[1],'deleting X keeps B checked');
        C.Items.Delete(0);
        Check(C.Checked[0],'deleting A keeps remaining B checked');
        Init; C.Items.Clear; C.Items.Add('X'); C.Items.Add('Y');
        Check(not C.Checked[1],'Items.Clear removes old checks');
        C.Clear; Check(C.Items.Count=0,'combo Clear removes items');
        C.Items.AddObject('A',Marker); C.Items.Add('B'); C.Checked[1] := True;
        Check(C.Items.Objects[0]=Marker,'client Objects remain untouched');
        // Compile-time coverage of the public API used before 577239e.
        L := C.ListBox;
        Check(L.Checked[1],'legacy TCheckListBox exposes the same checks');
        L.Checked[0] := True; Check(C.Checked[0],'legacy writes reach Combo.Checked');
        C.Checked[1] := False; Check(not L.Checked[1],'Combo writes reach legacy API');
        Check((not C.Checked[-1]) and (not C.Checked[99]),'new convenience getter bounds');
        C.Checked[-1] := True; C.Checked[99] := True;
        Changes := 0; C.CheckAll;
        Check(C.Checked[0] and C.Checked[1],'CheckAll checks all items');
        C.UnCheckAll; Check(not C.Checked[0] and not C.Checked[1],'UnCheckAll clears all checks');
        Check(Changes=0,'programmatic setters do not simulate user changes');
        L.State[0] := cbGrayed; Check(C.Checked[0],'legacy grayed state stays checked');
        C.Checked[1] := True;
        D.Assign(C);
        Check(D.Items.Text=C.Items.Text,'Assign copies items');
        Check(D.Checked[0] and D.Checked[1],'Assign copies checked states');
        Check(D.ListBox.State[0]=cbGrayed,'Assign preserves legacy grayed state');
        C.Assign(C); Check(C.Checked[0] and C.Checked[1],'self Assign preserves checks');
        C.BuildPopup; C.ReleasePopup;
        Check((C.Items.Count=2) and (C.Items[0]='A') and (C.Items[1]='B')
          and (C.Items.Objects[0]=Marker),'popup release keeps item data');
        Check(C.Checked[0] and C.Checked[1],'popup release keeps checked states');
        C.BuildPopup;
        Check(C.Checked[0] and C.Checked[1],'popup recreation keeps checked states');
        L := C.ListBox;
        L.HandleNeeded; L.ItemIndex := 1;
        Changes := 0; TChecklistAccess(L).SendSpace;
        Check(not C.Checked[1],'Space toggles once');
        Check(Changes=1,'Space sends one OnChange');
        TChecklistAccess(L).SendSpace;
        Check(C.Checked[1],'second Space toggles back');
        Check(Changes=2,'second Space sends one more OnChange');
        L.ItemEnabled[1] := False;
        if not L.ItemEnabled[1] then
        begin
          TChecklistAccess(L).SendSpace;
          Check(C.Checked[1] and (Changes=2),'disabled items do not toggle');
        end
        else Log.Add('SKIP widgetset does not support per-item disabling');
        C.ReleasePopup;
        C.Button.OnClick(C.Button);
        Check(C.ListBox.Parent.Visible,'popup opens after cache recreation');
        // Drain native layout before freeing: Cocoa 3.8's global scroll sizer
        // keeps an unowned control pointer in its asynchronous queue.
        Application.ProcessMessages;
        C.Free; C := nil;
        Application.ProcessMessages;
        Check(True,'destruction with open popup completes');
      finally D.Free; C.Free; end;
    end;
    C := TCompatCombo.Create(Self); C.Parent := Self;
    try
      C.Items.Add('Check'); L := C.ListBox;
      C.Button.OnClick(C.Button);
      L.SetBounds(0,0,220,40); L.ItemHeight := 32; L.HandleNeeded;
      Application.ProcessMessages;
      RowRect := L.ItemRect(0);
      Changes := 0; C.OnChange := @Changed; C.Checked[0] := False;
      // Exercise the row fallback and the native-already-toggled path separately.
      L.OnMouseDown(L,mbLeft,[],180,(RowRect.Top+RowRect.Bottom) div 2);
      L.OnMouseUp(L,mbLeft,[],180,(RowRect.Top+RowRect.Bottom) div 2);
      Check(C.Checked[0] and (Changes=1),'row text click toggles once');
      L.OnMouseDown(L,mbLeft,[],180,(RowRect.Top+RowRect.Bottom) div 2);
      L.OnMouseUp(L,mbLeft,[],180,(RowRect.Top+RowRect.Bottom) div 2);
      Check(not C.Checked[0] and (Changes=2),'second row click toggles back');
      L.OnMouseDown(L,mbLeft,[],4,(RowRect.Top+RowRect.Bottom) div 2);
      L.Checked[0] := True; L.OnClickCheck(L);
      L.OnMouseUp(L,mbLeft,[],4,(RowRect.Top+RowRect.Bottom) div 2);
      Check(C.Checked[0] and (Changes=3),'native checkbox click is not toggled twice');
      L.OnMouseDown(L,mbRight,[],180,(RowRect.Top+RowRect.Bottom) div 2);
      L.OnMouseUp(L,mbRight,[],180,(RowRect.Top+RowRect.Bottom) div 2);
      Check(C.Checked[0] and (Changes=3),'right click does not toggle');
      L.OnMouseDown(L,mbLeft,[],180,(RowRect.Top+RowRect.Bottom) div 2);
      L.OnMouseUp(L,mbLeft,[],180,200);
      Check(C.Checked[0] and (Changes=3),'release outside the row does not toggle');
      for Scheme := 0 to 1 do
      begin
        if Scheme=0 then begin C.DropDownColor := clWhite; C.DropDownFontColor := clBlack; end
        else begin C.DropDownColor := RGBToColor(20,20,20); C.DropDownFontColor := clWhite; end;
        if Scheme=0 then C.DropDownHighlight := RGBToColor(0,120,215)
        else C.DropDownHighlight := RGBToColor(70,70,70);
        C.DropDownFontHighlight := clWhite;
        for Hover := 0 to 1 do
        begin
          if Hover=0 then
          begin L.ItemIndex := -1; Background := C.DropDownColor; end
          else
          begin
            C.TrackPointer(L.ClientToScreen(Point(-1,-1)));
            L.OnMouseMove(L,[],(RowRect.Left+RowRect.Right) div 2,
              (RowRect.Top+RowRect.Bottom) div 2);
            Background := C.DropDownHighlight;
          end;
          Check((C.HoverItem=0)=(Hover=1),'row hover state prepared');
          EmptyPixels := C.CheckPixels(False,Background);
          CheckedPixels := C.CheckPixels(True,Background);
          Log.Add(Format('Contrast %d/%d: empty=%d checked=%d',[Scheme,Hover,EmptyPixels,CheckedPixels]));
          Check(EmptyPixels>25,'checkbox outline contrasts with background');
          Check(CheckedPixels>EmptyPixels+10,'checked mark adds contrasting pixels');
        end;
      end;
      L.Items.Add('Second'); L.Items.Add('Third');
      L.SetBounds(0,0,220,120); Application.ProcessMessages;
      Changes := 0; C.UnCheckAll;
      TChecklistAccess(L).SendKey(VK_HOME);
      Check((L.ItemIndex=0) and (C.HoverItem=0),'Home highlights the active row');
      TChecklistAccess(L).SendKey(VK_DOWN);
      Check((L.ItemIndex=1) and (C.HoverItem=1),'Down moves active row and highlight');
      C.TrackPointer(Mouse.CursorPos); C.TrackPointer(Mouse.CursorPos);
      Check((L.ItemIndex=1) and (C.HoverItem=1),'stationary Cocoa timer does not override keyboard');
      TChecklistAccess(L).SendSpace;
      Check(C.Checked[1] and not C.Checked[0] and (Changes=1),'Space toggles keyboard-active row once');
      RowRect := L.ItemRect(2);
      C.TrackPointer(L.ClientToScreen(Point(-1,-1)));
      C.TrackPointer(L.ClientToScreen(Point(100,(RowRect.Top+RowRect.Bottom) div 2)));
      Check((L.ItemIndex=2) and (C.HoverItem=2),'real pointer movement takes priority');
      TChecklistAccess(L).SendSpace;
      Check(C.Checked[2] and (Changes=2),'Space toggles mouse-active row once');
      TChecklistAccess(L).SendKey(VK_UP);
      Check((L.ItemIndex=1) and (C.HoverItem=1),'keyboard takes priority back from mouse');
      C.TrackPointer(Mouse.CursorPos);
      Check(L.ItemIndex=1,'stationary pointer still respects keyboard priority');
      TChecklistAccess(L).SendSpace;
      Check(not C.Checked[1] and C.Checked[2] and (Changes=3),'Space follows the newly active row');
      L.OnMouseLeave(L);
      Check(C.HoverItem=L.ItemIndex,'leaving list keeps keyboard target highlighted');
      TChecklistAccess(L).SendKey(VK_END);
      Check((L.ItemIndex=2) and (C.HoverItem=2),'End moves active row and highlight');
      TChecklistAccess(L).SendKey(VK_NEXT);
      Check(L.ItemIndex=2,'PageDown clamps at last row');
      TChecklistAccess(L).SendKey(VK_PRIOR);
      Check((L.ItemIndex=0) and (C.HoverItem=0),'PageUp moves active row and highlight');
      Check(Changes=3,'navigation and hover do not send check changes');
      Application.ProcessMessages;
    finally C.Free; end;
  except on E: Exception do
  begin
    Inc(Failures); Log.Add('EXCEPTION '+E.Message);
    Log.Add(BackTraceStrFunc(ExceptAddr));
    for FrameIndex := 0 to ExceptFrameCount-1 do
      Log.Add(BackTraceStrFunc(ExceptFrames[FrameIndex]));
  end; end;
  Marker.Free;
  Log.Add(Format('Checks: %d; Failures: %d',[Checks,Failures]));
  Log.SaveToFile(ExtractFilePath(Application.ExeName)+'checks-result.txt');
  Application.Terminate;
end;
begin
  Log := TStringList.Create;
  Application.Initialize; Application.CreateForm(TTestForm,MainForm); Application.Run;
  Log.Free;
  if Failures<>0 then Halt(1);
end.
