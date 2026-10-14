program ProbeDpiStartup;
{$mode objfpc}{$H+}
uses Interfaces, Forms, Controls, Graphics, SysUtils, Types,
  ExtCtrls, BCLabel, BCPanel, BCButton, BCTypes, BGRABitmap, BGRABitmapTypes;
type TMeasurement = record InkWidth, InkHeight, BorderWidth: Integer; end;
var F:TForm; L:TBCLabel; P:TBCPanel; B:TBCButton; Shot:TBGRABitmap;
  Base:TPoint; I, Pass, FontHeight, InitialWidth, InitialHeight:Integer;
  ML, MP, MB, BaseLabel, BasePanel: TMeasurement;
  InsetPanel: TBCPanel; Child: TPanel; EnableScaling: Boolean;
procedure Check(ACondition: Boolean; const AMessage: String);
begin if not ACondition then raise Exception.Create(AMessage); end;
function Measure(C:TControl; const Name:String): TMeasurement;
var O:TPoint; X,Y,TopInk,BottomInk,LeftInk,RightInk,RedWidth:Integer; Pix:TBGRAPixel;
begin
  O:=C.ClientToScreen(Point(0,0));
  LeftInk:=C.Width; RightInk:=-1; TopInk:=C.Height; BottomInk:=-1;
  for Y:=4 to C.Height-5 do for X:=4 to C.Width-5 do
  begin
    Pix:=Shot.GetPixel(O.X-Base.X+X,O.Y-Base.Y+Y);
    if (Pix.red<64) and (Pix.green<64) and (Pix.blue<64) then
    begin
      if X<LeftInk then LeftInk:=X; if X>RightInk then RightInk:=X;
      if Y<TopInk then TopInk:=Y; if Y>BottomInk then BottomInk:=Y;
    end;
  end;
  RedWidth:=0;
  for X:=0 to 12 do
  begin
    Pix:=Shot.GetPixel(O.X-Base.X+X,O.Y-Base.Y+C.Height div 2);
    if (Pix.green<80) and (((Pix.red>180) and (Pix.blue<80)) or
      ((Pix.blue>180) and (Pix.red<80))) then Inc(RedWidth);
  end;
  Result.InkWidth:=RightInk-LeftInk+1; Result.InkHeight:=BottomInk-TopInk+1; Result.BorderWidth:=RedWidth;
  WriteLn(Name,' ink=',RightInk-LeftInk+1,'x',BottomInk-TopInk+1,' red-border=',RedWidth);
end;
begin
  try
  RequireDerivedFormResource:=False; Application.Initialize;
  WriteLn('Screen.PixelsPerInch=',Screen.PixelsPerInch);
  FontHeight:=StrToIntDef(ParamStr(1),16);
  F:=TForm.Create(nil); F.DesignTimePPI:=96; F.SetBounds(20,20,700,500); F.Color:=clWhite;
  L:=TBCLabel.Create(F); L.Parent:=F; L.AutoSize:=False; L.Caption:='MMMM';
  L.SetBounds(20,20,240,90); L.FontEx.Name:='DejaVu Sans'; L.FontEx.Style:=[]; L.FontEx.Height:=FontHeight; L.FontEx.Color:=clBlack;
  L.Border.Style:=bboSolid; L.Border.ColorOpacity:=255; L.Border.Width:=2; L.Border.Color:=clRed; L.Background.Color:=clWhite; L.Background.Style:=bbsColor;
  P:=TBCPanel.Create(F); P.Parent:=F; P.Caption:='MMMM'; P.SetBounds(20,130,240,90);
  P.FontEx.Name:='DejaVu Sans'; P.FontEx.Style:=[]; P.FontEx.Height:=FontHeight; P.FontEx.Color:=clBlack; P.BorderBCStyle:=bpsBorder;
  P.Border.Style:=bboSolid; P.Border.ColorOpacity:=255; P.Border.Width:=2; P.Border.Color:=clRed; P.Background.Color:=clWhite; P.Background.Style:=bbsColor;
  B:=TBCButton.Create(F); B.Parent:=F; B.Caption:='MMMM'; B.SetBounds(20,240,240,90); B.Scaled:=True;
  B.StateNormal.FontEx.Name:='DejaVu Sans'; B.StateNormal.FontEx.Style:=[]; B.StateNormal.FontEx.Height:=FontHeight; B.StateNormal.FontEx.Color:=clBlack;
  B.StateNormal.Border.Style:=bboSolid; B.StateNormal.Border.ColorOpacity:=255; B.StateNormal.Border.Width:=2; B.StateNormal.Border.Color:=clRed;
  B.StateNormal.Background.Color:=clWhite; B.StateNormal.Background.Style:=bbsColor;
  B.Rounding.RoundX:=0; B.Rounding.RoundY:=0;
  B.StateHover.Assign(B.StateNormal); B.StateClicked.Assign(B.StateNormal);
  InsetPanel:=TBCPanel.Create(F); InsetPanel.Parent:=F;
  InsetPanel.SetBounds(320,20,240,120); InsetPanel.Caption:='';
  InsetPanel.BevelInner:=bvLowered; InsetPanel.BevelOuter:=bvRaised; InsetPanel.BevelWidth:=2;
  Child:=TPanel.Create(F); Child.Parent:=InsetPanel; Child.Align:=alClient;
  Check(not L.Scaled and not P.Scaled,'Scaled defaults to false');
  F.Show;
  Shot:=TBGRABitmap.Create;
  try
    for Pass:=0 to 3 do
    begin
      EnableScaling:=Odd(Pass);
      L.Scaled:=EnableScaling; P.Scaled:=EnableScaling;
      B.Scaled:=EnableScaling; B.UpdateControl; InsetPanel.Scaled:=EnableScaling;
      for I:=1 to 20 do begin Application.ProcessMessages; Sleep(20); end;
      Base:=F.ClientToScreen(Point(0,0));
      Shot.TakeScreenshot(Rect(Base.X,Base.Y,Base.X+F.ClientWidth,Base.Y+F.ClientHeight));
      ML:=Measure(L,'label'); MP:=Measure(P,'panel'); MB:=Measure(B,'button');
      Check(ML.InkHeight>0,'label text visible'); Check(MP.InkHeight>0,'panel text visible');
      if FontHeight<>0 then
      begin
        Check(Abs(ML.InkHeight-MB.InkHeight)<=1,'label text matches button scaling');
        Check(Abs(MP.InkHeight-MB.InkHeight)<=1,'panel text matches button scaling');
      end
      else if EnableScaling then
      begin
        Check(ML.InkHeight=BaseLabel.InkHeight,'default label font is not scaled twice');
        Check(MP.InkHeight=BasePanel.InkHeight,'default panel font is not scaled twice');
      end;
      if MB.BorderWidth=0 then WriteLn('SKIP: colored border sampling unavailable on this widgetset');
      {$IFDEF LCLgtk2}Check(MB.BorderWidth>0,'border pixels visible');{$ENDIF}
      Check(ML.BorderWidth=MB.BorderWidth,'label border matches button');
      Check(MP.BorderWidth=MB.BorderWidth,'panel border matches button');
      Check((L.FontEx.Height=FontHeight) and (P.FontEx.Height=FontHeight),'stored font unchanged');
      Check((L.Border.Width=2) and (P.Border.Width=2),'stored borders unchanged');
      Check(InsetPanel.BevelWidth=2,'stored bevel unchanged');
      if EnableScaling then
        Check(Child.Left=2*Round(2*Screen.PixelsPerInch/96),'scaled panel client inset')
      else Check(Child.Left=4,'unscaled panel client inset');
      if Pass=0 then begin BaseLabel:=ML; BasePanel:=MP; end;
      if Pass=2 then
      begin
        Check((ML.InkWidth=BaseLabel.InkWidth) and (ML.InkHeight=BaseLabel.InkHeight),'label restores original rendering');
        Check((MP.InkWidth=BasePanel.InkWidth) and (MP.InkHeight=BasePanel.InkHeight),'panel restores original rendering');
      end;
    end;
    L.Scaled:=False; L.AutoSize:=True;
    for I:=1 to 10 do begin Application.ProcessMessages; Sleep(20); end;
    InitialWidth:=L.Width; InitialHeight:=L.Height;
    L.Scaled:=True;
    for I:=1 to 10 do begin Application.ProcessMessages; Sleep(20); end;
    if Screen.PixelsPerInch>96 then
      Check((L.Width>InitialWidth) and (L.Height>InitialHeight),'scaled label autosize grows');
    L.Scaled:=False;
    for I:=1 to 10 do begin Application.ProcessMessages; Sleep(20); end;
    Check((L.Width=InitialWidth) and (L.Height=InitialHeight),'label autosize restores original size');
    WriteLn('PASS: font height=',FontHeight,' DPI=',Screen.PixelsPerInch);
  finally Shot.Free; F.Free; end;
  except on E:Exception do begin WriteLn('FAIL: ',E.Message); Flush(Output); Halt(1); end; end;
end.