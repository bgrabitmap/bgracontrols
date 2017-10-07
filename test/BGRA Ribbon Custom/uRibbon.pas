unit uRibbon;

{$mode objfpc}{$H+}

interface

uses
  Controls, Graphics, BCButton, BCTypes, BGRABitmap, BGRABitmapTypes, LResources;

type
  TBCRibbonStyle = (rsTab, rsButton, rsSeparator, rsMenu);


var
  MainColor,TabFntColor,BtnFntColor:TColor;
  BtnNormalColor, BtnHoverColor, BtnClickedColor:TColor;
  TabNormalColor, TabHoverColor, TabClickedColor:TColor;
  ToolNormalColor,ToolHoverColor,ToolClickedColor:TColor;

{ StyleButtons }

procedure StyleRibbonButtonsSample(AControl: TControl; AStyle: TBCRibbonStyle);
procedure StyleRibbonBody(AControl: TControl);

{ Drawings }

procedure DrawTabGradient(ABitmap: TBGRABitmap);
procedure DrawTitleGradient(ABitmap: TBGRABitmap);
procedure DrawBodyGradient(ABitmap: TBGRABitmap);
procedure DrawFormGradient(ABitmap: TBGRABitmap);

{ Buttons }

procedure RibbonTab(AButton: TBCButton);
procedure RibbonTool(AButton: TBCButton);
procedure RibbonButton(AButton: TBCButton);
procedure RibbonSeparator(AButton: TBCButton);
procedure RibbonMenu(AButton: TBCButton);

implementation

  { StyleButtons }

procedure StyleButtons(AControl: TControl; AButton: TBCButton);
var
  i: integer;
  WinControl: TWinControl;
begin
  if AControl is TBCButton then
    AControl.Assign(AButton);
  if AControl is TWinControl then
  begin
    WinControl := TWinControl(AControl);
    if WinControl.ControlCount = 0 then
      exit;
    for i := 0 to WinControl.ControlCount - 1 do
      StyleButtons(WinControl.Controls[i], AButton);
  end;
end;

procedure StyleRibbonButtonsSample(AControl: TControl; AStyle: TBCRibbonStyle);
var
  tempBCButton: TBCButton;
begin
  tempBCButton := TBCButton.Create(nil);
  case AStyle of
    rsTab: RibbonTab(tempBCButton);
    rsButton: RibbonButton(tempBCButton);
    rsSeparator: RibbonSeparator(tempBCButton);
    rsMenu: RibbonMenu(tempBCButton);
  end;
  StyleButtons(AControl, tempBCButton);
  tempBCButton.Free;
end;

procedure StyleRibbonBody(AControl: TControl);
var
  i: integer;
  WinControl: TWinControl;
begin
  if AControl is TWinControl then
  begin
    WinControl := TWinControl(AControl);
    if WinControl.ControlCount = 0 then
      exit;
    for i := 0 to WinControl.ControlCount - 1 do
    begin
      if WinControl.Controls[i] is TBCButton then
      begin
        if WinControl.Controls[i].Caption = '-' then
        begin
          StyleRibbonButtonsSample(WinControl.Controls[i], rsSeparator);
          WinControl.Controls[i].Caption := '';
        end
        else
          StyleRibbonButtonsSample(WinControl.Controls[i], rsButton);
      end;
    end;
  end;
end;

{ Drawings }

procedure DrawTabGradient(ABitmap: TBGRABitmap);
var
  r: TLResource;
  bmp: TBGRABitmap;
begin
  with ABitmap do
  begin
    Fill(MainColor);
    try
    begin
//      bmp := TBGRABitmap.Create('back5.png');
      r:=LazarusResources.Find('ribbon');
      bmp := TBGRABitmap.Create('back5.png');
        ABitmap.PutImage(width-220,1,bmp,dmDrawWithTransparency);
      bmp.Free;
    end;
    except
    end;

//    DrawHorizLine(0, 0, Width - 1, ColorToBGRA(MainColor));
    DrawHorizLine(1, Height - 1, Width - 2, $00F1F1F1);
  end;
end;

procedure DrawTitleGradient(ABitmap: TBGRABitmap);
var
  bmp: TBGRABitmap;
begin
  with ABitmap do
  begin
    Fill(MainColor);
    try
    begin
      bmp := TBGRABitmap.Create('back5.png');
      ABitmap.PutImage(width-300,-68,bmp,dmDrawWithTransparency);
      bmp.Free;
    end;
    except
    end;
  end;
end;

procedure DrawBodyGradient(ABitmap: TBGRABitmap);
begin
  with ABitmap do
  begin
{    GradientFill(0, 0, Width, Height, BGRA(247, 251, 255, 255),
      BGRA(220, 231, 245, 255), gtLinear, PointF(0, 0), PointF(0, Height - 3), dmSet);
    Rectangle(0, 0, Width, Height - 2, BGRA(255, 255, 255, 75), BGRAPixelTransparent,
      dmDrawWithTransparency);
}
    DrawHorizLine(1, Height - 1, Width - 2, $00D2D2D2); //Top
//    DrawVertLine(Width-1, 0, Height-1, ColorToBGRA(MainColor));  //Right
//    DrawVertLine(0, 0, Height-1, ColorToBGRA(MainColor)); //Left

  end;
end;

procedure DrawFormGradient(ABitmap: TBGRABitmap);
begin
  with ABitmap do
  begin
//    DrawVertLine(Width-1, 0, Height-1, ColorToBGRA(MainColor));  //Right
//    DrawHorizLine(0, Height - 1, Width - 1, ColorToBGRA(MainColor)); // Bottom
//    DrawVertLine(0, 0, Height-1, ColorToBGRA(MainColor)); //Left
    DrawHorizLine(1, 0, Width - 2, $00E6E6E6); // Top
//    GradientFill(0, 0, Width, Height, BGRA(197, 207, 223, 255),
//      BGRA(220, 229, 242, 255), gtLinear, PointF(0, 0), PointF(0, Height), dmSet);
  end;
end;

{ Buttons }

procedure RibbonTab(AButton: TBCButton);
begin
  with AButton do
  begin
    Rounding.RoundX := 0;
    Rounding.RoundY := 0;
    BorderSpacing.Top := 4;
    with StateNormal do
    begin
      FontEx.Color := clWhite;
      Border.Style := bboNone;
      Background.Color := TabNormalColor;
      Background.Style := bbsColor;
    end;
    with StateHover do
    begin
      FontEx.Color := clWhite;
      Border.Style := bboNone;
      Background.Color := TabHoverColor;
      Background.Style := bbsColor;
    end;
    with StateClicked do
    begin
      FontEx.Color := TabFntColor;
      Border.Style := bboNone;
      Background.Color := TabClickedColor;
      Background.Style := bbsColor;
    end;
  end;
end;

procedure RibbonButton(AButton: TBCButton);
begin
  with AButton do
  begin
    Rounding.RoundX := 0;
    Rounding.RoundY := 0;
    BorderSpacing.Top := 2;
    BorderSpacing.Left := 2;
    BorderSpacing.Right := 0;
    BorderSpacing.Bottom := 16;
    with StateNormal do
    begin
      Border.Style := bboNone;
      Background.Color := BtnNormalColor;
      Background.Style := bbsColor;
      FontEx.Name:='Segoe UI';
      FontEx.Shadow:=FALSE;
      FontEx.Color:=BtnFntColor;
      FontEx.Style:=[];
    end;
    with StateHover do
    begin
      Border.Style := bboNone;
      Background.Color := BtnHoverColor;
      Background.Style := bbsColor;
      FontEx.Name:='Segoe UI';
      FontEx.Shadow:=FALSE;
      FontEx.Color:=BtnFntColor;
      FontEx.Style:=[];
    end;
    with StateClicked do
    begin
      Border.Style := bboNone;
      Background.Color := BtnClickedColor;
      Background.Style := bbsColor;
      FontEx.Name:='Segoe UI';
      FontEx.Shadow:=FALSE;
      FontEx.Color:=BtnFntColor;
      FontEx.Style:=[];
    end;
  end;
end;

procedure RibbonTool(AButton: TBCButton);
begin
  with AButton do
  begin
    Rounding.RoundX := 0;
    Rounding.RoundY := 0;
    BorderSpacing.Around := 2;
    BorderSpacing.Bottom := 2;

    with StateNormal do
    begin
      FontEx.Color := clWhite;
      Border.Style := bboNone;
      Background.Color := ToolNormalColor;
      Background.Style := bbsColor;
    end;
    with StateHover do
    begin
      FontEx.Color := clWhite;
      Border.Style := bboNone;
      Background.Color := ToolHoverColor;
      Background.Style := bbsColor;
    end;
    with StateClicked do
    begin
      FontEx.Color := clWhite;
      Border.Style := bboNone;
      Background.Color := ToolClickedColor;
      Background.Style := bbsColor;
    end;
  end;
end;


procedure RibbonSeparator(AButton: TBCButton);
begin
  with AButton do
  begin
    StaticButton := True;
    Color:=clNone;
    BorderSpacing.Top:=2;
    BorderSpacing.Bottom:=2;
    ParentColor:= FALSE;
    with StateNormal do
    begin
      Border.Color := $00D2D0CF;
      Border.ColorOpacity := 255;
      Border.Style:=bboSolid;
      Background.Color := $00d2d2d2;
      Background.Style := bbsColor;

    end;
  end;
end;

procedure RibbonMenu(AButton: TBCButton);
begin
  with AButton do
  begin
    Rounding.RoundX := 0;
    Rounding.RoundY := 0;
    BorderSpacing.Top := 4;
    with StateNormal do
    begin
      FontEx.Color := clWhite;
      Border.Style := bboNone;
      Background.Color := TabNormalColor;
      Background.Style := bbsColor;
    end;
    with StateHover do
    begin
      FontEx.Color := clWhite;
      Border.Style := bboNone;
      Background.Color := TabHoverColor;
      Background.Style := bbsColor;
    end;
    with StateClicked do
    begin
      FontEx.Color := $009A572A;
      Border.Style := bboNone;
      Background.Color := TabClickedColor;
      Background.Style := bbsColor;
    end;
  end;

end;
end.
