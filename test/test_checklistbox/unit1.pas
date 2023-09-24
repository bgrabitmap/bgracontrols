unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CheckLst, Types,
  StdCtrls, BGRAThemeCheckBox, BGRABitmap, BGRABitmapTypes, BGRATheme;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAThemeCheckBox1: TBGRAThemeCheckBox;
    CheckListBox1: TCheckListBox;
    procedure CheckListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
  private
    procedure DrawCheckBox(aCaption: string; State: TBGRAThemeButtonState;
      aFocused: boolean; Checked: boolean; ARect: TRect;
      ASurface: TBGRAThemeSurface);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.CheckListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  surface: TBGRAThemeSurface;
  parentForm: TCustomForm;
  lclDPI: Integer;
begin
  parentForm := GetParentForm(Control, False);
  if Assigned(parentForm) then
    lclDPI := parentForm.PixelsPerInch
    else lclDPI := Screen.PixelsPerInch;
  surface := TBGRAThemeSurface.Create(ARect, TCheckListBox(Control).Canvas, 1, lclDPI);
  try
    DrawCheckBox(TCheckListBox(Control).Items[Index], btbsNormal, False, TCheckListBox(Control).Checked[Index], ARect, surface);
  finally
    surface.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CheckListBox1.AddItem('Red', nil);
  CheckListBox1.AddItem('Green', nil);
  CheckListBox1.AddItem('Blue', nil);
  CheckListBox1.AddItem('Alpha', nil);
end;

procedure TForm1.DrawCheckBox(aCaption: string; State: TBGRAThemeButtonState;
  aFocused: boolean; Checked: boolean; ARect: TRect; ASurface: TBGRAThemeSurface
  );
var
  Style: TTextStyle;
  aColor: TBGRAPixel;
  aleft, atop, aright, abottom: integer;
begin
  with ASurface do
  begin
    DestCanvas.Font.Color := clBlack;
    case State of
      btbsHover: aColor := BGRA(0, 120, 215);
      btbsActive: aColor := BGRA(0, 84, 153);
      btbsDisabled:
      begin
        DestCanvas.Font.Color := clGray;
        aColor := BGRA(204, 204, 204);
      end;
      else {btbsNormal}
        aColor := BGRABlack;
    end;

    Bitmap.Fill(BGRAWhite);
    BitmapRect := ARect;
    Bitmap.Rectangle(0, 0, Bitmap.Height, Bitmap.Height, aColor, BGRAWhite);
    aleft := 0;
    aright := Bitmap.Height;
    atop := 0;
    abottom := Bitmap.Height;
    if Checked then
      Bitmap.DrawPolyLineAntialias(Bitmap.ComputeBezierSpline(
        [BezierCurve(pointF(aleft + 2, atop + 3), PointF((aleft + aright - 1) / 2, abottom - 3)),
        BezierCurve(PointF((aleft + aright - 1) / 2, abottom - 3), PointF(
        (aleft + aright - 1) / 2, (atop * 2 + abottom - 1) / 3), PointF(aright - 2, atop - 2))]),
        Color, 1.5);
    DrawBitmap;

    if aCaption <> '' then
    begin
      fillchar(Style, sizeof(Style), 0);
      Style.Alignment := taLeftJustify;
      Style.Layout := tlCenter;
      Style.Wordbreak := True;
      DestCanvas.TextRect(ARect,
        ARect.Height, 0, aCaption, Style);
    end;
  end;
end;

end.

