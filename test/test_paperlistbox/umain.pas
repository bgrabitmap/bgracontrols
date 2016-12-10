unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BGRAVirtualScreen, BGRABitmap, BCTypes, BGRASliceScaling, Types, LCLType;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    BGRAVirtualScreen2: TBGRAVirtualScreen;
    BGRAVirtualScreen3: TBGRAVirtualScreen;
    ListBox1: TListBox;
    ListBox2: TListBox;
    procedure BGRAVirtualScreen2Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    shadow: TBGRASliceScaling;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BGRAVirtualScreen2Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  if Assigned(shadow) then
    shadow.Draw(Bitmap, 0, 0, Bitmap.Width, Bitmap.Height);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  shadow := TBGRASliceScaling.Create(ProgramDirectory + 'shadow.png');
  shadow.Margins := Margins(6, 9, 6, 9);

  with BGRAVirtualScreen2.ChildSizing do
  begin
    ControlsPerLine := 1;
    LeftRightSpacing := 4;
    TopBottomSpacing := 5;
  end;

  ListBox1.ItemHeight := ScaleY(48, 96);
  ListBox1.Style := lbOwnerDrawFixed;

  with BGRAVirtualScreen3.ChildSizing do
  begin
    ControlsPerLine := 1;
    LeftRightSpacing := 4;
    TopBottomSpacing := 5;
  end;

  ListBox2.ItemHeight := ScaleY(48, 96);
  ListBox2.Style := lbOwnerDrawFixed;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  shadow.Free;
end;

procedure TForm1.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  lb: TListBox;
  hg: integer;
begin
  lb := TListBox(Control);

  lb.Canvas.Clipping := False;

  if odSelected in State then
  begin
    lb.Canvas.Brush.Color := $00e4e4e4;
    lb.Canvas.Font.Style := [fsBold];
  end
  else
  begin
    lb.Canvas.Brush.Color := clWhite;
  end;
  lb.Canvas.FillRect(ARect);
  hg := lb.Canvas.TextHeight(lb.Items[Index]);
  lb.Canvas.Font.Color := clBlack;
  lb.Canvas.TextOut(ARect.Left + ScaleX(16, 96), ARect.Top + (lb.ItemHeight - hg) div 2, lb.Items[Index]);

  lb.Canvas.Clipping := True;
  lb.Canvas.ClipRect := Rect(0, 0, 0, 0);
end;

end.

