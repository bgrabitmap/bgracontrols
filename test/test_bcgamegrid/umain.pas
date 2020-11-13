unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Graphics, SysUtils, BCGameGrid, BGRABitmap,
  BGRABitmapTypes, types, Controls, Dialogs, ExtCtrls, LCLType, BCEffect;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCGameGrid2: TBCGameGrid;
    Timer1: TTimer;
    procedure BCGameGrid2ClickControl(Sender: TObject; n, x, y: integer);
    procedure BCGameGrid2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure BCGameGrid2MouseEnter(Sender: TObject);
    procedure BCGameGrid2MouseLeave(Sender: TObject);
    procedure BCGameGrid2MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure BCGameGrid2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure BCGameGrid2MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure BCGameGrid2MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure BCGameGrid2MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure BCGameGrid2RenderControl(Sender: TObject; Bitmap: TBGRABitmap;
      r: TRect; n, x, y: integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure SetFSelected(AValue: integer);
    { private declarations }
  public
    { public declarations }
    FSelected: integer;
    Fade: TFading;
    property Selected: integer read FSelected write SetFSelected;
  end;

var
  Form1: TForm1;

const
  L1 = VK_LEFT;
  R1 = VK_RIGHT;
  U1 = VK_UP;
  D1 = VK_DOWN;
  L2 = VK_A;
  R2 = VK_D;
  U2 = VK_W;
  D2 = VK_S;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BCGameGrid2RenderControl(Sender: TObject; Bitmap: TBGRABitmap;
  r: TRect; n, x, y: integer);
var
  cr, cg, cb, ca: byte;
  bmp: TBGRABitmap;
begin
  cr := Random(100);
  cg := Random(100);
  cb := Random(255);
  ca := Random(100);

  // selected

  if Selected = n then
  begin
    ca := 255;
    Bitmap.FillRect(r, BGRA(0, 0, 255, Fade.Execute), dmSet);
  end

  // colors

  else
  begin
    Bitmap.FillRect(r, BGRA(cr, cg, cb, ca), dmSet);
    Bitmap.Rectangle(r, BGRA(100, 100, 100, ca), dmDrawWithTransparency);
  end;

  // text

  Bitmap.TextRect(r, concat('n', IntToStr(n), ',x', IntToStr(x), ',y', IntToStr(y)),
    taCenter, tlCenter, BGRA(0, 0, 0, ca));

  // crazy effect

  if n = BCGameGrid2.GridWidth * BCGameGrid2.GridHeight -1 then { remove this if you want to see the original thing... }
  if Odd(n) then { this is for improve speed... }
  begin
    bmp := Bitmap.FilterBlurRadial(1, rbFast) as TBGRABitmap;
    Bitmap.BlendImage(Random(4), Random(4), bmp, boLinearBlend);
    bmp.Free;
  end;

end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  Timer1.Enabled := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Fade.Mode := fmFadeInOut;
  Fade.Step := 17;
  Fade.Reset;
end;

procedure TForm1.FormHide(Sender: TObject);
begin

end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);

begin
  if (Key = L1) or (Key = L2) then
    Selected := Selected - 1
  else if (Key = R1) or (Key = R2) then
    Selected := Selected + 1
  else if (Key = U1) or (Key = U2) then
    Selected := Selected - BCGameGrid2.GridWidth
  else if (Key = D1) or (Key = D2) then
    Selected := Selected + BCGameGrid2.GridWidth;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  BCGameGrid2.RenderAndDrawControl;
end;

procedure TForm1.SetFSelected(AValue: integer);
begin
  if FSelected = AValue then
    Exit;
  FSelected := AValue;
end;

procedure TForm1.BCGameGrid2ClickControl(Sender: TObject; n, x, y: integer);
begin
  Selected := n;
end;

procedure TForm1.BCGameGrid2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  {if Button = mbLeft then
    ...}
end;

procedure TForm1.BCGameGrid2MouseEnter(Sender: TObject);
begin
  //ShowMessage('Enter');
end;

procedure TForm1.BCGameGrid2MouseLeave(Sender: TObject);
begin
  //ShowMessage('Leave');
end;

procedure TForm1.BCGameGrid2MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  {if ssLeft in Shift then
    ...}
end;

procedure TForm1.BCGameGrid2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  {if ssLeft in Shift then
    ...}
end;

procedure TForm1.BCGameGrid2MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  //ShowMessage('Wheeeeel!');
end;

procedure TForm1.BCGameGrid2MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: boolean);
begin
  { Decrease grid }
  BCGameGrid2.GridWidth := BCGameGrid2.GridWidth - 1;
  BCGameGrid2.GridHeight := BCGameGrid2.GridHeight - 1;
end;

procedure TForm1.BCGameGrid2MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: boolean);
begin
  { Increase grid }
  BCGameGrid2.GridWidth := BCGameGrid2.GridWidth + 1;
  BCGameGrid2.GridHeight := BCGameGrid2.GridHeight + 1;
end;

end.
