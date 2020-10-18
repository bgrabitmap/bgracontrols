unit BCListBoxEx;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  LCLType;

type
  TBCListBoxEx = class(TCustomControl)
  private
    mousepos: TPoint;
    scrolly: integer;
    fitems: TStringList;
    itemselected: integer;
    itemheight: integer;
    lastitem: integer;
    invalidatecount: integer;
    scrollwidth: integer;
    function GetItemRect(index: integer): TRect;
    function GetItemVertically(y: integer): integer;
    procedure ScrollToItemTop();
    procedure ScrollToItemBottom();
    procedure ScrollToItem(index: integer);
    function ItemIsVisible(index: integer): boolean;
  protected
    procedure Click; override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): boolean; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Items: TStringList read Fitems;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCListBoxEx]);
end;

procedure TBCListBoxEx.Click;
var
  tempitem: integer;
begin
  tempitem := GetItemVertically(mousepos.Y);
  if tempitem <> itemselected then
  begin
    itemselected := tempitem;
    Invalidate;
  end;
end;

constructor TBCListBoxEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  scrolly := 0;
  itemheight := 150;
  scrollwidth := 20;
  lastitem := -1;
  invalidatecount := 0;
  itemselected := -1;
  fitems := TStringList.Create;
end;

destructor TBCListBoxEx.Destroy;
begin
  items.Free;
end;

procedure TBCListBoxEx.KeyDown(var Key: word; Shift: TShiftState);
var
  tempitem: integer;
begin
  case key of
    vk_down:
    begin
      tempitem := itemselected + 1;
      if (tempitem < items.Count) then
      begin
        itemselected := tempitem;
        if not ItemIsVisible(itemselected) then
          ScrollToItemBottom();
        if not ItemIsVisible(itemselected) then
          ScrollToItem(itemselected);
        Invalidate;
      end;
    end;
    vk_up:
    begin
      tempitem := itemselected - 1;
      if (tempitem >= 0) then
      begin
        itemselected := tempitem;
        if not ItemIsVisible(itemselected) then
          ScrollToItemTop();
        if not ItemIsVisible(itemselected) then
          ScrollToItem(itemselected);
        Invalidate;
      end;
    end;
  end;
end;

procedure TBCListBoxEx.MouseMove(Shift: TShiftState; X, Y: integer);
var
  tempitem: integer;
begin
  mousepos := Point(x, y);
  tempitem := GetItemVertically(mousepos.Y);
  if tempitem <> lastitem then
  begin
    lastitem := tempitem;
    Invalidate;
  end;
end;

function TBCListBoxEx.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): boolean;
var
  r: TRect;
begin
  result := False;
  r := GetItemRect(items.Count - 1);
  if (r.Bottom >= Height) then
  begin
    result := True;
    scrolly := scrolly - itemheight;
    Invalidate;
  end;
end;

function TBCListBoxEx.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): boolean;
var
  lastscroll: integer;
begin
  result := False;
  lastscroll := scrolly;
  scrolly := scrolly + itemheight;
  if (scrolly > 0) then
    scrolly := 0;
  if scrolly <> lastscroll then
  begin
    result := True;
    Invalidate;
  end;
end;

procedure TBCListBoxEx.Paint;
var
  i: integer;
  r: TRect;
  style: TTextStyle;
  start: integer;
begin
  style.Alignment := taCenter;
  style.Layout := tlCenter;
  start := -1;

  for i := trunc(abs(scrolly) / itemheight) to items.Count - 1 do
  begin
    r := GetItemRect(i);

    if (r.Top < Height) then
    begin
      if start = -1 then
        start := i;
      Canvas.Brush.Color := clGreen;
      if (GetItemVertically(mousepos.Y) = i) then
        canvas.Brush.Color := clMoneyGreen;
      if (itemselected = i) then
        canvas.Brush.Color := clBlue;
      Canvas.Rectangle(r);
      Canvas.Font.Color := clWhite;
      Canvas.TextRect(r, 0, 0, items[i], style);
      Caption := IntToStr(start) + '..' + IntToStr(i);
    end
    else
      break;
  end;

  Canvas.Brush.Color := clGray;
  Canvas.Rectangle(Width - scrollwidth, 0, Width, Height);

  Canvas.Font.Color := clRed;
  Canvas.TextOut(10, 10, IntToStr(invalidatecount));
  Inc(invalidatecount);
end;

function TBCListBoxEx.GetItemRect(index: integer): TRect;
begin
  Result := Rect(0, (index * itemheight) + scrolly, Width - scrollwidth,
    (index * itemheight) + scrolly + itemheight);
end;

function TBCListBoxEx.GetItemVertically(y: integer): integer;
var
  i: integer;
begin
  i := trunc(abs(scrolly) / itemheight);
  Result := i + trunc(y / itemheight);
  if (Result > items.Count) or (Result < 0) then
    Result := -1;
end;

procedure TBCListBoxEx.ScrollToItemTop();
begin
  scrolly := scrolly + itemheight;
end;

procedure TBCListBoxEx.ScrollToItemBottom();
begin
  scrolly := scrolly - itemheight;
end;

procedure TBCListBoxEx.ScrollToItem(index: integer);
begin
  scrolly := -itemheight * index;
end;

function TBCListBoxEx.ItemIsVisible(index: integer): boolean;
var
  r: TRect;
begin
  r := GetItemRect(index);
  Result := Rect(0, 0, Width, Height).Contains(r);
end;

end.
