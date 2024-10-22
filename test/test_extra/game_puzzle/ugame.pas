unit ugame;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF} Classes, SysUtils, Forms, Graphics, Controls, LCLType;

procedure ToggleFullScreen(Form: TForm; ARect: TRect);
procedure CenterControl(Control: TControl);
function CalculateAspectRatioH(const W1, H1, W2: integer): integer; //result H2
function CalculateAspectRatioW(const W1, H1, H2: integer): integer; //result W2
function CalculateDestRect(ImageW, ImageH, DestW, DestH: integer;
  Stretch, Proportional, Center: boolean): TRect;
procedure HighDPI(FromDPI: integer);
procedure ScaleDPI(Control: TControl; FromDPI: integer);
procedure ScaleAspectRatio(Control: TControl; OriginalParentW, OriginalParentH: integer);
procedure ScaleAspectRatio(Control: TControl; DestW, DestH: integer;
  Stretch, Proportional, Center: boolean);
{$IFDEF WINDOWS}
procedure SetScreenResolution(
  const Width, Height: integer); overload;
procedure SetScreenResolution(const Width, Height, colorDepth: integer); overload;
{$ENDIF}

implementation

procedure ToggleFullScreen(Form: TForm; ARect: TRect);
begin
  Form.SetBounds(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  if Form.BorderStyle <> bsNone then
  begin
    Form.BorderStyle := bsNone;
    Form.WindowState := wsMaximized;
  end
  else
  begin
    Form.BorderStyle := bsDialog;
    Form.WindowState := wsNormal;
  end;
end;

procedure CenterControl(Control: TControl);
begin
  if not Control.HasParent then
    Exit;
  Control.SetBounds(
    Round((Control.Parent.Width - Control.Width) div 2),
    Round((Control.Parent.Height - Control.Height) div 2),
    Control.Width, Control.Height);
end;

function CalculateAspectRatioH(const W1, H1, W2: integer): integer;
begin
  Result := Round(H1 / W1 * W2);
end;

function CalculateAspectRatioW(const W1, H1, H2: integer): integer;
begin
  Result := Round(W1 / H1 * H2);
end;

function CalculateDestRect(ImageW, ImageH, DestW, DestH: integer;
  Stretch, Proportional, Center: boolean): TRect;
var
  w: integer;
  h: integer;
begin
  // Stretch or Proportional when Image (Width or Height) is bigger than Destination
  if Stretch or (Proportional and ((ImageW > DestW) or (ImageH > DestH))) then
  begin
    // Proportional when Image (Width or Height) is bigger than 0
    if Proportional and (ImageW > 0) and (ImageH > 0) then
    begin
      w := DestW;
      h := CalculateAspectRatioH(ImageW, ImageH, DestW);
      if h > DestH then
      begin
        h := DestH;
        w := CalculateAspectRatioW(ImageW, ImageH, DestH);
      end;
      ImageW := w;
      ImageH := h;
    end
    // Stretch not Proportional or when Image (Width or Height) is 0
    else
    begin
      ImageW := DestW;
      ImageH := DestH;
    end;
  end;

  Result := Rect(0, 0, ImageW, ImageH);

  // Center: Destination (Width or Height) - Image divided by 2
  if Center then
  begin
    Result.Left := Round((DestW - ImageW) div 2);
    Result.Top := Round((DestH - ImageH) div 2);
  end;
end;

procedure HighDPI(FromDPI: integer);
var
  i: integer;
begin
  if Screen.PixelsPerInch = FromDPI then
    exit;

  for i := 0 to Screen.FormCount - 1 do
    ScaleDPI(Screen.Forms[i], FromDPI);
end;

procedure ScaleDPI(Control: TControl; FromDPI: integer);
var
  i: integer;
  WinControl: TWinControl;
begin
  if Screen.PixelsPerInch = FromDPI then
    exit;

  with Control do
  begin
    Left := ScaleX(Left, FromDPI);
    Top := ScaleY(Top, FromDPI);
    Width := ScaleX(Width, FromDPI);
    Height := ScaleY(Height, FromDPI);
  end;

  if Control is TWinControl then
  begin
    WinControl := TWinControl(Control);
    if WinControl.ControlCount = 0 then
      exit;

    with WinControl.ChildSizing do
    begin
      HorizontalSpacing := ScaleX(HorizontalSpacing, FromDPI);
      LeftRightSpacing := ScaleX(LeftRightSpacing, FromDPI);
      TopBottomSpacing := ScaleY(TopBottomSpacing, FromDPI);
      VerticalSpacing := ScaleY(VerticalSpacing, FromDPI);
    end;

    for i := 0 to WinControl.ControlCount - 1 do
      ScaleDPI(WinControl.Controls[i], FromDPI);
  end;
end;

procedure ScaleAspectRatio(Control: TControl; OriginalParentW, OriginalParentH: integer);
var
  l, t, w, h: integer;
begin
  l := MulDiv(Control.Left, Control.Parent.Width, OriginalParentW);
  t := MulDiv(Control.Top, Control.Parent.Height, OriginalParentH);
  w := MulDiv(Control.Width, Control.Parent.Width, OriginalParentW);
  h := MulDiv(Control.Height, Control.Parent.Height, OriginalParentH);
  Control.SetBounds(l, t, w, h);
end;

procedure ScaleAspectRatio(Control: TControl; DestW, DestH: integer;
  Stretch, Proportional, Center: boolean);
var
  i: integer;
  r: TRect;
  WinControl: TWinControl;
  w, h: integer;
begin
  if Control is TWinControl then
  begin
    WinControl := TWinControl(Control);
    w := WinControl.Width;
    h := WinControl.Height;

    r := CalculateDestRect(WinControl.Width, WinControl.Height, DestW,
      DestH, Stretch, Proportional, Center);
    WinControl.SetBounds(r.Left, r.Top, r.Right, r.Bottom);

    if WinControl.ControlCount = 0 then
      exit;

    for i := 0 to WinControl.ControlCount - 1 do
      ScaleAspectRatio(WinControl.Controls[i], w, h);
  end;
end;

{$IFDEF WINDOWS}
procedure SetScreenResolution(
  const Width, Height: integer); overload;
var
  mode: TDevMode;
begin
  zeroMemory(@mode, sizeof(TDevMode));
  mode.dmSize := sizeof(TDevMode);
  mode.dmPelsWidth := Width;
  mode.dmPelsHeight := Height;
  mode.dmFields := DM_PELSWIDTH or DM_PELSHEIGHT;
  ChangeDisplaySettings(mode, 0);
end;

procedure SetScreenResolution(const Width, Height, colorDepth: integer); overload;
var
  mode: TDevMode;
begin
  zeroMemory(@mode, sizeof(TDevMode));
  mode.dmSize := sizeof(TDevMode);
  mode.dmPelsWidth := Width;
  mode.dmPelsHeight := Height;
  mode.dmBitsPerPel := colorDepth;
  mode.dmFields := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL;
  ChangeDisplaySettings(mode, 0);
end;

{$ENDIF}

end.


