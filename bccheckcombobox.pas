// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BCCheckComboBox;

{$mode delphi}
{$ifdef DARWIN}
{$modeswitch objectivec1}
{$endif}

interface

uses
  {$ifdef WINDOWS}Windows,{$endif} Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, Graphics, Dialogs, BCButton,
  StdCtrls, BCTypes, BCBaseCtrls, BGRABitmap, BGRABitmapTypes, LMessages, LCLType,
  BGRATheme
  {$ifdef DARWIN}, CocoaAll, CocoaUtils, cocoa_extra{$endif};

type

  { TBCCheckComboBox }

  TBCCheckComboBox = class(TBCStyleCustomControl)
  private
    FButton: TBCButton;
    FCanvasScaleMode: TBCCanvasScaleMode;
    FDropDownBorderSize: integer;
    FDropDownCount: integer;
    FDropDownColor: TColor;
    FDropDownFontColor: TColor;
    FDropDownFontHighlight: TColor;
    FDropDownHighlight: TColor;
    FFocusBorderColor: TColor;
    FFocusBorderOpacity: byte;
    FItems: TStringList;
    FCheckedItems: array of Boolean;
    FItemIndex: integer;
    FForm: TForm;
    FDropDownHideDate: TDateTime;
    FHoverItem: integer;
    FItemHeight: integer;
    FListBox: TListBox;
    FDropDownBorderColor: TColor;
    FOnDrawItem: TDrawItemEvent;
    FOnDrawSelectedItem: TOnAfterRenderBCButton;
    FOnChange: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FDrawingDropDown: boolean;
    FTimerCheckFormHide: TTimer;
    FQueryDropDownHide: boolean;
    procedure ButtonClick(Sender: TObject);
    procedure DrawCheckBox(aCaption: string; State: TBGRAThemeButtonState;
      aFocused: boolean; Checked: boolean; ARect: TRect;
      ASurface: TBGRAThemeSurface);
    procedure FormDeactivate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    function GetArrowFlip: boolean;
    function GetCaption: String;
    function GetChecked(AIndex: Integer): Boolean;
    function GetComboCanvas: TCanvas;
    function GetArrowSize: integer;
    function GetArrowWidth: integer;
    function GetGlobalOpacity: byte;
    function GetItemText: string;
    function GetDropDownColor: TColor;
    function GetItemIndex: integer;
    function GetItems: TStrings;
    function GetMemoryUsage: TBCButtonMemoryUsage;
    function GetOnDrawSelectedItem: TOnAfterRenderBCButton;
    function GetRounding: TBCRounding;
    function GetStateClicked: TBCButtonState;
    function GetStateHover: TBCButtonState;
    function GetStateNormal: TBCButtonState;
    function GetStaticButton: boolean;
    procedure ListBoxClick(Sender: TObject);
    procedure ListBoxExit(Sender: TObject);
    procedure ListBoxKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure ListBoxMouseLeave(Sender: TObject);
    procedure ListBoxMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure ListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure OnAfterRenderButton(Sender: TObject; const ABGRA: TBGRABitmap;
      AState: TBCButtonState; ARect: TRect);
    procedure OnTimerCheckFormHide(Sender: TObject);
    procedure SetArrowFlip(AValue: boolean);
    procedure SetArrowSize(AValue: integer);
    procedure SetArrowWidth(AValue: integer);
    procedure SetCanvasScaleMode(AValue: TBCCanvasScaleMode);
    procedure SetCaption(AValue: String);
    procedure SetChecked(AIndex: Integer; AValue: Boolean);
    procedure SetDropDownColor(AValue: TColor);
    procedure SetGlobalOpacity(AValue: byte);
    procedure SetItemIndex(AValue: integer);
    procedure SetItems(AValue: TStrings);
    procedure SetMemoryUsage(AValue: TBCButtonMemoryUsage);
    procedure SetOnDrawSelectedItem(AValue: TOnAfterRenderBCButton);
    procedure SetRounding(AValue: TBCRounding);
    procedure SetStateClicked(AValue: TBCButtonState);
    procedure SetStateHover(AValue: TBCButtonState);
    procedure SetStateNormal(AValue: TBCButtonState);
    procedure SetStaticButton(AValue: boolean);
    procedure SyncCheckedArraySize;
  protected
    function GetStyleExtension: String; override;
    procedure WMSetFocus(var {%H-}Message: {$IFDEF FPC}TLMSetFocus{$ELSE}TWMSetFocus{$ENDIF}); message {$IFDEF FPC}LM_SETFOCUS{$ELSE}WM_SETFOCUS{$ENDIF};
    procedure WMKillFocus(var Message: {$IFDEF FPC}TLMKillFocus{$ELSE}TWMKillFocus{$ENDIF}); message {$IFDEF FPC}LM_KILLFOCUS{$ELSE}WM_KILLFOCUS{$ENDIF};
    procedure UpdateFocus(AFocused: boolean);
    procedure KeyDown(var Key: Word; {%H-}Shift: TShiftState); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure CreateDropDown;
    procedure FreeDropDown;
    function CloseDropDown: boolean;
    function GetListBox: TListBox;
    procedure UpdateButtonCanvasScaleMode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Assign the properties from Source to this instance }
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure CheckAll;
    procedure UnCheckAll;
    property HoverItem: integer read FHoverItem;
    property Button: TBCButton read FButton write FButton;
    property ListBox: TListBox read GetListBox;
    property Text: string read GetItemText;
    property Checked[AIndex: Integer]: Boolean read GetChecked write SetChecked;
  published
    property Anchors;
    property Canvas: TCanvas read GetComboCanvas;
    property CanvasScaleMode: TBCCanvasScaleMode read FCanvasScaleMode write SetCanvasScaleMode default csmAuto;
    property Caption: String read GetCaption write SetCaption;
    property Items: TStrings read GetItems write SetItems;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property ItemHeight: integer read FItemHeight write FItemHeight default 0;
    property ArrowSize: integer read GetArrowSize write SetArrowSize;
    property ArrowWidth: integer read GetArrowWidth write SetArrowWidth;
    property ArrowFlip: boolean read GetArrowFlip write SetArrowFlip default false;
    property FocusBorderColor: TColor read FFocusBorderColor write FFocusBorderColor default clBlack;
    property FocusBorderOpacity: byte read FFocusBorderOpacity write FFocusBorderOpacity default 255;
    property DropDownBorderColor: TColor read FDropDownBorderColor write FDropDownBorderColor default clWindowText;
    property DropDownBorderSize: integer read FDropDownBorderSize write FDropDownBorderSize default 1;
    property DropDownColor: TColor read GetDropDownColor write SetDropDownColor default clWindow;
    property DropDownFontColor: TColor read FDropDownFontColor write FDropDownFontColor default clWindowText;
    property DropDownCount: integer read FDropDownCount write FDropDownCount default 8;
    property DropDownHighlight: TColor read FDropDownHighlight write FDropDownHighlight default clHighlight;
    property DropDownFontHighlight: TColor read FDropDownFontHighlight write FDropDownFontHighlight default clHighlightText;
    property GlobalOpacity: byte read GetGlobalOpacity write SetGlobalOpacity;
    property MemoryUsage: TBCButtonMemoryUsage read GetMemoryUsage write SetMemoryUsage;
    property Rounding: TBCRounding read GetRounding write SetRounding;
    property StateClicked: TBCButtonState read GetStateClicked write SetStateClicked;
    property StateHover: TBCButtonState read GetStateHover write SetStateHover;
    property StateNormal: TBCButtonState read GetStateNormal write SetStateNormal;
    property StaticButton: boolean read GetStaticButton write SetStaticButton;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnDrawSelectedItem: TOnAfterRenderBCButton read GetOnDrawSelectedItem write SetOnDrawSelectedItem;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property TabStop;
    property TabOrder;
  end;

procedure Register;

implementation

uses math, PropEdits, BGRAText;

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCCheckComboBox]);
end;

{ TBCCheckComboBox }

procedure TBCCheckComboBox.ButtonClick(Sender: TObject);
const MinDelayReopen = 500/(1000*60*60*24);
var
  p: TPoint;
  w, h: Integer;
  f: TCustomForm;
begin
  CreateDropDown;

  if not CloseDropDown and (Now > FDropDownHideDate+MinDelayReopen) then
  begin
    // Prepare the list box
    FListBox.Items.Assign(FItems);
    FListBox.Font.Name := Button.StateNormal.FontEx.Name;
    FListBox.Font.Style := Button.StateNormal.FontEx.Style;
    FListBox.Font.Height := FontEmHeightSign*Button.StateNormal.FontEx.Height;
    h := self.Canvas.GetTextHeight('Hg') + 6;
    FListBox.ItemHeight := h;

    // List box size
    w := FButton.Width - 2*FDropDownBorderSize;
    h := FListBox.ItemHeight * min(FItems.Count, FDropDownCount);
    {$IFDEF DARWIN}
    inc(h, 20);
    {$ENDIF}
    FListBox.SetBounds(FDropDownBorderSize, FDropDownBorderSize, w, h);

    // Drop-down form size
    w := FListBox.Width + 2*FDropDownBorderSize;
    h := FListBox.Height + 2*FDropDownBorderSize;
    FForm.SetBounds(FForm.Left, FForm.Top,
      w + FForm.Width - FForm.ClientWidth,
      h + FForm.Height - FForm.ClientHeight);

    // Position
    p := ControlToScreen(Point(FButton.Left, FButton.Top + FButton.Height));
    FForm.Left := p.X;
    FForm.Top := p.Y;
    FForm.Color := FDropDownBorderColor;

    if Assigned(FOnDropDown) then FOnDropDown(self);
    FQueryDropDownHide := false;
    FTimerCheckFormHide.Enabled := true;
    FForm.Visible := True;
    {$IFDEF DARWIN}
    // Keep the drop-down above modal forms
    if FForm.HandleAllocated then
    begin
      NSView(FForm.Handle).window.setLevel(NSPopUpMenuWindowLevel);
      NSView(FForm.Handle).window.makeKeyAndOrderFront(NSApp);
    end;
    {$ENDIF}
    if FListBox.CanSetFocus then
      FListBox.SetFocus;
  end;
end;

procedure TBCCheckComboBox.FormDeactivate(Sender: TObject);
begin
  FQueryDropDownHide := true;
end;

procedure TBCCheckComboBox.FormHide(Sender: TObject);
begin
  FDropDownHideDate := Now;
end;

function TBCCheckComboBox.GetArrowFlip: boolean;
begin
  result := Button.FlipArrow;
end;

function TBCCheckComboBox.GetCaption: String;
begin
  Result := Button.Caption;
end;

function TBCCheckComboBox.GetChecked(AIndex: Integer): Boolean;
begin
  SyncCheckedArraySize;
  if (AIndex >= 0) and (AIndex < Length(FCheckedItems)) then
    Result := FCheckedItems[AIndex]
  else
    Result := False;
end;

function TBCCheckComboBox.GetComboCanvas: TCanvas;
begin
  if FDrawingDropDown then
    result := ListBox.Canvas
  else
    result := inherited Canvas;
end;

function TBCCheckComboBox.GetArrowSize: integer;
begin
  result := Button.DropDownArrowSize;
end;

function TBCCheckComboBox.GetArrowWidth: integer;
begin
  result := Button.DropDownWidth;
end;

function TBCCheckComboBox.GetGlobalOpacity: byte;
begin
  result := Button.GlobalOpacity;
end;

function TBCCheckComboBox.GetItemText: string;
begin
  if ItemIndex<>-1 then
    result := Items[ItemIndex]
  else
    result := '';
end;

function TBCCheckComboBox.GetDropDownColor: TColor;
begin
  if Assigned(FListBox) then
    result := FListBox.Color
    else result := FDropDownColor;
end;

function TBCCheckComboBox.GetItemIndex: integer;
begin
  if Assigned(FListBox) then
    result := FListBox.ItemIndex
    else
    begin
      if FItemIndex >= FItems.Count then
        FItemIndex := -1;
      result := FItemIndex;
    end;
end;

function TBCCheckComboBox.GetItems: TStrings;
begin
  Result := FItems;
end;

function TBCCheckComboBox.GetMemoryUsage: TBCButtonMemoryUsage;
begin
  result := Button.MemoryUsage;
end;

function TBCCheckComboBox.GetOnDrawSelectedItem: TOnAfterRenderBCButton;
begin
  result := FOnDrawSelectedItem;
end;

function TBCCheckComboBox.GetRounding: TBCRounding;
begin
  result := Button.Rounding;
end;

function TBCCheckComboBox.GetStateClicked: TBCButtonState;
begin
  result := Button.StateClicked;
end;

function TBCCheckComboBox.GetStateHover: TBCButtonState;
begin
  result := Button.StateHover;
end;

function TBCCheckComboBox.GetStateNormal: TBCButtonState;
begin
  result := Button.StateNormal;
end;

function TBCCheckComboBox.GetStaticButton: boolean;
begin
  result := Button.StaticButton;
end;

procedure TBCCheckComboBox.ListBoxClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := FListBox.ItemIndex;
  if (idx >= 0) and (idx < FItems.Count) then
  begin
    SyncCheckedArraySize;
    FCheckedItems[idx] := not FCheckedItems[idx];
    FListBox.Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TBCCheckComboBox.ListBoxExit(Sender: TObject);
begin
  CloseDropDown;
end;

procedure TBCCheckComboBox.ListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_SPACE then
  begin
    ListBoxClick(Sender);
    Key := 0;
  end
  else if (Key = VK_RETURN) or (Key = VK_ESCAPE) then
  begin
    if Assigned(FForm) then FForm.Visible := false;
    Key := 0;
  end;
end;

procedure TBCCheckComboBox.ListBoxMouseLeave(Sender: TObject);
begin
  FHoverItem := -1;
  FListBox.Repaint;
end;

procedure TBCCheckComboBox.ListBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  TempItem: integer;
begin
  TempItem := FListBox.ItemAtPos(Point(x, y), True);

  if TempItem <> FHoverItem then
  begin
    FHoverItem := TempItem;
    FListBox.Repaint;
  end;
end;

procedure TBCCheckComboBox.ListBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  surface: TBGRAThemeSurface;
  parentForm: TCustomForm;
  lclDPI: Integer;
  isChecked: Boolean;
  itemState: TBGRAThemeButtonState;
begin
  SyncCheckedArraySize;
  if (Index >= 0) and (Index < Length(FCheckedItems)) then
    isChecked := FCheckedItems[Index]
  else
    isChecked := False;

  if Index = FHoverItem then
    itemState := btbsHover
  else
    itemState := btbsNormal;

  parentForm := GetParentForm(Control, False);
  if Assigned(parentForm) then
    lclDPI := parentForm.PixelsPerInch
    else lclDPI := Screen.PixelsPerInch;
  surface := TBGRAThemeSurface.Create(ARect, TListBox(Control).Canvas, Control.GetCanvasScaleFactor, lclDPI);
  try
    DrawCheckBox(FItems[Index], itemState, False, isChecked, ARect, surface);
  finally
    surface.Free;
  end;
end;

procedure TBCCheckComboBox.DrawCheckBox(aCaption: string; State: TBGRAThemeButtonState;
  aFocused: boolean; Checked: boolean; ARect: TRect; ASurface: TBGRAThemeSurface
  );
var
  Style: TTextStyle;
  aColor, bgColor: TBGRAPixel;
  aleft, atop, aright, abottom: integer;
  penWidth: single;
  textColor: TColor;
begin
  with ASurface do
  begin
    // Determine colors based on state
    case State of
      btbsHover:
      begin
        aColor := BGRA(0, 120, 215);
        bgColor := ColorToBGRA(FDropDownHighlight);
        textColor := FDropDownFontHighlight;
      end;
      btbsActive:
      begin
        aColor := BGRA(0, 84, 153);
        bgColor := ColorToBGRA(FDropDownHighlight);
        textColor := FDropDownFontHighlight;
      end;
      btbsDisabled:
      begin
        aColor := BGRA(204, 204, 204);
        bgColor := ColorToBGRA(FDropDownColor);
        textColor := clGray;
      end;
      else {btbsNormal}
      begin
        aColor := BGRABlack;
        bgColor := ColorToBGRA(FDropDownColor);
        textColor := FDropDownFontColor;
      end;
    end;

    DestCanvas.Font.Color := textColor;

    // Fill background with appropriate color
    Bitmap.Fill(bgColor);
    BitmapRect := ARect;
    penWidth := ASurface.ScaleForBitmap(10) / 10;
    aleft := round(penWidth);
    aright := Bitmap.Height-round(penWidth);
    atop := round(penWidth);
    abottom := Bitmap.Height-round(penWidth);
    Bitmap.RectangleAntialias(aleft-0.5+penWidth/2, atop-0.5+penWidth/2,
      aright-0.5-penWidth/2, abottom-0.5-penWidth/2,
      aColor, penWidth);
    aleft := round(penWidth*2);
    aright := Bitmap.Height-round(penWidth*2);
    atop := round(penWidth*2);
    abottom := Bitmap.Height-round(penWidth*2);
    if Checked then
      Bitmap.DrawPolyLineAntialias(Bitmap.ComputeBezierSpline(
        [BezierCurve(pointF(aleft + 2, atop + 3), PointF((aleft + aright - 1) / 2, abottom - 3)),
        BezierCurve(PointF((aleft + aright - 1) / 2, abottom - 3), PointF(
        (aleft + aright - 1) / 2, (atop * 2 + abottom - 1) / 3), PointF(aright - 2, atop))]),
        aColor, penWidth*1.5);
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

procedure TBCCheckComboBox.OnAfterRenderButton(Sender: TObject;
  const ABGRA: TBGRABitmap; AState: TBCButtonState; ARect: TRect);
var
  focusMargin: integer;
begin
  if Assigned(FOnDrawSelectedItem) then
    FOnDrawSelectedItem(self, ABGRA, AState, ARect);
  if Focused then
  begin
    focusMargin := round(2 * Button.CanvasScale);
    ABGRA.RectangleAntialias(ARect.Left + focusMargin, ARect.Top + focusMargin,
      ARect.Right - focusMargin - 1, ARect.Bottom - focusMargin - 1,
      ColorToBGRA(FocusBorderColor, FocusBorderOpacity),
      Button.CanvasScale);
  end;
end;

procedure TBCCheckComboBox.OnTimerCheckFormHide(Sender: TObject);
  {$ifdef WINDOWS}
  function IsDropDownOnTop: boolean;
  begin
    result := Assigned(FForm) and (GetForegroundWindow = FForm.Handle);
  end;
  {$endif}

begin
  if Assigned(FForm) and FForm.Visible and
    ({$IFDEF DARWIN}not FForm.Active or {$ENDIF}
     {$IFDEF WINDOWS}not IsDropDownOnTop or{$ENDIF}
     FQueryDropDownHide) then
  begin
    FForm.Visible := false;
    FQueryDropDownHide := false;
    FTimerCheckFormHide.Enabled := false;
  end;
end;

procedure TBCCheckComboBox.SetArrowFlip(AValue: boolean);
begin
  Button.FlipArrow:= AValue;
end;

procedure TBCCheckComboBox.SetArrowSize(AValue: integer);
begin
  Button.DropDownArrowSize:= AValue;
end;

procedure TBCCheckComboBox.SetArrowWidth(AValue: integer);
begin
  Button.DropDownWidth:= AValue;
end;

procedure TBCCheckComboBox.SetCanvasScaleMode(AValue: TBCCanvasScaleMode);
begin
  if FCanvasScaleMode=AValue then Exit;
  FCanvasScaleMode:=AValue;
  UpdateButtonCanvasScaleMode;
end;

procedure TBCCheckComboBox.SetCaption(AValue: String);
begin
  Button.Caption := AValue;
end;

procedure TBCCheckComboBox.SetChecked(AIndex: Integer; AValue: Boolean);
begin
  SyncCheckedArraySize;
  if (AIndex >= 0) and (AIndex < Length(FCheckedItems)) then
  begin
    FCheckedItems[AIndex] := AValue;
    if Assigned(FListBox) then
      FListBox.Invalidate;
  end;
end;

procedure TBCCheckComboBox.SetDropDownColor(AValue: TColor);
begin
  if Assigned(FListBox) then
    FListBox.Color := AValue;
  FDropDownColor:= AValue;
end;

procedure TBCCheckComboBox.SetGlobalOpacity(AValue: byte);
begin
  Button.GlobalOpacity := AValue;
end;

procedure TBCCheckComboBox.SetItemIndex(AValue: integer);
begin
  if Assigned(FListBox) then
    FListBox.ItemIndex := AValue;
  if AValue <> FItemIndex then
  begin
    FItemIndex := AValue;
    Button.Caption := GetItemText;
  end;
end;

procedure TBCCheckComboBox.SetItems(AValue: TStrings);
begin
  FItems.Assign(AValue);
  SyncCheckedArraySize;
  if Assigned(FListBox) then
    FListBox.Items.Assign(FItems);
end;

procedure TBCCheckComboBox.SetMemoryUsage(AValue: TBCButtonMemoryUsage);
begin
  Button.MemoryUsage := AValue;
end;

procedure TBCCheckComboBox.SetOnDrawSelectedItem(AValue: TOnAfterRenderBCButton);
begin
  if @FOnDrawSelectedItem = @AValue then Exit;
  FOnDrawSelectedItem:= AValue;
  FButton.ShowCaption := not Assigned(AValue);
  UpdateButtonCanvasScaleMode;
end;

procedure TBCCheckComboBox.SetRounding(AValue: TBCRounding);
begin
  Button.Rounding := AValue;
end;

procedure TBCCheckComboBox.SetStateClicked(AValue: TBCButtonState);
begin
  Button.StateClicked := AValue;
end;

procedure TBCCheckComboBox.SetStateHover(AValue: TBCButtonState);
begin
  Button.StateHover := AValue;
end;

procedure TBCCheckComboBox.SetStateNormal(AValue: TBCButtonState);
begin
  Button.StateNormal := AValue;
end;

procedure TBCCheckComboBox.SetStaticButton(AValue: boolean);
begin
  Button.StaticButton:= AValue;
end;

procedure TBCCheckComboBox.SyncCheckedArraySize;
begin
  if Length(FCheckedItems) <> FItems.Count then
    SetLength(FCheckedItems, FItems.Count);
end;

function TBCCheckComboBox.GetStyleExtension: String;
begin
  result := 'bccheckcombo';
end;

procedure TBCCheckComboBox.WMSetFocus(var Message: TLMSetFocus);
begin
  UpdateFocus(True);
end;

procedure TBCCheckComboBox.WMKillFocus(var Message: TLMKillFocus);
begin
  if Message.FocusedWnd <> Handle then
    UpdateFocus(False);
end;

procedure TBCCheckComboBox.UpdateFocus(AFocused: boolean);
var
  lForm: TCustomForm;
begin
  lForm := GetParentForm(Self);
  if lForm = nil then Exit;

  {$IFDEF FPC}
  if AFocused then
    ActiveDefaultControlChanged(lForm.ActiveControl)
  else
    ActiveDefaultControlChanged(nil);
  {$ENDIF}
  FButton.UpdateControl;
  Invalidate;
end;

procedure TBCCheckComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    ButtonClick(nil);
    Key := 0;
  end
  else if Key = VK_DOWN then
  begin
    if ItemIndex + 1 < FItems.Count then
    begin
      ItemIndex := ItemIndex + 1;
      Button.Caption := GetItemText;
      if Assigned(FOnChange) then
        FOnChange(Self);
    end;
    Key := 0;
  end
  else if Key = VK_UP then
  begin
    if ItemIndex - 1 >= 0 then
    begin
      ItemIndex := ItemIndex - 1;
      Button.Caption := GetItemText;
      if Assigned(FOnChange) then
        FOnChange(Self);
    end;
    Key := 0;
  end;
end;

procedure TBCCheckComboBox.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  i: integer;
begin
  for i:=0 to FItems.Count-1 do
  begin
    if (FItems[i] <> '') and FItems[i].ToLower.StartsWith(LowerCase(UTF8Key)) then
    begin
      if ItemIndex <> i then
      begin
        ItemIndex := i;
        Button.Caption := GetItemText;
        if Assigned(FOnChange) then
          FOnChange(Self);
        break;
      end;
    end;
  end;
end;

procedure TBCCheckComboBox.CreateDropDown;
begin
  if FForm = nil then
  begin
    FForm := TForm.Create(Self);
    FForm.Visible := False;
    FForm.ShowInTaskBar:= stNever;
    FForm.BorderStyle := bsNone;
    FForm.OnDeactivate:= FormDeactivate;
    FForm.OnHide:=FormHide;
    FForm.FormStyle := fsStayOnTop;
  end;

  if FListBox = nil then
  begin
    FListBox := TListBox.Create(self);
    FListBox.Parent := FForm;
    FListBox.BorderStyle := bsNone;
    FListBox.OnClick := ListBoxClick;
    FListBox.OnMouseLeave:=ListBoxMouseLeave;
    FListBox.OnMouseMove:=ListBoxMouseMove;
    FListBox.OnExit := ListBoxExit;
    FListBox.Style := lbOwnerDrawFixed;
    FListBox.OnDrawItem:= ListBoxDrawItem;
    FListBox.Options := [];
    FListBox.OnKeyDown:=ListBoxKeyDown;
    FListBox.Items.Assign(FItems);
    FListBox.ItemIndex := FItemIndex;
    FListBox.Color := FDropDownColor;
  end;
end;

procedure TBCCheckComboBox.FreeDropDown;
begin
  if Assigned(FListBox) then
  begin
    if FListBox.LCLRefCount > 0 then exit;
    FItemIndex := FListBox.ItemIndex;
    FDropDownColor:= FListBox.Color;
    FreeAndNil(FListBox);
  end;
  FreeAndNil(FForm);
end;

function TBCCheckComboBox.CloseDropDown: boolean;
begin
  if Assigned(FForm) and FForm.Visible then
  begin
    FForm.Close;
    FDropDownHideDate := Now;
    result := true;
  end else
    result := false;
  FQueryDropDownHide := true;
end;

function TBCCheckComboBox.GetListBox: TListBox;
begin
  CreateDropDown;
  result := FListBox;
end;

procedure TBCCheckComboBox.UpdateButtonCanvasScaleMode;
begin
  if (CanvasScaleMode = csmFullResolution) or
     ((CanvasScaleMode = csmAuto) and not Assigned(FOnDrawSelectedItem)) then
     FButton.CanvasScaleMode:= csmFullResolution
     else FButton.CanvasScaleMode:= csmScaleBitmap;
end;

constructor TBCCheckComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TBCButton.Create(Self);
  FButton.Align := alClient;
  FButton.Parent := Self;
  FButton.OnClick := ButtonClick;
  FButton.DropDownArrow := True;
  FButton.OnAfterRenderBCButton := OnAfterRenderButton;
  FFocusBorderColor := clBlack;
  FFocusBorderOpacity := 255;
  UpdateButtonCanvasScaleMode;

  FItems := TStringList.Create;
  SetLength(FCheckedItems, 0);
  FHoverItem := -1;
  FItemIndex := -1;

  DropDownBorderSize := 1;
  DropDownColor := clWindow;
  DropDownBorderColor := clWindowText;
  DropDownCount := 8;
  DropDownFontColor := clWindowText;
  DropDownHighlight := clHighlight;
  DropDownFontHighlight := clHighlightText;

  FTimerCheckFormHide := TTimer.Create(self);
  {$IFDEF LCLgtk3}
  FTimerCheckFormHide.Enabled := false;
  {$ENDIF}
  FTimerCheckFormHide.Interval:= 30;
  FTimerCheckFormHide.OnTimer:= OnTimerCheckFormHide;
end;

destructor TBCCheckComboBox.Destroy;
begin
  FreeAndNil(FItems);
  SetLength(FCheckedItems, 0);
  inherited Destroy;
end;

procedure TBCCheckComboBox.Assign(Source: TPersistent);
var
  src: TBCCheckComboBox;
  i: Integer;
begin
  if Source is TBCCheckComboBox then
  begin
    src := TBCCheckComboBox(Source);
    Button.Assign(src.Button);
    Items.Assign(src.Items);
    ItemIndex := src.ItemIndex;
    DropDownBorderColor := src.DropDownBorderColor;
    DropDownBorderSize := src.DropDownBorderSize;
    DropDownColor := src.DropDownColor;
    DropDownFontColor := src.DropDownFontColor;
    DropDownCount := src.DropDownCount;
    DropDownHighlight := src.DropDownHighlight;
    DropDownFontHighlight := src.DropDownFontHighlight;
    // Copy checked states
    SyncCheckedArraySize;
    for i := 0 to FItems.Count - 1 do
      FCheckedItems[i] := src.Checked[i];
  end else
    inherited Assign(Source);
end;

procedure TBCCheckComboBox.Clear;
begin
  FItems.Clear;
  SetLength(FCheckedItems, 0);
  if Assigned(FListBox) then
    FListBox.Clear;
end;

procedure TBCCheckComboBox.CheckAll;
var
  i: Integer;
begin
  SyncCheckedArraySize;
  for i := 0 to Length(FCheckedItems) - 1 do
    FCheckedItems[i] := True;
  if Assigned(FListBox) then
    FListBox.Invalidate;
end;

procedure TBCCheckComboBox.UnCheckAll;
var
  i: Integer;
begin
  SyncCheckedArraySize;
  for i := 0 to Length(FCheckedItems) - 1 do
    FCheckedItems[i] := False;
  if Assigned(FListBox) then
    FListBox.Invalidate;
end;

end.
