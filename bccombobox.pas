// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BCComboBox;

{$mode delphi}

interface

uses
  {$ifdef WINDOWS}Windows,{$endif} Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, Graphics, Dialogs, BCButton,
  StdCtrls, BCTypes, BCBaseCtrls, BGRABitmap, BGRABitmapTypes, LMessages, LCLType;

type

  { TBCComboBox }

  TBCComboBox = class(TBCStyleCustomControl)
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
    FItemIndex: integer;
    FForm: TForm;
    FFormHideDate: TDateTime;
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
    FQueryFormHide: boolean;
    procedure ButtonClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    function GetArrowFlip: boolean;
    function GetComboCanvas: TCanvas;
    function GetArrowSize: integer;
    function GetArrowWidth: integer;
    function GetButtonHint: TTranslateString;
    function GetButtonShowHint: Boolean;
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
    procedure ListBoxKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure ListBoxMouseUp({%H-}Sender: TObject; {%H-}Button: TMouseButton;
                          {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure ListBoxMouseLeave(Sender: TObject);
    procedure ListBoxMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure ListBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure ListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure OnAfterRenderButton(Sender: TObject; const ABGRA: TBGRABitmap;
      AState: TBCButtonState; ARect: TRect);
    procedure OnTimerCheckFormHide(Sender: TObject);
    procedure SetArrowFlip(AValue: boolean);
    procedure SetArrowSize(AValue: integer);
    procedure SetArrowWidth(AValue: integer);
    procedure SetButtonHint(const AValue: TTranslateString);
    procedure SetButtonShowHint(AValue: Boolean);
    procedure SetCanvasScaleMode(AValue: TBCCanvasScaleMode);
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

    function GetOnButtonMouseDown: TMouseEvent;
    function GetOnButtonMouseEnter: TNotifyEvent;
    function GetOnButtonMouseLeave: TNotifyEvent;
    function GetOnButtonMouseMove: TMouseMoveEvent;
    function GetOnButtonMouseUp: TMouseEvent;
    function GetOnButtonMouseWheel: TMouseWheelEvent;
    function GetOnButtonMouseWheelDown: TMouseWheelUpDownEvent;
    function GetOnButtonMouseWheelUp: TMouseWheelUpDownEvent;

    procedure SetOnButtonMouseDown(AValue: TMouseEvent);
    procedure SetOnButtonMouseEnter(AValue: TNotifyEvent);
    procedure SetOnButtonMouseLeave(AValue: TNotifyEvent);
    procedure SetOnButtonMouseMove(AValue: TMouseMoveEvent);
    procedure SetOnButtonMouseUp(AValue: TMouseEvent);
    procedure SetOnButtonMouseWheel(AValue: TMouseWheelEvent);
    procedure SetOnButtonMouseWheelDown(AValue: TMouseWheelUpDownEvent);
    procedure SetOnButtonMouseWheelUp(AValue: TMouseWheelUpDownEvent);
  protected
    function GetStyleExtension: String; override;
    procedure WMSetFocus(var {%H-}Message: {$IFDEF FPC}TLMSetFocus{$ELSE}TWMSetFocus{$ENDIF}); message {$IFDEF FPC}LM_SETFOCUS{$ELSE}WM_SETFOCUS{$ENDIF};
    procedure WMKillFocus(var Message: {$IFDEF FPC}TLMKillFocus{$ELSE}TWMKillFocus{$ENDIF}); message {$IFDEF FPC}LM_KILLFOCUS{$ELSE}WM_KILLFOCUS{$ENDIF};
    procedure UpdateFocus(AFocused: boolean);
    procedure KeyDown(var Key: Word; {%H-}Shift: TShiftState); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure CreateForm;
    procedure FreeForm;
    function GetListBox: TListBox;
    procedure UpdateButtonCanvasScaleMode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Assign the properties from Source to this instance }
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    property HoverItem: integer read FHoverItem;
    property Button: TBCButton read FButton write FButton;
    property ListBox: TListBox read GetListBox;
    property Text: string read GetItemText;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Canvas: TCanvas read GetComboCanvas;
    property CanvasScaleMode: TBCCanvasScaleMode read FCanvasScaleMode write SetCanvasScaleMode default csmAuto;
    property Hint: TTranslateString read GetButtonHint write SetButtonHint;
    property Items: TStrings read GetItems write SetItems;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property ItemHeight: integer read FItemHeight write FItemHeight default 0;
    property ArrowSize: integer read GetArrowSize write SetArrowSize;
    property ArrowWidth: integer read GetArrowWidth write SetArrowWidth;
    property ArrowFlip: boolean read GetArrowFlip write SetArrowFlip default false;
    property FocusBorderColor: TColor read FFocusBorderColor write FFocusBorderColor default clBlack;
    property FocusBorderOpacity: byte read FFocusBorderOpacity write FFocusBorderOpacity default 0;
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
    property ShowHint: Boolean read GetButtonShowHint write SetButtonShowHint default False;
    property StateClicked: TBCButtonState read GetStateClicked write SetStateClicked;
    property StateHover: TBCButtonState read GetStateHover write SetStateHover;
    property StateNormal: TBCButtonState read GetStateNormal write SetStateNormal;
    property StaticButton: boolean read GetStaticButton write SetStaticButton;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnDrawSelectedItem: TOnAfterRenderBCButton read GetOnDrawSelectedItem write SetOnDrawSelectedItem;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseDown: TMouseEvent read GetOnButtonMouseDown write SetOnButtonMouseDown;
    property OnMouseMove: TMouseMoveEvent read GetOnButtonMouseMove write SetOnButtonMouseMove;
    property OnMouseUp: TMouseEvent read GetOnButtonMouseUp write SetOnButtonMouseUp;
    property OnMouseEnter: TNotifyEvent read GetOnButtonMouseEnter write SetOnButtonMouseEnter;
    property OnMouseLeave: TNotifyEvent read GetOnButtonMouseLeave write SetOnButtonMouseLeave;
    property OnMouseWheel: TMouseWheelEvent read GetOnButtonMouseWheel write SetOnButtonMouseWheel;
    property OnMouseWheelDown: TMouseWheelUpDownEvent read GetOnButtonMouseWheelDown write SetOnButtonMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read GetOnButtonMouseWheelUp write SetOnButtonMouseWheelUp;
    property TabStop;
    property TabOrder;
  end;

procedure Register;

implementation

uses math, PropEdits, BGRAText;

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCComboBox]);
end;

{ TBCComboBox }

procedure TBCComboBox.ButtonClick(Sender: TObject);
const MinDelayReopen = 500/(1000*60*60*24);
var
  p: TPoint;
  h: Integer;
  s: TSize;
begin
  {$IFDEF DARWIN}
  //if Assigned(FForm) and not FForm.Visible then FreeForm;
  {$ENDIF}

  CreateForm;

  if FForm.Visible then
    FForm.Close
  else
  if Now > FFormHideDate+MinDelayReopen then
  begin
    p := ControlToScreen(Point(FButton.Left, FButton.Top + FButton.Height));
    FForm.Left := p.X;
    FForm.Top := p.Y;
    FForm.Color := FDropDownBorderColor;
    FListBox.Font.Name := Button.StateNormal.FontEx.Name;
    FListBox.Font.Style := Button.StateNormal.FontEx.Style;
    FListBox.Font.Height := FontEmHeightSign*Button.StateNormal.FontEx.Height;
    self.Canvas.Font.Assign(FListBox.Font);
    if Assigned(FOnDrawItem) and (FItemHeight <> 0) then
      h := FItemHeight else h := self.Canvas.GetTextHeight('Hg');
    {$IFDEF WINDOWS}inc(h,6);{$ENDIF}
    FListBox.ItemHeight := h;
    {$IFDEF LINUX}inc(h,6);{$ENDIF}
    {$IFDEF DARWIN}inc(h,2);{$ENDIF}
    s := TSize.Create(FButton.Width, h*min(Items.Count, FDropDownCount) + 2*FDropDownBorderSize);
    FForm.ClientWidth := s.cx;
    FForm.ClientHeight := s.cy;
    FListBox.SetBounds(FDropDownBorderSize,FDropDownBorderSize,
      s.cx - 2*FDropDownBorderSize,
      s.cy - 2*FDropDownBorderSize);
    if FForm.Top + FForm.Height > Screen.WorkAreaTop + Screen.WorkAreaHeight then
      FForm.Top := FForm.Top - FForm.Height - Self.Height;
    if Assigned(FOnDropDown) then FOnDropDown(self);
    //FForm.Visible := True;
    if FListBox.CanSetFocus then
      FListBox.SetFocus;
    FTimerCheckFormHide.Enabled:= true;
    FQueryFormHide := false;
    FForm.ShowModal;
  end;
end;

procedure TBCComboBox.FormDeactivate(Sender: TObject);
begin
  FQueryFormHide := true;
end;

procedure TBCComboBox.FormHide(Sender: TObject);
begin
  FFormHideDate := Now;
end;

function TBCComboBox.GetArrowFlip: boolean;
begin
  result := Button.FlipArrow;
end;

function TBCComboBox.GetComboCanvas: TCanvas;
begin
  if FDrawingDropDown then
    result := ListBox.Canvas
  else
    result := inherited Canvas;
end;

function TBCComboBox.GetArrowSize: integer;
begin
  result := Button.DropDownArrowSize;
end;

function TBCComboBox.GetArrowWidth: integer;
begin
  result := Button.DropDownWidth;
end;

function TBCComboBox.GetButtonHint: TTranslateString;
begin
  result := FButton.Hint;
end;

function TBCComboBox.GetButtonShowHint: Boolean;
begin
  result := FButton.ShowHint;
end;

function TBCComboBox.GetGlobalOpacity: byte;
begin
  result := Button.GlobalOpacity;
end;

function TBCComboBox.GetItemText: string;
begin
  if ItemIndex<>-1 then
    result := Items[ItemIndex]
  else
    result := '';
end;

function TBCComboBox.GetDropDownColor: TColor;
begin
  if Assigned(FListBox) then
    result := FListBox.Color
    else result := FDropDownColor;
end;

function TBCComboBox.GetItemIndex: integer;
begin
  if Assigned(FListBox) then
    result := FListBox.ItemIndex
    else
    begin
      if FItemIndex >= Items.Count then
        FItemIndex := -1;
      result := FItemIndex;
    end;
end;

function TBCComboBox.GetItems: TStrings;
begin
  if Assigned(FListBox) then
    Result := FListBox.Items
    else Result := FItems;
end;

function TBCComboBox.GetMemoryUsage: TBCButtonMemoryUsage;
begin
  result := Button.MemoryUsage;
end;

function TBCComboBox.GetOnDrawSelectedItem: TOnAfterRenderBCButton;
begin
  result := FOnDrawSelectedItem;
end;

function TBCComboBox.GetRounding: TBCRounding;
begin
  result := Button.Rounding;
end;

function TBCComboBox.GetStateClicked: TBCButtonState;
begin
  result := Button.StateClicked;
end;

function TBCComboBox.GetStateHover: TBCButtonState;
begin
  result := Button.StateHover;
end;

function TBCComboBox.GetStateNormal: TBCButtonState;
begin
  result := Button.StateNormal;
end;

function TBCComboBox.GetStaticButton: boolean;
begin
  result := Button.StaticButton;
end;

procedure TBCComboBox.ListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) or (Key = VK_ESCAPE) then
  begin
    ButtonClick(nil);
    Key := 0;
  end;
end;

procedure TBCComboBox.ListBoxMouseUp(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer);
begin
  FForm.Close;
  FQueryFormHide := true;
end;

procedure TBCComboBox.ListBoxMouseLeave(Sender: TObject);
begin
  FHoverItem := -1;
  FListBox.Repaint;
end;

procedure TBCComboBox.ListBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  TempItem: integer;
begin
  TempItem := FListBox.ItemAtPos(Point(x, y), True);

  if TempItem <> FHoverItem then
  begin
    FHoverItem := TempItem;
    if (FHoverItem<>-1) and ([ssLeft,ssRight]*Shift <> []) then
      FListBox.ItemIndex := FHoverItem;
    FListBox.Repaint;
  end;
end;

procedure TBCComboBox.ListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  Button.Caption := GetItemText;
  if User and Assigned(FOnChange) then FOnChange(self);
end;

procedure TBCComboBox.ListBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  aCanvas: TCanvas;
begin
  if Assigned(FOnDrawItem) then
  begin
    FDrawingDropDown := true;
    Exclude(State, odSelected);
    if Index = HoverItem then Include(State, odSelected);
    if Index = ItemIndex then Include(State, odChecked);
    try
      FOnDrawItem(Control, Index, ARect, State);
    finally
      FDrawingDropDown := false;
    end;
    exit;
  end;

  aCanvas := TListBox(Control).Canvas;
  if Index = HoverItem then
  begin
    aCanvas.Brush.Color := DropDownHighlight;
    aCanvas.Font.Color := DropDownFontHighlight;
  end
  else
  begin
    aCanvas.Brush.Color := DropDownColor;
    aCanvas.Font.Color := DropDownFontColor;
  end;
  aCanvas.Pen.Style := psClear;
  aCanvas.FillRect(ARect);
  aCanvas.TextRect(ARect, ARect.Left+4, ARect.Top +
    (ARect.Height - aCanvas.GetTextHeight(Items[Index])) div 2,
    Items[Index]);
end;

procedure TBCComboBox.OnAfterRenderButton(Sender: TObject;
  const ABGRA: TBGRABitmap; AState: TBCButtonState; ARect: TRect);
var
  FocusMargin: integer;
begin
  if Assigned(FOnDrawSelectedItem) then
    FOnDrawSelectedItem(self, ABGRA, AState, ARect);
  if Focused then
  begin
    FocusMargin := round(2 * FButton.CanvasScale);
    ABGRA.RoundRectAntialias(
      ARect.Left + FocusMargin,
      ARect.Top + FocusMargin,
      ARect.Right - FocusMargin - 1,
      ARect.Bottom - FocusMargin - 1,
      Max(0, FButton.Rounding.RoundX - FocusMargin),
      Max(0, FButton.Rounding.RoundY - FocusMargin),
      ColorToBGRA(FFocusBorderColor, FFocusBorderOpacity),
      FButton.CanvasScale);
  end;
end;

procedure TBCComboBox.OnTimerCheckFormHide(Sender: TObject);
  {$ifdef WINDOWS}
  function IsDropDownOnTop: boolean;
  begin
    result := Assigned(FForm) and (GetForegroundWindow = FForm.Handle);
  end;
  {$endif}

begin
  //if Assigned(FForm) and FForm.Visible and
    //({$IFDEF DARWIN}not FForm.Active or {$ENDIF}
     //{$IFDEF WINDOWS}not IsDropDownOnTop or{$ENDIF}
     //FQueryFormHide) then
  //begin
    //FForm.Visible := false;
    //FQueryFormHide := false;
    //FTimerCheckFormHide.Enabled := false;
  //end;
end;

procedure TBCComboBox.SetArrowFlip(AValue: boolean);
begin
  Button.FlipArrow:= AValue;
end;

procedure TBCComboBox.SetArrowSize(AValue: integer);
begin
  Button.DropDownArrowSize:= AValue;
end;

procedure TBCComboBox.SetArrowWidth(AValue: integer);
begin
  Button.DropDownWidth:= AValue;
end;

procedure TBCComboBox.SetButtonHint(const AValue: TTranslateString);
begin
  FButton.Hint := AValue;
end;

procedure TBCComboBox.SetButtonShowHint(AValue: Boolean);
begin
  FButton.ShowHint := AValue;
end;

procedure TBCComboBox.SetCanvasScaleMode(AValue: TBCCanvasScaleMode);
begin
  if FCanvasScaleMode=AValue then Exit;
  FCanvasScaleMode:=AValue;
  UpdateButtonCanvasScaleMode;
end;

procedure TBCComboBox.SetDropDownColor(AValue: TColor);
begin
  if Assigned(FListBox) then
    FListBox.Color := AValue
    else FDropDownColor:= AValue;
end;

procedure TBCComboBox.SetGlobalOpacity(AValue: byte);
begin
  Button.GlobalOpacity := AValue;
end;

procedure TBCComboBox.SetItemIndex(AValue: integer);
begin
  if Assigned(FListBox) then
    FListBox.ItemIndex := AValue
    else
    begin
      if AValue <> FItemIndex then
      begin
        FItemIndex := AValue;
        Button.Caption := GetItemText;
      end;
    end;
end;

procedure TBCComboBox.SetItems(AValue: TStrings);
begin
  if Assigned(FListBox) then
    FListBox.Items.Assign(AValue)
    else FItems.Assign(AValue);
end;

procedure TBCComboBox.SetMemoryUsage(AValue: TBCButtonMemoryUsage);
begin
  Button.MemoryUsage := AValue;
end;

procedure TBCComboBox.SetOnDrawSelectedItem(AValue: TOnAfterRenderBCButton);
begin
  if @FOnDrawSelectedItem = @AValue then Exit;
  FOnDrawSelectedItem:= AValue;
  FButton.ShowCaption := not Assigned(AValue);
  UpdateButtonCanvasScaleMode;
end;

procedure TBCComboBox.SetRounding(AValue: TBCRounding);
begin
  Button.Rounding := AValue;
end;

procedure TBCComboBox.SetStateClicked(AValue: TBCButtonState);
begin
  Button.StateClicked := AValue;
end;

procedure TBCComboBox.SetStateHover(AValue: TBCButtonState);
begin
  Button.StateHover := AValue;
end;

procedure TBCComboBox.SetStateNormal(AValue: TBCButtonState);
begin
  Button.StateNormal := AValue;
end;

procedure TBCComboBox.SetStaticButton(AValue: boolean);
begin
  Button.StaticButton:= AValue;
end;

function TBCComboBox.GetOnButtonMouseDown: TMouseEvent;
begin
  result := FButton.OnMouseDown;
end;

function TBCComboBox.GetOnButtonMouseEnter: TNotifyEvent;
begin
  result := FButton.OnMouseEnter;
end;

function TBCComboBox.GetOnButtonMouseLeave: TNotifyEvent;
begin
  result := FButton.OnMouseLeave;
end;

function TBCComboBox.GetOnButtonMouseMove: TMouseMoveEvent;
begin
  result := FButton.OnMouseMove;
end;

function TBCComboBox.GetOnButtonMouseUp: TMouseEvent;
begin
  result := FButton.OnMouseUp;
end;

function TBCComboBox.GetOnButtonMouseWheel: TMouseWheelEvent;
begin
  result := FButton.OnMouseWheel;
end;

function TBCComboBox.GetOnButtonMouseWheelDown: TMouseWheelUpDownEvent;
begin
  result := FButton.OnMouseWheelDown;
end;

function TBCComboBox.GetOnButtonMouseWheelUp: TMouseWheelUpDownEvent;
begin
  result := FButton.OnMouseWheelUp;
end;

procedure TBCComboBox.SetOnButtonMouseDown(AValue: TMouseEvent);
begin
  FButton.OnMouseDown := AValue;
end;

procedure TBCComboBox.SetOnButtonMouseEnter(AValue: TNotifyEvent);
begin
  FButton.OnMouseEnter := AValue;
end;

procedure TBCComboBox.SetOnButtonMouseLeave(AValue: TNotifyEvent);
begin
  FButton.OnMouseLeave := AValue;
end;

procedure TBCComboBox.SetOnButtonMouseMove(AValue: TMouseMoveEvent);
begin
  FButton.OnMouseMove := AValue;
end;

procedure TBCComboBox.SetOnButtonMouseUp(AValue: TMouseEvent);
begin
  FButton.OnMouseUp := AValue;
end;

procedure TBCComboBox.SetOnButtonMouseWheel(AValue: TMouseWheelEvent);
begin
  FButton.OnMouseWheel := AValue;
end;

procedure TBCComboBox.SetOnButtonMouseWheelDown(AValue: TMouseWheelUpDownEvent);
begin
  FButton.OnMouseWheelDown := AValue;
end;

procedure TBCComboBox.SetOnButtonMouseWheelUp(AValue: TMouseWheelUpDownEvent);
begin
  FButton.OnMouseWheelUp := AValue;
end;

function TBCComboBox.GetStyleExtension: String;
begin
  result := 'bccombo';
end;

procedure TBCComboBox.WMSetFocus(var Message: TLMSetFocus);
begin
  UpdateFocus(True);
end;

procedure TBCComboBox.WMKillFocus(var Message: TLMKillFocus);
begin
  if Message.FocusedWnd <> Handle then
    UpdateFocus(False);
end;

procedure TBCComboBox.UpdateFocus(AFocused: boolean);
var
  lForm: TCustomForm;
begin
  lForm := GetParentForm(Self);
  if lForm = nil then Exit;

  {$IFDEF FPC}//#
  if AFocused then
    ActiveDefaultControlChanged(lForm.ActiveControl)
  else
    ActiveDefaultControlChanged(nil);
  {$ENDIF}
  FButton.UpdateControl;
  Invalidate;
end;

procedure TBCComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    ButtonClick(nil);
    Key := 0;
  end
  else if Key = VK_DOWN then
  begin
    if ItemIndex + 1 < Items.Count then
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

procedure TBCComboBox.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  i: integer;
begin
  for i:=0 to Items.Count-1 do
  begin
    if (Items[i] <> '') and Items[i].ToLower.StartsWith(LowerCase(UTF8Key)) then
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

procedure TBCComboBox.CreateForm;
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

    FListBox.OnMouseLeave:=ListBoxMouseLeave;
    FListBox.OnMouseMove:=ListBoxMouseMove;
    FListBox.OnMouseUp:= ListBoxMouseUp;
    FListBox.Style := lbOwnerDrawFixed;
    FListBox.OnDrawItem:= ListBoxDrawItem;
    FListBox.Options := []; // do not draw focus rect
    FListBox.OnKeyDown:=ListBoxKeyDown;
    if Assigned(FItems) then
    begin
      FListBox.Items.Assign(FItems);
      FreeAndNil(FItems);
    end;
    FListBox.ItemIndex := FItemIndex;
    FListBox.Color := FDropDownColor;
    FListBox.OnSelectionChange := ListBoxSelectionChange;
  end;
end;

procedure TBCComboBox.FreeForm;
begin
  if Assigned(FListBox) then
  begin
    if FListBox.LCLRefCount > 0 then exit;
    if FItems = nil then
      FItems := TStringList.Create;
    FItems.Assign(FListBox.Items);
    FItemIndex := FListBox.ItemIndex;
    FDropDownColor:= FListBox.Color;
    FreeAndNil(FListBox);
  end;
  FreeAndNil(FForm);
end;

function TBCComboBox.GetListBox: TListBox;
begin
  CreateForm;
  result := FListBox;
end;

procedure TBCComboBox.UpdateButtonCanvasScaleMode;
begin
  if (CanvasScaleMode = csmFullResolution) or
     ((CanvasScaleMode = csmAuto) and not Assigned(FOnDrawSelectedItem)) then
     FButton.CanvasScaleMode:= csmFullResolution
     else FButton.CanvasScaleMode:= csmScaleBitmap;
end;

constructor TBCComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TBCButton.Create(Self);
  FButton.Align := alClient;
  FButton.Parent := Self;
  FButton.OnClick := ButtonClick;
  FButton.DropDownArrow := True;
  FButton.OnAfterRenderBCButton := OnAfterRenderButton;
  FFocusBorderColor := clBlack;
  FFocusBorderOpacity := 0;
  UpdateButtonCanvasScaleMode;

  FItems := TStringList.Create;
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
  FTimerCheckFormHide.Interval:= 30;
  FTimerCheckFormHide.OnTimer:= OnTimerCheckFormHide;
end;

destructor TBCComboBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TBCComboBox.Assign(Source: TPersistent);
var
  src: TBCComboBox;
begin
  if Source is TBCComboBox then
  begin
    src := TBCComboBox(Source);
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
  end else
    inherited Assign(Source);
end;

procedure TBCComboBox.Clear;
begin
  Items.Clear;
end;

end.
