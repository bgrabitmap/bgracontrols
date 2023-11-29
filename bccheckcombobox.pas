unit BCCheckComboBox;

{$mode delphi}

interface

uses
  {$ifdef WINDOWS}Windows,{$endif} Classes, SysUtils, LResources, Forms, Controls, ExtCtrls, Graphics, Dialogs, BCButton,
  StdCtrls, BCTypes, BCBaseCtrls, BGRABitmap, BGRABitmapTypes, LMessages, LCLType,
  CheckLst, BGRATheme;

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
    FItemIndex: integer;
    FForm: TForm;
    FFormHideDate: TDateTime;
    FHoverItem: integer;
    FItemHeight: integer;
    FListBox: TCheckListBox;
    FDropDownBorderColor: TColor;
    FOnDrawItem: TDrawItemEvent;
    FOnDrawSelectedItem: TOnAfterRenderBCButton;
    FOnChange: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FDrawingDropDown: boolean;
    FTimerCheckFormHide: TTimer;
    FQueryFormHide: boolean;
    procedure ButtonClick(Sender: TObject);
    procedure DrawCheckBox(aCaption: string; State: TBGRAThemeButtonState;
      aFocused: boolean; Checked: boolean; ARect: TRect;
      ASurface: TBGRAThemeSurface);
    procedure FormDeactivate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    function GetArrowFlip: boolean;
    function GetCaption: String;
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
    procedure ListBoxKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState
      );
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
    procedure SetCanvasScaleMode(AValue: TBCCanvasScaleMode);
    procedure SetCaption(AValue: String);
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
  protected
    function GetStyleExtension: String; override;
    procedure WMSetFocus(var {%H-}Message: {$IFDEF FPC}TLMSetFocus{$ELSE}TWMSetFocus{$ENDIF}); message {$IFDEF FPC}LM_SETFOCUS{$ELSE}WM_SETFOCUS{$ENDIF};
    procedure WMKillFocus(var Message: {$IFDEF FPC}TLMKillFocus{$ELSE}TWMKillFocus{$ENDIF}); message {$IFDEF FPC}LM_KILLFOCUS{$ELSE}WM_KILLFOCUS{$ENDIF};
    procedure UpdateFocus(AFocused: boolean);
    procedure KeyDown(var Key: Word; {%H-}Shift: TShiftState); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure CreateForm;
    procedure FreeForm;
    function GetListBox: TCheckListBox;
    procedure UpdateButtonCanvasScaleMode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Assign the properties from Source to this instance }
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    property HoverItem: integer read FHoverItem;
    property Button: TBCButton read FButton write FButton;
    property ListBox: TCheckListBox read GetListBox;
    property Text: string read GetItemText;
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
  h: Integer;
  s: TSize;
begin
  {$IFDEF DARWIN}
  if Assigned(FForm) and not FForm.Visible then FreeForm;
  {$ENDIF}

  CreateForm;

  if FForm.Visible then
    FForm.Visible := false
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
    FForm.Visible := True;
    if FListBox.CanSetFocus then
      FListBox.SetFocus;
    FTimerCheckFormHide.Enabled:= true;
    FQueryFormHide := false;
  end;
end;

procedure TBCCheckComboBox.FormDeactivate(Sender: TObject);
begin
  FQueryFormHide := true;
end;

procedure TBCCheckComboBox.FormHide(Sender: TObject);
begin
  FFormHideDate := Now;
end;

function TBCCheckComboBox.GetArrowFlip: boolean;
begin
  result := Button.FlipArrow;
end;

function TBCCheckComboBox.GetCaption: String;
begin
  Result := Button.Caption;
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
      if FItemIndex >= Items.Count then
        FItemIndex := -1;
      result := FItemIndex;
    end;
end;

function TBCCheckComboBox.GetItems: TStrings;
begin
  if Assigned(FListBox) then
    Result := FListBox.Items
    else Result := FItems;
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

procedure TBCCheckComboBox.ListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) or (Key = VK_ESCAPE) then
  begin
    ButtonClick(nil);
    Key := 0;
  end;
end;

procedure TBCCheckComboBox.ListBoxMouseUp(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer);
begin
  FQueryFormHide := true;
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
    if (FHoverItem<>-1) and ([ssLeft,ssRight]*Shift <> []) then
      FListBox.ItemIndex := FHoverItem;
    FListBox.Repaint;
  end;
end;

procedure TBCCheckComboBox.ListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  Button.Caption := GetItemText;
  if User and Assigned(FOnChange) then FOnChange(self);
end;

procedure TBCCheckComboBox.ListBoxDrawItem(Control: TWinControl; Index: Integer;
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
  surface := TBGRAThemeSurface.Create(ARect, TCheckListBox(Control).Canvas, Control.GetCanvasScaleFactor, lclDPI);
  try
    DrawCheckBox(TCheckListBox(Control).Items[Index], btbsNormal, False, TCheckListBox(Control).Checked[Index], ARect, surface);
  finally
    surface.Free;
  end;
end;

procedure TBCCheckComboBox.DrawCheckBox(aCaption: string; State: TBGRAThemeButtonState;
  aFocused: boolean; Checked: boolean; ARect: TRect; ASurface: TBGRAThemeSurface
  );
var
  Style: TTextStyle;
  aColor: TBGRAPixel;
  aleft, atop, aright, abottom: integer;
  penWidth: single;
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
        Color, penWidth*1.5);
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
     FQueryFormHide) then
  begin
    FForm.Visible := false;
    FQueryFormHide := false;
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

procedure TBCCheckComboBox.SetDropDownColor(AValue: TColor);
begin
  if Assigned(FListBox) then
    FListBox.Color := AValue
    else FDropDownColor:= AValue;
end;

procedure TBCCheckComboBox.SetGlobalOpacity(AValue: byte);
begin
  Button.GlobalOpacity := AValue;
end;

procedure TBCCheckComboBox.SetItemIndex(AValue: integer);
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

procedure TBCCheckComboBox.SetItems(AValue: TStrings);
begin
  if Assigned(FListBox) then
    FListBox.Items.Assign(AValue)
    else FItems.Assign(AValue);
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

function TBCCheckComboBox.GetStyleExtension: String;
begin
  result := 'bccombo';
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
  oldCaption: string;
begin
  lForm := GetParentForm(Self);
  if lForm = nil then
    exit;

  {$IFDEF FPC}//#
  if AFocused then
    ActiveDefaultControlChanged(lForm.ActiveControl)
  else
    ActiveDefaultControlChanged(nil);
  {$ENDIF}

  oldCaption := FButton.Caption;
  FButton.Caption := FButton.Caption + '1';
  FButton.Caption := oldCaption;

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

procedure TBCCheckComboBox.UTF8KeyPress(var UTF8Key: TUTF8Char);
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

procedure TBCCheckComboBox.CreateForm;
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
    FListBox := TCheckListBox.Create(self);
    FListBox.Parent := FForm;
    FListBox.BorderStyle := bsNone;
    //FListBox.OnSelectionChange := ListBoxSelectionChange;
    FListBox.OnMouseLeave:=ListBoxMouseLeave;
    FListBox.OnMouseMove:=ListBoxMouseMove;
    //FListBox.OnMouseUp:= ListBoxMouseUp;
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
  end;
end;

procedure TBCCheckComboBox.FreeForm;
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

function TBCCheckComboBox.GetListBox: TCheckListBox;
begin
  CreateForm;
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

destructor TBCCheckComboBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TBCCheckComboBox.Assign(Source: TPersistent);
var
  src: TBCCheckComboBox;
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
  end else
    inherited Assign(Source);
end;

procedure TBCCheckComboBox.Clear;
begin
  Items.Clear;
end;

end.
