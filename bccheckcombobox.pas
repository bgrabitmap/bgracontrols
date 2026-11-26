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
  CheckLst, BGRATheme
  {$ifdef DARWIN}, CocoaAll, CocoaUtils, cocoa_extra{$endif};

type

  { TBCCheckComboBox }

  TBCCheckComboBox = class(TBCStyleCustomControl)
  private
    FButton: TBCButton;
    FCanvasScaleMode: TBCCanvasScaleMode;
    FScaled: boolean;
    FDropDownBorderSize: integer;
    FDropDownCount: integer;
    FDropDownColor: TColor;
    FDropDownFontColor: TColor;
    FDropDownFontHighlight: TColor;
    FDropDownHighlight: TColor;
    FFocusBorderColor: TColor;
    FFocusBorderOpacity: byte;
    FForm: TForm;
    FDropDownHideDate: TDateTime;
    FHoverItem: integer;
    FLastPointerPos: TPoint;
    FMouseDownItem: integer;
    FMouseCheckChanged: boolean;
    FItemHeight: integer;
    FItemPadding: integer;
    FListBox: TCheckListBox;
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
    procedure PaintDropDownBorder(Sender: TObject);
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
    procedure ListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure SetActiveItem(AIndex: Integer);
    procedure ListBoxKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure ListBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
    procedure SetScaled(AValue: boolean);
    procedure SetStateClicked(AValue: TBCButtonState);
    procedure SetStateHover(AValue: TBCButtonState);
    procedure SetStateNormal(AValue: TBCButtonState);
    procedure SetStaticButton(AValue: boolean);
    procedure EnsureListBox;
    procedure UpdateDropDownHeight;
  protected
    procedure TrackListPointer(const ScreenPos: TPoint);
    function GetStyleExtension: String; override;
    procedure WMSetFocus(var {%H-}Message: {$IFDEF FPC}TLMSetFocus{$ELSE}TWMSetFocus{$ENDIF}); message {$IFDEF FPC}LM_SETFOCUS{$ELSE}WM_SETFOCUS{$ENDIF};
    procedure WMKillFocus(var Message: {$IFDEF FPC}TLMKillFocus{$ELSE}TWMKillFocus{$ENDIF}); message {$IFDEF FPC}LM_KILLFOCUS{$ELSE}WM_KILLFOCUS{$ENDIF};
    procedure UpdateFocus(AFocused: boolean);
    procedure KeyDown(var Key: Word; {%H-}Shift: TShiftState); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure CreateForm;
    procedure FreeForm;
    procedure CreateDropDown;
    procedure FreeDropDown;
    function CloseDropDown: boolean;
    function GetListBox: TCheckListBox;
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
    property ListBox: TCheckListBox read GetListBox;
    property Text: string read GetItemText;
    property Checked[AIndex: Integer]: Boolean read GetChecked write SetChecked;
  published
    property Anchors;
    property Canvas: TCanvas read GetComboCanvas;
    property CanvasScaleMode: TBCCanvasScaleMode read FCanvasScaleMode write SetCanvasScaleMode default csmAuto;
    property Scaled: boolean read FScaled write SetScaled default false;
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

uses math, PropEdits, BGRAText, BCTools
  {$ifdef LCLgtk2}, Gtk2, Gtk2Proc, Gtk2Def{$endif}
  {$ifdef LCLgtk3}, LazGtk3, LazGdk3, Gtk3Widgets, Gtk3WSCheckLst, WSLCLClasses{$endif}
  {$ifdef LCLqt5}, Qt5, QtWidgets{$endif};

{$ifdef LCLgtk2}
type
  { Keep the native checklist model and keyboard handling, but let BGRA draw
    the whole row. Reapply after every native handle recreation. }
  TBCGtk2CheckListBox = class(TCheckListBox)
  protected
    procedure CreateWnd; override;
  end;

procedure TBCGtk2CheckListBox.CreateWnd;
var
  Info: PWidgetInfo;
  Tree: PGtkTreeView;
  Column: PGtkTreeViewColumn;
begin
  inherited CreateWnd;
  Info := GetWidgetInfo(Pointer(Handle));
  if (Info = nil) or (Info^.CoreWidget = nil) then Exit;
  if not GTK_IS_TREE_VIEW(Info^.CoreWidget) then Exit;
  // The outer popup already supplies the single-color border.
  gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(Handle), GTK_SHADOW_NONE);
  Tree := PGtkTreeView(Info^.CoreWidget);
  Column := gtk_tree_view_get_column(Tree, 0);
  if Column <> nil then gtk_tree_view_column_set_visible(Column, False);
end;
{$endif}

{$ifdef LCLqt5}
type
  TBCQt5CheckListBox = class(TCheckListBox)
  protected
    procedure CreateWnd; override;
  end;

procedure TBCQt5CheckListBox.CreateWnd;
begin
  inherited CreateWnd;
  // The popup supplies its own border; Qt's extra frame reduces the viewport.
  TQtListWidget(Handle).setFrameShape(QFrameNoFrame);
end;
{$endif}

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCCheckComboBox]);
end;

{ TBCCheckComboBox }

{$ifdef LCLgtk3}
function BCGtk3ClientToScreen(Control: TControl; const P: TPoint): TPoint; forward;
type
  TBCGtk3CheckListBox = class(TCheckListBox)
  protected
    procedure CNKeyDown(var Message: TLMKeyDown); message CN_KEYDOWN;
    class procedure WSRegisterClass; override;
    procedure CreateWnd; override;
  end;

  TBCGtk3DropDownForm = class(TForm)
  public
    KeyList: TBCGtk3CheckListBox;
    AnchorControl: TControl;
    destructor Destroy; override;
  protected
    procedure DoShow; override;
    procedure PositionAtAnchor(Data: PtrInt);
    procedure CNKeyDown(var Message: TLMKeyDown); message CN_KEYDOWN;
  end;

  { Lazarus GTK3 leaves these two listbox methods unimplemented. Register
    implementations only for our subclass; keep the installed LCL untouched. }
  TBCGtk3WSCheckListBox = class(TGtk3WSCustomCheckListBox)
  published
    class function GetIndexAtXY(const ACustomListBox: TCustomListBox; X, Y: integer): integer; override;
    class function GetItemRect(const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect): boolean; override;
  end;

class function TBCGtk3WSCheckListBox.GetIndexAtXY(const ACustomListBox: TCustomListBox; X, Y: integer): integer;
var Tree: PGtkTreeView; Path: PGtkTreePath; BX, BY: Integer;
begin
  Result := -1;
  if not ACustomListBox.HandleAllocated then Exit;
  Tree := PGtkTreeView(TGtk3Widget(ACustomListBox.Handle).GetContainerWidget);
  Tree^.convert_widget_to_bin_window_coords(X,Y,@BX,@BY);
  Path := nil;
  if Tree^.get_path_at_pos(BX,BY,@Path,nil,nil,nil) then
  begin
    if Path^.get_depth > 0 then Result := Path^.get_indices^;
    Path^.free;
  end;
end;

class function TBCGtk3WSCheckListBox.GetItemRect(const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect): boolean;
var Tree: PGtkTreeView; Path: PGtkTreePath; Area: TGdkRectangle; X,Y: Integer;
begin
  Result := False; ARect := Rect(0,0,0,0);
  if not ACustomListBox.HandleAllocated or (Index<0) or (Index>=ACustomListBox.Items.Count) then Exit;
  Tree := PGtkTreeView(TGtk3Widget(ACustomListBox.Handle).GetContainerWidget);
  Path := gtk_tree_path_new_from_indices(Index,[-1]);
  if Path = nil then Exit;
  try
    Tree^.get_background_area(Path,nil,@Area);
    Tree^.convert_bin_window_to_widget_coords(Area.x,Area.y,@X,@Y);
    ARect := Rect(X,Y,X+Area.width,Y+Area.height);
    Result := (Area.width>0) and (Area.height>0);
  finally Path^.free; end;
end;

procedure TBCGtk3DropDownForm.DoShow;
begin
  inherited DoShow;
  // Wait until GTK has mapped and laid out the native popup before anchoring it.
  Application.QueueAsyncCall(PositionAtAnchor,0);
end;

destructor TBCGtk3DropDownForm.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  inherited Destroy;
end;

procedure TBCGtk3DropDownForm.PositionAtAnchor(Data: PtrInt);
var P: TPoint; Native: PGtkWindow;
begin
  if not Visible or not HandleAllocated or not Assigned(AnchorControl) then Exit;
  P := BCGtk3ClientToScreen(AnchorControl,Point(0,AnchorControl.Height));
  SetBounds(P.X,P.Y,Width,Height);
  Native := PGtkWindow(TGtk3Widget(Handle).Widget);
  Native^.set_position(GTK_WIN_POS_NONE);
  Native^.set_gravity(GDK_GRAVITY_STATIC);
  Native^.move(P.X,P.Y);
end;

procedure TBCGtk3DropDownForm.CNKeyDown(var Message: TLMKeyDown);
begin
  // GTK3 also consumes arrows on the toplevel before propagating to its focused
  // list. Forward our popup's navigation to that list at the same entry point.
  if Assigned(KeyList) and KeyList.Visible and
    (Message.CharCode in [VK_UP,VK_DOWN,VK_HOME,VK_END,VK_PRIOR,VK_NEXT,VK_RETURN,VK_ESCAPE]) then
  begin
    KeyList.Perform(CN_KEYDOWN,Message.CharCode,Message.KeyData);
    Message.CharCode := 0;
    Message.Result := 1;
  end
  else inherited CNKeyDown(Message);
end;

procedure TBCGtk3CheckListBox.CNKeyDown(var Message: TLMKeyDown);
var Key: Word;
begin
  // GTK3 consumes arrow keys after CN_KEYDOWN and skips LM_KEYDOWN for lists.
  // Route only our navigation keys through the existing LCL KeyDown handler.
  // Leave Space to the native checklist so it is never toggled twice.
  if Message.CharCode in [VK_UP,VK_DOWN,VK_HOME,VK_END,VK_PRIOR,VK_NEXT,VK_RETURN,VK_ESCAPE] then
  begin
    Key := Message.CharCode;
    KeyDown(Key,KeyDataToShiftState(Message.KeyData));
    Message.CharCode := Key;
    Message.Result := 1;
  end
  else inherited CNKeyDown(Message);
end;

class procedure TBCGtk3CheckListBox.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterWSComponent(TBCGtk3CheckListBox, TBCGtk3WSCheckListBox);
end;

procedure TBCGtk3CheckListBox.CreateWnd;
var
  Native: TGtk3Widget;
  Column: PGtkTreeViewColumn;
begin
  inherited CreateWnd;
  Native := TGtk3Widget(Handle);
  if (Native = nil) or (Native.GetContainerWidget = nil) then Exit;
  PGtkScrolledWindow(Native.Widget)^.set_shadow_type(GTK_SHADOW_NONE);
  Column := PGtkTreeView(Native.GetContainerWidget)^.get_column(0);
  if Column <> nil then Column^.set_visible(False);
end;
{$endif}

{$ifdef LCLgtk3}
function BCGtk3ClientToScreen(Control: TControl; const P: TPoint): TPoint;
var Widget, TopLevel: PGtkWidget; X,Y,RootX,RootY: Integer; Local: TPoint;
begin
  Result := Control.ClientToScreen(P);
  Local := P;
  while not (Control is TWinControl) do
  begin
    Inc(Local.X,Control.Left); Inc(Local.Y,Control.Top);
    Control := Control.Parent;
    if Control = nil then Exit;
  end;
  if not TWinControl(Control).HandleAllocated then Exit;
  Widget := TGtk3Widget(TWinControl(Control).Handle).GetContainerWidget;
  if Widget = nil then Exit;
  TopLevel := Widget^.get_toplevel;
  if (TopLevel = nil) or (TopLevel^.window = nil) then Exit;
  if not Widget^.translate_coordinates(TopLevel,Local.X,Local.Y,@X,@Y) then Exit;
  TopLevel^.window^.get_origin(@RootX,@RootY);
  Result := Point(RootX+X,RootY+Y);
end;
{$endif}

procedure TBCCheckComboBox.ButtonClick(Sender: TObject);
const MinDelayReopen = 500/(1000*60*60*24);
var
  p: TPoint;
  w, h: Integer;

  measure: TBitmap;
  scale: single;

begin
  {$ifdef LINUX}
  // Like TBCComboBox, recreate a hidden popup so GTK does not reuse a stale position.
  // FreeDropDown keeps the checklist and its item/check data.
  if Assigned(FForm) and not FForm.Visible then FreeDropDown;
  {$endif}
  CreateDropDown;

  if not CloseDropDown and (Now > FDropDownHideDate+MinDelayReopen) then
  begin
    // Prepare the list box
    FListBox.Font.Name := Button.StateNormal.FontEx.Name;
    FListBox.Font.Style := Button.StateNormal.FontEx.Style;
    if Scaled then
      scale := Screen.PixelsPerInch / GetDesignTimePPI(Self)
    else
      scale := 1;
    FListBox.Font.Height := Round(FontEmHeightSign*Button.StateNormal.FontEx.Height*scale);
    measure := TBitmap.Create;
    try
      measure.Canvas.Font.Assign(FListBox.Font);
      h := measure.Canvas.GetTextHeight('Hg');
    finally
      measure.Free;
    end;
    {$ifdef LCLgtk2}
    Inc(h, 2); // GTK adds its own cell padding, as in TBCComboBox.
    {$else}
    Inc(h, 6);
    {$endif}
    FListBox.ItemHeight := h;

    // List box size
    w := FButton.Width - 2*FDropDownBorderSize;
    h := (FListBox.ItemHeight + FItemPadding) * min(Items.Count, FDropDownCount);
    {$IFDEF DARWIN}
    inc(h, 20);
    {$ENDIF}
    FListBox.SetBounds(FDropDownBorderSize, FDropDownBorderSize, w, h);

    // Drop-down form size
    w := FListBox.Width + 2*FDropDownBorderSize;
    h := FListBox.Height + 2*FDropDownBorderSize;
    // Set size and anchor together instead of carrying over the native popup position.
    p := ControlToScreen(Point(FButton.Left, FButton.Top + FButton.Height));
    FForm.SetBounds(p.X, p.Y,
      w + FForm.Width - FForm.ClientWidth,
      h + FForm.Height - FForm.ClientHeight);
    FForm.Color := FDropDownBorderColor;

    if Assigned(FOnDropDown) then FOnDropDown(self);
    FQueryDropDownHide := false;
    FLastPointerPos := Mouse.CursorPos;
    if (FListBox.ItemIndex < 0) and (Items.Count > 0) then
      SetActiveItem(0)
    else SetActiveItem(FListBox.ItemIndex);
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

procedure TBCCheckComboBox.PaintDropDownBorder(Sender: TObject);
begin
  if not Assigned(FForm) then Exit;
  FForm.Canvas.Brush.Color := FDropDownBorderColor;
  FForm.Canvas.FillRect(FForm.ClientRect);
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
  EnsureListBox;
  Result := (AIndex >= 0) and (AIndex < FListBox.Items.Count);
  if Result then Result := FListBox.Checked[AIndex];
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
  EnsureListBox;
  Result := FListBox.ItemIndex;
end;

function TBCCheckComboBox.GetItems: TStrings;
begin
  EnsureListBox;
  Result := FListBox.Items;
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
begin
  // Native checkbox clicks and Space arrive here after changing the state.
  FMouseCheckChanged := true;
  FListBox.Invalidate;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TBCCheckComboBox.ListBoxExit(Sender: TObject);
begin
  CloseDropDown;
end;

procedure TBCCheckComboBox.SetActiveItem(AIndex: Integer);
begin
  if FListBox.ItemIndex <> AIndex then FListBox.ItemIndex := AIndex;
  if FHoverItem <> FListBox.ItemIndex then
  begin
    FHoverItem := FListBox.ItemIndex;
    FListBox.Invalidate;
  end;
end;

procedure TBCCheckComboBox.ListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  SetActiveItem(FListBox.ItemIndex);
end;

procedure TBCCheckComboBox.ListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Index, Step: Integer;
begin
  // A stationary pointer must not take priority back after keyboard navigation.
  FLastPointerPos := Mouse.CursorPos;
  Index := FListBox.ItemIndex;
  Step := Max(1, FListBox.ClientHeight div Max(1, FListBox.ItemHeight));
  case Key of
    VK_UP: Dec(Index);
    VK_DOWN: Inc(Index);
    VK_HOME: Index := 0;
    VK_END: Index := FListBox.Items.Count-1;
    VK_PRIOR: Dec(Index, Step);
    VK_NEXT: Inc(Index, Step);
    VK_RETURN, VK_ESCAPE:
      begin
        if Assigned(FForm) then FForm.Visible := false;
        Key := 0;
        Exit;
      end;
    else Exit; // Leave Space to TCheckListBox: one toggle and one OnChange.
  end;
  if FListBox.Items.Count > 0 then
    SetActiveItem(Max(0, Min(Index, FListBox.Items.Count-1)));
  Key := 0;
end;

procedure TBCCheckComboBox.ListBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseDownItem := -1;
  FMouseCheckChanged := false;
  if Button = mbLeft then
  begin
    FMouseDownItem := FListBox.ItemAtPos(Point(X, Y), true);
    if FMouseDownItem >= 0 then SetActiveItem(FMouseDownItem);
  end;
end;

procedure TBCCheckComboBox.ListBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  Index := FMouseDownItem;
  FMouseDownItem := -1;
  // Native checkbox hits already toggled. Also accept clicks on the row text,
  // without toggling a native checkbox a second time.
  if (Button <> mbLeft) or FMouseCheckChanged or (Index < 0)
    or (Index >= FListBox.Items.Count) then Exit;
  if (FListBox.ItemAtPos(Point(X, Y), true) <> Index)
    or not FListBox.ItemEnabled[Index] or FListBox.Header[Index] then Exit;
  FListBox.Checked[Index] := not FListBox.Checked[Index];
  ListBoxClick(FListBox);
end;

procedure TBCCheckComboBox.ListBoxMouseLeave(Sender: TObject);
begin
  // Keep the active row visible: Space still acts on it outside the list.
end;

procedure TBCCheckComboBox.TrackListPointer(const ScreenPos: TPoint);
var
  P: TPoint;
  Index: Integer;
begin
  if (ScreenPos.X = FLastPointerPos.X) and
    (ScreenPos.Y = FLastPointerPos.Y) then Exit;
  FLastPointerPos := ScreenPos;
  {$ifdef LCLgtk3}
  P := BCGtk3ClientToScreen(FListBox,Point(0,0));
  P := Point(ScreenPos.X-P.X,ScreenPos.Y-P.Y);
  {$else}
  P := FListBox.ScreenToClient(ScreenPos);
  {$endif}
  if (P.X < 0) or (P.Y < 0) or (P.X >= FListBox.ClientWidth)
    or (P.Y >= FListBox.ClientHeight) then Exit;
  Index := FListBox.ItemAtPos(P, true);
  if Index >= 0 then SetActiveItem(Index);
end;

procedure TBCCheckComboBox.ListBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  {$ifdef LCLgtk3}
  TrackListPointer(BCGtk3ClientToScreen(FListBox,Point(X,Y)));
  {$else}
  TrackListPointer(FListBox.ClientToScreen(Point(X, Y)));
  {$endif}
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
  if (Index < 0) or (Index >= FListBox.Items.Count) then Exit;
  {$ifdef LCLgtk2}
  if Index = 0 then
    FItemPadding := Max(0, ARect.Height - FListBox.ItemHeight);
  {$endif}
  isChecked := FListBox.Checked[Index];

  if Index = FListBox.ItemIndex then
    itemState := btbsHover
  else
    itemState := btbsNormal;

  parentForm := GetParentForm(Control, False);
  if Assigned(parentForm) then
    lclDPI := parentForm.PixelsPerInch
    else lclDPI := Screen.PixelsPerInch;
  surface := TBGRAThemeSurface.Create(ARect, TCheckListBox(Control).Canvas, Control.GetCanvasScaleFactor, lclDPI);
  try
    DrawCheckBox(FListBox.Items[Index], itemState, False, isChecked, ARect, surface);
  finally
    surface.Free;
  end;
end;

procedure TBCCheckComboBox.DrawCheckBox(aCaption: string; State: TBGRAThemeButtonState;
  aFocused: boolean; Checked: boolean; ARect: TRect; ASurface: TBGRAThemeSurface
  );
var
  Style: TTextStyle;
  bgColor: TBGRAPixel;
  aColor: TBGRAPixel;
  aleft, atop, aright, abottom: integer;
  penWidth: single;
  textLeft: integer;
  textColor: TColor;
begin
  with ASurface do
  begin
    // Determine colors based on state
    case State of
      btbsHover:
      begin
        bgColor := ColorToBGRA(FDropDownHighlight);
        textColor := FDropDownFontHighlight;
      end;
      btbsActive:
      begin
        bgColor := ColorToBGRA(FDropDownHighlight);
        textColor := FDropDownFontHighlight;
      end;
      btbsDisabled:
      begin
        bgColor := ColorToBGRA(FDropDownColor);
        textColor := clGray;
      end;
      else {btbsNormal}
      begin
        bgColor := ColorToBGRA(FDropDownColor);
        textColor := FDropDownFontColor;
      end;
    end;

    // Follow the text color so checks remain visible on dark/highlight surfaces.
    aColor := ColorToBGRA(ColorToRGB(textColor));
    DestCanvas.Font.Color := textColor;

    // Fill background with appropriate color
    Bitmap.Fill(bgColor);
    BitmapRect := ARect;
    textLeft := ARect.Left + ARect.Height;
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
        textLeft, ARect.Top, aCaption, Style);
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
  {$ifdef LCLgtk2}
  // The first draw reports GTK's actual cell padding; resize outside painting.
  UpdateDropDownHeight;
  {$endif}
  {$ifdef DARWIN}
  // Cocoa 3.8 table views do not forward mouseMoved to the LCL. Reuse the
  // popup timer for hover tracking rather than changing the installed LCL.
  if Assigned(FForm) and FForm.Visible then
    TrackListPointer(Mouse.CursorPos);
  {$endif}
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

procedure TBCCheckComboBox.UpdateDropDownHeight;
var H: Integer;
begin
  if not Assigned(FForm) or not FForm.Visible or not Assigned(FListBox) then Exit;
  H := (FListBox.ItemHeight + FItemPadding) * Min(Items.Count, FDropDownCount);
  if FListBox.Height = H then Exit;
  FListBox.Height := H;
  FForm.ClientHeight := H + 2*FDropDownBorderSize;
end;

procedure TBCCheckComboBox.SetScaled(AValue: boolean);
begin
  if FScaled = AValue then Exit;
  FScaled := AValue;
  if Assigned(FButton) then
  begin
    FButton.Scaled := AValue;
    FButton.UpdateControl;
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
  EnsureListBox;
  if (AIndex >= 0) and (AIndex < FListBox.Items.Count) then
  begin
    FListBox.Checked[AIndex] := AValue;
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
  EnsureListBox;
  if AValue <> FListBox.ItemIndex then
  begin
    SetActiveItem(AValue);
    Button.Caption := GetItemText;
  end;
end;

procedure TBCCheckComboBox.SetItems(AValue: TStrings);
begin
  if AValue <> Items then Items.Assign(AValue);
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

procedure TBCCheckComboBox.EnsureListBox;
begin
  if Assigned(FListBox) then Exit;
  {$if defined(LCLgtk2)}
  FListBox := TBCGtk2CheckListBox.Create(Self);
  {$elseif defined(LCLgtk3)}
  FListBox := TBCGtk3CheckListBox.Create(Self);
  {$elseif defined(LCLqt5)}
  FListBox := TBCQt5CheckListBox.Create(Self);
  {$else}
  FListBox := TCheckListBox.Create(Self);
  {$endif}
  FListBox.BorderStyle := bsNone;
  FListBox.OnClickCheck := ListBoxClick;
  FListBox.OnMouseDown := ListBoxMouseDown;
  FListBox.OnMouseUp := ListBoxMouseUp;
  FListBox.OnMouseLeave := ListBoxMouseLeave;
  FListBox.OnMouseMove := ListBoxMouseMove;
  FListBox.OnExit := ListBoxExit;
  FListBox.OnSelectionChange := ListBoxSelectionChange;
  FListBox.Style := lbOwnerDrawFixed;
  FListBox.OnDrawItem := ListBoxDrawItem;
  FListBox.Options := [];
  FListBox.OnKeyDown := ListBoxKeyDown;
  FListBox.Color := FDropDownColor;
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
  CreateDropDown;
end;

procedure TBCCheckComboBox.FreeForm;
begin
  FreeDropDown;
end;

procedure TBCCheckComboBox.CreateDropDown;
begin
  EnsureListBox;
  if FForm = nil then
  begin
    {$ifdef LCLgtk3}
    FForm := TBCGtk3DropDownForm.CreateNew(Self);
    TBCGtk3DropDownForm(FForm).KeyList := TBCGtk3CheckListBox(FListBox);
    TBCGtk3DropDownForm(FForm).AnchorControl := FButton;
    {$else}
    FForm := TForm.Create(Self);
    {$endif}
    FForm.Visible := False;
    FForm.ShowInTaskBar := stNever;
    FForm.BorderStyle := bsNone;
    FForm.OnDeactivate := FormDeactivate;
    FForm.OnHide := FormHide;
    {$ifdef LCLgtk3}FForm.OnPaint := PaintDropDownBorder;{$endif}
    FForm.FormStyle := fsStayOnTop;
  end;
  if FListBox.Parent <> FForm then FListBox.Parent := FForm;
end;

procedure TBCCheckComboBox.FreeDropDown;
begin
  if Assigned(FListBox) then
  begin
    if FListBox.LCLRefCount > 0 then Exit;
    // Keep the item/check data in the checklist cache when its window is freed.
    FListBox.Parent := nil;
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

function TBCCheckComboBox.GetListBox: TCheckListBox;
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

  FHoverItem := -1;
  FMouseDownItem := -1;
  {$ifdef LCLgtk2}
  FItemPadding := 4; // Initial GTK cell margin, refined by the first row draw.
  {$endif}

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
  if Assigned(FTimerCheckFormHide) then FTimerCheckFormHide.Enabled := False;
  if Assigned(FListBox) then
  begin
    FListBox.OnExit := nil;
    FListBox.OnSelectionChange := nil;
    FListBox.OnClickCheck := nil;
    FListBox.OnMouseDown := nil;
    FListBox.OnMouseUp := nil;
    FListBox.OnDrawItem := nil;
    FListBox.OnKeyDown := nil;
    FListBox.OnMouseMove := nil;
    FListBox.OnMouseLeave := nil;
  end;
  if Assigned(FForm) then
  begin
    FForm.OnDeactivate := nil;
    FForm.OnHide := nil;
  end;
  // Release the separate popup before destroying its owner control.
  FreeDropDown;
  inherited Destroy;
end;

procedure TBCCheckComboBox.Assign(Source: TPersistent);
var
  src: TBCCheckComboBox;
  i: Integer;
begin
  if Source is TBCCheckComboBox then
  begin
    if Source = Self then Exit;
    src := TBCCheckComboBox(Source);
    Button.Assign(src.Button);
    Scaled := src.Scaled;
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
    for i := 0 to Items.Count - 1 do
      FListBox.State[i] := src.FListBox.State[i];
  end else
    inherited Assign(Source);
end;

procedure TBCCheckComboBox.Clear;
begin
  Items.Clear;
end;

procedure TBCCheckComboBox.CheckAll;
begin
  EnsureListBox;
  FListBox.CheckAll(cbChecked);
  FListBox.Invalidate;
end;

procedure TBCCheckComboBox.UnCheckAll;
begin
  EnsureListBox;
  FListBox.CheckAll(cbUnchecked);
  FListBox.Invalidate;
end;


end.
