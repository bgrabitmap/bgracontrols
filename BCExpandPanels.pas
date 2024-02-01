{
********************************************************************************
*                         BGRAExpandPanels   Version 1.0                       *
*                                                                              *
*                                                                              *
*   (c)  Massimo Magnano, Alexander Roth                                       *
*                                                                              *
*                                                                              *
********************************************************************************

See BcExpandPanels.txt for changelog and to-do
}

unit BCExpandPanels;


{$mode objfpc}{$H+}

// for debugging purposes
//{$DEFINE DEBUG_PAINT}

interface

uses
  Controls, Classes, ExtCtrls, Graphics, Math, LResources, Dialogs, SysUtils,
  Buttons, Themes, Types, Menus, BCPanel;

type
  TBCExpandPanelsBehaviour = (EPHotMouse, EPMultipanel, EPSinglePanel);
  //  TBoundEvent=procedure(sender:TObject; ALeft, ATop, AWidth, AHeight: integer) of object;
  TAnimationEvent = procedure(Sender: TObject; deltaLeft, deltaTop, deltaWidth, deltaHeight: integer) of object;
  TNormalProcedure = procedure of object;


  { TBCBoundButton }

  TGlyphLayout =
  (
    glLeft,
    glRight,
    glNone
  );

  TGlyphKind =
  (
    gkArrows,
    gkClose,
    gkMinMax
  );

  TTextLayout =
  (
    tlLeft,
    tlRight,
    tlCenter,
    tlNone
  );

  TBCBoundButtonStyle = (bbsButton, bbsTab, bbsLine, bbsLineDouble,
                       bbsLineTop, bbsLineBottom, bbsLineDoubleTop, bbsLineDoubleBottom);

  TBCBoundButton = class(TCustomSpeedButton)
  private
    rColorExpanded: TColor;
    rColorHighlight: TColor;
    rColorShadow: TColor;
    rGlyphKind: TGlyphKind;
    rGlyphLayout: TGlyphLayout;
    rStyle: TBCBoundButtonStyle;
    rTabWidth: Integer;
    rTextLayout: TTextLayout;

    procedure setColorExpanded(AValue: TColor);
    procedure SetColorHighlight(AValue: TColor);
    procedure SetColorShadow(AValue: TColor);
    procedure SetGlyphKind(AValue: TGlyphKind);
    procedure SetGlyphLayout(AValue: TGlyphLayout);
    procedure SetStyle(AValue: TBCBoundButtonStyle);
    procedure SetTabWidth(AValue: Integer);
    procedure SetTextLayout(AValue: TTextLayout);

  protected
    rGlyph :TButtonGlyph;
    rUserGlyphExpanded,
    rUserGlyphCollapsed,
    rGlyphExpanded,
    rGlyphCollapsed :TBitmap;

    procedure SetGlyphCollapsed(AValue: TBitmap);
    procedure SetGlyphExpanded(AValue: TBitmap);
    procedure LoadGlyph(GlyphDST :TBitmap; ResName :String);
    procedure BuildGlyphs;
    procedure Paint; override;
    procedure Loaded; override;

  (*  property AllowAllUp;
    property Down;
    property Glyph;
    property GroupIndex;
    property Height;            //Don't Decrease visibility :-O
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Layout;
    property Left;
    property Margin;
    property Name;
    property NumGlyphs;
    property Spacing;
    property ShowCaption;
    property Tag;
    property Top;
    property Width;
    property Transparent;
    *)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Caption;
    property Color nodefault;
    property ColorExpanded: TColor read rColorExpanded write setColorExpanded;
    property ColorHighlight: TColor read rColorHighlight write SetColorHighlight default clDefault;
    property ColorShadow: TColor read rColorShadow write SetColorShadow default clDefault;
    property Font;
    property Flat;
    property GlyphExpanded: TBitmap read rUserGlyphExpanded write SetGlyphExpanded;
    property GlyphCollapsed: TBitmap read rUserGlyphCollapsed write SetGlyphCollapsed;

    property GlyphLayout: TGlyphLayout read rGlyphLayout write SetGlyphLayout default glNone;
    property GlyphKind: TGlyphKind read rGlyphKind write SetGlyphKind default gkArrows;

    property ShowAccelChar;
    property TextLayout: TTextLayout read rTextLayout write SetTextLayout default tlLeft;
    property Style: TBCBoundButtonStyle read rStyle write SetStyle default bbsButton;

    //Negative Values is the % of Total Width, Positive is a Fixed Width
    property TabWidth: Integer read rTabWidth write SetTabWidth default -50;
  end;

  { TBCExpandPanel }

  TBCExpandPanel = class(TBCPanel)
  private
    FEPManagesCollapsing: TNotifyEvent;
    FButton:      TBCBoundButton;
    FButtonSize:  integer;
    FCollapseKind: TAnchorKind;
    FCollapsed:   boolean;
    FAnimated:    boolean;
    FOnExpand:    TNotifyEvent;
    FOnPreExpand: TNotifyEvent;
    FOnAnimate:   TAnimationEvent;
    FOnCollapse:  TNotifyEvent;
    FOnPreCollapse: TNotifyEvent;
    FOnButtonClick: TNotifyEvent;
    FInternalOnAnimate: TAnimationEvent;
    FButtonPosition: TAnchorKind;
    FExpandedButtonColor: TColor;
    FCollapsedButtonColor: TColor;
    FExpandedSize: integer;
    FAnimationSpeed: real;
    FTextAlignment: TAlignment;
    rBevelColorHighlight: TColor;
    rBevelColorShadow: TColor;
    rBevelRounded: Boolean;
    StopCircleActions: boolean;
    FAnimating:   boolean;
    FVisibleTotal: boolean;

    TargetAnimationSize:     integer;
    EndProcedureOfAnimation: TNormalProcedure;

    Timer: TTimer;

    function GetEnabled: Boolean;
    procedure SetBevelColorHighlight(AValue: TColor);
    procedure SetBevelColorShadow(AValue: TColor);
    procedure SetBevelRounded(AValue: Boolean);
    procedure SetEnabled(AValue: Boolean);
    procedure setExpandedSize(Value: integer);
    procedure setButtonSize(Value: integer);

    procedure setButtonPosition(Value: TAnchorKind);
    procedure setCollapseKind(Value: TAnchorKind);
    procedure setAnimationSpeed(Value: real);
    procedure setCollapsed(Value: boolean);

    procedure PositionButton;

    procedure SetRelevantSize(comp: TControl; AKind: TAnchorKind; ASize: Integer);
    function RelevantSize(comp: TControl; akind: TAnchorKind): integer;
    function RelevantOrthogonalSize(comp: TControl; akind: TAnchorKind): integer;
    function DeltaCoordinates(deltaMove, deltaSize: integer): TRect;  // the outpot (left,top right, bottom) has all the information: left and top encode the movement. rigth and bottom the size changes


    procedure Animate(aTargetSize: integer);
    procedure SetTextAlignment(AValue: TAlignment);

    procedure TimerAnimateSize(Sender: TObject);
    procedure EndTimerCollapse;
    procedure EndTimerExpand;
    procedure UpdateAll;

    procedure ButtonClick(Sender: TObject);
    procedure DoCollapse;
    procedure DoExpand;
    procedure AdjustClientRect(var ARect: TRect); override;

    property InternalOnAnimate: TAnimationEvent read FInternalOnAnimate write FInternalOnAnimate;
    property EPManagesCollapsing: TNotifyEvent read FEPManagesCollapsing write FEPManagesCollapsing;
  protected
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure Paint; override;
  public
    property Animating: boolean read FAnimating;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
  published
    property TextAlignment: TAlignment read FTextAlignment write SetTextAlignment;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property CollapseKind: TAnchorKind read FCollapseKind write setCollapseKind;   //To where should it collapse?
    property ExpandedSize: integer read FExpandedSize write setExpandedSize;
    property ButtonPosition: TAnchorKind read FButtonPosition write setButtonPosition;
    property ButtonSize: integer read FButtonSize write setButtonSize;

    property Button: TBCBoundButton read FButton;

    property AnimationSpeed: real read FAnimationSpeed write setAnimationSpeed;
    property Animated: boolean read FAnimated write FAnimated default True;
    property Collapsed: boolean read FCollapsed write setCollapsed default False;

    property BevelColorHighlight: TColor read rBevelColorHighlight write SetBevelColorHighlight default clBtnHighlight;
    property BevelColorShadow: TColor read rBevelColorShadow write SetBevelColorShadow default clBtnShadow;
    property BevelRounded: Boolean read rBevelRounded write SetBevelRounded default True;

    property OnAnimate: TAnimationEvent read FOnAnimate write FOnAnimate;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property OnPreExpand: TNotifyEvent read FOnPreExpand write FOnPreExpand;
    property OnExpand: TNotifyEvent read FOnExpand write FOnExpand;
    property OnCollapse: TNotifyEvent read FOnCollapse write FOnCollapse;
    property OnPreCollapse: TNotifyEvent read FOnPreCollapse write FOnPreCollapse;
  end;




  {==============================================================================
   Class:   TBCExpandPanels
   Description:
  ==============================================================================}

  { TBCExpandPanels }

  TBCExpandPanels = class(TComponent)
  private
    { Private-Deklarationen }
    PanelArray: TList;

    // Properties
    FArrangeKind: TAnchorKind;
    FButtonPosition, FCollapseKind: TAnchorKind;
    FButtonGlyphKind: TGlyphKind;
    FButtonGlyphLayout: TGlyphLayout;
    FButtonStyle: TBCBoundButtonStyle;
    FButtonTabWidth: Integer;
    FButtonTextLayout: TTextLayout;
    FOrthogonalAbove: integer;
    FAbove:     integer;
    FOrthogonalSize: integer;
    FBehaviour: TBCExpandPanelsBehaviour;
    FOnArrangePanels: TNotifyEvent;
    FFixedSize: integer;
    FUseFixedSize: boolean;
    FAutoCollapseIfTooHigh: boolean;

    FUseClientSize: boolean;

    function RelevantAbove(comp: TControl): integer;
    function RelevantOrthogonalAbove(comp: TControl): integer;
    function RelevantSize(comp: TControl): integer;
    function RelevantOrthogonalSize(comp: TControl): integer;
    procedure setButtonGlyphKind(AValue: TGlyphKind);
    procedure setButtonGlyphLayout(AValue: TGlyphLayout);
    procedure setButtonStyle(AValue: TBCBoundButtonStyle);
    procedure SetButtonTabWidth(AValue: Integer);
    procedure setButtonTextLayout(AValue: TTextLayout);
    procedure WriteRelevantAbove(comp: TBCExpandPanel; above: integer);
    procedure WriteRelevantSize(comp: TBCExpandPanel; size: integer);
    procedure WriteRelevantOrthogonalSize(comp: TBCExpandPanel; size: integer);
    procedure WriteRelevantOrthogonalAbove(comp: TBCExpandPanel; size: integer);

    procedure setArrangeKind(Value: TAnchorKind);
    procedure setButtonPosition(Value: TAnchorKind);
    procedure setCollapseKind(Value: TAnchorKind);
    procedure setUseClientSize(Value: boolean);
    procedure setUseFixedSize(Value: boolean);
    procedure setAutoCollapseIfTooHigh(Value: boolean);
    procedure setFixedSize(Value: integer);
    procedure setOrthogonalAbove(Value: integer);
    procedure setAbove(Value: integer);
    procedure setOrthogonalSize(Value: integer);
    procedure setBehaviour(Value: TBCExpandPanelsBehaviour);

    procedure MakeCorrectButtonClickPointers;

    procedure RollOutOnAnimate(Sender: TObject; deltaLeft, deltaTop, deltaWidth, deltaHeight: integer);

    procedure RollOutClick(Sender: TObject);
    procedure HotTrackSetActivePanel(Value: integer);
    procedure DelLastPanel;

    procedure RollOut1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  protected
    { Protected-Deklarationen }
  public
    { Public-Deklarationen }

    property OrthogonalAbove: integer read FOrthogonalAbove write setOrthogonalAbove;
    property Above: integer read FAbove write setAbove;
    property OrthogonalSize: integer read FOrthogonalSize write setOrthogonalSize;

    function IdxOfPanel(aname: string): integer; overload;

    procedure CollapseIfTooHigh;
    //    procedure SetCorrectSize;
    procedure AddPanel(rollout: TBCExpandPanel);
    procedure InsertPanel(idx: integer; rollout: TBCExpandPanel);
    function DeltePanel(aname: string): boolean; overload;
    function DeltePanel(idx: integer): boolean; overload;
    procedure DelteLastPanel;
    procedure ArrangePanels;
    function Count: integer;
    function Panel(idx: integer): TBCExpandPanel;

    property CollapseKind: TAnchorKind read FCollapseKind write setCollapseKind;
    property ButtonPosition: TAnchorKind read FButtonPosition write setButtonPosition;
    property ButtonGlyphLayout: TGlyphLayout read FButtonGlyphLayout write setButtonGlyphLayout;
    property ButtonGlyphKind: TGlyphKind read FButtonGlyphKind write setButtonGlyphKind;
    property ButtonStyle: TBCBoundButtonStyle read FButtonStyle write setButtonStyle;
    property ButtonTabWidth: Integer read FButtonTabWidth write SetButtonTabWidth;
    property ButtonTextLayout: TTextLayout read FButtonTextLayout write setButtonTextLayout;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published-Deklarationen }

    //    property FixedHeight:integer read FFixedHeight write setFixedSize;
    //    property UseFixedHeight:boolean read FUseFixedHeight write setUseFixedSize;
    //    property UseClientHeight:boolean read FUseClientHeight write setUseClientSize;
    //    property AutoCollapseIfTooHigh:boolean read FAutoCollapseIfTooHigh write setAutoCollapseIfTooHigh;
    property ArrangeKind: TAnchorKind read FArrangeKind write setArrangeKind;
    property OnArrangePanels: TNotifyEvent read FOnArrangePanels write FOnArrangePanels;
    property Behaviour: TBCExpandPanelsBehaviour read FBehaviour write setBehaviour;
  end;

procedure Register;

implementation

uses GraphType, LCLProc;

const
  //GrayScale a Color : Taken from BGRABitmap package
  redWeightShl10   = 306; // = 0.299
  greenWeightShl10 = 601; // = 0.587
  blueWeightShl10  = 117; // = 0.114


procedure korrigiere(var w: real; min, max: real);
var
  temp: real;
begin
  if max < min then
    begin
    temp := min;
    min  := max;
    max  := temp;
    end;

  if w < min then
    w := min;
  if w > max then
    w := max;
end;


//Function copied from BGRABitmap package may work ;-)
function Grayscale(AColor :TColor):TColor;
Var
  rColor, gray :Integer;

begin
  rColor :=ColorToRGB(AColor);
  gray  := (Red(rColor) * redWeightShl10 + Green(rColor) * greenWeightShl10 + Blue(rColor) * blueWeightShl10 + 512) shr 10;
  Result :=RGBToColor(gray, gray, gray);
end;

function GetHighlightColor(BaseColor: TColor; Value:Integer): TColor;
Var
  rColor :Integer;

begin
  rColor :=ColorToRGB(BaseColor);
  Result := RGBToColor(
       Min(Red(rColor) + Value, $FF),
       Min(Green(rColor) + Value, $FF),
       Min(Blue(rColor) + Value, $FF));
end;

function GetShadowColor(BaseColor: TColor; Value:Integer): TColor;
Var
  rColor :Integer;

begin
  rColor :=ColorToRGB(BaseColor);
  Result := RGBToColor(
       Max(Red(rColor) - Value, $22),
       Max(Green(rColor) - Value, $22),
       Max(Blue(rColor) - Value, $22));
end;

//Canvas Draw Functions
procedure Frame3d_Rounded(Canvas: TCanvas;
                          var ARect: TRect; const FrameWidth : integer; RX, RY:Integer;
                          const Style : TGraphicsBevelCut;
                          ShadowColor, HighlightColor, InternalColor: TColor);
var
   DRect: TRect;

   procedure drawUP;
   begin
     inc(DRect.Left,1); inc(DRect.Top,1);

     //is outside the Rect but in this way we don't have a hole of 1 px
     inc(DRect.Right,1); inc(DRect.Bottom,1);

     Canvas.Brush.Color :=ShadowColor;
     Canvas.Brush.Style :=bsSolid;
     Canvas.Pen.Color := clNone;
     Canvas.Pen.Width := 1;         //The Shadow is always 1 Pixel
     Canvas.Pen.Style := psClear;
     Canvas.RoundRect(DRect, RX,RY);

     dec(DRect.Left,1); dec(DRect.Top,1);
     dec(DRect.Right,2); dec(DRect.Bottom,2);
     Canvas.Brush.Color :=InternalColor;

     if (InternalColor = clNone)
     then Canvas.Brush.Style :=bsClear
     else Canvas.Brush.Style :=bsSolid;

     Canvas.Pen.Color :=HighlightColor;
     Canvas.Pen.Width := FrameWidth;
     Canvas.Pen.Style := psSolid;
     Canvas.RoundRect(DRect, RX,RY);

     Inc(ARect.Top, FrameWidth);
     Inc(ARect.Left, FrameWidth);
     Dec(ARect.Right, FrameWidth+1); //+The Shadow (1 Pixel) +1?
     Dec(ARect.Bottom, FrameWidth+1);
   end;

   procedure drawFLAT;
   begin
     Canvas.Brush.Color := InternalColor;

     if (InternalColor = clNone)
     then Canvas.Brush.Style :=bsClear
     else Canvas.Brush.Style :=bsSolid;

     Canvas.Pen.Color := clNone;
     Canvas.Pen.Width := FrameWidth;
     Canvas.Pen.Style := psClear;
     Canvas.RoundRect(DRect, RX,RY);
   end;

   procedure drawDOWN;
   begin
     Canvas.Brush.Color :=ShadowColor;
     Canvas.Brush.Style :=bsSolid;
     Canvas.Pen.Color := clNone;
     Canvas.Pen.Width := 1;
     Canvas.Pen.Style := psClear;
     Canvas.RoundRect(DRect, RX,RY);

     inc(DRect.Left,1); inc(DRect.Top,1);
     Canvas.Brush.Color :=InternalColor;

     if (InternalColor = clNone)
     then Canvas.Brush.Style :=bsClear
     else Canvas.Brush.Style :=bsSolid;

     Canvas.Pen.Color :=HighlightColor;
     Canvas.Pen.Width := FrameWidth;
     Canvas.Pen.Style := psSolid;
     Canvas.RoundRect(DRect, RX,RY);

     Inc(ARect.Top, FrameWidth+1); //+The Shadow (1 Pixel)
     Inc(ARect.Left, FrameWidth+1);
     Dec(ARect.Right, FrameWidth);
     Dec(ARect.Bottom, FrameWidth);
   end;

begin
     DRect :=ARect;
     Case Style of
     bvNone: drawFLAT;
     bvSpace: begin
                drawFLAT;
                InflateRect(ARect, -FrameWidth, -FrameWidth);
              end;
     bvRaised: drawUP;
     bvLowered: drawDOWN;
     end;
end;

procedure TBCBoundButton.SetColorHighlight(AValue: TColor);
begin
  if (rColorHighlight <> AValue) then
  begin
       rColorHighlight := AValue;

       if not(csLoading in ComponentState)
       then Invalidate;
  end;
end;

procedure TBCBoundButton.setColorExpanded(AValue: TColor);
begin
  if (rColorExpanded <> AValue) then
  begin
       rColorExpanded := AValue;

       if not(csLoading in ComponentState)
       then Invalidate;
  end;
end;

procedure TBCBoundButton.SetColorShadow(AValue: TColor);
begin
  if (rColorShadow <> AValue) then
  begin
       rColorShadow := AValue;

       if not(csLoading in ComponentState)
       then Invalidate;
  end;
end;

procedure TBCBoundButton.SetGlyphKind(AValue: TGlyphKind);
begin
  if (rGlyphKind <> AValue) then
  begin
       rGlyphKind:=AValue;

       if not(csLoading in ComponentState) then
       begin
            BuildGlyphs;
            Invalidate;
        end;
  end;
end;

procedure TBCBoundButton.SetGlyphLayout(AValue: TGlyphLayout);
begin
  if (rGlyphLayout <> AValue) then
  begin
       rGlyphLayout := AValue;

       if not(csLoading in ComponentState) then
       begin
            BuildGlyphs;
            Invalidate;
        end;
  end;
end;

procedure TBCBoundButton.SetStyle(AValue: TBCBoundButtonStyle);
begin
  if (rStyle <> AValue) then
  begin
       rStyle:=AValue;
       if not(csLoading in ComponentState)
       then Invalidate;
  end;
end;

procedure TBCBoundButton.SetTabWidth(AValue: Integer);
begin
  if (rTabWidth <> AValue) then
  begin
       rTabWidth:=AValue;
       if not(csLoading in ComponentState) and (rStyle = bbsTab)
       then Invalidate;
  end;
end;

procedure TBCBoundButton.SetTextLayout(AValue: TTextLayout);
begin
  if (rTextLayout <> AValue) then
  begin
       rTextLayout := AValue;
       if not(csLoading in ComponentState)
       then Invalidate;
  end;
end;

procedure TBCBoundButton.SetGlyphCollapsed(AValue: TBitmap);
begin
     rUserGlyphCollapsed.Assign(AValue);
     if not(csLoading in ComponentState) then
     begin
          BuildGlyphs;
          Invalidate;
      end;
end;

procedure TBCBoundButton.SetGlyphExpanded(AValue: TBitmap);
begin
     rUserGlyphExpanded.Assign(AValue);
     if not(csLoading in ComponentState) then
     begin
          BuildGlyphs;
          Invalidate;
      end;
end;

procedure TBCBoundButton.LoadGlyph(GlyphDST: TBitmap; ResName: String);
Var
   rGlyphO: TPortableNetworkGraphic;

begin
  rGlyphO :=TPortableNetworkGraphic.Create;
  rGlyphO.LoadFromLazarusResource(ResName);
  GlyphDST.Assign(rGlyphO);
  FreeAndNil(rGlyphO);
end;

procedure TBCBoundButton.BuildGlyphs;
begin
  if (rGlyphLayout <> glNone) then
  begin
       if (rUserGlyphCollapsed.Empty)
       then Case rGlyphKind of
            gkArrows: case TBCExpandPanel(Owner).CollapseKind of
                      akTop: LoadGlyph(rGlyphCollapsed, 'BCEXP_PANEL_BOTTOM');
                      akLeft: LoadGlyph(rGlyphCollapsed, 'BCEXP_PANEL_RIGHT');
                      akRight: LoadGlyph(rGlyphCollapsed, 'BCEXP_PANEL_LEFT');
                      akBottom: LoadGlyph(rGlyphCollapsed, 'BCEXP_PANEL_TOP');
                      end;
            gkClose: LoadGlyph(rGlyphCollapsed, 'BCEXP_PANEL_CLOSE');
            gkMinMax: if (TBCExpandPanel(Owner).CollapseKind in [akTop, akBottom])
                      then LoadGlyph(rGlyphCollapsed, 'BCEXP_PANEL_MAX_H')
                      else LoadGlyph(rGlyphCollapsed, 'BCEXP_PANEL_MAX_V');
            end
       else rGlyphCollapsed.Assign(rUserGlyphCollapsed);

       if (rUserGlyphExpanded.Empty)
       then Case rGlyphKind of
            gkArrows: case TBCExpandPanel(Owner).CollapseKind of
                      akTop: LoadGlyph(rGlyphExpanded, 'BCEXP_PANEL_TOP');
                      akLeft: LoadGlyph(rGlyphExpanded, 'BCEXP_PANEL_LEFT');
                      akRight: LoadGlyph(rGlyphExpanded, 'BCEXP_PANEL_RIGHT');
                      akBottom: LoadGlyph(rGlyphExpanded, 'BCEXP_PANEL_BOTTOM');
                      end;
            gkClose: LoadGlyph(rGlyphExpanded, 'BCEXP_PANEL_CLOSE');
            gkMinMax: if (TBCExpandPanel(Owner).CollapseKind in [akTop, akBottom])
                      then LoadGlyph(rGlyphExpanded, 'BCEXP_PANEL_MIN_H')
                      else LoadGlyph(rGlyphExpanded, 'BCEXP_PANEL_MIN_V');
            end
       else rGlyphExpanded.Assign(rUserGlyphExpanded);
  end;
end;

procedure TBCBoundButton.Paint;
var
  paintRect, fRect :TRect;
  xColor,
  xHColor,
  xSColor  :TColor;
  middleX,
  middleY,
  txtWidth,
  txtLeft,
  txtTop,
  glyphLeft,
  glyphTop :Integer;
  xCaption :String;
  FButtonPosition :TAnchorKind;
  FCollapsed, Rounded :Boolean;


  procedure drawGlyph(var ATop, ALeft :Integer);
  var
    AWidth, AHeight :Integer;

  begin
    AWidth :=paintRect.Right-paintRect.Left-2;
    AHeight :=paintRect.Bottom-paintRect.Top-2;

    if FCollapsed
    then rGlyph.Glyph.Assign(rGlyphCollapsed)
    else rGlyph.Glyph.Assign(rGlyphExpanded);

    //We must Calculate the Real Position of the Glyph
    Case FButtonPosition of
    akTop,
    akBottom : begin
                    if (rGlyphLayout = glLeft)
                    then begin
                              ALeft :=2;
                              ATop :=middleY-(rGlyph.Glyph.Height div 2);
                          end
                    else begin
                              ALeft :=AWidth-rGlyph.Glyph.Width;
                              ATop :=middleY-(rGlyph.Glyph.Height div 2);
                          end;
                end;
    akLeft :begin
                 if (rGlyphLayout = glLeft)
                 then begin  //Really on Bottom of paintRect
                           ALeft :=middleX-(rGlyph.Glyph.Width div 2);
                           ATop :=AHeight-rGlyph.Glyph.Height;
                       end
                 else begin  //Really on Top of paintRect
                           ALeft :=middleX-(rGlyph.Glyph.Width div 2);
                           ATop :=2;
                       end;
             end;
    akRight :begin
                 if (rGlyphLayout = glLeft)
                 then begin  //Really on Top of paintRect
                           ALeft :=middleX-(rGlyph.Glyph.Width div 2);
                           ATop :=2;
                       end
                 else begin  //Really on Bottom of paintRect
                           ALeft :=middleX-(rGlyph.Glyph.Width div 2);
                           ATop :=AHeight-rGlyph.Glyph.Height;
                       end;
             end;
    end;

    rGlyph.Draw(Canvas, paintRect, point(ALeft, ATop), FState, true, 0);
  end;

  procedure drawBtn(const ABorderStyle : TGraphicsBevelCut);
  var
     xTabWidth,
     tY, tX: Integer;

  begin
    Case rStyle of
    bbsButton: Frame3d_Rounded(Canvas, paintRect, 1, 5, 5, ABorderStyle, xSColor, xHColor, xColor);
    bbsTab: begin
              fRect :=paintRect;

              Case FButtonPosition of
              akTop : begin
                        //If rTabWidth is Negative Calculate the Tab Width
                        if (rTabWidth < 0)
                        then xTabWidth :=(fRect.Right-fRect.Left)*-rTabWidth div 100
                        else xTabWidth :=rTabWidth;

                        inc(paintRect.Left, middleX-(xTabWidth div 2));
                        paintRect.Right:=paintRect.Left+xTabWidth;
                        Frame3d_Rounded(Canvas, paintRect, 1, 5, 5, ABorderStyle, xSColor, xHColor, xColor);

                        tY :=fRect.Bottom-2;
                        Canvas.Pen.Style:=psSolid;
                        Canvas.Pen.Width:=1;
                        Canvas.Pen.Color :=xHColor;
                        if Rounded
                        then Canvas.MoveTo(2, tY)
                        else Canvas.MoveTo(0, tY);
                        Canvas.LineTo(paintRect.Left-3, tY);
                        Canvas.LineTo(paintRect.Left, tY-3);

                        if Rounded
                        then Canvas.MoveTo(fRect.Right-4, tY)
                        else Canvas.MoveTo(fRect.Right, tY);
                        Canvas.LineTo(paintRect.Right+2, tY);
                        Canvas.LineTo(paintRect.Right-1, tY-3);

                        Canvas.Pen.Color :=xColor;
                        Canvas.MoveTo(paintRect.Left-2, tY);
                        Canvas.LineTo(paintRect.Right+2, tY);
                        dec(tY);
                        Canvas.MoveTo(paintRect.Left-1, tY);
                        Canvas.LineTo(paintRect.Right+1, tY);

                        tY :=fRect.Bottom-1;
                        if FCollapsed then Canvas.Pen.Color :=xSColor;
                        if Rounded
                        then begin
                               Canvas.MoveTo(fRect.Left+2, tY);
                               Canvas.LineTo(fRect.Right-3, tY);
                              end
                        else begin
                               Canvas.MoveTo(fRect.Left, tY);
                               Canvas.LineTo(fRect.Right, tY);
                             end;
                      end;
              akBottom : begin
                        if (rTabWidth < 0)
                        then xTabWidth :=(fRect.Right-fRect.Left)*-rTabWidth div 100
                        else xTabWidth :=rTabWidth;

                        inc(paintRect.Left, middleX-(xTabWidth div 2));
                        paintRect.Right:=paintRect.Left+xTabWidth;
                        dec(paintRect.Top);
                        Frame3d_Rounded(Canvas, paintRect, 1, 5, 5, ABorderStyle, xSColor, xHColor, xColor);

                        Canvas.Pen.Style:=psSolid;
                        Canvas.Pen.Width:=1;
                        Canvas.Pen.Color :=xHColor;
                        if Rounded
                        then Canvas.MoveTo(2, 1)
                        else Canvas.MoveTo(0, 1);
                        Canvas.LineTo(paintRect.Left-3, 1);
                        Canvas.LineTo(paintRect.Left, 4);

                        if Rounded
                        then Canvas.MoveTo(fRect.Right-4, 1)
                        else Canvas.MoveTo(fRect.Right, 1);
                        Canvas.LineTo(paintRect.Right+2, 1);
                        Canvas.LineTo(paintRect.Right-1, 4);

                        Canvas.Pen.Color :=xColor;
                        Canvas.MoveTo(paintRect.Left-2, 1);
                        Canvas.LineTo(paintRect.Right+2, 1);
                        Canvas.MoveTo(paintRect.Left-1, 2);
                        Canvas.LineTo(paintRect.Right+1, 2);

                        if FCollapsed then Canvas.Pen.Color :=xSColor;
                        if Rounded
                        then begin
                               Canvas.MoveTo(fRect.Left+2, 0);
                               Canvas.LineTo(fRect.Right-3, 0);
                              end
                        else begin
                               Canvas.MoveTo(fRect.Left, 0);
                               Canvas.LineTo(fRect.Right, 0);
                             end;
                      end;
              akLeft : begin
                        if (rTabWidth < 0)
                        then xTabWidth :=(fRect.Bottom-fRect.Top)*-rTabWidth div 100
                        else xTabWidth :=rTabWidth;

                        inc(paintRect.Top, middleY-(xTabWidth div 2));
                        paintRect.Bottom:=paintRect.Top+xTabWidth;
                        Frame3d_Rounded(Canvas, paintRect, 1, 5, 5, ABorderStyle, xSColor, xHColor, xColor);

                        tX :=fRect.Right-2;
                        Canvas.Pen.Style:=psSolid;
                        Canvas.Pen.Width:=1;
                        Canvas.Pen.Color :=xHColor;

                        if Rounded
                        then Canvas.MoveTo(tX, 2)
                        else Canvas.MoveTo(tX, 0);
                        Canvas.LineTo(tX, paintRect.Top-3);
                        Canvas.LineTo(tX-3, paintRect.Top);

                        if Rounded
                        then Canvas.MoveTo(tX, fRect.Bottom-4)
                        else Canvas.MoveTo(tX, fRect.Bottom);
                        Canvas.LineTo(tX, paintRect.Bottom+2);
                        Canvas.LineTo(tX-3, paintRect.Bottom-1);

                        Canvas.Pen.Color :=xColor;
                        Canvas.MoveTo(tX, paintRect.Top-2);
                        Canvas.LineTo(tX, paintRect.Bottom+2);
                        dec(tX);
                        Canvas.MoveTo(tX, paintRect.Top-1);
                        Canvas.LineTo(tX, paintRect.Bottom+1);

                        tX :=fRect.Right-1;
                        if FCollapsed then Canvas.Pen.Color :=xSColor;
                        if Rounded
                        then begin
                               Canvas.MoveTo(tX, fRect.Top+2);
                               Canvas.LineTo(tX, fRect.Bottom-3);
                              end
                        else begin
                               Canvas.MoveTo(tX, fRect.Top);
                               Canvas.LineTo(tX, fRect.Bottom);
                             end;
                      end;
              akRight : begin
                        if (rTabWidth < 0)
                        then xTabWidth :=(fRect.Bottom-fRect.Top)*-rTabWidth div 100
                        else xTabWidth :=rTabWidth;

                        inc(paintRect.Top, middleY-(xTabWidth div 2));
                        paintRect.Bottom:=paintRect.Top+xTabWidth;
                        dec(paintRect.Left);
                        Frame3d_Rounded(Canvas, paintRect, 1, 5, 5, ABorderStyle, xSColor, xHColor, xColor);

                        Canvas.Pen.Style:=psSolid;
                        Canvas.Pen.Width:=1;
                        Canvas.Pen.Color :=xHColor;
                        if Rounded
                        then Canvas.MoveTo(1, 2)
                        else Canvas.MoveTo(1, 0);
                        Canvas.LineTo(1, paintRect.Top-3);
                        Canvas.LineTo(4, paintRect.Top);

                        if Rounded
                        then Canvas.MoveTo(1, fRect.Bottom-4)
                        else Canvas.MoveTo(1, fRect.Bottom);
                        Canvas.LineTo(1, paintRect.Bottom+2);
                        Canvas.LineTo(4, paintRect.Bottom-1);

                        Canvas.Pen.Color :=xColor;
                        Canvas.MoveTo(1, paintRect.Top-2);
                        Canvas.LineTo(1, paintRect.Bottom+2);
                        Canvas.MoveTo(2, paintRect.Top-1);
                        Canvas.LineTo(2, paintRect.Bottom+1);

                        if FCollapsed then Canvas.Pen.Color :=xSColor;
                        if Rounded
                        then begin
                               Canvas.MoveTo(0, fRect.Top+2);
                               Canvas.LineTo(0, fRect.Bottom-3);
                              end
                        else begin
                               Canvas.MoveTo(0, fRect.Top);
                               Canvas.LineTo(0, fRect.Bottom);
                             end;
                      end;
              end;
            end;
    end;
  end;


  procedure drawText;
  Var
     DTop, DLeft,
     AWidth, AHeight,
     txtH   :Integer;

    procedure CalcCuttedCaption(MaxWidth :Integer);
    Var
       txtMaxChars  :Integer;

    begin
         txtWidth :=0;
         if (MaxWidth < Canvas.TextWidth('...'))
         then xCaption :=''
         else begin
                   txtMaxChars :=Canvas.TextFitInfo(xCaption, MaxWidth);
                   txtWidth :=Canvas.TextWidth(xCaption);
                   while (txtWidth > MaxWidth) do
                   begin
                        dec(txtMaxChars, 3);    //-1 Chars fit better, -3 Chars for more speed
                        xCaption :=Copy(xCaption, 0, txtMaxChars)+'...';
                        txtWidth :=Canvas.TextWidth(xCaption);
                   end;
               end;
         (* Original Code, Test Speed
         if (txtW > AWidth)
         then begin
                   txtMaxChars :=Canvas.TextFitInfo(xCaption, AWidth);
                   xCaption :=Copy(xCaption, 0, txtMaxChars-3)+'...';
                   txtW :=Canvas.TextWidth(xCaption);
                   if (txtW > AWidth)
                   then xCaption :='';
               end;
         *)
    end;

  begin
    txtH :=Canvas.TextHeight(xCaption);
    AWidth :=paintRect.Right-paintRect.Left-2;
    AHeight :=paintRect.Bottom-paintRect.Top-2;

    Case FButtonPosition of
    akTop,
    akBottom : begin
                 Canvas.Font.Orientation := 0;

                 txtTop :=middleY-(txtH div 2);

                 if (rGlyphLayout <> glNone) then
                 begin
                   if (rTextLayout = tlCenter)
                   then dec(AWidth, rGlyph.Glyph.Width*2+4)
                   else dec(AWidth, rGlyph.Glyph.Width+2)
                 end;

                 CalcCuttedCaption(AWidth);

                 Case rTextLayout of
                 tlLeft :begin
                           txtLeft :=paintRect.Left+4;
                           if (rGlyphLayout = glLeft)
                           then inc(txtLeft, rGlyph.Glyph.Width+2);
                         end;
                 tlRight:begin
                           txtLeft :=paintRect.Left+AWidth-txtWidth;
                           if (rGlyphLayout = glLeft)
                           then inc(txtLeft, rGlyph.Glyph.Width+2);
                         end;
                 tlCenter:begin
                            txtLeft :=middleX-(txtWidth div 2);
                          end;
                 end;

                 //Disabled Position
                 DTop :=txtTop+1;
                 DLeft :=txtLeft+1;
               end;
    akLeft : begin
               //Vertically from Bottom to Top
               Canvas.Font.Orientation := 900;

               txtLeft:=middleX-(txtH div 2);

               if (rGlyphLayout <> glNone) then
               begin
                 if (rTextLayout = tlCenter)
                 then dec(AHeight, rGlyph.Glyph.Height*2+4)
                 else dec(AHeight, rGlyph.Glyph.Height+2)
               end;

               //Vertically the Max Width is Height
               CalcCuttedCaption(AHeight);

               Case rTextLayout of
               tlLeft :begin   //To Bottom of the ClientRect
                         txtTop :=paintRect.Top+AHeight-2;

                         if (rGlyphLayout = glRight)
                         then inc(txtTop, rGlyph.Glyph.Height+2);
                       end;
               tlRight:begin  //To Top of the ClientRect
                         txtTop :=paintRect.Top+txtWidth+2;
                         if (rGlyphLayout = glRight)
                         then inc(txtTop, rGlyph.Glyph.Height+2);
                       end;
               tlCenter:begin
                          txtTop :=middleY+(txtWidth div 2);
                        end;
               end;

               //Disabled Position
               DTop :=txtTop-1;
               DLeft :=txtLeft+1;
              end;
    akRight : begin
                //Vertically from Top to Bottom
                Canvas.Font.Orientation := -900;

                txtLeft:=middleX+(txtH div 2)+1; //+1 because is better centered

                if (rGlyphLayout <> glNone) then
                begin
                  if (rTextLayout = tlCenter)
                  then dec(AHeight, rGlyph.Glyph.Height*2+4)
                  else dec(AHeight, rGlyph.Glyph.Height+2)
                end;

                CalcCuttedCaption(AHeight);

                Case rTextLayout of
                tlLeft :begin  //To Top of the ClientRect
                          txtTop :=paintRect.Top+4;

                          if (rGlyphLayout = glLeft)
                          then inc(txtTop, rGlyph.Glyph.Height+2);
                        end;
                tlRight:begin  //To Bottom of the ClientRect
                          txtTop :=paintRect.Top+AHeight-txtWidth;
                          if (rGlyphLayout = glLeft)
                          then inc(txtTop, rGlyph.Glyph.Height+2);
                        end;
                tlCenter:begin
                           txtTop :=middleY-(txtWidth div 2);
                         end;
                end;

                //Disabled Position
                DTop :=txtTop+1;
                DLeft :=txtLeft-1;
              end;
    end;

    //Re Test here because we may not have space to draw the text, so now can be empty
    if (xCaption <> '') then
    begin
      if (FState = bsDisabled)
      then begin
             Canvas.Font.Color := clBtnHighlight;
             Canvas.TextOut(DLeft, DTop, xCaption);
             Canvas.Font.Color := clBtnShadow;
           end
      else Canvas.Font.Color := Font.Color;

      Canvas.Brush.Style:=bsClear;
      Canvas.TextOut(txtLeft, txtTop, xCaption);
    end
    else txtWidth:=0;
  end;

  procedure DrawLines;
  var
     d1, d2, d3, d4, dx :Integer;
     isVertical :Boolean;

     procedure calc_d(txtL, txtR, glyphL, glyphR :Integer);
     begin
       if (txtWidth > 0)
       then Case rTextLayout of
            tlLeft: begin
                      d1 :=txtR;
                      if (rGlyphLayout = glRight)
                      then d2 :=glyphL;
                    end;
            tlCenter:begin
                       d2 :=txtL;
                       d3 :=txtR;
                       if (rGlyphLayout = glLeft)
                       then d1 :=glyphR
                       else if (rGlyphLayout = glRight)
                            then d4 :=glyphL;
                     end;
            tlRight:begin
                      d2 :=txtL;
                      if (rGlyphLayout = glLeft)
                      then d1 :=glyphR;
                    end;
            end
       else if (rGlyphLayout = glLeft)
            then d1 :=glyphR
            else if (rGlyphLayout = glRight)
                 then d2 :=glyphL;
     end;

     procedure DrawALine(pCenterX, pCenterY :Integer);
     begin
       inc(d2); inc(d4); //LineTo don't paint the last Pixel

       if isVertical
       then begin
              //Avoid go outside the Box
              pCenterX :=EnsureRange(pCenterX, 0, paintRect.Right-2);

              Canvas.Pen.Color := {$ifdef DEBUG_PAINT} clLime {$else} xHColor {$endif};
              Canvas.MoveTo(pCenterX, d1);
              Canvas.LineTo(pCenterX, d2);
              if (d3 > -1) then
              begin
                Canvas.MoveTo(pCenterX, d3);
                Canvas.LineTo(pCenterX, d4);
              end;
              Canvas.Pen.Color := {$ifdef DEBUG_PAINT} clGreen {$else} xSColor {$endif};
              Canvas.MoveTo(pCenterX+1, d1+1);
              Canvas.LineTo(pCenterX+1, d2);
              if (d3 > -1) then
              begin
                Canvas.MoveTo(pCenterX+1, d3+1);
                Canvas.LineTo(pCenterX+1, d4);
              end;
            end
       else begin
              pCenterY :=EnsureRange(pCenterY, 0, paintRect.Bottom-2);

              Canvas.Pen.Color :={$ifdef DEBUG_PAINT} clLime {$else} xHColor {$endif};
              Canvas.MoveTo(d1, pCenterY);
              Canvas.LineTo(d2, pCenterY);
              if (d3 > -1) then
              begin
                Canvas.MoveTo(d3, pCenterY);
                Canvas.LineTo(d4, pCenterY);
              end;
              Canvas.Pen.Color :={$ifdef DEBUG_PAINT} clGreen {$else} xSColor {$endif};
              Canvas.MoveTo(d1+1, pCenterY+1);
              Canvas.LineTo(d2, pCenterY+1);
              if (d3 > -1) then
              begin
                Canvas.MoveTo(d3+1, pCenterY+1);
                Canvas.LineTo(d4, pCenterY+1);
              end;
            end;

       dec(d2); dec(d4); //return to the real Pixels
     end;

  begin
    d3 :=-1;
    isVertical :=(FButtonPosition in [akLeft, akRight]);

    //Assign to (d1-d2) Line All the space
    if isVertical
    then begin
           d1 :=paintRect.Top;
           d2 :=paintRect.Bottom-1;
         end
    else begin
           d1 :=paintRect.Left;
           d2 :=paintRect.Right-1;
          end;

    //Calculate the (d1-d2) (d3-d4) Lines between the Glyph and the Text elements
    if (rStyle in [bbsLine, bbsLineDouble]) then
    begin
      d4 :=d2;
      if isVertical
      then begin
             if (FButtonPosition = akRight)
             then calc_d(txtTop-3, txtTop+txtWidth+2, glyphTop-3, glyphTop+rGlyph.Glyph.Height+2)
             else begin
                    //Only in this case (akLeft) the point coordinate is from bottom to top
                    d1 :=paintRect.Bottom-1;
                    d2 :=paintRect.Top;
                    d4 :=d2;

                    calc_d(txtTop+2, txtTop-txtWidth-3, glyphTop+rGlyph.Glyph.Height+2, glyphTop-3);

                    //Exchange the values for Shadow coerence
                    dx :=d1; d1 :=d2; d2 :=dx;
                    if (d3 > -1) then begin dx :=d3; d3 :=d4; d4 :=dx; end;
                  end;
           end
      else calc_d(txtLeft-3, txtLeft+txtWidth+2, glyphLeft-3, glyphLeft+rGlyph.Glyph.Width+2);
    end;

    //Draw the Lines
    Canvas.Pen.Style:=psSolid;
    Canvas.Pen.Width:=1;
    Case rStyle of
    bbsLine: DrawALine(middleX, middleY);
    bbsLineDouble: begin
                     DrawALine(middleX-2, middleY-2);
                     DrawALine(middleX+2, middleY+2);
                   end;
    bbsLineTop: DrawALine(paintRect.Left, paintRect.Top);
    bbsLineBottom: DrawALine(paintRect.Right-2, paintRect.Bottom-2);
    bbsLineDoubleTop: begin
                        DrawALine(paintRect.Left, paintRect.Top);
                        DrawALine(paintRect.Left+3, paintRect.Top+3);
                      end;
    bbsLineDoubleBottom: begin
                           DrawALine(paintRect.Right-5, paintRect.Bottom-5);
                           DrawALine(paintRect.Right-2, paintRect.Bottom-2);
                         end;
    end;
  end;

begin
  paintRect :=GetClientRect;

  {$ifdef DEBUG_PAINT}
    Canvas.Brush.Color:=clYellow;
    Canvas.Brush.Style:=bsSolid;
    Canvas.FillRect(paintRect);
  {$endif}

  middleY :=paintRect.Top+((paintRect.Bottom-paintRect.Top) div 2);
  middleX :=paintRect.Left+((paintRect.Right-paintRect.Left) div 2);

  FButtonPosition :=TBCExpandPanel(Owner).FButtonPosition;
  FCollapsed :=TBCExpandPanel(Owner).FCollapsed;
  Rounded :=not(FCollapsed) and TBCExpandPanel(Owner).rBevelRounded;

  if FCollapsed
  then xColor :=Self.Color
  else xColor :=rColorExpanded;

  xCaption :=Caption;

  Case FState of
  Buttons.bsHot:begin
                     if (rColorHighlight = clDefault)
                     then xHColor :=GetHighlightColor(xColor, 120)
                     else xHColor :=rColorHighlight;

                     if (rColorShadow = clDefault)
                     then xSColor :=GetShadowColor(xColor, 40)
                     else xSColor :=rColorShadow;

                     xColor :=GetHighlightColor(xColor, 20);
                     drawBtn(bvRaised);
                end;
  Buttons.bsDown:begin
                      if (rColorHighlight = clDefault)
                      then xHColor :=GetHighlightColor(xColor, 60)
                      else xHColor :=rColorHighlight;

                      if (rColorShadow = clDefault)
                      then xSColor :=GetShadowColor(xColor, 60)
                      else xSColor :=rColorShadow;

                      xColor :=GetHighlightColor(xColor, 20);
                      drawBtn(bvLowered);
                  end;
  else begin
            if (FState = bsDisabled)
            then xColor :=GrayScale(xColor);

            if Flat
            then xHColor :=xColor
            else if (rColorHighlight = clDefault)
                 then xHColor :=GetHighlightColor(xColor, 60)
                 else xHColor :=rColorHighlight;

            if (rColorShadow = clDefault)
            then xSColor :=GetShadowColor(xColor, 60)
            else xSColor :=rColorShadow;

            if Flat
            then drawBtn(bvSpace)
            else drawBtn(bvRaised);
        end;
  end;

  if (rGlyphLayout <> glNone)
  then drawGlyph(glyphTop, glyphLeft)
  else begin
            glyphTop :=0;
            glyphLeft:=0;
        end;

  if (rTextLayout <> tlNone) and (xCaption <> '')
  then drawText
  else txtWidth:=0;

  if (rStyle in [bbsLine..bbsLineDoubleBottom])
  then DrawLines;
end;

procedure TBCBoundButton.Loaded;
begin
  inherited Loaded;

  if not(csDesigning in ComponentState) then
  begin
       //IF Used Outside TBCExpandPanel
       if not(Owner is TBCExpandPanel)
       then BuildGlyphs;
  end;
end;

constructor TBCBoundButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Color :=clSkyBlue;
  rColorExpanded := RGBToColor(23, 136, 248);
  rColorHighlight :=clDefault;
  rColorShadow :=clDefault;
  rGlyphLayout :=glNone;
  rGlyphKind :=gkArrows;
  rTextLayout :=tlLeft;
  Flat :=False;
  rStyle :=bbsButton;
  rTabWidth :=-50;

  //Why FGlyph is Private in ancestor?????
  rGlyph := TButtonGlyph.Create;
  rGlyph.IsDesigning := csDesigning in ComponentState;
  rGlyph.ShowMode := gsmAlways;

  rGlyphExpanded :=TBitmap.Create;
  rGlyphExpanded.Transparent := True;
  rGlyphCollapsed :=TBitmap.Create;
  rGlyphCollapsed.Transparent := True;
  rUserGlyphExpanded :=TBitmap.Create;
  rUserGlyphExpanded.Transparent := True;
  rUserGlyphCollapsed :=TBitmap.Create;
  rUserGlyphCollapsed.Transparent := True;

  SetSubComponent((Owner is TBCExpandPanel));
//  ControlStyle := ControlStyle + [csNoFocus, csNoDesignSelectable];
end;

destructor TBCBoundButton.Destroy;
begin
  FreeAndNil(rGlyphExpanded);
  FreeAndNil(rGlyphCollapsed);
  FreeAndNil(rUserGlyphExpanded);
  FreeAndNil(rUserGlyphCollapsed);
  FreeAndNil(rGlyph);
  inherited Destroy;
end;

{TBCExpandPanels}

constructor TBCExpandPanels.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  PanelArray := TList.Create;

  FCollapseKind := akTop;
  FButtonPosition := akTop;
  FButtonGlyphKind :=gkArrows;
  FButtonGlyphLayout :=glNone;
  FButtonStyle :=bbsButton;
  FButtonTabWidth :=-50;
  FButtonTextLayout :=tlLeft;
  FArrangeKind := akTop;
  FUseFixedSize := False;
  FUseClientSize := False;
  FFixedSize := 400;
  FAutoCollapseIfTooHigh := False;
  FAbove := 10;
  FOrthogonalAbove := 10;
  FOrthogonalSize := 200;
end;

destructor TBCExpandPanels.Destroy;
var
  i: integer;
begin
  for I := PanelArray.Count - 1 downto 0 do
    PanelArray.Delete(i);

  PanelArray.Free;
  PanelArray := nil;

  inherited Destroy;
end;

procedure TBCExpandPanels.AddPanel(rollout: TBCExpandPanel);
begin
  InsertPanel(PanelArray.Count, rollout);
end;

procedure TBCExpandPanels.InsertPanel(idx: integer; rollout: TBCExpandPanel);
begin
  if Count <= 0 then
    begin
    FAbove := RelevantAbove(rollout);
    FOrthogonalAbove := RelevantOrthogonalAbove(rollout);
    FOrthogonalSize := RelevantOrthogonalSize(rollout);
    end
  else
    begin
    WriteRelevantAbove(rollout, FAbove);
    WriteRelevantOrthogonalAbove(rollout, FOrthogonalAbove);
    WriteRelevantOrthogonalSize(rollout, FOrthogonalSize);
    end;

  with rollout do
    begin
    Tag := Idx;
    FButton.Tag := Idx;

    FButton.OnMouseMove := @RollOut1MouseMove;
    InternalOnAnimate   := @RollOutOnAnimate;
    end;


  PanelArray.Insert(idx, rollout);

  if FBehaviour <> EPMultipanel then
    HotTrackSetActivePanel(0);  //damit das erste ausgeklappt ist

  ArrangePanels;
  MakeCorrectButtonClickPointers;
end;




function TBCExpandPanels.DeltePanel(aname: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to PanelArray.Count - 1 do
    if TBCExpandPanel(PanelArray[i]).Name = aname then
      begin
      PanelArray.Delete(i);
      Result := True;
      break;
      end;
  ArrangePanels;
end;


function TBCExpandPanels.DeltePanel(idx: integer): boolean;
begin
  Result := False;
  if (idx >= 0) and (idx <= PanelArray.Count - 1) then
    begin
    PanelArray.Delete(idx);
    Result := True;
    end;
  ArrangePanels;
end;

procedure TBCExpandPanels.DelteLastPanel;
begin
  if (PanelArray.Count >= 1) then
    PanelArray.Delete(PanelArray.Count - 1);
  ArrangePanels;
end;




procedure TBCExpandPanels.DelLastPanel;
begin
  PanelArray.Delete(PanelArray.Count - 1);
end;


function TBCExpandPanels.RelevantAbove(comp: TControl): integer;
begin
  case FArrangeKind of
    akLeft: Result := comp.Left;
    akTop: Result  := comp.Top;
    end;
end;

function TBCExpandPanels.RelevantOrthogonalAbove(comp: TControl): integer;
begin
  case FArrangeKind of
    akTop: Result  := comp.Left;
    akLeft: Result := comp.Top;
    end;
end;

function TBCExpandPanels.RelevantSize(comp: TControl): integer;
begin
  case FArrangeKind of
    akLeft: Result := comp.Width;
    akTop: Result  := comp.Height;
    end;
end;

function TBCExpandPanels.RelevantOrthogonalSize(comp: TControl): integer;
begin
  case FArrangeKind of
    akLeft: Result := comp.Height;
    akTop: Result  := comp.Width;
    end;
end;

procedure TBCExpandPanels.setButtonGlyphKind(AValue: TGlyphKind);
var
   i: Integer;

begin
  if (FButtonGlyphKind <> AValue) then
  begin
    FButtonGlyphKind:=AValue;

    for i := 0 to PanelArray.Count - 1 do
      Panel(i).Button.GlyphKind := AValue;
  end;
end;

procedure TBCExpandPanels.setButtonGlyphLayout(AValue: TGlyphLayout);
var
   i: Integer;

begin
  if (FButtonGlyphLayout <> AValue) then
  begin
    FButtonGlyphLayout:=AValue;

    for i := 0 to PanelArray.Count - 1 do
      Panel(i).Button.GlyphLayout := AValue;
  end;
end;

procedure TBCExpandPanels.setButtonStyle(AValue: TBCBoundButtonStyle);
var
   i: Integer;

begin
  if (FButtonStyle <> AValue) then
  begin
    FButtonStyle:=AValue;

    for i := 0 to PanelArray.Count - 1 do
      Panel(i).Button.Style := AValue;
  end;
end;

procedure TBCExpandPanels.SetButtonTabWidth(AValue: Integer);
var
   i: Integer;

begin
  if (FButtonTabWidth <> AValue) then
  begin
    FButtonTabWidth:=AValue;

    for i := 0 to PanelArray.Count - 1 do
      Panel(i).Button.TabWidth := AValue;
  end;
end;

procedure TBCExpandPanels.setButtonTextLayout(AValue: TTextLayout);
var
   i: Integer;

begin
  if (FButtonTextLayout <> AValue) then
  begin
    FButtonTextLayout:=AValue;

    for i := 0 to PanelArray.Count - 1 do
      Panel(i).Button.TextLayout := AValue;
  end;
end;

procedure TBCExpandPanels.WriteRelevantAbove(comp: TBCExpandPanel; above: integer);
begin
  case FArrangeKind of
    akLeft: comp.Left := above;
    akTop: comp.Top   := above;
    end;
end;

procedure TBCExpandPanels.WriteRelevantSize(comp: TBCExpandPanel; size: integer);
begin
  case FArrangeKind of
    akLeft: comp.Width := size;
    akTop: comp.Height := size;
    end;
end;

procedure TBCExpandPanels.WriteRelevantOrthogonalSize(comp: TBCExpandPanel; size: integer);
begin
  case FArrangeKind of
    akLeft: comp.Height := size;
    akTop: comp.Width   := size;
    end;
end;

procedure TBCExpandPanels.WriteRelevantOrthogonalAbove(comp: TBCExpandPanel; size: integer);
begin
  case FArrangeKind of
    akLeft: comp.Top := size;
    akTop: comp.Left := size;
    end;
end;


procedure TBCExpandPanels.setArrangeKind(Value: TAnchorKind);
begin
  case Value of  //that is mean, but I haven't implemented the bottom and right yet....
    akRight: Value  := akLeft;
    akBottom: Value := akTop;
    end;

  if FArrangeKind = Value then
    exit;
  FArrangeKind := Value;

  ArrangePanels;
end;

procedure TBCExpandPanels.setButtonPosition(Value: TAnchorKind);
var
  i: integer;
begin
  if FButtonPosition = Value then
    exit;
  FButtonPosition := Value;

  for i := 0 to PanelArray.Count - 1 do
    Panel(i).ButtonPosition := Value;
end;

procedure TBCExpandPanels.setCollapseKind(Value: TAnchorKind);
var
  i: integer;
begin
  if FCollapseKind = Value then
    exit;
  FCollapseKind := Value;

  for i := 0 to PanelArray.Count - 1 do
    Panel(i).CollapseKind := Value;
end;

procedure TBCExpandPanels.setUseClientSize(Value: boolean);
begin
  FUseClientSize := Value;

  ArrangePanels;
end;

procedure TBCExpandPanels.setUseFixedSize(Value: boolean);
begin
  if FUseFixedSize = Value then
    exit;
  FUseFixedSize := Value;

  ArrangePanels;
end;

procedure TBCExpandPanels.setAutoCollapseIfTooHigh(Value: boolean);
begin
  if FAutoCollapseIfTooHigh = Value then
    exit;
  FAutoCollapseIfTooHigh := Value;

  if FAutoCollapseIfTooHigh then
    CollapseIfTooHigh;
end;


procedure TBCExpandPanels.setFixedSize(Value: integer);
var
  r: real;
begin
  if FFixedSize = Value then
    exit;

  r := Value;
  korrigiere(r, 20, 10000);
  FFixedSize := round(r);

  ArrangePanels;
end;



procedure TBCExpandPanels.setOrthogonalAbove(Value: integer);
begin
  if FOrthogonalAbove = Value then
    exit;
  FOrthogonalAbove := Value;

  ArrangePanels;
end;


procedure TBCExpandPanels.setAbove(Value: integer);
begin
  if FAbove = Value then
    exit;
  FAbove := Value;

  ArrangePanels;
end;


procedure TBCExpandPanels.setOrthogonalSize(Value: integer);
var
  i: integer;
begin
  FOrthogonalSize := Value;

  for I := 0 to PanelArray.Count - 1 do
    WriteRelevantOrthogonalSize(TBCExpandPanel(PanelArray[i]), FOrthogonalSize);
end;




procedure TBCExpandPanels.setBehaviour(Value: TBCExpandPanelsBehaviour);
var
  i: integer;
  isAlreadyOneExpand: boolean;
begin
  isAlreadyOneExpand := False;
  FBehaviour := Value;

  MakeCorrectButtonClickPointers;

  // look if more then one is open
  for I := 0 to PanelArray.Count - 1 do
    with TBCExpandPanel(PanelArray[i]) do
      if (Behaviour <> EPMultipanel) and not Collapsed then   //leave only the first open, if it is not MultiPanel
        if not isAlreadyOneExpand then
          isAlreadyOneExpand := True
        else
          Collapsed := True;
end;

procedure TBCExpandPanels.MakeCorrectButtonClickPointers;
var
  i: integer;
begin
  // set correct pointers
  for I := 0 to PanelArray.Count - 1 do
    with TBCExpandPanel(PanelArray[i]) do
      if FBehaviour <> EPMultipanel then
        EPManagesCollapsing := @RollOutClick
      else
        EPManagesCollapsing := nil;
end;



procedure TBCExpandPanels.CollapseIfTooHigh;
var
  i, h, max:    integer;
  tempanimated: boolean;
begin
  if Count <= 1 then
    exit;


  h   := RelevantAbove(Panel(0));
  max := RelevantSize(Panel(0).Parent);

  for i := 0 to Count - 1 do
    if h + RelevantSize(Panel(i)) > max then
      with Panel(i) do
        begin
        tempanimated := Animated;
        Animated     := False;
        Collapsed    := True;
        Animated     := tempanimated;

        h := h + TBCExpandPanel(Panel(i)).ButtonSize;
        end
    else
      h := h + RelevantSize(Panel(i));
end;



procedure TBCExpandPanels.RollOutOnAnimate(Sender: TObject; deltaLeft, deltaTop, deltaWidth, deltaHeight: integer);
var
  idx, i, size: integer;
begin
  idx := PanelArray.IndexOf(Sender);

  for i := idx + 1 to PanelArray.Count - 1 do
    begin
    size := RelevantAbove(TBCExpandPanel(PanelArray[i]));
    case FArrangeKind of
      akTop: size  := size + deltaTop + deltaHeight;
      akLeft: size := size + deltaLeft + deltaWidth;
      end;

    WriteRelevantAbove(TBCExpandPanel(PanelArray[i]), size);

    end;
end;




 //procedure TBCExpandPanels.SetCorrectSize;
 //const plus=1;   //extra Anstand
 //var
 //    i, exSize,
 //    countexpanded,
 //    SumSize, closedSize:Integer;
 //begin
 //  if PanelArray.Count<=0 then
 //    exit;

 //  SumSize:=FFixedSize;
 //  if FUseClientSize then
 //    SumSize:=TBCExpandPanel(PanelArray[0]).Parent.Height;


 //  countexpanded:=0;
 //  closedSize:=0;
 //  for I := 0 to PanelArray.count-1 do
 //    with TBCExpandPanel(PanelArray[i]) do
 //      begin
//      if not Collapsed and not Animating         //error producer!!!   animating does not neccessairily mean that it is expanding
 //       or Collapsed and Animating then
 //        inc(countexpanded)
 //      else
 //        closedSize:=closedSize+Height;
 //      end;

//  exSize:=SumSize- FTop- closedSize;

 //  case Behaviour of
 //    EPMultipanel:
 //      if countexpanded>0 then
 //        exSize:=trunc(exSize/countexpanded)
 //      else
 //        exSize:=400;
 //  end;

 //  for I := 0 to PanelArray.count-1 do
 //    with TBCExpandPanel(PanelArray[i]) do
 //      begin
 //      if not FUseFixedSize and not FUseClientSize then
 //        ExpandedSize:=200
 //      else
 //        ExpandedSize:=exSize;
 //      end;
 //end;



{==============================================================================
  Procedure:    ArrangePanels
  Belongs to:   TBCExpandPanels
  Result:       None
  Parameters:

  Description:
==============================================================================}
procedure TBCExpandPanels.ArrangePanels;
const
  plus = 1;   //extra Anstand
var
  i, t: integer;
begin
  if Count <= 0 then
    exit;


  //left setzen!!!
  //  SetCorrectSize;

  t := FAbove + plus;

  for I := 0 to PanelArray.Count - 1 do
    begin
    if not TBCExpandPanel(PanelArray[i]).Visible then
      continue;

    WriteRelevantAbove(TBCExpandPanel(PanelArray[i]), t);
    WriteRelevantOrthogonalAbove(TBCExpandPanel(PanelArray[i]), OrthogonalAbove);
    t := t + plus + self.RelevantSize(TBCExpandPanel(PanelArray[i]));
    end;

  if FAutoCollapseIfTooHigh then
    CollapseIfTooHigh;

  if Assigned(FOnArrangePanels) then
    FOnArrangePanels(Self);
end;



function TBCExpandPanels.Count: integer;
begin
  Result := PanelArray.Count;
end;

function TBCExpandPanels.Panel(idx: integer): TBCExpandPanel;
begin
  if idx < Count then
    Result := TBCExpandPanel(PanelArray.Items[idx])
  else
    Result := nil;
end;




{==============================================================================
  Procedure:    RollOutClick
  Belongs to:   TBCExpandPanels
  Result:       None
  Parameters:
                  Sender : TObject  =

  Description:
==============================================================================}
procedure TBCExpandPanels.RollOutClick(Sender: TObject);
begin
  if (Behaviour <> EPMultipanel) then
    HotTrackSetActivePanel(TBCBoundButton(Sender).Tag);
end;




procedure TBCExpandPanels.HotTrackSetActivePanel(Value: integer);
var
  i: integer;
begin
  for I := PanelArray.Count - 1 downto 0 do
    TBCExpandPanel(PanelArray[i]).Collapsed := Value <> i;
end;




procedure TBCExpandPanels.RollOut1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if (Behaviour = EPHotMouse) and (TBCExpandPanel(PanelArray[TBCBoundButton(Sender).Tag]).Collapsed) then
    HotTrackSetActivePanel(TBCBoundButton(Sender).Tag);
end;


function TBCExpandPanels.IdxOfPanel(aname: string): integer;
var
  i: integer;
begin
  Result := -1;      // is not here
  for i := 0 to PanelArray.Count - 1 do
    if TBCExpandPanel(PanelArray[i]).Name = aname then
      begin
      Result := i;
      break;
      end;
end;




{ TBCExpandPanel }


procedure TBCExpandPanel.setCollapsed(Value: boolean);
begin
{$ifopt D+}
  debugln('TBCExpandPanel.setCollapsed '+BoolToStr(Collapsed, True));
{$endif}

  if FCollapsed = Value then
    exit;

  FCollapsed := Value;

  if not(csLoading in ComponentState)
  then if Value
       then DoCollapse
       else DoExpand;
end;

procedure TBCExpandPanel.SetRelevantSize(comp: TControl; AKind: TAnchorKind; ASize: Integer);
begin
  case AKind of
    akTop, akBottom: comp.Height :=ASize;
    akLeft, akRight: comp.Width :=ASize;
  end;
end;

function TBCExpandPanel.RelevantSize(comp: TControl; akind: TAnchorKind): integer;
begin
  case akind of
    akTop, akBottom: Result := comp.Height;
    akLeft, akRight: Result := comp.Width;
    end;
end;

function TBCExpandPanel.RelevantOrthogonalSize(comp: TControl; akind: TAnchorKind): integer;
begin
  case akind of
    akTop, akBottom: Result := comp.Width;
    akLeft, akRight: Result := comp.Height;
    end;
end;

function TBCExpandPanel.DeltaCoordinates(deltaMove, deltaSize: integer): TRect;
begin
  Result := Rect(0, 0, 0, 0);

  case FCollapseKind of
    akTop: Result    := Rect(0, 0, 0, deltaSize);
    akLeft: Result   := Rect(0, 0, deltaSize, 0);
    akBottom: Result := Rect(0, deltaMove, 0, deltaSize);
    akRight: Result  := Rect(deltaMove, 0, deltaSize, 0);
    end;
end;



procedure TBCExpandPanel.TimerAnimateSize(Sender: TObject);
var
  step:  real;
  originalsize, size: integer;
  deltaMove, deltaSize: integer;
  delta: TRect;
  vorzeichen: integer;
begin
  deltaMove := 0;
  deltaSize := 0;
  StopCircleActions := False;
  FAnimating := True;
  step := FAnimationSpeed;


  Size := RelevantSize(Self, FCollapseKind);

  vorzeichen   := Sign(TargetAnimationSize - RelevantSize(self, FCollapseKind));  // muss ich delta addieren oder muss ich delta abziehen
  originalsize := ExpandedSize;


  //One huge step if not animated
  if not FAnimated or not (ComponentState * [csLoading, csDesigning] = []) then
    step := abs(Size - TargetAnimationSize);

  //small steps if animated
  if FAnimated and (ComponentState * [csLoading, csDesigning] = []) then
    begin
    step := step * originalsize / 200;
    if step < 3 then
      step := 3;
    end;


  //now actually do something

  if Abs(Size - TargetAnimationSize) > 0 then
    begin
    if Abs(Size - TargetAnimationSize) < abs(step) then  // if there is just a little bit left to go, set delta so it can go directly to the end size
      deltaSize := TargetAnimationSize - Size
    else
      deltaSize := vorzeichen * round(step);

    if (CollapseKind = akBottom) or (CollapseKind = akRight) then
      deltaMove := -deltaSize;


    delta := DeltaCoordinates(deltaMove, deltaSize);

    SetBounds(Left + delta.Left, Top + delta.Top, Width + delta.Right, Height + delta.Bottom);

    if assigned(FInternalOnAnimate) then
      FInternalOnAnimate(self, delta.Left, delta.Top, delta.Right, delta.Bottom);
    if assigned(FOnAnimate) then
      FOnAnimate(self, delta.Left, delta.Top, delta.Right, delta.Bottom);
    end;


  if Abs(Size - TargetAnimationSize) = 0 then        //it's finished  ( executes it NEXT time the timer activates!)
    begin
    Timer.Enabled := False;

    FAnimating := False;

    StopCircleActions := False;

    if assigned(EndProcedureOfAnimation) then
      EndProcedureOfAnimation;
    end;
end;



procedure TBCExpandPanel.EndTimerCollapse;
begin
  if assigned(OnCollapse) then
    OnCollapse(self);

  UpdateAll;
end;

procedure TBCExpandPanel.EndTimerExpand;
begin
  if assigned(OnExpand) then
    OnExpand(self);

  UpdateAll;
end;



procedure TBCExpandPanel.UpdateAll;
begin
  Update;
  //FButton.Update;
end;




procedure TBCExpandPanel.setExpandedSize(Value: integer);
begin
  {$ifopt D+}
  debugln('TBCExpandPanel.setExpandedSize');
  debugln(IntToStr(Value));
  {$endif}

  if (FExpandedSize = Value) then
    exit;

  FExpandedSize := Value;

  if not(csLoading in ComponentState) and not(FCollapsed)
  then Animate(FExpandedSize);
end;

function TBCExpandPanel.GetEnabled: Boolean;
begin
     Result :=inherited Enabled;
     if (FButton.Enabled <> Result) //Paranoic Think
     then FButton.Enabled :=Result;
end;

procedure TBCExpandPanel.SetBevelColorHighlight(AValue: TColor);
begin
  if (rBevelColorHighlight <> AValue) then
  begin
    rBevelColorHighlight := AValue;

    if not(csLoading in ComponentState)
    then Invalidate;
  end;
end;

procedure TBCExpandPanel.SetBevelColorShadow(AValue: TColor);
begin
  if (rBevelColorShadow <> AValue) then
  begin
    rBevelColorShadow := AValue;

    if not(csLoading in ComponentState)
    then Invalidate;
  end;
end;

procedure TBCExpandPanel.SetBevelRounded(AValue: Boolean);
begin
  if (rBevelRounded <> AValue) then
  begin
    rBevelRounded := AValue;

    if not(csLoading in ComponentState)
    then Invalidate;
   end;
end;

procedure TBCExpandPanel.SetEnabled(AValue: Boolean);
begin
     inherited Enabled :=AValue;
     FButton.Enabled :=AValue;
end;

procedure TBCExpandPanel.setButtonSize(Value: integer);
begin
  if FButtonSize = Value then
    exit;

  FButtonSize := Value;

  PositionButton;
end;

procedure TBCExpandPanel.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if not Collapsed and not Animating and (ComponentState * [csLoading] = []) then
    FExpandedSize := RelevantSize(self, FCollapseKind);
end;


procedure TBCExpandPanel.setButtonPosition(Value: TAnchorKind);
var
  wasanimated, wascollpased: boolean;
begin
  if FButtonPosition = Value then
    exit;

  wasanimated  := Animated;
  wascollpased := Collapsed;
  Animated     := False;
  if Collapsed then
    Collapsed := False;

  FButtonPosition := Value;
  PositionButton;

  Collapsed := wascollpased;
  Animated  := wasanimated;

  Invalidate;
end;


procedure TBCExpandPanel.setCollapseKind(Value: TAnchorKind);
var
  wasanimated, wascollpased: boolean;
begin
  if FCollapseKind = Value then
    exit;

  wasanimated  := Animated;
  wascollpased := Collapsed;
  Animated     := False;

  if Collapsed then
    Collapsed := False;

  FCollapseKind := Value;


  //switsch sizes

  case FCollapseKind of
    akLeft, akRight: FExpandedSize := Width;
    akTop, akBottom: FExpandedSize := Height;
    end;

  if not(csLoading in ComponentState) then
  begin
       FButton.BuildGlyphs;
       FButton.Invalidate;
  end;

  Collapsed := wascollpased;

  Animated := wasanimated;
end;

procedure TBCExpandPanel.setAnimationSpeed(Value: real);
begin
  korrigiere(Value, 3, 1000);
  FAnimationSpeed := Value;
end;

procedure TBCExpandPanel.PositionButton;

  function ButtonRect: TRect;
  begin
    case FButtonPosition of
      akBottom, akTop: Result := Rect(0, 0, RelevantOrthogonalSize(self, FButtonPosition), FButtonSize);
      akLeft, akRight: Result := Rect(0, 0, FButtonSize, RelevantOrthogonalSize(self, FButtonPosition));
      end;

    //this must come after the thing above!!!
    // this moves the button to the bottom, or the right
    case FButtonPosition of
      akBottom: Result.Top := Result.Top + RelevantSize(self, FButtonPosition) - FButtonSize;
      akRight: Result.Left := Result.Left + RelevantSize(self, FButtonPosition) - FButtonSize;
      end;
  end;

var
  new: TRect;
begin
  if StopCircleActions or not(Assigned(FButton)) or (csLoading in ComponentState)
  then exit;
  StopCircleActions := True;


  new := ButtonRect;
  FButton.SetBounds(new.Left, new.Top, new.Right, new.Bottom);


  //set anchors
  case FButtonPosition of
    akBottom: FButton.Anchors := [akTop, akLeft, akBottom, akRight] - [akTop];
    akLeft: FButton.Anchors   := [akTop, akLeft, akBottom, akRight] - [akRight];
    akTop: FButton.Anchors    := [akTop, akLeft, akBottom, akRight] - [akBottom];
    akRight: FButton.Anchors  := [akTop, akLeft, akBottom, akRight] - [akLeft];
    end;

  Invalidate;

  StopCircleActions := False;
end;

procedure TBCExpandPanel.ButtonClick(Sender: TObject);
begin
  if Assigned(FEPManagesCollapsing) then
    FEPManagesCollapsing(self)
  else
    Collapsed := not Collapsed;

  if Assigned(OnButtonClick) then
    OnButtonClick(self);
end;

procedure TBCExpandPanel.Animate(aTargetSize: integer);
var
  storAnimated: boolean;
begin
  if (FAnimating) then
  begin
    //  FinishLastAnimationFast
    storAnimated := FAnimated;
    FAnimated     := False;
    TimerAnimateSize(self);
    FAnimated := storAnimated;
  end;

  // Now do animation
  TargetAnimationSize := aTargetSize;

  if (ComponentState * [csLoading, csDesigning] = []) and FAnimated then
    begin
    Timer.Enabled := True;
    Timer.OnTimer := @TimerAnimateSize;
    //EndProcedureOfAnimation := nil; //On Collapse then EndTimerCollapse never Executed
    end
  else
    begin
    TimerAnimateSize(self);
    TimerAnimateSize(self);
    end;
end;

procedure TBCExpandPanel.SetTextAlignment(AValue: TAlignment);
begin
  if FTextAlignment=AValue then Exit;
  FTextAlignment:=AValue;
  Invalidate;
end;

procedure TBCExpandPanel.DoCollapse;
var
   i :Integer;
   curControl: TControl;

begin
  (* may work but is irrilevant because TSpeedButton is always on Bottom ????why?
  i :=0;
  while (i < ControlCount) do
  begin
    curControl :=Controls[i];

    if not(curControl is TBCBoundButton) then
    begin
      Self.SetChildZPosition(curControl, 0);
     end;

    inc(i)
  end;*)

  if assigned(OnPreCollapse) then
    OnPreCollapse(self);

  //FButton.Color := FCollapsedButtonColor;

  EndProcedureOfAnimation := @EndTimerCollapse;


  Animate(FButtonSize);

  {$ifopt D+}
  debugln('TBCExpandPanel.DoCollapse');
  debugln('FButtonSize ' + IntToStr(FButtonSize));
  {$endif}

end;

procedure TBCExpandPanel.DoExpand;
begin
  if assigned(OnPreExpand) then
    OnPreExpand(self);

  //  FButton.ControlStyle := FButton.ControlStyle + [csNoFocus, csNoDesignSelectable];
  //  FButton.Parent:=self;

  //FButton.Color := FExpandedButtonColor;

  EndProcedureOfAnimation := @EndTimerExpand;

  Animate(FExpandedSize);

  {$ifopt D+}
  debugln('TBCExpandPanel.DoExpand');
  debugln('FExpandedSize ' + IntToStr(FExpandedSize));
  {$endif}
end;

procedure TBCExpandPanel.AdjustClientRect(var ARect: TRect);
begin
  inherited AdjustClientRect(ARect);

  if Assigned(FButton) then
    case ButtonPosition of
      akTop:
        ARect.Top    := ARect.Top + fButton.Height;
      akBottom:
        ARect.Bottom := ARect.Bottom - fButton.Height;
      akLeft:
        ARect.Left   := ARect.Left + fButton.Width;
      akRight:
        ARect.Right  := ARect.Right - fButton.Width;
      end;
end;

procedure TBCExpandPanel.Loaded;
begin
     inherited Loaded;
end;

procedure TBCExpandPanel.CreateWnd;
begin
  inherited CreateWnd;

  FButton.BuildGlyphs; //Button Loaded is called Before Self.Loaded and cannot Build Glyphs

(*  if (FCollapsed)
  then SetRelevantSize(Self, FButtonPosition, FButtonSize)
  else SetRelevantSize(Self, FButtonPosition, FExpandedSize); *)

  PositionButton;
end;

procedure TBCExpandPanel.Paint;
var
  ARect: TRect;
  TS: TTextStyle;

begin
  if not(FCollapsed) then
  begin
    ARect := GetClientRect;
    Case FButtonPosition of
    akTop: inc(ARect.Top, FButtonSize);
    akBottom: dec(ARect.Bottom, FButtonSize);
    akLeft: inc(ARect.Left, FButtonSize);
    akRight: dec(ARect.Right, FButtonSize);
    end;

    {$ifdef DEBUG_PAINT}
      Canvas.Brush.Color:=clRed;
      Canvas.Brush.Style:=bsSolid;
      Canvas.FillRect(ARect);
    {$endif}

    // if BevelOuter is set then draw a frame with BevelWidth
    if (BevelOuter <> bvNone)
    then if rBevelRounded
         then Frame3d_Rounded(Self.Canvas, ARect, BevelWidth, 5, 5, BevelOuter,
                              rBevelColorShadow, rBevelColorHighlight, Color)
         else Self.Canvas.Frame3d(ARect, BevelWidth, BevelOuter);

    InflateRect(ARect, -BorderWidth, -BorderWidth);

    // if BevelInner is set then skip the BorderWidth and draw a frame with BevelWidth
    if (BevelInner <> bvNone)
    then if rBevelRounded
         then Frame3d_Rounded(Self.Canvas, ARect, BevelWidth, 5, 5, BevelInner,
                              rBevelColorShadow, rBevelColorHighlight, Color)
         else Self.Canvas.Frame3d(ARect, BevelWidth, BevelInner);

    if (Self.Caption <> '') then
    begin
      TS := Canvas.TextStyle;
      TS.Alignment := BidiFlipAlignment(Self.TextAlignment, UseRightToLeftAlignment);
      if (BiDiMode <> bdLeftToRight)
      then TS.RightToLeft:= True;
      TS.Layout:= Graphics.tlCenter;
      TS.Opaque:= false;
      TS.Clipping:= false;
      TS.SystemFont:=Canvas.Font.IsDefault;
      if not(Enabled) then
      begin
        Canvas.Font.Color := clBtnHighlight;
        Types.OffsetRect(ARect, 1, 1);
        Self.Canvas.TextRect(ARect, ARect.Left, ARect.Top, Self.Caption, TS);
        Self.Canvas.Font.Color := clBtnShadow;
        Types.OffsetRect(ARect, -1, -1);
       end
      else Self.Canvas.Font.Color := Font.Color;

      Self.Canvas.TextRect(ARect,ARect.Left,ARect.Top, Self.Caption, TS);
    end;
  end;
end;

constructor TBCExpandPanel.Create(TheOwner: TComponent);
begin
  StopCircleActions := True;

  inherited;

  FButtonSize := 27;
  FAnimated := True;
  FCollapseKind := akTop;
  FVisibleTotal := True;
  FCollapsed := False;
  FButtonPosition := akTop;
  FCollapsedButtonColor := clSkyBlue;
  FExpandedButtonColor := RGBToColor(23, 136, 248);
  rBevelColorHighlight:=clBtnHighlight;
  rBevelColorShadow:=clBtnShadow;
  rBevelRounded:=True;
  FExpandedSize := 200;
  Height  := FExpandedSize;
  Width   := 200;
  FAnimationSpeed := 20;
  Caption := '';

  Timer      := TTimer.Create(self);
  Timer.Enabled := False;
  Timer.Name := 'Animationtimer';
  Timer.Interval := 20;

  FButton := TBCBoundButton.Create(self);
  with FButton do
    begin
    Parent  := self;
    Name    := 'Button';
    Caption := 'Caption';
    ControlStyle := ControlStyle + [csNoFocus, csNoDesignSelectable];
    FButton.OnClick := @self.ButtonClick;
    end;

  StopCircleActions := False;

  //may be only in CreateWnd but the button is greater by some pixels
  PositionButton;
end;



destructor TBCExpandPanel.Destroy;
begin
  timer.Enabled := False;

  Timer.Free;

  if (ComponentState * [csLoading, csDesigning] = []) then
    FButton.Free;  // bringt einen Fehler in der Designtime wenn ich das hier mache

  //  FButton.Free;  // bringt einen Fehler in der Designtime wenn ich das hier mache

  inherited Destroy;
end;


{$IFDEF FPC}
procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCExpandPanel, TBCExpandPanels]);
end;
{$ENDIF}

initialization
   {$i BCExpandPanels.lrs}

end.
