{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCListBox;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LCLType, LResources, {$ENDIF}
  Forms, Controls, Graphics, Dialogs, StdCtrls,
  {$IFNDEF FPC}Types, BGRAGraphics, GraphType, FPImage, BCBaseCtrls,{$ENDIF}
  BGRAVirtualScreen, BGRABitmap, BGRASliceScaling;

type

  TBCListBox = class;
  TBCPaperPanel = class;

  { TBCPaperPanel }

  TBCPaperPanel = class(TBGRAVirtualScreen)
  private
    FShadow: TBGRASliceScaling;
    procedure LoadShadowFromBitmapResource;
  protected
    procedure BCRedraw(Sender: TObject; ABitmap: TBGRABitmap);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  { TBCListBox }

  TBCListBox = class(TListBox)
  private
    { Private declarations }
  protected
    procedure BCDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
  published
    { Published declarations }
  end;

  { TBCPaperListBox }

  TBCPaperListBox = class(TBCPaperPanel)
  private
    FListBox: TBCListBox;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property ListBox: TBCListBox read FListBox write FListBox;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

{$IFDEF FPC}
uses
  PropEdits;
{$ENDIF}

{$IFDEF FPC}
procedure Register;
begin
  RegisterComponents('BGRA Controls', [TBCListBox]);
  RegisterComponents('BGRA Controls', [TBCPaperPanel]);
  RegisterComponents('BGRA Controls', [TBCPaperListBox]);
{$IFDEF FPC}//#
  RegisterPropertyEditor(TypeInfo(TBCListBox),
    TBCPaperListBox, 'ListBox', TClassPropertyEditor);
{$ENDIF}
end;
{$ENDIF}

{ TBCPaperListBox }

constructor TBCPaperListBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Self.ChildSizing.ControlsPerLine := 1;
  Self.ChildSizing.LeftRightSPacing := 4;
  Self.ChildSizing.TopBottomSpacing := 5;
  FListBox := TBCListBox.Create(Self);
  FListBox.Align := alClient;
  FListBox.Parent := Self;
  FListBox.SetSubComponent(true);
end;

{ TBCPaperListBox }

procedure TBCPaperPanel.LoadShadowFromBitmapResource;
{$IFDEF FPC}
var
  res: TLazarusResourceStream;
{$ENDIF}
begin
{$IFDEF FPC}
  res := TLazarusResourceStream.Create('SHADOW', nil);
  FShadow := TBGRASliceScaling.Create(res);
  FShadow.Margins := Margins(6, 9, 6, 9);
  res.Free;
{$ENDIF}
end;

procedure TBCPaperPanel.BCRedraw(Sender: TObject; ABitmap: TBGRABitmap);
begin
  FShadow.Draw(ABitmap, 0, 0, ABitmap.Width, ABitmap.Height);
end;

constructor TBCPaperPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  LoadShadowFromBitmapResource;
  Self.OnRedraw := BCRedraw;
end;

destructor TBCPaperPanel.Destroy;
begin
  inherited Destroy;
  FShadow.Free;
end;

{ TBCListBox }

procedure TBCListBox.BCDrawItem(Control: TWinControl; Index: integer;
  ARect: TRect; State: TOwnerDrawState);
var
  lb: TListBox;
  hg: integer;
begin
  {$IFDEF FPC}
  lb := TListBox(Control);

  lb.Canvas.Clipping := False;

  if odFocused in State then
    lb.Canvas.Brush.Color := $00e4e4e4
  else
    lb.Canvas.Brush.Color := clWhite;

  if odSelected in State then
    lb.Canvas.Font.Style := [fsBold];

  lb.Canvas.FillRect(ARect);
  hg := lb.Canvas.TextHeight(lb.Items[Index]);
  lb.Canvas.Font.Color := clBlack;
  lb.Canvas.TextOut(ARect.Left + ScaleX(16, 96), ARect.Top +
    (lb.ItemHeight - hg) div 2, lb.Items[Index]);

  lb.Canvas.Clipping := True;
  lb.Canvas.ClipRect := Rect(0, 0, 0, 0);
  {$ENDIF}
end;

constructor TBCListBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Self.Style := lbOwnerDrawFixed;
  Self.OnDrawItem := BCDrawItem;
  {$IFDEF FPC}
  Self.ItemHeight := ScaleY(48, 96);
  {$ENDIF}
  Self.BorderStyle := bsNone;
end;

initialization

  {$IFDEF FPC}
  {$I bcpaperlistbox.lrs}
  {$ENDIF}

end.
