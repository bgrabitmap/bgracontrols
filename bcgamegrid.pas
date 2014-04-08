unit BCGameGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BCBaseCtrls, BGRABitmap, BGRABitmapTypes, LCLProc;

type

  TOnRenderControl = procedure(Sender: TObject; Bitmap: TBGRABitmap;
    r: TRect; n, x, y: integer) of object;
  TOnClickControl = procedure(Sender: TObject; n, x, y: integer) of object;

  { TBCCustomGrid }

  TBCCustomGrid = class(TBCGraphicControl)
  private
    FBGRA: TBGRABitmap;
    FGridWidth: integer;
    FGridHeight: integer;
    FBlockWidth: integer;
    FBlockHeight: integer;
    FOnRenderControl: TOnRenderControl;
    FOnClickControl: TOnClickControl;
  private
    procedure SetFBlockHeight(AValue: integer);
    procedure SetFBlockWidth(AValue: integer);
    procedure SetFGridHeight(AValue: integer);
    procedure SetFGridWidth(AValue: integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Click; override;
    procedure DrawControl; override;
    procedure RenderControl; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RenderAndDrawControl;
    property GridWidth: integer read FGridWidth write SetFGridWidth;
    property GridHeight: integer read FGridHeight write SetFGridHeight;
    property BlockWidth: integer read FBlockWidth write SetFBlockWidth;
    property BlockHeight: integer read FBlockHeight write SetFBlockHeight;
    property OnRenderControl: TOnRenderControl
      read FOnRenderControl write FOnRenderControl;
    property OnClickControl: TOnClickControl read FOnClickControl write FOnClickControl;
  published
    { Published declarations }
  end;

  TBCGameGrid = class(TBCCustomGrid)
  published
    property GridWidth;
    property GridHeight;
    property BlockWidth;
    property BlockHeight;
    // Support 'n, x, y'
    property OnRenderControl;
    property OnClickControl;
    // 'Classic' events, to be changed...
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    // Ok...
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I icons\bcgamegrid_icon.lrs}
  RegisterComponents('BGRA Controls', [TBCGameGrid]);
end;

{ TBCCustomGrid }

procedure TBCCustomGrid.SetFBlockHeight(AValue: integer);
begin
  if FBlockHeight = AValue then
    Exit;
  if AValue < 1 then
    FBlockHeight := 1
  else
    FBlockHeight := AValue;

  RenderAndDrawControl;
end;

procedure TBCCustomGrid.SetFBlockWidth(AValue: integer);
begin
  if FBlockWidth = AValue then
    Exit;
  if AValue < 1 then
    FBlockWidth := 1
  else
    FBlockWidth := AValue;

  RenderAndDrawControl;
end;

procedure TBCCustomGrid.SetFGridHeight(AValue: integer);
begin
  if FGridHeight = AValue then
    Exit;
  if AValue < 1 then
    FGridHeight := 1
  else
    FGridHeight := AValue;

  RenderAndDrawControl;
end;

procedure TBCCustomGrid.SetFGridWidth(AValue: integer);
begin
  if FGridWidth = AValue then
    Exit;
  if AValue < 1 then
    FGridWidth := 1
  else
    FGridWidth := AValue;

  RenderAndDrawControl;
end;

procedure TBCCustomGrid.Click;
var
  n, x, y: integer;
  r: TRect;
var
  pos: TPoint;
begin
  if (BlockWidth <= 0) or (BlockHeight <= 0) or (GridWidth <= 0) or
    (GridHeight <= 0) then
    Exit;

  pos := ScreenToClient(Mouse.CursorPos);

  n := 0;

  for y := 0 to GridHeight - 1 do
  begin
    for x := 0 to GridWidth - 1 do
    begin
      r.Left := BlockWidth * x;
      r.Top := BlockHeight * y;
      r.Right := r.Left + BlockWidth;
      r.Bottom := r.Top + BlockHeight;

      if (pos.x >= r.Left) and (pos.x <= r.Right) and (pos.y >= r.Top) and
        (pos.y <= r.Bottom) then
      begin
        //DebugLn(['TControl.Click ',DbgSName(Self)]);
        if Assigned(FOnClickControl) and (Action <> nil) and
          (not CompareMethods(TMethod(Action.OnExecute), TMethod(FOnClickControl))) then
          // the OnClick is set and differs from the Action => call the OnClick
          FOnClickControl(Self, n, x, y)
        else if (not (csDesigning in ComponentState)) and (ActionLink <> nil) then
          ActionLink.Execute(Self)
        else if Assigned(FOnClickControl) then
          FOnClickControl(Self, n, x, y);
      end;

      Inc(n);
    end;
  end;
end;

procedure TBCCustomGrid.DrawControl;
begin
  if FBGRA <> nil then
    FBGRA.Draw(Canvas, 0, 0, False);
end;

procedure TBCCustomGrid.RenderControl;
var
  n, x, y: integer;
  r: TRect;
begin
  if (BlockWidth <= 0) or (BlockHeight <= 0) or (GridWidth <= 0) or
    (GridHeight <= 0) then
    Exit;

  if FBGRA <> nil then
    FreeAndNil(FBGRA);

  FBGRA := TBGRABitmap.Create(Width, Height);

  n := 0;

  for y := 0 to GridHeight - 1 do
  begin
    for x := 0 to GridWidth - 1 do
    begin
      r.Left := BlockWidth * x;
      r.Top := BlockHeight * y;
      r.Right := r.Left + BlockWidth;
      r.Bottom := r.Top + BlockHeight;

      FBGRA.Rectangle(r, BGRA(127, 127, 127, 127), BGRA(255, 255, 255, 127),
        dmDrawWithTransparency);

      if Assigned(FOnRenderControl) then
        FOnRenderControl(Self, FBGRA, r, n, x, y);

      Inc(n);
    end;
  end;
end;

procedure TBCCustomGrid.RenderAndDrawControl;
begin
  RenderControl;
  Invalidate;
end;

constructor TBCCustomGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TBCCustomGrid.Destroy;
begin
  if FBGRA <> nil then
    FreeAndNil(FBGRA);
  inherited Destroy;
end;

end.
