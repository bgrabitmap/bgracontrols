//by Fox

unit BGRAResizeSpeedButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Buttons, LResources, Forms,
  Controls, Graphics, Dialogs, BGRASpeedButton, BGRABitmap;

type
  TBGRAResizeSpeedButton = class(TBGRASpeedButton)
  private
    { Private declarations }
    FBGRA: TBGRABitmap;
  protected
    { Protected declarations }
    function DrawGlyph(ACanvas: TCanvas; const AClient: TRect;
      const AOffset: TPoint; AState: TButtonState; ATransparent: boolean;
      BiDiFlags: longint): TRect; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

function TBGRAResizeSpeedButton.DrawGlyph(ACanvas: TCanvas;
  const AClient: TRect; const AOffset: TPoint; AState: TButtonState;
  ATransparent: boolean; BiDiFlags: longint): TRect;

begin
  if Glyph = nil then
    Exit;
  Result := AClient;
  if Assigned(Glyph) and not Glyph.Empty then
  begin
    FBGRA.Assign(Glyph);
    BGRAReplace(FBGRA, FBGRA.Resample(Self.Width - 6, Self.Height - 6));
    if (AState = bsDown) or (Down = True) then
      FBGRA.Draw(ACanvas, 4, 4, False)
    else
      FBGRA.Draw(ACanvas, 3, 3, False);
  end;
end;

constructor TBGRAResizeSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBGRA := TBGRABitmap.Create;
end;

destructor TBGRAResizeSpeedButton.Destroy;
begin
  FBGRA.Free;
  inherited Destroy;
end;

procedure Register;
begin
  {$I icons\bgraresizespeedbutton_icon.lrs}
  RegisterComponents('BGRA Controls', [TBGRAResizeSpeedButton]);
end;

end.
