// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ This component partialy solve problem with no alpha in lazarus GTK.
  It is using BGRABitmap library for drawing icons.

  originally written in 2011 by Krzysztof Dibowski dibowski at interia.pl
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BGRASpeedButton;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Forms, Controls, Graphics, Dialogs, Buttons, BGRABitmap,
  {$IFNDEF FPC}Types, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BGRABitmapTypes;

{$IFDEF LCLgtk}
  {$DEFINE BGRA_DRAW}
{$ELSE}
  {$IFDEF LCLgtk2}
    {$DEFINE BGRA_DRAW}
  {$ENDIF}
{$ENDIF}

type

  { TBGRASpeedButton }

  TBGRASpeedButton = class(TSpeedButton)
  private
    { Private declarations }
    {$IFDEF BGRA_DRAW}
    FBGRA: TBGRABitmap;
    {$ENDIF}
  protected
    { Protected declarations }
    {$IFDEF BGRA_DRAW}
    function DrawGlyph(ACanvas: TCanvas; const AClient: TRect;
      const AOffset: TPoint; AState: TButtonState; {%H-}ATransparent: boolean;
      {%H-}BiDiFlags: longint): TRect; override;
    {$ENDIF}
  public
    { Public declarations }
    {$IFDEF BGRA_DRAW}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$ENDIF}
  published
    { Published declarations }
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

{$IFDEF FPC}
procedure Register;
begin
  //{$I icons\bgraspeedbutton_icon.lrs}
  RegisterComponents('BGRA Button Controls', [TBGRASpeedButton]);
end;
{$ENDIF}

{$IFDEF BGRA_DRAW}
{ TBGRASpeedButton }

function TBGRASpeedButton.DrawGlyph(ACanvas: TCanvas; const AClient: TRect;
  const AOffset: TPoint; AState: TButtonState; ATransparent: boolean;
  BiDiFlags: longint): TRect;
begin
  {*** We are using BGRABitmap drawing only ***}
  {Result := inherited DrawGlyph(ACanvas, AClient, AOffset, AState,
    ATransparent, BiDiFlags); }

  if not Assigned(Glyph) then
    begin
      Result := Rect(0,0,0,0);
      Exit;
    end;
  { It's not good solution assigning glyph on each draw call but FGlyph and SetGlyph is
    in private section }
  FBGRA.Assign(Glyph);

  if (AState = bsDown) or (Down = True) then
    FBGRA.Draw(ACanvas, AOffset.x + 1, AOffset.y + 1, False)
  else
    FBGRA.Draw(ACanvas, AOffset.x, AOffset.y, False);

  Result := AClient;
end;

constructor TBGRASpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBGRA := TBGRABitmap.Create;
end;

destructor TBGRASpeedButton.Destroy;
begin
  FBGRA.Free;
  inherited Destroy;
end;

{$ENDIF}

end.
