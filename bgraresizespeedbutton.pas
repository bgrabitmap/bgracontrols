// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Created by Fox. Part of BGRA Controls.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BGRAResizeSpeedButton;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, Buttons, {$IFDEF FPC}LResources,{$ENDIF} Forms,
  Controls, Graphics, Dialogs,
  {$IFNDEF FPC}Types, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BGRASpeedButton, BGRABitmap;

type
  TBGRAResizeSpeedButton = class(TBGRASpeedButton)
  private
    { Private declarations }
    FBGRA: TBGRABitmap;
  protected
    { Protected declarations }
    function DrawGlyph(ACanvas: TCanvas; const AClient: TRect;
      const {%H-}AOffset: TPoint; AState: TButtonState; {%H-}ATransparent: boolean;
      {%H-}BiDiFlags: longint): TRect; {$IFDEF FPC}override;{$ENDIF}
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

function TBGRAResizeSpeedButton.DrawGlyph(ACanvas: TCanvas;
  const AClient: TRect; const AOffset: TPoint; AState: TButtonState;
  ATransparent: boolean; BiDiFlags: longint): TRect;

begin
  Result := Rect(0, 0, 0, 0);
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

{$IFDEF FPC}
procedure Register;
begin
  //{$I icons\bgraresizespeedbutton_icon.lrs}
  RegisterComponents('BGRA Button Controls', [TBGRAResizeSpeedButton]);
end;
{$ENDIF}

end.
