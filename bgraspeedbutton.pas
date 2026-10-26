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
  Classes, SysUtils, {$IFDEF FPC}LResources, LCLVersion,{$ENDIF} Forms, Controls, Graphics, Dialogs, Buttons, BGRABitmap,
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
  RegisterComponents('BGRA Button Controls', [TBGRASpeedButton]);
end;
{$ENDIF}

{$IFDEF BGRA_DRAW}
{ TBGRASpeedButton }

function TBGRASpeedButton.DrawGlyph(ACanvas: TCanvas; const AClient: TRect;
  const AOffset: TPoint; AState: TButtonState; ATransparent: boolean;
  BiDiFlags: longint): TRect;
var
  GlyphSize: TSize;
  ImageCount, UnscaledSize, ScaledSize, NewWidth, NewHeight: Integer;
begin
  // Image lists use the LCL renderer for state images, effects and DPI.
  if Assigned(Images) then
  begin
    Result := inherited DrawGlyph(ACanvas, AClient, AOffset, AState,
      ATransparent, BiDiFlags);
    Exit;
  end;

  if not Assigned(Glyph) then
    begin
      Result := Rect(0,0,0,0);
      Exit;
    end;
  { It's not good solution assigning glyph on each draw call but FGlyph and SetGlyph is
    in private section }
  FBGRA.Assign(Glyph);

  { LCL computes the glyph offset from its DPI-scaled size, so the bitmap must be
    drawn at that same size. Otherwise the icon is smaller than expected and is
    placed at the top-left instead of being centered. }
  if (Glyph.Width > 0) and (Glyph.Height > 0) then
  begin
    ImageCount := NumGlyphs;
    if ImageCount < 1 then
      ImageCount := 1;
    GlyphSize := GetGlyphSize(True, AClient);
    if (GlyphSize.cx > 0) and (GlyphSize.cy > 0) then
    begin
      UnscaledSize := Glyph.Width div ImageCount;
      ScaledSize := GlyphSize.cx;
      {$IF LCL_FULLVERSION >= 5000000}
      if GlyphImageLayout = gilVertical then
      begin
        UnscaledSize := Glyph.Height div ImageCount;
        ScaledSize := GlyphSize.cy;
      end;
      {$ENDIF}
      if UnscaledSize > 0 then
      begin
        NewWidth := Glyph.Width * ScaledSize div UnscaledSize;
        NewHeight := Glyph.Height * ScaledSize div UnscaledSize;
        if (NewWidth > 0) and (NewHeight > 0) and
          ((NewWidth <> Glyph.Width) or (NewHeight <> Glyph.Height)) then
          BGRAReplace(FBGRA, FBGRA.Resample(NewWidth, NewHeight));
      end;
    end;
  end;

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
