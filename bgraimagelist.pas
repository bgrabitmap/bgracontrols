// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ This component partialy solve problem with no alpha in lazarus GTK.
  It is using BGRABitmap library for drawing icons.

  originally written in 2011 by Krzysztof Dibowski dibowski at interia.pl
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BGRAImageList;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Forms, Controls, Graphics, Dialogs,
  GraphType, BGRABitmap, BGRABitmapTypes, {%H-}ImgList;

{$IFDEF LCLgtk}
  { $DEFINE BGRA_DRAW}
{$ELSE}
  {$IFDEF LCLgtk2}
    { $DEFINE BGRA_DRAW}
  {$ENDIF}
{$ENDIF}

type

  { TBGRAImageList }

  TBGRAImageList = class(TImageList)
  private
    { Private declarations }
    {$IFDEF BGRA_DRAW}
    FBGRA: TBGRABitmap;
    FBmp:  TBitmap;
    {$ENDIF}
  protected
    { Protected declarations }
  public
    { Public declarations }
    {$IFDEF BGRA_DRAW}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Draw(ACanvas: TCanvas; AX, AY, AIndex: integer;
      ADrawingStyle: TDrawingStyle; AImageType: TImageType;
      ADrawEffect: TGraphicsDrawEffect); override;
    {$ENDIF}
  published
    { Published declarations }
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

implementation

{$IFDEF FPC}
procedure Register;
begin
  //{$I icons\bgraimagelist_icon.lrs}
  RegisterComponents('BGRA Controls', [TBGRAImageList]);
end;
{$ENDIF}

{$IFDEF BGRA_DRAW}
{ TBGRAImageList }
constructor TBGRAImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBGRA := TBGRABitmap.Create;
  FBmp  := TBitmap.Create;
end;

destructor TBGRAImageList.Destroy;
begin
  FBGRA.Free;
  FBmp.Free;
  inherited Destroy;
end;

{ Problem with no alpha is only on GTK so on Windows we use default drawing }
procedure TBGRAImageList.Draw(ACanvas: TCanvas; AX, AY, AIndex: integer;
  ADrawingStyle: TDrawingStyle; AImageType: TImageType;
  ADrawEffect: TGraphicsDrawEffect);
begin
  //inherited; - We use TBGRABitmap drawing only

  // This is required part from TImageList.Draw
  if (AIndex < 0) or (AIndex >= Count) then
    Exit;
  ReferenceNeeded;

  {*** BGRA Drawing *** }
  case ADrawEffect of
    gdeDisabled:
    begin
      {$IFDEF FPC}
      GetBitmap(AIndex, FBmp, gdeNormal);
      {$ELSE}
      GetBitmapRaw(AIndex, FBmp, gdeNormal);
      {$ENDIF}
      FBGRA.Assign(FBmp);
      BGRAReplace(FBGRA, FBGRA.FilterGrayscale);
    end;
    else
    begin
      {$IFDEF FPC}
      GetBitmap(AIndex, FBmp, ADrawEffect);
      {$ELSE}
      GetBitmapRaw(AIndex, FBmp, ADrawEffect);
      {$ENDIF}
      FBGRA.Assign(FBmp);
    end;
  end;
  if ADrawingStyle in [dsFocus, dsSelected] then
    FBGRA.ApplyGlobalOpacity(128);
  FBGRA.Draw(ACanvas, AX, AY, False);
end;

{$ENDIF}

end.
