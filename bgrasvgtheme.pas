unit BGRASVGTheme;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRATheme, BGRABitmap, BGRABitmapTypes, BGRASVG;

type

  { TBGRASVGTheme }

  TBGRASVGTheme = class(TBGRATheme)
  private
    FCheckBoxChecked: String;
    FCheckBoxUnchecked: String;
    FRadioButtonChecked: String;
    FRadioButtonUnchecked: String;
    procedure SetCheckBoxChecked(AValue: String);
    procedure SetCheckBoxUnchecked(AValue: String);
    procedure SetRadioButtonChecked(AValue: String);
    procedure SetRadioButtonUnchecked(AValue: String);

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawRadioButton(Caption: string; State: TBGRAThemeButtonState;
    {%H-}Focused: boolean; Checked: boolean; ARect: TRect;
      DestCanvas: TCanvas); override;
    procedure DrawCheckBox(Caption: string; State: TBGRAThemeButtonState;
    {%H-}Focused: boolean; Checked: boolean; ARect: TRect;
      DestCanvas: TCanvas); override;
  published
    property CheckBoxUnChecked: String
      read FCheckBoxUnchecked write SetCheckBoxUnchecked;
    property CheckBoxChecked: String read FCheckBoxChecked
      write SetCheckBoxChecked;
    property RadioButtonUnchecked: String
      read FRadioButtonUnchecked write SetRadioButtonUnchecked;
    property RadioButtonChecked: String
      read FRadioButtonChecked write SetRadioButtonChecked;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Themes', [TBGRASVGTheme]);
end;

{ TBGRASVGTheme }

procedure TBGRASVGTheme.SetCheckBoxUnchecked(AValue: String);
begin
  if FCheckBoxUnchecked = AValue then
    Exit;
  FCheckBoxUnchecked := AValue;
end;

procedure TBGRASVGTheme.SetRadioButtonChecked(AValue: String);
begin
  if FRadioButtonChecked = AValue then
    Exit;
  FRadioButtonChecked := AValue;
end;

procedure TBGRASVGTheme.SetRadioButtonUnchecked(AValue: String);
begin
  if FRadioButtonUnchecked = AValue then
    Exit;
  FRadioButtonUnchecked := AValue;
end;

constructor TBGRASVGTheme.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // https://material.io/resources/icons/
  FCheckBoxUnchecked :=
    '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M19 5v14H5V5h14m0-2H5c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h14c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2z"/></svg>';
  FCheckBoxChecked :=
    '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M19 3H5c-1.11 0-2 .9-2 2v14c0 1.1.89 2 2 2h14c1.11 0 2-.9 2-2V5c0-1.1-.89-2-2-2zm-9 14l-5-5 1.41-1.41L10 14.17l7.59-7.59L19 8l-9 9z"/></svg>';
  FRadioButtonUnchecked :=
    '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.42 0-8-3.58-8-8s3.58-8 8-8 8 3.58 8 8-3.58 8-8 8z"/></svg>';
  FRadioButtonChecked :=
    '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M12 7c-2.76 0-5 2.24-5 5s2.24 5 5 5 5-2.24 5-5-2.24-5-5-5zm0-5C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.42 0-8-3.58-8-8s3.58-8 8-8 8 3.58 8 8-3.58 8-8 8z"/></svg>';
end;

destructor TBGRASVGTheme.Destroy;
begin
  inherited Destroy;
end;

procedure TBGRASVGTheme.DrawRadioButton(Caption: string;
  State: TBGRAThemeButtonState; Focused: boolean; Checked: boolean;
  ARect: TRect; DestCanvas: TCanvas);
var
  Style: TTextStyle;
  Bitmap: TBGRABitmap;
  svg: TBGRASVG;
  aleft, atop, aright, abottom: integer;
begin
  if Checked then
    svg := TBGRASVG.CreateFromString(FRadioButtonChecked)
  else
    svg := TBGRASVG.CreateFromString(FRadioButtonUnchecked);
  Bitmap := TBGRABitmap.Create(ARect.Height, ARect.Height);
  aleft := 0;
  aright := Bitmap.Height;
  atop := 0;
  abottom := Bitmap.Height;
  svg.StretchDraw(Bitmap.Canvas2D, aleft, atop, aright, abottom);
  Bitmap.Draw(DestCanvas, Arect.Left, Arect.Top, False);
  Bitmap.Free;
  svg.Free;

  if Caption <> '' then
  begin
    Style.Alignment := taLeftJustify;
    Style.Layout := tlCenter;
    Style.Wordbreak := True;
    Style.SystemFont := False;
    Style.Clipping := True;
    Style.Opaque := False;
    DestCanvas.TextRect(Rect(Arect.Height, 0, ARect.Right, ARect.Bottom),
      ARect.Height, 0, Caption, Style);
  end;
end;

procedure TBGRASVGTheme.SetCheckBoxChecked(AValue: String);
begin
  if FCheckBoxChecked = AValue then
    Exit;
  FCheckBoxChecked := AValue;
end;

procedure TBGRASVGTheme.DrawCheckBox(Caption: string; State: TBGRAThemeButtonState;
  Focused: boolean; Checked: boolean; ARect: TRect; DestCanvas: TCanvas);
var
  Style: TTextStyle;
  Bitmap: TBGRABitmap;
  svg: TBGRASVG;
  aleft, atop, aright, abottom: integer;
begin
  if Checked then
    svg := TBGRASVG.CreateFromString(FCheckBoxChecked)
  else
    svg := TBGRASVG.CreateFromString(FCheckBoxUnchecked);
  Bitmap := TBGRABitmap.Create(ARect.Height, ARect.Height);
  aleft := 0;
  aright := Bitmap.Height;
  atop := 0;
  abottom := Bitmap.Height;
  svg.StretchDraw(Bitmap.Canvas2D, aleft, atop, aright, abottom);
  Bitmap.Draw(DestCanvas, Arect.Left, Arect.Top, False);
  Bitmap.Free;
  svg.Free;

  if Caption <> '' then
  begin
    Style.Alignment := taLeftJustify;
    Style.Layout := tlCenter;
    Style.Wordbreak := True;
    Style.SystemFont := False;
    Style.Clipping := True;
    Style.Opaque := False;
    DestCanvas.TextRect(Rect(Arect.Height, 0, ARect.Right, ARect.Bottom),
      ARect.Height, 0, Caption, Style);
  end;
end;

end.
