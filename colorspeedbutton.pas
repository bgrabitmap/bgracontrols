unit ColorSpeedButton;

{$mode objfpc}{$H+}

{$ifdef windows}
{$define overridepaint}
{$endif}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, BGRASpeedButton, Types, LCLType, Themes, Math, LCLProc;

type

  { TColorState }

  TColorState = class(TPersistent)
  private
    FOwner: TControl;
    FBorderColor: TColor;
    FBorderWidth: integer;
    FColor: TColor;
    procedure SetFBorderColor(AValue: TColor);
    procedure SetFBorderWidth(AValue: integer);
    procedure SetFColor(AValue: TColor);
  public
    constructor Create(AOwner: TControl);
  published
    property Color: TColor read FColor write SetFColor;
    property BorderColor: TColor read FBorderColor write SetFBorderColor;
    property BorderWidth: integer read FBorderWidth write SetFBorderWidth;
  end;

  { TColorSpeedButton }

  TColorSpeedButton = class(TBGRASpeedButton)
  private
    FLastDrawDetails: TThemedElementDetails;
    FPopupMode: boolean;
    FStateActive: TColorState;
    FStateDisabled: TColorState;
    FStateHover: TColorState;
    FStateNormal: TColorState;
    procedure SetFPopupMode(AValue: boolean);
    procedure SetFStateActive(AValue: TColorState);
    procedure SetFStateDisabled(AValue: TColorState);
    procedure SetFStateHover(AValue: TColorState);
    procedure SetFStateNormal(AValue: TColorState);
  protected
    {$ifdef overridepaint}
    procedure DrawText(ACanvas: TPersistent; Details: TThemedElementDetails;
      const S: string; R: TRect; Flags, Flags2: cardinal);
    procedure MeasureDraw(Draw: boolean; PaintRect: TRect;
      out PreferredWidth, PreferredHeight: integer);
    procedure Paint; override;
    {$endif}
    procedure PaintBackground(var PaintRect: TRect); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property PopupMode: boolean read FPopupMode write SetFPopupMode;
    property StateNormal: TColorState read FStateNormal write SetFStateNormal;
    property StateHover: TColorState read FStateHover write SetFStateHover;
    property StateActive: TColorState read FStateActive write SetFStateActive;
    property StateDisabled: TColorState read FStateDisabled write SetFStateDisabled;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BGRA Controls', [TColorSpeedButton]);
end;

{ TColorSpeedButton }

procedure TColorSpeedButton.SetFStateActive(AValue: TColorState);
begin
  if FStateActive = AValue then
    Exit;
  FStateActive := AValue;
  Invalidate;
end;

procedure TColorSpeedButton.SetFPopupMode(AValue: boolean);
begin
  if FPopupMode = AValue then
    Exit;
  FPopupMode := AValue;
end;

procedure TColorSpeedButton.SetFStateDisabled(AValue: TColorState);
begin
  if FStateDisabled = AValue then
    Exit;
  FStateDisabled := AValue;
  Invalidate;
end;

procedure TColorSpeedButton.SetFStateHover(AValue: TColorState);
begin
  if FStateHover = AValue then
    Exit;
  FStateHover := AValue;
  Invalidate;
end;

procedure TColorSpeedButton.SetFStateNormal(AValue: TColorState);
begin
  if FStateNormal = AValue then
    Exit;
  FStateNormal := AValue;
  Invalidate;
end;

{$ifdef overridepaint}
procedure TColorSpeedButton.DrawText(ACanvas: TPersistent;
  Details: TThemedElementDetails; const S: string; R: TRect; Flags,
  Flags2: cardinal);
var
  TXTStyle: TTextStyle;
begin
  TXTStyle := Canvas.TextStyle;
  TXTStyle.Opaque := False;
  TXTStyle.Clipping := (Flags and DT_NOCLIP) = 0;
  TXTStyle.ShowPrefix := (Flags and DT_NOPREFIX) = 0;
  TXTStyle.SingleLine := (Flags and DT_SINGLELINE) <> 0;

  if (Flags and DT_CENTER) <> 0 then
    TXTStyle.Alignment := taCenter
  else
  if (Flags and DT_RIGHT) <> 0 then
    TXTStyle.Alignment := taRightJustify
  else
    TXTStyle.Alignment := taLeftJustify;

  if (Flags and DT_VCENTER) <> 0 then
    TXTStyle.Layout := tlCenter
  else
  if (Flags and DT_BOTTOM) <> 0 then
    TXTStyle.Layout := tlBottom
  else
    TXTStyle.Layout := tlTop;
  TXTStyle.RightToLeft := (Flags and DT_RTLREADING) <> 0;
    // set color here, otherwise SystemFont is wrong if the button was disabled before
  TXTStyle.SystemFont := Canvas.Font.IsDefault;//Match System Default Style

  TXTStyle.Wordbreak := (Flags and DT_WORDBREAK) <> 0;
  if not TXTStyle.Wordbreak then
    TXTStyle.EndEllipsis := (Flags and DT_END_ELLIPSIS) <> 0
  else
    TXTStyle.EndEllipsis := False;
  Canvas.TextRect(R, R.Left, R.Top, S, TXTStyle);
end;

procedure TColorSpeedButton.MeasureDraw(Draw: boolean; PaintRect: TRect;
  out PreferredWidth, PreferredHeight: integer);
var
  GlyphWidth, GlyphHeight: integer;
  Offset, OffsetCap: TPoint;
  ClientSize, TotalSize, TextSize, GlyphSize: TSize;
  M, S: integer;
  SIndex: longint;
  TMP: string;
  TextFlags: integer;
  DrawDetails: TThemedElementDetails;
  FixedWidth: boolean;
  FixedHeight: boolean;
  TextRect: TRect;
  HasGlyph: boolean;
  HasText: boolean;
  CurLayout: TButtonLayout;
begin
  if Glyph = nil then
    exit;

  DrawDetails := GetDrawDetails;

  PreferredWidth := 0;
  PreferredHeight := 0;

  if Draw then
  begin
    FLastDrawDetails:=DrawDetails;
    PaintBackground(PaintRect);
    FixedWidth := True;
    FixedHeight := True;
  end
  else
  begin
    FixedWidth := WidthIsAnchored;
    FixedHeight := HeightIsAnchored;
  end;
  ClientSize.cx := PaintRect.Right - PaintRect.Left;
  ClientSize.cy := PaintRect.Bottom - PaintRect.Top;
  //debugln(['TCustomSpeedButton.MeasureDraw Step1 ',DbgSName(Self),' PaintRect=',dbgs(PaintRect)]);
  // compute size of glyph
  GlyphSize := GetGlyphSize(Draw, PaintRect);
  GlyphWidth := GlyphSize.CX;
  if NumGlyphs > 1 then
    GlyphWidth := GlyphWidth div NumGlyphs;
  GlyphHeight := GlyphSize.CY;
  HasGlyph := (GlyphWidth <> 0) and (GlyphHeight <> 0);
  //debugln(['TCustomSpeedButton.MeasureDraw Step2 ',DbgSName(Self),' PaintRect=',dbgs(PaintRect),' GlyphSize=',GlyphWidth,'x',GlyphHeight]);

  // compute size of text
  CurLayout := BidiAdjustButtonLayout(UseRightToLeftReading, Layout);
  if ShowCaption and (Caption <> '') then
  begin
    TextRect := PaintRect;
    // for wordbreak compute the maximum size for the text
    if Margin > 0 then
      InflateRect(TextRect, -Margin, -Margin);
    if HasGlyph then
    begin
      if (Spacing >= 0) then
        if CurLayout in [blGlyphLeft, blGlyphRight] then
          Dec(TextRect.Right, Spacing)
        else
          Dec(TextRect.Bottom, Spacing);
      if CurLayout in [blGlyphLeft, blGlyphRight] then
        Dec(TextRect.Right, GlyphWidth)
      else
        Dec(TextRect.Bottom, GlyphHeight);
    end;
    if not FixedWidth then
    begin
      TextRect.Left := 0;
      TextRect.Right := High(TextRect.Right) div 2;
    end;
    if not FixedHeight then
    begin
      TextRect.Top := 0;
      TextRect.Bottom := High(TextRect.Bottom) div 2;
    end;
    TextSize := GetTextSize(Draw, TextRect);
  end
  else
  begin
    TextSize.cx := 0;
    TextSize.cy := 0;
  end;
  HasText := (TextSize.cx <> 0) or (TextSize.cy <> 0);

  if Caption <> '' then
  begin
    TMP := Caption;
    SIndex := DeleteAmpersands(TMP);
    if SIndex > 0 then
      if SIndex <= Length(TMP) then
      begin
        //FShortcut := Ord(TMP[SIndex]);
      end;
  end;

  if HasGlyph and HasText then
    S := Spacing
  else
    S := 0;
  M := Margin;
  if not Draw then
  begin
    if M < 0 then
      M := 2;
    if S < 0 then
      S := M;
  end;

  // Calculate caption and glyph layout
  if M = -1 then
  begin
    // auto compute margin to center content
    if S = -1 then
    begin
      // use the same value for Spacing and Margin
      TotalSize.cx := TextSize.cx + GlyphWidth;
      TotalSize.cy := TextSize.cy + GlyphHeight;
      if Layout in [blGlyphLeft, blGlyphRight] then
        M := (ClientSize.cx - TotalSize.cx) div 3
      else
        M := (ClientSize.cy - TotalSize.cy) div 3;
      S := M;
    end
    else
    begin
      // fixed Spacing and center content
      TotalSize.cx := GlyphWidth + S + TextSize.cx;
      TotalSize.cy := GlyphHeight + S + TextSize.cy;
      if Layout in [blGlyphLeft, blGlyphRight] then
        M := (ClientSize.cx - TotalSize.cx) div 2
      else
        M := (ClientSize.cy - TotalSize.cy) div 2;
    end;
  end
  else
  begin
    // fixed Margin
    if S = -1 then
    begin
      // use the rest for Spacing between Glyph and Caption
      TotalSize.cx := ClientSize.cx - (Margin + GlyphWidth);
      TotalSize.cy := ClientSize.cy - (Margin + GlyphHeight);
      if Layout in [blGlyphLeft, blGlyphRight] then
        S := (TotalSize.cx - TextSize.cx) div 2
      else
        S := (TotalSize.cy - TextSize.cy) div 2;
    end;
  end;

  //debugln(['TCustomSpeedButton.MeasureDraw Step3 ',DbgSName(Self),' PaintRect=',dbgs(PaintRect),' GlyphSize=',GlyphWidth,'x',GlyphHeight,' TextSize=',TextSize.cx,'x',TextSize.cy,' S=',S,' M=',M]);

  if Draw then
  begin
    case CurLayout of
      blGlyphLeft:
      begin
        Offset.X := M;
        Offset.Y := (ClientSize.cy - GlyphHeight) div 2;
        OffsetCap.X := Offset.X + GlyphWidth + S;
        OffsetCap.Y := (ClientSize.cy - TextSize.cy) div 2;
      end;
      blGlyphRight:
      begin
        Offset.X := ClientSize.cx - M - GlyphWidth;
        Offset.Y := (ClientSize.cy - GlyphHeight) div 2;
        OffsetCap.X := Offset.X - S - TextSize.cx;
        OffsetCap.Y := (ClientSize.cy - TextSize.cy) div 2;
      end;
      blGlyphTop:
      begin
        Offset.X := (ClientSize.cx - GlyphWidth) div 2;
        Offset.Y := M;
        OffsetCap.X := (ClientSize.cx - TextSize.cx) div 2;
        OffsetCap.Y := Offset.Y + GlyphHeight + S;
      end;
      blGlyphBottom:
      begin
        Offset.X := (ClientSize.cx - GlyphWidth) div 2;
        Offset.Y := ClientSize.cy - M - GlyphHeight;
        OffsetCap.X := (ClientSize.cx - TextSize.cx) div 2;
        OffsetCap.Y := Offset.Y - S - TextSize.cy;
      end;
    end;

    DrawGlyph(Canvas, PaintRect, Offset, FState, Transparent, 0);

    if ShowCaption and (Caption <> '') then
    begin
      with PaintRect, OffsetCap do
      begin
        Left := Left + X;
        Top := Top + Y;
      end;

      TextFlags := DT_LEFT or DT_TOP;
      if UseRightToLeftReading then
        TextFlags := TextFlags or DT_RTLREADING;

      if Draw then
        DrawText(Canvas, DrawDetails, Caption, PaintRect,
          TextFlags, 0);
    end;
  end
  else
  begin
    // measuring, not drawing
    case CurLayout of
      blGlyphLeft, blGlyphRight:
      begin
        PreferredWidth := 2 * M + S + GlyphWidth + TextSize.cx;
        PreferredHeight := 2 * M + Max(GlyphHeight, TextSize.cy);
      end;
      blGlyphTop, blGlyphBottom:
      begin
        PreferredWidth := 2 * M + Max(GlyphWidth, TextSize.cx);
        PreferredHeight := 2 * M + S + GlyphHeight + TextSize.cy;
      end;
    end;
  end;
end;

procedure TColorSpeedButton.Paint;
var
  PaintRect: TRect;
  PreferredWidth: integer;
  PreferredHeight: integer;
begin
  UpdateState(False);
  if Glyph = nil then
    exit;

  PaintRect := ClientRect;
  MeasureDraw(True, PaintRect, PreferredWidth, PreferredHeight);

  if Assigned(OnPaint) then
    OnPaint(Self);
end;

{$endif}

procedure TColorSpeedButton.PaintBackground(var PaintRect: TRect);
begin
  case FState of
    bsUp:
    begin
      Canvas.Pen.Color := FStateNormal.BorderColor;
      Canvas.Pen.Width := FStateNormal.BorderWidth;
      Canvas.Brush.Color := FStateNormal.Color;
    end;
    bsDisabled:
    begin
      Canvas.Pen.Color := FStateDisabled.BorderColor;
      Canvas.Pen.Width := FStateDisabled.BorderWidth;
      Canvas.Brush.Color := FStateDisabled.Color;
    end;
    bsDown, bsExclusive:
    begin
      Canvas.Pen.Color := FStateActive.BorderColor;
      Canvas.Pen.Width := FStateActive.BorderWidth;
      Canvas.Brush.Color := FStateActive.Color;
    end;
    bsHot:
    begin
      Canvas.Pen.Color := FStateHover.BorderColor;
      Canvas.Pen.Width := FStateHover.BorderWidth;
      Canvas.Brush.Color := FStateHover.Color;
    end;
  end;
  if Canvas.Pen.Width = 0 then
    Canvas.Pen.Color := Canvas.Brush.Color;
  Canvas.Rectangle(PaintRect);
end;

constructor TColorSpeedButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FStateNormal := TColorState.Create(Self);
  FStateHover := TColorState.Create(Self);
  FStateActive := TColorState.Create(Self);
  FStateDisabled := TColorState.Create(Self);
  { Windows Style }
  FStateNormal.Color := RGBToColor(225, 225, 225);
  FStateNormal.BorderColor := RGBToColor(173, 173, 173);
  FStateHover.Color := RGBToColor(229, 241, 251);
  FStateHover.BorderColor := RGBToColor(0, 120, 215);
  FStateActive.Color := RGBToColor(204, 228, 247);
  FStateActive.BorderColor := RGBToColor(0, 84, 153);
  FStateDisabled.Color := RGBToColor(204, 204, 204);
  FStateDisabled.Color := RGBToColor(191, 191, 191);
  Font.Color := clBlack;
end;

destructor TColorSpeedButton.Destroy;
begin
  FStateNormal.Free;
  FStateHover.Free;
  FStateActive.Free;
  FStateDisabled.Free;
  inherited Destroy;
end;

procedure TColorSpeedButton.Click;
var
  p: TPoint;
begin
  if PopupMode then
  begin
    p := Parent.ClientToScreen(Point(Left, Top));
    PopupMenu.PopUp(p.x, p.y + Height);
  end;
  inherited Click;
end;

{ TColorState }

procedure TColorState.SetFBorderColor(AValue: TColor);
begin
  if FBorderColor = AValue then
    Exit;
  FBorderColor := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TColorState.SetFBorderWidth(AValue: integer);
begin
  if FBorderWidth = AValue then
    Exit;
  FBorderWidth := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

procedure TColorState.SetFColor(AValue: TColor);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;

  FOwner.Perform(CM_CHANGED, 0, 0);
  FOwner.Invalidate;
end;

constructor TColorState.Create(AOwner: TControl);
begin
  inherited Create;
  FOwner := AOwner;
  BorderWidth := 1;
  BorderColor := clBlack;
  Color := clWhite;
end;

end.
