unit BGRASVGTheme;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRATheme, BGRABitmap, BGRABitmapTypes, BGRASVG, BGRASVGType, XMLConf,
  ComponentEditors, PropEdits, Menus, BGRASVGImageList, Math;

const
  DEFAULT_CHECKBOX_TEXT_SPACING = 2;
  DEFAULT_GLYPH_TEXT_SPACING = 6;
  DEFAULT_BUTTON_TEXT_SPACING = 6;

type

  { TBGRASVGTheme }

  TBGRASVGTheme = class(TBGRATheme)
  private
    FButtonTextSpacing: integer;
    FCheckboxTextSpacing: integer;
    FColorizeActiveOp: TBlendOperation;
    FColorizeDisabledOp: TBlendOperation;
    FColorizeHoverOp: TBlendOperation;
    FColorizeNormalOp: TBlendOperation;
    FGlyphTextSpacing: integer;
    FOwner: TComponent;
    FButtonActive: TStringList;
    FButtonHover: TStringList;
    FButtonNormal: TStringList;
    FButtonSliceScalingBottom: integer;
    FButtonSliceScalingLeft: integer;
    FButtonSliceScalingRight: integer;
    FButtonSliceScalingTop: integer;
    FCheckBoxChecked: TStringList;
    FCheckBoxUnchecked: TStringList;
    FColorizeActive: string;
    FColorizeDisabled: string;
    FColorizeHover: string;
    FColorizeNormal: string;
    FRadioButtonChecked: TStringList;
    FRadioButtonUnchecked: TStringList;
    procedure SetButtonActive(AValue: TStringList);
    procedure SetButtonHover(AValue: TStringList);
    procedure SetButtonNormal(AValue: TStringList);
    procedure SetButtonSliceScalingBottom(AValue: integer);
    procedure SetButtonSliceScalingLeft(AValue: integer);
    procedure SetButtonSliceScalingRight(AValue: integer);
    procedure SetButtonSliceScalingTop(AValue: integer);
    procedure SetButtonTextSpacing(AValue: integer);
    procedure SetCheckBoxChecked(AValue: TStringList);
    procedure SetCheckboxTextSpacing(AValue: integer);
    procedure SetCheckBoxUnchecked(AValue: TStringList);
    procedure SetColorizeActive(AValue: string);
    procedure SetColorizeActiveOp(AValue: TBlendOperation);
    procedure SetColorizeDisabled(AValue: string);
    procedure SetColorizeDisabledOp(AValue: TBlendOperation);
    procedure SetColorizeHover(AValue: string);
    procedure SetColorizeHoverOp(AValue: TBlendOperation);
    procedure SetColorizeNormal(AValue: string);
    procedure SetColorizeNormalOp(AValue: TBlendOperation);
    procedure SetGlyphTextSpacing(AValue: integer);
    procedure SetRadioButtonChecked(AValue: TStringList);
    procedure SetRadioButtonUnchecked(AValue: TStringList);
  protected
    procedure LoadTheme(const XMLConf: TXMLConfig);
    procedure SaveTheme(const XMLConf: TXMLConfig);
    procedure CheckEmptyResourceException(const aResource: string);
    procedure SliceScalingDraw(const Source: TBGRASVG;
      const marginLeft, marginTop, marginRight, marginBottom: integer;
      const Dest: TBGRABitmap; DestDPI: integer);
    procedure ColorizeSurface(ASurface: TBGRAThemeSurface; AState: TBGRAThemeButtonState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function PreferredButtonHeight(const hasGlyph: boolean): Integer; override;
    function PreferredButtonWidth(const hasGlyph: boolean): Integer; override;
    procedure DrawButton(Caption: string; State: TBGRAThemeButtonState;
      Focused: boolean; ARect: TRect; ASurface: TBGRAThemeSurface; AImageIndex: Integer = -1; AImageList: TBGRASVGImageList = nil); override;
    procedure DrawRadioButton(Caption: string; State: TBGRAThemeButtonState;
    {%H-}Focused: boolean; Checked: boolean; ARect: TRect;
      ASurface: TBGRAThemeSurface); override;
    procedure DrawCheckBox(Caption: string; State: TBGRAThemeButtonState;
    {%H-}Focused: boolean; Checked: boolean; ARect: TRect;
      ASurface: TBGRAThemeSurface); override;
  public
    // XML File
    procedure SaveToFile(AFileName: string);
    // XML File
    procedure LoadFromFile(AFileName: string);
    // String Stream
    procedure SaveToStream(AStream: TStream);
    // String Stream
    procedure LoadFromStream(AStream: TStream);
    // Resource
    procedure LoadFromResource(AResource: string);
    // Default Theme
    procedure LoadDefaultTheme;
  published
    // Check box unchecked state
    property CheckBoxUnchecked: TStringList read FCheckBoxUnchecked
      write SetCheckBoxUnchecked;
    // Check box checked state
    property CheckBoxChecked: TStringList read FCheckBoxChecked write SetCheckBoxChecked;
    // Radio button unchecked state
    property RadioButtonUnchecked: TStringList
      read FRadioButtonUnchecked write SetRadioButtonUnchecked;
    // Radio button checked state
    property RadioButtonChecked: TStringList
      read FRadioButtonChecked write SetRadioButtonChecked;
    // Spacing between checkbox/radiobutton and its text (in 96 DPI)
    property CheckBoxTextSpacing: integer read FCheckboxTextSpacing write SetCheckboxTextSpacing default DEFAULT_CHECKBOX_TEXT_SPACING;
    // Button normal state
    property ButtonNormal: TStringList read FButtonNormal write SetButtonNormal;
    // Button mouse over state
    property ButtonHover: TStringList read FButtonHover write SetButtonHover;
    // Button pressed state
    property ButtonActive: TStringList read FButtonActive write SetButtonActive;
    // 9-Slice-Scaling margin left
    property ButtonSliceScalingLeft: integer
      read FButtonSliceScalingLeft write SetButtonSliceScalingLeft;
    // 9-Slice-Scaling margin top
    property ButtonSliceScalingTop: integer
      read FButtonSliceScalingTop write SetButtonSliceScalingTop;
    // 9-Slice-Scaling margin right
    property ButtonSliceScalingRight: integer
      read FButtonSliceScalingRight write SetButtonSliceScalingRight;
    // 9-Slice-Scaling margin bottom
    property ButtonSliceScalingBottom: integer
      read FButtonSliceScalingBottom write SetButtonSliceScalingBottom;
    // Spacing between glyph and its text (in 96 DPI)
    property GlyphTextSpacing: integer read FGlyphTextSpacing write SetGlyphTextSpacing default DEFAULT_GLYPH_TEXT_SPACING;
    // Spacing between text and button border (in 96 DPI)
    property ButtonTextSpacing: integer read FButtonTextSpacing write SetButtonTextSpacing default DEFAULT_BUTTON_TEXT_SPACING;
    // CSS Color to tint the normal states, use rgba(0,0,0,0) to disable
    property ColorizeNormal: string read FColorizeNormal write SetColorizeNormal;
    property ColorizeNormalOp: TBlendOperation read FColorizeNormalOp write SetColorizeNormalOp default boTransparent;
    // CSS Color to tint the hover states, use rgba(0,0,0,0) to disable
    property ColorizeHover: string read FColorizeHover write SetColorizeHover;
    property ColorizeHoverOp: TBlendOperation read FColorizeHoverOp write SetColorizeHoverOp default boTransparent;
    // CSS Color to tint the active states, use rgba(0,0,0,0) to disable
    property ColorizeActive: string read FColorizeActive write SetColorizeActive;
    property ColorizeActiveOp: TBlendOperation read FColorizeActiveOp write SetColorizeActiveOp default boTransparent;
    // CSS Color to tint the disabled states, use rgba(0,0,0,0) to disable
    property ColorizeDisabled: string read FColorizeDisabled write SetColorizeDisabled;
    property ColorizeDisabledOp: TBlendOperation read FColorizeDisabledOp write SetColorizeDisabledOp default boTransparent;
  end;

  { TBGRASVGThemeComponentEditor }

  TBGRASVGThemeComponentEditor = class(TBaseComponentEditor)
  private
    FComponent: TBGRASVGTheme;
  public
    constructor Create({%H-}AComponent: TComponent;
      {%H-}ADesigner: TComponentEditorDesigner); override;
    procedure Copy; override;
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetComponent: TComponent; override;
    function GetCustomHint: String; override;
    function GetDesigner: TComponentEditorDesigner; override;
    function GetHook(out Hook: TPropertyEditorHook): boolean; override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    function IsInInlined: Boolean; override;
    procedure PrepareItem({%H-}Index: Integer; const {%H-}AnItem: TMenuItem); override;
    procedure Modified; override;
  end;

procedure Register;

implementation

uses BCTypes, BCTools;

const
  RES_CHECKBOXUNCHECKED =
    '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M19 5v14H5V5h14m0-2H5c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h14c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2z"/></svg>';
  RES_CHECKBOXCHECKED =
    '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M19 3H5c-1.11 0-2 .9-2 2v14c0 1.1.89 2 2 2h14c1.11 0 2-.9 2-2V5c0-1.1-.89-2-2-2zm-9 14l-5-5 1.41-1.41L10 14.17l7.59-7.59L19 8l-9 9z"/></svg>';
  RES_RADIOBUTTONUNCHECKED =
    '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.42 0-8-3.58-8-8s3.58-8 8-8 8 3.58 8 8-3.58 8-8 8z"/></svg>';
  RES_RADIOBUTTONCHECKED =
    '<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24"><path d="M0 0h24v24H0z" fill="none"/><path d="M12 7c-2.76 0-5 2.24-5 5s2.24 5 5 5 5-2.24 5-5-2.24-5-5-5zm0-5C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.42 0-8-3.58-8-8s3.58-8 8-8 8 3.58 8 8-3.58 8-8 8z"/></svg>';
  RES_BUTTON =
  '<svg  xmlns="http://www.w3.org/2000/svg" width="32" height="32" viewBox="0 0 32 32">'+
  '<defs>'+
  '  <linearGradient id="Grad1" x1="0%" y1="0%" x2="0%" y2="100%">'+
  '    <stop style="stop-color:#ffffff;stop-opacity:1" offset="30%"/>'+
  '    <stop style="stop-color:#87cdde;stop-opacity:1" offset="70%"/>'+
  '  </linearGradient>'+
  '</defs>'+
  '  <path style="fill:url(#Grad1);fill-opacity:1;stroke:#002255;stroke-width:1;stroke-opacity:1"'+
  '   d="m9.8,0.5h12.4c5.15,0 9.3,4.15 9.3,9.3v12.4c0,5.15-4.15,9.3-9.3,9.3h-12.4c-5.15, 0-9.3-4.15-9.3-9.3v-12.4c0-5.15 4.15-9.3 9.3-9.3z" />'+
  '</svg>';
  RES_COLORIZENORMAL = 'rgba(0,0,0,0)';
  RES_COLORIZEHOVER = 'rgba(255,255,255,0.5)';
  RES_COLORIZEACTIVE = 'rgba(0,0,0,0.5)';
  RES_COLORIZEDISABLED = 'rgba(127,127,127,0.7)';

procedure Register;
begin
  RegisterComponents('BGRA Themes', [TBGRASVGTheme]);
  RegisterComponentEditor(TBGRASVGTheme, TBGRASVGThemeComponentEditor);
end;

{ TBGRASVGThemeComponentEditor }

constructor TBGRASVGThemeComponentEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  FComponent := TBGRASVGTheme(AComponent);
end;

procedure TBGRASVGThemeComponentEditor.Copy;
begin

end;

procedure TBGRASVGThemeComponentEditor.Edit;
begin

end;

procedure TBGRASVGThemeComponentEditor.ExecuteVerb(Index: Integer);
var
  openDlg: TOpenDialog;
  saveDlg: TSaveDialog;
begin
  case Index of
    // Load from file
    0: begin
      openDlg := TOpenDialog.Create(nil);
      openDlg.Filter := 'XML|*.xml';
      try
        if openDlg.Execute then
        begin
          TBGRASVGTheme(GetComponent).LoadFromFile(openDlg.FileName);
        end;
      finally
        openDlg.Free;
      end;
    end;
    // Save to file
    1: begin
      saveDlg := TSaveDialog.Create(nil);
      saveDlg.Filter := 'XML|*.xml';
      try
        if saveDlg.Execute then
        begin
          TBGRASVGTheme(GetComponent).SaveToFile(saveDlg.FileName);
        end;
      finally
        saveDlg.Free;
      end;
    end;
  end;
end;

function TBGRASVGThemeComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Load From File...';
    1: Result := 'Save To File...';
  else
    result := '';
  end;
end;

function TBGRASVGThemeComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function TBGRASVGThemeComponentEditor.IsInInlined: Boolean;
begin
  result := False;
end;

procedure TBGRASVGThemeComponentEditor.PrepareItem(Index: Integer;
  const AnItem: TMenuItem);
begin

end;

procedure TBGRASVGThemeComponentEditor.Modified;
begin

end;

function TBGRASVGThemeComponentEditor.GetComponent: TComponent;
begin
  Result := FComponent;
end;

function TBGRASVGThemeComponentEditor.GetCustomHint: String;
begin
  result := 'SVG Theme';
end;

function TBGRASVGThemeComponentEditor.GetDesigner: TComponentEditorDesigner;
begin
  result := nil;
end;

function TBGRASVGThemeComponentEditor.GetHook(out Hook: TPropertyEditorHook
  ): boolean;
begin
  Hook := nil;
  result := false;
end;

{ TBGRASVGTheme }

procedure TBGRASVGTheme.SetCheckBoxUnchecked(AValue: TStringList);
begin
  CheckEmptyResourceException(AValue.Text);
  if (AValue <> FCheckBoxUnchecked) then
  begin
    FCheckBoxUnchecked.Assign(AValue);
    InvalidateThemedControls;
  end;
end;

procedure TBGRASVGTheme.SetColorizeActive(AValue: string);
begin
  if FColorizeActive = AValue then
    Exit;
  FColorizeActive := AValue;
  InvalidateThemedControls;
end;

procedure TBGRASVGTheme.SetColorizeActiveOp(AValue: TBlendOperation);
begin
  if FColorizeActiveOp=AValue then Exit;
  FColorizeActiveOp:=AValue;
  InvalidateThemedControls;
end;

procedure TBGRASVGTheme.SetColorizeDisabled(AValue: string);
begin
  if FColorizeDisabled = AValue then
    Exit;
  FColorizeDisabled := AValue;
  InvalidateThemedControls;
end;

procedure TBGRASVGTheme.SetColorizeDisabledOp(AValue: TBlendOperation);
begin
  if FColorizeDisabledOp=AValue then Exit;
  FColorizeDisabledOp:=AValue;
  InvalidateThemedControls;
end;

procedure TBGRASVGTheme.SetColorizeHover(AValue: string);
begin
  if FColorizeHover = AValue then
    Exit;
  FColorizeHover := AValue;
  InvalidateThemedControls;
end;

procedure TBGRASVGTheme.SetColorizeHoverOp(AValue: TBlendOperation);
begin
  if FColorizeHoverOp=AValue then Exit;
  FColorizeHoverOp:=AValue;
  InvalidateThemedControls;
end;

procedure TBGRASVGTheme.SetColorizeNormal(AValue: string);
begin
  if FColorizeNormal = AValue then
    Exit;
  FColorizeNormal := AValue;
  InvalidateThemedControls;
end;

procedure TBGRASVGTheme.SetColorizeNormalOp(AValue: TBlendOperation);
begin
  if FColorizeNormalOp=AValue then Exit;
  FColorizeNormalOp:=AValue;
  InvalidateThemedControls;
end;

procedure TBGRASVGTheme.SetGlyphTextSpacing(AValue: integer);
begin
  if FGlyphTextSpacing=AValue then Exit;
  FGlyphTextSpacing:=AValue;
  InvalidateThemedControls;
end;

procedure TBGRASVGTheme.SetRadioButtonChecked(AValue: TStringList);
begin
  CheckEmptyResourceException(AValue.Text);
  if (AValue <> FRadioButtonChecked) then
  begin
    FRadioButtonChecked.Assign(AValue);
    InvalidateThemedControls;
  end;
end;

procedure TBGRASVGTheme.SetRadioButtonUnchecked(AValue: TStringList);
begin
  CheckEmptyResourceException(AValue.Text);
  if (AValue <> FRadioButtonUnchecked) then
  begin
    FRadioButtonUnchecked.Assign(AValue);
    InvalidateThemedControls;
  end;
end;

procedure TBGRASVGTheme.LoadDefaultTheme;
begin
  FCheckBoxUnchecked.Text := RES_CHECKBOXUNCHECKED;
  FCheckBoxChecked.Text := RES_CHECKBOXCHECKED;
  FCheckboxTextSpacing:= DEFAULT_CHECKBOX_TEXT_SPACING;
  FRadioButtonUnchecked.Text := RES_RADIOBUTTONUNCHECKED;
  FRadioButtonChecked.Text := RES_RADIOBUTTONCHECKED;
  FButtonNormal.Text := RES_BUTTON;
  FButtonHover.Text := '';
  FButtonActive.Text := '';
  FButtonSliceScalingLeft := 10;
  FButtonSliceScalingTop := 10;
  FButtonSliceScalingRight := 10;
  FButtonSliceScalingBottom := 10;
  FGlyphTextSpacing := DEFAULT_GLYPH_TEXT_SPACING;
  FButtonTextSpacing := DEFAULT_BUTTON_TEXT_SPACING;
  FColorizeNormal := RES_COLORIZENORMAL;
  FColorizeHover := RES_COLORIZEHOVER;
  FColorizeActive := RES_COLORIZEACTIVE;
  FColorizeDisabled := RES_COLORIZEDISABLED;
  FColorizeNormalOp := boTransparent;
  FColorizeHoverOp := boTransparent;
  FColorizeActiveOp := boTransparent;
  FColorizeDisabledOp := boTransparent;
end;

procedure TBGRASVGTheme.LoadTheme(const XMLConf: TXMLConfig);
begin
  try
    XMLConf.RootName := 'BGRASVGTheme';
    // Button
    FButtonActive.Text := XMLConf.GetValue('Button/Active/SVG', RES_BUTTON){%H-};
    FButtonHover.Text := XMLConf.GetValue('Button/Hover/SVG', ''){%H-};
    FButtonNormal.Text := XMLConf.GetValue('Button/Normal/SVG', ''){%H-};
    FButtonSliceScalingBottom := XMLConf.GetValue('Button/SliceScaling/Bottom', 10);
    FButtonSliceScalingLeft := XMLConf.GetValue('Button/SliceScaling/Left', 10);
    FButtonSliceScalingRight := XMLConf.GetValue('Button/SliceScaling/Right', 10);
    FButtonSliceScalingTop := XMLConf.GetValue('Button/SliceScaling/Top', 10);
    FGlyphTextSpacing := XMLConf.GetValue('Button/GlyphSpacing', DEFAULT_GLYPH_TEXT_SPACING);
    FButtonTextSpacing := XMLConf.GetValue('Button/TextSpacing', DEFAULT_BUTTON_TEXT_SPACING);
    // CheckBox
    FCheckBoxChecked.Text := XMLConf.GetValue('CheckBox/Checked/SVG',
      RES_CHECKBOXCHECKED){%H-};
    FCheckBoxUnchecked.Text := XMLConf.GetValue('CheckBox/Unchecked/SVG',
      RES_CHECKBOXUNCHECKED){%H-};
    FCheckBoxTextSpacing := XMLConf.GetValue('CheckBox/TextSpacing', DEFAULT_CHECKBOX_TEXT_SPACING);
    // Colorize
    FColorizeActive := XMLConf{%H-}.GetValue('Colorize/Active', RES_COLORIZEACTIVE);
    FColorizeDisabled := XMLConf{%H-}.GetValue('Colorize/Disabled', RES_COLORIZEDISABLED);
    FColorizeHover := XMLConf{%H-}.GetValue('Colorize/Hover', RES_COLORIZEHOVER);
    FColorizeNormal := XMLConf{%H-}.GetValue('Colorize/Normal', RES_COLORIZENORMAL);
    FColorizeActiveOp := StrToBlendOperation(XMLConf{%H-}.GetValue('Colorize/ActiveOp', BlendOperationStr[boTransparent]));
    FColorizeDisabledOp := StrToBlendOperation(XMLConf{%H-}.GetValue('Colorize/DisabledOp', BlendOperationStr[boTransparent]));
    FColorizeHoverOp := StrToBlendOperation(XMLConf{%H-}.GetValue('Colorize/HoverOp', BlendOperationStr[boTransparent]));
    FColorizeNormalOp := StrToBlendOperation(XMLConf{%H-}.GetValue('Colorize/NormalOp', BlendOperationStr[boTransparent]));
    // RadioButton
    FRadioButtonChecked.Text :=
      XMLConf.GetValue('RadioButton/Checked/SVG', RES_RADIOBUTTONCHECKED{%H-}){%H-};
    FRadioButtonUnchecked.Text :=
      XMLConf.GetValue('RadioButton/Unchecked/SVG', RES_RADIOBUTTONUNCHECKED{%H-}){%H-};
  finally
    InvalidateThemedControls;
  end;
end;

procedure TBGRASVGTheme.SaveTheme(const XMLConf: TXMLConfig);
begin
  XMLConf.RootName := 'BGRASVGTheme';
  // Button
  XMLConf.SetValue('Button/Active/SVG', FButtonActive.Text{%H-});
  XMLConf.SetValue('Button/Hover/SVG', FButtonHover.Text{%H-});
  XMLConf.SetValue('Button/Normal/SVG', FButtonNormal.Text{%H-});
  XMLConf.SetValue('Button/SliceScaling/Bottom', FButtonSliceScalingBottom);
  XMLConf.SetValue('Button/SliceScaling/Left', FButtonSliceScalingLeft);
  XMLConf.SetValue('Button/SliceScaling/Right', FButtonSliceScalingRight);
  XMLConf.SetValue('Button/SliceScaling/Top', FButtonSliceScalingTop);
  XMLConf.SetValue('Button/GlyphSpacing', FGlyphTextSpacing);
  XMLConf.SetValue('Button/TextSpacing', FButtonTextSpacing);
  // CheckBox
  XMLConf.SetValue('CheckBox/Checked/SVG', FCheckBoxChecked.Text{%H-});
  XMLConf.SetValue('CheckBox/Unchecked/SVG', FCheckBoxUnchecked.Text{%H-});
  XMLConf.SetValue('CheckBox/TextSpacing', FCheckboxTextSpacing);
  // Colorize
  XMLConf.SetValue('Colorize/Active', FColorizeActive{%H-});
  XMLConf.SetValue('Colorize/Disabled', FColorizeDisabled{%H-});
  XMLConf.SetValue('Colorize/Hover', FColorizeHover{%H-});
  XMLConf.SetValue('Colorize/Normal', FColorizeNormal{%H-});
  XMLConf.SetValue('Colorize/ActiveOp', BlendOperationStr[FColorizeActiveOp{%H-}]);
  XMLConf.SetValue('Colorize/DisabledOp', BlendOperationStr[FColorizeDisabledOp{%H-}]);
  XMLConf.SetValue('Colorize/HoverOp', BlendOperationStr[FColorizeHoverOp{%H-}]);
  XMLConf.SetValue('Colorize/NormalOp', BlendOperationStr[FColorizeNormalOp{%H-}]);   // RadioButton
  XMLConf.SetValue('RadioButton/Checked/SVG', FRadioButtonChecked.Text{%H-});
  XMLConf.SetValue('RadioButton/Unchecked/SVG', FRadioButtonUnchecked.Text{%H-});
end;

procedure TBGRASVGTheme.CheckEmptyResourceException(const aResource: string);
begin
  if Trim(aResource).IsEmpty then
    raise Exception.Create('Resource must not be empty.');
end;

procedure TBGRASVGTheme.SliceScalingDraw(const Source: TBGRASVG;
  const marginLeft, marginTop, marginRight, marginBottom: integer;
  const Dest: TBGRABitmap; DestDPI: integer);
var
  svgBox: TSVGViewBox;
  svgTopLeft, svgBottomRight: TPointF;
  sourcePosX, sourcePosY: array[1..4] of single;
  destPosX, destPosY: array[1..4] of integer;
  y, x: integer;

  procedure DrawPart(sourceRect: TRectF; destRect: TRect);
  var
    zoom: TPointF;
  begin
    if sourceRect.IsEmpty or destRect.IsEmpty then
      exit;
    dest.ClipRect := destRect;
    zoom := PointF(destRect.Width / sourceRect.Width, destRect.Height /
      sourceRect.Height);
    Source.Draw(dest.Canvas2D, -sourceRect.Left * zoom.x + destRect.Left,
      -sourceRect.Top * zoom.y + destRect.Top, Source.DefaultDpi * zoom);
  end;

begin
  svgBox := Source.ViewBoxInUnit[cuPixel];
  svgTopLeft := svgBox.min;
  svgBottomRight := svgBox.min + svgBox.size;

  sourcePosX[1] := svgTopLeft.x;
  sourcePosX[2] := svgTopLeft.x + marginLeft;
  sourcePosX[3] := svgBottomRight.x - marginRight;
  sourcePosX[4] := svgBottomRight.x;
  sourcePosY[1] := svgTopLeft.y;
  sourcePosY[2] := svgTopLeft.y + marginTop;
  sourcePosY[3] := svgBottomRight.y - marginBottom;
  sourcePosY[4] := svgBottomRight.y;
  if sourcePosX[2] > sourcePosX[3] then
  begin
    sourcePosX[2] := (sourcePosX[1] + sourcePosX[4]) / 2;
    sourcePosX[3] := sourcePosX[2];
  end;
  if sourcePosY[2] > sourcePosY[3] then
  begin
    sourcePosY[2] := (sourcePosY[1] + sourcePosY[4]) / 2;
    sourcePosY[3] := sourcePosY[2];
  end;

  destPosX[1] := 0;
  destPosX[2] := round(marginLeft * DestDPI / 96);
  destPosX[3] := dest.Width - round(marginRight * DestDPI / 96);
  destPosX[4] := dest.Width;
  destPosY[1] := 0;
  destPosY[2] := round(marginTop * DestDPI / 96);
  destPosY[3] := dest.Height - round(marginBottom * DestDPI / 96);
  destPosY[4] := dest.Height;
  if destPosX[2] > destPosX[3] then
  begin
    destPosX[2] := round((destPosX[1] + destPosX[4]) / 2);
    destPosX[3] := destPosX[2];
  end;
  if destPosY[2] > destPosY[3] then
  begin
    destPosY[2] := round((destPosY[1] + destPosY[4]) / 2);
    destPosY[3] := destPosY[2];
  end;

  for y := 1 to 3 do
    for x := 1 to 3 do
      DrawPart(RectF(sourcePosX[x], sourcePosY[y], sourcePosX[x + 1], sourcePosY[y + 1]),
        Rect(destPosX[x], destPosY[y], destPosX[x + 1], destPosY[y + 1]));
  Dest.NoClip;
end;

procedure TBGRASVGTheme.ColorizeSurface(ASurface: TBGRAThemeSurface;
  AState: TBGRAThemeButtonState);
var
  color: String;
  op: TBlendOperation;
begin
  case AState of
    btbsNormal: begin color := FColorizeNormal; op := FColorizeNormalOp; end;
    btbsHover: begin color := FColorizeHover; op := FColorizeHoverOp; end;
    btbsActive: begin color := FColorizeActive; op := FColorizeActiveOp; end;
    else {btbsDisabled} begin color := FColorizeDisabled; op := FColorizeDisabledOp; end;
  end;
  ASurface.BitmapColorOverlay(StrToBGRA(color), op);
end;

constructor TBGRASVGTheme.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  FCheckBoxUnchecked := TStringList.Create;
  FCheckBoxChecked := TStringList.Create;
  FRadioButtonUnchecked := TStringList.Create;
  FRadioButtonChecked := TStringList.Create;
  FButtonNormal := TStringList.Create;
  FButtonHover := TStringList.Create;
  FButtonActive := TStringList.Create;
  LoadDefaultTheme;
end;

destructor TBGRASVGTheme.Destroy;
begin
  FCheckBoxUnchecked.Free;
  FCheckBoxChecked.Free;
  FRadioButtonUnchecked.Free;
  FRadioButtonChecked.Free;
  FButtonNormal.Free;
  FButtonHover.Free;
  FButtonActive.Free;
  inherited Destroy;
end;

function TBGRASVGTheme.PreferredButtonHeight(const hasGlyph: boolean): Integer;
begin
  Result := (FButtonTextSpacing * 2);
end;

function TBGRASVGTheme.PreferredButtonWidth(const hasGlyph: boolean): Integer;
begin
  Result := (FButtonTextSpacing * 2);
  if (hasGlyph) then
    Result := Result + FGlyphTextSpacing;
end;

procedure TBGRASVGTheme.DrawButton(Caption: string;
  State: TBGRAThemeButtonState; Focused: boolean; ARect: TRect;
  ASurface: TBGRAThemeSurface; AImageIndex: Integer;
  AImageList: TBGRASVGImageList);
var
  svg: TBGRASVG;
  svgCode: String;
  gs: TSize;
  bcFont: TBCFont;
  actualCaption: string;
  r, rGlyph: TRect;
  drawText: boolean = True;

begin
  with ASurface do
  begin
    case State of
      btbsNormal: svg := TBGRASVG.CreateFromString(FButtonNormal.Text);
      btbsHover:
        begin
          svgCode := FButtonHover.Text;
          if trim(svgCode) = '' then svgCode := FButtonNormal.Text;
          svg := TBGRASVG.CreateFromString(svgCode);
        end;
      btbsActive:
        begin
          svgCode := FButtonActive.Text;
          if trim(svgCode) = '' then svgCode := FButtonHover.Text;
          if trim(svgCode) = '' then svgCode := FButtonNormal.Text;
          svg := TBGRASVG.CreateFromString(svgCode);
        end;
      else {btbsDisabled}
          svg := TBGRASVG.CreateFromString(FButtonNormal.Text);
    end;
    SliceScalingDraw(svg, FButtonSliceScalingLeft, FButtonSliceScalingTop,
      FButtonSliceScalingRight, FButtonSliceScalingBottom, Bitmap,
      BitmapDPI);
    svg.Free;

    if Assigned(AImageList) and (AImageIndex > -1) and (AImageIndex < AImageList.Count) then
    begin
      gs := AImageList.GetScaledSize(BitmapDPI);
      if ARect.Width - gs.cx < ScaleForBitmap(GlyphTextSpacing + 2*ButtonTextSpacing) then
        drawText := false;
    end
      else gs := TSize.Create(0, 0);

    bcFont := TBCFont.Create(nil);
    bcFont.Assign(DestCanvas.Font);
    bcFont.Scale(BitmapDPI / DestCanvasDPI, false);
    bcFont.WordBreak := true;
    bcFont.PaddingBottom:= ScaleForBitmap(ButtonTextSpacing);
    bcFont.PaddingTop:= ScaleForBitmap(ButtonTextSpacing);
    bcFont.PaddingRight:= ScaleForBitmap(ButtonTextSpacing);
    bcFont.PaddingLeft:= ScaleForBitmap(ButtonTextSpacing);
    bcFont.TextAlignment:= bcaCenter;

    if drawText then
      actualCaption := Caption
      else actualCaption:= '';

    r := ScaleForBitmap(ARect, DestCanvasDPI);
    rGlyph := ComputeGlyphPosition(r, gs.cx, gs.cy, bcaCenter,
      ScaleForBitmap(GlyphTextSpacing), actualCaption, bcFont);
    if not rGlyph.IsEmpty then
      AImageList.Draw(AImageIndex, Bitmap, RectF(rGlyph));
    RenderText(r, bcFont, actualCaption, Bitmap, State <> btbsDisabled);

    bcFont.Free;
    ColorizeSurface(ASurface, State);
    DrawBitmap;
  end;
end;

procedure TBGRASVGTheme.DrawRadioButton(Caption: string;
  State: TBGRAThemeButtonState; Focused: boolean; Checked: boolean;
  ARect: TRect; ASurface: TBGRAThemeSurface);
var
  Style: TTextStyle;
  svg: TBGRASVG;
begin
  with ASurface do
  begin
    BitmapRect := RectWithSize(ARect.Left, ARect.Top, ARect.Height, ARect.Height);
    if Checked then
      svg := TBGRASVG.CreateFromString(FRadioButtonChecked.Text)
    else
      svg := TBGRASVG.CreateFromString(FRadioButtonUnchecked.Text);
    svg.StretchDraw(Bitmap.Canvas2D, 0, 0, Bitmap.Width, Bitmap.Height);
    svg.Free;
    ColorizeSurface(ASurface, State);
    DrawBitmap;

    if Caption <> '' then
    begin
      fillchar(Style, sizeof(Style), 0);
      Style.Alignment := taLeftJustify;
      Style.Layout := tlCenter;
      Style.Wordbreak := True;
      DestCanvas.TextRect(
        Rect(Arect.Height + ScaleForCanvas(CheckBoxTextSpacing), 0,
        ARect.Right, ARect.Bottom),
        ARect.Height + ScaleForCanvas(CheckBoxTextSpacing), 0, Caption, Style);
    end;
  end;
end;

procedure TBGRASVGTheme.SetCheckBoxChecked(AValue: TStringList);
begin
  CheckEmptyResourceException(AValue.Text);
  if (AValue <> FCheckBoxChecked) then
  begin
    FCheckBoxChecked.Assign(AValue);
    InvalidateThemedControls;
  end;
end;

procedure TBGRASVGTheme.SetCheckboxTextSpacing(AValue: integer);
begin
  if FCheckboxTextSpacing=AValue then Exit;
  FCheckboxTextSpacing:=AValue;
  InvalidateThemedControls;
end;

procedure TBGRASVGTheme.SetButtonActive(AValue: TStringList);
begin
  if (AValue <> FButtonActive) then
  begin
    FButtonActive.Assign(AValue);
    InvalidateThemedControls;
  end;
end;

procedure TBGRASVGTheme.SetButtonHover(AValue: TStringList);
begin
  if (AValue <> FButtonHover) then
  begin
    FButtonHover.Assign(AValue);
    InvalidateThemedControls;
  end;
end;

procedure TBGRASVGTheme.SetButtonNormal(AValue: TStringList);
begin
  CheckEmptyResourceException(AValue.Text);
  if (AValue <> FButtonNormal) then
  begin
    FButtonNormal.Assign(AValue);
    InvalidateThemedControls;
  end;
end;

procedure TBGRASVGTheme.SetButtonSliceScalingBottom(AValue: integer);
begin
  if FButtonSliceScalingBottom = AValue then
    Exit;
  FButtonSliceScalingBottom := AValue;
  InvalidateThemedControls;
end;

procedure TBGRASVGTheme.SetButtonSliceScalingLeft(AValue: integer);
begin
  if FButtonSliceScalingLeft = AValue then
    Exit;
  FButtonSliceScalingLeft := AValue;
  InvalidateThemedControls;
end;

procedure TBGRASVGTheme.SetButtonSliceScalingRight(AValue: integer);
begin
  if FButtonSliceScalingRight = AValue then
    Exit;
  FButtonSliceScalingRight := AValue;
  InvalidateThemedControls;
end;

procedure TBGRASVGTheme.SetButtonSliceScalingTop(AValue: integer);
begin
  if FButtonSliceScalingTop = AValue then
    Exit;
  FButtonSliceScalingTop := AValue;
  InvalidateThemedControls;
end;

procedure TBGRASVGTheme.SetButtonTextSpacing(AValue: integer);
begin
  if FButtonTextSpacing=AValue then Exit;
  FButtonTextSpacing:=AValue;
  InvalidateThemedControls;
end;

procedure TBGRASVGTheme.DrawCheckBox(Caption: string; State: TBGRAThemeButtonState;
  Focused: boolean; Checked: boolean; ARect: TRect; ASurface: TBGRAThemeSurface);
var
  Style: TTextStyle;
  svg: TBGRASVG;
  r: TRect;
begin
  with ASurface do
  begin
    BitmapRect := RectWithSize(ARect.Left, ARect.Top, ARect.Height, ARect.Height);
    if Checked then
      svg := TBGRASVG.CreateFromString(FCheckBoxChecked.Text)
    else
      svg := TBGRASVG.CreateFromString(FCheckBoxUnchecked.Text);
    svg.StretchDraw(Bitmap.Canvas2D, 0, 0, Bitmap.Width, Bitmap.Height);
    svg.Free;
    ColorizeSurface(ASurface, State);
    DrawBitmap;

    if Caption <> '' then
    begin
      fillchar(Style, sizeof(Style), 0);
      Style.Alignment := taLeftJustify;
      Style.Layout := tlCenter;
      Style.Wordbreak := True;
      DestCanvas.TextRect(
        Rect(Arect.Height + ScaleForCanvas(CheckBoxTextSpacing), 0,
        ARect.Right, ARect.Bottom),
        ARect.Height +  ScaleForCanvas(CheckBoxTextSpacing), 0, Caption, Style);
    end;
    if Focused then
    begin
      DestCanvas.Pen.Color := DestCanvas.Font.Color;
      DestCanvas.Pen.Style := psDash;
      DestCanvas.Brush.Style := bsClear;
      r := ARect;
      DestCanvas.Rectangle(r);
      DestCanvas.Pen.Style := psSolid;
    end;
  end;
end;

procedure TBGRASVGTheme.SaveToFile(AFileName: string);
var
  FXMLConf: TXMLConfig;
begin
  FXMLConf := TXMLConfig.Create(Self);
  try
    FXMLConf.Filename := AFileName;
    SaveTheme(FXMLConf);
    FXMLConf.Flush;
  finally
    FXMLConf.Free;
  end;
end;

procedure TBGRASVGTheme.LoadFromFile(AFileName: string);
var
  FXMLConf: TXMLConfig;
begin
  FXMLConf := TXMLConfig.Create(Self);
  try
    FXMLConf.Filename := AFileName;
    LoadTheme(FXMLConf);
  finally
    FXMLConf.Free;
  end;
end;

procedure TBGRASVGTheme.SaveToStream(AStream: TStream);
var
  FXMLConf: TXMLConfig;
begin
  FXMLConf := TXMLConfig.Create(Self);
  try
    SaveTheme(FXMLConf);
    FXMLConf.SaveToStream(AStream);
    FXMLConf.Flush;
  finally
    FXMLConf.Free;
  end;
end;

procedure TBGRASVGTheme.LoadFromStream(AStream: TStream);
var
  FXMLConf: TXMLConfig;
begin
  FXMLConf := TXMLConfig.Create(Self);
  try
    FXMLConf.RootName := 'BGRASVGTheme';
    AStream.Position := 0;
    FXMLConf.LoadFromStream(AStream);
    LoadTheme(FXMLConf);
  finally
    FXMLConf.Free;
  end;
end;

procedure TBGRASVGTheme.LoadFromResource(AResource: string);
var
  AStream: TStream;
begin
  AStream := BGRAResource.GetResourceStream(AResource);
  LoadFromStream(AStream);
  AStream.Free;
end;

end.
