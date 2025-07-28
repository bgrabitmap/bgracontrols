// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Part of BGRA Controls. Made by third party.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html

}
{******************************* CONTRIBUTOR(S) ******************************
- Sandy Ganz | sganz@pacbell.net
  Test Program for SuperLED
***************************** END CONTRIBUTOR(S) *****************************}

unit sltest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ComCtrls, ColorBox, Buttons, SpinEx, BGRATheme, SuperLED,
  BGRAImageList, about;

const
  VERSIONSTR = '1.00';            // SL TEST version, Should ALWAYS show as a delta when merging!

type

  { TSLTestFrm }

  TSLTestFrm = class(TForm)
    AutoScaleCb: TCheckBox;
    BackgroundColorCb: TColorBox;
    BackgroundColorLbl: TLabel;
    AutoScaleLbl: TLabel;
    ComboBox1: TComboBox;
    GroupBox7: TGroupBox;
    BorderColorCb: TColorBox;
    BorderColorLbl: TLabel;
    BorderOpacityLbl: TLabel;
    BorderOpacitySpe: TSpinEditEx;
    BorderStyleCb: TComboBox;
    BorderStyleLbl: TLabel;
    BorderThicknessLbl: TLabel;
    BorderThicknessSpe: TSpinEditEx;
    ActiveStateCb: TCheckBox;
    DrawingRoundRadiusSpe: TSpinEditEx;
    DrawingShapeCb: TComboBox;
    DrawingFillOpacitySpe: TSpinEditEx;
    DrawingActiveColorCb: TColorBox;
    ColorDialog1: TColorDialog;
    DrawingInactiveBrightnessSpe: TSpinEditEx;
    DrawingInactiveColorCb: TColorBox;
    DrawingActiveColorLbl: TLabel;
    DrawingFillOpacityLbl: TLabel;
    DrawingInactiveBrightnessLbl: TLabel;
    DrawingInactiveColorLbl: TLabel;
    DrawingRadiusLbl: TLabel;
    DrawingStyleCb: TComboBox;
    DrawingShapeLbl: TLabel;
    DrawingStyleLbl: TLabel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    HeightAddBtn: TBitBtn;
    HeightLbl: TLabel;
    HeightSubBtn: TBitBtn;
    HeightValLbl: TLabel;
    ImageList1: TBGRAImageList;
    DrawingAngleLbl: TLabel;
    LEDStateLbl: TLabel;
    LEDStatusLbl: TLabel;
    LeftAddBtn: TBitBtn;
    LeftLbl: TLabel;
    LeftSubBtn: TBitBtn;
    LeftValLbl: TLabel;
    MainMenu1: TMainMenu;
    DrawingAngleSpe: TFloatSpinEditEx;
    MenuItem1: TMenuItem;
    AboutMenu: TMenuItem;
    ExitMenu: TMenuItem;
    LEDTs: TPageControl;
    PresetsLbl: TLabel;
    ResetPositionBtn: TBitBtn;
    ResetSizeBtn: TBitBtn;
    MakeBigBtn: TBitBtn;
    SuperLED: TSuperLED;
    BasicTab: TTabSheet;
    BorderTab: TTabSheet;
    DrawingTab: TTabSheet;
    ToggleStateBitBtn: TBitBtn;
    TopAddBtn: TBitBtn;
    TopLbl: TLabel;
    TopSubBtn: TBitBtn;
    TopValLbl: TLabel;
    WidthAddBtn: TBitBtn;
    WidthHeightAddBtn: TBitBtn;
    WidthHeightSubBtn: TBitBtn;
    WidthLbl: TLabel;
    WidthSubBtn: TBitBtn;
    WidthValLbl: TLabel;
    procedure AboutMenuClick(Sender: TObject);
    procedure AutoScaleCbChange(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormConstrainedResize({%H-}Sender: TObject; var {%H-}MinWidth, {%H-}MinHeight,
      {%H-}MaxWidth, {%H-}MaxHeight: TConstraintSize);
    procedure MakeBigBtnClick(Sender: TObject);
    procedure SuperLEDDblClick(Sender: TObject);
    procedure ToggleStateBitBtnClick(Sender: TObject);
    procedure BorderColorCbChange(Sender: TObject);
    procedure BackgroundColorCbChange(Sender: TObject);
    procedure BorderOpacitySpeChange(Sender: TObject);
    procedure BorderStyleCbChange(Sender: TObject);
    procedure ActiveStateCbChange(Sender: TObject);
    procedure DrawingActiveColorCbChange(Sender: TObject);
    procedure DrawingAngleSpeChange(Sender: TObject);
    procedure DrawingFillOpacitySpeChange(Sender: TObject);
    procedure DrawingInactiveBrightnessSpeChange(Sender: TObject);
    procedure DrawingInactiveColorCbChange(Sender: TObject);
    procedure DrawingRoundRadiusSpeChange(Sender: TObject);
    procedure DrawingShapeCbChange(Sender: TObject);
    procedure DrawingStyleCbChange(Sender: TObject);
    procedure ExitMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BorderThicknessSpeChange(Sender: TObject);
    procedure HeightAddBtnClick(Sender: TObject);
    procedure HeightSubBtnClick(Sender: TObject);
    procedure LeftAddBtnClick(Sender: TObject);
    procedure LeftSubBtnClick(Sender: TObject);
    procedure ResetPositionBtnClick(Sender: TObject);
    procedure ResetSizeBtnClick(Sender: TObject);
    procedure SuperLEDClick(Sender: TObject);
    procedure TopAddBtnClick(Sender: TObject);
    procedure TopSubBtnClick(Sender: TObject);
    procedure WidthAddBtnClick(Sender: TObject);
    procedure WidthHeightAddBtnClick(Sender: TObject);
    procedure WidthHeightSubBtnClick(Sender: TObject);
    procedure WidthSubBtnClick(Sender: TObject);
  private
    FSavedLED: TSuperLED;
  public
    procedure LEDDefaults;
    procedure UpdateAllStats;
    procedure UpdateBasicStats;
    procedure UpdateWHStats;
    procedure UpdateLTStats;
    procedure UpdateBorderStats;
    procedure UpdateDrawingStats;
  end;

var
  SLTestFrm: TSLTestFrm;

implementation

{$R *.lfm}

{ TSLTestFrm }

procedure TSLTestFrm.ActiveStateCbChange(Sender: TObject);
begin
  SuperLED.Active := ActiveStateCb.Checked;
  UpdateBasicStats;
end;

procedure TSLTestFrm.DrawingActiveColorCbChange(Sender: TObject);
begin
  SuperLED.ActiveColor := DrawingActiveColorCb.Selected;
end;

procedure TSLTestFrm.DrawingAngleSpeChange(Sender: TObject);
begin
  SuperLED.Angle := DrawingAngleSpe.Value;
end;

procedure TSLTestFrm.DrawingFillOpacitySpeChange(Sender: TObject);
begin
  SuperLED.FillOpacity := DrawingFillOpacitySpe.Value;
end;

procedure TSLTestFrm.DrawingInactiveBrightnessSpeChange(Sender: TObject);
begin
  SuperLED.InactiveBrightness := DrawingInactiveBrightnessSpe.Value;
end;

procedure TSLTestFrm.DrawingInactiveColorCbChange(Sender: TObject);
begin
  SuperLED.InactiveColor := DrawingInactiveColorCb.Selected;
end;

procedure TSLTestFrm.DrawingRoundRadiusSpeChange(Sender: TObject);
begin
  SuperLED.RoundRadius := DrawingRoundRadiusSpe.Value;
end;

procedure TSLTestFrm.DrawingShapeCbChange(Sender: TObject);
begin
  // CB items must match the items in TSLEDShape
  SuperLED.Shape := TSLEDShape(DrawingShapeCb.ItemIndex);
end;

procedure TSLTestFrm.DrawingStyleCbChange(Sender: TObject);
begin
  // CB items must match the items in TSLEDStyle
  SuperLED.Style := TSLEDStyle(DrawingStyleCb.ItemIndex);
end;

procedure TSLTestFrm.ExitMenuClick(Sender: TObject);
begin
  Close;
end;

procedure TSLTestFrm.FormCreate(Sender: TObject);
begin
  Caption := 'Super LED Test Application ' + VERSIONSTR;

  UpdateAllStats;

  // Create a SuperLED to have defaults, clean up in form destroy

  FSavedLED := TSuperLED.Create(nil);
end;

procedure TSLTestFrm.FormDestroy(Sender: TObject);
begin
  FSavedLED.Free;
end;

procedure TSLTestFrm.BorderThicknessSpeChange(Sender: TObject);
begin
  SuperLED.BorderThickness := BorderThicknessSpe.Value;
end;

procedure TSLTestFrm.HeightAddBtnClick(Sender: TObject);
begin
  SuperLED.Height := SuperLED.Height + 5;
  UpdateWHStats;
end;

procedure TSLTestFrm.HeightSubBtnClick(Sender: TObject);
begin
  SuperLED.Height := SuperLED.Height - 5;
  UpdateWHStats;
end;

procedure TSLTestFrm.LeftAddBtnClick(Sender: TObject);
begin
  SuperLED.Left := SuperLED.Left + 5;
  UpdateLTStats;
end;

procedure TSLTestFrm.LeftSubBtnClick(Sender: TObject);
begin
  SuperLED.Left := SuperLED.Left - 5;
  UpdateLTStats;
end;

procedure TSLTestFrm.ResetPositionBtnClick(Sender: TObject);
begin
  SuperLED.Left := 100;
  SuperLED.Top := 100;
  UpdateLTStats;
end;

procedure TSLTestFrm.ResetSizeBtnClick(Sender: TObject);
begin
  SuperLED.Width := 32;
  SuperLED.Height := 32;

  // Make sure visiable

  ResetPositionBtn.Click;
  UpdateWHStats;
end;

procedure TSLTestFrm.SuperLEDClick(Sender: TObject);
begin
  ToggleStateBitBtn.OnClick(self);
end;

procedure TSLTestFrm.TopAddBtnClick(Sender: TObject);
begin
  SuperLED.Top := SuperLED.Top + 5;
  UpdateLTStats;
end;

procedure TSLTestFrm.TopSubBtnClick(Sender: TObject);
begin
  SuperLED.Top := SuperLED.Top - 5;
  UpdateLTStats;
end;

procedure TSLTestFrm.WidthAddBtnClick(Sender: TObject);
begin
  SuperLED.Width := SuperLED.Width + 5;
  UpdateWHStats;
end;

procedure TSLTestFrm.WidthHeightAddBtnClick(Sender: TObject);
begin
  SuperLED.Height := SuperLED.Height + 5;
  SuperLED.Width := SuperLED.Width + 5;
  SuperLED.Left := SuperLED.Left - 2;
  SuperLED.Top := SuperLED.Top - 2;
  UpdateWHStats;
  UpdateLTStats;
end;

procedure TSLTestFrm.WidthHeightSubBtnClick(Sender: TObject);
begin
  SuperLED.Height := SuperLED.Height - 5;
  SuperLED.Width := SuperLED.Width - 5;
  SuperLED.Left := SuperLED.Left + 2;
  SuperLED.Top := SuperLED.Top + 2;
  UpdateWHStats;
  UpdateLTStats;
end;

procedure TSLTestFrm.WidthSubBtnClick(Sender: TObject);
begin
  SuperLED.Width := SuperLED.Width - 5;
  UpdateWHStats;
end;

procedure TSLTestFrm.LEDDefaults;
begin
  // Reset anything from the saved default LED back to the active one

  with SuperLED do
  begin
    // Basic Settings, check properties for important ones
    // and add any new here!

    Width := FSavedLED.Width;
    Height := FSavedLED.Height;

    Angle := FSavedLED.Angle;
    AutoScale := FSavedLED.AutoScale;
    BorderColor := FSavedLED.BorderColor;
    BorderOpacity := FSavedLED.BorderOpacity;
    BorderStyle := FSavedLED.BorderStyle;
    BorderThickness := FSavedLED.BorderThickness;
    Color := FSavedLED.Color;
    FillOpacity := FSavedLED.FillOpacity;
    InactiveBrightness := FSavedLED.InactiveBrightness;
    InactiveColor := FSavedLED.InactiveColor;
    RoundRadius := FSavedLED.RoundRadius;
    Shape := FSavedLED.Shape;
    Style := FSavedLED.Style;
  end;
end;

procedure TSLTestFrm.UpdateAllStats;
begin
  UpdateWHStats;
  UpdateLTStats;
  UpdateBasicStats;
  UpdateBorderStats;
  UpdateDrawingStats;
end;

procedure TSLTestFrm.UpdateBasicStats;
begin
  AutoScaleCb.Checked := SuperLED.AutoScale;
  BackgroundColorCb.Selected := SuperLED.Color;
  AutoScaleLbl.Visible := SuperLED.AutoScale;

  if SuperLED.Active then
  begin
    LEDStatusLbl.Font.Color := SuperLED.ActiveColor;
    LEDStatusLbl.Caption := 'Active';
  end
  else
  begin
    LEDStatusLbl.Font.Color := SuperLED.InactiveColor;
    LEDStatusLbl.Caption := 'Inactive';
  end;

end;

procedure TSLTestFrm.UpdateWHStats;
begin
    WidthValLbl.Caption := IntToStr(SuperLED.Width);
    HeightValLbl.Caption := IntToStr(SuperLED.Height);
end;

procedure TSLTestFrm.UpdateLTStats;
begin
  LeftValLbl.Caption := IntToStr(SuperLED.Left);
  TopValLbl.Caption := IntToStr(SuperLED.Top);
end;

procedure TSLTestFrm.UpdateBorderStats;
begin
  BorderColorCb.Selected := SuperLED.BorderColor;
  BorderThicknessSpe.Value := SuperLED.BorderThickness;
  BorderStyleCb.ItemIndex := ord(SuperLed.BorderStyle);
  BorderOpacitySpe.Value := SuperLED.BorderOpacity;
  DrawingInactiveBrightnessSpe.Value := SuperLED.InactiveBrightness;
end;

procedure TSLTestFrm.UpdateDrawingStats;
begin
  DrawingActiveColorCb.Selected := SuperLED.ActiveColor;
  DrawingInactiveColorCb.Selected := SuperLED.InactiveColor;
  DrawingFillOpacitySpe.Value := SuperLED.FillOpacity;
  DrawingShapeCb.ItemIndex := ord(SuperLED.Shape);
  DrawingStyleCb.ItemIndex := ord(SuperLED.Style);
  DrawingRoundRadiusSpe.Value := SuperLED.RoundRadius;
  DrawingAngleSpe.Value := SuperLED.Angle;
end;

procedure TSLTestFrm.AboutMenuClick(Sender: TObject);
begin
  AboutFrm.VersionStr := VERSIONSTR;
  AboutFrm.show;
end;

procedure TSLTestFrm.AutoScaleCbChange(Sender: TObject);
begin
  SuperLED.AutoScale := AutoScaleCb.Checked;
  UpdateBasicStats;
end;

procedure TSLTestFrm.ComboBox1Change(Sender: TObject);
begin
  // set up any presets from combo here

  LEDDefaults;
  with SuperLED do
  begin
    case ComboBox1.ItemIndex of
    0:begin
        // reset to default, add anything else here for default if needed
      end;

    1:begin // Round Green/Red no transition in gradient colors
        ActiveColor := clLime;
        InactiveColor := clRed;
        Shape := slshRound;
        Style := slsShaded;
        InactiveBrightness := 0;  // allows full inactive color to be shown, no gradient
      end;
    2:begin // Triangle
      ActiveColor := clLime;
      InactiveColor := clRed;
      Shape := slshTriangle;
      Style := slsShaded;
      Angle := 60;  // Rotate so point down
      InactiveBrightness := 0;
      end;
    3:begin // Square
      ActiveColor := clLime;
      InactiveColor := clRed;
      Shape := slshSquare;
      Style := slsShaded;
      Angle := 45;  // Rotate so flat up
      InactiveBrightness := 0;
      end;
    4:begin // Pentagon
      ActiveColor := clLime;
      InactiveColor := clRed;
      Shape := slshPentagon;
      Style := slsShaded;
      Angle := 0;
      InactiveBrightness := 0;
      end;
    5:begin // Hexagon
      ActiveColor := clLime;
      InactiveColor := clRed;
      Shape := slshHexagon;
      Style := slsShaded;
      Angle := 0;
      InactiveBrightness := 0;
      end;
    6:begin // Triangle Rounded
      ActiveColor := clLime;
      InactiveColor := clRed;
      Shape := slshTriangle;
      Style := slsShaded;
      Angle := 60;  // Rotate so point down
      InactiveBrightness := 0;
      RoundRadius := 5;
      end;
    7:begin // Square Rounded
      ActiveColor := clLime;
      InactiveColor := clRed;
      Shape := slshSquare;
      Style := slsShaded;
      Angle := 45;  // Rotate so flat up
      InactiveBrightness := 0;
      RoundRadius := 5;
      end;
    8:begin // Flat Green Active, Red Inactive, Thick Border
        ActiveColor := clLime;
        InactiveColor := clRed;
        BorderThickness := 5;
        Shape := slshRound;
        Style := slsFlat;
        InactiveBrightness := 100;  // allows full inactive color to be shown, no gradient
      end;
    9:begin // Flat Green Active, Dark Green Inactive, Dotted Border
        ActiveColor := clLime;
        InactiveColor := clLime;
        BorderThickness := 3;
        BorderStyle := psDot;
        BorderColor := clBlack;
        Shape := slshRound;
        Style := slsFlat;
        InactiveBrightness := 20;  // light green for off
      end;
    10:begin
        ActiveColor := clLime;
        InactiveColor := clBlack;
        BorderThickness := 5;
        Shape := slshSquare;
        Style := slsShaded;
        Width := Height * 2;
        Left := 116 - Width div 2; // fudge it to the center of the rectangle
        Angle := 45;  // Rotate so flat up
        InactiveBrightness := 25; // just a touch of color
        RoundRadius := 5;
      end;
    11:begin
        ActiveColor := clLime;
        InactiveColor := clBlack;
        BorderThickness := 5;
        Shape := slshRound;
        Style := slsShaded;
        Width := Height * 2;
        Left := 116 - Width div 2; // fudge it to the center of the rectangle
        Angle := 0;  // No rotation on slshRound types!
        InactiveBrightness := 25; // just a touch of color
        RoundRadius := 5;
      end;
    end;
  end;
  UpdateAllStats;
end;

procedure TSLTestFrm.FormConstrainedResize(Sender: TObject; var MinWidth,
  MinHeight, MaxWidth, MaxHeight: TConstraintSize);
begin
  UpdateAllStats;
end;

procedure TSLTestFrm.MakeBigBtnClick(Sender: TObject);
begin
  with SuperLED do
  begin
    Height := Height * 2;
    Width := Width * 2;
    Left := 100 - Width div 2;
    Top := 100 - Height div 2;
  end;
  UpdateWHStats;
  UpdateLTStats;
end;

procedure TSLTestFrm.SuperLEDDblClick(Sender: TObject);
begin
  // Double Click just mess with colors to see if it works
  // Won't do much if the user changes the colors

  if SuperLED.ActiveColor = clRed then
    SuperLED.ActiveColor := clLime
  else
    SuperLED.ActiveColor := clRed;
  SuperLED.Active := True;  // force active just because
  UpdateBasicStats;
end;

procedure TSLTestFrm.ToggleStateBitBtnClick(Sender: TObject);
begin
  SuperLED.Active := not SuperLED.Active;
  ActiveStateCb.Checked := SuperLED.Active;
  UpdateBasicStats;
end;

procedure TSLTestFrm.BorderColorCbChange(Sender: TObject);
begin
  SuperLED.BorderColor := BorderColorCb.Selected;
end;

procedure TSLTestFrm.BackgroundColorCbChange(Sender: TObject);
begin
  SuperLED.Color := BackgroundColorCb.Selected;
end;

procedure TSLTestFrm.BorderOpacitySpeChange(Sender: TObject);
begin
  SuperLED.BorderOpacity := BorderOpacitySpe.Value;
end;

procedure TSLTestFrm.BorderStyleCbChange(Sender: TObject);
var
  bs : integer;
begin
  // assumes items are matching in the cb

  bs := BorderStyleCb.ItemIndex;
  SuperLED.BorderStyle := TPenStyle(bs);
end;

end.

