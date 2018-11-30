{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit bcReg;

{$I bgracontrols.inc}

interface

uses
  Classes, SysUtils, BCBaseCtrls,
  BCButton, BCButtonFocus, BCEffect, bcfilters, BCGameGrid, BCImageButton,
  BCLabel, BCListBox, BCMaterialDesignButton, BCPanel, BCRadialProgressBar,
  BCRTTI, BCSamples, BCStylesForm, BCSVGButton, BCSVGViewer, BCToolBar,
  BCTrackbarUpdown, BGRAFlashProgressBar, BGRAGraphicControl,
  BGRAImageList, BGRAImageManipulation, BGRAKnob, BGRAResizeSpeedButton,
  BGRAShape, BGRASpeedButton, BGRASpriteAnimation, BGRAVirtualScreen,
  ColorSpeedButton, DTAnalogClock, DTAnalogGauge, dtthemedclock,
  dtthemedgauge, MaterialColors, bcmdbutton, bcmdbuttonfocus;

procedure Register;

implementation

procedure Register;
begin
  {$IFDEF FPC}//#
  {$I images\bgracontrols_images.lrs}
  {$I icons\BGRAImageManipulation_icon.lrs}
  {$I icons\bcsvgbutton.lrs}
  {$I icons\bcimagebutton_icon.lrs}
  {$I icons\bcgamegrid_icon.lrs}
  {$I icons\bcradialprogressbar_icon.lrs}
  {$ENDIF}

  RegisterNoIcon([TBCCustomControl]);
//  RegisterComponents('BGRA Custom Drawn', [TBCDButton, TBCDEdit,
//    TBCDStaticText, TBCDProgressBar, TBCDSpinEdit, TBCDCheckBox, TBCDRadioButton, TBCDPanel]);

  RegisterComponents('BGRA Controls', [TBGRAShape, TBCListBox, TBCPaperPanel, TBCPaperListBox,
  TBCButton, TBCButtonFocus, TDTThemedGauge, TBCLabel, TBCImageButton, TBCXButton, TBCGameGrid,
  TDTThemedClock, TDTAnalogGauge, TDTAnalogClock, TColorSpeedButton,
  TBGRAVirtualScreen, TBGRASpriteAnimation, TBGRASpeedButton, TBGRAResizeSpeedButton,
  TBGRAKnob, TBGRAImageManipulation, TBGRAImageList, TBGRAGraphicControl, TBGRAFlashProgressBar,
  TBCTrackbarUpdown, TBCToolBar, TBCSVGViewer, TBCSVGButton, TBCRadialProgressBar,
  TBCPanel,TBCMDButtonFocus, TBCMDButton, TBCMaterialDesignButton
  {TBCDefaultThemeManager, TBCKeyboard, TBCNumericKeyboard, TBCRealNumericKeyboard}]);


{$IFDEF FPC}
   RegisterPropertyEditor(TypeInfo(TBCListBox),TBCPaperListBox, 'ListBox', TClassPropertyEditor);
   RegisterPropertyEditor(TypeInfo(integer), TBCButton,'ImageIndex', TBCButtonImageIndexPropertyEditor);
   RegisterPropertyEditor(TypeInfo(integer), TBCButtonFocus,'ImageIndex', TBCButtonImageIndexPropertyEditor);
   RegisterPropertyEditor(TypeInfo(TBCListBox), TBCPaperListBox, 'ListBox', TClassPropertyEditor);
{$ENDIF}
end;

end.

