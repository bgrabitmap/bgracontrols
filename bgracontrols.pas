{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit bgracontrols;

{$warn 5023 off : no warning about unused units}
interface

uses
  ATShapeLineBGRA, BCBaseCtrls, BCBrightAndContrast, BCButton, BCButtonFocus, 
  BCCheckComboBox, BCComboBox, BCDefaultThemeManager, BCEffect, 
  BCExpandPanels, BCFilters, BCFluentProgressRing, BCFluentSlider, BCGameGrid, 
  BCGradientButton, BCImageButton, BCKeyboard, BCLabel, BCListBox, 
  BCListBoxEx, BCMaterialDesignButton, BCMaterialEdit, 
  BCMaterialFloatSpinEdit, BCMaterialProgressBarMarquee, BCMaterialSpinEdit, 
  BCMDButton, BCMDButtonFocus, BCNumericKeyboard, BCPanel, 
  BCRadialProgressBar, BCRoundedImage, BCRTTI, BCSamples, BCStylesForm, 
  BCSVGButton, BCSVGViewer, BCThemeManager, BCToolBar, BCTools, 
  BCTrackbarUpdown, BCTypes, BGRAColorTheme, BGRAControlsInfo, 
  BGRACustomDrawn, BGRAFlashProgressBar, BGRAGraphicControl, BGRAImageList, 
  BGRAImageManipulation, BGRAImageTheme, BGRAKnob, BGRAShape, 
  BGRAResizeSpeedButton, BGRASpeedButton, BGRASpriteAnimation, 
  BGRASVGImageList, BGRASVGTheme, BGRATheme, BGRAThemeButton, 
  BGRAThemeCheckBox, BGRAThemeRadioButton, BGRAVirtualScreen, 
  ColorSpeedButton, DTAnalogClock, DTAnalogCommon, DTAnalogGauge, 
  DTThemedClock, DTThemedGauge, MaterialColors, bgrasvgimagelistform, 
  BCLeaLCDDisplay, BCLeaLED, BCLeaQLED, BCLeaRingSlider, BCLeaSelector, 
  BCLeaTheme, BCLeaLCDDisplay_EditorRegister, BCLeaBoard, BCLeaEngrave, 
  SuperGauge, SuperGaugeCommon, SuperLED, BGRADialogs, BGRAFormatUI,
  SuperSpinner, SuperSpinnerCommon, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ATShapeLineBGRA', @ATShapeLineBGRA.Register);
  RegisterUnit('BCButton', @BCButton.Register);
  RegisterUnit('BCButtonFocus', @BCButtonFocus.Register);
  RegisterUnit('BCCheckComboBox', @BCCheckComboBox.Register);
  RegisterUnit('BCComboBox', @BCComboBox.Register);
  RegisterUnit('BCDefaultThemeManager', @BCDefaultThemeManager.Register);
  RegisterUnit('BCExpandPanels', @BCExpandPanels.Register);
  RegisterUnit('BCFluentProgressRing', @BCFluentProgressRing.Register);
  RegisterUnit('BCFluentSlider', @BCFluentSlider.Register);
  RegisterUnit('BCGameGrid', @BCGameGrid.Register);
  RegisterUnit('BCGradientButton', @BCGradientButton.Register);
  RegisterUnit('BCImageButton', @BCImageButton.Register);
  RegisterUnit('BCKeyboard', @BCKeyboard.Register);
  RegisterUnit('BCLabel', @BCLabel.Register);
  RegisterUnit('BCListBox', @BCListBox.Register);
  RegisterUnit('BCMaterialDesignButton', @BCMaterialDesignButton.Register);
  RegisterUnit('BCMaterialEdit', @BCMaterialEdit.Register);
  RegisterUnit('BCMaterialFloatSpinEdit', @BCMaterialFloatSpinEdit.Register);
  RegisterUnit('BCMaterialProgressBarMarquee', 
    @BCMaterialProgressBarMarquee.Register);
  RegisterUnit('BCMaterialSpinEdit', @BCMaterialSpinEdit.Register);
  RegisterUnit('BCMDButton', @BCMDButton.Register);
  RegisterUnit('BCMDButtonFocus', @BCMDButtonFocus.Register);
  RegisterUnit('BCNumericKeyboard', @BCNumericKeyboard.Register);
  RegisterUnit('BCPanel', @BCPanel.Register);
  RegisterUnit('BCRadialProgressBar', @BCRadialProgressBar.Register);
  RegisterUnit('BCRoundedImage', @BCRoundedImage.Register);
  RegisterUnit('BCSVGButton', @BCSVGButton.Register);
  RegisterUnit('BCSVGViewer', @BCSVGViewer.Register);
  RegisterUnit('BCToolBar', @BCToolBar.Register);
  RegisterUnit('BCTrackbarUpdown', @BCTrackbarUpdown.Register);
  RegisterUnit('BGRAColorTheme', @BGRAColorTheme.Register);
  RegisterUnit('BGRACustomDrawn', @BGRACustomDrawn.Register);
  RegisterUnit('BGRAFlashProgressBar', @BGRAFlashProgressBar.Register);
  RegisterUnit('BGRAGraphicControl', @BGRAGraphicControl.Register);
  RegisterUnit('BGRAImageList', @BGRAImageList.Register);
  RegisterUnit('BGRAImageManipulation', @BGRAImageManipulation.Register);
  RegisterUnit('BGRAImageTheme', @BGRAImageTheme.Register);
  RegisterUnit('BGRAKnob', @BGRAKnob.Register);
  RegisterUnit('BGRAShape', @BGRAShape.Register);
  RegisterUnit('BGRAResizeSpeedButton', @BGRAResizeSpeedButton.Register);
  RegisterUnit('BGRASpeedButton', @BGRASpeedButton.Register);
  RegisterUnit('BGRASpriteAnimation', @BGRASpriteAnimation.Register);
  RegisterUnit('BGRASVGImageList', @BGRASVGImageList.Register);
  RegisterUnit('BGRASVGTheme', @BGRASVGTheme.Register);
  RegisterUnit('BGRATheme', @BGRATheme.Register);
  RegisterUnit('BGRAThemeButton', @BGRAThemeButton.Register);
  RegisterUnit('BGRAThemeCheckBox', @BGRAThemeCheckBox.Register);
  RegisterUnit('BGRAThemeRadioButton', @BGRAThemeRadioButton.Register);
  RegisterUnit('BGRAVirtualScreen', @BGRAVirtualScreen.Register);
  RegisterUnit('ColorSpeedButton', @ColorSpeedButton.Register);
  RegisterUnit('DTAnalogClock', @DTAnalogClock.Register);
  RegisterUnit('DTAnalogGauge', @DTAnalogGauge.Register);
  RegisterUnit('DTThemedClock', @DTThemedClock.Register);
  RegisterUnit('DTThemedGauge', @DTThemedGauge.Register);
  RegisterUnit('BCLeaLCDDisplay', @BCLeaLCDDisplay.Register);
  RegisterUnit('BCLeaLED', @BCLeaLED.Register);
  RegisterUnit('BCLeaQLED', @BCLeaQLED.Register);
  RegisterUnit('BCLeaRingSlider', @BCLeaRingSlider.Register);
  RegisterUnit('BCLeaSelector', @BCLeaSelector.Register);
  RegisterUnit('BCLeaTheme', @BCLeaTheme.Register);
  RegisterUnit('BCLeaLCDDisplay_EditorRegister', 
    @BCLeaLCDDisplay_EditorRegister.Register);
  RegisterUnit('BCLeaBoard', @BCLeaBoard.Register);
  RegisterUnit('BCLeaEngrave', @BCLeaEngrave.Register);
  RegisterUnit('SuperGauge', @SuperGauge.Register);
  RegisterUnit('SuperLED', @SuperLED.Register);
  RegisterUnit('BGRADialogs', @BGRADialogs.Register);
  RegisterUnit('SuperSpinner', @SuperSpinner.Register);
end;

initialization
  RegisterPackage('bgracontrols', @Register);
end.
