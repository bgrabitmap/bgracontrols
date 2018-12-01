{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit bgracontrols;

{$warn 5023 off : no warning about unused units}
interface

uses
  BCButton, BCButtonFocus, BCEffect, bcfilters, BCGameGrid, BCImageButton, 
  BCLabel, BCListBox, BCMaterialDesignButton, BCMDButton, BCMDButtonFocus, 
  BCPanel, BCRadialProgressBar, BCRTTI, BCSamples, BCStylesForm, BCSVGButton, 
  BCSVGViewer, BCToolBar, BCTrackbarUpdown, BGRACustomDrawn, 
  BGRAFlashProgressBar, BGRAGraphicControl, BGRAImageList, 
  BGRAImageManipulation, BGRAKnob, BGRAResizeSpeedButton, BGRAShape, 
  BGRASpeedButton, BGRASpriteAnimation, BGRAVirtualScreen, ColorSpeedButton, 
  DTAnalogClock, DTAnalogCommon, DTAnalogGauge, dtthemedclock, dtthemedgauge, 
  MaterialColors, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('BCButton', @BCButton.Register);
  RegisterUnit('BCButtonFocus', @BCButtonFocus.Register);
  RegisterUnit('BCGameGrid', @BCGameGrid.Register);
  RegisterUnit('BCImageButton', @BCImageButton.Register);
  RegisterUnit('BCLabel', @BCLabel.Register);
  RegisterUnit('BCListBox', @BCListBox.Register);
  RegisterUnit('BCMaterialDesignButton', @BCMaterialDesignButton.Register);
  RegisterUnit('BCMDButton', @BCMDButton.Register);
  RegisterUnit('BCMDButtonFocus', @BCMDButtonFocus.Register);
  RegisterUnit('BCPanel', @BCPanel.Register);
  RegisterUnit('BCRadialProgressBar', @BCRadialProgressBar.Register);
  RegisterUnit('BCSVGButton', @BCSVGButton.Register);
  RegisterUnit('BCSVGViewer', @BCSVGViewer.Register);
  RegisterUnit('BCToolBar', @BCToolBar.Register);
  RegisterUnit('BCTrackbarUpdown', @BCTrackbarUpdown.Register);
  RegisterUnit('BGRAFlashProgressBar', @BGRAFlashProgressBar.Register);
  RegisterUnit('BGRAGraphicControl', @BGRAGraphicControl.Register);
  RegisterUnit('BGRAImageList', @BGRAImageList.Register);
  RegisterUnit('BGRAImageManipulation', @BGRAImageManipulation.Register);
  RegisterUnit('BGRAKnob', @BGRAKnob.Register);
  RegisterUnit('BGRAResizeSpeedButton', @BGRAResizeSpeedButton.Register);
  RegisterUnit('BGRAShape', @BGRAShape.Register);
  RegisterUnit('BGRASpeedButton', @BGRASpeedButton.Register);
  RegisterUnit('BGRASpriteAnimation', @BGRASpriteAnimation.Register);
  RegisterUnit('BGRAVirtualScreen', @BGRAVirtualScreen.Register);
  RegisterUnit('ColorSpeedButton', @ColorSpeedButton.Register);
  RegisterUnit('DTAnalogClock', @DTAnalogClock.Register);
  RegisterUnit('DTAnalogGauge', @DTAnalogGauge.Register);
  RegisterUnit('dtthemedclock', @dtthemedclock.Register);
  RegisterUnit('dtthemedgauge', @dtthemedgauge.Register);
end;

initialization
  RegisterPackage('bgracontrols', @Register);
end.
