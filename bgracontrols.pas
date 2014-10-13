{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit bgracontrols;

interface

uses
  BCBaseCtrls, BCButton, BCEffect, bcfilters, BCGameGrid, BCImageButton, 
  BCLabel, BCPanel, BCRTTI, BCStylesForm, BCTileMap, BCTools, 
  BGRAFlashProgressBar, BGRAGraphicControl, BGRAImageList, 
  BGRAImageManipulation, BGRAKnob, BGRALED, BGRAResizeSpeedButton, BGRARKnob, 
  BGRAScript, BGRAShape, BGRASpeedButton, BGRASpriteAnimation, 
  BGRAVirtualScreen, DTAnalogClock, DTAnalogCommon, DTAnalogGauge, 
  dtthemedclock, dtthemedgauge, uEKnob, ueled, uEMultiTurn, uERotImage, 
  uESelector, BGRAPascalScript, uPSI_BGRAPascalScript, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('BCButton', @BCButton.Register);
  RegisterUnit('BCGameGrid', @BCGameGrid.Register);
  RegisterUnit('BCImageButton', @BCImageButton.Register);
  RegisterUnit('BCLabel', @BCLabel.Register);
  RegisterUnit('BCPanel', @BCPanel.Register);
  RegisterUnit('BGRAFlashProgressBar', @BGRAFlashProgressBar.Register);
  RegisterUnit('BGRAGraphicControl', @BGRAGraphicControl.Register);
  RegisterUnit('BGRAImageList', @BGRAImageList.Register);
  RegisterUnit('BGRAImageManipulation', @BGRAImageManipulation.Register);
  RegisterUnit('BGRAKnob', @BGRAKnob.Register);
  RegisterUnit('BGRALED', @BGRALED.Register);
  RegisterUnit('BGRAResizeSpeedButton', @BGRAResizeSpeedButton.Register);
  RegisterUnit('BGRARKnob', @BGRARKnob.Register);
  RegisterUnit('BGRAShape', @BGRAShape.Register);
  RegisterUnit('BGRASpeedButton', @BGRASpeedButton.Register);
  RegisterUnit('BGRASpriteAnimation', @BGRASpriteAnimation.Register);
  RegisterUnit('BGRAVirtualScreen', @BGRAVirtualScreen.Register);
  RegisterUnit('DTAnalogClock', @DTAnalogClock.Register);
  RegisterUnit('DTAnalogGauge', @DTAnalogGauge.Register);
  RegisterUnit('dtthemedclock', @dtthemedclock.Register);
  RegisterUnit('dtthemedgauge', @dtthemedgauge.Register);
  RegisterUnit('uEKnob', @uEKnob.Register);
  RegisterUnit('ueled', @ueled.Register);
  RegisterUnit('uEMultiTurn', @uEMultiTurn.Register);
  RegisterUnit('uERotImage', @uERotImage.Register);
  RegisterUnit('uESelector', @uESelector.Register);
  RegisterUnit('uPSI_BGRAPascalScript', @uPSI_BGRAPascalScript.Register);
end;

initialization
  RegisterPackage('bgracontrols', @Register);
end.
