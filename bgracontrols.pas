{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit bgracontrols;

interface

uses
  BCBaseCtrls, BCButton, BCEffect, bcfilters, BCGameGrid, BCImageButton, 
  BCLabel, BCPanel, BCRTTI, BCStylesForm, BCTools, BGRAFlashProgressBar, 
  BGRAGraphicControl, BGRAImageList, BGRAImageManipulation, BGRAKnob, 
  BGRAShape, BGRASpeedButton, BGRASpriteAnimation, BGRAVirtualScreen, 
  BGRAResizeSpeedButton, LazarusPackageIntf;

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
  RegisterUnit('BGRAShape', @BGRAShape.Register);
  RegisterUnit('BGRASpeedButton', @BGRASpeedButton.Register);
  RegisterUnit('BGRASpriteAnimation', @BGRASpriteAnimation.Register);
  RegisterUnit('BGRAVirtualScreen', @BGRAVirtualScreen.Register);
  RegisterUnit('BGRAResizeSpeedButton', @BGRAResizeSpeedButton.Register);
end;

initialization
  RegisterPackage('bgracontrols', @Register);
end.
