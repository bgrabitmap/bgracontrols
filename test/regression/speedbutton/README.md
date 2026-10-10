# SpeedButton image lists regression

Checks TImageList and TBGRAImageList rendering in TBGRASpeedButton,
TColorSpeedButton and TBGRAResizeSpeedButton, across all five button states.
The rendered pixels are compared with TSpeedButton, including explicit
pressed/disabled images and fallback states (issue #161).

Open `test_images.lpi` or build with `lazbuild --ws=gtk2 test_images.lpi`.
On Linux, an isolated display can be used: `xvfb-run -a ./test_images`.
Successful checks print `PASS`; failures print `FAIL` and return a nonzero
exit code. The interactive example is in `test/test_speedbutton_images`.
