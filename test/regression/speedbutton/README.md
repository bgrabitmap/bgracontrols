# SpeedButton regression tests

`test_images` checks TImageList and TBGRAImageList rendering in TBGRASpeedButton,
TColorSpeedButton and TBGRAResizeSpeedButton, across all five button states.
The rendered pixels are compared with TSpeedButton, including explicit
pressed/disabled images and fallback states (issue #161).

`test_dpi` checks that a Glyph is drawn at the size and position computed by
LCL for the current DPI. TBGRASpeedButton and TColorSpeedButton are compared
with TSpeedButton at 96, 120, 144, 168, 192 and 288 PPI by looking at the
visible bounds of the rendered icon. Exact pixels are not compared because
BGRA and LCL use different resampling.

Open `test_images.lpi` or `test_dpi.lpi`, or build with
`lazbuild --ws=gtk2 test_images.lpi` (same for `test_dpi`).
On Linux, an isolated display can be used: `xvfb-run -a ./test_dpi`.
Successful checks print `PASS`; failures print `FAIL` and return a nonzero
exit code. The interactive example is in `test/test_speedbutton_images`.
