Label/panel Scaled regression test
=================================

Build test_scaling.lpi with Lazarus and run with font height 16 or 0 (default):

    xvfb-run -a -s "-screen 0 1280x1024x24 -dpi 144" ./test_scaling 16

Repeat for X-server DPI 96, 144 and 192 and both font heights. The test prints
Screen.PixelsPerInch, which may differ slightly because of physical-size rounding.
Use QT_QPA_PLATFORM=xcb for Qt5 runs.

The test measures text and colored-border pixels in actual screenshots, comparing
TBCLabel and TBCPanel with TBCButton. It alternates Scaled false/true twice,
checks that styles retain their stored values, checks scaled panel child insets,
and verifies that label AutoSize grows and restores its original dimensions.
The button reference is explicitly redrawn after toggling its Scaled property.

Scaled defaults to false. When true, these controls render scaled copies of their
styles using Screen.PixelsPerInch / the containing form's DesignTimePPI, consistent
with TBCButton's DPI scaling. Stored styles are not rewritten. This is screen-wide
DPI handling; native per-monitor transitions and Retina backing-store scaling
are not covered by this test.
GTK2 snapshots provide measurable colored borders. In the tested Qt5 setup,
colored-border sampling yields zero and is reported as skipped; text, AutoSize,
style preservation and client-inset checks remain effective on both widgetsets.

FontEx.Height = 0 retains automatic system-DPI font sizing. Explicit heights
are scaled like the button reference. The default-font cases check that enabling
Scaled does not enlarge the already DPI-adjusted system font a second time.
