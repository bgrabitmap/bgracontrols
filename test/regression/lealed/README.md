# TBCLeaLED redraw regression

Build `test_redraw.lpi` using the local BGRABitmap/BGRAControls packages:

```sh
lazbuild --ws=gtk2 test_redraw.lpi
./test_redraw images
```

Use the platform's normal widgetset on Windows. A small test window appears
briefly while the control is drawn. The optional output directory receives
six PNG images covering each style in both lit and unlit states.

After warming up rendering, the test checks that 64 lit redraws retain no
significant Pascal heap allocation. It also exercises value/style/enabled
changes and a one-pixel control. The 4 KiB tolerance excludes minor runtime
bookkeeping; the original bug retains approximately 670 KiB over this loop.

The six pixel fingerprints printed by the test can be compared before and
after the fix on the same platform. The test reproduces the bitmap ownership
issue reported by gefest1980 in
[issue #275](https://github.com/bgrabitmap/bgracontrols/issues/275).
