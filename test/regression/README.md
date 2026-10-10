# Regression tests

This directory contains automated checks. Interactive component demos remain
in the other `test/` directories.

| Directory | Checks |
| --- | --- |
| `bckeyboard` | Queued callbacks and active-control lifetime |
| `bcpanel` | Mouse wheel propagation and consumed events (Linux/X11) |
| `speedbutton` | Image lists and button states compared with TSpeedButton |
| `svgimagelist` | Original SVG API, rendering and legacy form compatibility |
| `svgrasterimagelist` | Native image list integration, DPI and serialization |
| `lealed` | Redraw memory stability and unchanged rendering |

Open each `.lpi` in Lazarus with the local BGRABitmap/BGRAControls packages
registered. Alternatively, run `lazbuild project.lpi` followed by the executable
from its directory (`--ws=gtk2` for GTK2 on Linux). The SVG compatibility test
requires `legacy.lfm` as its argument.

Successful checks print `PASS`; failures print `FAIL` and return a nonzero exit
code. These tests use the LCL: Linux needs a graphical display, and the LED
test briefly shows a window. See the individual READMEs for details.
