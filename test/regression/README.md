# Regression tests

This directory contains automated checks. Interactive component demos remain
in the other `test/` directories.

| Directory | Checks |
| --- | --- |
| `bckeyboard` | Queued callbacks and active-control lifetime |
| `bcpanel` | Mouse wheel propagation and consumed events (Linux/X11) |
| `bccheckcombobox` | Item/check lifetime, legacy checklist API, Space and popup recreation |
| `themefpu` | Theme paint exception masks, invalid math and exception cleanup |
| `speedbutton` | Image lists, button states and DPI glyph bounds compared with TSpeedButton |
| `svgimagelist` | Original SVG API, rendering and legacy form compatibility |
| `svgrasterimagelist` | Native image list integration, DPI and serialization |
| `lealed` | Redraw memory stability and unchanged rendering |
| [`cssrgb`](cssrgb/README.md) | `BCColors.TryParseCSSRGB` syntax, channels, alpha and nesting limits (FPCUnit) |
| `color_value` | Color values: symbolic identity, text and CSS conversion, alpha round trips (FPCUnit) |
| [`color_svg`](color_svg/README.md) | BC color parser compared with BGRABitmap and SVG rendering (FPCUnit) |
| [`color_editor`](color_editor/README.md) | Color API, LFM streaming, property editors and automated picker OK/Cancel |
| [`legacy_colors`](legacy_colors/README.md) | Legacy TColor + opacity LFM pairs of BCComboBox read through the picker |

Open each `.lpi` in Lazarus with the local BGRABitmap/BGRAControls packages
registered. Alternatively, run `lazbuild project.lpi` followed by the executable
from its directory (`--ws=gtk2` for GTK2 on Linux). The SVG compatibility test
requires `legacy.lfm` as its argument.

Successful checks print `PASS`; failures print `FAIL` and return a nonzero exit
code. These tests use the LCL: Linux needs a graphical display, and the LED
test briefly shows a window. See the individual READMEs for details.

The FPCUnit tests are the exception: they are built with the `nogui` widgetset
(set in their `.lpi`) and need no display, so `.github/workflows/make.pas`
builds and runs them on CI (`--all --format=plain`). It only compiles the
other tests.
