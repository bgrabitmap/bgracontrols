# Legacy LFM colors and the picker

Loads `umain.lfm`: four `TBCComboBox` controls saved with the original `TColor`
and opacity properties (custom RGB, system colors, `clNone`, `clDefault`, LCL
colors without a CSS name, alpha 76 and 178).

Checks:

- literal values read from the LFM and the combo items;
- 100 linked pairs (focus border; per normal, hover and clicked state:
  background, border, light border, font shadow, both gradients' start and
  end): each `Alpha*` property equals its owner's pair, its text reads back to
  the same value, the picker shows that text, and OK leaves the pair
  unchanged.

Needs a real widgetset and a display: `.github/workflows/make.pas` only
compiles it. By hand:

```sh
lazbuild --build-all test_legacy_colors.lpi
./test_legacy_colors # .exe on Windows
```

`--demo` shows the form instead of running the checks.
