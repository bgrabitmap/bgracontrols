# Color values, property editors and picker regression

Checks, with the picker driven automatically:

- `TryBCColor` and all short hex values;
- `TBCColorString`, `TBCPixel`, `TBCCustomColor` and CSS text property
  editors: registration, attributes, multi-selection, inherited revert;
- swatch drawing;
- picker OK/Cancel in pixel, system-color and CSS text modes: wheel, light and
  opacity bars, channel fields, typed text, color list, aliases;
- `TBCColor`: value API, change notifications, `Assign`, conversions;
- `TBCColor` LFM streaming (`Color = '#RRGGBBAA'` and `KindName`): every kind
  with every alpha byte, literal LFM text, rejection of other `Color` data and
  of `Alpha`; not written when equal to the ancestor; the picker opened on
  loaded values keeps them on OK;
- `TBCLinkedColor`: reads and writes the owner's `TColor` + opacity pair, which
  alone is streamed.

Needs a real widgetset and a display: `.github/workflows/make.pas` only
compiles it. By hand:

```sh
lazbuild --build-all test_color_editor.lpi
./test_color_editor # .exe on Windows
```

Demo options instead of the checks: `--picker`, `--picker-pixel`,
`--picker-system`, `--picker-css` open the picker in that mode;
`--picker-shot <file> [<text> [<new text>]]` saves a picture of it.

Runtime checks and heap tracing:

```sh
lazbuild --build-all --opt='-Cr -Co -gh -gl' test_color_editor.lpi
```
