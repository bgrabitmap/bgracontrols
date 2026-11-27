# CSS RGB parser regression

Checks `BCColors.TryParseCSSRGB`:

- absolute `rgb()`/`rgba()` in space and comma syntax: channels, percentages,
  exponents, clamping and rounding;
- relative `rgb(from <origin> r g b [/ alpha])`: the origin is returned as
  text, not resolved; alpha is kept or overridden;
- valid origins: hex, named and system colors, nested functions up to 32
  levels;
- rejection of invalid syntax and of unknown or unsupported origins.

FPCUnit program built with the `nogui` widgetset; `.github/workflows/make.pas`
builds and runs it. By hand:

```sh
lazbuild --build-all test_cssrgb.lpi
./test_cssrgb --all --format=plain
```
