# CSS color / SVG compatibility

For each color text, compares:

- `TryStrToBC` (BCColorUtils): acceptance and pixel;
- `TryStrToBGRA` and `StrToBGRA` (BGRABitmap): acceptance and pixel;
- a 16x16 SVG rectangle filled with it, as a `fill` attribute and as an
  inline style; one alpha step is allowed.

Expected differences, checked explicitly:

- space-separated, relative and decimal RGB, and percentage alpha: accepted by
  BC only;
- `rgb(50%,0,0)`: red 128 in BC, 127 in BGRABitmap/SVG;
- `rgb 1,2,3` (no parentheses): accepted by BGRABitmap only;
- partial channels (`?`, empty): BGRABitmap only, checked separately;
- system names (`ThreeDFace`, `clBtnFace`): BC only; SVG uses its black
  fallback, also with a separate `fill-opacity`;
- `BCToCSS`: symbolic output keeps the system name; resolved hex output renders
  in SVG.

FPCUnit program built with the `nogui` widgetset; `.github/workflows/make.pas`
builds and runs it. With `nogui` system colors resolve to black; build with
`--ws=win32` to check real system RGB. By hand:

```sh
lazbuild --build-all test_color_svg.lpi
./test_color_svg --all --format=plain
```
