# SVG image list regression tests

Open `test_compatibility.lpi` in Lazarus with the local BGRABitmap and
BGRAControls packages registered, or build it with:

```sh
lazbuild --ws=gtk2 test_compatibility.lpi
./test_compatibility legacy.lfm
```

Use the normal platform widgetset on Windows or macOS. The executable takes
the path to the legacy fixture as its first argument.

The tests cover rendering without a raster target, cache replacement and
reordering, the original `Count` and `PopulateImageList` API, invalid input,
optional automatic rasterization, dimensions, cancellation of queued work,
and loading an unchanged legacy form with its `Items` payload and target
reference. Color override tests cover alpha preservation, restoring original
colors, unchanged SVG source, system color resolution, background-safe drawing,
canvas transforms, Canvas2D clipping and state preservation, untouched transparent
pixels and deferred raster refresh. Additional checks compare same-color override
rendering pixel by pixel on an opaque background (including overflowing strokes),
verify restoration of the canvas blending mode, check raster target colors after
changes and round-trip a non-default system color through component streaming.
A short-lived thread initializes FPC 3.2.2's deferred queue behavior.

## Runtime icon colors (issue #288)

```pascal
SVGImages.OverrideColor := clBtnText; // monochrome icons using the system color
SVGImages.OverrideColor := clNone;   // restore original SVG colors (default)
```

The override applies to bitmap generation, direct drawing and raster image list
population without modifying `SVGString`. It replaces all RGB colors, including
explicit SVG fills and strokes, while preserving opacity and antialiasing.
Changes queue a rebuild of `TargetRasterImageList` on the main thread.

When the system theme changes, call `SVGImages.Refresh` to queue a rebuild even
if the symbolic color is unchanged. Direct drawing resolves system colors on
every call; invalidate controls that draw the SVGs themselves. Image lists filled
manually with `PopulateImageList` must be repopulated manually.

`legacy.lfm` was generated with the unmodified `TBGRASVGImageList` from
BGRAControls commit `0bfa628` using Lazarus 4.8/FPC 3.2.2. Do not regenerate
it with the new implementation: it is a backward compatibility fixture.

The SVG cache and safe bitmap conversion improvements were adapted from
[ovidiopr's contribution in PR #273](https://github.com/bgrabitmap/bgracontrols/pull/273),
while retaining the original component and its serialized format.
