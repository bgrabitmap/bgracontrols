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
reference. A short-lived thread initializes FPC 3.2.2's deferred queue behavior.

`legacy.lfm` was generated with the unmodified `TBGRASVGImageList` from
BGRAControls commit `0bfa628` using Lazarus 4.8/FPC 3.2.2. Do not regenerate
it with the new implementation: it is a backward compatibility fixture.

The SVG cache and safe bitmap conversion improvements were adapted from
[ovidiopr's contribution in PR #273](https://github.com/bgrabitmap/bgracontrols/pull/273),
while retaining the original component and its serialized format.
