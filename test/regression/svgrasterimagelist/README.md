# TBGRASVGRasterImageList

This optional component is declared in `BGRASVGImageList` and registered on
the BGRA Themes palette. It inherits from `TImageList`, so it can be assigned
directly to standard LCL controls' `Images` properties.

```pascal
uses BGRASVGImageList;

Images := TBGRASVGRasterImageList.Create(Form1);
Images.AddSVG('<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16">' +
  '<rect width="16" height="16" fill="red"/></svg>');
Button1.Images := Images;
Button1.ImageIndex := 0;
```

Use `AddSVG`, `RemoveSVG`, `ReplaceSVG`, `ExchangeSVG` and `ClearSVG` to edit
SVG entries. `SVGCount` and `SVGString[Index]` refer to the SVG sources;
inherited `Count`, `Draw` and `GetBitmap` provide the normal raster image list.
Generated raster entries are rebuilt from the sources, so edit SVG entries
through the SVG methods rather than mixing them with raster-only additions.

The component owns an internal `TBGRASVGImageList` whose `TargetRasterImageList`
points to it. SVG parsing, rendering and serialization use that implementation.
The original standalone component and its API remain available unchanged.

Width and Height follow native `TImageList` sizing at 96 DPI. Resolutions for
100%, 125%, 150% and 200% are generated automatically, with duplicate widths
removed for small images. Additional widths can be registered using the
inherited `RegisterResolutions` API. Base-size changes discard additional
resolutions, as with a normal `TImageList`. The `Scaled` setting is preserved.

SVG sources are stored in the component's `SVGItems` binary property. Saved
components restore their SVG data and custom resolutions and remain editable
after loading. This format belongs to the new class; existing
`TBGRASVGImageList` forms do not need conversion.

## Tests

Open `test_raster_list.lpi` in Lazarus with the local packages registered, or:

```sh
lazbuild --ws=gtk2 test_raster_list.lpi
./test_raster_list
```

The test covers standard control assignment, immediate raster availability,
SVG edits, size changes through both class types, native DPI scaling,
resolution deduplication, clearing, malformed input recovery and streaming.

The standalone `TImageList` use case was proposed in
[ovidiopr's PR #273](https://github.com/bgrabitmap/bgracontrols/pull/273).
This composition approach preserves compatibility with existing projects.
