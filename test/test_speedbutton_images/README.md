# SpeedButton image lists

Open `test_images.lpi` with the local BGRAControls package registered. The four
columns compare TSpeedButton, TBGRASpeedButton, TColorSpeedButton and
TBGRAResizeSpeedButton. Test GTK2 to reproduce issue #161.

The first row uses TImageList (red normally, green while pressed). The second
uses TBGRAImageList with disabled buttons (blue, with Lazarus' disabled effect).
Both rows use transparent icons. The last row uses Glyph and checks the
existing bitmap rendering; ResizeSpeedButton stretches that glyph as before.

Image-list icons follow Lazarus' size and DPI rules, including in
ResizeSpeedButton. Only the standalone Glyph is stretched to fill the button.
