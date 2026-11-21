# TBGRAShape ellipse regression (#297)

Exercises the real `TBGRAShape.Paint` method through a test subclass that binds
its canvas to a `TBitmap`. This is an offscreen pixel regression using the native
LCL widgetset, not a mouse/keyboard interaction test. Production code is not
instrumented. A visible form supplies the native control context.

Requires Lazarus/FPC, LCL, IDEIntf, and a sibling `bgrabitmap` checkout, as in the
other regression projects. Build from this directory, for example on Mac Intel:

```sh
lazbuild --ws=cocoa --cpu=x86_64 test_ellipse.lpi
open -n -W test_ellipse.app
cat ellipse-result.txt
```

An isolated Lazarus profile can be selected with `--pcp=/path/to/profile`.
On other platforms, select the installed widgetset and launch `test_ellipse`
directly. The program exits automatically; failures return exit code 1 when
launched directly. macOS `open` does not forward that exit status, so inspect
the result file. Startup activation occurs from a timer after `Application.Run`
starts. An OS directory-permission prompt may still require manual approval.

Expected result: **22 PASS lines, `Failures: 0`**. Coverage includes a regular
polygon control, solid ellipse, fill and border gradients, constrained aspect
ratio, fill without a border, and a transparent center with a visible border.
Pixels outside the shape remain clear. The program saves one BMP per case and
`ellipse-result.txt` next to `Application.ExeName`; that directory must be writable.

Before the fix, with BGRAControls 577239e and BGRABitmap b2e02a9 (11.7), the polygon
passes but all six ellipse variants are invisible: 11 failed assertions. The
fix uses Canvas2D's explicit `ellipse` primitive instead of a full-turn circular
arc under a temporary transform. In this BGRABitmap version, unequal angles
that map to the same 16-bit angle are handled as a tiny arc; `0` and `2*Pi`
therefore collapse to two almost identical points. The explicit ellipse uses
the equal-angle full-ellipse convention and preserves the intended dimensions.
