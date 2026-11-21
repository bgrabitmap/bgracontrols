# Full circles independent of the BGRABitmap arc regression

Native LCL pixel regression for SuperLED, BCRadialProgressBar and
BCFluentProgressRing. A test subclass binds each real Paint method to a TBitmap.
No production instrumentation or mouse/keyboard automation is used. A visible
form supplies the widgetset context and activates from a startup timer.

Requires Lazarus/FPC, LCL, IDEIntf, and a sibling bgrabitmap checkout. On Mac Intel:

```sh
lazbuild --ws=cocoa --cpu=x86_64 test_fullcircles.lpi
open -n -W test_fullcircles.app
cat fullcircles-result.txt
```

Use --pcp=/path/to/profile for an isolated Lazarus profile. On other platforms,
select the installed widgetset and launch the executable directly. The app exits
automatically; direct launch returns 1 on failure. macOS open does not forward
that status, so inspect fullcircles-result.txt. Results and ten BMPs are written
beside Application.ExeName; that directory must be writable. OS permission
prompts can still need manual approval.

Expected: 37 checks, 0 failures. Cases: round SuperLED inactive / active flat /
active shaded, both progress controls at 0 / 50 / 100%, and the fluent ring
disabled at 100%. Pixel checks cover visible filled LED centers, full radial
backgrounds, gray and blue ring segments, clear outside corners and hollow ring
centers. Broad area thresholds avoid dependence on exact antialiasing pixels.
Animations and interactive designer behavior are outside this test.

With unmodified BGRAControls 3422872 and BGRABitmap 11.7 b2e02a9 (before #388),
16 assertions fail. Replacing fixed complete shapes by circle/ellipse and using
circle explicitly at maximum progress makes all 37 pass, both with b2e02a9 and
with BGRABitmap's full-turn fix 4d643ad. Genuine partial/animated arcs remain arcs.
