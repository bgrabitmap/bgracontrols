# Theme button floating-point exception scope

Requires Lazarus/FPC, LCL, IDEIntf and a sibling `bgrabitmap` checkout. On Mac Intel:

```sh
lazbuild --ws=cocoa --cpu=x86_64 test_mask.lpi
open -n -W test_mask.app
```

Use --pcp=/path/to/profile for an isolated Lazarus profile. Other platforms can
choose their installed widgetset and launch the executable directly. Results
are saved next to Application.ExeName in mask-result.txt; this directory must
be writable. Direct launch returns 1 on failure; inspect the result when using
macOS open. If macOS blocks initial loading from Documents, use an autonomous
copy of the bundle/executable in a local cache, as explained in the checklist
regression README.

Four cases run the real TBGRAThemeButton.Paint through a test subclass and a
probe theme: normal return, a deliberately raised non-FPU exception, masked
invalid math (sqrt of a negative value), and invalid math followed by an
exception. The caller enables invalid-operation exceptions; the theme must see
all floating exceptions masked during Paint and the caller must get its original
mask back afterwards. Expected non-FPU exceptions must still propagate.

Expected: 14 checks, 0 failures. The original implementation at 4db2841 fails
four mask-restoration checks. The test does not emulate a peripheral DLL or
promise preservation of sticky exception-status flags; the fix clears ignored
rendering exceptions before restoring the caller's mask. No production
instrumentation is used and the app exits automatically.
