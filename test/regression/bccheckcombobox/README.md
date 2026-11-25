# Checked item lifetime and legacy API

Requires Lazarus/FPC, LCL, IDEIntf and a sibling `bgrabitmap` checkout. On Mac Intel:

```sh
lazbuild --ws=cocoa --cpu=x86_64 test_checks.lpi
open -n -W test_checks.app
```

An isolated profile can be selected with `--pcp=/path/to/profile`. On other
platforms choose the installed widgetset and launch the executable directly.
The test exits automatically and writes `checks-result.txt` beside
`Application.ExeName`; this directory must be writable. Direct execution returns
1 on failure; macOS `open` does not forward that status, so inspect the result.
If macOS blocks a bundle while loading it from Documents, copy it to a local
cache directory with a real executable in Contents/MacOS (resolve Lazarus's
executable symlink), and read the results from that directory.

Coverage includes insertion/deletion/clearing, client-owned Objects, checking
through both APIs, CheckAll/UnCheckAll, grayed states, Assign/self-Assign,
protected CreateForm/FreeForm compatibility, popup teardown/recreation and
safe destruction of an open popup after native layout has settled, plus pixel
contrast checks on light/dark normal and highlighted rows. Tests run before and after explicitly
requiring the native handle. The program assigns `Combo.ListBox` to a
`TCheckListBox`, so the old public API is also checked at compile time.

Space tests invoke the real LCL KeyDown method and verify one toggle and one
OnChange per key; they do not synthesize OS keyboard input. Programmatic check
setters must not send user-change notifications. Per-item disabling is checked
only when supported by the widgetset; Cocoa in Lazarus 3.8 ignores that native
property, so the two relevant assertions are explicitly reported as SKIP.

Row-click tests invoke the mouse handlers, both without a native check change
and after a simulated native checkbox notification. They verify one toggle and
one notification, repeated clicks, right clicks and releases outside the row.
These checks do not synthesize OS mouse input; native clicks and Cocoa timer
hover tracking must also be validated interactively.

Active-row tests additionally invoke the real LCL KeyDown method and the
protected pointer-tracking routine used by the Cocoa timer. They cover
mouse/keyboard priority, unchanged pointer samples, Home/End/Page navigation,
and Space/OnChange counts. Pointer positions are supplied to the routine;
these tests do not move the OS pointer. The highlighted row is always the
Space target, including when the pointer leaves the list.

On Cocoa 3.8: expected 87 checks, zero failures and two SKIP lines. The LCL may
replace its TStrings implementation while recreating native handles: the test
checks preserved item content, object references and states, not collection
pointer identity. No production instrumentation is used.

Cocoa 3.8 has a separate LCL race: its global asynchronous scroll sizer can keep
an unowned pointer if a scroll control is freed immediately after showing it,
before queued layout runs. The lifecycle test processes native layout before
freeing the popup and checks subsequent event processing. This PR does not patch
that LCL helper or claim immediate destruction with pending native layout is safe.
