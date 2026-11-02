BCComboBox same-form dropdown dismissal
======================================

Build test_dismissal.lpi with Lazarus, with a sibling bgrabitmap source checkout.
Run the executable; results are written to dismissal-result.txt.
On Linux, run under an interactive display or xvfb-run.

The test checks that a stale list OnExit does not close a focused dropdown,
and that a real focus transfer still closes it. On Cocoa it also delivers
application mouse-down notifications to verify inside/own-button exclusions,
outside-click dismissal without a focus change, and handler removal on destroy.
These notifications exercise routing; they do not replace native manual testing.

Manual Cocoa checks (validated on Intel macOS Sonoma 14.8.4,
Lazarus 3.8 / FPC 3.2.2 during development):
- Open, close via the combo button, and reopen repeatedly.
- Click another button, the form background, or another combo: panel closes.
- Click/select/scroll inside the list: normal item handling continues to work.
- Check Enter, Escape, and switching to another application.
- Repeat with DropDownOnSameForm=False to check the separate popup is unchanged.

Keyboard selection highlight (#290)
-----------------------------------
Build test_highlight.lpi with the same sibling BGRABitmap checkout. The test
writes highlight-result.txt next to Application.ExeName and returns a nonzero
exit status on failure. Run in an interactive desktop session; on Cocoa allow
any macOS directory-access prompt and keep the test application foreground.
Running a copy from a temporary writable directory avoids Documents prompts.

It exercises the keyboard and mouse event handlers and explicitly applies the
selection update normally supplied by the native widgetset. It verifies default
background colors and the custom draw odSelected/odChecked contract, including
mouse takeover within the previously hovered row, in both popup modes. It does
not simulate OS key delivery or replace native interaction testing.

For native manual testing, run test_highlight -manual, optionally adding
-separate-popup. Open the list, hover a row, then navigate with Up/Down,
Home/End and Page Up/Page Down without moving the mouse. The keyboard-selected
row should be highlighted and the old hovered row should lose its highlight.
Move the mouse again (including within the old hovered row): mouse highlighting
should resume. Check Enter/Escape and repeat with both popup modes.
