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
