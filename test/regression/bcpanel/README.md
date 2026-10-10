# BCPanel mouse wheel regression

Checks that unhandled wheel events over BCPanel scroll the enclosing form,
including a FlowPanel layout and nested BCPanels (issue #272). A standard
TPanel provides the comparison. Child and parent handlers that mark the event
handled must prevent scrolling; the child handler must run only once.

Requires Linux/X11, Qt5 and `libXtst.so.6`. Run on an isolated display so synthetic
input does not interfere with the desktop:

```
lazbuild --ws=qt5 test_mousewheel.lpi
QT_QPA_PLATFORM=xcb xvfb-run -a ./test_mousewheel
```

The test briefly shows forms, prints `PASS` on success and returns a nonzero
status on failure. Other platforms and widgetsets print `SKIP`.
