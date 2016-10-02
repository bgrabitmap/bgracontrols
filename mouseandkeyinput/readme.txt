MouseAndKeyInput package is a tool for cross-platform manipulation with mouse and key input. You can move mouse cursor to specified location, send clicks and do key presses. It is suitable for GUI testing or program control demonstration.

Author
Tom Gregorovic

License
GPL

Change Log
* Version 0.1 

Restrictions
* it is not recommended calling mouse and key input directly from events like OnClick, use Application.QueueAsyncCall instead
* do not forget to set back mouse button and key state after Down method with Up method 

 Carbon
 * pressing alpha chars is not supported 

 Gtk1/2
 * needs Xtst library
 * ALT key pressing is not supported 