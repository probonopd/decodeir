decodeir
========

decodeir is an executable that converts Pronto Hex infrared codes into protocol, device, subdevice, command = OBC information. 

It is based on  DecodeIR v2.45 sources from http://www.hifi-remote.com/forums/viewtopic.php?t=14219 by John S. Fine, revision by Graham Dixon, with all Java and Windows code removed and converted from a library to a standalone executable by me.

Usage:
```
./decodeir 0000 006C 0022 0002 015B 00AD 0016 0016 0016 0016 0016 0016 0016 0016 0016 0016 0016 0016 0016 0016 0016 0016 0016 0041 0016 0041 0016 0041 0016 0041 0016 0041 0016 0041 0016 0016 0016 0041 0016 0041 0016 0016 0016 0016 0016 0016 0016 0016 0016 0016 0016 0016 0016 0016 0016 0016 0016 0041 0016 0041 0016 0041 0016 0041 0016 0041 0016 0041 0016 0041 0016 0622 015B 0057 0016 0E6C
protocol=NEC1 device=0 subdevice=191 obc=1 hex0=127 hex1=-1 hex2=-1 hex3=-1 misc= error=
```

TODO: 

    * Include raw to Pronto hex conversion routines from ExchangeIR.cpp by Graham Dixon from the source at http://www.hifi-remote.com/forums/dload.php?action=file&file_id=8460 - if anyone knows how to do this, please drop me a line (I can't get it to compile on Linux)
    * Make it compile on Mac OS X
