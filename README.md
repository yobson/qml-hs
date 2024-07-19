# WIP Qml Bindings for Haskell

## Build instructions

You need to install Qt6 (including all the qml stuff) for your system. Then you can build and install 
[dotherside](https://github.com/filcuc/dotherside).

On mac os, I change my CMake install prefix to my homebrew prefix and had to manually
edit the pkg-config file like so:
```
prefix=/opt/homebrew
libdir=/opt/homebrew/lib
includedir=/opt/homebrew/include

Name: DOtherSide
Description: C language library for creating binding for the Qt QML language
Version: 0.9.0
Libs: -L${libdir} -lDOtherSide -Wl,-rpath,/opt/homebrew/lib
Cflags: -I${includedir}/DOtherSide -I${includedir}
```

## What is implemented
- Complete raw bindings to dotherside
- The beginings of a simple interface to use those bindings

It is very very rough around the edges, and some of the code is of serious poor quality. But it somewhat works!
