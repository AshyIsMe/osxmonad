# osxmonad

This is a library which allows XMonad to manage Mac OS X windows.

## Status

* Only attached hook is `layoutHook`
* No Xinerama (multiple monitor support)
* No borders
* No `focusFollowsMouse`

## Installation

We need XMonad's compilation step to include the `-framework Cocoa`
flag to GHC. This repository includes a `xmonad.patch` (1 line diff)
that you must apply to the XMonad source:

    git clone git://github.com/xmonad/osxmonad.git
    darcs get http://code.haskell.org/xmonad
    cd xmonad
    darcs apply ../osxmonad/xmonad.patch
    LIBRARY_PATH=/opt/X11/lib:$LIBRARY_PATH cabal install "X11 >=1.5 && <1.7"
    cabal configure
    cabal install
    cd ../osxmonad
    cabal configure
    cabal install

**Note**: Mountain Lion users will have to download and install
[XQuartz](http://xquartz.macosforge.org/landing/).

## Configuration

Create `~/.xmonad/xmonad.hs`:

    import XMonad
    import OSXMonad.Core

    main = osxmonad defaultConfig {
             modMask = mod1Mask .|. mod4Mask,
             keys = osxKeys
           }

Now we can run `xmonad` to have our windows managed.

## Videos

[
![](http://b.vimeocdn.com/ts/369/421/369421287_640.jpg)
](https://vimeo.com/53482928)

[
![](http://b.vimeocdn.com/ts/351/155/351155192_640.jpg)
](https://vimeo.com/50960925)

## License

BSD-3
