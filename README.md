# cpkg

[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/cpkg/badge)](https://matrix.hackage.haskell.org/package/cpkg)
[![Hackage](https://img.shields.io/hackage/v/cpkg.svg)](http://hackage.haskell.org/package/cpkg)
[![Dependencies of latest version on Hackage](https://img.shields.io/hackage-deps/v/cpkg.svg)](https://hackage.haskell.org/package/cpkg)

`cpkg` is a build tool for C with a particular emphasis on cross compilation.
It is configured using
[Dhall](http://github.com/dhall-lang/dhall-haskell).

By considering a package to be a *function* taking a `cpkg`-supplied
installation directory to a series of steps, we can effectively package
C projects with diverse build systems and handle dependencies between them.

This tool provides reasonably good support for cross-compilation of C projects
and packages. It is not a full-fledged package manager.

- [Installation](#installation)
  - [Shell Completions](#shell-completions)
  - [Packages](#packages)
- [Example](#example)
  - [Configuration](#configuration)
  - [Dhall Prelude](#dhall-prelude)
  - [Cabal Integration](#cabal-integration)
- [Known Deficiencies](#known-deficiences)
  - [Security](#security)
  - [Performance](#performance)
  - [Dependency Solver](#dependency-solver)
  - [Garbage Collection](#garbage-collection)
- [Contents](#contents)

## Installation

```
cabal new-install cpkg
```

### Shell Completions

Add the following to your `~/.bashrc` for shell completions:

```
eval "$(cpkg --bash-completion-script cpkg)"
```

### Packages

To view available packages, use `cpkg list`

```
vanessa@thinkpad ~/programming/haskell/done/cpkg 🌸 cpkg list

autoconf
    url: https://ftp.gnu.org/gnu/autoconf/autoconf-2.69.tar.xz
    version: 2.69
    build dependencies: m4


automake
    url: https://ftp.gnu.org/gnu/automake/automake-1.16.1.tar.xz
    version: 1.16.1
    build dependencies: autoconf


at-spi2-atk
    url: http://ftp.gnome.org/pub/gnome/sources/at-spi2-atk/2.30/at-spi2-atk-2.30.0.tar.xz
    version: 2.30.0
    dependencies: at-spi2-core, atk, libxml2
    build dependencies: meson, ninja


at-spi2-core
    url: http://ftp.gnome.org/pub/gnome/sources/at-spi2-core/2.30/at-spi2-core-2.30.0.tar.xz
    version: 2.30.0
    dependencies: libXtst, glib
    build dependencies: meson, ninja


atk
    url: https://ftp.gnome.org/pub/gnome/sources/atk/2.30/atk-2.30.0.tar.xz
    version: 2.30.0
    build dependencies: gobject-introspection
⋮
```


## Example

To install `tar`:

```
cpkg install tar
```

To install `emacs`:

```
cpkg install emacs
```

### Configuration

Here is the configuration for Lua:

```dhall
let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let lua =
  λ(v : List Natural) →
    let printLuaOS =
      λ(os : types.OS) →
        merge
          { FreeBSD   = λ(_ : {}) → "freebsd"
          , OpenBSD   = λ(_ : {}) → "bsd"
          , NetBSD    = λ(_ : {}) → "bsd"
          , Solaris   = λ(_ : {}) → "solaris"
          , Dragonfly = λ(_ : {}) → "bsd"
          , Linux     = λ(_ : {}) → "linux"
          , Darwin    = λ(_ : {}) → "macosx"
          , Windows   = λ(_ : {}) → "mingw"
          , Redox     = λ(_ : {}) → "generic"
          , Haiku     = λ(_ : {}) → "generic"
          , IOS       = λ(_ : {}) → "generic"
          , AIX       = λ(_ : {}) → "generic"
          , Hurd      = λ(_ : {}) → "generic"
          , Android   = λ(_ : {}) → "generic"
          , NoOs      = λ(_ : {}) → "c89"
          }
          os
    in

    let luaBuild =
      λ(cfg : types.BuildVars) →
        let cc = prelude.mkCCArg cfg
        in

        let ldflags =
          (prelude.mkLDFlags cfg.linkDirs).value
        in

        let cflags =
          (prelude.mkCFlags cfg.includeDirs).value
        in

        let os =
          prelude.osCfg cfg
        in

        [ prelude.call (prelude.defaultCall ⫽ { program = "make"
                                              , arguments = cc # [ printLuaOS os, "MYLDFLAGS=${ldflags}", "MYCFLAGS=${cflags}", "MYLIBS=-lncurses" ]
                                              })
        ]
    in

    let luaInstall =
      λ(cfg : types.BuildVars) →
        [ prelude.call (prelude.defaultCall ⫽ { program = "make"
                                              , arguments = [ "install", "INSTALL_TOP=${cfg.installDir}" ]
                                              }) ]
          # prelude.symlinkBinaries [ "bin/lua", "bin/luac" ]
    in

    prelude.simplePackage { name = "lua", version = v } ⫽
      { pkgUrl = "http://www.lua.org/ftp/lua-${prelude.showVersion v}.tar.gz"
      , configureCommand = prelude.doNothing
      , buildCommand = luaBuild
      , installCommand = luaInstall
      , pkgDeps = [ prelude.unbounded "readline"
                  , prelude.unbounded "ncurses"
                  ]
      }
in

lua [5,3,5]
```

### Cabal Integration

After running

```
cpkg install libX11 --target=arm-linux-gnueabihf
cpkg install libXext --target=arm-linux-gnueabihf
cpkg install libXrandr --target=arm-linux-gnueabihf
cpkg install libXinerama --target=arm-linux-gnueabihf
cpkg install libXScrnSaver --target=arm-linux-gnueabihf
```

You can dump flags to be passed to cabal with

```
cpkg dump-cabal libX11 libXext libXrandr libXinerama libXScrnSaver --target=arm-linux-gnueabihf
```

which will produce something like

```
--extra-lib-dirs=/home/vanessa/.cpkg/arm-linux-gnueabihf/libX11-1.6.7-820c8166b4caadb/lib --extra-lib-dirs=/home/vanessa/.cpkg/arm-linux-gnueabihf/libXext-1.3.3-1bad0a89c6794a53/lib --extra-lib-dirs=/home/vanessa/.cpkg/arm-linux-gnueabihf/libXrandr-1.5.1-f58f951a622e5c2/lib --extra-lib-dirs=/home/vanessa/.cpkg/arm-linux-gnueabihf/libXinerama-1.1.4-516496f7e04d34be/lib --extra-lib-dirs=/home/vanessa/.cpkg/arm-linux-gnueabihf/libXScrnSaver-1.2.3-60f6993b79a87725/lib
```

This could be used, for example, to cross-compile `xmonad`, viz.

```
cabal new-install xmonad --with-ghc arm-linux-gnueabihf-ghc --with-ghc-pkg arm-linux-gnueabihf-ghc-pkg $(cpkg dump-cabal libX11 libXext libXrandr libXinerama libXScrnSaver --target=arm-linux-gnueabihf)
```

### Dhall Prelude

There is
a [prelude](https://github.com/vmchale/cpkg/blob/master/dhall/cpkg-prelude.dhall)
available containing functions which simplify the process of writing package
descriptions. As an example, we can install `sed`

```dhall
let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let sed =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "sed", version = v }
in

sed [4,5]
```

## Known Deficiencies

### Security

This tool is insecure.

### Performance

This tool is not performant.

### Dependency Solver

Currently, there is no dependency solver. It should be relatively easy to add
a version-based dependency solver to the code.

### Garbage Collection

Currently, there is no garbage collector à la `$ nix-collect-garbage`. This will
*not* be relatively easy to add, because it will require that the global package
index be re-implemented (ideally using a proper database).

## Contents

Lovingly provided by [polyglot](https://github.com/vmchale/polyglot):

```
-------------------------------------------------------------------------------
 Language             Files       Lines         Code     Comments       Blanks
-------------------------------------------------------------------------------
 Bash                     3          50           40            5            5
 Cabal                    1         157          143            0           14
 Cabal Project            1           4            3            0            1
 Dhall                    3        5191         4681            8          502
 Haskell                 32        1949         1602           37          310
 Markdown                 3         516          426            0           90
 YAML                     4         148          133            0           15
-------------------------------------------------------------------------------
 Total                   47        8015         7028           50          937
-------------------------------------------------------------------------------
```
