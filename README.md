# cpkg

[![Build Status](https://travis-ci.org/vmchale/cpkg.svg?branch=master)](https://travis-ci.org/vmchale/cpkg)
[![Windows build status](https://ci.appveyor.com/api/projects/status/github/vmchale/cpkg?svg=true)](https://ci.appveyor.com/project/vmchale/cpkg)
[![Hackage](https://img.shields.io/hackage/v/cpkg.svg)](http://hackage.haskell.org/package/cpkg)
[![Dependencies of latest version on Hackage](https://img.shields.io/hackage-deps/v/cpkg.svg)](https://hackage.haskell.org/package/cpkg)

cpkg is a build tool for C.
It is configured using
[Dhall](http://github.com/dhall-lang/dhall-haskell).

By considering a package to be a *function* taking a `cpkg`-supplied
installation directory to a series of instructions, we can effectively package
C projects with diverse build systems and handle dependencies between them.

This tool provides reasonably good support for cross-compilation of C projects
and packages.

- [Installation](#installation)
  - [Shell Completions](#shell-completions)
- [Example](#example)
  - [Configuration](#configuration)
  - [Dhall Prelude](#dhall-prelude)
  - [Cabal Integration](#cabal-integration)
- [Security](#security)
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

## Example

To install `tar`:

```
cpkg install tar
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
        let cc =
          Optional/fold types.TargetTriple cfg.targetTriple (List Text)
            (λ(tgt : types.TargetTriple) → ["CC=${prelude.printTargetTriple tgt}-gcc"])
              ([] : List Text)
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
                                              , arguments = cc # [ printLuaOS os, "MYLDFLAGS=${ldflags}", "MYCFLAGS=${cflags}" ]
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
      , pkgDeps = [ prelude.unbounded "readline" ]
      }
in

lua [5,3,5]
```

### Cabal Integration

After running

```
cpkg install libXext --target=arm-linux-gnueabihf
cpkg install libXrandr --target=arm-linux-gnueabihf
cpkg install libXinerama --target=arm-linux-gnueabihf
cpkg install libXScrnSaver --target=arm-linux-gnueabihf
```

You can dump flags to be passed to cabal with

```
cpkg dump-cabal libXext libXrandr libXinerama libXScrnSaver --target=arm-linux-gnueabihf
```

which will produce something like

```
--extra-lib-dirs=/home/vanessa/.cpkg/arm-linux-gnueabihf/libXext-1.3.3-63648c4324869741/lib --extra-lib-dirs=/home/vanessa/.cpkg/arm-linux-gnueabihf/libXrandr-1.5.1-72c136ebb1cdbee4/lib --extra-lib-dirs=/home/vanessa/.cpkg/arm-linux-gnueabihf/libXinerama-1.1.4-49761ceb8fb134d8/lib --extra-lib-dirs=/home/vanessa/.cpkg/arm-linux-gnueabihf/libXScrnSaver-1.2.3-11409d560d940784/lib
```

This could be used, for example, to cross-compile `X11`, viz.

```
cabal new-install X11 --with-ghc arm-linux-gnueabihf-ghc --with-ghc-pkg arm-linux-gnueabihf-ghc-pkg $(cpkg dump-cabal libXext libXrandr libXinerama libXScrnSaver --target=arm-linux-gnueabihf)
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

## Contents

Lovingly provided by [polyglot](https://github.com/vmchale/polyglot):

```
-------------------------------------------------------------------------------
 Language             Files       Lines         Code     Comments       Blanks
-------------------------------------------------------------------------------
 Bash                     3          35           34            0            1
 Cabal                    1         155          141            0           14
 Cabal Project            1           2            2            0            0
 Dhall                    3        3002         2673            2          327
 Haskell                 31        1665         1362           22          281
 Markdown                 5         363          314            0           49
 YAML                     4         155          140            0           15
-------------------------------------------------------------------------------
 Total                   48        5377         4666           24          687
-------------------------------------------------------------------------------
```
