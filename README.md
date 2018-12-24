# cpkg

[![Build Status](https://travis-ci.org/vmchale/cpkg.svg?branch=master)](https://travis-ci.org/vmchale/cpkg)
[![Windows build status](https://ci.appveyor.com/api/projects/status/github/vmchale/cpkg?svg=true)](https://ci.appveyor.com/project/vmchale/cpkg)

cpkg is a package manager for C.
It is configured using
[Dhall](http://github.com/dhall-lang/dhall-haskell).

By considering a package to be a *function* taking a `cpkg`-supplied
installation directory to a series of instructions, we can effectively package
C projects with diverse build systems and handle dependencies between them.

The goal is to eventually provide something like `cabal new-install` for C, but
with better support for cross-compilation.

- [Installation](#installation)
  - [Shell Completions](#shell-completions)
- [Example](#example)
  - [Configuration](#configuration)
  - [Dhall Prelude](#dhall-prelude)
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
          Optional/fold Text cfg.targetTriple (List Text) (λ(tgt : Text) → ["CC=${tgt}-gcc"]) ([] : List Text)
        in

        let ldflags =
          (prelude.mkLDFlags cfg.linkDirs).value
        in

        let cflags =
          (prelude.mkCFlags cfg.includeDirs).value
        in

        [ prelude.call (prelude.defaultCall ⫽ { program = prelude.makeExe cfg.buildOS
                                              , arguments = cc # [ printLuaOS cfg.buildOS, "MYLDFLAGS=${ldflags}", "MYCFLAGS=${cflags}" ]
                                              })
        ]
    in

    let luaInstall =
      λ(cfg : types.BuildVars) →
        [ prelude.call (prelude.defaultCall ⫽ { program = prelude.makeExe cfg.buildOS
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
 Autoconf                 7         195          154           30           11
 Bash                     2          28           28            0            0
 Batch                    1          22           17            0            5
 C                       77       41671        36693            0         4978
 C Header                26        5546         5007            0          539
 Cabal                    1         124          114            0           10
 Cabal Project            1           2            2            0            0
 Dash                     1          17           15            0            2
 Dhall                    3        2601         2305            3          293
 Haskell                 25        1337         1081           21          235
 Markdown                 7         541          449            0           92
 Perl                     1          74           57            0           17
 Plaintext                2         481          449            0           32
 Python                   3         112           79            7           26
 Sed                      2          16           16            0            0
 YAML                     4         154          139            0           15
 XML                      6         551          491            0           60
-------------------------------------------------------------------------------
 Total                  169       53472        47096           61         6315
-------------------------------------------------------------------------------
```
