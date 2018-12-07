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

Here is the configuration for Valgrind:

```dhall
let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let types = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-types.dhall
in

let valgrind =
  let valgrindConfigure =
    λ(cfg : types.ConfigureVars) →
      prelude.defaultConfigure cfg # [ prelude.mkExe "auxprogs/make_or_upd_vgversion_h" ]
  in

  λ(v : List Natural) →
    prelude.simplePackage { name = "valgrind", version = v } ⫽
      { pkgUrl = "http://www.valgrind.org/downloads/valgrind-${prelude.showVersion v}.tar.bz2"
      , configureCommand = valgrindConfigure
      , installCommand = prelude.installWithBinaries [ "bin/valgrind" ]
      }
in

valgrind [3,14,0]
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

## Security

This tool is insecure.

## Contents

Lovingly provided by [polyglot](https://github.com/vmchale/polyglot):

```
-------------------------------------------------------------------------------
 Language             Files       Lines         Code     Comments       Blanks
-------------------------------------------------------------------------------
 Assembly                50       76829        72529            0         4300
 Autoconf                52      109335        99539         4183         5613
 Automake                41       14136         9864         1800         2472
 Bash                     5         518          325           84          109
 C                     1155      381345       325672           68        55605
 C++                      3        1372         1102            6          264
 C Header               451      105937        96433            0         9504
 Cabal                    1         118          108            0           10
 Cabal Project            1           2            2            0            0
 CSS                      3         736          657            0           79
 Dash                   129       14252         8836         2742         2674
 Dhall                    4        1001          869            0          132
 Emacs Lisp               1          12           11            0            1
 Haskell                 22        1099          876           24          199
 HTML                    16       99738        95725            0         4013
 M4                       1        1110          958            0          152
 Makefile                 6        2104         1363          478          263
 Markdown                 9         965          736            0          229
 Perl                     6        1549         1163          196          190
 Plaintext               23        2974         2783            0          191
 Scheme                  16        1344          724          414          206
 Sed                      2          16           16            0            0
 TeX                      1       11727         8853         2063          811
 Yacc                     1        2428         2151            0          277
 YAML                     4         155          140            0           15
-------------------------------------------------------------------------------
 Total                 2003      830802       731435        12058        87309
-------------------------------------------------------------------------------
```
