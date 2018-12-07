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
 Awk                      3         267          129          117           21
 Autoconf                25       22120        18264         1578         2278
 Automake                20        2444         1608          447          389
 C                      302      292264       253792            1        38471
 C Header               142       18342        15459            0         2883
 Cabal                    1         118          108            0           10
 Cabal Project            1           2            2            0            0
 Dash                    36        2813         1635          755          423
 Dhall                    4         990          861            0          129
 Haskell                 22        1099          876           24          199
 M4                       1        2085         1803            0          282
 Makefile                 1        1299          949          154          196
 Markdown                 4         204          172            0           32
 Perl                     1          27           24            0            3
 PHP                      1          27           26            0            1
 Plaintext               59        9974         7873            0         2101
 Python                   1          30           17           10            3
 Scheme                 105        9505         6201         2235         1069
 Sed                      2          16           16            0            0
 TeX                      1        8638         6354         1592          692
 YAML                     4         155          140            0           15
-------------------------------------------------------------------------------
 Total                  736      372419       316309         6913        49197
-------------------------------------------------------------------------------
```
