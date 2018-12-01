# cpkg

cpkg is a package manager for C.
It is configured using
[Dhall](http://github.com/dhall-lang/dhall-haskell).

The goal is to provide something like `cabal new-build` for C.

## Installation

```
cabal new-install cpkg
```

## Security

This tool is insecure.

## Example

Put the following in a file called `valgrind.dhall`:

```dhall
let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let types = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-types.dhall
in

let valgrindConfigure =
  λ(cfg : types.ConfigureVars) →
    prelude.defaultConfigure cfg # [ prelude.mkExe "auxprogs/make_or_upd_vgversion_h" ]
in

let valgrind =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "valgrind"
      , pkgVersion = v
      , pkgUrl = "http://www.valgrind.org/downloads/valgrind-${prelude.showVersion v}.tar.bz2"
      , pkgSubdir = "valgrind-${prelude.showVersion v}"
      , configureCommand = valgrindConfigure
      }
in

valgrind [3,14,0]
```

Then you can install Valgrind to `~/.cpkg/valgrind-3.14.0` with:

```
cpkg install valgrind.dhall
```

## Contents

Lovingly provided by [polyglot](https://github.com/vmchale/polyglot):

```
-------------------------------------------------------------------------------
 Language             Files       Lines         Code     Comments       Blanks
-------------------------------------------------------------------------------
 Cabal                    1         150          132            0           18
 Cabal Project            1           2            2            0            0
 Dhall                   13         501          436            0           65
 Haskell                 19         636          508            6          122
 Markdown                 4         126          106            0           20
 YAML                     2          55           52            0            3
-------------------------------------------------------------------------------
 Total                   40        1470         1236            6          228
-------------------------------------------------------------------------------
```
