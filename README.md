# cpkg

cpkg is a build tool and Haskell library for building C projects with
dependencies. It is configured using
[Dhall](http://github.com/dhall-lang/dhall-haskell).

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

let valgrind =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "valgrind"
      , pkgVersion = v
      , pkgUrl = "http://www.valgrind.org/downloads/valgrind-${prelude.showVersion v}.tar.bz2"
      , pkgSubdir = "valgrind-${prelude.showVersion v}"
      , executableFiles = [ "configure", "auxprogs/make_or_upd_vgversion_h" ]
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
 Cabal                    1         149          131            0           18
 Cabal Project            1           2            2            0            0
 Dhall                   10         337          292            0           45
 Haskell                 19         602          479            6          117
 Markdown                 4          82           72            0           10
 YAML                     2          55           52            0            3
-------------------------------------------------------------------------------
 Total                   37        1227         1028            6          193
-------------------------------------------------------------------------------
```
