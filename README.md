# cpkg

cpkg is a package manager for C.
It is configured using
[Dhall](http://github.com/dhall-lang/dhall-haskell).

By considering a package to be a *function* taking a `cpkg`-supplied
installation directory to a series of instructions, we can effectively manage
C projects with diverse build systems.

The goal is to provide something like `cabal new-build` for C.

## Installation

```
cabal new-install cpkg
```

### Shell Completions

Add the following to your `~/.bashrc` for shell completions:

```
eval "$(cpkg --bash-completion-script cpkg)"
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
 Ada                     10        2840         2135          106          599
 Assembly                 8        5259         4327          574          358
 Autoconf                 4         969          421          414          134
 Automake                 1          45           34            0           11
 Batch                    2           4            4            0            0
 C                       42       26378        23127           16         3235
 C++                      5         907          706           31          170
 C Header                27        6710         5798            0          912
 Cabal                    1         150          132            0           18
 Cabal Project            1           2            2            0            0
 Dash                     2        1287          996          135          156
 Dhall                   16         608          529            0           79
 Haskell                 19         636          508            6          122
 HTML                     1         545          540            0            5
 M4                       1          32           27            0            5
 Makefile                 6         625          434           59          132
 Markdown                 4         143          119            0           24
 Pascal                   4        1443         1218            6          219
 Perl                     1         152          108           11           33
 Plaintext               22        3999         2956            0         1043
 SAS                      1          68           54            0           14
 YAML                     2          55           52            0            3
 XML                      1         116          111            0            5
-------------------------------------------------------------------------------
 Total                  181       52973        44338         1358         7277
-------------------------------------------------------------------------------
```
