let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let harfbuzz =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "harfbuzz"
      , pkgVersion = v
      , pkgUrl = "https://www.freedesktop.org/software/harfbuzz/release/harfbuzz-${prelude.showVersion v}.tar.bz2"
      , pkgSubdir = "harfbuzz-${prelude.showVersion v}"
      }
in

harfbuzz [2,2,0]
