let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let png =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "libpng"
      , pkgVersion = v
      , pkgUrl = "https://download.sourceforge.net/libpng/libpng-${prelude.showVersion v}.tar.xz"
      , pkgSubdir = "libpng-${prelude.showVersion v}"
      }
in

png [1,6,35]
