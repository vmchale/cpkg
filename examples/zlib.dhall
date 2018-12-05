let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let zlib =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "z"
      , pkgVersion = v
      , pkgUrl = "http://www.zlib.net/zlib-${prelude.showVersion v}.tar.xz"
      , pkgSubdir = "zlib-${prelude.showVersion v}"
      }
in

zlib [1,2,11]
