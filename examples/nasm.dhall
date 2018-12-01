let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let nasm =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "nasm"
      , pkgVersion = v
      , pkgUrl = "http://www.nasm.us/pub/nasm/releasebuilds/${prelude.showVersion v}/nasm-${prelude.showVersion v}.tar.xz"
      , pkgSubdir = "nasm-${prelude.showVersion v}"
      }
in

nasm [2,14]
