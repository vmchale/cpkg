let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let sed =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "sed"
      , pkgVersion = v
      , pkgUrl = "https://ftp.gnu.org/gnu/sed/sed-${prelude.showVersion v}.tar.xz"
      , pkgSubdir = "sed-${prelude.showVersion v}"
      }
in

sed [4,5]
