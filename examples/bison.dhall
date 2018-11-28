let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let bison =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "bison"
      , pkgVersion = v
      , pkgUrl = "https://ftp.gnu.org/gnu/bison/bison-${prelude.showVersion v}.tar.xz"
      , pkgSubdir = "bison-${prelude.showVersion v}"
      , executableFiles = [ "configure", "build-aux/move-if-change" ]
      }
in

bison [3,2,2]
