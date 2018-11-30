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
