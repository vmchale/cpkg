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
