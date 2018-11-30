let types = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-types.dhall
in

let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let gmpBuild =
  λ(cfg : { cpus : Natural, buildOS : types.OS } ) →
    [ "${prelude.makeExe cfg.buildOS} -j${Natural/show cfg.cpus}" ]
in

let gmp =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "gmp"
      , pkgVersion = v
      , pkgUrl = "https://gmplib.org/download/gmp/gmp-${prelude.showVersion v}.tar.xz"
      , pkgSubdir = "gmp-${prelude.showVersion v}"
      , executableFiles = [ "configure", "mpn/m4-ccas" ]
      , buildCommand = gmpBuild
      }
in

gmp [6,1,2]
