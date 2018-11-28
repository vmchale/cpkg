let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let gmpBuild =
  λ(cfg : { cpus : Natural, buildOS : OS } ) →
    let
      cpuString  = Natural/show cfg.cpus
      makeString = makeExe cfg.buildOS
    in
      [ "${makeString} -j${cpuString}", "${makeString} -j${cpuString} check" ]
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
