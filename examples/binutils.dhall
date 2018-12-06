let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let types = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-types.dhall
in

let binutils =
  let binutilsConfigure =
    λ(cfg : types.ConfigureVars) →
      prelude.defaultConfigure cfg # [ prelude.mkExe "mkinstalldirs" ]
  in

  λ(v : List Natural) →
    prelude.makeGnuExe { name = "binutils", version = v } ⫽
      { pkgUrl = "https://mirrors.ocf.berkeley.edu/gnu/binutils/binutils-${prelude.showVersion v}.tar.xz"
      , configureCommand = binutilsConfigure
      }
in

binutils [2,31]
