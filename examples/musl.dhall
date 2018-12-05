let types = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-types.dhall
in

let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let muslConfigure =
  λ(cfg : types.ConfigureVars) →
    prelude.mkExes [ "tools/install.sh" ] # prelude.defaultConfigure cfg
in

let musl =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "musl"
      , pkgVersion = v
      , pkgUrl = "https://www.musl-libc.org/releases/musl-${prelude.showVersion v}.tar.gz"
      , pkgSubdir = "musl-${prelude.showVersion v}"
      , configureCommand = muslConfigure
      }
in

musl [1,1,20]
