let types = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-types.dhall
in

let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let gawkConfigure =
  λ(cfg : types.ConfigureVars) →
    prelude.defaultConfigure cfg
      # [ prelude.mkExe "install-sh"
        , prelude.mkExe "extension/build-aux/install-sh"
        ]
in

let gawk =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "gawk", version = v } ⫽
      { pkgName = "awk"
      , configureCommand = gawkConfigure
      }
in

gawk [4,2,1]
