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
    prelude.defaultPackage ⫽
      { pkgName = "awk"
      , pkgVersion = v
      , pkgUrl = "https://ftp.gnu.org/gnu/gawk/gawk-${prelude.showVersion v}.tar.xz"
      , pkgSubdir = "gawk-${prelude.showVersion v}"
      , configureCommand = gawkConfigure
      }
in

gawk [4,2,1]
