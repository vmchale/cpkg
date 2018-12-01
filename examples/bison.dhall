let prelude = ../dhall/cpkg-prelude.dhall -- https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let types = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-types.dhall
in

let bisonConfigure =
  λ(cfg : types.ConfigureVars) →
    [ prelude.mkExe "configure"
    , prelude.call { program = "./configure"
                   , arguments = [ "--prefix=${cfg.installDir}"
                                 , mkTarget cfg.targetTriple
                                 ]
                   , environment = [] : Optional (List types.EnvVar)
                   , procDir = [] : Optional Text
                   }
    , prelude.mkExe "build-aux/move-if-change"
    ]
in
let bison =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "bison"
      , pkgVersion = v
      , pkgUrl = "https://ftp.gnu.org/gnu/bison/bison-${prelude.showVersion v}.tar.xz"
      , pkgSubdir = "bison-${prelude.showVersion v}"
      }
in

bison [3,2,2]
