let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let gc =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
        { pkgName = "gc"
        , pkgVersion = v
        , pkgUrl = "https://github.com/ivmai/bdwgc/releases/download/v${prelude.showVersion v}/gc-${prelude.showVersion v}.tar.gz"
        , pkgSubdir = "gc-${prelude.showVersion v}"
        }
in

gc [8,0,0]
