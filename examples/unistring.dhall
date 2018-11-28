let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let configure =
  λ(cfg : { installDir : Text, includeDirs : List Text}) →
    [ "./configure --prefix=" ++ cfg.installDir ]
in

let build =
  λ(cpus : Natural) →
    [ "make -j" ++ Natural/show cpus ]
in

let makeGnuPackage =
  λ(pkg : { name : Text, version : List Natural}) →
    { pkgName = pkg.name
    , pkgVersion = pkg.version
    , pkgUrl = "https://mirrors.ocf.berkeley.edu/gnu/lib${pkg.name}/lib${pkg.name}-${prelude.showVersion pkg.version}.tar.xz"
    , pkgSubdir = "lib${pkg.name}-${prelude.showVersion pkg.version}"
    }

let unistring =
  λ(v : List Natural) →
    makeGnuPackage { name = "unistring", version = v } ⫽
      { configureCommand = configure
      , executableFiles = [ "configure" ]
      , buildCommand = build
      , installCommand = [ "make install" ]
      }
in

unistring [0,9,10]
