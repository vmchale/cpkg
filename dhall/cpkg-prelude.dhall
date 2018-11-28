let concatMapSep = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Text/concatMapSep
in

let showVersion =
  λ(x : List Natural) → concatMapSep "." Natural Natural/show x
in

let configureGnu =
  λ(cfg : { installDir : Text, includeDirs : List Text}) →
    [ "./configure --prefix=" ++ cfg.installDir ]
in

let buildGnu =
  λ(cpus : Natural) →
    [ "make -j" ++ Natural/show cpus ]
in

let makeGnuPackage =
  λ(pkg : { name : Text, version : List Natural}) →
    { pkgName = pkg.name
    , pkgVersion = pkg.version
    , pkgUrl = "https://mirrors.ocf.berkeley.edu/gnu/lib${pkg.name}/lib${pkg.name}-${showVersion pkg.version}.tar.xz"
    , pkgSubdir = "lib${pkg.name}-${showVersion pkg.version}"
    , configureCommand = configure
    , executableFiles = [ "configure" ]
    , buildCommand = build
    , installCommand = [ "make install" ]
    }

{ showVersion = showVersion
, makeGnuPackage = makeGnuPackage
}
