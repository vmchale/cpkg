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

let unistring =
  λ(v : List Natural) →
    { pkgName = "unistring"
    , pkgVersion = v
    , pkgUrl = "https://mirrors.ocf.berkeley.edu/gnu/libunistring/libunistring-${prelude.showVersion v}.tar.xz"
    , pkgSubdir = "libunistring-${prelude.showVersion v}"
    , configureCommand = configure
    , executableFiles = [ "configure" ]
    , buildCommand = build
    , installCommand = [ "make install" ]
    }
in

unistring [0,9,10]
