let configure =
  λ(cfg : { installDir : Text, includeDirs : List Text}) →
    [ "./configure --prefix=" ++ cfg.installDir ]
in

let build =
  λ(cpus : Natural) →
    [ "make -j" ++ Natural/show cpus ]
in

{ pkgName = "unistring"
, pkgVersion = [0,9,10]
, pkgUrl = "https://mirrors.ocf.berkeley.edu/gnu/libunistring/libunistring-0.9.10.tar.xz"
, pkgSubdir = "libunistring-0.9.10"
, configureCommand = configure
, executableFiles = [ "configure" ]
, buildCommand = build
, installCommand = [ "make install" ]
}
