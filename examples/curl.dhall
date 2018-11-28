-- https://curl.haxx.se/download/curl-7.62.0.tar.xz
-- TODO: ssl, ssh, git2?

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

let curl =
  λ(v : List Natural) →
    { pkgName = "curl"
    , pkgVersion = v
    , pkgUrl = "https://curl.haxx.se/download/curl-${prelude.showVersion v}.tar.xz"
    , pkgSubdir = "curl-${prelude.showVersion v}"
    , configureCommand = configure
    , executableFiles = [ "configure" ]
    , buildCommand = build
    , installCommand = [ "make install" ]
    }
in

curl [7,62,0]
