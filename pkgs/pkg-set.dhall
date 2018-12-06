let types = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-types.dhall
in

let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

{- npth: https://www.gnupg.org/software/npth/index.html -}
let npth =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "npth", version = v } ⫽
      { pkgUrl = "https://gnupg.org/ftp/gcrypt/npth/npth-${prelude.showVersion v}.tar.bz2"
      }
in

{- gnupg: https://www.gnupg.org/ -}
let gnupg =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "gnupg", version = v } ⫽
      { pkgUrl = "https://gnupg.org/ftp/gcrypt/gnupg/gnupg-${prelude.showVersion v}.tar.bz2"
      , pkgDeps = [ prelude.lowerBound { name = "npth", lower = [1,2] }
                  , prelude.lowerBound { name = "libgpg-error", lower = [1,24] }
                  , prelude.lowerBound { name = "libgcrypt", lower = [1,7,0] }
                  , prelude.lowerBound { name = "libassuan", lower = [2,5,0] }
                  , prelude.lowerBound { name = "libksba", lower = [1,3,4] }
                  ]
      }
in

{- musl: https://www.musl-libc.org/ -}
let musl =
  let muslConfigure =
    λ(cfg : types.ConfigureVars) →
      prelude.mkExes [ "tools/install.sh" ] # prelude.defaultConfigure cfg
  in

  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "musl"
      , pkgVersion = v
      , pkgUrl = "https://www.musl-libc.org/releases/musl-${prelude.showVersion v}.tar.gz"
      , pkgSubdir = "musl-${prelude.showVersion v}"
      , configureCommand = muslConfigure
      }
in

let binutils =
  let binutilsConfigure =
    λ(cfg : types.ConfigureVars) →
      prelude.defaultConfigure cfg # [ prelude.mkExe "mkinstalldirs" ]
  in

  λ(v : List Natural) →
    prelude.makeGnuExe { name = "binutils", version = v } ⫽
      { pkgUrl = "https://mirrors.ocf.berkeley.edu/gnu/binutils/binutils-${prelude.showVersion v}.tar.xz"
      , configureCommand = binutilsConfigure
      }
in

let bison =
  let bisonConfigure =
    λ(cfg : types.ConfigureVars) →
      prelude.defaultConfigure cfg # [ prelude.mkExe "build-aux/move-if-change" ]
  in

  λ(v : List Natural) →
    prelude.makeGnuExe { name = "bison", version = v } ⫽
      { configureCommand = bisonConfigure }
in

{- cmake https://cmake.org/ -}
let cmake =
  let bootstrapConfigure =
    λ(cfg : types.ConfigureVars) →
      [ prelude.mkExe "bootstrap" ] # prelude.defaultConfigure cfg
  in

  λ(cfg : { version : List Natural, patch : Natural }) →
    let patchString = Natural/show cfg.patch
    in
    let versionString = prelude.showVersion cfg.version
    in

    prelude.defaultPackage ⫽
      { pkgName = "cmake"
      , pkgVersion = cfg.version # [ cfg.patch ]
      , pkgUrl = "https://cmake.org/files/v${versionString}/cmake-${versionString}.${patchString}.tar.gz"
      , pkgSubdir = "cmake-${versionString}.${patchString}"
      , configureCommand = bootstrapConfigure
      }
in

let curl =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "curl"
      , pkgVersion = v
      , pkgUrl = "https://curl.haxx.se/download/curl-${prelude.showVersion v}.tar.xz"
      , pkgSubdir = "curl-${prelude.showVersion v}"
      }
in

let dbus =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "dbus"
      , pkgVersion = v
      , pkgUrl = "https://dbus.freedesktop.org/releases/dbus/dbus-${prelude.showVersion v}.tar.gz"
      , pkgSubdir = "dbus-${prelude.showVersion v}"
      }
in

[ gnupg [2,2,11]
, npth [1,6]
, musl [1,1,20]
, binutils [2,31]
, bison [3,2,2]
, cmake { version = [3,13], patch = 0 }
, curl [7,62,0]
, dbus [1,12,10]
]
