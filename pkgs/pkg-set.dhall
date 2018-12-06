let types = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-types.dhall
in

let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

{- npth: https://www.gnupg.org/software/npth/index.html -}
let npth =
  λ(v : List Natural) →
    prelude.simplePackage { name = "npth", version = v } ⫽
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

let fltk =
  λ(cfg : { version : List Natural, patch : Natural }) →

    let versionString = prelude.showVersion cfg.version
    in
    let patchString = Natural/show cfg.patch
    in

    prelude.defaultPackage ⫽
      { pkgName = "fltk"
      , pkgVersion = cfg.version # [ cfg.patch ]
      , pkgUrl = "http://fltk.org/pub/fltk/${versionString}/fltk-${versionString}-${patchString}-source.tar.bz2"
      , pkgSubdir = "fltk-${versionString}-${patchString}"
      }
in

let gawk =
  let gawkConfigure =
    λ(cfg : types.ConfigureVars) →
      prelude.defaultConfigure cfg
        # [ prelude.mkExe "install-sh"
          , prelude.mkExe "extension/build-aux/install-sh"
          ]
  in

  λ(v : List Natural) →
    prelude.makeGnuExe { name = "gawk", version = v } ⫽
      { pkgName = "awk"
      , configureCommand = gawkConfigure
      }
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

let git =
  let gitConfigure =
    λ(cfg : types.ConfigureVars) →
      prelude.defaultConfigure cfg # prelude.mkExes [ "check_bindir" ]
  in

  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "git"
      , pkgVersion = v
      , pkgUrl = "https://mirrors.edge.kernel.org/pub/software/scm/git/git-${prelude.showVersion v}.tar.xz"
      , pkgSubdir = "git-${prelude.showVersion v}"
      , configureCommand = gitConfigure
      }
in

let glibc =
  let buildDir =
    [ "build" ] : Optional Text
  in

  let glibcConfigure =
    λ(cfg : types.ConfigureVars) →

      let maybeHost = prelude.mkHost cfg.targetTriple
      in
      let modifyArgs = λ(xs : List Text) → prelude.maybeAppend Text maybeHost xs
      in

      prelude.mkExes
        [ "configure", "scripts/mkinstalldirs", "scripts/rellns-sh" ]
          # [ prelude.createDir "build"
            , prelude.call { program = "../configure"
                          , arguments = modifyArgs [ "--prefix=${cfg.installDir}" ]
                          , environment = prelude.defaultEnv
                          , procDir = buildDir
                          }
            ]
  in

  let glibcBuild =
    λ(cfg : types.BuildVars) →
      [ prelude.call { program = prelude.makeExe cfg.buildOS
                    , arguments = [ "-j${Natural/show cfg.cpus}" ]
                    , environment = prelude.defaultEnv
                    , procDir = buildDir
                    }
      ]
  in

  let glibcInstall =
    λ(os : types.OS) →
      [ prelude.call { program = prelude.makeExe os
                    , arguments = [ "install" ]
                    , environment = prelude.defaultEnv
                    , procDir = buildDir
                    }
      ]
  in

  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "glibc"
      , pkgVersion = v
      , pkgUrl = "http://mirror.keystealth.org/gnu/libc/glibc-${prelude.showVersion v}.tar.xz"
      , pkgSubdir = "glibc-${prelude.showVersion v}"
      , configureCommand = glibcConfigure
      , buildCommand = glibcBuild
      , installCommand = glibcInstall
      }
in

let gmp =
  let gmpConfigure =
    λ(cfg : types.ConfigureVars) →
      prelude.defaultConfigure cfg # [ prelude.mkExe "mpn/m4-ccas" ]
  in

  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "gmp"
      , pkgVersion = v
      , pkgUrl = "https://gmplib.org/download/gmp/gmp-${prelude.showVersion v}.tar.xz"
      , pkgSubdir = "gmp-${prelude.showVersion v}"
      , configureCommand = gmpConfigure
      -- TODO: run 'make check' if not cross-compiling?
      }
in

let harfbuzz =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "harfbuzz"
      , pkgVersion = v
      , pkgUrl = "https://www.freedesktop.org/software/harfbuzz/release/harfbuzz-${prelude.showVersion v}.tar.bz2"
      , pkgSubdir = "harfbuzz-${prelude.showVersion v}"
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
, fltk { version = [1,3,4], patch = 2 }
, gawk [4,2,1]
, gc [8,0,0]
, git [2,19,2]
, glibc [2,28]
, gmp [6,1,2]
, harfbuzz [2,2,0]
]
