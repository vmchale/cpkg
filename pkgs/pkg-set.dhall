{- Dhall prelue imports -}
let concatMap = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Text/concatMap
in

{- cpkg prelude imports -}
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
    prelude.simplePackage { name = "gnupg", version = v } ⫽
      { pkgUrl = "https://gnupg.org/ftp/gcrypt/gnupg/gnupg-${prelude.showVersion v}.tar.bz2"
      , pkgDeps = [ prelude.lowerBound { name = "npth", lower = [1,2] }
                  , prelude.lowerBound { name = "libgpg-error", lower = [1,24] }
                  , prelude.lowerBound { name = "libgcrypt", lower = [1,7,0] }
                  , prelude.lowerBound { name = "libassuan", lower = [2,5,0] }
                  , prelude.lowerBound { name = "libksba", lower = [1,3,4] }
                  ]
      }
in

let libgpgError =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libgpg-error", version = v } ⫽
      -- TODO: a mkGPGPackage function...
      { pkgUrl = "https://www.gnupg.org/ftp/gcrypt/libgpg-error/libgpg-error-${prelude.showVersion v}.tar.bz2"
      }
in

let libgcrypt =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libgcrypt", version = v } ⫽
      { pkgUrl = "https://www.gnupg.org/ftp/gcrypt/libgcrypt/libgcrypt-${prelude.showVersion v}.tar.bz2"
      }
in

let libassuan =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libassuan", version = v } ⫽
      { pkgUrl = "https://www.gnupg.org/ftp/gcrypt/libassuan/libassuan-${prelude.showVersion v}.tar.bz2"
      }
in

let libksba =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libksba", version = v } ⫽
      { pkgUrl = "https://www.gnupg.org/ftp/gcrypt/libksba/libksba-${prelude.showVersion v}.tar.bz2"
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

let jpegTurbo =
  λ(v : List Natural) →
    prelude.cmakePackage ⫽
      { pkgName = "libjpeg-turbo"
      , pkgVersion = v
      , pkgUrl = "https://downloads.sourceforge.net/libjpeg-turbo/libjpeg-turbo-${prelude.showVersion v}.tar.gz"
      , pkgSubdir = "libjpeg-turbo-${prelude.showVersion v}"
      , pkgBuildDeps = [ prelude.unbounded "cmake" ]
      }
in

let libuv =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "libuv"
      , pkgVersion = v
      , pkgUrl = "https://dist.libuv.org/dist/v${prelude.showVersion v}/libuv-v${prelude.showVersion v}.tar.gz"
      , pkgSubdir = "libuv-v${prelude.showVersion v}"
      , configureCommand = prelude.autogenConfigure
      }
in

let nasm =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "nasm"
      , pkgVersion = v
      , pkgUrl = "http://www.nasm.us/pub/nasm/releasebuilds/${prelude.showVersion v}/nasm-${prelude.showVersion v}.tar.xz"
      , pkgSubdir = "nasm-${prelude.showVersion v}"
      }
in

let ncurses =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "ncurses"
      , pkgVersion = v
      , pkgUrl = "https://invisible-mirror.net/archives/ncurses/ncurses-${prelude.showVersion v}.tar.gz"
      , pkgSubdir = "ncurses-${prelude.showVersion v}"
      }
in

let pcre2 =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "pcre2"
      , pkgVersion = v
      , pkgUrl = "https://ftp.pcre.org/pub/pcre/pcre2-${prelude.showVersion v}.tar.gz"
      , pkgSubdir = "pcre2-${prelude.showVersion v}"
      }
in

let perl5 =
  let perlConfigure =
    λ(cfg : types.ConfigureVars) →

      [ prelude.mkExe "Configure"
      , prelude.call (prelude.defaultCall ⫽ { program = "./Configure"
                                            , arguments = [ "-des", "-Dprefix=${cfg.installDir}" ]
                                            })
      ]
  in

  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "perl"
      , pkgVersion = v
      , pkgUrl = "https://www.cpan.org/src/5.0/perl-${prelude.showVersion v}.tar.gz"
      , pkgSubdir = "perl-${prelude.showVersion v}"
      , configureCommand = perlConfigure
      }
in

let png =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "libpng"
      , pkgVersion = v
      , pkgUrl = "https://download.sourceforge.net/libpng/libpng-${prelude.showVersion v}.tar.xz"
      , pkgSubdir = "libpng-${prelude.showVersion v}"
      }
in

let sed =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "sed", version = v }
in

let tar =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "tar", version = v }
in

let unistring =
  λ(v : List Natural) →
    prelude.makeGnuLibrary { name = "unistring", version = v }
in

let valgrind =
  let valgrindConfigure =
    λ(cfg : types.ConfigureVars) →
      prelude.defaultConfigure cfg # [ prelude.mkExe "auxprogs/make_or_upd_vgversion_h" ]
  in

  λ(v : List Natural) →
    prelude.simplePackage { name = "valgrind", version = v } ⫽
      { pkgUrl = "http://www.valgrind.org/downloads/valgrind-${prelude.showVersion v}.tar.bz2"
      , configureCommand = valgrindConfigure
      }
in

let vim =
  let vimConfigure =
    λ(cfg : types.ConfigureVars) →
      prelude.mkExes [ "src/configure", "src/auto/configure", "src/which.sh" ]
        # prelude.defaultConfigure cfg
  in

  let squishVersion =
    λ(x : List Natural) → concatMap Natural Natural/show x
  in

  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "vim"
      , pkgVersion = v
      , pkgUrl = "http://ftp.vim.org/vim/unix/vim-${prelude.showVersion v}.tar.bz2"
      , pkgSubdir = "vim${squishVersion v}"
      , configureCommand = vimConfigure
      }
in

let xz =
  λ(v : List Natural) →
    prelude.simplePackage { name = "xz", version = v } ⫽
      { pkgUrl = "https://tukaani.org/xz/xz-${prelude.showVersion v}.tar.xz"
      }
in

let zlib =
  λ(v : List Natural) →
    prelude.simplePackage { name = "zlib", version = v } ⫽
      { pkgUrl = "http://www.zlib.net/zlib-${prelude.showVersion v}.tar.xz"
      }
in

[ binutils [2,31]
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
, gnupg [2,2,11]
, harfbuzz [2,2,0]
, jpegTurbo [2,0,1]
, libassuan [2,5,1]
, libgcrypt [1,8,4]
, libgpgError [1,32]
, libksba [1,3,5]
, libuv [1,24,0]
, musl [1,1,20]
, nasm [2,14]
, ncurses [6,1]
, npth [1,6]
, pcre2 [10,32]
, perl5 [5,28,1]
, png [1,6,35]
, sed [4,5]
, tar [1,30]
, unistring [0,9,10]
, valgrind [3,14,0]
, vim [8,1]
, xz [5,2,4]
, zlib [1,2,11]
]
