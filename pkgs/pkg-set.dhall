{- Dhall prelue imports -}
let concatMap = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Text/concatMap
in

{- cpkg prelude imports -}
let types = ../dhall/cpkg-types.dhall
in

let prelude = ../dhall/cpkg-prelude.dhall
in

{- gnupg: https://www.gnupg.org/ -}
let gpgPackage =
  λ(x : { name : Text, version : List Natural }) →
    prelude.simplePackage x ⫽
      { pkgUrl = "https://gnupg.org/ftp/gcrypt/${x.name}/${x.name}-${prelude.showVersion x.version}.tar.bz2" }
in

let gnupg =
  λ(v : List Natural) →
    gpgPackage { name = "gnupg", version = v } ⫽
      { pkgDeps = [ prelude.lowerBound { name = "npth", lower = [1,2] }
                  , prelude.lowerBound { name = "libgpg-error", lower = [1,24] }
                  , prelude.lowerBound { name = "libgcrypt", lower = [1,7,0] }
                  , prelude.lowerBound { name = "libassuan", lower = [2,5,0] }
                  , prelude.lowerBound { name = "libksba", lower = [1,3,4] }
                  -- , prelude.unbounded "gnutls"
                  ]
      , configureCommand = prelude.configureMkExes [ "tests/inittests", "tests/runtest", "tests/pkits/inittests" ]
      , installCommand = prelude.installWithBinaries [ "bin/gpg" ]
      }
in

let npth =
  λ(v : List Natural) →
    gpgPackage { name = "npth", version = v }
in

let libgpgError =
  λ(v : List Natural) →
    gpgPackage { name = "libgpg-error", version = v }
in

let libgcrypt =
  λ(v : List Natural) →
    gpgPackage { name = "libgcrypt", version = v }
in

let libassuan =
  λ(v : List Natural) →
    gpgPackage { name = "libassuan", version = v } ⫽
      { pkgDeps = [ prelude.lowerBound { name = "libgpg-error", lower = [1,24] } ] }
in

let libksba =
  λ(v : List Natural) →
    gpgPackage { name = "libksba", version = v }
in

{- musl: https://www.musl-libc.org/ -}
let musl =
  λ(v : List Natural) →
    prelude.simplePackage { name = "musl", version = v } ⫽
      { pkgUrl = "https://www.musl-libc.org/releases/musl-${prelude.showVersion v}.tar.gz"
      , installCommand = prelude.installWithBinaries [ "bin/musl-gcc" ]
      , configureCommand = prelude.configureMkExes [ "tools/install.sh" ]
      }
in

let binutils =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "binutils", version = v } ⫽
      { pkgUrl = "https://mirrors.ocf.berkeley.edu/gnu/binutils/binutils-${prelude.showVersion v}.tar.xz"
      , configureCommand = prelude.configureMkExes [ "mkinstalldirs" ]
      , installCommand =
          prelude.installWithBinaries [ "bin/ar", "bin/as", "bin/ld", "bin/strip", "bin/strings", "bin/readelf", "bin/objdump", "bin/nm" ]
      }
in

let bison =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "bison", version = v } ⫽
      { configureCommand = prelude.configureMkExes [ "build-aux/move-if-change" ]
      , installCommand = prelude.installWithBinaries [ "bin/bison", "bin/yacc" ]
      , pkgBuildDeps = [ prelude.unbounded "m4" ]
      }
in

{- cmake https://cmake.org/ -}
let cmake =
  λ(cfg : { version : List Natural, patch : Natural }) →
    let patchString = Natural/show cfg.patch
    in
    let versionString = prelude.showVersion cfg.version
    in
    let cmakeConfigure =
      λ(cfg : types.ConfigureVars) →
        prelude.configureMkExesExtraFlags { bins = [ "bootstrap" ]
                                          , extraFlags = [ "--parallel=${Natural/show cfg.cfgCpus}" ]
                                          } cfg
    in

    prelude.defaultPackage ⫽
      { pkgName = "cmake"
      , pkgVersion = cfg.version # [ cfg.patch ]
      , pkgUrl = "https://cmake.org/files/v${versionString}/cmake-${versionString}.${patchString}.tar.gz"
      , pkgSubdir = "cmake-${versionString}.${patchString}"
      , configureCommand = cmakeConfigure
      , installCommand = prelude.installWithBinaries [ "bin/cmake" ]
      }
in

let curl =
  λ(v : List Natural) →
    prelude.simplePackage { name = "curl", version = v } ⫽
      { pkgUrl = "https://curl.haxx.se/download/curl-${prelude.showVersion v}.tar.xz"
      , installCommand = prelude.installWithBinaries [ "bin/curl" ]
      }
in

let dbus =
  λ(v : List Natural) →
    prelude.simplePackage { name = "dbus", version = v } ⫽
      { pkgUrl = "https://dbus.freedesktop.org/releases/dbus/dbus-${prelude.showVersion v}.tar.gz" }
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
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "gawk", version = v } ⫽
      { configureCommand = prelude.configureMkExes [ "install-sh", "extension/build-aux/install-sh" ]
      , installCommand = prelude.installWithBinaries [ "bin/gawk", "bin/awk" ]
      }
in

let gc =
  λ(v : List Natural) →
    prelude.simplePackage { name = "gc", version = v } ⫽
        { pkgUrl = "https://github.com/ivmai/bdwgc/releases/download/v${prelude.showVersion v}/gc-${prelude.showVersion v}.tar.gz"
        , pkgDeps = [ prelude.unbounded "libatomic_ops" ]
        }
in

let libatomic_ops =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libatomic_ops", version = v } ⫽
        { pkgUrl = "https://github.com/ivmai/libatomic_ops/releases/download/v${prelude.showVersion v}/libatomic_ops-${prelude.showVersion v}.tar.gz" }
in

let git =
  λ(v : List Natural) →
    prelude.simplePackage { name = "git", version = v } ⫽
      { pkgUrl = "https://mirrors.edge.kernel.org/pub/software/scm/git/git-${prelude.showVersion v}.tar.xz"
      , configureCommand = prelude.configureMkExes [ "check_bindir" ]
      , installCommand = prelude.installWithBinaries [ "bin/git" ]
      , pkgBuildDeps = [ prelude.unbounded "gettext" ]
      }
in

let glibc =
  let buildDir =
    Some "build"
  in

  let glibcConfigure =
    λ(cfg : types.ConfigureVars) →

      let maybeHost = prelude.mkHost cfg.targetTriple
      in
      let modifyArgs = prelude.maybeAppend Text maybeHost
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
    λ(cfg : types.InstallVars) →
      [ prelude.call { program = prelude.makeExe cfg.installOS
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
      , pkgBuildDeps = [ prelude.unbounded "bison", prelude.unbounded "gawk" ]
      }
in

let gmp =
  λ(v : List Natural) →
    prelude.simplePackage { name = "gmp", version = v } ⫽
      { pkgUrl = "https://gmplib.org/download/gmp/gmp-${prelude.showVersion v}.tar.xz"
      , configureCommand = prelude.configureMkExes [ "mpn/m4-ccas" ]
      , pkgBuildDeps = [ prelude.unbounded "m4" ]
      -- TODO: run 'make check' if not cross-compiling?
      }
in

let harfbuzz =
  λ(v : List Natural) →
    prelude.simplePackage { name = "harfbuzz", version = v } ⫽
      { pkgUrl = "https://www.freedesktop.org/software/harfbuzz/release/harfbuzz-${prelude.showVersion v}.tar.bz2" }
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
    prelude.simplePackage { name = "nasm", version = v } ⫽
      { pkgUrl = "http://www.nasm.us/pub/nasm/releasebuilds/${prelude.showVersion v}/nasm-${prelude.showVersion v}.tar.xz"
      , installCommand = prelude.installWithBinaries [ "bin/nasm", "bin/ndisasm" ]
      }
in

let ncurses =
  λ(v : List Natural) →
    prelude.simplePackage { name = "ncurses", version = v } ⫽
      { pkgUrl = "https://invisible-mirror.net/archives/ncurses/ncurses-${prelude.showVersion v}.tar.gz" }
in

let pcre2 =
  λ(v : List Natural) →
    prelude.simplePackage { name = "pcre2", version = v } ⫽
      { pkgUrl = "https://ftp.pcre.org/pub/pcre/pcre2-${prelude.showVersion v}.tar.gz" }
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
    prelude.simplePackage { name = "perl", version = v } ⫽
      { pkgUrl = "https://www.cpan.org/src/5.0/perl-${prelude.showVersion v}.tar.gz"
      , configureCommand = perlConfigure
      , installCommand = prelude.installWithBinaries [ "bin/perl", "bin/cpan" ]
      }
in

let libpng =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libpng", version = v } ⫽
      { pkgUrl = "https://download.sourceforge.net/libpng/libpng-${prelude.showVersion v}.tar.xz" }
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
  λ(v : List Natural) →
    prelude.simplePackage { name = "valgrind", version = v } ⫽
      { pkgUrl = "http://www.valgrind.org/downloads/valgrind-${prelude.showVersion v}.tar.bz2"
      , installCommand = prelude.installWithBinaries [ "bin/valgrind" ]
      , configureCommand = prelude.configureMkExes [ "auxprogs/make_or_upd_vgversion_h" ]
      }
in

let vim =
  let squishVersion =
    concatMap Natural Natural/show
  in

  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "vim"
      , pkgVersion = v
      , pkgUrl = "http://ftp.vim.org/vim/unix/vim-${prelude.showVersion v}.tar.bz2"
      , pkgSubdir = "vim${squishVersion v}"
      , configureCommand =
          prelude.configureMkExesExtraFlags { bins = [ "src/configure", "src/auto/configure", "src/which.sh" ]
                                            , extraFlags = [ "--enable-luainterp=yes"
                                                           ]
                                            }
      , installCommand = prelude.installWithBinaries [ "bin/vim", "bin/xxd" ]
      , pkgDeps = [ prelude.unbounded "ncurses"
                  , prelude.unbounded "lua"
                  ]
      }
in

let xz =
  λ(v : List Natural) →
    prelude.simplePackage { name = "xz", version = v } ⫽
      { pkgUrl = "https://tukaani.org/xz/xz-${prelude.showVersion v}.tar.xz"
      , installCommand = prelude.installWithBinaries [ "bin/xz" ]
      }
in

-- FIXME: fix cross-compilation/configuration
let zlib =
  λ(v : List Natural) →

    let zlibConfigure =
      λ(cfg : types.ConfigureVars) →

        let host =
          Optional/fold Text cfg.targetTriple Text (λ(tgt : Text) → "${tgt}-") ""
        in

        [ prelude.mkExe "configure"
        , prelude.call (prelude.defaultCall ⫽ { program = "./configure"
                                              , arguments = [ "--prefix=${cfg.installDir}" ]
                                              , environment = Some [ { var = "CC", value = "${host}gcc" }, { var = "PATH", value = "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" } ]
                                              })
        ]
    in

    prelude.simplePackage { name = "zlib", version = v } ⫽
      { pkgUrl = "http://www.zlib.net/zlib-${prelude.showVersion v}.tar.xz"
      , configureCommand = zlibConfigure
      }
in

let gettext =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "gettext", version = v } ⫽
      { installCommand = prelude.installWithBinaries [ "bin/gettext", "bin/msgfmt" ] }
in

let gzip =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "gzip", version = v }
in

let wget =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "wget", version = v } ⫽
      { pkgUrl = "https://ftp.gnu.org/gnu/wget/wget-${prelude.showVersion v}.tar.gz"
      , pkgDeps = [ prelude.unbounded "gnutls" ]
      , pkgBuildDeps = [ prelude.unbounded "perl" ]
      , configureCommand = prelude.configureMkExes [ "doc/texi2pod.pl" ]
      }
in

let gnutls =
  λ(cfg : { version : List Natural, patch : Natural }) →
    let versionString = prelude.showVersion cfg.version
    in

    prelude.simplePackage { name = "gnutls", version = cfg.version # [ cfg.patch ] } ⫽
      { pkgUrl = "https://www.gnupg.org/ftp/gcrypt/gnutls/v${versionString}/gnutls-${versionString}.${Natural/show cfg.patch}.tar.xz"
      , pkgDeps = [ prelude.lowerBound { name = "nettle", lower = [3,4,1] }
                  , prelude.unbounded "unistring"
                  , prelude.lowerBound { name = "libtasn1", lower = [4,9] }
                  , prelude.lowerBound { name = "p11-kit", lower = [0,23,1] }
                  ]
      }
in

let lapack =
  λ(v : List Natural) →
    prelude.cmakePackage ⫽
      { pkgName = "lapack"
      , pkgVersion = v
      , pkgUrl = "http://www.netlib.org/lapack/lapack-${prelude.showVersion v}.tar.gz"
      , pkgSubdir = "lapack-${prelude.showVersion v}"
      , pkgBuildDeps = [ prelude.unbounded "cmake" ]
      }
in

let cairo =
  λ(v : List Natural) →
    prelude.simplePackage { name = "cairo", version = v } ⫽
     { pkgUrl = "https://www.cairographics.org/releases/cairo-${prelude.showVersion v}.tar.xz"
     , pkgDeps = [ prelude.lowerBound { name = "pixman", lower = [0,3,0] }
                 , prelude.unbounded "freetype"
                 ]
     }
in

let libnettle =
  λ(v : List Natural) →
    prelude.simplePackage { name = "nettle", version = v } ⫽
      { pkgUrl = "https://ftp.gnu.org/gnu/nettle/nettle-${prelude.showVersion v}.tar.gz"
      , pkgBuildDeps = [ prelude.unbounded "m4" ]
      }
in

let m4 =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "m4", version = v }
in

let nginx =
  λ(v : List Natural) →
    prelude.simplePackage { name = "nginx", version = v } ⫽
      { pkgUrl = "http://nginx.org/download/nginx-${prelude.showVersion v}.tar.gz"
      , pkgDeps = [ prelude.unbounded "zlib", prelude.unbounded "pcre2" ]
      }
in

let openssl =
  λ(v : List Natural) →
    prelude.simplePackage { name = "openssl", version = v } ⫽
      { pkgUrl = "https://www.openssl.org/source/openssl-${prelude.showVersion v}a.tar.gz"
      , configureCommand = prelude.generalConfigure "config" ([] : List Text)
      , pkgSubdir = "openssl-${prelude.showVersion v}a"
      }
in

let libssh2 =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libssh2", version = v } ⫽
      { pkgUrl = "https://www.libssh2.org/download/libssh2-${prelude.showVersion v}.tar.gz" }
in

let giflib =
  λ(v : List Natural) →
    prelude.simplePackage { name = "giflib", version = v } ⫽
      { pkgUrl = "https://downloads.sourceforge.net/giflib/giflib-${prelude.showVersion v}.tar.bz2" }
in

let emacs =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "emacs", version = v } ⫽
      { pkgDeps = [ prelude.unbounded "giflib" ]
      , configureCommand = prelude.configureMkExes [ "build-aux/move-if-change", "build-aux/update-subdirs" ]
      }
in

let which =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "which", version = v } ⫽
      { pkgUrl = "https://ftp.gnu.org/gnu/which/which-${prelude.showVersion v}.tar.gz" }
in

let automake =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "automake", version = v } ⫽
      { pkgBuildDeps = [ prelude.lowerBound { name =  "autoconf", lower = [2,65] } ]
      , installCommand = prelude.installWithBinaries [ "bin/automake", "bin/aclocal" ]
      }
in

let autoconf =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "autoconf", version = v } ⫽
      { pkgBuildDeps = [ prelude.lowerBound { name =  "m4", lower = [1,4,16] } ]
      , installCommand = prelude.installWithBinaries [ "bin/autoconf", "bin/autoheader", "bin/autom4te" ]
      }
in

let python =
  λ(v : List Natural) →
    let major = Optional/fold Natural (List/head Natural v) Text (Natural/show) ""
    in
    let versionString = prelude.showVersion v
    in

    prelude.simplePackage { name = "python${major}", version = v } ⫽
      { pkgUrl = "https://www.python.org/ftp/python/${versionString}/Python-${versionString}.tar.xz"
      , pkgSubdir = "Python-${versionString}"
      , configureCommand = prelude.configureWithFlags [ "--enable-optimizations" ]
      , installCommand = prelude.installWithBinaries [ "bin/python${major}" ]
      }
in

let lua =
  λ(v : List Natural) →
    let printLuaOS =
      λ(os : types.OS) →
        merge
          { FreeBSD   = λ(_ : {}) → "freebsd"
          , OpenBSD   = λ(_ : {}) → "bsd"
          , NetBSD    = λ(_ : {}) → "bsd"
          , Solaris   = λ(_ : {}) → "solaris"
          , Dragonfly = λ(_ : {}) → "bsd"
          , Linux     = λ(_ : {}) → "linux"
          , Darwin    = λ(_ : {}) → "macosx"
          , Windows   = λ(_ : {}) → "mingw"
          , Redox     = λ(_ : {}) → "generic"
          , NoOs      = λ(_ : {}) → "c89"
          }
          os
    in


    let luaBuild =
      λ(cfg : types.BuildVars) →
        let cc =
          Optional/fold Text cfg.buildTgt (List Text) (λ(tgt : Text) → ["CC=${tgt}-gcc"]) ([] : List Text)
        in

        let ldflags =
          (prelude.mkLDFlags (cfg.linkDirsBld)).value
        in

        let cflags =
          (prelude.mkCFlags (cfg.includeDirsBld)).value
        in

        [ prelude.call (prelude.defaultCall ⫽ { program = "make"
                                              , arguments = cc # [ printLuaOS cfg.buildOS, "MYLDFLAGS=${ldflags}", "MYCFLAGS=${cflags}" ]
                                              })
        ]
    in

    let luaInstall =
      λ(cfg : types.InstallVars) →
        [ prelude.call (prelude.defaultCall ⫽ { program = "make"
                                              , arguments = [ "install", "INSTALL_TOP=${cfg.installPath}" ]
                                              }) ]
          # prelude.symlinkBinaries [ "bin/lua", "bin/luac" ]
    in

    prelude.simplePackage { name = "lua", version = v } ⫽
      { pkgUrl = "http://www.lua.org/ftp/lua-${prelude.showVersion v}.tar.gz"
      , configureCommand = (λ(_ : types.ConfigureVars) → [] : List types.Command)
      , buildCommand = luaBuild
      , installCommand = luaInstall
      , pkgDeps = [ prelude.unbounded "readline" ]
      }
in

let libtasn1 =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libtasn1", version = v } ⫽
      { pkgUrl = "https://ftp.gnu.org/gnu/libtasn1/libtasn1-${prelude.showVersion v}.tar.gz" }
in

let p11kit =
  λ(v : List Natural) →
    prelude.simplePackage { name = "p11-kit", version = v } ⫽
      { pkgUrl = "https://github.com/p11-glue/p11-kit/releases/download/${prelude.showVersion v}/p11-kit-${prelude.showVersion v}.tar.gz"
      , pkgDeps = [ prelude.lowerBound { name = "libffi", lower = [3,0,0] } ]
      }
in

let libffi =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libffi", version = v } ⫽
      { pkgUrl = "https://sourceware.org/ftp/libffi/libffi-${prelude.showVersion v}.tar.gz" }
in

let gdb =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "gdb", version = v } ⫽
      { configureCommand = prelude.configureMkExes [ "mkinstalldirs" ] }
in

let libtool =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "libtool", version = v } ⫽
      { pkgUrl = "http://ftpmirror.gnu.org/libtool/libtool-${prelude.showVersion v}.tar.gz"
      , pkgBuildDeps = [ prelude.lowerBound { name =  "m4", lower = [1,4,16] } ]
      }
in

let pkg-config =
  λ(v : List Natural) →
    prelude.simplePackage { name = "pkg-config", version = v } ⫽
      { pkgUrl = "https://pkg-config.freedesktop.org/releases/pkg-config-${prelude.showVersion v}.tar.gz"
      , installCommand = prelude.installWithBinaries [ "bin/pkg-config" ]
      }
in

let qrencode =
  λ(v : List Natural) →
    prelude.simplePackage { name = "qrencode", version = v } ⫽
      { pkgUrl = "https://fukuchi.org/works/qrencode/qrencode-${prelude.showVersion v}.tar.gz"
      , configureCommand = prelude.configureWithFlags [ "--without-tools" ]
      }
in

let readline =
  λ(v : List Natural) →
    prelude.simplePackage { name = "readline", version = v } ⫽
      { pkgUrl = "https://ftp.gnu.org/gnu/readline/readline-${prelude.showVersion v}.tar.gz" }
in

let pixman =
  λ(v : List Natural) →
    prelude.simplePackage { name = "pixman", version = v } ⫽
      { pkgUrl = "https://www.cairographics.org/releases/pixman-${prelude.showVersion v}.tar.gz"
      , pkgDeps = [ prelude.unbounded "libpng" ]
      }
in

let freetype =
  λ(v : List Natural) →
    prelude.simplePackage { name = "freetype", version = v } ⫽
      { pkgUrl = "https://download.savannah.gnu.org/releases/freetype/freetype-${prelude.showVersion v}.tar.gz"
      , configureCommand = prelude.configureMkExes [ "builds/unix/configure" ]
      }
in

let sdl2 =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v
    in

    prelude.simplePackage { name = "sdl2", version = v } ⫽
      { pkgUrl = "https://www.libsdl.org/release/SDL2-${versionString}.tar.gz"
      , pkgSubdir = "SDL2-${versionString}"
      }
in

let zbar =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v
    in

    prelude.simplePackage { name = "zbar", version = v } ⫽
      { pkgUrl = "https://managedway.dl.sourceforge.net/project/zbar/zbar/${versionString}/zbar-${versionString}.tar.bz2"
      , configureCommand = prelude.configureWithFlags [ "--disable-video" ]
      , pkgDeps = [ prelude.lowerBound { name = "imagemagick", lower = [6,2,6] } ]
      }
in

let imageMagick =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v
    in

    prelude.simplePackage { name = "imagemagick", version = v } ⫽
      { pkgUrl = "https://imagemagick.org/download/ImageMagick-${versionString}-16.tar.xz"
      , pkgSubdir = "ImageMagick-${versionString}-16"
      }
in

[ autoconf [2,69]
, automake [1,16,1]
, binutils [2,31]
, bison [3,2,2]
, cairo [1,16,0]
, cmake { version = [3,13], patch = 0 }
, curl [7,62,0]
, dbus [1,12,10]
, emacs [25,3]
, fltk { version = [1,3,4], patch = 2 }
, freetype [2,9,1]
, gawk [4,2,1]
, gc [8,0,0]
, gdb [8,2]
, gettext [0,19,8]
, giflib [5,1,4]
, git [2,19,2]
, glibc [2,28]
, gmp [6,1,2]
, gnupg [2,2,11]
-- , gnutls { version = [3,6], patch = 5 }
, gnutls { version = [3,5], patch = 19 }
, gzip [1,9]
, harfbuzz [2,2,0]
, imageMagick [7,0,8]
, p11kit [0,23,14]
, python [2,7,15]
, python [3,7,1]
, lapack [3,8,0]
, jpegTurbo [2,0,1]
, libassuan [2,5,1]
, libatomic_ops [7,6,8]
, libffi [3,2,1]
, libgcrypt [1,8,4]
, libgpgError [1,32]
, libksba [1,3,5]
, libpng [1,6,35]
, libnettle [3,4,1]
, libssh2 [1,8,0]
, libtasn1 [4,13]
, libtool [2,4,6]
, libuv [1,24,0]
, lua [5,3,5]
, m4 [1,4,18]
, musl [1,1,20]
, nasm [2,14]
, ncurses [6,1]
, nginx [1,15,7]
, npth [1,6]
, openssl [1,1,1]
, pcre2 [10,32]
, perl5 [5,28,1]
, pixman [0,36,0]
, pkg-config [0,29,2]
, qrencode [4,0,2]
, readline [7,0]
, sdl2 [2,0,9]
, sed [4,5]
, tar [1,30]
, unistring [0,9,10]
, valgrind [3,14,0]
, vim [8,1]
, wget [1,20]
, which [2,21]
, xz [5,2,4]
, zbar [0,10]
, zlib [1,2,11]
]
