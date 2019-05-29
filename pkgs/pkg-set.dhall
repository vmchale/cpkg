{- Dhall prelue imports -}
let concatMapSep = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Text/concatMapSep
in

let concatMapText = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Text/concatMap
in

let not = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Bool/not
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
    gpgPackage { name = "libgcrypt", version = v } ⫽
      { pkgDeps = [ prelude.lowerBound { name = "libgpg-error", lower = [1,25] } ] }
in

let libassuan =
  λ(v : List Natural) →
    gpgPackage { name = "libassuan", version = v } ⫽
      { pkgDeps = [ prelude.lowerBound { name = "libgpg-error", lower = [1,24] } ] }
in

let libksba =
  λ(v : List Natural) →
    gpgPackage { name = "libksba", version = v } ⫽
      { pkgDeps = [ prelude.lowerBound { name = "libgpg-error", lower = [1,8] } ] }
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
          prelude.installWithBinaries [ "bin/ar", "bin/as", "bin/ld", "bin/strip", "bin/strings", "bin/readelf", "bin/objdump", "bin/nm", "bin/ranlib" ]
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
      λ(cfg : types.BuildVars) →
        prelude.configureMkExesExtraFlags { bins = [ "bootstrap" ]
                                          , extraFlags = [ "--parallel=${Natural/show cfg.cpus}" ]
                                          } cfg
    in

    prelude.defaultPackage ⫽
    -- TODO: build dep on gcc/g++
      { pkgName = "cmake"
      , pkgVersion = prelude.fullVersion cfg
      , pkgUrl = "https://cmake.org/files/v${versionString}/cmake-${versionString}.${patchString}.tar.gz"
      , pkgSubdir = "cmake-${versionString}.${patchString}"
      , configureCommand = cmakeConfigure
      , installCommand =
          λ(cfg : types.BuildVars) →
            let wrapper = "CMAKE_ROOT=${cfg.installDir}/share/cmake-${versionString}/ ${cfg.installDir}/bin/cmake $@"
            in
            let wrapped = "wrapper/cmake"
            in
            prelude.defaultInstall cfg
              # [ prelude.createDir "wrapper"
                , prelude.writeFile { file = wrapped, contents = wrapper }
                , prelude.mkExe wrapped
                , prelude.copyFile wrapped wrapped
                , prelude.symlinkBinary wrapped
                ]
      }
in

let curl =
  λ(v : List Natural) →
    prelude.simplePackage { name = "curl", version = v } ⫽
      { pkgUrl = "https://curl.haxx.se/download/curl-${prelude.showVersion v}.tar.xz"
      , installCommand = prelude.installWithBinaries [ "bin/curl" ]
      , pkgDeps = [ prelude.unbounded "zlib" ]
      }
in

let dbus =
  λ(v : List Natural) →
    prelude.simplePackage { name = "dbus", version = v } ⫽
      { pkgUrl = "https://dbus.freedesktop.org/releases/dbus/dbus-${prelude.showVersion v}.tar.gz"
      , pkgDeps = [ prelude.unbounded "expat"
                  , prelude.unbounded "libselinux"
                  ]
      , configureCommand = prelude.configureLinkExtraLibs [ "pcre" ]
      , pkgBuildDeps = [ prelude.unbounded "pkg-config" ]
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
      , pkgVersion = prelude.fullVersion cfg
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
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "gc", version = v } ⫽
        { pkgUrl = "https://github.com/ivmai/bdwgc/releases/download/v${versionString}/gc-${versionString}.tar.gz"
        , pkgDeps = [ prelude.unbounded "libatomic_ops" ]
        }
in

let libatomic_ops =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "libatomic_ops", version = v } ⫽
        { pkgUrl = "https://github.com/ivmai/libatomic_ops/releases/download/v${versionString}/libatomic_ops-${versionString}.tar.gz" }
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
    λ(cfg : types.BuildVars) →

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
    λ(cfg : types.BuildVars) →
      [ prelude.call { program = prelude.makeExe cfg.buildOS
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
      -- TODO: run 'make check'?
      }
in

let harfbuzz =
  let symlinkHarfbuzz =
    λ(h : Text) →
      prelude.symlink "include/harfbuzz/${h}" "include/${h}"
  in

  λ(v : List Natural) →
    prelude.simplePackage { name = "harfbuzz", version = v } ⫽
      { pkgUrl = "https://www.freedesktop.org/software/harfbuzz/release/harfbuzz-${prelude.showVersion v}.tar.xz"
      , pkgDeps = [ prelude.unbounded "freetype-prebuild"
                  , prelude.unbounded "glib"
                  ]
      , pkgBuildDeps = [ prelude.unbounded "pkg-config" ]
      , configureCommand = prelude.configureLinkExtraLibs [ "pcre", "z" ]
      , installCommand =
          λ(cfg : types.BuildVars) →
            prelude.defaultInstall cfg
              # [ symlinkHarfbuzz "hb-aat-layout.h"
                , symlinkHarfbuzz "hb-aat.h"
                , symlinkHarfbuzz "hb-blob.h"
                , symlinkHarfbuzz "hb-buffer.h"
                , symlinkHarfbuzz "hb-common.h"
                , symlinkHarfbuzz "hb-deprecated.h"
                , symlinkHarfbuzz "hb-face.h"
                , symlinkHarfbuzz "hb-font.h"
                , symlinkHarfbuzz "hb-ft.h"
                , symlinkHarfbuzz "hb-glib.h"
                , symlinkHarfbuzz "hb-icu.h"
                , symlinkHarfbuzz "hb-map.h"
                , symlinkHarfbuzz "hb-ot-color.h"
                , symlinkHarfbuzz "hb-ot-font.h"
                , symlinkHarfbuzz "hb-ot-layout.h"
                , symlinkHarfbuzz "hb-ot-math.h"
                , symlinkHarfbuzz "hb-ot-name.h"
                , symlinkHarfbuzz "hb-ot-shape.h"
                , symlinkHarfbuzz "hb-ot-var.h"
                , symlinkHarfbuzz "hb-ot.h"
                , symlinkHarfbuzz "hb-set.h"
                , symlinkHarfbuzz "hb-shape-plan.h"
                , symlinkHarfbuzz "hb-shape.h"
                , symlinkHarfbuzz "hb-subset.h"
                , symlinkHarfbuzz "hb-unicode.h"
                , symlinkHarfbuzz "hb-version.h"
                , symlinkHarfbuzz "hb.h"
                ]
      }
in

let libjpeg-turbo =
  λ(v : List Natural) →
    prelude.cmakePackage ⫽
      { pkgName = "libjpeg-turbo"
      , pkgVersion = v
      , pkgUrl = "https://downloads.sourceforge.net/libjpeg-turbo/libjpeg-turbo-${prelude.showVersion v}.tar.gz"
      , pkgSubdir = "libjpeg-turbo-${prelude.showVersion v}"
      , pkgBuildDeps = [ prelude.unbounded "cmake"
                       , prelude.unbounded "nasm"
                       ]
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
      { pkgUrl = "http://www.nasm.us/pub/nasm/releasebuilds/${prelude.showVersion v}.02/nasm-${prelude.showVersion v}.02.tar.xz"
      , pkgSubdir = "nasm-${prelude.showVersion v}.02"
      , installCommand = prelude.installWithBinaries [ "bin/nasm", "bin/ndisasm" ]
      }
in

let ncurses =
  λ(v : List Natural) →
    prelude.simplePackage { name = "ncurses", version = v } ⫽
      { pkgUrl = "https://ftp.gnu.org/pub/gnu/ncurses/ncurses-${prelude.showVersion v}.tar.gz"
      , configureCommand =
        λ(cfg : types.BuildVars) →
          let crossArgs =
            if cfg.isCross
              then [ "--disable-stripping" ]
              else [] : List Text
          in

          prelude.configureWithFlags ([ "--with-shared", "--enable-widec" ] # crossArgs) cfg
          -- enable-widec is necessary because util-linux uses libncursesw
      }
in

let pcre2 =
  λ(v : List Natural) →
    prelude.simplePackage { name = "pcre2", version = v } ⫽
      { pkgUrl = "https://ftp.pcre.org/pub/pcre/pcre2-${prelude.showVersion v}.tar.bz2" }
in

let pcre =
  λ(v : List Natural) →
    prelude.simplePackage { name = "pcre", version = v } ⫽
      { pkgUrl = "https://ftp.pcre.org/pub/pcre/pcre-${prelude.showVersion v}.tar.bz2" }
in

let perl5 =
  let perlConfigure =
    λ(cfg : types.BuildVars) →
      [ prelude.mkExe "Configure"
      , prelude.call (prelude.defaultCall ⫽ { program = "./Configure"
                                            , arguments = [ "-des", "-Dprefix=${cfg.installDir}" ] # (if cfg.static then [] : List Text else [ "-Duseshrplib" ])
                                            })
      ]
  in

  λ(v : List Natural) →
    let major = Optional/fold Natural (List/head Natural v) Text Natural/show ""
    in

    prelude.simplePackage { name = "perl", version = v } ⫽
      { pkgUrl = "https://www.cpan.org/src/${major}.0/perl-${prelude.showVersion v}.tar.gz"
      , configureCommand = perlConfigure
      , installCommand =
        λ(cfg : types.BuildVars) →
          let libperlFile =
            if cfg.static
              then "libperl.a"
              else "libperl.so"
          in

          prelude.installWithBinaries [ "bin/perl", "bin/cpan" ] cfg
            # [ prelude.symlink "lib/${prelude.showVersion v}/${prelude.printArch cfg.buildArch}-${prelude.printOS cfg.buildOS}/CORE/${libperlFile}" "lib/${libperlFile}" ]
      }
in

let libpng =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libpng", version = v } ⫽
      { pkgUrl = "https://download.sourceforge.net/libpng/libpng-${prelude.showVersion v}.tar.xz"
      , pkgDeps = [ prelude.unbounded "zlib" ]
      }
in

let sed =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "sed", version = v } -- TODO: require pcre?
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
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "vim"
      , pkgVersion = v
      , pkgUrl = "http://ftp.vim.org/vim/unix/vim-${prelude.showVersion v}.tar.bz2"
      , pkgSubdir = "vim${prelude.squishVersion v}"
      , configureCommand =
          prelude.configureMkExesExtraFlags { bins = [ "src/configure", "src/auto/configure", "src/which.sh" ]
                                            , extraFlags = [ "--enable-gui=no"
                                                           , "--enable-pythoninterp"
                                                           ]
                                            }
      , installCommand =
          λ(cfg : types.BuildVars) →
            let mkLibDynload =
              λ(libs : List Text) →
                concatMapSep ":" Text (λ(dir : Text) → "${dir}/python2.7/lib-dynload") libs
            in
            let mkPython =
              λ(libs : List Text) →
                concatMapSep ":" Text (λ(dir : Text) → "${dir}/python2.7/:${dir}/python2.7/lib-dynload") libs
            in
            -- TODO: change LD_RUN_PATH during build instead...
            -- or alternately, symlink lib-dynload stuff when installing python2??
            let wrapper = "LD_LIBRARY_PATH=${mkLibDynload cfg.linkDirs} PYTHONPATH=${mkPython cfg.linkDirs} ${cfg.installDir}/bin/vim $@"
            in
            let wrapped = "wrapper/vim"
            in

            prelude.installWithBinaries [ "bin/xxd" ] cfg
              # [ prelude.createDir "wrapper"
                , prelude.writeFile { file = wrapped, contents = wrapper }
                , prelude.mkExe wrapped
                , prelude.copyFile wrapped wrapped
                , prelude.symlinkBinary wrapped
                ]
      , pkgDeps = [ prelude.unbounded "ncurses"
                  , prelude.unbounded "libXpm"
                  , prelude.unbounded "libXt"
                  , prelude.unbounded "python2"
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

let zlib =
  λ(v : List Natural) →

    let zlibConfigure =
      λ(cfg : types.BuildVars) →

        let host =
          prelude.mkCCVar cfg
        in

        [ prelude.mkExe "configure"
        , prelude.call (prelude.defaultCall ⫽ { program = "./configure"
                                              , arguments = [ "--prefix=${cfg.installDir}" ]
                                              , environment = Some (host # [ { var = "PATH", value = "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" } ])
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
      { installCommand = prelude.installWithBinaries [ "bin/gettext", "bin/msgfmt", "bin/autopoint" ] }
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
      , installCommand =
          prelude.installWithWrappers [ "wget" ]
      }
in

let gnutls =
  λ(cfg : { version : List Natural, patch : Natural }) →
    let versionString = prelude.showVersion cfg.version
    in

    prelude.simplePackage { name = "gnutls", version = prelude.fullVersion cfg } ⫽
      { pkgUrl = "https://www.gnupg.org/ftp/gcrypt/gnutls/v${versionString}/gnutls-${versionString}.${Natural/show cfg.patch}.tar.xz"
      , pkgDeps = [ prelude.lowerBound { name = "nettle", lower = [3,1] }
                  , prelude.unbounded "unistring"
                  , prelude.lowerBound { name = "libtasn1", lower = [4,9] }
                  , prelude.lowerBound { name = "p11-kit", lower = [0,23,1] }
                  ]
      , configureCommand =
	λ(cfg : types.BuildVars) →
	  prelude.mkExes [ "src/gen-mech-list.sh" ]
	    # prelude.configureLinkExtraLibs [ "nettle", "hogweed" ] cfg
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
  let symlinkCairo =
    λ(h : Text) →
      prelude.symlink "include/cairo/${h}" "include/${h}"
  in

  λ(v : List Natural) →
    prelude.simplePackage { name = "cairo", version = v } ⫽
     { pkgUrl = "https://www.cairographics.org/releases/cairo-${prelude.showVersion v}.tar.xz"
     , pkgDeps = [ prelude.lowerBound { name = "pixman", lower = [0,30,0] }
                 , prelude.lowerBound { name = "freetype", lower = [9,7,3] }
                 , prelude.lowerBound { name = "fontconfig", lower = [2,2,95] }
                 , prelude.unbounded "libXext"
                 ]
     , installCommand =
        λ(cfg : types.BuildVars) →
          prelude.defaultInstall cfg #
            [ symlinkCairo "cairo-deprecated.h"
            , symlinkCairo "cairo-features.h"
            , symlinkCairo "cairo-ft.h"
            , symlinkCairo "cairo-gobject.h"
            , symlinkCairo "cairo-pdf.h"
            , symlinkCairo "cairo-ps.h"
            , symlinkCairo "cairo-script-interpreter.h"
            , symlinkCairo "cairo-script.h"
            , symlinkCairo "cairo-svg.h"
            , symlinkCairo "cairo-version.h"
            , symlinkCairo "cairo-xcb.h"
            , symlinkCairo "cairo-xlib-xrender.h"
            , symlinkCairo "cairo-xlib.h"
            , symlinkCairo "cairo.h"
            ]
     }
in

let pycairo =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.python2Package { name = "pycairo", version = v } ⫽
      { pkgUrl = "https://github.com/pygobject/pycairo/releases/download/v${versionString}/pycairo-${versionString}.tar.gz"
      , pkgDeps = [ prelude.unbounded "cairo" ]
      }
in

let libnettle =
  λ(v : List Natural) →
    prelude.simplePackage { name = "nettle", version = v } ⫽
      { pkgUrl = "https://ftp.gnu.org/gnu/nettle/nettle-${prelude.showVersion v}.tar.gz"
      , pkgBuildDeps = [ prelude.unbounded "m4" ]
      }
in

let diffutils =
  λ(v : List Natural) →
    prelude.simplePackage { name = "diffutils", version = v } ⫽
      { pkgUrl = "https://ftp.gnu.org/gnu/diffutils/diffutils-${prelude.showVersion v}.tar.xz"
      , installCommand = prelude.installWithBinaries [ "bin/diff" ]
      }
in

let patch =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "patch", version = v }
in

let m4 =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "m4", version = v } ⫽
      { configureCommand =
          λ(cfg : types.BuildVars) →
            [ prelude.patch (./patches/m4.patch as Text) ]
              # prelude.defaultConfigure cfg
      , pkgBuildDeps = [ prelude.unbounded "patch" ]
      }
in

let nginx =
  λ(v : List Natural) →
    prelude.simplePackage { name = "nginx", version = v } ⫽
      { pkgUrl = "http://nginx.org/download/nginx-${prelude.showVersion v}.tar.gz"
      , pkgDeps = [ prelude.unbounded "zlib", prelude.unbounded "pcre2" ]
      }
in

let openssl =
  let opensslCfgVars =
    λ(cfg : types.BuildVars) →
      Some (prelude.mkCCVar cfg # prelude.configEnv ([] : List Text) cfg)
  in

  λ(v : List Natural) →
    prelude.simplePackage { name = "openssl", version = v } ⫽
      { pkgUrl = "https://www.openssl.org/source/openssl-${prelude.showVersion v}a.tar.gz"
      , configureCommand =
          λ(cfg : types.BuildVars) →
            let sharedFlag =
              if cfg.static
                then "no-shared"
                else "shared"
            in
            -- TODO: actually do this sensibly
            let targetMakefile =
              if cfg.isCross
                then "gcc"
                else "linux-x86_64"
            in
            [ prelude.mkExe "Configure"
            , prelude.call (prelude.defaultCall ⫽ { program = "./Configure"
                                                  , arguments = [ "--prefix=${cfg.installDir}", targetMakefile, sharedFlag ] -- FIXME: gcc platform doesn't support shared libraries
                                                  , environment = opensslCfgVars cfg
                                                  })
            ]
      , pkgSubdir = "openssl-${prelude.showVersion v}a"
      , pkgBuildDeps = [ prelude.unbounded "perl" ]
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
      { pkgDeps = [ prelude.unbounded "giflib"
                  , prelude.unbounded "libXaw"
                  , prelude.unbounded "libpng"
                  , prelude.unbounded "libjpeg-turbo"
                  , prelude.unbounded "ncurses"
                  , prelude.unbounded "gtk3"
                  , prelude.unbounded "libotf"
                  , prelude.unbounded "m17n-lib"
                  , prelude.unbounded "gnutls"
                  , prelude.unbounded "libXft"
                  , prelude.unbounded "dbus"
                  , prelude.unbounded "cairo"
                  ]
      , configureCommand = prelude.configureMkExesExtraFlags
          { bins = [ "build-aux/move-if-change", "build-aux/update-subdirs" ]
          , extraFlags = [ "--with-tiff=no"
                         , "--with-libotf"
                         , "--with-m17n-flt"
                         , "--with-gnutls"
                         , "--with-xft"
                         , "--with-dbus"
                         , "--with-cairo=yes"
                         ]
          }
      , installCommand =
          λ(cfg : types.BuildVars) →
            if cfg.static
              then prelude.installWithBinaries [ "bin/emacs" ] cfg
              else prelude.installWithWrappers [ "emacs" ] cfg
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
      , installCommand = prelude.installWithBinaries [ "bin/autoconf", "bin/autoheader", "bin/autom4te", "bin/autoreconf" ]
      }
in

let python =
  λ(v : List Natural) →
    let major = Optional/fold Natural (List/head Natural v) Text (Natural/show) ""
    in
    let versionString = prelude.showVersion v
    in
    let pyEnv =
      λ(_ : List Text) →
      λ(cfg : types.BuildVars) →
        Some (prelude.configEnv ([] : List Text) cfg
          # [ { var = "CONFIG_SITE", value = "config.site" } ])
    in

    prelude.simplePackage { name = "python${major}", version = v } ⫽
      { pkgUrl = "https://www.python.org/ftp/python/${versionString}/Python-${versionString}.tar.xz"
      , pkgSubdir = "Python-${versionString}"
      , configureCommand =
        λ(cfg : types.BuildVars) →
          let config =
            ''
            ac_cv_file__dev_ptmx=yes
            ac_cv_file__dev_ptc=no
            ''
          in
          let staticFlag =
            if cfg.static
              then [] : List Text
              else [ "--enable-shared" ]
          in
          let crossArgs =
            if cfg.isCross
              then ["--disable-ipv6"]
              else [] : List Text
          in
          [ prelude.writeFile { file = "config.site", contents = config } ]
            # prelude.generalConfigure pyEnv "configure" ([] : List Text)
                ([ "--build=${prelude.printArch cfg.buildArch}" ] # crossArgs # staticFlag) cfg
          -- disable ipv6 for cross-compiling
          -- "--enable-optimizations" (takes forever)
      , pkgDeps = [ prelude.unbounded "libffi" ]
      , installCommand = prelude.installWithBinaries [ "bin/python${major}" ]
      -- , installCommand =
          -- prelude.installWithWrappers [ "python${major}" ]
      }
in

let lua =
  λ(v : List Natural) →
    let printLuaOS =
      λ(os : types.OS) →
        merge
          { FreeBSD   = "freebsd"
          , OpenBSD   = "bsd"
          , NetBSD    = "bsd"
          , Solaris   = "solaris"
          , Dragonfly = "bsd"
          , Linux     = "linux"
          , Darwin    = "macosx"
          , Windows   = "mingw"
          , Redox     = "generic"
          , Haiku     = "generic"
          , IOS       = "generic"
          , AIX       = "generic"
          , Hurd      = "generic"
          , Android   = "generic"
          , NoOs      = "c89"
          }
          os
    in

    let luaBuild =
      λ(cfg : types.BuildVars) →
        let cc = prelude.mkCCArg cfg
        in

        let ldflags =
          (prelude.mkLDFlags cfg.linkDirs).value
        in

        let cflags =
          (prelude.mkCFlags cfg).value
        in

        let os =
          prelude.osCfg cfg
        in

        [ prelude.call (prelude.defaultCall ⫽ { program = "make"
                                              , arguments = cc # [ printLuaOS os, "MYLDFLAGS=${ldflags}", "MYCFLAGS=${cflags}", "MYLIBS=-lncurses", "-j${Natural/show cfg.cpus}" ]
                                              })
        ]
    in

    let luaInstall =
      λ(cfg : types.BuildVars) →
        [ prelude.call (prelude.defaultCall ⫽ { program = "make"
                                              , arguments = [ "install", "INSTALL_TOP=${cfg.installDir}" ]
                                              })
        ]
          # prelude.symlinkBinaries [ "bin/lua", "bin/luac" ]
    in

    prelude.simplePackage { name = "lua", version = v } ⫽
      { pkgUrl = "http://www.lua.org/ftp/lua-${prelude.showVersion v}.tar.gz"
      , configureCommand = prelude.doNothing
      , buildCommand = luaBuild
      , installCommand = luaInstall
      , pkgDeps = [ prelude.unbounded "readline"
                  , prelude.unbounded "ncurses"
                  ]
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
      , pkgDeps = [ prelude.lowerBound { name = "libffi", lower = [3,0,0] }
                  , prelude.unbounded "libtasn1"
                  ]
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
      , configureCommand = prelude.configureWithFlags [ "--with-internal-glib" ]
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
      { pkgUrl = "https://ftp.gnu.org/gnu/readline/readline-${prelude.showVersion v}.tar.gz" } -- TODO: should this depend on ncurses?
in

let pixman =
  λ(v : List Natural) →
    prelude.simplePackage { name = "pixman", version = v } ⫽
      { pkgUrl = "https://www.cairographics.org/releases/pixman-${prelude.showVersion v}.tar.gz"
      , pkgDeps = [ prelude.unbounded "libpng" ]
      }
in

let freetype-shared =
  λ(x : { name : Text, version : List Natural }) →
    let versionString = prelude.showVersion x.version
    in

    prelude.simplePackage x ⫽
      { pkgUrl = "https://download.savannah.gnu.org/releases/freetype/freetype-${versionString}.tar.gz"
      , configureCommand = prelude.configureMkExes [ "builds/unix/configure" ]
      , pkgSubdir = "freetype-${versionString}"
      , pkgBuildDeps = [ prelude.unbounded "sed" ]
      , installCommand =
          λ(cfg : types.BuildVars) →
            prelude.defaultInstall cfg
              # [ prelude.symlink "include/freetype2/ft2build.h" "include/ft2build.h"
                , prelude.symlink "include/freetype2/freetype" "include/freetype"
                ]
      }
in

let freetype-prebuild =
  λ(v : List Natural) →
    freetype-shared { name = "freetype-prebuild", version = v } ⫽
      { pkgDeps = [ prelude.unbounded "zlib"
                  , prelude.unbounded "libpng"
                  ]
      , pkgBuildDeps = [ prelude.unbounded "pkg-config" ]
      }
in

let freetype =
  λ(v : List Natural) →
    freetype-shared { name = "freetype", version = v } ⫽
      { pkgDeps = [ prelude.unbounded "zlib"
                  , prelude.unbounded "harfbuzz"
                  , prelude.unbounded "libpng"
                  ]
      , configureCommand = prelude.configureMkExesExtraFlags { bins = [ "builds/unix/configure" ]
                                                             , extraFlags = [ "--enable-freetype-config" ]
                                                             }
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

let imageMagick =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v
    in
    let major = Optional/fold Natural (List/head Natural v) Text (Natural/show) ""
    in

    prelude.simplePackage { name = "imagemagick", version = v } ⫽
      { pkgUrl = "https://imagemagick.org/download/ImageMagick-${versionString}-21.tar.xz"
      , pkgSubdir = "ImageMagick-${versionString}-21"
      , installCommand =
          λ(cfg : types.BuildVars) →
            prelude.defaultInstall cfg
              # [ prelude.symlink "include/ImageMagick-${major}/MagickWand" "include/wand" ]
      }
in

let gtk2 =
  let gtkEnv =
    λ(cfg : types.BuildVars) →
      prelude.defaultPath cfg # [ { var = "LDFLAGS", value = (prelude.mkLDFlags cfg.linkDirs).value ++ " -lpcre -lfribidi" }
                                , prelude.mkCFlags cfg
                                , prelude.mkPkgConfigVar cfg.linkDirs
                                , prelude.libPath cfg
                                , prelude.mkLDRunPath cfg.linkDirs
                                , prelude.mkXdgDataDirs cfg.shareDirs
                                , prelude.mkLDPreload cfg.preloadLibs
                                ]
  in
  let gtkConfig =
    λ(cfg : types.BuildVars) →
      [ prelude.mkExe "configure"
      , prelude.call (prelude.defaultCall ⫽ { program = "./configure"
                                            , arguments = [ "--prefix=${cfg.installDir}" ]
                                            , environment = Some (gtkEnv cfg)
                                            })
      ]
  in

  λ(x : { version : List Natural, patch : Natural }) →
    let versionString = prelude.showVersion x.version
    in
    let fullVersion = versionString ++ "." ++ Natural/show x.patch
    in

    prelude.simplePackage { name = "gtk2", version = prelude.fullVersion x } ⫽
      { pkgUrl = "http://ftp.gnome.org/pub/gnome/sources/gtk+/${versionString}/gtk+-${fullVersion}.tar.xz"
      , pkgSubdir = "gtk+-${fullVersion}"
      , pkgDeps = [ prelude.lowerBound { name = "cairo", lower = [1,6] }
                  , prelude.lowerBound { name = "pango", lower = [1,20] }
                  , prelude.lowerBound { name = "atk", lower = [1,29,2] }
                  , prelude.lowerBound { name = "glib", lower = [2,28,0] }
                  , prelude.lowerBound { name = "gdk-pixbuf", lower = [2,38,0] }
                  ]
      , buildCommand =
          λ(cfg : types.BuildVars) →
            prelude.buildWith (gtkEnv cfg) cfg
      , configureCommand = gtkConfig
      , installCommand =
          λ(cfg : types.BuildVars) →
            prelude.defaultInstall cfg #
              [ prelude.symlink "include/gdk-pixbuf-2.0/gdk-pixbuf" "include/gdk-pixbuf" ]
      }
in

let mkXProto =
  λ(name : Text) →
  λ(v : List Natural) →
    prelude.simplePackage { name = name, version = v } ⫽
      { pkgUrl = "https://www.x.org/releases/individual/proto/${name}-${prelude.showVersion v}.tar.bz2" }
in

let mkXProtoWithPatch =
  λ(name : Text) →
  λ(patch : Text) →
  λ(v : List Natural) →
    mkXProto name v ⫽
      { configureCommand =
          λ(cfg : types.BuildVars) →
            [ prelude.patch patch ]
              # prelude.defaultConfigure cfg
      , pkgBuildDeps = [ prelude.unbounded "patch" ]
      }
in

let xproto =
  mkXProtoWithPatch "xproto" (./patches/xproto.patch as Text)
in

let renderproto =
  mkXProtoWithPatch "renderproto" (./patches/renderproto.patch as Text)
in

let randrproto =
  mkXProto "randrproto"
in

let scrnsaverproto =
  mkXProtoWithPatch "scrnsaverproto" (./patches/scrnsaverproto.patch as Text)
in

let recordproto =
  mkXProto "recordproto"
in

let xf86vidmodeproto =
  mkXProto "xf86vidmodeproto"
in

let glproto =
  mkXProto "glproto"
in

let dri2proto =
  mkXProto "dri2proto"
in

let pango =
  λ(x : { version : List Natural, patch : Natural }) →
    let versionString = prelude.showVersion x.version
    in
    let fullVersion = versionString ++ "." ++ Natural/show x.patch
    in

    let pangoCfgFile =
      ''
      option('enable_docs',
            description: 'Build API reference for Pango using GTK-Doc',
            type: 'boolean',
            value: false)
      option('gir',
            description: 'Build the GObject introspection data for Pango',
            type: 'boolean',
            value: false)
      ''
    in
    let no_gir =
      λ(cfg : types.BuildVars) →
        if cfg.isCross
          then
            [ prelude.writeFile { file = "meson_options.txt", contents = pangoCfgFile } ]
          else
            ([] : List types.Command)
    in

    prelude.simplePackage { name = "pango", version = prelude.fullVersion x } ⫽
      { pkgUrl = "http://ftp.gnome.org/pub/GNOME/sources/pango/${versionString}/pango-${fullVersion}.tar.xz"
      , configureCommand =
          λ(cfg : types.BuildVars) →
            no_gir cfg
              # prelude.mesonConfigure cfg
      , buildCommand = prelude.ninjaBuild
      , installCommand =
          λ(cfg : types.BuildVars) →
            prelude.ninjaInstallWithPkgConfig (prelude.mesonMoves [ "pango.pc", "pangocairo.pc", "pangoft2.pc" ]) cfg
              # [ prelude.symlink "include/pango-1.0/pango" "include/pango" ]
      , pkgBuildDeps = [ prelude.lowerBound { name = "meson", lower = [0,48,0] }
                       , prelude.unbounded "gobject-introspection"
                       ]
      , pkgDeps = [ prelude.lowerBound { name = "fontconfig", lower = [2,11,91] }
                  , prelude.lowerBound { name = "cairo", lower = [1,12,10] }
                  , prelude.lowerBound { name = "fribidi", lower = [0,19,7] }
                  , prelude.lowerBound { name = "harfbuzz", lower = [1,4,2] }
                  , prelude.unbounded "libXrender"
                  , prelude.unbounded "libxcb"
                  ]
      }
in

let libxml2 =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libxml2", version = v } ⫽
     { pkgUrl = "http://xmlsoft.org/sources/libxml2-${prelude.showVersion v}.tar.gz"
     , pkgDeps = [ prelude.unbounded "zlib"
                 , prelude.unbounded "xz"
                 , prelude.unbounded "python2"
                 ]
     }
in

let shared-mime-info =
  λ(v : List Natural) →
    prelude.simplePackage { name = "shared-mime-info", version = v } ⫽
     { pkgUrl = "http://freedesktop.org/~hadess/shared-mime-info-${prelude.showVersion v}.tar.xz"
     , buildCommand =
        λ(cfg : types.BuildVars) →
          [ prelude.call (prelude.defaultCall ⫽ { program = prelude.makeExe cfg.buildOS
                                                , environment = Some (prelude.defaultPath cfg # [ prelude.libPath cfg
                                                                                                , prelude.mkLDRunPath cfg.linkDirs
                                                                                                , prelude.mkPerlLib { libDirs = cfg.linkDirs, perlVersion = [5,28,1], cfg = cfg }
                                                                                                ])
                                                })
          ]
     , installCommand =
        λ(cfg : types.BuildVars) →
          prelude.defaultInstall cfg
            # [ prelude.symlink "share/pkgconfig" "lib/pkgconfig" ]
     , pkgDeps = [ prelude.unbounded "glib"
                 , prelude.unbounded "libxml2"
                 ]
     , pkgBuildDeps = [ prelude.lowerBound { name = "intltool", lower = [0,35,0] }
                      , prelude.unbounded "sed"
                      , prelude.unbounded "gettext"
                      ]
     }
in

let intltool =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "intltool", version = v } ⫽
      { pkgUrl = "https://launchpad.net/intltool/trunk/${versionString}/+download/intltool-${versionString}.tar.gz"
      , configureCommand =
          λ(cfg : types.BuildVars) →
            [ prelude.mkExe "configure"
            , prelude.call (prelude.defaultCall ⫽ { program = "./configure"
                                                  , arguments = [ "--prefix=${cfg.installDir}" ]
                                                  , environment = Some (prelude.defaultPath cfg
                                                      # [ prelude.mkPerlLib { libDirs = cfg.linkDirs, perlVersion = [5,28,1], cfg = cfg } ])
                                                  })
            ]
    , pkgDeps = [ prelude.unbounded "XML-Parser" ]
    , pkgBuildDeps = [ prelude.upperBound { name = "perl", upper = [5,30] } ] -- lower bound: 5.8.1
    }
in

let gdk-pixbuf =
  λ(x : { version : List Natural, patch : Natural }) →
    let versionString = prelude.showVersion x.version
    in
    let fullVersion = versionString ++ "." ++ Natural/show x.patch
    in

    let gdkInstall =
      λ(fs : List { src : Text, dest : Text }) →
      λ(cfg : types.BuildVars) →
        [ prelude.call (prelude.defaultCall ⫽ { program = "ninja"
                                              , environment = Some [ prelude.mkPkgConfigVar cfg.linkDirs
                                                                   , { var = "PATH", value = prelude.mkPathVar cfg.binDirs ++ ":${cfg.currentDir}/gdk-pixbuf-${fullVersion}/build/gdk-pixbuf:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" }
                                                                   , prelude.mkPy3Path cfg.linkDirs
                                                                   , prelude.libPath cfg
                                                                   , prelude.mkLDRunPath cfg.linkDirs
                                                                   , prelude.mkLDFlags cfg.linkDirs
                                                                   , prelude.mkCFlags cfg
                                                                   ]
                                              , arguments = [ "install" ]
                                              , procDir = Some "build"
                                              })
        , prelude.symlink "include/gdk-pixbuf-2.0/gdk-pixbuf" "include/gdk-pixbuf"
        ] # prelude.copyFiles fs
    in

    prelude.simplePackage { name = "gdk-pixbuf", version = prelude.fullVersion x } ⫽
      { pkgUrl = "http://ftp.gnome.org/pub/GNOME/sources/gdk-pixbuf/${versionString}/gdk-pixbuf-${fullVersion}.tar.xz"
      , configureCommand = prelude.mesonConfigure
      , buildCommand = prelude.ninjaBuild
      , installCommand =
          gdkInstall (prelude.mesonMoves [ "gdk-pixbuf-2.0.pc" ])
      , pkgDeps = [ prelude.unbounded "glib"
                  , prelude.unbounded "libjpeg-turbo"
                  , prelude.unbounded "libpng"
                  , prelude.unbounded "gobject-introspection"
                  , prelude.unbounded "shared-mime-info"
                  , prelude.unbounded "libX11"
                  ]
      }
in

let xmlParser =
  λ(v : List Natural) →
    prelude.simplePackage { name = "XML-Parser", version = v } ⫽
     { pkgUrl = "https://cpan.metacpan.org/authors/id/T/TO/TODDR/XML-Parser-${prelude.showVersion v}.tar.gz"
     , configureCommand = prelude.perlConfigure
     , pkgBuildDeps = [ prelude.unbounded "perl" ]
     , pkgDeps = [ prelude.unbounded "expat" ]
     }
in

let meson =
  λ(v : List Natural) →
    prelude.simplePackage { name = "meson", version = v } ⫽
      { pkgUrl = "https://github.com/mesonbuild/meson/archive/${prelude.showVersion v}.tar.gz"
      , configureCommand =
          λ(cfg : types.BuildVars) →
            prelude.python3Install cfg # prelude.mkPy3Wrapper "meson" cfg
      , buildCommand = prelude.doNothing
      , installCommand = prelude.doNothing
      , pkgDeps = [ prelude.unbounded "python3" ]
      }
in

let ninja =
  let ninjaConfigure =
    λ(cfg : types.BuildVars) →
      [ prelude.mkExe "configure.py"
      , prelude.mkExe "src/inline.sh"
      , prelude.call (prelude.defaultCall ⫽ { program = "./configure.py"
                                            , arguments = [ "--bootstrap" ]
                                            })
      ]
  in

  let ninjaInstall =
    λ(cfg : types.BuildVars) →
      [ prelude.copyFile "ninja" "bin/ninja"
      , prelude.symlinkBinary "bin/ninja"
      ]
  in

  λ(v : List Natural) →
    prelude.simplePackage { name = "ninja", version = v } ⫽
      { pkgUrl = "https://github.com/ninja-build/ninja/archive/v${prelude.showVersion v}.tar.gz"
      , configureCommand = ninjaConfigure
      , buildCommand = prelude.doNothing
      , installCommand = ninjaInstall
      , pkgBuildDeps = [ prelude.unbounded "python2" ]
      }
in

let fontconfig =
  λ(v : List Natural) →
    prelude.simplePackage { name = "fontconfig", version = v } ⫽
      { pkgUrl = "https://www.freedesktop.org/software/fontconfig/release/fontconfig-${prelude.showVersion v}.tar.bz2"
      , pkgDeps = [ prelude.unbounded "freetype"
                  , prelude.unbounded "expat"
                  , prelude.unbounded "util-linux"
                  ]
      , pkgBuildDeps = [ prelude.unbounded "gperf" ]
      }
in

let util-linux =
  λ(x : { version : List Natural, patch : Natural }) →
    let versionString = prelude.showVersion x.version
    in
    let fullVersion = versionString ++ "." ++ Natural/show x.patch
    in

    prelude.simplePackage { name = "util-linux", version = prelude.fullVersion x } ⫽
      { pkgUrl = "https://mirrors.edge.kernel.org/pub/linux/utils/util-linux/v${versionString}/util-linux-${fullVersion}.tar.xz"
      , configureCommand =
        λ(cfg : types.BuildVars) →
          let crossFlags =
            if cfg.isCross
              then [ "--disable-pylibmount"
                   , "--without-tinfo"
                   ]
              else [] : List Text
          in
          prelude.configureWithFlags ([ "--disable-makeinstall-chown", "--disable-bash-completion"] # crossFlags) cfg
      , pkgDeps = [ prelude.unbounded "ncurses" ]
      }
in

let fribidi =
  λ(v : List Natural) →
    prelude.simplePackage { name = "fribidi", version = v } ⫽
      { pkgUrl = "https://github.com/fribidi/fribidi/releases/download/v${prelude.showVersion v}/fribidi-${prelude.showVersion v}.tar.bz2" }
in

let gobject-introspection =
  λ(x : { version : List Natural, patch : Natural }) →
    let versionString = prelude.showVersion x.version
    in
    let fullVersion = versionString ++ "." ++ Natural/show x.patch
    in

    prelude.simplePackage { name = "gobject-introspection", version = prelude.fullVersion x } ⫽
      { pkgUrl = "https://download.gnome.org/sources/gobject-introspection/${versionString}/gobject-introspection-${fullVersion}.tar.xz"
      , pkgBuildDeps = [ prelude.unbounded "flex" ]
      , configureCommand = prelude.configureLinkExtraLibs [ "pcre", "gobject-2.0", "gio-2.0" ]
      , pkgDeps = [ prelude.lowerBound { name = "glib", lower = [2,58,0] } ]
      }
in

let flex =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v
    in
    let flexEnv =
      λ(_ : List Text) →
      λ(cfg : types.BuildVars) →
        Some (prelude.configEnv ([] : List Text) cfg
          # [ { var = "YFLAGS", value = "-Wno-error=yacc" } ])
    in

    prelude.simplePackage { name = "flex", version = v } ⫽
      { pkgUrl = "https://github.com/westes/flex/releases/download/v${versionString}/flex-${versionString}.tar.gz"
      , pkgBuildDeps = [ prelude.unbounded "m4"
                       , prelude.unbounded "bison"
                       ]
      , configureCommand = prelude.configWithEnv flexEnv
      }
in

let glib =
  λ(x : { version : List Natural, patch : Natural }) →
    let versionString = prelude.showVersion x.version
    in
    let fullVersion = versionString ++ "." ++ Natural/show x.patch
    in

    let glibConfigure =
      λ(cfg : types.BuildVars) →
        let crossArgs =
          if cfg.isCross
            then [ "--cross-file", "cross.txt" ]
            else [] : List Text
        in

        [ prelude.createDir "build"
        , prelude.writeFile { file = "build/cross.txt", contents = prelude.mesonCfgFile cfg }
        , prelude.call { program = "meson"
                       , arguments = [ "--prefix=${cfg.installDir}", "..", "-Dselinux=disabled" ] # crossArgs
                       , environment = Some [ prelude.mkPkgConfigVar cfg.linkDirs
                                            , { var = "PATH", value = prelude.mkPathVar cfg.binDirs ++ "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" }
                                            , { var = "LDFLAGS", value = (prelude.mkLDFlags cfg.linkDirs).value ++ " -lpcre" }
                                            , prelude.mkPy3Path cfg.linkDirs
                                            , prelude.libPath cfg
                                            , prelude.mkCFlags cfg
                                            , prelude.mkPkgConfigVar cfg.linkDirs
                                            ]
                       , procDir = Some "build"
                       }
        ]

    in

    let symlinkGio =
      λ(h : Text) →
        prelude.symlink "include/glib-2.0/gio/${h}" "include/gio/${h}"
    in

    let symlinkGunix =
      λ(h : Text) →
        prelude.symlink "include/gio-unix-2.0/gio/${h}" "include/gio/${h}"
    in

    prelude.simplePackage { name = "glib", version = prelude.fullVersion x } ⫽
      { pkgUrl = "http://ftp.gnome.org/pub/gnome/sources/glib/${versionString}/glib-${fullVersion}.tar.xz"
      , configureCommand = glibConfigure
      , buildCommand =
        λ(cfg : types.BuildVars) →
          prelude.ninjaBuild cfg
            # prelude.mkExes [ "build/gobject/glib-mkenums"
                             , "build/gobject/glib-genmarshal"
                             , "build/gio/gdbus-2.0/codegen/gdbus-codegen"
                             , "build/glib-gettextize"
                             ]
      , installCommand =
          λ(cfg : types.BuildVars) →
            prelude.ninjaInstallWithPkgConfig (prelude.mesonMoves [ "glib-2.0.pc"
                                                                  , "gobject-2.0.pc"
                                                                  , "gio-2.0.pc"
                                                                  , "gio-unix-2.0.pc" -- TODO: only on unix
                                                                  , "gmodule-no-export-2.0.pc"
                                                                  , "gmodule-export-2.0.pc"
                                                                  , "gmodule-2.0.pc"
                                                                  , "gthread-2.0.pc"
                                                                  ]) cfg
              # [ prelude.symlink "include/glib-2.0/glib" "include/glib"
                , prelude.symlink "include/glib-2.0/gobject" "include/gobject"
                , prelude.symlink "include/glib-2.0/glib.h" "include/glib.h"
                , prelude.symlink "include/glib-2.0/glib-object.h" "include/glib-object.h"
                , prelude.symlink "include/glib-2.0/glib-unix.h" "include/glib-unix.h" -- TODO: only symlink on unix
                , prelude.symlink "include/glib-2.0/gmodule.h" "include/gmodule.h"
                , symlinkGio "gaction.h"
                , symlinkGio "gactiongroup.h"
                , symlinkGio "gactiongroupexporter.h"
                , symlinkGio "gactionmap.h"
                , symlinkGio "gappinfo.h"
                , symlinkGio "gapplication.h"
                , symlinkGio "gapplicationcommandline.h"
                , symlinkGio "gasyncinitable.h"
                , symlinkGio "gasyncresult.h"
                , symlinkGio "gbufferedinputstream.h"
                , symlinkGio "gbufferedoutputstream.h"
                , symlinkGio "gbytesicon.h"
                , symlinkGio "gcancellable.h"
                , symlinkGio "gcharsetconverter.h"
                , symlinkGio "gcontenttype.h"
                , symlinkGio "gconverter.h"
                , symlinkGio "gconverterinputstream.h"
                , symlinkGio "gconverteroutputstream.h"
                , symlinkGio "gcredentials.h"
                , symlinkGio "gdatagrambased.h"
                , symlinkGio "gdatainputstream.h"
                , symlinkGio "gdataoutputstream.h"
                , symlinkGio "gdbusactiongroup.h"
                , symlinkGio "gdbusaddress.h"
                , symlinkGio "gdbusauthobserver.h"
                , symlinkGio "gdbusconnection.h"
                , symlinkGio "gdbuserror.h"
                , symlinkGio "gdbusinterface.h"
                , symlinkGio "gdbusinterfaceskeleton.h"
                , symlinkGio "gdbusintrospection.h"
                , symlinkGio "gdbusmenumodel.h"
                , symlinkGio "gdbusmessage.h"
                , symlinkGio "gdbusmethodinvocation.h"
                , symlinkGio "gdbusnameowning.h"
                , symlinkGio "gdbusnamewatching.h"
                , symlinkGio "gdbusobject.h"
                , symlinkGio "gdbusobjectmanager.h"
                , symlinkGio "gdbusobjectmanagerclient.h"
                , symlinkGio "gdbusobjectmanagerserver.h"
                , symlinkGio "gdbusobjectproxy.h"
                , symlinkGio "gdbusobjectskeleton.h"
                , symlinkGio "gdbusproxy.h"
                , symlinkGio "gdbusserver.h"
                , symlinkGio "gdbusutils.h"
                , symlinkGio "gdrive.h"
                , symlinkGio "gdtlsclientconnection.h"
                , symlinkGio "gdtlsconnection.h"
                , symlinkGio "gdtlsserverconnection.h"
                , symlinkGio "gemblem.h"
                , symlinkGio "gemblemedicon.h"
                , symlinkGio "gfile.h"
                , symlinkGio "gfileattribute.h"
                , symlinkGio "gfileenumerator.h"
                , symlinkGio "gfileicon.h"
                , symlinkGio "gfileinfo.h"
                , symlinkGio "gfileinputstream.h"
                , symlinkGio "gfileiostream.h"
                , symlinkGio "gfilemonitor.h"
                , symlinkGio "gfilenamecompleter.h"
                , symlinkGio "gfileoutputstream.h"
                , symlinkGio "gfilterinputstream.h"
                , symlinkGio "gfilteroutputstream.h"
                , symlinkGio "gicon.h"
                , symlinkGio "ginetaddress.h"
                , symlinkGio "ginetaddressmask.h"
                , symlinkGio "ginetsocketaddress.h"
                , symlinkGio "ginitable.h"
                , symlinkGio "ginputstream.h"
                , symlinkGio "gio-autocleanups.h"
                , symlinkGio "gio.h"
                , symlinkGio "gioenums.h"
                , symlinkGio "gioenumtypes.h"
                , symlinkGio "gioerror.h"
                , symlinkGio "giomodule.h"
                , symlinkGio "gioscheduler.h"
                , symlinkGio "giostream.h"
                , symlinkGio "giotypes.h"
                , symlinkGio "glistmodel.h"
                , symlinkGio "gliststore.h"
                , symlinkGio "gloadableicon.h"
                , symlinkGio "gmemoryinputstream.h"
                , symlinkGio "gmemoryoutputstream.h"
                , symlinkGio "gmenu.h"
                , symlinkGio "gmenuexporter.h"
                , symlinkGio "gmenumodel.h"
                , symlinkGio "gmount.h"
                , symlinkGio "gmountoperation.h"
                , symlinkGio "gnativevolumemonitor.h"
                , symlinkGio "gnetworkaddress.h"
                , symlinkGio "gnetworking.h"
                , symlinkGio "gnetworkmonitor.h"
                , symlinkGio "gnetworkservice.h"
                , symlinkGio "gnotification.h"
                , symlinkGio "goutputstream.h"
                , symlinkGio "gpermission.h"
                , symlinkGio "gpollableinputstream.h"
                , symlinkGio "gpollableoutputstream.h"
                , symlinkGio "gpollableutils.h"
                , symlinkGio "gpropertyaction.h"
                , symlinkGio "gproxy.h"
                , symlinkGio "gproxyaddress.h"
                , symlinkGio "gproxyaddressenumerator.h"
                , symlinkGio "gproxyresolver.h"
                , symlinkGio "gremoteactiongroup.h"
                , symlinkGio "gresolver.h"
                , symlinkGio "gresource.h"
                , symlinkGio "gseekable.h"
                , symlinkGio "gsettings.h"
                , symlinkGio "gsettingsbackend.h"
                , symlinkGio "gsettingsschema.h"
                , symlinkGio "gsimpleaction.h"
                , symlinkGio "gsimpleactiongroup.h"
                , symlinkGio "gsimpleasyncresult.h"
                , symlinkGio "gsimpleiostream.h"
                , symlinkGio "gsimplepermission.h"
                , symlinkGio "gsimpleproxyresolver.h"
                , symlinkGio "gsocket.h"
                , symlinkGio "gsocketaddress.h"
                , symlinkGio "gsocketaddressenumerator.h"
                , symlinkGio "gsocketclient.h"
                , symlinkGio "gsocketconnectable.h"
                , symlinkGio "gsocketconnection.h"
                , symlinkGio "gsocketcontrolmessage.h"
                , symlinkGio "gsocketlistener.h"
                , symlinkGio "gsocketservice.h"
                , symlinkGio "gsrvtarget.h"
                , symlinkGio "gsubprocess.h"
                , symlinkGio "gsubprocesslauncher.h"
                , symlinkGio "gtask.h"
                , symlinkGio "gtcpconnection.h"
                , symlinkGio "gtcpwrapperconnection.h"
                , symlinkGio "gtestdbus.h"
                , symlinkGio "gthemedicon.h"
                , symlinkGio "gthreadedsocketservice.h"
                , symlinkGio "gtlsbackend.h"
                , symlinkGio "gtlscertificate.h"
                , symlinkGio "gtlsclientconnection.h"
                , symlinkGio "gtlsconnection.h"
                , symlinkGio "gtlsdatabase.h"
                , symlinkGio "gtlsfiledatabase.h"
                , symlinkGio "gtlsinteraction.h"
                , symlinkGio "gtlspassword.h"
                , symlinkGio "gtlsserverconnection.h"
                , symlinkGio "gvfs.h"
                , symlinkGio "gvolume.h"
                , symlinkGio "gvolumemonitor.h"
                , symlinkGio "gzlibcompressor.h"
                , symlinkGio "gzlibdecompressor.h"
                -- TODO: only do this on unix
                , symlinkGunix "gdesktopappinfo.h"
                , symlinkGunix "gfiledescriptorbased.h"
                , symlinkGunix "gunixconnection.h"
                , symlinkGunix "gunixcredentialsmessage.h"
                , symlinkGunix "gunixfdlist.h"
                , symlinkGunix "gunixfdmessage.h"
                , symlinkGunix "gunixinputstream.h"
                , symlinkGunix "gunixmounts.h"
                , symlinkGunix "gunixoutputstream.h"
                , symlinkGunix "gunixsocketaddress.h"
                ]

      , pkgBuildDeps = [ prelude.unbounded "meson"
                       , prelude.unbounded "ninja"
                       ]
      , pkgDeps = [ prelude.unbounded "util-linux"
                  , prelude.unbounded "pcre" -- >= 8.31
                  , prelude.unbounded "libffi"
                  , prelude.unbounded "zlib"
                  , prelude.unbounded "dbus"
                  ]
      }
in

let atk =
  λ(x : { version : List Natural, patch : Natural }) →
    let versionString = prelude.showVersion x.version
    in
    let fullVersion = versionString ++ "." ++ Natural/show x.patch
    in

    prelude.ninjaPackage { name = "atk", version = prelude.fullVersion x } ⫽
      { pkgUrl = "https://ftp.gnome.org/pub/gnome/sources/atk/${versionString}/atk-${fullVersion}.tar.xz"
      , pkgBuildDeps = [ prelude.unbounded "gobject-introspection"
                       , prelude.unbounded "gettext"
                       ] -- TODO: disable introspection?
      , pkgDeps = [ prelude.unbounded "glib" ]
      , installCommand =
          prelude.ninjaInstallWithPkgConfig [{ src = "build/atk.pc", dest = "lib/pkgconfig/atk.pc" }]
      }
in

let re2c =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v
    in

    prelude.simplePackage { name = "re2c", version = v } ⫽
      { pkgUrl = "https://github.com/skvadrik/re2c/releases/download/${versionString}/re2c-${versionString}.tar.gz" }
in

let chickenScheme =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v
    in

    let printChickenOS =
      λ(os : types.OS) →
        merge
          { FreeBSD   = "bsd"
          , OpenBSD   = "bsd"
          , NetBSD    = "bsd"
          , Solaris   = "solaris"
          , Dragonfly = "bsd"
          , Linux     = "linux"
          , Darwin    = "macosx"
          , Windows   = "mingw"
          , Haiku     = "haiku"
          , IOS       = "ios"
          , AIX       = "aix"
          , Hurd      = "hurd"
          , Android   = "android"
          , Redox     = "error: no port for Redox OS"
          , NoOs      = "error: no port for no OS"
          }
          os
    in

    let chickenBuild =
      λ(cfg : types.BuildVars) →
        let cc =
          Optional/fold types.TargetTriple cfg.targetTriple (List Text)
            (λ(tgt : types.TargetTriple) → [ "C_COMPILER=${prelude.printTargetTriple tgt}-gcc" ])
              ([] : List Text)
        in
        let os =
          prelude.osCfg cfg
        in
        [ prelude.call (prelude.defaultCall ⫽ { program = prelude.makeExe cfg.buildOS
                                              , arguments = cc # [ "PLATFORM=${printChickenOS os}", "PREFIX=${cfg.installDir}" ]
                                              })
        , prelude.call (prelude.defaultCall ⫽ { program = prelude.makeExe cfg.buildOS
                                              , arguments = cc # [ "PLATFORM=${printChickenOS os}", "PREFIX=${cfg.installDir}", "install" ]
                                              })
        ]
          # prelude.symlinkBinaries [ "bin/csc", "bin/chicken-install", "bin/csi" ]
    in

    prelude.simplePackage { name = "chicken-scheme", version = v } ⫽
      { pkgUrl = "https://code.call-cc.org/releases/${versionString}/chicken-${versionString}.tar.gz"
      , configureCommand = prelude.doNothing
      , buildCommand = chickenBuild
      , installCommand = prelude.doNothing
      , pkgSubdir = "chicken-${versionString}"
      }
in

let xcb-proto =
  λ(v : List Natural) →
    prelude.simplePackage { name = "xcb-proto", version = v } ⫽
      { pkgUrl = "https://xorg.freedesktop.org/archive/individual/xcb/xcb-proto-${prelude.showVersion v}.tar.bz2" }
in

let libxcb =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libxcb", version = v } ⫽
      { pkgUrl = "https://xorg.freedesktop.org/archive/individual/xcb/libxcb-${prelude.showVersion v}.tar.bz2"
      , pkgDeps = [ prelude.lowerBound { name = "xcb-proto", lower = [1,13] }
                  , prelude.unbounded "libXau"
                  , prelude.unbounded "libpthread-stubs"
                  , prelude.unbounded "libXdmcp"
                  ]
      }
in

let libpthread-stubs =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libpthread-stubs", version = v } ⫽
      { pkgUrl = "https://www.x.org/archive/individual/xcb/libpthread-stubs-${prelude.showVersion v}.tar.bz2" }
in

let xorgConfigure =
  prelude.configureWithFlags [ "--disable-malloc0returnsnull" ] -- necessary for cross-compilation
in

let mkXLib =
  λ(name : Text) →
  λ(v : List Natural) →
    prelude.simplePackage { name = name, version = v } ⫽
      { pkgUrl = "https://www.x.org/releases/individual/lib/${name}-${prelude.showVersion v}.tar.bz2"
      , configureCommand = xorgConfigure
      , pkgBuildDeps = [ prelude.unbounded "pkg-config" ]
      }
in

let mkXLibDeps =
  λ(x : { name : Text, deps : List types.Dep }) →
  λ(v : List Natural) →
    mkXLib x.name v ⫽
      { pkgDeps = x.deps }
in

let libXdmcp =
  mkXLibDeps { name = "libXdmcp", deps = [ prelude.unbounded "xproto" ] }
in

let libXau =
  mkXLibDeps { name = "libXau", deps = [ prelude.unbounded "xproto" ] }
in

let mkXUtil =
  λ(name : Text) →
  λ(v : List Natural) →
    prelude.simplePackage { name = name, version = v } ⫽
      { pkgUrl = "https://www.x.org/releases/individual/util/${name}-${prelude.showVersion v}.tar.bz2" }
in

let libXrender =
  mkXLibDeps { name = "libXrender"
             , deps = [ prelude.unbounded "xproto"
                      , prelude.unbounded "renderproto"
                      , prelude.unbounded "libX11"
                      ]
             }
in

let util-macros =
  mkXUtil "util-macros"
in

let libXft =
  mkXLibDeps { name = "libXft"
             , deps = [ prelude.unbounded "freetype"
                      , prelude.unbounded "fontconfig"
                      , prelude.unbounded "libXrender"
                      , prelude.unbounded "libX11"
                      ]
             }
in

let kbproto =
  mkXProto "kbproto"
in

let libX11 =
  mkXLibDeps { name = "libX11"
             , deps = [ prelude.unbounded "libxcb"
                      , prelude.unbounded "kbproto"
                      , prelude.unbounded "xextproto"
                      , prelude.unbounded "inputproto"
                      , prelude.unbounded "xtrans"
                      ]
             }
in

let inputproto =
  mkXProto "inputproto"
in

let xineramaproto =
  mkXProtoWithPatch "xineramaproto" (./patches/xineramaproto.patch as Text)
in

let xtrans =
  mkXLib "xtrans"
in

let libXrandr =
  mkXLibDeps { name = "libXrandr"
             , deps = [ prelude.unbounded "util-macros"
                      , prelude.unbounded "libXext"
                      , prelude.unbounded "libXrender"
                      , prelude.unbounded "libX11"
                      , prelude.unbounded "randrproto"
                      ]
             }
in

let libXinerama =
  mkXLibDeps { name = "libXinerama"
             , deps = [ prelude.unbounded "util-macros"
                      , prelude.unbounded "libX11"
                      , prelude.unbounded "libXext"
                      , prelude.unbounded "xineramaproto"
                      ]
             }
in

let libXext =
  mkXLibDeps { name = "libXext"
             , deps = [ prelude.lowerBound { name = "xextproto", lower = [7,1,99] }
                      , prelude.lowerBound { name = "xproto", lower = [7,0,13] }
                      , prelude.lowerBound { name = "libX11", lower = [1,6] }
                      ]
             }
in

let xextproto =
  mkXProtoWithPatch "xextproto" (./patches/xextproto.patch as Text)
in

let fixesproto =
  mkXProto "fixesproto"
in

let damageproto =
  mkXProto "damageproto"
in

let libXScrnSaver =
  λ(v : List Natural) →
    mkXLib "libXScrnSaver" v ⫽
      { pkgDeps = [ prelude.unbounded "util-macros"
                    , prelude.unbounded "libXext" -- >= 1.2
                    , prelude.unbounded "scrnsaverproto" -- >= 1.2
                  ]
      }
in

let bzip2 =
  let cc = prelude.mkCCArg
  in
  let bzipInstall =
    λ(cfg : types.BuildVars) →
      [ prelude.call (prelude.defaultCall ⫽ { program = prelude.makeExe cfg.buildOS
                                            , arguments = cc cfg # [ "PREFIX=${cfg.installDir}", "install", "-j${Natural/show cfg.cpus}" ]
                                            })
      ]
  in

  λ(v : List Natural) →
    prelude.simplePackage { name = "bzip2", version = v } ⫽
      { pkgUrl = "https://www.sourceware.org/pub/bzip2/bzip2-${prelude.showVersion v}.tar.gz"
      , configureCommand = prelude.doNothing
      , buildCommand = prelude.doNothing
      , installCommand = bzipInstall
      }
in

let expat =
  λ(v : List Natural) →
    prelude.simplePackage { name = "expat", version = v } ⫽
      { pkgUrl = "https://github.com/libexpat/libexpat/releases/download/R_${prelude.underscoreVersion v}/expat-${prelude.showVersion v}.tar.bz2" }
in

let gperf =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "gperf", version = v } ⫽
      { pkgUrl = "http://ftp.gnu.org/pub/gnu/gperf/gperf-${prelude.showVersion v}.tar.gz" }
in

let coreutils =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "coreutils", version = v } ⫽
      { installCommand = prelude.installWithBinaries [ "bin/install", "bin/chmod", "bin/rm", "bin/cp", "bin/ln", "bin/mkdir", "bin/test", "bin/od", "bin/readlink" ] }
in

let libsepol =
  let cc = prelude.mkCCArg
  in
  -- TODO: proper separation
  let sepolInstall =
    λ(cfg : types.BuildVars) →
      [ prelude.call (prelude.defaultCall ⫽ { program = prelude.makeExe cfg.buildOS
                                            , arguments = cc cfg # [ "PREFIX=${cfg.installDir}", "SHLIBDIR=${cfg.installDir}/lib", "CFLAGS=-Wno-error -O2", "install", "-j${Natural/show cfg.cpus}" ]
                                            , environment =
                                                Some (prelude.defaultPath cfg # [ prelude.mkLDFlags cfg.linkDirs, prelude.mkCFlags cfg, prelude.mkPkgConfigVar cfg.linkDirs ])
                                            })
      ]
  in

  λ(v : List Natural) →
    prelude.simplePackage { name = "libsepol", version = v } ⫽
      { pkgUrl = "https://github.com/SELinuxProject/selinux/releases/download/20180524/libsepol-${prelude.showVersion v}.tar.gz"
      , configureCommand = prelude.doNothing
      , buildCommand = prelude.doNothing
      , installCommand = sepolInstall
      , pkgBuildDeps = [ prelude.unbounded "flex" ]
      }
in

let libselinux =
  let cc = prelude.mkCCArg
  in
  -- TODO: proper separation
  let selinuxInstall =
    λ(cfg : types.BuildVars) →
      [ prelude.call (prelude.defaultCall ⫽ { program = prelude.makeExe cfg.buildOS
                                            , arguments = cc cfg
                                                # [ "PREFIX=${cfg.installDir}"
                                                  , "SHLIBDIR=${cfg.installDir}/lib"
                                                  , "EXTRA_CFLAGS=-Wno-error -lpcre " ++ (prelude.mkCFlags cfg).value
                                                  , "install"
                                                  , "-j${Natural/show cfg.cpus}"
                                                  ]
                                            , environment =
                                                Some (prelude.defaultPath cfg # [ prelude.mkLDFlags cfg.linkDirs
                                                                                , prelude.mkCFlags cfg
                                                                                , prelude.mkPkgConfigVar cfg.linkDirs
                                                                                , prelude.libPath cfg
                                                                                ])
                                            })
      ]
  in

  λ(v : List Natural) →
    prelude.simplePackage { name = "libselinux", version = v } ⫽
      { pkgUrl = "https://github.com/SELinuxProject/selinux/releases/download/20180524/libselinux-${prelude.showVersion v}.tar.gz"
      , configureCommand = prelude.doNothing
      , buildCommand = prelude.doNothing
      , installCommand = selinuxInstall
      , pkgDeps = [ prelude.unbounded "pcre"
                  , prelude.unbounded "libsepol"
                  ]
      , pkgBuildDeps = [ prelude.unbounded "pkg-config" ]
      }
in

let libXtst =
  mkXLibDeps { name = "libXtst"
             , deps = [ prelude.unbounded "libXi"
                      , prelude.unbounded "recordproto"
                      ]
             }
in

let libXi =
  mkXLibDeps { name = "libXi"
             , deps = [ prelude.unbounded "libXext" ]
             }
in

let mkGnomeNinja =
  λ(name : Text) →
  λ(x : { version : List Natural, patch : Natural }) →
    let versionString = prelude.showVersion x.version
    in
    let fullVersion = versionString ++ "." ++ Natural/show x.patch
    in
    prelude.ninjaPackage { name = name, version = prelude.fullVersion x } ⫽
      { pkgUrl = "http://ftp.gnome.org/pub/gnome/sources/${name}/${versionString}/${name}-${fullVersion}.tar.xz" }
in

let at-spi-core =
  λ(x : { version : List Natural, patch : Natural }) →
    mkGnomeNinja "at-spi2-core" x ⫽
      { pkgDeps = [ prelude.unbounded "libXtst"
                  , prelude.unbounded "glib"
                  ]
      , installCommand =
          prelude.ninjaInstallWithPkgConfig [{ src = "build/meson-private/atspi-2.pc", dest = "lib/pkgconfig/atspi-2.pc" }]
      }
in

let at-spi-atk =
  λ(x : { version : List Natural, patch : Natural }) →
    mkGnomeNinja "at-spi2-atk" x ⫽
      { pkgDeps = [ prelude.unbounded "at-spi2-core"
                  , prelude.lowerBound { name = "atk", lower = [2,29,2] }
                  , prelude.unbounded "libxml2"
                  ]
      , installCommand =
          prelude.ninjaInstallWithPkgConfig (prelude.mesonMoves [ "atk-bridge-2.0.pc" ])
      }
in

let libdrm =
  λ(v : List Natural) →
    prelude.ninjaPackage { name = "libdrm", version = v } ⫽
      { pkgUrl = "https://dri.freedesktop.org/libdrm/libdrm-${prelude.showVersion v}.tar.bz2"
      , pkgDeps = [ prelude.unbounded "libpciaccess"
                  , prelude.unbounded "cairo"
                  ]
      , installCommand =
          prelude.ninjaInstallWithPkgConfig (prelude.mesonMoves [ "libdrm.pc"
                                                                , "libdrm_amdgpu.pc"
                                                                , "libdrm_intel.pc"
                                                                , "libdrm_nouveau.pc"
                                                                , "libdrm_radeon.pc"
                                                                , "libkms.pc"
                                                                ])
      }
in

let libpciaccess =
  mkXLib "libpciaccess"
in

let markupSafe =
  λ(v : List Natural) →
    prelude.python3Package { name = "MarkupSafe", version = v } ⫽
      { pkgUrl = "https://files.pythonhosted.org/packages/source/M/MarkupSafe/MarkupSafe-${prelude.showVersion v}.tar.gz" }
in

let mako =
  λ(v : List Natural) →
    prelude.python3Package { name = "Mako", version = v } ⫽
      { pkgUrl = "https://files.pythonhosted.org/packages/source/M/Mako/Mako-${prelude.showVersion v}.tar.gz" }
in

let elfutils =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "elfutils", version = v } ⫽
      { pkgUrl = "https://sourceware.org/ftp/elfutils/${versionString}/elfutils-${versionString}.tar.bz2" }
in

let mkGnomeSimple =
  λ(name : Text) →
  λ(x : { version : List Natural, patch : Natural }) →
    let versionString = prelude.showVersion x.version
    in
    let fullVersion = versionString ++ "." ++ Natural/show x.patch
    in
    prelude.simplePackage { name = name, version = prelude.fullVersion x } ⫽
      { pkgUrl = "http://ftp.gnome.org/pub/gnome/sources/${name}/${versionString}/${name}-${fullVersion}.tar.xz" }
in

let gtk3 =
  let mkLDFlagsGtk =
    λ(linkDirs : List Text) →
      concatMapSep " " Text (λ(dir : Text) → "-L${dir}") linkDirs
  in
  let gtkEnv =
    λ(cfg : types.BuildVars) →
      prelude.defaultPath cfg # [ { var = "LDFLAGS", value = (mkLDFlagsGtk cfg.linkDirs) ++ " -lpcre -lfribidi" }
                                , prelude.mkPkgConfigVar cfg.linkDirs
                                , prelude.mkLDPreload cfg.preloadLibs
                                , prelude.mkXdgDataDirs cfg.shareDirs
                                , prelude.mkCFlags cfg
                                ]
  in
  let gtkConfig =
    λ(cfg : types.BuildVars) →
      [ prelude.mkExe "configure"
      , prelude.call (prelude.defaultCall ⫽ { program = "./configure"
                                            , arguments = [ "--prefix=${cfg.installDir}" ]
                                            , environment = Some (gtkEnv cfg)
                                            })
      ]
  in

  λ(x : { version : List Natural, patch : Natural }) →
    mkGnomeSimple "gtk+" x ⫽
      { pkgName = "gtk3"
      , configureCommand = gtkConfig
      , buildCommand =
          λ(cfg : types.BuildVars) →
            prelude.buildWith (gtkEnv cfg) cfg
      , pkgDeps = [ prelude.lowerBound { name = "pango", lower = [1,41,0] }
                  , prelude.unbounded "at-spi2-atk"
                  , prelude.lowerBound { name = "atk", lower = [2,15,1] }
                  , prelude.lowerBound { name = "gdk-pixbuf", lower = [2,30,0] }
                  , prelude.unbounded "libXft"
                  , prelude.lowerBound { name = "libepoxy", lower = [1,4] }
                  , prelude.unbounded "libXi"
                  ]
      }
in

let graphviz =
  λ(v : List Natural) →
    prelude.simplePackage { name = "graphviz", version = v } ⫽
      { pkgUrl = "https://graphviz.gitlab.io/pub/graphviz/stable/SOURCES/graphviz.tar.gz"
      , configureCommand = prelude.configureMkExes [ "iffe" ]
      , pkgDeps = [ prelude.unbounded "perl" ]
      , installCommand = prelude.installWithBinaries [ "bin/dot" ]
      }
in

let wayland =
  λ(v : List Natural) →
    prelude.simplePackage { name = "wayland", version = v } ⫽
      { pkgUrl = "https://wayland.freedesktop.org/releases/wayland-${prelude.showVersion v}.tar.xz"
      , pkgDeps = [ prelude.unbounded "libxml2" ]
      , configureCommand = prelude.configureWithFlags [ "--disable-documentation" ]
      }
in

let swig =
  λ(v : List Natural) →
    prelude.simplePackage { name = "swig", version = v } ⫽
      { pkgUrl = "https://downloads.sourceforge.net/swig/swig-${prelude.showVersion v}.tar.gz"
      , configureCommand = prelude.configureMkExes [ "Tools/config/install-sh" ]
      , installCommand = prelude.installWithBinaries [ "bin/swig" ]
      }
in

let lmdb =

  let cc = prelude.mkCCArg
  in

  let ar =
    λ(cfg : types.BuildVars) →
      Optional/fold types.TargetTriple cfg.targetTriple (List Text)
        (λ(tgt : types.TargetTriple) → ["AR=${prelude.printTargetTriple tgt}-ar"])
          ([] : List Text)
  in

  let lmdbInstall =
    λ(cfg : types.BuildVars) →
      [ prelude.call (prelude.defaultCall ⫽ { program = "make"
                                            , arguments = cc cfg # ar cfg # [ "prefix=${cfg.installDir}", "install", "-j${Natural/show cfg.cpus}" ]
                                            , procDir = Some "libraries/liblmdb" -- TODO: path on windows?
                                            })
      ]
  in

  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "lmdb", version = v } ⫽
      { pkgUrl = "https://github.com/LMDB/lmdb/archive/LMDB_${versionString}.tar.gz"
      , pkgSubdir = "lmdb-LMDB_${versionString}"
      , configureCommand = prelude.doNothing
      , buildCommand = prelude.doNothing
      , installCommand = lmdbInstall
      }
in

let gsl =
  λ(v : List Natural) →
    prelude.simplePackage { name = "gsl", version = v } ⫽
      { pkgUrl = "http://gnu.mirror.constant.com/gsl/gsl-${prelude.showVersion v}.tar.gz" }
in

let postgresql =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "postgresql", version = v } ⫽
      { pkgUrl = "https://ftp.postgresql.org/pub/source/v${versionString}/postgresql-${versionString}.tar.bz2"
      , configureCommand = prelude.configureWithFlags [ "--without-readline" ] -- TODO: set USE_DEV_URANDOM=1 or USE_WIN32_RANDOM=1 on windows
      , installCommand = prelude.installWithBinaries [ "bin/pg_config" ]
      , pkgDeps = [ prelude.unbounded "zlib" ]
      }
in

let sqlite =
  λ(x : { year : Natural, version : List Natural }) →
    let versionString = prelude.squishVersion x.version in
    prelude.simplePackage { name = "sqlite", version = x.version } ⫽
      { pkgUrl = "https://sqlite.org/${Natural/show x.year}/sqlite-autoconf-${versionString}000.tar.gz"
      , pkgSubdir = "sqlite-autoconf-${versionString}000"
      }
in

let ragel =
  λ(v : List Natural) →
    prelude.simplePackage { name = "ragel", version = v } ⫽
      { pkgUrl = "http://www.colm.net/files/ragel/ragel-${prelude.showVersion v}.tar.gz"
      , installCommand = prelude.installWithBinaries [ "bin/ragel" ]
      }
in

let nano =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "nano", version = v } ⫽
      { pkgDeps = [ prelude.unbounded "ncurses" ] }
in

let libarchive =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libarchive", version = v } ⫽
      { pkgUrl = "https://www.libarchive.org/downloads/libarchive-${prelude.showVersion v}.tar.gz"
      , pkgDeps = [ prelude.unbounded "xz"
                  -- , prelude.unbounded "bzip2"
                  , prelude.unbounded "zlib"
                  ]
      }
in

let pygobject =
  λ(x : { version : List Natural, patch : Natural }) →
    mkGnomeSimple "pygobject" x ⫽
      { pkgDeps = [ prelude.unbounded "glib" ]
      , configureCommand = prelude.preloadCfg
      }
in

let pygtk =
  λ(x : { version : List Natural, patch : Natural }) →
    let versionString = prelude.showVersion x.version
    in
    let fullVersion = versionString ++ "." ++ Natural/show x.patch
    in
    mkGnomeSimple "pygtk" x ⫽
      { pkgUrl = "http://ftp.gnome.org/pub/gnome/sources/pygtk/${versionString}/pygtk-${fullVersion}.tar.bz2"
      , configureCommand =
          λ(cfg : types.BuildVars) →
            prelude.mkExes [ "py-compile" ]
              # prelude.preloadCfg cfg
      , pkgDeps = [ prelude.lowerBound { name = "glib", lower = [2,8,0] }
                  , prelude.lowerBound { name = "pygobject", lower = [2,21,3] }
                  , prelude.unbounded "python2"
                  ]
      }
in

let libglade =
  λ(x : { version : List Natural, patch : Natural }) →
    let versionString = prelude.showVersion x.version
    in
    let fullVersion = versionString ++ "." ++ Natural/show x.patch
    in
    prelude.simplePackage { name = "libglade", version = prelude.fullVersion x } ⫽
      { pkgUrl = "http://ftp.gnome.org/pub/gnome/sources/libglade/${versionString}/libglade-${fullVersion}.tar.bz2"
      , pkgDeps = [ prelude.lowerBound { name = "libxml2", lower = [2,4,10] }
                  , prelude.lowerBound { name = "gtk2", lower = [2,5,0] }
                  ]
      , configureCommand = prelude.configureLinkExtraLibs [ "fribidi" ]
      }
in

let scour =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.python3Package { name = "scour", version = v } ⫽
      { pkgUrl = "https://github.com/scour-project/scour/archive/v${versionString}/scour-${versionString}.tar.gz"
      , installCommand = prelude.installWithPy3Wrappers [ "scour" ]
      }
in

let libXpm =
  λ(v : List Natural) →
    mkXLib "libXpm" v ⫽
      { pkgDeps = [ prelude.unbounded "libXext"
                  , prelude.unbounded "libXt"
                  ]
      , pkgBuildDeps = [ prelude.unbounded "gettext" ]
      }
in

let libXt =
  mkXLibDeps { name = "libXt"
             , deps = [ prelude.unbounded "libICE"
                      , prelude.unbounded "libSM"
                      , prelude.unbounded "libX11"
                      , prelude.unbounded "kbproto"
                      ]
             }
in

let libICE =
  mkXLibDeps { name = "libICE"
             , deps = [ prelude.unbounded "xproto"
                      , prelude.unbounded "xtrans"
                      ]
             }
in

let libSM =
  mkXLibDeps { name = "libSM"
             , deps = [ prelude.unbounded "libICE"
                      , prelude.unbounded "util-linux"
                      ]
             }
in

let libXaw =
  mkXLibDeps { name = "libXaw"
             , deps = [ prelude.unbounded "libXmu"
                      , prelude.unbounded "libXpm"
                      ]
             }
in

let libXaw3d =
  mkXLibDeps { name = "libXaw3d"
             , deps = [ prelude.unbounded "libX11"
                      , prelude.unbounded "libXt"
                      , prelude.unbounded "libXmu"
                      , prelude.unbounded "libXext"
                      ]
             }
in

let libXmu =
  mkXLibDeps { name = "libXmu"
             , deps = [ prelude.unbounded "util-macros"
                      , prelude.unbounded "libXt"
                      , prelude.unbounded "libXext"
                      ]
             }
in

let libotf =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libotf", version = v } ⫽
      { pkgUrl = "http://download.savannah.gnu.org/releases/m17n/libotf-${prelude.showVersion v}.tar.gz"
      , pkgDeps = [ prelude.unbounded "freetype" ]
      }
in

let m17n =
  λ(v : List Natural) →
    prelude.simplePackage { name = "m17n-lib", version = v } ⫽
      { pkgUrl = "http://download.savannah.gnu.org/releases/m17n/m17n-lib-${prelude.showVersion v}.tar.gz"
      , buildCommand =
        λ(cfg : types.BuildVars) →
          [ prelude.call (prelude.defaultCall ⫽ { program = prelude.makeExe cfg.buildOS }) ]
      , pkgDeps = [ prelude.unbounded "libXt" ]
      }
in

let mkGimpPackage =
  λ(name : Text) →
  λ(x : { version : List Natural, patch : Natural }) →
    let versionString = prelude.showVersion x.version
    in
    let fullVersion = versionString ++ "." ++ Natural/show x.patch
    in
    prelude.simplePackage { name = name, version = prelude.fullVersion x } ⫽
      { pkgUrl = "https://download.gimp.org/pub/${name}/${versionString}/${name}-${fullVersion}.tar.bz2" }
in

let babl =
  mkGimpPackage "babl"
in

let gegl =
  λ(x : { version : List Natural, patch : Natural }) →
    mkGimpPackage "gegl" x ⫽
      { pkgDeps = [ prelude.lowerBound { name = "babl", lower = [0,1,58] }
                  , prelude.lowerBound { name = "glib", lower = [2,44,0] }
                  , prelude.unbounded "json-glib"
                  ]
      , configureCommand = prelude.preloadCfg
      }
in

let libexif =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "libexif", version = v } ⫽
      { pkgUrl = "https://nchc.dl.sourceforge.net/project/libexif/libexif/${versionString}/libexif-${versionString}.tar.bz2" }
in

let json-glib =
  λ(x : { version : List Natural, patch : Natural }) →
    mkGnomeNinja "json-glib" x ⫽
      { pkgDeps = [ prelude.unbounded "glib"
                  , prelude.unbounded "libjpeg-turbo"
                  , prelude.unbounded "libpng"
                  ]
      , installCommand =
          prelude.ninjaInstallWithPkgConfig (prelude.mesonMoves [ "json-glib-1.0.pc" ])
      }
in

let lcms2 =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "lcms2", version = v } ⫽
      { pkgUrl = "https://github.com/mm2/Little-CMS/archive/lcms${versionString}.tar.gz"
      , pkgSubdir = "Little-CMS-lcms${versionString}"
      }
in

let libtiff =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    -- TODO: use cmake + ninja build system if I ever figure out cross-compilation...
    prelude.simplePackage { name = "libtiff", version = v } ⫽
      { pkgUrl = "http://download.osgeo.org/libtiff/tiff-${versionString}.tar.gz"
      , pkgSubdir = "tiff-${versionString}"
      }
in

let libmypaint =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "libmypaint", version = v } ⫽
      { pkgUrl = "https://github.com/mypaint/libmypaint/releases/download/v${versionString}/libmypaint-${versionString}.tar.xz"
      , pkgDeps = [ prelude.unbounded "json-c" ]
      , pkgBuildDeps = [ prelude.unbounded "intltool" ]
      }
in

let json-c =
  λ(x : { version : List Natural, dateStr : Text }) →
    let versionString = "${prelude.showVersion x.version}-${x.dateStr}"
    in
    prelude.simplePackage { name = "json-c", version = x.version } ⫽
      { pkgUrl = "https://github.com/json-c/json-c/archive/json-c-${versionString}.tar.gz"
      , pkgSubdir = "json-c-json-c-${versionString}"
      }
in

let libopenjpeg =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "libopenjpeg", version = v } ⫽ prelude.cmakePackage ⫽
      { pkgUrl = "https://github.com/uclouvain/openjpeg/archive/v${versionString}.tar.gz"
      , pkgSubdir = "openjpeg-${versionString}"
      , installCommand =
          λ(cfg : types.BuildVars) →
            prelude.cmakeInstall cfg
              # [ prelude.symlink "lib/openjpeg-2.3/OpenJPEGConfig.cmake" "lib/OpenJPEGConfig.cmake" ]
      }
in

let libevent =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "libevent", version = v } ⫽
      { pkgUrl = "https://github.com/libevent/libevent/releases/download/release-${versionString}-stable/libevent-${versionString}-stable.tar.gz"
      , pkgSubdir = "libevent-${versionString}-stable"
      , pkgDeps = [ prelude.unbounded "openssl" ]
      }
in

let memcached =
  λ(v : List Natural) →
    prelude.simplePackage { name = "memcached", version = v } ⫽
      { pkgUrl = "https://memcached.org/files/memcached-${prelude.showVersion v}.tar.gz"
      , pkgDeps = [ prelude.unbounded "libevent" ]
      }
in

let motif =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "motif", version = v } ⫽
      { pkgUrl = "https://iweb.dl.sourceforge.net/project/motif/Motif%20${versionString}%20Source%20Code/motif-${versionString}.tar.gz"
      , pkgDeps = [ prelude.unbounded "libXft"
                  , prelude.unbounded "libpng"
                  , prelude.unbounded "freetype"
                  ]
      }
in

let libjpeg =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "libjpeg", version = v } ⫽
      { pkgUrl = "http://www.ijg.org/files/jpegsrc.v${versionString}c.tar.gz"
      , pkgSubdir = "jpeg-${versionString}c"
      }
in

let feh =
  let cc = prelude.mkCCArg
  let fehMake =
    λ(cfg : types.BuildVars) →
      { program = prelude.makeExe cfg.buildOS }
  let fehBuild =
    λ(v : List Natural) →
    λ(cfg : types.BuildVars) →
      [ prelude.call (prelude.defaultCall ⫽ fehMake cfg ⫽
                        { arguments = [ "feh.1" ]
                        , procDir = Some "man"
                        })
      , prelude.call (prelude.defaultCall ⫽ fehMake cfg ⫽
                        { arguments =
                            cc cfg #
                              [ "CFLAGS=${(prelude.mkCFlags cfg).value} -DPACKAGE=\\\"feh\\\" -DPREFIX=\\\"${cfg.installDir}\\\" -DVERSION=\\\"${prelude.showVersion v}\\\" ${(prelude.mkLDFlags cfg.linkDirs).value}"
                              , "feh"
                              ]
                        , procDir = Some "src"
                        })
      , prelude.call (prelude.defaultCall ⫽ fehMake cfg ⫽
                        { arguments = [ "feh.desktop" ]
                        , procDir = Some "share/applications"
                        })
      ]
  in
  let fehInstall =
    λ(cfg : types.BuildVars) →
      [ prelude.call (prelude.defaultCall ⫽ { program = prelude.makeExe cfg.buildOS
                                            , arguments = [ "CFLAGS=${(prelude.mkCFlags cfg).value}"
                                                          , "-j${Natural/show cfg.cpus}"
                                                          , "PREFIX=${cfg.installDir}"
                                                          , "install"
                                                          ]
                                            })
      ] # prelude.mkLDPathWrapper cfg "feh"
  in

  λ(v : List Natural) →
    prelude.simplePackage { name = "feh", version = v } ⫽
      { pkgUrl = "https://github.com/derf/feh/archive/${prelude.showVersion v}.tar.gz"
      , configureCommand = prelude.doNothing
      , buildCommand = fehBuild v
      , installCommand = fehInstall
      , pkgBuildDeps = [ prelude.unbounded "sed" ]
      , pkgDeps = [ prelude.unbounded "imlib2"
                  , prelude.unbounded "libXt"
                  , prelude.unbounded "libXinerama"
                  , prelude.unbounded "curl"
                  ]
      }
in

let imlib2 =
  λ(v : List Natural) →
    prelude.simplePackage { name = "imlib2", version = v } ⫽
      { pkgUrl = "https://downloads.sourceforge.net/enlightenment/imlib2-${prelude.showVersion v}.tar.bz2"
      , pkgDeps = [ prelude.unbounded "libXext"
                  , prelude.unbounded "freetype"
                  , prelude.unbounded "libjpeg"
                  , prelude.unbounded "libpng"
                  ]
      }
in

let libicu =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libicu", version = v } ⫽
      { pkgUrl = "http://download.icu-project.org/files/icu4c/${prelude.showVersion v}/icu4c-${prelude.underscoreVersion v}-src.tgz"
      , pkgSubdir = "icu/source"
      }
in

let jemalloc =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "jemalloc", version = v } ⫽
      { pkgUrl = "https://github.com/jemalloc/jemalloc/releases/download/${versionString}/jemalloc-${versionString}.tar.bz2"
      , configureCommand = prelude.configureMkExes [ "include/jemalloc/internal/private_symbols.sh"
                                                   , "include/jemalloc/internal/public_namespace.sh"
                                                   , "include/jemalloc/internal/public_unnamespace.sh"
                                                   , "include/jemalloc/jemalloc_rename.sh"
                                                   , "include/jemalloc/jemalloc_mangle.sh"
                                                   , "include/jemalloc/jemalloc.sh"
                                                   ]
      }
in

let gperftools =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "gperftools", version = v } ⫽
      { pkgUrl = "https://github.com/gperftools/gperftools/releases/download/gperftools-${versionString}/gperftools-${versionString}.tar.gz" }
in

let openssh =
  let opensshInstall =
    λ(cfg : types.BuildVars) →
      [ prelude.call (prelude.defaultCall ⫽ { program = prelude.makeExe cfg.buildOS
                                            , arguments =
                                                [ "PRIVSEP_PATH=${cfg.installDir}/var"
                                                , "install"
                                                , "-j${Natural/show cfg.cpus}"
                                                ]
                                            , environment =
                                                Some (prelude.defaultPath cfg # [ prelude.mkPkgConfigVar cfg.linkDirs
                                                                                , prelude.libPath cfg
                                                                                ])
                                            })
      ]
  in
  λ(v : List Natural) →
    prelude.simplePackage { name = "openssh", version = v } ⫽
      { pkgUrl = "https://mirrors.gigenet.com/pub/OpenBSD/OpenSSH/portable/openssh-${prelude.showVersion v}p1.tar.gz"
      , pkgSubdir = "openssh-${prelude.showVersion v}p1"
      , installCommand = opensshInstall
      , pkgDeps = [ prelude.unbounded "openssl" ]
      }
in

let libxslt =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libxslt", version = v } ⫽
      { pkgUrl = "http://xmlsoft.org/sources/libxslt-${prelude.showVersion v}.tar.gz"
      , pkgDeps = [ prelude.unbounded "libxml2" ]
      }
in

let libepoxy =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.ninjaPackage { name = "libepoxy", version = v } ⫽
      { pkgUrl = "https://github.com/anholt/libepoxy/releases/download/${versionString}/libepoxy-${versionString}.tar.xz"
      , pkgDeps = [ prelude.unbounded "mesa" ]
      , installCommand =
          prelude.ninjaInstallWithPkgConfig (prelude.mesonMoves [ "epoxy.pc" ])
      }
in

let mesa =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "mesa", version = v } ⫽
      { pkgUrl = "https://mesa.freedesktop.org/archive/mesa-${prelude.showVersion v}.tar.xz"
      , pkgDeps = [ prelude.lowerBound { name = "libdrm", lower = [2,4,75] }
                  , prelude.unbounded "libXdamage"
                  , prelude.unbounded "libXfixes"
                  , prelude.unbounded "libXxf86vm"
                  , prelude.lowerBound { name = "libxshmfence", lower = [1,1] }
                  , prelude.lowerBound { name = "glproto", lower = [1,4,14] }
                  , prelude.lowerBound { name = "dri2proto", lower = [2,8] }
                  , prelude.unbounded "libXrandr"
                  ]
      , configureCommand = prelude.configureWithFlags [ "--enable-autotools", "--with-gallium-drivers=nouveau,swrast" ] -- disable radeon drivers so we don't need LLVM (8.0.0 won't work?)
      }
in

let libXdamage =
  mkXLibDeps { name = "libXdamage"
             , deps = [ prelude.unbounded "libXfixes"
                      , prelude.unbounded "damageproto"
                      ]
             }
in

let libXfixes =
  mkXLibDeps { name = "libXfixes"
             , deps = [ prelude.unbounded "xproto"
                      , prelude.unbounded "fixesproto"
                      , prelude.unbounded "xextproto"
                      , prelude.unbounded "libX11"
                      ]
             }
in

let libXxf86vm =
  mkXLibDeps { name = "libXxf86vm"
             , deps = [ prelude.unbounded "xproto"
                      , prelude.unbounded "libX11"
                      , prelude.unbounded "xextproto"
                      , prelude.unbounded "libXext"
                      , prelude.unbounded "xf86vidmodeproto"
                      ]
             }
in

let libxshmfence =
  mkXLibDeps { name = "libxshmfence", deps = [ prelude.unbounded "xproto" ] }
in

let gnome-doc-utils =
  λ(x : { version : List Natural, patch : Natural }) →
    mkGnomeSimple "gnome-doc-utils" x ⫽
      { pkgDeps = [ prelude.lowerBound { name = "libxslt", lower = [1,1,8] }
                  , prelude.lowerBound { name = "libxml2", lower = [2,6,12] }
                  ]
      , pkgBuildDeps = [ prelude.lowerBound { name = "intltool", lower = [0,35,0] } ]
      , configureCommand = prelude.configureMkExes [ "py-compile" ]
      }
in

let itstool =
  λ(v : List Natural) →
    prelude.simplePackage { name = "itstool", version = v } ⫽
      { pkgUrl = "http://files.itstool.org/itstool/itstool-${prelude.showVersion v}.tar.bz2"
      , pkgDeps = [ prelude.unbounded "libxml2" ]
      }
in

let gexiv2 =
  λ(x : { version : List Natural, patch : Natural }) →
    mkGnomeNinja "gexiv2" x ⫽
      { pkgDeps = [ prelude.unbounded "exiv2"
                  , prelude.unbounded "glib"
                  ]
      , installCommand =
          prelude.ninjaInstallWithPkgConfig (prelude.mesonMoves [ "gexiv2.pc" ])
      }
in

let exiv2 =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "exiv2", version = v } ⫽ prelude.cmakePackage ⫽
      { pkgUrl = "http://www.exiv2.org/builds/exiv2-${versionString}-Source.tar.gz"
      , pkgSubdir = "exiv2-${versionString}-Source"
      }
in

let libtiff =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.ninjaPackage { name = "libtiff", version = v } ⫽
      { pkgUrl = "https://download.osgeo.org/libtiff/tiff-${versionString}.tar.gz"
      , pkgSubdir = "tiff-${versionString}"
      , configureCommand = prelude.cmakeConfigureNinja
      }
in

let nspr =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v
    in
    let bitFlag =
      λ(cfg : types.BuildVars) →
        if prelude.isX64 (prelude.archCfg cfg)
          then [ "--enable-64bit" ]
          else ([] : List Text)
    in

    prelude.simplePackage { name = "nspr", version = v } ⫽
      { pkgUrl = "https://archive.mozilla.org/pub/nspr/releases/v${versionString}/src/nspr-${versionString}.tar.gz"
      , pkgSubdir = "nspr-${versionString}/nspr"
      , configureCommand =
          λ(cfg : types.BuildVars) →
            prelude.configureWithFlags (bitFlag cfg) cfg
      }
in

let libthai =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libthai", version = v } ⫽
      { pkgUrl = "https://linux.thai.net/pub/thailinux/software/libthai/libthai-${prelude.showVersion v}.tar.xz"
      , pkgDeps = [ prelude.unbounded "libdatrie" ]
      }
in

let libdatrie =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libdatrie", version = v } ⫽
      { pkgUrl = "https://linux.thai.net/pub/thailinux/software/libthai/libdatrie-${prelude.showVersion v}.tar.xz" }
in

let joe =
  λ(v : List Natural) →
    prelude.simplePackage { name = "joe", version = v } ⫽
      { pkgUrl = "https://downloads.sourceforge.net/joe-editor/joe-${prelude.showVersion v}.tar.gz"
      , installCommand = prelude.installWithBinaries [ "bin/joe" ]
      }
in

let fossil =
  λ(v : List Natural) →
    prelude.simplePackage { name = "fossil", version = v } ⫽
      { pkgUrl = "https://fossil-scm.org/fossil/uv/fossil-src-${prelude.showVersion v}.tar.gz"
      , configureCommand = prelude.configureMkExes [ "autosetup/find-tclsh" ]
      , installCommand = prelude.installWithBinaries [ "bin/fossil" ]
      , pkgDeps = [ prelude.unbounded "zlib"
                  , prelude.unbounded "openssl"
                  ]
      }
in

let libcroco =
  λ(x : { version : List Natural, patch : Natural }) →
    mkGnomeSimple "libcroco" x ⫽
      { pkgDeps = [ prelude.lowerBound { name = "glib", lower = [2,0] }
                  , prelude.lowerBound { name = "libxml2", lower = [2,4,23] }
                  ]
      }
in

let libsoup =
  λ(x : { version : List Natural, patch : Natural }) →
    let libsoupCfgFile =
    ''
    option('gssapi',
      type : 'boolean',
      value : true,
      description : 'Build with GSSAPI support'
    )

    option('krb5_config',
      type : 'string',
      description : 'Where to look for krb5-config, path points to krb5-config installation (defaultly looking in PATH)'
    )

    option('ntlm',
      type : 'boolean',
      value : false,
      description : 'Build with NTLM support'
    )

    option('tls_check',
      type : 'boolean',
      value : true,
      description : 'Enable TLS support through glib-networking. If you are building a package, you can disable this to allow building libsoup anyway (since glib-networking is not actually required at compile time), but you should be sure to add a runtime dependency on it.'
    )

    option('gnome',
      type : 'boolean',
      value : true,
      description : 'Build libsoup with GNOME support'
    )

    option('introspection',
      type : 'boolean',
      value : true,
      description : 'Build GObject Introspection data'
    )

    option('vapi',
      type : 'boolean',
      value : false,
      description : 'Build Vala bindings'
    )

    option('doc',
      type: 'boolean',
      value: false,
      description: 'Enable generating the API reference'
    )

    option('tests',
      type: 'boolean',
      value: true,
      description: 'Enable unit tests compilation'
    )
    ''
    in
    mkGnomeNinja "libsoup" x ⫽
      { pkgDeps = [ prelude.unbounded "glib"
                  , prelude.unbounded "sqlite"
                  , prelude.unbounded "libxml2"
                  , prelude.unbounded "libpsl"
                  , prelude.unbounded "krb5"
                  , prelude.unbounded "gobject-introspection"
                  ]
      , pkgBuildDeps = [ prelude.unbounded "vala" ]
      , configureCommand =
          λ(cfg : types.BuildVars) →
            [ prelude.writeFile { file = "meson_options.txt", contents = libsoupCfgFile } ]
              # prelude.mesonConfigure cfg
      , installCommand =
          prelude.ninjaInstallWithPkgConfig (prelude.mesonMoves [ "libsoup-2.4.pc" ])
      }
in

let libpsl =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "libpsl", version = v } ⫽
      { pkgUrl = "https://github.com/rockdaboot/libpsl/releases/download/libpsl-${versionString}/libpsl-${versionString}.tar.gz"
      , configureCommand = prelude.configureMkExes [ "src/psl-make-dafsa" ]
      }
in


let krb5 =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "krb5", version = v } ⫽
      { pkgUrl = "https://kerberos.org/dist/krb5/${versionString}/krb5-${versionString}.tar.gz"
      , pkgSubdir = "krb5-${versionString}/src"
      , configureCommand = prelude.configureMkExes [ "config/move-if-changed", "config/mkinstalldirs" ]
      , pkgBuildDeps = [ prelude.unbounded "bison" ]
      }
in

let vala =
  λ(x : { version : List Natural, patch : Natural }) →
    mkGnomeSimple "vala" x ⫽
      { pkgBuildDeps = [ prelude.unbounded "flex" ]
      , pkgDeps = [ prelude.lowerBound { name = "glib", lower = [2,40,0] }
                  , prelude.lowerBound { name = "graphviz", lower = [2,15] }
                  ]
      , configureCommand = prelude.configureMkExes [ "build-aux/git-version-gen" ]
      }
in

let htop =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "htop", version = v } ⫽
      { pkgUrl = "https://hisham.hm/htop/releases/${versionString}/htop-${versionString}.tar.gz"
      , pkgDeps = [ prelude.unbounded "ncurses" ]
      , pkgBuildDeps = [ prelude.unbounded "python3" ]
      , configureCommand = prelude.configureMkExes [ "scripts/MakeHeader.py" ]
      , installCommand = prelude.installWithBinaries [ "bin/htop" ]
      }
in

let mpfr =
  λ(v : List Natural) →
    prelude.simplePackage { name = "mpfr", version = v } ⫽
      { pkgUrl = "https://ftp.gnu.org/gnu/mpfr/mpfr-${prelude.showVersion v}.tar.xz" }
in

let libsodium =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libsodium", version = v } ⫽
      { pkgUrl = "https://download.libsodium.org/libsodium/releases/libsodium-${prelude.showVersion v}.tar.gz" }
in

let libev =
  λ(v : List Natural) →
    prelude.simplePackage { name = "libev", version = v } ⫽
      { pkgUrl = "http://dist.schmorp.de/libev/Attic/libev-${prelude.showVersion v}.tar.gz" }
in

let ctags =
  λ(v : List Natural) →
    prelude.simplePackage { name = "ctags", version = v } ⫽
      { pkgUrl = "http://prdownloads.sourceforge.net/ctags/ctags-${prelude.showVersion v}.tar.gz"
      , configureCommand = prelude.configureMkExes [ "mkinstalldirs" ]
      , installCommand = prelude.installWithBinaries [ "bin/ctags" ]
      }
in

let tcc =
  λ(v : List Natural) →
    prelude.simplePackage { name = "tcc", version = v } ⫽
      { pkgUrl = "http://download.savannah.gnu.org/releases/tinycc/tcc-${prelude.showVersion v}.tar.bz2"
      , configureCommand = prelude.configureMkExes [ "texi2pod.pl" ]
      , pkgBuildDeps = [ prelude.unbounded "perl"
                       , prelude.unbounded "texinfo"
                       ]
      , installCommand = prelude.installWithBinaries [ "bin/tcc" ]
      }
in

let texinfo =
  λ(v : List Natural) →
    prelude.simplePackage { name = "texinfo", version = v } ⫽
      { pkgUrl = "https://ftp.gnu.org/gnu/texinfo/texinfo-${prelude.showVersion v}.tar.xz" }
in

let node =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "node", version = v } ⫽
      { pkgUrl = "https://nodejs.org/dist/v${versionString}/node-v${versionString}.tar.gz"
      , pkgSubdir = "node-v${versionString}"
      , installCommand =
          λ(cfg : types.BuildVars) →
            prelude.installWithBinaries [ "bin/node", "bin/npm" ] cfg
              # [ prelude.mkExe "${cfg.installDir}/lib/node_modules/npm/bin/npm-cli.js" ]
      }
in

let glu =
  λ(v : List Natural) →
    prelude.simplePackage { name = "glu", version = v } ⫽
      { pkgUrl = "https://mesa.freedesktop.org/archive/glu/glu-${prelude.showVersion v}.tar.gz"
      , pkgDeps = [ prelude.unbounded "mesa" ]
      }
in

let compositeproto =
  mkXProto "compositeproto"
in

let libXcomposite =
  mkXLibDeps { name = "libXcomposite", deps = [ prelude.unbounded "libX11"
                                              , prelude.lowerBound { name = "compositeproto", lower = [0,4] }
                                              , prelude.unbounded "libXfixes"
                                              ]
             }
in

let mosh =
  λ(v : List Natural) →
    prelude.simplePackage { name = "mosh", version = v } ⫽
      { pkgUrl = "https://mosh.org/mosh-${prelude.showVersion v}.tar.gz"
      , pkgBuildDeps = [ prelude.unbounded "protobuf" ]
      , pkgDeps = [ prelude.unbounded "zlib"
                  , prelude.unbounded "protobuf"
                  , prelude.unbounded "openssl"
                  ]
      , installCommand = prelude.installWithBinaries [ "bin/mosh" ]
      }
in

let protobuf =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "protobuf", version = v } ⫽
      { pkgUrl = "https://github.com/protocolbuffers/protobuf/releases/download/v${versionString}/protobuf-cpp-${versionString}.tar.gz"
      , pkgSubdir = "protobuf-${versionString}"
      }
in

let libcds =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "libcds", version = v } ⫽ prelude.cmakePackage ⫽
      { pkgUrl = "https://pilotfiber.dl.sourceforge.net/project/libcds/cds-${versionString}/cds-${versionString}.tar.gz"
      , pkgSubdir = "cds-${versionString}"
      , pkgDeps = [ prelude.unbounded "libboost" ]
      }
in

let libboost =
  -- TODO: use bootstrap.bat on windows
  let boostConfigure =
    λ(cfg : types.BuildVars) →
      [ prelude.call (prelude.defaultCall ⫽ { program = "./bootstrap.sh" }) ]
  in

  let boostInstall =
    λ(cfg : types.BuildVars) →
      [ prelude.call (prelude.defaultCall ⫽ { program = "./b2"
                                            , arguments = [ "install", "--prefix=${cfg.installDir}", "--without-python" ]
                                            })
      ]
  in

  λ(v : List Natural) →
    let versionString = prelude.underscoreVersion v in
    prelude.simplePackage { name = "libboost", version = v } ⫽
      -- TODO: allow pkgUrl to depend on os
      { pkgUrl = "https://dl.bintray.com/boostorg/release/${prelude.showVersion v}/source/boost_${versionString}.tar.bz2"
      , pkgSubdir = "boost_${versionString}"
      , pkgStream = False
      , configureCommand = boostConfigure
      , buildCommand = prelude.doNothing
      , installCommand = boostInstall
      }
in

let llvm =
  λ(v : List Natural) →
    let llvmBuild =
      λ(cfg : types.BuildVars) →
        [ prelude.call { program = "cmake"
                       , arguments = [ "--build", ".", "--config", "Release", "--", "-j", "3" ]
                       , environment = prelude.defaultEnv
                       , procDir = Some "build"
                       }
        ]
    in

    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "llvm", version = v } ⫽ prelude.cmakePackage ⫽
      { pkgUrl = "http://releases.llvm.org/${versionString}/llvm-${versionString}.src.tar.xz"
      , pkgSubdir = "llvm-${versionString}.src"
      , pkgStream = False
      , buildCommand = llvmBuild
      }
in

let pari =
  λ(v : List Natural) →
    prelude.simplePackage { name = "pari", version = v } ⫽
      { pkgUrl = "http://pari.math.u-bordeaux.fr/pub/pari/unix/pari-${prelude.showVersion v}.tar.gz"
      , configureCommand = prelude.generalConfigure prelude.configSome "Configure" ([] : List Text) ([] : List Text)
      , pkgStream = False
      }
in

let mpc =
  λ(v : List Natural) →
    prelude.simplePackage { name = "mpc", version = v } ⫽
      { pkgUrl = "https://ftp.gnu.org/gnu/mpc/mpc-${prelude.showVersion v}.tar.gz"
      , pkgDeps = [ prelude.unbounded "mpfr" ]
      }
in

-- http://www.netgull.com/gcc/releases/gcc-9.1.0/gcc-9.1.0.tar.xz
let gcc =
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "gcc", version = v } ⫽
      { pkgUrl = "http://mirror.linux-ia64.org/gnu/gcc/releases/gcc-${versionString}/gcc-${versionString}.tar.xz"
      , configureCommand =
        λ(cfg : types.BuildVars) →
          [ prelude.call { program = "contrib/download_prerequisites"
                        , arguments = [] : List Text
                        , environment = None (List types.EnvVar)
                        , procDir = None Text
                        }
          ] #
            prelude.configureWithFlags [ "--disable-multilib" ] cfg
      , installCommand = prelude.installWithBinaries [ "bin/gcc", "bin/g++", "bin/gcc-ar", "bin/gcc-nm", "bin/gfortran", "bin/gcc-ranlib" ]
      , pkgBuildDeps = [ prelude.unbounded "curl" ]
      , pkgStream = False
      }
in

let ruby =
  λ(x : { version : List Natural, patch : Natural }) →
    let versionString = prelude.showVersion x.version
    in
    let fullVersion = versionString ++ "." ++ Natural/show x.patch
    in

    prelude.simplePackage { name = "ruby", version = prelude.fullVersion x } ⫽
      { pkgUrl = "https://cache.ruby-lang.org/pub/ruby/${versionString}/ruby-${fullVersion}.tar.gz"
      , installCommand = prelude.installWithBinaries [ "bin/ruby", "bin/gem" ]
      , pkgStream = False
      , pkgDeps = [ prelude.unbounded "readline"
                  , prelude.unbounded "openssl"
                  ]
      }
in

-- https://hub.darcs.net/raichoo/hikari
-- https://versaweb.dl.sourceforge.net/project/schilytools/schily-2019-03-29.tar.bz2
[ autoconf [2,69]
, automake [1,16,1]
, at-spi-atk { version = [2,33], patch = 1 }
, at-spi-core { version = [2,33], patch = 1 }
, atk { version = [2,33], patch = 1 }
, babl { version = [0,1], patch = 60 }
, binutils [2,31]
, bison [3,3]
, bzip2 [1,0,6]
, cairo [1,16,0]
, chickenScheme [5,0,0]
, cmake { version = [3,13], patch = 4 }
, compositeproto [0,4]
, coreutils [8,30]
, ctags [5,8]
, curl [7,63,0]
, damageproto [1,2,1]
, dbus [1,12,10]
, diffutils [3,7]
, dri2proto [2,8]
, elfutils [0,175]
, emacs [26,1]
, exiv2 [0,27,0]
, expat [2,2,6]
, feh [3,1,1]
, fixesproto [5,0]
, fontconfig [2,13,1]
, fossil [2,7]
, flex [2,6,3] -- 2.6.4?
, fltk { version = [1,3,4], patch = 2 }
, freetype-prebuild [2,9,1] -- TODO: force both to have same version?
, freetype [2,9,1]
, fribidi [1,0,5]
, gawk [4,2,1]
, gc [8,0,4]
, gcc [9,1,0]
, gdb [8,2]
, gdk-pixbuf { version = [2,38], patch = 1 }
, gegl { version = [0,4], patch = 12 }
, gettext [0,19,8]
, gexiv2 { version = [0,11], patch = 0 }
, gperf [3,1]
, gperftools [2,7]
, giflib [5,1,4]
, git [2,19,2]
, glib { version = [2,60], patch = 3 } -- TODO: bump to 2.59.0 once gobject-introspection supports it
, glproto [1,4,17]
, glu [9,0,0]
, json-glib { version = [1,4], patch = 4 }
, glibc [2,28]
, gmp [6,1,2]
, gobject-introspection { version = [1,60], patch = 1 }
, gnome-doc-utils { version = [0,20], patch = 10 }
, gnupg [2,2,16]
, gnutls { version = [3,6], patch = 7 }
, graphviz [2,40,1]
, gsl [2,5]
, gtk2 { version = [2,24], patch = 32 }
, gtk3 { version = [3,24], patch = 8 }
, gzip [1,9]
, harfbuzz [2,5,0]
, htop [2,2,0]
, imageMagick [7,0,8]
, imlib2 [1,5,1]
, inputproto [2,3,2]
, intltool [0,51,0]
, itstool [2,0,5]
, jemalloc [5,2,0]
, joe [4,6]
, json-c { version = [0,13,1], dateStr = "20180305" }
, kbproto [1,0,7]
, krb5 [1,17]
, lapack [3,8,0]
, lcms2 [2,9]
, libarchive [3,3,3]
, libassuan [2,5,2]
, libatomic_ops [7,6,10]
, libboost [1,69,0]
, libcds [2,3,2]
, libcroco { version = [0,6], patch = 12 }
, libdatrie [0,2,12]
, libdrm [2,4,96]
, libepoxy [1,5,3]
, libev [4,25]
, libevent [2,1,10]
, libexif [0,6,21]
, libffi [3,2,1]
, libgcrypt [1,8,4]
, libglade { version = [2,6], patch = 4 }
, libgpgError [1,33]
, libICE [1,0,9]
, libicu [63,1]
, libjpeg [9]
, libjpeg-turbo [2,0,2]
, libksba [1,3,5]
, libmypaint [1,3,0]
, libnettle [3,4,1]
, libpciaccess [0,14]
, libpng [1,6,35]
, libpsl [0,21,0]
, libpthread-stubs [0,4]
, libopenjpeg [2,3,1]
, libotf [0,9,16]
, libselinux [2,8]
, libsepol [2,8]
, libsodium [1,0,17]
, libsoup { version = [2,67], patch = 1 }
, libssh2 [1,8,0]
, libtasn1 [4,13]
, libtiff [4,0,10]
, libtool [2,4,6]
, libuv [1,24,0]
, libSM [1,2,3]
, libthai [0,1,28]
, libX11 [1,6,7]
, libXau [1,0,8]
, libXaw [1,0,13]
, libXaw3d [1,6,3]
, libxcb [1,13]
, libXcomposite [0,4,4]
, libXdamage [1,1,4]
, libXdmcp [1,1,2]
, libXext [1,3,3]
, libXfixes [5,0,3]
, libXft [2,3,2]
, libXi [1,7]
, libXinerama [1,1,4]
, libxml2 [2,9,8]
, libXmu [1,1,2]
, libXpm [3,5,12]
, libXScrnSaver [1,2,3]
, libxshmfence [1,3]
, libxslt [1,1,33]
, libXrandr [1,5,1]
, libXrender [0,9,10]
, libXt [1,1,5]
, libXtst [1,2,3]
, libXxf86vm [1,1,4]
, llvm [8,0,0]
, lmdb [0,9,23]
, lua [5,3,5]
, m17n [1,8,0]
, m4 [1,4,18]
, mako [1,0,7]
, markupSafe [1,0]
, memcached [1,5,12]
, mesa [19,0,5]
, meson [0,50,1]
, mpc [1,0,3]
, mpfr [3,1,6] -- [4,0,2]
, mosh [1,3,2]
, motif [2,3,8]
, musl [1,1,20]
, nano [4,2]
, nasm [2,14]
, ncurses [6,1]
, nginx [1,15,7]
, ninja [1,9,0]
, node [10,15,1]
, node [8,15,1] ⫽ { pkgName = "node8" }
, npth [1,6]
, nspr [4,20]
, openssh [7,9]
, openssl [1,1,1]
, p11kit [0,23,16,1]
, pango { version = [1,43], patch = 0 }
, pari [2,11,1]
, patch [2,7]
, pcre [8,42]
, pcre2 [10,32]
, perl5 [5,28,1]
, pixman [0,36,0]
, pkg-config [0,29,2]
, postgresql [11,1]
, protobuf [3,8,0]
, pycairo [1,18,1]
, pygobject { version = [2,28], patch = 7 }
, pygtk { version = [2,24], patch = 0 }
, python [2,7,16]
, python [3,7,3]
, qrencode [4,0,2]
, ragel [6,10]
, randrproto [1,5,0]
, re2c [1,1,1]
, readline [7,0]
, recordproto [1,14,2]
, renderproto [0,11,1]
, ruby { version = [2,6], patch = 3 }
, scour [0,37]
, scrnsaverproto [1,2,2]
, sdl2 [2,0,9]
, sed [4,5]
, shared-mime-info [1,10]
, sqlite { year = 2018, version = [3,26,0] }
, swig [3,0,12]
, tar [1,30]
, texinfo [6,6]
, tcc [0,9,27]
, unistring [0,9,10]
, util-linux { version = [2,33], patch = 1 }
, util-macros [1,19,2]
, vala { version = [0,43], patch = 6 }
, valgrind [3,14,0]
, vim [8,1]
, wayland [1,16,0]
, wget [1,20]
, which [2,21]
, xcb-proto [1,13]
, xextproto [7,3,0]
, xf86vidmodeproto [2,3,1]
, xineramaproto [1,2]
, xmlParser [2,44]
, xproto [7,0,31]
, xtrans [1,3,5]
, xz [5,2,4]
, zlib [1,2,11]
]
