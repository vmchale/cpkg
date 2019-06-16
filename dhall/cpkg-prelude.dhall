{- Dhall prelude imports -}
let concatMapSep = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Text/concatMapSep
in

let concatMapText = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Text/concatMap
in

let concatMap = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/List/concatMap
in

let map = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/List/map
in

let mapOptional = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Optional/map
in

let not = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Bool/not
in

{- Imported types -}
let types = ../dhall/cpkg-types.dhall
in

let showVersion =
  concatMapSep "." Natural Natural/show
in

let maybeAppend =
  λ(a : Type) →
  λ(x : Optional a) →
  λ(xs : List a) →
    Optional/fold a x (List a) (λ(x : a) → (xs # [x])) xs
in

{- Print the architecture for use with GCC -}
let printArch =
  λ(arch : types.Arch) →
    merge
      { X64           = "x86_64"
      , AArch         = "aarch64"
      , Arm           = "arm"
      , RISCV64       = "riscv64"
      , PowerPC       = "powerpc"
      , PowerPC64     = "powerpc64"
      , PowerPC64le   = "powerpc64le"
      , Sparc64       = "sparc64"
      , S390x         = "s390x"
      , Alpha         = "alpha"
      , M68k          = "m68k"
      , Mips          = "mips"
      , MipsEl        = "mipsel"
      , Mips64        = "mips64"
      , Mips64El      = "mips64el"
      , X86           = "i686"
      , SH4           = "sh4"
      , HPPA          = "hppa"
      , HPPA64        = "hppa64"
      , MipsIsa32r6El = "mipsisa32r6el"
      , MipsIsa32r6   = "mipsisa32r6"
      , MipsIsa64r6El = "mipsisa64r6el"
      , MipsIsa64r6   = "mipsisa64r6"
      }
      arch
in

let printManufacturer =
  λ(x : types.Manufacturer) →
    merge
      { Unknown = "unknown"
      , Apple   = "apple"
      , IBM     = "ibm"
      , PC      = "pc"
      }
      x
in

let printOS =
  λ(os : types.OS) →
    merge
      { FreeBSD   = "freebsd"
      , OpenBSD   = "openbsd"
      , NetBSD    = "netbsd"
      , Solaris   = "solaris"
      , Dragonfly = "dragonfly"
      , Linux     = "linux"
      , Darwin    = "darwin"
      , Windows   = "w64"
      , Redox     = "redox"
      , Haiku     = "haiku"
      , IOS       = "darwin"
      , AIX       = "aix"
      , Hurd      = "hurd"
      , Android   = "android"
      , NoOs      = "none"
      }
      os
in

{- Print the ABI for use with GCC -}
let printABI =
  λ(os : types.ABI) →
    merge
      { GNU       = "gnu"
      , GNUabi64  = "gnuabi64"
      , GNUeabi   = "gnueabi"
      , GNUeabihf = "gnueabihf"
      , GNUspe    = "gnuspe"
      , MinGw     = "mingw32"
      }
      os
in

{- Print target triple for use with GCC -}
let printTargetTriple =
  λ(t : types.TargetTriple) →
    "${printArch t.arch}-${printOS t.os}" ++ Optional/fold types.ABI t.abi Text (λ(abi : types.ABI) → "-${printABI abi}") ""
in

{- Print --host flag for use with ./configure scripts -}
let mkHost =
  mapOptional types.TargetTriple Text (λ(tgt : types.TargetTriple) → "--host=${printTargetTriple tgt}")
in

{- Pick executable for 'make'. Use 'gmake' on BSDs, 'make' everywhere else. -}
let makeExe =
  λ(os : types.OS) →

    merge
      { FreeBSD   = "gmake"
      , OpenBSD   = "gmake"
      , NetBSD    = "gmake"
      , Solaris   = "gmake"
      , Dragonfly = "gmake"
      , Linux     = "make"
      , Darwin    = "make"
      , Windows   = "make"
      , Redox     = "make"
      , Haiku     = "make"
      , IOS       = "make"
      , AIX       = "make"
      , Hurd      = "make"
      , Android   = "make"
      , NoOs      = "make" -- this is bad but it's meaningless in this context
      }
      os
in

let mkExe =
  λ(x : Text) →
    types.Command.MakeExecutable { file = x }
in

let mkExes =
  map Text types.Command mkExe
in

let writeFile =
  types.Command.Write
in

let patch =
  λ(x : Text) →
    types.Command.Patch { patchContents = x }
in

let defaultEnv =
  None (List types.EnvVar)
in

let defaultCall =
  { arguments = [] : List Text
  , environment = defaultEnv
  , procDir = None Text
  }
in

let call =
  types.Command.Call
in

let symlinkBinary =
  λ(file : Text) →
    types.Command.SymlinkBinary { file = file }
in

let symlinkManpage =
  λ(file : Text) →
    types.Command.SymlinkManpage { file = file }
in

let symlink =
  λ(tgt : Text) →
  λ(lnk : Text) →
    types.Command.Symlink { tgt = tgt, linkName = lnk }
in

let copyFile =
  λ(src : Text) →
  λ(dest : Text) →
    types.Command.CopyFile { src = src, dest = dest }
in

let symlinkBinaries =
  map Text types.Command symlinkBinary
in

let symlinkManpages =
  map Text types.Command symlinkManpage
in

{- This is to be used on the build OS -}
let isUnix =
  λ(os : types.OS) →

    merge
      { FreeBSD   = True
      , OpenBSD   = True
      , NetBSD    = True
      , Solaris   = True
      , Dragonfly = True
      , Linux     = True
      , Darwin    = True
      , Windows   = False
      , Redox     = False
      , Haiku     = False
      , IOS       = True
      , AIX       = True
      , Hurd      = True
      , Android   = True
      , NoOs      = False -- bad but this should never happen
      }
      os
in

{- Environment variable LDFLAGS for a given configuration -}
let mkLDFlagsGeneral =
  λ(libDirs : List Text) →
  λ(linkLibs : List Text) →
    let flag0 = concatMapSep " " Text (λ(dir : Text) → "-L${dir}") libDirs
    in
    let flag1 = concatMapText Text (λ(dir : Text) → " -l${dir}") linkLibs
    let flag2 = concatMapText Text (λ(dir : Text) → " -Wl,-rpath-link,${dir}") libDirs
    in

    { var = "LDFLAGS", value = flag0 ++ flag1 ++ flag2 }
in

let mkLDFlags =
  λ(libDirs : List Text) →
    mkLDFlagsGeneral libDirs ([] : List Text)
in

let mkLDPath =
  λ(libDirs : List Text) →
    let flag = concatMapSep ":" Text (λ(dir : Text) → dir) libDirs
    in

    { var = "LD_LIBRARY_PATH", value = flag }
in

let mkLDRunPath =
  λ(libDirs : List Text) →
    let flag = concatMapSep ":" Text (λ(dir : Text) → dir) libDirs
    in

    { var = "LD_RUN_PATH", value = flag }
in

let mkStaPath =
  λ(libDirs : List Text) →
    let flag = concatMapText Text (λ(dir : Text) → "${dir}:") libDirs
    in

    { var = "LIBRARY_PATH", value = flag ++ "/usr/local/lib:/lib:/usr/lib" }
in

{- Get the host OS from a configuration. If we are not cross-compiling, we can
   just use the build OS as detected by cpkg
   -}
let osCfg =
  λ(cfg : types.BuildVars) →
    Optional/fold types.TargetTriple cfg.targetTriple types.OS
      (λ(tgt : types.TargetTriple) → tgt.os)
        cfg.buildOS
in

{- Get the host architecture for the build, using the method above -}
let archCfg =
  λ(cfg : types.BuildVars) →
    Optional/fold types.TargetTriple cfg.targetTriple types.Arch
      (λ(tgt : types.TargetTriple) → tgt.arch)
        cfg.buildArch
in


{- Used to set the PERL5LIB variable to the right thing. This is necessary since
   we install libraries to nonstandard locations.
   -}
let mkPerlLib =
  λ(x : { libDirs : List Text, perlVersion : List Natural, cfg : types.BuildVars }) →
    let os = x.cfg.buildOS
    in
    let arch = x.cfg.buildArch
    in
    let flag = concatMapSep ":" Text (λ(dir : Text) → dir ++ "/site_perl/${showVersion x.perlVersion}/${printArch arch}-${printOS os}/") x.libDirs
    in
    let major = Optional/fold Natural (List/head Natural x.perlVersion) Text (Natural/show) ""
    in

    { var = "PERL${major}LIB", value = flag }
in

let mkIncludePath =
  λ(incls : List Text) →
    let flag = concatMapSep ":" Text (λ(dir : Text) → dir) incls
    in

    { var = "C_INCLUDE_PATH", value = flag }
in

let mkCFlags =
  λ(cfg : types.BuildVars) →
    let flag = concatMapSep " " Text (λ(dir : Text) → "-I${dir}") cfg.includeDirs
    in
    let staFlag =
      if cfg.static
        then " -static"
        else ""
    in

    { var = "CPPFLAGS", value = flag ++ staFlag }
in

let mkPkgConfigVar =
  λ(libDirs : List Text) →
    let flag = concatMapSep ":" Text (λ(dir : Text) → "${dir}/pkgconfig") libDirs
    in

    { var = "PKG_CONFIG_PATH", value = flag }
in

{- This is used for GLib/GTK/some of that stuff -}
let mkXdgDataDirs =
  λ(shareDirs : List Text) →
    let flag = concatMapSep ":" Text (λ(dir : Text) → dir) shareDirs
    in

    { var = "XDG_DATA_DIRS", value = flag }
in

let mkPathVar =
  λ(binDirs : List Text) →
    concatMapText Text (λ(dir : Text) → "${dir}:") binDirs
in

let defaultPath =
  λ(cfg : types.BuildVars) →
    if isUnix cfg.buildOS
      then [ { var = "PATH", value = mkPathVar cfg.binDirs ++ "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" } ] : List types.EnvVar
      else [] : List types.EnvVar -- FIXME: handle non-unix case
in

{- Set either LD_LIBRARY_PATH or LIBRARY_PATH depending on whether we are doing
   a static build or not
   -}
let libPath =
  λ(cfg : types.BuildVars) →
    if cfg.static
      then
        mkStaPath cfg.linkDirs
      else
        mkLDPath cfg.linkDirs
in

{- This is vaguely terrible, but it's needed for GLib for some reason -}
let mkLDPreload =
  λ(libs : List Text) →
    let flag = concatMapSep " " Text (λ(lib : Text) → lib) libs
    in

    { var = "LD_PRELOAD", value = flag }
in

{- Default environment variables for a given configuration -}
let configEnv =
  λ(linkLibs : List Text) →
  λ(cfg : types.BuildVars) →
    defaultPath cfg # [ mkLDFlagsGeneral cfg.linkDirs linkLibs
                      , mkCFlags cfg
                      , mkPkgConfigVar (cfg.shareDirs # cfg.linkDirs)
                      , libPath cfg
                      , mkLDRunPath cfg.linkDirs
                      , mkPerlLib { libDirs = cfg.linkDirs, perlVersion = [5,28,1], cfg = cfg } -- TODO: take this as a parameter
                      ]
in

let buildEnv =
  λ(cfg : types.BuildVars) →
    defaultPath cfg # [ mkPkgConfigVar (cfg.shareDirs # cfg.linkDirs)
                      , mkPerlLib { libDirs = cfg.linkDirs, perlVersion = [5,28,1], cfg = cfg } -- TODO: take this as a parameter
                      , mkLDPath cfg.linkDirs
                      ]
in

let configSome =
  λ(linkLibs : List Text) →
  λ(cfg : types.BuildVars) →
    Some (configEnv linkLibs cfg)
in

{- The most general configuration setup. You probably want to use
   defaultConfigure
   -}
let generalConfigure =
  λ(envVars : List Text → types.BuildVars → Optional (List types.EnvVar)) →
  λ(filename : Text) →
  λ(linkLibs : List Text) →
  λ(extraFlags : List Text) →
  λ(cfg : types.BuildVars) →
    let maybeHost = mkHost cfg.targetTriple
    in
    let modifyArgs = maybeAppend Text maybeHost
    in

    [ mkExe filename
    , call (defaultCall ⫽ { program = "./${filename}"
                          , arguments = modifyArgs [ "--prefix=${cfg.installDir}" ] # extraFlags
                          , environment = envVars linkLibs cfg
                          })
    ]
in

let configWithEnv =
  λ(envVars : List Text → types.BuildVars → Optional (List types.EnvVar)) →
    generalConfigure envVars "configure" ([] : List Text) ([] : List Text)
in

let configureWithFlags =
  generalConfigure configSome "configure" ([] : List Text)
in

let defaultConfigure =
  configureWithFlags ([] : List Text)
in

let configureLinkExtraLibs =
  λ(linkLibs : List Text) →
    generalConfigure configSome "configure" linkLibs ([] : List Text)
in

let mkAclocalPath =
  λ(shareDirs : List Text) →
    let flag = concatMapSep ":" Text (λ(dir : Text) → "${dir}/aclocal:${dir}/autoconf/autoconf") shareDirs
    in

    { var = "ACLOCAL_PATH", value = flag }
in

let configureMkExesExtraFlags =
  λ(x : { bins : List Text, extraFlags : List Text }) →
  λ(cfg : types.BuildVars) →
    mkExes x.bins
      # configureWithFlags x.extraFlags cfg
in

let configureMkExes =
  λ(bins : List Text) →
    configureMkExesExtraFlags { bins = bins, extraFlags = ([] : List Text) }
in

let buildWith =
  λ(envs : List types.EnvVar) →
  λ(cfg : types.BuildVars) →
    [ call (defaultCall ⫽ { program = makeExe cfg.buildOS
                          , arguments = [ "-j${Natural/show cfg.cpus}" ]
                          , environment =
                              Some envs
                          })
    ]
in

let defaultBuild =
  λ(cfg : types.BuildVars) →
    buildWith (buildEnv cfg) cfg
in

let installWith =
  λ(envs : List types.EnvVar) →
  λ(cfg : types.BuildVars) →
    [ call (defaultCall ⫽ { program = makeExe cfg.buildOS
                          , arguments = [ "install" ]
                          , environment =
                              Some envs
                          })
    ]
in

let defaultInstall =
  λ(cfg : types.BuildVars) →
    installWith (buildEnv cfg) cfg
in

let installWithBinaries =
  λ(bins : List Text) →
  λ(installVars : types.BuildVars) →
    defaultInstall installVars
      # (if not installVars.isCross
            then symlinkBinaries bins
            else [] : List types.Command)
in

let unbounded =
  λ(x : Text) →
    { name = x
    , bound = types.VersionBound.NoBound
    }
in

let lowerBound =
  λ(pkg : { name : Text, lower : List Natural }) →
    { name = pkg.name
    , bound = types.VersionBound.Lower { lower = pkg.lower }
    }
in

let upperBound =
  λ(pkg : { name : Text, upper : List Natural }) →
    { name = pkg.name
    , bound = types.VersionBound.Upper { upper = pkg.upper }
    }
in

let defaultPackage =
  { configureCommand = defaultConfigure
  , buildCommand     = defaultBuild
  , installCommand   = defaultInstall
  , pkgBuildDeps     = [] : List types.Dep
  , pkgDeps          = [] : List types.Dep
  , pkgStream        = True
  }
in

let simplePackage =
  λ(pkg : { name : Text, version : List Natural}) →
    defaultPackage ⫽
      { pkgName = pkg.name
      , pkgVersion = pkg.version
      , pkgSubdir = "${pkg.name}-${showVersion pkg.version}"
      }
in

let makeGnuExe =
  λ(pkg : { name : Text, version : List Natural}) →
    simplePackage pkg ⫽
      { pkgUrl = "https://ftp.gnu.org/gnu/${pkg.name}/${pkg.name}-${showVersion pkg.version}.tar.xz"
      , installCommand = installWithBinaries [ "bin/${pkg.name}" ]
      }
in

let makeGnuLibrary =
  λ(pkg : { name : Text, version : List Natural}) →
    simplePackage pkg ⫽
      { pkgUrl = "https://mirrors.ocf.berkeley.edu/gnu/lib${pkg.name}/lib${pkg.name}-${showVersion pkg.version}.tar.xz"
      , pkgSubdir = "lib${pkg.name}-${showVersion pkg.version}"
      }
in

let createDir =
  λ(x : Text) →
    types.Command.CreateDirectory { dir = x }
in

let printCMakeOS =
  λ(os : types.OS) →
    merge
      { FreeBSD   = "BSD"
      , OpenBSD   = "BSD"
      , NetBSD    = "BSD"
      , Solaris   = "Solaris"
      , Dragonfly = "BSD"
      , Linux     = "Linux"
      , Darwin    = "Darwin"
      , Windows   = "Windows"
      , Redox     = "Redox"
      , Haiku     = "Haiku"
      , IOS       = "Darwin"
      , AIX       = "AIX"
      , Hurd      = "Hurd"
      , Android   = "Android"
      , NoOs      = "Generic"
      }
      os
in

let cmakeConfigureGeneral =
  λ(envVars : types.BuildVars → Optional (List types.EnvVar)) →
  λ(flags : List Text) →
  λ(cfg : types.BuildVars) →
    let host =
      Optional/fold types.TargetTriple cfg.targetTriple (List Text)
        (λ(tgt : types.TargetTriple) → ["-DCMAKE_C_COMPILER=${printTargetTriple tgt}-gcc", "-DCMAKE_CXX_COMPILER=${printTargetTriple tgt}-g++"])
          (["-DCMAKE_C_COMPILER=cc", "-DCMAKE_CXX_COMPILER=c++"])
    in
    let system =
      Optional/fold types.TargetTriple cfg.targetTriple (List Text)
        (λ(tgt : types.TargetTriple) → ["-DCMAKE_SYSTEM_NAME=${printCMakeOS tgt.os}"])
          ([] : List Text)
    in

    [ createDir "build"
    , call { program = "cmake"
           , arguments = [ "../", "-DCMAKE_INSTALL_PREFIX:PATH=${cfg.installDir}", "-DCMAKE_MAKE_PROGRAM=${makeExe cfg.buildOS}" ] # host # system # flags
           , environment = envVars cfg
           , procDir = Some "build"
           }
    ]
in

let cmakeEnv =
  λ(cfg : types.BuildVars) →
    [ mkPkgConfigVar cfg.shareDirs
    , { var = "CMAKE_INCLUDE_PATH", value = (mkIncludePath cfg.includeDirs).value }
    , { var = "CMAKE_LIBRARY_PATH", value = (libPath cfg).value }
    ]
      # defaultPath cfg
in

let cmakeSome =
  λ(cfg : types.BuildVars) →
    Some (cmakeEnv cfg)
in

let cmakeConfigureWithFlags =
  cmakeConfigureGeneral cmakeSome
in

let cmakeConfigure =
  cmakeConfigureWithFlags ([] : List Text)
in

let cmakeConfigureNinja =
  λ(cfg : types.BuildVars) →
    let host =
      Optional/fold types.TargetTriple cfg.targetTriple (List Text)
        (λ(tgt : types.TargetTriple) → ["-DCMAKE_C_COMPILER=${printTargetTriple tgt}-gcc", "-DCMAKE_CXX_COMPILER=${printTargetTriple tgt}-g++"])
          ([] : List Text)
    in

    let system =
      Optional/fold types.TargetTriple cfg.targetTriple (List Text)
        (λ(tgt : types.TargetTriple) → ["-DCMAKE_SYSTEM_NAME=${printCMakeOS tgt.os}"])
          ([] : List Text)
    in

    [ createDir "build"
    , call { program = "cmake"
           , arguments = [ "../", "-DCMAKE_INSTALL_PREFIX:PATH=${cfg.installDir}", "-G", "Ninja" ] # host # system
           , environment = defaultEnv
           , procDir = Some "build"
           }
    ]
in

let perlConfigure =
  λ(cfg : types.BuildVars) →

  [ call { program = "perl"
         , arguments = [ "Makefile.PL", "PREFIX=${cfg.installDir}" ]
         , environment = defaultEnv
         , procDir = None Text
         }
  ]
in

let cmakeBuild =
  λ(cfg : types.BuildVars) →
    [ call { program = "cmake"
           , arguments = [ "--build", ".", "--config", "Release", "--", "-j", Natural/show cfg.cpus ]
           , environment = defaultEnv
           , procDir = Some "build"
           }
    ]
in

let cmakeInstall =
  λ(cfg : types.BuildVars) →
    [ call { program = "cmake"
           , arguments = [ "--build", ".", "--target", "install", "--config", "Release" ]
           , environment = defaultEnv
           , procDir = Some "build"
           }
    ]
in

let cmakeInstallWithBinaries =
  λ(bins : List Text) →
  λ(installVars : types.BuildVars) →
    cmakeInstall installVars
      # symlinkBinaries bins
in

let cmakePackage =
  defaultPackage ⫽
    { configureCommand = cmakeConfigure
    , buildCommand     = cmakeBuild
    , installCommand   = cmakeInstall
    , pkgBuildDeps     = [ unbounded "cmake" ]
    }
in

let autogenConfigure =
  λ(cfg : types.BuildVars) →
    [ mkExe "autogen.sh"
    , call (defaultCall ⫽ { program = "./autogen.sh"
                          , environment = Some ([ mkAclocalPath cfg.shareDirs ]
                                                  # defaultPath cfg)
                          })
    ] # defaultConfigure cfg
in

let fullVersion =
  λ(x : { version : List Natural, patch : Natural }) →
    x.version # [x.patch]
in

let mkPyPath =
  λ(version : List Natural) →
  λ(libDirs : List Text) →
    let flag = concatMapSep ":" Text (λ(dir : Text) → "${dir}/python${showVersion version}/site-packages") libDirs
    in

    { var = "PYTHONPATH", value = flag }
in

let mkPy3Path =
  mkPyPath [3,7]
in

{- Write a cross-compilation configuration file for use with meson -}
let mesonCfgFile =
  λ(cfg : types.BuildVars) →
    let prefix =
      Optional/fold types.TargetTriple cfg.targetTriple Text
        (λ(tgt : types.TargetTriple) → "${printTargetTriple tgt}-")
          ""
    in

    "[binaries]\n" ++
    "c = '${prefix}gcc'\n" ++ -- FIXME: default to cc/c++ when no cfg.targetTriple is passed
    "cpp = '${prefix}g++'\n" ++
    "ar = '${prefix}ar'\n" ++
    "strip = '${prefix}strip'\n" ++
    "pkgconfig = 'pkg-config'\n" ++

    "[host_machine]\n" ++
    "system = '${printOS (osCfg cfg)}'\n" ++ -- TODO: printOSMeson function (w64 -> windows)
    "cpu_family = '${printArch (archCfg cfg)}'\n" ++
    "cpu = '${printArch (archCfg cfg)}'\n" ++
    "endian = 'little'" -- FIXME parse endianness in Haskell library?
in

let mesonEnv =
  λ(cfg : types.BuildVars) →
    Some [ mkPkgConfigVar cfg.linkDirs
         , { var = "PATH", value = mkPathVar cfg.binDirs ++ "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" }
         , mkPy3Path cfg.linkDirs
         , libPath cfg
         , mkLDRunPath cfg.linkDirs
         , mkLDFlags cfg.linkDirs
         , mkCFlags cfg
         ]
in

let mesonConfigureGeneral =
  λ(envs : types.BuildVars → Optional (List types.EnvVar)) →
  λ(flags : List Text) →
  λ(cfg : types.BuildVars) →
    let crossArgs =
      if cfg.isCross
        then [ "--cross-file", "cross.txt" ]
        else [] : List Text
    in

    [ createDir "build"
    , writeFile { file = "build/cross.txt", contents = mesonCfgFile cfg }
    , call { program = "meson"
           , arguments = [ "--prefix=${cfg.installDir}", ".." ] # crossArgs # flags
           , environment = envs cfg
           , procDir = Some "build"
           }
    ]
in

let mesonConfigureWithFlags =
  mesonConfigureGeneral mesonEnv
in

let mesonConfigure =
  mesonConfigureWithFlags ([] : List Text)
in

let ninjaBuildWith =
  λ(linkLibs : List Text) →
  λ(cfg : types.BuildVars) →
    [ call (defaultCall ⫽ { program = "ninja"
                          , environment = Some [ mkPkgConfigVar cfg.linkDirs
                                               , { var = "PATH", value = mkPathVar cfg.binDirs ++ "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" }
                                               , mkPy3Path cfg.linkDirs
                                               , libPath cfg
                                               , mkLDRunPath cfg.linkDirs
                                               , mkLDFlagsGeneral cfg.linkDirs linkLibs
                                               , mkCFlags cfg
                                               , mkPkgConfigVar cfg.linkDirs
                                               ]
                          , procDir = Some "build" }) ]
in

let ninjaBuild =
  ninjaBuildWith ([] : List Text)
in

let ninjaInstall =
  λ(cfg : types.BuildVars) →
    [ call (defaultCall ⫽ { program = "ninja"
                          , environment = Some [ mkPkgConfigVar cfg.linkDirs
                                               , { var = "PATH", value = mkPathVar cfg.binDirs ++ "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" }
                                               , mkPy3Path cfg.linkDirs
                                               , libPath cfg
                                               , mkLDRunPath cfg.linkDirs
                                               , mkLDFlags cfg.linkDirs
                                               , mkCFlags cfg
                                               ]
                          , arguments = [ "install" ]
                          , procDir = Some "build"
                          })
    ]
in

let ninjaPackage =
  λ(x : { name : Text, version : List Natural }) →
    simplePackage x ⫽
      { configureCommand = mesonConfigure
      , buildCommand = ninjaBuild
      , installCommand = ninjaInstall
      , pkgBuildDeps = [ unbounded "meson"
                       , unbounded "ninja"
                       ]
      }
in

let copyFiles =
  map { src : Text, dest : Text } types.Command types.Command.CopyFile
in

let ninjaInstallWithPkgConfig =
  λ(fs : List { src : Text, dest : Text }) →
  λ(cfg : types.BuildVars) →
    ninjaInstall cfg # copyFiles fs
in

let doNothing =
  λ(_ : types.BuildVars) → [] : List types.Command
in

let mesonMoves =
  map Text { src : Text, dest : Text} (λ(pcFile : Text) → { src = "build/meson-private/${pcFile}", dest = "lib/pkgconfig/${pcFile}" })
in

let pythonBuild =
  λ(version : List Natural) →
  λ(cfg : types.BuildVars) →
    let major = Optional/fold Natural (List/head Natural version) Text (Natural/show) ""
    in
    let versionString = showVersion version
    in
    [ createDir "${cfg.installDir}/lib/python${versionString}/site-packages"
    , call (defaultCall ⫽ { program = "python${major}"
                          , arguments = [ "setup.py", "build" ]
                          , environment = Some ([ { var = "PYTHONPATH", value = "${cfg.installDir}/lib/python${versionString}/site-packages" }
                                                  , mkPkgConfigVar cfg.linkDirs
                                                  , libPath cfg
                                                  ] # defaultPath cfg)
                          })
    ]
in

let pythonInstall =
  λ(version : List Natural) →
  λ(cfg : types.BuildVars) →
    let major = Optional/fold Natural (List/head Natural version) Text (Natural/show) ""
    in
    let versionString = showVersion version
    in
    [ createDir "${cfg.installDir}/lib/python${versionString}/site-packages"
    , call (defaultCall ⫽ { program = "python${major}"
                          , arguments = [ "setup.py", "install", "--prefix=${cfg.installDir}", "--optimize=1" ]
                          , environment = Some ([ { var = "PYTHONPATH", value = "${cfg.installDir}/lib/python${versionString}/site-packages" }
                                                , mkPkgConfigVar cfg.linkDirs
                                                , libPath cfg
                                                ] # defaultPath cfg)
                          })
    ]
in

let pythonPackage =
  λ(pyVersion : List Natural) →
  λ(x : { name : Text, version : List Natural }) →
    let major = Optional/fold Natural (List/head Natural pyVersion) Text (Natural/show) ""
    in
    simplePackage x ⫽
      { configureCommand = doNothing
      , buildCommand = pythonBuild pyVersion
      , installCommand = pythonInstall pyVersion
      , pkgBuildDeps = [ unbounded "python${major}" ]
      }
in

let python3Build =
  pythonBuild [3,7]
in

let python3Install =
  pythonInstall [3,7]
in

let python3Package =
  pythonPackage [3,7]
in

let python2Package =
  pythonPackage [2,7]
in

let mkCCVar =
  λ(cfg : types.BuildVars) →
    Optional/fold types.TargetTriple cfg.targetTriple (List types.EnvVar)
      (λ(tgt : types.TargetTriple) → [{ var = "CC", value = "${printTargetTriple tgt}-gcc" }])
        ([] : List types.EnvVar)
in

let squishVersion =
  concatMapText Natural Natural/show
in

let mkCCArg =
  λ(cfg : types.BuildVars) →
    Optional/fold types.TargetTriple cfg.targetTriple (List Text)
      (λ(tgt : types.TargetTriple) → ["CC=${printTargetTriple tgt}-gcc"])
        ([] : List Text)
in

let preloadEnv =
  λ(_ : List Text) →
  λ(cfg : types.BuildVars) →
    Some (defaultPath cfg # [ mkLDFlags cfg.linkDirs
                            , mkCFlags cfg
                            , mkPkgConfigVar cfg.linkDirs
                            , libPath cfg
                            , mkXdgDataDirs cfg.shareDirs
                            , mkLDPreload cfg.preloadLibs
                            , mkPerlLib { libDirs = cfg.linkDirs, perlVersion = [5,28,1], cfg = cfg } -- TODO: take this as a parameter
                            ])
in

let preloadCfg =
  generalConfigure preloadEnv "configure" ([] : List Text) ([] : List Text)
in

let printEnvVar =
  λ(var : types.EnvVar) →
    "${var.var}=${var.value}"
in

{- This writes + installs a shell script that allows you to use programs
   installed via cpkg with the appropriate environment variables set, since of
   course we install in nonstandard locations
   -}
let mkPyWrapper =
  λ(version : List Natural) →
  λ(binName : Text) →
  λ(cfg : types.BuildVars) →
    let wrapperContents = "${printEnvVar (libPath cfg)} ${printEnvVar (mkPyPath version cfg.linkDirs)}:${cfg.installDir}/lib/python${showVersion version}/site-packages ${cfg.installDir}/bin/${binName} $@"
    in
    let wrapped = "wrapper/${binName}"
    in

    [ createDir "wrapper"
    , writeFile { file = wrapped, contents = wrapperContents }
    , mkExe wrapped
    , copyFile wrapped wrapped
    , symlinkBinary wrapped
    ]
in

let mkPy3Wrapper =
  mkPyWrapper [3,7]
in

let mkPy2Wrapper =
  mkPyWrapper [2,7]
in

let installWithPyWrappers =
  λ(version : List Natural) →
  λ(binNames : List Text) →
  λ(cfg : types.BuildVars) →
    pythonInstall version cfg
      # concatMap Text types.Command (λ(bin : Text) → mkPyWrapper version bin cfg) binNames
in

let installWithPy3Wrappers =
  installWithPyWrappers [3,7]
in

{- This is used to make bash scripts that wrap an executable. Since executables
   are linked against libraries installed in nonstandard places, we wrap them
   with a shell script that sets LD_LIBRARY_PATH appropriately.

   For an example use, see the emacs package.
   -}
let mkLDPathWrapper =
  λ(cfg : types.BuildVars) →
  λ(binName : Text) →
    let wrapper = "${printEnvVar (mkLDPath cfg.linkDirs)}:${cfg.installDir}/lib LD_PRELOAD='${(mkLDPreload cfg.preloadLibs).value}' ${cfg.installDir}/bin/${binName} $@"
    in
    let wrapped = "wrapper/${binName}"
    in

    [ createDir "wrapper"
    , writeFile { file = wrapped, contents = wrapper }
    , mkExe wrapped
    , copyFile wrapped wrapped
    , symlinkBinary wrapped
    ]
in

let installWithWrappers =
  λ(bins : List Text) →
  λ(cfg : types.BuildVars) →
    defaultInstall cfg #
      concatMap Text types.Command (λ(bin : Text) → mkLDPathWrapper cfg bin) bins
      -- TODO: add to PATH for e.g. PERL interpreter
in

let underscoreVersion =
  concatMapSep "_" Natural Natural/show
in

let isX64 =
  λ(arch : types.Arch) →
    merge
      { X64           = True
      , AArch         = False
      , Arm           = False
      , RISCV64       = False
      , PowerPC       = False
      , PowerPC64     = False
      , PowerPC64le   = False
      , Sparc64       = False
      , S390x         = False
      , Alpha         = False
      , M68k          = False
      , Mips          = False
      , MipsEl        = False
      , Mips64        = False
      , Mips64El      = False
      , X86           = False
      , SH4           = False
      , HPPA          = False
      , HPPA64        = False
      , MipsIsa32r6El = False
      , MipsIsa32r6   = False
      , MipsIsa64r6El = False
      , MipsIsa64r6   = False
      }
      arch
in

{ showVersion         = showVersion
, makeGnuLibrary      = makeGnuLibrary
, makeGnuExe          = makeGnuExe
, defaultPackage      = defaultPackage
, unbounded           = unbounded
, lowerBound          = lowerBound
, upperBound          = upperBound
, makeExe             = makeExe
, printArch           = printArch
, printManufacturer   = printManufacturer
, printOS             = printOS
, printTargetTriple   = printTargetTriple
, call                = call
, mkExe               = mkExe
, mkExes              = mkExes
, createDir           = createDir
, mkHost              = mkHost
, defaultConfigure    = defaultConfigure
, defaultBuild        = defaultBuild
, defaultInstall      = defaultInstall
, cmakeConfigure      = cmakeConfigure
, cmakeConfigureGeneral = cmakeConfigureGeneral
, cmakeConfigureWithFlags = cmakeConfigureWithFlags
, cmakeBuild          = cmakeBuild
, cmakeInstall        = cmakeInstall
, cmakePackage        = cmakePackage
, autogenConfigure    = autogenConfigure
, defaultCall         = defaultCall
, defaultEnv          = defaultEnv
, maybeAppend         = maybeAppend
, mkCFlags            = mkCFlags
, mkLDFlags           = mkLDFlags
, mkLDPath            = mkLDPath
, mkLDRunPath         = mkLDRunPath
, mkStaPath           = mkStaPath
, libPath             = libPath
, mkPyPath            = mkPyPath
, mkPy3Path           = mkPy3Path
, mkIncludePath       = mkIncludePath
, isUnix              = isUnix
, defaultPath         = defaultPath
, simplePackage       = simplePackage
, symlinkBinary       = symlinkBinary
, symlinkManpage      = symlinkManpage
, symlink             = symlink
, symlinkBinaries     = symlinkBinaries
, symlinkManpages     = symlinkManpages
, installWithBinaries = installWithBinaries
, configureMkExes     = configureMkExes
, generalConfigure    = generalConfigure
, configureWithFlags  = configureWithFlags
, configureMkExesExtraFlags = configureMkExesExtraFlags
, writeFile           = writeFile
, cmakeInstallWithBinaries = cmakeInstallWithBinaries
, copyFile            = copyFile
, mkPathVar           = mkPathVar
, mkPkgConfigVar      = mkPkgConfigVar
, fullVersion         = fullVersion
, mesonConfigure      = mesonConfigure
, mesonConfigureGeneral = mesonConfigureGeneral
, mesonEnv            = mesonEnv
, mesonConfigureWithFlags = mesonConfigureWithFlags
, ninjaBuild          = ninjaBuild
, ninjaInstall        = ninjaInstall
, ninjaInstallWithPkgConfig = ninjaInstallWithPkgConfig
, ninjaPackage        = ninjaPackage
, doNothing           = doNothing
, perlConfigure       = perlConfigure
, copyFiles           = copyFiles
, mkPerlLib           = mkPerlLib
, mesonMoves          = mesonMoves
, python3Build        = python3Build
, python3Install      = python3Install
, python3Package      = python3Package
, mkLDPreload         = mkLDPreload
, configureLinkExtraLibs = configureLinkExtraLibs
, mkXdgDataDirs       = mkXdgDataDirs
, buildWith           = buildWith
, installWith         = installWith
, mkCCVar             = mkCCVar
, squishVersion       = squishVersion
, osCfg               = osCfg
, archCfg             = archCfg
, mkCCArg             = mkCCArg
, mesonCfgFile        = mesonCfgFile
, python2Package      = python2Package
, configEnv           = configEnv
, configSome          = configSome
, preloadEnv          = preloadEnv
, preloadCfg          = preloadCfg
, printEnvVar         = printEnvVar
, mkPyWrapper         = mkPyWrapper
, mkPy3Wrapper        = mkPy3Wrapper
, mkPy2Wrapper        = mkPy2Wrapper
, installWithPyWrappers = installWithPyWrappers
, installWithPy3Wrappers = installWithPy3Wrappers
, cmakeConfigureNinja = cmakeConfigureNinja
, mkLDPathWrapper     = mkLDPathWrapper
, installWithWrappers = installWithWrappers
, cmakeEnv            = cmakeEnv
, cmakeSome           = cmakeSome
, underscoreVersion   = underscoreVersion
, isX64               = isX64
, configWithEnv       = configWithEnv
, buildEnv            = buildEnv
, patch               = patch
}
