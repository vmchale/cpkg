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

let printArch =
  λ(arch : types.Arch) →
    merge
      { X64         = λ(_ : {}) → "x86_64"
      , AArch       = λ(_ : {}) → "aarch64"
      , Arm         = λ(_ : {}) → "arm"
      , RISCV64     = λ(_ : {}) → "riscv64"
      , PowerPC     = λ(_ : {}) → "powerpc"
      , PowerPC64   = λ(_ : {}) → "powerpc64"
      , PowerPC64le = λ(_ : {}) → "powerpc64le"
      , Sparc64     = λ(_ : {}) → "sparc64"
      , S390x       = λ(_ : {}) → "s390x"
      , Alpha       = λ(_ : {}) → "alpha"
      , M68k        = λ(_ : {}) → "m68k"
      , Mips        = λ(_ : {}) → "mips"
      , MipsEl      = λ(_ : {}) → "mipsel"
      , Mips64      = λ(_ : {}) → "mips64"
      , Mips64El    = λ(_ : {}) → "mips64el"
      , X86         = λ(_ : {}) → "i686"
      , SH4         = λ(_ : {}) → "sh4"
      , HPPA        = λ(_ : {}) → "hppa"
      , HPPA64      = λ(_ : {}) → "hppa64"
      }
      arch
in

let printManufacturer =
  λ(x : types.Manufacturer) →
    merge
      { Unknown = λ(_ : {}) → "unknown"
      , Apple   = λ(_ : {}) → "apple"
      , IBM     = λ(_ : {}) → "ibm"
      , PC      = λ(_ : {}) → "pc"
      }
      x
in

let printOS =
  λ(os : types.OS) →
    merge
      { FreeBSD   = λ(_ : {}) → "freebsd"
      , OpenBSD   = λ(_ : {}) → "openbsd"
      , NetBSD    = λ(_ : {}) → "netbsd"
      , Solaris   = λ(_ : {}) → "solaris"
      , Dragonfly = λ(_ : {}) → "dragonfly"
      , Linux     = λ(_ : {}) → "linux"
      , Darwin    = λ(_ : {}) → "darwin"
      , Windows   = λ(_ : {}) → "w64"
      , Redox     = λ(_ : {}) → "redox"
      , Haiku     = λ(_ : {}) → "haiku"
      , IOS       = λ(_ : {}) → "darwin"
      , AIX       = λ(_ : {}) → "aix"
      , Hurd      = λ(_ : {}) → "hurd"
      , Android   = λ(_ : {}) → "android"
      , NoOs      = λ(_ : {}) → "none"
      }
      os
in

let printABI =
  λ(os : types.ABI) →
    merge
      { GNU       = λ(_ : {}) → "gnu"
      , GNUabi64  = λ(_ : {}) → "gnuabi64"
      , GNUeabi   = λ(_ : {}) → "gnueabi"
      , GNUeabihf = λ(_ : {}) → "gnueabihf"
      , GNUspe    = λ(_ : {}) → "gnuspe"
      , MinGw     = λ(_ : {}) → "mingw32"
      }
      os
in

let printTargetTriple =
  λ(t : types.TargetTriple) →
    "${printArch t.arch}-${printOS t.os}" ++ Optional/fold types.ABI t.abi Text (λ(abi : types.ABI) → "-${printABI abi}") ""
in

let mkHost =
  mapOptional types.TargetTriple Text (λ(tgt : types.TargetTriple) → "--host=${printTargetTriple tgt}")
in

let mkHostEnv =
  mapOptional types.TargetTriple Text (λ(tgt : types.TargetTriple) → "CHOST=${printTargetTriple tgt}")
in

let makeExe =
  λ(os : types.OS) →

    let gmake = λ(_ : {}) → "gmake"
    in
    let make  = λ(_ : {}) → "make"
    in

    merge
      { FreeBSD   = gmake
      , OpenBSD   = gmake
      , NetBSD    = gmake
      , Solaris   = gmake
      , Dragonfly = gmake
      , Linux     = make
      , Darwin    = make
      , Windows   = make
      , Redox     = make
      , Haiku     = make
      , IOS       = make
      , AIX       = make
      , Hurd      = make
      , Android   = make
      , NoOs      = make -- this is bad but it's meaningless in this context
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

let isUnix =
  λ(os : types.OS) →

    let true = λ(_ : {}) → True
    in
    let false = λ(_ : {}) → False
    in

    merge
      { FreeBSD   = true
      , OpenBSD   = true
      , NetBSD    = true
      , Solaris   = true
      , Dragonfly = true
      , Linux     = true
      , Darwin    = true
      , Windows   = false
      , Redox     = false
      , Haiku     = false
      , IOS       = true
      , AIX       = true
      , Hurd      = true
      , Android   = true
      , NoOs      = false -- bad but this should never happen
      }
      os
in

let mkLDFlagsGeneral =
  λ(libDirs : List Text) →
  λ(linkLibs : List Text) →
    let flag0 = concatMapSep " " Text (λ(dir : Text) → "-L${dir}") libDirs
    in
    let flag1 = concatMapText Text (λ(dir : Text) → " -l${dir}") linkLibs
    in

    { var = "LDFLAGS", value = flag0 ++ flag1 }
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

let mkStaPath =
  λ(libDirs : List Text) →
    let flag = concatMapSep ":" Text (λ(dir : Text) → dir) libDirs
    in

    { var = "LIBRARY_PATH", value = flag }
in

let osCfg =
  λ(cfg : types.BuildVars) →
    Optional/fold types.TargetTriple cfg.targetTriple types.OS
      (λ(tgt : types.TargetTriple) → tgt.os)
        cfg.buildOS
in

let archCfg =
  λ(cfg : types.BuildVars) →
    Optional/fold types.TargetTriple cfg.targetTriple types.Arch
      (λ(tgt : types.TargetTriple) → tgt.arch)
        cfg.buildArch
in


let mkPerlLib =
  λ(x : { libDirs : List Text, perlVersion : List Natural, cfg : types.BuildVars }) →
    let tgt = x.cfg.targetTriple
    in
    let os = osCfg x.cfg
    in
    let arch = archCfg x.cfg
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
  λ(libDirs : List Text) →
    let flag = concatMapSep " " Text (λ(dir : Text) → "-I${dir}") libDirs
    in

    { var = "CPPFLAGS", value = flag }
in

let mkPkgConfigVar =
  λ(libDirs : List Text) →
    let flag = concatMapSep ":" Text (λ(dir : Text) → "${dir}/pkgconfig") libDirs
    in

    { var = "PKG_CONFIG_PATH", value = flag }
in

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
      else [] : List types.EnvVar
in

let libPath =
  λ(cfg : types.BuildVars) →
    if cfg.static
      then
        mkStaPath cfg.linkDirs
      else
        mkLDPath cfg.linkDirs
in

let configEnv =
  λ(linkLibs : List Text) →
  λ(cfg : types.BuildVars) →
    Some (defaultPath cfg # [ mkLDFlagsGeneral cfg.linkDirs linkLibs
                            , mkCFlags cfg.includeDirs
                            , mkPkgConfigVar (cfg.shareDirs # cfg.linkDirs)
                            , libPath cfg
                            , mkPerlLib { libDirs = cfg.linkDirs, perlVersion = [5,28,1], cfg = cfg } -- TODO: take this as a parameter
                            ])
in

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

let configureWithFlags =
  generalConfigure configEnv "configure" ([] : List Text)
in

let defaultConfigure =
  configureWithFlags ([] : List Text)
in

let configureLinkExtraLibs =
  λ(linkLibs : List Text) →
    generalConfigure configEnv "configure" linkLibs ([] : List Text)
in

let mkAclocalPath =
  λ(shareDirs : List Text) →
    let flag = concatMapSep ":" Text (λ(dir : Text) → "${dir}/aclocal") shareDirs
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
    buildWith (defaultPath cfg # [ mkPkgConfigVar cfg.linkDirs
                                 , libPath cfg
                                 ]) cfg
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
    installWith (defaultPath cfg # [ mkPkgConfigVar cfg.linkDirs
                                   , libPath cfg
                                   ]) cfg
in
let installWithBinaries =
  λ(bins : List Text) →
  λ(installVars : types.BuildVars) →
    defaultInstall installVars
      # symlinkBinaries bins
in

let unbounded =
  λ(x : Text) →
    { name = x
    , bound = types.VersionBound.NoBound {=}
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

let cmakeConfigure =
  λ(cfg : types.BuildVars) →
    let host =
      Optional/fold types.TargetTriple cfg.targetTriple (List Text) (λ(tgt : types.TargetTriple) → ["-DCMAKE_C_COMPILER=${printTargetTriple tgt}-gcc"]) ([] : List Text)
    in

    [ createDir "build"
    , call { program = "cmake"
           , arguments = [ "../", "-DCMAKE_INSTALL_PREFIX:PATH=${cfg.installDir}" ] # host
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
  λ(_ : types.BuildVars) →
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
    }
in

let autogenConfigure =
  λ(cfg : types.BuildVars) →
    [ mkExe "autogen.sh"
    , call (defaultCall ⫽ { program = "./autogen.sh"
                          -- ACLOCAL_PATH ??
                          })
    ] # defaultConfigure cfg
in

let fullVersion =
  λ(x : { version : List Natural, patch : Natural }) →
    x.version # [x.patch]
in

{- meson build helpers -}

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

let mesonCfgFile =
  λ(cfg : types.BuildVars) →
    let prefix =
      Optional/fold types.TargetTriple cfg.targetTriple Text
        (λ(tgt : types.TargetTriple) → "${printTargetTriple tgt}-")
          ""
    in

    "[binaries]\n" ++
    "c = '${prefix}gcc'\n" ++ -- FIXME: default to cc/c++ when no cfg is passed
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

let mesonConfigureWithFlags =
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
           , environment = Some [ mkPkgConfigVar cfg.linkDirs
                                , { var = "PATH", value = mkPathVar cfg.binDirs ++ "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" }
                                , mkPy3Path cfg.linkDirs
                                , libPath cfg
                                , mkLDFlags cfg.linkDirs
                                , mkCFlags cfg.includeDirs
                                , mkPkgConfigVar cfg.linkDirs
                                ]
           , procDir = Some "build"
           }
    ]

in

let mesonConfigure =
  mesonConfigureWithFlags ([] : List Text)
in

let ninjaBuild =
  λ(cfg : types.BuildVars) →
    [ call (defaultCall ⫽ { program = "ninja"
                          , environment = Some [ mkPkgConfigVar cfg.linkDirs
                                               , { var = "PATH", value = mkPathVar cfg.binDirs ++ ":/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" }
                                               , mkPy3Path cfg.linkDirs
                                               , libPath cfg
                                               , mkLDFlags cfg.linkDirs
                                               , mkCFlags cfg.includeDirs
                                               , mkPkgConfigVar cfg.linkDirs
                                               ]
                          , procDir = Some "build" }) ]
in

let ninjaInstall =
  λ(cfg : types.BuildVars) →
    [ call (defaultCall ⫽ { program = "ninja"
                          , environment = Some [ mkPkgConfigVar cfg.linkDirs
                                               , { var = "PATH", value = mkPathVar cfg.binDirs ++ ":/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" }
                                               , mkPy3Path cfg.linkDirs
                                               , libPath cfg
                                               , mkLDFlags cfg.linkDirs
                                               , mkCFlags cfg.includeDirs
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
  λ(version : List Natural) →
  λ(x : { name : Text, version : List Natural }) →
    let major = Optional/fold Natural (List/head Natural version) Text (Natural/show) ""
    in
    simplePackage x ⫽
      { configureCommand = doNothing
      , buildCommand = pythonBuild version
      , installCommand = pythonInstall version
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

let mkLDPreload =
  λ(libs : List Text) →
    let flag = concatMapSep " " Text (λ(lib : Text) → lib) libs
    in

    { var = "LD_PRELOAD", value = flag }
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
                            , mkCFlags cfg.includeDirs
                            , mkPkgConfigVar cfg.linkDirs
                            , libPath cfg
                            , mkXdgDataDirs cfg.shareDirs
                            , mkLDPreload cfg.preloadLibs
                            ])
in

let preloadCfg =
  generalConfigure preloadEnv "configure" ([] : List Text) ([] : List Text)
in

let printEnvVar =
  λ(var : types.EnvVar) →
    "${var.var}=${var.value}"
in

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
, mkExe               = mkExe -- TODO: rename this so it's not so confusing
, mkExes              = mkExes
, createDir           = createDir
, mkHost              = mkHost
, defaultConfigure    = defaultConfigure
, defaultBuild        = defaultBuild
, defaultInstall      = defaultInstall
, cmakeConfigure      = cmakeConfigure
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
, mkStaPath           = mkStaPath
, libPath             = libPath
, mkPyPath            = mkPyPath
, mkPy3Path           = mkPy3Path
, mkIncludePath       = mkIncludePath
, isUnix              = isUnix
, defaultPath         = defaultPath
, simplePackage       = simplePackage
, symlinkBinary       = symlinkBinary
, symlink             = symlink
, symlinkBinaries     = symlinkBinaries
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
, preloadEnv          = preloadEnv
, preloadCfg          = preloadCfg
, printEnvVar         = printEnvVar
, mkPyWrapper         = mkPyWrapper
, mkPy3Wrapper        = mkPy3Wrapper
, mkPy2Wrapper        = mkPy2Wrapper
, installWithPyWrappers = installWithPyWrappers
, installWithPy3Wrappers = installWithPy3Wrappers
}
