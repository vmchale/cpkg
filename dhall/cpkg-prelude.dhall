{- Dhall prelude imports -}
let concatMapSep = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Text/concatMapSep
in

let concatMap = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Text/concatMap
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

let mkHost =
  mapOptional Text Text (λ(tgt : Text) → "--host=${tgt}")
in

let mkHostEnv =
  mapOptional Text Text (λ(tgt : Text) → "CHOST=${tgt}")
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
      , NoOs      = false -- bad but this should never happen
      }
      os
in

let mkLDFlags =
  λ(libDirs : List Text) →
    let flag = concatMap Text (λ(dir : Text) → "-L${dir} ") libDirs
    in

    { var = "LDFLAGS", value = flag }
in

let mkCFlags =
  λ(libDirs : List Text) →
    let flag = concatMap Text (λ(dir : Text) → "-I${dir} ") libDirs
    in

    { var = "CPPFLAGS", value = flag }
in

let mkPkgConfigVar =
  λ(libDirs : List Text) →
    let flag = concatMap Text (λ(dir : Text) → "${dir}/pkgconfig:") libDirs
    in

    { var = "PKG_CONFIG_PATH", value = flag }
in


let mkPathVar =
  λ(binDirs : List Text) →
    concatMap Text (λ(dir : Text) → "${dir}:") binDirs
in

let defaultPath =
  λ(cfg : types.ConfigureVars) →
    if isUnix cfg.configOS
      then [ { var = "PATH", value = mkPathVar cfg.binDirs ++ "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" } ] : List types.EnvVar
      else [] : List types.EnvVar
in

let generalConfigure =
  λ(filename : Text) →
  λ(extraFlags : List Text) →
  λ(cfg : types.ConfigureVars) →
    let maybeHost = mkHost cfg.targetTriple
    in
    let modifyArgs = maybeAppend Text maybeHost
    in

    [ mkExe filename
    , call (defaultCall ⫽ { program = "./${filename}"
                          , arguments = modifyArgs [ "--prefix=${cfg.installDir}" ] # extraFlags
                          , environment =
                              Some (defaultPath cfg # [ mkLDFlags cfg.linkDirs, mkCFlags cfg.includeDirs, mkPkgConfigVar cfg.linkDirs ])
                          })
    ]
in

let defaultConfigure =
  generalConfigure "configure" ([] : List Text)
in

let configureWithFlags =
    generalConfigure "configure"
in

let bigConfigure =
  generalConfigure "Configure" ([] : List Text)
in

let configureMkExesExtraFlags =
  λ(x : { bins : List Text, extraFlags : List Text }) →
  λ(cfg : types.ConfigureVars) →
    mkExes x.bins
      # configureWithFlags x.extraFlags cfg
in

let configureMkExes =
  λ(bins : List Text) →
    configureMkExesExtraFlags { bins = bins, extraFlags = ([] : List Text) }
in

let defaultBuild =
  λ(cfg : types.BuildVars) →
    [ call (defaultCall ⫽ { program = makeExe cfg.buildOS
                          , arguments = [ "-j${Natural/show cfg.cpus}" ] })
    ]
in

let defaultInstall =
  λ(cfg : types.InstallVars) →
    [ call (defaultCall ⫽ { program = makeExe (cfg.installOS)
                          , arguments = [ "install" ] })
    ]
in

let installWithBinaries =
  λ(bins : List Text) →
  λ(installVars : types.InstallVars) →
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
  λ(cfg : types.ConfigureVars) →
    let host =
      Optional/fold Text cfg.targetTriple (List Text) (λ(tgt : Text) → ["-DCMAKE_C_COMPILER=${tgt}-gcc"]) ([] : List Text)
    in

    [ createDir "build"
    , call { program = "cmake"
           , arguments = [ "../", "-DCMAKE_INSTALL_PREFIX:PATH=${cfg.installDir}" ] # host
           , environment = defaultEnv
           , procDir = Some "build"
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
  λ(_ : types.InstallVars) →
    [ call { program = "cmake"
           , arguments = [ "--build", ".", "--target", "install", "--config", "Release" ]
           , environment = defaultEnv
           , procDir = Some "build"
           }
    ]
in

let cmakeInstallWithBinaries =
  λ(bins : List Text) →
  λ(installVars : types.InstallVars) →
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
  λ(cfg : types.ConfigureVars) →
    [ mkExe "autogen.sh"
    , call (defaultCall ⫽ { program = "./autogen.sh"
                          -- https://www.gnu.org/software/automake/manual/html_node/Macro-Search-Path.html
                          -- ACLOCAL_PATH / ACLOCAL_FLAGS ??
                          })
    ] # defaultConfigure cfg
in

let fullVersion =
  λ(x : { version : List Natural, patch : Natural }) →
    x.version # [x.patch]
in

{- meson build helpers -}

let mesonConfigure =
  λ(cfg : types.ConfigureVars) →

    [ createDir "build"
    , call { program = "meson"
           , arguments = [ "--prefix=${cfg.installDir}", ".." ]
           , environment = Some [ mkPkgConfigVar cfg.linkDirs
                                , { var = "PATH", value = mkPathVar cfg.binDirs ++ "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" }
                                ]
           , procDir = Some "build"
           }
    ]

in

let ninjaBuild =
  λ(cfg : types.BuildVars) →
    [ call (defaultCall ⫽ { program = "ninja", procDir = Some "build" }) ]
in

let ninjaInstall =
  λ(cfg : types.InstallVars) →
    [ call (defaultCall ⫽ { program = "ninja"
                          , arguments = [ "install" ]
                          , procDir = Some "build"
                          })
    ]
in

let copyFiles =
  map { src : Text, dest : Text } types.Command types.Command.CopyFile
in

let ninjaInstallWithPkgConfig =
  λ(fs : List { src : Text, dest : Text }) →
  λ(cfg : types.InstallVars) →
    ninjaInstall cfg # copyFiles fs
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
, isUnix              = isUnix
, defaultPath         = defaultPath
, simplePackage       = simplePackage
, symlinkBinary       = symlinkBinary
, symlinkBinaries     = symlinkBinaries
, installWithBinaries = installWithBinaries
, configureMkExes     = configureMkExes
, bigConfigure        = bigConfigure
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
, ninjaBuild          = ninjaBuild
, ninjaInstall        = ninjaInstall
, ninjaInstallWithPkgConfig = ninjaInstallWithPkgConfig
}
