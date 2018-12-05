{- Dhall prelude imports -}
let concatMapSep = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Text/concatMapSep
in

let map = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/List/map
in

let mapOptional = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Optional/map
in

let types = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-types.dhall
in

let showVersion =
  λ(x : List Natural) → concatMapSep "." Natural Natural/show x
in

let mkHost =
  λ(x : Optional Text) →
    mapOptional Text Text (λ(tgt : Text) → "--host=${tgt}") x
in

let mkHostEnv =
  λ(x : Optional Text) →
    mapOptional Text Text (λ(tgt : Text) → "CHOST=${tgt}") x
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
      , PowerPc     = λ(_ : {}) → "powerpc"
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
  λ(xs : List Text) →
    map Text types.Command mkExe xs
in

let defaultEnv =
  [] : Optional (List types.EnvVar)
in

let defaultCall =
  { arguments = [] : List Text
  , environment = defaultEnv
  , procDir = [] : Optional Text
  }
in

let call =
  λ(proc : types.Proc) →
    types.Command.Call proc
in

let defaultConfigure =
  λ(cfg : types.ConfigureVars) →
    let maybeHost = mkHost cfg.targetTriple
    in
    let modifyArgs = λ(xs : List Text) → maybeAppend Text maybeHost xs
    in

    [ mkExe "configure"
    , call (defaultCall ⫽ { program = "./configure"
                          , arguments = modifyArgs [ "--prefix=${cfg.installDir}" ] })
    ]
in

let defaultBuild =
  λ(cfg : types.BuildVars) →
    [ call (defaultCall ⫽ { program = makeExe cfg.buildOS
                          , arguments = [ "-j${Natural/show cfg.cpus}" ] })
    ]
in

let defaultInstall =
  λ(os : types.OS) →
    [ call (defaultCall ⫽ { program = makeExe os
                          , arguments = [ "install" ] })
    ]
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

let defaultPackage =
  { configureCommand = defaultConfigure
  , buildCommand     = defaultBuild
  , installCommand   = defaultInstall
  , pkgBuildDeps     = [] : List types.Dep
  , pkgDeps          = [] : List types.Dep
  }
in

let makeGnuCommon =
  λ(pkg : { name : Text, version : List Natural}) →
    defaultPackage ⫽
      { pkgName = pkg.name
      , pkgVersion = pkg.version
      }
in


let makeGnuExe =
  λ(pkg : { name : Text, version : List Natural}) →
    makeGnuCommon pkg ⫽
      { pkgUrl = "https://ftp.gnu.org/gnu/${pkg.name}/${pkg.name}-${showVersion pkg.version}.tar.xz"
      , pkgSubdir = "${pkg.name}-${showVersion pkg.version}"
      }
in

let makeGnuLibrary =
  λ(pkg : { name : Text, version : List Natural}) →
    makeGnuCommon pkg ⫽
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
    [ createDir "build"
    , call { program = "cmake"
           , arguments = [ "../", "-DCMAKE_INSTALL_PREFIX:PATH=${cfg.installDir}" ]
           , environment = defaultEnv
           , procDir = [ "build" ] : Optional Text
           }
    ]
in

let cmakeBuild =
  λ(cfg : types.BuildVars) →
    [ call { program = "cmake"
           , arguments = [ "--build", ".", "--config", "Release", "--", "-j", Natural/show cfg.cpus ]
           , environment = defaultEnv
           , procDir = [ "build" ] : Optional Text
           }
    ]
in

let cmakeInstall =
  λ(os : types.OS) →
    [ call { program = "cmake"
           , arguments = [ "--build", ".", "--target", "install", "--config", "Release" ]
           , environment = defaultEnv
           , procDir = [ "build" ] : Optional Text
           }
    ]
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
                          , arguments = [] : List Text })
    ] # defaultConfigure cfg
in

{ showVersion       = showVersion
, makeGnuLibrary    = makeGnuLibrary
, makeGnuExe        = makeGnuExe
, defaultPackage    = defaultPackage
, unbounded         = unbounded
, lowerBound        = lowerBound
, makeExe           = makeExe
, printArch         = printArch
, printManufacturer = printManufacturer
, call              = call
, mkExe             = mkExe -- TODO: rename this so it's not so confusing
, mkExes            = mkExes
, createDir         = createDir
, mkHost            = mkHost
, defaultConfigure  = defaultConfigure
, defaultBuild      = defaultBuild
, defaultInstall    = defaultInstall
, cmakeConfigure    = cmakeConfigure
, cmakeBuild        = cmakeBuild
, cmakeInstall      = cmakeInstall
, cmakePackage      = cmakePackage
, autogenConfigure  = autogenConfigure
, defaultCall       = defaultCall
, defaultEnv        = defaultEnv
, maybeAppend       = maybeAppend
}
