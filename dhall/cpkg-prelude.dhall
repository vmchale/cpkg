let concatMapSep = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Text/concatMapSep
in

let types = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-types.dhall
in

let Command = < CreateDirectory : { dir : Text }
              | MakeExecutable : { file : Text }
              | Call : { program : Text
                       , arguments : List Text
                       , environment : Optional (List types.EnvVar)
                       , procDir : Optional Text
                       }
              >
in

let showVersion =
  λ(x : List Natural) → concatMapSep "." Natural Natural/show x
in

let mkTarget =
  λ(x : Optional Text) →
    Optional/fold Text x Text (λ(tgt : Text) → "--target=${tgt}") ""
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
    Command.MakeExecutable { file = x }
in

let defaultConfigure =
  λ(cfg : types.ConfigureVars) →
    [ mkExe "configure"
    , Command.Call { program = "./configure"
                   , arguments = [ "--prefix=${cfg.installDir}"
                                 , mkTarget cfg.targetTriple
                                 ]
                   , environment = [] : Optional (List types.EnvVar)
                   , procDir = [] : Optional Text
                   }
    ]
in

let defaultBuild =
  λ(cfg : types.BuildVars) →
    [ Command.Call { program = makeExe cfg.buildOS
                   , arguments = [ "-j${Natural/show cfg.cpus}" ]
                   , environment = [] : Optional (List types.EnvVar)
                   , procDir = [] : Optional Text
                   }
    ]
in

let defaultInstall =
  λ(os : types.OS) →
    [ Command.Call { program = makeExe os
                   , arguments = [ "install" ]
                   , environment = [] : Optional (List types.EnvVar)
                   , procDir = [] : Optional Text
                   }
    ]
in

let unbounded =
  λ(x : Text) →
    { name = x
    , bound = types.VersionBound.NoBound
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

let makeGnuPackage =
  λ(pkg : { name : Text, version : List Natural}) →
    defaultPackage ⫽
      { pkgName = pkg.name
      , pkgVersion = pkg.version
      , pkgUrl = "https://mirrors.ocf.berkeley.edu/gnu/lib${pkg.name}/lib${pkg.name}-${showVersion pkg.version}.tar.xz"
      , pkgSubdir = "lib${pkg.name}-${showVersion pkg.version}"
      }
in

let createDir =
  λ(x : Text) →
    Command.CreateDirectory { dir = x }
in

{ showVersion       = showVersion
, makeGnuPackage    = makeGnuPackage
, defaultPackage    = defaultPackage
, unbounded         = unbounded
, makeExe           = makeExe
, printArch         = printArch
, printManufacturer = printManufacturer
, call              = Command.Call
, mkExe             = mkExe
, createDir         = createDir
, mkTarget          = mkTarget
, defaultConfigure  = defaultConfigure
, defaultBuild      = defaultBuild
, defaultInstall    = defaultInstall
}
