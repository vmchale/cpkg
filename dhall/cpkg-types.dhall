-- TODO: Add AIX
let OS = < FreeBSD : {}
         | OpenBSD : {}
         | NetBSD : {}
         | Solaris : {}
         | Dragonfly : {}
         | Linux : {}
         | Darwin : {}
         | Windows : {}
         | Redox : {}
         | Haiku : {}
         | IOS : {}
         | AIX : {}
         | Hurd : {}
         | Android : {}
         | NoOs : {}
         >
in

let Arch = < X64 : {}
           | AArch : {}
           | Arm : {}
           | RISCV64 : {}
           | PowerPC : {}
           | PowerPC64 : {}
           | PowerPC64le : {}
           | Sparc64 : {}
           | S390x : {}
           | Alpha : {}
           | M68k : {}
           | Mips : {}
           | MipsEl : {}
           | Mips64 : {}
           | Mips64El : {}
           | X86 : {}
           | SH4 : {}
           | HPPA : {}
           | HPPA64 : {}
           >
in

let Manufacturer = < Unknown : {}
                   | Apple : {}
                   | IBM : {}
                   | PC : {}
                   >
in

let ABI = < GNU : {}
          | GNUeabi : {}
          | GNUeabihf : {}
          | GNUspe : {}
          >
in

let TargetTriple = { arch : Arch
                   , manufacturer : Optional Manufacturer
                   , os : OS
                   , abi : Optional ABI
                   }
in

let BuildVars = { installDir : Text
                , currentDir : Text
                , targetTriple : Optional Text
                , includeDirs : List Text
                , preloadLibs : List Text
                , linkDirs : List Text
                , binDirs : List Text
                , buildOS : OS
                , buildArch : Arch
                , static : Bool
                , cpus : Natural
                }
in

let VersionBound = < Lower : { lower : List Natural }
                   | Upper : { upper : List Natural }
                   | LowerUpper : { lower : List Natural, upper : List Natural }
                   | NoBound : {} >
in

let Dep = { name : Text, bound : VersionBound }
in

let EnvVar = { var : Text, value : Text }
in

let Proc = { program : Text
           , arguments : List Text
           , environment : Optional (List EnvVar)
           , procDir : Optional Text
           }
in

let Command = < CreateDirectory : { dir : Text }
              | MakeExecutable : { file : Text }
              | Call : Proc
              | SymlinkBinary : { file : Text }
              | SymlinkLibrary : { file : Text }
              | Symlink : { tgt : Text, linkName : Text }
              | Write : { contents : Text, file : Text }
              | CopyFile : { src : Text, dest : Text }
              >
in

{ OS            = OS
, BuildVars     = BuildVars
, VersionBound  = VersionBound
, Dep           = Dep
, Arch          = Arch
, Manufacturer  = Manufacturer
, ABI           = ABI
, TargetTriple  = TargetTriple
, Command       = Command
, EnvVar        = EnvVar
, Proc          = Proc
}
