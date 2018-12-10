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
         | NoOs : {}
         >
in

let Arch = < X64 : {}
           | AArch : {}
           | Arm : {}
           | RISCV64 : {}
           | PowerPc : {}
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
           | HPPA : {} >
in

let Manufacturer = < Unknown : {}
                   | Apple : {}
                   | IBM : {}
                   | PC : {}
                   >
in

let ABI = < GNU : {}
          | Eabi : {}
          | GNUeabi : {}
          | GNUeabihf : {}
          >
in

let TargetTriple = { arch : Arch
                   , manufacturer : Optional Manufacturer
                   , os : OS
                   , abi : Optional ABI
                   }
in

let ConfigureVars = { installDir : Text
                    , targetTriple : Optional Text
                    , includeDirs : List Text
                    , linkDirs : List Text
                    , binDirs : List Text
                    , configOS : OS
                    }
in

let BuildVars = { cpus : Natural
                , buildOS : OS
                }
in

let InstallVars = { installPath : Text
                  , installOS : OS
                  }

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
              >
in

{ OS            = OS
, ConfigureVars = ConfigureVars
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
, InstallVars   = InstallVars
}
