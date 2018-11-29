let OS = < FreeBSD : {}
         | OpenBSD : {}
         | NetBSD : {}
         | Solaris : {}
         | Dragonfly : {}
         | Linux : {}
         | Darwin : {}
         | Windows : {}
         | Redox : {}
         >
in

let Arch = < X64 : {}
           | AArch : {}
           | Arm : {}
           | RISCV64 : {}
           | PowerPC64 : {}
           | PowerPC64le : {}
           | PowerPc : {}
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

let ConfigureVars = { installDir : Text, targetTriple : Optional Text, includeDirs : List Text, configOS : OS }
in

let BuildVars = { cpus : Natural, buildOS : OS }
in

let VersionBound = < Lower : { lower : List Natural }
                   | Upper : { upper : List Natural }
                   | LowerUpper : { lower : List Natural, upper : List Natural }
                   | NoBound : {} >

let Dep = { name : Text, bound : VersionBound }
in

{ OS            = OS
, ConfigureVars = ConfigureVars
, BuildVars     = BuildVars
, VersionBound  = VersionBound
, Dep           = Dep
, Arch          = Arch
}
