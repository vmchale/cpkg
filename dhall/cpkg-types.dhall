let OS = < FreeBSD : {}
         | OpenBSD : {}
         | NetBSD : {}
         | Solaris : {}
         | Dragonfly : {}
         | Linux : {}
         | Darwin : {}
         | Windows : {}
         >
in

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
}
