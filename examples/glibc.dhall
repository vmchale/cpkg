let types = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-types.dhall
in

let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let buildDir =
  [ "build" ] : Optional Text
in

let glibcConfigure =
  λ(cfg : types.ConfigureVars) →
    prelude.mkExes
      [ "configure", "scripts/mkinstalldirs", "scripts/rellns-sh" ]
        # [ prelude.createDir "build"
          , prelude.call { program = "../configure"
                         , arguments = [ "--prefix=${cfg.installDir}" ]
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
  λ(os : types.OS) →
    [ prelude.call { program = prelude.makeExe os
                   , arguments = [ "install" ]
                   , environment = prelude.defaultEnv
                   , procDir = buildDir
                   }
    ]
in

let glibc =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "glibc"
      , pkgVersion = v
      , pkgUrl = "http://mirror.keystealth.org/gnu/libc/glibc-${prelude.showVersion v}.tar.xz"
      , pkgSubdir = "glibc-${prelude.showVersion v}"
      , configureCommand = glibcConfigure
      , buildCommand = glibcBuild
      , installCommand = glibcInstall
      }
in

glibc [2,28]
