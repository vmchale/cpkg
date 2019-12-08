
let ats-temptory =
  let temptoryInstall =
    λ(cfg : types.BuildVars) →
      let patsVar = { var = "PATSHOME", value = "/home/vanessa/.cpkg/ats-0.3.13-abaa1d83fb4d996/lib/ats2-postiats-0.3.13" } in
      let buildDir = cfg.currentDir ++ "/ATS-Temptory-gmp-0.0.0" in
      [ prelude.call (prelude.defaultCall ⫽ { program = prelude.makeExe cfg.buildOS
                                            , arguments = [ "CFLAGS=${(prelude.mkCFlags cfg).value} -I${buildDir}/srcgen/CBOOT/ccomp/runtime -I${buildDir}/srcgen/CBOOT"
                                                          , "LDFLAGS='${(prelude.mkLDFlags cfg.linkDirs).value}'"
                                                          , "TEMPTORY=${buildDir}"
                                                          , "TEMPTORYLIBHOME=${cfg.installDir}"
                                                          ]
                                            , environment = Some ([ { var = "PATH", value = (prelude.unixPath cfg.binDirs).value ++ ":${buildDir}/bin" }
                                                                  , patsVar
                                                                  ])
                                            })
      ]
  in
  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "ATS-Temptory", version = v } ⫽
      { pkgUrl = "https://github.com/sparverius/Temptory-Release/releases/download/v${versionString}/ATS-Temptory-gmp-${versionString}.tgz"
      , pkgSubdir = "ATS-Temptory-gmp-0.0.0"
      , pkgBuildDeps = [ prelude.unbounded "ats" ]
      , pkgDeps = [ prelude.unbounded "gmp" ]
      , buildCommand = prelude.doNothing
      , installCommand = temptoryInstall
      }
in










let ats-temptory =
  let temptoryBuild =
    λ(cfg : types.BuildVars) →
      let buildDir = cfg.currentDir ++ "/ATS-Temptory-gmp-0.0.0" in
      [ prelude.call (prelude.defaultCall ⫽ { program = prelude.makeExe cfg.buildOS
                                            , arguments = [ "CFLAGS=${(prelude.mkCFlags cfg).value} -I${buildDir}/srcgen/CBOOT/ccomp/runtime -I${buildDir}/srcgen/CBOOT"
                                                          , "LDFLAGS=${(prelude.mkLDFlags cfg.linkDirs).value}"
                                                          , "-j${Natural/show cfg.cpus}"
                                                          , "PREFIX=${cfg.installDir}"
                                                          ]
                                            })
      ]
  in

  λ(v : List Natural) →
    let versionString = prelude.showVersion v in
    prelude.simplePackage { name = "ATS-Temptory", version = v } ⫽
      { pkgUrl = "https://github.com/sparverius/Temptory-Release/releases/download/v${versionString}/ATS-Temptory-gmp-${versionString}.tgz"
      , pkgSubdir = "ATS-Temptory-gmp-0.0.0"
      , pkgDeps = [ prelude.unbounded "gmp" ]
      , buildCommand = temptoryBuild
      }
in

