let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let types = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-types.dhall
in

let rubyConfigure =
  λ(cfg : types.ConfigureVars) →
    prelude.mkExes [ "tool/ifchange" ]
      # prelude.defaultConfigure cfg
in

let ruby =
  λ(cfg : { version : List Natural, patch : Natural }) →
    let patchString = Natural/show cfg.patch
    in
    let versionString = prelude.showVersion cfg.version
    in

    prelude.defaultPackage ⫽
      { pkgName = "ruby"
      , pkgVersion = cfg.version
      , pkgUrl = "https://cache.ruby-lang.org/pub/ruby/${versionString}/ruby-${versionString}.${patchString}.tar.gz"
      , pkgSubdir = "ruby-${versionString}.${patchString}"
      , configureCommand = rubyConfigure
      }
in

ruby { version = [2,5], patch = 3 }
