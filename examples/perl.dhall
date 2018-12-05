let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let types = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-types.dhall
in

let perlConfigure =
  λ(cfg : types.ConfigureVars) →

    [ prelude.mkExe "Configure"
    , prelude.call (prelude.defaultCall ⫽ { program = "./Configure"
                                          , arguments = [ "-des", "-Dprefix=${cfg.installDir}" ]
                                          })
    ]
in

let perl5 =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "perl"
      , pkgVersion = v
      , pkgUrl = "https://www.cpan.org/src/5.0/perl-${prelude.showVersion v}.tar.gz"
      , pkgSubdir = "perl-${prelude.showVersion v}"
      , configureCommand = perlConfigure
      }
in

perl5 [5,28,1]
