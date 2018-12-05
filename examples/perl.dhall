let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let types = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-types.dhall
in

let perlConfigure =
  λ(cfg : types.ConfigureVars) →

    let maybeHost = prelude.mkHost cfg.targetTriple
    in
    let modifyArgs = λ(xs : List Text) → prelude.maybeAppend Text maybeHost xs
    in

    [ prelude.mkExe "Configure"
    , prelude.call { program = "./Configure"
           , arguments = modifyArgs [ "-des", "-Dprefix=${cfg.installDir}" ]
           , environment = [] : Optional (List types.EnvVar)
           , procDir = [] : Optional Text
           }
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
