let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let types = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-types.dhall
in

let gitConfigure =
  λ(cfg : types.ConfigureVars) →
    prelude.defaultConfigure cfg # prelude.mkExes [ "check_bindir" ]
in

let git =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "git"
      , pkgVersion = v
      , pkgUrl = "https://mirrors.edge.kernel.org/pub/software/scm/git/git-${prelude.showVersion v}.tar.xz"
      , pkgSubdir = "git-${prelude.showVersion v}"
      , configureCommand = gitConfigure
      }
in

git [2,19,2]
