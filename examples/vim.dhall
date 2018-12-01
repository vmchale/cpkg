let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let types = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-types.dhall
in

let concatMap = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Text/concatMap
in

let vimConfigure =
  λ(cfg : types.ConfigureVars) →
    prelude.mkExes [ "src/configure", "src/auto/configure", "src/which.sh" ]
      # prelude.defaultConfigure cfg
in
let squishVersion =
  λ(x : List Natural) → concatMap Natural Natural/show x
in

let vim =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "vim"
      , pkgVersion = v
      , pkgUrl = "http://ftp.vim.org/vim/unix/vim-${prelude.showVersion v}.tar.bz2"
      , pkgSubdir = "vim${squishVersion v}"
      , configureCommand = vimConfigure
      }
in

vim [8,1]
