-- TODO: figure out why this fails
let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let llvm =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "llvm"
      , pkgVersion = v
      , pkgUrl = "http://releases.llvm.org/${prelude.showVersion v}/llvm-${prelude.showVersion v}.src.tar.xz"
      , pkgSubdir = "llvm-${prelude.showVersion v}"
      }
in

llvm [7,0,0]
