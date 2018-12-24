let types = ../dhall/cpkg-types.dhall
in

let prepkg = ./pre-pkg-set.dhall
in

prepkg (λ(_ : types.BuildVars) → env:HOME as Text)
