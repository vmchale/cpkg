let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let unistring =
  λ(v : List Natural) →
    prelude.makeGnuPackage { name = "unistring", version = v }
in

unistring [0,9,10]
