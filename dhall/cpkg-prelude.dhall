let concatMapSep = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/Text/concatMapSep
in

let showVersion =
  λ(x : List Natural) → concatMapSep "." Natural Natural/show x
in

{ showVersion = showVersion
}
