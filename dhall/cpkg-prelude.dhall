let showVersion =
  λ(x : List Natural) → concatMapSep "." Natural Natural/show x
in

{ showVersion = showVersion
}
