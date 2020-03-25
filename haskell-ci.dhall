let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:2de95c8bd086c21660c2849dfe2d9af72e675bed44396159d647292d329a20e4

let concatMapSep =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/9f259cd68870b912fbf2f2a08cd63dc3ccba9dc3/Prelude/Text/concatMapSep sha256:c272aca80a607bc5963d1fcb38819e7e0d3e72ac4d02b1183b1afb6a91340840

let showVersion = concatMapSep "." Natural Natural/show

let installLibarchive =
        λ(v : List Natural)
      → let versionString = showVersion v

        in  haskellCi.BuildStep.Name
              { name = "Install libarchive"
              , run =
                  ''
                  wget https://www.libarchive.org/downloads/libarchive-${versionString}.tar.gz
                  tar xvf libarchive-${versionString}.tar.gz
                  cd libarchive-${versionString}
                  ./configure
                  make -j
                  sudo make install
                  ''
              }

in    haskellCi.generalCi
        ([ installLibarchive [ 3, 4, 2 ] ] # haskellCi.matrixSteps)
        ( Some
            { ghc =
              [ haskellCi.GHC.GHC844
              , haskellCi.GHC.GHC865
              , haskellCi.GHC.GHC883
              ]
            , cabal = [ haskellCi.Cabal.Cabal30 ]
            }
        )
    : haskellCi.CI.Type
