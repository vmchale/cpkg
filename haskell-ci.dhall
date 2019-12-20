let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:049b967041f7959e86ef70d1d6f82cc826602adfb97839c1e73160a3b55d4249

let installLibarchive =
      haskellCi.BuildStep.Name
        { name = "Install libarchive"
        , run =
            ''
            wget https://www.libarchive.org/downloads/libarchive-3.4.0.tar.gz
            tar xvf libarchive-3.4.0.tar.gz
            cd libarchive-3.4.0
            ./configure
            make -j
            sudo make install
            ''
        }

in    haskellCi.generalCi
        ([ installLibarchive ] # haskellCi.matrixSteps)
        ( Some
            { ghc = [ haskellCi.GHC.GHC844, haskellCi.GHC.GHC865 ]
            , cabal = [ haskellCi.Cabal.Cabal30 ]
            }
        )
    : haskellCi.CI.Type
