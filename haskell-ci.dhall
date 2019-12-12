let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:992e2717c6ffae819f5c3165346e3530e2bd781c85562a9c539e2b1123749aa0

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
