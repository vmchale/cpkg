let dhallCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/dhall-ci.dhall sha256:50c5c1017d3661f47c915e0c64e9735773dc4e22770c07f46bae168a2dffc44d

in      dhallCi.dhallCi
          [ "pkgs/pkg-set.dhall"
          , "dhall/cpkg-prelude.dhall"
          , "dhall/cpkg-types.dhall"
          , "self-ci.dhall"
          ]
      ⫽ { on =
            [ dhallCi.printEvent dhallCi.Event.Push
            , dhallCi.printEvent dhallCi.Event.PullRequest
            ]
        }
    : dhallCi.CI
