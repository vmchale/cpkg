let dhallCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/dhall-ci.dhall sha256:f88d01d5a7f1b3baddd1ba356589fa3723674a2a8ba17e3ae698f6400a31e447

in      dhallCi.dhallCi
          [ "pkgs/pkg-set.dhall"
          , "dhall/cpkg-prelude.dhall"
          , "dhall/cpkg-types.dhall"
          , "dhall-ci.dhall"
          , "haskell-ci.dhall"
          ]
      ⫽ { on =
            [ dhallCi.Event.push
            , dhallCi.Event.pull_request
            ]
        }
    : dhallCi.CI
