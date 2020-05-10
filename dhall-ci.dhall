let dhallCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/dhall-ci.dhall sha256:e5a66a2eed19fea8ee8d397ea25218b32fbab2abed758d31b7a2fe0fdea6f433

in      dhallCi.dhallCi
          [ "pkgs/pkg-set.dhall"
          , "dhall/cpkg-prelude.dhall"
          , "dhall/cpkg-types.dhall"
          , "dhall-ci.dhall"
          , "haskell-ci.dhall"
          ]
      ⫽ { on = [ dhallCi.Event.push, dhallCi.Event.pull_request ] }
    : dhallCi.CI
