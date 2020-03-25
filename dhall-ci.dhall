let dhallCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/dhall-ci.dhall sha256:2a8e60c47ac16cfe0bd02d9a348b175b841095e62918448ce9b3bf65fe8cfe78

in      dhallCi.dhallCi
          [ "pkgs/pkg-set.dhall"
          , "dhall/cpkg-prelude.dhall"
          , "dhall/cpkg-types.dhall"
          , "dhall-ci.dhall"
          , "haskell-ci.dhall"
          ]
      ⫽ { on = [ dhallCi.Event.push, dhallCi.Event.pull_request ] }
    : dhallCi.CI
