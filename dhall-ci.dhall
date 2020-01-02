let dhallCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/dhall-ci.dhall sha256:0ef11bbce3ff55ed7c0320be282e4a547844539b0a7de3c54eb3fdec84090c8b

in      dhallCi.dhallCi
          [ "pkgs/pkg-set.dhall"
          , "dhall/cpkg-prelude.dhall"
          , "dhall/cpkg-types.dhall"
          , "dhall-ci.dhall"
          , "haskell-ci.dhall"
          ]
      ⫽ { on = [ dhallCi.Event.push, dhallCi.Event.pull_request ] }
    : dhallCi.CI
