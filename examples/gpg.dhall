let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

-- N.B. this doesn't work yet but it will once dependency resolution does
let curl =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "gnupg", version = v } ⫽
      { pkgUrl = "https://gnupg.org/ftp/gcrypt/gnupg/gnupg-${prelude.showVersion v}.tar.bz2"
      , pkgDeps = [ prelude.lowerBound { name = "npth", lower = [1,2] }
                  , prelude.unbounded "libgpg-error"
                  , prelude.unbounded "libgcrypt"
                  , prelude.unbounded "libassuan"
                  , prelude.unbounded "libksba"
                  ]
      }
in

curl [2,2,11]
