let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let curl =
  λ(v : List Natural) →
    prelude.makeGnuExe { name = "npth", version = v } ⫽
      { pkgUrl = "https://gnupg.org/ftp/gcrypt/npth/npth-${prelude.showVersion v}.tar.bz2"
      }
in

curl [1,6]
