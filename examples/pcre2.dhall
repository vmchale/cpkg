let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let pcre2 =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "pcre2"
      , pkgVersion = v
      , pkgUrl = "https://ftp.pcre.org/pub/pcre/pcre2-${prelude.showVersion v}.tar.gz"
      , pkgSubdir = "pcre2-${prelude.showVersion v}"
      }
in

pcre2 [10,32]
