let prelude = https://raw.githubusercontent.com/vmchale/cpkg/master/dhall/cpkg-prelude.dhall
in

let dbus =
  λ(v : List Natural) →
    prelude.defaultPackage ⫽
      { pkgName = "dbus"
      , pkgVersion = v
      , pkgUrl = "https://dbus.freedesktop.org/releases/dbus/dbus-${prelude.showVersion v}.tar.gz"
      , pkgSubdir = "dbus-${prelude.showVersion v}"
      }
in

dbus [1,12,10]
