module Package.C.Build.OS ( dhallOS
                          ) where

import           Package.C.Type.Shared
import           System.Info           (os)

dhallOS :: OS
dhallOS = case os of
    "freebsd"   -> FreeBSD
    "openbsd"   -> OpenBSD
    "netbsd"    -> NetBSD
    "solaris"   -> Solaris
    "dragonfly" -> Dragonfly
    "linux"     -> Linux
    "darwin"    -> Darwin
    "mingw32"   -> Windows
    _           -> error "unrecognized OS"
