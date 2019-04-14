module Package.C.Build.OS ( dhallOS
                          , dhallArch
                          ) where

import           Package.C.Triple.Type hiding (arch, os)
import           System.Info           (arch, os)

dhallArch :: Arch
dhallArch = case arch of
    "x86_64" -> X64
    "x86"    -> X86
    "arm"    -> Arm
    _        -> error "unrecognized architecture"

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
