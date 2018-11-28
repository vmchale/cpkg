module Main (main) where

import           Package.C

unistring :: CPkg
unistring = CPkg "unistring" "https://mirrors.ocf.berkeley.edu/gnu/libunistring/libunistring-0.9.10.tar.xz" "libunistring-0.9.10" configure [ "configure" ] [ "make" ] [ "make", "install" ]
    where configure (ConfigureVars dir _) = ["./configure", "--prefix=" ++ dir]

main :: IO ()
main = buildCPkg unistring
