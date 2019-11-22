{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Package.C.Triple.Type ( TargetTriple (..)
                             , Manufacturer (..)
                             , Arch (..)
                             , ABI (..)
                             , OS (..)
                             ) where

import           CPkgPrelude
import           Data.Text.Prettyprint.Doc.Custom

-- TODO: are the derived Binary instances inefficient? possibly replace them
-- with handwritten instances...
data TargetTriple = TargetTriple { arch         :: Arch
                                 , manufacturer :: Maybe Manufacturer
                                 , os           :: OS
                                 , abi          :: Maybe ABI
                                 }
                                 deriving (Eq, Ord, Hashable, Binary, Generic, ToDhall)

instance Pretty TargetTriple where
    pretty (TargetTriple a (Just m) o (Just ab)) = dashed [pretty a, pretty m, pretty o, pretty ab]
    pretty (TargetTriple a Nothing o (Just ab))  = dashed [pretty a, pretty o, pretty ab]
    pretty (TargetTriple a (Just m) o Nothing)   = dashed [pretty a, pretty m, pretty o]
    pretty (TargetTriple a Nothing o Nothing)    = dashed [pretty a, pretty o]

instance Show TargetTriple where
    show = show . pretty

data Manufacturer = Unknown
                  | Apple
                  | IBM
                  | PC
                  deriving (Eq, Ord, Hashable, Binary, Generic, ToDhall)

instance Pretty Manufacturer where
    pretty Unknown = "unknown"
    pretty Apple   = "apple"
    pretty IBM     = "ibm"
    pretty PC      = "pc"

data Arch = X64
          | AArch
          | Arm
          | RISCV64
          | PowerPC
          | PowerPC64
          | PowerPC64le
          | Sparc64
          | S390x
          | Alpha
          | M68k
          | Mips
          | MipsEl
          | Mips64
          | Mips64El
          | X86
          | SH4
          | HPPA
          | HPPA64
          | MipsIsa32r6El
          | MipsIsa32r6
          | MipsIsa64r6El
          | MipsIsa64r6
          deriving (Eq, Ord, Hashable, Binary, Generic, ToDhall)

instance Pretty Arch where
    pretty X64           = "x86_64"
    pretty AArch         = "aarch64"
    pretty Arm           = "arm"
    pretty RISCV64       = "riscv64"
    pretty PowerPC       = "powerpc"
    pretty PowerPC64     = "powerpc64"
    pretty PowerPC64le   = "powerpc64le"
    pretty Sparc64       = "sparc64"
    pretty S390x         = "s390x"
    pretty Alpha         = "alpha"
    pretty M68k          = "m68k"
    pretty Mips          = "mips"
    pretty MipsEl        = "mipsel"
    pretty Mips64        = "mips64"
    pretty Mips64El      = "mips64el"
    pretty X86           = "i686"
    pretty SH4           = "sh4"
    pretty HPPA          = "hppa"
    pretty HPPA64        = "hppa64"
    pretty MipsIsa32r6El = "mipsisa32r6el"
    pretty MipsIsa32r6   = "mipsisa32r6"
    pretty MipsIsa64r6El = "mipsisa64r6el"
    pretty MipsIsa64r6   = "mipsisa64r6"

data ABI = GNU
         | GNUabi64
         | GNUeabi
         | GNUeabihf
         | GNUspe
         | MinGw
         deriving (Eq, Ord, Hashable, Binary, Generic, ToDhall)

instance Pretty ABI where
    pretty GNU       = "gnu"
    pretty GNUabi64  = "gnuabi64"
    pretty GNUeabi   = "gnueabi"
    pretty GNUeabihf = "gnueabihf"
    pretty GNUspe    = "gnuspe"
    pretty MinGw     = "mingw32"

data OS = Darwin
        | Dragonfly
        | FreeBSD
        | Linux
        | OpenBSD
        | NetBSD
        | Solaris
        | Windows
        | Redox
        | Haiku
        | IOS
        | AIX
        | Hurd
        | Android
        | NoOs
        deriving (Eq, Ord, Hashable, Binary, Generic, ToDhall)
        -- IRIX? HP UX?

instance Pretty OS where
    pretty Darwin    = "darwin"
    pretty Dragonfly = "dragonfly"
    pretty FreeBSD   = "freebsd"
    pretty Linux     = "linux"
    pretty OpenBSD   = "openbsd"
    pretty NetBSD    = "netbsd"
    pretty Solaris   = "solaris"
    pretty Windows   = "w64"
    pretty Redox     = "redox"
    pretty Haiku     = "haiku"
    pretty IOS       = "ios"
    pretty AIX       = "aix"
    pretty Hurd      = "hurd"
    pretty Android   = "android"
    pretty NoOs      = "none"
