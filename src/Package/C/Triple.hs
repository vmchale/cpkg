{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Package.C.Triple ( TargetTriple (..)
                        , Manufacturer (..)
                        , Arch (..)
                        , ABI (..)
                        ) where

import           CPkgPrelude
import           Package.C.Type.Shared (OS)

data TargetTriple = TargetTriple { arch         :: Arch
                                 , manufacturer :: Maybe Manufacturer
                                 , os           :: OS
                                 , abi          :: Maybe ABI
                                 }
                                 deriving (Generic, Inject)

data Manufacturer = Unknown
                  | Apple
                  | IBM
                  | PC
                  deriving (Generic, Inject)

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
          deriving (Generic, Inject)

data ABI = GNU
         | GNUeabi
         | GNUeabihf
         | GNUspe
         deriving (Generic, Inject)
