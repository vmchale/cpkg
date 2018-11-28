{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Package.C.Dhall.Type ( CPkg (..)
                            , ConfigureVars (..)
                            , BuildVars (..)
                            ) where

import qualified Data.Text   as T
import           Dhall
import           GHC.Natural (Natural)

data ConfigureVars = ConfigureVars { installDir  :: T.Text
                                   , includeDirs :: [ T.Text ]
                                   } deriving (Generic, Inject)

newtype BuildVars = BuildVars { cpus :: Natural }
                  deriving newtype Inject

data CPkg = CPkg { pkgName          :: T.Text
                 , pkgVersion       :: [ Natural ]
                 , pkgUrl           :: T.Text
                 , pkgSubdir        :: T.Text
                 , configureCommand :: ConfigureVars -> [ T.Text ]
                 , executableFiles  :: [ T.Text ]
                 , buildCommand     :: BuildVars -> [ T.Text ]
                 , installCommand   :: [ T.Text ]
                 } deriving (Generic, Interpret)
