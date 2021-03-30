{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module Common.Conduit.Api.Packages.Packages where

import           Data.Aeson                             (FromJSON (..),
                                                         ToJSON (..))
import           GHC.Generics                           (Generic)

import           Common.Conduit.Api.Packages.Package (Package)

data Packages = Packages
  { packages      :: [Package]
  , packagesCount :: Int
  } deriving Show

fromList :: [Package] -> Packages
fromList = Packages <$> id <*> length

deriving instance Generic Packages
deriving instance ToJSON Packages
deriving instance FromJSON Packages
