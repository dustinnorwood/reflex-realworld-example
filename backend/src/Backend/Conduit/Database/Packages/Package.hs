{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns, StandaloneDeriving, TypeFamilies, TypeSynonymInstances                    #-}
module Backend.Conduit.Database.Packages.Package
  ( PackageT(..)
  , Package
  , PackageId
  , PrimaryKey(..)
  ) where

import Prelude hiding (id)

import Data.Text     (Text)
import Data.Time     (UTCTime)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Table (..))
import GHC.Generics  (Generic)

data PackageT f = Package
  { id          :: Columnar f Int
  , slug        :: Columnar f Text
  , title       :: Columnar f Text
  , description :: Columnar f Text
  , image       :: Columnar f Text
  , body        :: Columnar f Text
  , createdAt   :: Columnar f UTCTime
  , updatedAt   :: Columnar f UTCTime
  }

deriving instance Generic (PackageT f)
deriving instance Beamable PackageT

type Package = PackageT Identity

deriving instance Show Package
deriving instance Eq Package
deriving instance Ord Package

instance Table PackageT where
  data PrimaryKey PackageT f = PackageId
    { unPackageId :: Columnar f Int
    }
  primaryKey = PackageId . id

deriving instance Generic (PrimaryKey PackageT f)
deriving instance Beamable (PrimaryKey PackageT)

type PackageId = PrimaryKey PackageT Identity

deriving instance Show PackageId
deriving instance Eq PackageId
deriving instance Ord PackageId
