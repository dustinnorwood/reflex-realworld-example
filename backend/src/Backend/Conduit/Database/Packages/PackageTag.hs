{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns, StandaloneDeriving, TypeFamilies, TypeSynonymInstances                    #-}
module Backend.Conduit.Database.Packages.PackageTag
  ( PackageTagT(..)
  , PackageTag
  ) where

import Database.Beam (Beamable, Identity, PrimaryKey, Table (..))
import GHC.Generics  (Generic)

import Backend.Conduit.Database.Packages.Package (PackageT)
import Backend.Conduit.Database.Tags.Tag         (TagT)

data PackageTagT f = PackageTag
  { package :: PrimaryKey PackageT f
  , tag     :: PrimaryKey TagT f
  } deriving (Generic)

type PackageTag = PackageTagT Identity

deriving instance Show PackageTag

deriving instance Eq PackageTag

instance Beamable PackageTagT

instance Beamable (PrimaryKey PackageTagT)

instance Table PackageTagT where
  data PrimaryKey PackageTagT f
    = PackageTagId
        (PrimaryKey PackageT f)
        (PrimaryKey TagT f)
    deriving Generic
  primaryKey =
    PackageTagId
      <$> package
      <*> tag
