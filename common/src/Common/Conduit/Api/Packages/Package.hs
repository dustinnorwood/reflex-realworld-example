{-# LANGUAGE DeriveAnyClass, DeriveGeneric, StandaloneDeriving #-}
module Common.Conduit.Api.Packages.Package where

import Data.Aeson                  (FromJSON (..), ToJSON (..))
import Data.Set                    (Set)
import Data.Text                   (Text)
import Data.Time                   (UTCTime)
import GHC.Generics                (Generic)

data Package = Package
  { id             :: Int
  , slug           :: Text
  , title          :: Text
  , description    :: Text
  , image          :: Text
  , body           :: Text
  , tagList        :: Set Text
  , createdAt      :: UTCTime
  , updatedAt      :: UTCTime
  , favorited      :: Bool
  , favoritesCount :: Int
  } deriving Show

deriving instance Generic Package
deriving instance ToJSON Package
deriving instance FromJSON Package
