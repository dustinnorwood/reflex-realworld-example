{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}
module Common.Conduit.Api.Packages
  ( PackagesApi
  , PackageApi
  , module Package
  , module Packages
  , module Attributes
  , module Namespace
  ) where

import Servant.API

import Data.Text    (Text)
import Servant.Auth (Auth, JWT)

import Common.Conduit.Api.Packages.Package       as Package (Package (Package))
import Common.Conduit.Api.Packages.Packages      as Packages (Packages (Packages))
import Common.Conduit.Api.Packages.Attributes    as Attributes (PackageAttributes (PackageAttributes), CreatePackage,
                                                  UpdatePackage)
import Common.Conduit.Api.Namespace              as Namespace (Namespace (Namespace))

type PackagesApi token =
  (
     Auth '[JWT] token
  :> QueryParam "limit" Integer
  :> QueryParam "offset" Integer
  :> QueryParams "tag" Text
  :> QueryParams "favorited" Text
  :> Get '[JSON] Packages
  ) :<|> (
    Auth '[JWT] token
  :> ReqBody '[JSON] (Namespace "package" CreatePackage)
  :> PostCreated '[JSON] (Namespace "package" Package)
  ) :<|> (
    "feed"
  :> Auth '[JWT] token
  :> QueryParam "limit" Integer
  :> QueryParam "offset" Integer
  :> Get '[JSON] Packages
  )
  :<|> PackageApi token


type PackageApi token = (
  Auth '[JWT] token
  :> Capture "slug" Text
  :> (
    Get '[JSON] (Namespace "package" Package)
    )
  )
