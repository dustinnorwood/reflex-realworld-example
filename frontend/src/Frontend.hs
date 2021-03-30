{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms, RankNTypes #-}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, TypeApplications                                #-}

module Frontend where

import Control.Lens
import Reflex.Dom.Core hiding (Namespace)

import Control.Monad.Trans.Reader (mapReaderT)
import Data.List.NonEmpty         (NonEmpty)
import Data.Monoid                (appEndo)
import Obelisk.Frontend           (Frontend (Frontend), ObeliskWidget)
import Obelisk.Route.Frontend     (pattern (:/), R, RouteToUrl, RoutedT, SetRoute, mapRoutedT, subRoute_)
import Reflex.Dom.Storage.Base    (StorageT (..), StorageType (LocalStorage), runStorageT)
import Reflex.Dom.Storage.Class   (askStorageTag, pdmInsert, pdmRemove, tellStorage)


import           Common.Conduit.Api.Namespace    (unNamespace)
import qualified Common.Conduit.Api.User.Account as Account
import           Common.Route                    (FrontendRoute (..))
import           Frontend.Article                (article)
import qualified Frontend.Conduit.Client         as Client
import           Frontend.Editor                 (editor)
import           Frontend.FrontendStateT
import           Frontend.Head                   (htmlHead)
import           Frontend.HomePage               (homePage)
import           Frontend.LocalStorageKey        (LocalStorageTag (..))
import           Frontend.Login                  (login)
import           Frontend.Nav                    (nav)
import           Frontend.Package                (package)
import           Frontend.Profile                (profile)
import           Frontend.Register               (register)
import           Frontend.Settings               (settings)
import           Frontend.Utils                  (pathSegmentSubRoute, routeLinkClass)

mapStorageT :: (forall x. m x -> n x) -> StorageT t k m a -> StorageT t k n a
mapStorageT f = StorageT . mapReaderT (mapEventWriterT f) . unStorageT


type RoutedAppState t m = RoutedT t (R FrontendRoute) (AppState t m)

type AppState t m
  = EventWriterT t
    (NonEmpty FrontendEvent)
    (StorageT t LocalStorageTag (FrontendStateT t FrontendData m))


htmlBody
  :: forall t js m
  . ( ObeliskWidget js t (R FrontendRoute) m
    )
  => RoutedT t (R FrontendRoute) m ()
htmlBody = mapRoutedT unravelAppState $ do
  prerender_ (pure ()) $ do
    jwtDyn <- askStorageTag LocalStorageJWT
    pbE <- getPostBuild
    (currentUserE,currentUserErrE,_) <- Client.getCurrentUser jwtDyn pbE
    tellEvent $ (pure . LogIn . unNamespace) <$> currentUserE
    tellEvent $ (pure LogOut) <$ currentUserErrE
  nav
  elAttr "main" ("role"=:"main"<>"class"=:"container") $ subRoute_ pages
  footer
  where
    unravelAppState :: AppState t m () -> m ()
    unravelAppState m = mdo
      lsDyn <- foldDyn appEndo initialFrontendData (foldMap updateFrontendData <$> sE)
      sE <- flip runFrontendStateT lsDyn $ do
        runStorageT LocalStorage $ do
          (_, sInnerE) <- runEventWriterT m
          tellStorage (foldMap localEventToDMapPatch <$> sInnerE)
          pure sInnerE
      pure ()

    localEventToDMapPatch :: FrontendEvent -> PatchDMap LocalStorageTag Identity
    localEventToDMapPatch = \case
      LogOut  -> pdmRemove LocalStorageJWT
      LogIn a -> pdmInsert LocalStorageJWT (Account.token a)

    pages
      :: FrontendRoute a
      -> RoutedT t a
         (EventWriterT t
           (NonEmpty FrontendEvent)
           (StorageT t LocalStorageTag (FrontendStateT t FrontendData m)))
          ()
    pages r = case r of
      FrontendRoute_Home     -> homePage
      FrontendRoute_Login    -> login
      FrontendRoute_Register -> register
      FrontendRoute_Article  -> article
      FrontendRoute_Package  -> package
      FrontendRoute_Settings -> settings
      FrontendRoute_Profile  -> pathSegmentSubRoute profile
      FrontendRoute_Editor   -> editor

footer
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R (FrontendRoute)) m
     , SetRoute t (R (FrontendRoute)) m
     , MonadSample t m
     )
  => m ()
footer = elClass "footer" "footer" $ elClass "div" "container" $ do
  routeLinkClass "logo-font" (FrontendRoute_Home :/ ()) $ text "fojano"
  elClass "span" "attribution" $ do
    text "The trillion dollar business opportunity."

frontend :: Frontend (R FrontendRoute)
frontend = Frontend (prerender_ htmlHead htmlHead) htmlBody
