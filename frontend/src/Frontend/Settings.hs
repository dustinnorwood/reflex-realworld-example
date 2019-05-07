{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications                                                   #-}

module Frontend.Settings where

import Control.Lens
import Reflex.Dom.Core

import           Control.Monad          (mfilter)
import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.Map               as Map
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as Text
import           Obelisk.Route.Frontend (pattern (:/), R, SetRoute, setRoute)
import           Servant.Common.Req     (reqSuccess)

import           Common.Conduit.Api.Namespace    (Namespace (Namespace), unNamespace)
import qualified Common.Conduit.Api.User.Account as Account
import           Common.Conduit.Api.User.Update  (UpdateUser (UpdateUser))
import           Common.Route                    (FrontendRoute (..), Username (..))
import qualified Frontend.Conduit.Client         as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                  (buttonClass)

settings
  :: ( DomBuilder t m
     , PostBuild t m
     , Prerender js t m
     , SetRoute t (R FrontendRoute) m
     , EventWriter t (NonEmpty FrontendEvent) m
     , HasFrontendState t s m
     , HasLoggedInAccount s
     )
  => m ()
-- First we should look at userWidget !
settings = userWidget $ \acct -> elClass "div" "settings-page" $ do
  elClass "div" "container page" $
    elClass "div" "row" $
      elClass "div" "col-md-6 offset-md-3 col-xs-12" $ do
        elClass "h1" "text-xs-center" $ text "Your Settings"
        el "form" $ do
          -- When this FRP network is built, we want to load the existing data
          pbE <- getPostBuild
          let tokenDyn = constDyn . pure . Account.token $ acct
          -- The only input is the JWT token that we load from the FrontendState
          loadResE <- Client.getCurrentUser tokenDyn pbE

          -- Again, we throw away errors for now. We should fix this.
          let loadSuccessE = fmapMaybe (fmap unNamespace . reqSuccess) loadResE
          el "fieldset" $ do
            urlI <- elClass "fieldset" "form-group" $
              inputElement $ def
                & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                  [("class","form-control")
                  ,("placeholder","URL of profile picture")
                  ]
                -- Note that we set the form val from AJAX returned data
                & inputElementConfig_setValue .~ (fromMaybe "" . Account.image <$> loadSuccessE)
            usernameI <- elClass "fieldset" "form-group" $
              inputElement $ def
                & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                  [("class","form-control")
                  ,("placeholder","Your name")
                  ]
                & inputElementConfig_setValue .~ (Account.username <$> loadSuccessE)
            bioI <- elClass "fieldset" "form-group" $
              textAreaElement $ def
                & textAreaElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                  [("class","form-control")
                  ,("placeholder","Short bio about you")
                  ,("rows","8")
                  ]
                & textAreaElementConfig_setValue .~ (Account.bio <$> loadSuccessE)

            emailI <- elClass "fieldset" "form-group" $
              inputElement $ def
                & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                  [("class","form-control")
                  ,("placeholder","Email")
                  ,("type","input")
                  ]
                & inputElementConfig_setValue .~ (Account.email <$> loadSuccessE)
            passwordI <- elClass "fieldset" "form-group" $
              inputElement $ def
                & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                  [("class","form-control")
                  ,("placeholder","Password")
                  ,("type","password")
                  ]
            updateE <- buttonClass "btn btn-lg btn-primary pull-xs-right" $ text "Update Settings"
            -- Here we dont want to update the password if it was left blank
            let updateDyn = UpdateUser
                  <$> (mfilter (not . Text.null) . Just <$> passwordI ^. to _inputElement_value)
                  <*> (Just <$> emailI ^. to _inputElement_value)
                  <*> (Just <$> usernameI ^. to _inputElement_value)
                  <*> (Just <$> bioI ^. to _textAreaElement_value)
                  <*> (Just <$> urlI ^. to _inputElement_value)

            -- Make the backend call when the submit button is clicked
            -- and we have a valid UpdateUser
            updateResE <- Client.updateCurrentUser tokenDyn (pure . Namespace <$> updateDyn) updateE

            -- More throwing away errors
            let updateSuccessE = fmapMaybe (fmap unNamespace . reqSuccess) updateResE

            -- Once we have updated successfully, we redirect to the profile page.
            setRoute $
              (\newA ->
                FrontendRoute_Profile :/ (Username $ Account.username newA, Nothing)
              ) <$> updateSuccessE

          el "hr" blank
          -- Add a logout button that dispatches a logout event.
          logoutClick <- buttonClass "btn btn-outline-danger" $ text "Logout"
          tellEvent $ pure LogOut <$ logoutClick
          -- And redirect to home.
          setRoute $ FrontendRoute_Home :/ () <$ logoutClick
          pure ()
