{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Frontend.Article where

import           Control.Lens
import           Reflex.Dom.Core                              hiding (Element)

import           Control.Error                                (hush)
import           Control.Monad                                ((<=<))
import           Control.Monad.Fix                            (MonadFix)
import           Control.Monad.IO.Class                       (MonadIO)
import           Data.Default                                 (def)
import           Data.Functor                                 (void)
import           Data.Foldable                                (fold)
import qualified Data.Map                                     as Map
import           Data.Maybe                                   (fromMaybe)
import           Data.Monoid                                  (Endo (Endo),
                                                               appEndo)
import           Data.Text                                    (Text)
import           JSDOM.Document                               (createElement)
import           JSDOM.Element                                (setInnerHTML)
import           JSDOM.Types                                  (liftJSM)
import           Obelisk.Route.Frontend                       (pattern (:/), R,
                                                               RouteToUrl,
                                                               Routed, SetRoute,
                                                               askRoute,
                                                               routeLink)
import           Servant.Common.Req                           (reqSuccess)
import           Text.Pandoc                                  (readMarkdown,
                                                               runPure,
                                                               writeHtml5String)

import           Common.Route                                 (DocumentSlug (..),
                                                               FrontendRoute (..),
                                                               Username (..))
import           Frontend.ArticlePreview                      (profileImage,
                                                               profileRoute)
import           Frontend.FrontendStateT
import           Frontend.Utils                               (buttonClass,
                                                               routeLinkClass,
                                                               routeLinkDynClass,
                                                               showText)
import qualified RealWorld.Conduit.Api.Articles.Article       as Article
import qualified RealWorld.Conduit.Api.Articles.Comment       as Comment
import qualified RealWorld.Conduit.Api.Articles.CreateComment as CreateComment
import           RealWorld.Conduit.Api.Namespace              (Namespace (..),
                                                               unNamespace)
import qualified RealWorld.Conduit.Api.User.Account           as Account
import qualified RealWorld.Conduit.Api.User.Profile           as Profile
import           RealWorld.Conduit.Client                     (apiArticles, articleCommentCreate,
                                                               articleComments,
                                                               articleGet,
                                                               articlesArticle,
                                                               getClient)

article
  :: forall t m s js
  .  ( DomBuilder t m
     , Routed t DocumentSlug m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , HasDocument m
     , HasFrontendState t s m
     , HasLoggedInAccount s
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender js m
     )
  => m ()
article = elClass "div" "article-page" $ prerender (text "Loading...") $ do
  slugDyn <- askRoute
  pbE <- getPostBuild
  loadResE <- getClient ^. apiArticles . articlesArticle
     . to ($ (Identity $ Right . unDocumentSlug <$> slugDyn)) . articleGet
     . to ($ pbE)
  let loadSuccessE :: Event t (Maybe Article.Article) = fmap unNamespace . reqSuccess . runIdentity <$> loadResE
  articleDyn <- holdDyn Nothing loadSuccessE

  elClass "div" "banner" $
    elClass "div" "container" $ do
      el "h1" $ text "How to build webapps that scale"
      void $ dyn $ maybe blank articleMeta <$> articleDyn
  elClass "div" "container page" $ do
    articleContent articleDyn
    el "hr" blank
    elClass "div" "row article-actions" $
      void $ dyn $ maybe blank articleMeta <$> articleDyn
    elClass "div" "row" $
      elClass "div" "col-xs-12 col-md-8 offset-md-2" $ do
        comments slugDyn

markDownToHtml5 :: Text -> Maybe Text
markDownToHtml5 t = hush . runPure . (writeHtml5String def <=< readMarkdown def) $ t

articleMeta
  :: ( DomBuilder t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , PostBuild t m
     , MonadHold t m
     )
  => Article.Article
  -> m ()
articleMeta art = elClass "div" "article-meta" $ do
  let profile = Article.author art
  let authorRoute = FrontendRoute_Profile :/ (Username "foo", Nothing)
  routeLink authorRoute $ profileImage "" (constDyn . Profile.image $ profile)
  elClass "div" "info" $ do
    routeLinkClass "author" authorRoute $ text (Profile.username profile)
    elClass "span" "date" $ text (showText $ Article.createdAt art)
  actions profile
  where
    actions profile = do
      -- TODO : Do something with this click
      _ <- buttonClass "btn btn-sm btn-outline-secondary action-btn" $ do
        elClass "i" "ion-plus-round" blank
        text " Follow "
        text (Profile.username profile)
        text " ("
        -- TODO : Get this value
        elClass "span" "counter" $ text "0"
        text ")"
      -- TODO : Do something with this click
      text " "
      _ <- buttonClass "btn btn-sm btn-outline-primary action-btn" $ do
        elClass "i" "ion-heart" blank
        text " Favourite Post ("
        elClass "span" "counter" $ text $ showText (Article.favoritesCount art)
        text ")"
      pure ()

articleContent
  :: forall t m js
  .  ( DomBuilder t m
     , MonadHold t m
     , PerformEvent t m
     , HasDocument m
     , Prerender js m
     )
  => Dynamic t (Maybe Article.Article)
  -> m ()
articleContent articleDyn = prerender (text "Loading...") $ do
  let htmlDyn = fromMaybe "" . (markDownToHtml5 <=< (fmap Article.body)) <$> articleDyn
  elClass "div" "row article-content" $ do
    d <- askDocument
    htmlT <- sample . current $ htmlDyn
    e <- liftJSM $ do
      -- This wont execute scripts, but will allow users to XSS attack through
      -- event handling javascript attributes in any raw HTML that is let
      -- through the markdown renderer. But this is the simplest demo that
      -- mostly works. See https://github.com/qfpl/reflex-dom-template for a
      -- potentially more robust solution (we could filter out js handler attrs
      -- with something like that).
      -- It's worth noting that the react demo app does exactly what this does:
      -- https://github.com/gothinkster/react-redux-realworld-example-app/blob/master/src/components/Article/index.js#L60
      e <- createElement d ("div" :: String)
      setInnerHTML e htmlT
      pure e
    performEvent_ $ (liftJSM . setInnerHTML e) <$> updated htmlDyn
    placeRawElement e

comments
  :: forall t m s js
  .  ( DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , HasFrontendState t s m
     , HasLoggedInAccount s
     , Prerender js m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadIO (Performable m)
     )
  => Dynamic t DocumentSlug
  -> m ()
comments slugDyn = userWidget $ \acct -> prerender (text "Loading...") $ mdo
  pbE <- getPostBuild
  loadResE <- getClient ^. apiArticles . articlesArticle
     . to ($ (Identity $ Right . unDocumentSlug <$> slugDyn)) . articleComments
     . to ($ pbE)
  let loadSuccessE = fmapMaybe (fmap unNamespace . reqSuccess . runIdentity) loadResE
  let loadedMapE   = Map.fromList . (fmap (\c -> (Comment.id c, c))) <$> loadSuccessE
  commentsMapDyn <- foldDyn appEndo Map.empty $ fold
    [ Endo . const <$> loadedMapE
    , (\newComment -> Endo $ Map.insert (Comment.id newComment) newComment) <$> newCommentE
    ]

  newCommentE <- elClass "form" "card comment-form" $ mdo
    commentI <- elClass "div" "card-block" $ do
      textArea $ def
          & textAreaConfig_attributes .~ (constDyn (Map.fromList
            [("class","form-control")
            ,("placeholder","Write a comment")
            ,("rows","3")
            ]))
          & textAreaConfig_setValue .~ ("" <$ newE)
    let createCommentDyn = Identity . Right . Namespace <$> CreateComment.CreateComment
          <$> commentI ^.textArea_value
    postE <- elClass "div" "card-footer" $ do
      buttonClass "btn btn-sm btn-primary" $ text "Post Comment"
    submitResE <- getClient ^. apiArticles . articlesArticle
      . to ($ (Identity $ Right . unDocumentSlug <$> slugDyn)) . articleCommentCreate
      . to (\f -> f (constDyn . pure . pure $ Account.token acct) createCommentDyn postE)
    let newE = fmapMaybe (fmap unNamespace . reqSuccess . runIdentity) submitResE
    pure newE

  void $ list commentsMapDyn $ \commentDupeDyn -> do
    commentDyn <- holdUniqDyn commentDupeDyn
    let profileDyn = Comment.author <$> commentDyn
    elClass "div" "card" $ do
      elClass "div" "card-block" $ do
        elClass "p" "card-text" . dynText $ Comment.body <$> commentDyn
      elClass "div" "card-footer" $ do
        let authorRouteDyn = profileRoute <$> profileDyn
        routeLinkDynClass (constDyn "comment-author") authorRouteDyn $
          profileImage "comment-author-img" (Profile.image <$> profileDyn)
        text " "
        routeLinkDynClass "comment-author" authorRouteDyn $ dynText $ Profile.username <$> profileDyn
        elClass "span" "date-posted" $ display $ Comment.createdAt <$> commentDyn
  pure ()
