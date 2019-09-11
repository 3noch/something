{-# LANGUAGE FlexibleContexts #-}

module Frontend where

import Data.Semigroup (First (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Control.Category as Cat
import Control.Monad.Fix (MonadFix)
import Obelisk.Configs (HasConfigs (getConfig))
import Obelisk.Frontend (Frontend (..))
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Rhyolite.Api (ApiRequest)
import Rhyolite.Frontend.App (RhyoliteWidget, runObeliskRhyoliteWidget, watchViewSelector)

import Common.App (PrivateRequest, PublicRequest, View (..), ViewSelector (..))
import Common.Route
import Common.Prelude
import Common.Schema
import Obelisk.Generated.Static

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = runAppWidget appWidget
  }

runAppWidget ::
  ( HasConfigs m
  , TriggerEvent t m
  , PerformEvent t m
  , MonadHold t m
  , PostBuild t m
  , MonadFix m
  , Prerender x t m
  )
  => RoutedT t (R FrontendRoute) (RhyoliteWidget (ViewSelector SelectedCount) (ApiRequest () PublicRequest PrivateRequest) t m) a
  -> RoutedT t (R FrontendRoute) m a
runAppWidget = runObeliskRhyoliteWidget
  Cat.id
  "common/route"
  checkedFullRouteEncoder
  (BackendRoute_Listen :/ ())

type HasApp t m =
  ( MonadQuery t (ViewSelector SelectedCount) m
  , Requester t m
  , Request m ~ ApiRequest () PublicRequest PrivateRequest
  , Response m ~ Identity
  )

appWidget :: (HasApp t m, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
appWidget = do
  translations <- watchTranslations
  dyn_ $ ffor translations $ \case
    Nothing -> text "Loading..."
    Just ts -> do
      text "Translations"
      el "ol" $
        for_ ts $ \t -> el "li" $ text $ _translationName t

watchTranslations
  :: (HasApp t m, MonadHold t m, MonadFix m)
  => m (Dynamic t (Maybe (MonoidalMap TranslationId Translation)))
watchTranslations =
  (fmap . fmap) (fmap (fmap getFirst . snd) . getOption . _view_translations) $
    watchViewSelector $ pure $ mempty { _viewSelector_translations = Option $ Just 1 }
