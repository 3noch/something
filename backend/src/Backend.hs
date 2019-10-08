{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Control.Exception.Safe (finally)
import Obelisk.Backend (Backend (..))
import Obelisk.Route
import qualified Rhyolite.Backend.App as RhyoliteApp
import qualified Snap.Core as Snap

import Backend.NotifyHandler (notifyHandler)
import Backend.RequestHandler (requestHandler)
import Backend.Schema (withDb)
import Backend.Transaction (Transaction, runTransaction)
import Backend.ViewSelectorHandler (viewSelectorHandler)
import Common.Prelude
import Common.Route (BackendRoute (..), FrontendRoute, fullRouteEncoder)

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = backendRun
  , _backend_routeEncoder = fullRouteEncoder
  }

backendRun :: (MonadIO m) => ((R BackendRoute -> Snap.Snap ()) -> IO a) -> m a
backendRun serve = withDb $ \dbPool -> do
  let
    runTransaction' :: Transaction mode a -> IO a
    runTransaction' = runTransaction dbPool
  (handleListen, wsFinalizer) <- RhyoliteApp.serveDbOverWebsockets (RhyoliteApp.convertPostgresPool dbPool)
    (requestHandler runTransaction')
    (notifyHandler runTransaction')
    (RhyoliteApp.QueryHandler $ viewSelectorHandler runTransaction')
    RhyoliteApp.functorFromWire
    RhyoliteApp.standardPipeline

  flip finally wsFinalizer $ serve $ \case
    BackendRoute_Missing :/ _ -> Snap.writeText "404 Page not found"
    BackendRoute_Listen :/ _ -> handleListen
