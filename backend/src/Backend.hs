{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Common.Route
import Obelisk.Backend

import Backend.Schema (withDb)

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> withDb $ \_ -> serve $ const $ pure ()
  , _backend_routeEncoder = fullRouteEncoder
  }
