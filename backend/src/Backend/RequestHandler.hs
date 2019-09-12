module Backend.RequestHandler where

import Rhyolite.Api (ApiRequest (..))
import Rhyolite.Backend.App (RequestHandler (..))

import Backend.Transaction (Transaction)
import Common.App (PrivateRequest (..), PublicRequest (..))
import Common.Prelude

requestHandler :: MonadIO m => (forall x. Transaction mode x -> m x) -> RequestHandler (ApiRequest () PublicRequest PrivateRequest) m
requestHandler _runTransaction =
  RequestHandler $ \case
    ApiRequest_Public r -> case r of
      PublicRequest_NoOp -> return ()
    ApiRequest_Private _key r -> case r of
      PrivateRequest_NoOp -> return ()
