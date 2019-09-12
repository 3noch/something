module Backend.NotifyHandler where

import Rhyolite.Backend.Listen (DbNotification)

import Backend.Transaction (Transaction)
import Common.App (Notification, View (..), ViewSelector)

notifyHandler :: Semigroup a => (forall x. (forall mode. Transaction mode x) -> IO x) -> DbNotification Notification -> ViewSelector a -> IO (View a)
notifyHandler _runTransaction _msg _vs = pure $ View mempty mempty
