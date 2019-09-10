module Backend.NotifyHandler where

import Rhyolite.Backend.Listen (DbNotification)

import Backend.Transaction (Transaction)
import Common.App (Notification, View (..), ViewSelector)

notifyHandler :: (Transaction a -> IO a) -> DbNotification Notification -> ViewSelector a -> IO (View a)
notifyHandler _runTransaction _msg _vs = pure View
