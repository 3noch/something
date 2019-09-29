module Backend.NotifyHandler where

import qualified Data.Map.Monoidal as MMap
import qualified Data.Text as T
import Rhyolite.Backend.Listen (DbNotification (..))
import Data.Dependent.Sum      (DSum ((:=>)))

import Backend.Transaction (Transaction)
import Backend.Schema (Notification (..))
import Common.App (View (..), ViewSelector)
import Common.Prelude


notifyHandler :: forall a. Monoid a => (forall x. (forall mode. Transaction mode x) -> IO x) -> DbNotification Notification -> ViewSelector a -> IO (View a)
notifyHandler _runTransaction msg _vs = case _dbNotification_message msg of
  Notification_Tag :=> Identity x -> do
    putStrLn $ "Got notify to add thing " <> show x
    pure (mempty :: View a)
