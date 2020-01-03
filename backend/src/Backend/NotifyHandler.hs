module Backend.NotifyHandler where

import qualified Data.Map.Monoidal as MMap
import Rhyolite.Backend.Listen (DbNotification (..))
import Data.Dependent.Sum      (DSum ((:=>)))

import Backend.Transaction (Transaction)
import Backend.Schema (Notification (..))
import Common.App (TagOccurrence (..), View (..), ViewSelector (..), rederiveTags)
import Common.Prelude

notifyHandler :: forall a. Monoid a => (forall x. (forall mode. Transaction mode x) -> IO x) -> DbNotification Notification -> ViewSelector a -> IO (View a)
notifyHandler _runTransaction msg vs = case _dbNotification_message msg of
  Notification_Tag :=> Identity (_change, TagOccurrence tagName translationId interval) ->
    let
      entry = MMap.singleton translationId $ MMap.singleton tagName $ MMap.singleton interval ()
    in pure mempty
      { _view_verseRanges = _viewSelector_verseRanges vs
      , _view_tags = rederiveTags (_viewSelector_verseRanges vs) entry
      }
