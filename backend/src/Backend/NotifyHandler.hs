module Backend.NotifyHandler where

import Data.Dependent.Sum (DSum ((:=>)))
import Database.Beam
import qualified Data.Map.Monoidal as MMap
import Rhyolite.Backend.Listen (DbNotification (..))
import qualified Data.Sequence as Seq

import Backend.Transaction (Transaction, runQuery)
import Backend.Schema
import Common.App (TagOccurrence (..), View (..), ViewSelector (..), rederiveTags)
import Common.Prelude
import Common.Schema

notifyHandler :: forall a. Monoid a => (forall x. (forall mode. Transaction mode x) -> IO x) -> DbNotification Notification -> ViewSelector a -> IO (View a)
notifyHandler runTransaction msg vs = case _dbNotification_message msg of
  Notification_Tag :=> Identity (presence, TagOccurrence tagName translationId interval) ->
    let
      entry = MMap.singleton translationId $ MMap.singleton tagName $ MMap.singleton interval (First presence, ())
    in pure mempty
      { _view_verseRanges = _viewSelector_verseRanges vs
      , _view_tags = rederiveTags (_viewSelector_verseRanges vs) entry
      }

  Notification_SetNotes :=> Identity (tag, noteId) -> runTransaction $ do
    notes <- runQuery $ runSelectReturningList $ select $ orderBy_ (asc_ . _taggedrangenoteUpdated) $ do
      taggedRangeNote <- all_ (_dbTaggedRangeNote db)
      guard_ (pk taggedRangeNote ==. val_ noteId)
      pure taggedRangeNote

    pure $ case MMap.lookup tag (_viewSelector_tagNotes vs) of
      Nothing -> mempty
      Just a -> mempty { _view_tagNotes = MMap.singleton tag (a, First $ Seq.fromList $ map _taggedrangenoteContent notes ) }
