module Backend.NotifyHandler where

import Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.Map.Monoidal as MMap
import Database.Beam
import Rhyolite.Backend.Listen (DbNotification (..))

import Backend.Transaction (Transaction, runQuery)
import Backend.Schema
import qualified Backend.ViewSelectorHandler as VSH
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
      Just a | not (null notes) -> mempty
        { _view_tagNotes = MMap.singleton tag (a, First $ VSH.notesEntityToView notes)
        }
      _ -> mempty
