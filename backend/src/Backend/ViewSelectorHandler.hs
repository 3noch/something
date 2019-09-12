module Backend.ViewSelectorHandler where

import Data.Semigroup (First (..))
import Database.Beam

import Backend.Schema
import Backend.Transaction (Transaction, runQuery)
import Common.App (ClosedRange (..), View (..), ViewSelector (..))
import Common.Prelude
import Common.Schema
import qualified Data.Map.Monoidal as MMap

import Debug.Trace (trace)


viewSelectorHandler :: (Eq a, Semigroup a) => (forall x. (forall mode. Transaction mode x) -> IO x) -> ViewSelector a -> IO (View a)
viewSelectorHandler runTransaction vs = if vs == mempty then pure mempty else runTransaction $ do
  translations <- fmap Option $ case getOption $ _viewSelector_translations vs of
    Nothing -> pure Nothing
    Just a -> do
      translations <- runQuery $ runSelectReturningList $ select $ all_ (_dbTranslation db)
      pure $ Just (a, MMap.fromList [(pk t, First t) | t <- translations])

  verses <- if _viewSelector_verses vs == mempty then pure mempty else
    ifor (_viewSelector_verses vs) $ \(translationId, rng) a -> do
      trace ("Trying to do: " <> show rng) $ (a,) <$> getVersesInRange translationId rng

  pure $ View translations verses

getVersesInRange :: TranslationId -> ClosedRange VerseReference -> Transaction mode [Verse]
getVersesInRange translationId (ClosedRange lowRef highRef) =
  runQuery $
    runSelectReturningList $
      select $ do
        verse <- all_ (_dbVerse db)
        guard_ (_verseTranslation verse ==. val_ translationId)

        -- HACK: Apparently beam doesn't have a way to do ordering on composite types??
        let
          verseIntHack =
            let BookId bookInt = _verseBook verse
             in bookInt * val_ 100000000 + _verseChapter verse * val_ 100000 + _verseVerse verse
          toVerseIntHack (VerseReference book chapter vs) = val_ $ book * 100000000 + chapter * 100000 + vs
        guard_ (between_ verseIntHack (toVerseIntHack lowRef) (toVerseIntHack highRef))
        pure verse
