module Backend.ViewSelectorHandler where

import Data.Semigroup (First (..))
import Database.Beam

import Backend.Schema
import Backend.Transaction (Transaction, runQuery)
import Common.App (ClosedRange (..), View (..), ViewSelector (..))
import Common.Prelude
import Common.Schema
import qualified Data.Map.Monoidal as MMap
import qualified Database.Beam.Postgres as Pg

viewSelectorHandler :: (Eq a, Semigroup a) => (forall x. (forall mode. Transaction mode x) -> IO x) -> ViewSelector a -> IO (View a)
viewSelectorHandler runTransaction vs = if vs == mempty then pure mempty else runTransaction $ do
  translations <- fmap Option $ case getOption $ _viewSelector_translations vs of
    Nothing -> pure Nothing
    Just a -> do
      translations <- runQuery $ runSelectReturningList $ select $ all_ (_dbTranslation db)
      pure $ Just (a, MMap.fromList [(pk t, First t) | t <- translations])

  verses <- if _viewSelector_verses vs == mempty then pure mempty else
    ifor (_viewSelector_verses vs) $ \(translationId, rng) a ->
      (a,) <$> getVersesInRange translationId rng

  pure $ View translations verses

getVersesInRange :: TranslationId -> ClosedRange VerseReference -> Transaction mode [Verse]
getVersesInRange translationId (ClosedRange lowRef highRef) =
  runQuery $
    runSelectReturningList $
      select $ do
        verse <- all_ (_dbVerse db)
        guard_ (_verseTranslation verse ==. val_ translationId)

        let BookId bookInt = _verseBook verse
        guard_ (between_
            (Pg.array_ [bookInt, _verseChapter verse, _verseVerse verse])
            (Pg.array_ [val_ $ _verseReference_book lowRef, val_ $ _verseReference_chapter lowRef, val_ $ _verseReference_verse lowRef])
            (Pg.array_ [val_ $ _verseReference_book highRef, val_ $ _verseReference_chapter highRef, val_ $ _verseReference_verse highRef])
          )
        pure verse
