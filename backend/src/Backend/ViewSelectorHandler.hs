module Backend.ViewSelectorHandler where

import Data.Semigroup (First (..))
import Database.Beam
import Database.Beam.Backend (BeamSqlBackend)

import Backend.Schema
import Backend.Transaction (Transaction, runQuery)
import Common.App (View (..), ViewSelector (..))
import Common.Prelude
import Common.Schema
import Data.IntervalMap.Interval (Interval (..))
import qualified Data.Map.Monoidal as MMap
import qualified Database.Beam.Postgres as Pg

viewSelectorHandler :: (Eq a, Semigroup a) => (forall x. (forall mode. Transaction mode x) -> IO x) -> ViewSelector a -> IO (View a)
viewSelectorHandler runTransaction vs = if vs == mempty then pure mempty else runTransaction $ do
  translations <- fmap Option $ case getOption $ _viewSelector_translations vs of
    Nothing -> pure Nothing
    Just a -> do
      translations <- runQuery $ runSelectReturningList $ select $ all_ (_dbTranslation db)
      pure $ Just (a, MMap.fromList [(pk t, First t) | t <- translations])

  verses <-
    ifor (_viewSelector_verseRanges vs) $ \translationId ranges ->
      ifor ranges $ \range a ->
        -- TODO: Use IntervalSet to do only a single query for all relevant verses
        -- and then fan them back out to the appropriate clients.
        (a,) <$> getVersesInInterval translationId range

  pure $ View
    { _view_translations = translations
    , _view_verseRanges = (fmap.fmap) fst verses
    , _view_verses = verses <&> fold . MMap.elems . fmap snd
    }

getVersesInInterval :: TranslationId -> Interval VerseReference -> Transaction mode (MonoidalMap VerseReference Text)
getVersesInInterval translationId interval = fmap (MMap.fromDistinctAscList . map (\v -> (verseToVerseReference v, _verseText v))) $
  runQuery $
    runSelectReturningList $
      select $
        orderBy_ (\v -> (asc_ $ (\(BookId x) -> x) (_verseBook v), asc_ $ _verseChapter v, asc_ $ _verseVerse v))
        $ do
          verse <- all_ (_dbVerse db)
          guard_ (_verseTranslation verse ==. val_ translationId)

          let BookId bookInt = _verseBook verse
          guard_ $ withinInterval_
            (fmap (\x -> Pg.array_ [val_ $ _verseReference_book x, val_ $ _verseReference_chapter x, val_ $ _verseReference_verse x]) interval)
            (Pg.array_ [bookInt, _verseChapter verse, _verseVerse verse])
          pure verse

withinInterval_ :: BeamSqlBackend be => Interval (QGenExpr context be s a) -> QGenExpr context be s a -> QGenExpr context be s Bool
withinInterval_ interval b = case interval of
  IntervalCO a c -> a <=. b &&. b <. c
  ClosedInterval a c -> between_ b a c
  OpenInterval a c -> a <. b &&. b <. c
  IntervalOC a c -> a <. b &&. b <=. c
