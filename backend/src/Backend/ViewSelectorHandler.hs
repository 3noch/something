{-# LANGUAGE FlexibleContexts #-}
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
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Database.Beam.Postgres as Pg

viewSelectorHandler :: (Eq a, Monoid a) => (forall x. (forall mode. Transaction mode x) -> IO x) -> ViewSelector a -> IO (View a)
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

  tags <-
    ifor (_viewSelector_verseRanges vs) $ \translationId ranges ->
      -- TODO: Use IntervalSet to do only a single query for all relevant tags
      -- and then fan them back out to the appropriate clients.
      ifor ranges $ \range a ->
        (a,) <$> getTagsInInterval translationId range

  pure $ View
    { _view_translations = translations
    , _view_verseRanges = (fmap.fmap) fst verses
    -- TODO: Just look at this. No one wants to read this...let alone understand it. I had to "type hole" my way to this...
    , _view_verses = flip fmap verses
        $   fmap (\(a, text) -> (a, fmap First text))
        >>> MMap.elems
        >>> fold
        >>> (\(as, maps) -> (Seq.singleton as,) <$> maps)
    , _view_tags = flip fmap tags
        $   MMap.elems
        >>> fold
        >>> (\(a, map_) -> MMap.fromSet (const $ Seq.singleton a) <$> map_)
    }

getVersesInInterval :: TranslationId -> Interval VerseReference -> Transaction mode (MonoidalMap VerseReference Text)
getVersesInInterval translationId interval = fmap (MMap.fromDistinctAscList . map (\v -> (verseToVerseReference v, _verseText v))) $
  runQuery $ runSelectReturningList $ select $
    orderBy_ (\v -> (asc_ $ unBookId $ _verseBook v, asc_ $ _verseChapter v, asc_ $ _verseVerse v)) -- Important for 'fromDistinctAscList' above.
    $ do
      verse <- all_ (_dbVerse db)
      guard_ (_verseTranslation verse ==. val_ translationId)
      guard_ $ withinInterval_
        (Pg.array_ . map val_ . verseReferenceToList <$> interval)
        (Pg.array_ [unBookId $ _verseBook verse, _verseChapter verse, _verseVerse verse])
      pure verse

getTagsInInterval :: TranslationId -> Interval VerseReference -> Transaction mode (MonoidalMap Text (Set (VerseReference, Int, VerseReference, Int)))
getTagsInInterval translationId interval = fmap (MMap.fromAscListWith (<>) . map (second Set.singleton)) $
  runQuery $ runSelectReturningList $ select $
    orderBy_ (\(tagName, _) -> asc_ tagName) -- Important for 'fromAscList' above.
    $ do
      taggedRange <- all_ $ _dbTaggedRange db
      taggedRangeByWord <- join_ (_dbTaggedRangeByWord db) (\x -> _taggedrangebywordForRange x `references_` taggedRange)
      guard_ (_taggedrangebywordForTranslation taggedRangeByWord ==. val_ translationId)

      tag <- join_ (_dbTag db) (\x -> _taggedrangeForTag taggedRange `references_` x)

      let intervalArray = fmap (Pg.array_ . map val_ . verseReferenceToList) interval
      guard_ $
        withinInterval_ intervalArray (Pg.array_ $ verseReferenceToList $ _taggedrangeStart taggedRange)
        ||.
        withinInterval_ intervalArray (Pg.array_ $ verseReferenceToList $ _taggedrangeEnd taggedRange)

      pure (_tagName tag,
        ( _taggedrangeStart taggedRange, _taggedrangebywordStart taggedRangeByWord
        , _taggedrangeEnd taggedRange, _taggedrangebywordEnd taggedRangeByWord
        ) )

verseReferenceToList :: VerseReferenceT f -> [Columnar f Int]
verseReferenceToList x = [unBookId $ _versereferenceBook x, _versereferenceChapter x, _versereferenceVerse x]

withinInterval_ :: BeamSqlBackend be => Interval (QGenExpr context be s a) -> QGenExpr context be s a -> QGenExpr context be s Bool
withinInterval_ interval b = case interval of
  IntervalCO a c -> a <=. b &&. b <. c
  ClosedInterval a c -> between_ b a c
  OpenInterval a c -> a <. b &&. b <. c
  IntervalOC a c -> a <. b &&. b <=. c
