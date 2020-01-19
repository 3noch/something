{-# LANGUAGE FlexibleContexts #-}
module Backend.ViewSelectorHandler where

import Control.Arrow ((***))
import Data.List (foldl1')
import Data.Semigroup (First (..))
import qualified Data.Text as T
import qualified Data.Time as Time
import Database.Beam
import Database.Beam.Backend (BeamSqlBackend)

import Backend.Schema
import Backend.Transaction (Transaction, runQuery)
import Common.App (ClosedInterval' (..), View (..), ViewSelector (..))
import Common.Prelude
import Common.Schema
import Data.IntervalMap.Interval (Interval (..))
import qualified Data.Map.Monoidal as MMap
import qualified Data.Set as Set
import Data.Sequence (Seq)
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

  tags :: MonoidalMap TranslationId (MonoidalMap (Interval VerseReference) (a, MonoidalMap Text (Set (ClosedInterval' (VerseReference, Int))))) <-
    ifor (_viewSelector_verseRanges vs) $ \translationId ranges ->
      -- TODO: Use IntervalSet to do only a single query for all relevant tags
      -- and then fan them back out to the appropriate clients.
      ifor ranges $ \range a ->
        (a,) <$> getTagsInInterval translationId range

  tagNotes' <- getTaggedRangeNotes $ MMap.keysSet $ _viewSelector_tagNotes vs
  let
    tagNotes = flip MMap.mapMaybeWithKey tagNotes' $ \k v -> do
      a <- MMap.lookup k $ _viewSelector_tagNotes vs
      Just (a, First $ notesEntityToView $ toList v)

  pure $ View
    { _view_translations = translations
    , _view_verseRanges = (fmap.fmap) fst verses
    , _view_tagNotes = tagNotes
    -- TODO: Just look at this. No one wants to read this...let alone understand it. I had to "type hole" my way to this...
    , _view_verses = flip fmap verses
        $   fmap (\(a, text) -> (a, fmap First text))
        >>> fold
        >>> (\(as, maps) -> (Seq.singleton as,) <$> maps)
    , _view_tags = flip fmap tags
        $   fold
        >>> (\(a, map_) -> MMap.fromSet (const (First Present, Seq.singleton a)) <$> map_)
    }

notesEntityToView :: [TaggedRangeNote] -> (Text, UTCTime)
notesEntityToView notes =
  ( T.strip $ T.unlines $ map _taggedrangenoteContent notes
  , Time.localTimeToUTC Time.utc $ maximum $ map _taggedrangenoteUpdated notes
  )

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

getTagsInInterval :: TranslationId -> Interval VerseReference -> Transaction mode (MonoidalMap Text (Set (ClosedInterval' (VerseReference, Int))))
getTagsInInterval translationId interval = fmap (MMap.fromAscListWith (<>) . map (second $ Set.singleton . pairToClosedInterval)) $
  runQuery $ runSelectReturningList $ select $
    orderBy_ (\(tagName, _) -> asc_ tagName) -- Important for 'fromAscList' above.
    $ do
      (tag, taggedRange, taggedRangeByWord) <- allTagsAndRelated
      guardTagsByIntervals taggedRange $ Set.singleton interval
      guard_ (_taggedrangebywordForTranslation taggedRangeByWord ==. val_ translationId)
      pure (_tagName tag,
        ( _taggedrangeStart taggedRange, _taggedrangebywordStart taggedRangeByWord
        , _taggedrangeEnd taggedRange, _taggedrangebywordEnd taggedRangeByWord
        ) )
  where
    pairToClosedInterval (a, b, c, d) = ClosedInterval' (a, b) (c, d)

allTagsAndRelated
  :: Q Pg.Postgres Db s
      ( TagT (QExpr Pg.Postgres s)
      , TaggedRangeT (QExpr Pg.Postgres s)
      , TaggedRangeByWordT (QExpr Pg.Postgres s)
      )
allTagsAndRelated = do
  taggedRange <- all_ $ _dbTaggedRange db
  taggedRangeByWord <- join_ (_dbTaggedRangeByWord db) (\x -> _taggedrangebywordForRange x `references_` taggedRange)
  tag <- join_ (_dbTag db) (\x -> _taggedrangeForTag taggedRange `references_` x)
  pure (tag, taggedRange, taggedRangeByWord)


guardTagsByIntervals
  :: ( Foldable f
     , Columnar t Int ~ QGenExpr QValueContext Pg.Postgres s Int
     )
  => TaggedRangeT t
  -> f (Interval VerseReference) -> Q Pg.Postgres db s ()
guardTagsByIntervals taggedRange intervals =
  guard_ $ foldAny_ $ do
    interval <- toList intervals
    let intervalArray = fmap (Pg.array_ . map val_ . verseReferenceToList) interval
    pure (
      withinInterval_ intervalArray (Pg.array_ $ verseReferenceToList $ _taggedrangeStart taggedRange)
      ||.
      withinInterval_ intervalArray (Pg.array_ $ verseReferenceToList $ _taggedrangeEnd taggedRange)
      )

verseReferenceToList :: VerseReferenceT f -> [Columnar f Int]
verseReferenceToList x = [unBookId $ _versereferenceBook x, _versereferenceChapter x, _versereferenceVerse x]

withinInterval_ :: BeamSqlBackend be => Interval (QGenExpr context be s a) -> QGenExpr context be s a -> QGenExpr context be s Bool
withinInterval_ interval b = case interval of
  IntervalCO a c -> a <=. b &&. b <. c
  ClosedInterval a c -> between_ b a c
  OpenInterval a c -> a <. b &&. b <. c
  IntervalOC a c -> a <. b &&. b <=. c

getTaggedRangeNotes
  :: Set (Text, ClosedInterval' (VerseReference, Int))
  -> Transaction mode (MonoidalMap (Text, ClosedInterval' (VerseReference, Int)) (Seq TaggedRangeNote))
getTaggedRangeNotes tags = fmap (MMap.fromListWith (<>) . map (tuplesToKey *** Seq.singleton)) $
  runQuery $ runSelectReturningList $ select $ do
    tagTables@(tag, taggedRange, taggedRangeByWord) <- allTagsAndRelated
    guardExactTagRangeMatches tags tagTables
    taggedRangeNote <- join_ (_dbTaggedRangeNote db) (\x -> _taggedrangenoteForRange x `references_` taggedRange)
    let
      k =
        ( _tagName tag
        , ( _taggedrangeStart taggedRange, _taggedrangebywordStart taggedRangeByWord
          , _taggedrangeEnd taggedRange, _taggedrangebywordEnd taggedRangeByWord
          )
        )
    pure (k, taggedRangeNote)
  where
    tuplesToKey (name, (a, b, c, d)) = (name, ClosedInterval' (a, b) (c, d))

guardExactTagRangeMatches
  :: ( SqlIn (QGenExpr QValueContext Pg.Postgres s) (Columnar f1 Text)
     , SqlEq (QGenExpr QValueContext Pg.Postgres s) (Columnar f2 Int)
     , SqlEq (QGenExpr QValueContext Pg.Postgres s) (VerseReferenceT f3)
     , SqlValable (Columnar f1 Text), SqlValable (Columnar f2 Int)
     , SqlValable (VerseReferenceT f3), HaskellLiteralForQExpr (Columnar f1 Text) ~ Text
     , HaskellLiteralForQExpr (VerseReferenceT f3) ~ VerseReferenceT Identity
     , HaskellLiteralForQExpr (Columnar f2 Int) ~ Int
     )
  => Set (Text, ClosedInterval' (VerseReference, Int))
  -> (TagT f1, TaggedRangeT f3, TaggedRangeByWordT f2)
  -> Q Pg.Postgres db s ()
guardExactTagRangeMatches (tags :: Set (Text, ClosedInterval' (VerseReference, Int))) (tag, taggedRange, taggedRangeByWord) = do
  guard_ (_tagName tag `in_` map (val_ . fst) (toList tags))
  guard_ $ foldAny_ $ do
    (_, ClosedInterval' (ref1, word1) (ref2, word2)) <- toList tags
    pure
      $   _taggedrangeStart taggedRange ==. val_ ref1
      &&. _taggedrangeEnd taggedRange ==. val_ ref2
      &&. _taggedrangebywordStart taggedRangeByWord ==. val_ word1
      &&. _taggedrangebywordEnd taggedRangeByWord ==. val_ word2

foldAny_ :: Foldable t => t (QGenExpr context Pg.Postgres s Bool) -> QGenExpr context Pg.Postgres s Bool
foldAny_ = foldlStrictWithZero (||.) (val_ False)

foldlStrictWithZero :: Foldable t => (a -> a -> a) -> a -> t a -> a
foldlStrictWithZero o zero xs
  | null xs = zero
  | otherwise = foldl1' o $ toList xs
