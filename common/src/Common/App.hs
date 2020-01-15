{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-} -- For deriveJSONGADT
{-# LANGUAGE QuantifiedConstraints #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.App where

import Control.Lens.TH (makeLenses)
import Data.Aeson (parseJSON, toJSON)
import qualified Data.Aeson as Json
import Data.Align (Align (nil), Semialign (alignWith))
import qualified Data.Align as Align
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.IntervalMap.Generic.Strict (IntervalMap)
import qualified Data.IntervalMap.Generic.Strict as IntervalMap
import Data.IntervalMap.Interval (Interval (..))
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import qualified Data.Map.Monoidal as MMap
import Data.Semigroup (First (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Witherable (Witherable (wither))
import Data.MonoidMap (MonoidMap)
import Reflex.Query.Class (Query (QueryResult, crop), SelectedCount (..))
import Reflex.Patch (Additive, Group (negateG))
import Rhyolite.App (PositivePart (positivePart), standardPositivePart)

import Common.Prelude
import Common.Schema

-- | A simple pair describing the two endpoints of a closed interval.
data ClosedInterval' a = ClosedInterval' !a !a
  deriving (Eq, Functor, Foldable, Generic, Ord, Show, Traversable)
  deriving anyclass (FromJSON, Json.FromJSONKey, ToJSON, Json.ToJSONKey)

closedIntervalToInterval :: ClosedInterval' a -> Interval a
closedIntervalToInterval (ClosedInterval' a b) = ClosedInterval a b

data TagOccurrence = TagOccurrence
  { _tagOccurrence_name :: !Text
  , _tagOccurrence_translation :: !TranslationId
  , _tagOccurrence_interval :: !(ClosedInterval' (VerseReference, Int))
  } deriving (Eq, Generic, Ord, Show)
deriveJSON Json.defaultOptions 'TagOccurrence

data PublicRequest a where
  PublicRequest_AddTag :: !TagOccurrence -> PublicRequest ()
  PublicRequest_DeleteTag :: !TagOccurrence -> PublicRequest ()
  PublicRequest_SetNotes :: !(Text, ClosedInterval' (VerseReference, Int)) -> Text -> UTCTime -> PublicRequest ()
deriving instance Show a => Show (PublicRequest a)
fmap concat $ sequence
  [ deriveJSONGADT ''PublicRequest
  , deriveArgDict ''PublicRequest
  ]

data PrivateRequest a where
  PrivateRequest_NoOp :: PrivateRequest ()
fmap concat $ sequence
  [ deriveJSONGADT ''PrivateRequest
  , deriveArgDict ''PrivateRequest
  ]
deriving instance Show a => Show (PrivateRequest a)

-- ORPHANS
-- https://github.com/isomorphism/these/pull/121
deriving newtype instance Semialign Option
deriving newtype instance Align Option

-- https://github.com/fumieval/witherable/pull/43
instance Filterable Option where
  mapMaybe f = (>>= Option . f)
  {-# INLINE mapMaybe #-}
instance Witherable Option where
  wither f (Option x) = Option <$> wither f x
  {-# INLINE wither #-}

------------------
data IntervalCarrierType = IntervalCarrierType_CO | IntervalCarrierType_Closed | IntervalCarrierType_Open | IntervalCarrierType_OC
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (ToJSON, FromJSON)

intervalToCarrier :: Interval a -> (IntervalCarrierType, a, a)
intervalToCarrier = \case
  IntervalCO a b -> (IntervalCarrierType_CO, a, b)
  ClosedInterval a b -> (IntervalCarrierType_Closed, a, b)
  OpenInterval a b -> (IntervalCarrierType_Open, a, b)
  IntervalOC a b -> (IntervalCarrierType_OC, a, b)

carrierToInterval :: (IntervalCarrierType, a, a) -> Interval a
carrierToInterval (t, a, b) = case t of
  IntervalCarrierType_CO -> IntervalCO a b
  IntervalCarrierType_Closed -> ClosedInterval a b
  IntervalCarrierType_Open -> OpenInterval a b
  IntervalCarrierType_OC -> IntervalOC a b

-- Orphan
instance ToJSON a => ToJSON (Interval a) where
  toJSON = toJSON . intervalToCarrier
  toEncoding = Json.toEncoding . intervalToCarrier
instance FromJSON a => FromJSON (Interval a) where
  parseJSON = fmap carrierToInterval . parseJSON

instance ToJSON k => Json.ToJSONKey (Interval k)
instance FromJSON k => Json.FromJSONKey (Interval k)
------------------

nullToNothing :: Foldable f => f a -> Maybe (f a)
nullToNothing a = if null a then Nothing else Just a

mapMaybe2Deep :: (Foldable t, Filterable f, Filterable t) => (a -> Maybe b) -> f (t a) -> f (t b)
mapMaybe2Deep f = mapMaybe (nullToNothing . mapMaybe f)

data ViewSelector a = ViewSelector
  { _viewSelector_translations :: !(Option a)
  , _viewSelector_verseRanges :: !(MonoidalMap TranslationId (MonoidalMap (Interval VerseReference) a))
  , _viewSelector_tagNotes :: !(MonoidalMap (Text, ClosedInterval' (VerseReference, Int)) a)
  }
  deriving (Eq, Functor, Generic, Show)
deriveJSON Json.defaultOptions 'ViewSelector
makeLenses 'ViewSelector
instance Semigroup a => Semigroup (ViewSelector a) where
  a <> b = ViewSelector
    { _viewSelector_translations = _viewSelector_translations a <> _viewSelector_translations b
    , _viewSelector_verseRanges = _viewSelector_verseRanges a <> _viewSelector_verseRanges b
    , _viewSelector_tagNotes = _viewSelector_tagNotes a <> _viewSelector_tagNotes b
    }
instance Semigroup a => Monoid (ViewSelector a) where
  mempty = ViewSelector mempty mempty mempty
  mappend = (<>)
instance Semialign ViewSelector where
  alignWith f a b = ViewSelector
    { _viewSelector_translations = alignWith f (_viewSelector_translations a) (_viewSelector_translations b)
    , _viewSelector_verseRanges = getCompose $ alignWith f (Compose $ _viewSelector_verseRanges a) (Compose $ _viewSelector_verseRanges b)
    , _viewSelector_tagNotes = alignWith f (_viewSelector_tagNotes a) (_viewSelector_tagNotes b)
    }
  zipWith f a b = ViewSelector
    { _viewSelector_translations = Align.zipWith f (_viewSelector_translations a) (_viewSelector_translations b)
    , _viewSelector_verseRanges = getCompose $ Align.zipWith f (Compose $ _viewSelector_verseRanges a) (Compose $ _viewSelector_verseRanges b)
    , _viewSelector_tagNotes = Align.zipWith f (_viewSelector_tagNotes a) (_viewSelector_tagNotes b)
    }
instance Align ViewSelector where
  nil = ViewSelector nil nil nil
instance (Group a) => Group (ViewSelector a) where
  negateG = fmap negateG
instance (Semigroup a) => Additive (ViewSelector a)
instance (Ord k) => PositivePart (ViewSelector (MonoidMap k SelectedCount)) where
  positivePart x =
    let u = mapMaybe standardPositivePart x
    in if u == mempty then Nothing else Just u
instance Filterable ViewSelector where
  mapMaybe f x = ViewSelector
    { _viewSelector_translations = mapMaybe f (_viewSelector_translations x)
    , _viewSelector_verseRanges = mapMaybe2Deep f (_viewSelector_verseRanges x)
    , _viewSelector_tagNotes = mapMaybe f (_viewSelector_tagNotes x)
    }
instance (Monoid a, Eq a) => Query (ViewSelector a) where
  type QueryResult (ViewSelector a) = View a
  crop vs v = View
    { _view_translations = if null $ _viewSelector_translations vs then mempty else _view_translations v
    , _view_tagNotes = croppedIntersectionWith (flip const) (_viewSelector_tagNotes vs) (_view_tagNotes v)
    , _view_verseRanges = verseRanges
    , _view_verses = rederiveVerses verseRanges (_view_verses v)
    , _view_tags = rederiveTags verseRanges (_view_tags v)
    }
    where
      verseRanges = croppedIntersectionWith (croppedIntersectionWith const) (_viewSelector_verseRanges vs) (_view_verseRanges v)

-- Intersect a map from the ViewSelector and a map from the view to produce a cropped map for the view, dropping any key for which the entry is selected mempty (zero) times.
croppedIntersectionWith :: (Ord k, Eq a, Monoid a) => (a -> b -> c) -> MonoidalMap k a -> MonoidalMap k b -> MonoidalMap k c
croppedIntersectionWith f (MMap.MonoidalMap m) (MMap.MonoidalMap m') = MMap.MonoidalMap $
  Map.merge
    Map.dropMissing
    Map.dropMissing
    (Map.zipWithMaybeMatched (\_ a v -> if a == mempty then Nothing else Just (f a v)))
    m
    m'

rederiveVerses
  :: forall translationId a b. (Ord translationId)
  => MonoidalMap translationId (MonoidalMap (Interval VerseReference) a)
  -> MonoidalMap translationId (MonoidalMap VerseReference (b, First Text))
  -> MonoidalMap translationId (MonoidalMap VerseReference (Seq a, First Text))
rederiveVerses verseRanges verses = flip MMap.mapMaybeWithKey verses $ \k v ->
  let
    ranges :: IntervalMap (Interval VerseReference) a =
      maybe mempty (IntervalMap.fromDistinctAscList . MMap.toAscList) $ MMap.lookup k verseRanges
  in
    if null ranges then Nothing else nullToNothing $ flip MMap.mapMaybeWithKey v $ \ref (_, text) ->
      let correspondingRanges = ranges `IntervalMap.containing` ref
      in if null correspondingRanges then Nothing else Just (Seq.fromList $ IntervalMap.elems correspondingRanges, text)

rederiveTags
  :: forall translationId tagName a b. (Ord translationId)
  => MonoidalMap translationId (MonoidalMap (Interval VerseReference) a)
  -> MonoidalMap translationId (MonoidalMap tagName (MonoidalMap (ClosedInterval' (VerseReference, Int)) (First Presence, b)))
  -> MonoidalMap translationId (MonoidalMap tagName (MonoidalMap (ClosedInterval' (VerseReference, Int)) (First Presence, Seq a)))
rederiveTags verseRanges tags = flip MMap.mapMaybeWithKey tags $ \translationId tagNames ->
    let
      ranges :: IntervalMap (Interval VerseReference) a =
        maybe mempty (IntervalMap.fromDistinctAscList . MMap.toAscList) $ MMap.lookup translationId verseRanges
    in
      if null ranges then Nothing else nullToNothing $ flip MMap.mapMaybeWithKey tagNames $ \_ tagRanges ->
        nullToNothing $ flip MMap.mapMaybeWithKey tagRanges $ \(ClosedInterval' (ref1, _) (ref2, _)) (presence, _) ->
          let correspondingRanges = ranges `IntervalMap.intersecting` ClosedInterval ref1 ref2
          in if null correspondingRanges then Nothing else Just (presence, Seq.fromList (IntervalMap.elems correspondingRanges))

data View a = View
  { _view_translations :: !(Option (a, MonoidalMap TranslationId (First Translation)))
  , _view_verseRanges :: !(MonoidalMap TranslationId (MonoidalMap (Interval VerseReference) a))
  , _view_tagNotes :: !(MonoidalMap (Text, ClosedInterval' (VerseReference, Int)) (a, First (Text, UTCTime)))
  , _view_verses :: !(MonoidalMap TranslationId (MonoidalMap VerseReference (Seq a, First Text)))
  , _view_tags :: !(MonoidalMap TranslationId (MonoidalMap Text (MonoidalMap (ClosedInterval' (VerseReference, Int)) (First Presence, Seq a))))
  }
  deriving (Eq, Foldable, Functor, Generic, Show)
deriveJSON Json.defaultOptions 'View
makeLenses 'View
instance Monoid a => Semigroup (View a) where
  a <> b = View
    { _view_translations = _view_translations a <> _view_translations b
    , _view_verseRanges = verseRanges
    , _view_tagNotes = _view_tagNotes a <> _view_tagNotes b
    , _view_verses = rederiveVerses verseRanges (_view_verses a <> _view_verses b)
    , _view_tags = rederiveTags verseRanges (_view_tags a <> _view_tags b)
    }
    where
      verseRanges = _view_verseRanges a <> _view_verseRanges b
instance Monoid a => Monoid (View a) where
  mempty = View mempty mempty mempty mempty mempty
  mappend = (<>)
instance Filterable View where
  mapMaybe f x = View
    { _view_translations = mapMaybeView f (_view_translations x)
    , _view_tagNotes = mapMaybeView f (_view_tagNotes x)
    , _view_verseRanges = verseRanges
    , _view_verses = rederiveVerses verseRanges (_view_verses x)
    , _view_tags = rederiveTags verseRanges (_view_tags x)
    }
    where
      verseRanges = mapMaybe2Deep f $ _view_verseRanges x

mapMaybeView
  :: forall f v a b. (Filterable f)
  => (a -> Maybe b)
  -> f (a, v)
  -> f (b, v)
mapMaybeView f = mapMaybe ((_1 :: (a -> Maybe b) -> (a, v) -> Maybe (b, v)) f)

restrictKeys :: forall k v. Ord k => MonoidalMap k v -> Set k -> MonoidalMap k v
restrictKeys = coerce (Map.restrictKeys :: Map k v -> Set k -> Map k v)
