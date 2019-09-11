{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.App where

import Control.Lens (_1)
import Data.Aeson (parseJSON, toJSON)
import qualified Data.Aeson as Json
import Data.Align (Align (nil), Semialign (alignWith))
import qualified Data.Align as Align
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.GADT.Show.TH (deriveGShow)
import Data.Semigroup (First)
import Data.Witherable (Witherable (wither))
import Data.MonoidMap (MonoidMap)
import Reflex.Query.Class (Query (QueryResult, crop), SelectedCount (..))
import Reflex.Patch (Additive, Group (negateG))
import Rhyolite.App (PositivePart (positivePart), standardPositivePart)

import Common.Prelude
import Common.Schema

data PublicRequest a where
  PublicRequest_NoOp :: PublicRequest ()
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

-- WAT why is this is not already somewhere?
deriving newtype instance FromJSON SelectedCount
deriving newtype instance ToJSON SelectedCount


newtype TranslationAbbreviation = TranslationAbbreviation Text
  deriving newtype (Eq, Show, Ord, FromJSON, ToJSON)

data VerseReference = VerseReference
  { _verseReference_book :: !Int
  , _verseReference_chapter :: !Int
  , _verseReference_verse :: !Int
  } deriving (Eq, Generic, Show, Ord)
deriveJSON Json.defaultOptions 'VerseReference

data ClosedRange a = ClosedRange !a !a
  deriving (Eq, Generic, Show, Ord)
deriveJSON Json.defaultOptions 'ClosedRange

data ViewSelector a = ViewSelector
  { _viewSelector_translations :: !(Option a)
  , _viewSelector_verses :: !(MonoidalMap (TranslationAbbreviation, ClosedRange VerseReference) a)
  }
  deriving (Eq, Functor, Generic)
deriveJSON Json.defaultOptions 'ViewSelector
instance Semigroup a => Semigroup (ViewSelector a) where
  a <> b = ViewSelector
    { _viewSelector_translations = _viewSelector_translations a <> _viewSelector_translations b
    , _viewSelector_verses = _viewSelector_verses a <> _viewSelector_verses b
    }
instance Semigroup a => Monoid (ViewSelector a) where
  mempty = ViewSelector mempty mempty
  mappend = (<>)
instance Semialign ViewSelector where
  alignWith f a b = ViewSelector
    { _viewSelector_translations = alignWith f (_viewSelector_translations a) (_viewSelector_translations b)
    , _viewSelector_verses = alignWith f (_viewSelector_verses a) (_viewSelector_verses b)
    }
  zipWith f a b = ViewSelector
    { _viewSelector_translations = Align.zipWith f (_viewSelector_translations a) (_viewSelector_translations b)
    , _viewSelector_verses = Align.zipWith f (_viewSelector_verses a) (_viewSelector_verses b)
    }
instance Align ViewSelector where
  nil = ViewSelector nil nil
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
    , _viewSelector_verses = mapMaybe f (_viewSelector_verses x)
    }
instance (Monoid a) => Query (ViewSelector a) where
  type QueryResult (ViewSelector a) = View a
  crop ViewSelector{} v = v -- TODO

data View a = View
  { _view_translations :: !(Option (a, MonoidalMap TranslationId (First Translation)))
  , _view_verses :: !(MonoidalMap (TranslationAbbreviation, ClosedRange VerseReference) (a, [Verse]))
  }
  deriving (Eq, Foldable, Functor, Generic)
deriveJSON Json.defaultOptions 'View
instance Semigroup a => Semigroup (View a) where
  a <> b = View
    { _view_translations = _view_translations a <> _view_translations b
    , _view_verses = _view_verses a <> _view_verses b
    }
instance Semigroup a => Monoid (View a) where
  mempty = View mempty mempty
  mappend = (<>)
instance Filterable View where
  mapMaybe f x = View
    { _view_translations = mapMaybeView f (_view_translations x)
    , _view_verses = mapMaybeView f (_view_verses x)
    }

mapMaybeView
  :: forall f v a b.
  ( Filterable f )
  => (a -> Maybe b)
  -> f (a, v)
  -> f (b, v)
mapMaybeView f = mapMaybe ((_1 :: (a -> Maybe b) -> (a, v) -> Maybe (b, v)) f)

nothingOnNull :: Foldable f => f a -> Maybe (f a)
nothingOnNull f = if null f then Nothing else Just f

data Notification a where
  Notification_NoOp :: Notification ()
deriving instance Show (Notification a)
fmap concat $ sequence
  [ deriveJSONGADT ''Notification
  , deriveArgDict ''Notification
  , deriveGShow ''Notification
  ]
