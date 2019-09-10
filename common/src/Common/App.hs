{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.App where

import Data.Aeson (parseJSON, toJSON)
import qualified Data.Aeson as Json
import Data.Align (Align (nil), Semialign (alignWith, zipWith))
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.GADT.Show.TH (deriveGShow)
import Data.Witherable (Filterable)
import Data.MonoidMap (MonoidMap)
import Reflex.Query.Class (Query (QueryResult, crop), SelectedCount)
import Reflex.Patch (Additive, Group (negateG))
import Rhyolite.App (PositivePart (positivePart), standardPositivePart)

import Common.Prelude

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

data ViewSelector a = ViewSelector
  deriving (Eq, Functor, Generic)
deriveJSON Json.defaultOptions 'ViewSelector
instance Semigroup a => Semigroup (ViewSelector a) where
  ViewSelector <> ViewSelector = ViewSelector
instance Semigroup a => Monoid (ViewSelector a) where
  mempty = ViewSelector
  mappend = (<>)
instance Semialign ViewSelector where
  alignWith _f _xs _ys = ViewSelector
  zipWith _f _xs _ys = ViewSelector
instance Align ViewSelector where
  nil = ViewSelector
instance (Group a) => Group (ViewSelector a) where
  negateG = fmap negateG
instance (Semigroup a) => Additive (ViewSelector a)
instance (Ord k) => PositivePart (ViewSelector (MonoidMap k SelectedCount)) where
  positivePart x =
    let u = mapMaybe standardPositivePart x
    in if u == mempty then Nothing else Just u
instance Filterable ViewSelector where
  mapMaybe _f _x = ViewSelector
instance (Monoid a) => Query (ViewSelector a) where
  type QueryResult (ViewSelector a) = View a
  crop ViewSelector View = View

data View a = View
  deriving (Eq, Foldable, Functor, Generic)
deriveJSON Json.defaultOptions 'View
instance Semigroup a => Semigroup (View a) where
  View <> View = View
instance Semigroup a => Monoid (View a) where
  mempty = View
  mappend = (<>)
instance Semialign View where
  alignWith _f _xs _ys = View
  zipWith _f _xs _ys = View
instance Align View where
  nil = View
instance Filterable View where
  mapMaybe _f _x = View

data Notification a where
  Notification_NoOp :: Notification ()
deriving instance Show (Notification a)
fmap concat $ sequence
  [ deriveJSONGADT ''Notification
  , deriveArgDict ''Notification
  , deriveGShow ''Notification
  ]
