{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Bible where

import Data.Finite (Finite, natToFinite)
import Data.Functor.Identity (Identity)
import Data.Proxy (Proxy (..))
import Data.Universe (Universe (universe), universeGeneric)
import qualified Data.Universe as Universe
import GHC.Generics (Generic)

-- Type safety at the cost of boilerplate :/

data Two = Two_1 | Two_2
  deriving (Bounded, Enum, Eq, Generic, Ord, Show, Read)
instance Universe Two
instance Universe.Finite Two

twoToFinite :: Two -> Finite 2
twoToFinite Two_1 = natToFinite (Proxy :: Proxy 0)
twoToFinite Two_2 = natToFinite (Proxy :: Proxy 1)

data Three = Three_1 | Three_2 | Three_3
  deriving (Bounded, Enum, Eq, Generic, Ord, Show, Read)
instance Universe Three
instance Universe.Finite Three

threeToFinite :: Three -> Finite 3
threeToFinite Three_1 = natToFinite (Proxy :: Proxy 0)
threeToFinite Three_2 = natToFinite (Proxy :: Proxy 1)
threeToFinite Three_3 = natToFinite (Proxy :: Proxy 2)

data Four = Four_1 | Four_2 | Four_3 | Four_4
  deriving (Bounded, Enum, Eq, Generic, Ord, Show, Read)
instance Universe Four
instance Universe.Finite Four

fourToFinite :: Four -> Finite 4
fourToFinite Four_1 = natToFinite (Proxy :: Proxy 0)
fourToFinite Four_2 = natToFinite (Proxy :: Proxy 1)
fourToFinite Four_3 = natToFinite (Proxy :: Proxy 2)
fourToFinite Four_4 = natToFinite (Proxy :: Proxy 3)

-- Used as an index for 'BookIndex' to mean "ignore book indexes".
newtype Ignore a = Ignore ()

type family BookIndex f a where
  BookIndex Identity a = a
  BookIndex Ignore a = ()
  BookIndex f a = f a

data OldTestament' f
  = Genesis
  | Exodus
  | Leviticus
  | Numbers
  | Deuteronomy
  | Joshua
  | Judges
  | Ruth
  | Samuel (BookIndex f Two)
  | Kings (BookIndex f Two)
  | Chronicles (BookIndex f Two)
  | Ezra
  | Nehemiah
  | Esther
  | Job
  | Psalms
  | Proverbs
  | Ecclesiastes
  | SongOfSolomon
  | Isaiah
  | Jeremiah
  | Lamentations
  | Ezekiel
  | Daniel
  | Hosea
  | Joel
  | Amos
  | Obadiah
  | Jonah
  | Micah
  | Nahum
  | Habakkuk
  | Zephaniah
  | Haggai
  | Zechariah
  | Malachi
  deriving (Generic)
deriving instance Eq (OldTestament' Ignore)
deriving instance Eq (OldTestament' Identity)
deriving instance Ord (OldTestament' Ignore)
deriving instance Ord (OldTestament' Identity)
deriving instance Show (OldTestament' Ignore)
deriving instance Show (OldTestament' Identity)
deriving instance Read (OldTestament' Ignore)
deriving instance Read (OldTestament' Identity)
instance Universe (OldTestament' Ignore) where universe = universeGeneric
instance Universe.Finite (OldTestament' Ignore)
instance Universe (OldTestament' Identity) where universe = universeGeneric
instance Universe.Finite (OldTestament' Identity)
instance Bounded (OldTestament' f) where
  minBound = Genesis
  maxBound = Malachi

type OldTestament = OldTestament' Identity

data NewTestament' f
  = Matthew
  | Mark
  | Luke
  | John
  | Acts
  | Romans
  | Corinthians (BookIndex f Two)
  | Galatians
  | Ephesians
  | Philippians
  | Colossians
  | Thessalonians (BookIndex f Two)
  | Timothy (BookIndex f Two)
  | Titus
  | Philemon
  | Hebrews
  | James
  | Peter (BookIndex f Two)
  | JohnEpistle (BookIndex f Three)
  | Jude
  | Revelation
  deriving (Generic)
deriving instance Eq (NewTestament' Ignore)
deriving instance Eq (NewTestament' Identity)
deriving instance Ord (NewTestament' Ignore)
deriving instance Ord (NewTestament' Identity)
deriving instance Show (NewTestament' Ignore)
deriving instance Show (NewTestament' Identity)
deriving instance Read (NewTestament' Ignore)
deriving instance Read (NewTestament' Identity)
instance Universe (NewTestament' Ignore) where universe = universeGeneric
instance Universe.Finite (NewTestament' Ignore)
instance Universe (NewTestament' Identity) where universe = universeGeneric
instance Universe.Finite (NewTestament' Identity)
instance Bounded (NewTestament' f) where
  minBound = Matthew
  maxBound = Revelation

type NewTestament = NewTestament' Identity

data Canon' f
  = Canon_Old (OldTestament' f)
  | Canon_New (NewTestament' f)
  deriving (Generic)
deriving instance Eq (Canon' Ignore)
deriving instance Eq (Canon' Identity)
deriving instance Ord (Canon' Ignore)
deriving instance Ord (Canon' Identity)
deriving instance Show (Canon' Ignore)
deriving instance Show (Canon' Identity)
instance Universe (Canon' Ignore) where universe = universeGeneric
instance Universe.Finite (Canon' Ignore)
instance Universe (Canon' Identity) where universe = universeGeneric
instance Universe.Finite (Canon' Identity)
instance Bounded (Canon' f) where
  minBound = Canon_Old minBound
  maxBound = Canon_New maxBound

data Apocrypha' f
  = Tobit
  | Judith
  | AdditionsToEsther
  | WisdomOfSolomon
  | Sirach
  | Ecclesiasticus
  | LetterOfJeremiah
  | SongOfThreeYouths
  | PrayerOfAzariah
  | Susanna
  | BelAndTheDragon
  | Maccabees (BookIndex f Four)
  | Esdras (BookIndex f Two)
  | PrayerOfManasseh
  | AdditionalPsalm
  | PsalmsOfSolomon
  | EpistleToTheLaodiceans
  deriving (Generic)
deriving instance Eq (Apocrypha' Ignore)
deriving instance Eq (Apocrypha' Identity)
deriving instance Ord (Apocrypha' Ignore)
deriving instance Ord (Apocrypha' Identity)
deriving instance Show (Apocrypha' Ignore)
deriving instance Show (Apocrypha' Identity)
deriving instance Read (Apocrypha' Ignore)
deriving instance Read (Apocrypha' Identity)
instance Universe (Apocrypha' Ignore) where universe = universeGeneric
instance Universe.Finite (Apocrypha' Ignore)
instance Universe (Apocrypha' Identity) where universe = universeGeneric
instance Universe.Finite (Apocrypha' Identity)
instance Bounded (Apocrypha' f) where
  minBound = Tobit
  maxBound = EpistleToTheLaodiceans

type Apocrypha = Apocrypha' Identity