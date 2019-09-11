{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Schema where

import qualified Data.Aeson as Json
import Data.Text (Text)
import Database.Beam (Beamable, Columnar, PrimaryKey, Table (primaryKey))
import GHC.Generics (Generic)

import Common.Prelude

data TranslationT f = Translation
 { translationId :: Columnar f Int
 , translationName :: Columnar f Text
 , translationAbbreviation :: Columnar f Text
 } deriving (Generic, Beamable)
instance Table TranslationT where
  data PrimaryKey TranslationT f = TranslationId (Columnar f Int) deriving stock Generic deriving anyclass Beamable
  primaryKey = TranslationId . translationId

type Translation = TranslationT Identity
deriving instance Eq Translation
deriving instance Show Translation
instance FromJSON Translation
instance ToJSON Translation

type TranslationId = PrimaryKey TranslationT Identity
deriving instance Eq TranslationId
deriving instance Ord TranslationId
deriving instance Show TranslationId
instance FromJSON TranslationId
instance ToJSON TranslationId
instance Json.ToJSONKey TranslationId
instance Json.FromJSONKey TranslationId

data BookT f = Book
  { bookId :: Columnar f Int
  , bookName :: Columnar f Text
  } deriving (Generic, Beamable)
instance Table BookT where
  data PrimaryKey BookT f = BookId (Columnar f Int) deriving stock Generic deriving anyclass Beamable
  primaryKey = BookId . bookId

type BookId = PrimaryKey BookT Identity
deriving instance Eq BookId
deriving instance Ord BookId
deriving instance Show BookId
instance FromJSON BookId
instance ToJSON BookId

data VerseT f = VerseT
  { verseTranslation :: PrimaryKey TranslationT f
  , verseBook :: PrimaryKey BookT f
  , verseChapter :: Columnar f Int
  , verseVerse :: Columnar f Int
  , verseText :: Columnar f Text
  } deriving (Generic, Beamable)
instance Table VerseT where
  data PrimaryKey VerseT f = VerseId (PrimaryKey TranslationT f) (PrimaryKey BookT f) (Columnar f Int) (Columnar f Int) deriving stock Generic deriving anyclass Beamable
  primaryKey = VerseId <$> verseTranslation <*> verseBook <*> verseChapter <*> verseVerse

type Verse = VerseT Identity
deriving instance Eq Verse
deriving instance Show Verse
instance FromJSON Verse
instance ToJSON Verse

type VerseId = PrimaryKey VerseT Identity
deriving instance Eq VerseId
deriving instance Ord VerseId
deriving instance Show VerseId
instance FromJSON VerseId
instance ToJSON VerseId
