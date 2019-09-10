{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Schema where

import Data.Text (Text)
import Database.Beam (Beamable, Columnar, PrimaryKey, Table (primaryKey))
import GHC.Generics (Generic)

data TranslationT f = Translation
 { translationId :: Columnar f Int
 , translationName :: Columnar f Text
 , translationAbbreviation :: Columnar f Text
 } deriving (Generic, Beamable)
instance Table TranslationT where
  data PrimaryKey TranslationT f = TranslationId (Columnar f Int) deriving stock Generic deriving anyclass Beamable
  primaryKey = TranslationId . translationId

data BookT f = Book
  { bookId :: Columnar f Int
  , bookName :: Columnar f Text
  } deriving (Generic, Beamable)
instance Table BookT where
  data PrimaryKey BookT f = BookId (Columnar f Int) deriving stock Generic deriving anyclass Beamable
  primaryKey = BookId . bookId

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
