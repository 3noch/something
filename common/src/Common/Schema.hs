{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
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
  , bookAbbreviation :: Columnar f Text
  } deriving (Generic, Beamable)
instance Table BookT where
  data PrimaryKey BookT f = BookId (Columnar f Int) deriving stock Generic deriving anyclass Beamable
  primaryKey = BookId . bookId

data VerseT f = VerseT
  { verseId :: Columnar f Int
  , verseTranslation :: PrimaryKey TranslationT f
  , verseBook :: PrimaryKey BookT f
  , verseChapter :: Columnar f Int
  , verseVerse :: Columnar f Int
  , verseText :: Columnar f Text
  } deriving (Generic, Beamable)
instance Table VerseT where
  data PrimaryKey VerseT f = VerseId (Columnar f Int) deriving stock Generic deriving anyclass Beamable
  primaryKey = VerseId . verseId
