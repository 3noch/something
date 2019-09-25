{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Schema where

import qualified Data.Aeson as Json
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Beam (Beamable, Columnar, PrimaryKey, Table (primaryKey))
import GHC.Generics (Generic)

import Common.Prelude

-------------------------------------------------------------------------------
data TranslationT f = Translation
 { _translationId :: Columnar f Int
 , _translationName :: Columnar f Text
 , _translationAbbreviation :: Columnar f Text
 } deriving (Generic, Beamable)
instance Table TranslationT where
  data PrimaryKey TranslationT f = TranslationId (Columnar f Int) deriving stock Generic deriving anyclass Beamable
  primaryKey = TranslationId . _translationId
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
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
data BookT f = Book
  { _bookId :: Columnar f Int
  , _bookName :: Columnar f Text
  } deriving (Generic, Beamable)
instance Table BookT where
  data PrimaryKey BookT f = BookId (Columnar f Int) deriving stock Generic deriving anyclass Beamable
  primaryKey = BookId . _bookId

type BookId = PrimaryKey BookT Identity
deriving instance Eq BookId
deriving instance Ord BookId
deriving instance Show BookId
instance FromJSON BookId
instance ToJSON BookId
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
data VerseT f = VerseT
  { _verseTranslation :: PrimaryKey TranslationT f
  , _verseBook :: PrimaryKey BookT f
  , _verseChapter :: Columnar f Int
  , _verseVerse :: Columnar f Int
  , _verseText :: Columnar f Text
  } deriving (Generic, Beamable)
instance Table VerseT where
  data PrimaryKey VerseT f = VerseId (PrimaryKey TranslationT f) (PrimaryKey BookT f) (Columnar f Int) (Columnar f Int) deriving stock Generic deriving anyclass Beamable
  primaryKey = VerseId <$> _verseTranslation <*> _verseBook <*> _verseChapter <*> _verseVerse

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
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
data TagT f = TagT
  { _tagId :: Columnar f Int
  , _tagName :: Columnar f Text
  } deriving (Generic, Beamable)
instance Table TagT where
  data PrimaryKey TagT f = TagId (Columnar f Int) deriving stock Generic deriving anyclass Beamable
  primaryKey = TagId <$> _tagId

type Tag = TagT Identity
deriving instance Eq Tag
deriving instance Show Tag
instance FromJSON Tag
instance ToJSON Tag

type TagId = PrimaryKey TagT Identity
deriving instance Eq TagId
deriving instance Ord TagId
deriving instance Show TagId
instance FromJSON TagId
instance ToJSON TagId
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
data VerseReferenceT f = VerseReferenceT
  { _versereferenceBook :: PrimaryKey BookT f
  , _versereferenceChapter :: Columnar f Int
  , _versereferenceVerse :: Columnar f Int
  }
  deriving stock Generic
  deriving anyclass Beamable
deriving instance Eq (VerseReferenceT Identity)
deriving instance Show (VerseReferenceT Identity)
instance FromJSON (VerseReferenceT Identity)
instance ToJSON (VerseReferenceT Identity)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
data TaggedRangeT f = TaggedRangeT
  { _taggedrangeId :: Columnar f Int
  , _taggedrangeFor :: PrimaryKey TagT f
  , _taggedrangeStart :: VerseReferenceT f
  , _taggedrangeEnd :: VerseReferenceT f
  } deriving (Generic, Beamable)
instance Table TaggedRangeT where
  data PrimaryKey TaggedRangeT f = TaggedRangeId (Columnar f Int) deriving stock Generic deriving anyclass Beamable
  primaryKey = TaggedRangeId <$> _taggedrangeId
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
data TaggedRangeByWordT f = TaggedRangeByWordT
  { _taggedrangebywordForRange :: PrimaryKey TaggedRangeT f
  , _taggedrangebywordForTranslation :: PrimaryKey TranslationT f
  , _taggedrangebywordStart :: Columnar f Int
  , _taggedrangebywordEnd :: Columnar f Int
  } deriving (Generic, Beamable)
instance Table TaggedRangeByWordT where
  data PrimaryKey TaggedRangeByWordT f = TaggedRangeByWordId (PrimaryKey TaggedRangeT f) (PrimaryKey TranslationT f) deriving stock Generic deriving anyclass Beamable
  primaryKey = TaggedRangeByWordId <$> _taggedrangebywordForRange <*> _taggedrangebywordForTranslation
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
data VerseReference = VerseReference
  { _verseReference_book :: !Int
  , _verseReference_chapter :: !Int
  , _verseReference_verse :: !Int
  } deriving (Eq, Generic, Show, Ord)
deriveJSON Json.defaultOptions 'VerseReference
instance Json.ToJSONKey VerseReference
instance Json.FromJSONKey VerseReference

verseToVerseReference :: Verse -> VerseReference
verseToVerseReference v = VerseReference
  { _verseReference_book = let BookId x = _verseBook v in x
  , _verseReference_chapter = _verseChapter v
  , _verseReference_verse = _verseVerse v
  }

verseToVerseReferenceTuple :: VerseT f -> (PrimaryKey BookT f, Columnar f Int, Columnar f Int)
verseToVerseReferenceTuple v = (_verseBook v, _verseChapter v, _verseVerse v)

showVerseReference :: VerseReference -> Text
showVerseReference (VerseReference b c v) = T.pack (show b <> "@" <> show c <> ":" <> show v)
-------------------------------------------------------------------------------
