{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Schema where

import qualified Data.Aeson as Json
import Data.Text (Text)
import qualified Data.Text as T
import Database.Beam (Beamable, Columnar, PrimaryKey, Table (primaryKey))
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Data.Time (LocalTime)
import Common.Prelude
-------------------------------------------------------------------------------
data TranslationT f = Translation
 { _translationId :: Columnar f Int
 , _translationName :: Columnar f Text
 , _translationAbbreviation :: Columnar f Text
 } deriving (Generic, Beamable)
instance Table TranslationT where
  newtype PrimaryKey TranslationT f = TranslationId { unTranslationId :: Columnar f Int } deriving stock Generic deriving anyclass Beamable
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
  newtype PrimaryKey BookT f = BookId { unBookId :: Columnar f Int } deriving stock Generic deriving anyclass Beamable
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
  { _tagId :: Columnar f (SqlSerial Int)
  , _tagName :: Columnar f Text
  } deriving (Generic, Beamable)
instance Table TagT where
  newtype PrimaryKey TagT f = TagId (Columnar f (SqlSerial Int)) deriving stock Generic deriving anyclass Beamable
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

type VerseReference = VerseReferenceT Identity
deriving instance Eq VerseReference
deriving instance Ord VerseReference
deriving instance Show VerseReference
instance FromJSON VerseReference
instance ToJSON VerseReference
instance Json.ToJSONKey VerseReference
instance Json.FromJSONKey VerseReference
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
data TaggedRangeT f = TaggedRangeT
  { _taggedrangeId :: Columnar f (SqlSerial Int)
  , _taggedrangeForTag :: PrimaryKey TagT f
  , _taggedrangeStart :: VerseReferenceT f
  , _taggedrangeEnd :: VerseReferenceT f
  } deriving (Generic, Beamable)
instance Table TaggedRangeT where
  newtype PrimaryKey TaggedRangeT f = TaggedRangeId (Columnar f (SqlSerial Int)) deriving stock Generic deriving anyclass Beamable
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
data TaggedRangeNoteT f = TaggedRangeNoteT
  { _taggedrangenoteId :: Columnar f (SqlSerial Int)
  , _taggedrangenoteForRange :: PrimaryKey TaggedRangeT f
  , _taggedrangenoteContent :: Columnar f Text
  , _taggedrangenoteUpdated :: Columnar f LocalTime -- TODO: Use UTCTime
  } deriving (Generic, Beamable)
instance Table TaggedRangeNoteT where
  newtype PrimaryKey TaggedRangeNoteT f = TaggedRangeNoteId (Columnar f (SqlSerial Int)) deriving stock Generic deriving anyclass Beamable
  primaryKey = TaggedRangeNoteId <$> _taggedrangenoteId

type TaggedRangeNote = TaggedRangeNoteT Identity

type TaggedRangeNoteId = PrimaryKey TaggedRangeNoteT Identity
deriving instance Eq TaggedRangeNoteId
deriving instance Ord TaggedRangeNoteId
deriving instance Show TaggedRangeNoteId
instance FromJSON TaggedRangeNoteId
instance ToJSON TaggedRangeNoteId
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
data Presence = Present | Absent
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
verseToVerseReference :: VerseT f -> VerseReferenceT f
verseToVerseReference v = VerseReferenceT
  { _versereferenceBook = _verseBook v
  , _versereferenceChapter = _verseChapter v
  , _versereferenceVerse = _verseVerse v
  }

verseToVerseReferenceTuple :: VerseT f -> (PrimaryKey BookT f, Columnar f Int, Columnar f Int)
verseToVerseReferenceTuple v = (_verseBook v, _verseChapter v, _verseVerse v)

showVerseReference :: VerseReference -> Text
showVerseReference (VerseReferenceT b c v) = T.pack (show b <> "@" <> show c <> ":" <> show v)
-------------------------------------------------------------------------------
