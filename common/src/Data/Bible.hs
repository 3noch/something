{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Data.Bible where

import Data.Functor.Identity (Identity)
import Data.Universe (Universe (universe), universeGeneric)
import qualified Data.Universe as Universe
import GHC.Generics (Generic)

-- Type safety at the cost of boilerplate :/

data Two = Two_1 | Two_2
  deriving (Bounded, Enum, Eq, Generic, Ord, Show, Read)
instance Universe Two
instance Universe.Finite Two

data Three = Three_1 | Three_2 | Three_3
  deriving (Bounded, Enum, Eq, Generic, Ord, Show, Read)
instance Universe Three
instance Universe.Finite Three

data Four = Four_1 | Four_2 | Four_3 | Four_4
  deriving (Bounded, Enum, Eq, Generic, Ord, Show, Read)
instance Universe Four
instance Universe.Finite Four

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
instance Universe (OldTestament' Identity) -- Uses 'Enum' instance
instance Universe.Finite (OldTestament' Identity)
instance Bounded (OldTestament' f) where
  minBound = Genesis
  maxBound = Malachi
instance Enum (OldTestament' Identity) where
  fromEnum x = case x of
    Genesis -> 0
    Exodus -> 1
    Leviticus -> 2
    Numbers -> 3
    Deuteronomy -> 4
    Joshua -> 5
    Judges -> 6
    Ruth -> 7
    Samuel Two_1 -> 8
    Samuel Two_2 -> 9
    Kings Two_1 -> 10
    Kings Two_2 -> 11
    Chronicles Two_1 -> 12
    Chronicles Two_2 -> 13
    Ezra -> 14
    Nehemiah -> 15
    Esther -> 16
    Job -> 17
    Psalms -> 18
    Proverbs -> 19
    Ecclesiastes -> 20
    SongOfSolomon -> 21
    Isaiah -> 22
    Jeremiah -> 23
    Lamentations -> 24
    Ezekiel -> 25
    Daniel -> 26
    Hosea -> 27
    Joel -> 28
    Amos -> 29
    Obadiah -> 30
    Jonah -> 31
    Micah -> 32
    Nahum -> 33
    Habakkuk -> 34
    Zephaniah -> 35
    Haggai -> 36
    Zechariah -> 37
    Malachi -> 38
  toEnum x = case x of
    0 -> Genesis
    1 -> Exodus
    2 -> Leviticus
    3 -> Numbers
    4 -> Deuteronomy
    5 -> Joshua
    6 -> Judges
    7 -> Ruth
    8 -> Samuel Two_1
    9 -> Samuel Two_2
    10 -> Kings Two_1
    11 -> Kings Two_2
    12 -> Chronicles Two_1
    13 -> Chronicles Two_2
    14 -> Ezra
    15 -> Nehemiah
    16 -> Esther
    17 -> Job
    18 -> Psalms
    19 -> Proverbs
    20 -> Ecclesiastes
    21 -> SongOfSolomon
    22 -> Isaiah
    23 -> Jeremiah
    24 -> Lamentations
    25 -> Ezekiel
    26 -> Daniel
    27 -> Hosea
    28 -> Joel
    29 -> Amos
    30 -> Obadiah
    31 -> Jonah
    32 -> Micah
    33 -> Nahum
    34 -> Habakkuk
    35 -> Zephaniah
    36 -> Haggai
    37 -> Zechariah
    38 -> Malachi
    n -> error $ "OldTestament has no book at index " <> show n

type OldTestament = OldTestament' Identity

oldTestamentToString :: (Two -> String -> String) -> OldTestament' Identity -> String
oldTestamentToString showTwo = \case
  Genesis -> "Genesis"
  Exodus -> "Exodus"
  Leviticus -> "Leviticus"
  Numbers -> "Numbers"
  Deuteronomy -> "Deuteronomy"
  Joshua -> "Joshua"
  Judges -> "Judges"
  Ruth -> "Ruth"
  Samuel x -> showTwo x "Samuel"
  Kings x -> showTwo x "Kings"
  Chronicles x -> showTwo x "Chronicles"
  Ezra -> "Ezra"
  Nehemiah -> "Nehemiah"
  Esther -> "Esther"
  Job -> "Job"
  Psalms -> "Psalms"
  Proverbs -> "Proverbs"
  Ecclesiastes -> "Ecclesiastes"
  SongOfSolomon -> "Song of Solomon"
  Isaiah -> "Isaiah"
  Jeremiah -> "Jeremiah"
  Lamentations -> "Lamentations"
  Ezekiel -> "Ezekiel"
  Daniel -> "Daniel"
  Hosea -> "Hosea"
  Joel -> "Joel"
  Amos -> "Amos"
  Obadiah -> "Obadiah"
  Jonah -> "Jonah"
  Micah -> "Micah"
  Nahum -> "Nahum"
  Habakkuk -> "Habakkuk"
  Zephaniah -> "Zephaniah"
  Haggai -> "Haggai"
  Zechariah -> "Zechariah"
  Malachi -> "Malachi"

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
instance Universe (NewTestament' Identity) -- Uses 'Enum' instance
instance Universe.Finite (NewTestament' Identity)
instance Bounded (NewTestament' f) where
  minBound = Matthew
  maxBound = Revelation
instance Enum (NewTestament' Identity) where
  fromEnum x = case x of
    Matthew -> 0
    Mark -> 1
    Luke -> 2
    John -> 3
    Acts -> 4
    Romans -> 5
    Corinthians Two_1 -> 6
    Corinthians Two_2 -> 7
    Galatians -> 8
    Ephesians -> 9
    Philippians -> 10
    Colossians -> 11
    Thessalonians Two_1 -> 12
    Thessalonians Two_2 -> 13
    Timothy Two_1 -> 14
    Timothy Two_2 -> 15
    Titus -> 16
    Philemon -> 17
    Hebrews -> 18
    James -> 19
    Peter Two_1 -> 20
    Peter Two_2 -> 21
    JohnEpistle Three_1 -> 22
    JohnEpistle Three_2 -> 23
    JohnEpistle Three_3 -> 24
    Jude -> 25
    Revelation -> 26
  toEnum x = case x of
    0 -> Matthew
    1 -> Mark
    2 -> Luke
    3 -> John
    4 -> Acts
    5 -> Romans
    6 -> Corinthians Two_1
    7 -> Corinthians Two_2
    8 -> Galatians
    9 -> Ephesians
    10 -> Philippians
    11 -> Colossians
    12 -> Thessalonians Two_1
    13 -> Thessalonians Two_2
    14 -> Timothy Two_1
    15 -> Timothy Two_2
    16 -> Titus
    17 -> Philemon
    18 -> Hebrews
    19 -> James
    20 -> Peter Two_1
    21 -> Peter Two_2
    22 -> JohnEpistle Three_1
    23 -> JohnEpistle Three_2
    24 -> JohnEpistle Three_3
    25 -> Jude
    26 -> Revelation
    n -> error $ "NewTestament has no book at index " <> show n

type NewTestament = NewTestament' Identity

newTestamentToString
  :: (Two -> String -> String)
  -> (Three -> String -> String)
  -> NewTestament' Identity
  -> String
newTestamentToString showTwo showThree = \case
  Matthew -> "Matthew"
  Mark -> "Mark"
  Luke -> "Luke"
  John -> "John"
  Acts -> "Acts"
  Romans -> "Romans"
  Corinthians x -> showTwo x "Corinthians"
  Galatians -> "Galatians"
  Ephesians -> "Ephesians"
  Philippians -> "Philippians"
  Colossians -> "Colossians"
  Thessalonians x -> showTwo x "Thessalonians"
  Timothy x -> showTwo x "Timothy"
  Titus -> "Titus"
  Philemon -> "Philemon"
  Hebrews -> "Hebrews"
  James -> "James"
  Peter x -> showTwo x "Peter"
  JohnEpistle x -> showThree x "John"
  Jude -> "Jude"
  Revelation -> "Revelation"

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
instance Universe (Canon' Identity) -- Uses 'Enum' instance
instance Universe.Finite (Canon' Identity)
instance Bounded (Canon' f) where
  minBound = Canon_Old minBound
  maxBound = Canon_New maxBound
instance Enum (Canon' Identity) where
  fromEnum x = case x of
    Canon_Old b -> fromEnum b
    Canon_New b -> 1 + fromEnum b + fromEnum (maxBound :: OldTestament' Identity)
  toEnum x
    | x <= lastOtIndex = Canon_Old $ toEnum x
    | otherwise = Canon_New $ toEnum $ x - lastOtIndex - 1
    where
      lastOtIndex = fromEnum (maxBound :: OldTestament' Identity)

type Canon = Canon' Identity

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
